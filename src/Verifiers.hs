{-# LANGUAGE TemplateHaskell #-}

module Verifiers
  ( VerifierResult
  , Verifier(..)
  , allVerifiers
  ) where

import qualified Data.ByteString.Char8 as C8
import           Data.FileEmbed
import           Data.Monoid           ((<>))
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process

import           Debug.Trace
import           Types

allVerifiers :: [Verifier]
allVerifiers = [cbmc, vim, uautomizer, klee]

-- | This is the cbmc verifier. The last line of its output on stdout tells us
-- the result of the verification.
cbmc :: Verifier
cbmc = def { verifierName = "cbmc", execute =  run, version = cbmcVersion }
  where run fn = do
          let cmd = "cbmc --32 --error-label ERROR " <> fn
          (exitCode, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
          let lastLine = last $ lines out
          case (exitCode, lastLine) of
              (ExitSuccess,"VERIFICATION FAILED")     -> return VerificationFailed
              (ExitSuccess,"VERIFICATION SUCCESSFUL") -> return VerificationSuccessful
              _                                       -> return VerificationFailed
        cbmcVersion = Just . head . lines <$> readCreateProcess (shell "cbmc --version") ""



-- | This is not a real verifier. It uses vim to show the file to the user.
-- The user can then say successful by closing vim regularly (:q) or failed by closing vim with non-zero exit code (:cq)
vim :: Verifier
vim = def { verifierName = "vim", execute = run, version = vimVersion }
  where run fn = do
          let process  = (shell ("vim -R " ++ fn)) {
                  std_in        = Inherit
                , std_out       = Inherit
                , delegate_ctlc = False }

          (_,_,_,ph) <- createProcess process
          exitCode <- waitForProcess ph
          case exitCode of
            ExitSuccess   -> return VerificationSuccessful
            ExitFailure _ -> return VerificationFailed
        vimVersion = do
          s <- readCreateProcess (shell "vim --version") ""
          return $ Just <$> head $ lines s

uautomizer :: Verifier
uautomizer = def { verifierName = "uautomizer", execute = run, version = uautomizerVersion}
  where run fn = withSpec reachSafety $ \spec -> do
            let cmd = "Ultimate.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec
            putStrLn cmd
            (exitCode, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
            let lastLine = last $ lines out
            case (exitCode, lastLine) of
                (ExitSuccess,"TRUE")  -> return VerificationSuccessful
                (ExitSuccess,"FALSE") -> return VerificationFailed
                _                     -> return VerificationResultUnknown
        uautomizerVersion = Just . head . lines <$> readCreateProcess (shell "Ultimate.py --version") ""

klee :: Verifier
klee = def { verifierName = "klee", execute = kleeExecute, version = kleeVersion}
  where
    kleeVersion = do
      (_,out,_) <- readCreateProcessWithExitCode (shell "klee --version") ""
      return $ (Just . head . lines) out

    kleeExecute fn = withKleeH $ \kleeH ->
        withSystemTempFile "file.bc" $ \bc _ -> do
        callCommand $ "clang -emit-llvm -I " ++ kleeH ++ " -c -g " ++ fn ++ " -o " ++ bc
        (exitCode, out, _) <- readCreateProcessWithExitCode (shell $ "klee " ++ bc) ""
        case (exitCode, null out) of
          (ExitSuccess, True)  -> return VerificationSuccessful
          (ExitSuccess, False) -> return VerificationFailed
          (ExitFailure _, _ )  -> return VerificationResultUnknown

    withKleeH :: (FilePath -> IO a) -> IO a
    withKleeH actn = withSystemTempFile "klee.h"$ \f h -> do
      C8.hPutStrLn h $(embedFile "assets/klee.h")
      hFlush h
      actn f


withSpec :: Property -> (FilePath -> IO a) -> IO a
withSpec p f = withSystemTempFile "spec.prp" $ \fp hndl -> do
  hPutStr hndl p
  hFlush hndl
  f fp

reachSafety :: Property
reachSafety = "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )"
