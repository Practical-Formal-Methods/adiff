module ADiff.Verifier.Util
  ( Verifier(..)
  , System.Exit.ExitCode(..)
  , VerifierResult
  , Verdict(..)
  , module Safe
  , withSystemTempFile
  , hFlush
  , embedFile
  , withSpec
  , reachSafety
  , debugOutput
  , withTiming
  , ADiff.Verifier.Util.callCommand
  , Sys.readCreateProcess
  , Sys.shell
  , Sys.proc
  , Sys.CreateProcess(..)
  , Timing
  , VerifierEnv
  )

where

import           ADiff.Prelude

import qualified Data.ByteString.Char8 as C8
import           Data.FileEmbed
import           Safe
import           System.Exit
import           System.IO             (hPutStr)
import           System.Process        as Sys

import           ADiff.Data
import           ADiff.Timed

callCommand :: HasLogFunc env => [String] ->  RIO env ()
callCommand cmd = do
  logDebug $ display $ tshow cmd
  liftIO $ Sys.callCommand $ unwords cmd

withSpec :: (MonadUnliftIO m) => Property -> (FilePath -> m a) -> m a
withSpec p f = withSystemTempFile "spec.prp" $ \fp hndl -> do
  liftIO $ hPutStr hndl p
  liftIO $ hFlush hndl
  f fp

reachSafety :: Property
reachSafety = "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )"


debugOutput :: HasLogFunc env => Text -> ByteString -> RIO env ()
debugOutput vn out = do
  let ls = C8.lines out
  forM_ ls $ \l ->
    logDebug $  "[" <> display vn <> "] " <> displayBytesUtf8 l


execTimed :: HasTimeLimit env => CreateProcess -> Text -> RIO env (Maybe (ExitCode, Timing), ByteString, ByteString)
execTimed cp inp = do
  tl <- view timeLimitL
  liftIO $ exec cp True inp tl

withTiming :: (HasLogFunc env, HasTimeLimit env) =>
              CreateProcess
           -> Text
           -> (ExitCode -> ByteString -> ByteString -> RIO env Verdict)
           -> RIO env VerifierResult
withTiming cp inp cont = do
  logInfo $ "running command (withTiming) " <> displayShow (cmdspec cp)
  (termination, out, err) <- execTimed cp inp
  case termination of
    Nothing           -> do
      logInfo "command timed out"
      return $ VerifierResult Nothing Nothing Unknown
    Just (ec, t) -> do
      logInfo $ "command terminated with exit code " <> display (tshow ec)
      vrdct <- cont ec out err
      return $ VerifierResult (Just $ elapsedWall t) (Just $ maxResidentMemory t) vrdct

