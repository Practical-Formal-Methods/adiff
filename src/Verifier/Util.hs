module Verifier.Util
  ( Types.Verifier(..)
  , System.Exit.ExitCode(..)
  , VerifierResult(..)
  , Verdict(..)
  , module Safe
  , def
  , withSystemTempFile
  , hFlush
  , embedFile
  , module System.Process
  , withSpec
  , reachSafety
  , debugOutput
  , execTimed
  , withTiming
  , Timing
  , VerifierEnv
  )

where

import           RIO
import           System.IO             (hPutStr)

import           Data
import           Timed
import           Types

import qualified Data.ByteString.Char8 as C8
import           Data.Default          (def)
import           Data.FileEmbed
import qualified Data.Text             as T
import           Safe
import           System.Exit
import           System.Process

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

toDisplayBuilder :: String -> Utf8Builder
toDisplayBuilder = displayBytesUtf8 . C8.pack

{-# DEPRECATED execTimed "use withTiming instead" #-}
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
  logInfo $ "runing command: " <> displayShow (cmdspec cp)
  (termination, out, err) <- execTimed cp inp
  case termination of
    Nothing           -> do
      logInfo "command timed out"
      return VerifierTimedOut
    Just (ec, timing) -> do
      logInfo $ "command terminated with exit code " <> display (tshow ec)
      vrdct <- cont ec out err
      return $ VerifierTerminated vrdct timing

