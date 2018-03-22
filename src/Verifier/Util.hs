module Verifier.Util
  ( Types.Verifier(..)
  , System.Exit.ExitCode(..)
  , VerifierResult(..)
  , module Safe
  , def
  , withSystemTempFile
  , hFlush
  , embedFile
  , module System.Process
  , withSpec
  , reachSafety
  , execTimed
  , debugOutput
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


debugOutput :: HasLogFunc env => String -> String -> RIO env ()
debugOutput verifierName out = do
  let ls = lines out
  forM_ ls $ \l ->
    logDebug $ "[" <> toDisplayBuilder verifierName  <> "] " <> toDisplayBuilder l

toDisplayBuilder :: String -> DisplayBuilder
toDisplayBuilder = displayBytesUtf8 . C8.pack
