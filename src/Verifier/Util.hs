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
  , Timing
  , VerifierEnv
  )

where

import           RIO
import           System.IO      (hPutStr)

import           Data
import           Timed
import           Types

import           Data.Default   (def)
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
