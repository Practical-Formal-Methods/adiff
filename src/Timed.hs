-- | Provides a simple way to execute any @CreateProcess@ wrapped in a call to /usr/bin/time
-- | It's a very simple implementation. Also: Please do not use it for lazy IO as the string printed to stderr will be fully evaluated.
module Timed
  ( Timing
  , execTimed
  , userTime
  , systemTime
  , elapsedWall
  , maxResidentMemory
  ) where

import           Prelude        (read)
import           RIO
import qualified RIO.List       as L

import           Data.Default
import           System.Process

data Timing = Timing
  { userTime          :: Double -- ^ time spend in user space (in seconds)
  , systemTime        :: Double -- ^ time spend in kernel mode (in seconds)
  , elapsedWall       :: Double -- ^ elapsed wall time (in seconds)
  , maxResidentMemory :: Int    -- ^ maximum residential memory (in kilobytes)
  } deriving (Show)

instance Display Timing where
  display (Timing u s e m) = mconcat [ displayShow u <> "s user "
                                     , displayShow s <> "s system "
                                     , displayShow e <> "s wall "
                                     , display m <> "KiB mem"
                                     ]

instance Default Timing where
  def = Timing 0 0 0 0

execTimed :: HasLogFunc env => CreateProcess -> String -> RIO env (ExitCode, String, Timing)
execTimed cp inp = do
  (code, out, err) <- liftIO $ readCreateProcessWithExitCode cp' inp
  logInfo $ "executing timed command: " <> displayShow (cmdspec cp')
  timed <- liftIO $ parseTimed err
  logInfo $ "command terminated with timing: " <> display timed
  return (code, out, timed)
  where
    cp' = cp {cmdspec = modify (cmdspec cp) }
    modify (ShellCommand cmd)   = ShellCommand ("/usr/bin/time -f " ++ format ++ " " ++ cmd)
    modify (RawCommand fp args) = RawCommand "/usr/bin/time" ("-f":format:fp:args)
    format = "\"%U %S %e %M\""

data TimedError = ParsingError
  deriving Show

instance Exception TimedError

-- TODO: IO exceptions are not nice
parseTimed :: String -> IO Timing
parseTimed str = let w = words $ L.last $ lines str
                 in case w of
                      [u, s, e, m] -> return $ Timing (read u) (read s) (read e) (read m)
                      _            -> throwIO ParsingError


