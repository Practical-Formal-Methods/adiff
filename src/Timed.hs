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

import           Control.Exception (throwIO)
import           Data.Default
import           System.Exit
import           System.Process

data Timing = Timing
  { userTime          :: Double -- ^ time spend in user space (in seconds)
  , systemTime        :: Double -- ^ time spend in kernel mode (in seconds)
  , elapsedWall       :: Double -- ^ elapsed wall time (in seconds)
  , maxResidentMemory :: Int    -- ^ maximum residential memory (in kilobytes)
  } deriving (Show)

instance Default Timing where
  def = Timing 0 0 0 0

execTimed :: CreateProcess -> String -> IO (ExitCode, String, Timing)
execTimed cp inp = do
  (code, out, err) <- readCreateProcessWithExitCode cp' inp
  timed <- parseTimed err
  return (code, out, timed)
  where
    cp' = cp {cmdspec = modify (cmdspec cp) }
    modify (ShellCommand cmd)   = ShellCommand ("/usr/bin/time -f " ++ format ++ " " ++ cmd)
    modify (RawCommand fp args) = RawCommand "/usr/bin/time" ("-f":format:fp:args)
    format = "\"%U %S %e %M\""


parseTimed :: String -> IO Timing
parseTimed str = let w = words $ last $ lines str
                 in case w of
                      [u, s, e, m] -> return $ Timing (read u) (read s) (read e) (read m)
                      _            -> throwIO $ userError "error parsing output of the time command"


