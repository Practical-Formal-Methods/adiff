-- | Provides a simple way to execute any @CreateProcess@ wrapped in a call to /usr/bin/time
-- | It's a very simple implementation. Also: Please do not use it for lazy IO as the string printed to stderr will be fully evaluated.
module Timed
  ( Timing
  , exec
  , execTimed
  , userTime
  , systemTime
  , elapsedWall
  , maxResidentMemory
  ) where

import           Data.Text.IO       (hGetContents, hPutStr)
import           Prelude            (read)
import           RIO
import qualified RIO.List           as L

import           Control.Concurrent (forkIO, ThreadId)
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



exec :: CreateProcess -> Text -> Int -> IO (Maybe (ExitCode, Text, Text))
exec cp input microsecs = do
  let cp' = cp { std_in = CreatePipe
               , std_out = CreatePipe
               , std_err = CreatePipe
               }
  withCreateProcess cp' $ \(Just inh) (Just outh) (Just errh) ph -> do
    -- set-up "killer" thread
    killed <- newIORef False
    _ <- terminateDelayed ph killed microsecs

    -- set up streams
    hPutStr inh input >> hFlush inh
    out <- hGetContents outh
    err <- hGetContents errh

    -- wait for process to finsih
    code <- waitForProcess ph

    -- check if it was killed
    readIORef killed >>= \case
      False -> return $ Just (code, out, err)
      True -> return Nothing

terminateDelayed :: ProcessHandle -> IORef Bool -> Int -> IO ThreadId
terminateDelayed ph var microsecs = forkIO $ do
      threadDelay microsecs
      getProcessExitCode ph >>= \case
        Nothing -> do
          terminateProcess ph
          writeIORef var True
        Just _ -> return ()


execTimed :: HasLogFunc env => CreateProcess -> String -> RIO env (ExitCode, String, String, Timing)
execTimed cp inp = do
  logInfo $ "executing timed command: " <> displayShow (cmdspec cp')
  (code, out, err) <- liftIO $ readCreateProcessWithExitCode cp' inp
  timed <- liftIO $ parseTimed err
  logInfo $ "command terminated with timing: " <> display timed
  return (code, out, L.init err, timed)
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


