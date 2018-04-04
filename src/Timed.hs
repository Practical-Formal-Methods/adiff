-- | Provides a simple way to execute any @CreateProcess@ wrapped in a call to /usr/bin/time
-- | It's a very simple implementation. Also: Please do not use it for lazy IO as the string printed to stderr will be fully evaluated.
module Timed
  ( Timing
  , exec
  , userTime
  , systemTime
  , elapsedWall
  , maxResidentMemory
  ) where

import           Data.Text.IO       (hPutStr)
import           Prelude            (read)
import           RIO
import qualified RIO.List.Partial           as L

import           Control.Concurrent (forkIO, ThreadId)
import           Data.Default
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

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


readNonBlockingUntilTerminated :: ProcessHandle -> Handle -> IORef ByteString -> MVar () -> IO ()
readNonBlockingUntilTerminated ph h ref mutex = do
      open <- hIsOpen h
      if open
        then do
          -- Read any outstanding input.
          readChunk
          -- Check on the process.
          s <- getProcessExitCode ph
          -- Exit or loop.
          case s of
              Nothing -> readNonBlockingUntilTerminated ph h ref mutex
              Just _ -> do
                readChunk
                terminate
        else terminate
  where
    readChunk = do
          bs <- BS.hGetNonBlocking h (64 * 1024)
          modifyIORef ref (<> bs)
    terminate = tryPutMVar mutex () >> return ()
      
        

exec :: CreateProcess -> Text -> Int -> IO (Maybe (ExitCode, Timing), ByteString, ByteString)
exec cp input microsecs = do
  let cp' = (wrapTime cp) { std_in = CreatePipe
                          , std_out = CreatePipe
                          , std_err = CreatePipe
                          }
  withCreateProcess cp' $ \(Just inh) (Just outh) (Just errh) ph -> do
    -- set-up "killer" thread
    _ <- terminateDelayed ph microsecs

    -- set up streams
    hPutStr inh input >> hFlush inh
    out_ref <- newIORef ""
    err_ref <- newIORef ""
    m_out <- newEmptyMVar
    m_err <- newEmptyMVar
    _ <- forkIO $ readNonBlockingUntilTerminated ph outh out_ref m_out
    _ <- forkIO $ readNonBlockingUntilTerminated ph errh err_ref m_err

    -- wait for process to finish
    code <- waitForProcess ph

    -- wait for reader threads
    takeMVar m_out
    takeMVar m_err
    out <- readIORef out_ref
    err <- readIORef err_ref
    -- timing <- parseTimed err
    -- let err' = C8.unlines $ L.init $ C8.lines err
    case code of
      ExitSuccess -> do
        (timing, err') <- parseTimed err
        return (Just (code, timing), out, err')
      ExitFailure n -> if n < 0
                       then return (Nothing, out, err)
                       else do
                            (timing, err') <- parseTimed err
                            return (Just (code, timing), out, err')

terminateDelayed :: ProcessHandle -> Int -> IO ThreadId
terminateDelayed ph microsecs = forkIO $ do
      threadDelay microsecs
      getProcessExitCode ph >>= \case
        Nothing -> terminateProcess ph
        Just _ -> return ()

wrapTime :: CreateProcess -> CreateProcess
wrapTime cp = cp'
  where
    cp' = cp {cmdspec = modify (cmdspec cp) }
    modify (ShellCommand cmd)   = ShellCommand ("/usr/bin/time -f " ++ format ++ " sh -c '" ++ cmd ++ "'")
    modify (RawCommand fp args) = RawCommand "/usr/bin/time" ("-f":format:fp:args)
    format = "\"%U %S %e %M\""

data TimedError = ParsingError
  deriving Show

instance Exception TimedError

-- TODO: IO exceptions are not nice
parseTimed :: ByteString -> IO (Timing, ByteString)
parseTimed str = do
  let l = C8.lines str
      w = C8.words $ L.last l
  case map C8.unpack w  of
    [u, s, e, m] -> do
                      let timing = Timing (read u) (read s) (read e) (read m)
                      return (timing, C8.unlines (L.init l))
    _            -> throwIO ParsingError


