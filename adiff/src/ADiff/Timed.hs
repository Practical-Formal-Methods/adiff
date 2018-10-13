{-# LANGUAGE LambdaCase #-}

-- | Provides a simple way to execute any @CreateProcess@ wrapped in a call to /usr/bin/time
-- | It's a very simple implementation. Also: Please do not use it for lazy IO as the string printed to stderr will be fully evaluated.
module ADiff.Timed
  ( Timing(..)
  , exec
  ) where

import           Control.Concurrent      (ThreadId, forkIO)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8   as C8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text.IO            (hPutStr)
import qualified RIO.List.Partial        as L
import           Text.Read               (readMaybe)
import           ADiff.Prelude

import           System.Process

data Timing = Timing
  { userTime          :: !Double -- ^ time spend in user space (in seconds)
  , systemTime        :: !Double -- ^ time spend in kernel mode (in seconds)
  , elapsedWall       :: !Double -- ^ elapsed wall time (in seconds)
  , maxResidentMemory :: !Int    -- ^ maximum residential memory (in kilobytes)
  } deriving (Show)


instance Display Timing where
  display (Timing u s e m) = mconcat [ displayShow u <> "s user "
                                     , displayShow s <> "s system "
                                     , displayShow e <> "s wall "
                                     , display m <> "KiB mem"
                                     ]


readNonBlockingUntilTerminated :: ProcessHandle -> Handle -> IORef BS.Builder -> MVar () -> IO ()
readNonBlockingUntilTerminated ph h ref mutex = do
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
  where
    readChunk = do
          bs <- BS.hGetNonBlocking h (64 * 1024)
          when (BS.length bs > 0) $ modifyIORef ref (<> BS.byteString bs)
    terminate = void $ tryPutMVar mutex ()



exec :: CreateProcess -> Bool -> Text -> Timelimit -> IO (Maybe (ExitCode, Timing), ByteString, ByteString)
exec cp rkill input tl = do
  let cp' = (wrapTime cp) { std_in = CreatePipe
                          , std_out = CreatePipe
                          , std_err = CreatePipe
                          , create_group = True -- so we can kill later the whole group if necessary
                          }
  withCreateProcess cp' $ \(Just inh) (Just outh) (Just errh) ph -> do
    -- set-up "killer" thread
    _ <- terminateDelayed ph rkill tl

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
    out <- LBS.toStrict . BS.toLazyByteString <$> readIORef out_ref
    err <- LBS.toStrict . BS.toLazyByteString <$> readIORef err_ref


    -- timing <- parseTimed err
    -- let err' = C8.unlines $ L.init $ C8.lines err
    case code of
      ExitSuccess -> do
        (timing, err') <- parseTimed_ err
        return (Just (code, timing), out, err')
      ExitFailure n -> if n < 0
                       then return (Nothing, out, err)
                       else do
                         (timing, err') <- parseTimed_ err
                         return (Just (code, timing), out, err')

terminateDelayed :: ProcessHandle
                 -> Bool -- killall complete process group
                 -> Timelimit -- microseconds
                 -> IO ThreadId -- thread id of the created 'terminator process'
terminateDelayed ph rkill tl = forkIO $ do
      threadDelay (microseconds tl)
      getProcessExitCode ph >>= \case
        Nothing ->
          if rkill
            then do
              (Just pid) <- getPid ph
              callCommand $ "kill -9 -" ++ show pid
            else terminateProcess ph
        Just _ -> return ()

wrapTime :: CreateProcess -> CreateProcess
wrapTime cp = cp'
  where
    cp' = cp {cmdspec = modify (cmdspec cp) }
    modify (ShellCommand cmd)   = ShellCommand ("/usr/bin/time -f '" ++ format ++ "' sh -c '" ++ cmd ++ "'")
    modify (RawCommand fp args) = RawCommand "/usr/bin/time" ("-f":format:fp:args)
    format = "%U %S %e %M"

data TimedError = ParsingError
  deriving Show

instance Exception TimedError


parseTimed_ :: ByteString -> IO (Timing, ByteString)
parseTimed_ bs = case parseTimed bs of
                   Just res -> return res
                   Nothing -> error $ "terminated, but couldn't parse the output of `time`, output was " <> show bs


parseTimed :: ByteString -> Maybe (Timing, ByteString)
parseTimed str = do
  let l = C8.lines str
      w = C8.words $ L.last l
  case map C8.unpack w  of
    [u, s, e, m] -> do
      u' <- readMaybe u
      s' <- readMaybe s
      e' <- readMaybe e
      m' <- readMaybe m
      let timing = Timing u' s' e' m'
      return (timing, C8.unlines (L.init l))
    _            -> Nothing


