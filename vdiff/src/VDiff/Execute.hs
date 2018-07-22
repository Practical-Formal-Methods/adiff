{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module VDiff.Execute
  ( executeVerifier
  , executeVerifierInDocker
  , MemoryConstraint(..)
  , MemoryConstraintSize
  -- * Execution package
  , ExecutionPackage(..)
  , packageVerifierName
  , verifierExtraFlags
  , inputFile
  ) where

import           Docker.Client       as Docker

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           UnliftIO.Concurrent
import           VDiff.Data          hiding (memory)
import           VDiff.Prelude       hiding (force)

-- this is passed through the docker boundary
data ExecutionPackage
  = ExecutionPackage
  { _packageVerifierName :: VerifierName
  , _verifierExtraFlags  :: [Text]
  , _inputFile           :: Text
  } deriving (Show, Read)


executeVerifier :: Verifier -> FilePath -> RIO VerifierEnv VerifierResult
executeVerifier v fp = try (_verifierExecute v fp) >>= \case
  Left (e :: IOException) -> do
    logWarn $ "verifier " <> display (_name v) <> " just caused an IO exception: " <> display (tshow $ displayException e)
    return $ VerifierResult Nothing Nothing Unknown
  Right res -> return res


instance MonadUnliftIO (DockerT IO) where
  withRunInIO :: ((forall a. DockerT IO a -> IO a) -> IO b) -> DockerT IO b
  withRunInIO inner = do
    env <- ask
    liftIO $ inner $ runDockerT env




executeVerifierInDocker :: (HasLogFunc env) => VerifierResources -> VerifierName -> Text -> RIO env VerifierResult
executeVerifierInDocker resources vn source = do
  let pkgS = T.pack $ show $ ExecutionPackage vn [] source -- TODO: Handle Flags
  h <- liftIO defaultHttpHandler
  withSystemTempDirectory "exchange" $ \dirPath -> do
      -- serialize the package and write it into a temporary file
      writeFileUtf8 (dirPath <> "/package") pkgS
      writeFileUtf8 (dirPath <> "/output") ""  -- just to make sure that it is a file and not a directory

      -- prepare a few functions to be used in pure IO
      u <- askUnliftIO
      let logWarnIO = liftIO . unliftIO u . logWarn :: Utf8Builder -> IO ()
      let logInfoIO = liftIO . unliftIO u . logInfo :: Utf8Builder -> IO ()

      liftIO $ runDockerT (defaultClientOpts, h) $ do -- TODO: Find out why we cannot use unix:///var/run/docker.sock
        -- create container
        let cops  = mkCreateOpts resources dirPath
        cid <- createContainer cops Nothing
        case cid of
          Left err -> error $ "could not create container, error: " ++ show err
          Right i -> do
            (resultVar :: MVar (Maybe VerifierResult)) <- newEmptyMVar
            -- one thread for the container
            t_container <- forkIO $ do
              startContainer defaultStartOpts i
              liftIO $ logInfoIO $ "started container " <> display i <> " for verifier " <> display vn
              waitContainer i >>= \case
                Right ExitSuccess  -> do
                  runOutput <- readFileUtf8 (dirPath <> "/output")
                  case readMay $ T.unpack runOutput of
                    Nothing -> putMVar resultVar Nothing
                    Just r  -> putMVar resultVar (Just r)
                Right (ExitFailure err) -> liftIO $ logWarnIO $ "internal vdiff exited with status code " <> display err
                Left dockerErr -> putMVar resultVar Nothing

            -- one thread for the timer
            t_timer <- forkIO $ do
              threadDelay $ (resources ^. timelimit) * 1000 * 1000
              putMVar resultVar Nothing
            -- wait until at least one of the threads has completed
            result <- takeMVar resultVar
            case result of
              Nothing -> return $ VerifierResult Nothing Nothing Unknown
              Just r  -> return r
            `finally` do
              liftIO $ logInfoIO $ "deleting container " <> display i
              deleteContainer (defaultDeleteOpts { force = True}) i

mkCreateOpts :: VerifierResources -> FilePath -> CreateOpts
mkCreateOpts simpleConstraints dirPath =
  (defaultCreateOpts "vdiff/vdiff:latest")
  { containerConfig = (defaultContainerConfig "vdiff/vdiff:latest")
                      { env = [EnvVar "PATH" "/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"]
                      , cmd = ["/root/.local/bin/vdiff-run-package", "/exchange/package", "/exchange/output"]
                      }
  , hostConfig = defaultHostConfig
                 { binds = [Bind (T.pack dirPath) "/exchange" Nothing]
                 , resources =  defaultContainerResources
                                { memorySwap    = _memory simpleConstraints
                                , Docker.memory = _memory simpleConstraints
                                , cpusetCpus    = _cpus simpleConstraints
                                }
                 , logConfig = LogDriverConfig JsonFile []
                 }
  }

makeFieldsNoPrefix ''ExecutionPackage
