{-# LANGUAGE LambdaCase #-}

import           VDiff.Prelude

import           Control.Monad
import           Data.List
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           System.Random.Shuffle

import           VDiff.Instrumentation

description :: String
description =
  " This is a small script that is supposed to run from inside the sv-benchmark folder. " ++
  " It randomly selects a number of files from SV-Comp in a unbiased fashion. For that it" ++
  " 'round-robins' the subfolders of the given folder and randomly picks a (parseable) file "


data Parameters = Parameters
  { numFiles :: Int
  } deriving Show


parameterParser :: Parser Parameters
parameterParser =  Parameters <$> parseNum
  where
    parseNum = option auto $ mconcat [ long "num" , short 'n', help "limit the number of used cpus"]

opts :: ParserInfo Parameters
opts = info (parameterParser <**> helper) (progDesc description)


isNotInfixOf a b = not $ isInfixOf a b

-- filter out categories that have "float" in its name (we know that most tools don't handle those well)
filterFiles :: [FilePath] -> [FilePath]
filterFiles = filter flt
  where
    flt file = and $ [ w `isNotInfixOf` file | w <- blacklist]
    blacklist = ["float", "driver"]

data Finished = Finished

data TakeEnv = TakeEnv
  { logFunction :: LogFunc
  , memostore   :: IORef (Map FilePath Bool)
  }
instance HasLogFunc TakeEnv where
  logFuncL = lens logFunction (\env l -> env {logFunction = l})

main :: IO ()
main = do
  parameters <- execParser opts
  -- otherwise "tee" is a little unsatisfying
  System.IO.hSetBuffering stdout LineBuffering
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogMinLevel LevelInfo logOptions
  withLogFunc logOptions' $ \logger -> do
    memostoreRef <- newIORef (Map.empty)
    let env = TakeEnv logger memostoreRef
    runRIO env $ do
      logInfo "starting vdiff-take"
      folders <- (subfolders "." >>= shuffleM)
      filesRef <- newIORef Set.empty
      forM_ (cycle folders) $ \folder -> do
        files <- readIORef filesRef
        -- abort when we have files
        when (Set.size files >= numFiles parameters) $ liftIO exitSuccess
        -- try to pick a file
        pickFile folder >>= \case
          Nothing -> logInfo "no file found"
          Just f -> do
            logInfo $ "found file: " <> display (tshow f)
            when (not (Set.member f files)) $ do
                -- store the file in the set
                modifyIORef filesRef (Set.insert f)
                -- write the file to stdout
                liftIO $ putStrLn f

subfolders :: (MonadIO m, MonadReader env m, HasLogFunc env) => FilePath -> m [FilePath]
subfolders dir = do
  contents <- liftIO $ listDirectory dir
  filterM (liftIO . doesDirectoryExist) contents

pickFile :: FilePath -> RIO TakeEnv (Maybe FilePath)
pickFile dir = do
  logInfo $ "trying to pick file from " <> display (tshow dir)
  contents <- filterFiles . filter (".i" `isSuffixOf`) . map (\fn -> dir ++ "/" ++ fn) <$> liftIO (listDirectory dir)
  files <- shuffleM =<< filterM (liftIO.doesFileExist) contents
  takeFirstM testParse files

-- | this function uses the memostore in the environment to memoize its results
testParse :: FilePath -> RIO TakeEnv Bool
testParse file = do
  storeRef <- asks memostore
  store <- readIORef storeRef
  case (Map.lookup file store) of
    Nothing -> do
      res <- testParse' file
      liftIO $ writeIORef storeRef $ Map.insert file res store
      return res
    Just res -> return res


  where
    testParse' file = do
      res <- liftIO $ runRIO NoLogging $ openCFile file
      case (force $ prettyp <$> res) of
        Nothing -> return False
        Just  _ -> return True



takeFirstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
takeFirstM t []     = return Nothing
takeFirstM t (x:xs) = t x >>= \case
                                True -> return (Just x)
                                False -> takeFirstM t xs
