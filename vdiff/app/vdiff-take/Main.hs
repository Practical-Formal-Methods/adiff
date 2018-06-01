{-# LANGUAGE LambdaCase #-}


import           Control.Monad
import           Data.List
import qualified Data.Set              as Set
import           Options.Applicative
import           RIO
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           System.Random.Shuffle

import           VDiff.Instrumentation
import           VDiff.Types

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

main :: IO ()
main = do
  parameters <- execParser opts
  -- otherwise "tee" is a little unsatisfying
  RIO.hSetBuffering stdout LineBuffering
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogMinLevel LevelInfo logOptions
  withLogFunc logOptions' $ \logger -> runRIO logger $ do
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

pickFile :: (HasLogFunc env) => FilePath -> RIO env (Maybe FilePath)
pickFile dir = do
  logInfo $ "trying to pick file from " <> display (tshow dir)
  contents <- filterFiles . filter (".i" `isSuffixOf`) . map (\fn -> dir ++ "/" ++ fn) <$> liftIO (listDirectory dir)
  files <- shuffleM =<< filterM (liftIO.doesFileExist) contents
  takeFirstM testParse files

testParse :: (HasLogFunc env) => FilePath -> RIO env Bool
testParse file = do
  res <- liftIO $ runRIO NoLogging $ openCFile file
  case (force $ prettyp <$> res) of
    Nothing -> return False
    Just  _ -> return True


takeFirstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
takeFirstM t []     = return Nothing
takeFirstM t (x:xs) = t x >>= \case
                                True -> return (Just x)
                                False -> takeFirstM t xs
