{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude                (read)
import           VDiff.Prelude          hiding (catch, hFlush)

import           Control.Exception
import           Data.List
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Options.Applicative
import           Safe
import           System.Directory
import           System.Directory.Extra (listFilesRecursive)
import           System.FilePath.Posix
import           System.IO
import           System.Random.Shuffle

import           VDiff.Instrumentation

description :: String
description =
  " This is a small script that is supposed to run from inside the sv-benchmark folder. " ++
  " It randomly selects a number of files from SV-Comp in a unbiased fashion. For that it" ++
  " 'round-robins' the subfolders of the given folder and randomly picks a (parseable) file "

main :: IO ()
main = execParser opts >>= \case
  BuildDatabase fp -> do
    -- open database if exists
    db <- fromMaybe (Database Map.empty) <$> readDatabase "take-database"
    db_ref <- newIORef db
    allFiles <- listFilesRecursive fp
    relFiles <- filterM preferPreprocessed $ filterExisting db $ filterUnwanted allFiles
    -- process the remaining files
    _ <- abortable $ processQueue db_ref relFiles
    putStrLn "writing back to database file 'take-database'"
    readIORef db_ref >>= writeDatabase "take-database"

  FairSample n lvl -> do
    db <- readDatabase "take-database" >>= \case
      Nothing -> error "could not find take-database"
      Just db' -> return db'
    -- group into categories
    let categories = Map.elems $ groupIntoCategories db lvl
    -- shuffle the order of the categories
    categories' <- shuffleM categories
    -- shuffle each category in itself
    categories'' <- mapM shuffleM categories'
    -- read 'columnwise'
    let files = readColumnwise categories''
    mapM_ putStrLn $ take n files

processQueue :: IORef Database -> [FilePath] -> IO ()
processQueue db_ref files =
  forM_ files $ \f -> do
    putStrLn $ "testing " ++ f
    r <- testParse f
    let status = if r then Parseable else NotParseable
    putStrLn $ "result: " ++ show r
    atomicModifyIORef' db_ref (\(Database m) -> (Database (Map.insert f status m) ,()))


type CategoryLevel = Int

data Parameters
  = FairSample Int CategoryLevel
  | BuildDatabase FilePath
  deriving Show

parameterParser :: Parser Parameters
parameterParser =  sampleCmd <|> buildDatabaseCmd
  where
    sampleCmd = FairSample <$> parseNum <*> parseLevel
    buildDatabaseCmd = BuildDatabase <$> option str (long "build-db-for")
    parseNum = option auto $ mconcat [ long "num" , short 'n', help "limit the number of used cpus"]
    parseLevel = option auto $ mconcat [ long "category-level" , short 'l', help "defines what a category is", value 2]

opts :: ParserInfo Parameters
opts = info (parameterParser <**> helper) (progDesc description)



-- filter out categories that have "float" in its name (we know that most tools don't handle those well)
filterUnwanted :: [FilePath] -> [FilePath]
filterUnwanted = filter flt
  where
    flt file = and [ not (w `isInfixOf` file) | w <- blacklist]
    blacklist = ["float", "driver"]

--------------------------------------------------------------------------------
-- "Database"
newtype Database = Database (Map FilePath Status)

data Status = Parseable | NotParseable
  deriving (Read,Show,Eq)


readDatabase :: FilePath -> IO (Maybe Database)
readDatabase fp = do
  x <- doesFileExist fp
  if x
    then do
      cts <- T.readFile fp
      let m = foldl' insertLine Map.empty (T.lines cts)
      return $ Just $ Database m
    else
      return Nothing
  where
    insertLine :: Map FilePath Status -> Text -> Map FilePath Status
    insertLine m l = case T.splitOn ":" l of
                       [f,s] -> Map.insert (T.unpack f) (read (T.unpack s)) m
                       _     -> error "error parsing database file"

writeDatabase :: FilePath -> Database ->  IO ()
writeDatabase fp (Database m) = do
  h <- openFile fp WriteMode
  forM_ (Map.toAscList m ) $ \(f,s) -> do
    hPutStr h f
    hPutStr h ":"
    hPutStrLn h (show s)
  hFlush h

filterExisting :: Database -> [FilePath] -> [FilePath]
filterExisting (Database db) = filter $ \f -> f `Map.notMember` db


  
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- data mangling
preferPreprocessed :: FilePath -> IO Bool
preferPreprocessed fn
  | takeExtension fn == ".i" = return True
  | takeExtension fn == ".c" = not <$> doesFileExist ( replaceExtension fn ".i")
  | otherwise                = return False



readColumnwise :: [[a]] -> [a]
readColumnwise rows = let (r,ows) = takeFirstColumn rows
                      in r ++ readColumnwise ows

takeFirstColumn :: [[a]] -> ([a], [[a]])
takeFirstColumn rows = (r,rest)
  where
    r = mapMaybe headMay rows
    rest = filter (not . null) $ map tailSafe rows


groupIntoCategories :: Database -> Int -> Map Text [FilePath]
groupIntoCategories (Database db) lvl = foldl' insertIntoCategories Map.empty parseable
  where
    parseable    =  [(takePrefix f, f) | (f, Parseable) <- Map.assocs db]
    takePrefix f = T.intercalate "/" $ take lvl $ T.splitOn "/" $ T.pack f
    insertIntoCategories m (pref, f) = Map.insertWith (++) pref [f] m


-- executes the action, but catches ASyncExceptions (e.g. Heap Overflows, UserInterrupt, ThreadKilled)
abortable :: IO a -> IO (Maybe a)
abortable act = (Just <$> act) `catch` \(e :: AsyncException) -> do
  putStrLn $ displayException e
  return Nothing



testParse :: FilePath -> IO Bool
testParse f = testParse' `catch` (\(_ :: IOException) -> return False)
  where
    testParse' = do
      res <- liftIO $ runRIO NoLogging $ openCFile f
      case force $ prettyp <$> res of
        Nothing -> return False
        Just  _ -> return True
