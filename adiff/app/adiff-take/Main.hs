-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude                (read)
import           ADiff.Prelude          hiding (catch, hFlush)

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

import           ADiff.Instrumentation

description, descriptionGenerate, descriptionTake :: String
description = " This tool produces a fair sampling of benchmarks (e.g. from SV-Comp). "
descriptionGenerate = " Build a database that contains the parseability property of every file in the given directory"
descriptionTake = " Takes a fair sample of parseable benchmarks. It excludes a priori benchmarks containing the words " ++
                  " 'float' or 'driver'. 'Fair' means that it round-robins between the categories. Note that this is only" ++
                  " fair for smallsample sizes. When the sample size is too big, there might be categories that are already" ++
                  " depleted, so other there will be more benchmarks from other categories. "

type CategoryLevel = Int
type DbPath = FilePath

data Parameters
  = BuildDatabase DbPath FilePath
  | FairSample DbPath Int CategoryLevel
  deriving Show


main :: IO ()
main = execParser opts >>= \case
  BuildDatabase dbPath fp -> do
    -- open database if exists
    db <- fromMaybe (Database Map.empty) <$> readDatabase dbPath
    db_ref <- newIORef db
    allFiles <- listFilesRecursive fp
    relFiles <- filterM preferPreprocessed $ filterExisting db $ filterUnwanted allFiles
    -- process the remaining files
    _ <- abortable $ processQueue db_ref relFiles
    putStrLn $ "writing back to database file "++ dbPath
    readIORef db_ref >>= writeDatabase dbPath

  FairSample dbPath n lvl -> do
    db <- readDatabase dbPath >>= \case
      Nothing -> error "could not find take-database"
      Just db' -> return db'
    -- group into categories
    let categories = Map.elems $ groupIntoCategories db lvl
    -- shuffle the order of the categories and the categories itself
    categories' <- shuffleM =<< mapM shuffleM categories
    -- read 'columnwise'
    let files = readColumnwise categories'
    mapM_ putStrLn $ take n files

processQueue :: IORef Database -> [FilePath] -> IO ()
processQueue db_ref files =
  forM_ files $ \f -> do
    putStrLn $ "testing " ++ f
    r <- testParse f
    let status = if r then Parseable else NotParseable
    putStrLn $ "result: " ++ show r
    atomicModifyIORef' db_ref (\(Database m) -> (Database (Map.insert f status m) ,()))


-- filter out categories that have "float" in its name (we know that most tools don't handle those well)
filterUnwanted :: [FilePath] -> [FilePath]
filterUnwanted = filter flt
  where
    flt file = and [ not (w `isInfixOf` file) | w <- blacklist]
    blacklist = ["float", "driver", "eca-rers2012"]

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
    hPrint h s
  hFlush h

filterExisting :: Database -> [FilePath] -> [FilePath]
filterExisting (Database db) = filter $ \f -> f `Map.notMember` db

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


-- | Run the parser (and typechecker) for up to 5 seconds and catch any IOException
testParse :: FilePath -> IO Bool
testParse f = timeoutM (5 * 1000 * 1000) testParse' `catch` (\(_ :: IOException) -> return False)
  where
    testParse' = do
      res <- liftIO $ runRIO NoLogging $ openCFile f
      case force $ prettyp <$> res of
        Nothing -> return False
        Just  _ -> return True

--------------------------------------------------------------------------------
-- Parameter parsing
parameterParser  :: Parser Parameters
parameterParser = hsubparser $ mconcat [ command "generate" (info generateOptions (progDesc descriptionGenerate))
                                       , command  "take" (info takeOptions (progDesc descriptionTake))
                                       ]
  where
    generateOptions = BuildDatabase <$> dbFile <*> argument str (metavar "DIRECTORY" <> value "./" <> action "file")
    takeOptions     = FairSample <$> dbFile <*> parseNum <*> parseLevel
    parseNum        = option auto $ mconcat [ long "num" , short 'n', help "sample size"]
    parseLevel      = option auto $ mconcat [ long "category-level" , short 'l', help "defines what a category is", value 2]
    dbFile          = option str  $ mconcat [ long "database", short 'd', action "file" ]

opts :: ParserInfo Parameters
opts = info (parameterParser <**> helper) (progDesc description)


--------------------------------------------------------------------------------
-- Utils
-- | executes the action, but catches ASyncExceptions (e.g. Heap Overflows, UserInterrupt, ThreadKilled)
abortable :: IO a -> IO (Maybe a)
abortable act = (Just <$> act) `catch` \(e :: AsyncException) -> do
  putStrLn $ displayException e
  return Nothing


timeoutM :: Int -> IO Bool -> IO Bool
timeoutM t act = fromMaybe False <$> timeout t act
