{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Main where

import           VDiff.Prelude

import           Control.Lens.Operators                 hiding ((^.))
import           Control.Lens.TH
import qualified Data.List.Key                          as K
import qualified Data.Text.IO                           as Text
import qualified Database.SQLite.Simple                 as SQL
import           Graphics.Rendering.Chart.Backend.Cairo
import qualified Graphics.Rendering.Chart.Easy          as Chart
import qualified Prelude                                as P
import           RIO.List
import           System.Exit
import           System.IO
import qualified Text.PrettyPrint.Tabulate              as T

import           VDiff.Arguments                        hiding (command)
import           VDiff.Data
import           VDiff.Persistence                      (withDiffDB)
import qualified VDiff.Query                            as Q


data ViewCommand = Stats
                 | List Q.Query -- ^ list all findings
                 | Count Q.Query -- ^ count findings
                 | DistributionPerFile Q.Query -- ^ show the distribution
                 | GetProgram String
                 | Runs String
                 | TimeMemoryGraph FilePath
                 | Merge [FilePath]
                 deriving (Show, Eq)

data ViewParameters = ViewParameters
  { _command    :: ViewCommand
  }
makeFieldsNoPrefix ''ViewParameters


infos = (progDesc "viewer for vdiff")

main :: IO ()
main = do
  runVDiffApp viewParameters infos $ \vp -> do
    Q.updateIndices
    executeView (vp ^. command)

instance T.CellValueFormatter Text

executeView :: (HasMainEnv env) => ViewCommand -> RIO env ()
executeView Stats = do
  stats <- Q.stats
  liftIO $  T.printTable stats
  return ()
executeView (List q) = do
  rs <- Q.executeQuery q
  liftIO $ T.printTable rs
executeView (Count q) = do
  rs <- Q.executeQuery q
  liftIO $ print $ length rs
executeView (DistributionPerFile q) = do
  rs <- Q.executeQuery q
  let grouped = reverse $ sortOn length $ K.group Q._originalFn $ sortOn Q._originalFn $ rs
  let counts = map (\fs -> (Q._originalFn (P.head fs), length fs )) grouped
  liftIO $ T.printTable counts

executeView (GetProgram hsh) = do
  p <- Q.programByHash hsh
  liftIO $ case p of
    Just p' -> Text.putStr (p' ^. source)
    Nothing -> do
      hPutStrLn stderr $ "could not find program with hash: " <> hsh
      exitFailure
executeView (TimeMemoryGraph outp) = do
  d <- Q.allRuns
  liftIO $ renderPoints (cleanData d) outp

executeView (Runs hsh) = do
  runs <- Q.allRunsByHash hsh
  liftIO $ T.printTable runs

executeView (Merge files) = do
  mainConn <- view databaseL

  -- loop over the given databases
  liftIO $
    forM_ files $ \f -> do
      putStrLn $ "merging file " ++ f
      SQL.withConnection f $ \conn -> do

        putStrLn "merging programs"
        SQL.fold_ conn "SELECT code_hash,origin,content FROM programs" () $ \_ prg -> do
          let _ = prg :: (Text,Text,Text)
          SQL.execute mainConn "INSERT OR IGNORE INTO programs(code_hash,origin,content) VALUES(?,?,?)" prg
          putStr "."
        putStrLn ""

        putStrLn "merging runs"
        SQL.fold_ conn "SELECT run_id,verifier_name,result,time,memory,code_hash FROM runs;" () $ \_ run -> do
          let (_ :: Integer, vn :: Text, result :: Text, time :: Maybe Float, mem :: Maybe Integer, hsh :: Text ) = run
              runWithoutId = (vn, result, time, mem, hsh)
          SQL.execute mainConn "INSERT OR IGNORE INTO runs(verifier_name,result,time,memory,code_hash) VALUES(?,?,?,?,?)" runWithoutId
          putStr "."
        putStrLn ""



viewParameters :: Parser ViewParameters
viewParameters = ViewParameters <$> viewCommand
  where
    viewCommand = asum [ listCmd
                       , countCmd
                       , programCmd
                       , correlationCmd
                       , mergeCmd
                       , runsCmd
                       , distributionCmd
                       , statCmd ]

statCmd,listCmd,countCmd,programCmd,correlationCmd,mergeCmd,runsCmd :: Parser ViewCommand
statCmd = switch options $> Stats
  where options = mconcat [ long "stat"
                          , short 's'
                          , help "print basic statistics about this database"
                          ]

listCmd = switch options $> List <*> query
  where options = mconcat [ long "list"
                          , short 'l'
                          , help "prints a list"
                          ]

countCmd = switch options $> Count <*> query
  where options = mconcat [ long "count"
                          , help "returns the number of findings"
                          ]

programCmd = GetProgram <$> option str options
  where options = mconcat [ long "hash"
                          , help "returns the source code of a program with the given hash"
                          , metavar "HASH"
                          ]

correlationCmd = switch options $> TimeMemoryGraph <*> someFile
  where options = mconcat [ long "correlation"
                          , help "generates a scatter plot of memory consumption and runtime" ]

mergeCmd = switch options $>  Merge <*> many someFile
  where options = mconcat [ long "merge"
                          , help "merge database files into one"]

runsCmd = Runs <$> option str options
  where options = mconcat [ long "runs"
                          , help "shows all runs using the program with the given hash"
                          , metavar "HASH"
                          ]

distributionCmd = switch options $> DistributionPerFile <*> query
  where options = mconcat [ long "per-file"
                          , help "shows the number of findings per file" ]

query :: Parser Q.Query
query = incmpl <|> unsound <|> disagreement <|> unsoundKleeCbmc <|>  unsoundKleeCbmcSmack
  where incmpl = switch (long "incomplete") $> Q.Incomplete
        unsound = switch (long "unsound") $> Q.Unsound
        disagreement = switch (long "disagreement") $> Q.Disagreement
        unsoundKleeCbmc = switch (long "unsound-klee-cbmc") $> Q.UnsoundAccordingToKleeOrCbmc
        unsoundKleeCbmcSmack = switch (long "unsound-klee-cbmc-smack") $> Q.UnsoundAccordingToKleeOrCbmcOrSmack



--------------------------------------------------------------------------------
-- for the time / memory chart
--------------------------------------------------------------------------------

data DataLine = DataLine
  { verifier    :: String
  , proportions :: [(Double, Int)]
  }

cleanData :: [(String, String, Maybe Double, Maybe Int)] -> [DataLine]
cleanData runs =
  let terminated = [(s,t, m `div` 1024 ) | (s, _, Just t, Just m) <- runs] -- memory in MiB
      grouped = groupBy (\(x,_,_) (x',_,_) -> x == x') (sortOn fst3 terminated)
      tagged = [DataLine (fst3 (P.head g)) (e23 g) | g <- grouped]
  in tagged
  where
    e23 g = [(y,z) | (_,y,z) <- g]

fst3 (x,_,_) = x

clrs :: [Chart.AlphaColour Double]
clrs = map Chart.opaque [ Chart.red
                        , Chart.blue
                        , Chart.green
                        , Chart.yellow
                        , Chart.black
                        , Chart.brown
                        , Chart.coral
                        ]

renderPoints :: [DataLine] -> FilePath -> IO ()
renderPoints lns outp = do
  let fileOptions = (fo_format .~ SVG) Chart.def
  toFile fileOptions outp $ do
    Chart.layout_title .= "resident memory (MiB) / Time (s)"
    forM_ (zip lns (cycle clrs)) $ \(ln, c) -> do
      Chart.setColors [c]
      Chart.plot (Chart.points (verifier ln) (proportions ln))
