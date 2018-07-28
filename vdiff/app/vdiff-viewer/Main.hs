{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Main where

import           VDiff.Prelude

import           Control.Lens.Operators          hiding ((^.))
import qualified Data.List.Key                   as K
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust)
import           Data.Ord                        (Down (Down))
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Database.SQLite.Simple.Extended as SQL
import           Numeric
import qualified Prelude                         as P
import           RIO.List
import           System.Directory                (makeAbsolute)
import           System.Exit
import           System.IO
import qualified Text.PrettyPrint.Tabulate       as Tab

import           VDiff.Application
import           VDiff.Arguments                 hiding (command)
import           VDiff.Data
import           VDiff.Persistence
import qualified VDiff.Query2                    as Q2
import qualified VDiff.Statistics                as Statistics
import qualified VDiff.Util.Tables               as Tbl
import           VDiff.Verifier


data ViewCommand
  = Stats
  | List Q2.Query -- ^ list all findings
  | Count Q2.Query -- ^ count findings
  | DistributionPerFile Q2.Query -- ^ show the distribution
  | GetProgram Text
  | Runs Text
  | MergeOld [FilePath]
  | MergeOldList FilePath
  | MergeNew [FilePath]
  | Verdicts
  | RelativeInclusion Verdict Bool

newtype ViewParameters
  = ViewParameters { command :: ViewCommand }


infos = progDesc "viewer for vdiff"

main :: IO ()
main = runVDiffApp viewParameters infos $ \vp -> executeView (command vp)

instance Tab.CellValueFormatter Text
instance Tab.CellValueFormatter ProgramId
instance Tab.CellValueFormatter Verdict
instance Tab.CellValueFormatter VerifierResult
instance Tab.CellValueFormatter VerifierRun

instance Tab.Tabulate VerifierResult Tab.ExpandWhenNested
instance Tab.Tabulate VerifierRun Tab.ExpandWhenNested

executeView :: (HasMainEnv env) => ViewCommand -> RIO env ()
executeView Stats = do
  stats <- Q2.stats
  liftIO $  Tab.printTable stats
executeView (List q) = do
  rs <- Q2.executeQuerySimple q
  printFindingsTable rs

executeView (Count q) = do
  n <- Q2.executeQueryCount Q2.QueryFocusEverything q
  liftIO $ print n
executeView (DistributionPerFile q) = do
  rs <- Q2.executeQuerySimple q
  let grouped = reverse $ sortOn length $ K.group snd4 $ sortOn snd4  rs
  let counts = map (\fs -> (snd4  (P.head fs), length fs )) grouped
  liftIO $ Tab.printTableWithFlds flds counts
  where
    snd4 (_,p,_,_) = p
    flds = [ Tab.DFld $ maybe "<unknown>" T.unpack . fst
           , Tab.DFld snd
           ]

executeView (GetProgram hsh) = do
  p <- Q2.programByHash hsh
  liftIO $ case p of
    Just p' -> T.putStr (p' ^. source)
    Nothing -> do
      T.hPutStrLn stderr $ "could not find program with hash: " <> hsh
      exitFailure

executeView (Runs hsh) = do
  runs <- Q2.runsByHashR hsh
  liftIO $ Tab.printTableWithFlds flds runs
  where
   flds = [ Tab.DFld (^. runId)
          , Tab.DFld $ T.unpack . (^. verifierName)
          , Tab.DFld (^. (result . verdict))
          , Tab.DFld (^. iteration)
          ]

executeView (MergeOld files) = mergeFiles files

executeView (MergeOldList file) = do
  files <- lines <$> liftIO (readFile file)
  mergeFiles files

executeView Verdicts = do
  stats <- mapM  (\v -> (v ^. name,) <$>  Statistics.verdicts (Q2.QueryFocus [v ^. name])) allVerifiers
  let tbl = Tbl.table $ Tbl.row ["verifier", "sats", "unsats", "unknown"] : map (Tbl.toRow . (\(x,(a,b,c)) -> (x,a,b,c))) stats
  liftIO $ T.putStr $ Tbl.renderTable tbl

executeView (RelativeInclusion vrd ignoreUnknown) = do
  tbl <- Statistics.overPairs (Statistics.relative vrd ignoreUnknown)
  liftIO $ T.putStr $ Tbl.renderTable $ mkTable tbl
  where
    verifierNames = map (^. name) allVerifiers
    mkTable m = Tbl.table $ headers : [mkRow v1 | v1 <- verifierNames]
      where
        mkRow v1 = Tbl.row $ v1 : [ mkCell v1 v2 | v2 <- verifierNames]
        mkCell  v1 v2 = formatCorrelation $ fromJust $ Map.lookup (v1,v2) m
        headers = Tbl.row $ "  " : verifierNames

mergeFiles :: (HasMainEnv env) => [FilePath] -> RIO env ()
mergeFiles files =
  -- loop over the given databases
  forM_ files $ \f -> do
    logInfo $ "merging file " <> display (tshow f)
    -- collect some data for the tags
    absFn <- liftIO $ makeAbsolute f
    now <- nowISO
    let tags = [ ("metadata.merge.database",  T.pack absFn)
               , ("metadata.merge.date", now)]


    SQL.withConnection f $ \conn -> do
      logInfo "merging programs"

      SQL.fold_ conn "SELECT code_hash,origin,content FROM programs" (0::Int) $ \counter prg -> do
        let (hsh, origin, src) = prg :: (Text,Text,Text)
        let p = Program  hsh origin src :: Program
        Q2.storeProgram p
        Q2.tagProgram (toProgramId hsh) tags
        when (counter `mod` 10 == 0) $ logSticky $ "number of transferred programs: " <> display counter
        return (counter + 1)

      logInfo "merging runs"

      SQL.fold_ conn "SELECT run_id,verifier_name,result,time,memory,code_hash FROM runs;" (0::Int) $ \counter row -> do
        let (_ :: Integer, vn :: Text, vd :: Verdict, time :: Maybe Double, mem :: Maybe Int, hsh :: Text ) = row
        r <- Q2.storeRunFreshId $ VerifierRun (-1) vn (toProgramId hsh) (VerifierResult time mem vd ) (-1)
        Q2.tagRun (primaryKey r) tags
        when (counter `mod` 10 == 0) $ logSticky $ "number of transferred runs: " <> display counter
        return (counter + 1)



viewParameters :: Parser ViewParameters
viewParameters = ViewParameters <$> viewCommand
  where
    viewCommand = asum [ listCmd
                       , countCmd
                       , programCmd
                       , mergeOldCmd
                       , mergeOldListCmd
                       , runsCmd
                       , distributionCmd
                       , statCmd
                       , verdictsCmd
                       , relativeInclusionCmd
                       ]

statCmd, listCmd, countCmd, programCmd, mergeOldCmd, mergeOldListCmd, runsCmd, verdictsCmd  :: Parser ViewCommand
statCmd = switch options $> Stats
  where options = mconcat [ long "stat"
                          , short 's'
                          , help "print basic statistics about this database"
                          ]

listCmd = switch options $> List <*> parseQuery2
  where options = mconcat [ long "list"
                          , short 'l'
                          , help "prints a list"
                          ]

countCmd = switch options $> Count <*> parseQuery2
  where options = mconcat [ long "count"
                          , help "returns the number of findings"
                          ]

programCmd = GetProgram <$> option str options
  where options = mconcat [ long "hash"
                          , help "returns the source code of a program with the given hash"
                          , metavar "HASH"
                          ]

mergeOldCmd = switch options $>  MergeOld <*> many someFile
  where options = mconcat [ long "merge-old"
                          , help "merge database files into one"]


mergeOldListCmd = switch options $>  MergeOldList <*> someFile
  where options = mconcat [ long "merge-old-list"
                          , help "merge database files into one"]

runsCmd = Runs <$> option str options
  where options = mconcat [ long "runs"
                          , help "shows all runs using the program with the given hash"
                          , metavar "HASH"
                          ]

distributionCmd = switch options $> DistributionPerFile <*> parseQuery2
  where options = mconcat [ long "per-file"
                          , help "shows the number of findings per file" ]

verdictsCmd = switch options $> Verdicts
  where options = long "verdicts" <> help "counts the frequency of each verdict for the given verifiers"

relativeInclusionCmd = switch (long "relative-soundness")    $> RelativeInclusion Sat   True <|>
                       switch (long "relative-completeness") $> RelativeInclusion Unsat True <|>
                       switch (long "relative-recall")       $> RelativeInclusion Sat   False <|>
                       switch (long "relative-precision")    $> RelativeInclusion Unsat False


parseFocus = Q2.QueryFocus . map (\(vn,_,_)-> vn) <$> VDiff.Arguments.verifiers

parseQuery2 :: Parser Q2.Query
parseQuery2 = disagreement  <|> everything <|> unsound <|> incomplete
  where
    disagreement = switch (long "disagreement") $> Q2.Disagreement
    everything = switch (long "everything") $> Q2.Everything
    unsound = switch (long "unsound") $> Q2.Query Q2.SuspicionUnsound  <*> parseAccordingTo
    incomplete = switch (long "incomplete") $> Q2.Query Q2.SuspicionIncomplete <*> parseAccordingTo
    parseAccordingTo =  asum [ Q2.AnyOf <$> option listOfVerifierNames (long "according-to-any-of")
                             , Q2.AllOf <$> option listOfVerifierNames (long "according-to-all-of")
                             , pure Q2.Majority
                             ]
    listOfVerifierNames = map T.strip . T.splitOn "," <$>  str


printFindingsTable rs = liftIO $ Tab.printTableWithFlds dspls rs
  where
    dspls = [ Tab.DFld (\(r,_,_,_) -> (r ^. runId))
            , Tab.DFld (\(r,_,_,_) -> T.unpack (r ^. verifierName))
            , Tab.DFld (\(_,p,_,_) -> maybe " - " T.unpack p)
            , Tab.DFld (\(r,_,_,_) -> T.unpack (r ^. program))
            , Tab.DFld (\(_,_,sats,_) -> sats)
            , Tab.DFld (\(_,_,_,unsats) -> unsats)
            ]
