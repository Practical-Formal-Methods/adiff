{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module VDiff.Server.Controller where

import           VDiff.Server.Prelude
import           VDiff.Server.Widgets

import qualified Control.Concurrent.MSemN              as Sema
import qualified Data.Aeson                            as JSON
import qualified Data.Aeson.Text                       as JSON
import qualified Data.ByteString.Lazy                  as LBS
import           Data.FileEmbed
import           Data.List
import qualified Data.Map                              as Map
import           Data.Semigroup
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import qualified Data.Text.Lazy                        as LT
import           Database.Beam
import           Database.Beam.Sqlite
import           Network.Wai.Middleware.StaticEmbedded
import           Numeric
import           VDiff.Data
import           VDiff.Execute
import           VDiff.Persistence
import qualified VDiff.Query2                          as Q2
import           VDiff.Statistics
import           VDiff.Verifier                        (allVerifiers,
                                                        lookupVerifier)


endpoints :: (HasServerEnv env) => ScottyT SrvError (RIO env) ()
endpoints = do
  get "/" getIndex
  get "/overview" getOverview
  get "/program/:hash" getProgram
  get "/findings/" getFindings
  get "/scratchpad" getScratch
  get "/compare/" getCompare
  post "/run-verifier" postRunVerifier

  -- install static middleware
  middleware (static $(embedDir "static"))


getIndex :: (HasDatabase env, HasLogFunc env) => RioActionM env ()
getIndex = do
  statistics <- lift Q2.getStatistics
  verifierNames <- lift Q2.getVerifierNames
  defaultLayout "VDiff " $(shamletFile "templates/index.hamlet")
  where
    linkUnsoundness = "/findings?q=" <> JSON.encodeToLazyText (Q2.Query Q2.SuspicionUnsound Nothing (ConsensusBy defaultWeights))
    linkIncomplete  = "/findings?q=" <> JSON.encodeToLazyText (Q2.Query Q2.SuspicionIncomplete Nothing (ConsensusBy defaultWeights))

-- | shows all runs on one instrumented file
getProgram :: (HasDatabase env, HasLogFunc env) => RioActionM env ()
getProgram = do
  hash <- param "hash"
  (runs_ :: [VerifierRun]) <- lift $ runBeam $ runSelectReturningList $ select $ Q2.runsByHash hash
  let runs = groupRuns runs_
  (Just program) <- lift $ Q2.getProgramByHash hash
  tags <- lift $ Q2.tagsForProgram hash
  defaultLayout ("program: " <> hash) $(shamletFile "templates/program.hamlet")

data VerifierRunAggregate = VerifierRunAggregate
  { raName       :: Text
  , raVerdict    :: Verdict
  , raTime       :: (Double, Double)
  , raMemory     :: (Int, Int)
  , raOccurences :: Int
  }

groupRuns :: [VerifierRun] -> [VerifierRunAggregate]
groupRuns = map aggregate . groupBy sameNameAndVerdict . sortOn verdictAndName
  where
    sameNameAndVerdict r1 r2 = verdictAndName r1 == verdictAndName r2
    aggregate l@(r:rs) = VerifierRunAggregate (r ^. verifierName) (r ^. (result . verdict)) (0,0) (0,0) (length l)
    verdictAndName r = (show (r ^. (result . verdict)), r ^. verifierName)


getFindings :: (HasDatabase env, HasLogFunc env) => RioActionM env ()
getFindings = do
  verifierNames <- lift Q2.getVerifierNames
  q <- fromMaybe Q2.Everything <$> paramJsonMay "q"
  lift $ logInfo $ "processing query: " <> display (tshow q)
  (page :: Integer) <- param "page" `rescue` const (return 1)
  let pageSize = 30
  let offset = (page - 1) * 30
  countFindings <- lift $ Q2.executeQueryCount q
  findings <- lift $ Q2.executeQuery pageSize offset q
  pg <- mkPaginationWidget 30 countFindings (fromIntegral page)
  defaultLayout "Findings" $(shamletFile "templates/findings.hamlet")



getScratch ::  (HasDatabase env, HasLogFunc env) => RioActionM env ()
getScratch = do
  -- if this param is set, load the source from the database
  pid <- paramMay "program"
  code <- case pid of
    Nothing -> return "int main(){...}" -- TODO add default stuff
    Just pid -> do
      (Just p) <- lift $ Q2.getProgramByHash pid
      return $ p ^. source

  defaultLayout "Scratchpad" $(shamletFile "templates/scratchpad.hamlet")

postRunVerifier :: (HasLogFunc env, HasSemaphore env) => RioActionM env ()
postRunVerifier = do
  source <- param "source"
  timeout <- fromSeconds . read <$> param "timeout"
  vn <- param "verifier"
  sema <- lift $ view semaphore
  -- execute verifier here inside a semaphore-protected area
  res <- lift $ with' sema 1 $
    executeVerifierInDocker (defaultVerifierResources timeout) vn [] source

  html $ LT.fromStrict $ tshow (res ^. verdict)

getOverview :: (HasDatabase env, HasOverviewCache env, HasLogFunc env) => RioActionM env ()
getOverview = do
  -- this is quite ugly
  consensusAlgorithm <- (fromMaybe SimpleBinaryMajority) <$> paramMay "consensusAlgorithm"
  let consensusModel = Weights consensusAlgorithm defaultWeightsMap
  (soundnessTbl, completenessTbl, recallTbl, precisionTbl) <- do
    ref <- lift $ view overviewCache
    m <- liftIO (readIORef ref)
    case Map.lookup consensusModel m of
      Just y  -> return y
      Nothing -> do
        y <- lift $ (,,,) <$> overPairsWithConsensus consensusModel relativeSoundness
                          <*> overPairsWithConsensus consensusModel relativeCompleteness
                          <*> overPairsWithConsensus consensusModel relativeRecall
                          <*> overPairsWithConsensus consensusModel relativePrecision
        modifyIORef ref (Map.insert consensusModel y)
        return y

  defaultLayout "Overview" $(shamletFile "templates/overview.hamlet")
  where
    mkUnsoundnessLink, mkIncompletenessLink :: Relatee -> Relatee -> LT.Text
    mkUnsoundnessLink v1 v2               = "/findings?q=" <> JSON.encodeToLazyText (Q2.Query Q2.SuspicionUnsound  (Just v1) v2)
    mkIncompletenessLink v1 v2            = "/findings?q=" <> JSON.encodeToLazyText (Q2.Query Q2.SuspicionIncomplete (Just v1) v2)
    mkUnsoundnessWithUnknownLink v1 v2    = "/findings?q=" <> JSON.encodeToLazyText (Q2.ByVerdict [(v1, [Unknown, Unsat]), (v2, [Sat])])
    mkIncompletenessWithUnknownLink v1 v2 = "/findings?q=" <> JSON.encodeToLazyText (Q2.ByVerdict [(v1, [Unknown, Sat]), (v2, [Unsat])])
    selectIfEqual x y = if x == y then ("selected" :: Text) else ""


getCompare :: (HasDatabase env, HasOverviewCache env, HasLogFunc env) => RioActionM env ()
getCompare = do
  (mv1 :: Maybe VerifierName) <- paramMay "v1"
  (mv2 :: Maybe VerifierName) <- paramMay "v2"
  vns <- lift Q2.getVerifierNames

  mTableWidget <- case (mv1, mv2) of
    (Just v1, Just v2) ->  do
      (bigN, tbl) <- lift $ getBinaryComparison (RelateName v1) (RelateName v2)
      Just <$> mkBinaryComparisonTableWidget v1 v2 bigN tbl
    otherwise          -> return Nothing

  defaultLayout "Compare" $(shamletFile "templates/compare.hamlet")
  where
    selectIfEqual :: Maybe VerifierName -> VerifierName -> Text
    selectIfEqual (Just v1) v2 = if v1 == v2 then "selected" else ""
    selectIfEqual _ _          = ""

--------------------------------------------------------------------------------
with' :: (Integral i, MonadUnliftIO m, MonadIO m) => Sema.MSemN i -> i -> m a -> m a
with' sem i a = do
  env <- askUnliftIO
  liftIO $ Sema.with sem i $ unliftIO env a

instance Parsable ConsensusAlgorithm where
  parseParam t = case readMay (LT.unpack t) of
                   Nothing -> Left "cannot parse"
                   Just c  -> Right c

consensusAlgorithms
  = [ AbsoluteMajority
    , SimpleBinaryMajority
    , SimpleTernaryMajority
    , SimpleMajorityUnknownAs Sat
    , SimpleBinaryMajorityWithThreshold 2
    , SimpleBinaryMajorityWithThreshold 3
    , SimpleBinaryMajorityWithThreshold 4
    , SimpleBinaryMajorityWithThreshold 5
    ]
