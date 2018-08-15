{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{- signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
   Sometimes it is straight up impossible to write the types down because of ambiguous types .-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}


{- functions that start with get run in the RIO monad. -}
module VDiff.Query2 where

import           Data.Aeson
import qualified Data.List                                as L
import qualified Data.Map.Strict                          as Map
import           Data.Pool
import qualified Data.Text                                as T
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Query
import           Database.Beam.Sqlite                     hiding (runInsertReturningList)
import qualified Database.SQLite.Simple                   as SQL
import           Numeric
import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Prelude                            hiding (Disagreement)


-- a generalized query interface:
data Query
  = Query Suspicion (Maybe Relatee) Relatee
  | ByVerdict [(Relatee, [Verdict])]
  | Disagreement
  | Ties
  | Everything
  deriving (Show, Generic, ToJSON, FromJSON)

-- | returns all weights used in a query
weightsOfQuery :: Query -> [Weights]
weightsOfQuery q =
  case q of
    Query _ mr r -> catMaybes [mr >>= weightsRelatee, weightsRelatee r]
    ByVerdict l  -> catMaybes [ weightsRelatee r | (r,_) <- l ]
    Disagreement -> []
    Ties         -> []
    Everything   -> []

weightsRelatee :: Relatee -> Maybe Weights
weightsRelatee (ConsensusBy w) = Just w
weightsRelatee (RelateName _)  = Nothing

data Suspicion
  = SuspicionIncomplete
  | SuspicionUnsound
  deriving (Show, Generic, ToJSON, FromJSON)


data Finding
  = Finding
  { _findingProgramId      :: ProgramId
  , _findingOrigin         :: Text
  , _findingCountSat       :: Int
  , _findingCountUnsat     :: Int
  , _findingSatVerifiers   :: [VerifierName]
  , _findingUnsatVerifiers :: [VerifierName]
  } deriving (Show, Eq, Ord)

executeQuery :: (HasDatabase env, HasLogFunc env) => Integer -> Integer ->  Query -> RIO env [Finding]
executeQuery limit offset q = do
  forM_ (weightsOfQuery q) ensureConsensusExists

  fs <- runBeam $ runSelectReturningList $ select $ limit_ limit $ offset_ offset $ compileQuery q
  return [ Finding pid origin satN unsatN (toVerifierL sats) (toVerifierL unsats)
         | (pid, origin, satN, unsatN , sats, unsats) <- fs
         ]
  where
    toVerifierL =  sort . L.nub . filter (/= "") . T.split (`elem` [' ', ',']) . fromMaybe ""

ensureConsensusExists :: (HasDatabase env, HasLogFunc env) => Weights -> RIO env ()
ensureConsensusExists w = do
  ex <- weightExists w
  unless ex $ calculateConsensus w
  where
    weightExists w = runBeam $ do
      (Just (exists :: Bool)) <- runSelectReturningOne $ select $ return $ exists_ $
        filter_ (\c -> (c ^. consensusWeights) ==. val_ w) $ all_ (vdiffDb ^. tmpConsensus)
      return exists

compileQuery :: Query -> Q _ _ ctx _
compileQuery= \case
  (Query SuspicionIncomplete (Just r1) r2) -> findingsByVerdicts [(r1, [Sat]), (r2, [Unsat])]
  (Query SuspicionIncomplete Nothing r2)   -> filter_ (\(_,_,satCount,_,_,_) -> satCount >. 0) $ findingsByVerdicts [(r2, [Unsat])]
  (Query SuspicionUnsound (Just r1) r2)    -> findingsByVerdicts [(r1, [Unsat]), (r2, [Sat])]
  (Query SuspicionUnsound Nothing r2)      -> filter_ (\(_,_,_, unsatCount,_,_) -> unsatCount >. 0) $ findingsByVerdicts [(r2, [Sat])]
  (ByVerdict vrds) -> findingsByVerdicts vrds
  Everything -> findingsByVerdicts []


executeQueryCount :: (HasDatabase env, HasLogFunc env) => Query -> RIO env Int
executeQueryCount q = do
  forM_ (weightsOfQuery q) ensureConsensusExists
  (Just n) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ compileQuery q
  return n;

-- | like 'executeQuery', but without pagination
executeQuerySimple :: (HasDatabase env, HasLogFunc env) => Query -> RIO env [Finding]
executeQuerySimple = executeQuery 10000000000 0

getStatistics :: (HasDatabase env, HasLogFunc env) => RIO env [(Text, Text)]
getStatistics = runBeam $ do
  (Just runsN)  <- runSelectReturningOne $ select runCount
  (Just programsN) <- runSelectReturningOne $ select programCount
  (Just (distinctOriginsN :: Int)) <- runSelectReturningOne $ select distinctOrigins
  return [ ("runs", tshow runsN)
         , ("programs", tshow programsN)
         , ("original files used", tshow distinctOriginsN)
         ]
  where
    runCount        = aggregate_ (const countAll_) allRuns_
    programCount    = aggregate_ (const countAll_) allPrograms_
    distinctOrigins = aggregate_ (\c -> countOver_ distinctInGroup_ (c ^. origin)) allPrograms_


allRuns :: (HasDatabase env, HasLogFunc env) => RIO env [VerifierRun]
allRuns = runBeam $ runSelectReturningList $ select allRuns_

allPrograms :: (HasDatabase env, HasLogFunc env) => RIO env [Program]
allPrograms = runBeam $ runSelectReturningList $ select $ all_ (vdiffDb ^. programs)

getProgramByHash :: (HasDatabase env, HasLogFunc env) => Text -> RIO env (Maybe Program)
getProgramByHash hsh = runBeam $ runSelectReturningOne $ lookup_ (vdiffDb ^. programs) (toProgramId hsh)


runsByHash :: Text -> Q _ _ _ _
runsByHash hsh = filter_ (\r -> (r ^. program) ==. val_ hsh) allRuns_

runsByHashR :: (HasDatabase env, HasLogFunc env) => Text -> RIO env [VerifierRun]
runsByHashR hsh = runBeam $ runSelectReturningList $ select $ runsByHash hsh

getRunById :: (HasDatabase env, HasLogFunc env) => VerifierRunId -> RIO env (Maybe VerifierRun)
getRunById i = runBeam $ runSelectReturningOne $ lookup_ (vdiffDb ^. runs) i

-- | his currently produces two queries (one to check if the program already exists.) This could be avoided.
storeProgram :: (HasDatabase env, HasLogFunc env) => Program -> RIO env ()
storeProgram p = do
  exists <- isJust <$> getProgramByHash (p ^. hash)
  unless exists $ runBeam $ runInsert $ insert (vdiffDb ^. programs) $ insertValues [p]

storeRun :: (HasDatabase env, HasLogFunc env) => VerifierRun -> RIO env ()
storeRun r = runBeam $ runInsert $ insert (vdiffDb ^. runs) $ insertValues [r]

storeRunFreshId :: (HasDatabase env, HasLogFunc env) => VerifierRun -> RIO env VerifierRun
storeRunFreshId r = do
  [run] <- runBeam $ runInsertReturningList (vdiffDb ^. runs) $ insertExpressions
              [VerifierRun default_ (val_ (r ^. verifierName)) (val_ $ toProgramId (r ^. program)) (val_ (r ^. result)) (val_ (r ^. iteration))]
  return run

tagRun :: (HasDatabase env, HasLogFunc env) => VerifierRunId -> [(TagName, TagValue)] -> RIO env ()
tagRun rid pairs = runBeam $ runInsert $ insert (vdiffDb ^. tags) $ insertExpressions $
                     map (\(k,v) -> Tag default_ (just_ (val_ rid)) nothing_ (val_ k) (val_ v)) pairs

tagProgram :: (HasDatabase env, HasLogFunc env) => ProgramId -> [(TagName, TagValue)] -> RIO env ()
tagProgram hsh pairs = runBeam $ runInsert $ insert (vdiffDb ^. tags) $ insertExpressions $
                     map (\(k,v) -> Tag default_ nothing_ (just_ (val_ hsh)) (val_ k) (val_ v)) pairs


lookupRun :: (HasDatabase env, HasLogFunc env) => Text -> Text -> RIO env (Maybe VerifierRun)
lookupRun vn hs = runBeam $ runSelectReturningOne $ select s
  where
    s = filter_ (\r -> (r ^. program) ==. val_ hs &&. r ^. verifierName ==. val_ vn) $ all_ (vdiffDb ^. runs)


-- | morally produces [Finding]
findingsByVerdicts :: [(Relatee, [Verdict])] -> Q _ VDiffDb ctx _
findingsByVerdicts rvs = agg $ do
  -- the program
  p <- programByVerdicts rvs
  -- join with all runs that say "Sat"
  sats <- leftJoin_ (filter_ (\r -> (r ^. resultVerdict) ==. val_ Sat) $ all_ $ vdiffDb ^. runs)
            (\r -> (r ^. program) ==. (p ^. hash) )

  -- join with all runs that say "Unsat"
  unsats <- leftJoin_ (filter_ (\r -> (r ^. resultVerdict) ==. val_ Unsat) $ all_ $ vdiffDb ^. runs)
            (\r -> (r ^. program) ==. (p ^. hash) )

  return (pk p, p ^. origin, sats ^. verifierName, unsats ^. verifierName)
  where
    consensusByWeight w = filter_ (\c -> (c ^. consensusWeights) ==. val_ w) (all_ $ vdiffDb ^. tmpConsensus)
    agg = aggregate_ (\(pid, origin, sats, unsats) -> ( group_ pid
                                                      , group_ origin
                                                      , countDistinct_ sats
                                                      , countDistinct_ unsats
                                                      , concatComma_ sats
                                                      , concatComma_ unsats))

    concatComma_ :: (IsCustomSqlSyntax syntax) => QGenExpr QValueContext _ _ (Maybe Text) -> QGenExpr QAggregateContext syntax s (Maybe Text)
    concatComma_ = customExpr_ (\bytes -> "group_concat(" <> bytes <> ")")

    countDistinct_ :: (IsCustomSqlSyntax syntax) => QGenExpr QValueContext _ _ _ -> QGenExpr QAggregateContext syntax s Int
    countDistinct_ = customExpr_ (\bytes -> "COUNT(DISTINCT " <> bytes <> ")")



calculateConsensus:: (HasDatabase env, HasLogFunc env) => Weights -> RIO env ()
calculateConsensus w@(Weights consensusAlgorithm weights) = do
  -- runBeam $ runDelete $ delete (vdiffDb ^. tmpConsensus) (const $ val_ True)
  let bs = 100 :: Int
  logInfo $ "updating consensus table (using batch size " <> display bs <> ")"

  (Just numPrograms ) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. programs)
  logInfo $ "total number of programs in db: " <> display numPrograms
  vns <- getVerifierNames

  forM_ [0.. numPrograms `div` bs + 1] $ \i -> do
    let percentage = Numeric.showFFloat (Just 2) ((fromIntegral i / (fromIntegral numPrograms / fromIntegral bs + 1)) * 100) "" :: String
    logSticky $ "inserted consensuses for " <> display (i * bs) <> " of " <> display numPrograms <> " programs (" <> display (T.pack percentage) <> "%)"

    runBeam $ runInsert $ insert (vdiffDb ^. tmpConsensus) $ insertFrom $ do
      p <- limit_ (fromIntegral bs) $ offset_ (fromIntegral $ i * bs) $ all_ (vdiffDb ^. programs)

      satN     <- countVerdict (p ^. hash) weights Sat
      unsatN   <- countVerdict (p ^. hash) weights Unsat
      unknownN <- countVerdict (p ^. hash) weights Unknown

      let vrd = case consensusAlgorithm of
            AbsoluteMajority ->
              if_ [ (satN >. unsatN + unknownN) `then_` val_ Sat
                  , (unsatN >. satN + unknownN) `then_` val_ Unsat
                  ] (else_ (val_ Unknown))
            SimpleBinaryMajority ->
              if_ [ (satN >. unsatN) `then_` val_ Sat
                  , (satN <. unsatN) `then_` val_ Unsat
                  ] (else_ (val_ Unknown))
            SimpleTernaryMajority ->
              if_ [ (satN >. unsatN &&. satN >. unknownN) `then_` val_ Sat
                  , (unsatN >. satN &&. unsatN >. unknownN) `then_` val_ Unsat
                  ] (else_ (val_ Unknown))
            SimpleMajorityUnknownAs Sat ->
              if_ [ (satN + unknownN >. unsatN) `then_` val_ Sat
                  , (satN + unknownN <. unsatN) `then_` val_ Unsat
                  ] (else_ (val_ Unknown))
            SimpleMajorityUnknownAs Unsat ->
              if_ [ (unsatN + unknownN >. satN) `then_` val_ Unsat
                  , (unsatN + unknownN <. satN) `then_` val_ Sat
                  ] (else_ (val_ Unknown))
            SimpleMajorityUnknownAs Unknown -> -- the same as SimpleTernaryMajority
              if_ [ (satN >. unsatN &&. satN >. unknownN) `then_` val_ Sat
                  , (unsatN >. satN &&. unsatN >. unknownN) `then_` val_ Unsat
                  ] (else_ (val_ Unknown))
            SimpleBinaryMajorityWithThreshold threshold ->
              if_ [ (satN >. unsatN &&. satN >=. val_ threshold)   `then_` val_ Sat
                  , (satN <. unsatN &&. unsatN >=. val_ threshold) `then_` val_ Unsat
                  ] (else_ (val_ Unknown))

      return $ Consensus default_  (pk p) (val_ w) vrd
  logStickyDone "updating consensus table completed"
    where
      runsByVerifierAndVerdict :: VerifierName -> Verdict -> Q _ _ _ (VerifierRunT _)
      runsByVerifierAndVerdict vn vrd = filter_ (\r -> (((r ^. verifierName) ==. val_ vn) &&. (r ^. resultVerdict) ==. val_ vrd)) allRuns_

      countVerdict :: _ -> [(VerifierName, Int)] -> Verdict -> _
      countVerdict p weights vrd = do
        y <- forM weights $ \(vn, weighted) ->
          if weighted == 0
            then return 0
            else do
              x <- leftJoin_ (runsByVerifierAndVerdict vn vrd) (\r -> (r ^. program) ==. p)
              return $ as_ @Int $ maybe_ 0 (const $ fromIntegral weighted) x
        return (sum y)

cleanUp = do
  removeRedundantRuns
  removeSubsumedUnknowns

cleanTemporary = do
  logInfo "deleting tmpConsensus table"
  runBeam $ runDelete $ delete (vdiffDb ^. tmpConsensus) (const $ val_ True)

-- | This currently uses a hard-coded SQL query as the query generated by beam is not correc.
-- See https://github.com/tathougies/beam/issues/284
removeSubsumedUnknowns = do
  logInfo "removing superfluous 'Unknown' in runs"
  execute_ "DELETE FROM runs AS r WHERE EXISTS (SELECT * FROM runs AS t WHERE r.verifier_name = t.verifier_name AND r.code_hash = t.code_hash AND  r.result = 'unknown' AND t.result <> 'unknown')"
  -- runBeam $ runDelete $ delete (vdiffDb ^. runs) $ \r -> exists_ $ do
  --   t <- all_ (vdiffDb ^. runs)
  --   guard_ $ (r ^. verifierName) ==. (t ^. verifierName)
  --   guard_ $ (r ^. program) ==. (t ^. program)
  --   guard_ $ ((r ^. resultVerdict) ==. val_ Unknown) &&. ((t ^. resultVerdict) /=. val_ Unknown)
  --   return (t ^. runId)



-- | This currently uses a hard-coded SQL query as the query generated by beam is not correct.
-- See https://github.com/tathougies/beam/issues/284
removeRedundantRuns = do
  logInfo "removing redundant runs"
  execute_ "DELETE FROM runs AS r WHERE EXISTS(SELECT * FROM runs AS t WHERE r.verifier_name= t.verifier_name AND r.run_id > t.run_id AND r.result = t.result AND r.code_hash = t.code_hash);"
  n <- getNumChanges
  logInfo $ "removed " <> display n <> " runs"
  -- runBeam $ runDelete $ delete (vdiffDb ^. runs) $ \r -> exists_ $ do
  -- t <- all_ (vdiffDb ^. runs)
  -- guard_ $ (r ^. verifierName) ==. (t ^. verifierName)
  -- guard_ $ (r ^. runId) >. (t ^. runId)
  -- guard_ $ (r ^. resultVerdict) ==. (t ^. resultVerdict)
  -- guard_ $ (r ^. program) ==. (t ^. program)
  -- return (t ^. runId)

--------------------------------------------------------------------------------
tagsForProgram :: (HasDatabase env, HasLogFunc env) => Text -> RIO env _
tagsForProgram hsh = runBeam $ runSelectReturningList $ select $ project <$> relevantTags
  where
    project r = (r ^. tagName, r ^. tagValue)
    relevantTags = filter_ (\t -> (t ^. taggedProgramId) ==. val_ (Just hsh)) $ all_ (vdiffDb ^. tags)

--------------------------------------------------------------------------------
-- query fragments

allRuns_ :: Q _ _ _ (VerifierRunT (QExpr _ _))
allRuns_      = all_ (vdiffDb ^. runs)

allPrograms_ :: Q _ _ _ (ProgramT (QExpr _ _))
allPrograms_  = all_ (vdiffDb ^. programs)


programByVerdicts :: forall ctx . [(Relatee, [Verdict])] -> Q _ _ ctx (ProgramT _)
programByVerdicts specs = nub_ $ do
  p <- allPrograms_
  forM_ specs $ \(relatee, vrds) ->
    case relatee of
      RelateName vn -> do
        r <- if sort vrds == sort [Unsat, Sat, Unknown]
             then allRuns_
             else filter_ (\r -> (r ^. (result . verdict)) `in_` map val_ vrds) allRuns_
        guard_ $ (r ^. program) ==. p ^. hash
        guard_ $ (r ^. verifierName) ==. val_ vn
      (ConsensusBy w) -> do
        c <- filter_ (\c -> (c ^. consensusWeights) ==. val_ w ) $ all_ (vdiffDb ^. tmpConsensus)
        guard_ $ (c ^. consensusVerdict) `in_` map val_ vrds
        guard_ $ (p ^. hash) ==. (c ^. consensusProgramId)
  return p


getVerifierNames :: (HasDatabase env, HasLogFunc env) => RIO env [VerifierName]
getVerifierNames = runBeam $ runSelectReturningList $ select names
  where
    names = nub_ ((^. verifierName) <$> allRuns_)



getNumChanges :: (HasDatabase env) => RIO env Int
getNumChanges = do
  dbPool <- view databaseL
  liftIO $ withResource dbPool SQL.changes

-- all Template Haskell goes here
makeFieldsNoPrefix ''Finding
