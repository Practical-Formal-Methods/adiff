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

{- This will become the new type-safe query module after I figured out how to use beam -}
module VDiff.Query2 where

import           Data.Aeson
import qualified Data.List                                as L
import qualified Data.Map.Strict                          as Map
import qualified Data.Text                                as T
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Query
import           Database.Beam.Sqlite                     hiding (runInsertReturningList)
import           Numeric
import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Prelude                            hiding (Disagreement)


type Statistics = [(Text, Text)]

-- a generalized query interface:
data Query
  = Query Suspicion (Maybe Relatee) Relatee
  | Disagreement
  | Ties
  | Everything
  deriving (Show, Generic, ToJSON, FromJSON)

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
  }

executeQuery :: (HasDatabase env, HasLogFunc env) => Integer -> Integer ->  Query -> RIO env [Finding]
executeQuery limit offset q = do
   fs <- runBeam $ runSelectReturningList $ select $ limit_ limit $ offset_ offset $ compileQuery q
   return [ Finding pid origin satN unsatN (toVerifierL sats) (toVerifierL unsats)
          | (pid, origin, satN, unsatN , sats, unsats) <- fs
          ]
  where
    toVerifierL =  sort . L.nub . filter (/= "") . T.split (`elem` [' ', ',']) . fromMaybe ""

compileQuery :: Query -> Q _ _ ctx _
compileQuery= \case
  (Query SuspicionIncomplete (Just r1) r2) -> findingsByVerdicts [(r1, [Sat]), (r2, [Unsat])]
  (Query SuspicionIncomplete Nothing r2)   -> filter_ (\(_,_,satCount,_,_,_) -> satCount >. 0) $ findingsByVerdicts [(r2, [Unsat])]
  (Query SuspicionUnsound (Just r1) r2)    -> findingsByVerdicts [(r1, [Unsat]), (r2, [Sat])]
  (Query SuspicionUnsound Nothing r2)      -> filter_ (\(_,_,_, unsatCount,_,_) -> unsatCount >. 0) $ findingsByVerdicts [(r2, [Sat])]
  Everything -> findingsByVerdicts []


executeQueryCount :: (HasDatabase env, HasLogFunc env) => Query -> RIO env Int
executeQueryCount q = do
  (Just n) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ compileQuery q
  return n;

-- | like 'executeQuery', but without pagination and focusing on everything
executeQuerySimple :: (HasDatabase env, HasLogFunc env) => Query -> RIO env [Finding]
executeQuerySimple = executeQuery 10000000000 0

stats :: (HasDatabase env, HasLogFunc env) => RIO env Statistics
stats = runBeam $ do
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

programByHash :: (HasDatabase env, HasLogFunc env) => Text -> RIO env (Maybe Program)
programByHash hsh = runBeam $ (vdiffDb ^. programs) `byPK` toProgramId hsh


runsByHash :: Text -> Q _ _ _ _
runsByHash hsh = filter_ (\r -> (r ^. program) ==. val_ hsh) allRuns_

runsByHashR :: (HasDatabase env, HasLogFunc env) => Text -> RIO env [VerifierRun]
runsByHashR hsh = runBeam $ runSelectReturningList $ select $ runsByHash hsh

runById :: (HasDatabase env, HasLogFunc env) => Int -> RIO env (Maybe VerifierRun)
runById i = runBeam $ (vdiffDb ^. runs) `byPK` toRunId i

byPK table key = runSelectReturningOne $ select q
  where
    q =  filter_ flt (all_ table)
    flt prg = primaryKey prg ==. val_ key

-- | his currently produces two queries (one to check if the program already exists.) This could be avoided.
storeProgram :: (HasDatabase env, HasLogFunc env) => Program -> RIO env ()
storeProgram p = do
  exists <- isJust <$> programByHash (p ^. hash)
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

-- | returns a table with runIds and the count of the given verdict on the
-- program of the run. 'RunId' that have a count of 0 do not show up here, so make
-- sure that you use a left join and convert the NULL to a 0 in later steps.
-- runIdWithVerdict :: Verdict -> Q _ _ _ _
-- runIdWithVerdict v = aggregateGroupLeft $ filterRightVerdict $ do
--   r <- allRuns_
--   r' <- leftJoin_ allRuns_ (\r' -> (r' ^. program) ==. r ^. program)
--   return (r,r')
--   where
--     aggregateGroupLeft  = aggregate_ (\(r,_) -> (group_ (r ^. runId), countAll_))
--     filterRightVerdict  = filter_ (\(_, r) -> ((r ^. (result . verdict)) ==. val_ (Just v)))


runIdWithVerdict :: Verdict -> Q _ _ _ _
runIdWithVerdict v = aggreg $ do
  r <- allRuns_
  r' <- leftJoin_ (filter_ (\r -> r ^. (result . verdict) ==. val_ v) allRuns_) (\r' -> (r' ^. program) ==. r ^. program)
  return (r ^. runId, r' ^. verifierName)
  where
    aggreg = aggregate_ (\(rid,vn) -> (group_ rid, countOver_ distinctInGroup_ vn))




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



disagreementFindings, unsoundKleeCbmc, unsoundKleeCbmcSmack, tiesFindings :: forall ctx . Q _ VDiffDb ctx _

disagreementFindings = orderBy_ (asc_.diffSats) $ filter_ (\(_,_,sat,unsat) -> sat /=. 0 &&. unsat /=. 0) allFindings
  where
    diffSats (_,_,sat,unsat) =  abs (sat - unsat)
tiesFindings         = orderBy_  ordKey  $ filter_ (\(_,_,sat,unsat) -> sat /=. 0 &&. unsat /=. 0) allFindings
  where
    ordKey (_,_,sats,unsats) = (desc_ (min_ sats unsats), asc_ (abs (sats - unsats)), desc_ (sats + unsats))
    min_ x y = if_ [ ( x <. y) `then_` x] (else_ y)



allFindings :: Q _ _ _ (VerifierRunT _, QExpr _ _ (Maybe Text), QExpr _ _ Int , QExpr _ _ Int )
allFindings = do
  r <- allRuns_
  cts <- filter_ (\c -> (r ^. runId) ==. (c ^. countedRunId)) $ all_ (vdiffDb ^. tmpCounts)
  p <- leftJoin_ (all_ $ vdiffDb ^. programs) (\p -> (p ^. hash) ==. (r ^. program) )
  return (r, p ^. origin, cts ^. countedSats, cts ^. countedUnsats)

unsoundKleeCbmc = unsoundAccordingToAnyOf ["klee", "cbmc"]
unsoundKleeCbmcSmack = unsoundAccordingToAnyOf ["klee", "cbmc", "smack"]

unsoundAccordingToAnyOf, incompleteAccordingToAnyOf :: forall ctx . [VerifierName] -> Q _ VDiffDb ctx _
unsoundAccordingToAnyOf vs = do
  (r,origin,sats,unsats) <- filter_ (\(r,_,_,_) -> (r ^. (result . verdict))  ==. val_ Unsat) allFindings
  x <- checkers
  guard_ ( (r ^. program) ==. (x ^. program))
  return (r,origin,sats,unsats)
  where
    checkers = filter_ (\r -> ((r ^. (result . verdict)) ==. val_ Sat) &&.
                             ( (r ^. verifierName) `in_` map val_ vs)) allRuns_

incompleteAccordingToAnyOf vs = do
  (r,origin,sats,unsats) <- filter_ (\(r,_,_,_) -> (r ^. (result . verdict))  ==. val_ Sat) allFindings
  x <- checkers
  guard_ ( (r ^. program) ==. (x ^. program))
  return (r,origin,sats,unsats)
  where
    checkers = filter_ (\r -> ((r ^. (result . verdict)) ==. val_ Unsat) &&.
                             ( (r ^. verifierName) `in_` map val_ vs)) allRuns_

--- | This is quite memory-intensive, don't use on big tables.
updateCountsTable :: SqliteM ()
updateCountsTable = do
  -- delete all rows
  runDelete $ delete (vdiffDb ^. tmpCounts) (const $ val_ True)
  -- insert new counts
  runInsert $ insert (vdiffDb ^. tmpCounts) $ insertFrom $ do
    r <- all_ (vdiffDb ^. runs)
    (_,sats) <- leftJoin_ (runIdWithVerdict Sat) (\(x,_) -> x ==. (r ^. runId))
    (_,unsats) <- leftJoin_ (runIdWithVerdict Unsat) (\(x,_) -> x ==. (r ^. runId))
    return $ Counts default_ (pk r) (maybe_ (val_ 0) id sats) (maybe_(val_ 0) id unsats)


updateCountsTableProgressive :: (HasDatabase env, HasLogFunc env, HasLogFunc env) => RIO env ()
updateCountsTableProgressive = do
  let bs = 100000 :: Int
  logInfo $ "updating counts table (using batches of size " <> display bs <> ")"
  -- delete
  runBeam $ runDelete $ delete (vdiffDb ^. tmpCounts) (const $ val_ True)
  -- find highest id
  (Just maxRunId) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. runs)
  logInfo $ "total number of runs in db: " <> display maxRunId
  -- insert counts in batches
  forM_ [0.. (maxRunId `div` bs) + 1] $ \i -> do
    logSticky $ "calculating batch #" <> display i <> " of " <> display (maxRunId `div` bs + 1)
    runBeam $ runInsert $ insert (vdiffDb ^. tmpCounts) $ insertFrom $ do
      r <- filter_ (\r -> between_ (r ^. runId) (val_ $ i * bs) (val_ $ (i+1) * bs - 1)) $ all_ (vdiffDb ^. runs)
      (_,sats) <- leftJoin_ (runIdWithVerdict Sat) (\(x,_) -> x ==. (r ^. runId))
      (_,unsats) <- leftJoin_ (runIdWithVerdict Unsat) (\(x,_) -> x ==. (r ^. runId))
      return $ Counts default_ (pk r) (maybe_ (val_ 0) id sats) (maybe_(val_ 0) id unsats)
  logStickyDone "updating counts table completed"

updateCountsTableNecessary :: (HasDatabase env, HasLogFunc env) => RIO env Bool
updateCountsTableNecessary = do
  (Just runsN) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. runs)
  (Just countsN) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. tmpCounts)
  return $ runsN /= countsN



-- updateConsensus' :: (HasDatabase env, HasLogFunc env) => RIO env ()
-- updateConsensus' = do
--   vns <- verifierNames
--   -- this is the raw structure
--   -- let (m :: Map (ProgramId, VerifierName) [Verdict]) = undefined
--   runs <- runBeam $ runSelectReturningList $ select $ all_ (vdiffDb ^. runs)
--   let m = collect runs

--   -- then we map a decision procedure (e.g. Sat/Unsat > Unknown, then use majority)
--   let m' = fmap decide m :: Map (Text, VerifierName) Verdict
--   -- then we use the given weights to calcule
--   let (m'' :: Map ProgramId Verdict) = []
--   undefined
--   where
--     collect :: [VerifierRun] -> Map (Text, VerifierName) [Verdict]
--     collect = foldl' (\m r -> Map.insertWith (++) (r ^. program, r ^. verifierName) [r ^. (result . verdict)] m) Map.empty

--     decide :: [Verdict] -> Verdict
--     decide vrds =
--       let (sats, unsats) = partition (== Sat) $ filter (/= Unknown) vrds
--           satN           = length sats
--           unsatN         = length unsats
--       in if | satN >  unsatN -> Sat
--             | satN < unsatN -> Unsat
--             | otherwise -> Unknown

--     calculateConsensus :: Map (Text, VerifierName) Verdict -> [VerifierName] -> Weights -> [(Text, Verdict)]
--     calculateConsensus m vns w = undefined


-- | Problem: A left join can still increase the number of results if there is more than one matching on the right side!
updateConsensus :: (HasDatabase env, HasLogFunc env) => RIO env ()
updateConsensus = do
  runBeam $ runDelete $ delete (vdiffDb ^. tmpConsensus) (const $ val_ True)
  let bs = 100 :: Int
  logInfo $ "updating consensus table (using batch size " <> display bs <> ")"

  (Just numPrograms ) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. programs)
  logInfo $ "total number of programs in db: " <> display numPrograms
  vns <- verifierNames
  let weights = defaultWeights

  forM_ [0.. numPrograms `div` bs + 1] $ \i -> do
    let percentage = Numeric.showFFloat (Just 2) ((fromIntegral i / (fromIntegral numPrograms / fromIntegral bs + 1)) * 100) "" :: String
    logSticky $ "inserted consensuses for " <> display (i * bs) <> " of " <> display numPrograms <> " programs (" <> display (T.pack percentage) <> "%)"

    runBeam $ runInsert $ insert (vdiffDb ^. tmpConsensus) $ insertFrom $ do
      p <- limit_ (fromIntegral bs) $ offset_ (fromIntegral $ i * bs) $ all_ (vdiffDb ^. programs)


      -- positive score means sat majority, negative score means unsat majority
      score <- sum <$> mapM (weightedCount weights p) vns

      let vrd = if_ [ (score >. 0) `then_` val_ Sat
                    , (score <. 0) `then_` val_ Unsat
                    ] (else_ (val_ Unknown))

      return $ Consensus default_  (pk p) (val_ weights) vrd
  logStickyDone "updating consensus table completed"
    where
      runsByVerifier vn = filter_ (\r -> (r ^. verifierName) ==. val_ vn) allRuns_

      -- weightedCount :: _ -> VerifierName -> Q _ _ _ _
      weightedCount weights p vn = do
            let weighted = weightF weights vn
            x <- leftJoin_ (runsByVerifier vn) (\r -> (r ^. program) ==. (p ^. hash))
            return $ maybe_ 0 (\r -> if_ [ (r ^. (result . verdict) ==. val_ Sat) `then_` val_ weighted
                                         , (r ^. (result . verdict) ==. val_ Unsat) `then_` val_ (-weighted)
                                         ] (else_ 0)
                                ) x


updateConsensusTableNecessary :: (HasDatabase env, HasLogFunc env) => RIO env Bool
updateConsensusTableNecessary = do
  (Just programN) <-     runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. programs)
  (Just consensusesN) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. tmpConsensus)
  logInfo $ "number of programs: " <> display programN
  logInfo $ "number of entries in consensus table: " <> display consensusesN
  return $ programN /= consensusesN


cleanUp = do
  logInfo "removing redundant runs"
  removeRedundantRuns
  -- logInfo "removing superfluous 'Unknown' in runs"
  -- removeSuperfluousUnknowns

removeSuperfluousUnknowns = runBeam $ runDelete $ delete (vdiffDb ^. runs) $ \r -> exists_ $ do
  t <- all_ (vdiffDb ^. runs)
  guard_ $ (r ^. verifierName) ==. (t ^. verifierName)
  guard_ $ (r ^. program) ==. (t ^. program)
  guard_ $ ((r ^. resultVerdict) ==. val_ Unknown) &&. ((t ^. resultVerdict) /=. val_ Unknown)
  return (t ^. runId)


removeRedundantRuns = do
  execute_ "DELETE FROM runs AS r WHERE EXISTS(SELECT * FROM runs AS t WHERE r.verifier_name= t.verifier_name AND r.run_id > t.run_id AND r.result = t.result AND r.code_hash = t.code_hash);"
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

tagsForRun :: VerifierRunId -> RIO env [(Text,Text)]
tagsForRun = undefined

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

resultVerdict = result . verdict

verifierNames :: (HasDatabase env, HasLogFunc env) => RIO env [VerifierName]
verifierNames = runBeam $ runSelectReturningList $ select names
  where
    names = nub_ ((^. verifierName) <$> allRuns_)

makeFieldsNoPrefix ''Finding
