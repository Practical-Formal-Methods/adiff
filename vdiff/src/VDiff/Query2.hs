{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{- signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
   Sometimes it is straight up impossible to write the types down because of ambiguous types .-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}

{- This will become the new type-safe query module after I figured out how to use beam -}
module VDiff.Query2 where

import qualified Data.Text                                as T
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Sqlite                     hiding (runInsertReturningList)
import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Prelude                            hiding (Disagreement)


type Statistics = [(Text, Text)]

-- a generalized query interface:
data Query
  = Query Suspicion AccordingTo
  | Disagreement
  | Everything
  deriving (Show, Read)

data Suspicion
  = SuspicionIncomplete
  | SuspicionUnsound
  deriving (Show, Read)

data AccordingTo
  = AnyOf [VerifierName]
  | AllOf [VerifierName]
  | Majority
  deriving (Show, Read)

newtype QueryFocus = QueryFocus [VerifierName]
  deriving (Show,Read)

-- | at the moment, using 'el cheapo' parsing via read
parseQuery :: Text -> Either Text Query
parseQuery "everything" = Right Everything
parseQuery t = case readMay (T.unpack t) of
                 Nothing -> Left "not parseable"
                 Just q  -> Right q

executeQuery :: (HasDatabase env) => Integer -> Integer -> QueryFocus -> Query -> RIO env [(VerifierRun, Maybe Text, Int, Int)]
executeQuery limit offset qf q = do
  res <- runBeam $ runSelectReturningList $ select $ limit_ limit $ offset_ offset $ execQf qf $ case q of
    Everything                             -> allFindings
    Disagreement                           -> disagreementFindings
    (Query SuspicionIncomplete Majority)   -> incompleteFindings
    (Query SuspicionIncomplete (AnyOf vs)) -> incompleteAccordingToAnyOf vs
    (Query SuspicionUnsound  Majority)     -> unsoundFindings
    (Query SuspicionUnsound (AnyOf vs))    -> unsoundAccordingToAnyOf vs
  return res;

executeQueryCount :: (HasDatabase env) => QueryFocus -> Query -> RIO env Int
executeQueryCount qf q = do
  (Just n) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const (countAll_)) $ execQf qf $ case q of
    Everything                             -> allFindings
    Disagreement                           -> disagreementFindings
    (Query SuspicionIncomplete Majority)   -> incompleteFindings
    (Query SuspicionIncomplete (AnyOf vs)) -> incompleteAccordingToAnyOf vs
    (Query SuspicionUnsound  Majority)     -> unsoundFindings
    (Query SuspicionUnsound (AnyOf vs))    -> unsoundAccordingToAnyOf vs
  return n;

execQf (QueryFocus vs) = filter_ (\(r,_,_,_) -> (r ^. verifierName) `in_` map val_ vs )



stats :: (HasDatabase env) => RIO env Statistics
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


allRuns :: (HasDatabase env) => RIO env [VerifierRun]
allRuns = runBeam $ runSelectReturningList $ select allRuns_

allPrograms :: (HasDatabase env) => RIO env [Program]
allPrograms = runBeam $ runSelectReturningList $ select $ all_ (vdiffDb ^. programs)

programByHash :: (HasDatabase env) => Text -> RIO env (Maybe Program)
programByHash hsh = runBeam $ (vdiffDb ^. programs) `byPK` toProgramId hsh


runsByHash :: Text -> Q _ _ _ _
runsByHash hsh = filter_ (\r -> (r ^. program) ==. val_ hsh) allRuns_

runById :: (HasDatabase env) => Int -> RIO env (Maybe VerifierRun)
runById i = runBeam $ (vdiffDb ^. runs) `byPK` toRunId i

byPK table key = runSelectReturningOne $ select q
  where
    q =  filter_ flt (all_ table)
    flt prg = primaryKey prg ==. val_ key

-- | his currently produces two queries (one to check if the program already exists.) This could be avoided.
storeProgram :: (HasDatabase env) => Program -> RIO env ()
storeProgram p = do
  exists <- isJust <$> programByHash (p ^. hash)
  unless exists $ runBeam $ runInsert $ insert (vdiffDb ^. programs) $ insertValues [p]

storeRun :: (HasDatabase env) => VerifierRun -> RIO env ()
storeRun r = runBeam $ runInsert $ insert (vdiffDb ^. runs) $ insertValues [r]

storeRunFreshId :: (HasDatabase env) => VerifierRun -> RIO env VerifierRun
storeRunFreshId r = do
  [run] <- runBeam $ runInsertReturningList (vdiffDb ^. runs) $ insertExpressions
              [VerifierRun default_ (val_ (r ^. verifierName)) (val_ $ toProgramId (r ^. program)) (val_ (r ^. result)) (val_ (r ^. iteration))]
  return run

tagRun :: HasDatabase env => VerifierRunId -> [(TagName, TagValue)] -> RIO env ()
tagRun rid pairs = runBeam $ runInsert $ insert (vdiffDb ^. tags) $ insertExpressions $
                     map (\(k,v) -> Tag default_ (just_ (val_ rid)) nothing_ (val_ k) (val_ v)) pairs

tagProgram :: HasDatabase env => ProgramId -> [(TagName, TagValue)] -> RIO env ()
tagProgram hsh pairs = runBeam $ runInsert $ insert (vdiffDb ^. tags) $ insertExpressions $
                     map (\(k,v) -> Tag default_ nothing_ (just_ (val_ hsh)) (val_ k) (val_ v)) pairs

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




incompleteFindings, unsoundFindings, disagreementFindings, unsoundKleeCbmc, unsoundKleeCbmcSmack :: forall ctx . Q _ VDiffDb ctx _
incompleteFindings   = filter_ (\(r,_,sat,unsat) -> r ^. (result . verdict) ==. val_ Sat &&. sat <. unsat) allFindings
unsoundFindings      = filter_ (\(r,_,sat,unsat) -> r ^. (result . verdict) ==. val_ Unsat &&. unsat <. sat) allFindings
disagreementFindings = filter_ (\(_,_,sat,unsat) -> sat /=. 0 &&. unsat /=. 0) allFindings



-- allFindings :: Q _ _ _ (VerifierRunT (QExpr _ _) , QExpr _ _ Int, QExpr _ _ Int)
-- allFindings = do
--   r <- allRuns_
--   (_,sats) <- leftJoin_ (runIdWithVerdict Sat) (\(x,_) -> x ==. (r ^. runId))
--   (_,unsats) <- leftJoin_ (runIdWithVerdict Unsat) (\(x,_) -> x ==. (r ^. runId))
--   return (r, maybe_ (val_ 0) id sats, maybe_ (val_ 0) id unsats)

allFindings :: Q _ _ _ _
allFindings = do
  r <- allRuns_
  cts <- filter_ (\c -> (r ^. runId) ==. (c ^. countedRunId)) $ all_ (vdiffDb ^. tmpCounts)
  -- (_,sats) <- leftJoin_ (runIdWithVerdict Sat) (\(x,_) -> x ==. (r ^. runId))
  -- (_,unsats) <- leftJoin_ (runIdWithVerdict Unsat) (\(x,_) -> x ==. (r ^. runId))
  p <- leftJoin_ (all_ $ vdiffDb ^. programs) (\p -> (p ^. hash) ==. (r ^. program) )
  return (r, p ^. origin, cts ^. countedSats, cts ^. countedUnsats)

unsoundKleeCbmc = unsoundAccordingToAnyOf ["klee", "cbmc"]
unsoundKleeCbmcSmack = unsoundAccordingToAnyOf ["klee", "cbmc", "smack"]

unsoundAccordingToAnyOf, incompleteAccordingToAnyOf :: forall ctx . [Text] -> Q _ VDiffDb ctx _
unsoundAccordingToAnyOf vs = do
  (r,origin,sats,unsats) <- filter_ (\(r,_,_,_) -> (r ^. (result . verdict))  ==. val_ Unsat) allFindings
  x <- checkers
  guard_ ( (r ^. program) ==. (x ^. program))
  return (r,origin,sats,unsats)
  where
    checkers = filter_ (\r -> ((r ^. (result . verdict)) ==. val_ Sat) &&.
                             ( (r ^. verifierName) `in_` (map val_ vs))) allRuns_

incompleteAccordingToAnyOf vs = do
  (r,origin,sats,unsats) <- filter_ (\(r,_,_,_) -> (r ^. (result . verdict))  ==. val_ Sat) allFindings
  x <- checkers
  guard_ ( (r ^. program) ==. (x ^. program))
  return (r,origin,sats,unsats)
  where
    checkers = filter_ (\r -> ((r ^. (result . verdict)) ==. val_ Unsat) &&.
                             ( (r ^. verifierName) `in_` (map val_ vs))) allRuns_

-- | This is quite memory-intensive, don't use on big tables.
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


updateCountsTableProgressive :: (HasDatabase env, HasLogFunc env) => RIO env ()
updateCountsTableProgressive = do
  let bs = 100000 :: Int
  logInfo $ "updating counts table (using batches of size " <> display bs <> ")"
  -- delete
  runBeam $ runDelete $ delete (vdiffDb ^. tmpCounts) (const $ val_ True)
  -- find highest id
  (Just maxRunId) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. runs)
  logInfo $ "total number of runs in db: " <> display maxRunId
  -- insert counts in batches
  forM_ [0.. ((maxRunId `div` bs)) + 1] $ \i -> do
    logSticky $ "calculating batch #" <> display i <> " of " <> display (maxRunId `div` bs + 1)
    runBeam $ runInsert $ insert (vdiffDb ^. tmpCounts) $ insertFrom $ do
      r <- filter_ (\r -> (between_ (r ^. runId) (val_ $ i * bs) (val_ $ (i+1) * bs - 1))) $ all_ (vdiffDb ^. runs)
      (_,sats) <- leftJoin_ (runIdWithVerdict Sat) (\(x,_) -> x ==. (r ^. runId))
      (_,unsats) <- leftJoin_ (runIdWithVerdict Unsat) (\(x,_) -> x ==. (r ^. runId))
      return $ Counts default_ (pk r) (maybe_ (val_ 0) id sats) (maybe_(val_ 0) id unsats)
  logStickyDone "updating counts table completed"

updateCountsTableNecessary :: (HasDatabase env) => RIO env Bool
updateCountsTableNecessary = do
  (Just runsN) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. runs)
  (Just countsN) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ all_ (vdiffDb ^. tmpCounts)
  return $ runsN /= countsN



--------------------------------------------------------------------------------
-- query fragments

allRuns_ :: Q _ _ _ (VerifierRunT (QExpr _ _))
allRuns_      = all_ (vdiffDb ^. runs)

allPrograms_ :: Q _ _ _ (ProgramT (QExpr _ _))
allPrograms_  = all_ (vdiffDb ^. programs)
