{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{- signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
   Sometimes it is straight up impossible to write the types down because of ambiguous types .-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}

{- This will become the new type-safe query module after I figured out how to use beam -}
module VDiff.Query2 where

import           VDiff.Prelude                            hiding (Disagreement)

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           VDiff.Data
import           VDiff.Persistence



type Statistics = [(Text, Text)]

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
runIdWithVerdict :: Verdict -> Q _ _ _ _
runIdWithVerdict v = aggregateGroupLeft $ filterRightVerdict $ do
  r <- allRuns_
  r' <- leftJoin_ allRuns_ (\r' -> (r' ^. program) ==. r ^. program)
  return (r,r')
  where
    aggregateGroupLeft  = aggregate_ (\(r,_) -> (group_ (r ^. runId), countAll_))
    filterRightVerdict  = filter_ (\(_, r) -> ((r ^. (result . verdict)) ==. val_ (Just v)))



incompleteFindings, unsoundFindings, disagreementFindings :: Q _ _ _ _
incompleteFindings   = filter_ (\(r,sat,unsat) -> r ^. (result . verdict) ==. val_ Sat &&. sat <. unsat) allFindings
unsoundFindings      = filter_ (\(r,sat,unsat) -> r ^. (result . verdict) ==. val_ Unsat &&. unsat <. sat) allFindings
disagreementFindings = filter_ (\(_,sat,unsat) -> sat /=. 0 &&. unsat /=. 0) allFindings

allFindings :: Q _ _ _ (VerifierRunT (QExpr _ _) , QExpr _ _ Int, QExpr _ _ Int)
allFindings = do
  r <- allRuns_
  (_,sats) <- leftJoin_ (runIdWithVerdict Sat) (\(x,_) -> x ==. (r ^. runId))
  (_,unsats) <- leftJoin_ (runIdWithVerdict Unsat) (\(x,_) -> x ==. (r ^. runId))
  return (r, maybe_ (val_ 0) id sats, maybe_ (val_ 0) id unsats)

--------------------------------------------------------------------------------
-- query fragments

allRuns_ :: Q _ _ _ (VerifierRunT (QExpr _ _))
allRuns_      = all_ (vdiffDb ^. runs)

allPrograms_ :: Q _ _ _ (ProgramT (QExpr _ _))
allPrograms_  = all_ (vdiffDb ^. programs)
