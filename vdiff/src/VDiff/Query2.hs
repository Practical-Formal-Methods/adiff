{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{- This will become the new type-safe query module after I figured out how to use beam -}
module VDiff.Query2 where

import           VDiff.Prelude          hiding (Disagreement)

import           Database.Beam
import           VDiff.Data
import           VDiff.Persistence

allRuns_      = all_ (vdiffDb ^. runs)
allPrograms_  = all_ (vdiffDb ^. programs)


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
    runCount     = aggregate_ (const countAll_) allRuns_
    programCount = aggregate_ (const countAll_) allPrograms_
    distinctOrigins = aggregate_ (\c -> countOver_ distinctInGroup_ (c ^. origin)) allPrograms_


allRuns :: (HasDatabase env) => RIO env [VerifierRun]
allRuns = runBeam $ runSelectReturningList $ select allRuns_

allPrograms :: (HasDatabase env) => RIO env [Program]
allPrograms = runBeam $ runSelectReturningList $ select $ all_ (vdiffDb ^. programs)

programByHash :: (HasDatabase env) => Text -> RIO env (Maybe Program)
programByHash hsh = runBeam $ runSelectReturningOne $ select q
  where
    q =  filter_ flt (all_ (vdiffDb ^. programs))
    flt prg = prg ^. hash ==. val_ hsh

-- satsByHash :: (HasDatabase env) => PrimaryKey ProgramT _ -> RIO env Int
-- satsByHash hsh = runDB $ do
--   (Just n) <- runSelectReturningOne $ select $ aggregate_ (const countAll_) (relatedRuns hsh)
--   return n

-- relatedRuns :: PrimaryKey ProgramT _ -> Q _ VDiffDb _  (VerifierRunT _)
-- relatedRuns hsh = filter_ (\r -> r ^. program ==. hsh) (all_ (vdiffDb ^. runs))

-- numSats :: PrimaryKey ProgramT _  -> Verdict -> Q _ _ _ (QExpr _ _ Int)
-- numSats hsh v = aggregate_ (const countAll_) $
--                 filter_ (\r -> r ^. (result . verdict) ==. val_ v ) (relatedRuns hsh)



