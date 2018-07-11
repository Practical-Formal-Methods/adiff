{-# LANGUAGE PartialTypeSignatures #-}

-- similar to Query but slightly more high-level
module VDiff.Statistics where

import           Data.List            (intersperse)
import           Database.Beam
import           Database.Beam.Sqlite
import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Prelude
import           VDiff.Query2
import           VDiff.Util.Tables
import           VDiff.Verifier


verdicts :: (HasDatabase env) => QueryFocus -> RIO env (Int, Int, Int)
verdicts qf = do
  vs <- runBeam $ runSelectReturningList $ select $ agg $ flt allRuns_
  let sats = fromMaybe 0 $ lookup Sat vs
  let unsats = fromMaybe 0 $ lookup Unsat vs
  let unknown = fromMaybe 0 $ lookup Unknown vs
  return (sats, unsats, unknown)
 where
   flt = case qf of
           QueryFocusEverything -> filter_ ( const $ val_ True)
           QueryFocus vs -> filter_ (\r  -> (r ^. verifierName) `in_` map val_ vs)
   agg = aggregate_ $ \r -> (group_ (r ^. (result . verdict)), countAll_)

relativeInclusion :: (HasDatabase env) => Verdict -> VerifierName -> VerifierName -> RIO env Double
relativeInclusion vrd v1 v2 = do
  -- get total number of programs where v1 is unsat
  (Just n) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ programByVerifierAndVerdict v1 Sat
  -- number of programs for which r1 = unsat and r2 = unsat
  (Just implied) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ do
        p1 <- programByVerifierAndVerdict v1 Sat
        p2 <- programByVerifierAndVerdict v2 Sat
        guard_ (pk p1 ==. pk p2)
        return (p1,p2)
  return (fromIntegral implied / fromIntegral n)
  where
    programByVerifierAndVerdict :: VerifierName -> Verdict -> Q _ _ _ (ProgramT _)
    programByVerifierAndVerdict vn v = nub_ $ do
      p <- allPrograms_
      r <- filter_ (\r -> (r ^. (result . verdict)) ==. val_ v &&. (r ^. verifierName) ==. val_ vn) allRuns_
      -- this is an inner join
      guard_ ((r ^. program) ==. p ^. hash)
      -- only return p
      return p
