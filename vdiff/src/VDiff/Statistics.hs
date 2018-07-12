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


data HandleUnknown = IncludeUnknown | ExcludeUnknown

relativeInclusion :: (HasDatabase env) => Verdict -> HandleUnknown -> VerifierName -> VerifierName -> RIO env Double
relativeInclusion vrd handleUnknown v1 v2 = do
  let qTotal = case handleUnknown of
               IncludeUnknown -> [ (v1, [vrd]) , (v2, [Sat,Unsat,Unknown]) ]
               ExcludeUnknown -> [ (v1, [vrd]) , (v2, [Sat,Unsat]) ]
  (Just n) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ programByVerdicts qTotal
  (Just implied) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ programByVerdicts [ (v1, [vrd]), (v2, [vrd])]
  return (fromIntegral implied / fromIntegral n)

