{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}

-- similar to Query but slightly more high-level
module VDiff.Statistics where

import           Data.List            (intersperse)
import qualified Data.Map             as Map
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



relative :: (HasDatabase env) => Verdict
  -> Bool
  -> VerifierName
  -> VerifierName
  -> RIO env (Integer, Integer)
relative vrd allowUnknown v1 v2 = do
  (Just intersection) <- runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ programByVerdicts [ (v1, vrd : [Unknown | allowUnknown]), (v2, [vrd]) ]
  (Just totalV2) <-      runBeam $ runSelectReturningOne $ select $ aggregate_ (const countAll_) $ programByVerdicts [ (v1, [Sat, Unsat, Unknown])         , (v2, [vrd]) ]
  return (fromIntegral intersection,  fromIntegral totalV2)


relativeSoundness, relativeCompleteness, relativeRecall, relativePrecision
  :: (HasDatabase env) => VerifierName -> VerifierName -> RIO env (Integer, Integer)

relativeSoundness    = relative Sat True
relativeCompleteness = relative Unsat True
relativeRecall       = relative Sat False
relativePrecision    = relative Unsat False

overPairs f = Map.fromList <$> sequence [ ((v1, v2),) <$> f v1 v2
                                        | v1 <- verifierNames, v2 <- verifierNames ]
  where
    verifierNames = map (^. name) allVerifiers
