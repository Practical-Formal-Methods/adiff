{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | This is another simple strategy. Whenever this strategy finds a compound
-- statement, it first 'explores' each block item to get a 'score'. This score
-- is calculated by inserting one assertion per variable and checking for
-- disagreement. The 'stronger' the disagreement, the higher the score.
-- smartStrategy' then allocates budgets proportional to this core to the
-- statements and recurses on each statement.
module VDiff.Strategy.Smart (smartStrategy) where

import           RIO
import qualified RIO.Map                    as Map

import           Control.Lens               hiding (view)
import           Control.Monad.State.Strict
import           Data.List                  (sortBy)
import           Language.C.Data.Lens

import           VDiff.Data
import           VDiff.Instrumentation
import           VDiff.Strategy.Common
import           VDiff.Timed
import           VDiff.Types


data SmartState = SmartState
  { _budget   :: !Int                     -- ^ remaining budget
  , _averages :: Map VerifierName Double  -- ^ average runtime for each verifier
  , _runN     :: !Int                     -- ^ number of past runs
  }
makeFieldsNoPrefix ''SmartState

newtype Smart env a = Smart
  { unSmart :: StateT SmartState (BrowserT (RIO env)) a
  } deriving (Functor, Applicative, Monad, MonadBrowser, MonadIO, MonadReader env, MonadState SmartState)

smartStrategy :: (IsStrategyEnv env) => RIO env ()
smartStrategy = do
  logInfo "starting with smartStrategy"
  tu <- view translationUnit
  let (Just stmt) = tu ^? (ix "main" . functionDefinition . body)
  bdgt <- view (diffParameters . budget)
  let initState = SmartState bdgt Map.empty 0
  void $ runSmart initState stmt

runSmart :: IsStrategyEnv env => SmartState -> Stmt -> RIO env (((), SmartState), Stmt)
runSmart initState = runBrowserT (runStateT (unSmart smartStrategy') initState)



smartStrategy' :: (IsStrategyEnv env) => Smart env ()
smartStrategy' = do
  -- when budget is not depleted
  whenM ((>0) <$> use budget) $ do
    -- if in compound statement, go down
    in_compound <- isCompound <$> currentStmt
    if in_compound
      then go Down >> smartStrategy'
      else do
        -- find 'best' location
        logDebug "exploring level"
        rts <- sortBest <$> exploreLevel
        let totalRating = sum $ map fst rts
        totalBudget <- fromIntegral <$> use budget
        logDebug $ "ratings are: " <> display (tshow rts)
        forM_ rts $ \(rating, idx) -> do
          goto idx
          c <- currentStmt
          logDebug $ "at statement(rating = " <> display rating <> ") " <> display c
          -- try to find a place go down
          whenM goDownAtNextChance $ do
            -- allocate budget proportional to the rating
            let newBudget = ceiling $ totalBudget / totalRating * rating
            withBudget newBudget smartStrategy'
            go_ Up



-- | sets the budget to a smaller limit, but still subtracts from the original value
withBudget :: (IsStrategyEnv env) => Int -> Smart env a -> Smart env a
withBudget n act = do
  x <- use budget
  budget .= n
  logDebug $ "withBudget: " <> display n
  result <- act
  x' <- use budget
  let usedBudget = x - x'
  budget -= usedBudget
  return result

-- | moves the cursor down into the next statement if possible. If successful
-- returns True, otherwise False.
goDownAtNextChance :: (MonadBrowser m) => m Bool
goDownAtNextChance = untilJust $ do
    dwn <- go Down
    if dwn
      then return $ Just True
      else do
       next <- go Next
       if next
         then return Nothing
         else return $ Just False

untilJust :: (Monad m) => m (Maybe a) -> m a
untilJust a = a >>= \case Nothing -> untilJust a
                          Just x -> return x


-- | go through the level and calculate a 'score', the more disagreement at a
-- statement, the higher the score. This computation is wrapped in tryout, so it
-- won't change the callees' location
exploreLevel :: (IsStrategyEnv env) => Smart env [(Double, AstPosition)]
exploreLevel = tryout $ do
  pos <- currentPosition
  rating <- exploreStatement
  let el = (rating, pos)
  nxt <- go Next
  if nxt
    then (el:) <$> exploreLevel
    else return [el]



-- | produces a score of the current statement. If the statement has no reads
-- the score is 0, in all other cases the score depends on the "level of
-- disagreement" of the verifiers
exploreStatement :: (IsStrategyEnv env) => Smart env Double
exploreStatement = do
  vs <- findReads
  scores <- forM vs $ \(i,ty) ->
    tryout $ do
      asrt <- mkAssertion i ty
      insertBefore asrt
      tu <- buildTranslationUnit
      bdg <- use budget
      if bdg > 0
        then do
           -- run verifiers
          (res, conclusion) <- verify tu
          -- update moving average
          updateAverages res
          -- calculate score from conclusion (TODO: Also use timing differences)
          return $ toScore conclusion
        else return 0
  return $ maximum scores

-- "cumulative moving average"
updateAverages :: (IsStrategyEnv env) => [VerifierRun] -> Smart env ()
updateAverages rs = do
  n <- fromIntegral <$> use runN
  tl <- fromIntegral <$> view (diffParameters . timelimit)
  forM_ rs $ \r -> do
    let newTime = maybe tl elapsedWall (timing (verifierResult r))
    averages . ix (runVerifierName r) %= updateAverage n newTime

  where updateAverage n newTime old = (n * old + newTime ) /  (n + 1)




-- subject to change
toScore :: Conclusion -> Double
toScore (StrongAgreement _) = 0.0
toScore (WeakAgreement _)   = 1
toScore (Unsoundness _)     = 100
toScore (Incompleteness _)  = 10
toScore  Disagreement       = 3

--------------------------------------------------------------------------------
-- | NOTE: Only makes sense for positive numbers
maximum :: [Double] -> Double
maximum = foldl' max 0


sortBest :: [(Double, AstPosition)] -> [(Double, AstPosition)]
sortBest = sortBy (flip $ comparing fst)
