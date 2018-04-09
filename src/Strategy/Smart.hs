{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Strategy.Smart (smartStrategy) where

import           RIO

import           Control.Lens         hiding (view)
import           Control.Monad.State.Strict
import           Data.List            (sortBy)
import           Language.C.Data.Lens

import           Instrumentation
import           Strategy.Util
import           Types


data SmartState = SmartState
  {
    _budget :: !Int
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
  budget <- view (diffParameters . budget)
  let initState = SmartState budget
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
        logDebug $ "ratings are: " <> display (tshow rts)
        forM_ rts $ \(rating, idx) -> do
          goto idx
          c <- currentStmt
          logDebug $ "at statement(rating = " <> display rating <> ") " <> display c
          -- try to find a place go down
          whenM goDownAtNextChance $ do
          -- recurse from here, but remember a limit of runs after which we will come backup again
            bdg <- use budget
            withBudget (bdg `div` 10 + 1) smartStrategy' -- TODO: be smarter here
            go_ Up



-- | sets the budget to a smaller limit, but still subtracts from the original value
-- (TODO: Use this when recursing on some smaller part)
withBudget :: (IsStrategyEnv env) => Int -> Smart env a -> Smart env a
withBudget n act = do
  logDebug $ "withBudget: " <> display n
  act -- TODO: Implement withBudget

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


-- go through the level and calculate a 'score', the more disagreement at a statement, the higher the score.
-- This computation is wrapped in tryout, so it won't change the callees' location
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
          (res, conclusion) <- verify tu
          return $ toScore conclusion
        else return 0
  return $ maximum scores


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
