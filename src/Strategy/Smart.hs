{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Strategy.Smart (smartStrategy) where

import qualified Prelude                as P
import           RIO

import           Control.Lens.Operators
import           Control.Monad.State
import           GHC.Exts               (sortWith)
import           Language.C.Data.Lens

import           Instrumentation
import           Strategy.Util
import           Types


data SmartState = SmartState
  {
    _limit :: Int
  , _depth :: Int
  }

newtype Smart env a = Smart
  { unSmart :: StateT SmartState (BrowserT (RIO env)) a
  } deriving (Functor, Applicative, Monad, MonadBrowser, MonadIO, MonadReader env)

runSmart :: IsStrategyEnv env => SmartState -> Stmt -> RIO env (((), SmartState), Stmt)
runSmart initState = runBrowserT (runStateT (unSmart smartStrategy') initState)

smartStrategy :: (IsStrategyEnv env) => RIO env ()
smartStrategy = do
  logInfo "starting with smartStrategy"
  tu <- view translationUnit
  let (Just stmt) = tu ^? (ix "main" . functionDefinition . body)
  let initState = SmartState undefined undefined
  void $ runSmart initState stmt

smartStrategy' :: (IsStrategyEnv env) => Smart env ()
smartStrategy' = do
  -- find 'best' location
  logDebug "exploring level"
  rts <- sortBest <$> exploreLevel
  logDebug $ "ratings are: " <> display (tshow rts)
  forM_ rts $ \(rating, idx) -> do
    goto idx
    c <- currentStmt
    logDebug $ "at statement(rating = " <> display rating <> ") " <> display c

  undefined
  -- unless (null rts) $ do
  --   let best = maxIndex rts
  --   -- go there
  --   replicateM_ best (go Next)
  --   c <- currentStmt
  --   logDebug $ "currently at statement: " <> display (tshow (prettyp c))
  --   -- try to go deeper
  --   foundNext <- untilJust $ do
  --     x <- go Down
  --     if x
  --       then return $ Just True
  --       else do
  --         nxt <- go Next
  --         if nxt
  --           then return (Just False) -- exit
  --           else return Nothing -- loop
  --   if foundNext
  --     then do
  --       logDebug "moved down, recursing"
  --       smartStrategy' -- recurse
  --     else return () -- TODO: What should I do now? Try with second best location...

untilJust :: (Monad m) => m (Maybe a) -> m a
untilJust a = a >>= \case Nothing -> untilJust a
                          Just x -> return x


-- go through the level and calculate a 'score', the more disagreement at a statement, the higher the score.
-- TODO: Implement: This computation is wrapped in tryout, so it won't change the callees' location
exploreLevel :: (IsStrategyEnv env) => Smart env [Double]
exploreLevel = tryout $ do
  x <- scoreStatement
  nxt <- go Next
  if nxt
    then (x:) <$> exploreLevel
    else return [x]



-- | produces a score of the current statement. If the statement has no reads
-- the score is 0, in all other cases the score depends on the "level of
-- disagreement" of the verifiers
scoreStatement :: (IsStrategyEnv env) => Smart env Double
scoreStatement = do
  vs <- findReads
  scores <- forM vs $ \(i,ty) ->
    tryout $ do
      asrt <- mkAssertion i ty
      insertBefore asrt
      tu <- buildTranslationUnit
      res <- liftRIO $ verify tu
      return $ toScore $ conclude res
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

-- | TODO: This uses reverse, not efficient
maxIndex :: [Double] -> Int
maxIndex = snd . P.head . sortBest

sortBest :: [Double] -> [(Double, Int)]
sortBest vs = reverse $ sortWith fst (zip vs [0..])
