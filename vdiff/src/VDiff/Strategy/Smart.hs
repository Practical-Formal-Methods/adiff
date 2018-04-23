{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ParallelListComp       #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | This is another simple strategy. Whenever this strategy finds a compound
-- statement, it first 'explores' each block item to get a 'score'. This score
-- is calculated by inserting one assertion per variable and checking for
-- disagreement. The 'stronger' the disagreement, the higher the score.
-- smartStrategy' then allocates budgets proportional to this core to the
-- statements and recurses on each statement.
module VDiff.Strategy.Smart (smartStrategy) where

import           RIO                            hiding ((^.))

import           Control.Lens                   hiding (view)
import           Control.Monad.State.Strict
import           Data.List                      (intersperse, sortBy)
import           Language.C
import           Language.C.Analysis.SemRep     hiding (Stmt)
import           Language.C.Analysis.TypeUtils

import           VDiff.Data
import           VDiff.Instrumentation
import           VDiff.Strategy.Common
import           VDiff.Strategy.Common.Averages
import           VDiff.Timed
import           VDiff.Types





data SmartState = SmartState
  { _budget    :: !Int         -- ^ remaining budget
  , _averages  :: [Double]     -- ^ average runtime for each verifier
  , _averagesN :: !Double         -- ^ number of past runs
  , _constants :: ConstantPool -- ^ constants in the program
  }
makeFieldsNoPrefix ''SmartState


newtype Smart env a = Smart
  { unSmart :: StateT SmartState (BrowserT (RIO env)) a
  } deriving (Functor, Applicative, Monad, MonadBrowser, MonadIO, MonadReader env, MonadState SmartState)

-- this monad is also a average monad
instance AverageMonad SmartState (Smart env)

smartStrategy :: (IsStrategyEnv env) => RIO env ()
smartStrategy = do
  logInfo "starting with smartStrategy"
  tu <- view translationUnit
  n <- length <$> view (diffParameters . verifiers)
  bdgt <- view (diffParameters . budget)
  let cs = findAllConstants tu
      blurred = blurConstants cs
  logDebug $ "constants found : " <> display cs
  logDebug $ "constants blurred: " <> display blurred
  let initState = SmartState bdgt (replicate n 0) 0 blurred
  (st,_) <- runSmart initState tu
  logDebug "smart strategy terminated"
  logDebug $ "budget: " <> display (st ^. budget)
  logDebug $ "averages: " <> displayList (st ^. averages)
  return ()

runSmart :: IsStrategyEnv env => SmartState -> CTranslationUnit SemPhase-> RIO env (SmartState, CTranslationUnit SemPhase)
runSmart st stmt= do
  ((_,st'), stmt') <- runBrowserT (runStateT (unSmart smartStrategy') st) stmt
  return (st',stmt')


smartStrategy' :: (IsStrategyEnv env) => Smart env ()
smartStrategy' = do
  -- when budget is not depleted
  whenBudget_ (>0) $ do
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
          -- go to the statement
          goto idx
          c <- currentStmt
          logDebug $ "at statement(rating = " <> display rating <> ") " <> display c
          -- allocate budget proportional to the rating
          let newBudget = ceiling $ totalBudget / totalRating * rating
          -- and spent a half of it on this location
          findCalledFunction >>= \case
            Nothing -> withBudgetLimit (newBudget `div` 2) exploreStatementHeavy
            Just fn -> do
              withBudgetLimit (newBudget `div` 4) exploreStatementHeavy
              withBudgetLimit (newBudget `div` 4) $ tryout $ gotoFunction fn >> smartStrategy'
          -- ... and the other half "under" it
          whenM goDownAtNextChance $
            withBudgetLimit (newBudget `div` 2) smartStrategy'
  return ()


-- | sets the budget to a smaller limit, but still subtracts from the original value
withBudgetLimit :: (IsStrategyEnv env) => Int -> Smart env a -> Smart env a
withBudgetLimit n act = do
  x <- use budget
  budget .= n
  logDebug $ "withBudget: " <> display n
  result <- act
  x' <- use budget
  let usedBudget = x - x'
  budget .= x - usedBudget
  return result

-- | Only execute act when predicate holds on budget
whenBudget :: (IsStrategyEnv env) => (Int -> Bool) -> Smart env a -> Smart env (Maybe a)
whenBudget f act = do
  bdg <- use budget
  logDebug $ "current budget is " <> display bdg
  if f bdg
    then do
      x <- act
      return $ Just x
    else return Nothing

whenBudget_ :: (IsStrategyEnv env) => (Int -> Bool) -> Smart env a -> Smart env ()
whenBudget_ f act = void $ whenBudget f act

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
exploreLevel = do
  x <- use budget
  scores <- tryout exploreLevel'
  x' <- use budget
  logDebug $ "exploreLevel required a budget of " <> display (x - x') <> ", budget is now " <> display x'
  return scores
  where
    exploreLevel' = do
      pos <- currentPosition
      -- cursory exploration of the statement
      rating <- exploreStatement
      let el = (rating, pos)
      nxt <- go Next
      if nxt
        then (el:) <$> exploreLevel'
        else return [el]

-- tries with as many assertions as possible before the budget runs out
-- TODO
exploreStatementHeavy :: (IsStrategyEnv env) => Smart env ()
exploreStatementHeavy = do
  logDebug "exploreHeavy"
  -- read variables
  vs <- findReads
  logDebug $ "reads are " <> display (tshow vs)
  forM_ vs $ \(i,ty) ->
      whenBudget_ (>0) $ tryout $ do
        -- try a 'pool assertion' first, but if there's nothing in the pool use random
        asrt <- mkAssertionFromPool i ty >>= \case
                  Just x' -> return x'
                  Nothing -> mkAssertion i ty
        insertBefore asrt
        buildTranslationUnit >>= verify
  -- and loop if the budget is still not depleted
  whenBudget_ (>0) exploreStatementHeavy



-- | produces a score of the current statement. If the statement has no reads
-- the score is 0, in all other cases the score depends on the "level of
-- disagreement" of the verifiers. Uses an assert(false) statement.
exploreStatement :: (IsStrategyEnv env) => Smart env Double
exploreStatement = tryout $ do
  rs <- findReads
  if null rs
    then return 0
    else do
    insertBefore assertFalse
    (res,conclusion) <- buildTranslationUnit >>= verify
    -- update moving average
    updateAverages' res
    -- calculate score from conclusion
    let d = disagreement conclusion
    t <- timeIrregularity res
    let score = d + t
    return score

updateAverages' res = do
  tl <- fromIntegral <$> view (diffParameters  . timelimit)
  let times = map (maybe (tl / 1000000) elapsedWall . timing . verifierResult) res
  updateAverages times
  avgs <- use averages
  logDebug $ "averages: " <> display (tshow avgs)



-- subject to change
-- always more than zero
disagreement :: Conclusion -> Double
disagreement (StrongAgreement _) = 0.1
disagreement (WeakAgreement _)   = 1
disagreement (Unsoundness _)     = 100
disagreement (Incompleteness _)  = 10
disagreement  Disagreement       = 3

timeIrregularity :: [VerifierRun] -> Smart env Double
timeIrregularity result = do
  avgs <- use averages
  let times = map (maybe 0 elapsedWall . timing . verifierResult) result
  return $ timeIrregularity' avgs times

timeIrregularity' :: [Double] -> [Double] -> Double
timeIrregularity' avgs times =
  let
      props = [ t / a | t <- times | a <- avgs ]
      s = sum [ relativeError t1 t2 | t1 <- props , t2 <- props ]
      n = fromIntegral (length times) :: Double
  in s / (n * n)

-- |
-- Calculate relative error of two numbers:
--
-- \[ \frac{|a - b|}{\max(|a|,|b|)} \]
--
-- It lies in [0,1) interval for numbers with same sign and (1,2] for
-- numbers with different sign. If both arguments are zero or negative
-- zero function returns 0. If at least one argument is transfinite it
-- returns NaN
-- Source: math-functions
relativeError :: Double -> Double -> Double
relativeError a b
  | a == 0 && b == 0 = 0
  | otherwise        = abs (a - b) / max (abs a) (abs b)



-- | Chooses a constant: First tries to choose randomly from the pool for the type.
mkAssertionFromPool :: Ident -> Type -> Smart env (Maybe Stmt)
mkAssertionFromPool varName ty = do
  cs <- lookupPool ty <$> use constants
  chooseOneOf cs >>= \case
    Nothing -> return Nothing
    Just c' -> do
      let var = CVar varName (undefNode, ty)
          cnst = CConst c'
          expr = CBinary CNeqOp var cnst (undefNode, voidType)
      return $ Just $ assertStmt expr



sortBest :: [(Double, AstPosition)] -> [(Double, AstPosition)]
sortBest = sortBy (flip $ comparing fst)


displayList :: Display a => [a] -> Utf8Builder
displayList xs = mconcat $ intersperse ", " (map display xs)
