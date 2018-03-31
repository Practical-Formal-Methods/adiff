-- | common things that are necessary to implement strategies.
module Strategy.Util where

import           Control.Lens
import           Language.C
import           RIO                       hiding (view)
import           System.IO                 (hPutStr)
import           Text.PrettyPrint.HughesPJ (render)


import           Data
import           Persistence               (hash)
import           Types

class (HasTranslationUnit env, HasLogFunc env, HasDiffParameters env) => StrategyEnv env


-- | runs the given translation unit against the configured verifiers.
verify :: (HasLogFunc env, HasTranslationUnit env, HasDiffParameters env) => CTranslationUnit SemPhase -> RIO env [VerifierRun]
verify tu = do
  vs <- view (diffParameters . verifiers)
  withSystemTempFile "input.c" $ \fp h -> do
        -- write file
        let content = render . pretty $ tu
            hsh = hash content
        liftIO $ hPutStr h content >> hFlush h
        -- run each verifier
        forM vs $ \v -> do
            env <- mkVerifierEnv (15 * 1000 * 1000) -- 15 seconds
            r <- runRIO env $ execute v fp
            return $ VerifierRun (verifierName v) r hsh
