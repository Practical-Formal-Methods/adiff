{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

-- | argument parsers that are used by both vdiff and vdiff-viewer

module VDiff.Arguments
  ( module VDiff.Arguments
  , module Options.Applicative
  ) where

import           VDiff.Prelude

import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import           Options.Applicative
import qualified RIO.List            as L

import           VDiff.Strategy
import           VDiff.Verifier

cFile :: Parser FilePath
cFile =  argument str options
  where options = mconcat [ metavar "FILE"
                          , help "a C file"
                          , action "file"
                          ]

someFile :: Parser FilePath
someFile =  argument str options
  where options = mconcat [ metavar "FILE"
                          , help "file"
                          , action "file"
                          ]

verifiers :: Parser [Verifier]
verifiers = option verifierList options
  where options = mconcat [ long "verifiers"
                          , help ("the compared verifiers (available: " <> show (map (^. name) (allVerifiers ++ debuggingVerifiers)) <> ")"  )
                          , value allVerifiers
                          ]

verifierList = str >>= \s ->
  if s == ""
    then pure []
    else do
      let reqVer = T.words s
      let unavailable = reqVer L.\\ map (^. name) (allVerifiers ++ debuggingVerifiers)
      if null unavailable
        then pure $ filter (\v -> (v ^. name) `elem` reqVer) (allVerifiers ++ debuggingVerifiers)
        else readerError $ "unknown verifier(s): " ++ unwords (map T.unpack unavailable)


verifierFlags :: Parser (Map VerifierName [Text])
verifierFlags = foldl' addToMap Map.empty <$> many flagParsers
  where
    addToMap m (v,f) = Map.insertWith (++) v [f] m
    flagParsers      = asum $ map (mkFlagParser . (^. name))  allVerifiers
    mkFlagParser v   = (v,) <$> option strText (long (T.unpack v ++ "-flags"))

strText :: ReadM Text
strText = str

diffParameters :: Parser DiffParameters
diffParameters = DiffParameters
      <$> VDiff.Arguments.strategy
      <*> VDiff.Arguments.budget
      <*> ((*1000000) <$> option auto ( long "timeout" <> short 't' <> help "number of seconds a verifier is allowed to run before it is terminated" <> value 15))
      <*> VDiff.Arguments.verifiers
      <*> VDiff.Arguments.verifierFlags
      <*> VDiff.Arguments.searchMode
      <*> VDiff.Arguments.batchSize
      <*> cFile

budget :: Parser Text
budget = option str ( long "budget" <> short 'n' <> help "number runs the strategy is allowed to use" <> value "1")

batchSize :: Parser Int
batchSize = option auto options
  where options = mconcat [ long "batch-size"
                          , metavar "BATCH-SIZE"
                          , help "uses a conjunction of BATCH-SIZE inequalities for an assertion."
                          , value 1
                          ]

searchMode :: Parser SearchMode
searchMode =  flag' Subexpressions (long "read-subexpressions")
          <|> flag' IdentOnly (long "read-identifiers-only")
          <|> pure Subexpressions

strategy :: Parser Strategy
strategy = option stratParser options
  where options = mconcat [ long "strategy"
                          , help $ "guidance algorithm (available: " ++ txtStrategies ++ ")"
                          , value RandomWalkStrategy
                          , showDefaultWith strategyName
                          , metavar "STRATEGY"
                          , completeWith strategyNames
                          ]
        stratParser = lookupStrategy <$> str >>= \case
          Just s -> return s
          Nothing -> readerError $ "Accepted strategies are " ++ txtStrategies
        txtStrategies = unwords strategyNames
        strategyNames = map strategyName availableStrategies

seed :: Parser (Maybe Int)
seed = optional $ option auto options
  where options = mconcat [ long "seed"
                          , help "seed to initialize random generator"
                          , metavar "SEED"
                          ]
