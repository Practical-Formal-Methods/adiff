{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}

-- | argument parsers that are used by both vdiff and vdiff-viewer

module VDiff.Arguments
  ( module VDiff.Arguments
  , module Options.Applicative
  ) where

import           VDiff.Prelude

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
verifiers = option verifierParser options
  where options = mconcat [ long "verifiers"
                          , help ("the compared verifiers (available: " <> show (map (^. name) (allVerifiers ++ debuggingVerifiers)) <> ")"  )
                          , value allVerifiers
                          ]
        verifierParser = str >>= \s ->
          if s == ""
            then pure []
            else do
              let reqVer = T.words s
              let unavailable = reqVer L.\\ map (^. name) (allVerifiers ++ debuggingVerifiers)
              if null unavailable
                then pure $ filter (\v -> (v ^. name) `elem` reqVer) (allVerifiers ++ debuggingVerifiers)
                else readerError $ "unknown verifier(s): " ++ unwords (map T.unpack unavailable)


diffParameters :: Parser DiffParameters
diffParameters = DiffParameters
      <$> VDiff.Arguments.strategy       <*> option auto ( long "budget" <> short 'n' <> help "number runs the strategy is allowed to use" <> value 1)
      <*> ((*1000000) <$> option auto ( long "timeout" <> short 't' <> help "number of seconds a verifier is allowed to run before it is terminated" <> value 15))
      <*> VDiff.Arguments.verifiers
      <*> VDiff.Arguments.searchMode
      <*> cFile

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
