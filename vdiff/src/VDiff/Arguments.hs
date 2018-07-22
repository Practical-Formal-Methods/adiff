{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}

-- | argument parsers that are used by both vdiff and vdiff-viewer

module VDiff.Arguments
  ( module VDiff.Arguments
  , module Options.Applicative
  ) where

import           Prelude                    (read)
import           VDiff.Prelude

import           Data.Char                  (isAlphaNum, isDigit)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Docker.Client              (MemoryConstraint (..),
                                             MemoryConstraintSize (..))
import           Options.Applicative
import qualified RIO.List                   as L

import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

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

verifiers :: Parser [(VerifierName, [Text], Maybe VerifierName)]
verifiers = option verifierList options
  where options = mconcat [ long "verifiers"
                          , help ("the compared verifiers (available: " <> show (map (^. name) (allVerifiers ++ debuggingVerifiers)) <> ")"  )
                          , value [(v ^. name, [], Nothing) | v <- allVerifiers]
                          ]



verifierFlags = undefined
-- verifierFlags :: Parser (Map VerifierName [Text])
-- verifierFlags = foldl' addToMap Map.empty <$> many flagParsers
--   where
--     addToMap m (v,f) = Map.insertWith (++) v [f] m
--     flagParsers      = asum $ map (mkFlagParser . (^. name))  allVerifiers
--     mkFlagParser v   = (v,) <$> option strText (long (T.unpack v ++ "-flags"))

strText :: ReadM Text
strText = str

diffParameters :: Parser DiffParameters
diffParameters = DiffParameters
      <$> VDiff.Arguments.strategy
      <*> VDiff.Arguments.budget
      <*> VDiff.Arguments.resources
      <*> VDiff.Arguments.verifiers
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
seed = optional $ option auto (long "seed" <> help "seed to initialize random generator")

resources :: Parser VerifierResources
resources =  do
  tl <- (*1000000) <$> parseTime
  mem <- optional parseMemory
  cpus <- optional parseCpus
  return $ VerifierResources tl mem cpus
  where
    parseCpus   = option str  (long "cpuset-cpus" <> help "limit the process to the comma separated list of cpus")
    parseTime   = option auto (long "timeout" <> short 't' <> help "number of seconds a verifier is allowed to run before it is terminated" <> value 30)
    parseMemory = option mem  (long "memory" <> help "limit the number of used memory")
    mem = str >>= \s ->  return $ MemoryConstraint (parsePrefix s ) (parseSuffix s)
      where
        parsePrefix s = read $ takeWhile isDigit s
        parseSuffix s = case dropWhile isDigit s of
          "B"  -> B
          "M"  -> MB
          "MB" -> MB
          "G"  -> GB
          "GB" -> GB


-- | The verifier list is a space separated list of verifiers. A verifier can be indicated by either:
-- * its name, e.g. @smack@
-- * its name with a combination of parameters @smack(--loop-unroll=1)@
-- * its name with a combination of parameters and a new name: @smack(--loop-unroll=1)#smack-1@

type MParser = MP.Parsec Void Text

verifierList :: ReadM [(VerifierName, [Text], Maybe VerifierName)]
verifierList = str >>= \inp -> case MP.parse verifierList' "command-line" inp of
       Left err -> fail "could not parse"
       Right l  -> pure l
  where
    verifierList' :: MParser [(VerifierName, [Text], Maybe VerifierName)]
    verifierList' = many (MPL.lexeme sc verifier) <* MP.eof

    verifier :: MParser (VerifierName, [Text], Maybe VerifierName)
    verifier = do
      vn <- verifierName
      flags <- (MP.char '(' *> MP.takeWhileP Nothing (/= ')') <* MP.char ')') <|> pure ""
      newName <- optional $ do
        MP.char '#'
        MP.takeWhile1P Nothing (\c -> isAlphaNum c || c `elem` ['-', '_'])
      return (vn, T.words flags, newName)

    verifierName = MP.try $ asum $ map (MP.string . (^. name)) allVerifiers

    sc = MPL.space MP.space1 empty empty


