-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}

-- | argument parsers that are used by both adiff and adiff-viewer

module ADiff.Arguments
  ( module ADiff.Arguments
  , module Options.Applicative
  ) where

import           Prelude                    (read)
import           ADiff.Prelude

import           Data.Char                  (isAlphaNum, isDigit)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Docker.Client              (MemoryConstraint (..),
                                             MemoryConstraintSize (..))
import ADiff.Data
import           Options.Applicative
import qualified RIO.List                   as L

import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

import           ADiff.Strategy
import           ADiff.Verifier

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



strText :: ReadM Text
strText = str

diffParameters :: Parser DiffParameters
diffParameters = DiffParameters
      <$> ADiff.Arguments.strategy
      <*> ADiff.Arguments.budget
      <*> ADiff.Arguments.resources
      <*> ADiff.Arguments.verifiers
      <*> ADiff.Arguments.searchMode
      <*> ADiff.Arguments.batchSize
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

resources :: Parser [VerifierResources]
resources =  do
  tl <- parseTime
  mem <- optional parseMemory
  cpuSet <- optional parseCpus
  return $ case cpuSet of
    Nothing     -> [VerifierResources tl mem Nothing]
    Just cpuSet -> [VerifierResources tl mem (Just cpus) | cpus <- cpuSet]
  where
    parseCpus :: Parser [Text]
    parseCpus   = option cpuSets (long "cpus" <> help "limit the process to the comma separated list of cpus")
    parseTime   = fromSeconds <$> option auto (long "timeout" <> short 't' <> help "number of seconds a verifier is allowed to run before it is terminated" <> value 30)
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
sc = MPL.space MP.space1 empty empty
lexeme :: MParser a -> MParser a
lexeme = MPL.lexeme sc

verifierList :: ReadM [(VerifierName, [Text], Maybe VerifierName)]
verifierList = str >>= \inp -> case MP.parse verifierList' "command-line" inp of
       Left err -> fail "could not parse"
       Right l  -> pure l
  where
    verifierList' :: MParser [(VerifierName, [Text], Maybe VerifierName)]
    verifierList' = many (lexeme verifier) <* MP.eof

    verifier :: MParser (VerifierName, [Text], Maybe VerifierName)
    verifier = do
      vn <- verifierName
      flags <- (MP.char '(' *> MP.takeWhileP Nothing (/= ')') <* MP.char ')') <|> pure ""
      newName <- optional $ do
        MP.char '#'
        MP.takeWhile1P Nothing (\c -> isAlphaNum c || c `elem` ['-', '_'])
      return (vn, T.words flags, newName)

    verifierName = MP.try $ asum $ map (MP.string . (^. name)) allVerifiers

cpuSets :: ReadM [Text]
cpuSets = str >>= \(inp :: Text) -> case MP.parse cpuSets' "command-line" inp of
  Left err -> fail "cannot parse cpuset list"
  Right l  -> pure l
  where
    cpuSets' = some (lexeme cpuSet) <* MP.eof
    cpuSet = MP.char '[' *> innerList <* MP.char ']'
    innerList = MP.takeWhile1P Nothing (\c -> isDigit c || c `elem` [',', ' '])


relatee :: Parser Relatee
relatee = argument verifierName (help "verifier name or 'consensus'")
  where
    verifierName = str >>= \case
        "consensus" -> return (ConsensusBy defaultWeights)
        v -> return $ RelateName v
