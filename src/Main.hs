module Main where

import           Options.Applicative
import           Data.Monoid                ((<>))

import           Diff
import           Types

main :: IO ()
main = execParser opts >>= diff
  where
    opts = info (mainParameters <**> helper)
                (fullDesc
                 <> progDesc "vdiff - a simple tool to compare program verifiers"
                )


mainParameters :: Parser MainParameters
mainParameters = MainParameters
  <$> switch ( long "verbose" <> help "verbose output")
  <*> option stratParser ( long "strategy" <> help "guidance algorithm" <> value NaiveRandom <> showDefaultWith strategyName
                <> metavar "STRATEGY")
  <*> argument str (metavar "FILE")
  where
    stratParser :: ReadM Strategy
    stratParser= str >>= \case
        "naive"        -> return NaiveRandom
        "smart"          -> return SmartGuided
        _ -> readerError "Accepted strategies are 'naive' and 'smart'."
