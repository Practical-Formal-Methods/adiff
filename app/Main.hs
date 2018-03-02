module Main where

import           Options.Applicative
import           Data.Monoid                ((<>))
import Data.List ((\\))

import           Diff
import           Types
import Verifiers

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
  <*> option stratParser (mconcat [ long "strategy"
                                  , help "guidance algorithm"
                                  , value NaiveRandom
                                  , showDefaultWith strategyName
                                  , metavar "STRATEGY"])
  <*> option verifierParser (mconcat [ long "verifiers"
                                     , help ("the compared verifiers (available: " ++ show (map verifierName allVerifiers) ++ ")"  )
                                     , value []
                                     , metavar "VERIFIERS"
                                     ])
  <*> argument str (metavar "FILE")
  where
    stratParser :: ReadM Strategy
    stratParser = str >>= \case
        "naive"        -> return NaiveRandom
        "smart"          -> return SmartGuided
        _ -> readerError "Accepted strategies are 'naive' and 'smart'."
    verifierParser :: ReadM [Verifier]
    verifierParser = str >>= \s -> if s == ""
      then pure []
      else let reqVer = words s
               unavailable = reqVer \\ map verifierName allVerifiers
           in
             if null unavailable
             then pure $ filter (\v -> verifierName v `elem` reqVer) allVerifiers
             else readerError $ "unknown verifier(s): " ++ (unwords unavailable)
