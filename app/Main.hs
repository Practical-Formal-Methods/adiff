module Main where

import           Options.Applicative
import           Data.Monoid                ((<>))
import Data.Foldable (asum)
import Data.List ((\\))

import           Diff
import           Types
import Verifier (allVerifiers)

main :: IO ()
main = execParser opts >>= diff
  where
    opts = info (mainParameters <**> helper)
                (fullDesc
                 <> progDesc "vdiff - a simple tool to compare program verifiers"
                )


mainParameters :: Parser MainParameters
mainParameters = asum [pVersions, pParseTest, pRun]
  where
    pVersions  = CmdVersions <$ switch ( long "versions" <> help "prints versions of the available verifiers" )
    pParseTest = CmdParseTest <$ switch (long "parse" <> help "parses and prints the given file") <*> argument str (metavar "FILE")
    pRun       = CmdRun
      <$> switch ( long "verbose" <> help "verbose output")
      <*> option stratParser (mconcat [ long "strategy"
                                      , help "guidance algorithm"
                                      , value NaiveRandom
                                      , showDefaultWith strategyName
                                      , metavar "STRATEGY"])
      <*> option auto ( long "iterations" <> short 'n' <> help "number of iterations" <> value 1)
      <*> optional (strOption ( long "database" <> short 'd' <> help "sqlite database to save results"))
      <*> option verifierParser (mconcat [ long "verifiers"
                                        , help ("the compared verifiers (available: " ++ show (map verifierName allVerifiers) ++ ")"  )
                                        , value []
                                        , metavar "VERIFIERS"
                                        ])
      <*> argument str (metavar "FILE")
  
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
