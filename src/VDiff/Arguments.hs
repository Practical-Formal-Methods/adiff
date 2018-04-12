-- | argument parsers that are used by both vdiff and vdiff-viewer

module VDiff.Arguments
  ( module VDiff.Arguments
  , module Options.Applicative
  ) where

import           Options.Applicative
import           RIO
import qualified RIO.List            as L

import VDiff.Types
import VDiff.Verifier

databasePath :: Parser String
databasePath = option str options
  where options = mconcat [ long "database"
                          , short 'd'
                          , help "path to sqlite3 database (if not given, an in-memory database is used)"
                          , value ":memory:"
                          , action "file"
                          ]

cFile :: Parser FilePath
cFile =  argument str options
  where options = mconcat [ metavar "FILE"
                          , help "a C file"
                          , action "file"
                          ]


verifiers :: Parser [Verifier]
verifiers = option verifierParser options
  where options = mconcat [ long "verifiers"
                          , help ("the compared verifiers (available: " ++ show (map verifierName (allVerifiers ++ debuggingVerifiers)) ++ ")"  )
                          , value allVerifiers
                          ]
        verifierParser = str >>= \s -> if s == ""
                               then pure []
                               else let reqVer = words s
                                        unavailable = reqVer L.\\ map verifierName (allVerifiers ++ debuggingVerifiers)
                                    in
                                      if null unavailable
                                      then pure $ filter (\v -> verifierName v `elem` reqVer) (allVerifiers ++ debuggingVerifiers)
                                      else readerError $ "unknown verifier(s): " ++ unwords unavailable

level :: Parser LogLevel
level = option levelP options
  where
    options = mconcat [ long "log-level"
                      , help helpText
                      , metavar "LOGLEVEL"
                      , value LevelWarn
                      , completeWith values
                      ]
    values = ["debug", "info", "warning", "error"]
    helpText = "Allowed values: " ++ concat (L.intersperse ", " values)
    levelP = (str :: ReadM Text ) >>= \case
      "debug"   -> return LevelDebug
      "info"    -> return LevelInfo
      "warning" -> return LevelWarn
      "error"   -> return LevelError
      s -> readerError $ "unknown log-level " ++ show s
