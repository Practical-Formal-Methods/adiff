{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Main where

import           Control.Lens.TH
import qualified Database.SQLite.Simple    as SQL
import           RIO
import           System.Exit
import           System.IO
import qualified Text.PrettyPrint.Tabulate as T

import           Arguments                 hiding (command)
import           Data
import qualified Query                     as Q
import           Types


data ViewCommand = Stats
                 | List Q.Query
                 | Count Q.Query
                 | Program String
                 deriving (Show, Eq)

data ViewParameters = ViewParameters
  { _databaseFn :: FilePath
  , _command    :: ViewCommand
  }
makeFieldsNoPrefix ''ViewParameters




main :: IO ()
main = do
  vp <- execParser opts
  -- set up logging
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogMinLevel LevelDebug logOptions
  -- set up sql
  SQL.withConnection (vp ^. databaseFn) $ \conn ->
    withLogFunc logOptions' $ \logger -> do
      let viewEnv = MainEnv logger conn
      runRIO viewEnv $ executeView (vp ^. command)

instance T.CellValueFormatter Text

executeView :: (HasMainEnv env) => ViewCommand -> RIO env ()
executeView Stats = do
  stats <- Q.stats
  liftIO $  T.printTable stats
  return ()
executeView (List q) = do
  rs <- case q  of
        Q.Incomplete -> Q.allIncomplete
        Q.Unsound    -> Q.allUnsound
  liftIO $ T.printTable rs
executeView (Count q) = do
  rs <- case q  of
        Q.Incomplete -> Q.allIncomplete
        Q.Unsound    -> Q.allUnsound
  liftIO $ print $ length rs
executeView (Program hsh) = do
  p <- Q.programByHash hsh
  liftIO $ case p of
    Just p' -> putStr (p' ^. source)
    Nothing -> do
      hPutStrLn stderr $ "could not find program with hash: " <> hsh
      exitFailure

opts :: ParserInfo ViewParameters
opts = info (viewParameters <**> helper) (progDesc "viewer for vdiff")


viewParameters :: Parser ViewParameters
viewParameters = ViewParameters <$> databasePath  <*> viewCommand

viewCommand :: Parser ViewCommand
viewCommand = statCmd <|> listCmd <|> countCmd <|> programCmd

statCmd,listCmd,countCmd,programCmd :: Parser ViewCommand
statCmd = switch options $> Stats
  where options = mconcat [ long "stat"
                          , short 's'
                          , help "print basic statistics about this database"
                          ]

listCmd = switch options $> List <*> query
  where options = mconcat [ long "list"
                          , short 'l'
                          , help "prints a list"
                          ]

countCmd = switch options $> Count <*> query
  where options = mconcat [ long "count"
                          , help "returns the number of findings"
                          ]

programCmd = Program <$> option str options
  where options = mconcat [ long "hash"
                          , help "returns the source code of a program with the given hash"
                          ]

query :: Parser Q.Query
query = incmpl <|> unsound
  where incmpl = switch (long "incomplete") $> Q.Incomplete
        unsound = switch (long "unsound") $> Q.Unsound



