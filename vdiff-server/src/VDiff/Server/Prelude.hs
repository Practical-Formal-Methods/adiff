{-# LANGUAGE TemplateHaskell #-}

module VDiff.Server.Prelude
  ( module VDiff.Prelude
  , module VDiff.Server.Prelude
  , module Web.Scotty.Trans
  , Html
  , shamletFile
  , shamlet
  , renderHtml
  ) where


import           Control.Concurrent.MSemN
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Text               as JSON
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text.Lazy                as LT
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Hamlet
import           VDiff.Data
import           VDiff.Prelude                 hiding (body, showError)
import           VDiff.Statistics
import           Web.Scotty.Trans


--------------------------------------------------------------------------------
-- Some Scotty Setup
type SrvError = LT.Text
type RioScottyM env = ScottyT SrvError (RIO env)
type RioActionM env = ActionT SrvError (RIO env)

data ServerEnv = ServerEnv
  { _serverMainEnv                :: MainEnv
  , _semaphoreConcurrentVerifiers :: MSemN Int
  , _overviewCache :: IORef (Map Weights (RelativeTable, RelativeTable, RelativeTable, RelativeTable))
  }
serverMainEnv :: Lens' ServerEnv MainEnv
serverMainEnv = lens _serverMainEnv (\e s -> e { _serverMainEnv = s})


class HasSemaphore env where
  semaphore :: Lens' env (MSemN Int)

class HasOverviewCache env where
  overviewCache :: Lens' env (IORef (Map Weights (RelativeTable, RelativeTable, RelativeTable, RelativeTable)))

class (HasMainEnv env, HasSemaphore env, HasOverviewCache env) => HasServerEnv env

instance HasSemaphore ServerEnv where
  semaphore = lens _semaphoreConcurrentVerifiers (\e s -> e {_semaphoreConcurrentVerifiers = s})

instance HasDatabase ServerEnv where
  databaseL = serverMainEnv . databaseL

instance HasLogFunc ServerEnv where
  logFuncL  = serverMainEnv . logFuncL

instance HasOverviewCache ServerEnv where
  overviewCache = lens _overviewCache (\e c -> e {_overviewCache = c})

instance HasMainEnv ServerEnv

instance HasServerEnv ServerEnv
--------------------------------------------------------------------------------

defaultTemplate :: Text -> Html -> Html
defaultTemplate title content = $(shamletFile "templates/template.hamlet")

defaultLayout title content = raw $ renderHtml $ defaultTemplate title content

paramMay :: (Parsable a, Monad m, ScottyError e) => LT.Text -> ActionT e m (Maybe a)
paramMay n = (Just <$> param n) `rescue` const (return Nothing)

paramJsonMay :: (JSON.FromJSON a) => LT.Text -> RioActionM env  (Maybe a)
paramJsonMay n = do
  p <- (Just <$> param n) `rescue` const (return Nothing)
  case p of
    Nothing -> return Nothing
    Just p' -> case JSON.eitherDecode p' of
                 Left err -> raise (LT.pack err)
                 Right x  -> return $ Just x
