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
import qualified Data.Text.Lazy                as LT
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Hamlet
import           VDiff.Prelude                 hiding (body, showError)
import           Web.Scotty.Trans
--------------------------------------------------------------------------------
-- Some Scotty Setup
type SrvError = LT.Text
type RioScottyM env = ScottyT SrvError (RIO env)
type RioActionM env = ActionT SrvError (RIO env)

data ServerEnv = ServerEnv
  { _serverMainEnv                :: MainEnv
  , _semaphoreConcurrentVerifiers :: MSemN Int
  }
serverMainEnv :: Lens' ServerEnv MainEnv
serverMainEnv = lens _serverMainEnv (\e s -> e { _serverMainEnv = s})

class HasSemaphore env where
  semaphore :: Lens' env (MSemN Int)

class (HasMainEnv env, HasSemaphore env) => HasServerEnv env

instance HasSemaphore ServerEnv where
  semaphore = lens _semaphoreConcurrentVerifiers (\e s -> e {_semaphoreConcurrentVerifiers = s})

instance HasDatabase ServerEnv where
  databaseL = serverMainEnv . databaseL

instance HasLogFunc ServerEnv where
  logFuncL  = serverMainEnv . logFuncL

instance HasMainEnv ServerEnv
--------------------------------------------------------------------------------

defaultTemplate :: Text -> Html -> Html
defaultTemplate title content = $(shamletFile "templates/template.hamlet")

defaultLayout title content = raw $ renderHtml $ defaultTemplate title content

paramMay n = (Just <$> param n) `rescue` const (return Nothing)
