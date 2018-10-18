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

{-# LANGUAGE TemplateHaskell #-}

module ADiff.Server.Prelude
  ( module ADiff.Prelude
  , module ADiff.Server.Prelude
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
import           ADiff.Data
import           ADiff.Prelude                 hiding (body, showError)
import           ADiff.Statistics
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
