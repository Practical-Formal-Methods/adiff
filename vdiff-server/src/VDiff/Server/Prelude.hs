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

--------------------------------------------------------------------------------

defaultTemplate :: Text -> Html -> Html
defaultTemplate title content = $(shamletFile "templates/template.hamlet")

defaultLayout title content = raw $ renderHtml $ defaultTemplate title content

