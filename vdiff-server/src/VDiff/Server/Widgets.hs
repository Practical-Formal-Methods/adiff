{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module VDiff.Server.Widgets where

import qualified Data.Text            as T
import           VDiff.Server.Prelude

import           VDiff.Data
import qualified VDiff.Query2         as Q2

mkProgramLink :: Text -> Html
mkProgramLink hsh =
  let trunc = T.take 5 hsh
  in [shamlet| <a href="/program/#{hsh}">#{trunc}|]


-- | creates a program "widget"
mkProgramWidget :: (HasDatabase env) => Text -> RioActionM env Html
mkProgramWidget hash = do
  (Just program) <- lift $ Q2.programByHash hash
  return $(shamletFile "templates/widgets/source.hamlet")


mkPaginationWidget :: Int -> Int -> Int -> Text -> RioActionM env Html
mkPaginationWidget pageSize totalCount page qstring = do
  let numPages = totalCount `div` pageSize
      totalLinks = 10
      pref = [max (page - 5) 1 .. page - 1]
      pages = pref ++ [page .. min (page + ((totalLinks - 1) - (length pref))) numPages]
      showLeftArr = page > 1
      showRightArr = page < numPages
      prevPage = page - 1
      nextPage = page + 1
  return $(shamletFile "templates/widgets/pagination.hamlet")
