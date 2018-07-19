{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module VDiff.Server.Widgets where

import qualified Data.List            as L
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           VDiff.Server.Prelude

import           VDiff.Data
import qualified VDiff.Query2         as Q2
import           VDiff.Verifier       (allVerifiers)

mkProgramLink :: Text -> Html
mkProgramLink hsh =
  let trunc = T.take 5 hsh
  in [shamlet| <a href="/program/#{hsh}">#{trunc}|]



mkPaginationWidget :: Int -> Int -> Int -> Text -> Text -> RioActionM env Html
mkPaginationWidget pageSize totalCount page qstring qfstring = do
  let numPages = totalCount `div` pageSize
      totalLinks = 10
      pref = [max (page - 5) 1 .. page - 1]
      pages = pref ++ [page .. min (page + ((totalLinks - 1) - length pref)) numPages]
      showLeftArr = page > 1
      showRightArr = page < numPages
      prevPage = page - 1
      nextPage = page + 1
  return $(shamletFile "templates/widgets/pagination.hamlet")


correlationTable :: Map (VerifierName, VerifierName) (Integer, Integer)
  -> (VerifierName -> VerifierName -> Text)
  -> Html
correlationTable tbl mkLink = do
  let verifierNames  = L.nub $ map fst $ Map.keys tbl
  $(shamletFile "templates/widgets/correlationTable.hamlet")
