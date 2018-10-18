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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module ADiff.Server.Widgets where

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Text      as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as L
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy       as LT
import           Network.HTTP.Types
import           ADiff.Data
import qualified ADiff.Query2         as Q2
import           ADiff.Server.Prelude
import           ADiff.Statistics
import           ADiff.Verifier       (allVerifiers)

mkProgramLink :: ProgramId -> Html
mkProgramLink pid =
  let trunc = T.take 5 hsh
      hsh = programIdToHash pid
  in [shamlet| <a href="/program/#{hsh}">#{trunc}|]



mkPaginationWidget :: Int -> Int -> Int -> RioActionM env Html
mkPaginationWidget pageSize totalCount page = do
  let numPages = totalCount `div` pageSize
      totalLinks = 10
      pref = [max (page - 5) 1 .. page - 1]
      pages = pref ++ [page .. min (page + ((totalLinks - 1) - length pref)) numPages]
      showLeftArr = if page > 1 then "" else "disabled" :: Text
      showRightArr = if page < numPages then "" else "disabled" :: Text
      prevPage = page - 1
      nextPage = page + 1
  return $(shamletFile "templates/widgets/pagination.hamlet")


correlationTable :: Map (Relatee, Relatee) (Integer, Integer)
  -> (Relatee -> Relatee -> LT.Text)
  -> Html
correlationTable tbl mkLink = do
  let verifierNames  = (L.nub $ map fst $ Map.keys tbl) :: [Relatee]
  $(shamletFile "templates/widgets/correlationTable.hamlet")

mkBinaryComparisonTableWidget :: (HasDatabase env, HasLogFunc env) => VerifierName -> VerifierName -> Int -> Map (Verdict, Verdict) Int -> RioActionM env Html
mkBinaryComparisonTableWidget v1 v2 bigN table = do
  vns <- lift $ Q2.getVerifierNames
  return $(shamletFile "templates/widgets/binaryComparisonTable.hamlet")
  where
    renderCell vrd1 vrd2 =
      let v = fromJust (Map.lookup (vrd1,vrd2) table)
          q = Q2.ByVerdict [(RelateName v1, [vrd1]), (RelateName v2, [vrd2])]
          link = "findings?q=" <> (decodeUtf8 $ urlEncode False $ LBS.toStrict $ JSON.encode q)
      in [shamlet|<a class="tooltipped" href="#{link}" data-position="bottom" data-tooltip="#{v}"> #{ formatCorrelation (v, bigN)} |]


