{-# LANGUAGE TemplateHaskell #-}

module VDiff.Persistence.Internal where

import           RIO

import           Data.FileEmbed
import           Data.Text.Encoding         (decodeUtf8)
import           Database.SQLite.Simple
import           Language.Haskell.TH.Syntax

-- just use the filename, e.g. unsoundness.sql.
-- The path is automatically added.
embedQuery :: FilePath-> Q Exp
embedQuery fn = do
  let fp = "assets/sql/" ++ fn
  e1 <- embedOneFileOf [fp, "vdiff/" ++ fp]
  let e2 = VarE 'decodeUtf8 `AppE` e1
  return $ ConE 'Query `AppE` e2
