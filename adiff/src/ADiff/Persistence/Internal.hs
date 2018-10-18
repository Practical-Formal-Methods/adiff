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

module ADiff.Persistence.Internal where

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
  e1 <- embedOneFileOf [fp, "adiff/" ++ fp]
  let e2 = VarE 'decodeUtf8 `AppE` e1
  return $ ConE 'Query `AppE` e2
