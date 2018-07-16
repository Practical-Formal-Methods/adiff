{- This module is basically a clone of boxes. Difference: org-mode format, yay! -}
{-# LANGUAGE ParallelListComp     #-}
{-# LANGUAGE UndecidableInstances #-}

module VDiff.Util.Tables where

import qualified Data.Text     as T
import           Prelude       (repeat)
import           VDiff.Prelude

-- all cells have height 1. Let's keep it simple

type Cell = Text


data Row = Row
  { cells           :: [Cell]
  , minColumnWidths :: [Int]
  } deriving Show

row :: [Cell] -> Row
row cells = Row cells widths
  where widths = map T.length cells

data Table = Table
  { rows         :: [Row]
  , columnWidths :: [Int]
  }


table :: [Row] -> Table
table rows_ = Table rows_ columnWidths
  where columnWidths = foldl' f (repeat 0) rows_
        f wds (Row _ columnWidths) = [max c c' | c <- wds | c' <- columnWidths]

renderTable :: Table -> Text
renderTable (Table rows columnWidths) = T.unlines $ map renderRow rows
  where
    renderRow :: Row -> Text
    renderRow (Row cells _) = "| " <> withHsep [c `padd` w | c <- cells | w <- columnWidths] <> " |"

padd :: Cell -> Int -> Text
padd t w = t <> T.replicate (w - T.length t) " "

withHsep :: [Text] -> Text
withHsep = T.intercalate  " | "


toTable :: ToCell a => [[a]] -> Table
toTable = table . map row . (fmap.fmap) toCell

--------------------------------------------------------------------------------
class ToRow a where
  toRow :: a -> Row

class ToCell a where
  toCell :: a -> Cell

instance (ToCell a, ToCell b) => ToRow (a,b) where
  toRow (a,b) = row [toCell a, toCell b]

instance (ToCell a, ToCell b, ToCell c) => ToRow (a,b,c) where
  toRow (a,b,c) = row [toCell a, toCell b, toCell c]

instance (ToCell a, ToCell b, ToCell c, ToCell d) => ToRow (a,b,c,d) where
  toRow (a,b,c,d) = row [toCell a, toCell b, toCell c, toCell d]

instance (Display a) => (ToCell a) where
  toCell = utf8BuilderToText . display
