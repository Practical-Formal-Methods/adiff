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

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{- Custom prelude, to be used with all adiff projects -}
module ADiff.Prelude
  ( module RIO
  , module ADiff.Prelude.Types
  , (^.)
  , view
  -- * small utilities
  , nowISO
  , printD
  , displayList
  , formatCorrelation
  , sort
  -- * safe functions
  , readMay
  , headMay
  ) where

import           Control.Lens
import           Control.Lens.Operators
import           Data.List              (intersperse, sort)
import           Data.Ratio
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Format
import           Docker.Client
import           Numeric
import           Options.Applicative
import           RIO                    hiding (view, (^.))
import           Safe


import           ADiff.Prelude.Types


nowISO :: (MonadIO m) => m Text
nowISO =  do
  utc <- liftIO getCurrentTime
  let x = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) utc
  return $ T.pack x

printD :: (Display a, MonadIO m) => a -> m ()
printD = liftIO . T.putStrLn . utf8BuilderToText . display

displayList :: Display a => [a] -> Utf8Builder
displayList xs = mconcat $ intersperse ", " (map display xs)



-- formats rational numbers between 0.00 and 1.00 by rounding to up to 2 decimal
-- digits but ensures that a number that is bigger then 0 will never be rounded
-- to 0.00 and that a number smaller than 1 will never be rounded to 1.
formatCorrelation :: (Integral n) => (n, n) -> Text
formatCorrelation (num,denum) =
  if
    | denum == 0 ->  " "
    | s == "0.00" && num > 0 -> "> 0"
    | s == "1.00" && (num % denum) < 1 -> "< 1"
    | otherwise -> T.pack s
  where
    s = Numeric.showFFloat (Just 2) (fromIntegral num / fromIntegral denum) ""

instance Display ContainerID where
  display = display . T.take 12 . fromContainerID
