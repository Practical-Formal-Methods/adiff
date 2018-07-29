{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{- Custom prelude, to be used with all vdiff projects -}
module VDiff.Prelude
  ( module RIO
  , module VDiff.Prelude.Types
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


import           VDiff.Prelude.Types


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
