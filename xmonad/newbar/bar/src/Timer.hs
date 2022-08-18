{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Timer (
  Period,
  Size(..),
  epoch,
  half
  ) where

import Control.DeepSeq
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)

import Utils

data Size = Size {x_ :: Int, y_ :: Int} deriving (Show, Eq, Generic, NFData)

type Period = NominalDiffTime

instance Num Size where
  (+) (Size x0 y0) (Size x1 y1) = Size (x0 + x1) (y0 + y1)
  (-) (Size x0 y0) (Size x1 y1) = Size (x0 - x1) (y0 - y1)
  (*) (Size x0 y0) (Size x1 y1) = Size (x0 * x1) (y0 * y1)
  abs (Size x y) = Size (abs x) (abs y)
  signum (Size x y) = Size (signum x) (signum y)
  fromInteger a = Size (fi a) (fi a)

half :: Size -> Size
half (Size w h) = Size (w `div` 2) (h `div` 2)

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

