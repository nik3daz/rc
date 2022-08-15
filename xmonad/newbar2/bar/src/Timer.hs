{-# LANGUAGE Arrows, DeriveGeneric, DeriveAnyClass, LambdaCase #-}

module Timer (
  timerTask,
  getRootTickEvents,
  Period,
  Timestamp,
  Size(..),
  RootChan,
  RootInput(..),
  TimerCollectionInp(..),
  epoch,
  half
  ) where

import Control.Applicative
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import Prelude hiding ((.), id)

import qualified Data.Map as M

import Utils
import Graphics.X11.Xlib

data Size = Size {x_ :: Int, y_ :: Int} deriving (Show, Eq, Generic, NFData)
data RootInput = RNop | RTick | RTitle String | RExpose Window 
                      | RMotion Window (Maybe Size)
                      | RClick Window Size
                      | RExit | RInit deriving (Show, Generic, NFData)
type RootChan = Chan RootInput

type Period = NominalDiffTime
type Timestamp = NominalDiffTime


data TimerInp = TNop | TDel | TTick Timestamp deriving Show
data TimerCollectionInp = TCNop
                        | TCChangeUsage [(Period,Int)]
                        | TCTick Timestamp
                        deriving Show

type TimerAuto = Interval IO TimerInp (Blip Timestamp)

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

getRootTickEvents :: RootInput -> IO (Maybe TimerCollectionInp)
getRootTickEvents  RTick = Just . TCTick . (`diffUTCTime` epoch) <$> getCurrentTime
getRootTickEvents _      = return Nothing

getTimerChangeEvents :: TimerCollectionInp -> Maybe [(Period, Int)]
getTimerChangeEvents (TCChangeUsage chg) = Just chg
getTimerChangeEvents _                   = Nothing

getTimerTickEvents :: (M.Map Period Int, TimerCollectionInp) -> M.Map Period TimerInp
getTimerTickEvents (mp, evt) =
  case evt of
     TCTick ts       -> fmap (conv $ TTick ts) mp
     TCChangeUsage _ -> fmap (conv TNop) mp
     _               -> M.empty
  where
     conv _ 0            = TDel
     conv dflt u | u > 0 = dflt
     conv _ _            = error "Negative usage"

timerCollection :: RootChan -> Auto IO (M.Map Period TimerInp) (M.Map Period (Blip Timestamp))
timerCollection ch = muxManyI_ (timerInit ch)

timerTask :: RootChan -> Auto IO TimerCollectionInp [(Period,UTCTime)]
timerTask ch = proc inpEvt -> do
    usages <- scanB_ updateUsages M.empty <<< emitJusts getTimerChangeEvents -< inpEvt
    timersTs <- timerCollection ch -< getTimerTickEvents (usages, inpEvt)
    let timestamps = filterTs timersTs
    id -< M.toList timestamps
  where
    updateUsages mp updates = M.unionWith (+) mp (M.fromList updates)
    filterTs = fmap ((`addUTCTime` epoch) . blip 0 id) . M.filter triggered
    triggered (Blip _) = True
    triggered NoBlip = False


timerInit :: RootChan -> Period -> TimerAuto 
timerInit rootCh period = mkAutoM_ $ \case
    TDel -> return (Nothing, timerInit rootCh period)
    _    -> do
      delayCh <- newChan 
      thr <- forkIO $ forever $ do
        targetTs <- (`addUTCTime` epoch) <$> readChan delayCh
        dt <- diffUTCTime targetTs <$> getCurrentTime
        when (dt > 0) $ threadDelay (round $ dt * 1000000)
        writeChan rootCh RTick
      writeChan delayCh 0 -- wake up asap
      return (Just NoBlip, tickTimer thr delayCh rootCh Nothing period)

-- Produces next timestamp for given timestamp and period
tickTimer :: ThreadId -> Chan NominalDiffTime -> RootChan -> Maybe Timestamp -> Period -> TimerAuto
tickTimer thr delayCh rootCh start period = mkAutoM_ $ \inp -> do
  let timeToNextTick :: NominalDiffTime -> NominalDiffTime
      timeToNextTick ts = period - snd (properFraction (ts/period)) * period
  -- print ("tickTimer", start, period, inp)

  let alignTick tick  = do
       let ts' = tick + timeToNextTick tick
       -- print ("Next wakeup ", tick, ts')
       writeChan delayCh ts'
       return (Just (Blip tick), tickTimer thr delayCh rootCh (Just ts') period)
  let nextTick start' = do
       -- print ("Next periodic ", start' + period)
       writeChan delayCh (start' + period)
       return (Just (Blip start'), tickTimer thr delayCh rootCh (Just $ start' + period) period)

  case inp of
    TNop -> return (Just NoBlip, tickTimer thr delayCh rootCh start period)
    TDel -> do
      print ("Kill thread", period)
      killThread thr
      return (Nothing, timerInit rootCh period)
    TTick tick -> case start of
      Nothing -> alignTick tick
      Just start' ->
        if tick > start'
        then if tick - start' > 2 * period
           then alignTick tick -- drop ticks
           else nextTick start'
        else return (Just NoBlip, tickTimer thr delayCh rootCh start period)

