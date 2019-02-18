-- Copyright 2012 Google Inc. All Rights Reserved.
-- Author: vol@google.com (Ivan Volosyuk)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State (StateT, execStateT, runStateT, forever, liftIO, put)
import Data.Bits
import Data.List (find)
import Data.Time
import Data.Maybe
import Foreign.C
import GHC.Ptr (nullPtr)
import GHC.Word (Word64)
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Numeric
import System.Exit
import Text.Printf (printf)
import System.Locale
import System.Process (runCommand, terminateProcess)
import System.IO.Error

import DzenParse
import Icon
import Top
import Utils

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Font
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))

defaultPadding = 4 :: Int
barX = 1920 :: Int
barY = 0
barHeight = 24 :: Int
barWidth = 1920 :: Int
backgroundColor = 0x2F1B40
backgroundColorString = "#2F1B40"
foregroundColorString = "#000000"
frameBackground = "#181838"
graphBackgroundColor = 0x181838
memColorTable = [0x00FF00, 0x0000FF]
netColorTable = [0x0000FF, graphBackgroundColor, 0x00FF00]
marginTop = 1 :: Int
marginBottom = 1 :: Int
marginsVertical = marginTop + marginBottom :: Int
defaultFontName = "-*-fixed-medium-r-normal--15-*-*-*-*-*-iso10646-*"
fixedFontName = "-*-courier new-*-r-normal-*-17-*-*-*-*-*-*-*"
defaultTimeFormat = "%R"
textPadding = 2
trayerCmd rightMargin = printf "trayer --expand false --edge top --align right\
             \ --widthtype request --height %d --margin %d --monitor 1 --tint 0x2f1b40 --transparent true --alpha 0" height rightMargin

wc = defaultWidget
loadWidgets :: [WidgetConfig]
loadWidgets = [
   clock,
   cpu,
   mem,
   -- battery,
   -- (net "eth0") { widgetWidth = 40, refreshRate = 1 },
   trayer,
   title { widgetX = 10 }
   ]

defaultWidget = WidgetConfig {
    makeWidget = \s -> zEmptyWidget s >>= zWrap s,
    widgetWidth = 60,
    widgetId = 0,
    refreshRate = 1.0,
    widgetX = 0,
    widgetHeight = barHeight,
    drawFrame = True,
    widgetTooltip = Nothing,
    fontName = defaultFontName,
    widgetPadding = defaultPadding,
    frameBackgroundColor = frameBackground,
    textColor = "#C7AE86",
    colorTable = [0x70FF70, 0xFF8080, 0xF020F0, 0x3030FF],
    timeFormat = defaultTimeFormat,
    onClick = Nothing
    }

defaultTooltip = defaultWidget {
   refreshRate = 3,
   widgetWidth = 350,
   frameBackgroundColor = "#FFFFC0",
   widgetPadding = 0,
   textColor = "#000000",
   fontName = fixedFontName}

cpu = defaultWidget { makeWidget = makeZCpuWidget, widgetTooltip = Just cpuTopTooltip, onClick = Just "top.sh" }
mem = defaultWidget { makeWidget = makeZMemWidget, colorTable = memColorTable,
                      widgetTooltip = Just memTooltip, refreshRate = 120, onClick = Just "top.sh" }
clock = defaultWidget { makeWidget = makeZClockWidget, widgetTooltip = Just clockTooltip, onClick = Just "clock.sh" }
battery = defaultWidget { makeWidget = makeZBatteryWidget, widgetWidth = 100, refreshRate = 3,
			   widgetTooltip = Just batteryTooltip }

net dev = defaultWidget { makeWidget = makeZNetWidget dev, 
                          colorTable = netColorTable,
                          widgetTooltip = Just $ netTooltip dev }
title = defaultWidget { makeWidget = makeZTitleWidget, refreshRate = 0, drawFrame = False }
trayer = defaultWidget { makeWidget = makeZTrayer }

clockTooltip = defaultTooltip { makeWidget = makeZClockWidget, refreshRate = 1, widgetWidth = 300,
                                timeFormat = "%a, %e %b %Y - %X"}
memTooltip = defaultTooltip { makeWidget = makeZMemInfo, refreshRate = 3, widgetWidth = 450, widgetHeight = barHeight * 4 }
netTooltip dev = defaultTooltip { makeWidget = makeZNetInfo dev, refreshRate = 3, widgetWidth = 400, widgetHeight = barHeight * 2 }
cpuTopTooltip = defaultTooltip { makeWidget = makeZCpuTop, widgetHeight = barHeight * 4, widgetWidth = 270, refreshRate = 3 }
batteryTooltip = defaultTooltip { makeWidget = makeZBatteryRate, refreshRate = 5, widgetWidth = 300 }

placeWidgets pos [] = [] -- FIXME: get rid of magic?
placeWidgets pos [last] = last { widgetWidth = pos - (widgetX last) }:[] -- last widget takes remaining space
placeWidgets pos (config:xs) =
  let newpos = pos - (widgetWidth config) - (widgetPadding config) in -- FIXME: use widgetWidth and initialize it here?
  config { widgetX = newpos } : placeWidgets newpos xs

initWidget rs wrs chan (widgetId, config) = do
  let config' = config { widgetId = widgetId }
  widget <- (makeWidget config') (config', rs, wrs, chan)
  return (widgetId, widget)

height = barHeight - marginsVertical :: Int

newtype IOBox a = IOBox { exec :: BoxIO a }
type BoxIO a = IO (a, IOBox a)

type ControlChan = Chan (Window,CInt)

type ExtendedState = Widget

data WidgetConfig = WidgetConfig {
  makeWidget :: MakeWidget,
  widgetWidth :: Int,
  widgetId :: CInt,
  refreshRate :: Double,
  widgetX :: Int,
  widgetHeight :: Int,
  drawFrame :: Bool,
  widgetPadding :: Int,
  widgetTooltip :: Maybe WidgetConfig,
  fontName :: String,
  frameBackgroundColor :: String, 
  textColor :: String,
  colorTable :: [Pixel],
  timeFormat :: String,
  onClick :: Maybe String
  }
data RenderState = RenderState { getDisplay :: Display, getDrawable::Pixmap, getGC :: GC}
data WindowRenderState = WindowRenderState { getRealWindow :: Window, getRealWindowGC :: GC }
data GlobalState = GlobalState { widgetsById :: M.Map Window (M.Map CInt Widget),
                                 mainWindow :: Window,
                                 tooltipWindow :: Maybe (Window, CInt), -- Window and creator id
                                 globalChan :: ControlChan
                               }

data Widget = Widget {
    onDraw :: IO (),
    doUpdate :: IO (ExtendedState),
    onDestroy :: IO (),
    widgetConfig :: WidgetConfig,
    getRenderState :: RenderState,
    getWindowRenderState :: WindowRenderState
}
type MakeWidget = (WidgetConfig, RenderState, WindowRenderState, ControlChan) -> IO Widget

drawWindow rs (WindowRenderState w gc) = do
  setfg rs backgroundColor
  fillRect rs 0 0 barWidth barHeight

setfg (RenderState dpy w gc) color = setForeground dpy gc color
setbg (RenderState dpy w gc) color = setBackground dpy gc color
fillRect (RenderState dpy win gc) x y w h = fillRectangle dpy win gc (fi x) (fi y) (fi w) (fi h)

drawWidget (widgetId,widget) = onDraw widget >>= \w -> return (widgetId, w)

data EventResult = Done | ExitEventLoop | Update GlobalState

updateWindow = updateWindowRegion 0 barWidth

updateWindowRegion x width widget = do
  let RenderState dpy buf buf_gc = (getRenderState widget)
  let WindowRenderState w gc = (getWindowRenderState widget)
  copyArea dpy buf w gc x 0 (fi width) (fi $ widgetHeight . widgetConfig $ widget)
                            (fi x) 0
  sync dpy False

handleMessage :: GlobalState -> Event -> IO EventResult
handleMessage _ (ClientMessageEvent {ev_data = (-1):_}) = return ExitEventLoop
handleMessage gState (ClientMessageEvent {ev_window = w, ev_data = widgetId:_}) = do
  -- print $ "Update for widget: " ++ (show widgetId)
  let widgetsMap = widgetsById gState
  case M.lookup w widgetsMap of
   Nothing -> return Done
   Just windowWidgetMap -> do
     --print $ "Window found!"
     case M.lookup widgetId windowWidgetMap of
       Nothing -> return Done
       Just widget -> do
         -- print $ "Widget found!"
         widget <- doUpdate widget
   
         onDraw widget 
         let wConf = widgetConfig widget
         updateWindowRegion (fi $ widgetX wConf) (fi $ widgetWidth wConf) widget
         let windowWidgetMap' = M.insert widgetId widget windowWidgetMap
         let widgetsMap' = M.insert w windowWidgetMap' widgetsMap
         return . Update $ gState { widgetsById = widgetsMap' }
  
handleMessage gState (ButtonEvent {ev_x = pos, ev_window = ww}) = do
  let widgetsMap = widgetsById gState
  case M.lookup ww widgetsMap of
    Nothing -> return Done
    Just windowWidgetMap -> do
      let mbWidget = find (containsPoint pos) $ M.toList windowWidgetMap
      case mbWidget of
        Nothing ->  return Done
        Just (widgetId, widget) -> do
           let cmd = onClick . widgetConfig $ widget
           case cmd of
             Nothing -> return Done
             Just cmdLine -> runCommand cmdLine >> return Done

handleMessage gState (ExposeEvent {ev_window = w}) = do
  let widgetsMap = widgetsById gState
  case M.lookup w widgetsMap of
    Nothing -> return Done
    Just windowWidgetMap -> do
      let widgets = M.toList windowWidgetMap
      let (_,widget) = head widgets
      drawWindow (getRenderState widget) (getWindowRenderState widget)
      mapM drawWidget widgets
      print "MapNotify: expensive redraw all"
      updateWindow widget
      return $ Update gState { widgetsById = M.insert w (M.fromList widgets) widgetsMap }

handleMessage gState (MotionEvent {ev_x = pos, ev_window = ww, ev_event_display = dpy}) = do
  updateTooltip dpy gState ww pos

handleMessage gState (CrossingEvent {ev_event_type = 7, ev_window = ww, ev_x = pos, ev_event_display = dpy}) = do -- EnterNotify
  updateTooltip dpy gState ww pos
 
handleMessage gState (CrossingEvent {ev_event_type = 8, ev_window = ww, ev_event_display = dpy}) = do -- LeaveNotify
  print "Leave! Destroying Window"
  destroyTooltip dpy gState >>= return . Update

handleMessage gState (AnyEvent {ev_event_type = 14}) = return Done -- NoExpose

handleMessage _ event = (print $ "Unhandled event:" ++ (eventName event) ++ ": " ++ (show event)) >> return Done


createTooltip dpy pos gState creatorId Nothing = return Done

createTooltip dpy pos gState creatorId (Just widgetConfig) = do
  print "Enter! Creating Window"
  let tooltipWidth = widgetWidth widgetConfig
  let scr = (defaultScreen dpy)
  let visual = defaultVisual dpy scr
  let winPos = (min barWidth ((fi pos) + (tooltipWidth `div` 2))) - tooltipWidth
      attrmask = cWOverrideRedirect
  w <- allocaSetWindowAttributes $ \attributes -> do
         set_override_redirect attributes True
         createWindow dpy (defaultRootWindow dpy) (fi winPos) (fi 26)
                    (fi tooltipWidth) (fi $ widgetHeight widgetConfig) 0 copyFromParent
                    inputOutput visual attrmask attributes

  tooltopAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_TOOLTIP" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi tooltopAtom]

  gc <- createGC dpy w
  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed

  buf <- createPixmap dpy w (fi barWidth) (fi $ widgetHeight widgetConfig) (defaultDepth dpy scr)
  buf_gc <- createGC dpy buf
  setBackground dpy gc backgroundColor
  setLineAttributes dpy gc 1 lineSolid capRound joinRound -- FIXME: use sane attributes for performance

  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  selectInput dpy w (structureNotifyMask .|. exposureMask)
  mapWindow dpy w
  sync dpy False
  flush dpy

  let rState = RenderState dpy buf buf_gc
  let wrState = WindowRenderState w gc
  widgets <- mapM (initWidget rState wrState (globalChan gState)) $ zip [1..] $ placeWidgets tooltipWidth [widgetConfig]
  let widgetsMap = (widgetsById gState)
  let widgetsMap' = M.insert w (M.fromList widgets) widgetsMap
  return . Update $ gState { widgetsById = widgetsMap', tooltipWindow = Just (w, creatorId) }

destroyTooltip dpy gState = do
  case tooltipWindow gState of
    Nothing -> return gState
    Just (w,_) -> do
      let widgetsMap = widgetsById gState
      let windowWidgetMap = fromMaybe M.empty (M.lookup w widgetsMap)
      mapM onDestroy . M.elems $ windowWidgetMap
      destroyWindow dpy w
      return gState { widgetsById = M.delete w widgetsMap, tooltipWindow = Nothing }

containsPoint pos (_, widget) =
  let pos' = fi pos;
      wConf = widgetConfig widget;
      width = widgetWidth wConf;
      x = widgetX wConf in
  pos' >= x && pos' < (x + width)

updateTooltip dpy gState ww pos = do
  let widgetsMap = widgetsById gState
  case M.lookup ww widgetsMap of
    Nothing -> return Done
    Just windowWidgetMap -> do
      let mbWidget = find (containsPoint pos) $ M.toList windowWidgetMap
      case (mbWidget, tooltipWindow gState) of
        (Nothing, Just _) ->  destroyTooltip dpy gState >>= return . Update
        (Just (widgetId, widget), Nothing) -> createTooltip dpy pos gState widgetId (widgetTooltip . widgetConfig $ widget)
        (Just (widgetId, widget), Just (w, creatorId))
            | widgetId == creatorId -> return Done
            | widgetId /= creatorId -> do
                 gState <- destroyTooltip dpy gState
                 createTooltip dpy pos gState widgetId (widgetTooltip . widgetConfig $ widget)
        otherwise -> return Done

eventLoop dpy gState = do
  res <- allocaXEvent $ \ev -> do
    nextEvent dpy ev
    event <- getEvent ev
    -- print $ show $ event
    handleMessage gState event

  case res of
    ExitEventLoop -> return gState
    Done -> eventLoop dpy gState
    Update gState' -> eventLoop dpy gState'

sendClientEvent d a w val = do
    allocaXEvent $ \e -> do
         setEventType e clientMessage
         setClientMessageEvent e w a 32 val currentTime
         sendEvent d w False structureNotifyMask e
    sync d False

copyChanToX chan = do
  d   <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  forever $ do
     (w,x) <- readChan chan
     -- print $ "Copy to X: " ++ (show x)
     sendClientEvent d a w (fi x) `catchIOError`  \x -> do
       print $ "Exception caught: " ++ (show x)
       return ()

main = do
  dpy <- openDisplay "" -- FIXME: proper way to get display name
  let scr = (defaultScreen dpy)
  let visual = defaultVisual dpy scr
  w <- createWindow dpy (defaultRootWindow dpy) (fi barX) (fi barY)
                        (fi barWidth) (fi barHeight)
                    0 copyFromParent inputOutput visual 0 nullPtr
  lowerWindow dpy w
  let strutValues = [0, 0, fi barHeight :: CLong, 0,
                     0, 0, 0, 0,
                     fi barX, 2000, 0, 0]
  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi dockAtom]

  gc <- createGC dpy w
  setBackground dpy gc backgroundColor -- FIXME: figure out if this is needed

  buf <- createPixmap dpy w (fi barWidth) (fi barHeight) (defaultDepth dpy scr)
  buf_gc <- createGC dpy buf
  setBackground dpy gc backgroundColor
  setLineAttributes dpy gc 1 lineSolid capRound joinRound -- FIXME: use sane attributes for performance

  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  selectInput dpy w (structureNotifyMask .|. buttonPressMask 
                 .|. enterWindowMask .|. leaveWindowMask .|. pointerMotionMask
                 .|. exposureMask)
  mapWindow dpy w
  sync dpy False
  flush dpy

  chan <- newChan
  xSetErrorHandler
  eventCopyThread <- forkOS $ copyChanToX chan

  let rState = RenderState dpy buf buf_gc
  let wrState = WindowRenderState w gc
  widgets <- mapM (initWidget rState wrState chan) $ zip [1..] $ placeWidgets barWidth loadWidgets

  let gState = GlobalState (M.insert w (M.fromList widgets) M.empty) w Nothing chan
  gState <- eventLoop dpy gState
  
  killThread eventCopyThread
  mapM onDestroy . concat . map M.elems . M.elems . widgetsById $ gState
  destroyWindow dpy w
  closeDisplay dpy

makeSegment (x,y) = Segment x' y0' x' y1' where
  x' = fi x
  y0' = (fi $ height - 1 + marginTop)
  y1' = (fi $ height - 1 + marginTop) - (fi y)

drawColorSegment rs@(RenderState dpy w gc) (segments, color) = do
  setfg rs color
  drawSegments dpy w gc segments

getCpuData = readFile "/proc/stat" >>= return . map(read) . words . head . lines
delta newar ar = map (pair (-)) $ zip newar ar
updateGraph samples sample = newSamples where
  newSamples = map (\(n,o) -> o++[n]) $ zip sample $ map (drop 1) samples
accumulate = scanl (+)
safe total = if total /= 0 then total else 1
makeLine total = fi . (`div` safe total) . (* (height - 1))

readBatteryFile = readKeyValueFile $ head . words
readNetFile = readKeyValueFile $ map read . words

dropZeros [] = []
dropZeros ((i,0):xs) = dropZeros xs
dropZeros ((i,v):xs) = (i,v) : dropZeros xs

data ZWidget s = ZWidget {
    zState :: s, -- mutable state
    zDoUpdate :: s -> IO s,
    zOnDraw :: s -> IO (),
    zOnDestroy :: s -> IO ()
}

toColor str = fst . head . readHex . tail $ str 

zEmptyWidget (config, rs, wrs, ch) = return $ ZWidget { zOnDraw = \x -> return (), zDoUpdate = return, zOnDestroy = \x -> return (), zState = () }

zAddFrame (config, rs, wrs, ch) z = return $ z { zOnDraw = draw' } where
  draw' = case drawFrame config of
    False -> zOnDraw z
    True -> draw''
  bg = toColor . frameBackgroundColor $ config
  x = widgetX config
  width = widgetWidth config
  draw'' s = do
    (zOnDraw z) s
    setfg rs bg
    fillRect rs x marginTop width ((widgetHeight config) - marginsVertical)


zThreadWithIntervals firstIntervals makeState run global@(config, rs, wrs, ch) localChan = do
  time <- getCurrentTime
  state <- makeState >>= \x -> return $! x
  let intervals = firstIntervals ++ (repeat $ refreshRate config)
  
  loop intervals time state where
    loop (interval:intervals) time state = do
      threadDelay (truncate $ 1000000 * interval)
      newTime <- getCurrentTime
      let dt = getDt newTime time
      (output, newstate) <- run dt state >>= \x -> return $! x
      writeChan ch ((getRealWindow wrs), (widgetId config))
      writeChan localChan output
      -- print $ "Delay: " ++ (show refresh)
      loop intervals newTime newstate

zThread makeState run global@(config, rs, wrs, ch) localChan =
 zThreadWithIntervals [0.0] makeState run' global localChan where
    run' = \_ -> run

zStatelessThread run = zThread (return ()) wrappedRun where
  wrappedRun _ = run >>= \output -> return (output, ())

zAddThreadFilter (thr, val) global@(config, rs, wrs, ch) z@(ZWidget init update draw destroy) = do
  clientChan <- newChan
  threadId <- forkIO $ thr global clientChan
  widget clientChan threadId where
    widget clientChan threadId = return $ ZWidget init' update' draw' destroy' where
      init' = (init, val)
     
      update' (s, val) = do
        s <- update s
        -- print $ "Pickup a value: " ++ (show s)
        newval <- readChan clientChan
        return (s, newval)

      draw' (s, val) = draw s
     
      destroy' (s, val) = do
        destroy s
        killThread threadId

zAddStaticValueFilter val global@(config, rs, wrs, ch) z@(ZWidget init update draw destroy) =
    return $ ZWidget init' update' draw' destroy' where
      init' = (init, val)
      update' (s, val) = update s >>= \s' -> return (s', val)
      draw' (s, val) = draw s
      destroy' (s, val) = destroy s

zGraphDisplayFilter global@(config, rs,wrs, ch) (ZWidget init@(s,sample) update draw destroy) =
     return $ ZWidget init' update' draw' destroy' where
  pos = widgetX config

  -- init' = (init, replicate (widgetWidth config) sample)
  init' = (init, replicate (length sample) $ replicate (widgetWidth config) 0)

  draw' (val, graph) = do
    draw val
    let segments = map (\a -> map makeSegment $ dropZeros $ zip [pos..] a) graph -- FIXME: segments using x position
    mapM (drawColorSegment rs) $ zip segments (colorTable config)
    return ()
     
  update' (s, graph) = do
    s@(_, newSample) <- update s
    return (s, updateGraph graph newSample)

  destroy' (s, val) = destroy s

zCpuWidget global@(config, rs, wrs, ch) z = 
  zAddThreadFilter (thr, val) global z >>= zGraphDisplayFilter global where
    val = replicate 4 0
    thr = zThread getCpuData $ \procData -> do
      newProcData <- getCpuData
      let procDelta = delta newProcData procData
      return (makeCpuSample procDelta, newProcData)

makeCpuSample :: [Int] -> [Dimension]
makeCpuSample (_ :user:nice:sys:idle:io:tail) = map (makeLine total) values where
  (total:values) = reverse $ accumulate 0 [sys + io, nice, user, idle]

zMemWidget global@(config, rs, wrs, ch) z = 
  zAddThreadFilter (thr, val) global z >>= zGraphDisplayFilter global where
    val = replicate 2 0
    thr = zStatelessThread $ do
      memState <- readBatteryFile "/proc/meminfo"
      return $ makeMemSample memState

makeMemSample input = map (makeLine total) values where
  total:free:cached:[] = map (read . (input !)) ["MemTotal", "MemFree", "Cached"]
  values = [total - free, total - free - cached]

getNetBytes input = [inbound, outbound] where
  atIndex idx = fi $ input !! idx
  inbound = atIndex 0
  outbound = atIndex 8

zNetWidget dev global@(config, rs, wrs, ch) z = 
  zAddThreadFilter (thr, val) global z >>= zGraphDisplayFilter global where
    val = replicate 3 0
    thr = zThread (readNetFile "/proc/net/dev") $ \netState -> do
      newNetState <- readNetFile "/proc/net/dev"
      let netDelta = delta (newNetState ! dev) (netState ! dev)
      -- print $ show $ makeNetSample netDelta
      return ((makeNetSample netDelta), newNetState)
    f x = log (x + 1)
    f2 x = (f x) * (f x)
    maxF = (*2) . f2 $ 10000000 * refreshRate config -- max rate 10 Mb/s
    makeNetSample input = map (makeLine total) values where
      [inbound, outbound] = map f2 . getNetBytes $ input
      total = truncate maxF
      values = map truncate [maxF, maxF - inbound, outbound] :: [Int]


zTitleWidget global@(config, rs, wrs, ch) z =
  zAddThreadFilter (thr, val) global z >>= zDzenDisplayFilter global where
    val = []
    (WindowRenderState win _) = wrs
    simpleThread = zStatelessThread (getLine >>= return . parseLine)
    thr global@(_, _, _, ch) localChan = simpleThread global localChan `catchIOError` exit where
      exit _ = do
        print "<End of input>"
        writeChan ch (win, (-1))

zTextDisplayFilter global@(config, rs,wrs, ch) z = do
  let dpy = getDisplay rs
  font <- xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) (fontName config)
  widget font where
    widget font = return $ z { zOnDraw = draw' } where
      draw' val@(s, msg) = do
        (zOnDraw z) val
        width <- xftTextExtents (getDisplay rs) font msg >>= return . xglyphinfo_width
        let padding = ((widgetWidth config) - width) `div` 2
        withDraw rs $ \d -> withColor (textColor config) rs $ \c ->
               xftDrawString d c font (widgetX config + padding) 18 msg

zMultilineTextDisplayFilter global@(config, rs,wrs, ch) z = do
  let dpy = getDisplay rs
  font <- xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) (fontName config)
  widget font where
    widget font = return $ z { zOnDraw = draw' } where
      x = widgetX config + textPadding
      draw' val@(s, strings) = do
        (zOnDraw z) val
        height <- xftTextExtents (getDisplay rs) font "Az^.#" >>= return . xglyphinfo_height
        
        --print msg
        withDraw rs $ \d -> withColor (textColor config) rs $ drawStrings strings height d
      drawStrings strings height d c = drawOne 0 strings where
        drawOne y [] = return ()
        drawOne y (str : xs) = do
          xftDrawString d c font x (y + 18) str
          drawOne (y + barHeight) xs


zDzenDisplayFilter global@(config, rs,wrs, ch) (ZWidget init update draw destroy) = do
  let dpy = getDisplay rs
  font <- xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) (fontName config)
  emptyIconCache <- makeIconCache dpy
  widget font emptyIconCache where
    widget font emptyIconCache = return $ ZWidget init' update' draw' destroy' where
      init' = (init, emptyIconCache)

      update' (s, iconCache) = do
        s'@(_, newMsg) <- update s
        iconCache' <- loadIcons iconCache newMsg
        return (s', iconCache')

      loadIcons iconCache [] = return iconCache
      loadIcons iconCache (IconRef winid:xs) = do
        iconCache' <- loadIconImage iconCache $ fi winid
        loadIcons iconCache' xs

      loadIcons iconCache (x:xs) = loadIcons iconCache xs

      destroy' (s, _) = destroy s

      draw' (s@(_, input), iconCache) = do
        draw s
        drawDzenXft font iconCache input rs config

withDefault defaultColor color = case color of
  "" -> defaultColor
  otherwise -> color

defaultVisualAndColormap rs = (dpy, w, vis, colormap) where
  (RenderState dpy w gc) = rs
  scr = defaultScreen dpy
  vis = defaultVisual dpy scr
  colormap = defaultColormap dpy scr

withDraw rs action = withXftDraw dpy w vis colormap action where
  (dpy, w, vis, colormap) = defaultVisualAndColormap rs

withColor color rs action = withXftColorName dpy vis colormap color action where
  (dpy, w, vis, colormap) = defaultVisualAndColormap rs

drawDzenXft font iconCache input rs wConf = do
  let pos = widgetX wConf
  cache2 <- withDraw rs $ \d -> do
    xftDrawSetClipRectangles d 0 0 [Rectangle (fi $ widgetX wConf) 0
                                    (fi $ widgetWidth wConf) (fi barHeight)]
    draw foregroundColorString backgroundColorString pos input d
  return () where
    (RenderState dpy w gc) = rs

    draw _ _ pos [] d = do
      let maxpos = (widgetX wConf) + (widgetWidth wConf)
      withColor backgroundColorString rs $ \c ->
        xftDrawRect d c pos 0 (maxpos - pos) barHeight
      return ()

    draw fg bg pos (Annotation Foreground fg' : xs) d = draw (withDefault foregroundColorString fg') bg pos xs d
    draw fg bg pos (Annotation Background bg' : xs) d = draw fg (withDefault backgroundColorString bg') pos xs d
    draw fg bg pos (IconRef winid : xs) d = do
      case getIconImage (fi winid) iconCache  of
        Nothing -> draw fg bg pos xs d

        Just (CachedIcon width height img) -> do
          putImage dpy w gc img 0 0 (fi pos) 0 (fi width) (fi height)
          draw fg bg (pos + width) xs d

    draw fg bg pos (Text msg : xs) d = do
      glyphInfo <- xftTextExtents dpy font msg
      withColor bg rs $ \c -> xftDrawRect d c pos 0 (xglyphinfo_xOff glyphInfo) barHeight
      withColor fg rs $ \c -> xftDrawString d c font (pos + xglyphinfo_x glyphInfo) 18 msg
      draw fg bg (pos + xglyphinfo_xOff glyphInfo) xs d



zBatteryWidget global@(config, rs, wrs, ch) z =
  zAddThreadFilter (thr, "") global z >>= zTextDisplayFilter global where
    thr = zStatelessThread $ do
      batteryInfo <- readBatteryFile "/proc/acpi/battery/BAT0/info"
      batteryState <- readBatteryFile "/proc/acpi/battery/BAT0/state"
      let capacity = read $ batteryInfo ! "design capacity"
          rate = read $ batteryState ! "present rate" :: Int
          remainingCapacity = read $ batteryState ! "remaining capacity"
          (h, m) = (remainingCapacity * 60 `div` rate) `divMod` 60
          percent = remainingCapacity * 100 `div` capacity
      return $ case batteryState ! "charging state" of
        "discharging" | rate /= 0 -> printf "%d%%(%d:%02d)" percent h m
        otherwise -> printf "%d%%C" percent

zBatteryRateWidget global@(config, rs, wrs, ch) z =
  zAddThreadFilter (thr, "") global z >>= zTextDisplayFilter global where
    thr = zStatelessThread $ do
      batteryState <- readKeyValueFile strip "/proc/acpi/battery/BAT0/state"
      return . printf " Present Rate: %s" $ batteryState ! "present rate"

getDt newTs ts = (/1e12) . fromIntegral . fromEnum $ diffUTCTime newTs ts

zClockWidget global@(config, rs, wrs, ch) z =
  zAddThreadFilter (thr, "") global z >>= zTextDisplayFilter global where
    thr = zStatelessThread $ do
      -- print "Clock!"
      time <- getCurrentTime
      timezone <- getCurrentTimeZone
      let localtime = utcToLocalTime timezone time
      return $ formatTime defaultTimeLocale (timeFormat config) localtime

loadavg = readFile "/proc/loadavg" >>= return . ("Load avg: "++)
                                       . join " " . take 3 . words

zTopWidget global@(config, rs, wrs, ch) z = do
  startVal <- loadavg
  zAddThreadFilter (thread, [startVal]) global z >>= zMultilineTextDisplayFilter global where
    thread = zThreadWithIntervals [0.6] pickCpuUsage makeOutput
    makeOutput dt cpuMap = do
      newCpuMap <- pickCpuUsage
      avg <- loadavg
      diff <- makeCpuDiff newCpuMap cpuMap dt
      return (avg:diff, newCpuMap)

zNetInfoWidget dev global@(config, rs, wrs, ch) z = 
  zAddThreadFilter (thread, ["..."]) global z >>= zMultilineTextDisplayFilter global where
    thread = zThreadWithIntervals [1.0] getInitialState makeOutput
    getInitialState = getNetState >>= \x -> return $! (x, [0, 0], 0.0)
    getNetState = readNetFile "/proc/net/dev" >>= \x -> return $! x

    makeOutput dt (netState, total, totalDt) = do
      newNetState <- getNetState
      let netDelta = delta (newNetState ! dev) (netState ! dev)
          curr@[inbound, outbound] = map (bytes . (perSec dt)) . getNetBytes $ netDelta
          total' = map (pair $ (+)) $ zip total $ getNetBytes netDelta
          totalDt' = totalDt + dt
          [avgIn, avgOut] = map (bytes . perSec totalDt') total'
          message = printf "In: %s/s : Out: %s/s" inbound outbound
          totalMessage = printf "Avg In: %s/s : Out: %s/s" avgIn avgOut
      return ([message, totalMessage], (newNetState, total', totalDt'))

zMemInfoWidget global@(config, rs, wrs, ch) z = 
  zAddThreadFilter (thread, ["..."]) global z >>= zMultilineTextDisplayFilter global where
    thread = zStatelessThread $ do
      x' <- readKeyValueFile ((`div` 1024) . read . head . words) "/proc/meminfo" :: IO (M.Map String Int)
      let x = M.insert "Swap" ((x M.! "SwapTotal") - (x M.! "SwapFree")) x'
      let values = ["MemFree", "Cached", "Buffers", "Swap"]
      let mem = map (\n -> printf "%7s: %4d MB" n (x M.! n)) values
      perPidInfo <- memInfo
      return perPidInfo
      return $ map (pair $ (++)) $ zip mem perPidInfo

zTrayerPlaceholder global@(config, rs, wrs, ch) z = do
  handle <- runCommand . trayerCmd $ barWidth - (widgetX config) - (widgetWidth config)
  widget handle where
    widget handle = return $ z { zOnDestroy = destroy' } where
      destroy' s = do
        (zOnDestroy z) s
        terminateProcess handle

testStrings = [
  "Load avg: 0.01 0.05 0.05",
  "   2% - (ghc)",
  "   2% - (bar)",
  "   1% - (plugin-containe)"]

makeZTitleWidget s = zEmptyWidget s >>= zAddFrame s >>= zTitleWidget s >>= zWrap s
makeZBatteryWidget s = zEmptyWidget s >>= zAddFrame s >>= zBatteryWidget s >>= zWrap s
makeZClockWidget s = zEmptyWidget s >>= zAddFrame s >>= zClockWidget s >>= zWrap s
makeZCpuWidget s = zEmptyWidget s >>= zAddFrame s >>= zCpuWidget s >>= zWrap s
makeZMemWidget s = zEmptyWidget s >>= zAddFrame s >>= zMemWidget s >>= zWrap s
makeZNetWidget dev s = zEmptyWidget s >>= zAddFrame s >>= zNetWidget dev s >>= zWrap s
makeZCpuTop s = zEmptyWidget s >>= zAddFrame s >>= zTopWidget s >>= zWrap s
makeZBatteryRate s = zEmptyWidget s >>= zAddFrame s >>= zBatteryRateWidget s >>= zWrap s
makeZNetInfo dev s = zEmptyWidget s >>= zAddFrame s >>= zNetInfoWidget dev s >>= zWrap s
makeZMemInfo s = zEmptyWidget s >>= zAddFrame s >>= zMemInfoWidget s >>= zWrap s
makeZTrayer s = zEmptyWidget s >>= zTrayerPlaceholder s >>= zWrap s

zWrap settings@(config, rs, wrs, ch) z@(ZWidget initialState update draw destroy) = return $ wrap initialState where
   wrap state = Widget {
     onDraw = draw state,
     doUpdate = update state >>= return . wrap,
     onDestroy = destroy state,
     widgetConfig = config,
     getRenderState = rs,
     getWindowRenderState = wrs
     }
