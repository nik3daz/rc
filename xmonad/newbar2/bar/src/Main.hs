{-# LANGUAGE Arrows, DeriveGeneric, DeriveAnyClass, LambdaCase #-}

import Control.Applicative
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Auto.Time
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bits
import Data.IORef
import Data.List
import Data.Map ((!))
import Data.Maybe
import Data.Time
import Data.Time.Zones
import Data.Word
import Foreign.Ptr
import GHC.Generics (Generic)
import Graphics.X11.Xft
import Graphics.X11.Xinerama
import Graphics.X11.Xlib hiding (Screen)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrender
import Numeric
import System.IO.Error
import System.Process (runCommand, terminateProcess)
import Text.Printf

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude hiding (id, (.))

import DzenParse
import Icon
import Timer
import Top
import Utils

barHeight :: Int
barHeight = 24

defaultFont :: String
defaultFont = "-*-latin modern sans-*-*-*-*-17-*-*-*-*-*-*-*"

barBackground :: String
barBackground = "#2F1B40"

infoBackground :: String
infoBackground = "#181838"

tooltipBackground :: String
tooltipBackground = "#FFFFC0"

defaultTextColor :: String
defaultTextColor = "#C7AE86"

trayerCmd :: Int -> Int -> String
trayerCmd = printf "trayer --expand false --edge top --align right\
             \ --widthtype request --height %d --margin %d --monitor 1 --transparent true --alpha 0 --tint 0x2F1B40"

-- Tooltip graphs refreshing while hidden
persistentTimers :: [Period]
persistentTimers = [batteryGraphTimer, netstatusRefresh]

bars :: [Bar]
bars = [bar1, bar2]
-- bars = [bar1]

bar1 :: Bar
bar1 = Bar barBackground barHeight (XineramaScreen 0) GravityTop [
        clock # TimeFormat "%R" # RefreshRate 60 # OnClick "clock.sh"
              # Width 60 # RightPadding 4
              # LocalTimeZone # BackgroundColor infoBackground
              # clockTooltip,
        logtm cpu # cpuTooltip # OnClick "top.sh",
        logtm mem # memTooltip,
        logtm (net "eno1"),
        trayer,

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000" # Width 5000
      ]

bar2 :: Bar
bar2 = Bar barBackground (barHeight) (XineramaScreen 1) GravityTop [
        clock # TimeFormat "%R" #
            Width 60 # RightPadding 4 #
            LocalTimeZone # BackgroundColor infoBackground #
            clockTooltip,

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

clockTooltip :: Tooltip
clockTooltip = Tooltip tooltipBackground (Size 460 (barHeight * 4)) Horizontal [
        frame Vertical [
                         tooltipClock #OtherTimeZone "GMT",
                         tooltipClock #OtherTimeZone "America/Los_Angeles"
                                       -- 0527 06:17:59.956236
                                      #TimeFormat "%m%d %H:%M:%S",
                         tooltipClock #OtherTimeZone "America/Los_Angeles",
                         tooltipClock
                       ] #Width 340,
        frame Vertical [
           tooltipLabel #Message "GMT: " #JustifyRight,
           tooltipLabel #Message "MTV TS: " #JustifyRight,
           tooltipLabel #Message "MTV: " #JustifyRight,
           tooltipLabel #Message "Local: " #JustifyRight
                       ]
 ]

cpuTooltip :: Tooltip
cpuTooltip = Tooltip tooltipBackground (Size 300 (8*barHeight)) Vertical [
     tooltip cpu #RefreshRate 1 #LinearTime #Height (2*barHeight)
                 #BottomPadding 1 #RightPadding 1 # LeftPadding 1,
     hseparator,
     tooltipText cpuTop
     ]

memTooltip :: Tooltip
memTooltip = Tooltip tooltipBackground (Size 490 (6*barHeight)) Horizontal [
     tooltipText memstatus #Width 430 #LeftPadding 5,
     tooltip mem #RefreshRate 1 #LogTime 3
     ]

netTooltip :: String -> Tooltip
netTooltip netdev = Tooltip tooltipBackground (Size 480 (2*barHeight)) Horizontal [
     tooltip (tooltipNet netdev) #RefreshRate 1 #LinearTime # Width 100
            # TopPadding 1 # BottomPadding 1,
     tooltipText (netstatus netdev) #RefreshRate 3 #JustifyLeft #LeftPadding 10
     ]

batteryGraphTimer :: Period
batteryGraphTimer = 75

batteryTooltip :: String -> Tooltip
batteryTooltip name = Tooltip tooltipBackground (Size 380 (7+barHeight*8)) Vertical [
     tooltip (batteryGraph name) #RefreshRate batteryGraphTimer #Height (barHeight*7)
             # BottomPadding 2  # LeftPadding 2 #RightPadding 2,
     hseparator,
     tooltipText (batteryRate name) # BottomPadding 3
     ]

logtm :: Widget -> Widget
logtm w = w # LogTime 8 # Width 129 # RefreshRate 1 -- One week worth of data

tooltip :: Widget -> Widget
tooltip w = w #BackgroundColor "#FFFFC0"
              #TopPadding 0 #BottomPadding 1 #LeftPadding 0 #RightPadding 1

tooltipText :: Widget -> Widget
tooltipText w = tooltip w  #TextColor "#000000"
              #SetFont "-*-courier new-*-r-normal-*-17-*-*-*-*-*-*-*"

tooltipClock :: Widget
tooltipClock = tooltipText clock #TimeFormat "%a, %e %b %Y - %X"

tooltipLabel :: Widget
tooltipLabel = tooltipText label

tooltipNet :: String -> Widget
tooltipNet netdev = Graph defaultAttr (GraphDef (SampleDef 1 $ Net netdev) (LogTime 8))
                    ["#6060FF", tooltipBackground, "#60FF60"]  # Width 129

data Attribute = Width Int | Height Int | LeftPadding Int | RightPadding Int
               | TopPadding Int | BottomPadding Int
               | TextColor Main.Color | BackgroundColor Main.Color
               | TimeFormat String | Message String | SetFont String
               | RefreshRate Period | OnClick String

type Color = String
type Font = String
type Pos = Size

type OnClickCmd = String

data Gravity = GravityTop | GravityBottom deriving (Show, Eq)
data Screen = DefaultScreen | XineramaScreen Int deriving (Show, Eq)
data Orientation = Horizontal | Vertical deriving (Show, Eq)

data ClockTimeZone = LocalTimeZone | OtherTimeZone String deriving (Show, Eq)
data Justify = JustifyLeft | JustifyMiddle | JustifyRight deriving (Show, Eq)
data Padding = Padding Size Size deriving (Show, Eq)
data TextAttributes = TextAttributes Main.Color Justify Main.Font Int deriving (Show, Eq)
data WidgetAttributes = WidgetAttributes {
  size :: Size,
  position :: Pos,
  padding :: Padding,
  color :: Main.Color,
  onclick :: Maybe OnClickCmd,
  mbtooltip :: Maybe Tooltip } deriving (Show, Eq)

-- int = n linear points before 2x compression
data GraphType = Cpu | Net String | Mem | Battery String deriving (Show,Eq,Ord, Generic, NFData)
data TimeScale = LinearTime | LogTime Int deriving (Show,Eq,Ord, Generic, NFData)
data SampleDef = SampleDef {period_ :: Period, type_ :: GraphType} deriving (Show, Eq, Ord, Generic, NFData)
data GraphDef = GraphDef {sample_ :: SampleDef, tscale_ :: TimeScale} deriving (Show, Eq, Ord, Generic, NFData)

data Bar = Bar String Int Screen Gravity [Widget] deriving Show
data Tooltip = Tooltip String Size Orientation [Widget] deriving (Show, Eq)

data Widget = Clock   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, fmt_ :: String, tz_ :: ClockTimeZone, refreshRate :: Period }
          | Label   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, label_ ::  String }
          | Title   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | CpuTop  {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period}
          | NetStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period, netdev_ :: String}
          | MemStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period}
          | Frame   {attr_ :: WidgetAttributes, frameOrientation :: Orientation, children :: [Widget]}
          | Graph   {attr_ :: WidgetAttributes, graph_ :: GraphDef, graphColorTable :: [String]}
          | BatteryStatus   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, batteryName :: String, refreshRate :: Period }
          | BatteryRate     {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, batteryName :: String, refreshRate :: Period }
          | Trayer  {attr_ :: WidgetAttributes}
          deriving (Show, Eq)

defaultAttr :: WidgetAttributes
defaultAttr = WidgetAttributes (Size 120 barHeight) 0 (Padding 1 1) infoBackground Nothing Nothing

defaultTAttr :: TextAttributes
defaultTAttr = TextAttributes defaultTextColor JustifyMiddle defaultFont barHeight

clock :: Widget
clock = Clock defaultAttr defaultTAttr "%R" LocalTimeZone 1

cpu :: Widget
cpu = Graph defaultAttr (GraphDef (SampleDef 1 Cpu) (LogTime 8)) ["#70FF70", "#FF8080", "#F020F0", "#3030FF"] -- # Width 129

mem :: Widget
mem = Graph defaultAttr (GraphDef (SampleDef 1 Mem) (LogTime 8)) ["#00FF00", "#6060FF"] -- # Width 129

batteryGraph :: String -> Widget
batteryGraph name = Graph defaultAttr (GraphDef (SampleDef 1 (Battery name)) LinearTime) ["#0760F2"]

net :: String -> Widget
net netdev = Graph defaultAttr (GraphDef (SampleDef 1 (Net netdev)) (LogTime 8))
             ["#6060FF", infoBackground, "#60FF60"] # Width 129 #netTooltip netdev

netstatusRefresh :: NominalDiffTime
netstatusRefresh = 3

netstatus :: String -> Widget
netstatus = NetStatus defaultAttr defaultTAttr netstatusRefresh

memstatus :: Widget
memstatus = MemStatus defaultAttr defaultTAttr 1 #JustifyLeft

label :: Widget
label = Label defaultAttr defaultTAttr ""

hseparator = Label (WidgetAttributes (Size 1000 1) 0 (Padding 1 1) "#c0c0c0" Nothing Nothing)
                   defaultTAttr "" # LeftPadding 20 # RightPadding 20

title :: Widget
title = Title defaultAttr defaultTAttr

cpuTop :: Widget
cpuTop = CpuTop defaultAttr defaultTAttr 3 # JustifyLeft

trayer :: Widget
trayer = Trayer defaultAttr # Width barHeight

battery :: String -> Widget
battery name = BatteryStatus defaultAttr defaultTAttr name 10 # Width 120 # batteryTooltip name

batteryRate :: String -> Widget
batteryRate name = BatteryRate defaultAttr defaultTAttr name 1

frame :: Orientation -> [Widget] -> Widget
frame = Frame (WidgetAttributes (Size 5000 barHeight) 0 (Padding 0 0)
                               "#181838" Nothing Nothing)

class Apply a where
  apply :: a -> Widget -> Widget

instance Apply ClockTimeZone where
  apply tz ww = ww { tz_ = tz }

instance Apply Justify where
  apply j ww = let TextAttributes c _ f hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}

instance Apply TimeScale where
  apply t ww = let g = graph_ ww in ww { graph_ = g { tscale_ = t } }

instance Apply Tooltip where
  apply tip ww = let WidgetAttributes ws x p c cmd _ = attr_ ww
                   in ww { attr_ = WidgetAttributes ws x p c cmd (Just tip) }

withAttr :: Widget -> (WidgetAttributes -> WidgetAttributes) -> Widget
withAttr ww f = ww { attr_ = f (attr_ ww) }

withPadding :: Widget -> (Padding -> Padding) -> Widget
withPadding ww f = withAttr ww $ \wa -> wa { padding = f (padding wa) }

instance Apply Attribute where
  apply (TextColor c) ww = let TextAttributes _ j f hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}
  apply (SetFont f) ww = let TextAttributes c j _ hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}

  apply (Width w) ww = withAttr ww $ \wa -> wa { size = Size w (y_ . size $ wa)}
  apply (Height h) ww = withAttr ww $ \wa -> wa { size = Size (x_ . size $ wa) h}
  apply (OnClick cmd) ww = withAttr ww $ \wa -> wa { onclick = Just cmd }

  apply (LeftPadding l) ww = withPadding ww $ \p -> let Padding (Size _ t) pbr = p in Padding (Size l t) pbr
  apply (TopPadding t) ww = withPadding ww $ \p -> let Padding (Size l _) pbr = p in Padding (Size l t) pbr
  apply (RightPadding r) ww = withPadding ww $ \p -> let Padding plt (Size _ b) = p in Padding plt (Size r b)
  apply (BottomPadding b) ww = withPadding ww $ \p -> let Padding plt (Size r _) = p in Padding plt (Size r b)
  apply (BackgroundColor c) ww = withAttr ww $ \attr -> let WidgetAttributes ws x p _ cmd tip = attr in
                                                     WidgetAttributes ws x p c cmd tip
  apply (TimeFormat fmt) ww = ww { fmt_ = fmt }
  apply (Message s) ww = ww { label_ = s }
  apply (RefreshRate r) ww@(Graph _ def@GraphDef{sample_ = s} _) = ww { graph_ = def {sample_ = s { period_ = r }}}
  apply (RefreshRate r) ww = ww { refreshRate = r }

infixl 9 #
(#) :: (Apply a) => Widget -> a  -> Widget
(#) w a = apply a w

makeFont :: RenderState -> TextAttributes -> IO XftFont
makeFont RenderState { display = dpy } (TextAttributes _ _ fontName _) =
  xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) fontName

localTimezone :: IO (UTCTime -> IO ZonedTime)
localTimezone = do
  timezone <- getCurrentTimeZone
  return $ \utcTime -> do
    let localTime = utcToLocalTime timezone utcTime
    return $ ZonedTime localTime timezone

otherTimezone :: String -> IO (UTCTime -> IO ZonedTime)
otherTimezone timezone = do
  tz <- loadSystemTZ timezone
  return $ \utcTime -> do
    let timeZone = timeZoneForUTCTime tz utcTime
    let localTime = utcToLocalTime timeZone utcTime
    return $ ZonedTime localTime timeZone

formatClock :: String -> (UTCTime -> IO ZonedTime) -> IO String
formatClock fmt zoned = do
  time <- getCurrentTime
  zonedTime <- zoned time
  return $ formatTime Data.Time.defaultTimeLocale fmt zonedTime

type ControlChan = Chan RootInput

data RenderState = RenderState {
  display :: Display,
  window :: Window,
  buffer :: Pixmap,
  gc_ :: GC,
  windowWidth :: Int,
  windowHeight :: Int,
  windowBackground :: String,
  pos :: (Int, Int, Int, Int, Int, Int)
}

instance Eq RenderState where
  (==) a b = (window a) == (window b)

data WindowState = WindowState RenderState [Widget]

instance Show RenderState where
  show RenderState {} = "RenderState "

visualColormap :: Display -> (Visual, Colormap)
visualColormap dpy = (vis, colormap) where
  scr = defaultScreen dpy
  vis = defaultVisual dpy scr
  colormap = defaultColormap dpy scr

withDraw :: RenderState  -> (XftDraw -> IO a) -> IO a
withDraw  RenderState { display = dpy, buffer = w } = withXftDraw dpy w vis colormap where
  (vis, colormap) = visualColormap dpy

withColor :: Display -> String -> (XftColor -> IO ()) -> IO ()
withColor dpy = withXftColorName dpy vis colormap where
  (vis, colormap) = visualColormap dpy


drawMessage :: RenderState -> WidgetAttributes -> TextAttributes
               -> XftFont -> XftDraw -> (IconCache, Size)
               -> Message -> IO (IconCache, Size)
drawMessage rs attr tattr font d (icons, off) (Text fg bg msg) = do
  let WidgetAttributes sz pos  _ wbg _ _ = attr
  let TextAttributes wfg justify _ ths = tattr
  let dpy = display rs
  glyphInfo <- xftTextExtents dpy font msg
  let (Size ws _, Size x y, Size xoff yoff) = (sz, pos, off)
  let [dx, dy, txoff] = map ($ glyphInfo) [
       xglyphinfo_x, xglyphinfo_y, xglyphinfo_xOff]
  let x' = x + case justify of
                  JustifyLeft -> dx
                  JustifyMiddle -> (ws - txoff) `div` 2
                  JustifyRight ->  ws - txoff
  let y' = y + ((ths + dy) `div` 2)
  let fg' = fromMaybe wfg fg
  drawRect dpy d (fromMaybe wbg bg) (x + xoff) (y + yoff) ws ths
  withColor dpy fg' $ \c -> xftDrawString d c font (x' + xoff) (y' + yoff) msg
  return (icons, Size (xoff + txoff) yoff)

drawMessage rs attr tattr _ _ (icons, off) (IconRef icon) = do
  let (Size x y, Size xoff yoff) = (position attr, off)
  let TextAttributes _ _ _ ths = tattr

  (icons', CachedIcon width height img) <- loadIconImage icons icon
  putImage (display rs) (buffer rs) (gc_ rs)
           img 0 0 (fi $ x + xoff) (fi $ y + yoff + ((ths - height) `div` 2)) (fi width) (fi height)
  return (icons', Size (xoff + width) yoff)

drawMessages :: RenderState -> WidgetAttributes -> TextAttributes
             -> XftFont -> XftDraw -> (IconCache, Int) -> String -> IO (IconCache, Int)
drawMessages rs attr tattr font d (icons, yoff) msg = do
  let TextAttributes _ _ _ ths = tattr
  (icons', _) <- foldM (drawMessage rs attr tattr font d) (icons, Size 0 yoff) (parseLine msg)
  return (icons', yoff + ths)

type GraphSample = [Int]
data GraphData = LinearGraph ![GraphSample] | LogGraph Int ![[GraphSample]] deriving (Show, Generic, NFData)

makeGraph :: TimeScale -> Int -> Int -> GraphData
makeGraph LinearTime ws l = LinearGraph $! replicate ws $ replicate l 0
makeGraph (LogTime n) ws _ = LogGraph n $! replicate (1 + ws `div` n) []

avgSamp :: [GraphSample] -> GraphSample -- (a + b + 1) /2
avgSamp ar = ar `deepseq` map (\v -> (sum v + 1) `div` length v) $ transpose ar

updateGraph :: Int -> GraphData -> GraphSample -> GraphData
updateGraph ws (LinearGraph g) s = g `deepseq` LinearGraph $! s : take ws g
updateGraph _ (LogGraph n g) s = g `deepseq` LogGraph n $ updateLayer g s where
  updateLayer :: [[GraphSample]] -> GraphSample -> [[GraphSample]]
  updateLayer [] _ = []
  updateLayer (vv:xs) v
    | length vv == (n+2) = let (a,b) = splitAt n vv in (v:a):updateLayer xs (avgSamp b)
    | otherwise = (v:vv):xs

exportGraph :: GraphData -> [[Int]]
exportGraph (LinearGraph g) = g
exportGraph (LogGraph n g) = concatMap (take' n) g where
  take' n' ar = let (a,b) = splitAt (n'-1) ar in a ++ (case b of
       [] -> []
       _ -> [avgSamp b])

readBatteryFile :: FilePath -> IO (M.Map String String)
readBatteryFile = readKeyValueFile $ head . words

readNetFile :: FilePath -> IO (M.Map String [Integer])
readNetFile = readKeyValueFile $ map read . words

readFileWithFallback :: String -> IO String
readFileWithFallback x = readFully x `catchIOError` \_ -> return "0"


batteryFile :: String -> String -> String
batteryFile n x = "/sys/class/power_supply/" ++ n ++ "/" ++ x

readBatteryString :: String -> String -> IO String
readBatteryString n x = (head . lines) <$> readFileWithFallback (batteryFile n x)

readBatteryInt :: String -> String -> IO Int
readBatteryInt n x = read <$> readBatteryString n x :: IO Int

getDt :: Fractional t => UTCTime -> UTCTime -> t
getDt newTs ts = (/1e12) . fromIntegral . fromEnum $ diffUTCTime newTs ts

getNetBytes :: (Num t, Integral a) => [a] -> [t]
getNetBytes input = [inbound, outbound] where
  atIndex idx = fi $ input !! idx
  inbound = atIndex 0
  outbound = atIndex 8

makeSegment y0 height (x,y) = Segment x' y0' x' y1' where
  x' = fi x
  y0' = fi $ height + y0
  y1' = fi $ height + y0 - fi y

drawColorSegment :: RenderState -> ([Segment], Pixel) -> IO ()
drawColorSegment rs (segments, color) = do
  let RenderState {display = dpy, buffer = w, gc_ = gc} = rs
  setForeground dpy gc color
  drawSegments dpy w gc segments

toColor = fst . head . readHex . tail
drawRect dpy d bg x y w h = withColor dpy bg $ \c -> xftDrawRect d c x y w h

flp o = if o == Horizontal then Vertical else Horizontal
dir o = if o == Horizontal then Size 1 0 else Size 0 1
clamp (Size w h) = Size (max 0 w) (max 0 h)

reserve :: WidgetAttributes -> Size -> Size -> Orientation -> (WidgetAttributes, Size)
reserve wa wpos wsz ort =
  let WidgetAttributes sz _ p@(Padding plt prb) bg cmd tip = wa
      newpos = clamp $ wsz - dir ort * (sz + plt + prb)
      sz' = wsz - dir ort * newpos - plt - prb
      pos' = wpos + dir ort * newpos + plt
      in (WidgetAttributes sz' pos' p bg cmd tip, newpos)

layoutTooltip :: Maybe Tooltip -> Maybe Tooltip
layoutTooltip Nothing = Nothing
layoutTooltip (Just (Tooltip bg sz orien wds)) = Just $ Tooltip bg sz orien $ layoutWidgets orien sz wds

layoutWidget :: Size -> Orientation -> Size -> Widget -> (Size, [Widget])
layoutWidget wpos ort wsz (Frame wa cort cwds) =
  let (WidgetAttributes ws pos _ _ _ _, newpos) = reserve wa wpos wsz ort
      cwds' = concat $ snd $ mapAccumL (layoutWidget pos cort) ws cwds
   in (newpos, cwds')

layoutWidget wpos ort wsz wd =
  let (wa', newpos) = reserve (attr_ wd) wpos wsz ort
      wa'' = wa' { mbtooltip = layoutTooltip $mbtooltip wa' }
   in (newpos, [wd { attr_ = wa'' }])

layoutWidgets :: Orientation -> Size -> [Widget] -> [Widget]
layoutWidgets orien wsz wds =
  let (pos, ar) = mapAccumL (layoutWidget (Size 0 0) orien) wsz wds
      (first : others) = reverse $ concat ar
      change = pos * dir orien
      update wa = wa { position = position wa - change, size = size wa + change } in
  (withAttr first update : others)

windowMapAndSelectInput :: Display -> Window -> Word64 -> IO ()
windowMapAndSelectInput dpy w mask = do
  selectInput dpy w mask
  mapWindow dpy w
  sync dpy False
  flush dpy

sendClientEvent :: Display -> Atom -> Window -> Atom -> IO ()
sendClientEvent d a w val = do
    allocaXEvent $ \e -> do
         setEventType e clientMessage
         setClientMessageEvent e w a 32 val currentTime
         sendEvent d w False structureNotifyMask e
    sync d False

copyChanToX :: ControlChan -> Window -> IO ()
copyChanToX chan w = do
  ch <- dupChan chan
  d <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  forever $ do
     _ <- readChan ch
     sendClientEvent d a w 0 `catchIOError`  \x -> do
       print $ "Exception caught: " ++ show x
       sync d False


checkBattery wd n = do
  r <- tryIOError $ readFully $ batteryFile n "status"
  case r of
    Left _ -> print ("Unknown battery", n) >> return Nothing
    Right _ -> return (Just wd)

checkNetdev wd n = do
  net <- readNetFile "/proc/net/dev"
  return $ if M.member n net then Just wd else Nothing

removeBroken wd@Graph {graph_ = GraphDef {sample_ = SampleDef {type_ = Battery n}}} = checkBattery wd n
removeBroken wd@BatteryRate {batteryName = n} = checkBattery wd n
removeBroken wd@BatteryStatus {batteryName = n} = checkBattery wd n
removeBroken wd@Graph {graph_ = GraphDef {sample_ = SampleDef {type_ = Net n}}} = checkNetdev wd n
removeBroken wd@NetStatus {netdev_ = n} = checkNetdev wd n
removeBroken wd = return $ Just wd


makeBar :: Display -> ControlChan -> Bar -> IO WindowState
makeBar dpy controlCh (Bar bg height screen gravity wds) = do

  let scr = defaultScreen dpy
  xiscr <- case screen of
       DefaultScreen -> return Nothing
       XineramaScreen x -> maybe Nothing (find (\s -> (xsi_screen_number s) == fi x))
                          <$> xineramaQueryScreens dpy
  forM_ xiscr $ \a -> print a

  let (scX, scY, scWidth, scHeight) = case xiscr of
       Nothing -> (0, 0, displayWidth dpy scr, displayHeight dpy scr)
       Just xi -> (xsi_x_org xi, xsi_y_org xi, fi (xsi_width xi), fi (xsi_height xi))

  -- left, right, top, bottom,
  -- left_start_y, left_end_y, right_start_y, right_end_y,
  -- top_start_x, top_end_x, bottom_start_x, bottom_end_x
  let (y, strutValues) = if gravity == GravityTop
      then (scY,    [0, 0, fi scY + fi height, 0,
                     0, 0, 0, 0,
                     fi scX, fi scX + fi scWidth - 1, 0, 0])
      else (scY + fi scHeight - fi height,
                    [0, 0, 0, fi scY + fi height,
                     0, 0, 0, 0,
                     0, 0, fi scX, fi scX + fi scWidth - 1])

  rootwin <- rootWindow dpy scr
  w <- createWindow dpy rootwin
                    (fi scX) (fi y) (fi scWidth) (fi height)
                    0 copyFromParent inputOutput (defaultVisual dpy scr) 0 nullPtr
  gc <- createGC dpy w
  buf <- createPixmap dpy w (fi scWidth) (fi height) (defaultDepth dpy scr)

  let rs = RenderState { display = dpy, window = w, buffer = buf, gc_ = gc,
                         windowWidth = fi scWidth, windowHeight = height,
                         windowBackground = bg,
                         pos = (fi scX, fi y, fi scX, fi scY, fi scWidth, fi scHeight)}

  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi dockAtom]

  windowMapAndSelectInput dpy w $ exposureMask
                              .|. structureNotifyMask
                              .|. buttonPressMask
                              .|. enterWindowMask
                              .|. leaveWindowMask
                              .|. pointerMotionMask

  wds' <- catMaybes <$> mapM removeBroken wds
  let widgets = layoutWidgets Horizontal (Size (fi scWidth) height) wds'
  return $ WindowState rs widgets

getMotionEvent :: RootInput -> Maybe (Window, Maybe Size)
getMotionEvent (RMotion w pos) = Just (w, pos)
getMotionEvent _ = Nothing

getClickEvent :: RootInput -> Maybe (Window, Maybe Size)
getClickEvent (RClick w pos) = Just (w, Just pos)
getClickEvent _ = Nothing

layers Cpu = 3
layers (Net _) = 3
layers Mem = 2
layers (Battery _) = 1

drops Mem = 0
drops _ = 1

scaleG hs (total:vals) = map ((`div` (if total == 0 then 1 else total)) . (*hs)) vals
readCPU = map read . tail. words . head . lines <$> readFully "/proc/stat"

readNet :: String -> IO ([Int], UTCTime)
readNet netdev = do
  ts <- getCurrentTime
  net <- readNetFile "/proc/net/dev"
  let inout = ($!) getNetBytes (net ! netdev)
  return (inout, ts)

f3 :: Double -> Double
f3 x = f x * f x * f x where
  f x = log (x + 1)

revf3 :: Double -> Double
revf3 x = exp (x ** (1/3)) - 1 + 0.5

sampleTask :: GraphType -> Auto IO Period [Int]
sampleTask Cpu = seqer <<< proc t -> id <<< mkState calcCpu [0,0,0,0,0] <<< seqer <<< effect readCPU -< t where
   calcCpu n o =
     let (user:nice:sys:idle:io:_) = ($!) zipWith (-) n o
      in ([sys + io, nice, user, idle], n)

sampleTask Mem = mkConstM $ do
    mem <- readBatteryFile "/proc/meminfo"
    let [total,free,cached] = ($!) map (read . (mem !))
           ["MemTotal", "MemFree", "Cached"]
        out = [total - free - cached, cached, free]
    out `deepseq` return out

sampleTask (Battery n) = mkConstM $ do
    capacity <- readBatteryInt n "energy_full"
    remainingCapacity <- readBatteryInt n "energy_now"
    let out = [remainingCapacity, capacity - remainingCapacity]
    out `deepseq` return out

sampleTask (Net netdev) =
    seqer <<< proc t -> id <<< mkState_ calcNet ([0,0], epoch) <<< effect (readNet netdev) -< t
  where
    calcNet :: ([Int], UTCTime) -> ([Int], UTCTime) -> ([Int], ([Int], UTCTime))
    calcNet (n, nts) (o, ots) =
      let f x = log (x + 1)
          f3 x = f x * f x * f x
          dt = getDt nts ots :: Double
          [inbound, outbound] = ($!) map (f3 . fromIntegral) $ zipWith (-) n o :: [Double]
          maxspeed = f3 $ dt * 100000000 :: Double
          output = ($!) map (truncate . max 0)
              [inbound, maxspeed - inbound - outbound, outbound, 0] :: [Int]
       in (output, (n, nts)) :: ([Int], ([Int], UTCTime))

createSample :: SampleDef -> Auto IO (Period, UTCTime) (Blip (SampleDef, [Int]))
createSample sampdef@(SampleDef period typ) = proc t -> do
    matchPeriod <- emitOn (period ==) -< fst t
    sample <- perBlip (sampleTask typ) -< matchPeriod
    id <<< seqer <<< dropB (drops typ) -< fmap (\s -> (sampdef, s)) sample

createGraph :: (GraphDef,Int) -> Auto IO (Blip (SampleDef, [Int])) (Maybe (GraphDef, GraphData))
createGraph (def@(GraphDef sample tscale), ws) = proc samp -> do
    matchSamp <- filterB (\s -> fst s == sample) -< samp
    graph <- perBlip getGraph <<< seqer -< fmap snd matchSamp
    id <<< seqer <<< asMaybes -< fmap (\x -> (def, x)) graph
  where
    getGraph = proc samp ->
      id <<< seqer <<< accum_ (($!) updateGraph ws) (($!) makeGraph tscale ws (layers Cpu)) -< samp


dump :: (Show a) => String -> Auto IO a ()
dump msg = mkFuncM $ \inp -> print (msg, inp)

mergeRect (Rectangle x0 y0 w0 h0) (Rectangle x1 y1 w1 h1) = Rectangle x y (fi w) (fi h)
  where
  calc a0 a1 b0 b1 = let (mn, mx) = (min a0 a1, max b0 b1) in (mn, mx - mn)
  (x, w) = calc x0 x1 (x0+fi w0) (x1+fi w1)
  (y, h) = calc y0 y1 (y0+fi h0) (y1+fi h1)

combinePaints paints = M.elems $ foldr f M.empty paints where
  f (rs, rect) = M.insertWith merge (window rs) (rs, rect)
  merge (rs, rect1) (_, rect2) = (rs, mergeRect rect1 rect2)

data ZEvent = ZNop | REv !RootInput | TEv !(Period, UTCTime)
                   | GEv !(GraphDef, GraphData) deriving (Show, Generic, NFData)

getRefreshRate Graph {graph_ = GraphDef {sample_ = SampleDef {period_ = p}}} = Just p
-- FIXME: reduce duplication
getRefreshRate CpuTop { refreshRate = p } = Just p
getRefreshRate Clock { refreshRate = p } = Just p
getRefreshRate NetStatus { refreshRate = p } = Just p
getRefreshRate MemStatus { refreshRate = p } = Just p
getRefreshRate BatteryStatus { refreshRate = p } = Just p
getRefreshRate BatteryRate { refreshRate = p } = Just p
getRefreshRate _ = Nothing

createTooltip :: Display -> RenderState
              -> Widget -> Tooltip
              -> IO ((WindowDraw,[Period]), Window)
createTooltip dpy parent_rs pwd tip = do
  let Tooltip bg tsz@(Size width height) orien widgets = tip
  let (px, py, scX, scY, scWidth, scHeight) = (pos parent_rs)

  let WidgetAttributes sz pos _ _ _ _ = attr_ pwd
  let wpos = Size px py
  let place = if y_ wpos == scY
                 then wpos + pos + sz * dir Vertical - half (tsz-sz) * dir Horizontal - Size 0 0
                 else wpos + pos - half (tsz-sz) * dir Horizontal - tsz * dir Vertical + Size 0 0
  let x = max scX $ min (x_ place) (scX + scWidth - width)
  let scr = defaultScreen dpy

  print $ "Enter! Creating Window " ++ show place ++ " wpos " ++ show wpos ++ " size " ++ show tsz

  let visual = defaultVisual dpy scr
      attrmask = cWOverrideRedirect
  w <- allocaSetWindowAttributes $ \attributes -> do
         set_override_redirect attributes True
         createWindow dpy (rootWindowOfScreen (defaultScreenOfDisplay dpy))
                    (fi x) (fi $ y_ place)
                    (fi width) (fi height) 0 copyFromParent
                    inputOutput visual attrmask attributes

  tooltopAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_TOOLTIP" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi tooltopAtom]

  gc <- createGC dpy w
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  buf <- createPixmap dpy w (fi width) (fi height) (defaultDepth dpy scr)

  windowMapAndSelectInput dpy w (structureNotifyMask .|. exposureMask)
  let periods = mapMaybe getRefreshRate widgets
  print periods

  let rs = RenderState dpy w buf gc width height bg (x, y_ place, scX, scY, scWidth, scHeight)
  let widgetDraws = bgWidget rs : map (makeWidget rs) widgets
  let draw = proc x -> id <<< zipAuto ZNop widgetDraws -< repeat x
  return ((draw, periods), w)

updateTooltip :: Display -> Maybe Window
              -> Auto IO (Maybe (RenderState, Widget))
                         (Maybe (WindowDraw,[Period]))
updateTooltip dpy mbw = mkAutoM_ $ \inp -> do
  forM_ mbw $ \w -> print "destroy" >> destroyWindow dpy w

  case inp of
    Nothing       -> return (Nothing, updateTooltip dpy Nothing)
    Just (rs, wd) ->
      case mbtooltip $ attr_ wd of
        Nothing -> return (Nothing, updateTooltip dpy Nothing)
        Just tip -> do
          (tip, neww) <- createTooltip dpy rs wd tip
          return (Just tip, updateTooltip dpy $ Just neww)

tooltipContainer :: Maybe WindowDraw
                 -> Auto IO (Blip (Maybe WindowDraw), [ZEvent], [ZEvent]) [DrawResult]
tooltipContainer mbtip = mkAutoM_ $ \(newtip, initevts, evts) -> do
  (initres, mbtip') <- case newtip of
         Blip (Just t) -> step t initevts []
         Blip Nothing -> return ([], Nothing)
         _ -> return ([], mbtip)
  case mbtip' of
      Nothing -> return ([], tooltipContainer Nothing)
      Just tip -> do
        (res, mbtip'') <- step tip evts []
        return (initres ++ res, tooltipContainer mbtip'')
  where
    step :: WindowDraw -> [ZEvent] -> [[DrawResult]]
         -> IO ([DrawResult], Maybe WindowDraw)
    step tip [] out = return (concat out, Just tip)
    step tip (e:xs) out = do
      (v, tip') <- e `deepseq` stepAuto tip e
      step tip' xs (v : out)

handleClick :: Auto IO (RenderState, Widget) ()
handleClick = mkFuncM $ \(_, wd) -> do
  print "Click"
  let attr = attr_ wd
  forM_ (onclick attr) runCommand

dataTask :: Display -> Auto IO TimerCollectionInp [(Period, UTCTime)]
                    -> ((Window, Maybe Size) -> Maybe (RenderState, Widget))
                    -> [Auto IO (Period, UTCTime) (Blip (SampleDef, [Int]))]
                    -> [Auto IO (Blip (SampleDef, [Int])) (Maybe (GraphDef, GraphData))]
                    -> [WidgetDraw]
                    -> Auto IO RootInput ()
dataTask dpy timerTask mouse samples graphs widgets = proc evt -> do
    -- dump "Event === " -< evt

    tooltipChangeReq <- onChange_ <<< holdWith_ Nothing
                    <<< modifyBlips mouse
                    <<< emitJusts getMotionEvent -< evt

    perBlip handleClick <<< mapMaybeB mouse <<< emitJusts getClickEvent -< evt

    -- dump "Tooltip === " -< tooltipChangeReq
    tooltipChange <- perBlip (updateTooltip dpy Nothing) -< tooltipChangeReq

    let tooltipTimers :: Blip (M.Map Period Int)
        tooltipTimers = fmap (M.fromList . flip zip (repeat 1) . maybe [] snd) tooltipChange
    prevTooltipTimers <- delay_ M.empty <<< holdWith_ M.empty -< tooltipTimers
    let tooltipTimerChange = prevTooltipTimers `deepseq` fmap (TCChangeUsage . M.toList . M.unionWith (+) (fmap (0-) prevTooltipTimers)) tooltipTimers
    perBlip (dump "TimerChange") -< tooltipTimerChange

    timerBlip <- onJusts <<< arrM getRootTickEvents -< evt

    let timerCollectBlips = mergeL tooltipTimerChange timerBlip
    timerCollectionEvts <- fromBlips TCNop -< timerCollectBlips
    timerEvts <- timerTask -< timerCollectionEvts
    -- accelOverList $ dump "Timers === " -< timerEvts
    sampBlips <- accelOverList $ zipAuto dummy samples -< map repeat timerEvts
    dataBlips <- accelOverList $ zipAuto NoBlip graphs -< map repeat (concat sampBlips)
    let graphChanges :: [(GraphDef, GraphData)]
        graphChanges = catMaybes . concat $ dataBlips
    latestGraphs <- map GEv . M.toList <$> accum_ (foldr (\(k,v) m -> M.insert k v m)) M.empty -< graphChanges
    -- accelOverList $ dump "Graphs changes === " -< graphChanges
    let zevts = (REv evt : map GEv graphChanges) ++ map TEv timerEvts
    -- accelOverList $ dump "Paint input === " -< zevts
    paints <- accelOverList $ zipAuto ZNop widgets -< map repeat zevts
    tipPaints <- tooltipContainer Nothing -< (fmap (fmap fst) tooltipChange, latestGraphs, zevts)

    let rect = combinePaints . catMaybes $ concat paints ++ tipPaints
    -- accelOverList $ dump "Rectangles === " -< rect
    accelOverList (mkFuncM updateWindow) -< rect
    -- id -< concat dataBlips
    id -< ()
  where
    dummy = (1, epoch)

    updateGraphSet st inp = S.union st $ S.fromList inp
    updateWindow (RenderState {display = dpy, buffer = buf, window = w, gc_ = gc},
                  Rectangle x y width height) = do
      copyArea dpy buf w gc (fi x) (fi y) (fi width) (fi height) (fi x) (fi y)
      sync dpy False

getBounds wd =
  let WidgetAttributes sz pos  _ _ _ _ = attr_ wd
  -- FIXME: use Word32 for dimensions?
  in Rectangle (fi $ x_ pos) (fi $ y_ pos) (fi $ x_ sz) (fi $ y_ sz)

myCache f = mkAutoM_ $ \_ -> do
  v <- f
  return (v, mkConst v)

mkDrawStringWidget :: RenderState -> Widget -> Auto IO [String] (RenderState, Rectangle)
mkDrawStringWidget rs@RenderState { display = dpy } wd = proc strings -> do
  -- FIXME cache_ didn't work
    fn <- myCache (makeFont rs $ tattr_ wd) -< ()
    draw -< (fn, strings)
  where
    WidgetAttributes (Size ws hs) (Size x y) _ wbg _ _ = attr_ wd
    draw :: Auto IO (XftFont, [String]) (RenderState, Rectangle)
    draw = mkAutoM_ $ \inp -> makeIconCache dpy >>= \icons -> draw' icons inp
    draw' icons (fn, strings) = do
      print ("Draw Strings: ", strings)
      (icons', _) <- withDraw rs $ \d -> do
        xftDrawSetClipRectangles d 0 0 [Rectangle (fi x) (fi y) (fi ws) (fi hs)]
        drawRect (display rs) d wbg x y ws hs
        foldM (drawMessages rs (attr_ wd) (tattr_ wd) fn d) (icons, y) strings
      return ((rs, getBounds wd), mkAutoM_ $ draw' icons')

type DrawResult = Maybe (RenderState, Rectangle)
type WidgetDraw = Auto IO ZEvent DrawResult
type WindowDraw = Auto IO ZEvent [DrawResult]
data Pin a = InOut a | Out deriving Show

lastIn v (InOut s) = s
lastIn v Out = v

wrapAction :: a
           -> (ZEvent -> Maybe (Pin a))
           -> Auto IO a (RenderState, Rectangle)
           -> WidgetDraw
wrapAction dflt filterEv action = proc ev -> do
  v <- accumB_ lastIn dflt <<< mapMaybeB filterEv -< Blip ev
  id <<< asMaybes <<< perBlip action -< v

filterTimer rs wd (TEv (ival, t))   | Just ival == getRefreshRate wd  = Just $ InOut t
filterTimer rs wd (REv (RExpose w)) | w == window rs                  = Just Out
filterTimer _  _  _     = Nothing

bgWidget rs = wrapAction () filterEv paint' where
  filterEv (REv (RExpose w)) | w == window rs  = Just Out
  filterEv _     = Nothing
  paint' = effect $ do
    let RenderState dpy ww b gc w h bg _ = rs
    withDraw rs $ \d -> drawRect dpy d bg 0 0 w h
    return (rs, Rectangle 0 0 (fi w) (fi h))

makeWidget :: RenderState -> Widget -> WidgetDraw
makeWidget rs wd@Title {} = wrapAction [""] filterEv (mkDrawStringWidget rs wd)where
    filterEv (REv (RTitle s)) = Just $ InOut [s]
    filterEv (REv (RExpose w)) | w == window rs  = Just Out
    filterEv _     = Nothing

makeWidget rs wd@Clock { refreshRate = rate } = wrapAction epoch (filterTimer rs wd) make where
  make = proc t -> do
    tz <- myCache (case tz_ wd of
          LocalTimeZone -> localTimezone
          OtherTimeZone z -> otherTimezone z) -< ()
    -- FIXME: ignores s
    s <- mkFuncM (formatClock (fmt_ wd)) -< tz
    id <<< mkDrawStringWidget rs wd -< [s]

makeWidget rs wd@Label { label_ = msg } = wrapAction [msg] filterEv (mkDrawStringWidget rs wd) where
    filterEv (REv (RExpose w)) | w == window rs  = Just Out
    filterEv _     = Nothing

makeWidget rs wd@(Graph attr def colors) = wrapAction (LinearGraph []) filterEv painter where
    filterEv (GEv (def', grdata)) | def == def'  = Just $ InOut grdata
    filterEv (REv (RExpose w))  | w == window rs  = Just Out
    filterEv _     = Nothing
    painter = mkFuncM $ \grdata -> do
      let WidgetAttributes sz pos  _ bg _ _ = attr
          (Size ws hs, Size x0 y0) = (sz, pos)
      let samp = transpose . fmap (scaleG hs . reverse . tail . scanl (+) 0) . exportGraph $ grdata
      let colorTable = map toColor colors
      withDraw rs $ \d -> do
        drawRect (display rs) d bg x0 y0 ws hs
        let segments = map (map (makeSegment y0 hs) . filter ((/=0) . snd)
                            . zip [x0+ws-1,x0+ws-2..]) samp
        -- print ("Paint Graph", segments, samp)
        mapM_ (drawColorSegment rs) $ zip segments colorTable
      -- FIXME: update only changed part
      return (rs, getBounds wd)

makeWidget rs wd@NetStatus { netdev_ = netdev, refreshRate = r} =
    wrapAction (LinearGraph []) filterEv make where
  filterEv (GEv (def', grdata)) | GraphDef (SampleDef r $ Net netdev) LinearTime == def' = Just $ InOut grdata
  filterEv (REv (RExpose w))  | w == window rs  = Just Out
  filterEv _     = Nothing

  fmt :: String -> [Int] -> String
  fmt hdr v =
    let [inbound, _, outbound, _] = map (fmtBytes . round . (/realToFrac r) . revf3 . fromIntegral) v
     in printf "%s In: %s/s : Out: %s/s" hdr inbound outbound

  printNet (LinearGraph []) = return ["Loading..."]
  printNet (LinearGraph samples@(sample:_)) =
    return [fmt "" sample, fmt "Avg" $ avgSamp samples ]

  make = proc t -> do
    msg <- mkFuncM printNet  -< t
    id <<< mkDrawStringWidget rs wd -< msg

makeWidget rs wd@CpuTop {} = wrapAction epoch filterEv make where
  loadavg :: IO String
  loadavg = foldl (\a b -> a++" "++b) "Load avg: " . take 3 . words <$> readFully "/proc/loadavg"
  filterEv = filterTimer rs wd

  readProcs :: UTCTime -> IO ([Int], M.Map String (String, Int), UTCTime)
  readProcs ts = do
    cpu <- readCPU
    procs <- pickCpuUsage
    return (cpu, procs, ts)

  printCpu v Nothing = do
    msgs <- (: []) <$> loadavg
    return $ msgs `deepseq` (msgs, Just (v, msgs))

  printCpu (_, _, ts2) v@(Just ((_, _, ts1), msg)) | ts1 == ts2  =
    return (msg, v)

  printCpu v@(cpu2, procs2, ts2) (Just ((cpu1, procs1, ts1), msg)) = do
    avg <- loadavg
    let dt = getDt ts2 ts1
    top <- makeCpuDiff procs2 procs1 dt
    let msg = avg:top
    return (msg, Just (v, msg))

  make = proc t -> do
    msg <- mkStateM_ printCpu Nothing <<< seqer <<< mkFuncM readProcs -< t
    id <<< mkDrawStringWidget rs wd -< msg

makeWidget rs wd@MemStatus {} = wrapAction epoch filterEv make where
  filterEv = filterTimer rs wd
  make = proc t -> id <<< mkDrawStringWidget rs wd <<< effect memStatus  -< t
  memStatus = do
    x <- readKeyValueFile ((`div` 1024) . read . head . words) "/proc/meminfo" :: IO (M.Map String Int)
    let x' = M.insert "Swap" ((x M.! "SwapTotal") - (x M.! "SwapFree")) x
    let values = ["MemFree", "Cached", "Buffers", "Swap", "Dirty", "Hugetlb"]
    let mem = map (\n -> printf "%7s: %5d MB" n (x' M.! n)) values
    perPidInfo <- memInfo
    return $ zipWith (++) mem perPidInfo

makeWidget rs wd@BatteryStatus {batteryName = n} = wrapAction epoch filterEv make where
  filterEv = filterTimer rs wd
  make = proc t -> id <<< mkDrawStringWidget rs wd <<< effect batteryStatus -< t
  batteryStatus = do
    capacity <- readBatteryInt n "energy_full"
    rate <- readBatteryInt n "power_now"
    remainingCapacity <- readBatteryInt n "energy_now"
    state <- readBatteryString n "status" :: IO String
    let (h, m) = (remainingCapacity * 60 `div` rate) `divMod` 60
        percent = remainingCapacity * 100 `div` capacity
    return $ (: []) $ case state of
      "Discharging" | rate /= 0 -> printf "%d%%(%d:%02d)" percent h m
      _ -> printf "%d%%C" percent

makeWidget rs wd@BatteryRate {batteryName = n} = wrapAction epoch filterEv make where
  filterEv = filterTimer rs wd
  make = proc t -> id <<< mkDrawStringWidget rs wd <<< effect makeMessage -< t
  makeMessage = do
    rate <- readBatteryString n "power_now"
    let rateDouble = read rate / 1000000 :: Double
    return . (: []) . printf " Present Rate: %.3f mA" $ rateDouble

makeWidget rs wd@Trayer {} = mkStateM_ handler Nothing where
  handler (REv RInit) _ = do
    let (pos, sz) = (position $ attr_ wd, size $ attr_ wd)
        cmd = trayerCmd (windowHeight rs) $ windowWidth rs - x_ pos - x_ sz
    print ("trayer cmd ", cmd)
    handle <- runCommand cmd
    return (Nothing, Just handle)

  handler (REv RExit) h = do
    forM_ h terminateProcess
    return (Nothing, Nothing)
  handler _ h = return (Nothing, h)

makeWidget _ _ = proc ev -> do
  dump "makeWidget" -< ev
  id -< Nothing

extractGraphs :: Widget -> [(GraphDef, Int)]
extractGraphs (Graph attr graph _) = [(graph, x_ $ size attr)]
extractGraphs NetStatus {refreshRate = r, netdev_ = n} = [(GraphDef (SampleDef r (Net n)) LinearTime, 20)]
extractGraphs _ = []

extractAllGraphs wd = extractGraphs wd ++ fromTooltip where
  fromTooltip = case mbtooltip $ attr_ wd of
      Nothing -> []
      Just (Tooltip _ _ _ wds) -> concatMap extractGraphs wds

inbounds :: WidgetAttributes -> Size -> Bool
inbounds WidgetAttributes {size = (Size ws hs), position = (Size wx wy)} (Size x y) =
  x > wx && x < wx + ws && y > wy && y < wy + hs

mouseHitWds :: [Widget] -> Size -> Maybe Widget
mouseHitWds wds pos =
  let match wd = inbounds (attr_ wd) pos
   in find match wds

mouseHitWins :: [WindowState] -> (Window, Maybe Size) -> Maybe (RenderState, Widget)
mouseHitWins _ (w, Nothing) = Nothing
mouseHitWins wins (w, Just pos) =
  let matchWin (WindowState rs _) = w == window rs
      matchWds (WindowState rs wds) = mouseHitWds wds pos >>= \wd -> Just (rs, wd)
   in find matchWin wins >>= matchWds

initRootTask :: Display -> ControlChan -> [WindowState] -> IO(Auto IO RootInput ())
initRootTask dpy ch wins = do
  forkIO $ forever $
    tryIOError getLine >>= \case
      Left _ -> writeChan ch RExit
      Right line -> writeChan ch (RTitle line)

  let graphs = concatMap extractAllGraphs $ concatMap getWidgets wins
      uniq_graphs = M.toList . foldr (\(k,v) m -> M.insertWith max k v m) M.empty $ graphs
      graphs' = map createGraph uniq_graphs

      sampledefs = map (sample_ . fst) uniq_graphs 
      uniq_sdef = S.toList $ S.fromList sampledefs
      samples = map createSample uniq_sdef

      mouse = mouseHitWins wins
      widgets = concatMap mkWindow wins
      periods = mapMaybe getRefreshRate $ concatMap getWidgets wins
      timers = timerTask ch
  (_, timers') <- stepAuto timers . TCChangeUsage $ zip (periods ++ persistentTimers) (repeat 1)
  return $ dataTask dpy timers' mouse samples graphs' widgets where
        getWidgets (WindowState _ wds) = wds
        mkWindow (WindowState rs wds) = bgWidget rs : map (makeWidget rs) wds


eventLoop :: Display -> RootChan -> Auto IO RootInput () -> IO ()
eventLoop dpy ch auto = do
  rootEv <- ($!) allocaXEvent $ \ev -> do
    nextEvent dpy ev
    event <- getEvent ev
    case event of
       ClientMessageEvent {ev_data = 0:_} -> readChan ch
       ExposeEvent { ev_window = w } ->      return $ RExpose w
       ButtonEvent {ev_x = x, ev_y = y, ev_window = ww} ->
         return $ RClick ww $ Size (fi x) (fi y)

       MotionEvent {ev_x = x, ev_y = y, ev_window = ww} ->
         return $ RMotion ww $ Just (Size (fi x) (fi y))

       e@CrossingEvent {ev_x = x, ev_y = y, ev_window = ww} ->
         if ev_event_type e == enterNotify
            then return $ RMotion ww $ Just (Size (fi x) (fi y))
            else return $ RMotion ww Nothing
       _ -> return RNop
  (_, auto') <- stepAuto auto rootEv
  case rootEv of
    RExit -> print "Exiting..."
    _ -> eventLoop dpy ch auto'

main :: IO ()
main = do
  xSetErrorHandler
  dpy <- openDisplay ""
  -- FIXME: increase to improve throughput?
  controlCh <- newChan

  wins <- mapM (makeBar dpy controlCh) bars

  let WindowState firstRs _ = head wins
  forkOS $ copyChanToX controlCh $ window firstRs

  auto <- initRootTask dpy controlCh wins
  (_, auto') <- stepAuto auto RInit
  eventLoop dpy controlCh auto'

