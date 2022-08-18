{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Loops
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
import System.Exit
import System.IO.Error
import System.Posix.Process
import System.Process (runCommand, terminateProcess)
import Text.Printf
import qualified Control.Category as Cat
import qualified GHC.Exts.Heap as Heap
import qualified GHC.Exts.Heap.Closures as Closures

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude hiding (id)

import DzenParse
import Icon
import Timer
import Top
import Utils
import Text.Parsec.Token (GenTokenParser(dot))

barHeight :: Int
barHeight = 24

defaultFont :: String
defaultFont = "-*-*-medium-r-normal--15-*-*-*-*-*-iso10646-*"

largeFont = "-*-*-medium-r-normal--35-*-*-*-*-*-iso10646-*"

barBackground :: String
barBackground = "#BEBEBE"

infoBackground :: String
infoBackground = "#181838"

tooltipBackground :: String
tooltipBackground = "#FFFFC0"

defaultTextColor :: String
defaultTextColor = "#C7AE86"

trayerCmd :: Int -> Int -> String
trayerCmd = printf "trayer --expand false --edge top --align right\
             \ --widthtype request --height %d --margin %d"

bars :: [Bar]
--bars = [bar1, bar2]
bars = [bar1]

bar1 :: Bar
bar1 = Bar barBackground barHeight (XineramaScreen 0) GravityTop [
        clock # TimeFormat "%R" # RefreshRate 60 # OnClick "clock.sh"
              # Width 60 # RightPadding 4
              # LocalTimeZone # BackgroundColor infoBackground
              # clockTooltip,
        logtm cpu # cpuTooltip # OnClick "top.sh",
        logtm mem # memTooltip,
        logtm (net "eth0"),
        logtm (net "brkvm"),
        logtm (net "wlp2s0"),
        logtm (net "wlp0s20f3"),
        battery "BAT0",
        battery "BAT1",
        trayer,

        title # LeftPadding 5 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

bar2 :: Bar
bar2 = Bar barBackground (barHeight*2) (XineramaScreen 0) GravityBottom [
        clock # TimeFormat "%R" #RefreshRate 60 #
            Width 60 # RightPadding 4 #
            LocalTimeZone # BackgroundColor infoBackground #
            clockTooltip,
        logtm cpu # cpuTooltip # OnClick "top.sh" #LinearTime # RefreshRate 0.02,

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

clockTooltip :: Tooltip
clockTooltip = Tooltip tooltipBackground (Size 460 (barHeight * 5)) Vertical [
     tooltipClock #TimeFormat "%a, %e %b - %X" #JustifyMiddle
              # Height (barHeight * 2) # RightPadding 4 # LocalTimeZone
              #SetFont largeFont #SetTextHeight (2 *barHeight),
     frame Horizontal [
          frame Vertical [
                 tooltipClock #OtherTimeZone "America/Los_Angeles"
                               -- 0527 06:17:59.956236
                              #TimeFormat "%Y-%m-%d %H:%M:%S",
                 tooltipClock #OtherTimeZone "America/Los_Angeles",
                 tooltipClock #OtherTimeZone "GMT"
                 ] #Width 340,
          frame Vertical [
                 tooltipLabel #Message "MTV TS: " #JustifyRight,
                 tooltipLabel #Message "MTV: " #JustifyRight,
                 tooltipLabel #Message "GMT: " #JustifyRight
                 ]
          ] #Height (barHeight * 3)
     ]

cpuTooltip :: Tooltip
cpuTooltip = Tooltip tooltipBackground (Size 600 (8*barHeight)) Horizontal [
     tooltip cpu #RefreshRate 0.05 #LinearTime #Width 100
                   #BottomPadding 1 #RightPadding 1 # LeftPadding 1
                   #Width 100 #Refresh WhenVisible,
     tooltip cpu #RefreshRate 1 #LinearTime #Width 200
                   #BottomPadding 1 #RightPadding 1 # LeftPadding 1,
     tooltipText cpuTop #Width 300
     ]

memTooltip :: Tooltip
memTooltip = Tooltip tooltipBackground (Size 490 (6*barHeight)) Horizontal [
     tooltipText memstatus #Width 430 #LeftPadding 5,
     tooltip mem #RefreshRate 1 #LogTime 3
     ]

netTooltip :: String -> Tooltip
netTooltip netdev = Tooltip tooltipBackground (Size 480 (10*barHeight)) Vertical [
     frame Horizontal [
           tooltip (tooltipNet netdev) #RefreshRate 0.05 #Refresh WhenVisible #LinearTime # Width 200,
           tooltip (tooltipNet netdev) #RefreshRate 1 #LinearTime # Width 200
                           # TopPadding 1 # BottomPadding 1
           ] #Height (8 * barHeight),
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
tooltipClock = tooltipText clock #TimeFormat "%a, %e %b %Y - %X" #JustifyLeft

tooltipLabel :: Widget
tooltipLabel = tooltipText label

tooltipNet :: String -> Widget
tooltipNet netdev = Graph defaultAttr (GraphDef (Net netdev) (LogTime 8) Always)
                    ["#6060FF", tooltipBackground, "#60FF60"] 1  # Width 129

data Attribute = Width Int | Height Int | LeftPadding Int | RightPadding Int
               | TopPadding Int | BottomPadding Int
               | TextColor Main.Color | BackgroundColor Main.Color
               | TimeFormat String | Message String | SetFont String | SetTextHeight Int
               | RefreshRate Period | OnClick String | Refresh RefreshType

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
data GraphType = Cpu | Net String | Mem | Battery String deriving (Show, Eq, Ord, Generic, NFData)
data TimeScale = LinearTime | LogTime Int deriving (Show, Eq, Ord, Generic, NFData)
data RefreshType = Always | WhenVisible deriving (Show, Eq, Ord, Generic, NFData)
data GraphDef = GraphDef {type_ :: GraphType, tscale_ :: TimeScale, refresh_type_ :: RefreshType} deriving (Show, Eq, Ord, Generic, NFData)

data Bar = Bar String Int Screen Gravity [Widget] deriving Show
data Tooltip = Tooltip String Size Orientation [Widget] deriving (Show, Eq)

data Widget = Clock   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, fmt_ :: String, tz_ :: ClockTimeZone, refreshRate :: Period }
          | Label   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, label_ ::  String }
          | Title   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | CpuTop  {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period}
          | NetStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period, netdev_ :: String}
          | MemStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period}
          | Frame   {attr_ :: WidgetAttributes, frameOrientation :: Orientation, children :: [Widget]}
          | Graph   {attr_ :: WidgetAttributes, graph_ :: GraphDef, graphColorTable :: [String], refreshRate :: Period}
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
cpu = Graph defaultAttr (GraphDef Cpu (LogTime 8) Always) ["#70FF70", "#FF8080", "#F020F0", "#3030FF"] 1 -- # Width 129

mem :: Widget
mem = Graph defaultAttr (GraphDef Mem (LogTime 8) Always) ["#00FF00", "#6060FF"] 1 -- # Width 129

batteryGraph :: String -> Widget
batteryGraph name = Graph defaultAttr (GraphDef (Battery name) LinearTime Always) ["#0760F2"] 1

net :: String -> Widget
net netdev = Graph defaultAttr (GraphDef (Net netdev) (LogTime 8) Always)
             ["#6060FF", infoBackground, "#60FF60"] 1 # Width 129 #netTooltip netdev

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
  apply (SetTextHeight hs) ww = let TextAttributes c j f _ = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}

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
  apply (RefreshRate r) ww = ww { refreshRate = r }
  apply (Refresh t) ww@Graph{graph_ = g} = ww {graph_ = g { refresh_type_ = t } }

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

data SenderX = SenderX Display Window Atom

data RenderState = RenderState {
  display :: Display,
  window :: Window,
  buffer :: Pixmap,
  gc_ :: GC,
  windowWidth :: Int,
  windowHeight :: Int,
  windowBackground :: String,
  pos :: (Int, Int, Int, Int, Int, Int),
  refresh_ :: Window
}

instance Eq RenderState where
  (==) a b = window a == window b

type WindowState = (RenderState, [Widget])

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
  let yoff' = yoff + ((ths + dy) `div` 2)
  let fg' = fromMaybe wfg fg
  drawRect dpy d (fromMaybe wbg bg) (x + xoff) yoff ws ths
  withColor dpy fg' $ \c -> xftDrawString d c font (x' + xoff) yoff' msg
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


-- Should be data for strictness
type GraphSample = [Int]
data GraphData = LinearGraph [GraphSample] | LogGraph Int [[GraphSample]] deriving (Show, Generic, NFData)

makeGraph :: TimeScale -> Int -> Int -> GraphData
makeGraph LinearTime ws l = LinearGraph $ replicate ws $ replicate l 0
makeGraph (LogTime n) ws _ = LogGraph n $ replicate (1 + ws `div` n) []

avgSamp :: [GraphSample] -> GraphSample -- (a + b + 1) /2
avgSamp ar = map (\v -> (sum v + 1) `div` length v) $ transpose ar

updateGraph :: Int -> GraphData -> GraphSample -> GraphData
updateGraph ws (LinearGraph g) s = LinearGraph $ s : take ws g
updateGraph _ (LogGraph n g) s = LogGraph n $ updateLayer g s where
  updateLayer :: [[GraphSample]] -> GraphSample -> [[GraphSample]]
  updateLayer [] _ = []
  updateLayer (vv:xs) v
    | length vv == (n+2) = let (a,b) = splitAt n vv in (v:a):updateLayer xs (avgSamp b)
    | otherwise = (v:vv):xs

exportGraph :: GraphData -> [GraphSample]
exportGraph (LinearGraph g) = g
exportGraph (LogGraph n g) = concatMap (take' n) g where
  take' n' ar = let (a,b) = splitAt (n'-1) ar in a ++ (case b of
       [] -> []
       _ -> [avgSamp b])

readBatteryFile :: FilePath -> IO (M.Map String String)
readBatteryFile = readKeyValueFile $ head . words

readNetFile :: FilePath -> IO (M.Map String [Int])
readNetFile = readKeyValueFile $ map read . words

readFileWithFallback :: String -> IO String
readFileWithFallback x = readFully x `catchIOError` \_ -> return "0"


batteryFile :: String -> String -> String
batteryFile n x = "/sys/class/power_supply/" ++ n ++ "/" ++ x

readBatteryString :: String -> String -> IO String
readBatteryString n x = head . lines <$> readFileWithFallback (batteryFile n x)

readBatteryInt :: String -> String -> IO Int
readBatteryInt n x = read <$> readBatteryString n x :: IO Int
readBatteryDouble n x = read <$> readBatteryString n x :: IO Double

getDt :: Fractional t => UTCTime -> UTCTime -> t
getDt newTs ts = (/1e12) . fromIntegral . fromEnum $ diffUTCTime newTs ts

getNetBytes :: [Int] -> GraphSample
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

makeSenderX w = do
  d <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  return $ SenderX d w a

sendX :: SenderX -> IO ()
sendX (SenderX dpy w a) = do

  sendClientEvent dpy a w 0 `catchIOError`  \x -> do
       print $ "Exception caught: " ++ show x
       sync dpy False


checkBattery wd n = do
  r <- tryIOError $ readFully $ batteryFile n "status"
  case r of
    Left _ -> print ("Unknown battery", n) >> return Nothing
    Right _ -> return (Just wd)

checkNetdev wd n = do
  net <- readNetFile "/proc/net/dev"
  return $ if M.member n net then Just wd else Nothing

removeBroken wd@Graph {graph_ = GraphDef {type_ = Battery n}} = checkBattery wd n
removeBroken wd@BatteryRate {batteryName = n} = checkBattery wd n
removeBroken wd@BatteryStatus {batteryName = n} = checkBattery wd n
removeBroken wd@Graph {graph_ = GraphDef {type_ = Net n}} = checkNetdev wd n
removeBroken wd@NetStatus {netdev_ = n} = checkNetdev wd n
removeBroken wd = return $ Just wd


makeBar :: Display -> Bar -> IO WindowState
makeBar dpy (Bar bg height screen gravity wds) = do

  let scr = defaultScreen dpy
  xiscr <- case screen of
       DefaultScreen -> return Nothing
       XineramaScreen x -> maybe Nothing (find (\s -> xsi_screen_number s == fi x))
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
  refresh <- makeSenderX w

  let rs = RenderState { display = dpy, window = w, buffer = buf, gc_ = gc,
                         windowWidth = fi scWidth, windowHeight = height,
                         windowBackground = bg,
                         pos = (fi scX, fi y, fi scX, fi scY, fi scWidth, fi scHeight),
                         refresh_ = w }

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
  return (rs, widgets)

layers Cpu = 3
layers (Net _) = 3
layers Mem = 2
layers (Battery _) = 1

drops Mem = 0
drops _ = 1

scaleG hs (total:vals) = map ((`div` (if total == 0 then 1 else total)) . (*hs)) vals

readCPU :: IO GraphSample
readCPU = map read . tail. words . head . lines <$> readFully "/proc/stat"

readNet :: String -> IO (GraphSample, UTCTime)
readNet netdev = do
  ts <- getCurrentTime
  net <- readNetFile "/proc/net/dev"
  let inout = getNetBytes (net ! netdev)
  return (inout, ts)


f3 :: Double -> Double
f3 x = f x * f x * f x where
  f x = log (x + 1)

revf3 :: Double -> Double
revf3 x = exp (x ** (1/3)) - 1 + 0.5

-- alternative definition of Module:
--newtype Module m a b =
--      Module (a -> m (b, Module m a b))
--runModule :: (Monad m) => Module m a b -> a -> m (b, Module m a b)
--runModule (Module m) = m

newtype Module m a b = Module { runModuleI :: a -> m (b, Module m a b) } deriving (Generic, NFData)

runModule m a = do
  (b, m') <- runModuleI m a
  return (b, m')

execModule :: (Monad m, NFData a, NFData b) => Module m a b -> a -> m (Module m a b)
execModule m inp = snd <$> runModule m inp

instance (Monad m) => Functor (Module m a) where
  fmap f (Module mf) =
    Module $ \inp -> do
        (b, m') <- mf inp
        return (f b, fmap f m')

instance (Monad m) => Applicative (Module m a) where
  pure a = Module $ \_ -> return (a, pure a)
  Module f1 <*> Module f2 = Module $ \inp -> do
    (f, m1') <- f1 inp
    (x, m2') <- f2 inp
    let v = f x
    return (v, m1' <*> m2')

instance (Monad m) => Cat.Category (Module m) where
   id = Module $ \a -> return (a, Cat.id)
   (.) m2 m1 = Module $ \a -> do
        (b, m1') <- runModuleI m1 a
        (c, m2') <- runModuleI m2 b
        return (c, m2' Cat.. m1')

instance (Monad m) => Arrow (Module m) where
  -- not monadic version
  arr f = Module $ \a -> let v = f a in return (v, arr f)
  first m = Module $ \(a,b) -> do
       (a', m') <- runModuleI m a
       return ((a', b), first m')

pipeModules :: (Monad m) => Module m b c -> Module m a b -> Module m a c
pipeModules m2 m1 = m2 Cat.. m1

{-
pipeModules :: (Monad m, NFData a, NFData c) => Module m b c -> Module m a b -> Module m a c
pipeModules m2 m1 = Module $ \a -> do
        (b, m1') <- runModuleI m1 a
        (c, m2') <- runModuleI m2 b
        return (c, m2' `pipeModules` m1')
-}

mkConst :: (Monad m) => b -> Module m a b
mkConst v = Module $ \_ -> return (v, mkConst v)

mkConstM :: (Monad m, NFData b) => m b -> Module m a b
mkConstM mv = Module $ \_ -> mv >>= \x -> return (x, mkConstM mv)

mkFunc :: (Monad m, NFData a, NFData b) => (a -> b) -> Module m a b
--mkFunc v = v `deepseq` arr v
mkFunc f = Module $ \a -> let v = f a in return (v, mkFunc f)


mkFuncM :: (Monad m, NFData b) => (a -> m b) -> Module m a b
mkFuncM f = Module $ \a -> do
  v <- f a
  return (v, mkFuncM f)

accum :: (Monad m, NFData acc, NFData b) => acc -> (acc -> a -> (b, acc)) -> Module m a b
accum acc f = Module $ \input -> do
    let (output, acc') = f acc input
    return (output, accum acc' f)

delayedEcho :: (Monad m, NFData a) => a -> Module m a a
delayedEcho acc = accum acc (,)

delayedEcho' :: (Monad m, NFData a) => Module m a a
delayedEcho' = Module $ \x -> return (x, delayedEcho x)

statelessModule :: (Monad m, NFData b) => m b -> Module m a b
statelessModule = mkConstM


{- unused
runModules :: (Monad m) => [Module m a b] -> a -> m ([b], [Module m a b])
runModules [] _ = return ([], [])
runModules (x:xs) a = do
  (b, x') <- runModule x a
  (bs, xs') <- runModules xs a
  return (b:bs, x':xs')

zipModules :: (Monad m) => [Module m a b] -> Module m a [b]
zipModules ms = Module $ \a -> do
  (bs, ms') <- runModules ms a
  return (bs, zipModules ms')

constModule :: (Monad m) => b -> Module m a b
constModule v  = Module $ \_ -> do
  return (v, constModule v)
-}

{- Module seem like cannot be Monad as mf not doesn't persist over call
instance (Monad m) => Monad (Module m a) where
  (>>=) f mf = Module $ \inp -> do
      (v, m1') <- runModule f inp
      (v2, m2') <- runModule (mf v) inp
      return (v2, pipeModules m1' m2')
-}

debugCounter v = Module $ \_ -> do
  print $ "counter: " ++ show v
  return ((), debugCounter (v+1))

{-
dummy typ = proc _ -> do
  t <- sampleTask typ -< ()
  returnA -< t
-}

sampleTask :: GraphType -> Module IO () GraphSample
sampleTask Cpu = sampleTaskCpu [0,0,0,0,0]
  where
  sampleTaskCpu prev_cpu = Module $ \_ -> do
    cpu <- readCPU
    let output = let (user:nice:sys:idle:io:_) = zipWith (-) cpu prev_cpu
                 in [sys + io, nice, user, idle]
    return (output, sampleTaskCpu cpu)

{- cannot figure out memory leaks with proc
sampleTask Cpu = proc _ -> do
  cpu <- mkConstM readCPU -< ()
  prev_cpu <- delayedEcho' -< cpu
  returnA -< let (user:nice:sys:idle:io:_) = zipWith (-) cpu prev_cpu
             in GraphSample [sys + io, nice, user, idle]
-}

sampleTask Mem = mkConstM $ do
    mem <- readBatteryFile "/proc/meminfo"
    let [total,free,cached] = map (read . (mem !))
           ["MemTotal", "MemFree", "Cached"]
        out = [total - free - cached, cached, free]
    return out

sampleTask (Battery n) = mkConstM $ do
    capacity <- readBatteryInt n "energy_full"
    remainingCapacity <- readBatteryInt n "energy_now"
    let out = [remainingCapacity, capacity - remainingCapacity]
    return out


sampleTask (Net netdev) = sampleTaskNetFirst
  where
    sampleTaskNet (old_time, old_net) = Module $ \_ -> do
       time <- getCurrentTime
       nets <- readNetFile "/proc/net/dev"
       let net = getNetBytes (nets ! netdev)  :: GraphSample
           f x = log (x + 1)
           f3 x = f x * f x * f x
           dt = getDt time old_time :: Double
           [inbound, outbound] = map (f3 . fromIntegral) $ zipWith (-) net old_net :: [Double]
           maxspeed = f3 $ dt * 100000000 :: Double
           output = map (truncate . max 0)
                [inbound, maxspeed - inbound - outbound, outbound, 0] :: [Int]
       return (output, sampleTaskNet (time, net))
    sampleTaskNetFirst = Module $ \_ -> do
       time <- getCurrentTime
       nets <- readNetFile "/proc/net/dev"
       let net = getNetBytes (nets ! netdev)  :: GraphSample
       runModule (sampleTaskNet (time, net)) ()


{- cannot figure out memory leaks with proc
sampleTask (Net netdev) = proc _ -> do
  debugCounter 10 -< ()
  time <- mkConstM getCurrentTime -< ()
  nets <- mkConstM (readNetFile "/proc/net/dev") -< ()
  let net = ($!) getNetBytes (nets ! netdev)  :: [Int]
  (old_time, old_net) <- delayedEcho' -< (time, net)
  let f x = log (x + 1)
      f3 x = f x * f x * f x
      dt = getDt time old_time :: Double
      [inbound, outbound] = ($!) map (f3 . fromIntegral) $ zipWith (-) net old_net :: [Double]
      maxspeed = f3 $ dt * 100000000 :: Double
      output = ($!) map (truncate . max 0)
          [inbound, maxspeed - inbound - outbound, outbound, 0] :: [Int]
  returnA -< force output
-}

data Rect = Rect Int Int Int Int deriving (Generic, NFData)

mergeRect (Rect x0 y0 w0 h0) (Rect x1 y1 w1 h1) = Rect x y (fi w) (fi h)
  where
  calc a0 a1 b0 b1 = let (mn, mx) = (min a0 a1, max b0 b1) in (mn, mx - mn)
  (x, w) = calc x0 x1 (x0+fi w0) (x1+fi w1)
  (y, h) = calc y0 y1 (y0+fi h0) (y1+fi h1)

combinePaints paints = M.elems $ foldr f M.empty paints where
  f (rs, rect) = M.insertWith merge (window rs) (rs, rect)
  merge (rs, rect1) (_, rect2) = (rs, mergeRect rect1 rect2)


createTooltip :: RenderState -> Widget -> Tooltip -> IO WindowState
createTooltip parent_rs pwd tip = do
  let dpy = display parent_rs
  let Tooltip bg tsz@(Size width height) orien widgets = tip
  let (px, py, scX, scY, scWidth, scHeight) = pos parent_rs

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

  let rs = RenderState dpy w buf gc width height bg (x, y_ place, scX, scY, scWidth, scHeight) (window parent_rs)
  return (rs, widgets)

getBounds wd =
  let WidgetAttributes sz pos  _ _ _ _ = attr_ wd
  -- FIXME: use Word32 for dimensions?
  in Rect (x_ pos) (y_ pos) (x_ sz) (y_ sz)

-- Always updating graphs
type BgGraphInfo = (GraphDef, Int, NominalDiffTime)
extractGlobalGraphInfo :: Widget -> Maybe BgGraphInfo
extractGlobalGraphInfo (Graph attr graph@GraphDef { refresh_type_ = Always} _ refresh) = Just (graph, x_ $ size attr, refresh)
extractGlobalGraphInfo NetStatus {refreshRate = r, netdev_ = n} = Just (GraphDef (Net n) LinearTime Always, 20, r)
extractGlobalGraphInfo _ = Nothing

writeIORef' ref v = atomicModifyIORef' ref (const (v, ()))

makeBackgroundGraph (GraphDef typ tscale _, ws, rate) = do
  let graphdata = makeGraph tscale ws (layers typ)
  ref <- newIORef graphdata
  let sampler = sampleTask typ
  return (updater ref sampler graphdata, rate, ref)
  where
    updater ref sampler' graphdata' = Module $ \_  -> do
        (sample, sampler'') <- runModule sampler' ()
        let graphdata'' = updateGraph ws graphdata' sample
        graphdata'' `deepseq` writeIORef' ref graphdata''
        return ((), updater ref sampler'' graphdata'')

type BgGraphs = [(BgGraphInfo, IORef GraphData)]

makeBackgroundGraphs :: [WindowState] -> IO BgGraphs
makeBackgroundGraphs wins = do
  let leafWidgets Frame{ children = c} = concatMap leafWidgets c
      leafWidgets w = [w]
      tooltipWidgets (Tooltip bg tsz orien widgets) = widgets

      widgets = concatMap (concatMap leafWidgets . snd) wins
      tooltips = mapMaybe (mbtooltip . attr_) widgets
      ttWidgets = concatMap (concatMap leafWidgets . tooltipWidgets) tooltips
      graphInfos = mapMaybe extractGlobalGraphInfo ttWidgets
  print graphInfos
  (tasks, rates, refs) <- unzip3 <$> mapM makeBackgroundGraph graphInfos
  runThread $ updateStep (return ()) $ zip tasks rates
  return $ zip graphInfos refs


data Repaint = RepaintAll | RepaintUpdated deriving (Generic, NFData)
type PaintWidgetCallback = Module IO Repaint (Maybe Rect)
type PaintWindowModule = Module IO Repaint ()
type TooltipInfo = (RenderState, Widget)

type DrawCallback = IO (Maybe Rect)
type UpdaterTickModule a =  Module IO () a
type UpdaterDef = (UpdaterTickModule (), Period)
type DynamicWidgetDef = (PaintWidgetCallback, Maybe UpdaterDef)


drawStrings rs wd fn icons strings d = do
       let WidgetAttributes (Size ws hs) (Size x y) _ wbg _ _ = attr_ wd
       xftDrawSetClipRectangles d 0 0 [Rectangle (fi x) (fi y) (fi ws) (fi hs)]
       drawRect (display rs) d wbg x y ws hs
       (icons', _) <- foldM (drawMessages rs (attr_ wd) (tattr_ wd) fn d) (icons, y) strings
       return (icons', Just $ getBounds wd)

makeIcons rs@RenderState { display = dpy} = makeIconCache dpy

makeUpdatingPainterWithArg :: IORef (a -> IO (a, Maybe Rect))
                            -> IORef Bool
                            ->  a
                            ->  Module IO Repaint (Maybe Rect)
makeUpdatingPainterWithArg paint dirty arg = Module $ \repaint -> perform arg repaint
    where
       perform arg RepaintAll = do
          writeIORef' dirty False
          painter <- readIORef paint
          (arg',rect) <- painter arg
          return (rect, makeUpdatingPainterWithArg paint dirty arg')
       perform arg RepaintUpdated = do
          is_dirty <- readIORef dirty
          if is_dirty
             then perform arg RepaintAll
             else return (Nothing, makeUpdatingPainterWithArg paint dirty arg)


makeUpdatingWidget :: NominalDiffTime -> UpdaterTickModule DrawCallback  -> IO DynamicWidgetDef
makeUpdatingWidget rate updater = do
  paint <- newIORef $ \_ -> return ( (), Nothing)
  dirty <- newIORef False
  let updateWrap = mkFuncM $ \drawable -> do
          writeIORef' paint $ \_ -> do
             mbrect <- drawable
             return ((), mbrect)
          writeIORef' dirty True
  return (makeUpdatingPainterWithArg paint dirty (), Just (updateWrap `pipeModules` updater, rate))

makeUpdatingTextWidget :: RenderState -> Widget
                       -> NominalDiffTime
                       -> UpdaterTickModule [String]
                       -> IO DynamicWidgetDef
makeUpdatingTextWidget rs wd rate text_updater = do
  fn <- makeFont rs (tattr_ wd)
  icons <- makeIcons rs
  let updater text_updater' = Module $ \_ -> do
        (text, text_updater'') <- runModule text_updater' ()
        return (snd <$> withDraw rs (drawStrings rs wd fn icons text), updater text_updater'')
  makeUpdatingWidget rate $ updater text_updater


-- Paint function, global timers, temporary timers
makeWidget :: RenderState -> BgGraphs -> Widget -> IO (PaintWidgetCallback, Maybe UpdaterDef)

makeWidget rs _ wd@Label { label_ = msg } = do
  fn <- makeFont rs (tattr_ wd)
  icons <- makeIcons rs
  let painter RepaintUpdated = return Nothing
      painter RepaintAll = snd <$> withDraw rs (drawStrings rs wd fn icons [msg])
  return (mkFuncM painter, Nothing)

makeWidget rs _ wd@Clock { refreshRate = rate } = do
  tz <- case tz_ wd of
       LocalTimeZone -> localTimezone
       OtherTimeZone z -> otherTimezone z
  makeUpdatingTextWidget rs wd rate $ mkConstM $ do
          message <- formatClock (fmt_ wd) tz
          -- print message
          return [message]

makeWidget rs _ wd@Title {} = do
  let RenderState { display = dpy, window = w} = rs
  let terminate msg = do
        print msg
        --exitSuccess
        exitImmediately ExitSuccess
        return "a"

  fn <- makeFont rs (tattr_ wd)
  icons <- makeIcons rs
  paint <- newIORef $ \_ -> return (icons, Nothing)
  dirty <- newIORef False
  sender <- makeSenderX w
  runThread $ forever $ do
     -- doesn't work with multiple titles on multiple bars
     title <- getLine `catchIOError` terminate
     writeIORef' paint $ \iconsA -> do
         if title == ""
            then unmapWindow dpy w
            else mapWindow dpy w
         withDraw rs (drawStrings rs wd fn iconsA [title])

     writeIORef' dirty True
     sendX sender
  return (makeUpdatingPainterWithArg paint dirty icons, Nothing)

makeWidget rs bgGraphs wd@(Graph attr (GraphDef typ tscale refresh_type) colors rate) = do
  let grInfo = extractGlobalGraphInfo wd
  let global = grInfo >>= flip lookup bgGraphs
  graphdata <- maybe (return $ makeGraph tscale (x_ $ size attr) (layers typ)) readIORef global
  let sampler = sampleTask typ
  makeUpdatingWidget rate $ updater sampler graphdata
  where
    updater sampler' graphdata' = Module $ \_  -> do
        let WidgetAttributes sz pos  _ bg _ _ = attr
            (Size ws hs, Size x0 y0) = (sz, pos)
        (sample, sampler'') <- runModule sampler' ()
        let graphdata'' = updateGraph ws graphdata' sample
        let samp = transpose . fmap (scaleG hs . reverse . tail . scanl (+) 0) . take ws . exportGraph $ graphdata''
        let colorTable = map toColor colors
        let painter = withDraw rs $ \d -> do
              drawRect (display rs) d bg x0 y0 ws hs
              let segments = map (map (makeSegment y0 hs) . filter ((/=0) . snd)
                                  . zip [x0+ws-1,x0+ws-2..]) samp
              -- print ("Paint Graph", segments, samp)
              mapM_ (drawColorSegment rs) $ zip segments colorTable
              -- FIXME: update only changed part
              return $ Just $ getBounds wd

        return (painter, updater sampler'' graphdata'')

makeWidget rs bgGraphs wd@NetStatus { netdev_ = netdev, refreshRate = rate} = do
  let grInfo = extractGlobalGraphInfo wd
  grdata <- readIORef . fromJust $ lookup (fromJust grInfo) bgGraphs
  let sampler = sampleTask (Net netdev)
      fmt :: String -> [Int] -> String
      fmt hdr v =
          let [inbound, _, outbound, _] = map (fmtBytes . round . (/realToFrac rate) . revf3 . fromIntegral) v
             in printf "%s In: %s/s : Out: %s/s" hdr inbound outbound

      printNet (LinearGraph []) = ["Loading..."]
      printNet (LinearGraph samples@(sample:_)) =
         [fmt "" sample, fmt "Avg" $ avgSamp samples ]

      updater sampler' graphdata' = Module $ \_ -> do
          (sample, sampler'') <- runModule sampler' ()
          let graphdata'' = updateGraph 20 graphdata' sample -- sync size with extractGlobalGraphInfo
          let msgs = printNet graphdata''
          return (msgs, updater sampler'' graphdata'')

  makeUpdatingTextWidget rs wd rate $ updater sampler grdata


makeWidget rs _ wd@CpuTop {refreshRate = rate} = do
  print rate
  let loadavg :: IO String
      loadavg = foldl (\a b -> a++" "++b) "Load avg: " . take 3 . words <$> readFully "/proc/loadavg"

      readProcs :: UTCTime -> IO ([Int], M.Map String (String, Int), UTCTime)
      readProcs ts = do
        cpu <- readCPU
        procs <- pickCpuUsage
        return (cpu, procs, ts)

      printCpu v Nothing = (: []) <$> loadavg

      printCpu v@(cpu2, procs2, ts2) (Just (cpu1, procs1, ts1)) = do
        avg <- loadavg
        let dt = getDt ts2 ts1
        top <- makeCpuDiff procs2 procs1 dt
        let msg = avg:top
        return msg

      updater r' = Module $ \_ -> do
         r'' <- getCurrentTime >>= readProcs
         msgs <- printCpu r'' r' :: IO [String]
         return (msgs, updater (Just r''))

  makeUpdatingTextWidget rs wd rate $ updater Nothing


makeWidget rs _ wd@MemStatus {refreshRate = rate} = do
  makeUpdatingTextWidget rs wd rate $ mkConstM $ do
    x <- readKeyValueFile ((`div` 1024) . read . head . words) "/proc/meminfo" :: IO (M.Map String Int)
    let x' = M.insert "Swap" ((x M.! "SwapTotal") - (x M.! "SwapFree")) x
    let values = ["MemFree", "Cached", "Buffers", "Swap", "Dirty", "Hugetlb"]
    let mem = map (\n -> printf "%7s: %5d MB" n (x' M.! n)) values
    zipWith (++) mem <$> memInfo


makeWidget rs _ wd@BatteryStatus {batteryName = n, refreshRate = rate} = do
  makeUpdatingTextWidget rs wd rate $ mkConstM $ do
    capacity <- readBatteryInt n "energy_full"
    rate <- readBatteryInt n "power_now"
    remainingCapacity <- readBatteryInt n "energy_now"
    state <- readBatteryString n "status" :: IO String
    let (h, m) = (remainingCapacity * 60 `div` rate) `divMod` 60
        percent = remainingCapacity * 100 `div` capacity
    return $ (: []) $ case state of
      "Discharging" | rate /= 0 -> printf "%d%%(%d:%02d)" percent h m
      _ -> printf "%d%%C" percent

makeWidget rs _ wd@BatteryRate {batteryName = n, refreshRate = rate} = do
  makeUpdatingTextWidget rs wd rate $ mkConstM $ do
    power <- readBatteryDouble n "power_now"
    volts <- readBatteryDouble n "voltage_now"
    return . (: []) $ printf "Current current: %.2f A" $ power / volts

makeWidget rs _ wd@Trayer {} = do
    let (pos, sz) = (position $ attr_ wd, size $ attr_ wd)
        cmd = trayerCmd (windowHeight rs) $ windowWidth rs - x_ pos - x_ sz
    print ("trayer cmd ", cmd)
    handle <- runCommand cmd
    return (mkConst Nothing, Nothing)

makeWidget rs _ wd = do
   print $ "Not implemented: " ++ show rs
   return (mkConst Nothing, Nothing)


inbounds :: WidgetAttributes -> Size -> Bool
inbounds WidgetAttributes {size = (Size ws hs), position = (Size wx wy)} (Size x y) =
  x > wx && x < wx + ws && y > wy && y < wy + hs

mouseHitWds :: [Widget] -> Size -> Maybe Widget
mouseHitWds wds pos =
  let match wd = inbounds (attr_ wd) pos
   in find match wds

mouseHitWins :: [WindowState] -> (Window, Maybe Size) -> Maybe TooltipInfo
mouseHitWins _ (w, Nothing) = Nothing
mouseHitWins wins (w, Just pos) =
  let matchWin (rs, _) = w == window rs
      matchWds (rs, wds) = mouseHitWds wds pos >>= \wd -> Just (rs, wd)
   in find matchWin wins >>= matchWds

maybeCopyArea rs Nothing = return ()
maybeCopyArea rs (Just (Rect x y width height)) = do
   let RenderState {display = dpy, buffer = buf, window = w, gc_ = gc} = rs
   copyArea dpy buf w gc (fi x) (fi y) (fi width) (fi height) (fi x) (fi y)
   sync dpy False


paintWindow :: RenderState -> [PaintWidgetCallback] -> PaintWindowModule
paintWindow rs drawableWidgets = Module $ \repaint -> do
   rootRect <- paintBackground rs repaint
   res <- mapM (`runModule` repaint ) drawableWidgets
   let (mbRects, drawableWidgets') = unzip res -- FIXME ($)
   let mbRect = combinedRects $ catMaybes (rootRect:mbRects)
   maybeCopyArea rs mbRect
   return ((), paintWindow rs drawableWidgets')
   where
        paintBackground rs RepaintUpdated = return Nothing
        paintBackground rs RepaintAll = do
          let RenderState dpy ww b gc w h bg _ _ = rs
          withDraw rs $ \d -> drawRect dpy d bg 0 0 w h
          return $ Just $ Rect 0 0 (fi w) (fi h)

        combinedRects [] = Nothing
        combinedRects (x:xs) = Just $ foldr mergeRect x xs

runThread :: IO () -> IO ThreadId
runThread = forkIO

allRec :: M.Map String Int -> a -> IO (M.Map String Int)
allRec m v = do
  c <- Heap.getClosureData v
  let i = show c
  let l = M.lookup i m
  handle m i c l
    where
       handle m i c (Just _) = return m
       handle m i c Nothing = do
             let m' = M.insert i 1 m
             let cc = Closures.allClosures c
             if null cc
                then
                  return m'
                else do
                    -- print $ show $ Closures.info c
                    foldM allRec m' cc

examine :: String -> a -> IO ()
examine label a = do
   let m = M.empty
   m' <- allRec m a
   print $ label ++ ", size " ++ show (M.size m')
   return ()

updateStep :: IO () -> [(Module IO () (), Period)] -> IO ()
updateStep onupdated timers = do
    let min_period [] = 3600 -- empty thread doing nothing
        min_period timers = minimum $ map snd timers
    print $ "Min refresh rate: " ++ show (min_period timers)
    iterateM_ (updateStep' onupdated (min_period timers)) (0, timers)

--updateStep' :: IO () -> NominalDiffTime -> NominalDiffTime -> [(Module IO () (), Period)] -> IO ()
updateStep' onupdatedIn min_periodIn (tmIn, timersIn) = do
  side_effects onupdatedIn min_periodIn tmIn timersIn
    where
       side_effects onupdated min_period tm timers = do
           time <- getCurrentTime >>= \t -> return $ diffUTCTime t epoch :: IO NominalDiffTime
           let actual_tm = fi (truncate (time / min_period)) * min_period

           let next_tm = tm + min_period
           next_tm' <- if actual_tm > next_tm + min_period
              then do
                 print $ "timer behind schedule by " ++ show (actual_tm - next_tm)
                 return actual_tm
              else return next_tm

           (updated, timers') <- unzip <$> mapM (updateOne (tm-min_period) (next_tm'-min_period)) timers
           when (or updated) onupdated

           actual_tm' <- getCurrentTime >>= \t -> return $ diffUTCTime t epoch :: IO NominalDiffTime
           let dt = next_tm' - actual_tm'
           when (dt > 0) $ threadDelay $ truncate (1000000 * dt)

           return (next_tm', timers')

updateOne :: NominalDiffTime -> NominalDiffTime -> (Module IO () (), Period) -> IO(Bool, (Module IO () (), Period))
updateOne tm next_tm (mod, period) = do
    let a = truncate (tm / period) :: Int
    let b = truncate (next_tm / period) :: Int
    if a /= b
        then execModule mod () >>= \m' -> return (True, (m', period))
        else return (False, (mod, period))

initWidgets :: Display -> Window -> BgGraphs -> WindowState -> IO (PaintWindowModule, ThreadId)
initWidgets dpy parent_w bgGraphs (rs, widgets) = do
  res <- mapM (makeWidget rs bgGraphs) widgets
  let (drawables, mbTimers) = unzip res
  let timers = catMaybes mbTimers :: [ UpdaterDef ]
  sender <- makeSenderX  parent_w
  -- global timers thread
  tid <- runThread $ updateStep (sendX sender) timers
  return (paintWindow rs drawables, tid)


eventLoop :: Display -> Window -> BgGraphs -> [PaintWindowModule] -> [WindowState]
                     -> Maybe TooltipInfo -> Maybe (Window, ThreadId) -> IO ()
eventLoop dpy parent_w bgGraphs painters wins tooltipInfo tooltipState = do
  (tooltipInfo', painters') <- ($!) allocaXEvent $ \ev -> do

    nextEvent dpy ev
    event <- getEvent ev
    let mouseHit ww x y = mouseHitWins wins (ww, Just (Size (fi x) (fi y)))

    case event of
       ClientMessageEvent {ev_window = ww, ev_data = 0:_} -> do
            painters' <- mapM (`execModule` RepaintUpdated) painters
            return (tooltipInfo, painters')

       ExposeEvent { ev_window = w } -> do
            painters' <- mapM (`execModule` RepaintAll) painters
            return (tooltipInfo, painters')

       ButtonEvent {ev_x = x, ev_y = y, ev_window = ww} -> do
            let hit = mouseHit ww x y
            let cmd = hit >>= (onclick . attr_ . snd)
            print cmd
            when (isJust cmd) $ void $ runCommand (fromJust cmd)
            return (hit, painters)

       MotionEvent {ev_x = x, ev_y = y, ev_window = ww} -> do
            return (mouseHit ww x y, painters)

       e@CrossingEvent {ev_x = x, ev_y = y, ev_window = ww} -> do
            let hit = if ev_event_type e == enterNotify
                 then mouseHit ww x y
                 else Nothing
            return (hit, painters)

       _ -> return (tooltipInfo, painters)
  if tooltipInfo' == tooltipInfo
  then eventLoop dpy parent_w bgGraphs painters' wins tooltipInfo tooltipState
  else do
    -- captures painters'
    let destroy Nothing = return painters'
        destroy (Just state) = do
             let (w, tid) = state
             killThread tid
             destroyWindow dpy w
             return $ tail painters'

    let tooltip = tooltipInfo' >>= (mbtooltip . attr_ . snd)
    let create Nothing paintersA = return (paintersA, Nothing)
        create (Just tooltip) paintersA = do
            let (rs, widget) = fromJust tooltipInfo'
            let RenderState {display = dpy}  = rs
            t <- createTooltip rs widget tooltip
            (painter, tid) <- initWidgets dpy parent_w bgGraphs t
            return (painter:paintersA, Just (window . fst $ t, tid))

    (painters'', tooltipState') <- destroy tooltipState >>= create tooltip
    eventLoop dpy parent_w bgGraphs painters'' wins tooltipInfo' tooltipState'


main :: IO ()
main = do
  xSetErrorHandler
  dpy <- openDisplay ""
  controlCh <- newChan

  wins <- mapM (makeBar dpy) bars

  let firstRs = fst $ head wins
  let parent_w = window firstRs
  -- forkOS $ copyChanToX controlCh $ window firstRs

  painters <- map fst <$> mapM (initWidgets dpy parent_w []) wins
  bgGraphs <- makeBackgroundGraphs wins

  eventLoop dpy parent_w bgGraphs painters wins Nothing Nothing
