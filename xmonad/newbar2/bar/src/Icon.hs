module Icon (
  Bitmap,
  CachedIcon(..),
  IconCache,
  makeIconCache,
  loadIconImage,
  getIconImage
  ) where

import Data.Maybe
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.Word
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Numeric
import System.IO.Error

import qualified Data.Map as M

data Bitmap a = Bitmap { width :: Int, height :: Int, pixels :: [a] } deriving (Show)
data CachedIcon = CachedIcon Int Int Image
data IconCache = IconCache { getDisplay :: Display, getAtom :: Atom, getCache :: M.Map Word64 CachedIcon, getIconConfig :: IconConfig }

data IconConfig = IconConfig { pickSize :: Int
                             , postProcessing :: Bitmap [Word8] -> Bitmap [Word8]
                             , bgColor :: String
                             }

makeIconConfig = IconConfig {
   pickSize = 16,
   postProcessing = shadow 2 "#00000050" . shift 0 (-1) . scaleLinear 25 . shadow 2 "#000000FF" . scaleLinear 64,
   bgColor = "#401B2FFF"
   }

makeIconCache dpy = do 
  iconProperty <- internAtom dpy "_NET_WM_ICON" False
  return $ IconCache dpy iconProperty M.empty makeIconConfig

fetchIconData :: IconCache -> Window -> IO (Maybe [CLong])
fetchIconData (IconCache dpy atom _ _) = getWindowProperty32 dpy atom

splitPixel :: CLong -> [Word8]
splitPixel px = map fromIntegral [r,g,b,a] where
 (xr, r) = px `divMod` 256
 (xg, g) = xr `divMod` 256
 (a, b) = xg `divMod` 256

makeIconList :: [CLong] -> [Bitmap [Word8]]
makeIconList [] = []
makeIconList (w:h:rest) = Bitmap width height (map splitPixel pixels) : makeIconList rest' where
  [width,height] = map fromIntegral [w,h]
  (pixels,rest') = splitAt (width * height) rest

bestMatch sz (icon:icons) = foldl betterIcon icon icons where
    betterIcon a b = if xBetter then a else b where
      [wa, wb] = map width [a,b]
      (aBigger, aBigEnough, bBigEnough) = (wa > wb, wa > sz, wb > sz)
      xBetter = (aBigEnough && bBigEnough) == not aBigger


defaultIcon = [1, 1, 0]

loadIconImage :: IconCache -> Window -> IO (IconCache, CachedIcon)
loadIconImage c@(IconCache dpy atom cache cfg) win =
  case M.lookup win cache of
     Just cachedIcon -> do
        print "Using cached icon"
        return (c, cachedIcon)
     Nothing -> do
       print "Start fetching icon data"
       iconRawData <- fetchIconData c win `catchIOError` \_ -> return $ Just defaultIcon
       print "Stop fetching icon data"
       let iconRawData' = fromMaybe defaultIcon iconRawData
           icons = makeIconList iconRawData'
           icon = bestMatch (pickSize cfg) icons
           setBG = colorFilter . onTop . toColor $ bgColor cfg
           (Bitmap width height iconData) = setBG . postProcessing cfg $ icon
           scr = defaultScreen dpy
           visual = defaultVisual dpy scr
       pixels <- newArray $ concat iconData
       img <- createImage dpy visual 24 zPixmap 0 (castPtr pixels) (fromIntegral width)
                                              (fromIntegral height) 32 0
       let cachedImage = CachedIcon width height img
       let newCache = c { getCache = M.insert win cachedImage cache }
       return (newCache, cachedImage)

getIconImage :: Window -> IconCache -> Maybe CachedIcon
getIconImage win cache = M.lookup win (getCache cache)
       

chunksOf _ [] = []
chunksOf n s = x : chunksOf n xs where (x,xs) = splitAt n s

toColor str = map (fst . head . readHex) . chunksOf 2 $ tail str 

blend a1 a2 (v1, v2) = fromIntegral $ (a1' * v1' + a2' * v2') `div` 255 where
 [a1', a2', v1', v2'] = map fromIntegral [a1, a2, v1, v2]


onTop :: [Word8] -> [Word8] -> [Word8]
onTop bg color = map (blend (255-a) a) $ zip bg color where
  a = last color

scaleSimple :: Int -> Int -> [a] -> [a]
scaleSimple size newsize = pick newsize newsize size where
  pick acc newsize size [] = []
  pick acc newsize size (x:xs) =
    if acc >= size
    then x : pick (acc - size) newsize size (x:xs)
    else pick (acc + newsize) newsize size xs

class Math a where
  mul :: Int -> a -> a
  add :: a -> a -> a
  normalize :: Int -> a -> a
  scale :: Int -> Int -> [a] -> [a]
  scale size newsize (a:ar) = pick 0 0 0 zero (a:ar) where
    zero = mul 0 a
    pick pos newpos maxpos acc [] = []
    pick pos newpos maxpos acc (a:ar) = if (pos + newsize) < (newpos + size)
      then let pos' = pos + newsize;
               acc' = ((pos' - maxpos) `mul` a) `add` acc in
           pick pos' newpos pos' acc' ar
      else let newpos' = newpos + size;
               acc' = ((newpos' - maxpos) `mul` a) `add` acc in
           (acc' : pick pos newpos' newpos' zero (a:ar))
    

instance Math Int where
  mul a x = a * x
  normalize a x = x `div` a
  add x y = x + y

pair op (a,b) = op a b

instance (Math a) => Math [a] where
  mul a = map (mul a)
  normalize a = map (normalize a)
  add xs ys = map (pair add) $ zip xs ys

scale1D newsize a = map (normalize size) . scale size newsize $ a where
  size = length a

scale2D :: Int -> Int -> Int -> [[Int]] -> [[Int]]
scale2D w h newsize = concat . normalize (w * h) . scale h newsize
                   . map (scale w newsize) . chunksOf w

scaleNearest :: Int -> Bitmap [Word8] -> Bitmap [Word8]
scaleNearest sz (Bitmap w h px ) = Bitmap sz sz . concat 
     . scaleSimple w sz . map (scaleSimple w sz) . chunksOf w $ px

scaleLinear sz (Bitmap w h px) = Bitmap sz sz
      . cast . scale2D w h sz . cast $ px

cast :: (Integral a, Num b) => [[a]] -> [[b]]
cast = map (map fromIntegral)

black [r,g,b,a] = [0,0,0,a]
nothing = [0,0,0,0]
setColor str = \[_,_,_,a2] -> [r,g,b, fromIntegral $ (fromIntegral a2 * fromIntegral a) `div` 255] where
  [r,g,b,a] = toColor str

colorFilter :: ([Word8] -> [Word8]) -> Bitmap [Word8] -> Bitmap [Word8]
colorFilter f (Bitmap w h px) = Bitmap w h newpx where
  newpx = map f px

shift :: Int -> Int -> Bitmap [Word8] -> Bitmap [Word8]
shift x y (Bitmap w h px) = Bitmap w h (concat newgrid2) where
  grid = chunksOf w px
  row = replicate w nothing
  newgrid = if y > 0
    then replicate y row ++ take (h - y) grid
    else drop (-y) grid ++ replicate (-y) row
  newgrid2 = if x > 0
    then map (\r -> replicate x nothing ++ take (w - x) r) newgrid
    else map (\r -> drop (-x) r ++ replicate (-x) nothing) newgrid

resize w2 h2 (Bitmap w h px) = Bitmap w2 h2 $ concat newgrid2 where
  grid = chunksOf w px
  newgrid = take (min h h2) grid ++ replicate (max 0 $ h2 - h) (replicate w nothing)
  newgrid2 = map (\r -> take (min w w2) r ++ replicate (max 0 $ w2 - w) nothing) newgrid

layer :: Bitmap [Word8] -> Bitmap [Word8] -> Bitmap [Word8]
layer (Bitmap w h px) (Bitmap w2 h2 px2) = Bitmap (min w w2) (min h h2) newpx where
  grid = chunksOf w px
  grid2 = chunksOf w2 px2
  newpx = concatMap (map (pair onTop) . pair zip) $ zip grid2 grid

shadow :: Int -> String -> Bitmap [Word8] -> Bitmap [Word8]
shadow off c b = layer b2 $ shift off off . colorFilter (setColor c) $ b2 where
  b2 = resize (off + width b) (off + height b) b

