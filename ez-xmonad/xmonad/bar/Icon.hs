module Icon (
  Bitmap,
  CachedIcon(..),
  IconCache,
  makeIconCache,
  loadIconImage,
  getIconImage
  ) where

import Foreign.C.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign.Marshal.Array
import Foreign.Ptr
import Text.Printf
import GHC.Word
import Numeric
import System.IO.Error

import qualified Data.Map as M

data Bitmap a = Bitmap { width :: Int, height :: Int, pixels :: [a] } deriving (Show)
data CachedIcon = CachedIcon Int Int Image
data IconCache = IconCache { getDisplay :: Display, getAtom :: Atom, getCache :: (M.Map Word64 CachedIcon), getIconConfig :: IconConfig }

data IconConfig = IconConfig { pickSize :: Int
                             , postProcessing :: Bitmap [Word8] -> Bitmap [Word8]
                             , bgColor :: String
                             , cacheIcon :: Bool
                             }

defaultIconConfig = IconConfig {
   pickSize=16, postProcessing=(scaleNearest 22), bgColor="#000000FF",
   cacheIcon=True
   }

-- FIXME: move to client code
myIconConfig = defaultIconConfig {
   pickSize = 16,
   postProcessing = scaleLinear 25 . scaleLinear 64,
   cacheIcon = True,
   bgColor = "#2F1B40FF"
   }

makeIconCache dpy = do 
  iconProperty <- internAtom dpy "_NET_WM_ICON" False
  return $ IconCache dpy iconProperty M.empty myIconConfig

fetchIconData :: IconCache -> Window -> IO (Maybe [CLong])
fetchIconData (IconCache dpy atom _ _) win = getWindowProperty32 dpy atom win


-- FIXME: deprecated
parseIconData :: Maybe [CLong] -> (CLong, CLong, [CInt])
parseIconData Nothing = (1, 1, [0])
parseIconData (Just (width:height:xs)) = (width, height, take sz iconData) where
  iconData = map fromIntegral xs :: [CInt]
  sz = fromIntegral (width * height)

splitPixel :: CLong -> [Word8]
splitPixel px = map fromIntegral [r,g,b,a] where
 (xr, r) = px `divMod` 256
 (xg, g) = xr `divMod` 256
 (xb, b) = xg `divMod` 256
 (_,  a) = xb `divMod` 256

makeIconList :: [CLong] -> [Bitmap [Word8]]
makeIconList [] = []
makeIconList (w:h:rest) = Bitmap width height (map splitPixel pixels) : makeIconList rest' where
  [width,height] = map fromIntegral [w,h]
  (pixels,rest') = splitAt (width * height) rest

bestMatch sz (icon:icons) = bestMatch' sz icon icons where
  bestMatch' sz icon [] = icon
  bestMatch' sz y (x:xs) = bestMatch' sz betterIcon xs where
    betterIcon = if xBetter then x else y
    [wx, wy] = map width [x,y]
    (xBigger, xBigEnough, yBigEnough) = (wx > wy, wx > sz, wy > sz)
    xBetter = (xBigger && not yBigEnough) || (yBigEnough && xBigEnough && not xBigger)


defaultIcon = [1, 1, 0]
getIconDataDefault mb = case mb of
  Just ic -> ic
  Nothing -> defaultIcon

loadIconImage :: IconCache -> Window -> IO (IconCache)
loadIconImage c@(IconCache dpy atom cache cfg) win = do
  case M.lookup win cache of
     Just cachedIcon -> do
        print "Using cached icon"
        return c
     Nothing -> do
       print "Start fetching icon data"
       iconRawData <- fetchIconData c win `catchIOError` \x -> return $ Just defaultIcon
       print "Stop fetching icon data"
       let iconRawData' = getIconDataDefault iconRawData
           icons = makeIconList $ iconRawData'
           icon = bestMatch (pickSize cfg) icons
           setBG = colorFilter . onTop . toColor $ bgColor cfg
           (Bitmap width height iconData) = setBG . (postProcessing cfg) $ icon
           scr = defaultScreen dpy
           visual = defaultVisual dpy scr
       pixels <- newArray $ concat iconData
       img <- createImage dpy visual 24 zPixmap 0 (castPtr pixels) (fromIntegral width)
                                              (fromIntegral height) 32 (fromIntegral 0)
       let cachedImage = CachedIcon width height img
       let newCache = c { getCache = M.insert win cachedImage cache }
       return newCache

getIconImage :: Window -> IconCache -> Maybe CachedIcon
getIconImage win cache = M.lookup win (getCache cache)
       

chunksOf n [] = []
chunksOf n s = x : chunksOf n xs where (x,xs) = splitAt n s

toColor str = map (fst . head . readHex) . chunksOf 2 $ tail str 

blend a1 a2 (v1, v2) = fromIntegral $ (a1' * v1' + a2' * v2') `div` 255 where
 [a1', a2', v1', v2'] = map fromIntegral [a1, a2, v1, v2]


onTop :: [Word8] -> [Word8] -> [Word8]
onTop bg color = map (blend (255-a) a) $ zip bg color where
  a = last color

scaleSimple :: Int -> Int -> [a] -> [a]
scaleSimple size newsize ar = pick newsize newsize size ar where
  pick acc newsize size [] = []
  pick acc newsize size (x:xs) =
    if acc >= size
    then (x : pick (acc - size) newsize size (x:xs))
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
  mul a xs = map (mul a) $ xs
  normalize a xs = map (normalize a) $ xs
  add xs ys = map (pair add) $ zip xs ys

scale1D newsize a = map (normalize size) . scale size newsize $ a where
  size = length a

scale2D :: Int -> Int -> Int -> [[Int]] -> [[Int]]
scale2D w h newsize = concat . (normalize (w * h)) . scale h newsize
                   . (map $ scale w newsize) . chunksOf w

scaleNearest :: Int -> Bitmap [Word8] -> Bitmap [Word8]
scaleNearest sz (Bitmap w h px ) = Bitmap sz sz . concat 
     . scaleSimple w sz . (map $ scaleSimple w sz) . chunksOf w $ px

scaleLinear sz (Bitmap w h px) = Bitmap sz sz
      . cast . scale2D w h sz . cast $ px

cast :: (Integral a, Num b) => [[a]] -> [[b]]
cast = map (map fromIntegral)

black [r,g,b,a] = [0,0,0,a]
nothing [r,g,b,a] = [0,0,0,0]
setColor str = \[_,_,_,a2] -> [r,g,b, fromIntegral $ (fromIntegral a2 * fromIntegral a) `div` 255] where
  [r,g,b,a] = toColor str

colorFilter :: ([Word8] -> [Word8]) -> Bitmap [Word8] -> Bitmap [Word8]
colorFilter f (Bitmap w h px) = Bitmap w h newpx where
  newpx = map f $ px

shift :: Int -> Int -> Bitmap [Word8] -> Bitmap [Word8]
shift x y (Bitmap w h px) = Bitmap w h (concat newgrid2) where
  grid = chunksOf w px
  row = map nothing $ head grid
  cell = nothing . head $ px
  newgrid = case y > 0 of
    True -> (take y $ repeat $ row) ++ (take (h - y) grid)
    False -> (drop (-y) grid) ++ (take (-y) $ repeat $ row)
  newgrid2 = case x > 0 of
    True -> map (\r -> (take x $ repeat $ cell) ++ (take (w - x) r)) newgrid
    False -> map (\r -> (drop (-x) r) ++ (take (-x) $ repeat $ cell)) newgrid

resize w2 h2 (Bitmap w h px) = Bitmap w2 h2 $ concat newgrid2 where
  grid = chunksOf w px
  row = map nothing $ head grid
  cell = nothing . head $ px
  newgrid = (take (min h h2) grid) ++ (take (max 0 $ h2 - h) $ repeat $ row)
  newgrid2 = map (\r -> (take (min w w2) r) ++ (take (max 0 $ w2 - w) $ repeat $ cell)) newgrid

layer :: Bitmap [Word8] -> Bitmap [Word8] -> Bitmap [Word8]
layer (Bitmap w h px) (Bitmap w2 h2 px2) = Bitmap (min w w2) (min h h2) newpx where
  grid = chunksOf w px
  grid2 = chunksOf w2 px2
  newpx = concat . map (map (pair onTop) . pair zip) $ zip grid2 grid

shadow :: Int -> String -> Bitmap [Word8] -> Bitmap [Word8]
shadow off c b = layer b2 $ shift off off . colorFilter (setColor c) $ b2 where
  b2 = resize (off + (width b)) (off + (height b)) b

