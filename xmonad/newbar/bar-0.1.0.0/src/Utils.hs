module Utils (
  --join,
  strip,
  fi,
  fmtBytes,
  perSec,
  readKeyValueFile,
  readFully
  ) where

import Text.Printf
import qualified Data.Map as M
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as Char8

strip :: String -> String
strip = reverse . dropWhile p . reverse . dropWhile p where
  p = (==' ')

split1 :: Eq t => t -> [t] -> ([t], [t])
split1 ch s = (x, drop 1 xs) where
  (x,xs) = break (==ch) s

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

readFully :: FilePath -> IO String
readFully f = Char8.unpack <$> Str.readFile f

fmtBytes :: Int -> String
fmtBytes b
  | b < 1024 = printf "%d bytes" b
  | b < (1024 * 1024) = printf "%d KiB" (b `div` 1024)
  | b < (10 * 1024 * 1024) = printf "%.1f MB" (bf / (1024 * 1024))
  | b >= (10 * 1024 * 1024) = printf "%d MB" (b `div` (1024 * 1024))
  | otherwise = ""
 where
    bf = fromIntegral b :: Double

readKeyValueFile :: (String -> a) -> FilePath -> IO (M.Map String a)
readKeyValueFile pp filename = makeMap <$> readFully filename where
  makeMap l = M.fromList $ map parseLine . lines $ l
  parseLine l = (strip k, pp v) where
     (k,v) = split1 ':' l

perSec :: (Integral i) => Double -> i -> i
perSec sec val = truncate $ fi val / sec


