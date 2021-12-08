
module Utils (
  pair,
  join,
  split,
  split1,
  strip,
  fi,
  loop,
  bytes,
  perSec,
  readKeyValueFile
  ) where

import Text.Printf
import qualified Data.Map as M

pair op (a,b) = op a b
join sep [] = ""
join sep (x:xs) = foldl (\x y ->  x++sep++y ) x xs

split :: Char -> String -> [String]
split ch s =  case dropWhile (==ch) s of
  "" -> []
  s' -> word : split ch s''
    where (word, s'') = break (==ch) s'

strip :: String -> String
strip s = reverse . dropWhile p . reverse . dropWhile p $ s where
  p = (==' ')

split1 ch s = (x, safeTail xs) where
  safeTail [] = []
  safeTail (x:xs) = xs
  (x,xs) = break (==ch) s

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

loop f input = do
  output <- f input
  loop f output

bytes :: Int -> String
bytes b 
  | b < 1024 = printf "%d bytes" b
  | b < (1024 * 1024) = printf "%d KiB" (b `div` 1024)
  | b < (10 * 1024 * 1024) = printf "%.1f MB" (bf / (1024 * 1024))
  | b >= (10 * 1024 * 1024) = printf "%d MB" (b `div` (1024 * 1024)) where
    bf = fromIntegral b :: Double

readKeyValueFile pp filename = readFile filename >>= return . makeMap where
  makeMap l = M.fromList $ map parseLine . lines $ l
  parseLine l = (strip k, pp $ v) where
     (k,v) = split1 ':' l

perSec :: (Integral i) => Double -> i -> i
perSec sec val = truncate $ (fi val) / sec


