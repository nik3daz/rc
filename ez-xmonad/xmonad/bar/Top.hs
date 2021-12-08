
module Top (
  pickCpuUsage,
  makeCpuDiff,
  memInfo
  ) where


import System.Directory
import Data.Char
import Text.Printf
import qualified Data.Map as M
import System.IO.Error
import Control.Concurrent
import Data.Function
import Data.List
import Utils
import System.IO.Error

{-
0 pid           process id
1 tcomm         filename of the executable
2 state         state (R is running, S is sleeping, D is sleeping in an
                uninterruptible wait, Z is zombie, T is traced or stopped)
3 ppid          process id of the parent process
4 pgrp          pgrp of the process
5 sid           session id
6 tty_nr        tty the process uses
7 tty_pgrp      pgrp of the tty
8 flags         task flags
9 min_flt       number of minor faults
10cmin_flt      number of minor faults with child's
11maj_flt       number of major faults
12cmaj_flt      number of major faults with child's
13utime         user mode jiffies
14stime         kernel mode jiffies
15cutime        user mode jiffies with child's
16cstime        kernel mode jiffies with child's
17priority      priority level
18nice          nice level
19num_threads   number of threads
20it_real_value	(obsolete, always 0)
21start_time    time the process started after system boot
22vsize         virtual memory size
23rss           resident set memory size
24rsslim        current limit in bytes on the rss
-}

valuePicker selector file = (pid, (name, usage)) where
  w@(pid:rest) = words file
  usage = selector (drop (length(w) - 52) w)
  name = unwords $ take (length(w) - 51) rest

makeCpuDiff newCpuInfo cpuInfo sec = do
  let diff = M.elems $ M.differenceWith (\(n,a) (_,b) -> Just $ (n,a - b)) newCpuInfo cpuInfo
  let active = filter ( (/= 0) . snd) diff
  print $ show active
  let sorted = take 3 . reverse . sortBy (compare `on` snd) $ active
  return $ map output sorted where
    output (name, val) = printf "   %2d%% - %s" (perSec sec val) name

readFiles [] = return []
readFiles (pid:pids) = do
  content <- (readFile pid >>= return . Just) `catchIOError` \_ -> return Nothing
  case content of
    Nothing -> readFiles pids
    Just c -> do
      others <- readFiles pids
      return $ (c:others)
  
pickCpuUsage :: IO (M.Map String (String, Int))
pickCpuUsage = pickProcValues cpuSelector >>= return . M.fromList

pickProcValues selector = do
  x <- getDirectoryContents "/proc"
  let pids = map (printf "/proc/%s/stat") $ filter (isDigit . head) $ x :: [String]
  files <- readFiles pids
  return $! map (valuePicker selector) files

memInfo = do
  mem <- pickProcValues memSelector :: IO [(String, (String, Int))]
  return $! map display . take 4 . reverse . sortBy (compare `on` snd) . map (snd) $ mem where
    display (name, val) = printf " %7s - %s" (bytes val) name :: String
  

cpuSelector = sum . map read . take 2 . drop 13
memSelector = (*4096) . read . head . drop 23 :: [String] -> Int

main2 = do
  cpuInfo <- pickCpuUsage
  threadDelay 1000000
  newCpuInfo <- pickCpuUsage
  makeCpuDiff newCpuInfo cpuInfo 1 :: IO [String]
  threadDelay 1000000
  newCpuInfo <- pickCpuUsage
  makeCpuDiff newCpuInfo cpuInfo 2 :: IO [String]

main = do
  m <- memInfo
  print $ show m
