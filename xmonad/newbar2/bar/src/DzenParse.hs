module DzenParse (
  Message(..),
  MbColor,
  parseLine,
  dzenMsg
  ) where

import Data.Map()
import Graphics.X11.Xlib (Window)
import Text.ParserCombinators.Parsec

dzenMsg :: String
dzenMsg = "^fg(white)^bg(#2b4f98) 1 ^fg()^bg()^fg(black)^bg(#cccccc) 2 ^fg()^bg()^fg(black)^bg(#cccccc) 3 ^fg()^bg()^fg(black)^bg(#cccccc) 9 ^fg()^bg()  ^fg(#202020){56623107}dzen.sh (~/.xmonad/ivan) - GVIM^fg()"

parseLine :: String -> [Message]
parseLine input = do
   let res = parse (parseMessage Nothing Nothing "") "(unknown)" input :: Either ParseError [Message]
   case res of
     (Right x) -> mergeText x
     (Left _) -> [ Text Nothing Nothing input ]

data ColorType = Foreground | Background
type MbColor = Maybe String
data Message = Text MbColor MbColor String | IconRef Window deriving Show

parseColorType :: GenParser Char st ColorType
parseColorType = (string "fg" >> return Foreground) <|> (string "bg" >> return Background)

parseColor :: GenParser Char st MbColor
parseColor = (\c -> if c == "" then Nothing else Just c) <$> many (noneOf ")") 

parseMessage :: MbColor -> MbColor -> String -> GenParser Char st [Message]
parseMessage fg bg s =
  let wrapup f = do
       rest <- f
       return $ if s == "" then rest else Text fg bg (reverse s) : rest in
        try (do
          _ <- string "^"
          typ <- parseColorType
          _ <- string "("
          color <- parseColor
          _ <- string ")"
          wrapup (case typ of
            Foreground -> parseMessage color bg ""
            Background -> parseMessage fg color ""))
        <|> try (do
          _ <- string "{"
          winid <- many1 digit 
          _ <- string "}"
          rest <- wrapup (parseMessage fg bg "")
          return (IconRef (read winid) : rest))
        <|> (do
          c <- anyChar
          parseMessage fg bg (c : s))
        <|> wrapup (return [])

mergeText :: [Message] -> [Message]
mergeText [] = []
mergeText (t1@(Text fg1 bg1 s1) : t2@(Text fg2 bg2 s2 : xs)) = 
  if fg1 == fg2 && bg1 == bg2
  then mergeText (Text fg1 bg1 (s1 ++ s2) : xs)
  else t1 : mergeText t2
mergeText (x : xs) = x : mergeText xs
