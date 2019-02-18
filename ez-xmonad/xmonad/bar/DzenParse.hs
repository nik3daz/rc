module DzenParse (
  AnnotationType(..),
  Message(..),
  parseLine
  ) where

import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.String
--import Text.ParserCombinators.Parsec.Expr
--import Text.ParserCombinators.Parsec.Token
import qualified Data.Map as M

dzenMsg = "^fg(white)^bg(#2b4f98) 1 ^fg()^bg()^fg(black)^bg(#cccccc) 2 ^fg()^bg()^fg(black)^bg(#cccccc) 3 ^fg()^bg()^fg(black)^bg(#cccccc) 9 ^fg()^bg()  ^fg(#202020){56623107}dzen.sh (~/.xmonad/ivan) - GVIM^fg()"

parseLine :: String -> [Message]
parseLine input = do
   let res = parse parseMessage "(unknown)" input :: Either ParseError [Message]
   case res of
     (Right x) -> mergeText . collapsRepeativeAnnotations . stripUnusedAnnotations $ x
     (Left _) -> [ Text input ]

data AnnotationType = Foreground | Background deriving (Show, Eq, Ord)
data Message = Annotation AnnotationType String | IconRef Int | Text String deriving (Show)

parseAnnotationType :: GenParser Char st AnnotationType
parseAnnotationType = (string "fg" >> return Foreground)
                  <|> (string "bg" >> return Background)

parseColor = many (noneOf ")")

parseMessage :: GenParser Char st [Message]
parseMessage = (try $ do
           string "^"
           typ <- parseAnnotationType
           string "("
           color <- parseColor
           string ")"
           text <- parseMessage
           return $ ((Annotation typ color) : text))
        <|> (try $ do
           string "{"
           winid <- many1(digit) 
           string "}"
           text <- parseMessage
           return $ (IconRef (read winid) : text))
        <|> (do
               c <- anyChar
               rest <- parseMessage
               return $ case rest of
                 (Text msg : xs) -> Text (c:msg) : xs
                 otherwise -> Text [c] : rest)
        <|> (return [])

stripUnusedAnnotations = strip M.empty where
  strip _ [] = []
  strip m (x:xs) = case x of
    (Annotation k _) -> strip (M.insert k x m) xs
    _ -> (M.elems m) ++ (x : (strip M.empty xs))

collapsRepeativeAnnotations = collapse M.empty where
  collapse _ [] = []
  collapse m (x:xs) = case x of
    (Annotation k v) ->
       let oldv = M.lookup k m;
           newv = Just v in
       case oldv == newv of
          True -> collapse m xs
          False -> x : collapse (M.insert k v m) xs
    _ -> x : (collapse m xs)

mergeText :: [Message] -> [Message]
mergeText [] = []
mergeText ((Text t1) : (Text t2) : xs) = mergeText (Text (t1 ++ t2) : xs)
mergeText (x : xs) = x : mergeText xs
