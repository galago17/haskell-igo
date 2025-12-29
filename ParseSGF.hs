module ParseSGF where

import Types
import Data.Char (digitToInt)

splitOnChar :: String -> Char -> [String]
splitOnChar str char = lines replaced
  where
    replaced = [if x == char then '\n'  else x | x <- str]

splitBlock :: String -> Char -> [String]
splitBlock [] _ = []
splitBlock str char = first : splitBlock rest char
  where
    first = takeWhile (/= char) str ++ [char]
    rest = drop 1 $ dropWhile (/= char) str

sgfToCoord :: (Char, Char) -> Coord
sgfToCoord (col, row) = ((['A'..'J'] ++ ['K'..'Z']) !! abcToInt col ['a'..'z'], abcToInt row ['a'..'z'])
  where
    abcToInt char lst = if head lst == char then 1 else 1 + abcToInt char (tail lst)
-- >>> sgfToCoord ('p', 'p')
-- ('Q',16)

getCoords :: Char -> [String] -> [Coord]
getCoords color sgfBody = [sgfToCoord (col, row) | (clr:_:col:row:_) <- sgfBody, clr == color]

parseFile :: String -> [[String]]
parseFile contents = splitBlock (head nodes) ']' : [tail nodes]
  where
    nodes = splitOnChar (init $ tail $ tail contents) ';'

