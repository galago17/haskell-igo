-- SGF to list of coords for each color as well as parsed header

module ParseSGF where

import Types

adjCoords :: [(Int, Int)]
adjCoords = [(0, 1), (-1, 0), (1, 0), (0, -1)]

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
sgfToCoord (col, row) = (abcToInt col ['a'..'z'], abcToInt row ['a'..'z'])
  where
    abcToInt char lst = if head lst == char then 1 else 1 + abcToInt char (tail lst)

getCoords :: Char -> [String] -> [Coord]
getCoords color sgfBody = [sgfToCoord (col, row) | (clr:_:col:row:_) <- sgfBody, clr == color]

parseHeader :: [String] -> [HeaderArg]
parseHeader [] = []
parseHeader sgfHeader = (title, value) : parseHeader (tail sgfHeader)
  where
    title = takeWhile (/= '[') (head sgfHeader)
    value = takeWhile (/= ']') $ drop 1 $ dropWhile (/= '[') (head sgfHeader)

parseFile :: String -> [[String]]
parseFile contents = splitBlock (head nodes) ']' : [tail nodes]
  where
    nodes = splitOnChar (init $ tail $ tail contents) ';'

access :: Coord -> BoardState -> Point
access coord [bstones, wstones]
  | coord `elem` bstones = Black
  | coord `elem` wstones = White
  | otherwise = Empty

onBoard :: Int -> Coord -> Bool
onBoard boardSize (row, col) = (row `elem` [1..boardSize]) && (col `elem` [1..boardSize])

getAdjacent :: Coord -> [Coord]
getAdjacent (col, row) = map (\x -> (col + fst x, row + snd x)) adjCoords

countLiberties :: Int -> Coord -> Coord -> BoardState -> Int
countLiberties boardSize (oCol, oRow) (col, row) [bstones, wstones] =
  let
    color = access (col, row) [bstones, wstones]
    adjacent = filter (/= (oCol, oRow)) $ getAdjacent (col, row)
    check coord
      | not (onBoard boardSize coord) = 0
      | access coord [bstones, wstones] == Empty = 1
      | access coord [bstones, wstones] == color = countLiberties boardSize (col, row) coord [bstones, wstones]
      | otherwise = 0
  in
    sum (map check adjacent)
