module Main where

import ParseSGF
import Types

bstonechar = '●'
wstonechar = '○'

access :: Coord -> [Coord] -> [Coord] -> Point 
access coord bstones wstones 
  | coord `elem` bstones = Black
  | coord `elem` wstones = White
  | otherwise = Empty

getRow :: Int -> Int -> Int -> [Coord] -> [Coord] -> String
getRow rown boardSize i bstones wstones
  | i > boardSize = [] 
  | coord `elem` bstones =  bstonechar: ' ' : getRow rown boardSize (i + 1) bstones wstones
  | coord `elem` wstones = wstonechar : ' ': getRow rown boardSize (i + 1) bstones wstones
  | otherwise = '+' : ' ' : getRow rown boardSize (i + 1) bstones wstones
  where coord = (['A'..'Z'] !! i, rown)
printBoard :: [Coord] -> [Coord] -> [String]
printBoard bstones wstones = map (\x -> getRow x 19 1 bstones wstones) [1..19]



main :: IO()
main = do
  file <- readFile "test2.sgf"
  let parsed = ParseSGF.parseFile file
  let body = parsed !! 1
  let bstones = getCoords 'B' body
  let wstones = getCoords 'W' body
  mapM_ putStrLn (printBoard bstones wstones)


