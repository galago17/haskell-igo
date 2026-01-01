module Board where

import ParseSGF
import Types
import Util

bstonechar, wstonechar, emptychar :: String
bstonechar = "●"
wstonechar = "○"
emptychar = "+"


getRow :: Int -> Int -> Int -> BoardState -> [Point]
getRow rown boardSize i [bstones, wstones]
  | i > boardSize = []
  | coord `elem` bstones =  Black : getRow rown boardSize (i + 1) [bstones, wstones]
  | coord `elem` wstones =  White : getRow rown boardSize (i + 1) [bstones, wstones]
  | otherwise = Empty : getRow rown boardSize (i + 1) [bstones, wstones]
  where coord = (i, rown)

getBoard :: Int -> BoardState -> [[Point]]
getBoard boardSize [bstones, wstones] = map (\x -> getRow x boardSize 1 [bstones, wstones]) [1..boardSize]

printBoard :: [[Point]] -> [String]
printBoard = map convert
  where
    conversion = [(Black, bstonechar), (White, wstonechar), (Empty, emptychar)]
    convert [] = []
    convert row = getValue (head row) conversion ++ " " ++ convert (tail row)

allAlive :: Int -> [Coord] -> BoardState -> Bool
allAlive boardSize coords state = any not (map (\x -> isAlive boardSize x state) coords)

isAlive :: Int -> Coord -> BoardState -> Bool
isAlive boardSize coord state = countLiberties boardSize coord coord state == 0
