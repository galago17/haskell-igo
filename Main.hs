module Main where

import ParseSGF
import Board
import Types

main :: IO()
main = do
  file <- readFile "test2.sgf"
  let parsed = ParseSGF.parseFile file
  let body = parsed !! 1
  let boardSize = 19
  let bstones = getCoords 'B' body
  let wstones = getCoords 'W' body
  mapM_ putStrLn  (printBoard (getBoard boardSize [bstones, wstones]))
  print $ countLiberties boardSize (16, 4) (16, 4) [bstones, wstones]
