module Types where

data Point = Black | White | Empty deriving (Show)
type Board = [[Point]]

type Coord = (Char, Int)

