module Types where

data Point = Black | White | Empty deriving (Eq, Show)
type Board = [[Point]]

type Coord = (Int, Int)
type HeaderArg = (String, String)
type BoardState = [[Coord]]
