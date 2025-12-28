import Data.List 

data Point = Black | White | Empty deriving (Show)

main :: IO ()
main = do
  filename <- getLine
  file <- readFile filename
  print $ init $ tail file

