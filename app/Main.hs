module Main (main) where

import Lib
import Text.Parsec

main = do 
    contents <- readFile "data/data.txt"
    case parse workoutFile "data/data.txt" contents of
        Left err -> print err
        Right xs -> print xs
