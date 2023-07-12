module Main (main) where

import Parse (workoutFile)
import Types (Workout(..), Exercise(..), makeJson)
import Attendance (averageWorkOutsPerWeek)
import Cleaning (cleanWorkouts)

import Text.Parsec
import Data.Set

main = do 
    contents <- readFile "data/data.txt"
    case parse workoutFile "data/data.txt" contents of
        Left err -> print err
        Right xs -> print $ averageWorkOutsPerWeek xs
        
        

listUniqueExercises xs = (toList.fromList) $ Prelude.map name (concatMap (exercises) (cleanWorkouts xs))

        
