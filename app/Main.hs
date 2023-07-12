{-# LANGUAGE ExtendedDefaultRules #-}
module Main (main) where

import Parse (workoutFile)
import Types (Workout(..), Exercise(..), makeJson)
import Attendance (averageWorkOutsPerWeek)
import Cleaning (cleanWorkouts)

import Text.Parsec
import Data.Set
import Graphics.Matplotlib

--main = do 
    --contents <- readFile "data/data.txt"
    --case parse workoutFile "data/data.txt" contents of
        --Left err -> print err
        --Right xs -> print $ averageWorkOutsPerWeek xs

main = file "./data/output.png" (contourF (\a b -> sin (a*pi/180.0) + cos (b*pi/180.0)) (-100) 100 (-200) 200 10)
        
        

listUniqueExercises xs = (toList.fromList) $ Prelude.map name (concatMap (exercises) (cleanWorkouts xs))

        
