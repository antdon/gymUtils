{-# LANGUAGE ExtendedDefaultRules #-}
module Main (main) where

import Parse (workoutFile)
import Types (Workout(..), Exercise(..), makeJson)
import Attendance (averageWorkOutsPerWeek)
import Cleaning (cleanWorkouts)

import Text.Parsec
import Data.Set
import Data.Maybe
import Graphics.Matplotlib
import Data.Time.Calendar.OrdinalDate (showOrdinalDate)
import Data.Time (Day(ModifiedJulianDay), diffDays)
import Data.Aeson (Series)
import Data.Aeson.Encoding (day)
import Debug.Trace

main :: IO (Either String String)
main = do
    contents <- readFile "data/data.txt"
    case parse workoutFile "data/data.txt" contents of
        Left err -> undefined
        Right xs -> do
            xs <- return (cleanWorkouts xs)
            -- print sumUp.dayDifferences $ getDatesExerciseOccured xs "bench press"
            scatterBench xs

            --print $ getExercisesByName xs "bench press"

--main = file "./data/output.png" (contourF (\a b -> sin (a*pi/180.0) + cos (b*pi/180.0)) (-100) 100 (-200) 200 10)



listUniqueExercises :: [Workout] -> [String]
listUniqueExercises xs = (toList.fromList) $ Prelude.map name (concatMap (exercises) (cleanWorkouts xs))

--scatterBench :: [Workout] -> IO ()
scatterBench :: [Workout] -> IO (Either String String)
scatterBench xs = file "./data/output.png" ((scatter (sumUp.dayDifferences $ getDatesExerciseOccured xs "bench press") (Prelude.map weight (getExercisesByName xs "bench press"))) ## lit ".ylim(bottom=0)")

getExercisesByName :: [Workout] -> String -> [Exercise]
getExercisesByName xs exName = Prelude.filter (\x -> exName == name x) (concatMap exercises (Prelude.filter (ligit . date) xs))
    where ligit Nothing = False 
          ligit (Just _) = True

getDatesExerciseOccured :: [Workout] -> String -> [Day]
getDatesExerciseOccured xs exName = removeNothings $ Prelude.map date (Prelude.filter (\y -> any (\x -> exName == name x) (exercises y)) xs)

dayDifferences :: [Day] -> [Integer]
dayDifferences (x1:x2:xs) = diffDays x2 x1 : dayDifferences (x2 : xs)
dayDifferences (_:_) = [0]
dayDifferences [] = []


sumUp :: [Integer] -> [Integer]
sumUp = sumUp' []
    where
        sumUp' before (x:xs) = (sum before + x) : sumUp' (before ++ [x]) xs
        sumUp' _ [] = []



removeNothings :: [Maybe a] -> [a]
removeNothings (Nothing:xs) = removeNothings xs
removeNothings (Just x:xs) = x : removeNothings xs
removeNothings [] = []

