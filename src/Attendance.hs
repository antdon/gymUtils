module Attendance
    (averageWorkOutsPerWeek
    ) where

import Data.Time
import Data.Time.Clock.System
import Parse (workoutFile)
import Types (Workout(..), Exercise(..))

{-
Given a list of dates we want to them placed into sublists for each week
Args:
    current: the current day :: Day
    days: the remaining days :: [Maybe Day]
Returns:
    a list of weeks :: [[Maybe Day]]
-}
weeks :: Day -> [Maybe Day] -> [[Maybe Day]]
weeks current (Nothing:days) = weeks current days
weeks current (day:days)
    | current > maximum (map (incompRemoveMaybe) (day:days)) = []
    | diffDays current (incompRemoveMaybe day) > 7 = (day:week) : rest
    | otherwise = [day] : weeks (iterate succ current !! 7) (days)
    where 
        (week:rest) = weeks current days
        incompRemoveMaybe (Just day) = day -- we can do this because the Nothing case is taken care of above
        incompRemoveMaybe Nothing = parseTimeOrError True defaultTimeLocale "%d/%m/%y" "11/07/23"

{-
Given a list of Maybe days, return the first Just day 
Args:
    days: a list of days :: [Maybe Day]
Returns:
    the first day :: Day
-}
firstDay :: [Maybe Day] -> Day
firstDay [] = error "No days"
firstDay (Nothing:days) = firstDay days
firstDay (Just day:days) = day

{- 
Given a list of workouts, return the average number of workouts per week 
Args:
    workouts: a list of workouts :: [Workout]
Returns:
    the average number of workouts per week :: Int
-}
averageWorkOutsPerWeek :: [Workout] -> Int
averageWorkOutsPerWeek workouts = sum attendances `div` length attendances
    where attendances = map length $ weeks (firstDay (map date workouts)) (map date workouts)
        