{-
Below is the list of exercise names that have been used. Some of these refer to the same
exercise. This Module should be used to clean up the data and make sure that all the exercise
names are standardised.

"[\"Assisted Dip \",\"Assisted dip \",
\"Assisted dips \",\"Assisted pull ups \",
\"Assisted pullups \",\"Bench \",\"Bench press \",
\"Bent leg raises \",\"Bent over dumbbell pull \",
\"Bicep curl \",\"Bicep curls \",\"Calf machine \",
\"Calf raises \",\"Curls \",\"Deadlift \",\"Decline situps \",
\"Dips \",\"Dumbbell Romanians \",\"Dumbbell curls \",
\"Dumbbell shoulder press \",\"Dumbell curls \",
\"Dumbell shoulder press \",\"Fly machine \",\"Fly pull \",
\"Flys machine \",\"Hanging bent leg raises \",
\"Hip thrust machine \",\"Incline bench press \",
\"Incline dumbbell bench \",\"Incline dumbell bench \",
\"Lat pull down \",\"Lat pulldown \",\"Lateral raises \",
\"Leant over dumbbell pull \",\"Leg curl \",\"Leg press \",
\"Leg raises \",\"Lung \",\"Lungs \",\"Overhead tricep extension \",
\"Plank \",\"Pull ups \",\"Pulls ups \",\"Reverse situps \",\"Romanians \",
\"Seated leg curl \",\"Seated leg raise \",\"Seated row \",
\"Shoulder press \",\"Shrugs \",\"Sled \",\"Squat \",\"Supported row \",\"Tricep pulldown \"]"
-}

module Cleaning 
    ( cleanWorkouts
    )where

import Types (Workout(..), Exercise(..))
import Data.Char

{-
Remove trailing white space, convert multiple spaces into singular spaces and lower case all characters
-}
tidy :: String -> String
tidy = unwords . words . map toLower

{-
Standardise the exercise names
-}
clean :: String -> String
clean "assisted dips" = "assisted dip"
clean "assisted pull ups" = "assisted pullup"
clean "assisted pullups" = "assisted pullup"
clean "bench" = "bench press"
clean "bicep curls" = "curl"
clean "curls" = "curl"
clean "dumbbell curls" = "curl"
clean "dumbbell shoulder press" = "dumbell shoulder press"
clean "flys machine" = "fly machine"
clean "incline dumbbell bench" = "incline dumbell bench"
clean "lat pulldown" = "lat pull down"
clean "lung" = "lunges"
clean "lungs" = "lunges"
clean "pulls ups" = "pull ups"
clean "shoulder press" = "dumbell shoulder press"
clean "seated leg curl" = "leg curl"
clean "seated leg raise" = "leg raises"
clean xs = xs

cleanExercise :: Exercise -> Exercise
cleanExercise e = e {name = clean $ tidy $ name e}

cleanWorkout :: Workout -> Workout
cleanWorkout w = w {exercises = map cleanExercise (exercises w)}

cleanWorkouts :: [Workout] -> [Workout]
cleanWorkouts = map cleanWorkout

