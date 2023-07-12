{-# LANGUAGE DeriveGeneric #-}
module Types 
    ( Workout(..), Exercise(..), makeJson
    ) where

import GHC.Generics
import Data.ByteString.Lazy  
import Data.Time
import Data.Aeson

data Workout = Workout {
    date :: Maybe Day,
    workoutType :: Maybe String,
    exercises :: [Exercise]
} deriving (Show, Generic)


data Exercise = Exercise {
  name :: String,
  reps :: Int,
  sets :: Int,
  weight :: Maybe Double,
  failure :: Maybe Bool,
  improvement :: Maybe Bool
} deriving (Show, Generic)

-- The default definitions of the methods are defined in terms
-- of the methods provided by the Generic type class.
instance ToJSON Exercise where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Exercise
instance ToJSON Workout where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Workout

makeJson :: [Workout] -> IO ()
makeJson workouts = Data.ByteString.Lazy.writeFile "./data/output.json" (encode workouts)