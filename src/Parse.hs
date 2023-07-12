module Parse
    ( workoutFile
    ) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad.Identity
import Debug.Trace
import Data.Maybe
import Data.Time
import Types (Workout(..), Exercise(..))

workoutFile :: ParsecT String u Identity [Workout]
workoutFile = option [] (sepEndBy workout (many1 endOfLine)) <* eof
workout = do
  --trace ("reading workout") $ return ()
  skipMany (satisfy (/= '\n')) -- Skip over any characters that are not end-of-line characters
  endOfLine
  date <- optionMaybe $ try $ manyTill anyChar endOfLine
  --trace ("date: " ++ show date) $ return ()
  workoutType <- optionMaybe (try (string "Push") <|> try (string "Pull") <|> string "Legs")
  skipMany (satisfy (/= '\n')) -- Skip over any characters that are not end-of-line characters
  endOfLine
  --trace ("workoutType: " ++ show workoutType) $ return ()
  exercises <- option [] $ manyTill exercise (lookAhead (try endOfLine <|> (try eof >> return ' ')))
  ---- trace exercises
  ----trace ("exercises: " ++ show exercises) $ return ()
  return $ Workout (parseTimeM True defaultTimeLocale "%d/%m/%y" (fromMaybe "" date) :: Maybe Day) workoutType exercises

exercise = do
    name <- manyTill anyChar (lookAhead digit)
    reps <- read <$> many1 digit
    _ <- char ' '
    sets <- read <$> many1 digit
    _ <- char ' '
    weight <- char '-' *> return Nothing <|> (Just . read <$> many1 (digit <|> char '.'))
    _ <- char ' '
    failure <- optionMaybe (char 'y' *> return True <|> char 'n' *> return False <|> char '-' *> return False)
    _ <- char ' '
    improvement <- optionMaybe (char 'y' *> return True <|> char 'n' *> return False <|> char '-' *> return False)
    skipMany (char ' ')
    _ <- endOfLine
    return $ Exercise name reps sets weight failure improvement


{-
Keep this here incase we ever want to give up doing our data science in haskell lmao
This function will write the parsed data to a json file
-}
--main :: IO ()
--main = do
    --contents <- Prelude.readFile "data.txt"
    --case parse workoutFile "" contents of
        --Left err -> putStrLn $ "Parsing error: " ++ show err
        --Right workouts -> do
            --putStrLn "Parsed workouts:"
            --Data.ByteString.Lazy.writeFile "output.json" (encode workouts)



