import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad (unless)
import System.Exit (exitFailure)

-- import WarmupExercises (wordStartsWithA, selectElementsOfListStartingWithA)


module WarmupExercisesSpec where

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = True

-- deepCheck p = quickCheckWith (stdArgs{ maxSuccess = 1000 }) p

-- main :: IO ()
-- main = do
    -- can also call "deepCheck" or "verboseCheck"
    -- result <- quickCheckResult (checkAllStartWithA . selectElementsOfListStartingWithA)
    -- unless (isSuccess result) exitFailure
