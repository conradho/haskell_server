import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad (unless)
import System.Exit (exitFailure)

import WarmupExercises (wordStartsWithA, selectElementsOfListStartingWithA)


checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = and $ map wordStartsWithA x

-- deepCheck p = quickCheckWith (stdArgs{ maxSuccess = 1000 }) p

main :: IO ()
main = do
    -- can also call "deepCheck" or "verboseCheck"
    result <- quickCheckResult (checkAllStartWithA . selectElementsOfListStartingWithA)
    unless (isSuccess result) exitFailure
