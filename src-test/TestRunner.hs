import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import WarmupExercises as W
import Control.Monad (unless)
import System.Exit (exitFailure)

selectElementsOfListStartingWithA :: [String] -> [String]
selectElementsOfListStartingWithA s = s

wordStartsWithA :: String -> Bool
wordStartsWithA (x:xs) = x == 'a'
wordStartsWithA [] = True

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = and $ map wordStartsWithA x

-- deepCheck p = quickCheckWith (stdArgs{ maxSuccess = 1000 }) p

main :: IO ()
main = do
    -- can also call "deepCheck" or "verboseCheck"
    result <- quickCheckResult (checkAllStartWithA . W.selectElementsOfListStartingWithA)
    unless (isSuccess result) exitFailure
    -- if we don't exit with error, the cabal thinks the tests passed. possible to do `cabal test --show-details=always` or `=failures` to print the results, but still would still think it passed
