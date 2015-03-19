import Data.Char
import Test.QuickCheck
import WarmupExercises as W

wordStartsWithA :: String -> Bool
wordStartsWithA (x:xs) = x == 'a'

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA abc = and $ map wordStartsWithA abc

main = do
    quickCheck (checkAllStartWithA . W.selectElementsOfListStartingWithA)
