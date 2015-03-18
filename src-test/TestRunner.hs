import Data.Char
import Test.QuickCheck
import WarmupExercises as W

main = do
    quickCheck ((\s -> s == s) :: [Char] -> Bool)
