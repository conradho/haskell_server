import Data.Char
import Test.QuickCheck

main = do
    quickCheck ((\s -> s == s) :: [Char] -> Bool)
