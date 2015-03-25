module WarmupExercises where

-- note: add time to cabal dependency
-- don't use defaultTimeLocale from System.Locale (the old-locale package)
import Data.Time (FormatTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Data.Map as Map
import qualified Data.Tuple (swap)

-- helper functions
wordStartsWithA :: String -> Bool
wordStartsWithA (x:xs) = x == 'a'
wordStartsWithA [] = False


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:rest) =
    let smallerSorted = quicksort [e | e <- rest, e <= pivot]
        biggerSorted = quicksort [e | e <- rest, e > pivot]
    in smallerSorted ++ [pivot] ++ biggerSorted


everyPossiblePair :: [String] -> [String]
everyPossiblePair [] = []
everyPossiblePair (x:xs) = [x ++ other | other <- xs] ++ everyPossiblePair xs


-- first question
selectElementsOfListStartingWithA :: [String] -> [String]
selectElementsOfListStartingWithA words = filter wordStartsWithA words


-- note if say there was a function declaration for [], it needs to go right after the type declaration
-- because otherwise "x" matches everything
everyPossiblePairSorted :: [String] -> [String]
everyPossiblePairSorted x = everyPossiblePair $ quicksort x


getFirstHalf :: [a] -> [a]
getFirstHalf [] = []
getFirstHalf xs = take (n `div` 2) xs
    where n = length xs


isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True  -- same as matching (x:[])
isPalindrome (xStart:xs)
    | xStart == xEnd = isPalindrome xMiddle
    | otherwise = False
    where xEnd = last xs
          (xMiddle, _) = splitAt (length xs - 1) xs


formatDate :: FormatTime a => a -> String
-- %0Y zeropads to 4 digits (vs _Y blank space pads to 4 digits)
formatDate t = formatTime defaultTimeLocale "%0Y-%m-%d" t

flipKeyVal :: (Ord k, Ord v) => Map.Map k v -> Map.Map v k
flipKeyVal dictionary = Map.fromList $ map Data.Tuple.swap (Map.toList dictionary)

