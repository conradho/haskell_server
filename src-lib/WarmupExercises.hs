
module WarmupExercises where

wordStartsWithA :: String -> Bool
wordStartsWithA (x:xs) = x == 'a'
wordStartsWithA [] = True

selectElementsOfListStartingWithA :: [String] -> [String]
selectElementsOfListStartingWithA words = filter wordStartsWithA words
