module WarmupExercises where

wordStartsWithA :: String -> Bool
wordStartsWithA (x:xs) = x == 'a'
wordStartsWithA [] = False

selectElementsOfListStartingWithA :: [String] -> [String]
selectElementsOfListStartingWithA words = filter wordStartsWithA words

-- concatenate pairs of strings
everyPossiblePair :: [String] -> [String]
everyPossiblePair (x:xs) = [x ++ other | other <- xs] ++ everyPossiblePair xs
everyPossiblePair [] = []
