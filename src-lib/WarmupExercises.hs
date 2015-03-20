module WarmupExercises where

-- help functions
wordStartsWithA :: String -> Bool
wordStartsWithA (x:xs) = x == 'a'
wordStartsWithA [] = False

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:rest) =
    let smallerSorted = quicksort [e | e <- rest, e <= pivot]
        biggerSorted = quicksort [e | e <- rest, e > pivot]
    in smallerSorted ++ [pivot] ++ biggerSorted

sortComponentsAndList :: Ord a => [[a]] -> [[a]]
-- if there was a [] pattern, it needs to be first here, because otherwise the strings pattern matches everything
sortComponentsAndList strings = quicksort $ map quicksort strings

-- first question
selectElementsOfListStartingWithA :: [String] -> [String]
selectElementsOfListStartingWithA words = filter wordStartsWithA words

-- concatenate pairs of strings
everyPossiblePair :: [String] -> [String]
everyPossiblePair [] = []
everyPossiblePair (x:xs) = sortComponentsAndList $ [x ++ other | other <- xs] ++ everyPossiblePair xs
