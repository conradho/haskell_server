module WarmupExercises where

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
