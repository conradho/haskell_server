module ParallelExercise where

import Control.Parallel.Strategies


fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


tripleFib :: Int -> Int
tripleFib n = runEval $ do
    a <- rpar (fibonacci n)
    b <- rpar (fibonacci $ n + 1)
    c <- rpar (fibonacci $ n + 2)
    rseq a
    rseq b
    rseq c
    return (a + b + c)
