import Criterion.Main

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

-- Our benchmark harness.
main :: IO()
main = defaultMain [
  bgroup "fib" [ bench "fib(5)"  $ whnf fib 5
               , bench "fib(10)"  $ whnf fib 10
               , bench "fib(15)"  $ whnf fib 15
               , bench "fib(20)" $ whnf fib 20
               ]
  ]
