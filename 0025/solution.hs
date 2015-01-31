import Data.List (find)
import Data.Maybe (fromJust)

fibs :: [Integer]
fibs = fib 1 1
  where
    fib m n = m : (fib n (m+n))

solve :: Integer
solve = fst $ fromJust $ find (\(i,n) -> (length $ show n) >= 1000) $ zip [1..] fibs

main = putStrLn $ show solve
