main :: IO ()
main = putStrLn $ show $ solve

solve :: Integer
solve = sumEvenFib 4000000

sumEvenFib :: Integer -> Integer
sumEvenFib n = sum $ filter even $ takeWhile (< n) fib

fib :: [Integer]
fib = fibHelper 1 2
  where fibHelper m n = m : fibHelper n (m+n)

