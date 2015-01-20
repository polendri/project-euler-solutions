main = putStrLn $ show $ solve

solve :: Integer
solve = primes !! 10000

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

