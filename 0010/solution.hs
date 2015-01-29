import Data.List.Ordered (minus, union)

main = putStrLn $ show $ solve

solve :: Integer
solve = sum $ map fromIntegral $ primes 2000000

primes :: Int -> [Int]
primes m = sieve [2..m] where
    sieve (p:xs)
        | p*p > m   = p : xs
        | otherwise = p : sieve (xs `minus` [p*p, p*p+p..m])

