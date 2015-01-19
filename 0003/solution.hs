import Data.List (find)
import Data.Maybe (fromMaybe)

main = putStrLn $ show $ solve

solve :: Integer
solve = largestFactor 600851475143

largestFactor :: Integer -> Integer
largestFactor n = fromMaybe n $ find (\x -> n `mod` x == 0) $ reverse $ possiblePrimeFactors n

possiblePrimeFactors :: Integer -> [Integer]
possiblePrimeFactors n = takeWhile (< (floor $ sqrt (fromIntegral n))) primes

-- Super naive prime number sieve. I didn't time it, but it took over 20 minutes
-- to solve the problem on my machine. It's got a super small memory footprint
-- though, so why implement something faster when you can whip this up in seconds
-- and leave it to run? :)
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

