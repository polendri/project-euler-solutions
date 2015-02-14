import Data.Int (Int64)

-- Establishing an upper bound:
--
-- For an x-digit number, the greatest digit sum is when each digit is a 9,
-- and the smallest number with that many digits is 10^(x-1). By finding the
-- point at which the smallest number is always greater than the largest digit
-- sum, we have an upper bound for our search. (It's x > 6.59, so 7-digit
-- numbers are the biggest ones we need to search, though we could doubtless
-- find a tighter upper bound if we wanted.)
main = putStrLn $ show solve

solve :: Int64
solve = subtract 1 $ sum $ map digitsToNum $ filter (\x -> digitsToNum x == digitFifthPower x) allNums

allNums :: [[Int64]]
allNums = [[a,b,c,d,e,f,g] | a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], e <- [0..9], f <- [0..9], g <- [0..9]]

digitsToNum :: (Num a) => [a] -> a
digitsToNum [] = 0
digitsToNum (d:ds) = d + 10*(digitsToNum ds)

digitFifthPower :: [Int64] -> Int64
digitFifthPower = sum . (map (^5))

