import Data.Char (digitToInt)

fact :: Integer -> Integer
fact 2 = 2
fact n = n * (fact (n-1))

solve :: Int
solve = sum $ map digitToInt $ show $ fact 100

main = putStrLn $ show solve
