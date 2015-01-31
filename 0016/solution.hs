import Data.Char (digitToInt)

solve :: Int
solve = sum $ map digitToInt $ show $ (2 :: Integer)^1000

main = putStrLn $ show solve
