import Data.List (sortBy)

main = putStrLn $ show solve

solve :: Int
solve = head $ sortBy (\x y -> compare y x) $ palindromeProducts 100 999

palindromeProducts :: Int -> Int -> [Int]
palindromeProducts l h = map read $ filter isPalindrome $ map show [x*y | x <- [l..h], y <- [x..h]]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
