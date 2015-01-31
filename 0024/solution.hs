import Data.List (permutations, sort)

solve :: String
solve = (sort $ permutations "0123456789") !! 999999

main = putStrLn $ solve
