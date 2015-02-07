import Data.List (maximumBy)

-- Computes the multiplicative order of a number. It just so happens that the
-- multiplicative order of n and 10 is equal to the length of the decimal period of
-- m/n :)
multOrder :: Int -> Int -> Int
multOrder n a = f n a 1 1
  where
    f n a a' c = if a'' == 1 then c else f n a a'' (c+1)
      where a'' = a*a' `mod` n

solve :: Int
solve = fst $ maximumBy (\(_,x) (_,y) -> compare x y) $ map (\(n,x) -> (n,multOrder x 10)) $ filter (\(_,x) -> gcd x 10 == 1) $ zip [2..999] [2..999]

main = putStrLn $ show solve

