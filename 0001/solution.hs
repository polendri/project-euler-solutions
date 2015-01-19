main :: IO ()
main = putStrLn $ show $ solve

solve :: Int
solve = sum [sumMultiplesToN 3 1000, sumMultiplesToN 5 1000, -sumMultiplesToN 15 1000]

sumMultiplesToN :: (Integral a) => a -> a -> a
sumMultiplesToN m n = m * (sumToN $ (n-1) `div` m)

sumToN :: (Integral a) => a -> a
sumToN n = n * (n+1) `div` 2
