main = putStrLn $ show solve

solve :: Int
solve = numSpiralDiagSum 1001

numSpiralDiagSum :: Int -> Int
numSpiralDiagSum n = sum $ map (sum . (uncurry every)) $ take (div (n+1) 2) $ zip (1:[2,4..]) numSpiral
  where
    f n xs = every n xs

-- Each layer of the infinite number spiral (e.g. [[1], [2,3,4,5,6,7,8,9], ..])
numSpiral :: [[Int]]
numSpiral = [1] : f 3 [2..]
  where
    f n k = let t = 4*(n-1) in (take t k) : f (n+2) (drop t k)

every n xs = case drop (n-1) xs of
                  [] -> []
                  (y:ys) -> y : every n ys

