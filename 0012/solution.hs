import Data.List (find)
import Data.Maybe (fromJust)

main = putStrLn $ show solve

solve :: Int
solve = fst $ fromJust $ find (\x -> snd x > 500) $ map (\x -> (x, numDivisors x)) triangleNums

numDivisors :: Integral a => a -> Int
numDivisors 1 = 1
numDivisors n = foldr (+) 0 $ map (\x -> if n `mod` x == 0 then 2 else 0) [1..sqrtN]
  where
    sqrtN = floor $ sqrt $ fromIntegral n

triangleNums :: [Int]
triangleNums = drop 1 $ f 1 0
  where
    f n acc = acc : (f (n+1) (acc + n))

