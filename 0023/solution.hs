import Data.Maybe (isNothing)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = 1 : (filter (\x -> n `mod` x == 0) [2..(div n 2)])

-- Given a target value and a sorted vector, determines if a pair exists in the
-- vector such that they add to the target value, and if so returns that pair.
pairSum :: (Num a, Ord a) => a -> Vector a -> Maybe (a, a)
pairSum n v = f n v 0 $ (V.length v) - 1
  where
    f n v i1 i2
      | i1 > i2 = Nothing
      | s < n    = f n v (i1+1) i2
      | s > n    = f n v i1 (i2-1)
      | otherwise = Just (v ! i1, v ! i2)
      where
        s = (v ! i1) + (v ! i2)

abundants :: Int -> [Int]
abundants n = f n 12
  where
    f n j
      | j > n = []
      | sum (divisors j) > j = j : (f n (j+1))
      | otherwise = f n (j+1)

solve :: Int
solve = sum $ filter (\x -> isNothing $ pairSum x abundantsV) [1..28183]
  where abundantsV = V.fromList $ abundants $ 28183

main = putStrLn $ show solve

