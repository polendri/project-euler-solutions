import Data.Char (chr)
import Data.Map (Map)
import qualified Data.Map as Map

main = putStrLn $ show solve

-- Eventually the digit factorials are eclipsed by the number's value. With
-- 9! being the biggest factorial, this occurs when (9!)*x < 10^x - 1 (where
-- x is the number of digits), or when x is greater than about 6.36. As such,
-- 7-digit numbers and greater are ruled out, so we have a loose upper bound
-- of 999999.
solve :: Int
solve = sum $ filter isCurious [3..999999]

isCurious :: Int -> Bool
isCurious n = n == digitFacts n
  where
    digitFacts n = sum $ map digitFactorial $ show n

digitFactorial :: Char -> Int
digitFactorial c = factMap Map.! c
  where
    factMap :: Map Char Int
    factMap = foldr (\x y -> Map.insert (chr (x+48)) (factorial x) y) Map.empty [0..9]
    factorial n
      | n == 0 || n == 1 = 1
      | otherwise        = n * factorial (n-1)
