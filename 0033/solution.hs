import Data.List (delete, intersect, nub)
import Data.Ratio (Ratio, (%), denominator)

main = putStrLn $ show solve

solve :: Int
solve = denominator $ product cancelFracs

cancelFracs :: [Ratio Int]
cancelFracs = map (uncurry (%)) $ filter (uncurry isCancelFrac) pairs

isCancelFrac :: Int -> Int -> Bool
isCancelFrac x y = any (preservesFrac x y) $ commonDigits x y
  where
    preservesFrac x y d = ry /= 0 && x % y == rx % ry
      where rx = removeDigit d x
            ry = removeDigit d y
    removeDigit d i = (read $ delete d $ show i) :: Int
    commonDigits x y = nub $ intersect (show x) (show y)

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [10..99], y <- [(x+1)..99], mod x 10 /= 0, mod y 10 /= 0]

