import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = 1 : (filter (\x -> n `mod` x == 0) [2..(div n 2)])

amicables :: Int -> [Int]
amicables n = takeA $ genA [2..n] Map.empty
  where
    genA :: [Int] -> Map Int Int -> Map Int Int
    genA [] m = m
    genA (x:xs) m = genA xs $ Map.insert x (sum $ divisors x) m
    takeA :: Map Int Int -> [Int]
    takeA m = Map.elems $ Map.filterWithKey (\k v -> k /= v && Map.lookup v m == Just k) m

solve :: Int
solve = sum $ amicables 9999

main = putStrLn $ show solve
