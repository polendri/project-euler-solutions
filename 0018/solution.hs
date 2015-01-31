import Data.Vector (Vector)
import qualified Data.Vector as Vec

triangle :: Vector (Vector Int)
triangle = Vec.fromList [
    Vec.fromList [75],
    Vec.fromList [95, 64],
    Vec.fromList [17, 47, 82],
    Vec.fromList [18, 35, 87, 10],
    Vec.fromList [20, 04, 82, 47, 65],
    Vec.fromList [19, 01, 23, 75, 03, 34],
    Vec.fromList [88, 02, 77, 73, 07, 63, 67],
    Vec.fromList [99, 65, 04, 28, 06, 16, 70, 92],
    Vec.fromList [41, 41, 26, 56, 83, 40, 80, 70, 33],
    Vec.fromList [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    Vec.fromList [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    Vec.fromList [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    Vec.fromList [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    Vec.fromList [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    Vec.fromList [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

maxPathSum :: Vector (Vector Int) -> Int
maxPathSum t = f t 0 0
  where
    f t iy ix
      | iy == Vec.length t - 1 = t Vec.! iy Vec.! ix
      | otherwise = (t Vec.! iy Vec.! ix) + (max (f t (iy+1) ix) (f t (iy+1) (ix+1)))

solve :: Int
solve = maxPathSum triangle

main = putStrLn $ show solve
