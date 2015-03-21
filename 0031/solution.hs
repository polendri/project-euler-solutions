import qualified Data.Map as M

main = putStrLn $ show solution

solution :: Int
solution = coinWays 200

coinWays :: Int -> Int
coinWays n = coinWays' n [1,2,5,10,20,50,100,200]
  where
    coinWays' _ [] = 0
    coinWays' 0 _ = 1
    coinWays' n (m:ms)
      | n < m = 0
      | otherwise = (coinWays' (n-m) (m:ms)) + (coinWays' n ms)

