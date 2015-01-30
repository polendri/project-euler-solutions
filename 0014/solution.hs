import Data.List (maximumBy)

main = putStrLn $ show solve

solve :: Int
solve = fst $ maximumBy (\p1 p2 -> (snd p1) `compare`  (snd p2)) $ map (\x -> (x, collatzLen x)) [1..1000000]

collatzLen :: Int -> Int
collatzLen n = f n 0
  where
    f 1 acc = acc + 1
    f n acc = f (collatz n) (acc + 1)

collatz :: Int -> Int
collatz n
    | even n    = div n 2
    | otherwise = 3*n + 1
