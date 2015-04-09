import Data.List (nubBy, permutations)

main = putStrLn $ show solve

solve :: Int
solve = sum $
  map thrd $
  nubBy (\t1 t2 -> thrd t1 == thrd t2) $
  filter (uncurry3 isProduct) $
  map (\(i,j,k) -> (read i, read j, read k)) $
  concat $
  map split $
  permutations "123456789"

-- Given three numbers representing a multiplicand, a multiplier and
-- a third number, returns true if the third number represents the product of
-- the first two.
isProduct :: Int -> Int -> Int -> Bool
isProduct i j k = i * j == k

-- Given a list of length at least 3, returns all ways it can be split into 3
-- non-empty lists.
split :: [a] -> [([a], [a], [a])]
split xs = split3 $ split2 xs

-- Given a list, returns all ways it can be split into 2 non-empty lists.
split2 :: [a] -> [([a], [a])]
split2 xs = map (\n -> splitAt n xs) [1..(length xs - 1)]

-- Given a list of list pairs, splits the second list of each pair using
-- `split2`, returning a list of triples.
split3 :: [([a], [a])] -> [([a], [a], [a])]
split3 [] = []
split3 ((xs, ys):ts)
  | length ys < 2 = []
  | otherwise     = (map (\(ys', zs) -> (xs, ys', zs)) $ split2 ys) ++ split3 ts

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

