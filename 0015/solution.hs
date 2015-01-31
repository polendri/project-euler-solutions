import qualified Data.Map as Map

-- In hindsight, this problem is equivalent to counting the permutations of a
-- list containing 20 'down's and 20 'right's, which can be computed with a
-- single equation.

latticePaths :: Int -> Int
latticePaths n = fst $ f Map.empty n n
  where
    f m 0 _ = (1,m)
    f m _ 0 = (1,m)
    f m j k = let key = (min j k, max j k) in
      case Map.lookup (min j k, max j k) m of
        Just i  -> (i,m)
        Nothing ->
          let (c1,m1) = f m (j-1) k
              (c2,m2) = f m1 j (k-1)
              ret = c1+c2 in
          (ret, Map.insert (min j k, max j k) ret m2)

solve :: Int
solve = latticePaths 20

main = putStrLn $ show solve
