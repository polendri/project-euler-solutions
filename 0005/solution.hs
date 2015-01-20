main = putStrLn $ show solve

-- This is just the lowest common multiple. Note that this is also very easy to
-- work out by hand for numbers up to 20 (the answer is 2^4*3^2*5*7*11*13*17*19).
solve :: Int
solve = foldr lcm 2 [3..20]
