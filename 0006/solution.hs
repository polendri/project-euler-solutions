main = putStrLn $ show solve

solve :: Integer
solve = squareOfSum xs - sumOfSquares xs
  where xs = [1..100]

squareOfSum :: [Integer] -> Integer
squareOfSum xs = (sum xs) ^ 2

sumOfSquares :: [Integer] -> Integer
sumOfSquares = sum . (map (^ 2))
