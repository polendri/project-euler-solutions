-- There's no need to build the full strings before taking the length; each
-- helper function could just return a length. I'm not too concerned with
-- performance here though, and this way is much easier to debug.

onesStr :: Int -> String
onesStr n = case n of
  0 -> ""
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"

tensStr :: Int -> String
tensStr n = case n of
  0 -> ""
  1 -> "ten"
  2 -> "twenty"
  3 -> "thirty"
  4 -> "forty"
  5 -> "fifty"
  6 -> "sixty"
  7 -> "seventy"
  8 -> "eighty"
  9 -> "ninety"

hundredsStr :: Int -> String
hundredsStr 0 = ""
hundredsStr n = onesStr n ++ "hundred"

numStr :: (Int, Int, Int) -> String
numStr (h,t,o)
  | null hstr = lastTwo t o
  | null tstr && null ostr = hstr
  | otherwise = hstr ++ "and" ++ (lastTwo t o)
  where
    hstr = hundredsStr h
    tstr = tensStr t
    ostr = onesStr o
    lastTwo 0 n = onesStr n
    lastTwo 1 0 = "ten"
    lastTwo 1 1 = "eleven"
    lastTwo 1 2 = "twelve"
    lastTwo 1 3 = "thirteen"
    lastTwo 1 4 = "fourteen"
    lastTwo 1 5 = "fifteen"
    lastTwo 1 6 = "sixteen"
    lastTwo 1 7 = "seventeen"
    lastTwo 1 8 = "eighteen"
    lastTwo 1 9 = "nineteen"
    lastTwo n m = tstr ++ ostr

numStrsTo1k :: [String]
numStrsTo1k = (drop 1 $ map numStr [(x,y,z) | x <- [0..9], y <- [0..9], z <- [0..9]]) ++ ["onethousand"]

solve :: Int
solve = sum $ map length numStrsTo1k

main = putStrLn $ show solve
