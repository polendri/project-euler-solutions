type Date = (Int, Int, Int, Int)

nextDate :: Date -> Date
nextDate (y,m,d,w)
  | d == lastDay y m = if m == 12
    then (y+1,1,1,w')
    else (y,m+1,1,w')
  | otherwise = (y,m,d+1,w')
  where
    w' = (w+1) `mod` 7
    lastDay y m
      | m == 2 = if y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)
        then 29
        else 28
      | m `elem` [4,6,9,11] = 30
      | otherwise = 31

datesFrom :: Date -> [Date]
datesFrom = iterate nextDate

firstSundays :: [Date] -> [Date]
firstSundays = filter (\(_,_,d,w) -> d == 1 && w == 0)

solve :: Int
solve = length $ takeWhile (\(y,_,_,_) -> y <= 2000) $ firstSundays $ datesFrom (1901,1,1,2)

main = putStrLn $ show solve
