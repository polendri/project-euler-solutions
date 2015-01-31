import Data.Int (Int64)
import Data.List (sort)

charScore :: Char -> Int64
charScore c = (fromIntegral $ fromEnum c) - 64

nameScore :: Int64 -> String -> Int64
nameScore i n = i * (sum $ map charScore n)

nameScores :: [String] -> [Int64]
nameScores ns = map (uncurry nameScore) $ zip [1..] ns

readNames :: FilePath -> IO [String]
readNames p = do
  str <- readFile p
  return $ read $ "[" ++ str ++ "]"

solve :: IO Int64
solve = do
  ns <- readNames "names.txt"
  return $ sum $ nameScores $ sort ns

main = (putStrLn . show) =<< solve
