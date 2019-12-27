import System.Environment
import Data.Char (digitToInt)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let numbers  = map digitToInt $ head $ lines content
  let patterns = map createPattern $ take (length numbers) [1..]

  -- XXX not a very fast solution...
  let result1 = toString $ take 8 $ head $ drop 100 $ iterate (phase patterns) numbers
  putStrLn $ "Result of puzzle 1: " ++ show result1


phase :: [[Int]] -> [Int] -> [Int]
phase [] _ = []
phase (pattern:patterns) numbers = (onesDigit:phase patterns (tail numbers))
                                   where onesDigit = (abs $ sum $ zipWith (*) numbers pattern) `mod` 10

createPattern :: Int -> [Int]
createPattern multiplier = dropWhile (== 0) $ cycle pattern
                           where basePattern = [0, 1, 0, -1]
                                 pattern     = concatMap (replicate multiplier) basePattern

toString :: [Int] -> String
toString xs = concatMap show xs
