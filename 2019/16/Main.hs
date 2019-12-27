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
phase (pattern:patterns) numbers = (onesDigit:phase patterns numbers)
                                   where sum       = foldl1 (+) $ zipWith (*) numbers pattern
                                         onesDigit = mod (abs sum) 10

createPattern :: Int -> [Int]
createPattern multiplier = tail $ cycle $ first ++ second ++ third ++ fourth
                           where first  = replicate multiplier 0
                                 second = replicate multiplier 1
                                 third  = replicate multiplier 0
                                 fourth = replicate multiplier (-1)

toString :: [Int] -> String
toString xs = foldl1 (++) $ map show xs
