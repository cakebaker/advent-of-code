import System.Environment
import Data.Char (isDigit)
import Data.List (sortBy)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let rules = sortBy sortByLowerBound $ map parse $ lines content
  let resultPuzzle1 = findLowestNonBlockedIP rules 0

  putStrLn $ "Lowest non-blocked IP is: " ++ show resultPuzzle1


findLowestNonBlockedIP :: [(Int, Int)] -> Int -> Int
findLowestNonBlockedIP ((lowerBound, upperBound):rules) ip 
  | ip < lowerBound  = ip
  | ip > upperBound  = findLowestNonBlockedIP rules ip
  | ip >= lowerBound = findLowestNonBlockedIP rules (succ upperBound)


parse :: String -> (Int, Int)
parse s = (lowerBound, upperBound)
          where lowerBound = read $ takeWhile isDigit s
                upperBound = read $ reverse $ takeWhile isDigit $ reverse s


sortByLowerBound :: (Int, Int) -> (Int, Int) -> Ordering
sortByLowerBound (a1, b1) (a2, b2)
  | a1 < a2   = LT
  | a1 > a2   = GT
  | otherwise = compare b1 b2
