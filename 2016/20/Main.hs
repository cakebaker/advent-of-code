import System.Environment
import Data.Char (isDigit)
import Data.List (sortBy)


maxIP :: Int
maxIP = 4294967295

minIP :: Int
minIP = 0


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let rules = sortBy sortByLowerBound $ map parse $ lines content

  let resultPuzzle1 = findLowestNonBlockedIP rules minIP
  putStrLn $ "Lowest non-blocked IP is: " ++ show resultPuzzle1

  let resultPuzzle2 = getAllowedIPCount rules
  putStrLn $ "The blacklist allows " ++ show resultPuzzle2 ++ " IPs"


findLowestNonBlockedIP :: [(Int, Int)] -> Int -> Int
findLowestNonBlockedIP ((lowerBound, upperBound):rules) ip 
  | ip < lowerBound  = ip
  | ip > upperBound  = findLowestNonBlockedIP rules ip
  | ip >= lowerBound = findLowestNonBlockedIP rules (succ upperBound)


getAllowedIPCount :: [(Int, Int)] -> Int
getAllowedIPCount rules = countAllowedIPs 0 $ normalizeRules rules


countAllowedIPs :: Int -> [(Int, Int)] -> Int
countAllowedIPs counter ((_, upper):[])                   = counter + (maxIP - upper)
countAllowedIPs counter ((_, upperA):(lowerB, upperB):xs) = countAllowedIPs (counter + (lowerB - upperA - 1)) ((lowerB, upperB):xs)


normalizeRules :: [(Int, Int)] -> [(Int, Int)]
normalizeRules [] = []
normalizeRules (x:[]) = (x:[])
normalizeRules ((lowerA, upperA):(lowerB, upperB):xs)
  | upperA > lowerB && upperA < upperB = normalizeRules ((lowerA, upperB):xs)
  | upperA > lowerB && upperA > upperB = normalizeRules ((lowerA, upperA):xs)
  | upperA < lowerB                    = (lowerA, upperA) : normalizeRules ((lowerB, upperB):xs)


parse :: String -> (Int, Int)
parse s = (lowerBound, upperBound)
          where lowerBound = read $ takeWhile isDigit s
                upperBound = read $ reverse $ takeWhile isDigit $ reverse s


sortByLowerBound :: (Int, Int) -> (Int, Int) -> Ordering
sortByLowerBound (a1, b1) (a2, b2)
  | a1 < a2   = LT
  | a1 > a2   = GT
  | otherwise = compare b1 b2
