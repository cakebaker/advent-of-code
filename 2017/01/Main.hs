import System.Environment
import Data.Char (digitToInt)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let captcha = map digitToInt $ head $ lines content
  let resultPuzzle1 = sumMatchingDigits (captcha ++ [head captcha]) 0

  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


sumMatchingDigits :: [Int] -> Int -> Int
sumMatchingDigits [] total  = total
sumMatchingDigits [_] total = total
sumMatchingDigits (x:y:xs) total
  | x == y    = sumMatchingDigits (y:xs) (total + x)
  | otherwise = sumMatchingDigits (y:xs) total
