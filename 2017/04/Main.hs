import System.Environment
import Data.List (sort)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let passphrases = map words $ lines content

  let resultPuzzle1 = length $ filter isValid passphrases
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = length $ filter isValid' passphrases
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


isValid :: [String] -> Bool
isValid = not . hasDuplicate

isValid' :: [String] -> Bool
isValid' = isValid . map sort

hasDuplicate :: [String] -> Bool
hasDuplicate []  = False
hasDuplicate [_] = False
hasDuplicate (x:xs)
  | elem x xs = True
  | otherwise = hasDuplicate xs
