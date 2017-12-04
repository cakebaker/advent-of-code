import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let passphrases = map words $ lines content

  let resultPuzzle1 = length $ filter isValid passphrases
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


isValid :: [String] -> Bool
isValid = not . hasDuplicate

hasDuplicate :: [String] -> Bool
hasDuplicate []  = False
hasDuplicate [_] = False
hasDuplicate (x:xs)
  | elem x xs = True
  | otherwise = hasDuplicate xs
