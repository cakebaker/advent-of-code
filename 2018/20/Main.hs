import System.Environment
import Data.List (isInfixOf, sortOn)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let regexp = head $ lines content

  let resultPuzzle1 = parse regexp
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


parse :: String -> Int
parse s
  | isInfixOf "(" s = parse $ replace s $ findBranch s
  | otherwise       = length $ filter isValidChar s

findBranch :: String -> (Int, Int)
findBranch s = findBranch' s undefinedPos undefinedPos
               where undefinedPos = (-1)

findBranch' :: String -> Int -> Int -> (Int, Int)
findBranch' (x:xs) pos openBracketPos
  | x == '('  = findBranch' xs currentPos currentPos
  | x == ')'  = (openBracketPos, currentPos)
  | otherwise = findBranch' xs currentPos openBracketPos
  where currentPos = pos + 1

replace :: String -> (Int, Int) -> String
replace s (openBracketPos, closeBracketPos) = concat [start, handleBranch branch, end]
                                              where (withBranch, end) = splitAt (closeBracketPos + 1) s
                                                    (start, branch)   = splitAt openBracketPos withBranch

handleBranch :: String -> String
handleBranch (_:xs)
  | last s == '|' = ""
  | otherwise     = last $ sortOn length $ wordsWhen (== '|') s
  where s = init xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                     "" -> []
                     s' -> w : wordsWhen p s''
                           where (w, s'') = break p s'

isValidChar :: Char -> Bool
isValidChar 'N' = True
isValidChar 'S' = True
isValidChar 'E' = True
isValidChar 'W' = True
isValidChar _   = False
