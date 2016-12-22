import System.Environment
import Data.List (sortBy, transpose)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let columns = transpose $ lines content
  let charCounts = map countChars columns

  let resultPuzzle1 = map mostFrequent charCounts
  putStrLn ("Error corrected version of the message: " ++ resultPuzzle1)


countChars :: String -> [(Char, Int)]
countChars s = [ (x,c) | x <- ['a'..'z'], let c = (length.filter (==x)) s, c > 0 ]


mostFrequent :: [(Char, Int)] -> Char
mostFrequent xs = fst . head $ sortBy sortByMostFrequent xs


sortByMostFrequent :: (Char, Int) -> (Char, Int) -> Ordering
sortByMostFrequent (a1, b1) (a2, b2)
  | b1 < b2   = GT
  | b1 > b2   = LT
  | otherwise = compare a1 a2
