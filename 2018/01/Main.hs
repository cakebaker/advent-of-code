import System.Environment
import qualified Data.IntSet as IntSet

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let frequencyChanges = map parse $ lines content

  let resultPuzzle1 = sum $ frequencyChanges
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


  let frequencies = scanl (+) 0 $ cycle $ frequencyChanges
  let resultPuzzle2 = firstDuplicate IntSet.empty frequencies
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


parse :: String -> Int
parse ('+':s) = read s
parse ('-':s) = 0 - read s

firstDuplicate :: IntSet.IntSet -> [Int] -> Int
firstDuplicate processed (x:xs)
  | IntSet.member x processed = x
  | otherwise                 = firstDuplicate (IntSet.insert x processed) xs
