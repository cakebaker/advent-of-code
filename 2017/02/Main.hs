import System.Environment
import Data.List (sort)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let rows = map stringsToInts $ map words $ lines content

  let resultPuzzle1 = sum $ map calcChecksum rows
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


calcChecksum :: [Int] -> Int
calcChecksum xs = maximum xs - minimum xs

stringsToInts :: [String] -> [Int]
stringsToInts = map (\s -> read s :: Int)
