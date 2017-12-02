import System.Environment
import Data.List (sort)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let rows = map stringsToInts $ map words $ lines content

  let resultPuzzle1 = sum $ map calcChecksum rows
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = sum $ map calcEvenlyDivisible rows
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


stringsToInts :: [String] -> [Int]
stringsToInts = map (\s -> read s :: Int)

calcChecksum :: [Int] -> Int
calcChecksum xs = maximum xs - minimum xs

calcEvenlyDivisible :: [Int] -> Int
calcEvenlyDivisible xs = fst evenlyDivisiblePair `div` snd evenlyDivisiblePair
                         where evenlyDivisiblePair = head $ filter isEvenlyDivisible [(x, y) | x <- xs, y <- xs, x /= y]

isEvenlyDivisible :: (Int, Int) -> Bool
isEvenlyDivisible (x, y) = x `mod` y == 0
