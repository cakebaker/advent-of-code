import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let layers = map (fst . last) $ map (readP_to_S layer) $ lines content

  let resultPuzzle1 = calculateSeverity $ filter isCaught layers
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


isCaught :: (Int, Int) -> Bool
isCaught (depth, range) = depth `mod` repetition == 0
                          where repetition = (2 * (range - 1))

calculateSeverity :: [(Int, Int)] -> Int
calculateSeverity caught = foldl (\acc (depth, range) -> acc + (depth * range)) 0 caught

layer :: ReadP (Int, Int)
layer = do
  depth <- many1 $ satisfy isDigit
  string ": "
  range <- many1 $ satisfy isDigit
  return (read depth, read range)
