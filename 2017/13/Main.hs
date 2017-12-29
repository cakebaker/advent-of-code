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

  let resultPuzzle2 = findDelay 0 $ map toScanner layers
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


isCaught :: (Int, Int) -> Bool
isCaught (depth, range) = depth `mod` repetition == 0
                          where repetition = (2 * (range - 1))

calculateSeverity :: [(Int, Int)] -> Int
calculateSeverity caught = foldl (\acc (depth, range) -> acc + (depth * range)) 0 caught

toScanner :: (Int, Int) -> [Bool]
toScanner (depth, range) = drop depth $ cycle (True:(replicate notCaught False))
                           where notCaught = (2 * (range - 1)) - 1

findDelay :: Int -> [[Bool]] -> Int
findDelay counter xs
  | isNotCaught = counter
  | otherwise   = findDelay (counter + 1) (map tail xs)
  where isNotCaught = all (\x -> head x == False) xs

layer :: ReadP (Int, Int)
layer = do
  depth <- many1 $ satisfy isDigit
  string ": "
  range <- many1 $ satisfy isDigit
  return (read depth, read range)
