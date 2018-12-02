import System.Environment
import Data.List (group, sort)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let boxIDs = lines content

  let resultPuzzle1 = checksum boxIDs
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


checksum :: [String] -> Int
checksum xs = twoLetterCount * threeLetterCount
              where twoLetterCount   = length $ filter (hasXLetters 2) xs
                    threeLetterCount = length $ filter (hasXLetters 3) xs

hasXLetters :: Int -> String -> Bool
hasXLetters l s = not . null $ filter (\xs -> length xs == l) $ group $ sort s
