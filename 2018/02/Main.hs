import System.Environment
import Data.List (group, sort)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let boxIDs = lines content

  let resultPuzzle1 = checksum boxIDs
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = common boxIDs
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


checksum :: [String] -> Int
checksum xs = twoLetterCount * threeLetterCount
              where twoLetterCount   = length $ filter (hasXLetters 2) xs
                    threeLetterCount = length $ filter (hasXLetters 3) xs

hasXLetters :: Int -> String -> Bool
hasXLetters l s = not . null $ filter (\xs -> length xs == l) $ group $ sort s

common :: [String] -> String
common boxIDs = head $ filter (\x -> length x == lengthOfCorrectBoxIDs) commonLetters
                where combinations          = [(x,y) | x <- boxIDs, y <- boxIDs, x < y]
                      commonLetters         = map (\(x,y) -> positionalIntersect x y) combinations
                      lengthOfCorrectBoxIDs = (length $ head boxIDs) - 1

positionalIntersect :: String -> String -> String
positionalIntersect [] [] = []
positionalIntersect (x:xs) (y:ys)
  | x == y    = x:positionalIntersect xs ys
  | otherwise = positionalIntersect xs ys
