import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (sortBy)
import Data.Ord (comparing)


type Position = (Int, Int)


puzzleInput :: Int
puzzleInput = 1364


-- 50 steps + initial position
maxPositions :: Int
maxPositions = 51


main :: IO ()
main = do
  let startPosition = (1,1)
  let resultPuzzle2 = getReachablePositionsCount startPosition
  putStrLn $ show resultPuzzle2 ++ " distinct locations can be reached in at most 50 steps"


getReachablePositionsCount:: Position -> Int
getReachablePositionsCount startPosition = length $ foldr1 removeDuplicates $ find [startPosition] []


removeDuplicates :: [Position] -> [Position] -> [Position]
removeDuplicates [] acc = acc
removeDuplicates (x:xs) acc
  | notElem x acc = removeDuplicates xs (x:acc)
  | otherwise     = removeDuplicates xs acc


find :: [Position] -> [Position] -> [[Position]]
find [] visited = [visited]
find (position:positions) visited
  | length visited == maxPositions = [visited]
  | notElem position visited       = do
                                      nextPosition <- getNextPositions position
                                      find (positions ++ [nextPosition]) (position:visited)
  | otherwise                      = find positions visited


getNextPositions :: Position -> [Position]
getNextPositions (x,y) = nextPositions
                         where allPositions  = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
                               nextPositions = fst $ unzip $ filter (\(_, openSpace) -> openSpace) $
                                               zip allPositions $ map isOpenSpace allPositions


isOpenSpace :: Position -> Bool
isOpenSpace (x, y)
  | x < 0 || y < 0 = False
  | odd noOfOnes   = False
  | otherwise      = True
  where calc     = x * x + 3 * x + 2 * x * y + y + y * y + puzzleInput
        bits     = showIntAtBase 2 intToDigit calc ""
        noOfOnes = length $ filter (== '1') bits
