import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (sortBy)
import Data.Ord (comparing)


type Position = (Int, Int)


puzzleInput :: Int
puzzleInput = 1364


targetPosition :: Position
targetPosition = (31,39)


main :: IO ()
main = do
  let startPosition = (1,1)
  let resultPuzzle1 = findShortestPath startPosition
  putStrLn $ "The fewest number of steps is: " ++ show resultPuzzle1


findShortestPath :: Position -> Int
findShortestPath startPosition = length $ head $ sortBy (comparing length) $ find [startPosition] []


find :: [Position] -> [Position] -> [[Position]]
find [] _ = []
find (position:positions) visited
  | position == targetPosition = [visited]
  | notElem position visited   = do
                                   nextPosition <- getNextPositions position
                                   find (positions ++ [nextPosition]) (position:visited)
  | otherwise                  = find positions visited


getNextPositions :: Position -> [Position]
getNextPositions (x,y) = nextPositions
                         where allPositions = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
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
