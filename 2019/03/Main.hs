import Prelude hiding (Left, Right)
import System.Environment
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let wires = map (fst . last) $ map (readP_to_S wire) $ lines content

  let wireA = path Set.empty centralPort $ wires !! 0
  let wireB = path Set.empty centralPort $ wires !! 1
  
  let intersections = Set.filter (/= centralPort) $ Set.intersection wireA wireB

  let result1 = Set.findMin $ Set.map manhattanDistance intersections
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let result2 = fewestStepsToIntersection (Set.toList intersections) (wires !! 0) (wires !! 1)
  putStrLn $ "Result of puzzle 2: " ++ show result2


data Direction = Up Int | Down Int | Left Int | Right Int deriving (Show)
type Position  = (Int, Int)

centralPort :: Position
centralPort = (0,0)

fewestStepsToIntersection :: [Position] -> [Direction] -> [Direction] -> Int
fewestStepsToIntersection intersections directionsA directionsB = minimum $ zipWith (+) stepsA stepsB
                                                                  where stepsA = map (\intersection -> pathLength centralPort intersection directionsA) intersections
                                                                        stepsB = map (\intersection -> pathLength centralPort intersection directionsB) intersections

manhattanDistance :: Position -> Int
manhattanDistance (x,y) = abs x + abs y

path :: Set Position -> Position -> [Direction] -> Set Position
path currentPath _ []                            = currentPath
path currentPath position (direction:directions) = path newPath nextPosition directions
                                                   where nextPosition     = last visitedPositions
                                                         newPath          = Set.union currentPath $ Set.fromList visitedPositions
                                                         visitedPositions = positions position direction

pathLength :: Position -> Position -> [Direction] -> Int
pathLength startPosition endPosition (direction:directions)
  | elem endPosition visitedPositions = length $ takeWhile (/= endPosition) visitedPositions
  | otherwise                         = (length visitedPositions - 1) + pathLength nextPosition endPosition directions
  where visitedPositions = positions startPosition direction
        nextPosition     = last visitedPositions

positions :: Position -> Direction -> [Position]
positions (x,y) (Right steps) = [(a,y)      | a <- [x..x+steps]]
positions (x,y) (Left steps)  = [(x - a, y) | a <- [0..steps]]
positions (x,y) (Up steps)    = [(x,b)      | b <- [y..y+steps]]
positions (x,y) (Down steps)  = [(x, y - b) | b <- [0..steps]]

-- parser

wire :: ReadP [Direction]
wire = do
  directions <- sepBy direction (string ",")
  return directions

direction :: ReadP Direction
direction = do
  dir      <- choice [char 'U', char 'D', char 'L', char 'R']
  distance <- many1 $ satisfy isDigit
  return $ toDirection dir $ toInt distance

toDirection :: Char -> Int -> Direction
toDirection 'U' = Up
toDirection 'D' = Down
toDirection 'L' = Left
toDirection 'R' = Right

toInt :: String -> Int
toInt s = read s
