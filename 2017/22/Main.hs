import System.Environment
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let infectedPositions = extractInfectedPositions $ lines content

  let resultPuzzle1 = solve infectedPositions (0,0) North
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


data Direction = North | East | South | West deriving (Show)
type Position  = (Int, Int)

solve :: Set Position -> Position -> Direction -> Int
solve infectedPositions currentPosition direction = infectionCount
                                                    where (_, _, _, infectionCount) = head $ drop 10000 $ iterate burst (infectedPositions, currentPosition, direction, 0)

burst :: (Set Position, Position, Direction, Int) -> (Set Position, Position, Direction, Int)
burst (infectedPositions, currentPosition, direction, infectionCount)
  | isInfected = (S.delete currentPosition infectedPositions, moveForward currentPosition (turnRight direction), turnRight direction, infectionCount)
  | otherwise  = (S.insert currentPosition infectedPositions, moveForward currentPosition (turnLeft direction), turnLeft direction, infectionCount + 1)
  where isInfected = S.member currentPosition infectedPositions

moveForward :: Position -> Direction -> Position
moveForward (x,y) North = (x, y + 1)
moveForward (x,y) East  = (x + 1, y)
moveForward (x,y) South = (x, y - 1)
moveForward (x,y) West  = (x - 1, y)

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

extractInfectedPositions :: [String] -> Set Position
extractInfectedPositions xs = S.fromList infectedPos
                              where withYPos     = zip xs [0..]
                                    withXPos     = foldl1 (++) $ map (\(s,y) -> zip3 s [0..] (repeat y)) withYPos
                                    onlyInfected = map (\(_,x,y) -> (x,y)) $ filter (\(s,_,_) -> s == '#') withXPos
                                    offset       = (length xs) `div` 2
                                    infectedPos  = map (\(x,y) -> (x - offset, (y * (-1)) + offset)) onlyInfected
