-- very, very slow ;-)

import System.Environment
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let infectedPositions = extractInfectedPositions $ lines content

  let resultPuzzle2 = solve infectedPositions (0,0) North
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


data State     = Weakened | Infected | Flagged deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show)
type Position  = (Int, Int)

solve :: Map Position State -> Position -> Direction -> Int
solve infectedPositions currentPosition direction = infectionCount
                                                    where (_, _, _, infectionCount) = head $ drop 10000000 $ iterate burst (infectedPositions, currentPosition, direction, 0)

burst :: (Map Position State, Position, Direction, Int) -> (Map Position State, Position, Direction, Int)
burst (infectedPositions, currentPosition, direction, infectionCount)
  | isClean           = (M.insert currentPosition Weakened infectedPositions, moveForward currentPosition (turnLeft direction), turnLeft direction, infectionCount)
  | state == Weakened = (M.insert currentPosition Infected infectedPositions, moveForward currentPosition direction, direction, infectionCount + 1)
  | state == Infected = (M.insert currentPosition Flagged infectedPositions, moveForward currentPosition (turnRight direction), turnRight direction, infectionCount)
  | otherwise         = (M.delete currentPosition infectedPositions, moveForward currentPosition (reverseDirection direction), reverseDirection direction, infectionCount)
  where isClean = M.notMember currentPosition infectedPositions
        state   = infectedPositions M.! currentPosition

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

reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection East  = West
reverseDirection South = North
reverseDirection West  = East

extractInfectedPositions :: [String] -> Map Position State
extractInfectedPositions xs = M.fromList infectedPos
                              where withYPos     = zip xs [0..]
                                    withXPos     = foldl1 (++) $ map (\(s,y) -> zip3 s [0..] (repeat y)) withYPos
                                    onlyInfected = map (\(_,x,y) -> (x,y)) $ filter (\(s,_,_) -> s == '#') withXPos
                                    offset       = (length xs) `div` 2
                                    infectedPos  = map (\(x,y) -> ((x - offset, (y * (-1)) + offset), Infected)) onlyInfected
