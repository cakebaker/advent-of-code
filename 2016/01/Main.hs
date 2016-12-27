import Prelude hiding (Left, Right)
import System.Environment
import Data.Char (isDigit)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ words $ head $ lines content
  let startPosition = ((0, 0), North)

  let resultPuzzle1 = getDistanceToHQ instructions startPosition
  putStrLn $ "Distance to Easter Bunny HQ: " ++ show resultPuzzle1 ++ " blocks"

  let resultPuzzle2 = getDistanceToRealHQ instructions startPosition
  putStrLn $ "Distance to first location visited twice: " ++ show resultPuzzle2 ++ " blocks"


data RelativeDirection = Left | Right deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show)
type Instruction = (RelativeDirection, Int)
type Position = (Point, Direction)
type Point = (Int, Int)


parse :: String -> Instruction
parse (x:xs)
  | x == 'L' = (Left, blocks)
  | x == 'R' = (Right, blocks)
  where blocks = read $ takeWhile isDigit xs


getDistanceToHQ :: [Instruction] -> Position -> Int
getDistanceToHQ instructions startPosition@(startPoint, _) = calculateDistance startPoint endPoint
                                                             where (endPoint, _) = foldr move startPosition (reverse instructions)


getDistanceToRealHQ :: [Instruction] -> Position -> Int
getDistanceToRealHQ instructions startPosition@(startPoint, _) = calculateDistance startPoint endPoint
                                                                 where endPoint = execute instructions startPosition []


calculateDistance :: Point -> Point -> Int
calculateDistance (startX, startY) (endX, endY) = abs (startX - endX) + abs (startY - endY)


execute :: [Instruction] -> Position -> [Point] -> Point
execute (instruction:instructions) position@(currentPoint, _) visited = case getFirstDuplicate points visited of
                                                                          Just x  -> x
                                                                          Nothing -> execute instructions newPosition (points ++ visited)
                                                                        where newPosition@(newPoint, _) = move instruction position
                                                                              points                    = getPointsTo currentPoint newPoint


getPointsTo :: Point -> Point -> [Point]
getPointsTo startPoint@(startX, startY) endPoint@(endX, endY)
  | startX == endX && startY == endY = []
  | startX == endX                   = startPoint : getPointsTo (startX, next startY) endPoint
  | startY == endY                   = startPoint : getPointsTo (next startX, startY) endPoint
  where diffX = endX - startX
        diffY = endY - startY
        next  = if diffX < 0 || diffY < 0 then pred else succ


getFirstDuplicate :: [Point] -> [Point] -> Maybe Point
getFirstDuplicate [] _ = Nothing
getFirstDuplicate (x:xs) ys
  | elem x ys = Just x
  | otherwise = getFirstDuplicate xs (x:ys)


move :: Instruction -> Position -> Position
move instruction@(relativeDir, blocks) position@(_, dir) = case dir of
                                                             North -> moveHorizontal instruction position
                                                             South -> moveHorizontal (toggleRelativeDirection relativeDir, blocks) position
                                                             West  -> moveVertical instruction position
                                                             East  -> moveVertical (toggleRelativeDirection relativeDir, blocks) position


moveHorizontal :: Instruction -> Position -> Position
moveHorizontal (relativeDir, blocks) ((x, y), _) = case relativeDir of
                                                     Left  -> ((x - blocks, y), West)
                                                     Right -> ((x + blocks, y), East)


moveVertical :: Instruction -> Position -> Position
moveVertical (relativeDir, blocks) ((x, y), _) = case relativeDir of
                                                   Left  -> ((x, y - blocks), South)
                                                   Right -> ((x, y + blocks), North)


toggleRelativeDirection :: RelativeDirection -> RelativeDirection
toggleRelativeDirection Left  = Right
toggleRelativeDirection Right = Left
