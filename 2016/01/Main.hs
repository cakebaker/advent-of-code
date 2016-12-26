import Prelude hiding (Left, Right)
import System.Environment
import Data.Char (isDigit)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ words $ head $ lines content
  let resultPuzzle1 = getDistanceToHQ instructions

  putStrLn $ "Distance to Easter Bunny HQ: " ++ show resultPuzzle1 ++ " blocks"


data RelativeDirection = Left | Right deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show)
type Instruction = (RelativeDirection, Int)
type Position = (Int, Int, Direction)


parse :: String -> Instruction
parse (x:xs)
  | x == 'L' = (Left, blocks)
  | x == 'R' = (Right, blocks)
  where blocks = read $ takeWhile isDigit xs


getDistanceToHQ :: [Instruction] -> Int
getDistanceToHQ instructions = abs x + abs y
                               where (x, y, _) = foldr move (0, 0, North) (reverse instructions)


move :: Instruction -> Position -> Position
move instruction@(relativeDir, blocks) position@(x, y, dir) = case dir of
                                                                North -> moveHorizontal instruction position
                                                                South -> moveHorizontal (toggleRelativeDirection relativeDir, blocks) position
                                                                West  -> moveVertical instruction position
                                                                East  -> moveVertical (toggleRelativeDirection relativeDir, blocks) position


moveHorizontal :: Instruction -> Position -> Position
moveHorizontal (relativeDir, blocks) (x, y, _) = case relativeDir of
                                                     Left  -> (x - blocks, y, West)
                                                     Right -> (x + blocks, y, East)


moveVertical :: Instruction -> Position -> Position
moveVertical (relativeDir, blocks) (x, y, _) = case relativeDir of
                                                     Left  -> (x, y - blocks, South)
                                                     Right -> (x, y + blocks, North)


toggleRelativeDirection :: RelativeDirection -> RelativeDirection
toggleRelativeDirection Left  = Right
toggleRelativeDirection Right = Left
