import Prelude hiding (Left, Right)
import System.Environment
import Data.Char (isDigit)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ words $ head $ lines content
  let result1 = getDistanceToHQ instructions

  putStrLn $ "Distance to Easter Bunny HQ: " ++ show result1 ++ " blocks"


data RelativeDirection = Left | Right deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show)
data Instruction = Instruction { relativeDirection :: RelativeDirection, blocks :: Int } deriving (Show)


parse :: String -> Instruction
parse (x:xs)
  | x == 'L' = Instruction Left blocks
  | otherwise = Instruction Right blocks
  where blocks = read $ takeWhile isDigit xs


getDistanceToHQ :: [Instruction] -> Int
getDistanceToHQ instructions = (abs $ fst destination) + (abs $ snd destination)
                               where destination = move (0, 0) North instructions


move :: (Int, Int) -> Direction -> [Instruction] -> (Int, Int)
move position _ [] = position
move (x, y) North (instruction:instructions)
  | rDirection == Left = move (x - b, y) West instructions
  | otherwise          = move (x + b, y) East instructions
  where rDirection = relativeDirection instruction
        b          = blocks instruction
move (x, y) East (instruction:instructions)
  | rDirection == Left = move (x, y + b) North instructions
  | otherwise          = move (x, y - b) South instructions
  where rDirection = relativeDirection instruction
        b          = blocks instruction
move (x, y) South (instruction:instructions)
  | rDirection == Left = move (x + b, y) East instructions
  | otherwise          = move (x - b, y) West instructions
  where rDirection = relativeDirection instruction
        b          = blocks instruction
move (x, y) West (instruction:instructions)
  | rDirection == Left = move (x, y - b) South instructions
  | otherwise          = move (x, y + b) North instructions
  where rDirection = relativeDirection instruction
        b          = blocks instruction
