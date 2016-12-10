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


parse :: String -> (RelativeDirection, Int)
parse (x:xs)
  | x == 'L' = (Left, blocks)
  | otherwise = (Right, blocks)
  where blocks = read $ takeWhile isDigit xs


getDistanceToHQ :: [(RelativeDirection, Int)] -> Int
getDistanceToHQ instructions = abs $ (fst destination) + (snd destination)
                               where destination = move (0, 0) North instructions


move :: (Int, Int) -> Direction -> [(RelativeDirection, Int)] -> (Int, Int)
move position _ [] = position
move (x, y) North (instruction:instructions)
  | relativeDirection == Left = move (x - blocks, y) West instructions
  | otherwise         = move (x + blocks, y) East instructions
  where relativeDirection = fst instruction
        blocks    = snd instruction
move (x, y) East (instruction:instructions)
  | direction == Left = move (x, y + blocks) North instructions
  | otherwise         = move (x, y - blocks) South instructions
  where direction = fst instruction
        blocks    = snd instruction
move (x, y) South (instruction:instructions)
  | relativeDirection == Left = move (x + blocks, y) East instructions
  | otherwise         = move (x - blocks, y) West instructions
  where relativeDirection = fst instruction
        blocks    = snd instruction
move (x, y) West (instruction:instructions)
  | direction == Left = move (x, y - blocks) South instructions
  | otherwise         = move (x, y + blocks) North instructions
  where direction = fst instruction
        blocks    = snd instruction
