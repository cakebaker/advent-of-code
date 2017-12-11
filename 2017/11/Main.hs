import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char (isLower)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let path = fst . last $ readP_to_S steps $ head $ lines content

  let resultPuzzle1 = solve path
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = solve2 path
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


data Step = N | NE | SE | S | SW | NW deriving (Show)

type Position = (Int, Int, Int)


solve :: [Step] -> Int
solve steps = distance start end
              where start = (0,0,0)
                    end   = foldl (\currentPos step -> next currentPos step) start steps

solve2 :: [Step] -> Int
solve2 steps = maximum distances
               where start     = (0,0,0)
                     positions = foldl addNext [start] steps
                     distances = map (distance start) positions

addNext :: [Position] -> Step -> [Position]
addNext positions@(currentPos:_) step = ((next currentPos step):positions)

-- using cubic coordinates, see https://www.redblobgames.com/grids/hexagons/
next :: Position -> Step -> Position
next (x,y,z) step = case step of N  -> (x, y + 1, z - 1)
                                 NE -> (x + 1, y, z - 1)
                                 SE -> (x + 1, y - 1, z)
                                 S  -> (x, y - 1, z + 1)
                                 SW -> (x - 1, y, z + 1)
                                 NW -> (x - 1, y + 1, z)

distance :: Position -> Position -> Int
distance (x1,y1,z1) (x2,y2,z2) = (abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)) `div` 2

steps :: ReadP [Step]
steps = do
  xs <- sepBy (many1 $ satisfy isLower) (char ',')
  return (map toStep xs)

toStep :: String -> Step
toStep "n"  = N
toStep "ne" = NE
toStep "se" = SE
toStep "s"  = S
toStep "sw" = SW
toStep "nw" = NW
