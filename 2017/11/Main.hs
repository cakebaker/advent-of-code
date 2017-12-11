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


data Step = N | NE | SE | S | SW | NW deriving (Show)

type Point = (Int, Int, Int)

solve :: [Step] -> Int
solve steps = distance start end
              where start = (0,0,0)
                    end   = doStep steps start

-- using cubic coordinates, see https://www.redblobgames.com/grids/hexagons/
doStep :: [Step] -> Point -> Point
doStep [] p           = p
doStep (a:as) (x,y,z) = case a of N  -> doStep as (x, y + 1, z - 1)
                                  NE -> doStep as (x + 1, y, z - 1)
                                  SE -> doStep as (x + 1, y - 1, z)
                                  S  -> doStep as (x, y - 1, z + 1)
                                  SW -> doStep as (x - 1, y, z + 1)
                                  NW -> doStep as (x - 1, y + 1, z)

distance :: Point -> Point -> Int
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
