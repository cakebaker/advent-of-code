import System.Environment
import Data.Char (isDigit)
import Data.List (sortOn)
import Control.Applicative
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let nanobots = map (fst . last) $ map (readP_to_S nanobot) $ lines content

  let resultPuzzle1 = withinRangeOfStrongest nanobots
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type Nanobot  = (Position, Int)
type Position = (Int, Int, Int)

strongestNanobot :: [Nanobot] -> Nanobot
strongestNanobot nanobots = last $ sortOn snd nanobots

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1,y1,z1) (x2,y2,z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

withinRangeOfStrongest :: [Nanobot] -> Int
withinRangeOfStrongest nanobots = length $ filter (<= range) $ map (\(p,_) -> manhattanDistance pos p) nanobots
                                  where (pos, range) = strongestNanobot nanobots

-- parsing

nanobot :: ReadP Nanobot
nanobot = do
  pos <- position
  string ", "
  range <- range
  return (pos, range)

position :: ReadP Position
position = do
  string "pos="
  (x:y:z:[]) <- between (char '<') (char '>') (sepBy1 number (char ','))
  return (x, y, z)

range :: ReadP Int
range = do
  string "r="
  r <- positiveNumber
  return r

number :: ReadP Int
number = do
  x <- negativeNumber <|> positiveNumber
  return x

positiveNumber :: ReadP Int
positiveNumber = do
  x <- many1 $ satisfy (isDigit)
  return (read x)

negativeNumber :: ReadP Int
negativeNumber = do
  char '-'
  digits <- many1 $ satisfy (isDigit)
  return (read ("-" ++ digits))
