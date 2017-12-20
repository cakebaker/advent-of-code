import System.Environment
import Control.Applicative
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.List (elemIndex)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let particles = map (fst . last) $ map (readP_to_S particle) $ lines content

  let resultPuzzle1 = lowestAccelerationParticleIndex particles
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


type Particle     = (Position, Velocity, Acceleration)
type Position     = (Int, Int, Int)
type Velocity     = (Int, Int, Int)
type Acceleration = (Int, Int, Int)

lowestAccelerationParticleIndex :: [Particle] -> Int
lowestAccelerationParticleIndex particles = index
                                            where accelerations      = map (\(_,_,acc) -> manhattanDistance acc) particles
                                                  lowestAcceleration = minimum accelerations
                                                  Just index         = elemIndex lowestAcceleration accelerations

manhattanDistance :: Position -> Int
manhattanDistance (x,y,z) = (abs x) + (abs y) + (abs z)

particle :: ReadP Particle
particle = do
  string "p="
  pos <- triplet
  string ", v="
  vel <- triplet
  string ", a="
  acc <- triplet
  return (pos, vel, acc)

triplet :: ReadP (Int, Int, Int)
triplet = do
  triplet <- between (char '<') (char '>') (sepBy number (char ','))
  return (triplet !! 0, triplet !! 1, triplet !! 2)

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
