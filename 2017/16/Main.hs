import System.Environment
import Data.Char (isDigit)
import Control.Applicative
import Text.ParserCombinators.ReadP
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let moves = (fst . last) $ readP_to_S danceMoves $ head $ lines content
  let programs = V.fromList ['a'..'p']

  let resultPuzzle1 = executeMoves moves programs
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


data DanceMove = Spin Int | Exchange Int Int | Partner Char Char deriving (Show)

executeMoves :: [DanceMove] -> Vector Char -> Vector Char
executeMoves [] v                     = v
executeMoves ((Spin x):moves) v       = executeMoves moves (V.update v reindexed)
                                        where len       = length v
                                              indexed   = V.indexed v
                                              reindexed = V.map (\(i,a) -> ((i + x) `mod` len, a)) indexed
executeMoves ((Exchange a b):moves) v = executeMoves moves (v V.// [(a, bVal), (b, aVal)])
                                        where aVal = v V.! a
                                              bVal = v V.! b
executeMoves ((Partner a b):moves) v  = executeMoves moves (v V.// [(aIndex, b), (bIndex, a)])
                                        where (Just aIndex) = V.findIndex (== a) v
                                              (Just bIndex) = V.findIndex (== b) v

danceMoves :: ReadP [DanceMove]
danceMoves = do
  moves <- sepBy (spin <|> exchange <|> partner) (char ',')
  return moves

spin :: ReadP DanceMove
spin = do
  char 's'
  s <- many1 $ satisfy isDigit
  return (Spin (read s))

exchange :: ReadP DanceMove
exchange = do
  char 'x'
  a <- many1 $ satisfy isDigit
  char '/'
  b <- many1 $ satisfy isDigit
  return (Exchange (read a) (read b))

partner :: ReadP DanceMove
partner = do
  char 'p'
  a <- satisfy (\char -> char >= 'a' && char <= 'p')
  char '/'
  b <- satisfy (\char -> char >= 'a' && char <= 'p')
  return (Partner a b)
