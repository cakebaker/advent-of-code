import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isLower)
import Data.List (isInfixOf)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let programs = map (fst . last) $ map (readP_to_S program) $ lines content
  
  let resultPuzzle1 = solve programs
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


type Program = (String, Int, [String])

solve :: [Program] -> String
solve programs = findBottomProgram $ filter hasAbovePrograms programs

findBottomProgram :: [Program] -> String
findBottomProgram xs = find names abovePrograms
                       where names         = map (\(name, _, _) -> name) xs
                             abovePrograms = map (\(_, _, abovePrograms) -> abovePrograms) xs

find :: [String] -> [[String]] -> String
find (x:xs) ys
  | hasElement x ys = find xs ys
  | otherwise       = x

hasElement :: String -> [[String]] -> Bool
hasElement _ []     = False
hasElement s (x:xs)
  | isInfixOf [s] x = True
  | otherwise       = hasElement s xs

hasAbovePrograms :: Program -> Bool
hasAbovePrograms (_, _, abovePrograms) = (not . null) abovePrograms

program :: ReadP Program
program = do
  n <- name
  w <- weight
  programs <- option [] abovePrograms
  return (n, w, programs)

name :: ReadP String
name = do
  n <- many1 $ satisfy isLower
  string " "
  return n

weight :: ReadP Int
weight = do
  w <- between (char '(') (char ')') (many1 $ satisfy isDigit)
  return (read w)

abovePrograms :: ReadP [String]
abovePrograms = do
  string " -> "
  programs <- sepBy (many1 $ satisfy isLower) (string ", ")
  return programs
