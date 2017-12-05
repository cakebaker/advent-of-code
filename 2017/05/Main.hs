import System.Environment
import Data.Array

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = listArray (0, (length instructions) - 1) instructions
                     where instructions = map (\s -> read s :: Int) $ lines content

  let resultPuzzle1 = countSteps instructions succ
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = countSteps instructions calcOffset
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


countSteps :: Array Int Int -> (Int -> Int) -> Int
countSteps a f = move (a, initialPos) f initialCount
                 where initialPos   = 0
                       initialCount = 0

move :: (Array Int Int, Int) -> (Int -> Int) -> Int -> Int
move (a, pos) offsetFn acc
  | pos > (snd $ bounds a) = acc
  | otherwise              = move (a // [(pos, offsetFn v)], pos + v) offsetFn (succ acc)
                             where v = a ! pos

calcOffset :: Int -> Int
calcOffset o
  | o >= 3    = pred o
  | otherwise = succ o
