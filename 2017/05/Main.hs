import System.Environment
import Data.Array

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = listArray (0, (length instructions) - 1) instructions
                     where instructions = map (\s -> read s :: Int) $ lines content

  let resultPuzzle1 = countSteps instructions
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


countSteps :: Array Int Int -> Int
countSteps a = move (a, initialPos) initialCount
               where initialPos   = 0
                     initialCount = 0

move :: (Array Int Int, Int) -> Int -> Int
move (a, pos) acc
  | pos > (snd $ bounds a) = acc
  | otherwise = move (a // [(pos, succ v)], pos + v) (succ acc)
                where v = a ! pos
