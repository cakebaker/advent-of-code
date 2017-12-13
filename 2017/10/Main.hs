import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let ls = fst . last $ readP_to_S lengths $ head $ lines content
  let numbers = [0..255]

  let resultPuzzle1 = solve (V.fromList numbers) ls
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


solve :: Vector Int -> [Int] -> Int
solve numbers ls = (hash V.! 0) * (hash V.! 1)
                   where hash = step numbers ls 0 0

step :: Vector Int -> [Int] -> Int -> Int -> Vector Int
step v [] _ _              = v
step v (x:xs) current skip = step modified xs next (skip + 1)
                             where len      = V.length v
                                   next     = (current + x + skip) `mod` len
                                   modified = reverseSublist v x current

reverseSublist :: Vector Int -> Int -> Int -> Vector Int
reverseSublist v x current = v V.// (zip ids values)
                             where len    = V.length v
                                   ids    = map (\y -> (y + current) `mod` len) $ take x [0..]
                                   values = reverse $ map (\id -> v V.! id) ids

lengths :: ReadP [Int]
lengths = do
  ls <- sepBy (many1 $ satisfy isDigit) (char ',')
  return (map read ls)
