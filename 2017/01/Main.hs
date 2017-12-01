import System.Environment
import Data.Char (digitToInt)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let captcha = map digitToInt $ head $ lines content
  let infiniteCaptcha = cycle captcha

  let resultPuzzle1 = sumMatching captcha (tail infiniteCaptcha) 0
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let offset = length captcha `div` 2
  let resultPuzzle2 = sumMatching captcha (drop offset infiniteCaptcha) 0
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


sumMatching :: [Int] -> [Int] -> Int -> Int
sumMatching [] _ total = total
sumMatching (x:xs) (y:ys) total
  | x == y    = sumMatching xs ys (total + x)
  | otherwise = sumMatching xs ys total
