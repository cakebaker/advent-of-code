import System.Environment
import Data.Char (digitToInt)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let captcha = map digitToInt $ head $ lines content
  let infiniteCaptcha = cycle captcha

  let resultPuzzle1 = sumMatching captcha (tail infiniteCaptcha)
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let offset = length captcha `div` 2
  let resultPuzzle2 = sumMatching captcha (drop offset infiniteCaptcha)
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


sumMatching :: [Int] -> [Int] -> Int
sumMatching [] _ = 0
sumMatching (x:xs) (y:ys)
  | x == y    = x + sumMatching xs ys
  | otherwise = sumMatching xs ys
