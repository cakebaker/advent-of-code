import System.Environment


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map (\xs -> map parse xs) $ lines content
  let resultPuzzle1 = getBathroomCode instructions

  putStrLn $ "Bathroom code is: " ++ resultPuzzle1


parse :: Char -> (Int -> Int)
parse 'U' = move (-3) [1, 2, 3]
parse 'D' = move   3  [7, 8, 9]
parse 'L' = move (-1) [1, 4, 7]
parse 'R' = move   1  [3, 6, 9]


startButton :: Int
startButton = 5


getBathroomCode :: [[(Int -> Int)]] -> String
getBathroomCode instructions = foldr1 (++) $ map show $ tail $ scanl execute startButton instructions


execute :: Int -> [(Int -> Int)] -> Int
execute button [] = button
execute button (f:fs) = execute (f button) fs


move :: Int -> [Int] -> Int -> Int
move step border x
  | elem x border = x
  | otherwise     = x + step
