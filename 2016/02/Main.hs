import System.Environment


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map (\xs -> map parse xs) $ lines content
  let resultPuzzle1 = getBathroomCode instructions

  putStrLn $ "Bathroom code is: " ++ resultPuzzle1


parse :: Char -> (Int -> Int)
parse 'U' = up
parse 'D' = down
parse 'L' = left
parse 'R' = right


startButton :: Int
startButton = 5


getBathroomCode :: [[(Int -> Int)]] -> String
getBathroomCode instructions = foldr1 (++) $ map show $ tail $ scanl execute startButton instructions


execute :: Int -> [(Int -> Int)] -> Int
execute button [] = button
execute button (f:fs) = execute (f button) fs


up :: Int -> Int
up x
  | x < 4 = x
  | otherwise = x - 3


down :: Int -> Int
down x
  | x > 6 = x
  | otherwise = x + 3


left :: Int -> Int
left x
  | elem x [1, 4, 7] = x
  | otherwise = x - 1


right :: Int -> Int
right x
  | elem x [3, 6, 9] = x
  | otherwise = x + 1
