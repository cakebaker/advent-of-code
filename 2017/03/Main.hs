import System.Environment

main :: IO ()
main = do
  let puzzleInput = 347991

  let resultPuzzle1 = calculateDistance puzzleInput
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


calculateDistance :: Int -> Int
calculateDistance field = halfDistance + abs (((rightBottom - field) `mod` distance) - halfDistance)
                          where (rightBottom, distance) = findRightBottom 1 field
                                halfDistance            = distance `div` 2

findRightBottom :: Int -> Int -> (Int, Int)
findRightBottom x field
  | rightBottom < field = findRightBottom (x + 2) field
  | otherwise           = (rightBottom, distance)
  where rightBottom     = x * x
        distance        = x - 1
