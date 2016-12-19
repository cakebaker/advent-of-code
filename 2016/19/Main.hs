

main :: IO ()
main = do
  let elves = 3004953
  let resultPuzzle1 = getElveWithMostPresents [(i, 1) | i <- [1..elves]]
  putStrLn $ "The elve with the most presents is: " ++ show resultPuzzle1


getElveWithMostPresents :: [(Int, Int)] -> Int
getElveWithMostPresents ((elve, _):[]) = elve
getElveWithMostPresents xs
  | even $ length xs = getElveWithMostPresents $ takePresents xs
  | otherwise        = getElveWithMostPresents $ (last xs) : (takePresents $ init xs)


takePresents :: [(Int, Int)] -> [(Int, Int)]
takePresents []                                    = []
takePresents (x:[])                                = x : []
takePresents ((elve, presentsA):(_, presentsB):xs) = (elve, presentsA + presentsB) : takePresents xs
