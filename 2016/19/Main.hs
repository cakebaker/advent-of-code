

main :: IO ()
main = do
  let elves = 3004953
  let resultPuzzle1 = getElveWithAllPresents [(i, 1) | i <- [1..elves]]
  putStrLn $ "The elve with all presents is: " ++ show resultPuzzle1


getElveWithAllPresents :: [(Int, Int)] -> Int
getElveWithAllPresents ((elve, _):[]) = elve
getElveWithAllPresents xs
  | even $ length xs = getElveWithAllPresents $ takePresents xs
  | otherwise        = getElveWithAllPresents $ (last xs) : (takePresents $ init xs)


takePresents :: [(Int, Int)] -> [(Int, Int)]
takePresents []                                    = []
takePresents (x:[])                                = x : []
takePresents ((elve, presentsA):(_, presentsB):xs) = (elve, presentsA + presentsB) : takePresents xs
