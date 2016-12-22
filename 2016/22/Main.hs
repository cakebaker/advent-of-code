import System.Environment


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let diskUsages = map parse $ drop 2 $ lines content
  let resultPuzzle1 = countViablePairs diskUsages
  putStrLn $ "Number of viable pairs of nodes: " ++ show resultPuzzle1


type UsageRecord = (String, Int, Int)


countViablePairs :: [UsageRecord] -> Int
countViablePairs xs = countPairs xs xs 0


countPairs :: [UsageRecord] -> [UsageRecord] -> Int -> Int
countPairs [] _ counter      = counter
countPairs (a:as) bs counter = countPairs as bs (counter + (length $ filter (isViablePair a) bs))


isViablePair :: UsageRecord -> UsageRecord -> Bool
isViablePair (nameA, usedA, availA) (nameB, usedB, availB)
  | usedA == 0     = False
  | nameA == nameB = False
  | usedA > availB = False
  | otherwise      = True


parse :: String -> UsageRecord
parse s = (name, used, avail)
          where elements = words s
                name     = elements !! 0
                used     = toInt $ elements !! 2
                avail    = toInt $ elements !! 3
                toInt    = (\x -> read $ init x)
