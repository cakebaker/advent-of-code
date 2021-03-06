import System.Environment


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let discs = map parse $ lines content

  let resultPuzzle1 = timeStep 1 discs
  putStrLn $ "The first time to press the button is: " ++ show resultPuzzle1

  let newDisc = parse "Disc #7 has 11 positions; at time=0, it is at position 0."
  let resultPuzzle2 = timeStep 1 (discs ++ [newDisc])
  putStrLn $ "The first time to press the button with the new disc is: " ++ show resultPuzzle2


parse :: String -> (Int, [Int])
parse s = (highestPosition, initDisc discID currentPosition positions)
          where elements        = words s
                discID          = read $ tail $ elements !! 1
                positions       = read $ elements !! 3
                currentPosition = read $ init $ last elements
                highestPosition = positions - 1


initDisc :: Int -> Int -> Int -> [Int]
initDisc idOffset currentPosition positions = drop idOffset $ drop currentPosition $ cycle [0..(positions - 1)]


timeStep :: Int -> [(Int, [Int])] -> Int
timeStep time discs
  | areDiscsAtHighestPosition = time
  | otherwise                 = timeStep (succ time) (zip highestPositions movedPositions)
  where areDiscsAtHighestPosition = all (\(highest, next) -> highest == next) $ zip highestPositions nextPositions
        nextPositions             = map head positions
        movedPositions            = map (drop 1) positions
        highestPositions          = map (\(x, _) -> x) discs
        positions                 = map (\(_, x) -> x) discs
