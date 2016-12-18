import System.Environment


safeTile :: Char
safeTile = '.'

trap :: Char
trap = '^'


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let initialRow = head $ lines content

  let resultPuzzle1 = countSafeTiles 40 0 initialRow
  putStrLn $ "40 rows contain " ++ show resultPuzzle1 ++ " safe tiles"

  let resultPuzzle2 = countSafeTiles 400000 0 initialRow
  putStrLn $ "400000 rows contain " ++ show resultPuzzle2 ++ " safe tiles"


countSafeTiles :: Int -> Int -> String -> Int
countSafeTiles 0 safeTileCounter _            = safeTileCounter
countSafeTiles rowCounter safeTileCounter row = countSafeTiles (pred rowCounter) (safeTileCounter + safeTiles) (generateNextRow row)
                                                where safeTiles = length $ filter (== safeTile) row


generateNextRow :: String -> String
generateNextRow s = generate $ safeTile : s ++ (safeTile : [])


generate :: String -> String
generate [] = []
generate (_:[]) = []
generate (_:_:[]) = []
generate (left:center:right:xs)
  | left == trap     && center == trap     && right == safeTile = trap : generate rest
  | left == safeTile && center == trap     && right == trap     = trap : generate rest
  | left == trap     && center == safeTile && right == safeTile = trap : generate rest
  | left == safeTile && center == safeTile && right == trap     = trap : generate rest
  | otherwise                                                   = safeTile : generate rest
  where rest = center:right:xs
