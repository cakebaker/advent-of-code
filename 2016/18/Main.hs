import System.Environment


safeTile :: Char
safeTile = '.'

trap :: Char
trap = '^'


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let initialRow = lines content
  let resultPuzzle1 = countSafeTiles $ generateRows 40 initialRow
  putStrLn $ "40 rows contain " ++ show resultPuzzle1 ++ " safe tiles"


generateRows :: Int -> [String] -> [String]
generateRows maxRows rows@(x:_)
  | maxRows == length rows = rows
  | otherwise              = generateRows maxRows (generateNextRow x : rows)


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


countSafeTiles :: [String] -> Int
countSafeTiles rows = sum $ map (\row -> length $ filter (== safeTile) row) rows
