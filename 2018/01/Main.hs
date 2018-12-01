import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let resultPuzzle1 = sum $ map parse $ lines content

  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


parse :: String -> Int
parse ('+':s) = read s
parse ('-':s) = 0 - read s
