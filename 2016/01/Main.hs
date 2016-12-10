import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map dropComma $ words $ head $ lines content

  print instructions


dropComma :: String -> String
dropComma s
  | last s == ',' = init s
  | otherwise     = s
