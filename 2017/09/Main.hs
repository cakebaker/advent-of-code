import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let stream = head $ lines content

  let resultPuzzle1 = solve stream
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


solve :: String -> Int
solve s = score withoutGarbage 1 0
          where withoutCanceled = removeCanceled s ""
                withoutGarbage  = removeGarbage withoutCanceled ""

removeCanceled :: String -> String -> String
removeCanceled [] acc     = acc
removeCanceled (a:b:xs) acc
  | a == '!'              = removeCanceled xs acc
  | otherwise             = removeCanceled (b:xs) (a:acc)
removeCanceled (a:xs) acc = reverse (a:acc)

removeGarbage :: String -> String -> String
removeGarbage [] acc = reverse acc
removeGarbage (x:xs) acc
  | x == '<'         = removeGarbage (drop 1 $ dropWhile (/= '>') xs) acc
  | otherwise        = removeGarbage xs (x:acc)

score :: String -> Int -> Int -> Int
score [] _ total = total
score (x:xs) i total
  | x == '{'     = score xs (i + 1) (total + i)
  | x == '}'     = score xs (i - 1) total
  | otherwise    = score xs i total
