import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let masses = map toInt $ lines content
  
  let requiredFuel = sum $ map calculateFuel masses
  putStrLn $ "Result of puzzle 1: " ++ show requiredFuel


calculateFuel :: Int -> Int
calculateFuel x = (div x 3) - 2

toInt :: String -> Int
toInt s = read s
