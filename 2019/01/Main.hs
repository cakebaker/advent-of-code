import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let masses = map toInt $ lines content
  
  let result1 = sum $ map calculateFuel masses
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let result2 = sum $ map calculateTotalFuel masses
  putStrLn $ "Result of puzzle 2: " ++ show result2


calculateFuel :: Int -> Int
calculateFuel x = x `div` 3 - 2

calculateTotalFuel :: Int -> Int
calculateTotalFuel x
  | fuel <= 0 = 0
  | otherwise = fuel + calculateTotalFuel fuel
  where fuel  = calculateFuel x

toInt :: String -> Int
toInt s = read s
