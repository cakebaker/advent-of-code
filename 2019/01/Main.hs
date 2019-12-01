import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let masses = map toInt $ lines content
  
  let result1 = sum $ map calculateFuel masses
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let result2 = sum $ map (\x -> calculateTotalFuel x 0) masses
  putStrLn $ "Result of puzzle 2: " ++ show result2


calculateFuel :: Int -> Int
calculateFuel x = (div x 3) - 2

calculateTotalFuel :: Int -> Int -> Int
calculateTotalFuel x fuelCount
  | fuel <= 0 = fuelCount
  | otherwise = calculateTotalFuel fuel (fuelCount + fuel)
  where fuel  = calculateFuel x

toInt :: String -> Int
toInt s = read s
