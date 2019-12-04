import Data.Char (digitToInt)
import Data.List (group)

main :: IO ()
main = do
  let start = 108457
  let end   = 562041
  
  let potentialPasswords = [start..end]

  let result1 = length $ filter isPassword potentialPasswords
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let result2 = length $ filter isAdvancedPassword potentialPasswords
  putStrLn $ "Result of puzzle 2: " ++ show result2


isPassword :: Int -> Bool
isPassword x = doDigitsNeverDecrease digits && containsDigitPair digits
               where digits = toDigits x

isAdvancedPassword :: Int -> Bool
isAdvancedPassword x = doDigitsNeverDecrease digits && containsStrictDigitPair digits
                       where digits = toDigits x

containsDigitPair :: [Int] -> Bool
containsDigitPair xs = any (\x -> length x >= 2) $ group xs

containsStrictDigitPair :: [Int] -> Bool
containsStrictDigitPair xs = any (\x -> length x == 2) $ group xs

doDigitsNeverDecrease :: [Int] -> Bool
doDigitsNeverDecrease []     = True
doDigitsNeverDecrease (_:[]) = True
doDigitsNeverDecrease (a:b:xs)
  | a > b     = False
  | otherwise = doDigitsNeverDecrease (b:xs)

toDigits :: Int -> [Int]
toDigits x = map digitToInt $ show x
