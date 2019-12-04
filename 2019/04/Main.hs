import Data.Char (digitToInt)

main :: IO ()
main = do
  let start = 108457
  let end   = 562041
  
  let potentialPasswords = [start..end]

  let result1 = length $ filter isPassword potentialPasswords
  putStrLn $ "Result of puzzle 1: " ++ show result1


isPassword :: Int -> Bool
isPassword x = doDigitsNeverDecrease digits && containsDigitPair digits
               where digits = toDigits x

containsDigitPair :: [Int] -> Bool
containsDigitPair []     = False
containsDigitPair (_:[]) = False
containsDigitPair (a:b:xs)
  | a == b    = True
  | otherwise = containsDigitPair (b:xs)

doDigitsNeverDecrease :: [Int] -> Bool
doDigitsNeverDecrease []     = True
doDigitsNeverDecrease (_:[]) = True
doDigitsNeverDecrease (a:b:xs)
  | a > b     = False
  | otherwise = doDigitsNeverDecrease (b:xs)

toDigits :: Int -> [Int]
toDigits x = map digitToInt $ show x
