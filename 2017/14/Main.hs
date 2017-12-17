import KnotHash (hash)
import System.Environment
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main = do
  let input = "uugsqrei"

  let resultPuzzle1 = sum $ map (\s -> length $ filter (== '1') s) $ map hexToBinary $ map (generateHash input) [0..127]
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


generateHash :: String -> Int -> String
generateHash s x = hash (s ++ "-" ++ (show x))

hexToBinary :: String -> String
hexToBinary s = showIntAtBase 2 intToDigit (read hex) ""
                where hex = "0x" ++ s
