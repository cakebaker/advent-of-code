import Data.Bits ((.&.))

main :: IO ()
main = do
  let generatorA = iterate (generate factorA) 783
  let generatorB = iterate (generate factorB) 325

  let resultPuzzle1 = judge $ take 40000000 $ zip generatorA generatorB
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


factorA :: Int
factorA = 16807

factorB :: Int
factorB = 48271

divisor :: Int
divisor = 2147483647

generate :: Int -> Int -> Int
generate multiplier x = (x * multiplier) `rem` divisor

judge :: [(Int, Int)] -> Int
judge pairs = length $ filter matchLowest16Bits pairs

matchLowest16Bits :: (Int, Int) -> Bool
matchLowest16Bits (a, b) = a .&. 0xFFFF == b .&. 0xFFFF
