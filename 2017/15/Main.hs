import Data.Bits ((.&.))

main :: IO ()
main = do
  let inputA = 783
  let inputB = 325

  let generatorA  = iterate (generate factorA) inputA
  let generatorB  = iterate (generate factorB) inputB
  let generatorA' = iterate (generate' divisibleBy4 factorA) inputA
  let generatorB' = iterate (generate' divisibleBy8 factorB) inputB

  let resultPuzzle1 = judge $ take 40000000 $ zip generatorA generatorB
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = judge $ take 5000000 $ zip generatorA' generatorB'
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


factorA :: Int
factorA = 16807

factorB :: Int
factorB = 48271

divisor :: Int
divisor = 2147483647

generate :: Int -> Int -> Int
generate multiplier x = (x * multiplier) `rem` divisor

generate' :: (Int -> Bool) -> Int -> Int -> Int
generate' cond multiplier x
  | cond result = result
  | otherwise   = generate' cond multiplier result
  where result = generate multiplier x

divisibleBy4 :: Int -> Bool
divisibleBy4 x = x .&. 3 == 0

divisibleBy8 :: Int -> Bool
divisibleBy8 x = x .&. 7 == 0

judge :: [(Int, Int)] -> Int
judge pairs = length $ filter matchLowest16Bits pairs

matchLowest16Bits :: (Int, Int) -> Bool
matchLowest16Bits (a, b) = a .&. 0xFFFF == b .&. 0xFFFF
