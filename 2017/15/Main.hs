import Data.Int (Int16, Int64)

main :: IO ()
main = do
  let generatorA = generate factorA 783
  let generatorB = generate factorB 325

  let resultPuzzle1 = length $ filter (\(a,b) -> toInt16 a == toInt16 b) $ take 40000000 $ zip generatorA generatorB
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


factorA :: Int64
factorA = 16807

factorB :: Int64
factorB = 48271

divisor :: Int64
divisor = 2147483647

generate :: Int64 -> Int64 -> [Int64]
generate multiplier x = y:generate multiplier y
                        where y = rem (x * multiplier) divisor

toInt16 :: Int64 -> Int16
toInt16 = fromIntegral
