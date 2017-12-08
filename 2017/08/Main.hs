import System.Environment
import qualified Data.Map as Map

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ lines content

  let resultPuzzle1 = solve instructions
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = solve2 instructions
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


type Instruction = (String, Int, String, (Int -> Bool))

solve :: [Instruction] -> Int
solve instructions = maximum $ registerValues
                     where registerValues = Map.elems $ execute instructions registers
                           registers      = Map.empty

solve2 :: [Instruction] -> Int
solve2 instructions = execute' instructions registers highestValue
                      where registers    = Map.empty
                            highestValue = 0

parse :: String -> Instruction
parse s = (register, offset, condRegister, condFn)
          where elements     = words s
                register     = elements !! 0
                offset       = offsetToInt (elements !! 1) (elements !! 2)
                condRegister = (elements !! 4)
                condFn       = toCondFn (elements !! 5) (read $ elements !! 6)

offsetToInt :: String -> String -> Int
offsetToInt op s
  | op == "dec" = (-1) * read s
  | otherwise   = read s

toCondFn :: String -> Int -> (Int -> Bool)
toCondFn ">" v  = (> v)
toCondFn "<" v  = (< v)
toCondFn "==" v = (== v)
toCondFn "!=" v = (/= v)
toCondFn ">=" v = (>= v)
toCondFn "<=" v = (<= v)

execute :: [Instruction] -> Map.Map String Int -> Map.Map String Int
execute [] m = m
execute (x:xs) m
  | executeCondition m condRegister condFn      = execute xs (Map.insertWith (+) register value m)
  | otherwise                                   = execute xs m
  where (register, value, condRegister, condFn) = x

execute' :: [Instruction] -> Map.Map String Int -> Int -> Int
execute' [] m max = max
execute' (x:xs) m max
  | executeCondition m condRegister condFn      = execute' xs updated (maximum [max, updated Map.! register])
  | otherwise                                   = execute' xs m max
  where (register, value, condRegister, condFn) = x
        updated                                 = Map.insertWith (+) register value m
 
executeCondition :: Map.Map String Int -> String -> (Int -> Bool) -> Bool
executeCondition m register f
  | Map.member register m = f (m Map.! register)
  | otherwise             = f 0
