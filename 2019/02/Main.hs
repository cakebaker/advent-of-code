import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let intCodes = head $ map (fst . last) $ map (readP_to_S codes) $ lines content
  let program  = V.fromList intCodes

  let result1 = V.head $ execute initialInstructionPointer $ program V.// [(1, 12), (2, 2)]
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let nounAndVerbs = [(noun, verb) | noun <- [0..99], verb <- [0..99]]
  let wantedOutput = 19690720
  let (noun, verb) = findInputs nounAndVerbs wantedOutput program
  let result2      = 100 * noun + verb
  putStrLn $ "Result of puzzle 2: " ++ show result2


initialInstructionPointer :: Int
initialInstructionPointer = 0

execute :: Int -> Vector Int -> Vector Int
execute instructionPointer memory
  | opcode == 1 = execute nextInstructionPointer $ executeOpcode parameters (+) memory
  | opcode == 2 = execute nextInstructionPointer $ executeOpcode parameters (*) memory
  | otherwise   = memory
  where opcode                 = memory V.! instructionPointer
        nextInstructionPointer = instructionPointer + instructionLength
        parameters             = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
        instructionLength      = 4

executeOpcode :: [Int] -> (Int -> Int -> Int) -> Vector Int -> Vector Int
executeOpcode (a:b:target:[]) op memory = memory V.// [(target, op valA valB)]
                                          where valA = memory V.! a
                                                valB = memory V.! b

findInputs :: [(Int, Int)] -> Int -> Vector Int -> (Int, Int)
findInputs ((noun, verb):xs) wantedOutput memory
  | V.head result == wantedOutput = (result V.! nounAddress, result V.! verbAddress)
  | otherwise                     = findInputs xs wantedOutput memory
  where result      = execute initialInstructionPointer $ memory V.// [(nounAddress, noun), (verbAddress, verb)]
        nounAddress = 1
        verbAddress = 2

codes :: ReadP [Int]
codes = do
  xs <- sepBy (many1 $ satisfy isDigit) (string ",")
  return $ map toInt xs

toInt :: String -> Int
toInt s = read s
