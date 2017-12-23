import System.Environment
import Data.Char (isLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ lines content
  let registers = M.fromList $ zip ['a'..'h'] (repeat 0)

  let resultPuzzle1 = solve instructions registers
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


data Operand     = Value Int | Register Char deriving (Show)
data Instruction = Set Operand Operand | Sub Operand Operand | Mul Operand Operand | Jnz Operand Operand deriving (Show)
type Registers   = Map Char Int

solve :: [Instruction] -> Registers -> Int
solve instructions registers = execute instructions 0 registers 0

execute :: [Instruction] -> Int -> Registers -> Int -> Int
execute instructions index registers mulCount
  | index >= length instructions = mulCount
  | otherwise                    = case instructions !! index of
                                     Set a b -> execute instructions (index + 1) (set registers a b) mulCount
                                     Sub a b -> execute instructions (index + 1) (sub registers a b) mulCount
                                     Mul a b -> execute instructions (index + 1) (mul registers a b) (mulCount + 1)
                                     Jnz a b -> execute instructions (index + (jnz registers a b)) registers mulCount

set :: Registers -> Operand -> Operand -> Registers
set registers (Register r) (Value v)    = M.insert r v registers
set registers (Register r) (Register x) = M.insert r v registers
                                          where v = registers M.! x

sub :: Registers -> Operand -> Operand -> Registers
sub registers (Register r) (Value v)    = M.insert r (z - v) registers
                                          where z = registers M.! r
sub registers (Register r) (Register x) = M.insert r (z - v) registers
                                          where z = registers M.! r
                                                v = registers M.! x

mul :: Registers -> Operand -> Operand -> Registers
mul registers (Register r) (Value v)    = M.insert r (v * multiplier) registers
                                          where multiplier = registers M.! r
mul registers (Register r) (Register x) = M.insert r (v * multiplier) registers
                                          where v          = registers M.! x
                                                multiplier = registers M.! r

jnz :: Registers -> Operand -> Operand -> Int
jnz registers (Register r) (Value v)
  | x /= 0    = v
  | otherwise = 1
  where x = registers M.! r
jnz registers (Value x) (Value v)
  | x /= 0    = v
  | otherwise = 1

parse :: String -> Instruction
parse s = case (words s) of
            ["set", operandA, operandB] -> Set (parseOperand operandA) (parseOperand operandB)
            ["sub", operandA, operandB] -> Sub (parseOperand operandA) (parseOperand operandB)
            ["mul", operandA, operandB] -> Mul (parseOperand operandA) (parseOperand operandB)
            ["jnz", operandA, operandB] -> Jnz (parseOperand operandA) (parseOperand operandB)

parseOperand :: String -> Operand
parseOperand s
  | length s == 1 && isLower (head s) = Register (head s)
  | otherwise                         = Value (read s)
