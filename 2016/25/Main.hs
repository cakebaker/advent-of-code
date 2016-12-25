import System.Environment
import Data.Array
import Data.List (isPrefixOf)


data Operand = Value Int | Register Int deriving (Show)
data Instruction = Cpy Operand Operand | Inc Operand | Dec Operand | Jnz Operand Operand | Out Operand deriving (Show)
type Registers = Array Int Int


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ lines content
  let registers = array (0, 3) [(i, 0) | i <- [0..3]]
  let resultPuzzle1 = getLowestIntForInit instructions registers 0
  putStrLn $ "The lowest integer for initialization is: " ++ show resultPuzzle1


getLowestIntForInit :: [Instruction] -> Registers -> Int -> Int
getLowestIntForInit instructions registers init
  | clockSignal == targetClockSignal = init
  | otherwise                        = getLowestIntForInit instructions registers (succ init)
  where initializedRegister = registers // [(0, init)]
        clockSignal         = take 20 $ execute instructions 0 initializedRegister
        targetClockSignal   = take 20 $ cycle [0,1]


execute :: [Instruction] -> Int -> Registers -> [Int]
execute instructions index registers = case instructions !! index of
                                         Inc (Register r)       -> execute instructions (succ index) (inc r registers)
                                         Dec (Register r)       -> execute instructions (succ index) (dec r registers)
                                         Cpy from (Register to) -> execute instructions (succ index) (cpy from to registers)
                                         Jnz value (Value to)   -> execute instructions (jnz value to index registers) registers
                                         Out value              -> (out value registers) : (execute instructions (succ index) registers)


inc :: Int -> Registers -> Registers
inc register registers = registers // [(register, succ value)]
                         where value = registers ! register


dec :: Int -> Registers -> Registers
dec register registers = registers // [(register, pred value)]
                         where value = registers ! register


cpy :: Operand -> Int -> Registers -> Registers
cpy (Value value) to registers   = registers // [(to, value)]
cpy (Register from) to registers = registers // [(to, value)]
                                   where value = registers ! from


jnz :: Operand -> Int -> Int -> Registers -> Int
jnz (Value value) to index _                = if value /= 0 then (index + to) else (succ index)
jnz (Register register) to index registers  = if value /= 0 then (index + to) else (succ index)
                                              where value = registers ! register


out :: Operand -> Registers -> Int
out (Value value) _ = value
out (Register register) registers = registers ! register


parse :: String -> Instruction
parse s = case (words s) of
            ["inc", operand] -> Inc (parseOperand operand)
            ["dec", operand] -> Dec (parseOperand operand)
            ["cpy", from, to] -> Cpy (parseOperand from) (parseOperand to)
            ["jnz", value, to] -> Jnz (parseOperand value) (parseOperand to)
            ["out", operand] -> Out (parseOperand operand)


parseOperand :: String -> Operand
parseOperand "a" = Register 0
parseOperand "b" = Register 1
parseOperand "c" = Register 2
parseOperand "d" = Register 3
parseOperand s   = Value (read s)
