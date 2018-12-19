import System.Environment
import Data.Sequence as Seq
import Data.Char (digitToInt)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let (instructionPointer, program) = parse $ lines content

  let resultPuzzle1 = solvePuzzle1 instructionPointer program
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type Instruction = (OpCode, Parameters)
type Parameters  = (Int, Int, Int)
type Registers   = Seq Int
type OpCode      = (Parameters -> Registers -> Registers)

solvePuzzle1 :: Int -> Seq Instruction -> Int
solvePuzzle1 instructionPointer program = finalRegisters `index` 0
                                          where finalRegisters   = execute instructionPointer program initialRegisters
                                                initialRegisters = Seq.fromList [0,0,0,0,0,0]

execute :: Int -> Seq Instruction -> Registers -> Registers
execute instructionPointer program registers
  | instructionID < Seq.length program = execute instructionPointer program (incInstructionPointer instructionPointer $ opCode params registers)
  | otherwise                          = registers
  where instructionID    = registers `index` instructionPointer
        (opCode, params) = program `index` instructionID

incInstructionPointer :: Int -> Registers -> Registers
incInstructionPointer instructionPointer registers = update instructionPointer (succ v) registers 
                                                     where v = registers `index` instructionPointer

addr :: OpCode
addr (inputA, inputB, output) registers = update output newValue registers
                                          where newValue = registers `index` inputA + registers `index` inputB

addi :: OpCode
addi (inputA, inputB, output) registers = update output newValue registers
                                          where newValue = registers `index` inputA + inputB

mulr :: OpCode 
mulr (inputA, inputB, output) registers = update output newValue registers
                                          where newValue = registers `index` inputA * registers `index` inputB

muli :: OpCode
muli (inputA, inputB, output) registers = update output newValue registers
                                          where newValue = registers `index` inputA * inputB

setr :: OpCode
setr (inputA, _, output) registers = update output newValue registers
                                     where newValue = registers `index` inputA

seti :: OpCode
seti (inputA, _, output) registers = update output newValue registers
                                     where newValue = inputA

gtrr :: OpCode
gtrr (inputA, inputB, output) registers = update output newValue registers
                                          where newValue = if registers `index` inputA > registers `index` inputB then 1 else 0

eqrr :: OpCode
eqrr (inputA, inputB, output) registers = update output newValue registers
                                          where newValue = if registers `index` inputA == registers `index` inputB then 1 else 0

-- parsing

parse :: [String] -> (Int, Seq Instruction)
parse (x:xs) = (instructionPointer, Seq.fromList $ parseProgram xs)
               where instructionPointer = digitToInt $ last x

parseProgram :: [String] -> [Instruction]
parseProgram []     = []
parseProgram (x:xs) = (parseInstruction x):parseProgram xs 

parseInstruction :: String -> Instruction
parseInstruction s
  | opCode == "addr" = (addr, params)
  | opCode == "addi" = (addi, params)
  | opCode == "mulr" = (mulr, params)
  | opCode == "muli" = (muli, params)
  | opCode == "setr" = (setr, params)
  | opCode == "seti" = (seti, params)
  | opCode == "gtrr" = (gtrr, params)
  | opCode == "eqrr" = (eqrr, params)
  where opCode = head $ words s
        ps     = map read $ tail $ words s
        params = (ps !! 0, ps !! 1, ps !! 2)
