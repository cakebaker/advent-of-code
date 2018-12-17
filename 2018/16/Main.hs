import System.Environment
import Data.Bits
import Data.Sequence as Seq hiding (filter, length, sortBy)
import Data.List (groupBy, sortBy)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let samples = parse $ lines content

  let resultPuzzle1 = solvePuzzle1 samples
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type Instruction = (Int, Int, Int, Int)
type Registers   = Seq Int
type OpCode      = (Instruction -> Registers -> Registers)
type Sample      = (Instruction, (Registers, Registers))

solvePuzzle1 :: [Sample] -> Int
solvePuzzle1 samples = sum $ map snd $ filter (\(x,_) -> x > 2) $ map (\x -> (countPassingOpCodes $ head x, length x)) $ groupBy groupByOpCode $ sortBy sortByOpCode samples

countPassingOpCodes :: Sample -> Int
countPassingOpCodes (instruction, (before, after)) = length $ filter (== True) $ map (\fn -> after == fn instruction before) opCodes

groupByOpCode :: Sample -> Sample -> Bool
groupByOpCode ((a,_,_,_),_) ((b,_,_,_),_) = a == b

sortByOpCode :: Sample -> Sample -> Ordering
sortByOpCode ((a,_,_,_),_) ((b,_,_,_),_) = compare a b

opCodes :: [OpCode]
opCodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

addr :: OpCode
addr (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA + registers `index` inputB

addi :: OpCode
addi (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA + inputB

mulr :: OpCode 
mulr (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA * registers `index` inputB

muli :: OpCode
muli (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA * inputB

banr :: OpCode
banr (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA .&. registers `index` inputB

bani :: OpCode
bani (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA .&. inputB

borr :: OpCode
borr (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA .|. registers `index` inputB

bori :: OpCode
bori (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = registers `index` inputA .|. inputB

setr :: OpCode
setr (_, inputA, _, output) registers = update output newValue registers
                                        where newValue = registers `index` inputA

seti :: OpCode
seti (_, inputA, _, output) registers = update output newValue registers
                                        where newValue = inputA

gtir :: OpCode
gtir (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = if inputA > registers `index` inputB then 1 else 0

gtri :: OpCode
gtri (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = if registers `index` inputA > inputB then 1 else 0

gtrr :: OpCode
gtrr (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = if registers `index` inputA > registers `index` inputB then 1 else 0

eqir :: OpCode
eqir (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = if inputA == registers `index` inputB then 1 else 0

eqri :: OpCode
eqri (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = if registers `index` inputA == inputB then 1 else 0

eqrr :: OpCode
eqrr (_, inputA, inputB, output) registers = update output newValue registers
                                             where newValue = if registers `index` inputA == registers `index` inputB then 1 else 0

-- parsing

parse :: [String] -> [Sample]
parse []                            = []
parse ("":"":"":xs)                 = [] -- ignore input for puzzle 2 for now
parse ("":xs)                       = parse xs
parse (before:instruction:after:xs) = (parseInstruction instruction, (parseRegisters before, parseRegisters after)):parse xs

parseInstruction :: String -> Instruction
parseInstruction s = (ints !! 0, ints !! 1, ints !! 2, ints !! 3)
                     where ints = map read $ words s

parseRegisters :: String -> Registers
parseRegisters s = Seq.fromList $ map read ([s !! 9]:[s !! 12]:[s !! 15]:[s !! 18]:[])
