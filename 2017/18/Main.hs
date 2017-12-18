import System.Environment
import Data.Char (isLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ lines content

  let resultPuzzle1 = solve instructions
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


data Operand     = Value Int | Register Char deriving (Show)
data Instruction = Snd Operand | Set Operand Operand | Add Operand Operand | Mul Operand Operand | Mod Operand Operand | Rcv Operand | Jgz Operand Operand deriving (Show)
type Registers   = Map Char Int

soundRegister :: Char
soundRegister = '@'

solve :: [Instruction] -> Int
solve instructions = zeroOrValue registers soundRegister
                     where registers = execute instructions 0 M.empty

execute :: [Instruction] -> Int -> Registers -> Registers
execute instructions index registers
  | index >= length instructions = registers
  | otherwise                    = case instructions !! index of
                                     Add a b -> execute instructions (index + 1) (add registers a b)
                                     Mod a b -> execute instructions (index + 1) (modulo registers a b)
                                     Mul a b -> execute instructions (index + 1) (mul registers a b)
                                     Set a b -> execute instructions (index + 1) (set registers a b)
                                     Snd a   -> execute instructions (index + 1) (sound registers a)
                                     Jgz a b -> execute instructions (index + (jgz registers a b)) registers
                                     Rcv a   -> case (rcv registers a) of
                                                  True  -> registers
                                                  False -> execute instructions (index + 1) registers
                                     

add :: Registers -> Operand -> Operand -> Registers
add registers a b = op registers (+) a b

modulo :: Registers -> Operand -> Operand -> Registers
modulo registers a b = op registers mod a b

mul :: Registers -> Operand -> Operand -> Registers
mul registers a b = op registers (*) a b

set :: Registers -> Operand -> Operand -> Registers
set registers (Register r) (Value v)    = M.insert r v registers
set registers (Register r) (Register x) = M.insert r v registers
                                          where v = zeroOrValue registers x

sound :: Registers -> Operand -> Registers
sound registers o = set registers (Register soundRegister) o

rcv :: Registers -> Operand -> Bool
rcv registers (Value v)    = v /= 0
rcv registers (Register r) = v /= 0 
                             where v = zeroOrValue registers r

jgz :: Registers -> Operand -> Operand -> Int
jgz registers (Register r) (Value v)
  | value > 0 = v
  | otherwise = 1
  where value = zeroOrValue registers r
jgz registers (Register r) (Register x)
  | valueR > 0 = valueX
  | otherwise  = 1
  where valueR = zeroOrValue registers r
        valueX = zeroOrValue registers x

op :: Registers -> (Int -> Int -> Int) -> Operand -> Operand -> Registers
op registers f (Register r) (Value v)    = M.insert r (f a v) registers
                                           where a = zeroOrValue registers r
op registers f (Register r) (Register x) = M.insert r (f a b) registers
                                           where a = zeroOrValue registers r
                                                 b = zeroOrValue registers x 

zeroOrValue :: Registers -> Char -> Int
zeroOrValue registers k = case registers M.!? k of
                            Just a  -> a
                            Nothing -> 0

parse :: String -> Instruction
parse s = case (words s) of
            ["snd", operand]           -> Snd (parseOperand operand)
            ["set", register, operand] -> Set (parseOperand register) (parseOperand operand)
            ["add", register, operand] -> Add (parseOperand register) (parseOperand operand)
            ["mul", register, operand] -> Mul (parseOperand register) (parseOperand operand)
            ["mod", register, operand] -> Mod (parseOperand register) (parseOperand operand)
            ["rcv", operand]           -> Rcv (parseOperand operand)
            ["jgz", register, operand] -> Jgz (parseOperand register) (parseOperand operand)

parseOperand :: String -> Operand
parseOperand s
  | length s == 1 && isLower (head s) = Register (head s)
  | otherwise                         = Value (read s)
