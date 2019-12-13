{-# LANGUAGE PatternSynonyms #-}

import System.Environment
import Control.Applicative
import Text.ParserCombinators.ReadP
import Data.Char (digitToInt, isDigit)
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let initialInstructionPointer = 0
  let inputValuePuzzle1 = 1
  let inputValuePuzzle2 = 5
  let intCodes = head $ map (fst . last) $ map (readP_to_S codes) $ lines content
  let program  = V.fromList intCodes

  let (_, _, result1) = exec inputValuePuzzle1 (initialInstructionPointer, program, [])
  mapM_ print result1

  let (_, _, result2) = exec inputValuePuzzle2 (initialInstructionPointer, program, [])
  mapM_ print result2


type InstructionPointer = Int
type Memory             = Vector Int
type Output             = [Int]
type ProgramState       = (InstructionPointer, Memory, Output)

pattern PositionMode  = 0
pattern ImmediateMode = 1

exec :: Int -> ProgramState -> ProgramState
exec inputValue programState@(instructionPointer, memory, _)
  | opcode == 1 = execFn $ execOp parameterModes (+) programState
  | opcode == 2 = execFn $ execOp parameterModes (*) programState
  | opcode == 3 = execFn $ input inputValue programState
  | opcode == 4 = execFn $ output mode1stParam programState
  | opcode == 5 = execFn $ jmpIfTrue parameterModes programState
  | opcode == 6 = execFn $ jmpIfFalse parameterModes programState
  | opcode == 7 = execFn $ lessThan parameterModes programState
  | opcode == 8 = execFn $ equals parameterModes programState
  | otherwise   = programState
  where (mode3rdParam:mode2ndParam:mode1stParam:d:e:[]) = leftPad 5 $ toDigits $ memory V.! instructionPointer
        opcode                                          = (10 * d) + e
        parameterModes                                  = [mode1stParam, mode2ndParam, mode3rdParam]
        execFn                                          = exec inputValue

execOp :: [Int] -> (Int -> Int -> Int) -> ProgramState -> ProgramState
execOp (modeA:modeB:_:[]) op (instructionPointer, memory, out) = (instructionPointer + instructionLength, memory V.// [(target, op valA valB)], out)
                                                                 where (a:b:target:[])   = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
                                                                       valA              = resolveMode a modeA memory
                                                                       valB              = resolveMode b modeB memory
                                                                       instructionLength = 4

input :: Int -> ProgramState -> ProgramState
input inputValue (instructionPointer, memory, out) = (instructionPointer + instructionLength, memory V.// [(target, inputValue)], out)
                                                     where target            = V.head $ V.tail $ V.slice instructionPointer instructionLength memory
                                                           instructionLength = 2

jmpIfTrue :: [Int] -> ProgramState -> ProgramState
jmpIfTrue modes programState = jmpIf modes (/= 0) programState

jmpIfFalse :: [Int] -> ProgramState -> ProgramState
jmpIfFalse modes programState = jmpIf modes (== 0) programState

jmpIf :: [Int] -> (Int -> Bool) -> ProgramState -> ProgramState
jmpIf (modeA:modeB:_:[]) op programState@(instructionPointer, memory, out)
  | op valA   = (valB, memory, out)
  | otherwise = (instructionPointer + instructionLength, memory, out)
  where (a:b:[])          = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
        valA              = resolveMode a modeA memory
        valB              = resolveMode b modeB memory
        instructionLength = 3

lessThan :: [Int] -> ProgramState -> ProgramState
lessThan modes programState = compareBy modes (<) programState

equals :: [Int] -> ProgramState -> ProgramState
equals modes programState = compareBy modes (==) programState

compareBy :: [Int] -> (Int -> Int -> Bool) -> ProgramState -> ProgramState
compareBy (modeA:modeB:_:[]) op(instructionPointer, memory, out)
  | op valA valB = (instructionPointer + instructionLength, memory V.// [(c, 1)], out)
  | otherwise    = (instructionPointer + instructionLength, memory V.// [(c, 0)], out)
  where (a:b:c:[])        = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
        valA              = resolveMode a modeA memory
        valB              = resolveMode b modeB memory
        instructionLength = 4

output :: Int -> ProgramState -> ProgramState
output mode (instructionPointer, memory, out) = (instructionPointer + instructionLength, memory, out ++ [outputValue])
                                                where param             = V.head $ V.tail $ V.slice instructionPointer instructionLength memory
                                                      outputValue       = resolveMode param mode memory
                                                      instructionLength = 2

resolveMode :: Int -> Int -> Memory -> Int
resolveMode x PositionMode memory = memory V.! x
resolveMode x ImmediateMode _     = x

leftPad :: Int -> [Int] -> [Int]
leftPad l xs = padding ++ xs
               where padCount = l - length xs
                     padding  = take padCount $ repeat 0

toDigits :: Int -> [Int]
toDigits x = map digitToInt $ show x

-- Parser

codes :: ReadP [Int]
codes = do
  xs <- sepBy integer (string ",")
  return xs

integer :: ReadP Int
integer = do
  x <- negativeInteger <|> positiveInteger
  return x

negativeInteger :: ReadP Int
negativeInteger = do
  char '-'
  x <- positiveInteger
  return $ negate x

positiveInteger :: ReadP Int
positiveInteger = do
  x <- many1 $ satisfy isDigit
  return $ read x
