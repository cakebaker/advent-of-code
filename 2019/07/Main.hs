{-# LANGUAGE PatternSynonyms #-}

import System.Environment
import Control.Applicative
import Text.ParserCombinators.ReadP
import Data.Char (digitToInt, isDigit)
import Data.List (permutations)
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let inputSignal   = 0
  let intCodes      = head $ map (fst . last) $ map (readP_to_S codes) $ lines content
  let program       = V.fromList intCodes
  let phaseSettings = permutations [0..4]

  let result1 = maximum $ map (\phaseSetting -> calcThrusterSignal inputSignal program phaseSetting) phaseSettings
  putStrLn $ "Result of puzzle 1: " ++ show result1


calcThrusterSignal :: Int -> Memory -> [Int] -> Int
calcThrusterSignal inputSignal program (a:b:c:d:e:[]) = head outAmpE
                                                        where (_,_,_, outAmpA)   = exec (instructionPointer, program, [a, inputSignal], [])
                                                              (_,_,_, outAmpB)   = exec (instructionPointer, program, [b, head outAmpA], [])
                                                              (_,_,_, outAmpC)   = exec (instructionPointer, program, [c, head outAmpB], [])
                                                              (_,_,_, outAmpD)   = exec (instructionPointer, program, [d, head outAmpC], [])
                                                              (_,_,_, outAmpE)   = exec (instructionPointer, program, [e, head outAmpD], [])
                                                              instructionPointer = 0

-- IntCode computer from day 5

type InstructionPointer = Int
type Memory             = Vector Int
type Input              = [Int]
type Output             = [Int]
type ProgramState       = (InstructionPointer, Memory, Input, Output)

pattern PositionMode  = 0
pattern ImmediateMode = 1

exec :: ProgramState -> ProgramState
exec programState@(instructionPointer, memory, _, _)
  | opcode == 1 = exec $ execOp parameterModes (+) programState
  | opcode == 2 = exec $ execOp parameterModes (*) programState
  | opcode == 3 = exec $ input programState
  | opcode == 4 = exec $ output mode1stParam programState
  | opcode == 5 = exec $ jmpIfTrue parameterModes programState
  | opcode == 6 = exec $ jmpIfFalse parameterModes programState
  | opcode == 7 = exec $ lessThan parameterModes programState
  | opcode == 8 = exec $ equals parameterModes programState
  | otherwise   = programState
  where (mode3rdParam:mode2ndParam:mode1stParam:d:e:[]) = leftPad 5 $ toDigits $ memory V.! instructionPointer
        opcode                                          = (10 * d) + e
        parameterModes                                  = [mode1stParam, mode2ndParam, mode3rdParam]

execOp :: [Int] -> (Int -> Int -> Int) -> ProgramState -> ProgramState
execOp (modeA:modeB:_:[]) op (instructionPointer, memory, ins, outs) = (instructionPointer + instructionLength, memory V.// [(target, op valA valB)], ins, outs)
                                                                      where (a:b:target:[])   = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
                                                                            valA              = resolveMode a modeA memory
                                                                            valB              = resolveMode b modeB memory
                                                                            instructionLength = 4

input :: ProgramState -> ProgramState
input (instructionPointer, memory, (inputValue:ins), outs) = (instructionPointer + instructionLength, memory V.// [(target, inputValue)], ins, outs)
                                                             where target            = V.head $ V.tail $ V.slice instructionPointer instructionLength memory
                                                                   instructionLength = 2

jmpIfTrue :: [Int] -> ProgramState -> ProgramState
jmpIfTrue modes programState = jmpIf modes (/= 0) programState

jmpIfFalse :: [Int] -> ProgramState -> ProgramState
jmpIfFalse modes programState = jmpIf modes (== 0) programState

jmpIf :: [Int] -> (Int -> Bool) -> ProgramState -> ProgramState
jmpIf (modeA:modeB:_:[]) op programState@(instructionPointer, memory, ins, outs)
  | op valA   = (valB, memory, ins, outs)
  | otherwise = (instructionPointer + instructionLength, memory, ins, outs)
  where (a:b:[])          = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
        valA              = resolveMode a modeA memory
        valB              = resolveMode b modeB memory
        instructionLength = 3

lessThan :: [Int] -> ProgramState -> ProgramState
lessThan modes programState = compareBy modes (<) programState

equals :: [Int] -> ProgramState -> ProgramState
equals modes programState = compareBy modes (==) programState

compareBy :: [Int] -> (Int -> Int -> Bool) -> ProgramState -> ProgramState
compareBy (modeA:modeB:_:[]) op(instructionPointer, memory, ins, outs)
  | op valA valB = (instructionPointer + instructionLength, memory V.// [(c, 1)], ins, outs)
  | otherwise    = (instructionPointer + instructionLength, memory V.// [(c, 0)], ins, outs)
  where (a:b:c:[])        = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
        valA              = resolveMode a modeA memory
        valB              = resolveMode b modeB memory
        instructionLength = 4

output :: Int -> ProgramState -> ProgramState
output mode (instructionPointer, memory, ins, outs) = (instructionPointer + instructionLength, memory, ins, outs ++ [outputValue])
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
