{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

import System.Environment
import Control.Applicative
import Text.ParserCombinators.ReadP
import Data.Char (digitToInt, isDigit)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let initialInstructionPointer = 0
  let intCodes = head $ map (fst . last) $ map (readP_to_S codes) $ lines content
  let program  = V.fromList intCodes

  let !result1 = exec (initialInstructionPointer, program)
  return ()


type Memory = Vector Int
type ProgramState = (Int, Memory)

pattern PositionMode  = 0
pattern ImmediateMode = 1

exec :: ProgramState -> ProgramState
exec programState@(instructionPointer, memory)
  | opcode == 1 = exec $ execOp parameterModes (+) programState
  | opcode == 2 = exec $ execOp parameterModes (*) programState
  | opcode == 3 = exec $ input programState
  | opcode == 4 = exec $ output mode1stParam programState
  | otherwise   = programState
  where (mode3rdParam:mode2ndParam:mode1stParam:d:e:[]) = leftPad 5 $ toDigits $ memory V.! instructionPointer
        opcode                                          = (10 * d) + e
        parameterModes                                  = [mode1stParam, mode2ndParam, mode3rdParam]

execOp :: [Int] -> (Int -> Int -> Int) -> ProgramState -> ProgramState
execOp (modeA:modeB:_:[]) op (instructionPointer, memory) = (instructionPointer + instructionLength, memory V.// [(target, op valA valB)])
                                                            where (a:b:target:[])   = V.toList $ V.tail $ V.slice instructionPointer instructionLength memory
                                                                  valA              = resolveMode a modeA memory
                                                                  valB              = resolveMode b modeB memory
                                                                  instructionLength = 4

input :: ProgramState -> ProgramState
input (instructionPointer, memory) = (instructionPointer + instructionLength, memory V.// [(target, inputValue)])
                                     where target            = V.head $ V.tail $ V.slice instructionPointer instructionLength memory
                                           instructionLength = 2
                                           inputValue        = 1

output :: Int -> ProgramState -> ProgramState
output mode (instructionPointer, memory) = trace (show $ resolveMode value mode memory) (instructionPointer + instructionLength, memory)
                                           where value             = V.head $ V.tail $ V.slice instructionPointer instructionLength memory
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
