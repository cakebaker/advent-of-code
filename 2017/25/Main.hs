{-# LANGUAGE BangPatterns #-}
import Prelude hiding (Left, Right)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let steps = 12861455
  let initialState = 'A'
  let blueprint = M.fromList [('A', ((1, Right, 'B'), (0, Left,  'B'))),
                              ('B', ((1, Left,  'C'), (0, Right, 'E'))),
                              ('C', ((1, Right, 'E'), (0, Left,  'D'))),
                              ('D', ((1, Left,  'A'), (1, Left,  'A'))),
                              ('E', ((0, Right, 'A'), (0, Right, 'F'))),
                              ('F', ((1, Right, 'E'), (1, Right, 'A')))]

  let tape = Tape (repeat 0) 0 0 0 (repeat 0)

  let resultPuzzle1 = solve blueprint initialState tape steps
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


data Direction = Right | Left deriving (Show)
type Instruction = (Int, Direction, Char)

-- inspired by https://stackoverflow.com/questions/46813868/how-do-you-build-an-infinite-grid-like-data-structure-in-haskell
data Tape a = Tape [a] !a !Int !Int [a]
instance (Show a) => Show (Tape a) where
  show (Tape ls x n c rs) = show (reverse (take 3 ls)) ++ " " ++ show (x,n,c) ++ " " ++ show (take 3 rs)

solve :: Map Char (Instruction, Instruction) -> Char -> Tape Int -> Int -> Int
solve blueprint state tape steps = getCounter $ snd $ head $ drop steps $ iterate (step blueprint) (state, tape)

step :: Map Char (Instruction, Instruction) -> (Char, Tape Int) -> (Char, Tape Int)
step blueprint (!currentState, !tape) = (newState, movedTape)
                                        where (newValue, direction, newState) = getInstruction (blueprint M.! currentState) (getCurrentValue tape)
                                              movedTape                       = move (update tape newValue) direction

getInstruction :: (Instruction, Instruction) -> Int -> Instruction
getInstruction (instruction, _) 0 = instruction
getInstruction (_, instruction) 1 = instruction

update :: Tape Int -> Int -> Tape Int
update (Tape ls 0 n c rs) 1 = Tape ls 1 n (c + 1) rs
update (Tape ls 1 n c rs) 0 = Tape ls 0 n (c - 1) rs
update (Tape ls x n c rs) v = Tape ls v n c rs

getCurrentValue :: Tape a -> a
getCurrentValue (Tape _ v _ _ _) = v

getCounter :: Tape a -> Int
getCounter (Tape _ _ _ c _) = c

move :: Tape a -> Direction -> Tape a
move (Tape (l:ls) x n c rs) Left  = Tape ls l (n-1) c (x:rs)
move (Tape ls x n c (r:rs)) Right = Tape (x:ls) r (n+1) c rs
