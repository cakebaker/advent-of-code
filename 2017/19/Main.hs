import Prelude hiding (Left, Right)
import System.Environment
import Data.Array

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let diagram = createDiagram $ lines content

  let resultPuzzle1 = solve diagram
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


data Direction = Up | Down | Left | Right deriving (Eq)
type Diagram = Array (Int, Int) Char
type Position = (Int, Int)

solve :: Diagram -> String
solve d = move d startPosition Down ""
          where startPosition = findStart d (0,0)

createDiagram :: [String] -> Diagram
createDiagram all@(x:_) = listArray ((0,0), (rows, cols)) . concat $ all
                          where rows = (length all) - 1
                                cols = (length x) - 1

findStart :: Diagram -> Position -> Position
findStart d position@(row, col)
  | d ! position /= ' ' = position
  | otherwise           = findStart d (row , col + 1)

move :: Diagram -> Position -> Direction -> String -> String
move d position@(row, col) dir collection
  | isOutside d position = reverse collection
  | value == ' '         = reverse collection
  | value == '|'         = move d nextPosition dir collection
  | value == '-'         = move d nextPosition dir collection
  | value == '+'         = move d selectedPosition selectedDir collection
  | otherwise            = move d nextPosition dir (value:collection)
  where value                           = d ! position
        nextPosition                    = (nextRow row dir, nextCol col dir)
        (selectedPosition, selectedDir) = whereTo d position dir

nextCol :: Int -> Direction -> Int
nextCol col Left  = col - 1
nextCol col Right = col + 1
nextCol col _     = col

nextRow :: Int -> Direction -> Int
nextRow row Up   = row - 1
nextRow row Down = row + 1
nextRow row _    = row

whereTo :: Diagram -> Position -> Direction -> (Position, Direction)
whereTo d position dir = selectPosition d (candidatePositions position dir)

candidatePositions :: Position -> Direction -> ((Position, Direction), (Position, Direction))
candidatePositions (row, col) dir
  | dir == Up || dir == Down = (((nextRow row Left, nextCol col Left), Left), ((nextRow row Right, nextCol col Right), Right))
  | otherwise                = (((nextRow row Up, nextCol col Up), Up), ((nextRow row Down, nextCol col Down), Down))

selectPosition :: Diagram -> ((Position, Direction), (Position, Direction)) -> (Position, Direction)
selectPosition d (a@(positionA, _), b@(positionB, _))
  | isOutside d positionA && d ! positionB == ' ' = a
  | isOutside d positionB && d ! positionA == ' ' = b
  | isOutside d positionA                         = b
  | isOutside d positionB                         = a
  | d ! positionA == ' '                          = b
  | d ! positionB == ' '                          = a

isOutside :: Diagram -> Position -> Bool
isOutside d (row, col)
  | row < 0 || col < 0           = True
  | row > maxRow || col > maxCol = True
  | otherwise                    = False
  where maxCol = snd $ snd $ bounds d
        maxRow = fst $ snd $ bounds d
