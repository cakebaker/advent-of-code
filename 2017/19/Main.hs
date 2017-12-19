import Prelude hiding (Left, Right)
import System.Environment
import Data.Array
import Data.Char (isAlpha)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let diagram = createDiagram $ lines content

  let resultPuzzle1 = solve diagram charCollector
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = length $ solve diagram stepCollector
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


data Direction = Up | Down | Left | Right deriving (Eq)
type Diagram = Array (Int, Int) Char
type Position = (Int, Int)

solve :: Diagram -> (Char -> String -> String) -> String
solve d collector = walk collector d (startPosition, Down) ""
                    where startPosition = findStart d (0,0)

createDiagram :: [String] -> Diagram
createDiagram all@(x:_) = listArray ((0,0), (rows, cols)) . concat $ all
                          where rows = (length all) - 1
                                cols = (length x) - 1

findStart :: Diagram -> Position -> Position
findStart d position@(row, col)
  | d ! position /= ' ' = position
  | otherwise           = findStart d (row , col + 1)

stepCollector :: Char -> String -> String
stepCollector c s = (c:s)

charCollector :: Char -> String -> String
charCollector c s
  | isAlpha c = s ++ [c]
  | otherwise = s

walk :: (Char -> String -> String) -> Diagram -> (Position, Direction) -> String -> String
walk f d (position, dir) collection
  | isOutside d position || value == ' ' = collection
  | otherwise                            = walk f d (step d position dir) (f value collection)
  where value = d ! position

step :: Diagram -> Position -> Direction -> (Position, Direction)
step d position@(row, col) dir
  | value == '+'         = (selectedPosition, selectedDir)
  | otherwise            = (nextPosition, dir)
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
