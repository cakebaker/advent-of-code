import KnotHash (hash)
import System.Environment
import Numeric (showIntAtBase)
import Data.Char (intToDigit, digitToInt)
import Data.Array

main :: IO ()
main = do
  let input = "uugsqrei"

  let grid = map hexToBinary $ map (generateHash input) [0..127]

  let resultPuzzle1 = countOnes grid
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = visit (toGridArray grid) [0..127] [0..127] 0
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


type Position = (Int, Int)
type Grid = Array (Int, Int) Int

countOnes :: [String] -> Int
countOnes grid = sum $ map (\s -> length $ filter (== '1') s) grid

generateHash :: String -> Int -> String
generateHash s x = hash (s ++ "-" ++ (show x))

hexToBinary :: String -> String
hexToBinary s = showIntAtBase 2 intToDigit (read hex) ""
                where hex = "0x" ++ s

toGridArray :: [String] -> Grid
toGridArray xs = listArray ((0,0), (127,127)) . concat $ ys
                 where ys = map (\s -> map digitToNegativeInt s) (normalizeTo128Bits xs)

normalizeTo128Bits :: [String] -> [String]
normalizeTo128Bits xs = map (\s -> replicate (128 - (length s)) '0' ++ s) xs

digitToNegativeInt :: Char -> Int
digitToNegativeInt x = 0 - (digitToInt x)

visit :: Grid -> [Int] -> [Int] -> Int -> Int
visit _ [] _ c         = c
visit grid (x:xs) [] c = visit grid xs [0..127] c
visit grid allXs@(x:xs) (y:ys) c
  | value == -1 = visit (markRegion grid [(x,y)] [] (c + 1)) allXs ys (c + 1)
  | otherwise   = visit grid allXs ys c
  where value = grid ! (x,y)

markRegion :: Grid -> [Position] -> [Position] -> Int -> Grid
markRegion grid [] _ _                              = grid
markRegion grid (position:positions) visited marker = markRegion (grid // [(position, marker)]) (positions ++ toVisit) (position:visited) marker
                                                      where toVisit = filter (\pos -> notElem pos visited && notElem pos positions) $ getNeighbors grid position

getNeighbors :: Grid -> Position -> [Position]
getNeighbors grid (x,y) = filter (isRegionNeighbor grid) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isRegionNeighbor :: Grid -> Position -> Bool
isRegionNeighbor grid (x,y)
  | x < 0 || x > 127 = False
  | y < 0 || y > 127 = False
  | value /= -1      = False
  | otherwise        = True
  where value = grid ! (x,y)
