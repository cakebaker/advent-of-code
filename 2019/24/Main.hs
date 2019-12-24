import System.Environment
import Data.Array

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let grid = createGrid $ map parse $ lines content
  
  let result1 = bioDiversityRating $ runMinutesUntilRepetition grid []
  putStrLn $ "Result of puzzle 1: " ++ show result1


data Tile = Bug | Space deriving (Eq, Show)

type Grid     = Array (Int, Int) Tile
type Position = (Int, Int)

maxGridIndex :: Int
maxGridIndex = 4

bioDiversityRating :: Grid -> Int
bioDiversityRating grid = sum $ map ((2^) . fst) $ filter ((== Bug) . snd) $ indexedTiles
                          where indexedTiles = zipWith (\(_,tile) i -> (i, tile)) (assocs grid) [0..]

runMinutesUntilRepetition :: Grid -> [Grid] -> Grid
runMinutesUntilRepetition grid history
  | elem grid history = grid
  | otherwise         = runMinutesUntilRepetition newGrid (grid:history)
  where newGrid = nextMinute grid

nextMinute :: Grid -> Grid
nextMinute grid = array ((0,0), (maxGridIndex,maxGridIndex)) [((i,j), nextTile grid (i,j)) | i <- [0..maxGridIndex], j <- [0..maxGridIndex]]

nextTile :: Grid -> Position -> Tile
nextTile grid position
  | currentTile == Bug && bugCount /= 1                      = Space
  | currentTile == Space && (bugCount == 1 || bugCount == 2) = Bug
  | otherwise                                                = currentTile
  where currentTile = grid ! position
        bugCount    = adjacentBugCount grid position

adjacentBugCount :: Grid -> Position -> Int
adjacentBugCount grid position@(x,y) = length $ filter (== Bug) $ map (grid !) $ positions
                                       where positions = filter isValidPosition $ adjacentPositions position

isValidPosition :: Position -> Bool
isValidPosition (x,y)
  | x < 0 || y < 0                       = False
  | x > maxGridIndex || y > maxGridIndex = False
  | otherwise                            = True

adjacentPositions :: Position -> [Position]
adjacentPositions (x,y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

createGrid :: [[Tile]] -> Grid
createGrid all@(x:_) = listArray ((0,0), (rows, cols)) . concat $ all
                       where rows = (length all) - 1
                             cols = (length x) - 1

parse :: String -> [Tile]
parse = map parseChar

parseChar :: Char -> Tile
parseChar '#' = Bug
parseChar '.' = Space
