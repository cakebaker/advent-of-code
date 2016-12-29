import Prelude hiding (Left, Right)
import Data.Hash.MD5
import Data.List (sortBy)
import Data.Ord (comparing)


data Direction = Up | Down | Left | Right

instance Show Direction where
  show Up    = "U"
  show Down  = "D"
  show Left  = "L"
  show Right = "R"


type Position = (Int, Int)


targetPosition :: Position
targetPosition = (3,3)


gridSize :: Int
gridSize = 4


main :: IO ()
main = do
  let passcode = "mmsxrhfx"

  let resultPuzzle1 = findShortestPath passcode (0,0)
  putStrLn $ "The shortest path is: " ++ resultPuzzle1

  let resultPuzzle2 = findLongestPathLength passcode (0,0)
  putStrLn $ "The length of the longest path is: " ++ show resultPuzzle2


findShortestPath :: String -> Position -> String
findShortestPath passcode position = map head $ map show $ head $ findAndSortPaths passcode position


findLongestPathLength :: String -> Position -> Int
findLongestPathLength passcode position = length $ last $ findAndSortPaths passcode position


findAndSortPaths :: String -> Position -> [[Direction]]
findAndSortPaths passcode position = sortBy (comparing length) $ find passcode [] position


find :: String -> [Direction] -> Position -> [[Direction]]
find passcode directions position
  | position == targetPosition = [directions]
  | otherwise = do
                  direction <- getNextDirections passcode directions position
                  find passcode (directions ++ [direction]) (getNewPosition position direction)


getNextDirections :: String -> [Direction] -> Position -> [Direction]
getNextDirections passcode directions position = possibleDirections
                                                 where allDirections = [Up, Down, Left, Right]
                                                       path = map head $ map show directions
                                                       hash = generateHash (passcode ++ path)
                                                       possibleDirections = map (\(dir, _) -> dir) $
                                                                            filter (\(dir, _) -> isValidPosition $ getNewPosition position dir) $
                                                                            filter (\(_, open) -> open) $
                                                                            zipWith (\dir c -> (dir, isDoorOpen c)) allDirections hash


getNewPosition :: Position -> Direction -> Position
getNewPosition (x,y) Up    = (x, y - 1)
getNewPosition (x,y) Down  = (x, y + 1)
getNewPosition (x,y) Left  = (x - 1, y)
getNewPosition (x,y) Right = (x + 1, y)


isDoorOpen :: Char -> Bool
isDoorOpen c
  | elem c "bcdef" = True
  | otherwise      = False


isValidPosition :: Position -> Bool
isValidPosition (x,y)
  | x >= 0 && 
    x < gridSize &&
    y >= 0 && 
    y < gridSize    = True
  | otherwise       = False


generateHash :: String -> String
generateHash s = take 4 $ md5s $ Str s
