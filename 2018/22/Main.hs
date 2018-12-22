import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let resultPuzzle1 = solvePuzzle1
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


data RegionType = Rocky | Wet | Narrow deriving Show
type Coordinate = (Int, Int)
type GeologicalIndexMap = Map.Map Coordinate Int

depth :: Int
depth = 4848

target :: Coordinate
target = (15, 700)

caveMouth :: Coordinate
caveMouth = (0,0)

grid :: [Coordinate]
grid = [(x,y) | x <- [(fst caveMouth)..(fst target)], y <- [(snd caveMouth)..(snd target)]]

solvePuzzle1 :: Int
solvePuzzle1 = sum $ map (riskLevel . regiontype . erosionLevel . snd) $ Map.toList $ geologicalIndexes Map.empty grid

geologicalIndexes :: GeologicalIndexMap -> [Coordinate] -> GeologicalIndexMap
geologicalIndexes m []     = m
geologicalIndexes m (x:xs) = geologicalIndexes (Map.insert x (geologicalIndex m x) m) xs

geologicalIndex :: GeologicalIndexMap -> Coordinate -> Int
geologicalIndex m pos@(x,y)
  | pos == caveMouth  = 0
  | pos == target     = 0
  | y == 0            = x * 16807
  | x == 0            = y * 48271
  | otherwise         = erosionLevelA * erosionLevelB
  where erosionLevelA = erosionLevel $ m Map.! (x-1,y)
        erosionLevelB = erosionLevel $ m Map.! (x,y-1)

erosionLevel :: Int -> Int
erosionLevel geologicalIndex = mod (geologicalIndex + depth) 20183

regiontype :: Int -> RegionType
regiontype erosionLevel
  | x == 0    = Rocky
  | x == 1    = Wet
  | otherwise = Narrow
  where x = mod erosionLevel 3

riskLevel :: RegionType -> Int
riskLevel Rocky  = 0
riskLevel Wet    = 1
riskLevel Narrow = 2
