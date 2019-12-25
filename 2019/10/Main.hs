import System.Environment
import Data.List (nub, partition)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let positions = parseAsteroidPositions 0 $ lines content
  
  let result1 = maximum $ countVisibleAsteroids positions positions
  putStrLn $ "Result of puzzle 1: " ++ show result1


type Position = (Int, Int)

asteroid :: Char
asteroid = '#'

countVisibleAsteroids :: [Position] -> [Position] -> [Int]
countVisibleAsteroids [] _ = []
countVisibleAsteroids (pos@(x,y):positions) allPositions = (visibleAsteroids:countVisibleAsteroids positions allPositions)
                                                           where positionsWithoutCurrent     = filter (/= pos) allPositions
                                                                 (onSameAxis, notOnSameAxis) = partition (isOnSameAxis pos) positionsWithoutCurrent
                                                                 visibleAsteroids            = countVisibleOnSameAxis pos onSameAxis + countVisibleNotOnSameAxis pos notOnSameAxis

countVisibleNotOnSameAxis :: Position -> [Position] -> Int
countVisibleNotOnSameAxis pos@(x,y) positions = sum $ map length $ map nub $ map (map (slope pos)) [as, bs, cs, ds]
                                                where (as,bs) = partition ((> y) . snd) $ filter ((> x) . fst) positions
                                                      (cs,ds) = partition ((> x) . fst) $ filter ((<= x) . fst) positions

countVisibleOnSameAxis :: Position -> [Position] -> Int
countVisibleOnSameAxis (x,y) positions = length $ filter (> 0) $ map length [as, bs, cs, ds]
                                         where (as,bs) = partition ((> y) . snd) $ filter ((== x) . fst) positions
                                               (cs,ds) = partition ((> x) . fst) $ filter ((== y) . snd) positions

isOnSameAxis :: Position -> Position -> Bool
isOnSameAxis (x1,y1) (x2,y2)
  | x1 == x2  = True
  | y1 == y2  = True
  | otherwise = False

slope :: Position -> Position -> Float
slope (x1,y1) (x2,y2) = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)

parseAsteroidPositions :: Int ->[String] -> [Position]
parseAsteroidPositions _ []           = []
parseAsteroidPositions y (line:lines) = asteroidPositions ++ parseAsteroidPositions (y + 1) lines
                                        where xs                 = [0..]
                                              ys                 = repeat y
                                              potentialPositions = zip xs ys
                                              asteroidPositions  = map snd $ filter ((== asteroid) . fst) $ zip line potentialPositions
