import System.Environment
import Data.List (nub, nubBy, partition, sortOn)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let positions = parseAsteroidPositions 0 $ lines content
  
  let (locationMonitoringStation, result1) = last $ sortOn snd $ countVisibleAsteroids positions positions
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let (x,y)   = head $ drop 199 $ visibleAsteroidsFrom locationMonitoringStation positions
  let result2 = x * 100 + y
  putStrLn $ "Result of puzzle 2: " ++ show result2


type Position = (Int, Int)

asteroid :: Char
asteroid = '#'

visibleAsteroidsFrom :: Position -> [Position] -> [Position]
visibleAsteroidsFrom position@(x,y) allPositions = getVisibleFrom position q1 ++ getVisibleFrom position q2 ++ getVisibleFrom position q3 ++ getVisibleFrom position q4
                                                   where positionsWithoutCurrent = filter (/= position) allPositions
                                                         q1 = filter (\(x1,y1) -> x1 >= x && y1 < y) positionsWithoutCurrent
                                                         q2 = filter (\(x1,y1) -> x1 > x && y1 >= y) positionsWithoutCurrent
                                                         q3 = filter (\(x1,y1) -> x1 <= x && y1 > y) positionsWithoutCurrent
                                                         q4 = filter (\(x1,y1) -> x1 < x && y1 <= y) positionsWithoutCurrent

getVisibleFrom :: Position -> [Position] -> [Position]
getVisibleFrom position positions = map fst $ sortOn snd $ nubBy (\(_,x) (_, y) -> x == y) $ map (\a -> (a, slope position a)) positions

countVisibleAsteroids :: [Position] -> [Position] -> [(Position, Int)]
countVisibleAsteroids [] _ = []
countVisibleAsteroids (pos@(x,y):positions) allPositions = ((pos, visibleAsteroids):countVisibleAsteroids positions allPositions)
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
