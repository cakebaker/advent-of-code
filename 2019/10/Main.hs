import System.Environment
import Data.List (nubBy, sortOn)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let positions = parse $ lines content
  
  let (locationMonitoringStation, result1) = last $ sortOn snd $ countVisibleAsteroids positions positions
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let (x,y)   = head $ drop 199 $ visibleAsteroidsFrom locationMonitoringStation positions
  let result2 = x * 100 + y
  putStrLn $ "Result of puzzle 2: " ++ show result2


type Position = (Int, Int)

asteroid :: Char
asteroid = '#'

countVisibleAsteroids :: [Position] -> [Position] -> [(Position, Int)]
countVisibleAsteroids [] _                              = []
countVisibleAsteroids (position:positions) allPositions = ((position, asteroidCount):countVisibleAsteroids positions allPositions)
                                                          where asteroidCount = length $ visibleAsteroidsFrom position allPositions

visibleAsteroidsFrom :: Position -> [Position] -> [Position]
visibleAsteroidsFrom position@(x,y) allPositions = getVisibleFrom position q1 ++ getVisibleFrom position q2 ++ getVisibleFrom position q3 ++ getVisibleFrom position q4
                                                   where positionsWithoutCurrent = filter (/= position) allPositions
                                                         q1 = filter (\(x1,y1) -> x1 >= x && y1 < y) positionsWithoutCurrent
                                                         q2 = filter (\(x1,y1) -> x1 > x && y1 >= y) positionsWithoutCurrent
                                                         q3 = filter (\(x1,y1) -> x1 <= x && y1 > y) positionsWithoutCurrent
                                                         q4 = filter (\(x1,y1) -> x1 < x && y1 <= y) positionsWithoutCurrent

getVisibleFrom :: Position -> [Position] -> [Position]
getVisibleFrom position positions = map fst $ sortOn snd $ nubBy (\(_,x) (_, y) -> x == y) $ map (\a -> (a, slope position a)) positions

slope :: Position -> Position -> Float
slope (x1,y1) (x2,y2) = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)

parse :: [String] -> [Position]
parse lines = foldr1 (++) $ map parseLine $ zip lines [0..]

parseLine :: (String, Int) -> [Position]
parseLine (line, y) = map snd $ filter ((== asteroid) . fst) $ zip line potentialPositions
                      where xs                 = [0..]
                            ys                 = repeat y
                            potentialPositions = zip xs ys
