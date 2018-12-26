import System.Environment
import Data.Char (isDigit)
import Data.List (partition)
import Control.Applicative
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let points = map (fst . last) $ map (readP_to_S point) $ lines content

  let resultPuzzle1 = length $ findConstellations points []
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type Constellation = [Point]
type Point         = (Int, Int, Int, Int)

findConstellations :: [Point] -> [Constellation] -> [Constellation]
findConstellations [] constellations     = constellations
findConstellations (x:xs) constellations = findConstellations xs (process x constellations)

process :: Point -> [Constellation] -> [Constellation]
process pt constellations
  | memberships == 0  = ([pt]:constellations)
  | memberships == 1  = addToConstellation pt (head memberShipIDs) constellations
  | otherwise         = addToMergedConstellation pt memberShipIDs constellations
  where memberShipIDs = map fst $ filter (\(_,b) -> b) $ zip [0..] $ map (isInConstellation pt) constellations
        memberships   = length memberShipIDs

addToConstellation :: Point -> Int -> [Constellation] -> [Constellation]
addToConstellation pt x constellations = concat [first, [updated], (tail second)]
                                         where (first, second) = splitAt x constellations
                                               updated         = (pt:(head second))

addToMergedConstellation :: Point -> [Int] -> [Constellation] -> [Constellation]
addToMergedConstellation pt xs constellations = concat [merged, noMerge]
                                                where (toMerge, noMerge) = partition (\constellation -> elem constellation (map (constellations !!) xs)) constellations
                                                      merged             = [(pt:(foldl1 (++) toMerge))]

isInConstellation :: Point -> Constellation -> Bool
isInConstellation _ []       = False
isInConstellation pt (x:xs)
  | inSameConstellation pt x = True
  | otherwise                = isInConstellation pt xs

inSameConstellation :: Point -> Point -> Bool
inSameConstellation p1 p2 = (manhattanDistance p1 p2) < 4

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1,y1,z1,t1) (x2,y2,z2,t2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (t1 - t2)

-- parsing

point :: ReadP Point
point = do
  skipSpaces
  (x:y:z:t:[]) <- sepBy1 number (char ',')
  return (x, y, z, t)

number :: ReadP Int
number = do
  x <- negativeNumber <|> positiveNumber
  return x

positiveNumber :: ReadP Int
positiveNumber = do
  x <- many1 $ satisfy (isDigit)
  return (read x)

negativeNumber :: ReadP Int
negativeNumber = do
  char '-'
  digits <- many1 $ satisfy (isDigit)
  return (read ("-" ++ digits))
