import System.Environment
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP
import Data.List (transpose)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let initialPattern = [".#.", "..#", "###"]
  let iterationCount = 5
  let rules = M.fromList $ map (fst . last) $ map (readP_to_S transformationRule) $ lines content

  let resultPuzzle1 = solve rules iterationCount initialPattern
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


type Grid = [String]

solve :: Map [String] [String] -> Int -> Grid -> Int
solve rules n grid = length $ filter (== '#') $ concat $ head $ drop n $ iterate (step rules) grid

step :: Map [String] [String] -> Grid -> Grid
step rules grid = combine $ enhance rules $ divide grid

divide :: Grid -> [Grid]
divide grid
  | isDivisibleBy2 = to2x2 $ map (chunkOf 2) grid
  | otherwise      = to3x3 $ map (chunkOf 3) grid
  where isDivisibleBy2 = (length grid) `mod` 2 == 0

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = (take n xs):(chunkOf n $ drop n xs)

to2x2 :: [Grid] -> [Grid]
to2x2 []       = []
to2x2 (a:b:xs) = (map (\(c,d) -> [c, d]) $ zip a b) ++ (to2x2 xs)

to3x3 :: [Grid] -> [Grid]
to3x3 []         = []
to3x3 (a:b:c:xs) = (map (\(x,y,z) -> [x, y, z]) $ zip3 a b c) ++ (to3x3 xs)

enhance :: Map [String] [String] -> [Grid] -> [Grid]
enhance rules = map (\grid -> enhanceGrid rules grid)

enhanceGrid :: Map [String] [String] -> Grid -> Grid
enhanceGrid rules grid = findEnhancement rules possibleGrids
                         where allRotations  = take 4 $ iterate rotate grid
                               allFlips      = map reverse allRotations
                               possibleGrids = allRotations ++ allFlips

findEnhancement :: Map [String] [String] -> [Grid] -> Grid
findEnhancement rules (grid:grids) = case rules M.!? grid of
                                       Just enhancedGrid -> enhancedGrid
                                       Nothing           -> findEnhancement rules grids

rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

combine :: [Grid] -> Grid
combine []        = []
combine xs@(_:[]) = concat xs
combine (a:b:xs)  = (map (\(c,d) -> c ++ d) $ zip a b) ++ combine xs

transformationRule :: ReadP ([String], [String])
transformationRule = do
  fromPattern <- pattern
  string " => "
  toPattern <- pattern
  return (fromPattern, toPattern)

pattern :: ReadP [String]
pattern = do
  elems <- sepBy (many1 $ satisfy (\c -> c == '.' || c == '#')) (char '/') 
  return elems
