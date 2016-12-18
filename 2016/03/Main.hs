import System.Environment
import Data.List (sort, transpose)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let triangleSides = map toInt $ map words $ lines content
                      where toInt = (\xs -> map read xs)

  let resultPuzzle1 = getNoOfPossibleTriangles $ map sort triangleSides
  putStrLn $ "Puzzle 1, possible triangles: " ++ show resultPuzzle1

  let resultPuzzle2 = getNoOfPossibleTriangles $ map sort $ transformData triangleSides
  putStrLn $ "Puzzle 2, possible triangles: " ++ show resultPuzzle2


getNoOfPossibleTriangles :: [[Int]] -> Int
getNoOfPossibleTriangles triangleSides = length $ filter isTriangle triangleSides


isTriangle :: [Int] -> Bool
isTriangle (x:y:z:[])
    | x + y > z = True
    | otherwise = False


transformData :: [[Int]] -> [[Int]]
transformData xs = groupTriangleSides $ foldr1 (++) $ transpose xs
                   where groupTriangleSides = chunkOf 3


chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = take n xs : (chunkOf n $ drop n xs)
