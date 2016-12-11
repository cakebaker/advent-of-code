import System.Environment
import Data.List

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let triangleSideStrings = map words $ lines content
  let triangleSides = map (\xs -> sort $ map (\x -> read x :: Int) xs) triangleSideStrings
  let result1 = getNoOfPossibleTriangles triangleSides

  putStrLn $ "Possible triangles: " ++ show result1


getNoOfPossibleTriangles :: [[Int]] -> Int
getNoOfPossibleTriangles triangleSides = length $ filter isTriangle triangleSides


isTriangle :: [Int] -> Bool
isTriangle (x:y:z:[])
    | x + y > z = True
    | otherwise = False
