import System.Environment
import Data.List (minimumBy, transpose)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let imageHeight = 6
  let imageWidth  = 25
  let picture = map toInt $ chunkOf 1 $ head $ lines content
  let layers  = toLayers imageHeight imageWidth picture
  
  let layerFewestZeros = minimumBy zeros layers
  let result1 = (countElement 1 layerFewestZeros) * (countElement 2 layerFewestZeros)
  putStrLn $ "Result of puzzle 1: " ++ show result1

  putStrLn $ "Result of puzzle 2:"
  mapM_ print $ chunkOf imageWidth $ foldl1 (++) $ map colorToString $ map getColor $ transpose layers


chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = take n xs : (chunkOf n $ drop n xs)

colorToString :: Int -> String
colorToString 0 = " "
colorToString x = show x

countElement :: Int -> [Int] -> Int
countElement element xs = length $ filter (\x -> x == element) xs

getColor :: [Int] -> Int
getColor xs = head $ dropWhile isTransparent xs
              where isTransparent = (\x -> x == 2)

toInt :: String -> Int
toInt s = read s

toLayers :: Int -> Int -> [Int] -> [[Int]]
toLayers _ _ []          = []
toLayers height width xs = (take layerSize xs):(toLayers height width (drop layerSize xs))
                           where layerSize = height * width

zeros :: [Int] -> [Int] -> Ordering
zeros xs ys
  | zerosX < zerosY = LT
  | zerosX > zerosY = GT
  | otherwise       = EQ
  where zerosX = countElement 0 xs
        zerosY = countElement 0 ys
