import System.Environment
import Data.Array
import Data.List (isPrefixOf)
import Data.Char (isDigit)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map parse $ lines content
  let resultPuzzle1 = getLitPixelCount initialScreen instructions
  putStrLn $ "There are " ++ show resultPuzzle1 ++ " pixels lit"


type Screen = Array (Int, Int) Bool

screenHeight :: Int
screenHeight = 6

screenWidth :: Int
screenWidth = 50

initialScreen :: Screen
initialScreen = array ((0,0), (rows, cols)) [((i, j), False) | i <- [0..rows], j <- [0..cols]]
                where rows = screenHeight - 1
                      cols = screenWidth - 1


getLitPixelCount :: Screen -> [(Screen -> Screen)] -> Int
getLitPixelCount initialScreen instructions = length $ filter id $ elems finalScreen
                                              where finalScreen = foldl (\screen f -> f screen) initialScreen instructions


parse :: String -> (Screen -> Screen)
parse s
  | isPrefixOf "rect" s          = getRectFn s
  | isPrefixOf "rotate row" s    = getRotateFn rotateRow s
  | isPrefixOf "rotate column" s = getRotateFn rotateColumn s


getRectFn :: String -> (Screen -> Screen)
getRectFn s = rect height width
              where width  = read $ takeWhile isDigit $ drop 5 s
                    height = read $ reverse $ takeWhile isDigit $ reverse s


getRotateFn :: (Int -> Int -> Screen -> Screen) -> String -> (Screen -> Screen)
getRotateFn f s = f rowOrCol offset
                  where rowOrCol = read $ drop 2 $ elements !! 2
                        offset   = read $ last elements
                        elements = words s


rect :: Int -> Int -> Screen -> Screen
rect height width sc = sc // [((i, j), True) | i <- [0..(height - 1)], j <- [0..(width - 1)]]


rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn column offset sc = sc // [((i, column), vs !! i) | i <- [0..(screenHeight - 1)]] 
                                where vs   = take screenHeight $ drop (screenHeight - offset) $ foldr1 (++) $ repeat vals
                                      vals = [sc ! (i, column) | i <- [0..(screenHeight - 1)]]


rotateRow :: Int -> Int -> Screen -> Screen
rotateRow row offset sc = sc // [((row, i), vs !! i) | i <- [0..(screenWidth - 1)]]
                          where vs   = take screenWidth $ drop (screenWidth - offset) $ foldr1 (++) $ repeat vals
                                vals = [sc ! (row, i) | i <- [0..(screenWidth - 1)]]
