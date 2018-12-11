import Data.Array

main :: IO ()
main = do
  let serialNumber = 3999
  let dimension    = 300

  let resultPuzzle1 = findLargestPower3x3 $ createGrid serialNumber dimension
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


createGrid :: Int -> Int -> Array (Int, Int) Int
createGrid serialNumber dimension = array ((1,1), (dimension,dimension)) [((x,y), calculatePowerLevel x y serialNumber) | x <- [1..dimension], y <- [1..dimension]]

calculatePowerLevel :: Int -> Int -> Int -> Int
calculatePowerLevel x y serialNumber = powerLevel
                                       where rackID     = x + 10
                                             powerLevel = hundredsDigit (((rackID * y) + serialNumber) * rackID) - 5

findLargestPower3x3 :: Array (Int, Int) Int -> (Int, Int)
findLargestPower3x3 grid = fst $ foldl1 (\((x1,y1), v1) ((x2,y2), v2) -> if v1 > v2 then ((x1,y1), v1) else ((x2,y2), v2)) $ powerByCoordinate
                           where powerByCoordinate = [((x,y), calculatePower3x3 grid x y) | x <- [1..(dimension-2)], y <- [1..(dimension-2)]]
                                 dimension         = fst $ snd $ bounds grid  

calculatePower3x3 :: Array (Int, Int) Int -> Int -> Int -> Int
calculatePower3x3 grid x y = sum $ [grid ! (a,b) | a <- [x..(x+2)], b <- [y..(y+2)]] 

hundredsDigit :: Int -> Int
hundredsDigit x = div ((mod x 1000) - (mod x 100)) 100
