import System.Environment
import Data.Char (isUpper, toLower, toUpper)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let units = head $ lines content

  let resultPuzzle1 = lengthOfReactedPolymer units
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  -- XXX very slow
  let resultPuzzle2 = minLengthOfReactedPolymers units ['a'..'z']
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


minLengthOfReactedPolymers :: [Char] -> [Char] -> Int
minLengthOfReactedPolymers units unitTypes = minimum $ map (\x -> lengthOfReactedPolymer $ removeUnitType x units) $ unitTypes

removeUnitType :: Char -> [Char] -> [Char]
removeUnitType c xs = filter (\x -> x /= c && x /= toOppositePolarity c) xs

lengthOfReactedPolymer :: [Char] -> Int
lengthOfReactedPolymer xs = length $ process xs

process :: [Char] -> [Char]
process [] = []
process xs
  | hasOppositePolarities xs = process $ removeOppositePolarities xs
  | otherwise                = xs

hasOppositePolarities :: [Char] -> Bool
hasOppositePolarities []     = False
hasOppositePolarities (a:[]) = False
hasOppositePolarities (a:b:xs)
  | isOppositePolarity a b = True
  | otherwise              = hasOppositePolarities (b:xs)

removeOppositePolarities :: [Char] -> [Char]
removeOppositePolarities []     = []
removeOppositePolarities (a:[]) = [a]
removeOppositePolarities (a:b:xs)
  | isOppositePolarity a b = removeOppositePolarities xs
  | otherwise              = a:removeOppositePolarities (b:xs)

isOppositePolarity :: Char -> Char -> Bool
isOppositePolarity c1 c2 = toOppositePolarity c1 == c2

toOppositePolarity :: Char -> Char
toOppositePolarity c
  | isUpper c = toLower c
  | otherwise = toUpper c
