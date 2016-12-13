import System.Environment
import Data.Array


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let instructions = map (\xs -> map parse xs) $ lines content
  let resultPuzzle1 = getBathroomCode keypad (2,2) instructions

  putStrLn $ "Bathroom code is: " ++ resultPuzzle1


type Position = (Int, Int)
type Keypad = Array (Int, Int) Char


keypad :: Keypad
keypad = array ((1,1), (3,3)) [((1,1), '1'), ((2,1), '2'), ((3,1), '3'),
                               ((1,2), '4'), ((2,2), '5'), ((3,2), '6'),
                               ((1,3), '7'), ((2,3), '8'), ((3,3), '9')]


parse :: Char -> Keypad -> Position -> Position
parse 'U' = move   0 (-1)
parse 'D' = move   0   1
parse 'L' = move (-1)  0
parse 'R' = move   1   0


getBathroomCode :: Keypad -> Position -> [[Keypad -> Position -> Position]] -> String
getBathroomCode k p instructions = map (\x -> k ! x) $ tail $ scanl (execute k) p instructions


execute :: Keypad -> Position -> [(Keypad -> Position -> Position)] -> Position
execute _ position [] = position
execute k position (f:fs) = execute k (f k position) fs


move :: Int -> Int -> Keypad -> Position -> Position
move xOffset yOffset k currentPosition@(x,y)
  | isInBounds k newPosition = newPosition
  | otherwise                = currentPosition
  where newPosition = (x + xOffset, y + yOffset)


isInBounds :: Keypad -> Position -> Bool
isInBounds k (x,y)
  | x < minBound || x > maxBound = False
  | y < minBound || y > maxBound = False
  | otherwise                    = True
  -- assumption: keypads are squares
  where minBound = fst $ fst $ bounds k
        maxBound = snd $ snd $ bounds k
