import Prelude hiding (Left, Right)
import System.Environment
import Data.List (isPrefixOf)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let operations = map parse $ lines content
  let startValue = "abcdefgh"

  let resultPuzzle1 = execute operations startValue
  putStrLn $ "The scrambled password is: " ++ resultPuzzle1


data Direction = Left | Right


parse :: String -> (String -> String)
parse s
  | isPrefixOf "move position" s     = movePosition (read $ elements !! 2) (read $ last elements)
  | isPrefixOf "rotate right" s      = rotate Right (read $ elements !! 2)
  | isPrefixOf "rotate left" s       = rotate Left (read $ elements !! 2)
  | isPrefixOf "rotate based" s      = rotateOnPosition $ head $ last elements
  | isPrefixOf "swap letter" s       = swapLetter (head $ elements !! 2) (head $ last elements)
  | isPrefixOf "swap position" s     = swapPosition (read $ elements !! 2) (read $ last elements)
  | isPrefixOf "reverse positions" s = reversePositions (read $ elements !! 2) (read $ last elements)
  where elements = words s


execute :: [(String -> String)] -> String -> String
execute [] s     = s
execute (f:fs) s = execute fs (f s)


swapPosition :: Int -> Int -> String -> String
swapPosition posX posY s = foldr swap [] $ zip [0..] s
                           where x = s !! posX
                                 y = s !! posY
                                 swap (i, c) acc
                                   | i == posX = y : acc
                                   | i == posY = x : acc
                                   | otherwise = c : acc


swapLetter :: Char -> Char -> String ->String
swapLetter x y s = foldr swap [] s
                   where swap c acc
                           | c == x    = y : acc
                           | c == y    = x : acc
                           | otherwise = c : acc


movePosition :: Int -> Int -> String -> String
movePosition oldPos newPos s = insertAtIndex newPos value $ removeAtIndex oldPos s
                               where value = s !! oldPos


insertAtIndex :: Int -> Char -> String -> String
insertAtIndex index c s = take index s ++ [c] ++ drop index s


removeAtIndex :: Int -> String -> String
removeAtIndex index s = take index s ++ drop (index + 1) s


reversePositions :: Int -> Int -> String -> String
reversePositions startPos endPos s = front ++ reverse middle ++ back
                                     where front = take startPos s
                                           back = drop (succ endPos) s
                                           middle = take ((succ endPos) - startPos) $ drop startPos s


rotate :: Direction -> Int -> String -> String
rotate Left steps s  = take len $ drop steps $ cycle s
                       where len = length s
rotate Right steps s = take len $ drop (len - (mod steps len)) $ cycle s
                       where len = length s


rotateOnPosition :: Char -> String -> String
rotateOnPosition c s
  | index >= 4 = rotate Right (1 + index + 1) s
  | otherwise  = rotate Right (1 + index) s
  where index = elemIndex s c 0


elemIndex :: String -> Char -> Int -> Int
elemIndex (x:xs) c index
  | x == c    = index
  | otherwise = elemIndex xs c (succ index)
