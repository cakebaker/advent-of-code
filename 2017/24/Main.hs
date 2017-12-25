import System.Environment
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.List (delete, maximumBy)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let components = map (fst . last) $ map (readP_to_S component) $ lines content
  let bridges = buildBridges components components [] 0

  let resultPuzzle1 = maximum $ map strength bridges
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = fst $ maximumBy lengthStrength $ map (\x -> (strength x, length x)) bridges
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


type Component = (Int, Int)

buildBridges :: [Component] -> [Component] -> [Component] -> Int -> [[Component]]
buildBridges [] _ path _   = [reverse path]
buildBridges all [] path _ = [reverse path]
buildBridges all (component@(a,b):xs) path z
  | a == z    = (buildBridges withoutComponent withoutComponent (component:path) b) ++ (buildBridges all xs path z)
  | b == z    = (buildBridges withoutComponent withoutComponent (component:path) a) ++ (buildBridges all xs path z)
  | otherwise = buildBridges all xs path z
  where withoutComponent = delete component all

strength :: [Component] -> Int
strength xs = foldl (\acc (a,b) -> acc + a + b) 0 xs

lengthStrength :: (Int, Int) -> (Int, Int) -> Ordering
lengthStrength (a1, b1) (a2, b2)
  | b1 < b2   = LT
  | b1 > b2   = GT
  | otherwise = compare a1 a2

component :: ReadP Component
component = do
  portA <- many1 $ satisfy isDigit
  char '/'
  portB <- many1 $ satisfy isDigit
  return (read portA, read portB)
