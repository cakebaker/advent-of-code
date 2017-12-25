import System.Environment
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.List (delete)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let components = map (fst . last) $ map (readP_to_S component) $ lines content

  let resultPuzzle1 = maximum $ map strength $ buildBridges components components [] 0
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


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

component :: ReadP Component
component = do
  portA <- many1 $ satisfy isDigit
  char '/'
  portB <- many1 $ satisfy isDigit
  return (read portA, read portB)
