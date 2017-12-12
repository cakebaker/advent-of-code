import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let programs = V.fromList $ map (fst . last) $ map (readP_to_S connectedProgramIDs) $ lines content

  let resultPuzzle1 = solve programs
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1

  let resultPuzzle2 = solve2 programs
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


solve :: Vector [Int] -> Int
solve programs = length $ getGroup programs [0] Set.empty

solve2 :: Vector [Int] -> Int
solve2 programs = getGroups programs [0..(V.length programs) - 1] 0 Set.empty

getGroups :: Vector [Int] -> [Int] -> Int -> Set Int -> Int
getGroups v [] counter _    = counter
getGroups v (i:is) counter usedIDs
  | Set.notMember i usedIDs = getGroups v is (counter + 1) (Set.union usedIDs (getGroup v [i] Set.empty))
  | otherwise               = getGroups v is counter usedIDs

getGroup :: Vector [Int] -> [Int] -> Set Int -> Set Int
getGroup v [] connected     = connected
getGroup v (x:xs) connected = getGroup v (updatePipeline xs subGroup connected) totalConnected
                              where subGroup       = v V.! x
                                    totalConnected = Set.union (Set.fromList subGroup) connected

updatePipeline :: [Int] -> [Int] -> Set Int -> [Int]
updatePipeline pipeline [] _ = pipeline
updatePipeline pipeline (x:xs) s
  | Set.notMember x s        = updatePipeline (x:pipeline) xs s
  | otherwise                = updatePipeline pipeline xs s

connectedProgramIDs :: ReadP [Int]
connectedProgramIDs = do
  many1 $ satisfy isDigit
  string " <-> "
  ids <- sepBy (many1 $ satisfy isDigit) (string ", ")
  return (map read ids)
