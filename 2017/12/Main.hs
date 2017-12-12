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

  let programs = V.fromList $ map (fst . last) $ map (readP_to_S program) $ lines content

  let resultPuzzle1 = solve programs
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


solve :: Vector [Int] -> Int
solve programs = length $ getGroup programs [0] Set.empty

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

program :: ReadP [Int]
program = do
  id <- programID
  string " <-> "
  ids <- connectedProgramIDs
  return ids

programID :: ReadP Int
programID = do
  id <- many1 $ satisfy isDigit
  return (read id)

connectedProgramIDs :: ReadP [Int]
connectedProgramIDs = do
  ids <- sepBy (many1 $ satisfy isDigit) (string ", ")
  return (map read ids)
