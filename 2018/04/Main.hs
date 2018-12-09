import System.Environment
import Data.Char (isDigit)
import Data.List (group, sort)
import Control.Applicative
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let sortedRecords = sort $ map (fst . last) $ map (readP_to_S record) $ lines content

  let resultPuzzle1 = solvePuzzle1 sortedRecords
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type Record    = (Timestamp, Event)
data Timestamp = Timestamp Int Int Int Int Int deriving (Show, Eq, Ord)
data Event     = Asleep | Wakeup | Guard Int deriving (Show, Eq, Ord)

solvePuzzle1 :: [Record] -> Int
solvePuzzle1 records = sleepiestGuard * mostAsleepMin
                       where sleepiestGuard = findSleepiestGuard sleepingTimes
                             mostAsleepMin  = findMostAsleepMinute sleepingTimes sleepiestGuard
                             sleepingTimes  = recordsToSleepingTimes Map.empty records 0

recordsToSleepingTimes :: Map.Map Int [Int] -> [Record] -> Int -> Map.Map Int [Int]
recordsToSleepingTimes m [] _                = m
recordsToSleepingTimes m ((_, Guard g):xs) _ = recordsToSleepingTimes m xs g
recordsToSleepingTimes m (((Timestamp _ _ _ _ asleepMin), Asleep):((Timestamp _ _ _ _ wakeupMin), Wakeup):xs) guard = 
                                               recordsToSleepingTimes newMap xs guard
                                               where newMap = Map.insertWith (++) guard [asleepMin..wakeupMin - 1] m

findSleepiestGuard :: Map.Map Int [Int] -> Int
findSleepiestGuard m = fst $ foldl1 (\(a,b) (c,d) -> if length b > length d then (a,b) else (c,d)) $ Map.toList m

findMostAsleepMinute :: Map.Map Int [Int] -> Int -> Int
findMostAsleepMinute sleepingTimes guard = head $ foldl1 (\a b -> if length a > length b then a else b) $ group $ sort $ (sleepingTimes Map.! guard)

-- parsing

record :: ReadP Record
record = do
  ts <- between (char '[') (char ']') timestamp
  char ' '
  ev <- event
  return (ts, ev)

timestamp :: ReadP Timestamp
timestamp = do
  year <- number
  char '-'
  month <- number
  char '-'
  day <- number
  char ' '
  hour <- number
  char ':'
  minute <- number
  return (Timestamp year month day hour minute)

event :: ReadP Event
event = do
  ev <- asleep <|> wakeup <|> guard
  return ev
  
guard :: ReadP Event
guard = do
  string "Guard #"
  i <- number
  return (Guard i)

asleep :: ReadP Event
asleep = do
  string "falls asleep"
  return Asleep

wakeup :: ReadP Event
wakeup = do
  string "wakes up"
  return Wakeup

number :: ReadP Int
number = do
  x <- many1 $ satisfy isDigit
  return (read x)
