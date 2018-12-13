import System.Environment
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let (initialState, notes) = parse $ lines content

  let resultPuzzle1 = solvePuzzle1 notes initialState
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type PuzzleInput = (String, NotesMap)
type Note        = (String, Char)
type NotesMap    = Map.Map String Char

solvePuzzle1 :: NotesMap -> [Char] -> Int
solvePuzzle1 notes initialState = sum $ map snd $ filter isPlant generation21
                                  where initialStateWithPos = zip initialState [0..]
                                        generation21        = head $ drop 20 $ iterate (nextGeneration notes) initialStateWithPos
                                        isPlant             = (\(a,_) -> a == '#')

nextGeneration :: NotesMap -> [(Char, Int)] -> [(Char, Int)]
nextGeneration notes currentState = generateNextGeneration notes (pad currentState) []

generateNextGeneration :: NotesMap -> [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
generateNextGeneration notes all@(_:xs) acc
  | length key == keyLength = generateNextGeneration notes xs ((notes Map.! key, pos):acc)
  | otherwise               = reverse acc
  where keyLength = 5
        key       = map fst $ take keyLength all
        pos       = snd $ head $ drop 2 all


pad :: [(Char, Int)] -> [(Char, Int)]
pad all@((_,pos):xs) = zip padded [(pos - (length padStr))..]
                       where padded = padStr ++ (map fst all) ++ padStr
                             padStr = "....."


-- parsing

parse :: [String] -> PuzzleInput
parse (initialState:_:notes) = (state, Map.fromList $ parseNotes notes)
                               where state = drop (length "initial state: ") initialState

parseNotes :: [String] -> [Note]
parseNotes [] = []
parseNotes (x:xs) = (pattern, hasPlant):parseNotes xs
                    where pattern  = take 5 x
                          hasPlant = last x
