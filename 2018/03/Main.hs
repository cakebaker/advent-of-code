import System.Environment
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let claims = map (fst . last) $ map (readP_to_S claim) $ lines content

  let resultPuzzle1 = countMultiClaimed claims
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


countMultiClaimed :: [Claim] -> Int
countMultiClaimed claims = length $ filter (/= 1) $ Map.elems $ process Map.empty claims

process :: ClaimedPositions -> [Claim] -> ClaimedPositions
process m []     = m
process m (x:xs) = process (processClaim m x) xs

processClaim :: ClaimedPositions -> Claim -> ClaimedPositions
processClaim m ((x,y), w, h) = claimPosition m claimed
                               where claimed = [(a,b) | a <- [x..(x+w-1)], b <- [y..(y+h-1)]]

claimPosition :: ClaimedPositions -> [Position] -> ClaimedPositions
claimPosition m []     = m
claimPosition m (x:xs) = claimPosition (Map.insertWith (+) x 1 m) xs


type ClaimedPositions = Map.Map Position Int
type Claim            = (Position, Width, Height)
type Position         = (Int, Int)
type Width            = Int
type Height           = Int

-- parsing

claim :: ReadP Claim
claim = do
  skipMany $ satisfy (/= '@')
  string "@ "
  pos <- position
  string ": "
  w <- number
  char 'x'
  h <- number
  return (pos, w, h)

position :: ReadP Position
position = do
  x <- number
  char ','
  y <- number
  return (x,y)

number :: ReadP Int
number = do
  x <- many1 $ satisfy isDigit
  return (read x)
