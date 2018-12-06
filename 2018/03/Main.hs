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

  let resultPuzzle2 = idOfNonOverlappingClaim claims
  putStrLn $ "Result of puzzle 2: " ++ show resultPuzzle2


type ClaimedPositions = Map.Map Position Int
type Claim            = (Int, Position, Width, Height)
type Position         = (Int, Int)
type Width            = Int
type Height           = Int

-- puzzle 1

countMultiClaimed :: [Claim] -> Int
countMultiClaimed claims = length $ filter (/= 1) $ Map.elems $ process Map.empty claims

process :: ClaimedPositions -> [Claim] -> ClaimedPositions
process m []     = m
process m (x:xs) = process (processClaim m x) xs

processClaim :: ClaimedPositions -> Claim -> ClaimedPositions
processClaim m (_, (x,y), w, h) = claimPosition m claimed
                                  where claimed = [(a,b) | a <- [x..(x+w-1)], b <- [y..(y+h-1)]]

claimPosition :: ClaimedPositions -> [Position] -> ClaimedPositions
claimPosition m []     = m
claimPosition m (x:xs) = claimPosition (Map.insertWith (+) x 1 m) xs

-- puzzle 2

idOfNonOverlappingClaim :: [Claim] -> Int
idOfNonOverlappingClaim claims = id
                                 where (id,_,_,_)        = head $ filter (\(id,_,_,_) -> notElem id $ overlappedClaims claimCombinations) claims
                                       claimCombinations = [(x,y) | x <- claims, y <- claims, x < y]

overlappedClaims :: [(Claim, Claim)] -> [Int]
overlappedClaims [] = []
overlappedClaims ((claimA@(idA, _, _, _), claimB@(idB, _, _, _)):xs)
  | overlapped claimA claimB = idA:idB:overlappedClaims xs
  | otherwise                = overlappedClaims xs

overlapped :: Claim -> Claim -> Bool
overlapped (_, (x1,y1), w1, h1) (_, (x2,y2), w2, h2)
  | x2 > (x1 + w1 - 1) || (x2 + w2 - 1) < x1 = False
  | y2 > (y1 + h1 - 1) || (y2 + h2 - 1) < y1 = False
  | otherwise                                = True

-- parsing

claim :: ReadP Claim
claim = do
  char '#'
  id <- number
  skipMany $ satisfy (/= '@')
  string "@ "
  pos <- position
  string ": "
  w <- number
  char 'x'
  h <- number
  return (id, pos, w, h)

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
