import System.Environment
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let area = toArea $ lines content

  let resultPuzzle1 = solvePuzzle1 area
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type Position = (Int, Int)
type Area     = Map.Map Position Acre
data Acre     = OpenGround | Tree | Lumberyard deriving Eq

solvePuzzle1 :: Area -> Int
solvePuzzle1 area = lumberyards * trees
                    where lumberyards     = length $ filter (== Lumberyard) acresAfter10Min
                          trees           = length $ filter (== Tree) acresAfter10Min
                          acresAfter10Min = map snd $ Map.toList $ head $ drop 10 $ iterate nextMinute area

areaSize :: Int
areaSize = 49

nextMinute :: Area -> Area
nextMinute area = Map.mapWithKey (\pos v -> transform area pos v) area

transform :: Area -> Position -> Acre -> Acre
transform area pos acre
  | acre == OpenGround = if trees >= 3 then Tree else OpenGround
  | acre == Tree       = if lumberyards >= 3 then Lumberyard else Tree
  | acre == Lumberyard = if lumberyards > 0 && trees > 0 then Lumberyard else OpenGround
  where lumberyards    = length $ filter (== Lumberyard) adjacentValues
        trees          = length $ filter (== Tree) adjacentValues
        adjacentValues = map (\p -> area Map.! p) $ adjacent pos

toAcre :: Char -> Acre
toAcre '.' = OpenGround
toAcre '|' = Tree
toAcre '#' = Lumberyard

toArea :: [String] -> Area
toArea acres = Map.fromList [((x,y), toAcre ((acres !! y) !! x)) | x <- [0..areaSize], y <- [0..areaSize]]

adjacent :: Position -> [Position]
adjacent (x,y) = filter insideArea [(x-1, y), (x-1, y-1), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y), (x+1, y-1), (x+1, y+1)]

insideArea :: Position -> Bool
insideArea (x,y)
  | x < 0 || y < 0                     = False
  | x > areaSize || y > areaSize       = False
  | otherwise                          = True
