import Data.Sequence as Seq
import Data.Foldable (toList)

main :: IO ()
main = do
  let initialRecipes = ((0,1), Seq.fromList [3,7])
  let puzzleInput    = 681901

  let resultPuzzle1 = solvePuzzle1 puzzleInput initialRecipes
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1

  let resultPuzzle2 = solvePuzzle2 puzzleInput initialRecipes
  putStrLn $ "The result of puzzle 2 is: " ++ show resultPuzzle2


solvePuzzle1 :: Int -> ((Int, Int), Seq Int) -> String
solvePuzzle1 n recipes = toString $ lastN 10 $ snd $ head $ dropWhile (\(_,xs) -> Seq.length xs < maxRecipes) $ iterate nextRecipes recipes
                         where maxRecipes = n + 10
                               toString   = \x -> foldl1 (++) $ map show $ toList x

solvePuzzle2 :: Int -> ((Int, Int), Seq Int) -> Int
solvePuzzle2 n recipes = Seq.length foundRecipes - Seq.length needle - offset
                         where foundRecipes = snd $ head $ dropWhile (\(_,xs) -> not $ containsNearEnd needle xs) $ iterate nextRecipes recipes
                               needle       = Seq.fromList $ splitIntoDigits n
                               offset       = getOffset needle foundRecipes

nextRecipes :: ((Int, Int), Seq Int) -> ((Int, Int), Seq Int)
nextRecipes ((posA, posB), recipes) = ((newPosA, newPosB), allRecipes)
                                      where valA       = index recipes posA
                                            valB       = index recipes posB
                                            allRecipes = foldl (Seq.|>) recipes (splitIntoDigits (valA + valB))
                                            newPosA    = mod (posA + valA + 1) (Seq.length allRecipes)
                                            newPosB    = mod (posB + valB + 1) (Seq.length allRecipes)

containsNearEnd :: Seq Int -> Seq Int -> Bool
containsNearEnd needle haystack
  | needle == patternAtEnd          = True
  | needle == patternAtOneBeforeEnd = True
  | otherwise                       = False
  where haystackLen           = Seq.length haystack
        needleLen             = Seq.length needle
        patternAtEnd          = snd $ Seq.splitAt (haystackLen - needleLen) haystack
        patternAtOneBeforeEnd = Seq.take needleLen $ snd $ Seq.splitAt (haystackLen - needleLen - 1) haystack

getOffset :: Seq Int -> Seq Int -> Int
getOffset needle haystack
  | needle == patternAtEnd = 0
  | otherwise              = 1
  where patternAtEnd = lastN (Seq.length needle) haystack

lastN :: Int -> Seq a -> Seq a
lastN n xs = snd $ Seq.splitAt (Seq.length xs - n) xs

splitIntoDigits :: Int -> [Int]
splitIntoDigits x = map (\x -> read [x] :: Int) (show x)
