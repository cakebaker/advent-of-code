import Data.Sequence as Seq
import Data.Foldable (toList)

main :: IO ()
main = do
  let initialRecipes = ((0,1), Seq.fromList [3,7])
  let puzzleInput    = 681901

  let resultPuzzle1 = solvePuzzle1 puzzleInput initialRecipes
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


solvePuzzle1 :: Int -> ((Int, Int), Seq Int) -> String
solvePuzzle1 n recipes = toString $ lastTen $ head $ dropWhile (\(_,xs) -> Seq.length xs < maxRecipes) $ iterate nextRecipes recipes
                         where maxRecipes = n + 10
                               toString   = \x -> foldl1 (++) $ map show $ toList x

nextRecipes :: ((Int, Int), Seq Int) -> ((Int, Int), Seq Int)
nextRecipes ((posA, posB), recipes) = ((newPosA, newPosB), allRecipes)
                                      where valA       = index recipes posA
                                            valB       = index recipes posB
                                            allRecipes = foldl (Seq.|>) recipes (splitIntoDigits (valA + valB))
                                            newPosA    = mod (posA + valA + 1) (Seq.length allRecipes)
                                            newPosB    = mod (posB + valB + 1) (Seq.length allRecipes)

lastTen :: ((a, a), Seq a) -> Seq a
lastTen (_, xs) = snd $ Seq.splitAt (Seq.length xs - 10) xs

splitIntoDigits :: Int -> [Int]
splitIntoDigits x = map (\x -> read [x] :: Int) (show x)
