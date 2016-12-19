import Prelude hiding (drop, length, splitAt)
import Data.Sequence


main :: IO ()
main = do
  let elves = 3004953
  let resultPuzzle2 = getElveWithAllPresents $ fromList [(i, 1) | i <- [1..elves]]
  putStrLn $ "The elve with all presents is: " ++ show resultPuzzle2


type ElveWithPresents = (Int, Int)


getElveWithAllPresents :: Seq ElveWithPresents -> Int
getElveWithAllPresents xs
  | (length xs) == 1 = fst $ index xs 0
  | otherwise        = getElveWithAllPresents $ takePresents $ splitAt i xs
  where i = div (length xs) 2


takePresents :: (Seq ElveWithPresents, Seq ElveWithPresents) -> Seq ElveWithPresents
takePresents (xs, ys) = drop 1 xs >< (drop 1 $ ys |> elve)
                        where elve      = (fst $ index xs 0, presentsX + presentsY)
                              presentsX = snd $ index xs 0
                              presentsY = snd $ index ys 0
