import System.Environment
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let memoryBanks = V.fromList $ map (\s -> read s :: Int) $ words $ head $ lines content

  let resultPuzzle1 = startRedistributionCycles memoryBanks
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


startRedistributionCycles :: V.Vector Int -> Int
startRedistributionCycles v = doRedistributionCycle v []


doRedistributionCycle :: V.Vector Int -> [V.Vector Int] -> Int
doRedistributionCycle v history
  | elem v history = length history
  | otherwise      = doRedistributionCycle (redistribute v) (v:history)


redistribute :: V.Vector Int -> V.Vector Int
redistribute v = reallocate v' (i + 1) blocksToDistribute
                 where i                  = V.maxIndex v
                       blocksToDistribute = v V.! i
                       v'                 = v V.// [(i, 0)]


reallocate :: V.Vector Int -> Int -> Int -> V.Vector Int
reallocate v _ 0      = v
reallocate v i blocks = reallocate v' (j + 1) (blocks - 1)
                        where j  = i `mod` (V.length v)
                              v' = v V.// [(j, (v V.! j) + 1)]
