
main :: IO ()
main = do
  let resultPuzzle1 = fillBuffer [1..2017] 0 [0]
  putStrLn $ "Result of puzzle 1: " ++ show resultPuzzle1


step :: Int
step = 371

fillBuffer :: [Int] -> Int -> [Int] -> Int
fillBuffer [] current buffer     = buffer !! current
fillBuffer (x:xs) current buffer = fillBuffer xs next updatedBuffer
                                   where at            = (current + step) `mod` (length buffer)
                                         (a, b)        = splitAt at buffer
                                         updatedBuffer = a ++ [x] ++ b
                                         next          = at + 1
