import System.Environment
import Data.Char (isDigit)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let compressedText = head $ lines content
  let resultPuzzle1 = length $ decompress compressedText
  putStrLn $ "The decompressed length is: " ++ show resultPuzzle1


decompress :: String -> String
decompress [] = []
decompress s
  | not $ elem '(' s = s
  | otherwise        = decompressed ++ decompressedSubsequence ++ (decompress $ drop len s)
                       where decompressed              = takeWhile (\x -> not $ elem x "(") s
                             marker                    = takeWhileAndOne (\x -> not $ elem x ")") $ drop (length decompressed) s
                             (subsequentChars, ntimes) = parseMarker marker
                             subsequence               = take subsequentChars $ drop (length decompressed + length marker) s
                             decompressedSubsequence   = foldr1 (++) $ replicate ntimes subsequence
                             len                       = length decompressed + length marker + length subsequence


parseMarker :: String -> (Int, Int)
parseMarker s = (subsequentChars, ntimes)
                where subsequentChars = read $ takeWhile isDigit $ tail s
                      ntimes          = read $ reverse $ takeWhile isDigit $ tail $ reverse s


takeWhileAndOne :: (a -> Bool) -> [a] -> [a]
takeWhileAndOne _ [] = []
takeWhileAndOne p (x:xs)
  | p x       = x : takeWhileAndOne p xs
  | otherwise = x : []
