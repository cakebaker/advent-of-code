import Data.Hash.MD5
import Data.List (isInfixOf)


main :: IO ()
main = do
  let salt = "qzyelonm"
  let resultPuzzle1 = get64thKey salt

  putStrLn $ "64th key: " ++ show resultPuzzle1


type IndexedHash = (String, Int)
type HashGenerator = (Int -> [IndexedHash])


get64thKey :: String -> Int
get64thKey salt = snd $ head $ drop 63 $ filter (isKey hashGenerator) $ hashGenerator 0
                  where hashGenerator = generateHashes salt


generateHashes :: String -> Int -> [IndexedHash]
generateHashes salt index = map (\x -> (md5s (Str $ salt ++ show x), x)) [index,(index + 1)..]


isKey :: HashGenerator -> IndexedHash -> Bool
isKey hashGenerator (hash, index)
  | hasTriple hash && hasQuinsInNext1kHashes hashGenerator (index + 1) (getFirstTripleChar hash) = True
  | otherwise = False


hasQuinsInNext1kHashes :: HashGenerator -> Int -> Char -> Bool
hasQuinsInNext1kHashes hashGenerator index c = any (\xs -> isInfixOf quins (fst xs)) $ take 1000 $ hashGenerator index
                                               where quins = replicate 5 c


hasTriple :: String -> Bool
hasTriple []         = False
hasTriple (_:[])     = False
hasTriple (_:_:[])   = False
hasTriple (a:b:c:xs)
  | a == b && a == c = True
  | otherwise        = hasTriple (b:c:xs)


getFirstTripleChar :: String -> Char
getFirstTripleChar (a:b:c:xs)
  | a == b && a == c = a
  | otherwise        = getFirstTripleChar (b:c:xs)
