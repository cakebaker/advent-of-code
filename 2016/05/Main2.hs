import Data.Hash.MD5
import Data.List (isInfixOf, sortBy)


main :: IO ()
main = do
  let doorID = "ugkcyxxp"
  let resultPuzzle2 = findPassword doorID

  putStrLn $ "Password: " ++ resultPuzzle2


findPassword :: String -> String
findPassword doorID = toString $ sortBy sortByPosition $ takePasswordElements [] $ map getPasswordElement $ filter containsPasswordElement $ generateHashes doorID
                      where toString = map (\x -> head $ fst x)


takePasswordElements :: [Int] -> [(String, Int)] -> [(String, Int)]
takePasswordElements positions ((passwordElement, position):xs)
  | length positions == 8 = []
  | isPositionUsed        = takePasswordElements positions xs
  | otherwise             = (passwordElement, position) : takePasswordElements (position : positions) xs
  where isPositionUsed = elem position positions


getPasswordElement :: String -> (String, Int)
getPasswordElement hash = (nth 7 hash, read $ nth 6 hash)


containsPasswordElement :: String -> Bool
containsPasswordElement hash
  | startsWithZeros && validPosition = True
  | otherwise                        = False
  where startsWithZeros = take 5 hash == "00000"
        validPosition   = isInfixOf (nth 6 hash) ['0'..'7']


generateHashes :: String -> [String]
generateHashes doorID = map (\x -> md5s $ Str $ doorID ++ show x) [0,1..]


nth :: Int -> [a] -> [a]
nth i xs = take 1 $ drop (pred i) xs


sortByPosition :: (String, Int) -> (String, Int) -> Ordering
sortByPosition (a1, b1) (a2, b2)
  | b1 < b2  = LT
  | b1 > b2  = GT
  | b1 == b2 = compare a1 a2
