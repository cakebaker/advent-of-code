import Data.Hash.MD5


main :: IO ()
main = do
  let doorID = "ugkcyxxp"
  let resultPuzzle1 = findPassword doorID

  putStrLn $ "Password: " ++ resultPuzzle1


findPassword :: String -> String
findPassword doorID = map head $ take 8 $ map getPasswordElement $ filter containsPasswordElement $ generateHashes doorID


getPasswordElement :: String -> String
getPasswordElement hash = take 1 $ drop 5 hash


containsPasswordElement :: String -> Bool
containsPasswordElement hash = take 5 hash == "00000"


generateHashes :: String -> [String]
generateHashes doorID = map (\x -> md5s $ Str $ doorID ++ show x) [0,1..]
