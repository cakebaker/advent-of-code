import System.Environment


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let ips = lines content
  let resultPuzzle1 = countIPsWithTLS ips

  putStrLn $ "IPs with TLS support: " ++ show resultPuzzle1


countIPsWithTLS :: [String] -> Int
countIPsWithTLS ips = length $ filter (not . hasHyperNetWithABBA) ipsWithABBAs
                      where ipsWithABBAs = filter hasABBA ips


hasABBA :: String -> Bool
hasABBA []              = False
hasABBA (_:[])          = False
hasABBA (_:_:[])        = False
hasABBA (_:_:_:[])      = False
hasABBA (a:b:c:d:xs)
  | isABBA (a:b:c:d:[]) = True
  | otherwise           = hasABBA (b:c:d:xs)


isABBA :: String -> Bool
isABBA (a:b:c:d:[])
  | a == d && b == c && a /= b = True
  | otherwise                  = False


hasHyperNetWithABBA :: String -> Bool
hasHyperNetWithABBA s = any hasABBA $ getHyperNets s []


getHyperNets :: String -> String -> [String]
getHyperNets [] [] = []
getHyperNets (x:[]) buffer
  | x == ']'  = buffer : []
  | otherwise = []
getHyperNets (x:xs) buffer
  | x == '['  = getHyperNets xs []
  | x == ']'  = buffer : (getHyperNets xs [])
  | otherwise = getHyperNets xs (buffer ++ [x])
