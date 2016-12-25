import System.Environment
import Data.List (isInfixOf)


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let ips = lines content

  let resultPuzzle1 = countIPsWithTLS ips
  putStrLn $ "IPs with TLS support: " ++ show resultPuzzle1

  let resultPuzzle2 = countIPsWithSSL ips
  putStrLn $ "IPs with SSL support: " ++ show resultPuzzle2


countIPsWithTLS :: [String] -> Int
countIPsWithTLS ips = length $ filter (not . hasHypernetWithABBA) ipsWithABBAs
                      where ipsWithABBAs = filter hasABBA ips


countIPsWithSSL :: [String] -> Int
countIPsWithSSL ips = length $ filter hasSSL ipsWithABAs
                      where ipsWithABAs = filter hasABA ips


hasSSL :: String -> Bool
hasSSL ip = hasBAB babs hypernets
            where supernets = getSupernets ip []
                  hypernets = getHypernets ip []
                  babs      = map toBAB $ foldr1 (++) $ map getABAs supernets


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


hasHypernetWithABBA :: String -> Bool
hasHypernetWithABBA s = any hasABBA $ getHypernets s []


getHypernets :: String -> String -> [String]
getHypernets [] [] = []
getHypernets (x:[]) buffer
  | x == ']'  = buffer : []
  | otherwise = []
getHypernets (x:xs) buffer
  | x == '['  = getHypernets xs []
  | x == ']'  = buffer : (getHypernets xs [])
  | otherwise = getHypernets xs (buffer ++ [x])


getSupernets :: String -> String -> [String]
getSupernets [] buffer = []
getSupernets (x:[]) buffer
  | x == ']'  = []
  | otherwise = (buffer ++ [x]) : []
getSupernets (x:xs) buffer
  | x == '['  = buffer : (getSupernets xs [])
  | x == ']'  = getSupernets xs []
  | otherwise = getSupernets xs (buffer ++ [x])


hasABA :: String -> Bool
hasABA []            = False
hasABA (_:[])        = False
hasABA (_:_:[])      = False
hasABA (a:b:c:xs)
  | isABA (a:b:c:[]) = True
  | otherwise        = hasABA (b:c:xs)


hasBAB :: [String] -> [String] -> Bool
hasBAB [] _ = False
hasBAB (aba:abas) xs
  | any id $ map (isInfixOf aba) xs = True
  | otherwise                       = hasBAB abas xs


isABA :: String -> Bool
isABA (a:b:c:[])
  | a == c && a /= b = True
  | otherwise        = False


getABAs :: String -> [String]
getABAs []           = []
getABAs (_:[])       = []
getABAs (_:_:[])     = []
getABAs (a:b:c:xs)
  | isABA (a:b:c:[]) = (a:b:c:[]) : getABAs (b:c:xs)
  | otherwise        = getABAs (b:c:xs)


toBAB :: String -> String
toBAB (a:b:_) = (b:a:b:[])
