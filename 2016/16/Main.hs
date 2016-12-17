
diskLength :: Int
diskLength = 272


main :: IO ()
main = do
  let initialState = "01111001100111011"
  let resultPuzzle1 = getChecksum initialState

  putStrLn $ "Checksum: " ++ resultPuzzle1


getChecksum :: String -> String
getChecksum xs = toChecksum $ expand xs


expand :: String -> String
expand xs
  | length expanded >= diskLength = expanded
  | otherwise                     = expand expanded
  where expanded = xs ++ "0" ++ (map invert $ reverse xs)


toChecksum :: String -> String
toChecksum xs
  | odd $ length checksum = checksum
  | otherwise             = toChecksum checksum
  where checksum = calculateChecksum (take diskLength xs)


calculateChecksum :: String -> String
calculateChecksum [] = []
calculateChecksum (a:b:xs)
  | a == b    = '1' : calculateChecksum xs
  | otherwise = '0' : calculateChecksum xs


invert :: Char -> Char
invert '1' = '0'
invert '0' = '1'
