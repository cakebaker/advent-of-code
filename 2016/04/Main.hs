import System.Environment
import Data.Char (chr, isLower, ord)
import Data.List (sortBy)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let rooms = lines content
  let realRooms = filter isRealRoom rooms

  let result1 = sum $ map getSectorID realRooms
  putStrLn $ "The sum of the sector IDs of the real rooms is: " ++ show result1

  let result2 = getSectorIDOfRoomWithNorthPoleObjects $ decryptRoomNames realRooms
  putStrLn $ "The sector ID of the room with North Pole objects is: " ++ show result2


isRealRoom :: String -> Bool
isRealRoom room
  | calculatedChecksum == checksum = True
  | otherwise                      = False
  where 
    checksum = getChecksum room
    calculatedChecksum = calculateChecksum $ getEncryptedName room


-- Example of a room record: aaaaa-bbb-z-y-x-123[abxyz]
-- aaaaa-bbb-z-y-x -> Encrypted name (variable length)
-- 123 -> Sector ID (fixed length)
-- abxyz -> Checksum (fixed length)


checksumLength :: Int
checksumLength = 5


sectorIDLength :: Int
sectorIDLength = 3


getEncryptedName :: String -> String
getEncryptedName room = reverse $ drop 11 $ reverse room


getSectorID :: String -> Int
getSectorID room = read $ reverse $ take sectorIDLength $ drop 7 $ reverse room


getChecksum :: String -> String
getChecksum room = reverse $ take checksumLength $ tail $ reverse room


calculateChecksum :: String -> String
calculateChecksum encryptedName = map fst mostFrequentChars
                                  where mostFrequentChars = take checksumLength charFrequency
                                        charFrequency = getCharFrequency $ stripDashes encryptedName


stripDashes :: String -> String
stripDashes = filter (not . (`elem` "-"))


getCharFrequency :: String -> [(Char, Int)]
getCharFrequency s = sortBy sortByFrequency [ (x,c) | x <- ['a'..'z'], let c = (length . filter (== x)) s, c > 0 ]


sortByFrequency :: (Char, Int) -> (Char, Int) -> Ordering
sortByFrequency (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2


getSectorIDOfRoomWithNorthPoleObjects :: [(String, Int)] -> Int
getSectorIDOfRoomWithNorthPoleObjects roomNames = snd $ head $ filter (\x -> (take (length "northpole") (fst x)) == "northpole") roomNames


decryptRoomNames :: [String] -> [(String, Int)]
decryptRoomNames = map (\room -> (decryptRoomName (getEncryptedName room) (getSectorID room), getSectorID room))


decryptRoomName :: String -> Int -> String
decryptRoomName encryptedRoomName sectorID = map (rotateLetter rotations) encryptedRoomName
                                             where rotations = mod sectorID 26


rotateLetter :: Int -> Char -> Char
rotateLetter _ '-' = ' '
rotateLetter rotations c
  | ord c + rotations > ord 'z' = chr (ord c + rotations - 26)
  | otherwise = chr (ord c + rotations)
