import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let initialLevel = 0
  let orbits = map parse $ lines content
  
  let result1 = countOrbits [centerOfMass] orbits initialLevel
  putStrLn $ "Result of puzzle 1: " ++ show result1

  let result2 = orbitalTransfersBetween "YOU" "SAN" orbits
  putStrLn $ "Result of puzzle 2: " ++ show result2


type Orbit = (String, String)

centerOfMass :: String
centerOfMass = "COM"

countOrbits :: [String] -> [Orbit] -> Int -> Int
countOrbits [] _ _ = 0
countOrbits (x:xs) orbits currentLevel
  | objects == [] = currentLevel + countOrbits xs orbits currentLevel
  | otherwise     = currentLevel + countOrbits xs orbits currentLevel + countOrbits objects orbits nextLevel
  where objects   = inOrbitAround x orbits
        nextLevel = currentLevel + 1

inOrbitAround :: String -> [Orbit] -> [String]
inOrbitAround target orbits = map snd $ filter (\x -> fst x == target) orbits

findFirstDuplicate :: [String] -> [String] -> String
findFirstDuplicate (x:xs) ys
  | elem x ys = x
  | otherwise = findFirstDuplicate xs ys

orbitalTransfersBetween :: String -> String -> [Orbit] -> Int
orbitalTransfersBetween objectA objectB orbits = length pathAToIntersection + length pathBToIntersection
                                                 where pathA               = pathToCenterOfMass objectA orbits
                                                       pathB               = pathToCenterOfMass objectB orbits
                                                       intersection        = findFirstDuplicate pathA pathB
                                                       pathAToIntersection = takeWhile (/= intersection) pathA
                                                       pathBToIntersection = takeWhile (/= intersection) pathB

pathToCenterOfMass :: String -> [Orbit] -> [String]
pathToCenterOfMass target orbits
  | target == centerOfMass = []
  | otherwise              = object:(pathToCenterOfMass object orbits)
  where object = fst $ head $ filter (\x -> snd x == target) orbits

parse :: String -> Orbit
parse s = (takeWhile (/= ')') s, tail $ dropWhile (/= ')') s)
