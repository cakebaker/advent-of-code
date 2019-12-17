import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let centerOfMass = "COM"
  let initialLevel = 0
  let orbits = map parse $ lines content
  
  let result1 = countOrbits [centerOfMass] orbits initialLevel
  putStrLn $ "Result of puzzle 1: " ++ show result1


type Orbit = (String, String)

countOrbits :: [String] -> [Orbit] -> Int -> Int
countOrbits [] _ _ = 0
countOrbits (x:xs) orbits currentLevel
  | objects == [] = currentLevel + countOrbits xs orbits currentLevel
  | otherwise     = currentLevel + countOrbits xs orbits currentLevel + countOrbits objects orbits nextLevel
  where objects   = inOrbitAround x orbits
        nextLevel = currentLevel + 1

inOrbitAround :: String -> [Orbit] -> [String]
inOrbitAround target orbits = map snd $ filter (\x -> fst x == target) orbits

parse :: String -> Orbit
parse s = (takeWhile (/= ')') s, tail $ dropWhile (/= ')') s)
