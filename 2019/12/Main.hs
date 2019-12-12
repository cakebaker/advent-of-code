import Data.Char (digitToInt)
import Data.List (group)

main :: IO ()
main = do
  let simulationSteps = 1000
  let initialVelocity = (0, 0, 0)
  let moons = [((  1,   4,  4), initialVelocity), 
               (( -4,  -1, 19), initialVelocity),
               ((-15, -14, 12), initialVelocity),
               ((-17,   1, 10), initialVelocity)]

  let result1 = sum $ map totalEnergy $ runSimulationSteps simulationSteps moons
  putStrLn $ "Result of puzzle 1: " ++ show result1


type Moon     = (Position, Velocity)
type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

runSimulationSteps :: Int -> [Moon] -> [Moon]
runSimulationSteps x moons = head $ drop x $ iterate step moons

step :: [Moon] -> [Moon]
step moons = map applyVelocity $ map (\moon -> applyGravity moon moon moons) moons

applyGravity :: Moon -> Moon -> [Moon] -> Moon
applyGravity _ m []                                     = m
applyGravity moon@(posA, _) (posB, v) ((posC, _):moons) = applyGravity moon (posB, applyGravityToVelocity posA posC v) moons

applyGravityToVelocity :: Position -> Position -> Velocity -> Velocity
applyGravityToVelocity (x1,y1,z1) (x2,y2,z2) (vx,vy,vz) = (applyGravityToAxis x1 x2 vx, applyGravityToAxis y1 y2 vy, applyGravityToAxis z1 z2 vz)

applyGravityToAxis :: Int -> Int -> Int -> Int
applyGravityToAxis a b v
  | a < b     = v + 1
  | a > b     = v - 1
  | otherwise = v

applyVelocity :: Moon -> Moon
applyVelocity ((x,y,z), velocity@(vx,vy,vz)) = ((x + vx, y + vy, z + vz), velocity)

totalEnergy :: Moon -> Int
totalEnergy m = potentialEnergy m * kineticEnergy m

potentialEnergy :: Moon -> Int
potentialEnergy ((x,y,z), _) = abs x + abs y + abs z

kineticEnergy :: Moon -> Int
kineticEnergy (_, (vx,vy,vz)) = abs vx + abs vy + abs vz
