import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let intCodes = head $ map (fst . last) $ map (readP_to_S codes) $ lines content
  let program = V.fromList intCodes

  let result1 = V.head (execute 0 $ program V.// [(1, 12), (2, 2)])
  putStrLn $ "Result of puzzle 1: " ++ show result1


execute :: Int -> Vector Int -> Vector Int
execute programPointer program
  | opcode == 1 = execute nextProgramPointer (executeOpcode positions (+) program)
  | opcode == 2 = execute nextProgramPointer (executeOpcode positions (*) program)
  | otherwise   = program
  where opcode             = program V.! programPointer
        nextProgramPointer = programPointer + codeLength
        positions          = V.toList $ V.tail $ V.slice programPointer codeLength program
        codeLength         = 4

executeOpcode :: [Int] -> (Int -> Int -> Int) -> Vector Int -> Vector Int
executeOpcode (a:b:target:[]) op program = program V.// [(target, op valA valB)]
                                           where valA = program V.! a
                                                 valB = program V.! b

codes :: ReadP [Int]
codes = do
  xs <- sepBy (many1 $ satisfy isDigit) (string ",")
  return $ map toInt xs

toInt :: String -> Int
toInt s = read s
