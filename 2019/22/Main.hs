import System.Environment
import Control.Applicative
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let techniques = map (fst . last) $ map (readP_to_S technique) $ lines content
  let deck       = V.fromList [0..10006]
  let wantedCard = 2019

  let result1 = V.findIndex (== wantedCard) $ exec deck techniques
  putStrLn $ "Result of puzzle 1: " ++ show result1


type Deck        = Vector Int
type TechniqueFn = (Deck -> Deck)

exec :: Deck -> [TechniqueFn] -> Deck
exec deck []       = deck
exec deck (fn:fns) = exec (fn deck) fns

doCut :: Int -> Deck -> Deck
doCut n xs
  | n < 0     = (V.drop (l + n) xs) V.++ (V.take (l + n) xs)
  | otherwise = (V.drop n xs) V.++ (V.take n xs) 
  where l = V.length xs

doDealIntoNewStack :: Deck -> Deck
doDealIntoNewStack = V.reverse

doDealWithIncrement :: Int -> Deck -> Deck
doDealWithIncrement n xs = V.update xs newIndices
                           where newIndices = V.zip (V.imap (\i _ -> mod (n * i) l) xs) xs
                                 l          = V.length xs

-- parser

technique :: ReadP TechniqueFn
technique = do
  t <- choice [cut, dealIntoNewStack, dealWithIncrement]
  return t

cut :: ReadP TechniqueFn
cut = do
  string "cut "
  x <- integer
  return $ doCut x

dealIntoNewStack :: ReadP TechniqueFn
dealIntoNewStack = do
  string "deal into new stack"
  return doDealIntoNewStack

dealWithIncrement :: ReadP TechniqueFn
dealWithIncrement = do
  string "deal with increment "
  x <- positiveInteger
  return $ doDealWithIncrement x

integer :: ReadP Int
integer = do
  x <- negativeInteger <|> positiveInteger
  return x

negativeInteger :: ReadP Int
negativeInteger = do
  char '-'
  x <- positiveInteger
  return $ negate x

positiveInteger :: ReadP Int
positiveInteger = do
  x <- many1 $ satisfy isDigit
  return $ read x
