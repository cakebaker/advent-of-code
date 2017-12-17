module KnotHash
( hash
) where

import Data.Char (intToDigit, ord)
import Data.Bits (xor)
import Numeric (showIntAtBase)
import Data.Vector (Vector)
import qualified Data.Vector as V


standardLengthSuffix :: [Int]
standardLengthSuffix = [17,31,73,47,23]

hash :: String -> String
hash s = foldl1 (++) $ map toHex $ denseHash $ sparseHash 64 (numbers, 0, 0) asciiChars
         where asciiChars = (map ord s) ++ standardLengthSuffix
               numbers    = V.fromList [0..255]

sparseHash :: Int -> (Vector Int, Int, Int) -> [Int] -> Vector Int
sparseHash 0 (v,_,_) _             = v
sparseHash i (v, current, skip) xs = sparseHash (i - 1) (step v xs current skip) xs

denseHash :: Vector Int -> [Int]
denseHash v = reverse $ foldl (\acc x -> (xors x):acc) [] $ chunkOf 16 (V.toList v)

xors :: [Int] -> Int
xors = foldr1 (\acc x -> x `xor` acc)

toHex :: Int -> String
toHex x
  | length hex == 1 = "0" ++ hex
  | otherwise       = hex
  where hex = showIntAtBase 16 intToDigit x ""

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = take n xs : (chunkOf n $ drop n xs)

step :: Vector Int -> [Int] -> Int -> Int -> (Vector Int, Int, Int)
step v [] current skip     = (v, current, skip)
step v (x:xs) current skip = step modified xs next (skip + 1)
                             where len      = V.length v
                                   next     = (current + x + skip) `mod` len
                                   modified = reverseSublist v x current

reverseSublist :: Vector Int -> Int -> Int -> Vector Int
reverseSublist v x current = v V.// (zip ids values)
                             where len    = V.length v
                                   ids    = map (\y -> (y + current) `mod` len) $ take x [0..]
                                   values = reverse $ map (\id -> v V.! id) ids
