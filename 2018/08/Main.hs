import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename

  let numbers = parse $ head $ lines content

  let resultPuzzle1 = sum $ getMetadata numbers []
  putStrLn $ "The result of puzzle 1 is: " ++ show resultPuzzle1


type ParentHeaders = [(Int, Int)]

getMetadata :: [Int] -> ParentHeaders -> [Int]
getMetadata [] _ = []
getMetadata (quantityChildNodes:quantityMetadata:xs) parentHeaders
  | quantityChildNodes /= 0                             = getMetadata xs ((quantityChildNodes,quantityMetadata):parentHeaders)
  | quantityChildNodes == 0 && quantitySiblingNodes > 1 = metadata ++ getMetadata rest (decChildNodesOfParent parentHeaders)
  | otherwise                                           = metadata ++ parentMetadata ++ getMetadata restWithoutParentMetadata updatedParentHeaders
  where (metadata, rest)          = splitAt quantityMetadata xs
        (quantitySiblingNodes, _) = head parentHeaders
        parentMetadata            = getParentMetadata rest parentHeaders
        restWithoutParentMetadata = drop (length parentMetadata) rest
        updatedParentHeaders      = decChildNodesOfParent $ removeOneChildParents parentHeaders

getParentMetadata :: [Int] -> ParentHeaders -> [Int]
getParentMetadata xs []     = []
getParentMetadata xs ((quantityChildNodes, quantityMetadata):parentHeaders)
  | quantityChildNodes == 1 = metadata ++ getParentMetadata rest parentHeaders
  | otherwise               = []
  where (metadata, rest) = splitAt quantityMetadata xs

decChildNodesOfParent :: ParentHeaders -> ParentHeaders
decChildNodesOfParent []    = []
decChildNodesOfParent ((quantityChildNodes, quantityMetadata):parentHeaders) = (quantityChildNodes - 1, quantityMetadata):parentHeaders

removeOneChildParents :: ParentHeaders -> ParentHeaders
removeOneChildParents [] = []
removeOneChildParents all@((quantityChildNodes,_):parentHeaders)
  | quantityChildNodes == 1 = removeOneChildParents parentHeaders
  | otherwise               = all

parse :: String -> [Int]
parse s = map (\x -> read x) $ words s
