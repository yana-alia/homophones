module Format where

import Control.Monad

-- helps in converting the dictionary to fit list of pair format
convertToPairList :: IO ()
convertToPairList = do
    s <- readFile "TestDict.txt"
    let newContents = [n | c <- lines s, n <- "(\"" ++ c ++ "\"]), "]
    unless (null newContents) $
        writeFile "Convert.txt" newContents

mergeElems :: [(String, [String])] -> IO ()
mergeElems ls = do
    let mergedList = mergeElems' ls
    writeFile "merged.txt" (unlines $ map show mergedList)

mergeElems' :: [(String, [String])] -> [(String, [String])]
mergeElems' (l:[]) = [l]
mergeElems' (l:ls:lss)
  | pK1 == pK2 = mergeElems' ((pK1, snd l ++ snd ls):lss)
  | otherwise  = l: mergeElems' (ls:lss)
  where
    pK1 = head $ splitOn '(' (fst l) 
    pK2 = head $ splitOn '(' (fst ls)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d [] = []
splitOn d s = x : splitOn d (drop 1 y) 
  where
    (x,y) = span (/= d) s