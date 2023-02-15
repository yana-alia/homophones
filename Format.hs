module Format where

import Control.Monad

import PhoneDictList

testList = [("A", ["AH0"]), 
  ("A(1)", ["EY1"]), 
  ("A'S", ["EY1 Z"]), 
  ("A.", ["EY1"]), 
  ("A.'S", ["EY1 Z"]), 
  ("A.D.", ["EY2 D IY1"]), 
  ("A.M.", ["EY2 EH1 M"]), 
  ("A.S", ["EY1 Z"]), 
  ("A42128", ["EY1 F AO1 R T UW1 W AH1 N T UW1 EY1 T"]), 
  ("AA", ["EY2 EY1"]), 
  ("AAA", ["T R IH2 P AH0 L EY1"]), 
  ("AAAI", ["T R IH2 P AH0 L EY2 AY1"]), 
  ("AABERG", ["AA1 B ER0 G"]), 
  ("AACHEN", ["AA1 K AH0 N"]), 
  ("AACHENER", ["AA1 K AH0 N ER0"]), 
  ("AAH", ["AA1"]), 
  ("AAKER", ["AA1 K ER0"]), 
  ("AALIYAH", ["AA2 L IY1 AA2"])]

-- helps in converting the dictionary to fit list of pair format
convertToPairList :: IO ()
convertToPairList = do
    s <- readFile "cmudict-list"
    let newContents = [n | c <- lines s, n <- "(\"" ++ c ++ "\"]), "]
    unless (null newContents) $
        writeFile "cmudict-list.txt" newContents

mergeElems :: IO ()
mergeElems = do
    let mergedList = mergeElems' testList
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