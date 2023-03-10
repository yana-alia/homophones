module Format where

import Control.Monad
import System.IO.Unsafe
import Data.Char

-- helps in converting the dictionary to fit list of pair format
convertToPairList :: IO ()
convertToPairList = do
    s <- readFile "TestDict.txt"
    let newContents = [n | c <- lines s, n <- "(\"" ++ c ++ "\"]), "]
    unless (null newContents) $
        writeFile "Convert.txt" newContents

-- merge words with multiple pronunciations to have only one entry
-- e.g [("READ","R EH1 D"),("READ(1),"R IY1 D")] -> ("READ",["R EH1 D","R IY1 D"])
mergeElems :: [(String, [String])] -> IO ()
mergeElems ls = do
    let mergedList = mergeElems' ls
    writeFile "merged.txt" (unlines $ map show mergedList)

mergeElems' :: [(String, [String])] -> [(String, [String])]
mergeElems' [] = []
mergeElems' [l] = [l]
mergeElems' (l:ls:lss)
    | pK1 == pK2 = mergeElems' ((pK1, snd l ++ snd ls):lss)
    | otherwise  = l: mergeElems' (ls:lss)
    where
        pK1 = head $ splitOn '(' (fst l) 
        pK2 = head $ splitOn '(' (fst ls)

-- splits list on specified elem
-- e.g splitOn ' ' "abc 123" = ["abc","123"]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d s = x : splitOn d (drop 1 y) 
    where
        (x,y) = span (/= d) s



-- Formats arpabetDict txt file to have [[String]] pronunciation instead of [String] and omits stress numbers
-- (e.g ["R EH1 D","R IY1 D"] -> [["R","EH","D"],["R","IY","D"]])
convertNoStress :: IO ()
convertNoStress = writeFile "ArpabetDict.txt" (unlines $ map show tuple)
    where
        tuple = map (splitArpabet . readDictStress) $ lines getFile
        getFile = unsafePerformIO $ do
            readFile "data/ArpabetDictStress.txt"

        readDictStress :: String -> (String, [String])
        readDictStress = read

        -- formating ARPABET of words 
        -- (e.g ("READ",["R EH1 D","R IY1 D"]) -> ("READ", [["R","EH1","D"],["R","IY1","D"]]))
        splitArpabet :: (String, [String]) -> (String, [[String]])
        splitArpabet (k, v) = (k, v')
            where
                v' = map (omitStress . splitOn ' ') v
        -- e.g ["R","EH1","D"] -> ["R","EH","D"]
        omitStress = map (filter (not . isDigit))