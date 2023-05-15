module Utility where

import Control.Monad
import System.IO.Unsafe
import Data.Char
import Data.Tuple
import Data.List
import qualified Data.Map as Map

import Homophone

-- helps in converting the dictionary to fit list of pair format
convertToPairList :: IO ()
convertToPairList = do
    s <- readFile "data/TestDict.txt"
    let newContents = [n | c <- lines s, n <- "(\"" ++ c ++ "\"]), "]
    unless (null newContents) $
        writeFile "Convert.txt" newContents

-- merge words with multiple pronunciations to have only one entry
-- e.g [("READ","R EH1 D"),("READ(1),"R IY1 D")] -> ("READ",["R EH1 D","R IY1 D"])
mergePhonetics :: [(String, [String])] -> IO ()
mergePhonetics ls = do
    let mergedList = mergePhonetics' ls
    writeFile "merged.txt" (unlines $ map show mergedList)

mergePhonetics' :: [(String, [String])] -> [(String, [String])]
mergePhonetics' []  = []
mergePhonetics' [l] = [l]
mergePhonetics' (l:ls:lss)
    | pK1 == pK2 = mergePhonetics' ((pK1, snd l ++ snd ls):lss)
    | otherwise  = l: mergePhonetics' (ls:lss)
    where
        pK1 = head $ splitOn '(' (fst l) 
        pK2 = head $ splitOn '(' (fst ls)

-- splits list on specified elem
-- e.g splitOn ' ' "abc 123" = ["abc","123"]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d s  = x : splitOn d (drop 1 y) 
    where
        (x,y) = span (/= d) s

-- Formats arpabetDict txt file to have [[String]] pronunciation instead of [String] and omits stress numbers
-- (e.g ["R EH1 D","R IY1 D"] -> [["R","EH","D"],["R","IY","D"]])
{-# NOINLINE convertNoStress #-}
convertNoStress :: IO ()
convertNoStress = writeFile "ArpabetDictCopy.txt" (unlines $ map show tuple)
    where
        tuple   = map (splitArpabet . readDictStress) $ lines getFile
        getFile = unsafePerformIO $ do
            readFile "data/ArpabetDictStress.txt"

readDictStress :: String -> (String, [String])
readDictStress = read

-- formating ARPABET of words 
-- (e.g ("READ",["R EH1 D","R IY1 D"]) -> ("READ", [["R","EH1","D"],["R","IY1","D"]]))
splitArpabet :: (String, [String]) -> (String, [[String]])
splitArpabet (k, v) = (k, v')
    where
        v'         = map (omitStress . splitOn ' ') v
        -- e.g ["R","EH1","D"] -> ["R","EH","D"]
        omitStress = map (filter (not . isDigit))


-- =================================== REVERSE DICT =================================== --

-- takes a .txt file of pairs, reverses the pair and write into another file
-- e.g. ("ABC", "123") -> ("123", "ABC")
{-# NOINLINE swapPair #-}
swapPair :: IO ()
swapPair = do
    let m = sort $ map swap file
    let formated = map splitArpabetRev m
    let mergedList = mergeWords formated
    writeFile "mergedRev.txt" (unlines $ map show mergedList)
    where
        file = map readPair $ lines getFile

        getFile :: String
        getFile = unsafePerformIO $ do
            readFile "data/PhoneDict.txt"
            
        readPair :: String -> ([String], String)
        readPair = read

-- FOR REVERSE DICT:
--  merge homophones to have only one entry
--  e.g [("AE1 B IY0",["ABBEY"]), ("AE1 B IY0",["ABBIE"])] -> ("AE1 B IY0",["ABBEY","ABBIE"])
mergeWords :: Eq a => [(a, [String])] -> [(a, [String])]
mergeWords []  = []
mergeWords [l] = [l]
mergeWords (l:ls:lss)
    | fst l == fst ls = mergeWords ((fst l, snd l ++ snd ls):lss)
    | otherwise  = l: mergeWords (ls:lss)

-- FOR REVERSE DICT:
--  formating ARPABET of words 
--  (e.g ("READ",["R EH1 D","R IY1 D"]) -> ("READ", [["R","EH1","D"],["R","IY1","D"]]))
splitArpabetRev :: (String, [String]) -> ([String], [String])
splitArpabetRev (k, v) = (k', v)
    where
        k'         = (omitStress . splitOn ' ') k
        -- e.g ["R","EH1","D"] -> ["R","EH","D"]
        omitStress = map (filter (not . isDigit))

-- =================================== TESTCASE GEN =================================== --

checkEqTuple :: Eq a => (a, a) -> (a, a) -> Bool
checkEqTuple (x, y) (x', y') = x == y' && y == x'

createPureTests :: IO()
createPureTests = writeFile "tests.txt" (unlines $ map show filtered)
    where
        filtered = nubBy checkEqTuple tests
        tests = filter (uncurry (/=)) [(k, v) | k <- keys, v <- homophones k]
        keys = Map.keys dictMap