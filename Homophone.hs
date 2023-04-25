module Homophone where

import Debug.Trace
import Control.Monad
import qualified Data.Map as Map
import Data.List
import Data.IntMap (fromList)
import Data.Char
import System.IO.Unsafe
import Data.Maybe
import Data.Array

import Format
import Arpabet

data Accent = British | Cockney | None | All
            deriving (Eq, Show)

type Pronunciation = [ARPABET]

-- Takes two words and compares their phonetic spellings and outputs True
-- if they are pure homophones of each other and false otherwise
isPureHomophone :: [String] -> [String] -> Accent -> Bool
isPureHomophone x y acc = matchAny (combine phonX) (combine phonY)
    where
        phonX = map (maybe [] (map (map toArpabet)) . lookupArpabet) x
        phonY = map (maybe [] (map (map toArpabet)) . lookupArpabet) y

-- Takes two words and compares their phonetic spellings and outputs True
-- if their fuzzy score is below a certain threshold
isHomophone :: [String] -> [String] -> Accent -> Bool
isHomophone x y acc = fuzzyScore combX combY < 100
    where
        combX = convertToAccent acc (combine phonX)
        combY = convertToAccent acc (combine phonY)
        phonX = map (maybe [] (map (map toArpabet)) . lookupArpabet) x
        phonY = map (maybe [] (map (map toArpabet)) . lookupArpabet) y
        
        fuzzyScore :: [Pronunciation] -> [Pronunciation] -> Int
        fuzzyScore x y = minimum $ map (\z -> minimum $ map (fuzzy z) y) x

-- the lower the score, the more similar the words are. (0 score = pure homophone)
-- assumptions:
--      * length of both words should be similar to be considered fuzzy
--      * 100 is arbitrary high number to depict infinity score
fuzzy :: [ARPABET] -> [ARPABET] -> Int
fuzzy [] [] = 0
fuzzy [] _ = 100
fuzzy _ [] = 100
fuzzy (x:xs) (y:ys) = distMatrix ! (x, y) + fuzzy xs ys

-- provides a list of all combination of pronunciations that can exist for a list of words
-- e.g. ["a","why"] which has pronunciations: [[["AH"],["EY"]],[["W","AY"],["HH","W","AY"]]]
--      outputs [["AH","W","AY"],["AH","HH","W","AY"],["EY","W","AY"],["EY","HH","W","AY"]]
combine :: [[Pronunciation]] -> [Pronunciation]
combine []       = []
combine [w]      = w
combine (w : ws) = combine' w (combine ws)
    where
        combine' :: [Pronunciation] -> [Pronunciation] -> [Pronunciation]
        combine' x y = [xs ++ ys | xs <- x, ys <- y]


-- look up ARPABET spelling of word if exists in dictionary
lookupArpabet :: String -> Maybe [[String]]
lookupArpabet s = Map.lookup (map toUpper s) dictMap

-- outputs True if any elem from list xs matches any elem from list ys
matchAny :: Eq a => [a] -> [a] -> Bool
matchAny [] _  = False
matchAny xs ys = any (`elem` ys) xs

-- Converts the ArpabetDict into a HashMap for faster lookup
{-# NOINLINE dictMap #-}
dictMap :: Map.Map String [[String]]
dictMap = Map.fromList file
    where
        file    = map readDict $ lines getFile
        getFile = unsafePerformIO $ do
            readFile "data/ArpabetDictStr.txt"

-- e.g "("A",[AH,EY])" -> ("A",[AH,EY]) with correct type specified assuming
-- formatting of elements in the String matches specified type
readDict :: String -> (String, [[String]])
readDict = read

-- ============================================= ACCENTS ============================================= --

convertToAccent :: Accent -> [Pronunciation] -> [Pronunciation]
convertToAccent None ls = ls
convertToAccent British ls = map convertToBritish ls
convertToAccent All ls = map head . group . sort $ map convertToBritish ls ++ ls

-- (map head . group . sort) is O(N log N) compared to O(N^2) of nub
convertToBritish :: [ARPABET] -> [ARPABET]
convertToBritish []  = []
convertToBritish [x] = [x | x /= R]
convertToBritish (R : xs)
    | isConsonent $ head xs = convertToBritish xs
    | otherwise = R : convertToBritish xs
convertToBritish (x : xs) = x : convertToBritish xs


-- =========================================== REVERSE DICT =========================================== --

homophones :: String -> [String]
homophones s = map head (group $ sort words)
    where
        words = concatMap (fromMaybe [] . lookupWords) arp
        arp = fromMaybe [] (lookupArpabet s)

-- look up words that corresponds to the given ARPABET spelling if exists in dictionary
lookupWords :: [String] -> Maybe [String]
lookupWords s = Map.lookup s revDictMap

-- Converts the RevArpabetDict into a HashMap for faster lookup
{-# NOINLINE revDictMap #-}
revDictMap :: Map.Map [String] [String]
revDictMap = Map.fromList file
    where
        file    = map readRevDict $ lines getFile
        getFile = unsafePerformIO $ do
            readFile "data/RevArpabetDict.txt"

readRevDict :: String -> ([String], [String])
readRevDict = read