module Homophone where

import Debug.Trace
import Control.Monad
import qualified Data.Map as Map
import Data.IntMap (fromList)
import Data.Char
import System.IO.Unsafe
import Data.Maybe
import Data.Array

import Format
import Arpabet

data Accent = British | Cockney
            deriving (Eq)

type Pronunciation = [ARPABET]

-- TODO: changes out put type to be an int similarity level to support fuzzy homophones

-- Takes two words and compares their phonetic spellings and outputs True
-- if they are pure homophones of each other and false otherwise
pureHomophone :: [String] -> [String] -> Bool
pureHomophone x y = matchAny (combine phonX) (combine phonY)
    where
        phonX = map (fromMaybe [] . lookupArpabet) x
        phonY = map (fromMaybe [] . lookupArpabet) y

-- Takes two words and compares their phonetic spellings and outputs True
-- if their fuzzy score is below a certain threshold
homophone :: [String] -> [String] -> Bool
homophone x y = fuzzyScore (combine phonX) (combine phonY) < 100
    where
        phonX = map (fromMaybe [] . lookupArpabet) x
        phonY = map (fromMaybe [] . lookupArpabet) y

-- the lower the score, the more similar the words are. (0 score = pure homophone)
-- assumptions:
--      * length of both words should be similar to be considered fuzzy
--      * 100 is arbitrary high number to depict infinity score
fuzzy :: [ARPABET] -> [ARPABET] -> Int
fuzzy [] [] = 0
fuzzy [] _ = 100
fuzzy _ [] = 100
fuzzy (x:xs) (y:ys) = distMatrix ! (x, y) + fuzzy xs ys

fuzzyScore :: [Pronunciation] -> [Pronunciation] -> Int
fuzzyScore x y = minimum $ map (\z -> minimum $ map (fuzzy z) y) x

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
lookupArpabet :: String -> Maybe [Pronunciation]
lookupArpabet s = Map.lookup (map toUpper s) dictMap

-- outputs True if any elem from list xs matches any elem from list ys
matchAny :: Eq a => [a] -> [a] -> Bool
matchAny [] _  = False
matchAny xs ys = any (`elem` ys) xs

-- Converts the phoneDict into a HashMap for faster lookup
dictMap :: Map.Map String [Pronunciation]
dictMap = Map.fromList file
    where
        file    = map readDict $ lines getFile
        getFile = unsafePerformIO $ do
            readFile "data/ArpabetDict.txt"

-- e.g "("A",[AH,EY])" -> ("A",[AH,EY]) with correct type specified assuming
-- formatting of elements in the String matches specified type
readDict :: String -> (String, [[ARPABET]])
readDict = read

-- ============================================= ACCENTS ============================================= --

convertToAccent :: [String] -> Accent -> [String]
convertToAccent [] _ = []
convertToAccent (a:as) act = a' ++ convertToAccent as act
    where
        a' = case act of
                    British -> convertToBritish a
                    _ -> [a]

-- TODO: remove empty string from omitting "R"
convertToBritish :: String -> [String]
convertToBritish "R" = []
convertToBritish "OW" = ["AX","UH"] 
convertToBritish _ = ["init"]

-- ============================================= CLUSTERING ============================================= --

