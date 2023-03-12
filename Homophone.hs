module Homophone where

import Debug.Trace
import Control.Monad
import qualified Data.Map as Map
import Data.IntMap (fromList)
import Data.Char
import System.IO.Unsafe
import Data.Maybe

import Format

{-
data ARPABET = AA | AE | AH | AO | AW | AX | AXR | AY | EH | ER | EY | -- VOWELS
               IH | IX | IY | OW | OY | UH | UW | UX |                 -- VOWELS
               B | CH | D | DH | DX | EL | EM | EN | F | G | HH | JH | -- CONSONENTS
               K | L | M | N | NG | NX | P | Q | R | S | SH | T | TH | -- CONSONENTS
               V | W | WH | Y | Z | ZH                                 -- CONSONENTS
-}

data Accent = British | Cockney
            deriving (Eq)

type Arpabet = String
type Pronunciation = [Arpabet]

testList :: [(String, [String])]
testList = [("A",["AH0","EY1"]),
    ("A'S",["EY1 Z"]),
    ("A.",["EY1"]),
    ("A.'S",["EY1 Z"]),
    ("A.D.",["EY2 D IY1"]),
    ("A.M.",["EY2 EH1 M"]),
    ("THEIR", ["DH EH1 R"]), 
    ("THERE", ["DH EH1 R"]), 
    ("PATRON",["P EY1 T R AH0 N"]),
    ("RED", ["R EH1 D"]),
    ("REED",["R IY1 D"]),
    ("READ",["R EH1 D","R IY1 D"]),
    ("PATRONAGE",["P AE1 T R AH0 N IH0 JH","P EY1 T R AH0 N AH0 JH","P EY1 T R AH0 N IH0 JH"]),
    ("PATRONIZES",["P EY1 T R AH0 N AY2 Z AH0 Z"])]

-- TODO: changes out put type to be an int similarity level to support fuzzy homophones

-- Takes two words and compares their phonetic spellings and outputs True
-- if they're are homophones of each other and false otherwise
homophone :: [String] -> [String] -> Bool
homophone x y = matchAny (combine phonX) (combine phonY)
    where
        phonX = map (fromMaybe [] . lookupArpabet) x
        phonY = map (fromMaybe [] . lookupArpabet) y

-- provides a list of all combination of pronunciations that can exist for a list of words
-- e.g. ["a","why"] which has pronunciations: [[["AH"],["EY"]],[["W","AY"],["HH","W","AY"]]]
--      outputs [["AH","W","AY"],["AH","HH","W","AY"],["EY","W","AY"],["EY","HH","W","AY"]]
combine :: [[Pronunciation]] -> [Pronunciation]
combine [] = []
combine [w] = w
combine (w : ws) = combine' w (combine ws)
    where
        combine' :: [Pronunciation] -> [Pronunciation] -> [Pronunciation]
        combine' x y = [xs ++ ys | xs <- x, ys <- y]


-- look up ARPABET spelling of word if exists in dictionary
lookupArpabet :: String -> Maybe [Pronunciation]
lookupArpabet s = Map.lookup (map toUpper s) dictMap

-- outputs True if any elem from list xs matches any elem from list ys
matchAny :: Eq a => [a] -> [a] -> Bool
matchAny [] _ = False
matchAny xs ys = any (`elem` ys) xs

-- Converts the phoneDict into a HashMap for faster lookup
dictMap :: Map.Map String [Pronunciation]
dictMap = Map.fromList file
    where
        file = map readDict $ lines getFile
        getFile = unsafePerformIO $ do
            readFile "data/ArpabetDict.txt"

-- e.g "("A",["AH","EY"])" -> ("A",["AH","EY"]) with correct type specified assuming
-- formatting of elements in the String matches specified type
readDict :: String -> (String, [[String]])
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
convertToBritish _ = ["rbit"]