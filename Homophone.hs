module Homophone where

import Debug.Trace
import Control.Monad
import qualified Data.Map as Map
import Data.IntMap (fromList)
import Data.Char
import System.IO.Unsafe

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

-- Takes two words and compares their phonetic spellings and outputs True
-- if they're are homophones of each other and false otherwise
homophone :: String -> String -> Bool
homophone x y = do 
    let dict = dictMap
    let phonX = Map.lookup (map toUpper x) dict
    let phonY = Map.lookup (map toUpper y) dict
    matchAny phonX phonY

-- outputs True if any elem from list xs matches any elem from list ys
matchAny :: Eq a => Maybe [a] -> Maybe [a] -> Bool
matchAny Nothing _ = False
matchAny _ Nothing = False
matchAny (Just xs) (Just ys) = any (`elem` ys) xs

-- Converts the phoneDict into a HashMap for faster lookup
dictMap :: Map.Map String [[String]]
dictMap = Map.fromList file
    where
        file = map readDict $ lines getFile
        getFile = unsafePerformIO $ do
            s <- readFile "data/ArpabetDict.txt"
            return s

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