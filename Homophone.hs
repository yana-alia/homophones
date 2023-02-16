module Homophone where

import Debug.Trace
import Control.Monad
import qualified Data.Map as Map
import Data.IntMap (fromList)
import Data.Char

-- import MergedDict

data ARPABET = AA | AE | AH | AO | AW | AX | AXR | AY | EH | ER | EY | -- VOWELS
               IH | IX | IY | OW | OY | UH | UW | UX |                 -- VOWELS
               B | CH | D | DH | DX | EL | EM | EN | F | G | HH | JH | -- CONSONENTS
               K | L | M | N | NG | NX | P | Q | R | S | SH | T | TH | -- CONSONENTS
               V | W | WH | Y | Z | ZH                                 -- CONSONENTS


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

-- Converts the phoneDict into a Map
dictMap :: Map.Map String [String]
dictMap = Map.fromList testList

encodeArpabet :: String -> ARPABET
encodeArpabet s = AA