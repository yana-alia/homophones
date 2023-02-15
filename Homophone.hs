module Homophone where

import Debug.Trace
import Control.Monad
import qualified Data.Map as Map
import Data.IntMap (fromList)
import Data.Char

import MergedDict

testList :: [(String, [String])]
testList = [("A",["AH0","EY1"]),
    ("A'S",["EY1 Z"]),
    ("A.",["EY1"]),
    ("A.'S",["EY1 Z"]),
    ("A.D.",["EY2 D IY1"]),
    ("A.M.",["EY2 EH1 M"]),
    ("PATRON",["P EY1 T R AH0 N"]),
    ("PATRONAGE",["P AE1 T R AH0 N IH0 JH","P EY1 T R AH0 N AH0 JH","P EY1 T R AH0 N IH0 JH"]),
    ("PATRONE",["P AA0 T R OW1 N IY0"]),
    ("PATRONESS",["P EY1 T R AH0 N AH0 S"]),
    ("PATRONIZE",["P EY1 T R AH0 N AY2 Z","P AE1 T R AH0 N AY2 Z"]),
    ("PATRONIZED",["P EY1 T R AH0 N AY2 Z D","P AE1 T R AH0 N AY2 Z D"]),
    ("PATRONIZES",["P EY1 T R AH0 N AY2 Z AH0 Z"])]

-- Takes two words and compares their phonetic spellings and outputs True
-- if they're are homophones of each other and false otherwise
homophone x y = do 
    let dict = dictMap
    let phonX = Map.lookup (map toUpper x) dict
    let phonY = Map.lookup (map toUpper y) dict
    matchAny phonX phonY

-- outputs True if any elem from list xs matches any elem from list ys
matchAny :: Eq a => Maybe [a] -> Maybe [a] -> Bool
matchAny Nothing _ = False
matchAny _ Nothing = False
matchAny (Just xs) (Just ys) = any ((flip elem) ys) xs

-- Converts the phoneDict into a Map
dictMap :: Map.Map String [String]
dictMap = Map.fromList phoneDict

