module Homophone where

import Debug.Trace
import Control.Monad
import qualified Data.Map as Map
import Data.IntMap (fromList)
import Data.Char

import PhoneDict

testList = [("A", "AH0"), 
  ("A(1)", "EY1"), 
  ("A'S", "EY1 Z"), 
  ("A.", "EY1"), 
  ("A.'S", "EY1 Z"), 
  ("A.D.", "EY2 D IY1"), 
  ("A.M.", "EY2 EH1 M"), 
  ("A.S", "EY1 Z"), 
  ("A42128", "EY1 F AO1 R T UW1 W AH1 N T UW1 EY1 T"), 
  ("AA", "EY2 EY1"), 
  ("AAA", "T R IH2 P AH0 L EY1"), 
  ("AAAI", "T R IH2 P AH0 L EY2 AY1"), 
  ("AABERG", "AA1 B ER0 G"), 
  ("AACHEN", "AA1 K AH0 N"), 
  ("AACHENER", "AA1 K AH0 N ER0"), 
  ("AAH", "AA1"), 
  ("AAKER", "AA1 K ER0"), 
  ("AALIYAH", "AA2 L IY1 AA2")]

-- Takes two words and compares their phonetic spellings and outputs True
-- if they're are homophones of each other and false otherwise
homophone :: String -> String -> Bool
homophone x y = phonX == phonY
    where
        phonX = convertToPhon x
        phonY = convertToPhon y

homophone2 x y = do 
    let dict = dictMap
    let phonX = Map.lookup (map toUpper x) dict
    let phonY = Map.lookup (map toUpper y) dict
    phonX == phonY

-- Converts the phoneDict into a Map
dictMap :: Map.Map String String
dictMap = Map.fromList phoneDict

-- Used if using a List of pairs with key: word, elem: spelling
lookupDict :: [(String, String)] -> String -> (String, String)
lookupDict [] _ = ("","")
lookupDict (d:ds) w
    | fst d == upperW = d
    | otherwise = lookupDict ds w
    where
        upperW = map toUpper w

-- TODO: Switch to using Maybe to handle words not in dictionary
convertToPhon :: String -> String
convertToPhon w = phon
    where
        (_ , phon) = lookupDict phoneDict w

-- helps in converting the dictionary to fit list of pair format
convertToPairList :: IO ()
convertToPairList = do
    s <- readFile "cmudict-list"
    let newContents = [n | c <- lines s, n <- "(\"" ++ c ++ "\"), "]
    unless (null newContents) $
        writeFile "cmudict-list.txt" newContents
