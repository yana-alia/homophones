module Homophone where
-- import qualified Data.ByteString.Lazy as BS
-- import qualified Data.Map.Strict as Map
-- import GHC.IO.Unsafe
-- import Debug.Trace
-- import Data.Binary
-- import Data.Char
import Control.Monad
import qualified Data.Map as Map

-- import PhoneDict
import Data.IntMap (fromList)
import Data.Char

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

-- Takes two words and compares their phonetic spellings and outputs True if
-- if their are homophones of each other and false otherwise
homophones :: String -> String -> Bool
homophones x y = True

convertToPairList :: IO ()
convertToPairList = do
    s <- readFile "cmudict-list"
    let newContents = [n | c <- lines s, n <- "(\"" ++ c ++ "\"), "]
    unless (null newContents) $
        writeFile "cmudict-list.txt" newContents

-- Converts the phoneDict into a Map
-- dictMap :: Map.Map String String
-- dictMap = Map.fromList phoneDict

lookupDict :: [(String, String)] -> String -> (String, String)
lookupDict [] _ = ("","")
lookupDict (d:ds) w
    | fst d == upperW = d
    | otherwise = lookupDict ds w
    where
        upperW = map toUpper w

convertToPron :: String -> String
convertToPron w = pron
    where
        (_ , pron) = lookupDict testList w