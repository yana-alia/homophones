module Homophone where

import qualified Data.Map as Map
import Data.List
import Data.IntMap (fromList)
import Data.Char
import System.IO.Unsafe
import Data.Maybe
import Data.Array

import Arpabet

data Accent = British | None | All
            deriving (Eq, Show, Read)

type Pronunciation = [ARPABET]

-- Takes two list of words and compares their phonetic spellings and outputs True
-- if they are pure homophones of each other and false otherwise
isPureHomophone :: [String] -> [String] -> Accent -> Bool
isPureHomophone x y acc = matchAny combX combY
    where
        combX = convertToAccent acc (combine phonX)
        combY = convertToAccent acc (combine phonY)
        phonX = map (maybe [] (map (map toArpabet)) . lookupArpabet) x
        phonY = map (maybe [] (map (map toArpabet)) . lookupArpabet) y

-- Takes two list of words and compares their phonetic spellings and outputs True
-- if their fuzzy score is below a certain threshold
isHomophone :: [String] -> [String] -> Accent -> Bool
isHomophone x y acc = fuzzyScore combX combY < 2
    where
        combX = miscChange $ convertToAccent acc (combine phonX)
        combY = miscChange $ convertToAccent acc (combine phonY)
        phonX = map (maybe [] (map (map toArpabet)) . lookupArpabet) x
        phonY = map (maybe [] (map (map toArpabet)) . lookupArpabet) y
        
-- Uses fuzzy scoring system to determine most similar pronunciation between
-- 2 list of words. Each scoring is divided by the length of pronunciation to normalise
-- the score (unless score >= inf where it is obviously not a homophone and thus
-- ignored).
fuzzyScore :: [Pronunciation] -> [Pronunciation] -> Float
fuzzyScore x y = minimum' $ map (minimum' . fuzzyScore' y) x
    where
        -- Similar to minimum but defaults to high score if list is empty
        minimum' :: [Float] -> Float
        minimum' [] = inf
        minimum' x = minimum x

-- *non-normalised ver*
-- fuzzyScore :: [Pronunciation] -> [Pronunciation] -> Float
-- fuzzyScore x y = minimum $ map (\z -> minimum $ map (fuzzy z) y) x

fuzzyScore' :: [Pronunciation] -> Pronunciation -> [Float]
fuzzyScore' [] _ = [] 
fuzzyScore' (x : xs) y
    | score >= inf = score : fuzzyScore' xs y
    | otherwise    = score / fromIntegral (length y) : fuzzyScore' xs y
    where
        score = fuzzy x y

-- the lower the score, the more similar the words are. (0 score = pure homophone)
-- assumptions:
--      * length of both words should be similar to be considered fuzzy
--      * inf is arbitrary high number to depict infinity score
fuzzy :: Pronunciation -> Pronunciation -> Float
fuzzy [] [] = 0
fuzzy [] _  = inf
fuzzy _ []  = inf
fuzzy (x : xs) (y : ys) = if score == inf then inf else score + fuzzy xs ys
    where
        score = distMatrix ! (x, y)

-- provides a list of all combination of pronunciations that can exist for a list of words
-- e.g. ["a","why"] which has pronunciations: [[[AH],[EY]],[[W,AY],[HH,W,AY]]]
--      outputs [[AH,W,AY],[AH,HH,W,AY],[EY,W,AY],[EY,HH,W,AY]]
combine :: [[[a]]] -> [[a]]
combine []       = []
combine [w]      = w
combine (w : ws) = combine' w (combine ws)
    where
        combine' :: [[a]] -> [[a]] -> [[a]]
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

-- e.g "("A",["AH","EY"])" -> ("A",["AH","EY"]) with correct type specified
-- assuming formatting of elements in the String matches specified type
readDict :: String -> (String, [[String]])
readDict = read

-- ============================================= ACCENTS ============================================= --

-- (map head . group . sort) is O(N log N) compared to O(N^2) of nub
convertToAccent :: Accent -> [Pronunciation] -> [Pronunciation]
convertToAccent None ls    = ls
convertToAccent British ls = map convertToBritish ls
convertToAccent All ls     = map head . group . sort $ map convertToBritish ls ++ ls

convertToBritish :: [ARPABET] -> [ARPABET]
convertToBritish []  = []
convertToBritish [x] = [x | x /= R]
convertToBritish (R : xs)
    | isConsonent $ head xs = convertToBritish xs
    | otherwise = R : convertToBritish xs
convertToBritish (x : xs) = x : convertToBritish xs

-- ============================================ MISC CHANGES ============================================ --

miscChange :: [Pronunciation] -> [Pronunciation]
miscChange [] = []
miscChange xx@(x : xs)
    | AY `elem` x = x : changeAYAH x : miscChange xs
    | ER `elem` x = x : changeAOR x : miscChange xs
    | otherwise   = x : miscChange xs

-- e.g. past oral vs pastoral [["P","AE","S","T"],["AO","R","AH","L"]] vs ["P","AE","S","T","ER","AH","L"]
--      corp oral vs corporal [["K","AO","R","P"],["AO","R","AH","L"]]  vs ["K","AO","R","P","ER","AH","L"]
changeAOR :: Pronunciation -> Pronunciation
changeAOR [] = []
changeAOR (ER : xs) = AO : R : changeAOR xs
changeAOR (x : xs) = x : changeAOR xs

-- changing AY -> AY,AH is chosen over AY,AH -> AY to increase potential fuzzy homophones
-- from multiple words.
-- e.g. vial vs vile ["V","AY","AH","L"] vs ["V","AY","L"]
changeAYAH :: Pronunciation -> Pronunciation
changeAYAH []        = []
changeAYAH [AY]      = [AY]
changeAYAH (AY : AH : xs) = AY : AH : changeAYAH xs
changeAYAH (AY : xs) = AY : AH : changeAYAH xs
changeAYAH (x : xs)  = x : changeAYAH xs


-- similar to Elem but supports a consecutive list to compare.
-- e.g. includes [1,2] [0,1,2,3] = True
--      includes [1,2] [0,1,3,4] = False
--      includes [1,2] [0,1,3,2] = False
includes :: Eq a => [a] -> [a] -> Bool
includes x y = includes' x y x
    where
        includes' :: Eq a => [a] -> [a] -> [a] -> Bool
        includes' [] _ _ = True
        includes' _ [] _ = False
        includes' (x : xs) (y : ys) xx@(z : zs)
            | x == y    = includes' xs ys xx
            | otherwise = if y == z then includes' zs ys xx else includes' xx ys xx

-- =========================================== REVERSE DICT =========================================== --

threshold :: Float
threshold = 3

-- generates words that have the exact same phonetic spelling as the given word.
-- e.g. "READ" -> ["READ","READE","RED","REDD","REED","REID","RIED","RIEDE","WREDE"]
homophones :: String -> [String]
homophones s = map head (group $ sort words)
    where
        words = concatMap (fromMaybe [] . lookupWords) arp
        arp   = fromMaybe [] (lookupArpabet s)

-- generates words that have similar phonetic spelling as the given word.
fuzzyHomophones :: String -> [String]
fuzzyHomophones s = map head (group $ sort words)
    where
        words   = concatMap (fromMaybe [] . lookupWords . map fromArpabet) fuzzArp
        fuzzArp = concatMap (fuzzyArpabet . map toArpabet) arp
        arp     = fromMaybe [] (lookupArpabet s)

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


-- -- * Method 1: Limit number of changes
-- -- Generate multiple Arpabet spellings that sound similar to given Pronunciation.
-- -- Only allow changes up to a certain scoring threshold
-- -- e.g. [R,AE,D] -> [[R,EH,D],[R,AE,DH],[R,EH,T],[R,UH,D],...]
-- fuzzyArpabet :: Pronunciation -> [Pronunciation]
-- fuzzyArpabet = fuzzyArpabet' 0

-- -- "map (arp :)" will cons arp onto every inner list and therefore needs a inner list to
-- -- map over, hence "[[]]" as the base case.
-- fuzzyArpabet' :: Float -> Pronunciation -> [Pronunciation]
-- fuzzyArpabet' _ [] = [[]]
-- fuzzyArpabet' 2 xx = [xx]
-- fuzzyArpabet' n (x : xs)
--     = [ m | arp <- [AA .. ZH], distMatrix ! (x, arp) <= threshold,
--         m <- map (arp :) (fuzzyArpabet' (if x == arp then n else n + 1) xs) ]

-- Method 2: Limiting overall fuzzy score
-- Generate multiple Arpabet spellings that sound similar to given Pronunciation.
-- Only allow changes up to a certain scoring threshold
-- e.g. [R,AE,D] -> [[R,EH,D],[R,AE,DH],[R,EH,T],[R,UH,D],...]
fuzzyArpabet :: Pronunciation -> [Pronunciation]
fuzzyArpabet = fuzzyArpabet' 0

-- "map (arp :)" will cons arp onto every inner list and therefore needs a inner list to
-- map over, hence "[[]]" as the base case.
fuzzyArpabet' :: Float -> Pronunciation -> [Pronunciation]
fuzzyArpabet' _ [] = [[]]
fuzzyArpabet' score (x : xs)
    = [ m | arp <- [AA .. ZH], score + distMatrix ! (x, arp) <= threshold,
        m <- map (arp :) (fuzzyArpabet' (score + distMatrix ! (x, arp)) xs) ]

-- =========================================== DEBUG MODE =========================================== --
-- Debug mode shows the score of each words generated by debugFuzzyHomophones. The lower the score
-- the less modified the pronunciation of the generated word is. 

-- generates words that have similar phonetic spelling as the given word.
debugFuzzyHomophones :: String -> [(Float, String)]
debugFuzzyHomophones s = sort $ debugFuzzyHomophones' fuzzArp
    where
        fuzzArp = concatMap (debugFuzzyArpabet 0 . map toArpabet) arp
        arp     = fromMaybe [] (lookupArpabet s)

debugFuzzyHomophones' :: [(Float, Pronunciation)] -> [(Float, String)]
debugFuzzyHomophones' [] = []
debugFuzzyHomophones' ((n, s) : ls) = zip n' s' ++ debugFuzzyHomophones' ls
    where
        n' = [n' | x <- [1 .. length s'], n' <- [n]]
        s' = fromMaybe [] . lookupWords $ map fromArpabet s

-- Method 1
-- "map (arp :)" will cons arp onto every inner list and therefore needs a inner list to
-- map over, hence "[[]]" as the base case.
-- debugFuzzyArpabet :: Float -> Pronunciation -> [(Float, Pronunciation)]
-- debugFuzzyArpabet n = debugFuzzyArpabet' n 0

-- debugFuzzyArpabet' :: Float -> Int -> Pronunciation -> [(Float, Pronunciation)]
-- debugFuzzyArpabet' n _ [] = [(n, [])]
-- debugFuzzyArpabet' n 2 xx = [(n, xx)]
-- debugFuzzyArpabet' n i (x : xs)
--     = nub [ (n', arp : m) | arp <- [AA .. ZH]
--     , distMatrix ! (x, arp) <= threshold
--     , (n', m) <- debugFuzzyArpabet' (n + distMatrix ! (x, arp)) (if x == arp then i else i + 1) xs]

-- Method 2
-- "map (arp :)" will cons arp onto every inner list and therefore needs a inner list to
-- map over, hence "[[]]" as the base case.
debugFuzzyArpabet :: Float -> Pronunciation -> [(Float, Pronunciation)]
debugFuzzyArpabet n [] = [(n, [])]
debugFuzzyArpabet n (x : xs)
    = nub [ (n', arp : m) | arp <- [AA .. ZH]
    , n + distMatrix ! (x, arp) <= threshold
    , (n', m) <- debugFuzzyArpabet (n + distMatrix ! (x, arp)) xs ]


-- ========================================== MISCELLANEOUS ========================================== --

{-
    Spoonerism: 2 words in which initial letters or syllables get swapped and create new words
    e.g. Blushing Crow -> Crushing Blow
         Key Naps -> Kneecaps
         Shoving Leopard -> Loving Shepard
         Fighting Liar -> Lighting Fire
         Dean Busy -> Bean Dizzy
         Nosy Cook -> Cozy Nook
-}

-- This function takes in a pair of words and outputs a list of pairs of words that are generated
-- by spoonerism.
-- e.g. spoonerism ("BLUSHING","CROW") = [("CRUSHING","BLEAU"),("CRUSHING","BLOW"),("CRUSHING","BLOWE")]
--      spoonerism ("fighting","liar") = [("LIGHTING","FIRE")]
spoonerism :: (String, String) -> [(String,String)]
spoonerism (w1, w2) = [ words | a <- p1, b <- p2, words <- spoonerism' a b]
    where 
        p1 = fromMaybe [] (lookupArpabet w1)
        p2 = fromMaybe [] (lookupArpabet w2)
    
spoonerism' :: [String] -> [String] -> [(String, String)]
spoonerism' x y = [ (s1, s2) | s1 <- swap1, s2 <- swap2]
    where
        swap1 = fromMaybe [] $ lookupWords $ map fromArpabet (cons2 ++ rest1)
        swap2 = fromMaybe [] $ lookupWords $ map fromArpabet (cons1 ++ rest2)
        (cons1, rest1) = splitWhen isVowel (map toArpabet x)
        (cons2, rest2) = splitWhen isVowel (map toArpabet y)

-- splits list by given predicate
-- e.g splitWhen (> 3) [1,2,3,4,5,2] = ([1,2,3],[4,5,2])
splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen func xx = splitWhen' func xx []
    where
        splitWhen' :: (a -> Bool) -> [a] -> [a] -> ([a],[a])
        splitWhen' func xx@(x : xs) leftover
            | func x = (leftover, xx)
            | otherwise = splitWhen' func xs (leftover ++ [x])
            