module Syllables where


import Data.List
import Debug.Trace

{-
    C = Consonant      V = Vowel

    We can represent English syllable structure by the formula C(0-3) V C(0-4)
    * The Handbook of English Pronunciation

    Rules of Breaking Words into Syllables:
    * https://thriveedservices.com/syllable-division-rules-how-to-divide-words-into-syllables/

    Rule 1: VC/CV – Split 2 consonants that are between vowels.
    Rule 2: C+le – The ending -le usually takes the consonant before it to make one syllable. (silent e)
    Rule 3: V/CV & VC/V – Split before or after a consonant that comes between 2 vowels.
    Rule 4: V/V – Split 2 vowels next to each other that do not work as a team.
    Rule 5: VC/CCV & VCC/CV – Split before or after the second consonant when 3 consonants come together.
    Rule 6: Divide after a prefix and before a suffix. 

-}

-- splits a word into its syllables
splitSyllables :: String -> [String]
splitSyllables []   = []
splitSyllables word = syllable : splitSyllables remaining
  where
    (syllable, remaining) = findSyllable word

-- returns a pair of the first syllable of given string and the remaining string         
findSyllable :: String -> (String, String)
findSyllable word@(w : ws)
    | w == 'y'  = vPortion ws "y" 1 -- treating y as a consonant
    | vowel w   = vPortion word "" 0
    | otherwise = cPortion word "" 0

-- Used to handle vowels (V) portion. Assumes to be the start of a word or the 
-- previous letter to be a consonant
vPortion :: String -> String -> Int -> (String, String)
vPortion [] syllable _    = (syllable, "")
vPortion "le" syllable _  = (syllable ++ "le", "") -- rule 2 (silent e)
vPortion "se" syllable _  = (syllable ++ "se", "") -- silent e
vPortion "me" syllable _  = (syllable ++ "me", "") -- silent e
vPortion "ne" syllable _  = (syllable ++ "ne", "") -- silent e
vPortion "ion" syllable _ = (syllable ++ "ion", "")
vPortion [c] syllable 2
    | vowel c   = (syllable, [c])
    | otherwise = (syllable ++ [c], "")
vPortion [c] syllable n = (syllable ++ [c], "")
vPortion c syllable 2   = (syllable, c) -- CVCV is not allowed
vPortion cc@(c1 : c2 : cs) syllable n
    | "ious" `isPrefixOf` cc            = (syllable ++ "ious", drop 4 cc)
    | any (`isPrefixOf` cc) vowelTeams2 = cPortion cs (syllable ++ [c1, c2]) (n + 1) -- legal VV in 1 syllable
    | vowel c1 && vowel c2              = (syllable ++ [c1], c2 : cs) -- split at CV/VC
    | not (vowel c1)                    = (syllable, cc) -- split at VC/CV
    | n == 2                            = (syllable ++ [c1], c2 : cs)
    | otherwise                         = cPortion (c2:cs) (syllable ++ [c1]) (n + 1)   

-- Used to handle consonants (C) portion. Assumes to be the start of a word or the 
-- previous letter to be a vowel
cPortion :: String -> String -> Int -> (String, String)
cPortion [] syllable _   = (syllable, "")
cPortion "le" syllable _ = (syllable ++ "le", "") -- rule 2 (silent e)
cPortion "se" syllable _ = (syllable ++ "se", "") -- silent e
cPortion "me" syllable _ = (syllable ++ "me", "") -- silent e
cPortion "ne" syllable _ = (syllable ++ "ne", "") -- silent e
cPortion [c] syllable _  = (syllable ++ [c], "") 
cPortion cc@(c1 : c2 : cs@(c3 : css)) syllable n
    | n /= 0 && isConsTeams3 && vowel (head cs) = (syllable, cc) -- legal CCC consTeams in 1 syllable
    | n /= 0 && isConsTeams2 && vowel c3        = (syllable, cc) -- legal CC in 1 syllable
    | isConsTeams3                              = vPortion css (syllable ++ [c1, c2, c3]) (n + 1) -- legal CCC 
    | isConsTeams2                              = vPortion cs (syllable ++ [c1, c2]) (n + 1) -- legal CC
    | c1 == 'y'                                 = vPortion (c2:cs) (syllable ++ [c1]) (n + 1) -- y is a consonant too
    | vowel c1                                  = (syllable, cc) -- split at CV/VC
    | n /= 0 && not (vowel c1) && vowel c2      = (syllable, cc)
    | otherwise                                 = vPortion (c2:cs) (syllable ++ [c1]) (n + 1)  
    where
        isConsTeams3 = any (`isPrefixOf` cc) consTeams3
        isConsTeams2 = any (`isPrefixOf` cc) consTeams2
cPortion cc@(c1 : c2 : cs) syllable n
    | any (`isPrefixOf` cc) consTeams2     = vPortion cs (syllable ++ [c1, c2]) (n + 1) -- legal CC in 1 syllable
    | c1 == 'y'                            = vPortion (c2:cs) (syllable ++ [c1]) (n + 1) -- y is a consonant too
    | vowel c1                             = (syllable, cc) -- split at CV/VC
    | n /= 0 && not (vowel c1) && vowel c2 = (syllable, cc)
    | otherwise                            = vPortion (c2:cs) (syllable ++ [c1]) (n + 1)  

vowel :: Char -> Bool
vowel c = c `elem` "aeiouy"

-- a combination of letters that can be considered a single sound
vowelTeams2 :: [String]
vowelTeams2 = ["ai", "ay", "ea", "ey", "ee", "ea", "ei", "ie", -- "igh", "eigh",
              "oa", "oe", "ew", "ue", "eu", "oi", "oy", "ou", "ow", "au", "aw", "oo"]

consTeams2 :: [String] -- Consonant Blends and Consonant Diagraphs 2 letters
consTeams2 = ["bl", "cl", "fl", "gl", "pl", "sl", "sc", "sk", "sm", "sn", "ld",
              "sp", "st", "br", "cr", "dr", "fr", "gr", "pr", "tr", "lt",
              "ch", "ck", "sh", "th", "wh", "ph", "ng", "gh", "mb", "qu"]

consTeams3 :: [String] -- Consonant Blends and Consonant Diagraphs 3 letters
consTeams3 = ["scr", "spl", "spr", "str", "shr", "squ", "thr"]