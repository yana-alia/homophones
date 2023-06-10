module Arpabet where

import Data.Ix
import Data.Array

data ARPABET = AA | AE | AH | AO | AW | AX | AXR | AY | EH | ER | EY | -- VOWELS
               IH | IX | IY | OW | OY | UH | UW | UX |                 -- VOWELS
               B | CH | D | DH | DX | EL | EM | EN | F | G | HH | JH | -- CONSONENTS
               K | L | M | N | NG | NX | P | Q | R | S | SH | T | TH | -- CONSONENTS
               V | W | WH | Y | Z | ZH                                 -- CONSONENTS
            deriving (Show, Enum, Ord, Eq, Read, Ix)

{-

* Consonents to Vowels are valued inf (max distance) and vice versa *

Special case: CH is similar to K ONLY when it is after a vowel in a syllable (synch vs sink) *TODO*
              R and ER are similar when it is at the end of a syllable (liar vs lyre)
              IY and AH are similar when combining words (candy dates vs candidates)
              Z and S are similar when Z is at the end (holmes pun vs home spun)

-}

isVowel :: ARPABET -> Bool
isVowel arp = arp >= AA && arp <= UX

isConsonent :: ARPABET -> Bool
isConsonent = not . isVowel
   
-- Constant to define infinity score
inf :: Float
inf = 1000

distMatrix :: Array (ARPABET, ARPABET) Float
distMatrix = array ((AA,AA),(ZH,ZH)) (zip ix val)
    where
       ix = [ (i, j) | i <- [AA .. ZH], j <- [AA .. ZH]]
       --        AA   AE   AH   AO   AW   AX  AXR   AY   EH   ER   EY   IH   IX   IY   OW   OY   UH   UW   UX    B   CH    D   DH   DX   EL   EM   EN    F    G   HH JH    K    L    M    N   NG   NX    P    Q    R    S   SH    T   TH    V    W   WH    Y    Z   ZH
       val   = [  0,   3,   1,   1,   3,   1,   1, inf,   3,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  3,   0,   3,   3,   3,   3,   3,   4,   1,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   0,   1,   2,   1,   1, inf,   3,   2, inf, inf, inf,   4, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   1,   0,   3,   1,   1, inf,   3,   2, inf, inf, inf, inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  3,   3,   2,   3,   0,   3,   3, inf,   3, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   1,   1,   3,   0,   1, inf,   3,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   1,   1,   3,   1,   0, inf,   3,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf,   4, inf, inf, inf, inf, inf,   0,   3, inf,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  3,   1,   3,   3,   3,   3,   3,   3,   0,   3,   1,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  2,   3,   2,   2, inf,   2,   1, inf,   3,   0,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf,   2,   1,   3,   0,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf,   3,   3,   3,   0,   1,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf,   3,   3,   3,   1,   0,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf,   4, inf, inf, inf, inf, inf,   3, inf,   3,   1,   1,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   2,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0,   1,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1,   0,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1,   1,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0,   1,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3,   3, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1,   0,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3,   3, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   2,   1,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3,   3, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf,   1,   2,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf,   0,   1,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   2, inf, inf, inf, inf, inf, inf, inf,   1,   0,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf,   2,   1,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf,   3, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf, inf,   1, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf, inf,   2,   1,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0,   1, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1,   0, inf, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0,   1, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1,   0, inf, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   0, inf, inf,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1,   2, inf, inf, inf, inf, inf, inf,   0,   1,
                inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf,   1,   0]

-- A check to make sure that the distMatrix is symmetrical
checkMatrixSym :: Bool
checkMatrixSym = and [ distMatrix ! (i, j) == distMatrix ! (j, i) | i <- [AA .. ZH], j <- [AA .. ZH]]

toArpabet :: String -> ARPABET
toArpabet "AA" = AA
toArpabet "AE" = AE
toArpabet "AH" = AH
toArpabet "AO" = AO
toArpabet "AW" = AW
toArpabet "AX" = AX
toArpabet "AXR" = AXR
toArpabet "AY" = AY
toArpabet "EH" = EH
toArpabet "ER" = ER
toArpabet "EY" = EY
toArpabet "IH" = IH
toArpabet "IX" = IX
toArpabet "IY" = IY
toArpabet "OW" = OW
toArpabet "OY" = OY
toArpabet "UH" = UH
toArpabet "UW" = UW
toArpabet "UX" = UX
toArpabet "B" = B
toArpabet "CH" = CH
toArpabet "D" = D
toArpabet "DH" = DH
toArpabet "DX" = DX
toArpabet "EL" = EL
toArpabet "EM" = EM
toArpabet "EN" = EN
toArpabet "F" = F
toArpabet "G" = G
toArpabet "HH" = HH
toArpabet "JH" = JH
toArpabet "K" = K
toArpabet "L" = L
toArpabet "M" = M
toArpabet "N" = N
toArpabet "NG" = NG
toArpabet "NX" = NX
toArpabet "P" = P
toArpabet "Q" = Q
toArpabet "R" = R
toArpabet "S" = S
toArpabet "SH" = SH
toArpabet "T" = T
toArpabet "TH" = TH
toArpabet "V" = V
toArpabet "W" = W
toArpabet "WH" = WH
toArpabet "Y" = Y
toArpabet "Z" = Z
toArpabet "ZH" = ZH
toArpabet x = error (show x ++ " not an ARPABET")

fromArpabet :: ARPABET -> String
fromArpabet = show 

