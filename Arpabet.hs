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

   AA  AE  AH  AO  AW  AX  AXR  AY  EH  ER  EY  IH  IX  IY  OW  OY  UH  UW  UX  
AA  0   3   1   1   3   1    1   5   3   2   3   4   4   4   4   6   3   3   3
AE  3   0   3   3   3   3    3   4   1   3   2   3   3   3   3   5   3   3   3
AH  1   3   0   1   2   1    1   5   3   2   3   4   4   4   3   5   3   3   3
AO  1   3   1   0   3   1    1   5   3   2   3   4   4   4   4   6   3   3   3
AW  3   3   2   3   0   3    3   5   3   4   3   3   3   3   1   3   3   3   3
AX  1   3   1   1   3   0    1   5   3   2   3   4   4   4   4   6   3   3   3
AXR 1   3   1   1   3   1    0   5   3   1   3   3   3   3   4   6   3   3   3
AY  5   4   5   5   5   5    5   0   3   5   2   5   5   5   5   7   6   6   6
EH  3   1   3   3   3   3    3   3   0   3   1   3   3   3   3   5   3   3   3
ER  2   3   2   2   4   2    1   5   3   0   3   3   3   3   5   7   4   4   4
EY  3   2   3   3   3   3    3   2   1   3   0   3   3   3   3   5   4   4   4
IH  4   3   4   4   3   4    3   5   3   3   3   0   1   1   3   5   3   3   3
IX  4   3   4   4   3   4    3   5   3   3   3   1   0   1   3   5   3   3   3
IY  4   3   4   4   3   4    3   5   3   3   3   1   1   0   3   5   3   3   3
OW  4   3   3   4   1   4    4   5   3   5   3   3   3   3   0   2   3   3   3
OY  6   5   5   6   3   6    6   7   5   7   5   5   5   5   2   0   5   5   5
UH  3   3   3   3   3   3    3   6   3   4   4   3   3   3   3   5   0   1   1
UW  3   3   3   3   3   3    3   6   3   4   4   3   3   3   3   5   1   0   1
UX  3   3   3   3   3   3    3   6   3   4   4   3   3   3   3   5   1   1   0


     B  CH   D  DH  DX  EL  EM  EN   F   G  HH  JH   K   L   M   N  NG  NX   P   Q   R   S  SH   T  TH   V   W  WH   Y   Z  ZH
B    0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
CH inf   0 inf inf inf inf inf inf inf inf inf   3   3 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
D  inf inf   0   1   2 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   3   3 inf inf inf inf inf inf
DH inf inf   1   0   1 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   3   3 inf inf inf inf inf inf
DX inf inf   2   1   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   3   3 inf inf inf inf inf inf
EL inf inf inf inf inf   0 inf inf inf inf inf inf inf   1 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
EM inf inf inf inf inf inf   0 inf inf inf inf inf inf inf   1 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
EN inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf   1   2   3 inf inf inf inf inf inf inf inf inf inf inf inf inf
F  inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
G  inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
HH inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
JH inf   3 inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
K  inf   3 inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
L  inf inf inf inf inf   1 inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
M  inf inf inf inf inf inf   1 inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf
N  inf inf inf inf inf inf inf   1 inf inf inf inf inf inf inf   0   1   2 inf inf inf inf inf inf inf inf inf inf inf inf inf
NG inf inf inf inf inf inf inf   2 inf inf inf inf inf inf inf   1   0   1 inf inf inf inf inf inf inf inf inf inf inf inf inf
NX inf inf inf inf inf inf inf   3 inf inf inf inf inf inf inf   2   1   0 inf inf inf inf inf inf inf inf inf inf inf inf inf
P  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf inf
Q  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf inf
R  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf inf
S  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf inf inf inf
SH inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf inf   2   1
T  inf inf   3   3   3 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0   1 inf inf inf inf inf inf
TH inf inf   3   3   3 inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   1   0 inf inf inf inf inf inf
V  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0 inf inf inf inf inf
W  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0   1 inf inf inf
WH inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   1   0 inf inf inf
Y  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   0 inf inf
Z  inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   2 inf inf inf inf inf inf   0   1
ZH inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf inf   1 inf inf inf inf inf inf   1   0

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
       --        AA   AE   AH   AO   AW   AX  AXR   AY   EH   ER   EY   IH   IX   IY   OW   OY   UH   UW   UX    B   CH    D   DH   DX   EL   EM   EN    F    G   HH   JH    K    L    M    N   NG   NX    P    Q    R    S   SH    T   TH    V    W   WH    Y    Z   ZH
       val   = [  0,   3,   1,   1,   3,   1,   1, inf,   3,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  3,   0,   3,   3,   3,   3,   3,   4,   1,   3,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   0,   1,   2,   1,   1, inf,   3,   2, inf, inf, inf,   4, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   1,   0,   3,   1,   1, inf,   3,   2, inf, inf, inf, inf, inf, inf,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  3,   3,   2,   3,   0,   3,   3, inf,   3, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   1,   1,   3,   0,   1, inf,   3,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  1,   3,   1,   1,   3,   1,   0, inf,   3,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf,   4, inf, inf, inf, inf, inf,   0,   3, inf,   2, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  3,   1,   3,   3,   3,   3,   3,   3,   0,   3,   1,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                  2,   3,   2,   2, inf,   2,   1, inf,   3,   0,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,   1, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
                inf,   2, inf, inf, inf, inf, inf,   2,   1,   3,   0,   3,   3,   3, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf, inf,
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
fromArpabet AA = "AA"
fromArpabet AE = "AE"
fromArpabet AH = "AH"
fromArpabet AO = "AO"
fromArpabet AW = "AW"
fromArpabet AX = "AX"
fromArpabet AXR = "AXR"
fromArpabet AY = "AY"
fromArpabet EH = "EH"
fromArpabet ER = "ER"
fromArpabet EY = "EY"
fromArpabet IH = "IH"
fromArpabet IX = "IX"
fromArpabet IY = "IY"
fromArpabet OW = "OW"
fromArpabet OY = "OY"
fromArpabet UH = "UH"
fromArpabet UW = "UW"
fromArpabet UX = "UX"
fromArpabet B = "B"
fromArpabet CH = "CH"
fromArpabet D = "D"
fromArpabet DH = "DH"
fromArpabet DX = "DX"
fromArpabet EL = "EL"
fromArpabet EM = "EM"
fromArpabet EN = "EN"
fromArpabet F = "F"
fromArpabet G = "G"
fromArpabet HH = "HH"
fromArpabet JH = "JH"
fromArpabet K = "K"
fromArpabet L = "L"
fromArpabet M = "M"
fromArpabet N = "N"
fromArpabet NG = "NG"
fromArpabet NX = "NX"
fromArpabet P = "P"
fromArpabet Q = "Q"
fromArpabet R = "R"
fromArpabet S = "S"
fromArpabet SH = "SH"
fromArpabet T = "T"
fromArpabet TH = "TH"
fromArpabet V = "V"
fromArpabet W = "W"
fromArpabet WH = "WH"
fromArpabet Y = "Y"
fromArpabet Z = "Z"
fromArpabet ZH = "ZH"
