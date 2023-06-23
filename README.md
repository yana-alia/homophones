# Homophones
A Haskell library to validate and generate homophones. Also includes these miscellaneous features: Spoonerism and Syllable Splitting.

## Usage
Options for accents: None, Cockney, British (None defaults to Standard North American accent used by CMUdict)

### Main Functions
```haskell
$ ghci Homophones.hs

-- For pure homophones only
-- isPureHomophone [String] [String] Accent
ghci> isPureHomophone ["red"] ["read"] None
True

-- pureHomophones String
ghci> pureHomophones "rain"
["RAIN","RAINE","RAYNE","REIGN","REIN","REINE"]

-- Allows pure and fuzzy homophones
-- isHomophone [String] [String] Accent
ghci> isHomophone ["pie","crust"] ["pike","rust"] British
True

-- homophones [String] Accent
ghci> homophones ["book"] All
["BALK","BOOCK","BOOK","BORCK","BORK","BOURQUE","BUTCH"]
```
### Miscellaneous Work
```haskell
-- Spoonerism
$ ghci Homophones.hs
-- spoonerism (String, String)
ghci> spoonerism ("blushing","crow")
[("CRUSHING","BLEAU"),("CRUSHING","BLOW"),("CRUSHING","BLOWE")]

-- Splits words into syllables
$ ghci Syllables.hs
-- splitSyllables String
ghci> splitSyllables "colourful"
["co","lour","ful"]
```


