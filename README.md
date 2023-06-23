# Homophones
A Haskell library to validate and generate homophones. Also includes these miscellaneous features: Spoonerism and Syllable Splitting.

## Usage
Options for accents: None, Cockney, British (None defaults to Standard North American accent used by CMUdict)

### Main Functions
```haskell
$ ghci Homophones.hs

-- For true homophones only
ghci> isPureHomophone ["red"] ["read"] None
True

ghci> pureHomophones "rain"
["RAIN","RAINE","RAYNE","REIGN","REIN","REINE"]

-- Allows pure and fuzzy homophones
ghci> isHomophone ["pie","crust"] ["pike","rust"] British
True

ghci> homophones ["book"] All
["BALK","BOOCK","BOOK","BORCK","BORK","BOURQUE","BUTCH"]
```
### Miscellaneous Work
```haskell
-- Spoonerism
$ ghci Homophones.hs
ghci> spoonerism ("blushing","crow")
[("CRUSHING","BLEAU"),("CRUSHING","BLOW"),("CRUSHING","BLOWE")]

-- Splits words into syllables
$ ghci Syllables.hs
ghci> splitSyllables "colourful"
["co","lour","ful"]
```


