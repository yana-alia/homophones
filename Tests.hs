module Tests where

import TestSuite
import Homophone

{-
    Homophones based off of homophonesTable from Morse solver. 
    In cases where multiple words are homophones of each other, it has been
    split into pairs. E.g. , (["buy"], ["by"], ["bye"] is split into (["buy"], ["by"]) and
    (["by"], ["bye"]). (["buy"], ["bye"]) is implied by transitivity.
-}

homophonesTable
  = [ (["brood"], ["brewed"]) ==> True
    , (["bees"], ["bs"]) ==> True -- bs = multiple Bs (will not work with current method)
    , (["cruise"], ["crews"]) ==> True
    , (["de"], ["day"]) ==> True -- Latin
    , (["essay"], ["sa"]) ==> True
    , (["eyes"], ["is"]) ==> True -- expected pronunciation not in dict
    , (["ewes"], ["us"]) ==> True
    , (["oh"], ["o"]) ==> True
    , (["owe"], ["o"]) ==> True
    , (["owes"], ["os"]) ==> True
    , (["pea"], ["p"]) ==> True
    , (["pee"], ["p"]) ==> True
    , (["see"], ["c"]) ==> True
    , (["seedy"], ["cd"]) ==> True
    , (["te"], ["t"]) ==> True
    , (["tee"], ["t"]) ==> True
    , (["teas"], ["ts"]) ==> True
    , (["tees"], ["ts"]) ==> True
    , (["use"], ["us"]) ==> True
    , (["why"], ["y"]) ==> True
    , (["zee"], ["z"]) ==> True
    , (["ad"], ["add"]) ==> True
    , (["ail"], ["ale"]) ==> True
    , (["air"], ["heir"]) ==> True
    , (["aisle"], ["I'll"]) ==> True 
    , (["aisle"], ["isle"]) ==> True
    , (["allowed"], ["aloud"]) ==> True
    , (["alms"], ["arms"]) ==> True
    , (["altar"], ["alter"]) ==> True
    , (["arc"], ["ark"]) ==> True
    , (["are"], ["ar"]) ==> True
    , (["ah"], ["r"]) ==> True
    , (["aren't"], ["aunt"]) ==> True
    , (["ate"], ["eight"]) ==> True
    , (["auger"], ["augur"]) ==> True
    , (["aural"], ["oral"]) ==> True
    , (["awe"], ["oar"]) ==> True
    , (["awe"], ["or"]) ==> True
    , (["oar"], ["ore"]) ==> True
    , (["or"], ["ore"]) ==> True
    , (["axel"], ["axle"]) ==> True
    , (["aye"], ["eye"]) ==> True
    , (["eye"], ["i"]) ==> True
    , (["bail"], ["bale"]) ==> True
    , (["bait"], ["bate"]) ==> True
    , (["baize"], ["bays"]) ==> True
    , (["bald"], ["bawled"]) ==> True
    , (["ball"], ["bawl"]) ==> True
    , (["band"], ["banned"]) ==> True
    , (["bard"], ["barred"]) ==> True
    , (["bare"], ["bear"]) ==> True
    , (["baron"], ["barren"]) ==> True
    , (["base"], ["bass"]) ==> True
    , (["bay"], ["bey"]) ==> True
    , (["bazaar"], ["bizarre"]) ==> True
    , (["be"], ["bee"]) ==> True
    , (["be"], ["b"]) ==> True
    , (["beach"], ["beech"]) ==> True
    , (["bean"], ["been"]) ==> True
    , (["beat"], ["beet"]) ==> True
    , (["beau"], ["bow"]) ==> True
    , (["beer"], ["bier"]) ==> True
    , (["bel"], ["bell"]) ==> True
    , (["bell"], ["belle"]) ==> True
    , (["burn"], ["bern"]) ==> True
    , (["berry"], ["bury"]) ==> True
    , (["berth"], ["birth"]) ==> True
    , (["bight"], ["bite"]) ==> True
    , (["byte"], ["bite"]) ==> True
    , (["billed"], ["build"]) ==> True
    , (["blew"], ["blue"]) ==> True
    , (["bloc"], ["block"]) ==> True
    , (["boar"], ["bore"]) ==> True
    , (["board"], ["bored"]) ==> True
    , (["boarder"], ["border"]) ==> True
    , (["bold"], ["bowled"]) ==> True
    , (["boos"], ["booze"]) ==> True
    , (["born"], ["borne"]) ==> True
    , (["bough"], ["bow"]) ==> True
    , (["boy"], ["buoy"]) ==> True
    , (["brae"], ["bray"]) ==> True
    , (["brake"], ["break"]) ==> True
    , (["bread"], ["bred"]) ==> True
    , (["brews"], ["bruise"]) ==> True
    , (["bridal"], ["bridle"]) ==> True
    , (["broach"], ["brooch"]) ==> True
    , (["bur"], ["burr"]) ==> True
    , (["but"], ["butt"]) ==> True
    , (["buy"], ["by"]) ==> True
    , (["buy"], ["bye"]) ==> True
    , (["calendar"], ["calender"]) ==> True
    , (["call"], ["caul"]) ==> True
    , (["canvas"], ["canvass"]) ==> True
    , (["cast"], ["caste"]) ==> True
    , (["caster"], ["castor"]) ==> True
    , (["caught"], ["court"]) ==> True
    , (["caw"], ["corps"]) ==> True
    , (["core"], ["caw"]) ==> True
    , (["cede"], ["seed"]) ==> True
    , (["ceiling"], ["sealing"]) ==> True
    , (["cell"], ["sell"]) ==> True
    , (["censer"], ["censor"]) ==> True
    , (["censor"], ["sensor"]) ==> True
    , (["cent"], ["scent"]) ==> True
    , (["scent"], ["sent"]) ==> True
    , (["cereal"], ["serial"]) ==> True
    , (["check"], ["cheque"]) ==> True
    , (["choir"], ["quire"]) ==> True
    , (["chord"], ["cord"]) ==> True
    , (["cite"], ["sight"]) ==> True
    , (["cite"], ["site"]) ==> True
    , (["coal"], ["kohl"]) ==> True
    , (["coarse"], ["course"]) ==> True
    , (["colonel"], ["kernel"]) ==> True
    , (["conger"], ["conga"]) ==> True
    , (["complacent"], ["complaisant"]) ==> True
    , (["complement"], ["compliment"]) ==> True
    , (["coo"], ["coup"]) ==> True
    , (["council"], ["counsel"]) ==> True
    , (["creak"], ["creek"]) ==> True
    , (["crews"], ["cruise"]) ==> True
    , (["cue"], ["q"]) ==> True
    , (["cue"], ["kyu"]) ==> True
    , (["queue"], ["cue"]) ==> True
    , (["curb"], ["kerb"]) ==> True
    , (["dam"], ["damn"]) ==> True
    , (["days"], ["daze"]) ==> True
    , (["dear"], ["deer"]) ==> True
    , (["dee"], ["d"]) ==> True
    , (["descent"], ["dissent"]) ==> True
    , (["desert"], ["dessert"]) ==> True
    , (["dew"], ["due"]) ==> True
    , (["die"], ["dye"]) ==> True
    , (["discreet"], ["discrete"]) ==> True
    , (["dissent"], ["descent"]) ==> True
    , (["doe"], ["dough"]) ==> True
    , (["doh"], ["dough"]) ==> True
    , (["done"], ["dun"]) ==> True
    , (["douse"], ["dowse"]) ==> True
    , (["draft"], ["draught"]) ==> True
    , (["dual"], ["duel"]) ==> True
    , (["duct"], ["ducked"]) ==> True
    , (["earn"], ["urn"]) ==> True
    , (["ease"], ["ee"]) ==> True -- ee = multiple Es (will not work with current method)
    , (["ease"], ["es"]) ==> True 
    , (["ewe"], ["u"]) ==> True
    , (["yew"], ["you"]) ==> True
    , (["u"], ["yew"]) ==> True
    , (["ex"], ["x"]) ==> True
    , (["eye"], ["i"]) ==> True
    , (["faint"], ["feint"]) ==> True
    , (["fair"], ["fare"]) ==> True
    , (["farther"], ["father"]) ==> True
    , (["fate"], ["fete"]) ==> True
    , (["fay"], ["fey"]) ==> True
    , (["faze"], ["phase"]) ==> True
    , (["feat"], ["feet"]) ==> True
    , (["few"], ["phew"]) ==> True
    , (["fie"], ["phi"]) ==> True
    , (["find"], ["fined"]) ==> True
    , (["fir"], ["fur"]) ==> True
    , (["flair"], ["flare"]) ==> True
    , (["flaw"], ["floor"]) ==> True
    , (["flea"], ["flee"]) ==> True
    , (["flex"], ["flecks"]) ==> True
    , (["flew"], ["flu"]) ==> True
    , (["flu"], ["flue"]) ==> True
    , (["floe"], ["flow"]) ==> True
    , (["flour"], ["flower"]) ==> True
    , (["for"], ["fore"]) ==> True
    , (["for"], ["four"]) ==> True
    , (["foreword"], ["forward"]) ==> True
    , (["fort"], ["fought"]) ==> True
    , (["forth"], ["fourth"]) ==> True
    , (["foul"], ["fowl"]) ==> True
    , (["franc"], ["frank"]) ==> True
    , (["freeze"], ["frieze"]) ==> True
    , (["friar"], ["fryer"]) ==> True
    , (["gait"], ["gate"]) ==> True
    , (["gays"], ["gaze"]) ==> True
    , (["gee"], ["g"]) ==> True
    , (["genes"], ["jeans"]) ==> True
    , (["gild"], ["guild"]) ==> True
    , (["gilt"], ["guilt"]) ==> True
    , (["gnaw"], ["nor"]) ==> True
    , (["gneiss"], ["nice"]) ==> True
    , (["gorilla"], ["guerilla"]) ==> True
    , (["grate"], ["great"]) ==> True
    , (["greave"], ["grieve"]) ==> True
    , (["grisly"], ["grizzly"]) ==> True
    , (["groan"], ["grown"]) ==> True
    , (["guessed"], ["guest"]) ==> True
    , (["hail"], ["hale"]) ==> True
    , (["hair"], ["hare"]) ==> True
    , (["hall"], ["haul"]) ==> True
    , (["hangar"], ["hanger"]) ==> True
    , (["hart"], ["heart"]) ==> True
    , (["haw"], ["hoar"]) ==> True
    , (["haw"], ["whore"]) ==> True
    , (["hay"], ["hey"]) ==> True
    , (["heal"], ["heel"]) ==> True
    , (["heal"], ["hell"]) ==> True
    , (["hear"], ["here"]) ==> True
    , (["heard"], ["herd"]) ==> True
    , (["heel"], ["he'll"]) ==> True -- he'll is a contraction of he will
    , (["heroin"], ["heroine"]) ==> True
    , (["hew"], ["hue"]) ==> True
    , (["hi"], ["high"]) ==> True
    , (["higher"], ["hire"]) ==> True
    , (["him"], ["hymn"]) ==> True
    , (["ho"], ["hoe"]) ==> True
    , (["hoard"], ["horde"]) ==> True
    , (["hoarse"], ["horse"]) ==> True
    , (["holy"], ["wholly"]) ==> True
    , (["hour"], ["our"]) ==> True
    , (["i"], ["eye"]) ==> True
    , (["idle"], ["idol"]) ==> True
    , (["in"], ["inn"]) ==> True
    , (["indict"], ["indite"]) ==> True
    , (["its"], ["its"]) ==> True
    , (["jay"], ["j"]) ==> True
    , (["jewel"], ["joule"]) ==> True
    , (["key"], ["quay"]) ==> True
    , (["knave"], ["nave"]) ==> True
    , (["knead"], ["need"]) ==> True
    , (["knew"], ["new"]) ==> True
    , (["knight"], ["night"]) ==> True
    , (["knit"], ["nit"]) ==> True
    , (["knock"], ["nock"]) ==> True
    , (["knot"], ["not"]) ==> True
    , (["know"], ["no"]) ==> True
    , (["knows"], ["nose"]) ==> True
    , (["lac"], ["lack"]) ==> True
    , (["lade"], ["laid"]) ==> True
    , (["lain"], ["lane"]) ==> True
    , (["lam"], ["lamb"]) ==> True
    , (["laps"], ["lapse"]) ==> True
    , (["larva"], ["lava"]) ==> True
    , (["law"], ["lore"]) ==> True
    , (["lay"], ["ley"]) ==> True
    , (["lea"], ["lee"]) ==> True
    , (["leach"], ["leech"]) ==> True
    , (["lead"], ["led"]) ==> True
    , (["leak"], ["leek"]) ==> True
    , (["lean"], ["lien"]) ==> True
    , (["lessen"], ["lesson"]) ==> True
    , (["levee"], ["levy"]) ==> True
    , (["liar"], ["lyre"]) ==> True -- [L,AY,ER] [L,AY,R]
    , (["licence"], ["license"]) ==> True
    , (["licker"], ["liquor"]) ==> True
    , (["lie"], ["lye"]) ==> True
    , (["lieu"], ["loo"]) ==> True
    , (["links"], ["lynx"]) ==> True
    , (["lo"], ["low"]) ==> True
    , (["load"], ["lode"]) ==> True
    , (["loan"], ["lone"]) ==> True
    , (["locks"], ["lox"]) ==> True
    , (["loop"], ["loupe"]) ==> True
    , (["loot"], ["lute"]) ==> True
    , (["made"], ["maid"]) ==> True
    , (["mail"], ["male"]) ==> True
    , (["main"], ["mane"]) ==> True
    , (["maize"], ["maze"]) ==> True
    , (["mall"], ["maul"]) ==> True
    , (["manna"], ["manner"]) ==> True
    , (["mantel"], ["mantle"]) ==> True
    , (["mare"], ["mayor"]) ==> True
    , (["mark"], ["marque"]) ==> True
    , (["marshal"], ["martial"]) ==> True
    , (["marten"], ["martin"]) ==> True
    , (["maw"], ["more"]) ==> True
    , (["me"], ["mi"]) ==> True
    , (["mean"], ["mien"]) ==> True
    , (["meat"], ["meet"]) ==> True
    , (["meat"], ["mete"]) ==> True
    , (["medal"], ["meddle"]) ==> True
    , (["metal"], ["mettle"]) ==> True
    , (["meter"], ["metre"]) ==> True
    , (["might"], ["mite"]) ==> True
    , (["miner"], ["minor"]) ==> True
    , (["mind"], ["mined"]) ==> True
    , (["missed"], ["mist"]) ==> True
    , (["moat"], ["mote"]) ==> True
    , (["mode"], ["mowed"]) ==> True
    , (["moor"], ["more"]) ==> True
    , (["moose"], ["mousse"]) ==> True
    , (["morning"], ["mourning"]) ==> True
    , (["muscle"], ["mussel"]) ==> True
    , (["naval"], ["navel"]) ==> True
    , (["nay"], ["neigh"]) ==> True
    , (["new"], ["neu"]) ==> True
    , (["nigh"], ["nye"]) ==> True
    , (["none"], ["nun"]) ==> True
    , (["ode"], ["owed"]) ==> True
    , (["oh"], ["owe"]) ==> True
    , (["one"], ["won"]) ==> True
    , (["packed"], ["pact"]) ==> True
    , (["packs"], ["pax"]) ==> True
    , (["pail"], ["pale"]) ==> True
    , (["pain"], ["pane"]) ==> True
    , (["pair"], ["pare"]) ==> True
    , (["pair"], ["pear"]) ==> True
    , (["palate"], ["palette"]) ==> True
    , (["palette"], ["pallet"]) ==> True
    , (["pascal"], ["paschal"]) ==> True
    , (["patten"], ["pattern"]) ==> True
    , (["pause"], ["paws"]) ==> True
    , (["pores"], ["pours"]) ==> True
    , (["pause"], ["pours"]) ==> True
    , (["pawn"], ["porn"]) ==> True
    , (["pea"], ["pee"]) ==> True
    , (["pee"], ["p"]) ==> True
    , (["peace"], ["piece"]) ==> True
    , (["peak"], ["peek"]) ==> True
    , (["peak"], ["pique"]) ==> True
    , (["peal"], ["peel"]) ==> True
    , (["pedal"], ["peddle"]) ==> True
    , (["peer"], ["pier"]) ==> True
    , (["pi"], ["pie"]) ==> True
    , (["plain"], ["plane"]) ==> True
    , (["pleas"], ["please"]) ==> True
    , (["plum"], ["plumb"]) ==> True
    , (["pole"], ["poll"]) ==> True
    , (["practice"], ["practise"]) ==> True
    , (["praise"], ["prays"]) ==> True
    , (["praise"], ["preys"]) ==> True
    , (["principal"], ["principle"]) ==> True
    , (["profit"], ["prophet"]) ==> True
    , (["quarts"], ["quartz"]) ==> True
    , (["queue"], ["q"]) ==> True
    , (["rain"], ["reign"]) ==> True
    , (["rain"], ["rein"]) ==> True
    , (["raise"], ["rays"]) ==> True
    , (["raise"], ["rase"]) ==> True
    , (["rase"], ["raze"]) ==> True
    , (["raised"], ["razed"]) ==> True
    , (["rap"], ["wrap"]) ==> True
    , (["raw"], ["roar"]) ==> True
    , (["read"], ["reed"]) ==> True
    , (["read"], ["red"]) ==> True
    , (["real"], ["reel"]) ==> True
    , (["reek"], ["wreak"]) ==> True
    , (["rest"], ["wrest"]) ==> True
    , (["review"], ["revue"]) ==> True
    , (["right"], ["rite"]) ==> True
    , (["wright"], ["write"]) ==> True
    , (["wright"], ["rite"]) ==> True
    , (["ring"], ["wring"]) ==> True
    , (["road"], ["rode"]) ==> True
    , (["roe"], ["row"]) ==> True
    , (["role"], ["roll"]) ==> True
    , (["rood"], ["rude"]) ==> True
    , (["root"], ["route"]) ==> True
    , (["rose"], ["rows"]) ==> True
    , (["rota"], ["rotor"]) ==> True
    , (["rote"], ["wrote"]) ==> True
    , (["rough"], ["ruff"]) ==> True
    , (["rouse"], ["rows"]) ==> True
    , (["rung"], ["wrung"]) ==> True
    , (["rye"], ["wry"]) ==> True
    , (["spade"], ["spayed"]) ==> True
    , (["sale"], ["sail"]) ==> True
    , (["sane"], ["seine"]) ==> True
    , (["sauce"], ["source"]) ==> True
    , (["saw"], ["soar"]) ==> True
    , (["saw"], ["sore"]) ==> True
    , (["scene"], ["seen"]) ==> True
    , (["scent"], ["sent"]) ==> True
    , (["scull"], ["skull"]) ==> True
    , (["sea"], ["see"]) ==> True
    , (["see"], ["c"]) ==> True
    , (["seam"], ["seem"]) ==> True
    , (["sear"], ["seer"]) ==> True
    , (["seer"], ["sere"]) ==> True
    , (["seas"], ["sees"]) ==> True
    , (["sees"], ["seize"]) ==> True
    , (["sew"], ["so"]) ==> True
    , (["so"], ["sow"]) ==> True
    , (["shake"], ["sheikh"]) ==> True -- fuzzy score: 3
    , (["shear"], ["sheer"]) ==> True
    , (["shoe"], ["shoo"]) ==> True
    , (["sic"], ["sick"]) ==> True
    , (["side"], ["sighed"]) ==> True
    , (["sign"], ["sine"]) ==> True
    , (["sink"], ["synch"]) ==> True
    , (["slay"], ["sleigh"]) ==> True
    , (["sloe"], ["slow"]) ==> True
    , (["sole"], ["soul"]) ==> True
    , (["some"], ["sum"]) ==> True
    , (["son"], ["sun"]) ==> True
    , (["sort"], ["sought"]) ==> True
    , (["spa"], ["spar"]) ==> True -- [S,P,AA] [S,P,AA,R]
    , (["staid"], ["stayed"]) ==> True
    , (["stair"], ["stare"]) ==> True
    , (["stake"], ["steak"]) ==> True
    , (["stalk"], ["stork"]) ==> True -- [S,T,AO,K] [S,T,AO,R,K]
    , (["stationary"], ["stationery"]) ==> True
    , (["steal"], ["steel"]) ==> True
    , (["stile"], ["style"]) ==> True
    , (["storey"], ["story"]) ==> True
    , (["straight"], ["strait"]) ==> True
    , (["swayed"], ["suede"]) ==> True
    , (["sweet"], ["suite"]) ==> True
    , (["tacks"], ["tax"]) ==> True
    , (["tale"], ["tail"]) ==> True
    , (["talk"], ["torque"]) ==> True -- [T,AO,K] [T,AO,R,K]
    , (["taught"], ["taut"]) ==> True
    , (["taught"], ["tort"]) ==> True -- [T,AO,T] [T,AO,R,T]
    , (["te"], ["tea"]) ==> True
    , (["tea"], ["tee"]) ==> True
    , (["tee"], ["t"]) ==> True
    , (["team"], ["teem"]) ==> True
    , (["tear"], ["tier"]) ==> True
    , (["teas"], ["tease"]) ==> True
    , (["tenner"], ["tenor"]) ==> True
    , (["tern"], ["turn"]) ==> True -- [T,ER,N,Z] [T,ER,N]
    , (["there"], ["their"]) ==> True
    , (["their"], ["they're"]) ==> True
    , (["threw"], ["through"]) ==> True
    , (["throes"], ["throws"]) ==> True
    , (["throne"], ["thrown"]) ==> True
    , (["thyme"], ["time"]) ==> True
    , (["tic"], ["tick"]) ==> True
    , (["tide"], ["tied"]) ==> True
    , (["tie"], ["thai"]) ==> True
    , (["tire"], ["tyre"]) ==> True
    , (["to"], ["too"]) ==> True
    , (["too"], ["two"]) ==> True
    , (["toad"], ["toed"]) ==> True
    , (["toed"], ["towed"]) ==> True
    , (["told"], ["tolled"]) ==> True
    , (["tole"], ["toll"]) ==> True
    , (["tor"], ["tore"]) ==> True
    , (["tough"], ["tuff"]) ==> True
    , (["troop"], ["troupe"]) ==> True
    , (["tuba"], ["tuber"]) ==> True -- fuzzy score: 2
    , (["vain"], ["vane"]) ==> True
    , (["vane"], ["vein"]) ==> True
    , (["vale"], ["veil"]) ==> True
    , (["vial"], ["vile"]) ==> True -- [V,AY,AH,L] [V,AY,L]
    , (["wail"], ["wale"]) ==> True
    , (["wale"], ["whale"]) ==> True
    , (["wain"], ["wane"]) ==> True
    , (["waist"], ["waste"]) ==> True
    , (["wait"], ["weight"]) ==> True
    , (["waive"], ["wave"]) ==> True
    , (["war"], ["wore"]) ==> True
    , (["ware"], ["wear"]) ==> True
    , (["wear"], ["where"]) ==> True
    , (["warn"], ["worn"]) ==> True
    , (["watt"], ["what"]) ==> True
    , (["wax"], ["whacks"]) ==> True
    , (["way"], ["whey"]) ==> True
    , (["weigh"], ["way"]) ==> True
    , (["we"], ["wee"]) ==> True
    , (["we","will"], ["we'll"]) ==> False -- not actually a homophone. contraction
    , (["we'll"], ["wheel"]) ==> True
    , (["weak"], ["week"]) ==> True
    , (["we'd"], ["weed"]) ==> True
    , (["we'll"], ["wheel"]) ==> True
    , (["weather"], ["whether"]) ==> True
    , (["weir"], ["we're"]) ==> True
    , (["wet"], ["whet"]) ==> True
    , (["which"], ["witch"]) ==> True
    , (["whig"], ["wig"]) ==> True
    , (["while"], ["wile"]) ==> True
    , (["whine"], ["wine"]) ==> True
    , (["whirl"], ["whorl"]) ==> True
    , (["whirled"], ["world"]) ==> True
    , (["whit"], ["wit"]) ==> True
    , (["white"], ["wight"]) ==> True
    , (["who's"], ["whose"]) ==> True
    , (["woe"], ["whoa"]) ==> True
    , (["wood"], ["would"]) ==> True
    , (["why"], ["y"]) ==> True
    , (["yaw"], ["yore"]) ==> True -- British accent
    , (["yore"], ["your"]) ==> True
    , (["your"], ["you're"]) ==> True
    , (["yoke"], ["yolk"]) ==> True
    , (["you'll"], ["yule"]) ==> True
    ]

homTableNotInDict
  = [ (["accessary"], ["accessory"]) ==> True
    , (["bees"], ["bb"]) ==> True
    , (["cavy"], ["kv"]) ==> True
    , (["cayenne"], ["kn"]) ==> True
    , (["crude"], ["crewed"]) ==> True
    , (["cutie"], ["qt"]) ==> True
    , (["envy"], ["nv"]) ==> True
    , (["essen"], ["sn"]) ==> True
    , (["essex"], ["sx"]) ==> True
    , (["excel"], ["xl"]) ==> True
    , (["ells"], ["ll"]) ==> True
    , (["els"], ["ll"]) ==> True
    , (["exe"], ["x"]) ==> True
    , (["eyed"], ["ied"]) ==> True
    , (["kale"], ["kl"]) ==> True
    , (["kewpie"], ["qp"]) ==> True
    , (["meant"], ["ment"]) ==> True
    , (["peas"], ["ps"]) ==> True
    , (["pees"], ["ps"]) ==> True
    , (["seas"], ["cs"]) ==> True
    , (["sees"], ["cs"]) ==> True
    , (["teepee"], ["tp"]) ==> True
    , (["all"], ["awl"]) ==> True
    , (["auk"], ["orc"]) ==> True
    , (["away"], ["aweigh"]) ==> True
    , (["bark"], ["barque"]) ==> True
    , (["bitten"], ["bittern"]) ==> True
    , (["braid"], ["brayed"]) ==> True
    , (["braise"], ["brays"]) ==> True
    , (["brays"], ["braze"]) ==> True
    , (["buyer"], ["byre"]) ==> True
    , (["cheap"], ["cheep"]) ==> True
    , (["clack"], ["claque"]) ==> True
    , (["clew"], ["clue"]) ==> True
    , (["climb"], ["clime"]) ==> True
    , (["close"], ["cloze"]) ==> True
    , (["coign"], ["coin"]) ==> True
    , (["cops"], ["copse"]) ==> True
    , (["cousin"], ["cozen"]) ==> True
    , (["currant"], ["current"]) ==> True
    , (["cymbol"], ["symbol"]) ==> True
    , (["deviser"], ["divisor"]) ==> True
    , (["eery"], ["eyrie"]) ==> True
    , (["fah"], ["far"]) ==> True
    , (["fate"], ["phate"]) ==> True
    , (["faun"], ["fawn"]) ==> True
    , (["ferrule"], ["ferule"]) ==> True
    , (["file"], ["phial"]) ==> True
    , (["fizz"], ["phiz"]) ==> True
    , (["foaled"], ["fold"]) ==> True
    , (["furs"], ["furze"]) ==> True
    , (["galipot"], ["gallipot"]) ==> True
    , (["gallop"], ["galop"]) ==> True
    , (["gamble"], ["gambol"]) ==> True
    , (["giro"], ["gyro"]) ==> True
    , (["greys"], ["graze"]) ==> True
    , (["hed"], ["heed"]) ==> True
    , (["holey"], ["holy"]) ==> True
    , (["hockey"], ["oche"]) ==> True
    , (["ivy"], ["iv"]) ==> True
    , (["knob"], ["nob"]) ==> True
    , (["laager"], ["lager"]) ==> True
    , (["lase"], ["laze"]) ==> True
    , (["mask"], ["masque"]) ==> True
    , (["minor"], ["mynah"]) ==> True
    , (["od"], ["odd"]) ==> True
    , (["paten"], ["patten"]) ==> True
    , (["peke"], ["pique"]) ==> True
    , (["pearl"], ["purl"]) ==> True
    , (["pica"], ["pika"]) ==> True
    , (["place"], ["plaice"]) ==> True
    , (["poof"], ["pouffe"]) ==> True
    , (["quean"], ["queen"]) ==> True
    , (["raised"], ["rased"]) ==> True
    , (["retch"], ["wretch"]) ==> True
    , (["rheum"], ["room"]) ==> True
    , (["roo"], ["roux"]) ==> True
    , (["roo"], ["rue"]) ==> True
    , (["rondo"], ["rondeau"]) ==> True
    , (["saver"], ["savour"]) ==> True
    , (["satire"], ["satyr"]) ==> True
    , (["sign"], ["ssign"]) ==> True
    , (["swat"], ["swot"]) ==> True
    , (["tare"], ["tear"]) ==> True
    , (["terce"], ["terse"]) ==> True
    , (["ton"], ["tun"]) ==> True
    , (["trooper"], ["trouper"]) ==> True
    , (["wailer"], ["whaler"]) ==> True
    , (["wall"], ["waul"]) ==> True
    , (["wart"], ["wort"]) ==> True
    , (["we"], ["whee"]) ==> True
    , (["weal"], ["we'll"]) ==> True
    , (["wean"], ["ween"]) ==> True
    , (["weaver"], ["weever"]) ==> True
    , (["were"], ["whirr"]) ==> True
    , (["wheald"], ["wheeled"]) ==> True -- wheald is not in the dictionary
    ]
  
homophones
  = [ (["deign"], ["dane"]) ==> True
    ] 
multWords
  = [ (["in","close"], ["enclose"]) ==> True
    , (["add","hock"], ["ad-hoc"]) ==> True
    , (["high","jack"], ["hijack"]) ==> True
    ]
allTestCases
      = [ TestCase "Morse homophoneTable" (uncurry homophone) homophonesTable
        , TestCase "Pure homophones" (uncurry homophone) homophones
        , TestCase "Multi-word homophones" (uncurry homophone) multWords ]

runTests = mapM_ goTest allTestCases

main = do
    putStrLn "Starting..."
    time runTests
    putStrLn "Done."