module Tests where

import System.IO.Unsafe
import qualified Data.Map as Map

import TestSuite
import Homophone


{-
    Homophones based off of homophonesTable from Morse solver. 
    In cases where multiple words are homophones of each other, it has been
    split into pairs. E.g. , (["buy"], ["by"], ["bye"] is split into (["buy"], ["by"]) and
    (["by"], ["bye"]). (["buy"], ["bye"]) is implied by transitivity.
-}

homophonesTable
  = [ (["brood"], ["brewed"], All) ==> True
    , (["bees"], ["bs"], All) ==> True -- bs = multiple Bs (will not work with current method)
    , (["cruise"], ["crews"], All) ==> True
    , (["de"], ["day"], All) ==> True -- Latin
    , (["essay"], ["sa"], All) ==> True
    , (["eyes"], ["is"], All) ==> True -- expected pronunciation not in dict
    , (["ewes"], ["us"], All) ==> True
    , (["oh"], ["o"], All) ==> True
    , (["owe"], ["o"], All) ==> True
    , (["owes"], ["os"], All) ==> True
    , (["pea"], ["p"], All) ==> True
    , (["pee"], ["p"], All) ==> True
    , (["see"], ["c"], All) ==> True
    , (["seedy"], ["cd"], All) ==> True
    , (["te"], ["t"], All) ==> True
    , (["tee"], ["t"], All) ==> True
    , (["teas"], ["ts"], All) ==> True
    , (["tees"], ["ts"], All) ==> True
    , (["use"], ["us"], All) ==> True
    , (["why"], ["y"], All) ==> True
    , (["zee"], ["z"], All) ==> True
    , (["ad"], ["add"], All) ==> True
    , (["ail"], ["ale"], All) ==> True
    , (["air"], ["heir"], All) ==> True
    , (["aisle"], ["I'll"], All) ==> True 
    , (["aisle"], ["isle"], All) ==> True
    , (["allowed"], ["aloud"], All) ==> True
    , (["alms"], ["arms"], All) ==> True
    , (["altar"], ["alter"], All) ==> True
    , (["arc"], ["ark"], All) ==> True
    , (["are"], ["ar"], All) ==> True
    , (["ah"], ["r"], All) ==> True
    , (["aren't"], ["aunt"], All) ==> True
    , (["ate"], ["eight"], All) ==> True
    , (["auger"], ["augur"], All) ==> True
    , (["aural"], ["oral"], All) ==> True
    , (["awe"], ["oar"], All) ==> True
    , (["awe"], ["or"], All) ==> True
    , (["oar"], ["ore"], All) ==> True
    , (["or"], ["ore"], All) ==> True
    , (["axel"], ["axle"], All) ==> True
    , (["aye"], ["eye"], All) ==> True
    , (["eye"], ["i"], All) ==> True
    , (["bail"], ["bale"], All) ==> True
    , (["bait"], ["bate"], All) ==> True
    , (["baize"], ["bays"], All) ==> True
    , (["bald"], ["bawled"], All) ==> True
    , (["ball"], ["bawl"], All) ==> True
    , (["band"], ["banned"], All) ==> True
    , (["bard"], ["barred"], All) ==> True
    , (["bare"], ["bear"], All) ==> True
    , (["baron"], ["barren"], All) ==> True
    , (["base"], ["bass"], All) ==> True
    , (["bay"], ["bey"], All) ==> True
    , (["bazaar"], ["bizarre"], All) ==> True
    , (["be"], ["bee"], All) ==> True
    , (["be"], ["b"], All) ==> True
    , (["beach"], ["beech"], All) ==> True
    , (["bean"], ["been"], All) ==> True
    , (["beat"], ["beet"], All) ==> True
    , (["beau"], ["bow"], All) ==> True
    , (["beer"], ["bier"], All) ==> True
    , (["bel"], ["bell"], All) ==> True
    , (["bell"], ["belle"], All) ==> True
    , (["burn"], ["bern"], All) ==> True
    , (["berry"], ["bury"], All) ==> True
    , (["berth"], ["birth"], All) ==> True
    , (["bight"], ["bite"], All) ==> True
    , (["byte"], ["bite"], All) ==> True
    , (["billed"], ["build"], All) ==> True
    , (["blew"], ["blue"], All) ==> True
    , (["bloc"], ["block"], All) ==> True
    , (["boar"], ["bore"], All) ==> True
    , (["board"], ["bored"], All) ==> True
    , (["boarder"], ["border"], All) ==> True
    , (["bold"], ["bowled"], All) ==> True
    , (["boos"], ["booze"], All) ==> True
    , (["born"], ["borne"], All) ==> True
    , (["bough"], ["bow"], All) ==> True
    , (["boy"], ["buoy"], All) ==> True
    , (["brae"], ["bray"], All) ==> True
    , (["brake"], ["break"], All) ==> True
    , (["bread"], ["bred"], All) ==> True
    , (["brews"], ["bruise"], All) ==> True
    , (["bridal"], ["bridle"], All) ==> True
    , (["broach"], ["brooch"], All) ==> True
    , (["bur"], ["burr"], All) ==> True
    , (["but"], ["butt"], All) ==> True
    , (["buy"], ["by"], All) ==> True
    , (["buy"], ["bye"], All) ==> True
    , (["calendar"], ["calender"], All) ==> True
    , (["call"], ["caul"], All) ==> True
    , (["canvas"], ["canvass"], All) ==> True
    , (["cast"], ["caste"], All) ==> True
    , (["caster"], ["castor"], All) ==> True
    , (["caught"], ["court"], All) ==> True
    , (["caw"], ["corps"], All) ==> True
    , (["core"], ["caw"], All) ==> True
    , (["cede"], ["seed"], All) ==> True
    , (["ceiling"], ["sealing"], All) ==> True
    , (["cell"], ["sell"], All) ==> True
    , (["censer"], ["censor"], All) ==> True
    , (["censor"], ["sensor"], All) ==> True
    , (["cent"], ["scent"], All) ==> True
    , (["scent"], ["sent"], All) ==> True
    , (["cereal"], ["serial"], All) ==> True
    , (["check"], ["cheque"], All) ==> True
    , (["choir"], ["quire"], All) ==> True
    , (["chord"], ["cord"], All) ==> True
    , (["cite"], ["sight"], All) ==> True
    , (["cite"], ["site"], All) ==> True
    , (["coal"], ["kohl"], All) ==> True
    , (["coarse"], ["course"], All) ==> True
    , (["colonel"], ["kernel"], All) ==> True
    , (["conger"], ["conga"], All) ==> True
    , (["complacent"], ["complaisant"], All) ==> True
    , (["complement"], ["compliment"], All) ==> True
    , (["coo"], ["coup"], All) ==> True
    , (["council"], ["counsel"], All) ==> True
    , (["creak"], ["creek"], All) ==> True
    , (["crews"], ["cruise"], All) ==> True
    , (["cue"], ["q"], All) ==> True
    , (["cue"], ["kyu"], All) ==> True
    , (["queue"], ["cue"], All) ==> True
    , (["curb"], ["kerb"], All) ==> True
    , (["dam"], ["damn"], All) ==> True
    , (["days"], ["daze"], All) ==> True
    , (["dear"], ["deer"], All) ==> True
    , (["dee"], ["d"], All) ==> True
    , (["descent"], ["dissent"], All) ==> True
    , (["desert"], ["dessert"], All) ==> True
    , (["dew"], ["due"], All) ==> True
    , (["die"], ["dye"], All) ==> True
    , (["discreet"], ["discrete"], All) ==> True
    , (["dissent"], ["descent"], All) ==> True
    , (["doe"], ["dough"], All) ==> True
    , (["doh"], ["dough"], All) ==> True
    , (["done"], ["dun"], All) ==> True
    , (["douse"], ["dowse"], All) ==> True
    , (["draft"], ["draught"], All) ==> True
    , (["dual"], ["duel"], All) ==> True
    , (["duct"], ["ducked"], All) ==> True
    , (["earn"], ["urn"], All) ==> True
    , (["ease"], ["ee"], All) ==> True -- ee = multiple Es (will not work with current method)
    , (["ease"], ["es"], All) ==> True -- [["IY","Z"]] [["EH","S"]]
    , (["ewe"], ["u"], All) ==> True
    , (["yew"], ["you"], All) ==> True
    , (["u"], ["yew"], All) ==> True
    , (["ex"], ["x"], All) ==> True
    , (["eye"], ["i"], All) ==> True
    , (["faint"], ["feint"], All) ==> True
    , (["fair"], ["fare"], All) ==> True
    , (["farther"], ["father"], All) ==> True
    , (["fate"], ["fete"], All) ==> True
    , (["fay"], ["fey"], All) ==> True
    , (["faze"], ["phase"], All) ==> True
    , (["feat"], ["feet"], All) ==> True
    , (["few"], ["phew"], All) ==> True
    , (["fie"], ["phi"], All) ==> True
    , (["find"], ["fined"], All) ==> True
    , (["fir"], ["fur"], All) ==> True
    , (["flair"], ["flare"], All) ==> True
    , (["flaw"], ["floor"], All) ==> True
    , (["flea"], ["flee"], All) ==> True
    , (["flex"], ["flecks"], All) ==> True
    , (["flew"], ["flu"], All) ==> True
    , (["flu"], ["flue"], All) ==> True
    , (["floe"], ["flow"], All) ==> True
    , (["flour"], ["flower"], All) ==> True
    , (["for"], ["fore"], All) ==> True
    , (["for"], ["four"], All) ==> True
    , (["foreword"], ["forward"], All) ==> True
    , (["fort"], ["fought"], All) ==> True
    , (["forth"], ["fourth"], All) ==> True
    , (["foul"], ["fowl"], All) ==> True
    , (["franc"], ["frank"], All) ==> True
    , (["freeze"], ["frieze"], All) ==> True
    , (["friar"], ["fryer"], All) ==> True
    , (["gait"], ["gate"], All) ==> True
    , (["gays"], ["gaze"], All) ==> True
    , (["gee"], ["g"], All) ==> True
    , (["genes"], ["jeans"], All) ==> True
    , (["gild"], ["guild"], All) ==> True
    , (["gilt"], ["guilt"], All) ==> True
    , (["gnaw"], ["nor"], All) ==> True
    , (["gneiss"], ["nice"], All) ==> True
    , (["gorilla"], ["guerilla"], All) ==> True
    , (["grate"], ["great"], All) ==> True
    , (["greave"], ["grieve"], All) ==> True
    , (["grisly"], ["grizzly"], All) ==> True
    , (["groan"], ["grown"], All) ==> True
    , (["guessed"], ["guest"], All) ==> True
    , (["hail"], ["hale"], All) ==> True
    , (["hair"], ["hare"], All) ==> True
    , (["hall"], ["haul"], All) ==> True
    , (["hangar"], ["hanger"], All) ==> True
    , (["hart"], ["heart"], All) ==> True
    , (["haw"], ["hoar"], All) ==> True
    , (["haw"], ["whore"], All) ==> True
    , (["hay"], ["hey"], All) ==> True
    , (["heal"], ["heel"], All) ==> True
    , (["heal"], ["he'll"], All) ==> True
    , (["hear"], ["here"], All) ==> True
    , (["heard"], ["herd"], All) ==> True
    , (["heel"], ["he'll"], All) ==> True -- he'll is a contraction of he will
    , (["heroin"], ["heroine"], All) ==> True
    , (["hew"], ["hue"], All) ==> True
    , (["hi"], ["high"], All) ==> True
    , (["higher"], ["hire"], All) ==> True
    , (["him"], ["hymn"], All) ==> True
    , (["ho"], ["hoe"], All) ==> True
    , (["hoard"], ["horde"], All) ==> True
    , (["hoarse"], ["horse"], All) ==> True
    , (["holy"], ["wholly"], All) ==> True
    , (["hour"], ["our"], All) ==> True
    , (["i"], ["eye"], All) ==> True
    , (["idle"], ["idol"], All) ==> True
    , (["in"], ["inn"], All) ==> True
    , (["indict"], ["indite"], All) ==> True
    , (["its"], ["its"], All) ==> True
    , (["jay"], ["j"], All) ==> True
    , (["jewel"], ["joule"], All) ==> True
    , (["key"], ["quay"], All) ==> True
    , (["knave"], ["nave"], All) ==> True
    , (["knead"], ["need"], All) ==> True
    , (["knew"], ["new"], All) ==> True
    , (["knight"], ["night"], All) ==> True
    , (["knit"], ["nit"], All) ==> True
    , (["knock"], ["nock"], All) ==> True
    , (["knot"], ["not"], All) ==> True
    , (["know"], ["no"], All) ==> True
    , (["knows"], ["nose"], All) ==> True
    , (["lac"], ["lack"], All) ==> True
    , (["lade"], ["laid"], All) ==> True
    , (["lain"], ["lane"], All) ==> True
    , (["lam"], ["lamb"], All) ==> True
    , (["laps"], ["lapse"], All) ==> True
    , (["larva"], ["lava"], All) ==> True
    , (["law"], ["lore"], All) ==> True
    , (["lay"], ["ley"], All) ==> True
    , (["lea"], ["lee"], All) ==> True
    , (["leach"], ["leech"], All) ==> True
    , (["lead"], ["led"], All) ==> True
    , (["leak"], ["leek"], All) ==> True
    , (["lean"], ["lien"], All) ==> True
    , (["lessen"], ["lesson"], All) ==> True
    , (["levee"], ["levy"], All) ==> True
    , (["liar"], ["lyre"], All) ==> True -- [L,AY,ER] [L,AY,R]
    , (["licence"], ["license"], All) ==> True
    , (["licker"], ["liquor"], All) ==> True
    , (["lie"], ["lye"], All) ==> True
    , (["lieu"], ["loo"], All) ==> True
    , (["links"], ["lynx"], All) ==> True
    , (["lo"], ["low"], All) ==> True
    , (["load"], ["lode"], All) ==> True
    , (["loan"], ["lone"], All) ==> True
    , (["locks"], ["lox"], All) ==> True
    , (["loop"], ["loupe"], All) ==> True
    , (["loot"], ["lute"], All) ==> True
    , (["made"], ["maid"], All) ==> True
    , (["mail"], ["male"], All) ==> True
    , (["main"], ["mane"], All) ==> True
    , (["maize"], ["maze"], All) ==> True
    , (["mall"], ["maul"], All) ==> True
    , (["manna"], ["manner"], All) ==> True
    , (["mantel"], ["mantle"], All) ==> True
    , (["mare"], ["mayor"], All) ==> True
    , (["mark"], ["marque"], All) ==> True
    , (["marshal"], ["martial"], All) ==> True
    , (["marten"], ["martin"], All) ==> True
    , (["maw"], ["more"], All) ==> True
    , (["me"], ["mi"], All) ==> True
    , (["mean"], ["mien"], All) ==> True
    , (["meat"], ["meet"], All) ==> True
    , (["meat"], ["mete"], All) ==> True
    , (["medal"], ["meddle"], All) ==> True
    , (["metal"], ["mettle"], All) ==> True
    , (["meter"], ["metre"], All) ==> True
    , (["might"], ["mite"], All) ==> True
    , (["miner"], ["minor"], All) ==> True
    , (["mind"], ["mined"], All) ==> True
    , (["missed"], ["mist"], All) ==> True
    , (["moat"], ["mote"], All) ==> True
    , (["mode"], ["mowed"], All) ==> True
    , (["moor"], ["more"], All) ==> True
    , (["moose"], ["mousse"], All) ==> True
    , (["morning"], ["mourning"], All) ==> True
    , (["muscle"], ["mussel"], All) ==> True
    , (["naval"], ["navel"], All) ==> True
    , (["nay"], ["neigh"], All) ==> True
    , (["new"], ["neu"], All) ==> True
    , (["nigh"], ["nye"], All) ==> True
    , (["none"], ["nun"], All) ==> True
    , (["ode"], ["owed"], All) ==> True
    , (["oh"], ["owe"], All) ==> True
    , (["one"], ["won"], All) ==> True
    , (["packed"], ["pact"], All) ==> True
    , (["packs"], ["pax"], All) ==> True
    , (["pail"], ["pale"], All) ==> True
    , (["pain"], ["pane"], All) ==> True
    , (["pair"], ["pare"], All) ==> True
    , (["pair"], ["pear"], All) ==> True
    , (["palate"], ["palette"], All) ==> True
    , (["palette"], ["pallet"], All) ==> True
    , (["pascal"], ["paschal"], All) ==> True
    , (["patten"], ["pattern"], All) ==> True
    , (["pause"], ["paws"], All) ==> True
    , (["pores"], ["pours"], All) ==> True
    , (["pause"], ["pours"], All) ==> True
    , (["pawn"], ["porn"], All) ==> True
    , (["pea"], ["pee"], All) ==> True
    , (["pee"], ["p"], All) ==> True
    , (["peace"], ["piece"], All) ==> True
    , (["peak"], ["peek"], All) ==> True
    , (["peak"], ["pique"], All) ==> True
    , (["peal"], ["peel"], All) ==> True
    , (["pedal"], ["peddle"], All) ==> True
    , (["peer"], ["pier"], All) ==> True
    , (["pi"], ["pie"], All) ==> True
    , (["plain"], ["plane"], All) ==> True
    , (["pleas"], ["please"], All) ==> True
    , (["plum"], ["plumb"], All) ==> True
    , (["pole"], ["poll"], All) ==> True
    , (["practice"], ["practise"], All) ==> True
    , (["praise"], ["prays"], All) ==> True
    , (["praise"], ["preys"], All) ==> True
    , (["principal"], ["principle"], All) ==> True
    , (["profit"], ["prophet"], All) ==> True
    , (["quarts"], ["quartz"], All) ==> True
    , (["queue"], ["q"], All) ==> True
    , (["rain"], ["reign"], All) ==> True
    , (["rain"], ["rein"], All) ==> True
    , (["raise"], ["rays"], All) ==> True
    , (["raise"], ["rase"], All) ==> True
    , (["rase"], ["raze"], All) ==> True
    , (["raised"], ["razed"], All) ==> True
    , (["rap"], ["wrap"], All) ==> True
    , (["raw"], ["roar"], All) ==> True
    , (["read"], ["reed"], All) ==> True
    , (["read"], ["red"], All) ==> True
    , (["real"], ["reel"], All) ==> True
    , (["reek"], ["wreak"], All) ==> True
    , (["rest"], ["wrest"], All) ==> True
    , (["review"], ["revue"], All) ==> True
    , (["right"], ["rite"], All) ==> True
    , (["wright"], ["write"], All) ==> True
    , (["wright"], ["rite"], All) ==> True
    , (["ring"], ["wring"], All) ==> True
    , (["road"], ["rode"], All) ==> True
    , (["roe"], ["row"], All) ==> True
    , (["role"], ["roll"], All) ==> True
    , (["rood"], ["rude"], All) ==> True
    , (["root"], ["route"], All) ==> True
    , (["rose"], ["rows"], All) ==> True
    , (["rota"], ["rotor"], All) ==> True
    , (["rote"], ["wrote"], All) ==> True
    , (["rough"], ["ruff"], All) ==> True
    , (["rouse"], ["rows"], All) ==> True
    , (["rung"], ["wrung"], All) ==> True
    , (["rye"], ["wry"], All) ==> True
    , (["spade"], ["spayed"], All) ==> True
    , (["sale"], ["sail"], All) ==> True
    , (["sane"], ["seine"], All) ==> True
    , (["sauce"], ["source"], All) ==> True
    , (["saw"], ["soar"], All) ==> True
    , (["saw"], ["sore"], All) ==> True
    , (["scene"], ["seen"], All) ==> True
    , (["scent"], ["sent"], All) ==> True
    , (["scull"], ["skull"], All) ==> True
    , (["sea"], ["see"], All) ==> True
    , (["see"], ["c"], All) ==> True
    , (["seam"], ["seem"], All) ==> True
    , (["sear"], ["seer"], All) ==> True
    , (["seer"], ["sere"], All) ==> True
    , (["seas"], ["sees"], All) ==> True
    , (["sees"], ["seize"], All) ==> True
    , (["sew"], ["so"], All) ==> True
    , (["so"], ["sow"], All) ==> True
    , (["shake"], ["sheikh"], All) ==> True -- fuzzy score: 3
    , (["shear"], ["sheer"], All) ==> True
    , (["shoe"], ["shoo"], All) ==> True
    , (["sic"], ["sick"], All) ==> True
    , (["side"], ["sighed"], All) ==> True
    , (["sign"], ["sine"], All) ==> True
    , (["sink"], ["synch"], All) ==> True
    , (["slay"], ["sleigh"], All) ==> True
    , (["sloe"], ["slow"], All) ==> True
    , (["sole"], ["soul"], All) ==> True
    , (["some"], ["sum"], All) ==> True
    , (["son"], ["sun"], All) ==> True
    , (["sort"], ["sought"], All) ==> True
    , (["spa"], ["spar"], All) ==> True -- [S,P,AA] [S,P,AA,R]
    , (["staid"], ["stayed"], All) ==> True
    , (["stair"], ["stare"], All) ==> True
    , (["stake"], ["steak"], All) ==> True
    , (["stalk"], ["stork"], All) ==> True -- [S,T,AO,K] [S,T,AO,R,K]
    , (["stationary"], ["stationery"], All) ==> True
    , (["steal"], ["steel"], All) ==> True
    , (["stile"], ["style"], All) ==> True
    , (["storey"], ["story"], All) ==> True
    , (["straight"], ["strait"], All) ==> True
    , (["swayed"], ["suede"], All) ==> True
    , (["sweet"], ["suite"], All) ==> True
    , (["tacks"], ["tax"], All) ==> True
    , (["tale"], ["tail"], All) ==> True
    , (["talk"], ["torque"], All) ==> True -- [T,AO,K] [T,AO,R,K]
    , (["taught"], ["taut"], All) ==> True
    , (["taught"], ["tort"], All) ==> True -- [T,AO,T] [T,AO,R,T]
    , (["te"], ["tea"], All) ==> True
    , (["tea"], ["tee"], All) ==> True
    , (["tee"], ["t"], All) ==> True
    , (["team"], ["teem"], All) ==> True
    , (["tear"], ["tier"], All) ==> True
    , (["teas"], ["tease"], All) ==> True
    , (["tenner"], ["tenor"], All) ==> True
    , (["tern"], ["turn"], All) ==> True -- [T,ER,N,Z] [T,ER,N]
    , (["there"], ["their"], All) ==> True
    , (["their"], ["they're"], All) ==> True
    , (["threw"], ["through"], All) ==> True
    , (["throes"], ["throws"], All) ==> True
    , (["throne"], ["thrown"], All) ==> True
    , (["thyme"], ["time"], All) ==> True
    , (["tic"], ["tick"], All) ==> True
    , (["tide"], ["tied"], All) ==> True
    , (["tie"], ["thai"], All) ==> True
    , (["tire"], ["tyre"], All) ==> True
    , (["to"], ["too"], All) ==> True
    , (["too"], ["two"], All) ==> True
    , (["toad"], ["toed"], All) ==> True
    , (["toed"], ["towed"], All) ==> True
    , (["told"], ["tolled"], All) ==> True
    , (["tole"], ["toll"], All) ==> True
    , (["tor"], ["tore"], All) ==> True
    , (["tough"], ["tuff"], All) ==> True
    , (["troop"], ["troupe"], All) ==> True
    , (["tuba"], ["tuber"], All) ==> True -- fuzzy score: 2
    , (["vain"], ["vane"], All) ==> True
    , (["vane"], ["vein"], All) ==> True
    , (["vale"], ["veil"], All) ==> True
    , (["vial"], ["vile"], All) ==> True -- [V,AY,AH,L] [V,AY,L]
    , (["wail"], ["wale"], All) ==> True
    , (["wale"], ["whale"], All) ==> True
    , (["wain"], ["wane"], All) ==> True
    , (["waist"], ["waste"], All) ==> True
    , (["wait"], ["weight"], All) ==> True
    , (["waive"], ["wave"], All) ==> True
    , (["war"], ["wore"], All) ==> True
    , (["ware"], ["wear"], All) ==> True
    , (["wear"], ["where"], All) ==> True
    , (["warn"], ["worn"], All) ==> True
    , (["watt"], ["what"], All) ==> True
    , (["wax"], ["whacks"], All) ==> True
    , (["way"], ["whey"], All) ==> True
    , (["weigh"], ["way"], All) ==> True
    , (["we"], ["wee"], All) ==> True
    , (["we","will"], ["we'll"], All) ==> False -- not actually a homophone. contraction
    , (["we'll"], ["wheel"], All) ==> True
    , (["weak"], ["week"], All) ==> True
    , (["we'd"], ["weed"], All) ==> True
    , (["we'll"], ["wheel"], All) ==> True
    , (["weather"], ["whether"], All) ==> True
    , (["weir"], ["we're"], All) ==> True
    , (["wet"], ["whet"], All) ==> True
    , (["which"], ["witch"], All) ==> True
    , (["whig"], ["wig"], All) ==> True
    , (["while"], ["wile"], All) ==> True
    , (["whine"], ["wine"], All) ==> True
    , (["whirl"], ["whorl"], All) ==> True
    , (["whirled"], ["world"], All) ==> True
    , (["whit"], ["wit"], All) ==> True
    , (["white"], ["wight"], All) ==> True
    , (["who's"], ["whose"], All) ==> True
    , (["woe"], ["whoa"], All) ==> True
    , (["wood"], ["would"], All) ==> True
    , (["why"], ["y"], All) ==> True
    , (["yaw"], ["yore"], All) ==> True -- British accent
    , (["yore"], ["your"], All) ==> True
    , (["your"], ["you're"], All) ==> True
    , (["yoke"], ["yolk"], All) ==> True
    , (["you'll"], ["yule"], All) ==> True
    ]

homTableNotInDict
  = [ (["accessary"], ["accessory"], All) ==> True
    , (["bees"], ["bb"], All) ==> True
    , (["cavy"], ["kv"], All) ==> True
    , (["cayenne"], ["kn"], All) ==> True
    , (["crude"], ["crewed"], All) ==> True
    , (["cutie"], ["qt"], All) ==> True
    , (["envy"], ["nv"], All) ==> True
    , (["essen"], ["sn"], All) ==> True
    , (["essex"], ["sx"], All) ==> True
    , (["excel"], ["xl"], All) ==> True
    , (["ells"], ["ll"], All) ==> True
    , (["els"], ["ll"], All) ==> True
    , (["exe"], ["x"], All) ==> True
    , (["eyed"], ["ied"], All) ==> True
    , (["kale"], ["kl"], All) ==> True
    , (["kewpie"], ["qp"], All) ==> True
    , (["meant"], ["ment"], All) ==> True
    , (["peas"], ["ps"], All) ==> True
    , (["pees"], ["ps"], All) ==> True
    , (["seas"], ["cs"], All) ==> True
    , (["sees"], ["cs"], All) ==> True
    , (["teepee"], ["tp"], All) ==> True
    , (["all"], ["awl"], All) ==> True
    , (["auk"], ["orc"], All) ==> True
    , (["away"], ["aweigh"], All) ==> True
    , (["bark"], ["barque"], All) ==> True
    , (["bitten"], ["bittern"], All) ==> True
    , (["braid"], ["brayed"], All) ==> True
    , (["braise"], ["brays"], All) ==> True
    , (["brays"], ["braze"], All) ==> True
    , (["buyer"], ["byre"], All) ==> True
    , (["cheap"], ["cheep"], All) ==> True
    , (["clack"], ["claque"], All) ==> True
    , (["clew"], ["clue"], All) ==> True
    , (["climb"], ["clime"], All) ==> True
    , (["close"], ["cloze"], All) ==> True
    , (["coign"], ["coin"], All) ==> True
    , (["cops"], ["copse"], All) ==> True
    , (["cousin"], ["cozen"], All) ==> True
    , (["currant"], ["current"], All) ==> True
    , (["cymbol"], ["symbol"], All) ==> True
    , (["deviser"], ["divisor"], All) ==> True
    , (["eery"], ["eyrie"], All) ==> True
    , (["fah"], ["far"], All) ==> True
    , (["fate"], ["phate"], All) ==> True
    , (["faun"], ["fawn"], All) ==> True
    , (["ferrule"], ["ferule"], All) ==> True
    , (["file"], ["phial"], All) ==> True
    , (["fizz"], ["phiz"], All) ==> True
    , (["foaled"], ["fold"], All) ==> True
    , (["furs"], ["furze"], All) ==> True
    , (["galipot"], ["gallipot"], All) ==> True
    , (["gallop"], ["galop"], All) ==> True
    , (["gamble"], ["gambol"], All) ==> True
    , (["giro"], ["gyro"], All) ==> True
    , (["greys"], ["graze"], All) ==> True
    , (["hed"], ["heed"], All) ==> True
    , (["holey"], ["holy"], All) ==> True
    , (["hockey"], ["oche"], All) ==> True
    , (["ivy"], ["iv"], All) ==> True
    , (["knob"], ["nob"], All) ==> True
    , (["laager"], ["lager"], All) ==> True
    , (["lase"], ["laze"], All) ==> True
    , (["mask"], ["masque"], All) ==> True
    , (["minor"], ["mynah"], All) ==> True
    , (["od"], ["odd"], All) ==> True
    , (["paten"], ["patten"], All) ==> True
    , (["peke"], ["pique"], All) ==> True
    , (["pearl"], ["purl"], All) ==> True
    , (["pica"], ["pika"], All) ==> True
    , (["place"], ["plaice"], All) ==> True
    , (["poof"], ["pouffe"], All) ==> True
    , (["quean"], ["queen"], All) ==> True
    , (["raised"], ["rased"], All) ==> True
    , (["retch"], ["wretch"], All) ==> True
    , (["rheum"], ["room"], All) ==> True
    , (["roo"], ["roux"], All) ==> True
    , (["roo"], ["rue"], All) ==> True
    , (["rondo"], ["rondeau"], All) ==> True
    , (["saver"], ["savour"], All) ==> True
    , (["satire"], ["satyr"], All) ==> True
    , (["sign"], ["ssign"], All) ==> True
    , (["swat"], ["swot"], All) ==> True
    , (["tare"], ["tear"], All) ==> True
    , (["terce"], ["terse"], All) ==> True
    , (["ton"], ["tun"], All) ==> True
    , (["trooper"], ["trouper"], All) ==> True
    , (["wailer"], ["whaler"], All) ==> True
    , (["wall"], ["waul"], All) ==> True
    , (["wart"], ["wort"], All) ==> True
    , (["we"], ["whee"], All) ==> True
    , (["weal"], ["we'll"], All) ==> True
    , (["wean"], ["ween"], All) ==> True
    , (["weaver"], ["weever"], All) ==> True
    , (["were"], ["whirr"], All) ==> True
    , (["wheald"], ["wheeled"], All) ==> True -- wheald is not in the dictionary
    ]

falsePositives
  = [ (["rockett"], ["racket"], All) ==> False
    , (["bach"], ["book"], All) ==> False
    , (["beak"], ["buck"], All) ==> False
    , (["build"], ["bowled"], All) ==> False
    , (["throb"], ["train"], All) ==> False
    , (["throng"], ["train"], All) ==> False
    , (["purse"], ["piece"], All) ==> False
    , (["right"], ["wrought"], All) ==> False
    , (["write"], ["route"], All) ==> False -- beyond this is taken from datamuse sl and their respective score
    , (["grief"], ["graph"], All) ==> False -- (100,95)
    , (["ball"], ["bill"], All) ==> False -- (100,95)
    , (["ball"], ["bail"], All) ==> False -- (100,95)
    , (["crazy"], ["arese"], All) ==> False -- (100,88)
    , (["dollar"], ["hollar"], All) ==> False -- (100,88)
    , (["dollar"], ["dealer"], All) ==> False -- (100,95)
    , (["coconut"], ["kitchenette"], All) ==> False -- (100,82)
    , (["eagle"], ["legal"], All) ==> False -- (100,90)
    , (["eagle"], ["oogle"], All) ==> False -- (100,93)
    , (["roar"], ["drawer"], All) ==> False -- (100,90)
    , (["brain"], ["brown"], All) ==> False -- (100,95)
    , (["topic"], ["tropic"], All) ==> False -- (100,90)
    , (["topic"], ["toothpick"], All) ==> False -- (100,85)
    , (["treat"], ["st"], All) ==> False -- (100,90)
    , (["elephant"], ["elegant"], All) ==> False -- (100,85)
    , (["scale"], ["school"], All) ==> False -- (100,97)
    , (["scale"], ["sail"], All) ==> False -- (100,90)
    , (["crack"], ["quack"], All) ==> False -- (100,90)
    , (["chair"], ["sheer"], All) ==> False -- (100,90)
    , (["chair"], ["teur"], All) ==> False -- (100,90)
    , (["fish"], ["fit"], All) ==> False -- (100,90)
    , (["fish"], ["fetch"], All) ==> False -- (100,92)
    , (["donut"], ["donate"], All) ==> False -- (100,95)
    , (["dance"], ["das"], All) ==> False -- (100,90)
    , (["money"], ["mummy"], All) ==> False -- (100,95)
    , (["crude"], ["crowd"], All) ==> False -- (100,97)
    , (["fine"], ["phone"], All) ==> False -- (100,95)
    , (["colour"], ["cull"], All) ==> False -- (100,90)
    , (["read"], ["rude"], All) ==> False -- (100,95)
    , (["rhode"], ["arrayed"], All) ==> False -- (100,95)
    , (["monkey"], ["mulkey"], All) ==> False -- (100,91)
    , (["camp"], ["clamp"], All) ==> False -- (100,90)
    , (["baby"], ["boobie"], All) ==> False -- (100,95)
    , (["child"], ["wilde"], All) ==> False -- (100,88)
    , (["child"], ["shield"], All) ==> False -- (100,93)
    , (["kill"], ["kyle"], All) ==> False -- (100,95)
    , (["lucky"], ["leaky"], All) ==> False -- (100,95)
    , (["lucky"], ["lucks"], All) ==> False -- (100,90)
    , (["lego"], ["logo"], All) ==> False -- (100,95)
    , (["buy"], ["boe"], All) ==> False -- (100,95)
    , (["cat"], ["kit"], All) ==> False -- (100,95)
    , (["cat"], ["cant"], All) ==> False -- (100,90)
    , (["cat"], ["cash"], All) ==> False -- (100,90)
    , (["stab"], ["steep"], All) ==> False -- (100,90)
    , (["nugget"], ["negate"], All) ==> False -- (100,92)
    , (["shock"], ["chic"], All) ==> False -- (100,95)
    ] 

multWords
  = [ (["in","close"], ["enclose"], All) ==> True
    , (["add","hock"], ["ad-hoc"], All) ==> True
    , (["high","jack"], ["hijack"], All) ==> True
    , (["mei","bee"], ["maybe"], All) ==> True
    , (["cray","z"], ["crazy"], All) ==> True
    , (["mech","op"], ["makeup"], All) ==> True
    , (["boar","inn"], ["boring"], All) ==> True
    , (["star","sheep"], ["starship"], All) ==> True
    , (["cannes","vess"], ["canvas"], All) ==> True
    , (["cree","maid"], ["cremate"], All) ==> True
    , (["e","z"], ["easy"], All) ==> True
    , (["s","x"], ["essex"], All) ==> True
    , (["x","l"], ["excel"], All) ==> True
    , (["k","n"], ["cayenne"], All) ==> True
    , (["hoe","leigh"], ["holy"], All) ==> True
    , (["marque","at"], ["market"], All) ==> True
    , (["tea","meade"], ["timid"], All) ==> True
    , (["x","s"], ["excess"], All) ==> True
    , (["m","t"], ["empty"], All) ==> True
    , (["n","t"], ["anti"], All) ==> True
    , (["i","v"], ["ivy"], All) ==> True
    , (["tied","marx"], ["tide","marks"], All) ==> True -- Guardian 24384 29 Across
    , (["seafarer"], ["see","fairer"], All) ==> True -- Guardian 24384 5 Down
    , (["aye","sup"], ["ice","up"], All) ==> True -- Guardian 25929 10 Across
    , (["candy","dates"], ["candidates"], All) ==> True -- Independent 10,735 14 Down
    , (["boo","k"], ["bouquet"], All) ==> True -- Guardian Cryptic N° 26,064 17 Down
    , (["you're","asia"], ["eurasia"], All) ==> True -- Guardian Quiptic 1,226 4 Down
    , (["to","kill","a"], ["tequila"], All) ==> True -- Guardian Cryptic N° 26,064 19 Down
    , (["home","spun"], ["holmes","pun"], All) ==> True -- Guardian Cryptic 28,692 16 Down (will pass if Z == S)
    , (["her","suit"], ["hirsute"], All) ==> True -- Guardian 23,993 15 Across
    , (["sioux","dough"], ["pseudo"], All) ==> True -- Guardian 25,512 1 Down
    , (["pike","rust"], ["pie","crust"], All) ==> True -- Guardian 25,512 7 Down
    , (["proper","gait"], ["propagate"], All) ==> True -- Guardian 25,512 15 Down
    , (["read","panned","a"], ["red","panda"], All) ==> True -- Guardian 25,512 8 Down
    , (["boos","crews"], ["booze","cruise"], All) ==> True -- Guardian Cryptic 28,754 9,3 Across
    , (["who","to"], ["hutu"], All) ==> True -- Guardian Cryptic 28,729 18 Across
    , (["or","lock"], ["oarlock"], All) ==> True -- Everyman 3,993 24 Across
    , (["sell","fish"], ["selfish"], All) ==> True -- Guardian Cryptic 29,070 10 Across
    , (["in","cider"], ["insider"], All) ==> True -- Guardian Cryptic 29,070 18 Down
    , (["can","ah","pee"], ["canopy"], All) ==> True
    , (["past","oral"], ["pastoral"], All) ==> True -- Fuzzy homophone. Remove "R"
    , (["x","l","anse"], ["excellence"], All) ==> True
    ]

{-# NOINLINE megaTest #-}
megaTest :: [(([String], [String], Accent), Bool)]
megaTest = map readMegaTest $ lines getFile
  where
    getFile = unsafePerformIO $ do
      readFile "MegaTest.txt"

    readMegaTest :: String -> (([String], [String], Accent), Bool)
    readMegaTest = read

allTestCases
      = [ TestCase "Morse homophoneTable" (uncurry3 isHomophone) homophonesTable
        , TestCase "Non-Homophones" (uncurry3 isHomophone) falsePositives
        , TestCase "Pure homophones" (uncurry3 isHomophone) megaTest
        , TestCase "Multi-word homophones" (uncurry3 isHomophone) multWords ]

runTests = mapM_ goTest allTestCases

main = do
    putStrLn "Starting..."
    time runTests
    putStrLn "Done."