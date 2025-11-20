extends GridContainer
var current_row := 0
var current_col := 0
var rows := 5
var cols := 5
var tiles := []
var words = [
"abase","abate","abbey","abbot","abhor","abide","abler","abode","abort","about",
"above","abuse","abyss","acorn","acrid","actor","acute","adage","adapt","adept",
"admin","admit","adobe","adopt","adore","adorn","adult","aegis","affix","afire",
"afoot","afoul","after","again","agape","agate","agent","agile","aging","aglow",
"agony","agora","agree","ahead","aider","aimed","aisle","alack","alarm","album",
"alder","alert","algae","algal","algid","alias","alibi","alien","align","alike",
"alive","alkyd","allay","alley","allot","allow","alloy","aloft","alone","along",
"aloof","aloud","alpha","altar","alter","amass","amaze","amber","amble","amend",
"amiss","amity","among","ample","amply","amuse","angel","anger","angle","angry",
"angst","anime","ankle","annex","annoy","annul","anode","antic","antre","anvil",
"apace","apart","aphid","aping","apnea","apple","apply","apron","arbor","ardor",
"arena","argon","argot","argue","arise","armed","armor","aroma","arose","array",
"arrow","arson","artsy","asana","ascot","ashen","aside","askew","aspen","aspic",
"assay","asset","aster","astir","asura","atlas","atoll","atone","attic","audio",
"audit","auger","aught","augur","aunty","aural","auric","avail","avert","avian",
"avoid","await","awake","award","aware","awash","awful","awoke","axial","axiom",
"axion","azure","babel","bacon","badge","badly","bagel","baggy","baker","baldy",
"baler","balky","bally","balmy","balsa","banal","bandy","banjo","banty","barge",
"baron","basal","basic","basil","basin","basis","baste","batch","bathe","baton",
"batty","bawdy","bayou","beach","beady","beard","beast","beech","beefy","befit",
"began","begat","beget","begin","begun","being","belay","belch","belie","belle",
"belly","below","bench","bendy","beret","berry","berth","beset","betel","bevel",
"bezel","bible","bicep","biddy","bigot","bilge","billy","binge","bingo","biome",
"birch","birth","bison","bitty","black","blade","blame","bland","blank","blare",
"blast","blaze","bleak","bleat","bleed","blend","bless","blimp","blind","blink",
"bliss","blitz","bloat","block","bloke","blond","blood","bloom","blown","bluff",
"blunt","blurb","blurt","blush","board","boast","bobby","bocci","bocce","bogey",
"boggy","bogus","boing","boink","boils","bolus","bombe","bongo","bonus","booby",
"books","booms","boomy","boost","booth","booty","boozy","borax","borne","bosom",
"bossy","botch","bough","boule","bound","bounty","bout","bower","boxer","brace",
"braid","brain","brake","brand","brash","brass","brave","bravo","brawl","brawn",
"bread","break","breed","briar","bribe","brick","bride","brief","brine","bring",
"brink","briny","brisk","broad","broil","broke","brood","brook","broom","broth",
"brown","brunt","brush","brute","buddy","budge","buggy","bugle","build","built",
"bulge","bulky","bully","bunch","bunny","burly","burnt","burst","bushy","busty",
"butch","butte","buxom","buyer","bylaw","cabin","cable","cacao","cache","cacti",
"caddy","cadet","cagey","cairn","camel","cameo","canal","candy","canny","canoe",
"canon","caper","caput","carat","cargo","carol","carry","carve","caste","catch",
"cater","catty","caulk","cause","cavil","cease","cedar","cello","cense","chafe",
"chaff","chain","chair","chalk","champ","chant","chaos","chape","charm","chart",
"chase","chasm","chass","chats","cheap","cheat","check","cheek","cheer","chefs",
"chess","chest","chewy","chick","chide","chief","child","chili","chill","chime",
"china","chirp","chirk","chive","chock","choir","choke","chord","chore","chose",
"chuck","chuff","chump","chunk","churn","chute","cider","cinch","circa","civic",
"civil","clack","claim","clamp","clang","clank","clash","clasp","class","clean",
"clear","cleat","cleft","clerk","click","cliff","climb","clime","cling","clink",
"cloak","clock","clone","close","cloth","cloud","clout","clown","cluck","clump",
"clung","coach","coast","cobra","cocoa","colon","color","comet","comic","comma",
"conch","condo","conic","copse","coral","corer","corny","couch","cough","could",
"count","coupe","court","coven","cover","covet","covey","cower","coyly","crack",
"craft","cramp","crane","crank","crash","crate","crave","crawl","craze","crazy",
"cream","credo","creek","creep","crest","crews","cried","crime","crimp","crisp",
"croak","crock","crone","crony","crook","cross","croup","crowd","crown","crude",
"cruel","crumb","crush","crust","crypt","cubic","cumin","curio","curly","curry",
"curse","curve","curvy","cuter","cyber","cycle","cynic","daddy","daily","dairy",
"daisy","dance","dandy","datum","daunt","dealt","death","debar","debug","debut",
"decaf","decal","decay","decor","decoy","decry","defer","deify","deign","deity",
"delay","delve","demon","demur","denim","dense","depot","depth","derby","desks",
"deter","detox","devil","diary","dicey","digit","dilly","dimly","diner","dingo",
"dingy","diode","dirty","ditch","ditto","ditty","diver","divot","dizzy","dodge",
"dodgy","dogma","doing","dolce","dolly","donor","donut","dopey","doubt","dough",
"douse","dowdy","dowel","downy","dowry","dozen","draft","drain","drake","drama",
"drank","drape","drawl","drawn","dread","dream","dress","dried","drier","drift",
"drill","drink","drive","droid","droll","drone","drool","droop","dross","drove",
"drown","druid","drunk","dryer","duchy","dully","dummy","dumpy","dunce","dusky",
"dusty","dutch","duvet","dwarf","dwell","dwelt","dying","eager","eagle","early",
"earth","easel","eaten","eater","ebony","edict","edify","eerie","egret","eight",
"eject","elate","elbow","elder","elect","elegy","elite","elope","elude","email",
"embed","ember","emcee","empty","enact","endow","endue","enemy","enjoy","ennui",
"ensue","enter","entry","envoy","epoch","epoxy","equal","equip","erase","erect",
"erode","error","erupt","essay","ether","ethic","ethos","ethyl","evade","event",
"every","evict","evoke","exact","exalt","excel","exert","exile","exist","expel",
"extol","extra","exude","exult","fable","facet","faint","fairy","faith","false",
"fancy","farce","fatal","fatty","fault","fauna","favor","feast","fecal","feign",
"fella","felon","femme","femur","fence","feral","ferry","fetal","fetch","fetid",
"fetus","fever","fewer","fiber","fibre","field","fiend","fiery","fifth","fifty",
"fight","filly","filmy","filth","final","finch","finer","first","fishy","fixer",
"fizzy","fjord","flack","flail","flair","flake","flame","flank","flare","flash",
"flask","fleet","flesh","flick","flier","fling","flint","flirt","float","flood",
"floor","flora","floss","flour","flout","flown","fluff","fluid","fluke","flume",
"flung","flunk","flush","flute","flyer","foamy","focal","focus","foggy","foist",
"folly","foray","force","forge","forgo","forte","forth","forty","forum","found",
"foyer","frail","frame","frank","fraud","freak","freed","freer","fresh","friar",
"fried","frill","frisk","fritz","frock","frond","front","frost","froth","frown",
"froze","fruit","fudge","fugue","fully","fungi","funky","funny","furor","furry",
"fused","fussy","fuzzy","gaffe","gaily","gamer","gamma","gamut","gassy","gaudy",
"gauge","gaunt","gauzy","gavel","gawky","gayer","gayly","gazer","gecko","geeky",
"geese","genie","genre","ghost","ghoul","giant","giddy","gipsy","girly","girth",
"given","giver","glade","gland","glare","glass","glaze","gleam","glean","glide",
"glint","glitz","gloat","globe","gloom","glory","gloss","glove","glued","glyph",
"gnash","gnome","godly","going","golly","goner","goody","gooey","goofy","goose",
"gorge","gouge","gourd","grace","grade","graft","grail","grain","grand","grant",
"grape","graph","grasp","grass","grate","grave","gravy","graze","great","greed",
"green","greet","grief","grill","grime","grimy","grind","gripe","groan","groin",
"groom","grope","gross","group","grout","grove","growl","grown","gruel","gruff",
"grunt","guard","guava","guess","guest","guide","guild","guile","guilt","guise",
"gulch","gully","gumbo","gummy","guppy","gusto","gusty","gypsy","habit","hairy",
"halve","handy","happy","hardy","harem","harpy","harry","harsh","haste","hasty",
"hatch","hater","haunt","haven","havoc","hazel","heady","heard","heart","heath",
"heave","heavy","hedge","hefty","heist","hello","hence","herbs","hilly","hinge",
"hippo","hippy","hitch","hoard","hoary","hobby","hogan","hoist","hokey","honey",
"honor","horde","horny","horse","hotel","hotly","hound","house","hovel","hover",
"howdy","human","humid","humor","humph","humus","hunch","hunky","hurry","husky",
"hussy","hutch","hydra","hyena","hymen","hyper","icily","icing","icons","ideal",
"idiom","idiot","idler","idyll","igloo","iliac","image","imbue","impel","imply",
"inane","inbox","incur","index","inept","inert","infer","ingot","inlay","inlet",
"inner","input","intro","ionic","irate","irony","islet","issue","itchy","ivory",
"jazzy","jelly","jerky","jetty","jewel","jiffy","jimmy","japan","jolly","joker",
"joust","judge","juice","juicy","jumbo","jumpy","junta","junto","juror","kappa",
"karma","kayak","kebab","kempt","ketch","kiddo","kilty","kinda","kinky","kiosk",
"kitty","knack","knave","knead","kneed","kneel","knelt","knife","knock","knoll",
"known","koala","krill","label","labor","laden","ladle","lager","lance","lanky",
"lapel","lapse","large","larva","lasso","latch","later","lathe","latte","laugh",
"layer","leach","leafy","league","leaky","leant","leapt","learn","leash","least",
"leave","ledge","leech","leery","lefty","legal","leggy","lemon","lemur","leper",
"level","lever","libel","liege","light","liken","lilac","limbo","limit","linen",
"liner","lingo","lipid","lithe","liver","livid","llama","loamy","loath","lobby",
"local","locus","lodge","lofty","logic","login","loopy","loose","lorry","loser",
"louse","lousy","loved","lover","lower","lowly","loyal","lucid","lucky","lumen",
"lumpy","lunar","lunch","lunge","lupus","lurch","lurid","lusty","lying","lymph",
"lynch","lyric","macro","magma","magic","magma","maize","major","maker","malar",
"maple","march","marry","marsh","mason","match","matey","mauve","maxim","maybe",
"mayor","mealy","meant","meaty","mecca","medal","media","medic","melee","melon",
"mercy","merge","merit","merry","metal","meter","metro","micro","midge","midst",
"might","milky","mimic","mince","miner","minim","minor","minty","minus","mirth",
"miser","missy","mocha","modal","model","modem","mogul","moist","molar","moldy",
"money","month","moody","moose","moral","moron","morph","mossy","motel","motif",
"motor","motto","moult","mound","mount","mourn","mouse","mousy","mouth","mover",
"movie","mower","mucky","mucus","muddy","mulch","mummy","munch","murky","mushy",
"music","musky","musty","myopy","naive","nanny","nasal","nasty","natal","naval",
"navel","needy","neigh","nerdy","nerve","never","newer","newly","nexus","nicer",
"niche","nifty","night","ninja","ninny","ninth","noble","nodal","noise","noisy",
"nomad","noose","north","nosey","notch","noted","novel","nudge","nurse","nutty",
"nylon","nymph","oaken","oasis","oaten","obese","occur","ocean","octal","octet",
"odder","oddly","offal","offer","often","olden","older","olive","omega","onion",
"onset","opera","opine","opium","optic","orbit","order","organ","other","otter",
"ought","ounce","outdo","outer","outgo","ovary","ovate","overt","owing","owner",
"oxide","ozone","paddy","pagan","paint","paler","palsy","panel","panic","pansy",
"papal","paper","parer","parka","parry","parse","party","pasta","paste","pasty",
"patch","patio","patsy","patty","pause","payee","payer","peace","peach","pearl",
"peaty","pecan","pedal","penal","pence","penne","penny","peony","perch","peril",
"perky","perry","pesto","petal","petty","phase","phone","phony","photo","piano",
"picky","piece","piety","piggy","pilot","pinch","piney","pinky","pinot","pinto",
"piper","pique","pitch","pithy","pivot","pixel","pizza","place","plaid","plain",
"plane","plank","plant","plate","plaza","plead","pleat","plied","plier","pluck",
"plumb","plume","plump","plunk","plush","poach","poesy","point","poise","poker",
"polar","polka","polyp","pooch","poppy","porch","poser","posit","posse","pouch",
"pound","power","prank","prate","prawn","preen","press","price","prick","pride",
"pried","prime","primo","print","prior","prism","privy","prize","probe","prone",
"prong","proof","prose","proud","prove","prowl","proxy","prude","prune","psalm",
"pubic","pudgy","puffy","pulpy","pulse","punch","pupil","puppy","puree","purer",
"purge","purse","pushy","putty","pygmy","quack","quail","quake","qualm","quark",
"quart","quash","quasi","queen","queer","quell","query","quest","queue","quick",
"quiet","quill","quilt","quirk","quite","quota","quote","rabbi","rabid","racer",
"radar","radio","rainy","raise","rajah","rally","ralph","ramen","ranch","randy",
"range","rapid","rarer","raspy","ratio","ratty","raven","rayon","razor","reach",
"react","ready","realm","rearm","rebar","rebel","rebid","rebus","rebut","recap",
"recur","recto","reedy","refer","refit","regal","rehab","reich","reign","relax",
"relay","relic","remit","renal","renew","repay","repel","reply","rerun","reset",
"resin","retch","retro","retry","reuse","revel","revue","rhino","rhyme","rider",
"ridge","rifle","right","rigid","riley","rinse","ripen","riper","risen","riser",
"risky","rival","river","rivet","roach","roast","robin","robot","rocky","rodeo",
"roger","rogue","roman","roost","rooty","roped","roper","rosin","rotor","rouge",
"rough","round","rouse","route","rover","rowdy","rower","royal","ruddy","ruder",
"rugby","ruler","rumba","rumor","runic","runny","rural","rusty","sadly","safer",
"saint","salad","salon","sandy","saner","sappy","sassy","satin","satyr","sauce",
"sauna","saute","savvy","scald","scale","scalp","scaly","scamp","scant","scare",
"scarf","scary","scene","scent","scion","scoff","scold","scoop","scoot","scope",
"score","scorn","scour","scout","scowl","scram","scrap","screw","scrub","scuba",
"scuff","seamy","seize","semen","sense","sepia","serif","serum","serve","setup",
"seven","sever","sewer","shack","shade","shady","shaft","shake","shako","shale",
"shall","shalt","shame","shank","shape","share","shark","sharp","shave","shawl",
"shear","sheen","sheep","sheer","sheet","sheik","shelf","shell","shied","shift",
"shine","shiny","shire","shirk","shirt","shoal","shock","shone","shook","shoot",
"shore","short","shout","shove","shown","showy","shrew","shrub","shrug","shuck",
"shunt","shush","shyly","siege","sieve","sigma","silky","silly","since","sinew",
"singh","singe","siren","sissy","sixth","sixty","skate","skier","skiff","skill",
"skimp","skirt","skulk","skull","skunk","slack","slain","slang","slant","slash",
"slate","slave","sleek","sleep","sleet","slept","slice","slick","slide","slime",
"slimy","sling","slink","sloop","slope","slosh","sloth","slump","slung","slunk",
"slurp","slush","slyly","smack","small","smart","smash","smear","smell","smelt",
"smile","smirk","smite","smith","smock","smoke","smoky","smote","snack","snail",
"snake","snaky","snare","snarl","sneak","sneer","snide","snipe","snoop","snoot",
"snore","snort","snout","snowy","snuck","snuff","soapy","sober","soggy","solar",
"solid","solve","sonar","sonic","sooth","sooty","sorry","sound","south","sower",
"space","spade","spank","spare","spark","spasm","spawn","speak","spear","speck",
"speed","spell","spelt","spend","spent","sperm","spice","spicy","spied","spiel",
"spike","spiky","spill","spilt","spine","spiny","spire","spite","splat","split",
"spoil","spoke","spoof","spook","spool","spoon","spore","sport","spout","spray",
"spree","sprig","spunk","spurn","spurt","squad","squat","squib","stack","staff",
"stage","staid","stain","stair","stake","stale","stalk","stall","stamp","stand",
"stank","stare","stash","state","stave","stead","steak","steal","steam","steed",
"steel","steep","steer","stein","stern","stick","stiff","still","stilt","sting",
"stint","stock","stoic","stoke","stole","stomp","stone","stony","stood","stool",
"stoop","store","stork","storm","story","stout","stove","strap","straw","stray",
"strip","strut","stuck","studs","study","stuff","stump","stung","stunk","stunt",
"style","suave","sugar","suing","suite","sulky","sully","sumac","sunny","super",
"supra","surer","surge","surly","sushi","swami","swamp","swarm","swash","swath",
"swear","sweat","sweep","sweet","swell","swept","swift","swill","swine","swing",
"swirl","swish","swoon","swoop","sword","sworn","synod","syrup","tabby","table",
"taboo","tacit","tacky","taffy","taint","taken","taker","tally","talon","tamer",
"tango","tangy","taper","tapir","tardy","tarot","tarry","taste","tasty","tatty",
"taunt","tawny","teach","teary","tease","teddy","teens","teeny","teeth","tempo",
"tenet","tenor","tense","tenth","tepid","terra","terse","testy","thank","theft",
"their","theme","there","these","theta","thick","thief","thigh","thing","think",
"third","thong","thorn","those","three","threw","throb","throw","thrum","thumb",
"thump","thyme","tiara","tibia","tidal","tiger","tight","tilde","timer","timid",
"tipsy","titan","tithe","title","toast","today","toddy","token","tonal","toner",
"tonic","tooth","topaz","topic","torch","torso","torus","total","totem","touch",
"tough","towel","tower","toxic","toxin","trace","track","tract","trade","trail",
"train","trait","tramp","traps","trash","tread","treat","trend","trial","tribe",
"trice","trick","tried","tripe","trite","troll","tromp","troop","trope","trout",
"trove","truce","truck","truer","truly","trump","trunk","trust","truth","tryst",
"tubal","tuber","tulip","tulle","tumor","tunic","turbo","tutor","twang","tweak",
"tweed","tweet","twice","twine","twirl","twist","twixt","tying","udder","ulcer",
"ultra","umbra","uncle","uncut","under","unfed","unfit","union","unite","unity",
"untie","until","unwed","unzip","upper","upset","urban","urine","usage","usher",
"using","usual","usurp","utile","utter","vague","valet","valid","valor","value",
"valve","vapid","vapor","vault","vaunt","vegan","venom","venal","venue","verge",
"verse","verso","verve","vicar","video","vigil","villa","vinyl","viola","viper",
"viral","virus","visit","visor","vista","vital","vivid","vixen","vocal","vodka",
"vogue","voice","voila","vomit","voter","vouch","vowel","vying","wacky","wafer",
"wager","wagon","waist","waive","waltz","warty","waste","watch","water","waver",
"waxen","weary","weave","wedge","weedy","weigh","weird","welcome","welsh",
"whack","whale","wharf","wheat","wheel","whelp","where","which","whiff","while",
"whine","whiny","whirl","whisk","white","whole","whoop","whose","widen","wider",
"widow","width","wield","wight","willy","wimpy","wince","winch","windy","wiser",
"witch","witty","woken","woman","women","woody","wooer","wooly","woozy","wordy",
"world","worry","worse","worst","worth","would","wound","woven","wrack","wrang",
"wrapt","wrath","wreak","wreck","wrest","wring","wrist","write","wrong","wrote",
"wrung","wryly","xenon","xerox","xylem","yacht","yearn","yeast","yield","young",
"youth","zesty","zonal","zebra","zesty","zippy","zonal","zoned","zoned"
]

var word = get_word()
# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	print("READY")
	focus_mode = Control.FOCUS_ALL
	set_process_input(true)
	grab_focus()
	for r in range(rows):
		tiles.append([])
		for c in range(cols):
			var i = r * cols + c
			var tile = get_child(i)
			tiles[r].append(tile)

func _input(event):
	if not(event is InputEventKey and event.pressed):
		return
			
	var key = event.as_text()
	if key.length() == 1 and key >= "A" and key <= "Z":
		add_letter(key.to_upper())
	
	elif event.keycode == KEY_BACKSPACE:
		delete_letter()
			
	elif event.keycode == KEY_ENTER:
		var guess
		guess = submit_word()
		end_game(check_guess(guess, word))
		
		
		
func submit_word() -> String:
	if(current_col < cols):
		print("Not Enough Letters!")
		return ""
	
	var guess :String = ""
	for c in range(cols):
		guess += tiles[current_row][c].get_node("Label").text
	
	print("Player guessed: ", guess)
	return guess
	
func add_letter(letter : String):
	if current_col >= cols:
		return
	
	tiles[current_row][current_col].get_node("Label").text = letter
	current_col += 1
	
func delete_letter():
	if current_col == 0:
		return
	
	current_col -= 1
	tiles[current_row][current_col].get_node("Label").text = ""
	
func get_word() -> String:
	var random_word = randi_range(0, 999) 
	return words[random_word]
	
func check_guess(guess: String, answer: String) -> int:
	if(guess.length() != 5):
		return 0
		
	var guess_char = guess.to_upper().split("")
	var answer_char = answer.to_upper().split("")
	var result := [ "gray", "gray", "gray", "gray", "gray" ]

	for i in range(5):
		if guess_char[i] == answer_char[i]:
			result[i] = "green"
			answer_char[i] = "*"
			
	for i in range(5):
		if result[i] == "green":
			continue
		var in_word = answer_char.find(guess_char[i])
		if(in_word) != -1:
			result[i] = "yellow"
			answer_char[in_word] = '*' 
	
	for col in range(5):
		var tile = tiles[current_row][col]
		var label = tile.get_node("Label")
		label.text = guess_char[col]
		if result[col] == "green":
			tile.modulate = Color(0.2, 0.7, 0.2)
		elif result[col] == "yellow":
			tile.modulate = Color(0.8, 0.8, 0.2)
		else: tile.modulate = Color(0.4, 0.4, 0.4)
	current_row += 1
	current_col = 0
	
	var count = 0
	for i in range(5):
		if(result[i] == "green"):
			count += 1
	
	if(count == 5):
		return 1
	else: 
		return 0	
func display_correct_answer():
	var label = get_node("../HBoxContainer2/AnswerLabel")
	label.text = "Answer: " + word.to_upper()
	
func end_game(correct : int):
	if(correct == 1):
		print("CORRECT GUESS, THE WORD IS: ", word)
	
	if(current_row >= 5):
		print("GAME OVER, THE WORD WAS: ", word)
		display_correct_answer()
		


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	pass
