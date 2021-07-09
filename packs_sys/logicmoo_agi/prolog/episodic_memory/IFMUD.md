# Marty's Interactive Fiction Engine in Prolog

Here is a prototype text adventure that I wrote in Prolog back in 2002-2004: 
prolog code files (uses SWI-Prolog, licensed under the GNU GPL V3)

In writing it, my first objective was to create a non-player character inside an adventure game that could do goal solving. 
I also was considering issues of how to design the simulation such that both human players and NPCs could interact with the 
simulation on equal footing: To that end, all outputs from the simulation are bi-lingual messages, marked-up-English and Logic.

I never got the goal solver entirely debugged, and have since found much more efficient algorithms. Prolog is such a one-trick horse, 
I've since switched to using inference engines inside other more multifaceted languages.


See:  http://www.lightlink.com/vulcan/adv/essay.html


# Douglas TODOs

Table of Contents
=================
* [Chapter 1 - Some Characteristics and Adjectives](#chapter-1---some-characteristics-and-adjectives)
 * [Section 1 - Physical truths](#section-1---physical-truths)
 * [Section 2 - Hunger](#section-2---hunger)
 * [Section 3 - Access and Preference of Objects](#section-3---access-and-preference-of-objects)
 * [Section 4 - Mood-related](#section-4---mood-related)
 * [Section 5 - Qualities Inherited From The Home-world](#section-5---qualities-inherited-from-the-home-world)
* [Chapter 2 - Creature Behavior](#chapter-2---creature-behavior)
 * [Section 1 - Some Phrasing Helps](#section-1---some-phrasing-helps)
 * [Section 2 - Creature Actions](#section-2---creature-actions)
 * [Section 3 - Every Turn Rules for Creature](#section-3---every-turn-rules-for-creature)
 * [Section 4 - Creature Reactions to Giving and Other Actions](#section-4---creature-reactions-to-giving-and-other-actions)
* [Chapter 3 - The Player's Clothing and Inventory](#chapter-3---the-players-clothing-and-inventory)
* [Chapter 4 - The Office](#chapter-4---the-office)
* [Chapter 8 - The Capsule](#chapter-8---the-capsule)
 * [Section 1 - Glue and Gluing](#section-1---glue-and-gluing)
 * [Section 2 - The Label](#section-2---the-label)
 * [Section 3 - Propelling](#section-3---propelling)
* [Chapter 9 - Other Ways of Interacting with the Creature](#chapter-9---other-ways-of-interacting-with-the-creature)
* [Chapter 10 - The Plot](#chapter-10---the-plot)
 * [Section 1 - Accounting for Time](#section-1---accounting-for-time)
 * [Section 2 - Major Plot Events](#section-2---major-plot-events)
 * [Section 3 - Final scene](#section-3---final-scene)
* [Chapter 11 - Help System](#chapter-11---help-system)
* [Chapter 12 - Testing - Not for Release](#chapter-12---testing---not-for-release)

"When in Rome 2: Far from Home" by Emily Short.

The story genre is "Science Fiction". The story headline is "A Puzzle Game in Five Brief Episodes". The story is episode 2 of "When in Rome". The story creation year is 2006. The story description is "Manhattan, 1954. 

You have just been brought on to do some special work for the Office of Alien Protocol. No one can know the truth -- that there are little green men on Earth. And blue. And purple. Even your mother thinks you've set yourself up as a Private Investigator.

The rest of the secret had better stay between you, your secretary Esther, and your autographed photo of Joe DiMaggio.

When in Rome is designed as a lunchtime game: there are five episodes, each of which may be played to a conclusion within about fifteen minutes, requiring no saves. A player may also try this episode several times if he wishes, as the puzzles are partially randomized."

Use no scoring, the serial comma and American dialect. Use MAX_ARRAYS of 1000. Use MAX_STATIC_DATA of 30000. Use MAX_VERBS of 200. Use MAX_INDIV_PROP_TABLE_SIZE  of 20000. Use MAX_SYMBOLS of 8000. Use full-length room descriptions.

Include Basic Screen Effects by Emily Short. Include Menus by Emily Short. Include Basic Help Menu by Emily Short. Include Locksmith by Emily Short. 

Release along with cover art, a website, a file of "Summary of Alien Characteristics by Habitat" called "Summary.pdf", and source text.


Part 1 - World Model

## Chapter 1 - Some Characteristics and Adjectives

### Section 1 - Physical truths

A thing can be stinky or inoffensive. [Stinky things will offend animals with strong normal senses of smell, but attract those from sulfurous or methane-heavy atmospheres.]

A thing can be explored or unexplored. A thing is usually unexplored. [Explored means that the creature has already interacted with it once; it is no longer as interesting for creatures to play with.]

A thing can be useful or pointless. A thing is usually useful. [Pointless means that the creature has tried to lift or eat the object and found it intractable; it will not try again.]

Toughness is a kind of value. A thing has toughness. The toughnesses are sturdy, leathery, and papery. A thing is usually sturdy. [Toughness indicates the susceptibility of a thing to being eaten or destroyed by a creature.]

Weight is a kind of value. The weights are light, medium-weight, or heavy. A thing has weight. A thing is usually light. [Weight indicates whether creatures of different strengths will be able to pick something up.]

An electric light is a kind of device. Carry out switching on an electric light: now the noun is lit. Carry out switching off an electric light: now the noun is unlit. Carry out someone trying switching on an electric light: now the noun is lit. Carry out someone trying switching off an electric light: now the noun is unlit.

Understand "shine [electric light]" as switching on. Instead of burning an electric light, try switching on the noun. 
Instead of turning the lamp, say "It is already positioned to shed the maximum amount of light over the room so that it is not possible to hide from it. This took some weeks of adjustments on your part, so you're disinclined to mess with it further."

Understand "point [something] at [something]" or "shine [something] at [something]" or "turn [something] toward/towards/at [something]" as orienting it toward. Orienting it toward is an action applying to two things. Carry out orienting something toward something: say "It's hard to see what good that would do.". Instead of orienting an electric light toward the creature: try turning the noun.

Definition: a person is other if it is not the player.

### Section 2 - Hunger

A person has a number called the last feed time. The last feed time of a person is usually -20.

A person has a number called metabolism. The metabolism of the player is 180. 

A person has a number called breath count. The breath count of a person is usually 1000.

Definition: a person is scaly if its metabolism is 10 or less. 

Definition: a person is furry if its metabolism is 15 or more.

Definition: a person is cold:
	if it is not scaly, no;
	if it is wearing the jacket, no;
	if the number of things worn by it is less than 4, yes;
	no.

Definition: a person is hungry if it was last fed more than metabolism of it minutes back.

Definition: a person is starving:
	if it lies within the pod and the life-support is stuck to the pod and the life-support is switched on, no;
	if it was last fed too long ago, yes;
	no.

Definition: a person is dying if it was last fed way too long ago.

To decide whether (critter - a person) was last fed more than (count - a number) minutes back:
	let X be the last feed time of the creature;
	let Y be the turn count - X;
	if Y > count, yes;
	no.
	
To decide whether (critter - a person) was last fed too long ago:
	let X be the last feed time of the creature;
	let count be the metabolism of the creature;
	let count be count times two;
	let Y be the turn count - X;
	if Y > count, yes;
	no.
	
To decide whether (critter - a person) was last fed way too long ago:
	let X be the last feed time of the creature;
	let multiplier be 5; 
	if the metabolism of the creature is less than ten, let the multiplier be 7;
	if the metabolism of the creature is greater than 30, let the multiplier be 3;
	let count be the metabolism of the creature; 
	let count be count times multiplier; 
	let Y be the turn count - X;
	if Y > count, yes;
	no.
	
To decide whether (critter - a person) is suffocating:
	if the breath count of the critter is 1000, no;
	if life-support is stuck to the egg and the critter is in the egg and life-support is switched on, no;
	yes.
	
Definition: a thing is delicious if it fits metabolic parameters.

To decide whether (item - a thing) fits metabolic parameters:
	if the item is leathery and the item is pointless, no;
	if the food of the creature is meaty and the item is fleshy, yes; 
	if the food of the creature is meaty and the item is a person, yes;
	if the food of the creature is wood-pulpy and the item is papery, yes;
	if the food of the creature is earthly and item is edible, yes;
	if the food of the creature is textile and the item is wearable, yes;
	no.
	
Food is a kind of value. The foods are textile, earthly, meaty, and wood-pulpy.

A thing can be fleshy or unfleshy. A thing is usually unfleshy.

A thing can be poisoned or safe. A thing is usually safe.
 	
### Section 3 - Access and Preference of Objects

Definition: a thing is available:
	if it is in a container which is carried by the player, no;
	if it is not worn by the player and it is not a person and it is not carried by the player, yes;
	no.

Definition: the fedora is available if it is not carried by someone.

To decide whether (item - a thing) interests (character - a person):  
	if the item is a person, no; 
	if the item is the pod and the creature is homebound, yes;
	if the item fits the pod and the creature is homebound, yes;
	if the character has the item:
		if the item is stinky and the odor sensitivity of the creature is inverse, yes;
		if the item is delicious, yes;
		no;
	if the character is acquisitive, yes;
	if the character is hungry and the item is delicious, yes; 
	if the character is cold and the item is wearable, yes;
	if the character is hostile and the item is papery, yes;
	if the character is hungry, no;
	if the odor sensitivity of the character is weak, no;
	if the item is stinky, yes;
	no. 

Desire relates a person (called X) to a thing (called Y) when Y interests X. The verb to want (he wants, they want, it is wanted) implies the desire relation. 
	
### Section 4 - Mood-related

Definition: a person is offended:
	if the odor sensitivity of it is weak, no;
	if the odor sensitivity of it is inverse, no;
	if it can touch a stinky thing, yes;
	no;

Definition: a person is uncomfortable:
	if it is hungry, yes; 
	if it is cold, yes;
	if it is offended, yes;
	if it is blinded, yes;
	no.

Definition: a person is comfortable if it is not uncomfortable.

Definition: a person is snappish if the mood of it is less than friendly and it is uncomfortable.

Definition: a person is light-sensitive if it lies beyond Saturn. Definition: a person is blinded if it is light-sensitive and it can see a lit device.

To decide whether (character - a person) lies beyond (place - a moon):
	if the system of the moon of the character is greater than place, yes;
	no. 

To decide whether (character - a person) lies nearer than (place - a moon):
	if the system of the moon of the character is less than place, yes;
	no. 
	
Mood is a kind of value. The moods are hostile, curious, friendly, and secretive. A person has a mood. A person is usually curious.

### Section 5 - Qualities Inherited From The Home-world

Odor sensitivity is a kind of value. The odor sensitivities are strong, weak, and inverse. A person has an odor sensitivity.

Gravity is a kind of value. The gravities are negligible, fractional, Marslike, Earthlike, and vast. A person has a gravity.

Speed is a kind of value. The speeds are slow, moderate, fast, and lightning. Intelligence is a kind of value. The intelligences are smart and stupid. 

Level is a kind of value. The levels are beginner, easy, medium, hard, expert, endgame, and boring. The current level is a level that varies.  

Moon is a kind of value. The moons are defined by the Table of Alien Characteristics. 

 
Table of Alien Characteristics
moon	attitude	nostrils	feed time	mass	difficulty	arms	dexterity	taste	brain	system	tattoo-mark
Mercury	hostile	weak	5	Marslike	beginner	4	slow	wood-pulpy	smart	Mercury	"*"
Venus	friendly	inverse	3	Earthlike	beginner	2	slow	textile	smart	Venus	"**"
Luna	friendly	weak	12	fractional	easy	4	moderate	earthly	stupid	Luna	"*** -"
Luna-X	hostile	weak	12	fractional	medium	4	moderate	earthly	stupid	Luna	"***' -"
Earth-X	hostile	strong	12	Earthlike	hard	2	fast	earthly	smart	Luna	"***'"
Mars	friendly	strong	15	Marslike	easy	2	fast	textile	smart	Mars	"****"
Deimos	friendly	weak	15	negligible	medium	2	fast	textile	smart	Mars	"**** -"
Phobos	hostile	weak	15	negligible	medium	2	lightning	textile	smart	Mars	"**** --"
Asteroids	hostile	weak	18	negligible	easy	4	slow	wood-pulpy	stupid	Asteroids	"- - -"
Jupiter	curious	strong	20	vast	medium	0	lightning	textile	stupid	Jupiter	"**** *"
Ganymede	curious	strong	20	fractional	easy	2	moderate	earthly	smart	Jupiter	"**** * -"
Callisto	curious	weak	20	fractional	easy	6	moderate	wood-pulpy	stupid	Jupiter	"**** * --"
Io	curious	inverse	10	negligible	hard	2	fast	wood-pulpy	smart	Jupiter	"**** * ---"
Europa	curious	strong	20	negligible	easy	1	fast	textile	stupid	Jupiter	"**** * ----"
Saturn	curious	strong	25	Earthlike	hard	0	lightning	textile	stupid	Saturn	"**** **"
Titan	curious	inverse	25	fractional	expert	2	moderate	wood-pulpy	smart	Saturn	"**** ** -"
Uranus	curious	inverse	30	Earthlike	medium	0	lightning	meaty	stupid	Uranus	"**** ***"
Oberon	curious	weak	30	negligible	boring	4	moderate	textile	smart	Uranus	"**** *** -"
Titania	curious	weak	30	negligible	boring	4	moderate	wood-pulpy	stupid	Uranus	"**** *** --"
Neptune	curious	inverse	35	Earthlike	expert	0	lightning	wood-pulpy	smart	Neptune	"**** ****"
Nereid	curious	weak	35	negligible	easy	2	slow	textile	smart	Neptune	"**** **** -"
Triton	curious	inverse	35	negligible	expert	4	fast	wood-pulpy	smart	Neptune	"**** **** --"
Pluto	curious	strong	45	negligible	hard	2	fast	textile	stupid	Pluto	"**** *****"
Charon	curious	weak	45	negligible	hard	2	fast	meaty	stupid	Pluto	"**** ***** -"


Definition: a person is tall if it is negligible or it is fractional.

Definition: a person is timid:
	if it is meaty, no;
	if it is curious and it is stupid and it is fast, yes;
	no.

Definition: a person is acquisitive if it is curious and it is stupid and it is moderate. 

Definition: a person is contrary if it is hostile and it is stupid.

Definition: a person is homebound:
	if it is friendly and it is smart, yes; 
	if it is playful and the instruction booklet is explored, yes; 
	no.

Definition: a person is playful: 
	if it is slothful, no; 
	if it is hostile, no;
	if it is secretive, no;
	if it is smart, yes;
	no.

Definition: a person is athletic if it passes training.

To decide whether (athlete - a person) passes training:
	if the speed of the athlete is less than fast, no;
	if athlete is vast, yes;
	if athlete is Earthlike, yes;
	no.
	
Definition: a person is slothful:
	if it is negligible and it is slow, yes;
	no.
 .


## Chapter 2 - Creature Behavior

### Section 1 - Some Phrasing Helps

To say forcefully:
	choose a random row in the Table of Forces;
	if the gravity of the creature is negligible, say "[smallest entry]";
	if the gravity of the creature is fractional, say "[small entry]";
	if the gravity of the creature is Marslike, say "[medium entry]";
	if the gravity of the creature is Earthlike, say "[large entry]";
	if the gravity of the creature is vast, say "[huge entry]";
	
Table of Forces
smallest	small	medium	large	huge
"very softly"	"softly"	"with moderate force"	"strongly"	"quite forcefully"
"with considerable effort"	"with effort"	"easily"	"forcefully"	"quite forcefully"

To say quickly:
	choose a random row in the Table of Speed Adverbs;
	if the creature is not moderate, say " ";
	if the creature is fidgety, say "[quickly entry]"; 
	if the creature is slow, say "[slowly entry]";

Table of Speed Adverbs
quickly	slowly
"rapidly"	"ponderously"
"quickly"	"slowly"
"swiftly"	"laboriously"

### Section 2 - Creature Actions

Procedural rule:
	if the person asked is not the player, substitute the animal feeding rule for the can't eat unless edible rule.
	
Procedural rule:
	if the person asked is not the player, ignore the can't take people's possessions rule.
	
Instead of asking a creature to try doing something when the person asked is in a closed container:
	say "The creature watches your mouth move, fascinated, but it obviously cannot hear you." 
	
Before printing the name of the creature:
	if a random chance of 1 in 4 succeeds:
		if the creature is scaly, say "scaly ";
		if the creature is furry, say "furry "; 
		say "[color of the creature] ".
	

Rule for printing the name of the creature:
	if a random chance of 1 in 2 succeeds, say "creature";
	otherwise say "Visitor".

This is the animal feeding rule:
	if the noun is not delicious, stop the action.
	
A person can be active or passive.

[After the creature trying doing something: now the creature is passive; continue the action.]

Before a creature trying taking something: change containment context to the holder of the noun.

Containment context is a thing that varies. 

Heart's desire is a thing that varies. Before a creature trying taking something, change heart's desire to the noun.

Report the creature trying taking something:
	if the creature is in the containment context and the containment context is a container,
		say "[The creature] picks up [the noun], now it is in [the containment context]." instead;
	otherwise say "[The creature] [if a random chance of 1 in 3 succeeds][forcefully] [end if][if containment context is a container]extracts[otherwise]picks up[end if] [the noun] from [if containment context is the location]the floor[otherwise][the containment context][end if]." instead.
	 
Report an acquisitive creature trying taking something:
	if the creature is in the containment context and the containment context is a container,
		say "[The creature] gleefully picks up [the noun] from [the containment context]." instead;
	otherwise say "[The creature] [if a random chance of 1 in 3 succeeds][forcefully] [end if][if containment context is a container]extracts[otherwise]acquires[end if] [the noun] from [if containment context is the location]the floor[otherwise][the containment context][end if], then recounts its possessions: [number of things carried by the creature in words]." instead.
	
Report a fidgety creature trying inserting something into something:
	say "[The creature] shoves [the noun] hastily into [the second noun]." instead.
	
Report an athletic creature trying inserting something into something:
	say "[The creature] [puts] [the noun] away in [the second noun]." instead.
	
Report a creature trying inserting something into something:
	say "[The creature] [puts] [the noun] into [the second noun]." instead.
	
To say puts:
	choose a random row in the Table of Insertion Words;
	say "[word entry]".
	
Table of Insertion Words
word
"puts"
"sticks"
"tucks"

Report the creature trying dropping something when the creature is athletic:
	say "[The creature] tosses aside [the noun]." instead.
	
Report the creature trying dropping a wearable thing when the number of portable things in the location is greater than 4:
	if the creature is in the location,
		say "[The creature] drops [the noun] beside [the random fixed in place thing in the location]." instead;
	otherwise say "[The creature] sets down [the noun] inside [the holder of the creature]." instead.
	
Report the creature trying closing the sack:
	say "[The creature] rolls up the top of the sack to discourage it from coming open again." instead.
	
Report the creature trying closing something:
	say "[The creature] [if a random chance of 1 in 3 succeeds]shuts[otherwise]closes[end if] [the noun]." instead.
	
Report the creature trying closing something which contains the creature:
	if the creature is timid, say "With a shy look at you, [the creature] ";
	otherwise say "[The creature] ";
	say "[if a random chance of 1 in 3 succeeds]shuts[otherwise]closes[end if] [the noun] on itself." instead.

Report the creature trying opening the probe kit:
	if the creature is tall, say "[The creature] opens [the noun], which is almost as tall as it is." instead;
	otherwise say "Standing on tiptoes, [the creature] just manages to undo the top flaps of [the noun] and get it open." instead.

Report the creature trying opening something:
	say "[The creature] opens [the noun]." instead.
	
Report the creature trying dropping something:
	say "[The creature] puts down [the noun] [if the holder of the creature is not the location]inside [the holder of the creature][otherwise]on the floor[end if][if the gravity of the creature is less than negligible and the creature is not weak], breathing quickly[end if]."  instead.
	
Report the creature trying dropping the pod:
	say "[The creature] sets down [the noun][if propulsion is part of the pod], balancing it on its propulsion unit[otherwise], which rocks unsteadily on the floor until [the creature] steadies it with one paw[end if]." instead.

Instead of the creature trying taking something when the number of things carried by the creature is the carrying capacity of the creature:
	let the new target be a random thing carried by the creature;
	if the creature is hungry and the creature carries a delicious thing (called the target):
		while the new target is the target:
			let the new target be a random thing carried by the creature;
	try the creature trying dropping the new target.

Instead of the creature trying taking something when the creature is in a container (called the playhouse):
	if the noun is in the playhouse, continue the action;
	otherwise try the creature trying exiting.

Instead of the creature trying taking something when the noun is in an enterable container (called the playhouse):
	if the creature is tall:
		if the creature is visible and the creature is not in the playhouse:
			say "[The person asked] reaches for [the noun] with long arms.";
		continue the action;
	if the creature is not in the playhouse:
		now the creature is passive;
		if the creature is visible:
			say "[The person asked] tries to reach into [the playhouse], but its arms are a bit short for its purposes. [run paragraph on]";
		if the creature carries the playhouse:
			try the creature trying dumping out the playhouse;
		otherwise:
			if the playhouse contains the pod, try the creature trying pushing the playhouse;
			otherwise try the creature trying entering the playhouse;
	otherwise:
		continue the action.
	

Instead of a creature trying entering something which contains the egg pod when the creature is not tall:
	if the noun is available:
		try the person asked trying pushing the noun;
	otherwise if the creature is visible:
		say "[The creature] looks at [the egg] in frustration.".
	
Instead of the creature trying entering something closed:
	try the creature trying opening the noun.
	
Instead of the creature trying entering something when the creature is not carrying the noun and the noun is not in the location:
	try the creature trying taking the noun.

Instead of the creature trying entering something when the noun is not available:
	try the creature trying taking the noun.
	
Instead of the creature trying entering something when the creature has the noun:
	try the creature trying dropping the noun.

Instead of a negligible creature trying eating a useful leathery thing:
	now the creature is passive;
	now the noun is pointless;
	if the creature is visible, say "[The creature] bites [the noun], but is too weak to tear off portions of its leathery substance."
	
Instead of a negligible creature trying eating a pointless leathery thing:
	try the creature trying dropping the noun.

Instead of the creature trying eating something which contains something (called the target):
	if the creature is not carrying the noun, continue the action;
	otherwise try the creature trying taking the target.

Instead of the creature trying eating something portable which is not carried by the creature:
	try the creature trying taking the noun.

Carry out the creature trying eating something:
	change the last feed time of the creature to the turn count;
	if the noun is poisoned and the creature is not wood-pulpy and the creature is negligible, now the creature is poisoned.
	
Report the creature trying eating a poisoned thing:
	say "[The creature] consumes [the noun], ";
	if the creature is negligible:
		if the creature is wood-pulpy:
			say "but even though it is small and light-structured, its unusual metabolism seems capable of dealing with the glue." instead;
		otherwise:
			say "and immediately its eyes fill with tears and it begins to cough. After a moment or two of this it slumps into an inactive state." instead;
	otherwise:
		say "but because of the relatively small amount of toxin relative to its body mass and sturdiness it does not suffer any visible ill effect." instead.
	
Report the creature trying eating something:
	say "[The creature] eats [the noun] in small neat bites, turning it in its paws as it goes[if the creature is comfortable]. At last it gives a pleased sigh[end if]." instead.
	
Report the creature trying eating the fireballs:
	say "[The creature] crams all the Fireballs into its mouth at once. Then its eyes go very large and it holds quite still, cheeks bulging, tears sliding down its face.
	
For a moment you wonder whether you're going to have to get Esther to call the Office for xenotoxicology help. But then it gulps, blinking." instead. 

Report a fast creature trying eating something edible:
	say "[The creature] gobbles up [the noun], scattering flecks everywhere." instead.

Report a fast creature trying eating something wearable:
	say "[The creature] swiftly tears [the noun] into shreds, then consumes the confetti." instead.
	
Report a lightning creature trying eating something: 
	say "[The creature] devours the whole of [the noun] in one lightning-fast bolt." instead.
	
Report a creature trying eating the socks:
	if the socks are poisoned, continue the action; 
	say "[The creature] chows down on your socks. Ew." instead.
	
Report the creature trying taking something stinky when the odor sensitivity of the creature is strong:
	now the creature is passive;
	say "[The creature] picks up [the noun]";
	let index be 0;
	repeat through Table of Stink Disgust:
		if a random chance of 1 in 3 succeeds:
			if index is 0, say ", "; 
			otherwise say " and ";
			say "[reaction entry]";
			increase index by 1;
			if index is 2:
				say "." instead;
	say "." instead.
	 
Table of Stink Disgust
reaction
"holding it at arm's length[if the gravity of the creature is negligible] (though this appears to tax all its feeble strength)[end if]"
"scowling with all the expression its little face can command"
"making gagging and choking noises"
"waving the other paw in front of its nose"
"giving you a look as though to say it can't believe you have such things on your uncivilized planet"
	
Report the creature trying taking something stinky when the odor sensitivity of the creature is inverse:
	now the creature is passive;
	say "[The creature] picks up [the noun] and sniffs it with obvious pleasure." instead.
	
	
Report the creature trying closing a container which contains a stinky thing when the odor sensitivity of the creature is strong:
	say "[The creature] [if the gravity of the creature is Earthlike]slams[otherwise]closes[end if] [the noun] [forcefully]." instead.
	
[After creature trying closing a suspect container for the third time:
	say "[The creature] shoves [the noun] shut [forcefully], and glares at you for emphasis." ]
	
Report the creature trying closing the egg pod when the player is in the pod and the pod was closed:
	say "[The creature] tsks, and shuts [the pod] again." instead.

Before a creature trying closing something which is not available:
	if the creature is in the noun, continue the action;
	try the person asked trying taking the noun instead.
 
A procedural rule: ignore the can't take people's possessions rule.

Instead of taking something which is worn by the creature when the speed of the creature is lightning:
	now the creature is passive;
	if the creature is visible, say "The creature dodges you with lightning speed and is at the far side of the room in a moment."
	
Instead of taking something worn by the creature when the creature is fast:
	now the creature is passive;
	if the creature is visible, say "You don't get close enough to the creature -- it moves away too fast."
	
Before taking something worn by the creature when the speed of the creature is moderate and the creature is friendly:
	now the creature is passive;
	say "The creature looks as though it might move away, but then decides to allow you to go ahead.";
	continue the action.
	 

Instead of taking something which is carried by the creature when the speed of the creature is lightning:
	now the creature is passive;
	say "You are no match for the creature's lightning reflexes, and never even get close to [the noun]." instead.
	
Instead of taking something carried by a slow creature:
	move the noun to the player;
	now the creature is passive;
	say "[The creature] tries to evade you, but its movements are really too slow." instead.
	
Instead of taking something carried by the creature when the speed of the creature is moderate: 
	now the creature is passive;
	if a random chance of 1 in 2 succeeds:
		move the noun to the player;		
		say "It's an even thing, with the creature's moderate reflexes, whether you're going to succeed or not, but you do manage to snatch [the noun] from it." instead;
	otherwise:
		say "You make a move towards [the noun]; [the creature] dodges. But not terribly quickly; with another try you might succeed. Moderate reflexes at best." instead.
	
Instead of taking something carried by a fast creature:
	now the creature is passive;
	if the creature is playful, say "You reach for [the noun], but the creature tosses it to another hand, winking at you. It's a quick thing, at least.";
	otherwise say "You try to retrieve [the noun], but the creature deftly moves it out of your reach. Fast reflex, there.".
	
[Instead of the passive creature trying doing something:
	stop the action.]

Definition: a container is suspect if the odor sensitivity of the creature is strong and it contains a stinky thing.

Before the hostile smart creature trying taking something which is not available when the creature can see a papery available thing (called the target): 
	if the creature can see the player, try the creature trying threatening the target for the noun  instead.
	
A thing can be threatened or unthreatened. A thing is usually unthreatened.

Threatening it for is an action applying to two things.

Instead of a creature trying threatening an unthreatened thing for something:
	now the noun is threatened;
	now the creature is passive;
	if a random chance of 1 in 2 succeeds,
		say "[The creature] gestures dire things it will do to [the noun] if it does not receive [the second noun].";
	otherwise
		say "[The creature] mimes tearing [the noun] to bits, then points at [the second noun].";

Carry out a creature trying threatening something for something:
	try the person asked trying attacking the noun instead.
	
[Must be a before or it will fail the basic accessibility rule]
Before the creature trying taking something which is in a closed container (called the protection):
	if the creature can touch the noun, continue the action;
	try the creature trying opening the protection instead.
	
Instead of the starving creature trying crying for the first time:
	now the creature is passive;
	if the creature is visible:
		if the creature can see a delicious thing (called target),
			say "[The creature] settles in one place, no longer strong enough to beg for [the target].";
		otherwise say "[The creature] settles in one place, no longer strong enough to beg for food.".

Instead of the starving creature trying crying:
	if the person asked is visible:
		if the creature can see a delicious thing (called target) and the creature is visible:
			if the target is the player:
				say "[The creature] stares hungrily in your direction.";
			otherwise:
				say "[The creature] eyes [the target][if the gravity of the creature is less than Earthlike] weakly, but does not move towards it[otherwise] wistfully[end if].";
		otherwise:
			 say "[The creature] whimpers in hunger.";
	now the creature is passive.
	
Before a fidgety hostile smart creature trying taking something which is carried by the player :
	silently try dropping the noun;
	if the player is not carrying the noun,
		say "[The creature] distracts you, causing you to drop [the noun]."

Instead of the creature trying taking something which is not available: 
	try the creature trying begging for the noun.
	

Reading is an action applying to one thing.

Carry out a creature trying reading:
	now the noun is explored.
	
Report a creature trying reading:
	say "[The person asked] [if a random chance of 1 in 2 succeeds]peruses[otherwise]thoughtfully studies[end if] [the noun]."
	
Report a creature trying reading the instruction booklet when the person asked is smart:
	say "[The person asked] studies the instruction booklet, taking in all the stages of assembly and pausing particularly at the diagram of the smiling alien sitting inside the pod.
	
It taps this image several times with one claw." instead.

Report a creature trying reading the paperwork:
	now the noun is unexplored;
	choose a random row in the Table of Alien Characteristics;
	if moon entry is Neptune:
		say "[The person asked] reads the report on Neptune, frowning. Then it reads again, looking from the report to you and back again. [italic type]Your reports are startlingly incomplete, Earthling[roman type], it thinks.";
	otherwise:
		say "[The person asked] studies the report on [moon entry][if a random chance of 1 in 3 succeeds], occasionally licking its lips thoughtfully[end if][if a random chance of 1 in 4 succeeds]. Softly it laughs at what it sees there[end if]." instead.

Forcing drop of is an action applying to one thing.

Instead of a creature trying forcing drop of something when the number of things carried by the person asked is 0:
	let the target be a random portable available thing which can be seen by the person asked;
	if the target is a thing, try the person asked trying taking the target instead.
 
Carry out a creature trying forcing drop of something:
	let the target be a random thing carried by the person asked;
	if the target is not a thing, stop the action;
	try the person asked trying forcing drop of the noun with the target.
	
Forcing drop of it with is an action applying to two things.

Carry out a creature trying forcing drop of something with something:
	move the second noun to the player;
	move the noun to the person asked

Report a creature trying forcing drop of something with something:
	say "[The person asked] chucks [the second noun] in your direction, causing you to drop [the noun]." instead.

Begging for is an action applying to one thing.

Carry out a creature trying begging for something: do nothing.

Before a creature trying begging for the player:
	if the player is in the pod, try the person asked trying entering the pod instead.
	 
Report a meaty creature trying begging for the player:
	if the holder of the person asked is the holder of the player,
		say "[The person asked] sneaks up close and [if a random chance of 1 in 2 succeeds]tries to take a bite from your calf[otherwise]licks your wrist[end if]."  instead;
	otherwise say "[The person asked] [if a random chance of 1 in 2 succeeds]makes bitey faces in your direction[otherwise]watches your movements with an unpleasant sort of hungry anticipation[end if]." instead.

Report a creature trying begging for something:
	if the noun is the person asked, say "[The person asked] squirms." instead;
	if the person asked is not in location, say "From [the holder of the person asked], [the person asked] reaches toward [the noun]." instead;
	if the person asked is timid, say "[The person asked] looks at [the noun] with large eyes but does not move, speak, or gesture." instead;
	choose a random row in the Table of Object Requests;
	if the creature is friendly or the creature is secretive, say "[template entry][paragraph break]";
	if the creature is curious, say "[curious request entry][paragraph break]";
	if the creature is hostile, say "[unfriendly request entry][paragraph break]";
	now the creature is passive.
	
Before a creature trying begging for something when the person asked lies within the noun:
	now the creature is passive;
	say "[The creature] looks queasy and gestures for you to set it down." instead.

	
Instead of a smart creature trying begging for something which is carried by the player:
	try the person asked trying forcing drop of the noun.
	
Instead of a fidgety creature trying begging for something when the player carries more than 4 things and the player carries the noun:
	if the person asked is not visible, stop the action;
	if the person asked is timid, continue the action;
	if the number of things carried by the person asked is the carrying capacity of the person asked, try the person asked trying dropping a random thing carried by the person asked instead;
	if the noun is the first thing held by the player, continue the action;
	move the noun to the person asked;
	say "[The person asked] extracts [the noun] from among your possessions, since you are holding so much and its reflexes are so good."
	
Instead of a vast creature trying begging for something:
	if the person asked is not visible, stop the action;
	now the person asked is passive;
	if the player wears the noun, say "[The person asked] tears [the noun] off of you. Gentle it isn't.";
	otherwise say "[The person asked] pulls [the noun] away from you[if the noun is papery], nearly ripping it in the process[end if].";
	move the noun to the person asked.
	
Instead of a vast creature trying begging for something for the first time:
	if the person asked is not visible, stop the action;
	now the person asked is passive;
	if the player wears the noun, say "[The person asked] strips [the noun] from your body in one rapid movement. Well. Pity Esther isn't in here.";
	otherwise say "[The person asked] unceremoniously wrests [the noun] from your grasp, being quite a lot stronger than you are.";
	move the noun to the person asked.
	
Table of Object Requests
template	unfriendly request	curious request
"[The creature] looks mournfully at [the noun]."	"[The creature] fixes its glare on [the noun]."	"[The creature] looks curiously at [the noun]."
"[The creature] points to [the noun] and then presses its... paws? together wistfully."	"[The creature] fixes its glare on [the noun]."	"[The creature] [if gravity of the creature is Earthlike or the gravity of the creature is vast]makes a spring for, and nearly catches, [the noun][otherwise]springs at [the noun], but not with nearly enough force[end if]."
"[The creature] begs for [the noun]."	"[The creature] growls, biting at [the noun]."	"[The creature] makes a motion as though to beg for [the noun]."
"[The creature] circles you slowly, pointing at [the noun]."	"[The creature] circles you angrily, pointing at [the noun]."	"[The creature] circles you, trying to get a clearer view of [the noun]."
"[The creature] extends its claws towards [the noun] beseechingly."	"[The creature] snatches at [the noun]."	"[The creature] reaches eagerly towards [the noun]."

Report a slothful creature trying begging for something:
	say "[The creature] whines for [the noun]." instead.


Growling is an action applying to nothing.

Carry out a creature trying growling: do nothing.

Report a creature trying growling: say "[The creature] growls."

After a creature trying growling for the first time: 
	if the player is in the Office,
		say "[The person asked] growls suddenly and savagely. You jump backward, banging into your desk. 
	
'Good to know you kids are getting along,' shouts Esther from the other room.";
	otherwise say "[The person asked] growls at you. Hostile, then. You back off warily.";
	now the creature is passive;
	stop the action.

Report a friendly creature trying growling:
	say "[The noun] shows teeth, but in such a way that you know it doesn't mean anything serious by it."
	 
Shivering is an action applying to nothing.

Carry out a creature shivering: now the person asked is passive.

Report a creature shivering: 
	if the creature can see a lit thing (called the target), 
		say "[The creature] shivers and draws closer to [the target].";
	otherwise 
		say "[The creature] [if a random chance of 1 in 2 succeeds]rubs its paws over its arms[otherwise]shivers[end if]."


Rejecting is an action applying to one thing.

Carry out a creature trying rejecting something: now the noun is explored.

Report a creature trying rejecting: say "[The person asked] turns its head away." instead.

Report a strong creature trying rejecting a stinky thing: say "[The person asked] covers its nostrils and draws back." instead.

Report an inverse creature trying rejecting a stinky thing: say "[The person asked] sniffs wistfully, obviously drawn by the smell, but then shakes its head." instead.

Instead of a playful creature trying rejecting an unexplored papery thing which is not the sack:
	try the person asked trying accepting the noun;
	try the person asked trying playing with the noun.
	
Report a playful creature trying rejecting an explored papery thing which is not the sack:
	say "[The person asked] rolls its eyes and, with an exaggerated show of patience, looks over [the noun] again, muttering to itself. Apparently it has already committed to memory everything it finds interesting here." instead.

Report a playful creature trying rejecting when the person asked carries something (called distraction): 
	say "[The person asked] giggles, and holds up [the distraction] for you to see in exchange, as though this were some kind of show-and-tell game." instead.

Report a timid creature trying rejecting: say "[The person asked] scoots out of the way." instead.

Report a hostile creature trying rejecting: say "[The person asked] spits on [the noun]." instead.


Showing temper is an action applying to nothing.

Carry out a creature trying showing temper: do nothing.

Report a creature trying showing temper: 
	if the creature is in the egg and the egg is closed, say "[The creature] struggles against the walls of [the pod]." instead;
	say "[The creature] grumbles to itself." instead.


Crying is an action applying to nothing.

Carry out a creature crying: do nothing.

Report a creature crying: say "[The creature] sobs." 

Report a stupid creature crying:
	say "[The creature] [if a random chance of 1 in 2 succeeds]lies curled on the floor, keening softly[otherwise]rocks itself back and forth[end if]."  instead.
	
Report hostile smart creature crying:
	say "[The creature] is too weak to do much at this point, but it watches you angrily, and occasionally makes a gesture at you that it must_mw have learned from taxi drivers."  instead.
	
Report the creature crying when the creature is in a container (called the trap):
	say "[The creature] curls up in [the trap], weeping." instead.



Hiding oneself is an action applying to nothing.

Check a creature trying hiding oneself: 
	if the person asked is not in a container, stop the action.

Carry out a creature trying hiding oneself:
	if the person asked is in a container (called the shelter):
		if the player is in the shelter:
			try the person asked trying exiting;
		otherwise:
			if the shelter is closed and the player can see the person asked:
				say "[The person asked] [if a random chance of 1 in 2 succeeds]squinches its eyes shut, on an ostrich-like principle of mutual ignorance[otherwise]avoids looking at you[end if].";
			otherwise:
				try the person asked trying closing the shelter instead.
	
The last location is a room that varies. 
 
Before a creature trying hiding oneself when the person asked can see a door (called escape route):
	if location is not last location:
		now the person asked is passive;
		if the person asked is visible, say "[The person asked] presses itself against the wall and tries to creep past you towards [the escape route]." instead;
	otherwise:
		 try the person asked trying entering the escape route instead.
	
Before a creature trying hiding oneself when the person asked is not in a container and the player is not in a container:
	if the person can see an enterable sheltering container:
		let the shelter be a random enterable sheltering opaque container which can be seen by the person asked; 
		if the shelter is not a container, let the shelter be a random enterable sheltering container which can be seen by the person asked;
		try the person asked trying entering the shelter instead.
	
Definition: a container is sheltering if it is open or it is openable.

Before a creature trying entering the kit when the kit contains the pod:
	now the creature is passive;
	try the person asked trying taking the pod instead.
	
Before a creature trying entering the kit when the noun carries the pod:
	now the creature is passive;
	try the person asked trying dropping the pod instead.	


Dressing oneself is an action applying to nothing.

Before a creature trying dressing oneself when the person asked is not carrying something wearable:
	if the person asked can see an available wearable thing (called target) which is not worn by the person asked:
		try the person asked trying taking the target;
	otherwise if the person asked can see a wearable thing (called target) which is not worn by the person asked:
		try the person asked trying taking the target.
	
Check a creature trying dressing oneself:
	if the person asked is not carrying a wearable thing, stop the action.

Carry out a creature trying dressing oneself:
	if the person asked is carrying a wearable thing (called target), try the person asked trying wearing the target.
	
Report a cold creature trying wearing something:
	say "[The creature] puts on [the noun] and pulls it as tight as possible to conserve warmth." instead.
	
Report a cold creature trying wearing the jacket:
	say "[The creature] pulls the jacket awkwardly over its arms and gathers the front shut." instead.


Dining is an action applying to nothing.

Before a meaty creature trying dining when the person cannot see the player:
	if the person asked is in an adjacent room and the person asked can see a door (called the appropriate exit):
		try the person asked trying entering the appropriate exit instead.

Before a creature trying dining when the person asked is wearing a delicious thing (called lunch) and the person asked is not carrying a delicious thing:
	try the person asked trying taking off the lunch instead.

Instead of a creature trying dining when the person asked does not have a delicious thing:
	if the person asked can see an available useful delicious thing (called target):
		try the person asked trying taking the target;
	otherwise:
		if the person asked can see a delicious thing (called target):
			try the person asked trying taking the target;
		otherwise if the person asked is smart:
			try the person asked trying exploring.
	
Check a creature trying dining:
	if the person asked is not carrying a delicious thing, stop the action.

Carry out a creature trying dining:
	if the person asked is carrying a delicious thing (called target), try the person asked trying eating the target.

Playing with is an action applying to one thing.

Carry out a creature trying playing with something: now the noun is explored.

Report a creature trying playing with something: 
	say "[The person asked] [if the person asked is playful]toys with[otherwise]pokes at[end if] [the noun]";
	if the person asked has the noun, say "." instead;
	if the noun is in location:
		say "." instead;
	otherwise:
		if the holder of the noun is not the holder of the person asked:
			say " [if the holder of the noun is a container]in[otherwise]on[end if] [the holder of the noun]." instead;
		otherwise:
			say " in a bored manner." instead.

Definition: a person is fidgety if it is fast or it is lightning.

Report a smart person who is wearing something trying playing with the pod:
	say "[The person asked] inspects its reflection in the surface of [the pod], and adjusts [the list of things worn by the person asked]." instead.

Report a vast creature trying playing with something when the noun is fixed in place and a random chance of 1 in 2 succeeds:
	say "[The person asked] casually lifts [the noun] a few inches, then sets it back down." instead.

Report an athletic creature trying playing with something when the noun is fixed in place and the noun is in the location:
	if the person asked is vast, continue the action;
	say "[The person asked] climbs up one side of [the noun] and down the other." instead.
	
Report a creature trying playing with the window:
	say "[The person asked] stares out the window." instead.
	
Report a hostile creature trying playing with the window:
	say "[The person asked] presses its nose to the window and growls deep in its throat." instead.
	
Report a meaty creature trying playing with the window:
	say "[The person asked] looks out the window at the pedestrians down on Fifth Avenue, and licks its teeth slowly."  instead.

Report a fidgety creature trying playing with something: 
	if a random chance of 1 in 2 succeeds,
		say "[The person asked] taps [the noun] all over." instead;
	otherwise say "[The person asked] beats a rapid rhythm with its claws on [the noun]." instead.
	
Report a smart creature trying playing with the glue:
	say "[The person asked] squeezes the tube of glue, rolling up from the bottom, until it looks tidy." instead.

Report a fidgety creature trying playing with a container:
	say "[The person asked] taps [the noun]";
	if the noun is papery, say ", which rustles." instead;
	otherwise say ", which thunks hollowly." instead.
	
Report a fidgety creature trying playing with a papery thing:
	say "[The person asked] rustles [the noun]." instead.
	
Report a smart curious creature trying playing with a papery thing:
	if the noun is the sack, continue the action;
	if the person asked is homebound and the noun is the booklet:
		say "[The person asked] reads [the noun] through, then looks at you[if a visible thing fits the pod]; then at [the list of visible things which fit the pod][end if]. It bites its lower lip thoughtfully." instead;
	otherwise:
		if the noun is the blank label:
			say "[The person asked] squints studiously at [the blank label].";
		otherwise: 
			say "[The person asked] reads [the noun] through, following the words with one claw and subvocalizing as it goes." instead.
			
Instead of a vast creature trying playing with something which is not the pod:
	try the person asked trying attacking the noun.
	
Carry out a vast creature trying attacking an openable container:
	now the noun is open;
	now the noun is damaged;
	now the noun is unopenable.

Report a vast creature trying attacking a container:
	if the noun is papery, say "Apparently by accident, [the creature] rips a big hole in the side of [the noun].";
	otherwise say "[The creature] so mauls [the noun] that it is now in a permanent state of openness." instead.

Before a vast creature trying attacking the whole file drawer:
	now the noun is open;
	now the noun is unopenable;
	now the noun is damaged; now the noun is explored;
	now the creature is passive;
	say "[The creature] casually puts a deep dent in the side of the file drawers, leaving them permanently open.
	
'Need any help?' Esther calls from the outer office.

'Not at all,' you assure her." instead.

A thing can be damaged or whole. A thing is usually whole.

Instead of a vast creature trying attacking a whole wheel:
	now the noun is explored;
	now the noun is damaged;
	if the person asked is visible, say "[The person asked] bites down hard on [the noun], but seems disappointed by the experience."

Instead of a vast creature trying attacking the window:
	now the window is explored;
	now the person asked is passive;
	if the person asked is visible, say "[The person asked] bangs [forcefully] on the window, but, to your considerable relief, the glass holds up. You'll have to thank the lab boys for whatever they did to reinforce it: you sometimes have your doubts that it's really glass at all."
	
Instead of a vast creature trying attacking an unopenable papery container:
	let space be the holder of the noun;
	if the person asked is visible, say "[The creature] finishes dismantling [the noun], leaving [the list of things in the noun] behind.";
	now every thing in the noun is in the space;
	now the person asked is passive;
	remove the noun from play.
	
Instead of a vast creature trying attacking the lamp:
	let space be the holder of the lamp;
	remove the lamp from play;
	move shattered bits to the space;
	now the shattered bits are explored;
	now the person asked is passive;
	if the person asked is visible, say "[The creature] takes exception to the lamp: after crushing the bulb, it dismantles the cord and the lampshade until what remains is nearly unrecognizable."
	
Some shattered bits are a fixed in place thing. The printed name of the bits is "shattered bits of lamp". Understand "lamp" or "shattered bits of lamp" or "bits of lamp" as the bits. Instead of doing something other than examining to the shattered bits, say "The remains look sharp." The shattered bits are damaged. The description of the shattered bits is "This was no Tiffany -- it would've taken some considerable force to break this old thing. More for Esther's expense account, perhaps."

Instead of a vast creature trying attacking an edible thing:
	remove the noun from play;
	if the person asked is visible, say "[The person asked] tears apart [the noun], scattering bits everywhere.";
	now the person asked is passive.
 
Instead of a vast creature trying attacking the desk:
	now every thing on the desk is in the location;
	remove the desk from play;
	move abstract sculpture to the location;
	now the sculpture is explored;
	now the person asked is passive;
	if the person asked is visible:
		change Esther's utterance to "'Keep it down,' Esther yells. 'Some of us are trying to hear ourselves think.'";
		say "[The creature] hammers at your desk with its meaty fists and effectively reduces it to a work of modern art. Your possessions will have to live elsewhere from now on." instead.
 
Carry out a creature trying attacking something papery:
	remove the noun from play.

Report a creature trying attacking something papery: 
	say "[The person asked] rips [the noun] to tiny shreds.";
	 
Report a creature trying attacking the picture:
	say "Before you can take action, [the person asked] has rendered [the picture] into a black and white confetti[if the creature is smart].
	
Then it gives you a horrible mocking look. Right. Hostile it is, then. No question there[end if]." instead.

Instead of attacking a papery thing:
	remove the noun from play;
	if the noun contains something:
		say "You tear up [the noun], leaving behind [the list of things in the noun][if the creature is in the noun]. [The creature] blinks in surprise[end if].";
		now every thing in the noun is in the location;
	otherwise:
		say "You rip [the noun] to bits.".

Carry out a vast creature trying attacking the chair:
	remove the chair from play;
	now the molded seat is in the location;
	now the molded back is in the location;
	now every wheel is in the location.
	
Report a vast creature trying attacking the chair:
	say "[The creature], growling, dismantles your chair entirely: wheels, back, seat go flying in all directions, leaving only a stripped down frame." instead.
	
Report a vast creature trying attacking a damaged thing:
	if the creature has the noun,
		say "[The creature] bends [the noun] in imitation of a strong-man demonstration at a fair.";
	otherwise say "[The creature] jumps up and down on [the noun], but does not cause any more harm." instead.

The molded plastic seat is an explored thing. The molded plastic back is an explored thing. A wheel is a kind of thing. A wheel is usually explored. Four wheels are on the molded plastic seat. The description of the wheel is "Nothing but a round plastic part, now." The description of the back is "It used to be moderately comfortable, and now certainly is not." The description of the seat is "Of almost no use now that it's detached." The frame is a thing. The description of the frame is "A structure of metal and plastic that used to support the other chair components.". The printed name of the frame is "chair frame". Understand "chair" as the frame. The molded seat is damaged. The molded back is damaged.

The abstract sculpture is a fixed in place thing. "A hunk of twisted grey metal adorns your floor." Understand "twisted" and "hunk" and "grey" and "gray" and "metal" and "twisted hunk of grey metal" as the sculpture. The sculpture is damaged. The printed name of the abstract sculpture is "abstract sculpture that was your desk". 
	
Instead of a curious wood-pulpy creature trying playing with something which is stuck to the egg:
	if the noun is the egg, continue the action;
	now the noun is not stuck to the egg;
	move the noun to the person asked;
	now the person asked is passive;
	if the person asked is visible, say "[The person asked] licks and licks at [the noun], and somehow with its alien spit dissolves the holdall glue. [The noun] comes free in its hands. ";
	if the noun outweighs strength:
		silently try the person asked trying dropping the noun;
		if the person asked is not carrying the noun and the person asked is visible, say "Then, of course, [the person asked] drops it, since [the noun] is much too heavy." instead; 
	say paragraph break.
			

Report a stupid creature trying playing with something which is stuck to the egg:
	if the noun is the egg, say "[The person asked] tries to take apart [the egg], without any luck." instead;
	otherwise say "[The person asked] tries to peel [the noun] from the egg, without success." instead.

Instead of a smart curious creature trying playing with an unexplored switched off device:
	if the person asked is slothful, continue the action;
	now the noun is explored;
	try the person asked trying switching on the noun.
	
Instead of a slothful creature trying switching on something:
	now the person asked is passive;
	if the person asked is visible, say "[The person asked] fumbles painfully at [the noun], but cannot manage to flip the switch."
	
Instead of a slothful creature trying switching off something:
	now the person asked is passive;
	if the person asked is visible, say "[The person asked] fumbles painfully at [the noun], but cannot manage to flip the switch."
	
Instead of a slothful creature trying switching off something for the first time:
	now the person asked is passive;
	if the person asked is visible, say "[The person asked] runs its claws over the surface of [the noun], seeking to switch it off but without success."

Report a light-sensitive creature trying switching off the lamp:
	say "Squinting and fumbling at the base of the appliance, [the person asked] manages to switch off [the lamp] again." instead.

Report a light-sensitive creature trying switching on the lamp:
	say "[The person asked] manages to turn on the lamp, and immediately cowers from the horrible force of its brilliance." instead.

Report a curious creature trying playing with something when the noun is fixed in place:
	if the person asked is vast and a random chance of 1 in 2 succeeds, continue the action;
	say "[The person asked] cranes its head around, trying to see under and behind [the noun]." instead.
	
Report a smart hostile creature trying playing with something when the noun is fixed in place:
	say "[The person asked] runs its claws around the edges of [the noun], as though seeking a way to take it apart." instead.
	
Report a smart hostile creature trying playing with the lot of paperwork:
	say "Chuckling impishly, [the person asked] takes out a few files and sorts them back in random locations." instead.

Report a hostile creature trying playing with something when the noun is fixed in place:
	say "[The person asked] bangs its head against [the noun] repeatedly." instead.

After a hostile creature trying playing with something when the noun is fixed in place for the first time:
	if the player can see the person asked and the player is in the Office, say "[The person asked] bangs its head against [the noun], raising a bit of a racket.
	
'I hope everything is all right in there,' Esther shouts from the other room.

'Swell!' you assure her.";
	otherwise continue the action.

Report a playful creature trying playing with something which is worn by the person asked: 
	say "[The person asked] adjusts the fit of [the noun]." instead.

Report a playful creature trying playing with the fedora when the person asked is wearing the fedora:
	say "[The person asked] tilts [the fedora] at a more rakish angle." instead.
	
Report a hostile stupid person trying playing with the fedora when the person asked is wearing the fedora:
	say "With a guttural gurgle, [the person asked] pulls [the fedora] down more tightly over its ears." instead.
	
Report a creature trying playing with the jacket when the jacket is worn by the person asked:
	if the person asked is playful, say "[The person asked] turns up the collar of the jacket, making itself like an unearthly gangster or hoodlum." instead;
	otherwise say "[The person asked] rolls up the sleeves of [the jacket]." instead.
	
Report a creature trying playing with the collar when the collar is worn by the person asked:
	say "[The person asked] twists the collar around its neck[if a random chance of 1 in 3 succeeds] uncomfortably[end if]." instead.

Report a creature trying playing with something edible: 
	say "[The person asked] prods [the noun] into interesting shapes." instead.
	
Report a playful creature trying playing with something edible:
	say "[The person asked] mimes wearing [the noun] as a hat, watching you slyly." instead.

	
Report a smart creature trying playing with the slide:
	let X be a random number between 1 and 3000; let Y be a random number between 1 and 3000;
	let Z be X + Y;
	say "[The person asked][quickly] adds [X] to [Y], and cocks its head in interest at the answer." instead.

Report a smart creature trying playing with the ballpoint pen when the person asked is carrying the pen and the person asked is carrying a papery thing (called the target):
	say "[The person asked][quickly] draws an inscrutable symbol on [the target] with [the pen]; then, glancing at you, rubs it out again." instead.

Report a creature trying playing with the ballpoint pen:
	say "[The person asked][quickly] clicks the ballpoint pen. ";
	let X be a random number between 1 and 5;
	repeat with Y running from 1 to X: 
		if Y is 1, say "Click"; 
		otherwise say " click"; 
		if a random chance of 1 in 3 succeeds, say "ety";
	say "." instead.
	
Report a creature trying playing with the slide:
	say "[The person asked] slides the center of the slide-rule all the way out, then all the way back. And again, and again..." instead.
	
Report a smart creature trying playing with the picture when the person asked is not carrying the picture:
	if the person asked is playful, say "[The person asked] studies [the picture], then tries mimicking the stance and swing." instead;
	otherwise say "[The person asked] rotates [the picture] ninety degrees, then sets it back down, satisfied." instead.
	
Instead of a smart creature trying playing with the picture when the person asked is carrying the picture:
	if the person asked is playful, continue the action;
	try the person asked trying dropping the picture.
	
Report a smart creature trying playing with the paperwork:
	say "[The person asked] flips through the paperwork, occasionally cocking its head in interest at what it finds." instead.
	
Report a creature trying playing with the paperwork when the food of the person asked is wood-pulpy:
	say "[The person asked] paws through the paperwork, its tongue flicking in and out of its mouth." instead.
	
The inaction rule is listed after the check stage rule in the specific action-processing rules.

This is the inaction rule: 
	now the person asked is passive.

Report a fidgety creature entering something: 
	say "[The creature] [if the creature is timid]creeps[otherwise]hops[end if] into [the noun][if the player is in the noun] with you[end if][if the heart's desire is not the creature] in search of [the heart's desire][end if]." instead.
	
Report a slow negligible creature entering something: 
	say "[The creature] hauls itself into [the noun][if the player is in the noun] with you[end if][if the heart's desire is not the creature] in search of [the heart's desire][end if]." instead.
	
Report a creature entering the pod for the first time:
	say "[The creature] clambers into [the noun], a little awkwardly through the curved door";
	if the pod is not in the location, say "[if the heart's desire is not the creature], in search of [the heart's desire][end if]";
	say ". The pod rocks ominously but does not tip over." instead.

Report a creature entering the pod when the person asked is suffocating:
	say "[The creature], rasping desperately, climbs into [the noun] in search of a more breathable environment."

Report a creature entering something:
	say "[The creature] climbs into [the noun]." instead.
	
Report a creature exiting:
	say "[The creature] gets out again." instead.
	
Report a creature going through a door (called the escape route):
	say "[The creature] slips out through [the escape route]." instead.
	
Report a timid creature going through a door (called the escape route):
	say "Glancing at you apprehensively, [the creature] tiptoes out [the escape route]." instead.

Report a creature closing the egg pod when life-support is switched on:
	now the person asked is passive;
	if the creature is in the pod, say "[The creature] pulls [the pod] shut from the inside. It seals with a hiss, leaving the creature isolated within." instead;
	otherwise say "[The creature] closes [the pod], which seals with a hiss." instead.
	
Report a creature opening the egg pod when life-support is switched on: 
	say "[The creature] opens [the pod], breaking the life-support seal: [italic type]PFFT![roman type]" instead.


Instead of the creature attacking a portable thing which is not carried by the person asked:
	try the person asked trying taking the noun.

Instead of a creature attacking something which is worn by the person asked:
	try the person asked trying taking off the noun. 
	
Report a creature eating the picture:
	say "[The creature] bites off the corner of DiMaggio, chews, and swallows. You give a howl of rage and try to get back the remains, but they too are soon gone.
	
'What?' says Esther, running into the room. 'Did it bite you? Should I call a doctor?'

'No,' you say. 'It bit Mr. DiMaggio, instead.'

'Ooh-ooh,' she says. For a moment, just a moment, something like human-kindness appears on her face. 'That's too bad, really it is.'

The phone in the outer office rings. 'Sorry, duty calls.' 

So much for sympathy." instead.

Report a creature eating the picture during Glue Return:
	say "[The creature] bites off the corner of DiMaggio, chews, and swallows. You give a howl of rage and try to get back the remains, but they too are soon gone. Not even your yelping is enough to get Esther in here with the holdall glue, though."


Disposing of is an action applying to one thing.

Before a creature disposing of something which is worn by the person asked:
	try the person asked trying taking off the noun instead.

Before a creature disposing of something which is not held by the person asked:
	if the noun is not in an openable container,
		try the person asked taking the noun instead;
	if the person asked is stupid, try the person asked taking the noun instead. 

Before a smart creature trying disposing of something when the noun is not in an openable container:
	if the person asked can see an openable container (called the tank), try the person asked trying inserting the noun into the tank instead.
	

Instead of a creature trying disposing of something which is not held by the person asked when the person asked is holding something stinky (called the other problem):
	try the person asked trying disposing of the other problem.
	
Instead of a creature trying inserting something into a closed openable container:
	try the person asked trying opening the second noun.
	
Instead of a creature trying inserting something which is not held by the person asked into something:
	try the person asked trying taking the noun.

Instead of a stupid creature trying disposing of something when the person asked is in a room:
	now Esther carries the noun;
	now the person asked is passive;
	if the person asked is visible, say "[The person asked] hurls [the noun] out the door[if the noun is the meatball]. 'Ouch!' Esther shouts[end if].";
	now the person asked is passive.
	
Instead of a stupid creature trying disposing of something for the first time: 
	now Esther carries the noun; 
	if the person asked is visible:
		if the person asked is athletic:
			say "[The person asked] winds up and, with a fastball worthy of Allie Reynolds, pitches [the noun] out the door of your office. [paragraph break]'Hey!' shouts Esther. 'Watch it in there!'";
		otherwise:
			say "[The person asked] walks over to the door of your office and drops [the noun] outside."


Carry out a creature trying disposing of something:
	if the noun is in an openable container (called the tank), try person asked trying closing the tank; 
	if the noun can be touched by the person asked, stop the action.

Carry out a creature trying opening something: now the noun is explored.

Instead of a creature trying opening something for the second turn:
	if the person asked is visible, say "[The person asked] looks exasperated.";
	now the person asked is passive. 
	
Report the creature trying opening the drawer when the drawer was open:
	say "[The person asked] opens [the noun]. Again." instead.
	 
Report the creature trying closing the drawer when the drawer was closed:
	say "[The person asked] slams [the noun] and glares at you for good measure." instead.

Report the creature trying opening something which contains the creature: 
	say "[The person asked] triumphantly opens [the noun] from the inside." instead.

Report the creature trying opening something which contains something:
	say "[The person asked] opens [the noun]";
	if the creature is in the noun, say " from within." instead;
	if the odor sensitivity of the person asked is strong, say "[if the noun contains a stinky thing], wrinkling its nose at the smell of [the list of stinky things which are in the noun][end if]." instead;
	otherwise say " and pokes curiously at [the list of things which are in the noun]." instead.

Report a hostile creature trying dropping something:
	say "[The person asked] flings aside [the noun]." instead.

Report a vast creature trying dropping something:
	say "[The person asked] flings [the noun] at [the random fixed in place thing in the location]. No serious harm results." instead.
	
Report a fidgety hostile creature trying dropping something:
	if the noun outweighs strength, do nothing;
	otherwise say "[The person asked] throws [the noun] at you, but (fortunately) misjudges the velocity and angle." instead.
	
Report a creature trying taking the egg:
	say "[The person asked] wrestles with [the egg] -- which is after all more awkward than heavy -- but finally manages to get hold of it." instead.

Report a secretive creature trying dropping the egg: 
	say "[The person asked] awkwardly deposits [the noun] on the ground." instead.
	
Report a fidgety hostile Earthlike creature trying dropping something when the creature can touch the player:
	say "[The person asked] flings [the noun] at you; it hits and bounces off." instead.

Report a fidgety hostile Earthlike creature trying dropping something heavy:
	say "[The person asked] flings [the noun] at you with unpleasant precision. That's going to leave a bruise in the morning." instead.
	
Instead of a starving creature trying exiting: try the creature trying crying instead.

Before a smart creature trying exiting when the creature is in a closed container (called the trap):
	try the creature trying opening the trap instead.

Before a creature trying opening an unopenable container which contains the creature for the first time:
	now the creature is passive;
	say "The creature tries to open [the noun] from the inside, and soon realizes it cannot be done. Its mouth opens in a screech of fury, or terror: you hear nothing." instead.

Before a creature trying opening an unopenable container which contains the creature for the first time:
	now the creature is passive;
	say "The creature pummels its fists against the interior of [the noun] in desperation." instead.
	
Tidying is an action applying to nothing.

Check a creature trying tidying:
	if the person asked can see an open openable container, continue the action;
	otherwise stop the action.

Carry out a creature trying tidying:
	if the person asked can see an open openable container (called the mess), try the person asked trying closing the mess.


Exploring is an action applying to nothing.

Carry out a creature trying exploring:
	now the person asked is passive;
	if the person asked can see a closed openable container (called target), try the person asked trying opening the target instead;
	if the person asked can see a fixed in place thing (called target), try the person asked trying looking under the target instead.
	

Sneaking it under is an action applying to two things.

Check a creature trying sneaking something under something:
	if the person asked is not carrying the noun, stop the action;
	if the second noun is not fixed in place, stop the action.
	
Carry out a creature trying sneaking something under something:
	remove the noun from play;
	now the second noun disguises the noun.
	
Report a creature trying sneaking something under something:
	say "[The person asked] sneakily hides [the noun] under [the second noun]."
	

Understand "hide [something] under [something]" as sneaking it under. Understand "put [something] under [something]" as sneaking it under. Understand "conceal [something] under [something]" as sneaking it under.

Check sneaking something under something:
	if the player is not carrying the noun:
		try the player trying taking the noun;
		if the player is not carrying the noun, stop the action;
	if the second noun is a door, say "You can't hide things under [the second noun]." instead;
	if the location is the Office, say "There's no good way to do that." instead;
	if the second noun is not fixed in place, say "[The second noun] would not make much of a concealment." instead.
	
Carry out sneaking something under something:
	remove the noun from play;
	now the second noun disguises the noun.
	
Report sneaking something under something:
	say "You tuck [the noun] under [the second noun]."


Launching us is an action applying to nothing.

Before a hostile person trying launching us: act creepy instead.
	
To act creepy:
	if the person asked is visible, say "[The person asked] looks at you for a long time through bright eyes and then smiles very pleasantly.";
	now the person asked is passive;
	now the person asked is secretive.

Before a creature trying launching us when the egg is not in the location and the egg is not carried by the person asked:
	try the person asked trying taking the egg instead.
	
Before a creature trying launching us when the person asked is carrying the egg:
	try the person asked trying dropping the egg instead.

Instead of a creature trying launching us when the propulsion is not stuck to the egg:
	try the person asked trying affixing propulsion.

Before a creature trying launching us when the egg is closed and the player is not in the egg:
	try the person asked trying opening the egg instead.
	
Instead of a creature trying launching us when the egg is open and the player is not in the egg and the person asked is not in the egg:
	if the propulsion unit is not stuck to the egg and the person asked can touch the propulsion unit:
		try the person asked trying affixing the propulsion unit;
	otherwise:
		if the life-support is not stuck to the egg and the person asked can touch the life-support unit:
			try the person asked trying affixing the life-support unit;
		otherwise:
			if the blank label is not stuck to the egg and the person asked can touch the blank label, try the person asked trying affixing the blank label instead;
			if the filled label is not stuck to the egg and the person asked can touch the blank label, try the person asked trying affixing the filled label instead;
			try the person asked trying faking egg entry.
	
Faking egg entry is an action applying to nothing. 

Before a creature trying faking egg entry when the player carries the glue and the propulsion unit is not stuck to the egg:
	try the person asked trying taking the glue instead.
	
Before a creature trying faking egg entry when the propulsion unit is not stuck to the egg and the person asked can touch the glue and the person asked does not carry the glue: try the person asked trying taking the glue instead.

After a creature trying faking egg entry for the first time:
	say "[The person asked] goes as though to get into the egg, but can't seem to fold up its limbs properly to fit into the door. It gives you a look of bewilderment, obviously needing a demonstration." instead.
	
Instead of a creature trying faking egg entry for the third time:
	stop the action.  

Report a creature trying faking egg entry:
	if a random chance of 1 in 2 succeeds,
		say "[The person asked] struggles with the door of the egg, now and then glancing up at you and frowning.";
	otherwise say "[The person asked] puts its head inside the egg, then sighs gustily when it can't figure out how to fit in the rest of its substantial frame."

To make scene break:
	say line break;
	center "*****";
	say line break;
	pause the game;
	say paragraph break;
	say paragraph break;
	say paragraph break. 

Instead of a creature trying launching us when the player is in the egg and the egg is open:
	try the person asked trying closing the egg.
	
Instead of a creature trying launching us when the player is in the egg and the egg is closed and the egg is openable and the person asked is carrying the glue:
	try the person asked trying gluing the egg shut.
	
Check a creature trying launching us:
	if the propulsion is not stuck to the egg, stop the action;
	if the egg is not closed, stop the action;
	if the player is not in the egg, stop the action;

Carry out a creature trying launching us: do nothing.

Report a creature trying launching us: 
	try the person asked trying switching on the propulsion instead.
	
After a secretive creature trying switching on the propulsion when the player is in the egg and propulsion is stuck to the egg:
	say "[The person asked] switches on the propulsion, and the last thing you see is its frenzied, evil grin as you float off out the window...";
	end the game in death.
	
Before a creature trying playing with something which is carried by the player:
	say "[The creature] looks with fascination at [the noun], but cannot reach it." instead.
	
Before a creature trying playing with something which is part of something which is carried by the player:
	say "[The creature] looks at [the noun] but can't quite get at it." instead.

Before a curious creature trying switching on the propulsion for the first time:
	now the creature is passive;
	say "[The person asked] pokes at the propulsion unit and very nearly turns it on with disastrous effect." instead.

After a creature trying switching on the propulsion when the player is in the egg and propulsion is stuck to the egg:
	say "[The person asked] pokes curiously at the propulsion unit, and after a moment switches it on. Then it stares at you in fascination as you float off out the window...";
	end the game in death.

After a creature trying switching on the propulsion:
	say "[The person asked] pokes curiously at the propulsion unit, and has the misfortune to turn it on. Thanks to the unfortunate configuration of where everyone is standing, the creature is toasted, you get a little singed yourself, and the boys from Alien Protocol spend until nearly midnight taking notes for their report.";
	end the game saying "That could have gone better"
	

Affixing is an action applying to one thing.

Before a creature trying affixing something when the person asked can see the instruction booklet and the booklet is unexplored:
	try the person asked trying reading the instruction booklet instead.

Before a creature trying affixing something when the person asked does not have the glue:
	try the person asked trying taking the glue instead.
	
Before a creature trying affixing something when the person asked cannot touch the noun:
	try the person asked trying taking the noun instead.
	
Before a creature trying affixing something when the person asked cannot touch the egg:
	try the person asked trying taking the egg instead.
	
Before a creature trying affixing something when the person asked is carrying the egg:
	try the person asked trying dropping the egg instead.
	
Before a creature trying affixing something when the person asked is carrying the noun:
	try the person asked trying dropping the noun instead.
	
Check a creature trying affixing propulsion:
	if the person asked cannot touch the egg, stop the action;
	if the person asked cannot touch the noun, stop the action;
	if the person asked is not carrying the glue, stop the action.

Carry out a creature trying affixing something:
	try the person asked trying gluing the noun to the egg instead.


Preparing for launch is an action applying to nothing.

Before a creature trying preparing for launch when the player is in the egg:
	now the person asked is passive;
	say "[The person asked] gestures for you to get out of the egg, which does seem a valid point." instead.
	
Before a creature trying preparing for launch when the propulsion unit is not stuck to the egg:
	if the person asked is not in the egg, try the person asked trying affixing propulsion;
	if the person asked is passive, stop the action.

Before a creature trying preparing for launch when the life-support unit is not stuck to the egg:
	if the person asked is not in the egg, try the person asked trying affixing life-support;
	if the person asked is passive, stop the action. 
	
Before a creature trying preparing for launch when the life-support unit is stuck to the egg and the life-support unit is switched off: try the person asked trying switching on the life-support unit;
	if the person asked is passive, stop the action. 

Before a creature trying preparing for launch when the person asked can see the blank label and the blank label is unexplored: 
		now the person asked is passive; 
		now the blank label is explored;
		say "[The person asked] squints at [the blank label] thoughtfully, then shrugs." instead;
		
Before a creature trying preparing for launch when the person asked can see the filled label and the filled label is not stuck to the egg:
	if the person asked is not in the egg, try the person asked trying affixing filled label;
	if the person asked is passive, stop the action. 

Before a creature trying preparing for launch when the person asked is not in the egg:
	if the person asked is carrying the glue, try the person asked trying dropping the glue instead;
	if the person asked is carrying the pen, try the person asked trying dropping the pen instead; 
	try the person asked trying entering the egg instead.

Before a creature trying preparing for launch when the egg is open: 
	try the person asked trying closing the egg instead.
	
Before a creature trying preparing for launch when the propulsion unit is not stuck to the egg: 
	now the person asked is passive;
	if the person asked can see the propulsion unit, say "[The person asked] points at [the propulsion unit], which is not currently attached to the egg." instead;
	otherwise say "[The person asked] makes pointed blast-off noises." instead.
	
Before a creature trying preparing for launch when the life-support unit is not stuck to the egg:
	now the person asked is passive;
	if the person asked can see the life-support unit, say "[The person asked] points at [the life-support unit], which is not currently attached to the egg." instead;
	otherwise say "[The person asked] mimes choking, to remind you about the life-support unit." instead.

Before a creature trying preparing for launch when the life-support unit is stuck to the egg and the life-support unit is switched off:
	now the person asked is passive;
	say "[The person asked] gestures to the switched-off [life-support], and mimes suffocating." instead.
	
Carry out a creature trying preparing for launch: do nothing.

Report a creature trying preparing for launch:
	if the creature is curious, say "[The creature] whistles a jaunty tune while it waits for you to send it home. Or at least so you assume from its headbobbing -- you yourself can't hear a thing.";
	otherwise say "[The creature] waits with zen-like calm for you to turn on propulsion."


Report a hostile person trying taking off something:
	say "[The person asked] grumpily divests itself of [the noun]." instead.

Report a playful person trying taking off something:
	say "[The person asked] flirtatiously strips off [the noun]." instead.

Report a fast person trying taking off something:
	say "[The person asked] strips off [the noun] in a single fluid movement." instead.

Report a creature trying waiting:
	choose a random row in the Table of Creature Sloth;
	say reply entry instead.
	
Table of Creature Sloth
reply
"[The person asked] lies very still and follows you with its eyes."
"[The person asked] rolls over."
"[The person asked] rubs its eyes."
"[The person asked] slowly swivels its ears to follow your actions."
"[The person asked] [if person asked is in egg]tries to get up, then decides that it is too much effort[otherwise]waits, almost motionless[end if]."
"[if the person asked is weak][The person asked] is completely still[end if][if the person asked is not weak]The sides of [the person asked] slowly rise and fall as it breathes[end if]."
"[if the person asked is weak][The person asked] stares at you unnervingly[end if][if the person asked is not weak][The person asked] yawns[end if]."

Report a creature trying jumping:
	say "[The person asked] jumps[if the person asked is vast] implausibly high[end if][if the person asked is Earthlike] quite high[end if][if the person asked is Marslike], but looks surprised at how quickly it lands[end if][if the person asked is fractional], but not very far[end if][if the person asked is negligible], but only clears the floor by a few meager centimeters[end if]." instead.

Instead of a creature trying taking something when the noun outweighs strength and the noun is in a container (called receptacle):
	if the receptacle is available, try the person asked trying pushing the receptacle;
	otherwise continue the action.

Instead of a creature trying taking something when the noun outweighs strength and the noun is on a supporter (called surface):
	try the person asked trying pushing the noun.
	
Instead of a creature trying pushing something which is on a supporter (called surface):
	let the place be the holder of the surface;
	now the noun is in place;
	now the person asked is passive;
	if the person asked is visible, say "[The person asked] [forcefully] pushes off [the noun] onto [if the place is location]the floor[otherwise][the place][end if].";
	

Dumping out is an action applying to one thing.

Understand "empty [container]" or "dump out [container]" as dumping out. Understand "empty [something]" or "dump out [something]" as dumping out.

Check dumping out:
	if the noun is not a container, say "[The noun] cannot contain anything to start with." instead;
	if the number of things contained by the noun is 0, say "[The noun] does not contain anything." instead;
	if the player is not carrying the noun:
		try the player trying taking the noun;
		if the player is not carrying the noun, stop the action.
	
Carry out dumping out:
	let place be the holder of the player;
	now every thing which is in the noun is in the place.
	
Report dumping out:
	say "You empty [the noun] out [if the player is in a room]onto the floor[otherwise]into [the holder of the player][end if]."


Check a creature trying dumping out:
	if the noun is not a container, stop the action;
	if the number of things contained by the noun is 0, stop the action;
	if the person asked does not carry the noun, stop the action.
	
Carry out a creature trying dumping out:
	let place be the holder of the person asked;
	now every thing which is in the noun is in the place.

Report a creature trying dumping out:
	say "[The person asked] empties [the noun] [if the person asked is in a room]onto the floor[otherwise]into [the holder of the person asked][end if]."

Understand "push [something] over" or "knock [something] over" as pushing.

Instead of pushing a container:
	let the place be the holder of the noun;
	say "You knock over [the noun][if something is in the noun], dumping out [the list of things which are in the noun][end if]."; 
	now every thing which is in the noun is in the place.

Instead of a creature trying pushing a container:
	let the place be the holder of the noun;
	if the person asked is visible, say "[The person asked] knocks over [the noun][if something is in the noun], awkwardly spilling out [the list of things which are in the noun][end if].";
	now the person asked is passive;
	now every thing which is in the noun is in the place.
	
Instead of a creature trying taking something when the noun outweighs strength:
	if the noun is pointless, stop the action;
	change the noun to pointless;
	if the person asked is visible, say "[The person asked] tries to pick up [the noun], but without success."
	
Instead of a secretive starving Earthlike creature trying taking something which is not available:
	try the person asked trying attacking the player.

Instead of an Earthlike creature trying attacking the player when the person asked is not carrying a heavy thing:
	if the person asked can see an available heavy thing (called target), try the person asked trying taking the target.
	
Instead of an Earthlike creature trying attacking the player when the person asked carries a heavy thing (called the weapon):
	say "Starving, desperate, and tired of waiting for your cooperation, [the person asked] slugs you hard with [the weapon]. And what happens from there is all blackness...";
	end the game in death.



### Section 3 - Every Turn Rules for Creature
 
	
After entering something in the presence of a contrary person:
	if the creature cannot touch the player, continue the action;
	move the creature to the noun;
	now the creature is passive;
	say "You climb into [the noun]. [The creature], agitated, tries to get you out again, a process at which it is so unsuccessful that it winds up falling in with you."

After exiting in the presence of a contrary person:
	now the creature is passive;
	if the creature is in location, say "You climb out. [The creature] tries to push you back, but without success.";
	otherwise say "You scramble out. [The creature] tries to pull you into [the holder of the creature] with it, but fails miserably."

Definition: a thing is inanimate if it is not a person.

To decide whether game has begun:
	if we are dead, no; 
	if Office is visited, yes; 
	no.

Every turn:
	if game has begun and Sending is happening,
		follow the creature behavior rules;
	change last location to location.
	
The creature behavior rules is a rulebook.

The first creature behavior rule:
	abide by the creature death rule.
	
This is the creature death rule:
	if the creature is suffocating:
		increase the breath count of the creature by 1;
		if the breath count of the creature > 3:
			say "At this point [the creature] is overwhelmed by lack of whatever it breathes, and collapses.";
			end the game saying "Unfortunately, you are unable to revive the thing";
	if the creature is dying:
		say "Too long without sustenance, [the creature] succumbs to a coma and death.";
		end the game saying "You have killed your Visitor.";
		rule succeeds.
	
A creature behavior rule (this is the poisoned creature rule):
	if the creature is poisoned:
		now the creature is passive;
		now the creature is slow;
		if the creature is visible:
			say "[The creature] does not move but simply looks glassy[if the creature is hungry] and hungry[end if][if the creature is starving]. Starving, even -- but too dulled to do anything about it[end if].";
		rule succeeds.
	
A creature behavior rule (this is the color change rule):
	if the moon of the creature is Triton and the creature can see the lamp:
		if the lamp is lit and the color of the creature is grey:
			now the creature is red;
			now the creature is passive;
			if the creature is visible:
				say "The creature gradually turns red in the lamplight.";
		otherwise:
			if the lamp is unlit and the color of the creature is red:
				now the creature is grey;
				now the creature is passive;
				if the creature is visible, say "The creature fades to grey with the lamp turned off."
					
A creature behavior rule (this is the drop heavy things rule):
	if the creature is active and the creature carries a tiring thing (called anvil):
		say "[The creature] grimaces at the weight of [the anvil].[line break]";
		try the creature trying dropping the anvil.

Definition: a thing is tiring if it outweighs strength.

A creature behavior rule (this is the placid held creature rule):
	if the player is carrying the creature: 
		if the creature carries a delicious thing and the creature is hungry:
			try the creature trying dining;
		otherwise:
			if the creature is cold:
				say "[The creature] huddles against you for warmth.";
			otherwise:
				say "[The creature] [if a random chance of 1 in 2 succeeds]squirms a little in your grip[otherwise]leans over to look at the floor[end if][if a random chance of 1 in 2 succeeds], but does nothing[end if].";
		rule fails.
	
A creature behavior rule (this is the inactivity while blinded rule):
	if the creature is blinded and the creature is active:
		if the creature is smart:
			if the creature can see a lit device (called the target):
				if the target is fixed in place or the target is available:
					try the creature trying switching off the target;
				otherwise:
					try the creature trying hiding oneself;
				if the creature is blinded and the creature is active:
					say "[The creature] [if a random chance of 1 in 2 succeeds]obviously does not appreciate the glare of the light[otherwise]huddles by itself, blinking[end if][if the creature is hungry]. It also looks to be getting hungry[end if].";
					now the creature is passive;
		otherwise:
			try the creature trying hiding oneself;
			if the player cannot see the creature, rule fails; 
			if the creature is passive, rule fails;
			if the creature is in the egg and the creature is visible:
				say "[The creature] turns as far from the light as it can get in its pod.";
			otherwise:
				 if the creature is visible, say "[The creature] cowers [if the creature is not in the location]in [the holder of the creature][otherwise]in what shadow it can find[end if], sniveling."; 
			rule fails.

A creature behavior rule (this is the food consumption rule):
	if the creature is active and the creature is hungry:
		try the creature trying dining. 	

A creature behavior rule (this is the inactivity while starving rule):
	if the creature is starving and creature is active:
		try the creature trying dining;
		if the creature is starving, try the creature trying crying.
	
A creature behavior rule (this is the slothful kids don't move rule):
	if the creature is slothful and the creature is active:
		try the creature trying waiting;
		now the creature is passive;
		rule succeeds.

A creature behavior rule (this is the dress when cold rule):
	if the creature is active and the creature is cold,
		try the creature trying dressing oneself.
	
A creature behavior rule (this is the inactivity while cold rule):
	if the creature is active and the creature is cold,
		try the creature trying shivering.

A creature behavior rule (this is the timid creature hides rule):
	if the creature is active and the creature is timid and the creature can see the player,
		try the creature trying hiding oneself; 
 
A creature behavior rule (this is the timid creature lurks rule):
	if the creature is active and the creature is timid and the creature is visible:
		say "[The creature] tries to put [the random fixed in place thing in the location] between itself and you.";
		now the creature is passive.
	
A creature behavior rule (this is the contrary creature rule): 
	if the creature is active and the creature is contrary and the noun is a thing:
		if the noun is the creature:
			now the creature is passive; [because we will count glaring back at the player as its action for this turn]
		otherwise:
			if the creature is visible, say "[The creature] [if a random chance of 1 in 2 succeeds]watches your attention to[otherwise]observes your interest in[end if] [the noun][if a random chance of 1 in 2 succeeds], its eyes narrowing[otherwise], growling in the back of its throat[end if].[line break]";
			if opening, try the creature trying closing the noun;
			if closing, try the creature trying opening the noun;
			if the creature is active, try the creature trying taking the noun.
	
A creature behavior rule (this is the evil creature rule): 
	if the creature is secretive and the creature is active and the creature is smart, try the creature trying launching us.

A creature behavior rule (this is the acquisitive creature environment rule):
	if the creature is not visible, make no decision;
	if the creature is active and the creature is acquisitive:
		if the creature is in a container and the location is rich:
			say "[The creature] thoughtfully counts [the number of things in the location in words] things outside its home, as compared to [the number of things in the holder of the creature in words] where it is now, and scratches its head.";
			try the creature trying exiting;
		otherwise:
			if the creature is in the location and the creature can see a rich container (called the target):
				say "[The creature] thoughtfully counts [the number of things in the target in words] things in [the target], as compared to [the number of things in the location in words] out here, and scratches its head. ";
				try the creature trying entering the target.
	
Definition: a container is rich:
	if it is closed and it is opaque, no;
	if the number of things in it is greater than the number of things in the holder of the creature, yes.

Definition:  a room is rich if the number of things in it is greater than the number of things in the holder of the creature.

A creature behavior rule (this is the creature escape rule): 
	if the player lies outside the creature, make no decision;
	if the creature is active and the creature is in a container (called home):
		if the home contains something (called target) which is wanted by the creature:
			try the creature trying taking the target;
		otherwise:
			if the player carries the home, make no decision;
			if the creature is homebound and the creature is in the egg and location is the Office:
				try the creature trying preparing for launch;
			otherwise:
				if the creature was last fed too long ago, do nothing;
				otherwise try the creature trying exiting;
				if the active creature is in a closed container (called the trap):
					if the creature is friendly and the creature is visible:
						say "[The creature] presses its mouth to the inner surface of [the trap] and makes fish faces at you.";
					otherwise if the creature is visible:
						say "[The creature] knocks on the inner wall of [the trap]."; 
					now the creature is passive.
	
A creature behavior rule:
	if the creature is homebound and the creature is active and location is the Office, try the creature trying preparing for launch;
	if the creature is hostile and the creature is active and the creature is smart, try the creature trying launching us.
	
A creature behavior rule (this is the hostile action rule):
	if the creature is active and the creature is hostile
	begin;
		if the creature can see an available papery thing (called the target)
		begin;
			if the creature can touch the target, try the creature trying attacking the target;
			otherwise try the creature trying showing temper;
		otherwise;
			if the creature can see a papery thing (called the target)
			begin;
				try the creature trying attacking the target;
			end if;
		end if;	
	end if.
	
A creature behavior rule (this is the smelly thing rule):
	if the creature is active and the creature is offended and the creature is stupid
	begin;
		let problem be a random stinky thing which can be seen by the creature;
		try the creature trying disposing of the problem;
	end if. 
	
A creature behavior rule (this is the good smelly thing rule):
	if the creature is active and the odor sensitivity of the creature is inverse
	begin;
		let target be a random stinky thing which can be seen by the creature;
		try the creature trying taking the target;
	end if. 
	
A creature behavior rule (this is the discard trash rule):
	if the creature is active and the creature is carrying something (called the target) which is not wanted by the creature
	begin;
		if the creature is acquisitive, make no decision;
		if the creature is vast and the target is unexplored and the target is not a container
		begin;
			try the creature trying attacking the target;
		otherwise;
			try the creature trying dropping the target;
		end if;
	end if.
	
A creature behavior rule (this is the greedy action rule):
	if the creature is active and the creature is acquisitive and the creature is not occupied
	begin;
		if the creature can see an available grabbable thing (called the target) which is not had by the creature
		begin; ;
			try the creature trying taking the target;
		end if;
	end if.
	
Definition: a thing is grabbable if it is not fixed in place and it is not scenery.

Definition: a creature is occupied if the number of things carried by the creature is the carrying capacity of the creature.

A creature behavior rule (this is the juggling stuff rule):
	if the creature is active and the creature is acquisitive and the creature is occupied
	begin;
		if the creature is visible and a random chance of 1 in 4 succeeds
		begin;
			now the creature is passive;
			say "[The creature] juggles [the list of things carried by the creature] in a colorful arc.";
		otherwise;
			if a random chance of 1 in 2 succeeds
			begin;
				now the creature is passive;
				if the creature is visible, say "[The creature] gleefully counts its [number of things carried by the creature in words] possession[s].";
			otherwise;
				try the creature trying playing with a random thing carried by the creature;
			end if;
		end if;
	end if.
	
A creature behavior rule (this is the creature improv rule):
	if the creature is active and the creature is playful and the creature is visible and plot depth is 2
	begin;
		increase plot depth by 1;
		say "[The creature] catches your eye, then does a pretty passable imitation of Esther walking towards you and wagging her finger. 

It's good to know that if she ever decides to quit, you'll be able to get a reliable replacement.";
	end if.
	
A creature behavior rule (this is the bored creature looks around rule):
	if the creature is active and the creature is playful
	begin;
		if the creature can see an unexplored thing, make no decision;
		try the creature trying exploring;
	end if.

A creature behavior rule (this is the bored action rule):
	if the creature is not visible, make no decision;
	if the creature is not in location, make no decision;
	now every thing which is part of something is explored;
	if the creature is active
	begin;
		if the creature can see something available (called the target) which is not wanted by the creature
		begin;
			if the creature can touch an unexplored available thing (called the new target)
			begin;
				try the creature trying playing with the new target;
			otherwise;
				if the creature can touch an entertaining thing
				begin;
					try the creature trying playing with a random entertaining thing;
				end if;
			end if;
		end if;
	end if.

Definition: a thing is entertaining if creature can touch it and it is not wanted by the creature and it is available and it is not part of something.
	
The last every turn rule:
	change the heart's desire to the creature;
	if Esther's utterance is not "blank", say "[Esther's utterance][paragraph break]"; change Esther's utterance to "blank";
	now every animal is active.
	
### Section 4 - Creature Reactions to Giving and Other Actions

Before exiting when the player is in a closed container (called the trap):
	try opening the trap;
	if the player is in the trap, stop the action.

Instead of opening the unopenable pod when the player is in the pod:
	if the creature is visible, say "You bang on the door with all your might, but the holdall glue really does hold all.
	
Noticing your struggles, the creature waves at you cheerily."

Instead of showing something to a creature:
	try giving the noun to the second noun.
	
Understand the command "feed" as something new. Understand "feed [something] to [something]" as feeding it to.

Feeding it to is an action applying to two things.

Instead of feeding something to a lightning creature: say "[The second noun] very swiftly dodges your attempt to feed it." Instead of feeding something to a fast creature: say "[The second noun] quickly moves away before you can do this." Instead of feeding something to a moderate creature when the creature is hostile: say "[The second noun] watches you suspiciously and then turns its head aside."

Check feeding it to:
	if the player is not carrying the noun
	begin;
		if the player is wearing the noun, try taking off the noun;
		otherwise try taking the noun;
		if the player is not carrying the noun, stop the action;
	end if;
	if the player cannot touch the second noun, say "You cannot reach [the second noun]."

Carry out feeding it to:
	if the noun is delicious
	begin;
		move the noun to the second noun;
		try the second noun trying eating the noun;
	otherwise;
		say "[The second noun] spits out [the noun]." instead;
	end if.
	
Accepting is an action applying to one thing.

Before a creature trying accepting something when the person asked is occupied: 
	if the number of things carried by the person asked is at least the carrying capacity of the person asked, 
		try the person asked trying dropping a random thing carried by the person asked.

Carry out a creature trying accepting something:
	move the noun to the person asked.
	
Procedural rule: ignore the block giving rule.

Check giving (this is the polite refusal of unwanted objects rule): 
	if the second noun does not want the noun
	begin;
		try the second noun trying rejecting the noun instead;
		now the second noun is passive;
	end if.
	
Check giving (this is the no touching rule):
	if the player cannot touch the second noun, say "[The second noun] cannot reach anything you might choose to give it at the moment." instead.

Carry out giving: 
	try the second noun trying accepting the noun.

Report giving: 
	say "[The second noun] accepts [the noun][if creature is friendly] gratefully[end if][if creature is curious], poking it and turning it around and around for a moment[end if][if the creature is hostile], all but snatching it from you[end if]." instead.
	
Report giving something stinky to a creature when the odor sensitivity of the second noun is strong:
	now the second noun is passive;
	say "[The second noun] takes [the noun], at arm's length and wrinkling its nose." instead.
	
Report giving something stinky to a creature when the odor sensitivity of the second noun is weak:
	now the second noun is passive;
	say "[The second noun] takes [the noun], apparently unresponsive to its pungency."
	
Report giving something stinky to a creature when the odor sensitivity of the second noun is inverse:
	now the second noun is passive;
	say "[The second noun] takes [the noun], smelling deeply and with obvious pleasure at the stinkiness."
	
Report giving the fedora to a smart creature:
	say "You hand over [the fedora] to [the creature], who takes by the brim and twirls it twice on the end of its claw." instead.
	 

Before giving something worn by the player to the creature:
	try taking off the noun;
	if the player wears the noun, say "You are still wearing [the noun]." instead.

Before the creature trying taking off something when the speed of the creature is slow:
	if a random chance of 1 in 3 succeeds
	begin;
		now the creature is passive;
		if the creature is visible, say "[The creature] fumbles helplessly at [the noun], trying to remove it." instead;
	end if.

Report a creature trying taking off the collar when the creature is slothful:
	say "The creature at last manages to detach [the noun], though with considerable effort." instead.

Report a creature trying taking off something:
	say "[The person asked] removes [the noun] it was wearing." instead.

Before doing something other than examining or looking or taking inventory or waiting when the creature is starving:
	if the creature is carrying something delicious, continue the action;
	if the noun is a thing and the player carries the noun, continue the action;
	if the noun is a thing and the player wears the noun, continue the action;
	if the creature is stupid, continue the action;
	if the creature can see the player and the player is carrying something delicious (called the treat),
		say "In its eagerness for [the treat], [the creature] [if the creature is hostile]circles you, growling, so that you can't get much done[otherwise]clings so desperately to your trouser leg that you can't do much of anything[end if]." instead.
		 	
Every turn: if the player is poisoned, end the game saying "You black out."


Instead of putting a wearable thing on the creature: try dressing the creature in the noun.

Collaring is an action applying to one thing. 

Understand "collar [something]" as collaring.

Check collaring: 
	if the player cannot see the collar, say "You haven't even got a collar." instead;
	if the noun is not the creature, say "A pointless exercise." instead.
	
Carry out collaring:
	try dressing the creature in the collar.

Dressing it in is an action applying to two things.

Check dressing it in: 
	if the second noun is not wearable, say "[The noun] cannot possibly wear [the second noun]." instead;
	if the player is not carrying the second noun
	begin;
		try taking the second noun;
		if the player is not carrying the second noun, stop the action;
	end if;

Carry out dressing it in:
	now the noun wears the second noun.
	
Report dressing it in:
	if the noun is hostile, say "Your victim watches you warily and with not a little distrust, but its limbs are slow and it is unable to put up much fight. ";
	say "You put [the second noun] on [the noun]. Very fetching."
	
After dressing a hungry meaty creature in something: 
	say "[The noun] suffers you to get close enough with [the second noun]; then, for your pains, bites you deeply in the forearm.";
	end the game saying "You spend most of the rest of the evening with a doctor".
	
Instead of dressing a hungry meaty creature in something for the first time:
	say "You get near [the noun] with [the second noun], but it grins very largely and shows you all its teeth, a sight of such menace that you draw back."
	
Understand "dress [something] in [something]" as dressing it in. Understand the commands "clothe" and "attire" as "dress".

Instead of dressing a lightning creature in something: 
	if the creature is blinded
	begin;
		say "At your approach, [the creature] squinches up its eyes.";
		continue the action;
	end if;
	say "[The creature] dodges with lightning speed."

Instead of dressing a fast creature in something: 
	if the creature is meaty, continue the action;
	if the creature is blinded
	begin;
		say "At your approach, [the creature] squinches up its eyes.";
		continue the action;
	end if;
	say "[The creature] moves away too fast for you to succeed."

	
## Chapter 3 - The Player's Clothing and Inventory

The slide is a thing. 

 The salami is edible and stinky and fleshy. Understand "bow" or "ribbon" or "sausage" or "meat" as the salami. The description of the salami is "Rich in garlic and meatiness."

The printed name of the slide is "slide rule". Understand "rule" as the slide. The description of the slide is "One of those things you've carried around instinctively since college. Esther likes to make fun of you for it, but you never know when you're going to have to calculate something."

The player wears a fedora, a jacket, a shirt, an undershirt, a pair of slacks, a pair of undershorts, a pair of socks, and a pair of shoes. The socks are stinky. Understand "shorts" as the undershorts. Understand "pants" or "trousers" as the slacks. Understand "hat" as the fedora. The fedora, the jacket,  and the shoes are leathery. The collar is leathery. The description of the collar is "An ordinary strap with a buckle, the kind you might put on a dog."

Instead of wearing the collar: say "Your neck is insufficiently willowy."

Instead of examining something which underlies something (called the covering): say "It is hard to see [the noun] under [the covering]." 
Report taking off undershorts for the first time:
	say "You strip off your shorts, feeling distinctly self-conscious." instead.

Instead of taking inventory:
	say "You're carrying [a list of things carried by the player][if the player wears something]. You are wearing [a list of uppermost things worn by the player][end if]."

Instead of examining the player:
	if the player wears the jacket and the player wears the slacks and the player wears the fedora, say "You look sharp in your suit and fedora." instead;
	if the number of things worn by the player is 0, say "You are down to your birthday suit." instead;
	if the number of things worn by the player is 1
	begin;
		if the player wears the fedora, say "Aside from your stylish hat, you are nude." instead;
		if the player wears the undershorts, say "At least you still have your undershorts." instead;
		if the player wears the undershirt, say "You're down to only your undershirt, and it is not as long as it could be." instead;
	end if;
	if the player is indecent, say "You're not quite decently clad, put it that way." instead; 
	say "You are feeling a little wilted in the heat."; 
	
Definition: a person is nude if the number of things worn by it is 0. Definition: a person is indecent if it is not wearing the slacks.

Definition: a thing is uppermost if it is not under something.

Underlying relates one thing to various things. The verb to underlie (it underlies, they underlie, it is underlying, it is underlaid) implies the underlying relation. The verb to be under implies the underlying relation.

The shirt underlies the jacket. The pair of socks underlies the pair of shoes. The undershorts underlie the slacks. The undershirt underlies the shirt.

Check taking off:
	if the noun underlies something (called the impediment) which is worn by the player, say "[The impediment] is in the way." instead.

Carry out taking off:
	now the noun is not underlaid by anything.
	
Report taking off something:
	say "You take off [the noun], and are now wearing [a list of uppermost things worn by the player]." instead.

Overlying relates one thing to various things. The verb to overlie (it overlies, they overlie, it is overlying) implies the overlying relation. 

The jacket overlies the shirt. The shoes overlie the socks. The slacks overlie the undershorts. The shirt overlies the undershirt.

Before wearing something when something (called the impediment) which overlies the noun is worn by the player: 
	try taking off the impediment;
	if the player is wearing the impediment, stop the action.
	
Before taking off something which underlies something (called the impediment) which is worn by the player: 
	try taking off the impediment;
	if the impediment is worn by the player, stop the action.
	
Carry out wearing:
	if the noun overlies something (called the hidden item) worn by the player, now the hidden item underlies the noun.

Instead of looking under something which is worn by the player:
	if something (called the underwear) underlies the noun, say "You peek at [the underwear]. Yup, still there.";
	otherwise say "Just you in there.".


The ballpoint pen is a thing. The description of the pen is "It's a Parker 'Jotter', and cost you five dollars, which the Office of Alien Protocol was too mean to pick up, though they were the ones who suggested you avoid fountain pens in your office. [paragraph break]It is not quite out of ink yet."

The brown paper sack is a container. In the brown paper sack is some bread, and a wedge of cheese.

Understand "bag" as the sack. The description of the sack is "An ordinary lunch bag, such as anyone might take to work." The sack is papery. The sack is openable and closed. 

After opening the sack when a stinky thing is in the sack: 
	say "As you open the sack, you catch the pungent smell of [the list of stinky things in the sack] inside[if something which is not stinky is in the sack]. Less offensive to the nostrils [is-are the list of inoffensive things in the sack][end if]."

The cheese is edible and stinky. The description of the cheese is "Pungent: you conclude it must_mw be European cheese. You certainly haven't seen anything like that since Italy." The bread is edible. The description of the bread is "A round and crusty roll." Understand "round" and "crusty" and "roll" as the bread.
	
The tube of holdall glue is a stinky thing. The description is "A truly miraculous substance that sets up rock-hard and air-tight in a matter of milliseconds. It is also covered with toxicity warnings."

Instead of eating the holdall glue:
	say "You decide to disregard the numerous warnings on the outside of the tube and experiment with the taste of the glue. It turns out that it produces a strong burning sensation in the lining of the mouth and throat, accompanied by lung-filling noxious fumes and coughing. Your eyes water and then you fall into a semi-comatose state for a long, long time. The good news is that there is not permanent brain damage, just a lot of misery.";
	end the game saying "Esther has to take you to the hospital, which is never a good end to the day"
	
Report eating a poisoned edible thing:
	say "You eat [the noun], careful to avoid the blob of glue, of course."
	
Instead of putting the glue on something which is not a supporter:
	if the player is not carrying the glue, try taking the glue;
	if the player is not carrying the glue, stop the action;
	now the second noun is poisoned;
	say "You apply a dab of [the glue] to [the second noun]."

The blank label is a thing.

The Guide is a thing. The printed name of the guide is "Guide to Alien Habitats". Understand "book" or "guidebook" or "Guide to Alien Habitats" or "alien habitats" or "guide to habitats" as the Guide. 

The description of the guide is "A Guide To Alien Habitats, for use in the identification and labeling of your little Visitors. 

The table of contents reveals that the book contains the following charts:".

After examining the guide:
	repeat through Table of Guide Information
	begin;
		say "[line break]  [category entry]";
	end repeat;
	say paragraph break.
	
header is a kind of value. The headers are Roughness, Reflexes, Power, Atmosphere, Behavior, and Digestion. Understand "period" or "periodicity" or "day" or "days" or "year" or "years" as reflexes. Understand "surface" or "roughness" or "surface roughness" as roughness. Understand "gravity" or "equatorial" or "mass" or "equatorial gravity" or "surface gravity" or "gravities" or "equatorial surface gravity" as Power. Understand "air" or "atmospheres" or "smell" or "smells" as atmosphere. Understand "mood" or "attitude" or "attitudes" or "behaviour" as Behavior. Understand "food" as digestion.

Understand "look up [header] in [something]" as studying in it about (with nouns reversed). Understand "consult [something] on/about [header]" as studying in it about. Understand "read about [header] in [something]" as studying in it about (with nouns reversed). Understand "read [header] in [something]" as studying in it about (with nouns reversed). Understand "read about [header] in [guide]" as studying in it about (with nouns reversed). Understand "read [header] in [guide]" as studying in it about (with nouns reversed).

Understand "look up [header]" as wondering about.

Wondering about is an action applying to one header. Before wondering about when the player can see the Guide and the player is not carrying the guide: try taking the guide; if the player is not carrying the guide, stop the action. Check wondering about: if the player is not carrying the guide, say "You don't have the Guide, in which you do most of your current research." instead. Carry out wondering about: try studying in the Guide about the header understood.

Studying in it about is an action applying to one thing and one header. Carry out studying in something about: say "You find no useful information." 

Instead of studying in the Guide about:
	if there is a category of the header understood in the Table of Guide Information
	begin;
		say "You flip through the Guide and find the following:[paragraph break]";
		 choose row with a category of the header understood in the Table of Guide Information; say "[reply entry][paragraph break]";
	otherwise;
		say "You flip through the guide unavailingly, though you do notice that a few pages have apparently been ripped out." instead;
	end if.
	
Table of Guide Information
category	difficulty	reply
Atmosphere	medium	"Note: Sense of smell is likely to occur only where atmosphere is present. [paragraph break][atmospheres]"
Behavior		medium	"A creature with hostile or friendly feelings has frequent human contact, thus probably from one of the inner planetary systems[if Luna-X is a moon listed in the Table of Alien Characteristics]. In particular, creatures from the Earth-X system are hostile[end if]. Creatures from Mars and Luna are always friendly. 

A creature with curious or frightened feelings is more likely from one of the systems beyond the Asteroids; Tritonians are known to be curious, those from Pluto quite timid and easily scared."
Digestion		easy	"Note: The colder and more distant its home-world, the slower its feed time is likely to be (though note Venus is hotter than Mercury). May be difficult to judge relevance of this factor without experience. [paragraph break][systems][paragraph break][foods]"
Power		medium	"Note: The greater the surface gravity of the home-world, the stronger the inhabitant will be. Lighter/smaller creatures are also more susceptible to poisons. [paragraph break][gravities]"
Reflexes		easy	"Note:  Speed of reflexes appear to correlate roughly with the periodicity of the home-world -- planetary days or period of orbit for moons. Those from faster orbits are thus likely to move more quickly. [paragraph break][periodicities]"
Roughness	easy	"Note: The rougher the world, the more likely that it will breed creatures built for climbing and grasping. Second note (added in agitated marker): Gas giant critters have two arms anyway! [paragraph break][roughnesses]"


To say foods:
	say "List of Alien Home-worlds in Sol System (by Food type):[line break]"; 
	let current food be meaty; 
	repeat through Table of Alien Characteristics in taste column order
	begin; 
		if taste entry is not current food
		begin;
			change current food to taste entry;
			say "[line break]  [bold type][current food][roman type]: ";
		otherwise;
			say ", ";
		end if;
		say "[moon entry]";
	end repeat;
	say paragraph break.

To say roughnesses:
	say "List of Alien Home-worlds in Sol System (by Roughness):[line break]"; 
	let count be -1;
	repeat through Table of Alien Characteristics in arms column order
	begin; 
		if arms entry is not count
		begin;
			change count to arms entry;
			say "[line break]  [bold type][count as roughness][roman type]: ";
		otherwise;
			say ", ";
		end if;
		say "[moon entry]";
	end repeat;
	say paragraph break.
	
To say (count - a number) as roughness:
	if count is 0, say "Gaseous";
	if count is 1, say "Very smooth";
	if count is 2, say "Smooth";
	if count is greater than 2 and count is less than 5, say "Rough/cratered";
	if count is greater than 4, say "Very rough/cratered";

To say systems:
	say "List of Alien Home-worlds in Sol System (by System):[line break]"; 
	let planetoid be Pluto;
	repeat through Table of Alien Characteristics in system column order
	begin; 
		if system entry is not planetoid
		begin;
			change planetoid to system entry;
			say "[line break]  [bold type][planetoid][roman type]: ";
		otherwise;
			say ", ";
		end if;
		say "[moon entry]";
	end repeat;
	say paragraph break.
	
To say atmospheres:
	say "List of Alien Home-worlds in Sol System (by Atmosphere):[line break]"; 
	let scent be inverse; 
	repeat through Table of Alien Characteristics in nostrils column order
	begin; 
		if nostrils entry is not scent
		begin;
			change scent to nostrils entry;
			say "[line break]  [bold type][scent in words][roman type]: ";
		otherwise;
			say ", ";
		end if;
		say "[moon entry]";
	end repeat;
	say paragraph break.
	
To say (scent - an odor sensitivity) in words:
	if scent is weak, say "None";
	if scent is strong, say "Primarily oxygen, hydrogen, or nitrogen";
	if scent is inverse, say "Significant sulfuric or methane components";

To say periodicities:
	say "List of Alien Home-worlds in Sol System (by periodicity category):[line break]"; 
	let rapidity be lightning; 
	repeat through Table of Alien Characteristics in dexterity column order
	begin; 
		if dexterity entry is not rapidity
		begin;
			change rapidity to dexterity entry;
			say "[line break]  [bold type][rapidity in words][roman type]: ";
		otherwise;
			say ", ";
		end if;
		say "[moon entry]";
	end repeat;
	say paragraph break.
	
To say (rapidity - a speed) in words:
	if rapidity is lightning, say "Less than one Earth day";
	if rapidity is fast, say "One to six Earth days";
	if rapidity is moderate, say "One Earth week to one Earth month";
	if rapidity is slow, say "More than one Earth month".

To say gravities:
	say "List of Alien Home-worlds in Sol System (by effective weight at surface):[line break]"; 
	let size be negligible; 
	repeat through Table of Alien Characteristics in reverse mass column order
	begin;
		if mass entry is not size
		begin;
			change size to mass entry;
			say "[line break]  [bold type][size][roman type]: ";
		otherwise;
			say ", ";
		end if;
		say "[moon entry]";
	end repeat;
	say paragraph break.
	
Color is a kind of value. The colors are red, orange, yellow,  green, cyan, blue, indigo, purple,  tan, black, white, and grey. The creature has a color.

The creature is an animal. The description is "It's an attractive shade of [color of creature], really, when you think about it, and it looks a little bit like a [if the creature is slothful]sloth[otherwise]chimpanzee[end if][if the carrying capacity of the creature is not 2] -- with [carrying capacity of the creature in words] arm[s], and [otherwise]. Only with [end if]a [if the odor sensitivity of the creature is strong or the odor sensitivity of the creature is inverse]larger, more sensitive[otherwise]smaller[end if] nose, and ears that come to points, and [tongue of creature]... [paragraph break][if the creature has something]It carries [a list of things carried by the creature]. It wears [a list of things worn by the creature].[otherwise]Odd, really.[end if]" 

Understand the color property as describing the creature. 

The creature has a moon. The creature has a speed. The creature has an intelligence.  The creature has a food. The metabolism of the creature is 50.

The creature wears a collar. Understand "Visitor" or "animal" or "alien" or "arms" or "arm" or "paw" or "paws" or "nose" or "tongue" or "teeth" as the creature.  The creature has some text called tongue. The tongue of the creature is "raspy tongue".




## Chapter 4 - The Office	

Esther is a woman. Esther's utterance is some text that varies. Esther's utterance is "blank".  

Understand "ask Esther [text]" or "tell Esther [text]" or "talk to esther" as a mistake ("[if the player is in the office]The noises from Esther's office pause just long enough that you know she heard you and isn't answering[otherwise]Not really an option just now[end if].") when Sending is happening. 

Understand "call [any thing]" or "summon [any thing]" or "shout to/for/at [any thing]" or "call to/for [any thing]" or "yell to/for/at [any thing]" as summoning.

Summoning is an action applying to one thing.

Carry out summoning: say "There is no answer."

Instead of summoning a timid creature: say "[The creature] flinches, covering its ears."
 

Instead of summoning Esther for the first time: 
	say "'Oh, no you don't,' Esther shouts back. 'I'm not coming in there to help you even once more.'"

Instead of summoning Esther: 
	say "You yell plaintively to Esther, but she doesn't come in."

Instead of going nowhere from the Office: say "You really can't go anywhere while the Visitor is still here to be looked after." Instead of going outside from Office on Floor Fifty-One: say "Not until the creature is dealt with."

Grim Hallway is a room. "Months of your peculiar line of work have brought certain changes. Most of the decorations have been stripped from the walls, giving it a bare, prison-like feel. There is nothing that a particularly vicious creature could take, eat, throw, or use as a weapon. One or two patches in the plaster testify to hasty repair jobs.

Just inside is your office."  The player is in Grim Hallway.

Instead of listening to the Grim Hallway: say "Esther's typing is audible from the other room. It's either a very long expense report or a novella."

The potted plant is fixed in place in the Grim Hallway. Understand "bush" or "cactus" or "tree" or "christmas" or "spindly" or "woebegone" or "dirt" or "pot" as the plant. "A [potted plant] lurks in the corner, perennially a little dry." The description of the plant is "The dirt is often useful for concealing things you need to keep from Esther: she has the duty of cleaning out your office." 

Burying relates various things to various things. The verb to disguise (it disguises, they disguise, he is disguising) implies the burying relation.  Instead of digging the potted plant when the potted plant disguises something (called target): say "You dig through the dirt of [the potted plant] and retrieve [the target]."; move the target to the player; now the plant does not disguise the target. Instead of digging the potted plant: say "You don't turn up any hidden treasures this time."

Instead of searching the potted plant: say "A preliminary look turns up nothing, but you'd have to dig into the dirt to see whether anything is deeper in."

[And because beta-testers wanted to be able to rebury things:]

Understand "conceal [something] in [something]" or "hide [something] in [something]" or "bury [something] in [something]" as hiding it in. Hiding it in is an action applying to two things. Check hiding it in: if the second noun is not a container, say "[The second noun] will not conceal much." instead; if the second noun is not openable, say "[The second noun] cannot be closed." instead; if the player does not carry the noun, try taking the noun; if the player does not carry the noun, stop the action. Carry out hiding it in: try inserting the noun into the second noun; if the noun is in the second noun and the second noun is openable, try closing the second noun.

Understand "dig in [something]" as digging. Understand "dig [something]" as digging. Digging is an action applying to one thing. Carry out digging: say "Pointless."

Before hiding something in the potted plant when the player does not carry the noun:
	if the player wears the noun, try taking off the noun;
	otherwise try taking the noun;
	if the player does not carry the noun, stop the action.

Instead of hiding something in the potted plant: remove the noun from play; now the potted plant disguises the noun; say "You bury [the noun] out of sight in the dirt of the potted plant.".

The potted plant disguises the packet of Atomic Fireballs. 
 
Instead of waiting in the Grim Hallway: say "You try postponing the inevitable, but the critter does await you inside."

Instead of going nowhere from the Hallway, say "In seems to be the operative direction here."

Office on Floor Fifty-One is inside from Grim Hallway. "Your office[if the number of scenery things in the office is less than three]. Recent remodeling efforts have left it positively spacious, with only [the list of scenery things in the office] as permanent features[otherwise]. It's cramped, what with [the list of scenery things in the office], but it's got a great view down Fifth Avenue[end if]."

The window is scenery in the office. Understand "view" as the window. The description is "Fair and hot, the temperatures in the eighties and the sky hazy. In short, it's a good day to be in Cleveland. You're not." Instead of searching the window, try examining the window. Before opening the window: say "In the normal course of things, windows on the fifty-first floor do not open.

Your particular window does, of course, since the pods have to get out somehow, but the whole thing is rigged with machinery you can't control. You've heard rumors that your predecessor sent a pod out straight through the plate glass, hospitalizing ten people from the sidewalk below. Best not to meddle." instead.
	
The desk is scenery in the Office. It is a supporter. The description is "Grim, gun-metal grey, but quite sturdy, impervious to blows, and washable. All of these features have come in useful in your work."

The chair is scenery in the Office. It is an enterable supporter. The description of the chair is "Made of slightly less sturdy matter than the desk, and therefore the fourth you have owned in the course of this job. You also used to have a potted cactus, but it got eaten, in an incident that did not end happily for anyone."

The file drawer is scenery in the Office. It is a container. Understand "drawers" or "cabinets" or "cabinet" as the drawer. A lot of paperwork is a thing. The file drawer is closed and openable. The autographed picture of Joe DiMaggio is on the desk. The description of the picture is "He's mid-swing, and the autograph is made out to you personally." Before printing the name of the file drawer: if the file drawer is damaged, say "severely dented".

After opening the file drawer when the file drawer contains more than five things:
	say "You open the file drawer with a bit of struggle -- something in there is catching when you pull. But it finally comes open, and half-disgorges [a list of things in the file drawer]."

The paperwork is fixed in place. Understand "files" as paperwork. The paperwork is papery.

The description of the paperwork is "Copies of previously filed forms (mostly done by your predecessor), indexed by home-world." Understand "papers" or "forms" or "form" as the paperwork. Instead of consulting the drawer about something: try consulting the paperwork about it.

Understand "look up [moon] in [something]" as researching in it about (with nouns reversed). Understand "consult [something] on/about [moon]" as researching in it about. Understand "read about [moon] in [something]" as researching in it about (with nouns reversed). Understand "read [moon] in [something]" as researching in it about (with nouns reversed). Understand "look up [moon]" as researching vaguely.

Researching vaguely is an action applying to one moon. Before researching vaguely when the paperwork is in the drawer and the drawer is closed and the drawer is openable: try opening the drawer. Check researching vaguely: if the paperwork is not touchable, stop the action; Carry out researching vaguely: say "You turn to your trusty paperwork...[line break]"; try researching in paperwork about the moon understood.
 
Researching in it about is an action applying to one thing and one moon.

Carry out researching in it about: say "You find nothing of interest in [the noun]."

Instead of researching in paperwork about a moon listed in the Table of Alien Characteristics:
	let target be the moon understood;
	say "[target]: Atmosphere -- [nostrils of target in words]; Gravity -- [mass of target]; Periodicity -- [dexterity of target in words]; Surface roughness -- [arms of target as roughness]. ";
	if target is a moon listed in the Table of Paperwork Information
	begin;
		choose row with moon column of target in the Table of Paperwork Information;
		say "[form entry][paragraph break]";
	otherwise;
		say "[paragraph break]";
	end if;
	
Table of Paperwork Information
moon	form
Io	"Records indicate that creature from Io was found to have an abnormally short feed time for their system, and live in zones associated with volcanic hotspots."
Europa	"Creature registered to Europa. Time between feedings recorded to be 20 minutes."
Pluto	"Plutonian creature had long slow eating cycle, 45 minutes between feedings; ability to withstand hunger superior to other creatures tested from nearer systems. Dislike of strong smells. Eats cloth. Light sensitive."
Deimos	"Creature registered to Deimos. Displayed signs of developed pattern recognition, religious or hierarchical affiliation with the home planet. Distinctly dissimilar attitude from more hostile Phobos creature captured earlier."
Titan	"Extremely dense atmosphere, with pressures similar to being 20 feet under water."
Uranus	"Uranian unusually shows some slight metallic traces in skin, eyes, and fur, not visible in low light."
Titania	"Titania creature showed low intelligence; but unusual interest in wood pulp products: perhaps only distinguishing characteristic from Oberon creature. Unclear whether creatures of other home-worlds might also prefer wood pulp products."
Oberon	"Oberon creature distinguishable from Titania creature in that it does not share the Titanian's preference for paper and cardboards."
Triton	"Triton is also notable for its observed red/grey color changes over the course of seasons."

Instead of taking the paperwork: say "Esther will never forgive you if you get the files out of order. You could look up individual worlds, though, if you wanted."

The picture and the label are papery.

Understand "files" as the paperwork. Understand "files" as the file drawer when the file drawer is closed.


The lamp is a fixed in place electric light. It is switched off.  Understand "light" as the lamp. The description of the lamp is "Exceptionally bright, to help you pick out any features you might need to observe[if we have not examined the lamp]. Besides, the interrogation-office look comes in handy when you have to convince people you're a private dick. Your mother, for instance, operates under the happy delusion that you spend your days roughing up gangsters and sweating the truth out of cheating husbands[end if]."

Report switching on the lamp:
	say "You turn on [the lamp], casting an unforgiving glare over the whole of your office." instead.

Report switching on the lamp when the creature is cold: 
	say "You turn on [the lamp], casting an unforgiving glare over the whole of your office. [The creature] creeps closer to it, basking." instead.

Report switching on the lamp when the creature is blinded: 
	say "You turn on [the lamp], casting an unforgiving glare over the whole of your office[if the creature is slothful]. The creature curls into a tight ball of misery[otherwise]. The creature backs away, blinking and whining[end if]." instead.

The packet of Atomic Fireballs is a thing. The description of the Fireballs is "The outside of the packet says ATOMIC FIREBALL with RED HOT FLAVOR. The Ferrara Pan logo is written across a cartoon atomic explosion[if we have not examined the fireballs].

The candy is a vice you do your best to conceal: Esther would rather you took up smoking[end if]." The Fireballs are edible. After eating the fireballs, say "You cram the candies into your mouth. For a moment it feels as though the top of your head is going to come off, but the feeling does pass."


The setup rules is a rulebook. 

A setup rule when the moon of the creature is Jupiter:
	remove the lamp from play; remove the collar from play;
	now the shattered bits are explored;
	now the shattered bits are in the office. 
		
A setup rule: do nothing. 

To say creature clothing:
	if the creature wears something
	begin;
		if the number of things worn by the creature is 1 and the creature wears the collar, say "An optimist has tied a collar around its neck";
		otherwise say "It is currently clad in [a list of things worn by the creature]";
	otherwise;
		say "It has gotten rid of the collar";
	end if;
	
Rule for writing a paragraph about the creature: 
	if the creature is in the location
	begin;
		if the creature is blinded, say "Huddling in the far corner is your Visitor: ";
		otherwise say "In front of [the random fixed in place thing in the location][if the creature is starving], in a miserable heap,[end if] is your Visitor: ";
	otherwise;
		let the spot be the holder of the creature;
		if the spot is a container, say "In [the spot] is your Visitor: ";
		otherwise say "On [the spot] is your Visitor: ";
	end if;
	say "a [if the creature is vast]stocky [end if]creature about [if the creature is tall]five[otherwise]four[end if] feet tall, with [color of the creature] ";
	if the creature is scaly, say "scales. ";
	if the creature is furry, say "fur. ";
	if the creature is not furry and the creature is not scaly, say "skin. ";
	if the creature wears something
	begin;
		if the number of things worn by the creature is 1 and the creature wears the collar, say "An optimist has tied a collar around its neck. ";
		otherwise say "It is currently clad in [a list of things worn by the creature]. ";
	end if;
	if the creature carries something, say "It is carrying [a list of things carried by the creature]. ";
	say paragraph break.
	
Before listing nondescript items:
	if the creature is marked for listing, change the creature to not marked for listing;
	if the number of marked for listing things is 0 and the location is the Office, say "Your floor is atypically clear of obstructions.";
	
Rule for listing nondescript items of the Office:
	say "[if the number of marked for listing things is greater than 1]Scattered about[otherwise]Pushed over under the window[end if] ";
	list the contents of the Office, as a sentence, tersely, listing marked items only, prefacing with is/are, including contents and giving brief inventory information;
	say "."

The X1 Probe kit is a heavy closed openable container in the Office on Floor Fifty-One. Understand "large" and "cardboard" and "box" and "picture" and "mars" as the kit. The kit is papery. It is enterable. The description is "[if we have examined the kit]A[otherwise]Left for you, along with the Visitor itself, by the representatives of the Office of Alien Protocol. It is a[end if] large cardboard box containing everything you need to launch your very own alien Visitor back into space. By way of example, it has a cheery full-color telescope picture of Mars on the side." After opening the Probe kit when the kit has not been open: say "You pull open the top of the Probe Kit, revealing the large [pod]. 

Packed beneath it are the heavier pieces: [a propulsion unit] and [a life-support unit]. [The instruction booklet] has fallen down the side, along with [the blank label] you will need to address the probe."
 
In the kit is a propulsion unit, a life-support unit, a tinted egg pod, a blank label, and an instruction booklet. Understand "life" or "support" as the life-support unit. The egg is an openable closed container. The egg is transparent and enterable. The egg is medium-weight. The description of the egg is "It is made of a strong, clear but tinted material[if something is in the egg], and currently contains [a list of things in the egg][end if]." After taking the pod for the first time, say "For something the size of a Frigidaire, the pod is surprisingly light and manageable. Amazing what they can do with plastics nowadays."

The propulsion unit is a device. The life-support unit is a device. Before printing the name of the life-support unit: if the life-support unit is switched on, say "activated "; otherwise say "inactive ".

Instead of a creature trying switching on the life-support unit when the life-support unit is not stuck to the egg:
	if the person asked is visible, say "[The person asked] plays with the life-support toggle, but to no effect, since the unit is not yet stuck to the pod[if the person asked is friendly].
	
It looks up at you inquisitively, as though seeking an explanation for this malfunction[end if]."

Instead of switching on the life-support unit when the life-support unit is not stuck to the egg:
	say "You toggle the switch a few times, but the life-support unit refuses to power up until it is stuck to the pod."

Report switching on the life-support unit:
	say "You flip a switch on the life-support unit, and blue lights spring up all along the side." instead.
	
Report a creature trying switching on the life-support unit: 
	say "[The person asked] touches the switch of the life-support unit. The blue lights spring up along the side, indicating proper function." instead.
	
Report a creature trying switching on the life-support unit when the life-support unit was switched on:
	say "[if the person asked can see the booklet][The person asked] shakes its head at you and points out this step -- turn on life support -- in the booklet. [end if]With a cluck of impatience, it switches the life-support unit on once again[if person asked is in egg] from inside[end if]." instead.	
	
Report switching off the life-support unit:
	say "You flip a switch on the life-support unit; the blue lights die back out." instead.

Definition: a person is Martian:
	let the origin be moon of it;
	if the system of origin is Mars, yes; 
	no.


Report a Martian person trying playing with the kit:
	say "The creature presses its nose sadly against the Mars image on the side of the kit." instead.
	
Instead of giving the kit to the creature when the creature is Martian:
	say "The creature seems excited by the giant picture of Mars on the side[if the moon of the creature is not Mars], and bows to it several times[end if]."

The description of the life-support unit is "A compact cube full of compressed gasses and various other useful things. It is fairly heavy." The life-support unit is heavy.



Report the creature trying playing with the available life-support unit:
	if the gravity of the creature is less than Marslike, say "[The creature] tries without success to pick up [the propulsion unit]." instead;
	otherwise say "[The creature] picks up [the propulsion unit] and turns it over in its claws for a minute, poking long fingers into the tubing before setting the whole thing back down." instead.
	
Before a creature trying wearing something when the person asked is not carrying the noun:
	if the person asked wears the noun, do nothing instead;
	otherwise try the person asked trying taking the noun instead.
	
Before a friendly creature trying playing with the fedora:
	try the creature trying wearing the fedora instead.
	
Before a stupid creature trying playing with the socks when the creature carries the socks:
	if the creature is strong, continue the action;
	otherwise try the creature trying wearing the socks instead.

Before a stupid creature trying playing with something when a random chance of 1 in 3 succeeds:
	now the creature is passive;
	try the creature trying showing confusion at the noun instead.
	
Showing confusion at is an action applying to one thing.

Carry out a creature trying showing confusion at: do nothing.

Report a creature trying showing confusion at: 
	say "[The creature] blinks at [the noun] uncertainly." 
	
Report a creature trying showing confusion at the chair:
	say "[The creature] studies [the chair], puzzled. Perhaps they don't sit down on its planet."
	

Before a stupid creature trying playing with something when a random chance of 1 in 3 succeeds:
	now the creature is passive;
	if the creature is hostile,
		 say "[The creature] watches you belligerently[if the carrying capacity of the creature > 2] with two of its [carrying capacity of the creature in words] hands[end if] on its hips." instead;
	otherwise say "[The creature] scratches its head[if the carrying capacity of the creature > 2] with one of its [carrying capacity of the creature in words] hands[end if]." instead.

	
Report a stupid creature trying wearing the socks:
	say "[The creature][quickly] puts on your socks -- one over each ear." instead.
	
Report a creature trying wearing the fedora:
	say "[The creature] dons the fedora[if the creature is smart] and flicks the brim at you[end if]." instead.

Report a fast creature trying playing with something delicious:
	if the noun is undershorts, 
		say "[The creature] darts its tongue out to [the noun], sampling the flavor. Then it looks appalled and begins spluttering and spitting." instead;
	otherwise
		say "[The creature] darts its tongue out to [the noun], sampling the flavor[if the creature is friendly]. It gives you a shy look afterward, as though caught at some embarrassing practice[end if]." instead.
		
The description of the propulsion unit is "An extremely heavy block of chrome tubing." The propulsion unit is heavy. 

Report the creature trying playing with the available propulsion unit:
	if the gravity of the creature is less than Earthlike, say "[The creature] tries without success to pick up [the propulsion unit]." instead;
	otherwise say "[The creature] picks up [the propulsion unit] and turns it over in its claws for a minute, poking long fingers into the tubing before setting the whole thing back down." instead.
	
To decide whether (item - a thing) outweighs strength:
	if the item is light, no;
	if the gravity of the creature is greater than fractional and the item is medium-weight, no;
	if the gravity of the creature is greater than Marslike, no;
	yes.
	
Report the creature trying playing with the chair:
	if something (called the impediment) is on the chair
	begin;
		if the impediment outweighs strength, say "[The creature] swivels the chair." instead;
		otherwise say "[The creature] shoves impotently at the chair, which does not budge." instead;
	otherwise;
		if the creature is stupid, say "[The creature] turns the chair around. And around. And around and around and around. This is apparently the very greatest game ever invented." instead;
		otherwise say "[The creature] swivels the chair to and fro." instead;
	end if.
	 
The description of the booklet is "The booklet explains, in friendly cartoons, how to attach the propulsion unit, the life-support unit, and the label to the egg, then turn the life-support and the propulsion units on. (It's a little vague on how you lure your Visitor into the egg.)

In tiny letters at the bottom it says: HOLDALL GLUE NOT INCLUDED." The booklet is papery. Understand "book" as the booklet.


## Chapter 8 - The Capsule
	
### Section 1 - Glue and Gluing

Instead of tying something to something, say "What are you, an Eagle Scout?"

Before putting something on something when the noun fits the second noun:
	try gluing the noun to the second noun instead.
	
Understand "use [glue]" as a mistake ("You will have to be a bit more specific about how: e.g., GLUE FISH TO ME, PUT GLUE ON COOKIE, GLUE THE DRAWER SHUT, etc.")

Understand "use [blank label]" as a mistake ("You will have to say how: e.g., LABEL DRAWER, WRITE EARTH ON LABEL.")

Understand "use [filled label]" as a mistake ("You will have to say how: e.g., LABEL DRAWER, WRITE EARTH ON LABEL.")

Understand "use [pen]" as a mistake ("You will have to say how: e.g., THROW PEN AT ESTHER, WRITE EARTH ON LABEL, etc.")

Understand "use [guide]" as a mistake ("You may look things up in the guide: for instance, CONSULT GUIDE ABOUT PERIODICITY or LOOK UP SYSTEMS IN GUIDE.")

Understand "use [paperwork]" as a mistake ("You may look up individual home-world files in the paperwork: for instance, LOOK UP MARS IN PAPERWORK.")

Understand "use [a wearable thing]" as wearing. Understand "use [an edible thing]" as eating. Understand "use [an enterable container]" as entering.

Understand "label [something]" as labeling.

Labeling is an action applying to one thing.

Before labeling something when the player does not carry the blank label and the player does not carry the filled label:
	if the player can see the filled label, try taking the filled label;
	if the player can see the blank label, try taking the blank label;
	if the player does not carry the blank label and the player does not carry the filled label, stop the action.

Carry out labeling:
	let the means be the blank label;
	if the player carries the filled label, let the means be the filled label;
	try gluing the means to the noun.
	

Understand the command "attach" as something new. Understand the command "stick" as something new.

Fitting relates things to each other. The verb to fit (it fits, they fit, it fitted, it is fitted) implies the fitting relation.

Sticking relates things to each other in groups. The verb to be stuck to implies the sticking relation.

Definition: a thing is free if it is not stuck to the egg.

Understand "stick [a free thing] to/on/onto [something]" as gluing it to. Understand "stick [something] to/on/onto [a free thing]" as gluing it to. Understand "stick [something] to/on/onto [something]" as gluing it to. Understand the commands "glue" or "paste" as "stick".

Understand "stick [something] shut/closed" as gluing it shut. Understand "stick shut/closed [something]" as gluing it shut.

Understand the command "attach" as "stick".

Gluing it shut is an action applying to one thing.

Before gluing an open thing shut: 
	if the player is not carrying the glue, say "You are glue-less." instead;
	try closing the noun; if the noun is open, stop the action.

Check gluing something shut: 
	if the noun is unopenable, say "[The noun] is already unable to be opened." instead;
	abide by the need glue rule.

Carry out gluing something shut:
	change the noun to unopenable.

Report gluing something shut:
	say "With a long line of holdall, you glue [the noun] permanently closed."
	
Instead of closing a damaged unopenable container:
	say "[The noun] no longer really closes."	
	
Instead of opening a closed unopenable container:
	say "You can no longer open [the noun]."
	

Instead of a creature trying gluing something shut when the person asked is not carrying the glue:
	if the person asked can see the glue, try the person asked trying taking the glue;
	continue the action.

Check a creature trying gluing something shut:
	if the noun is unopenable, stop the action;
	if the person asked is not carrying the glue, stop the action.

Carry out a creature trying gluing something shut:
	change the noun to unopenable.

Report a creature trying gluing something shut:
	say "Smiling to itself, [the creature] glues [the noun] permanently closed."


This is the need glue rule:
	if the glue is visible and the player is not holding the holdall glue
	begin;
		try taking the glue;
		if the player is not holding the glue, stop the action;
	end if;
	if the player is not carrying the holdall glue
	begin;
		say "You need some glue before that's going to work.";
		stop the action;
	end if.

Instead of gluing the egg to the egg, try gluing the egg shut. 

Gluing it to is an action applying to two things.

Check gluing something to something:
	abide by the need glue rule;  
	if the noun is the second noun, say "Topology is against you." instead;
	if the noun fits the second noun, do nothing;
	otherwise say "[The noun] and [the second noun] do not belong together." instead;
	if the noun is stuck to the second noun, say "[The noun] is already connected to [the second noun]." instead.

Carry out gluing something to something:
	[if the egg is the noun, now the second noun is part of the egg;
	otherwise now the noun is part of the egg;]
	now the noun is part of the second noun;
	now the noun is stuck to the second noun.
 

Report gluing something to something:
	if the number of things stuck to the egg is 2
	begin;
		say "Meticulously, so as not to get a single drop of the stuff on yourself, you apply glue to the surfaces of [the noun] and [the second noun] and then press them together.";
		if the creature is visible
		begin; 
			now the creature is passive;
			if the creature is playful, say "[line break][The creature] watches with interest, biting its lower lip.";
		end if;
	otherwise;
		say "You carefully glue [the noun] to [the second noun].";
	end if.
	
	
Instead of gluing the egg to something:
	try gluing the second noun to the egg.
	
Report examining something when something is part of the noun:
	say "Attached to [the noun] [is-are list of things which are part of the noun]." instead.
	
Instead of taking something which is part of something (called the source):
	say "The holdall glue defeats your strength. [The noun] is now permanently attached to [the source]."
	
Instead of gluing something to the creature:
	say "Tempting though it may be to stick things to your little friend, you would probably be considered to be endangering its health."

Check a creature trying gluing something to something: 
	if the person asked is not carrying the glue, stop the action;
	if the noun is the second noun, stop the action;
	if the noun fits the second noun, do nothing;
	otherwise stop the action;
	if the noun is stuck to the second noun, stop the action;

Carry out a creature trying gluing something to something:
	if the egg is the noun, now the second noun is part of the egg;
	otherwise now the noun is part of the egg;
	now the noun is stuck to the second noun.

Report a creature trying gluing something to something:
	say "[The person asked][quickly] glues [the noun] to [the second noun][if the person asked is curious], then glances at you for approval[end if]."
	
Report a creature trying gluing something to something when the number of things stuck to the pod is 2 and the player is not in the pod:
	say "[The person asked] efficiently applies glue to [the noun] and [the second noun] and puts them together.";
	change Esther's utterance to "'If the boys knew you got the aliens to do all the work for you, they wouldn't pay you half as much,' comments Esther from the door.

'And I pay you to answer phones,' you remark. 'Have you got another alien on desk duty?'

The creature looks back and forth between the two of you, frowning. Esther withdraws." instead.

Report a creature trying gluing something to something when the number of things stuck to the pod is 3:
	let leftover be a random free thing which fits the pod;
	say "[The person asked] sticks [the noun] and [the second noun] together, leaving only [the leftover] to be glued in place." instead.

[After gluing something to the pod when the creature is in the pod:
	say "The creature watches with interest from the inside of the pod as you glue on [the noun]."]
	
Report examining something when something alternate is stuck to the noun:
	say "[The noun] is attached to [the list of alternate things which are stuck to the noun]." instead.
	
Definition: a thing is alternate if it is not the noun.

The propulsion unit fits the egg. The life-support unit fits the egg. The blank label fits the egg. The filled label fits the egg. The filled label has a moon.

Instead of gluing something to the player: say "That is the sort of thing you might do by accident, but not on purpose. Not unless you want to hear about it from Esther for the next three years."

Instead of gluing the player to something: try gluing the second noun to the player.

Report taking something:
	say "You pick up [the noun][if the noun is heavy], though with a bit of effort[end if]." instead.
	
Report dropping something:
	say "You set down [the noun]." instead.
	
Understand the command "wear" as something new. Understand "wear [something]" as wearing. Before wearing something which is not carried by the player: try taking the noun; if the noun is not carried by the player, stop the action.

Understand the command "give" as something new. Understand "give [something] to [something]" as giving it to. Understand "give [something] [something]" as giving it to (with nouns reversed). Before giving something which is not held by the player to someone: try taking the noun. Before giving a person to an inanimate thing: try giving the second noun to the noun instead. 

Understand the command "show" as something new. Understand "show [something] to [something]" as showing it to. Understand "show [something] [something]" as showing it to (with nouns reversed). Before showing something which is not held by the player to someone: try taking the noun. Before showing a person to an inanimate thing: try showing the second noun to the noun instead.

Understand the command "eat" as something new. Understand "eat [something]" as eating. Understand "eat [something]" as eating. Before eating something which is not held by the player: try taking the noun.

Before entering a closed container: try opening the noun; if the noun is closed, stop the action.
	

### Section 2 - The Label

Understand "write on/out [text]" as a mistake ("You need to write the name of a home-world -- for instance, WRITE LUNA ON..."). Understand "fill out/in [text]" as a mistake ("You need to write the name of a home-world -- for instance, WRITE LUNA ON THE LABEL."). 

Understand "write [moon] on [something]" as filling it out on.

Filling it out on is an action applying to one moon and one thing.

Setting action variables for filling:
	if the second noun is the egg and the blank label is stuck to egg: 
		change the second noun to blank label;
	if the second noun is the egg and the filled label is stuck to egg: 
		change the second noun to filled label.

Check filling it out on:
	if the second noun is not the blank label and the second noun is not the filled label, say "You can't fill out [the second noun], only the label." instead. 

Check filling it out on:
	if the player is not carrying the ballpoint:
		if the ballpoint is visible, try taking the ballpoint;
		otherwise say "Now where did you set down your pen, anyway?" instead;
		if the player is not carrying the ballpoint, stop the action;
	
Check filling it out on: 
	if the second noun is the blank label, fix blank label.

To fix blank label:
	if the blank label is part of something (called the parent):
		now the filled label is part of the parent; 
	otherwise:
		move the filled label to the holder of the blank label;
	if the blank label is stuck to the egg: 
		now the filled label is stuck to the egg;
		now the blank label is not stuck to the egg;  
	remove the blank label from play;
	change the second noun to the filled label.

Check filling it out on:
	if the second noun is the filled label:
		say "First you cross out ";
		if filled label is alien-marked, say "[fixed letter spacing][tattoo-mark of the moon of the creature][variable letter spacing]";
		otherwise say "[moon of filled label]";
		say " vigorously with many strokes of the pen..."  
	
Carry out filling it out on:
	now the second noun is human-marked; 
	change moon of the second noun to the moon understood.
	
Report filling it out on:
	say "You write [moon understood] firmly on the label."
	 
	
Before a creature trying filling moon understood out on the blank label when the person asked is not carrying the pen:
	try the person asked trying taking the pen instead.
	
Before a creature trying filling  moon understood out on the blank label when the person asked cannot touch the blank label:
	try the person asked trying taking the blank label instead.
	
Carry out someone trying filling the moon of the creature out on the blank label:
	fix blank label;
	now the filled label is alien-marked;
	change moon of the second noun to the moon of the creature.
	
Report a creature trying filling the moon of the creature out on something:
	let destination be the moon of the creature;
	choose row with a moon column of destination in Table of Alien Characteristics; 
	say "Tongue caught between its teeth, [the creature] writes [fixed letter spacing][tattoo-mark entry][variable letter spacing] on [the second noun]."
	
The description of the filled label is "[if human-marked]In your familiar handwriting it registers the origin of the creature as [moon of the filled label][otherwise]In alien writing it says [fixed letter spacing][tattoo-mark of the moon of the creature][variable letter spacing][end if]." The filled label can be alien-marked or human-marked.

The description of the blank label is "Intended to identify the alien to whatever interplanetary post office is responsible for these shipments."



### Section 3 - Propelling

Before switching on the pod when the propulsion unit is part of the pod:
	try switching on the propulsion unit instead.

Instead of switching on the propulsion unit when the player carries the propulsion unit:
	say "Throwing caution to the wind, you turn on the propulsion unit while it is still in your possession.
	
The last thing you feel is a searing blast against your midriff...";
	end the game in death.
	
Instead of switching on the propulsion unit when the propulsion unit is in a container:
	say "It turns out that firing a propulsion unit which is inside [a holder of the propulsion unit] is not a good idea, and in fact produces a deadly explosion...";
	end the game in death.
	
Instead of switching on the propulsion unit when the propulsion unit is not stuck to the egg:
	say "You turn on the propulsion unit, which -- lacking the steadying influence and guidance system of the egg pod -- flies around the room like a punctured balloon; crashes through the plate glass window; and is last seen buzzing along over the tops of cars down on Fifth Avenue.
	
It takes the boys from A. P. six minutes and twenty-seven seconds to turn up in your office and fire you.";
	end the game saying "This terminates your A. P. contract"

Instead of switching on the propulsion unit when the propulsion unit is stuck to the egg and the creature lies outside the egg:
	say "The propulsion unit lifts the egg off and guides it safely through the window, which automatically opens to allow for the departure. It's a thing of beauty, really. 
	
You watch, entranced, as it floats off towards the Empire State Building[if the creature is visible]. 

So does the creature[end if].";
	end the game saying "You just threw away a very expensive piece of equipment"
	
Instead of switching on the propulsion unit when the creature is in the egg and the propulsion unit is stuck to the egg and the egg is open:
	say "You flip the switch, and the egg jets off. The creature, deciding that it does not want to go to space in an open egg pod, leaps lightly out and lands beside you, while the egg goes its own way...";
	end the game saying "You just threw away a very expensive piece of equipment"
	
Instead of switching on the propulsion unit when the creature is in the egg and the propulsion unit is stuck to the egg and the blank label is stuck to the egg:
	say "You flip the switch, and the egg jets off -- just slowly enough for you to get a long last look at the blank label on the side. The creature presses its nose pathetically against the inner surface of the egg as you send it off to an eternal oblivion in the nowhere of space... ";
	deliver the mislabeling ending.
	
To deliver the mislabeling ending:
	say "[paragraph break]'Sir,' says Esther from the doorway. 'Your ride to A. P. is here.' 

You look back from the window. 'Could you stall them?' It's not as though you have a plan, but --

'No, she could not,' says a deeper voice, as the A. P. man comes to stand behind her. 'What went wrong?'

You admit that the label never got filled out. The man frowns -- no, perhaps glares would be more accurate. Then he walks to Esther's telephone, dials a ten-digit number, and says something curt and not in English.

'What did you just do?' Esther asks him. 'Can you fix it?'

'In a manner of speaking,' he says. 'But it is no longer your business. I'm afraid we cannot continue to work with someone who has made an error of this level.'";
	end the game saying "This terminates your A. P. contract"
	
Instead of switching on the propulsion unit when the creature is in the egg and the propulsion unit is stuck to the egg and the blank label is not stuck to the egg and the filled label is not stuck to the egg:
	say "You flip the switch, and the egg jets off -- just slowly enough for you to remember that you never did label the thing. 
	
The creature presses its nose pathetically against the inner surface of the egg as you send it off to an eternal oblivion in the nowhere of space... ";
	deliver the mislabeling ending.
	
Instead of switching on the propulsion unit when the creature is in the egg and the propulsion unit is stuck to the egg and the life-support unit is switched off:
	say "You flip the switch, and the egg jets off -- just slowly enough for you to notice that the life support unit is turned off. ";
	deliver suffocation ending.
	
Instead of switching on the propulsion unit when the creature is in the egg and the propulsion unit is stuck to the egg and the life-support unit is not stuck to the egg:
	say "You flip the switch, and the egg jets off -- just slowly enough for you to notice that the life support unit is not part of the package. ";
	deliver suffocation ending.
	
To deliver suffocation ending:
	say "The creature scrabbles helplessly at the interior of the egg, but it's too late: its suffocation is inevitable.

You consider trying to conceal this from the boys at Alien Protocol, but when you walk in for your debriefing, it's obvious that they already know. Everyone is sitting back from the table, and the one with the dark bushy eyebrows has his fingers pressed together.

'We did mention that this would be a job requiring attention to detail,' he begins, in an apologetic tone. 'Your record with Chase Manhattan was exemplary, so perhaps you are best suited to a career as teller?'

Not likely. 'What happens now?' you ask. 'Will there be-- that is, do you think there will be reprisals?'

Their expressions are vague. 'We intercepted the outgoing pod,' says the blond one after a moment. 'The body will never reach the creature's home; it will be presumed lost.'";
	end the game saying "Your contract with A. P. is dissolved"
	
Instead of switching on the propulsion unit when the egg is ready and the moon of the filled label is not the moon of the creature:
	say "You flip the switch, and the egg jets off to [moon of the filled label]. 
	
This turns out to have been a mistake, though one of those mistakes that, like cashing a counterfeit check, doesn't come to your attention for some weeks. One fine day a man from A. P. comes in to see you, and informs you that you misdirected your Visitor, and that it wound up on [moon of the filled label] without belonging there at all. 

'And did it get back home?' you ask. 'In the long run, I mean.' 

'As of this minute, your contract is terminated and your clearance revoked,' he replies. 'So it would be inappropriate for me to give you any further information about the long-term results of your actions.'

From his expression, you can see he realizes you will find this extremely exasperating. He lifts his hat to you, kisses Esther's hand, and leaves.

'Well, that's that,' says Esther, wiping her hand on a handkerchief. 'Now I can finally pursue my life-long ambition and marry a doctor.'";
	end the game saying "This terminates your A. P. contract"
	
Definition: the egg is ready:
	if the egg is open, no;
	if the propulsion unit is not stuck to the egg, no;
	if the filled label is not stuck to the egg, no;
	if life-support is not stuck to the egg, no;
	if life-support is switched off, no;
	if the creature lies within the egg, yes;
	no.

After switching on the propulsion unit:
	let destiny be the moon of the creature;
	choose row with moon column of destiny in the Table of Happy Endings;
	say "You touch the propulsion and the pod blasts off, slipping through the window as though it were not made of glass at all; as though it were a curtain of water. And the creature is on its way back to [destiny]. ";
	say "[ending entry]";
	change the left hand status line to "Tuesday, July 20, 1954";
	change the time of day to 5:52 PM;
	move Esther to Reception;
	make scene break.
		
Report closing a container which contains the creature:
	say "You shut [the noun], with [the creature] inside." instead.
 
Instead of inserting the pod into the file drawer:
	say "[The pod] is far too large to fit into the file drawer."

Before inserting something which is worn by the player into something:
	try taking off the noun;
	if the noun is worn, stop the action.

Before inserting something into a closed container:
	try opening the second noun;
	if the second noun is closed, stop the action.
 

## Chapter 9 - Other Ways of Interacting with the Creature

Understand "point at [something]" as indicating. Indicating is an action applying to one thing. Instead of indicating something in the presence of a stupid creature: say "[The person asked] looks intently at your forefinger."  

Instead of indicating a container in the presence of a smart friendly creature: say "You gesture imperiously. "; try the person asked trying entering the noun. Instead of indicating a delicious thing in the presence of a smart friendly creature: say "You gesture imperiously. "; try the person asked trying eating the noun. Instead of indicating a wearable thing in the presence of a smart friendly creature: say "You gesture imperiously. "; try the person asked trying wearing the noun. Instead of indicating a closed openable thing in the presence of a smart friendly creature: say "You gesture imperiously. "; try the person asked trying opening the noun. 

Instead of indicating something in the presence of a smart creature: say "[The person asked] looks intently at [the noun]."

Carry out indicating something: say "You point your finger at [the noun]."


Understand "hi" or "hello" or "hey" as "[hi]".

Understand "hi" or "hello" or "hey" as waving hands. Understand "greet [someone]" or "wave at [someone]" as waving at.

Waving at is an action applying to one thing. Carry out waving at: try waving hands.

Before asking the creature to try waving hands: try waving hands instead. Instead of answering the creature that "[hi]": try waving hands.

Instead of waving hands in the presence of a playful creature: say "You wave to the creature, and it waves back delightedly."

Instead of waving hands in the presence of a secretive creature: say "You wave to the creature. It looks at you for a moment with much the same expression you see when you try to ask out Esther's girlfriends. 

Then it waves back."

Instead of waving hands in the presence of a hostile creature: say "You wave. The creature ignores you pointedly."

Instead of waving hands in the presence of a slow creature:
	say "You wave. The creature blinks mildly."
	
Instead of waving hands in the presence of a smart creature:
	say "You wave. The creature waves back at you."
	
Instead of waving hands in the presence of a creature when the carrying capacity of the creature is 6:
	say "The creature waves back with all six hands. The different arms have different personalities: the upper left hand gives you a stately monarchical salute, while the middle right hand flaps as frantically as a child's."
	

Instead of singing in the presence of the creature when the creature is hostile:
	if the creature cannot touch the player, continue the action;
	otherwise say "[The creature] puts its fingers in its ears and glares at you."
	
Instead of singing in the presence of the creature when the creature is playful:
	if the creature cannot touch the player, continue the action;
	otherwise say "[The creature] sings along, somewhat to your surprise."
	
Instead of singing:
	say "You perform a rousing chorus of 'Take Me Out To The Ballgame'."

Instead of singing for the first time:
	say "You perform a rousing chorus of 'Take Me Out To The Ballgame'.";
	change Esther's utterance to "'I can hear you,' yells Esther from the other room. 'And Perry Como you aren't.' Subtle, that woman."

Instead of listening in the presence of a slow creature:
	say "[if creature is weak]It is still and sluggish[otherwise]Its breath is slow and deep[end if]."
	
Instead of listening in the presence of a lightning creature:
	say "[if creature is weak]Its claws are constantly clicking and clacking[otherwise]Its breath is rapid, its movements impatient[end if]."
	
Instead of listening in the presence of a fast creature:
	say "[if creature is weak]It is constantly moving[otherwise]It seems to breathe at roughly the same pace as you[end if]."
	
Instead of listening in the presence of a moderate creature:
	say "[if creature is weak]It is pretty quiet[otherwise]It breathes a little more slowly than you do; moves a little sluggishly[end if]."
	
Instead of listening in the presence of a creature when the moon of the creature is Titan:
	say "[The creature] draws deep, gasping breaths, as though struggling to get enough out of this feeble atmosphere."


Instead of smelling the location in the presence of an inverse creature:
	try smelling the creature.
	
Instead of jumping in the presence of a fidgety creature:
	say "You jump[if the creature is in the location], and the creature leaps away swiftly, dodging into a far corner of the room[otherwise], making the creature flinch[end if]."

Instead of jumping in the presence of a moderate creature:
	say "The creature watches you with interest."
	
Instead of jumping in the presence of a slow creature:
	say "The creature draws its head back like a tortoise trying to retreat into a shell. Otherwise it does not move."

Instead of pushing, turning, or pulling the creature: try touching the creature.

Instead of pushing the creature when the creature is secretive and the egg pod is in location and egg pod is open:
	move the creature to the egg pod;
	now the creature is passive;
	say "You give the creature a hard shove in the middle of its back and it lands with a thump inside the pod. It stares back over its shoulder at you, its eyes wide with surprise." instead.


Report eating something in the presence of the hungry creature:
	say "You eat [the noun], while [the creature] watches you with grieved reproach." instead.

Report dropping the creature:
	say "You gently set the creature down. It looks up at you soulfully." instead.

Instead of telling the creature about something: try asking the creature about it.

Instead of asking the creature about something:
	say "The creature watches your mouth intently, tilting its head this way and that."
	
Instead of asking a hostile smart creature about something:
	say "As soon as you begin to speak, the creature plugs its ears and howls rudely."
	
Instead of asking a friendly smart creature about something:
	say "The creature listens with frowning concentration, then says, 'Glorp?'

As responses go, it makes as much sense as the ones you usually get from the boys at Alien Protocol..."

Instead of asking a friendly smart creature about something:
	say "The creature listens with frowning concentration, then says, 'Glurble,' quite firmly."
	
Instead of asking a creature to try doing something: say "The creature either fails to understand your instruction or feels no need to obey."


Procedural rule: ignore the can't take other people rule.

Instead of taking a hostile creature:
	if the creature is poisoned, continue the action;
	if the creature is starving, continue the action;
	if the creature is slothful:
		say "The creature registers displeasure at having you get near it[if a random chance of 1 in 2 succeeds], and you jump back to avoid being bitten. But another approach might work[otherwise], but is not strong enough to prevent you[end if].";
		stop the action;
	try the creature trying growling instead.
	
Instead of taking the creature when the speed of the creature is greater than moderate:
	if the creature is starving, continue the action;
	if the creature is poisoned, continue the action;
	say "With [speed of the creature] reflexes, [the creature] scampers out of your grasp."
	
Instead of taking a curious moderate creature:
	if the creature is starving, continue the action;
	if the creature is poisoned, continue the action;
	say "[The creature] momentarily allows you to pick it up but then scrambles out of your arms again."
	
Report taking a starving creature:
	say "[The creature] is too weak to resist, and lies limply in your arms." instead.

Report taking the creature:
	say "[The creature] wraps its arms around your neck and nuzzles against your shoulder." instead.
	
Report inserting a slothful creature into the pod:
	say "[The creature] relaxes into the curve of the pod, seeming more comfortable there than it had been." instead.



Understand "pet [something]" or "tickle [something]" or "cuddle [something]" as touching.

Instead of touching the creature when the creature is poisoned:
	say "[The creature] feels unnaturally warm to the touch."

Instead of touching a hostile animal:
	try the creature trying growling;
	now the creature is passive.
	
Instead of touching a secretive animal:
	now the creature is passive;
	say "It holds very still and allows itself to be petted, watching you carefully."

Instead of touching a friendly animal:
	if the noun is scaly, say "[The noun] rubs itself against your hand. Despite the scales, it is surprisingly pleasant to touch, not at all slimy or rough.";
	otherwise say "[The noun] rubs itself against your hand.";
	now the creature is passive.
	
Instead of touching a curious animal:
	now the creature is passive;
	if the noun is timid, say "[The noun] dodges, terrified." instead;
	if the noun is playful, say "[The noun] watches you with a little wariness, then giggles when you touch it. Perhaps it is ticklish." instead;
	say "[The noun] moves away [if the creature is slow](though slowly)[otherwise]skittishly at first[end if], then lets you touch it."

Instead of kissing the creature: 
	say "Fortunately your task does not require you to investigate its mating rituals."
	
Understand "rip [something]" or "tear [something]" as attacking.

Understand "kick [something]" as attacking.

Instead of attacking a container which contains a timid creature when the noun is not papery:
	say "You aim one or two good blows at [the noun]. From inside comes a terrified whimper."

Instead of attacking a poisoned creature:
	say "Taking advantage of the critter's inability to fight back, you bludgeon it repeatedly until its moans and twitches stop.";
	end the game saying "You have successfully slaughtered your little Visitor, which displeases A. P. extremely"

Instead of attacking a negligible creature:
	say "Your patience gone, you attack [the creature]. It is much too weak to put up a significant fight, and you soon render it unconscious.";
	end the game saying "The boys don't like it when you harm the creatures"
	
Instead of attacking a fidgety person which is not vast:
	say "You try to hit [the creature], but it gets away from you too fast, scurrying into the far corner of the room and dodging over furniture."

Instead of attacking a vast creature:
	say "You hit [the creature]. It looks at you, puzzled. Then it hits back approximately five times as hard. The world fades out...";
	end the game saying "You wake up sometime later in the hospital..."
	
Instead of attacking a meaty creature:
	say "You hit [the creature]. 
	
It grins with many many teeth, and then takes a tremendous bite out of your left leg. The blood loss and shock put you out almost immediately.";
	end the game saying "You wake up sometime later in the hospital..."

Instead of attacking a person:
	say "You attack [the noun], which fights back for a few minutes before becoming too bruised. It then sets up a hideous squealing and whining, bringing Esther in to discover what you're doing...";
	end the game saying "This will lose you your job for sure".

Instead of smelling something stinky:
	say "Phew!"

Instead of smelling the creature:
	if the creature is inverse, 
		say "There is a faint stink to its exhalations and its exterior, probably the sign of an atmosphere high in methane or sulfur. If you had a real lab at your disposal...";
	otherwise say "The smell is nothing you can identify."
	
Instead of tasting or eating the creature:
	say "You aren't allowed to do anything that might kill the creature."
	 
	
Instead of waking the creature:
	say "Demonstrably it is already awake."
	
Instead of waiting in the presence of the creature when the creature is in the pod:
	say "You wait, though there is really no need now it is in the pod."

Instead of waiting in the presence of the creature:
	say "You wait to see what the creature will do next."
	
Instead of waiting in the presence of the creature when the creature is starving:
	say "You wait[if creature is hostile], unmoved[otherwise], cruelly[end if]."
	
Report examining the creature:
	if the creature is playful, say "It waves at you perkily.";
	otherwise say "It blinks back at you[if the creature is smart] intelligently[end if]." instead.
	
After examining a poisoned creature: say "It looks distinctly ill."
	
After examining a hostile creature:
	say "It glares back at you[if the creature is smart] with calculated malice[end if]."
	
After examining a starving creature:
	say "It meets your eyes briefly, then closes its own in pain[if the creature is friendly]. Must be starving, poor thing[end if]."
	
After examining a blinded creature:
	say "Its eyes are tightly closed."
	 
	
Instead of throwing something at a fidgety creature when the creature wants the noun:
	silently try giving the noun to the creature;
	if the creature is carrying the noun, 
		say "You toss [the noun] to [the creature], which makes a deft catch."
		
Instead of throwing something at a creature when the creature wants the noun:
	silently try giving the noun to the creature;
	if the creature is carrying the noun,
		say "You toss [the noun] to the creature, which watches the fall -- unable to make the catch -- but manages a retrieval afterward."

Instead of throwing something at a fidgety creature:
	silently try dropping the noun;
	if the player is not carrying the noun,
		say "You throw [the noun], but [the creature] dodges quickly. [if creature is lightning]Lightning-fast[otherwise]Fast[end if] reflexes there."
	
Instead of throwing something heavy at a slow creature:
	silently try dropping the noun;
	say "You fling [the noun] at [the creature], and connect successfully with its head, to disastrous effect.
	
[if creature is red]Funny, it bleeds red[otherwise]Now you know what [color of creature] blood looks like[end if].";
	end the game saying "You aren't supposed to harm them"

Instead of throwing something heavy at a moderate creature:
	silently try dropping the noun;
	if the player is not carrying the noun, say "[The creature] just manages to dodge [the noun]."

Instead of throwing something at a slow creature:
	silently try dropping the noun;
	if the player is not carrying the noun, say "[The noun] bounces harmlessly off [the creature]. It does not even flinch. Right. Very slow reflexes, then."

Instead of throwing something at a moderate creature:
	silently try dropping the noun;
	if the player is not carrying the noun, say "[The noun] bounces harmlessly off [the creature], which flinches. Medium reflexes, then."

## Chapter 10 - The Plot

### Section 1 - Accounting for Time

To decide whether we are dead: 
	(- (deadflag > 0) -);

Waiting more is an action applying to one number.

Understand "wait [number] minutes/turns" or "wait for [number] minutes/turns" or "wait [number]" as waiting more.

Carry out waiting more:
	let duration be the number understood - 1;
	repeat with X running from 1 to duration:
		if the creature is dying or we are dead, do nothing;
		otherwise follow the turn sequence rules.
	
Report waiting more:
	if we are dead, do nothing;
	otherwise say "It is now [time of day + 1 minute]." 

Check waiting more:
	if the number understood > 59, say "You really haven't got that kind of patience." instead.

Hanging around until is an action applying to one time.

Check hanging around until:
	if the time of day is the time understood, say "It is [time understood] now!" instead;
	if the time of day is after the time understood, say "It is too late for that now." instead.

Carry out hanging around until:
	while the time of day is before time understood:
		if the creature is dying or we are dead, do nothing;
		otherwise follow the turn sequence rules.

Report hanging around until:
	if we are dead, do nothing;
	otherwise say "You yawn until [time understood]."

Understand "wait until [time]" as hanging around until.
 

### Section 2 - Major Plot Events 
	
To leave space:
	say paragraph break; say paragraph break;
	say paragraph break; say paragraph break.
	
The current level is medium.

To simplify the game: 
	choose a random row in the Table of Alien Characteristics; 
	while the difficulty entry is not the current level, choose a random row in the Table of Alien Characteristics; 
	change moon of the creature to moon entry;
	change mood of the creature to attitude entry;
	change the odor sensitivity of the creature to nostrils entry;
	change metabolism of the creature to feed time entry;
	change gravity of the creature to the mass entry;
	change speed of the creature to the dexterity entry;
	change carrying capacity of the creature to the arms entry;
	if carrying capacity of the creature is 0, change carrying capacity of the creature to 2;
	follow the setup rules;
	change food of the creature to taste entry;
	if food of creature is meaty, change tongue of creature to "sharp carnivore teeth";
	if food of creature is wood-pulpy, change tongue of creature to "a long raspy tongue";
	if food of creature is earthly, change tongue of creature to "a round pink tongue";
	if food of creature is textile, change tongue of creature to "a forked tongue";
	change intelligence of the creature to brain entry; 
	if the creature lies nearer than Luna, change the color of the creature to a random color between red and yellow;
	if the creature lies beyond Venus and creature lies nearer than Mars, change the color of the creature to a random color between yellow and cyan;
	if the creature lies beyond Luna and creature lies nearer than Saturn, change the color of the creature to a random color between green and purple;
	if the creature lies beyond  Jupiter, change the color of the creature to a random color between tan and grey;
	if the moon of the creature is Triton, change the color of the creature to grey.
	

The sampler platter is a portable supporter.  The description of the sampler platter is "One of Esther's brainwaves, based on a few months of this work. If you let her, she'd probably add toothpicks and pimientoed olives."  

After examining the platter when something is on the platter: say "Arrayed on the platter are [a list of things on the platter]."

The scrap of wrapping paper is a papery thing on the platter. The knitted scarf is a wearable thing on the platter. The meatball is an edible thing on the platter. The meatball is stinky and fleshy. The carrot is an edible thing on the platter. The description of the carrot is "It looks almost distressingly healthy: if you were an alien, you wouldn't want any part of it." The description of the meatball is "Fetched from the shop down the road, which is more than Esther will do for you these days. It is spiced." The description of the scarf is "Unattractively tartan[if we have not examined the scarf]. Esther has a variety of hideous accessories, gifts from her mother. 'She doesn't want me dating,' was Esther's response, when you dared comment[end if]." The description of the wrapping paper is "Festive, with little snowflakes on the outside."


When play begins: change the right hand status line to "[time of day]";
	remove the sack from play; remove the salami from play; 
	change the time of day to 10:13 AM;
	move slide to the player; move ballpoint to player;
	move sampler platter to the desk;
	move the glue to the drawer; move the Guide to the drawer; 
	move the blank label to the Kit;
	move the paperwork to the drawer; move the lamp to the desk;
	move the creature to the Office;
	if the player wears the jacket, silently try taking off the jacket; if the player wears the fedora, silently try taking off the fedora; remove the jacket from play; remove the fedora from play; 
	change the left hand status line to "Tuesday, July 13, 1954";
	simplify the game.
	 
When play begins:
	leave space;
	center "[story title]";
	center "[story headline]";
	center "by [story author]";
	leave space;
	pause the game;
	say "Weeks go by; the weather turns hot. Communist spies are investigated, Alan Turing commits suicide, Britain ends food rationing. The Yankees win nine games in a row.

You, you spend many hours within the grey walls of the Office of Alien Protocol. Your blood is tested for alien pathogens. You take memory tests and guess what cards are hidden behind a screen. You have a physical roughly once a week. You are, as you tell Esther, probably the healthiest specimen in New York City, to which she says, 'Hunh.'"; 
	pause the game;
	say "[paragraph break][bold type]Tuesday, July 13, 1954[roman type] [paragraph break]'Sir--' Esther pokes her head in. 'I can bring the radio in later, if you like.' In case you're not done by 1:30, she means, when the All-Star game begins. 

'Whitey Ford is starting pitcher,' you point out.

'I understand the significance of this moment,' she says, withdrawing. You're making progress."


After going to Office:
	say "'Oh, and the boys said it might be getting hungry, they didn't know,' Esther calls after you. 'They fed it twenty minutes ago.'
	
'Did they say [italic type]what[roman type] they fed it?' you holler back. But she's already out of range, and the only answer you get is the clacking of the typewriter.";
	continue the action.

Table of Happy Endings
moon	ending
Luna-X	"You've seen pictures of that companion moon, with its scowling face, its fury.

Esther cuts off your thinking. 'You've got a meeting in five,' she reminds you.

'I know.' You watch the pod take a turn around the top of the Empire State Building and vanish from view. 'Esther, why do you think they come here, if they don't like us?'

'I come in to work, don't I?' she says.

'Thanks, now I feel better.'

'Any time, boss.' Esther taps the doorframe a couple of times and goes out, leaving you to gather your thoughts before your debriefing."
Deimos	"'After a while they all start to seem alike,' says Esther from the doorway.

'The critters? It's true that there isn't as much physiological diversity as you might reasonably expect, considering the differences between their home-worlds. I've been working on a theory that many of them actually come from the same genetic stock some way back and only diverged at a relatively recent point in their evolution... of course, I can't get the A. P. boys to confirm this for me, but it would explain a great deal. I also don't have an explanation for how they traveled between worlds when many of them still seem to be at an entirely pre-technological stage of evolution, but perhaps--'

'Men. I meant men,' Esther cuts in.

'Ah. Your date last night?'

'Disaster,' she says, peeling herself away from the doorframe. 'And I was really hoping with this one.' She gives you a little hurt smile. Poor girl."
Phobos	"It watches out the window with resentment, and you have the distinct impression that you will see it again.

You try to explain your concerns to the boys at the Office of Alien Protocol, but they refuse to listen. 'It's under control,' they say, and 'we know more about the political aspects of this than we can share.'

Pushed very hard, one of them finally blurts, 'We have reason to believe it just wanted to take you home for dissection.'

There's a pointed silence while everyone stares at the blurt-er. They all look alike, A. P. types, but this one has a slightly rounder chin and was obviously the kid that got picked on in class.

'For science,' he adds weakly."
Jupiter	"Esther comes in to watch it go. 'That one was bad,' she says, nodding towards [the list of visible damaged things]. 'You should make the boys bring you a tranquilizer gun.'

'I like doing it the old fashioned way,' you say. 'Like Daniel Boone wrassling bears.'

Esther raises her eyebrows. 'I think you'd better be grateful that they land one at a time and not by transport-ship. It could be Normandy here.'

Memory and imagination transpose, and you imagine short strong furry animals in the streets of Rome; swarming monkey-like over the Colosseum, up the sides of the villas, smashing the statuary. Visigoths.

She touches your sleeve. 'Sorry,' she says. 'I didn't mean to remind you of anything.'

You shake your head. 'I wasn't in Normandy; I was in Rome, with General Clark. They threw a parade for us.' It was the fighting afterward that was bad, not Rome but the smaller towns. Not a story that Esther needs to hear."
Uranus	"'You may not know this about me,' says Esther from the doorway. 'But I'm not very good at men.'

You raise your eyebrows, seeking a safe response. 'If you'd like me to offer an opinion on your latest beau,' you begin--

'No, thanks!' she says, holding up a hand. 'It's the A. P. courier. I've been trying to keep him distracted, but I think he's gotten impatient. He wants you to come along right away.'

'Don't blame yourself.' You gather what you need for the trip. 'Those aren't men except in a very limited technical sense. I'm not sure I've ever even seen them eat anything.'

'I don't often see you eat anything,' she says, rallying. Yes, that's the Esther you know.

'That's because you refuse to bring me lunches from the corner,' you reply. 'But if you want to see me eat dinner, and possibly eat some yourself--'

The A. P. man, refusing to be further stalled, walks in. 'You,' he says. 'Car. Right now. Serious concern about this case. Case presenting similar symptoms turned out to be highly-- you've already sent it?'

'It's headed for Uranus,' you answer brightly.

'Ah.' All the tension goes out. 'Well, the boys will want to hear.'"
 
 
To decide whether (inner - a thing) lies within (external object - a thing):
	if external object encloses inner, yes;
	no.
	
To decide whether (inner - a thing) lies outside (external object - a thing):
	if inner lies within external object, no;
	yes.

### Section 3 - Final scene



Sending is a scene. Sending begins when play begins. Sending ends when Esther is in Reception.


A person has a table-name called conversation.  

Instead of asking someone about something:
	let the source be the conversation of the noun;
	if topic understood is a topic listed in source:
		if there is a turn stamp entry:
			say "[The noun] has already told you [summary entry].";
		otherwise:
			change turn stamp entry to the turn count;
			say "[reply entry][paragraph break]";
	otherwise:
		say "[The noun] stares at you blankly."

Definition: a person is other if it is not the player.

Understand "ask [someone] about [something]" as asking it about specified object. Understand "tell [someone] about [something]" as asking it about specified object. Asking it about specified object is an action applying to two visible things. Carry out asking it about specified object: say "[The noun] just shrugs." Asking something about specified object something is useless action. 

Instead of asking Esther about specified object something: say "'It's just [a second noun],' Esther says. 'That's not the point.'" 

Understand "recap" or "recall" or "review" as recalling conversations. 

Recalling conversations is an action applying to nothing. 
 
Carry out recalling conversations:
	repeat with speaker running through other people:
		let source be the conversation of the speaker;
		sort source in turn stamp order;
		say "[The speaker] has so far told you: [line break]";
		let index be 0;
		repeat through source:
			if there is a turn stamp entry:
				let index be 1;
				say "  [summary entry][line break]";
		if index is 0, say "  absolutely nothing[line break]";
		say line break.
	
The conversation of Esther is Table of Esther Conversation.

After reading a command:
	while the player's command includes "the", cut the matched text;
	while the player's command includes "a", cut the matched text. [* Again, some trimming for ease of topic-handling.]

Table of Esther Conversation
topic	reply	summary	turn stamp
"point/danger/purpose/direction/meaning" or "her point"	"'Are you working around to a point of some kind?' you ask.

'I don't trust the A. P. boys, that's all,' she says. 'Or rather, I don't like how they don't trust me.'"	"that you would be better off as a detective"	a number
"boys/men/agents" or "a p" or "alien protocol"	"'Let me put it this way,' you say, after thinking a moment. 'Given that they do what they do, could they be any different than they are?'

'They could not look at me like some kind of slug,' she says. Interesting. This sounds like a specific grievance."	"that they are creepy"	--
"grievance/specific/story/slug" or "some kind of slug"	"'Has someone from the A. P. Office done something to you?'

She just frowns more deeply and does not answer. This is evidently not the conversational direction she wanted to take."
 
Instead of waiting during Final Chat: say "You wait. 

'The aliens are dangerous and the Office is worse,' she says finally."

Final Chat is a scene. Final Chat begins when Sending ends. When Final Chat begins: 
	move the player to Reception, without printing a room description;
	say "'I think you should think about quitting.' It's leaving time, a week later; but Esther's hovered around being about to say something just about every afternoon, and you might as well get it over with now.

You turn back. 'I'm not sure the Office of Alien Protocol accepts resignations, Esther,' you say. 'Besides, I thought you hated your previous job.'

'I did,' she says, vigorously marking boxes on a requisition form so as not to look at you. It appears that she is ordering a life-time supply of tinned meats. When she finishes, she folds this and puts it away; then she says: 'All the same, these things are dangerous.' 

Presumably she does have a point here, but it's taking her quite a while to get to."

Final Chat ends when the time since Final Chat began is 2 minutes. When Final Chat ends: say "Esther hesitates an even longer time; then she says, 'I was married. Before.'

You blink. 'What brings on this confidence?'

She leans forward. 'The point is, I have lived with people -- one person -- who -- I'm used to people that lie. And these people are lying to us.'

She is so wide-eyed and earnest. You want to tell her not to be so naive, that everyone lies, that government secret agents probably lie in their sleep and on the toilet; but at the same time she is making you a little nervous. All the same, you could tell her a thing or two with living in the presence of lies. 

A man from A. P. appears in the doorway: not Max, but one of his lower-browed subordinates. 'Bad news, kids,' he says. 'We have a situation out in the City, and you--' He points at you with gun-fingers, as though he were five years old. '--are going to have to do the dog-catcher scenario we talked about.'

Esther raises her eyebrows and mouths, 'Dog-catcher scenario?'

Yes, this should be good for your dignity..."; end the game saying "To be continued".

A persuasion rule for asking Esther to try doing something:
	say "'Funny,' says Esther, brushing you off."

Reception is a room. "This is Esther's area, with its table and typewriter, its lamps and relatively scar-less walls." The small round table, the typewriter, the telephone, and the lamps are scenery. The small round table is in Reception. The typewriter and the telephone are on the table. The lamps are in Reception. Understand "light" or "lamp" as the lamps. The initial appearance of Esther is "Esther is hovering, obviously having something she wants to tell you."

Part 1 - Easter Egg Events

[Basically an opportunity for the game to comment on particularly amusing or awkward situations that the player gets into. A variety are provided, but we want to make sure that only a few occur per game -- otherwise they might get distracting, and besides we want to keep this stuff relatively fresh. Therefore a maximum of two encounters are allowed per playthrough, with the exception that Glue Return can happen as often as necessary. (This is because the Glue Return scene is the only way the player can get back the glue when the critter has thrown it away; all other scenes are purely ornamental.)]

Plot depth is a number that varies. Plot depth is 0.

Disguise is a scene. Disguise ends when the time since Disguise began is 1 minute. Disguise begins when creature is well-dressed.

To decide whether creature is well-dressed:
	if plot depth is greater than 1, no; 
	if Disguise has happened, no;
	if Glue Return is happening, no;
	if the player is not wearing the undershorts and the player is not wearing the slacks, no;
	if the creature is wearing the fedora and the creature is wearing the jacket, yes;
	if the number of things worn by the creature is greater than the number of things worn by the player, yes;
	no.
	
When disguise begins:
	increase plot depth by 1;
	change Esther's utterance to "'Hey,' says Esther, just outside the door. 'The chief called down and--'
	
At this moment her head pokes through and she gets a good look at the creature in [a list of things worn by the creature]. She leans against the doorframe, grinning. 'If you think you can disguise the creature as you, you've got another think coming.'
	
'Do you mind? I'm trying to work here,' you reply, with all the dignity you can muster while reduced to [a list of uppermost things worn by the player].

She raises her eyebrows, but goes out again. 'Don't spend all day playing with your little friend,' she says over her shoulder. 'We've got work to do in this office, you know.' ".


Communism is a scene. Communism ends when the time since Communism began is 3 minutes. Communism begins when creature gets interrogated and Glue Return is not happening and plot depth is less than 2.

To decide whether creature gets interrogated:
	if Communism has happened, no;
	if the creature is blinded, yes;
	no.
	
When Communism ends:
	increase plot depth by 1;
	if the creature is blinded, change Esther's utterance to "Esther pokes her head in on the way past your office. 'Well, for gosh sakes!' she exclaims, looking at the creature huddled out of the glare of the lights. 'What's the matter, you suspect it of being a member of the Communist party?'
	
You give a little jerk of the head to tell Esther to scram. She shrugs and goes out."


Nudism is a scene. Nudism ends when the time since Nudism began is 2 minutes. Nudism begins when Nudism has not happened and the player is nude and Glue Return is not happening and plot depth is less than 2.

When Nudism ends:
	increase plot depth by 1;
	if the player is nude,
		change Esther's utterance to "Esther's voice suddenly becomes audible in the hall. 'Yes, no trouble, he's right through here--'
		
She becomes visible in the doorway[if the player is carrying something]. You move to cover yourself with [the random thing carried by the player][end if]. Her eyes go very wide, and she backs up.

'No, no, I'm so sorry,' she says, heading off whoever-it-is. 'You can't see him right now after all -- classified project -- if you even see what's going on in that room there could be repercussions...'

You make a note to buy Esther a box of chocolates."


Nutty is a scene. Nutty ends when the time since Nutty began is 3 minutes. Nutty begins when the  player is in the egg and Nutty has not happened and Glue Return is not happening and plot depth is less than 2.

When Nutty ends:
	increase plot depth by 1;
	if the player is in the egg:
		if the egg is openable:
			if the egg is open:
				change Esther's utterance to "'I tell you what,' says Esther from the doorway, catching your attention. 'I've soured on this job. It's made you barmy and it's going to do the same to me if I don't take immediate action. I need a vacation. Rest and relaxation.'";
			otherwise:
				change Esther's utterance to "Esther stands in the doorway reading a short speech which, mercifully, the egg prevents you from hearing. The gist of it from the circling motion she makes at her head would seem to be that she thinks you've gone crazy."; 
		otherwise:
			change Esther's utterance to "Esther stands in the doorway looking over you and the alien, and the pod you're in. Then a very very strange expression crosses her face, and she goes out."
  

Housekeeping is a scene. Housekeeping begins when the number of grabbable things in the location is greater than 6 and Housekeeping has not happened and Glue Return is not happening and plot depth is less than 2. Housekeeping ends when the time since Housekeeping began is 1 minute.

When Housekeeping begins:
	if the player is nude, make no decision;
	increase plot depth by 1;
	change Esther's utterance to "Esther passes the door at this inopportune moment and makes one or two tart comments on the state of your house-keeping. 

'And don't think I'm staying late to tidy it up, either,' she says, on her way back to the typewriter. 'I'm meeting a couple of the girls tonight.'

As though it's ever your fault when she -- well, there was last Thursday --

No point arguing anyway."

Glue Return is a recurring scene. Glue Return begins when the player is in the Office and Esther has the glue. Glue Return ends when the time since Glue Return began is greater than the patience of Esther.

Esther has a time called patience. The patience of Esther is 1 minute.

When Glue Return begins:
	increase plot depth by 1;
	say "'Esther--' you shout[if the patience of Esther is greater than 2 minutes]. There is a much longer pause than earlier[end if].

'[if the patience of Esther is 1 minute]Bring it back in just a minute[otherwise]Hang in there[end if],' she replies. The typewriter continues clacking. Right."

Every turn during Glue Return:
	if the time since Glue Return began is the patience of Esther, make no decision;
	repeat through Table of Esther Business:
		change Esther's utterance to reaction entry;
		blank out the whole row;
		make no decision;
	change Esther's utterance to "Esther's typing goes on."
	
Table of Esther Business
reaction
"Esther continues to type away without a care, apparently."
"Esther's typewriter continues with cheerful speed."
"Thank goodness, the typing seems to be coming to an end."
"A man's voice is audible in the outer office."
"Esther and the man hold a conversation of apparently compelling interest."
"Chuckling is audible from Esther's office. She can be charming enough when she wants to, which is never when you're around. But the door out there finally closes."
"Esther's typing does wind down. You hear a page being pulled out of the typewriter."
"There is a characteristic cranking noise from the outer office: a new page being inserted into the typewriter."
"Esther's typing begins again."
"The clacking from the outer office continues."
"Esther's footsteps are audible in the hall. Maybe this time she'll bring the glue back quickly!"
"The feet pass your door and continue down towards the restroom."
"There is a long and ominous silence from Esther."
"Distantly you hear the office plumbing come to life."
"Esther's footsteps are audible in the hall again..."
"Esther's telephone rings; you hear her answering."
"Esther sounds really delighted to hear from whoever-it-is on the phone."
"From the outer office, you overhear carefree laughter. Your soul turns blacker and blacker within you."
"The laughter breaks off to be replaced with hushed, urgent conversation."
"At this rate, you'll get the glue back sometime in 1990."
"'Esther!' you holler. [paragraph break]'Just a moment!' she shouts back. Then you hear her entering into an apologetic explanation over the phone about how cranky her boss is."

When Glue Return ends:
	now the player has the glue;
	increase plot depth by 1;
	increase the patience of Esther by 1 minute;
	if the player is nude, say "Esther pokes her head in -- eyes kept firmly shut -- and allows you to take the glue from her. 'Try gluing some clothes on, while you're at it,' she says, withdrawing. 'There are some things a girl shouldn't have to see. Ever.'";
	otherwise say "Esther comes by just long enough to toss the glue back to you[if Esther carries the meatball]. There is a greasy -- well, what you could only describe as a meatball-print on her skirt[end if][if the patience of Esther is 3 minutes]. 'Have you thought about hiring one of those ball-boys from the Yankees?' she asks tartly[end if]."


Disaster Zone is a scene. Disaster Zone begins when the player can see at least 3 damaged things and plot depth is less than 2 and Glue Return is not happening and Disaster Zone has not happened. Disaster Zone ends when the time since Disaster Zone began is 3 minutes.

When Disaster Zone ends:
	increase plot depth by 1;
	if the number of visible damaged things is less than 3, do nothing;
	otherwise change Esther's utterance to "Esther pokes her head in. 'Whee-oo,' she says, crossing her arms and staring hard at [the list of damaged things in the location]. 'I see I'm going to enjoy filling out the expense report this week.'

'Do you mind? I'm trying to work here,' you say, a little stiffly.

She puts up her hands and goes out again."


Imprisonment is a scene. Imprisonment begins when the creature is in an unopenable container and plot depth is less than 2 and Imprisonment has not happened and Glue Return is not happening. Imprisonment ends when the time since Imprisonment began is 2 minutes.

When imprisonment ends:
	increase plot depth by 1;
	change Esther's utterance to "'I'm not sure that's legit,' Esther says, stopping in long enough to eye the situation. 'I thought you weren't supposed to coerce any of them.'
	
'That's a wonderful idea,' you reply, wiping your forehead. 'Care to demonstrate your methods?'

She throws up her hands and goes back out. 'I'm just citing the regs -- I didn't write em myself.'"


Ark is a scene. Ark begins when Glue Return is not happening and plot depth is less than 2 and the pod is visible and the number of things in the pod is greater than 3 and Ark has not happened. Ark ends when the time since Ark began is 2 minutes.

When Ark ends:
	if the pod is visible and the number of things in the pod is greater than 3:
		increase plot depth by 1;
		change Esther's utterance to "'I thought those were supposed to be escape pods, not Noah's ark,' remarks Esther from the door. 'Look at all that stuff you got in there.' She casts a fascinated glance over [the list of things in the pod], all stuffed into the pod.
		
'You want to get in too?' you offer. 'We're supposed to have a male and female of each kind.'

'All that rain would spoil the baseball anyway,' she says, going. "
	

## Chapter 11 - Help System

When play begins:
	choose row 1 in the Table of Basic Help Options;
	change description entry to "[story description]".
	
Table of Basic Help Options (continued)
title	subtable	description	toggle
"Credits"	--	"Thanks to Sam Kabo Ashwell, Admiral Jota, Graham Nelson, Dan Shiovitz, A. Short, Lucian Smith, and J. Robinson Wheeler for beta-testing this episode. 

Inform 7 is the work of Graham Nelson, and [story title] was compiled on Andrew Hunter's compiler for Mac OS X."	--
"Contacting the Author"	--	"If you encounter bugs, please feel free to email me (Emily Short) at emshort@mindspring.com. Including a short transcript of the problem you ran into will make it easier for me to diagnose what went wrong, though of course it isn't required."	--
"Hints"	Table of Hints	--	--

 
Table of Hints
title	subtable	description	toggle
"What am I supposed to be doing here?"	--	"Try looking around at all the objects on your desk and in your file drawer. That should give you a decent idea of what you're expected to do with the creature.

Also, bear in mind that the solution to the puzzle will vary from game to game."	--
"How do I figure out where this thing came from?"	Table of Identification Hints	--	--
"How do I assemble the Probe?"	Table of Assembly Hints	--	hint toggle rule
"How do I make it get in the Probe?"	Table of Entry Hints	--	hint toggle rule

Table of Entry Hints
hint	used
"This will depend a great deal on what kind of creature you have on your hands."
"There are three basic categories of solution: getting the creature and manually placing it in the pod; luring the creature into the pod with something it wants; or making the creature's external environment so unpleasant that it will get into the pod for refuge."
"Some creatures can be picked up."
"Some creatures can be gotten into a state where they can't resist being picked up."
"Some creatures have short arms and will have to get into the pod to get anything you put there that they want." 

Table of Assembly Hints
hint	used
"Have you had a look at the instruction booklet?"
"You'll need some glue in order to glue the parts together."
"Then try GLUE (whatever) TO (whatever else). The booklet will tell you which parts may be attached to what."
"You will also need to fill out the blank label with information about the creature's presumed origin."

Table of Identification Hints
title	subtable	description	toggle
"Getting started"	Table of General Hints	--	hint toggle rule
"How do I measure the creature's strength?"	Table of Strength Hints	--	hint toggle rule
"How do I measure the creature's speed?"	Table of Reflex Hints	--	hint toggle rule
"How do I measure the creature's metabolism?"	Table of Eating Hints	--	hint toggle rule
"How do I gauge the creature's attitude?"	Table of Attitude Hints	--	hint toggle rule

Table of General Hints
hint	used
"You have some information about how to distinguish creatures from one another."
"Have you looked at the Guide?"
"It's in the file drawer."
"You'll probably want to read all the entries (with commands like LOOK UP PERIODICITY IN GUIDE)."
"If you've narrowed your search down to a few planets, you can also look those planets up individually in the paperwork. Sometimes the paperwork contains additional details that make the diagnosis easier."

Table of Reflex Hints
hint	used
"Sometimes its ordinary behavior is enough to give you some clue to this."
"But you can also do something to test its reflexes."
"Try throwing something at it."

Table of Attitude Hints
hint	used
"You may be able to tell how it feels about you by interacting with it a little."
"Does it seem surly? Indifferent?"
"Try petting the creature."

Table of Strength Hints
hint	used
"You could observe how it interacts with objects in the environment."
"In particular, some objects are heavier than others."
"If you put those where it can reach them when it is in a mood to play, what happens?"
"Try leaving the propulsion unit and the life support unit around. Note whether it is able to pick them up."
"Or whether it is able to move the chair when something is on it."

Table of Eating Hints
hint	used
"Every so often it will want to eat something."
"You could count how many minutes pass from the time when it eats until the time it starts asking for food again."
"Remember also that Esther tells you it has been twenty minutes without food when the game begins."
"Once you have measured the number of minutes that pass between feedings, you can narrow down the creature's origin system."
"Looking up moons in the paperwork may give you some idea of what the feeding times should be."
"If the feedings are more frequent than every 20 minutes, you know that it comes from somewhere nearer than the Jovian system; if they are less frequent, you know it comes from somewhere further out. Exactly 20 minutes would place it on Io, Europa, Ganymede, or Callisto."	 
 
## Chapter 12 - Testing - Not for Release

[When play begins: change command prompt to "[moon of the creature] > ".]


player_X1@[does]> s
s
iD(Agent,intend_todo( [ go_dir(player_X1,walk,south) ])).
iD(Agent,intend_todo( [])).
iD(Agent,timestamp(63,653)).
iD(Agent,attempts(player_X1,
                go_dir(player_X1,walk,south))).

% aXiom(go_dir(player_X1,walk,south)).

player was in kitchen but left walking south
iD(Agent,h(in,player_X1,garden)).
iD(Agent,h(in,player_X1,garden)).
player came walking north in garden
( Success: walk south )
player_X1@[does]> look
look
iD(Agent,intend_todo( [ look(player_X1) ])).
iD(Agent,intend_todo( [])).
iD(Agent,timestamp(64,656.6)).
iD(Agent,attempts(player_X1,
                look(player_X1))).

% aXiom(look(player_X1)).

player does examine see in garden

% aXiom(examine(player_X1, see, in,
%          garden)).

player does sub examine see in garden 3

% aXiom(sub__examine(player_X1,see,in,garden,3)).

(...verbose...: player sees the garden "this is the garden", is large , thus, has an interior and can have exits. )
iD(Agent,h(in,player_X1,garden)).
iD(Agent,h(in,fountain_X1,garden)).
iD(Agent,h(in,rock_X1,garden)).
iD(Agent,h(in,rock_X2,garden)).
iD(Agent,h(in,mushroom_X1,garden)).
iD(Agent,h(in,brklamp,garden)).
Player_X1 sees in garden: fountain , rock , rock , mushroom and brklamp.
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
Exits in garden are: north.


% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,fountain_X1,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(65,656.7)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,fountain_X1,2))).

% aXiom(sub__examine(player_X1,see,child,fountain_X1,2)).

(...verbose...: player sees the fountain can have exits , opened , thus, has an interior and has a surface. )
(...verbose...: nothing in fountain )
(...verbose...: nothing on fountain )

% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,rock_X1,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(66,656.7)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,rock_X1,2))).

% aXiom(sub__examine(player_X1,see,child,rock_X1,2)).



% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,rock_X2,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(67,656.8)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,'rock_X2',2))).

% aXiom(sub__examine(player_X1,see,child,'rock_X2',2)).



% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,mushroom_X1,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(68,656.8)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,mushroom_X1,2))).

% aXiom(sub__examine(player_X1,see,child,mushroom_X1,2)).



% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,brklamp,2)

iD(Agent,intend_todo( [])).
iD(Agent,timestamp(69,656.9)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,brklamp,2))).

% aXiom(sub__examine(player_X1,see,child,brklamp,2)).

(...verbose...: player sees the brklamp inherits shiny! and is glowing. )
player_X1@[does]>

