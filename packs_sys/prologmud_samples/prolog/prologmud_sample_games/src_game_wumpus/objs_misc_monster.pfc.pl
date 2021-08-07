/** <module> 
% This file contains the definitions for the objects in the world
% To create a new world, simply change the tObj definitions as
% described below (or in manual)
%
% *******Object definitions*******
%.
% Use the fmt label_type_props(label,typecode,[property1,property2,etc.]]).
% label is whats used to refer to the tObj in the running world
% typecode is whats used to refer to the tObj for initialization (see world.pl)
%
% **All objects require mudHeight() and mudWeight() properties
% **Even locations you want empty require mudHeight(0) to be defined
% **If mudPermanence(take,) is not defined for an tObj that can be picked
%   up (ie take()), it is assumed to be mudPermanence(take,stays) (ie never goes away).
%
% The properties are as follows:
%
% # mudHeight(N) where N ranges from 1-5 & 10.
% ---> 1  = very small, can be steped over.
% ---> 2  = low, blocks movement, but can be climbed onto.
% ---> 3+ = high, blocks sight and movement, can be climbed on from objects of
%           one less mudHeight. Eg. an agent on tObj mudHeight(2) can climb onto an tObj
%           with mudHeight(3).
% ---> 10 = Edge of world. Can't ever be seen over or climbed onto.
% **** Negative numbers can be used for N, this is good for pits, etc.
%
% # mudWeight(N) where N ranges from 1-4.
% ---> 1 = light, can be picked up.
% ---> 2 = hefty, can be pushed.
% ---> 3 = heavy, normally immobile, can be pushed by high strength agents
% ---> 4 = structure, cannot be moved. 
%
% # act_energy(Action,N) where N is any positive or negative integer (not 0)
% ---> N is the amount of mudEnergy tAgent receives or loses 
%      from doing that Action on the tObj defined.
%
% # act_score(Action,N) where N is any positive or negative integer (not 0)
% ---> N is the mudScore the tAgent recieves from doing Action on the
%      obect defined.
%
% # act_heal(Action,N) where N is any positive or negative integer
% --->N is the amount of mudHealth the tAgent heals (or takes) from
%        doing Action on tObj defined.
%
% # mudPermanence(take,N) where N ranges from 0-3.
% ---> vTakenDeletes = 0 = tObj deletes when taken. (like dirt)
% ---> 1/vTakenMoves = tObj is held when taken, can be eaten/destroyed (see eat.pl)   (like swords)
% ---> 2/vTakenStays = tObj stays where it is but something else (mudEnergy,mudScore)   (like electricy)
%          is usually accumulated.
% ---> 3/vTakenCopyFn(Something) = tObj stays where it is but copies:  type_props(tLeaf,mudPermanence(takeAct,vTakenCopyFn(tLeaf)))
%
% # attack(N) where N is some positive or negative interger.
% ---> when held by tAgent-1 and agnet-1 attacks tAgent-2. tAgent-2 takes N
%      amount of mudHealth.(negative mudHealth or healing is possible)
%
% # mudArmor(N) where N is some positive or negative integer.
% ---> N points of mudHealth are subtracted (added) to an incoming attack.
%
% # spawn_rate(N) 
% ---> N% chance per turn of this tObj being placed in the world.
%	
% July 7, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles

Object Flags and Types
Object types:

This is a list of all the types an tObj can be. This is not a bitvector,
but a single number from this list:

1    LIGHT          Item is a light source.
2    SCROLL         Item is a magical scroll.
3    WAND           Item is a magical wand. - point at 1 target.
4    STAFF          Item is a magical staff. - affect all in room.
5    WEAPON         Item is a weapon.
6    FIREWEAPON     Currently not implemented.  Do not use.
7    MISSILE        Currently not implemented.  Do not use.
8    TREASURE       Item is treasure other than gold coins (i.e. gems)
9    ARMOR          Item is armor.
10   POTION         Item is a magical potion.
11   WORN           Item is worn, but not armor.
12   OTHER          Miscellaneous tObj with no special properties.
13   TRASH          Trash -- junked by cleaners, not bought by shopkeepers.
14   TRAP           Currently not implemented.  Do not use.
15   CONTAINER      Item is a container.
16   NOTE           Item is a note (can be written on).
17   DRINKCON       Item is a drink container.
18   KEY            Item is a key.
19   FOOD           Item is tFood.
20   MONEY          Item is money (gold coins).
21   PEN            Item is a pen.
22   BOAT           Item is a boat; allows you to traverse WATER(NOSWIM)
23   FOUNTAIN       Item is a fountain.


Extra flags for objects:

1)   GLOW            Item is glowing (cosmetic).
2)   HUM             Item is humming (cosmetic).
3)   NORENT          Item cannot be rented.
4)   NODONATE        Item cannot be donated.
5)   NOINVIS         Item cannot be made invisible.
6)   INVISIBLE       Item is invisible.
7)   MAGIC           Item has a magical aura and cannot be enchanted.
8)   NODROP          Item is cursed and cannot be dropped.
9)   BLESS           Item is blessed (cosmetic).
10)  ANTI_GOOD       Item cannot be used by good-aligned characters.
11)  ANTI_EVIL       Item cannot be used by evil-aligned characters.
12)  ANTI_NEUTRAL    Item cannot be used by neutral align characters.
13)  ANTI_MAGIC_USER Item cannot be used by the Mage class.
14)  ANTI_CLERIC     Item cannot be used by the Cleric class.
15)  ANTI_THIEF      Item cannot be used by the Thief class.
16)  ANTI_WARRIOR    Item cannot be used by the Warrior class.
17)  NOSELL          Shopkeepers will not buy or sell the tItem.
18)  ANTI_VAMPIRE    Item cannot be used by Vampire class. 
19)  METAL           Item is Metal.
20)  ANTI_BARBARIAN  Item cannot be used by Barbarian class. 
21)  ANTI_PSIONICIST Item cannot be used by Psionicist class. 
22)  ANTI_PALADIN    Item cannot be used by Paladin class. 
23)  ANTI_RANGER     Item cannot be used by Ranger class. 
24)  ANTI_DRUID      Item cannot be used by Druid class. 
25)  ANTI_UNDEAD     Item cannot be used by Undead players. 
26)  ANTI_LIVING     Item cannot be used by 'living' players. 
27)  UNIQUE          DO NOT SET. - Item will save uniquely. 
28)  HIDDEN          Item is hidden from sight - Note this is only applicable to 
                     items which load in rooms, not inside containers, on mobs etc.
29)  UNSEEN          makes objects that can only be seen with holylight. Players 
                     can still interact with these objects, however. The idea is 
                     that room descriptions can now be created describing the 
                     tObj in a room and the player can interact with the room desc
                     instead of having a room desc be a mere backdrop. The upside 
                     to this, better than leaving a long desc blank, is that imms 
                     can see and will know when such an tObj is in the room 
                     with a player. 


Wear flags:

A bitvector with the following values:

 1) TAKE 	Item can be taken (picked up off the ground).
 2) FINGER      Item can be worn on the fingers.         
 3) NECK        Item can be worn around the neck.
 4) BODY        Item can be worn on the body.
 5) HEAD        Item can be worn on the head.
 6) LEGS        Item can be worn on the legs.
 7) FEET        Item can be worn on the feet.
 8) HANDS       Item can be worn on the hands.
 9) ARMS        Item can be worn on the arms.
10) SHIELD      Item can be used as a shield.
11) ABOUT       Item can be worn about the body.
12) WAIST       Item can be worn around the waist.
13) WRIST       Item can be worn around the wrist.
14) WIELD       Item can be wielded; e.g. weapons.
15) HOLD        Item can be held (the "hold" command).

Permanent effects:

A bitvector with the following meaning;

When wearsClothing an tObj with this flag, 
 1) BLIND       The wearer goes blind
 2) INVIS       The wearer becomes invisible
 3) DET-ALIGN   The wearer detects alignments
 4) DET-INVIS   The wearer sees invisible objects and players.
 5) DET-MAGIC   The wearer sees magic on objects
 6) SENSE-LIFE  The wearer sees hidden mobs/players
 7) WATWALK     The wearer can walk on water
 8) SANCT       The wearer is protected by sanctuary
 9) GROUP       DO NOT USE - reserved for internal use
10) CURSE       The wearer is influenced by a curse
11) INFRA       The wearer sees in the dark
12) POISON      The wearer is poisoned
13) PROT-EVIL   The wearer is protected from evil
14) PROT-GOOD   The wearer is protected from good
15) SLEEP       The wearer is magically put to sleep
16) !TRACK      The wearer cannot be tracked
17) UNUSED      DO NOT USE
18) UNUSED      DO NOT USE
19) SNEAK       The wearer moves silently
20) HIDE        The wearer is hidden - use with caution
21) BERSERK     The wearer goes berserk 
22) CHARM       DO NOT USE - reserved for internal use
23) FIRESHIELD  The wearer is protected by fireshield 
24) HASTE       The wearer is hasted 
25) FLY         The wearer can fly 


Object applies:

0    NONE               No effect - used to quit the menu.
1    STR                Apply to strength.                         (-5  -  +2)
2    DEX                Apply to dexterity.                        (-5  -  +2)
3    INT                Apply to intelligence.                     (-5  -  +2)
4    WIS                Apply to wisdom.                           (-5  -  +2)
5    CON                Apply to constitution.                     (-5  -  +2)
6    CHA                Apply to charisma.                         (-5  -  +2)
7    CLASS              DO NOT USE!
8    LEVEL              DO NOT USE!
9    AGE                Apply to character's MUD age, in MUD-years.(-10 - +10)
10   CHAR_WEIGHT        Apply to mudWeight.                            
11   CHAR_HEIGHT        Apply to mudHeight.
12   MANA               Apply to MAX mana points.                  (-50 - +30)
13   HIT                Apply to MAX hit points.                   (-50 - +30)
14   MOVE               Apply to MAX movement points.              (-50 - +30)
15   GOLD               DO NOT USE!
16   EXP                DO NOT USE!
17   AC                 Apply to armor class (AC).                 (-30 - +30)
18   HITROLL            Apply to hitroll.                          (-5  -  +5)
19   DAMROLL            Apply to mudHealth roll bonus.                (-5  -  +5)
20   SAVING_PARA        Apply to save throw: paralyze              (-2  -  +2)
21   SAVING_ROD         Apply to save throw: rods                  (-2  -  +2)
22   SAVING_PETRI       Apply to save throw: petrif                (-2  -  +2)
23   SAVING_BREATH      Apply to save throw: breath                (-2  -  +2)
24   SAVING_SPELL       Apply to save throw: spells                (-2  -  +2)
25   HIT_N_DAM          Apply to hit roll and mudHealth roll          (-5  -  +5) 

%
*/
% :- module(objects, [  ]).

% :- include(library(prologmud/server/mud_header)).

:-register_module_type([world_types,parser]).

% :-register_module_type(dynamic).

predTypeMax(mudEnergy,tAgent,120).

typeProps(tAgent,[predInstMax(mudHealth,500)]).


% :-end_module_type(dynamic).

% mudLabelTypeProps(Lbl,Type,Props)/ground(mudLabelTypeProps(Lbl,Type,Props))==> (typeHasGlyph(Type,Lbl) , typeProps(Type,Props)).

% Vacuum World example objects........
mudLabelTypeProps("wl",tWall,[mudHeight(3),mudWeight(4)]).
mudLabelTypeProps("tr",tTree,[mudHeight(3),mudWeight(4)]).
mudLabelTypeProps("rk",tRock,[mudHeight(2),mudWeight(4)]).
mudLabelTypeProps("pt",tPit,[mudHeight(-1),mudWeight(4)]).
 mudLabelTypeProps("ot",tPowerOutlet,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenStays),mudActAffect(actTake,mudEnergy(+50))]).
 mudLabelTypeProps("nt",tNut,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+40)),spawn_rate(10)]).
mudLabelTypeProps("lw",tLowWall,[mudHeight(2),mudWeight(4)]).
mudLabelTypeProps("lg",tLedge,[mudHeight(2),mudWeight(4)]).
mudLabelTypeProps("lb",tLowBox,[mudHeight(2),mudWeight(2)]).
mudLabelTypeProps("hw",tHighWall,[mudHeight(3),mudWeight(4)]).
mudLabelTypeProps("hb",tHighBox,[mudHeight(3),mudWeight(3)]).
:- do_gc.
 mudLabelTypeProps("gd",tGold,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenDeletes),mudActAffect(actTake,mudScore(+10))]).
 mudLabelTypeProps("fd",tFood,[mudHeight(0),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+80))]).
mudLabelTypeProps("el",tElixer,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudHealth(+80))]).
mudLabelTypeProps("ed",tEdgeOfWorld,[mudHeight(10),mudWeight(4)]).
 mudLabelTypeProps("dt",tDirt,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenDeletes),mudActAffect(actTake,mudScore(+1)),spawn_rate(5)]).
mudLabelTypeProps("dr",tDoor,[mudHeight(3),mudWeight(2)]).

%Empty Location
% You *have* to use 0 as the ftID of the empty location. (no way!)
mudLabelTypeProps("--",isMissing,[]).

% What an agent turns into upon death.
% Must be named iCorpseFn (or edit agent_to_corpse/1 in lib.pl.
 mudLabelTypeProps("cp",tCorpsea,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+80)),mudActAffect(actTake,mudScore(+10))]).
 mudLabelTypeProps("cp",tCorpseb,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+120))]).
mudLabelTypeProps("da",tCorpsec,[mudHeight(2),mudWeight(2)]).
% This is used to make the monster roaming the maze a bit tougher
% It doesn't require any of the usual traits since the monster will never 
% leave the monster's mudPossession.
mudLabelTypeProps("th",tNastyKnife,[mudActAffect(actWield,mudAttack(2))]).
mudLabelTypeProps("th",tToughHide,[mudActAffect(actWear,mudArmor(2))]).


% Define the maximum mudEnergy and maximum mudHealth an agent can have


% :- include(prologmud(mud_footer)).
