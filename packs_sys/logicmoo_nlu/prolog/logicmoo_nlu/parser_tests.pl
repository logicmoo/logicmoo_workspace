:- if((prolog_load_context(source, File), prolog_load_context(file, File))).
:- module(parser_tests, []).
:- parser_e2c:export(parser_e2c:test_e2c/2).
:- import(parser_e2c:test_e2c/2).
:- multifile(parser_e2c:test_e2c/2).
:- dynamic(parser_e2c:test_e2c/2).
:- else.
:- multifile(test_e2c/2).
:- dynamic(test_e2c/2).
:- endif.

test_e2c(X,[ape(Y)]):- ape_test(Y,X).
test_e2c(X,[owlswrl(Y)]):- current_predicate(test_owlswrl/2),call(call,test_owlswrl(Y,X)), \+ clause(ape_test(Y,X),true).


ape_test(13, 'Every man likes at least 3 things.').
ape_test(14, 'If a thing A is taller than a thing B then B is shorter than A.').
ape_test(15, 'If something A is taller than something B and B is taller than something C then A is taller than C.').
ape_test('15-1', 'If a thing A is taller than a thing that is taller than a thing C then A is taller than C.').

% SWRL
ape_test('15-2', 'If a dog is taller than a cat that is taller than a mouse then the dog is taller than the mouse.').

ape_test(16, 'Everybody who writes something is a human.').
ape_test(17, 'Everybody who writes at least 1 thing is a human.').

ape_test(18, 'Everything that somebody writes is a book or is a paper.').

ape_test(19, 'Everything identifies at most 1 thing.').

ape_test(20, 'If a thing A loves a thing B then B loves A.').

ape_test(21, 'Nobody who likes a carrot is a carnivore.').

ape_test(22, 'Every man likes at least 3 cars.').

ape_test(23, 'Every man likes some cars.').

ape_test(24, 'John likes every car.').

ape_test(25, 'John likes no car.').

ape_test(27, 'If there is a man then a dog likes the man.').

ape_test(28, 'If there is a cat then at least 2 things like the cat.').

ape_test(29, 'If there is a cat then at least 2 persons like the cat.').

ape_test(30, 'For every thing at most 1 thing is identified by it.').

ape_test(31, 'John likes Mary. Bill sees Mary.').

ape_test(32, 'Every man is somebody that a dog likes.').

ape_test(33, 'If a man sees a woman then the woman sees John.').

% SWRL
ape_test(34, 'If a man sees a woman then a dog sees a cat.').

ape_test(35, 'If a man sees a woman then John knows the man.').

ape_test(36, 'Every man waits.').

ape_test(37, 'Every man does not wait.').

ape_test(38, 'Every man is an animal or does not wait.').

ape_test(39, 'No man waits.').

% SWRL
ape_test(40, 'If a man sees a dog then the man hears the dog.').

ape_test(41, 'For every carnivore everything that the carnivore eats is a meat.').

ape_test(42, 'Every man who sees a dog hears a cat that sees itself.').
ape_test(43, 'Every man who sees a dog hears a cat that sees a mouse that hates the cat.').
ape_test(44, 'Every man who sees a dog hears a cat that sees a mouse that hates itself.').
ape_test(45, 'Every man who likes himself is strange.').
ape_test(46, 'Every man likes himself.').

ape_test(47, 'Every man who is not liked by a woman and who owns a dog sees a cat.').
ape_test(48, 'If there is a man and it is false that the man is liked by a woman and that a dog is owned by the man then the man sees a cat.').

ape_test(49, 'John\'s age is 30.').
ape_test(50, 'John\'s address is "Poland".').
ape_test(51, 'John is not Mary.').
ape_test(52, 'It is false that a man sees a woman.').
ape_test(53, 'Everybody who waits is a grown-up.').
ape_test(54, 'Everybody whose age is 31 is a grown-up.').
ape_test(55, 'Everybody whose address is "Poland" is a human.').
ape_test(56, 'Everybody whose age is 31 and who waits is a grown-up.').
ape_test(57, 'Every man likes no dog.').
ape_test(58, 'Every student is John or is Mary.').
ape_test(59, 'Everybody who is John or who is Mary is a student.').
ape_test(60, 'John is a man or owns less than 3 cars.').
ape_test(61, 'John does not like Mary.').
ape_test(62, 'John does not own a car.').
ape_test(63, 'John does not own more than 3 cars.').

ape_test(64, 'A man is taller than more than 3 animals.').
ape_test(65, 'A man is not a woman.').
ape_test(66, 'Every carnivore eats every meat.').
ape_test(67, 'Everybody likes everybody.').
ape_test(68, 'John\'s brother likes everybody.').

ape_test(69, 'If somebody X loves somebody Y then it is false that X hates Y.').

ape_test(70, 'If a man likes somebody that is a person then the person owns a car.').
ape_test(71, 'If John is a man then the man is a person.').

ape_test(72, 'If a man owns a dog and the man owns a cat and the dog likes the cat then the man is a human.').
ape_test(73, 'Every man is at most 3 cars.').

ape_test(74, 'Mary is liked by nobody.').

ape_test(75, 'Every man is something that likes something that owns a car and that likes Mary.').

ape_test(76, 'Every man is something that likes a car and that likes a bike.').

ape_test(77, 'No man is something that likes a car and that likes a bike.').

ape_test(78, 'If there is a goat and everything that the goat eats is an apple then the goat is an animal.').

ape_test(79, 'If there is a goat and everything that the goat eats is not an apple then the goat is an animal.').

ape_test(80, 'Mary likes a cat. Every man likes the cat.').

ape_test(81, 'John does not like every dog.').

ape_test(82, 'John\'s brother likes Mary. An age of the brother is 10.').

ape_test(83, 'John is something that is not Mary.').

ape_test(84, 'John is not something that is Mary.').

ape_test(85, 'Everything that likes something that sees something that hears something hates it.').

ape_test(86, 'Every man owns exactly 3 cars.').

ape_test(87, 'Every man owns exactly 3 things.').

ape_test(88, 'If a man likes a dog that likes a cat and the man likes a cow that likes a sheep then the man owns a car.').

ape_test(89, 'If there is a man then the man likes a dog that likes a cat and the man likes a cow that likes a sheep.').

ape_test(90, 'If there is a man then the man likes a dog that likes a cat and that likes a rat and the man likes a cow that likes a sheep and that likes a pig.').

ape_test(91, 'If a man is a dog that is a cat and the man is a cow that is a sheep then the man is a car.').

ape_test(92, 'If there is a man then the man is a dog that is a cat and that is a rat and the man is a cow that is a sheep and that is a pig.').

ape_test(93, 'If John likes Mary then Mary likes Bill.').

ape_test(94, 'If John owns a car then there are at least 3 women that like John.').

ape_test(95, 'There is at least 1 man.').

ape_test(96, 'John likes at most 3 women.').
ape_test(97, 'John likes less than 3 women.').
ape_test(98, 'John likes exactly 3 women.').
ape_test(99, 'John likes at least 3 women.').
ape_test(100, 'John likes more than 3 women.').
ape_test(101, 'John likes at most 1 woman.').
ape_test(102, 'John likes less than 1 woman.').
ape_test(103, 'John likes exactly 1 woman.').
ape_test(104, 'John likes at least 1 woman.').
ape_test(105, 'John likes more than 1 woman.').

ape_test(106, 'Everybody who loves somebody loves himself.').

ape_test(107, 'If a man likes Mary and Mary hates a dog then the man owns a car.').

ape_test(108, 'If a man likes Mary and Mary does not hate a dog then the man owns a car.').

ape_test(109, 'Every man is John who owns a car.').

ape_test(110, 'Every man is John who does not own a car.').

ape_test(111, 'Everybody\'s age is 31.').
ape_test(112, 'Everybody\'s address is "Poland".').
ape_test(113, 'If somebody\'s age is 31 then his address is "Poland".').

ape_test(114, 'John\'s father is Bill.').

/* The following 6 sentences cannot be translated into OWL.
Note that 'E' and 'F' are variables and not proper names.
Therefore we have heavy anaphoric references between the IF and the THEN parts.
BUG: Maybe it's possible to convert this to SWRL though?
Probably not, since we have disjunction and negation here. */
ape_test(115, 'If a room contains E and contains F then if the room contains a sculpture X then X is E or is F.').
ape_test(116, 'For every room that contains E and that contains F if the room contains a sculpture X then X is E or is F.').
ape_test(117, 'For every room that contains E and that contains F every sculpture that the room contains is E or is F.').
ape_test(118, 'If a room contains E and contains F then it is false that the room contains a sculpture X and that it is false that X is E or is F.').
ape_test(119, 'If a room contains E and contains F then it is false that the room contains a sculpture X and that X is not E and that X is not F.').
ape_test(120, 'No room that contains E and that contains F contains a sculpture that is not E and that is not F.').

ape_test(121, 'If there is a number X then X + 1 = John.').
ape_test(122, 'If 1.1 * 2 = 2.2 then 0.9 = 2 - 1.1.').
% Note: E is a variable, Pi is a proper name.
ape_test(123, 'If E approaches 2 then 3.14 approaches Pi.').

ape_test(124, 'Bill is John\'s father.').
ape_test(125, 'John\'s father likes Bill.').
ape_test(126, 'Bill likes John\'s father.').
ape_test(127, 'If something X is a father of something Y then X is a parent of Y.').
ape_test(128, 'If something X is a part of something Y and Y is a part of something Z then X is a part of Z.').

% Maps to a SWRL rule with complex classes (negation and disjunction) as atoms
ape_test(129, 'Every man that owns a car and that is not a manager cleans the car.').
ape_test(130, 'Every man that does not ride a car, and that rides a bus or that rides a bike owns a dog that likes the man.').

ape_test(131, 'For every thing X for every thing Y if X owns something that contains Y then X owns Y.').

ape_test(132, 'If a man likes something X then the man sees X.').

ape_test(133, 'John is a man.').

ape_test(134, 'John owns a car.').

ape_test(135, 'John is somebody.').

% BUG: RDF/XML is not generated (note: RDF/XML is deprecated now anyway)
ape_test(136, 'If somebody X sees something that is heard by somebody Y then X likes Y.').

test_e2c("Every man that paints likes monet.", [ bratko]).
test_e2c("A woman that admires John paints.", [ bratko]).
test_e2c("Every woman that likes a man that admires monet paints.", [ bratko]).
test_e2c("John likes Annie.", [ bratko]).
test_e2c("Annie likes a man that admires monet.", [ bratko]).
test_e2c("Bertrand Russell wrote principia.", [ bratko]).
test_e2c("An author wrote principia.", [ bratko]).
test_e2c("Iraq is a country.", [ bratko]).
test_e2c("A happy author wrote principia.", [ bratko]).
test_e2c("Is Bertrand an author?", [ bratko]).
test_e2c("Bertrand is an author.", [ bratko]).
test_e2c("Is Bertrand an author?", [ bratko]).
test_e2c("Every author is a programmer.", [ bratko]).
test_e2c("Is Bertrand an programmer?", [ bratko]).  % Just a sanity test
test_e2c("What is a author?", [ bratko]).  % Just a sanity test
test_e2c("What did Bertrand write?", [ bratko]).
test_e2c("What is a book?", [ bratko]).
test_e2c("Principia is a book.", [ bratko]).
test_e2c("Bertrand is Bertrand.", [ bratko]).
test_e2c("Shrdlu halts.", [ bratko]).
test_e2c("Every student wrote a program.", [ bratko]).
test_e2c("Terry writes a program.", [ bratko]).
test_e2c("Terry writes a program that halts.", [ bratko]).

test_e2c("An author of every book wrote a program.", [ bratko]).
test_e2c("A man hapilly maried paints.", [ bratko]).
test_e2c("A hapilly maried man paints.", [ bratko]).
test_e2c("A man who knows paints.", [ bratko]).
test_e2c("A man gives something.", [ bratko]).
test_e2c("A man gives his word.", [ bratko]).

test_e2c("A man of his word paints.", [ bratko]).
test_e2c("A man paints.", [ bratko]).
test_e2c("A man that paints paints.", [ bratko]).
test_e2c("A man walks.", [ bratko]).
test_e2c("A man that walks paints.", [ bratko]).
test_e2c("It halts.", [ bratko]).

test_e2c("A man of his word that walks paints.", [ bratko]).
test_e2c("The cost of what the product is changes.", [ bratko]).



test_e2c("We need a virtual machine server.", [ aindy]).
test_e2c("The virtual machine server should have several VMs.", [ aindy]).
test_e2c("One VM should be the POSI VM.", [ aindy]).
test_e2c("One VM should be the FRDCSA.org VM.", [ aindy]).
test_e2c("One VM should be the mail server.", [ aindy]).
test_e2c("One computer should be the backup server.", [ aindy]).
test_e2c("I should not spend too much money, prefering instead to save money for future technology and emergencies.", [ aindy]).
test_e2c("I need a fast computer for work.", [ aindy]).
test_e2c("I wouldn't mind a windows computer for various tasks, but it's not necessary.", [ aindy]).
test_e2c("I need a dedicated computer for network security research.", [ aindy]).
test_e2c("I need a machine to run Tails on.", [ aindy]).
test_e2c("I need a machine off the network for airgap security.", [ aindy]).
test_e2c("One VM should be the game server for running game development projects.", [ aindy]).
test_e2c("I can repurpose justin.frdcsa.org to do AI work.", [ aindy]).
test_e2c("I could read about how to build a private watson.", [ aindy]).
test_e2c("Need backups.", [ aindy]).
test_e2c("Practice setting up backups of a machine to a different machine.", [ aindy]).
test_e2c("Set up schedules for updating the software on all machines.", [ aindy]).
test_e2c("Read books on server room layout.", [ aindy]).
test_e2c("I need a computer to set hadoop on and run NLP virtual machines on.", [ aindy]).
test_e2c("I need a computer to run all of the responsibilities of my private computers.", [ aindy]).
test_e2c("I want a gaming computer.  Do I?", [ aindy]).
test_e2c("I want a windows 7 computer for work.", [ aindy]).
test_e2c("I want a fast linux computer for work.", [ aindy]).
test_e2c("I want a computer to run AI stuff on, namely justin.frdcsa.org.", [ aindy]).
test_e2c("I could upgrade justin to have more capabilities!!!", [ aindy]).
test_e2c("I could install a fresh operating system on justin for work.", [ aindy]).
test_e2c("I probably want a separate git computer.", [ aindy]).
test_e2c("I need to start buying servers.", [ aindy]).
test_e2c("I want a rackmount case for servers.", [ aindy]).



test_e2c("Each african country is bordered by 2 oceans.", [ chat80(tell)]).
test_e2c("2 oceans border each african country.", [ chat80(tell)]).
test_e2c("There are 10 large cars.", [ quants]).
test_e2c("There are 10 oceans.", [ quants]).
test_e2c("There are 10 women.", [ quants]).
test_e2c("An ocean borders an African country.", [ chat80(tell)]).
test_e2c("What is the ocean that borders african countries and that borders asian countries?" , [tell]).
test_e2c("Indian ocean is the ocean that borders african countries and that borders asian countries.", [ tell]).

test_e2c("Bertrand wrote a book.", [ bratko(book)]).
test_e2c("Bertrand wrote nothing.", [ bratko(book)]).

% passes the above and fails the below
test_e2c("Bertrand wrote.", [ bratko(book)]).
test_e2c("Bertrand wrote a book about Gottlob.", [ bratko(book)]).
test_e2c("Bertrand wrote about Gottlob.", [ bratko(book)]).

test_e2c("Bertrand wrote nothing about Gottlob.", [ bratko(book)]).
test_e2c("Bertrand wrote nothing about Gottlob to Fred.", [ bratko(book)]).

test_e2c("What did alfred write to Bertrand?", [ bratko(book)]).
test_e2c("Alfred wrote a letter.", [ bratko(book)]).
test_e2c("Alfred wrote a letter to Bertrand.", [ bratko(book)]).
test_e2c("Alfred wrote something to Bertrand.", [ bratko(book)]).
test_e2c("Alfred wrote to Bertrand.", [ bratko(book)]).
test_e2c("Alfred wrote to Bertrand a letter.", [ bratko(book)]).
test_e2c("Alfred wrote Bertrand a letter.", [ bratko(book)]).
test_e2c("Who did alfred write a letter to?", [ bratko(book)]).

test_e2c("Alfred gave it.", [ bratko(book)]).
test_e2c("Alfred gave a book.", [ bratko(book)]).

test_e2c("a pride of lions paint", [of]).
test_e2c("a flock of birds paint", [of]).
test_e2c("a litter of pups paint", [of]).
test_e2c("a prickle of porcupines paint", [of]).
test_e2c("a litter of cubs paint", [of]).
test_e2c("a pack of dogs paint", [of]).
test_e2c("a colony of beavers paint", [of]).
test_e2c("a gaggle of geese paint", [of]).
test_e2c("a family of otters paint", [of]).
test_e2c("a huddle of walruses paint", [of]).
test_e2c("a herd of deer paint", [of]).
test_e2c("a culture of bacteria paint", [of]).
test_e2c("a swarm of bees paint", [of]).
test_e2c("a bed of clams paint", [of]).
test_e2c("a school of cod paint", [of]).
test_e2c("a herd of dinosaurs paint", [of]).
test_e2c("a mess of iguanas paint", [of]).
test_e2c("a mob of wombats paint", [of]).
test_e2c("a pod of pelicans paint", [of]).
test_e2c("a troop of boy scouts paint", [of]).
test_e2c("a team of athletes paint", [of]).
test_e2c("a panel of experts paint", [of]).
test_e2c("a crew of sailors paint", [of]).
test_e2c("a band of robbers paint", [of]).
test_e2c("a troupe of performers paint", [of]).
test_e2c("a crowd of onlookers paint", [of]).
test_e2c("a curse of painters paint", [of]).
test_e2c("a fleet of cars paint", [of]).
test_e2c("a pair of shoes paint", [of]).
test_e2c("a fleet of ships paint", [of]).
test_e2c("an anthology of stories paint", [of]).


test_e2c("One woman paints.", [ quants]).
test_e2c("No woman paints.", [ quants]).
test_e2c("Some woman paints.", [ quants]).
test_e2c("Every woman paints.", [ quants]).
test_e2c("Each woman paints.", [ quants]).
test_e2c("Any woman paints.", [ quants]).
test_e2c("The woman paints.", [ quants]).

% ;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\candc;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\ape;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\prolog

% test_e2c(S, _T) :- \+ ground(S), !, fail.

test_e2c("The not woman paints.", [ quants]).  % ? The Good Place "the not a girl"

test_e2c("Not a woman paints.", [ quants]).
test_e2c("Not one woman paints.", [ quants]).
test_e2c("Not no woman paints.", [ quants_neg_postcond]).
test_e2c("Not some woman paints.", [ quants]).   % ? not just some woman paints
test_e2c("Not every woman paints.", [ quants]).
test_e2c("Not each woman paints.", [ quants]).
test_e2c("Not any woman paints.", [ quants]).



test_e2c("The women paint.", [ quants]).

test_e2c("Women paint.", [ quants]).
test_e2c("Some women paint.", [ quants]).
test_e2c("No women paint.", [ quants]).
test_e2c("All women paint.", [ quants]).
test_e2c("Any women paint.", [ quants]).

test_e2c("Not women paint.", [ quants]).

test_e2c("Not no women paint.", [ quants]).
test_e2c("Not all women paint.", [ quants]).
test_e2c("Not any women paint.", [ quants]).


test_e2c("The three women paint.", [ quants]).

test_e2c("Three women paint.", [ quants]).
test_e2c("Some three women paint.", [ quants]).
test_e2c("No three women paint.", [ quants]).
test_e2c("Every three women paint.", [ quants]).
test_e2c("All three women paint.", [ quants]).
test_e2c("Any three women paint.", [ quants]).

test_e2c("Not three women paint.", [ quants]).
test_e2c("Not some three women paint.", [ quants]).
test_e2c("Not no three women paint.", [ quants]).
test_e2c("Not all three women paint.", [ quants]).
test_e2c("Not every three women paint.", [ quants]).
test_e2c("Not any three women paint.", [ quants]).

test_e2c("Not three of the women paint.", [ quants]).
test_e2c("Not some of the three women paint.", [ quants]).
test_e2c("Not no three of the women paint.", [ quants]).
test_e2c("Not all three of the women paint.", [ quants]).
test_e2c("Not every three of the women paint.", [ quants]).
test_e2c("Not any three of the women paint.", [ quants]).

test_e2c("Not three of the four women paint.", [ quants]).
test_e2c("Not none of three out of the four women paint.", [ quants]).
test_e2c("Not all three of the four women paint.", [ quants]).
test_e2c("Not any three of the four women paint.", [ quants]).

test_e2c("Three of the four women paint.", [ quants]).
test_e2c("Three out of the four women paint.", [ quants]).
test_e2c("All three of the four women paint.", [ quants]).
test_e2c("Any three of the four women paint.", [ quants]).


test_e2c("I paint", [ pronoun]).
test_e2c("you paint", [ pronoun]).
test_e2c("We paint", [ pronoun]).
test_e2c("None paint", [ pronoun]).
test_e2c("They paint", [ pronoun]).
test_e2c("Some paint", [ pronoun]).

test_e2c("It paints", [ pronoun]).
test_e2c("he paints", [ pronoun]).
test_e2c("She paints", [ pronoun]).
test_e2c("Someone paints", [ pronoun]).

test_e2c("Anybody paints", [ pronoun]).
test_e2c("Anyone paints", [ pronoun]).
test_e2c("Anything paints", [ pronoun]).
test_e2c("Everybody paints", [ pronoun]).
test_e2c("Everyone paints", [ pronoun]).
test_e2c("Everything paints", [ pronoun]).
test_e2c("Nobody paints", [ pronoun]).
test_e2c("No one paints", [ pronoun]).
test_e2c("Nothing paints", [ pronoun]).
test_e2c("One paints", [ pronoun]).
test_e2c("Somebody paints", [ pronoun]).
test_e2c("Something paints", [ pronoun]).

test_e2c("Not anybody paints", [ pronoun]).
test_e2c("Not anyone paints", [ pronoun]).
test_e2c("Not anything paints", [ pronoun]).
test_e2c("Not everybody paints", [ pronoun]).
test_e2c("Not everyone paints", [ pronoun]).
test_e2c("Not everything paints", [ pronoun]).
test_e2c("Not nothing paints", [ pronoun]).
test_e2c("Not one paints", [ pronoun]).
test_e2c("Not somebody paints", [ pronoun]).
test_e2c("Not something paints", [ pronoun]).

test_e2c("She likes i", [ pronoun]).
test_e2c("She likes me", [ pronoun]).
test_e2c("She likes you", [ pronoun]).
test_e2c("She likes it", [ pronoun]).
test_e2c("She likes us", [ pronoun]).
test_e2c("She likes them", [ pronoun]).
test_e2c("She likes no one", [ pronoun]).
%test_e2c("She likes noone", [ pronoun]).
test_e2c("She likes none", [ pronoun]).


% test_e2c("She likes her", [ pronoun]).
test_e2c("She likes him", [ pronoun]).
test_e2c("She likes herself", [ pronoun]).
test_e2c("She likes himself", [ pronoun]).  % Maybe

test_e2c("It is us that paints", [ pronoun]).
test_e2c("It is them that paints", [ pronoun]).
test_e2c("It is he that paints", [ pronoun]).
test_e2c("It is she that paints", [ pronoun]).
test_e2c("It is her that paints", [ pronoun]).
test_e2c("It is him that paints", [ pronoun]).



test_e2c("We are us that paint", [ pronoun]).
test_e2c("We are all of us that paint", [ pronoun]).
test_e2c("It is everybody that paints", [ pronoun]).

/* Prediate:  descriptionTest/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(33).
number_of_clauses(1).
Pattern: descriptionTest(_G1470,_G1471). 
 */
descriptionTest('NpcCol1000-Geordi684',["Lieutenant","Commander","Geordi","LaForge","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Geordi","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things"]).
descriptionTest('NpcCol1002-Worf720',["Lieutenant","Worf","Klingon","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3400","#$PunchingSomething mudBareHandDamage: 9d9+60","Worf","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong"]).
descriptionTest(vacuum(1),["Lieutenant","Commander","Data","Android"]).
descriptionTest(v12,["Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He stowed super-human strength, and is extremely tough","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000","#$PunchingSomething mudBareHandDamage: 10d10+75","Data","CycLBot","CycBot","CycBot1","Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He stowed super-human strength, and is extremely tough"]).
descriptionTest(explorer(player1),["Lieutenant","Commander","Human","Player",
            "Explorer Player",
            "ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON",
            "NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000",
            "#$PunchingSomething mudBareHandDamage: 10d10+75","Player","Player","Human",
            "Logged on player character"]).
descriptionTest('NpcCol1002-Worf720',["Lieutenant","Worf","Klingon","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3400","#$PunchingSomething mudBareHandDamage: 9d9+60","Worf","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong"]).
descriptionTest('NpcCol1003-Dr-Crusher677',["Doctor","Beverly","Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Dr. Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard"]).
descriptionTest('NpcCol1004-Troi712',["Counselor","Deanna","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds"]).
descriptionTest('NpcCol1005-Riker707',["Commander","William","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in charge of keeping the crew in line","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+52","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in charge of keeping the crew in line"]).
descriptionTest('NpcCol1006-Picard701',["Captain","Jean","Luc","Jean-Luc","Picard","Captain Picard","Captain Jean-Luc Picard is standing here, watching you","Captain Picard is a very important man","He's in charge of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_SANCTUARY","NPC_NOTRACK","+mudToHitArmorClass0: 0","mudMaxHitPoints: 20d20+5000","#$PunchingSomething mudBareHandDamage: 12d12+75","Picard","Captain Picard","Captain Jean-Luc Picard is standing here, watching you","Captain Picard is a very important man","He's in charge of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!"]).
descriptionTest('NpcCol1007-Guinan689',["Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender"]).
descriptionTest('NpcCol1008-OBrien696',["Chief","O'Brien","Transporter","Chief O'Brien","Chief O'Brien is here, waiting to teleport you somewhere","Chief O'Brien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","O'Brien","Chief O'Brien","Chief O'Brien is here, waiting to teleport you somwhere","Chief O'Brien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms"]).
descriptionTest('NpcCol1009-Wesley716',["Wesley","Crusher","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father","ACT_STAY_ZONE","ACT_WIMPY","wimpy mobile will try to flee when it gets low on hit points.","A mobile which is both aggressive and wimpy will not attack a player that is awake","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+1400","#$PunchingSomething mudBareHandDamage: 9d9+24","Wesley","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father"]).
descriptionTest('NpcCol1010-Livingston726',["Livingston","fish","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Livingston","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive"]).
descriptionTest('NpcCol1011-Spot727',["spot","the","cat","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Spot","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal"]).
descriptionTest('NpcCol1012-Ensign728',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign732',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign736',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign740',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign744',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign748',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign752',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1013-Alexander671',["alexander","rozhenko","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Alexander","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong"]).
descriptionTest('ArtifactCol1000-Phaser676',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers.", "It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1000-Phaser776',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers.", "It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1000-Phaser700',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers.", "It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1000-Phaser724',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers.", "It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1001-5-Phaser-Rifle705',["phaser","rifle","a phaser rifle","A large phaser rifle is lying here","damageNumberDice 7","damageSizeDice 6","WeaponBlasting","This phaser rifle looks pretty powerful.", "These weapons are used mainly on assault type missions, where power is important","5 Phaser Rifle","a phaser rifle"]).
descriptionTest('ArtifactCol1002-Red-Uniform704',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships.", "It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform710',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships.", "It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform719',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships.", "It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform739',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships.", "It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform743',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships.", "It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform675',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships.", "It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform775',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships.", "It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform687',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships.", "It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform699',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships.", "It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform723',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships.", "It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform731',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships.", "It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform735',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships.", "It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform680',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships.", "It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform715',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships.", "It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform747',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships.", "It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform751',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships.", "It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform755',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships.", "It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1005-Boots673',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).



descriptionTest('ArtifactCol1005-Boots773',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots678',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots685',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots697',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots702',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots708',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots713',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots717',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots721',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots729',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots733',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots737',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots741',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots745',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots749',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots753',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty.", "They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1006-Comm-Badge674',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge774',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge679',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge686',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1",
                     "These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge698',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge703',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge709',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge714',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge718',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge722',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge730',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge734',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge738',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge742',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge746',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge750',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge754',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship.", "It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1007-Sash725',["worf's","worf","sash","Worf's sash","Worf's silver chain sash has been left here","armorLevel: 8","Worf's sash is some sort of Klingon clothing.","Worf always wears it, which makes you wonder how you managed to get a hold of it..","Sash","Worf's sash"]).
descriptionTest('ArtifactCol1008-VISOR688',["geordi","geordi's","visor","Geordi's VISOR","Geordi's VISOR is lying here","armorLevel: 2","Geordi's VISOR was made specially for him, because he's blind.","This piece of equipment allows him to see things, but differently than normal eyes.","I wonder how Geordi is managing, now that you've stolen his only way of seeing?","VISOR","Geordi's VISOR"]).
descriptionTest('ArtifactCol1009-Medical-Tricorder681',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts.", "While it isn't made for major injuries, it can help you limp home.", " To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Medical-Tricorder682',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts.", "While it isn't made for major injuries, it can help you limp home.", " To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Medical-Tricorder683',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts.", "While it isn't made for major injuries, it can help you limp home.", " To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Tricorder759',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts.", "While it isn't made for major injuries, it can help you limp home.", " To use, hold it and then use it","Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Tricorder760',["medical","tricorder",
                     "a medical Tricorder","A medical Tricorder is lying here, ready to be used",
                     "mudLevelOf: 10",
                     "chargeCapacity: 5",
                     "chargeRemaining: 5",
                     "This medical Tricorder is used to heal small wounds and cuts.", "While it isn't made for major injuries, it can help you limp home.", " To use, hold it and then use it",
                     "Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Tricorder761',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts.", "While it isn't made for major injuries, it can help you limp home.", " To use, hold it and then use it","Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1010-Dilithium-Crystal756',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships.","This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
descriptionTest('ArtifactCol1010-Dilithium-Crystal757',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships.","This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
descriptionTest('ArtifactCol1010-Dilithium-Crystal758',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships.","This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
descriptionTest('ArtifactCol1011-5-Picards-Flute706',["picard","picard's","flute","Picard's flute","Captain Picard's wooden flute is sitting here","Captain Picard recieved this flute when he lost his memory and was stuck on some strange world.","Now, he plays it to relieve stress","5 Picard's Flute","Picard's flute"]).
descriptionTest('ArtifactCol1012-Trombone711',["riker","riker's","trombone","Riker's trombone","Commander Riker's trombone has been placed here","Commander Riker considers himself to be a talented jazz musician.","He practices on this trombone all the time","Trombone","Riker's trombone"]).
descriptionTest('ArtifactCol1020-Tea690',["tea","cup","a small cup","A small cup of tea is sitting here","Tea","a small cup"]).
descriptionTest('ArtifactCol1021-Synthehol691',["wine","bottle","synthehol","a synthehol","A bottle of synthehol is standing here","Synthehol","a synthehol"]).
descriptionTest('ArtifactCol1022-Ferengi-Ale692',["ale","ferengi","bottle","a Ferengi bottle","A bottle of Ferengi ale is sitting here","Ferengi Ale","a Ferengi bottle"]).
descriptionTest('ArtifactCol1023-Romulan-Whisky693',["whisky","whiskey","romulan","bottle","a Romulan bottle","A bottle of Romulan whiskey is sitting here","Romulan Whisky","a Romulan bottle"]).
descriptionTest('ArtifactCol1024-Lemonade-Prune-Juice694',["lemonade","prune","juice","glass","a small glass","A small glass of prune juice is sitting here","Lemonade","Prune Juice","a small glass"]).
descriptionTest('ArtifactCol1025-Vulcan-Beer695',["beer","vulcan","bottle","a Vulcan bottle","A bottle of Vulcan beer is standing here","Vulcan Beer","a Vulcan bottle"]).
descriptionTest('iArea1000',["Main Engineering","You find yourself in the middle of main engineering","The room is longer than it is wide, and it has fairly low ceilings","Computer terminals cover all the walls, and a large table built into the floor sits in the middle of the room","At the far end of the room you see the warp core, a large pulsating vertical tube"]).
descriptionTest('iArea1002',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
descriptionTest('iArea1001',["Geordi's Quarters","You're in the middle of Geordi's quarters","The room is sparsely decorated, due to the fact that Geordi is blind","A small personal computer sits on a desk against the western wall, in between two windows that look out into space","A neatly made bed has been placed against the northern wall"]).
descriptionTest('iArea1005',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
descriptionTest('iArea1003',["Data's Quarters","You're in the middle of Data's quarters","Some easils and paintings have been left scattered around the southern part of the room, while a huge computer screen showing a cross section of the Enterprise covers the entire northern wall","In front of the screen is a large desk, which is covered in computer controls","You can't see a bed in this room, but you figure it's because Data doesn't sleep"]).
descriptionTest('iArea1004',["The Brig","You're in the dimly lit Brig","This is where all the criminals and prisoners are kept while on board the Enterprise","Three fairly large cells can been seen in the southern part of the room, and they're all empty","A computer control panel is situated in the northwestern corner of the room, which is where the force fields for the cells are controlled",'The panel says:

***************************************************
*                                                 *
*            NCC-1701-D - ENTERPRISE              *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 1            *
*                                                 *
***************************************************
']).
descriptionTest('iArea1008',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
descriptionTest('iArea1006',["Transporter Room","You're in the Enterprise transporter room","A computer terminal is sitting near the southern wall, where the transporter chief can control the transporters","Eight round transport pads have been arranged in a circle, on a raised platform against the northern wall"]).
descriptionTest('iArea1042',["Transporter Beam","You find yourself in a transporter beam","All you can see is blue flashing light","It feels as though your body is racing around at high speeds","As you try to look down at your body, you realize that there's nothing there!"]).
descriptionTest('iArea1007',["School","You step through the doors and find yourself in a large school room","Various tables and chairs are set up all around the room, and many paintings and drawings have been attached to the walls","Several computer consoles with a children's interface on them can be seen on the tables"]).
descriptionTest('iArea1010',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1009',["Holodeck 2","You're now on Holodeck 2","The room is just a large cube, with jet black walls and a yellow grid painted on the floors, the walls, and the ceiling","This is where different programs can be loaded and experienced, which seem totally real","Right now, this holodeck is not functioning",'
***************************************************
*                                                 *
*            NCC-1701-D - "ENTERPRISE"            *
*                    HOLODECK 2                   *
*                                                 *
*              STATUS : Inactive                  *
*     CURRENT PROGRAM : N/A                       *
*            SAFETIES : N/A                       *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING:", "While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
***************************************************']).
descriptionTest('iArea1011',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can accessthe other decks on the Enterprise"]).
descriptionTest('iArea1013',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
descriptionTest('iArea1032',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1012',["Cargo Bay 1","You're in the main cargo bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
descriptionTest('iArea1016',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
descriptionTest('iArea1014',["Riker's Quarters","You've arrived in Riker's quarters","The room is very neat and tidy, with a couch and several chairs aranged around a coffee table by the eastern wall","A small partition at the northern part of the room seperates his sleeping area with the rest of the room"]).
descriptionTest('iArea1015',["Sick Bay","You're in the middle of the Enterprise's Sick Bay","About a dozen beds are arranged along the walls of the room, while several carts covered with medical supplies are scattered around the room","A large glass window in the northern part of the room separates the doctor's office with the rest of the room",'
***************************************************
*                                                 *
*            NCC-1701-D - "ENTERPRISE"            *
*                    HOLODECK 4                   *
*                                                 *
*              STATUS : Active                    *
*     CURRENT PROGRAM : Sherlock Holmes (19th     *
*                       century London)           *
*            SAFETIES : Disabled                  *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING:", "While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
*             ---ENTER WHEN READY---              *
*                                                 *
*************************************************** ']).
descriptionTest('iArea1019',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
descriptionTest('iArea1017',["Holodeck 4 Entrance - A Narrow Alley","You emerge into a dark narrow alley","Tall dark brick buildings block your way north and south","You can see that the windows on the buildings are fairly high, and some have been boarded up","The smell from the rotting food and garbage mixing with the foul water on the ground is unbearable","You can hear the sounds of a bustling marketpace to the east","The archway leading out of the holodeck is west"]).
descriptionTest('iArea1018',["Crusher's Quarters","You're in Doctor Crusher's quarters","Several different paintings are attached to the walls, and you also notice a few sculptures","A neatly made bed is located by the northern wall, in between two large windows looking out into space"]).
descriptionTest('iArea1021',["Ten Forward","You're now in Ten Forward, the entertainment room of the Enterprise","The entire northern wall is covered with windows looking out into space, while two large wooden doors with the Starfleet insignia stamped on them face south","Many round metal tables are scattered around the room, surrounded by metal chairs","A long bar spans almost the entire length of the southern part of the room, and about two dozen bar stools are sitting in front of it","It's very noisy in here, due to all the talking and laughing"]).
descriptionTest('iArea1020',["Enterprise Security","You're standing in the dimly lit Enterprise Security","Weapons lockers cover all of the walls, except along the northern wall, where a large glass window protects dozens of different phasors, blaster rifles, and other high tech weapons","Three long tables surrounded by chairs stretch across the room"]).
descriptionTest('iArea1022',["Shuttle Bay","You're in the main shuttle bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see three different shuttle crafts sitting here, waiting to be flown","A large grey door leads into space"]).
descriptionTest('iArea1024',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
descriptionTest('iArea1039',["Outer Space by the Enterprise","You're floating in outer space right beside the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold","A large grey door leads into the Enterprise's Shuttle Bay"]).
descriptionTest('iArea1023',["Troi's Quarters","You're in Counselor Deanna Troi's quarters","Several different paintings have been hung from the walls, and a small couch and a recliner are positioned around a coffee table","A neatly made bed is partially hidden behind a curtain at the northern part of the room"]).
descriptionTest('iArea1027',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape",
"
***************************************************
*                                                 *
*            NCC-1701-D - ENTERPRISE            *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 3            *
*                                                 *
***************************************************
"]).
descriptionTest('iArea1025',["Worf's Quarters","You're in Worf's quarters","A small table is sitting in the southeastern corner, and on it is a small potted plant","An impressive selection of Klingon weapons have been mounted on the northern wall, and a partition splits this room with Worf's bedroom to the east"]).
descriptionTest('iArea1026',["Enterprise Gym","You emerge into the Enterprise gym","The room is quite large, with a soft grey floor","A set of lockers against the southern wall contain all of the necessary equipment needed for using the gym","A thick stack of mats have been piled high in one corner, which can be used for different activities","Captain Picard likes to come here to practice his fencing"]).
descriptionTest('iArea1030',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
descriptionTest('iArea1028',["Picard's Quarters","You find yourself standing by the door of Captain Picard's quarters","He isn't very fond of visitors, but you decide to stay and have a look around","You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south","A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table","Two large windows offer a great view of space","A small partition at the northern part of the room contains Picard's sleeping area"]).
descriptionTest('iArea1029',["Science Lab","You're in the Enterprise science lab","A strange looking machine sits in the middle of the room, up on a slightly raised platform","It looks as though something(or someone) could be placed inside, hooked up to the multitude of wires and cables, and have scientific tests performed on it(or them)","A complex looking computer console is facing this machine","Around the rest of the room are counterops with with the odd computer terminal"]).
descriptionTest('iArea1031',["Cargo Bay 2","You're in the cargo bay 2 of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
descriptionTest('iArea1033',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1034',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1036',["Main Bridge - Upper Half","You find yourself on the upper half of the main bridge of the USS Enterprise","Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship","The entire southern wall is covered with computer consoles, where the ship's main systems are controlled","Two small curved ramps on either side of the room lead north to the lower part of the bridge, and a large circular skylight shows the space outside the ship"]).
descriptionTest('iArea1035',["Picard's Ready Room","You're standing in Captain Picard's ready room","A long couch has been placed beside the door, while a large U shaped desk is located by the northern wall","A small computer screen is sitting on the desk, as well as several other papers and documents","A single high window beside the desk looks into space, and a fish tank is located in the northwestern corner of the room","This is where the Captain makes all of his important decisions"]).
descriptionTest('iArea1038',["Main Bridge - Lower Half","You find yourself on the lower half of the main bridge of the USS Enterprise","An enormous view screen covers almost the entire northern wall, and is currently displaying a view of the stars rushing by","Three large chairs in the northern part of the room, in front of the railing, face the screen","This is where the Captain, Commander Riker, and Counselor Troi sit","Two computer consoles with built in chairs rest about ten feet in front of the chairs, also facing the view screen","This is where the ship's pilot and information officer sit"]).
descriptionTest('iArea1037',["Conference Room","You're in the conference room of the Enterprise","A large glass rectangular table sits in the middle of the room, surrounded by about a dozen comfortable looking office chairs","The entire eastern wall is covered with windows, looking out into space","This is where the senior officers of the Enterprise meet and discuss important issues"]).
descriptionTest('iArea1040',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).
descriptionTest('iArea1041',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).

parser_e2c:test_e2c(E,[descriptionTest]):- findall(Sents,descriptionTest(_,[_|Sents]),LL),flatten(LL,FL),list_to_set(FL,SS),member(E,SS),
  \+ atom_contains(E,+),\+ atom_contains(E,':'),\+ atom_contains(E,'*'),
  atom_chars(E,[C|_]),char_type(C,upper),concat_atom(L,' ',E),length(L,Len),Len>2.

:- use_module(library(logicmoo_nlu/e2c/e2c_fracas)).

:- if((prolog_load_context(source, File), prolog_load_context(file, File))).
:- fixup_exports.
:- endif.

