% Usage:
% $ swipl -f none -g main -test_owlswrl halt -s test_owlswrl.pl
% Writes the test results into STDIN and statistics into STDERR.

:- assert(user:file_search_path(ape, '../prolog')).

:- use_module(ape('parser/ace_to_drs'), [
		acetext_to_drs/5
	]).

:- use_module(ape('utils/owlswrl/drs_to_owlswrl')).
:- use_module(ape('utils/owlswrl/owlswrl_to_xml'), [
		owlswrl_to_xml/2
	]).
:- use_module(ape('utils/owlswrl/owlswrl_to_fss'), [
		owlswrl_to_fss/1
	]).

:- use_module(ape('logger/error_logger'), [
		clear_messages/0,
		get_messages/1
	]). 

:- use_module(ape('utils/xmlterm_to_xmlatom'), [
		xmlterm_to_xmlatom/2
	]).

:- use_module(ape('lexicon/ulex'), [
		read_ulex/1
	]).

:- set_prolog_flag(float_format, '%.11g').


test_owlswrl('1-1', 'Every man is a human.').
test_owlswrl('1-2', 'Every man is somebody.').
test_owlswrl('1-3', 'Every man is something.').
test_owlswrl(2, 'Every human is a male or is a female.').
test_owlswrl(3, 'No dog is a cat.').
test_owlswrl(4, 'Every driver owns a car.').
test_owlswrl(5, 'Everything that a goat eats is some grass.').
test_owlswrl(6, 'John likes Mary.').
test_owlswrl(7, 'Everybody who loves somebody likes him/her.').
test_owlswrl(8, 'Every professor is a scientist and is a teacher.').
test_owlswrl(9, 'Everybody who is not a child is an adult.').
test_owlswrl(10, 'Nothing is a man and is a woman.').
test_owlswrl(11, 'If there is a woman then she does not like a snake.').
test_owlswrl(12, 'There is a woman who everybody likes.').

test_owlswrl(13, 'Every man likes at least 3 things.').
test_owlswrl(14, 'If a thing A is taller than a thing B then B is shorter than A.').
test_owlswrl(15, 'If something A is taller than something B and B is taller than something C then A is taller than C.').
test_owlswrl('15-1', 'If a thing A is taller than a thing that is taller than a thing C then A is taller than C.').

% SWRL
test_owlswrl('15-2', 'If a dog is taller than a cat that is taller than a mouse then the dog is taller than the mouse.').

test_owlswrl(16, 'Everybody who writes something is a human.').
test_owlswrl(17, 'Everybody who writes at least 1 thing is a human.').

test_owlswrl(18, 'Everything that somebody writes is a book or is a paper.').

test_owlswrl(19, 'Everything identifies at most 1 thing.').

test_owlswrl(20, 'If a thing A loves a thing B then B loves A.').

test_owlswrl(21, 'Nobody who likes a carrot is a carnivore.').

test_owlswrl(22, 'Every man likes at least 3 cars.').

test_owlswrl(23, 'Every man likes some cars.').

test_owlswrl(24, 'John likes every car.').

test_owlswrl(25, 'John likes no car.').

test_owlswrl(27, 'If there is a man then a dog likes the man.').

test_owlswrl(28, 'If there is a cat then at least 2 things like the cat.').

test_owlswrl(29, 'If there is a cat then at least 2 persons like the cat.').

test_owlswrl(30, 'For every thing at most 1 thing is identified by it.').

test_owlswrl(31, 'John likes Mary. Bill sees Mary.').

test_owlswrl(32, 'Every man is somebody that a dog likes.').

test_owlswrl(33, 'If a man sees a woman then the woman sees John.').

% SWRL
test_owlswrl(34, 'If a man sees a woman then a dog sees a cat.').

test_owlswrl(35, 'If a man sees a woman then John knows the man.').

test_owlswrl(36, 'Every man waits.').

test_owlswrl(37, 'Every man does not wait.').

test_owlswrl(38, 'Every man is an animal or does not wait.').

test_owlswrl(39, 'No man waits.').

% SWRL
test_owlswrl(40, 'If a man sees a dog then the man hears the dog.').

test_owlswrl(41, 'For every carnivore everything that the carnivore eats is a meat.').

test_owlswrl(42, 'Every man who sees a dog hears a cat that sees itself.').
test_owlswrl(43, 'Every man who sees a dog hears a cat that sees a mouse that hates the cat.').
test_owlswrl(44, 'Every man who sees a dog hears a cat that sees a mouse that hates itself.').
test_owlswrl(45, 'Every man who likes himself is strange.').
test_owlswrl(46, 'Every man likes himself.').

test_owlswrl(47, 'Every man who is not liked by a woman and who owns a dog sees a cat.').
test_owlswrl(48, 'If there is a man and it is false that the man is liked by a woman and that a dog is owned by the man then the man sees a cat.').

test_owlswrl(49, 'John\'s age is 30.').
test_owlswrl(50, 'John\'s address is "Poland".').
test_owlswrl(51, 'John is not Mary.').
test_owlswrl(52, 'It is false that a man sees a woman.').
test_owlswrl(53, 'Everybody who waits is a grown-up.').
test_owlswrl(54, 'Everybody whose age is 31 is a grown-up.').
test_owlswrl(55, 'Everybody whose address is "Poland" is a human.').
test_owlswrl(56, 'Everybody whose age is 31 and who waits is a grown-up.').
test_owlswrl(57, 'Every man likes no dog.').
test_owlswrl(58, 'Every student is John or is Mary.').
test_owlswrl(59, 'Everybody who is John or who is Mary is a student.').
test_owlswrl(60, 'John is a man or owns less than 3 cars.').
test_owlswrl(61, 'John does not like Mary.').
test_owlswrl(62, 'John does not own a car.').
test_owlswrl(63, 'John does not own more than 3 cars.').

test_owlswrl(64, 'A man is taller than more than 3 animals.').
test_owlswrl(65, 'A man is not a woman.').
test_owlswrl(66, 'Every carnivore eats every meat.').
test_owlswrl(67, 'Everybody likes everybody.').
test_owlswrl(68, 'John\'s brother likes everybody.').

test_owlswrl(69, 'If somebody X loves somebody Y then it is false that X hates Y.').

test_owlswrl(70, 'If a man likes somebody that is a person then the person owns a car.').
test_owlswrl(71, 'If John is a man then the man is a person.').

test_owlswrl(72, 'If a man owns a dog and the man owns a cat and the dog likes the cat then the man is a human.').
test_owlswrl(73, 'Every man is at most 3 cars.').

test_owlswrl(74, 'Mary is liked by nobody.').

test_owlswrl(75, 'Every man is something that likes something that owns a car and that likes Mary.').

test_owlswrl(76, 'Every man is something that likes a car and that likes a bike.').

test_owlswrl(77, 'No man is something that likes a car and that likes a bike.').

test_owlswrl(78, 'If there is a goat and everything that the goat eats is an apple then the goat is an animal.').

test_owlswrl(79, 'If there is a goat and everything that the goat eats is not an apple then the goat is an animal.').

test_owlswrl(80, 'Mary likes a cat. Every man likes the cat.').

test_owlswrl(81, 'John does not like every dog.').

test_owlswrl(82, 'John\'s brother likes Mary. An age of the brother is 10.').

test_owlswrl(83, 'John is something that is not Mary.').

test_owlswrl(84, 'John is not something that is Mary.').

test_owlswrl(85, 'Everything that likes something that sees something that hears something hates it.').

test_owlswrl(86, 'Every man owns exactly 3 cars.').

test_owlswrl(87, 'Every man owns exactly 3 things.').

test_owlswrl(88, 'If a man likes a dog that likes a cat and the man likes a cow that likes a sheep then the man owns a car.').

test_owlswrl(89, 'If there is a man then the man likes a dog that likes a cat and the man likes a cow that likes a sheep.').

test_owlswrl(90, 'If there is a man then the man likes a dog that likes a cat and that likes a rat and the man likes a cow that likes a sheep and that likes a pig.').

test_owlswrl(91, 'If a man is a dog that is a cat and the man is a cow that is a sheep then the man is a car.').

test_owlswrl(92, 'If there is a man then the man is a dog that is a cat and that is a rat and the man is a cow that is a sheep and that is a pig.').

test_owlswrl(93, 'If John likes Mary then Mary likes Bill.').

test_owlswrl(94, 'If John owns a car then there are at least 3 women that like John.').

test_owlswrl(95, 'There is at least 1 man.').

test_owlswrl(96, 'John likes at most 3 women.').
test_owlswrl(97, 'John likes less than 3 women.').
test_owlswrl(98, 'John likes exactly 3 women.').
test_owlswrl(99, 'John likes at least 3 women.').
test_owlswrl(100, 'John likes more than 3 women.').
test_owlswrl(101, 'John likes at most 1 woman.').
test_owlswrl(102, 'John likes less than 1 woman.').
test_owlswrl(103, 'John likes exactly 1 woman.').
test_owlswrl(104, 'John likes at least 1 woman.').
test_owlswrl(105, 'John likes more than 1 woman.').

test_owlswrl(106, 'Everybody who loves somebody loves himself.').

test_owlswrl(107, 'If a man likes Mary and Mary hates a dog then the man owns a car.').

test_owlswrl(108, 'If a man likes Mary and Mary does not hate a dog then the man owns a car.').

test_owlswrl(109, 'Every man is John who owns a car.').

test_owlswrl(110, 'Every man is John who does not own a car.').

test_owlswrl(111, 'Everybody\'s age is 31.').
test_owlswrl(112, 'Everybody\'s address is "Poland".').
test_owlswrl(113, 'If somebody\'s age is 31 then his address is "Poland".').

test_owlswrl(114, 'John\'s father is Bill.').

/* The following 6 sentences cannot be translated into OWL.
Note that 'E' and 'F' are variables and not proper names.
Therefore we have heavy anaphoric references between the IF and the THEN parts.
BUG: Maybe it's possible to convert this to SWRL though?
Probably not, since we have disjunction and negation here. */
test_owlswrl(115, 'If a room contains E and contains F then if the room contains a sculpture X then X is E or is F.').
test_owlswrl(116, 'For every room that contains E and that contains F if the room contains a sculpture X then X is E or is F.').
test_owlswrl(117, 'For every room that contains E and that contains F every sculpture that the room contains is E or is F.').
test_owlswrl(118, 'If a room contains E and contains F then it is false that the room contains a sculpture X and that it is false that X is E or is F.').
test_owlswrl(119, 'If a room contains E and contains F then it is false that the room contains a sculpture X and that X is not E and that X is not F.').
test_owlswrl(120, 'No room that contains E and that contains F contains a sculpture that is not E and that is not F.').

test_owlswrl(121, 'If there is a number X then X + 1 = John.').
test_owlswrl(122, 'If 1.1 * 2 = 2.2 then 0.9 = 2 - 1.1.').
% Note: E is a variable, Pi is a proper name.
test_owlswrl(123, 'If E approaches 2 then 3.14 approaches Pi.').

test_owlswrl(124, 'Bill is John\'s father.').
test_owlswrl(125, 'John\'s father likes Bill.').
test_owlswrl(126, 'Bill likes John\'s father.').
test_owlswrl(127, 'If something X is a father of something Y then X is a parent of Y.').
test_owlswrl(128, 'If something X is a part of something Y and Y is a part of something Z then X is a part of Z.').

% Maps to a SWRL rule with complex classes (negation and disjunction) as atoms
test_owlswrl(129, 'Every man that owns a car and that is not a manager cleans the car.').
test_owlswrl(130, 'Every man that does not ride a car, and that rides a bus or that rides a bike owns a dog that likes the man.').

test_owlswrl(131, 'For every thing X for every thing Y if X owns something that contains Y then X owns Y.').

test_owlswrl(132, 'If a man likes something X then the man sees X.').

test_owlswrl(133, 'John is a man.').

test_owlswrl(134, 'John owns a car.').

test_owlswrl(135, 'John is somebody.').

% BUG: RDF/XML is not generated (note: RDF/XML is deprecated now anyway)
test_owlswrl(136, 'If somebody X sees something that is heard by somebody Y then X likes Y.').

test_owlswrl(137, 'For every day a man does not see a woman.').

test_owlswrl(138, 'For every day John does not see a woman.').

test_owlswrl(139, 'For every day John does not see Mary.').

test_owlswrl(140, 'For every day John sees a dog or sees a cat.').

test_owlswrl(141, 'Exactly 2 countries border Estonia.').

test_owlswrl(142, 'Who does Mary like?').

test_owlswrl(143, 'There is a continent.').
test_owlswrl(144, 'There are at least 4 continents.').
test_owlswrl(145, 'There are exactly 4 continents.').
test_owlswrl(146, 'There are at most 4 continents.').
test_owlswrl(147, 'It is false that there are more than 4 continents.').
test_owlswrl(148, 'It is false that there are exactly 4 continents.').
test_owlswrl(149, 'It is false that there are less than 4 continents.').

test_owlswrl(150, 'Which countries border Estonia?').
test_owlswrl(151, 'Which countries do not border Estonia?').
test_owlswrl(152, 'What is a country that borders Estonia or that Switzerland borders?').

test_owlswrl(153, 'John is fond-of Mary.'). % pos
test_owlswrl(154, 'John is as rich as Mary.'). % pos_as
test_owlswrl(155, 'John is fonder-of Mary.'). % comp
test_owlswrl(156, 'John is more rich than Mary.'). % comp_than
test_owlswrl(157, 'John is fondest-of Mary.'). % sup

/* The following 6 sentences are all logically equivalent.
158--161 are lexically equivalent; 162--163 are lexically equivalent. */
test_owlswrl(158, 'If a room contains Sculpture-E and contains Sculpture-F then if the room contains a sculpture X then X is Sculpture-E or is Sculpture-F.').
test_owlswrl(159, 'For every room that contains Sculpture-E and that contains Sculpture-F if the room contains a sculpture X then X is Sculpture-E or is Sculpture-F.').
test_owlswrl(160, 'For every room that contains Sculpture-E and that contains Sculpture-F every sculpture that the room contains is Sculpture-E or is Sculpture-F.').
test_owlswrl(161, 'If a room contains Sculpture-E and contains Sculpture-F then it is false that the room contains a sculpture X and that it is false that X is Sculpture-E or is Sculpture-F.').
test_owlswrl(162, 'If a room contains Sculpture-E and contains Sculpture-F then it is false that the room contains a sculpture X and that X is not Sculpture-E and that X is not Sculpture-F.').
test_owlswrl(163, 'No room that contains Sculpture-E and that contains Sculpture-F contains a sculpture that is not Sculpture-E and that is not Sculpture-F.').

test_owlswrl(164, 'If Constant-E approaches 2 then 3.14 approaches Constant-Pi.').
test_owlswrl(165, 'If Constant-E approaches 3 then Constant-Pi approaches 4.').

test_owlswrl(166, 'No city overlaps-with a city that is not the city.').
test_owlswrl(167, 'Every city overlaps-with itself or does not overlap-with a city.').
test_owlswrl(168, 'If a city overlaps-with something X then X is the city or X is not a city.').

test_owlswrl(169, 'Every man who owns a car that likes a cat is a dog that is hated by a bike or that likes itself.').
test_owlswrl(170, 'Every man who owns a car is a dog that is hated by a bike or that likes itself.').
test_owlswrl(171, 'Every man is a dog that is hated by a bike or that likes itself.').
test_owlswrl(172, 'Every man is a dog that hates a bike or that likes itself.').

test_owlswrl(173, 'Which countries border a lake and use the lake?'). % should fail
test_owlswrl(174, 'What is a country that borders a territory that guards the country?'). % should fail
test_owlswrl(175, 'What is a country that borders a territory that guards the territory?'). % should succeed

test_owlswrl(176, 'Which territories guard themselves?').
test_owlswrl(177, 'Which territories border more than 3 territories that guard themselves?').
test_owlswrl(178, 'Which territories border nothing but lakes?').
test_owlswrl(179, 'What is a country that borders no sea?').

test_owlswrl(180, 'Everybody\'s ancestor is Adam.').
test_owlswrl(181, 'Everybody\'s ancestor is not Adam.').
test_owlswrl(182, 'Every part of EU is a country.').

test_owlswrl(183, 'If X is a friend of something that is an enemy of Y then X is an enemy of Y.').

test_owlswrl(184, 'Every man owns 2.5 cars.').

test_owlswrl(185, 'If a man owns a car then the man likes the car and likes exactly 3 cats.').
test_owlswrl(186, 'If John owns a car then Mary likes a car and likes at most 3 cats.').
test_owlswrl(187, 'Which man is John?').

test_owlswrl(188, 'John is a man that owns a car.').
test_owlswrl(189, 'John is at least 1 man.').
test_owlswrl(190, 'John is at least 2 men.').
test_owlswrl(191, 'John is a man. Mary is a woman.').
test_owlswrl(192, 'John is a man that is a manager.').
test_owlswrl(193, 'A man is a human.').
test_owlswrl(194, 'A man is John.').
test_owlswrl(195, 'John is John.').
test_owlswrl(196, 'A man is a man.').
test_owlswrl(197, 'A man is the man.').
test_owlswrl(198, 'A country is what?').

test_owlswrl(199, 'John is nothing but Mary.').
test_owlswrl(200, 'John\'s child is not Mary.').
test_owlswrl(201, 'John\'s child is nothing but Mary.').
test_owlswrl(202, 'Every child of John is Mary.').
test_owlswrl(203, 'Whose child is Mary and is nothing but Mary?').
test_owlswrl(204, 'Who is a person whose child is Mary and whose child is nothing but Mary?').

test_owlswrl(205, 'John knows a man. Who is the man?').

test_owlswrl(206, 'John owns a car. Mary owns the car. What is it?').
test_owlswrl(207, 'John owns a car. Mary owns the car. What is it? Bill sees the car.').
test_owlswrl(208, 'John owns a car. Mary owns the car. What is it? Bill sees John.').

test_owlswrl(209, 'Who owns nothing but cats?').

test_owlswrl(210, 'John is a man that sees Mary.').
test_owlswrl(211, 'John is a man that is seen by Mary.').
test_owlswrl(212, 'Mary sees John that is not a man.').

test_owlswrl(213, 'John is a man. Mary is a woman. Everybody likes the woman. Nobody likes the man.').

test_owlswrl(214, 'If there is a circle C and C\'s radius is R and C\'s area is S then S = Pi * (R * R) / 1.').

test_owlswrl(215, 'If John likes Mary then Bill is not William.').

test_owlswrl(216, 'If 1 + 2 = 3 then 1 = 3 - 2.').

test_owlswrl(217, 'If 1 + 2 = X then X = 3.').

test_owlswrl(218, 'If a country X1 surrounds a territory X2 and the territory X2 is a part of a country X3 and it is false that the country X3 is the country X1 then the territory X2 is an enclave.').

test_owlswrl(219, 'If "12" & "34" = "1234" then John owns at most 5 cars.').

test_owlswrl(220, 'If X = 1 + 2 then X = 3.').
test_owlswrl(221, 'For everything John likes Mary.').
test_owlswrl(222, 'If there is X and there is Y then X knows Y.').

% simple adjectives, both attributive and predicative
test_owlswrl(223, 'John is rich.').
test_owlswrl(224, 'John has a fast car.').
test_owlswrl(225, 'John has more than 5 fast cars.').
test_owlswrl(226, 'John has less than 5 fast cars.').
test_owlswrl(227, 'Every fast car is owned by a rich man.').
test_owlswrl(228, 'Every fast car is owned by at least 5 rich men.').
test_owlswrl(229, 'Every fast car is owned by at most 5 rich men.').
test_owlswrl(230, 'If John is rich then Mary is richer.'). % do not support that
test_owlswrl(231, 'If John is rich then Mary is pretty.').
test_owlswrl(232, 'Every talented and beautiful woman is rich and famous.').
test_owlswrl(233, 'Every talented and beautiful woman is rich and famous and is smarter than her own husband.').
test_owlswrl(234, 'Every man has at least 5 cars that are red.').
test_owlswrl(235, 'Every man has at most 5 cars that are red.').

% max. conditions that have a relative clause.
% there was a bug that was fixed only in ape_6_6_101116
test_owlswrl(236, 'Every man knows at least 2 persons whose age is 42.').
test_owlswrl(237, 'Every man knows at most 2 persons whose age is 42.').

test_owlswrl(238, 'Every man knows at least 2 persons whose friend is Bill.').
test_owlswrl(239, 'Every man knows at most 2 persons whose friend is Bill.').

test_owlswrl(240, 'Every man knows at least 2 persons that do not own no cat.').
test_owlswrl(241, 'Every man knows at most 2 persons that do not own no cat.').

% These should map to OWL and not to SWRL (fixed after ape-6.6-101116 was released)
test_owlswrl(242, 'There is X. If X is a fireman then X is a man.').
test_owlswrl(243, 'There is X. There is Y. If X likes Y then X is a man.').
test_owlswrl(244, 'There is X. There is Y. If X likes Y then Y is a man.').
test_owlswrl(245, 'There is X. If X likes John then X is a woman.').

test_owlswrl(246, 'There is X. If John likes X then John is a man.').

test_owlswrl(247, 'If John likes Mary then 1 = 2.').
test_owlswrl(248, 'If John likes Mary then 1 \\= 2.').
test_owlswrl(249, 'If John likes Mary then 1 < 2.').
test_owlswrl(250, 'If John likes Mary then 1 =< 2.').
test_owlswrl(251, 'If John likes Mary then 1 > 2.').
test_owlswrl(252, 'If John likes Mary then 1 >= 2.').

test_owlswrl(253, 'If John likes Mary then 1 is not 2.').
test_owlswrl(254, 'Everybody\'s address is not "Poland".').

test_owlswrl(255, 'There is X. If a man owns a dog that likes X then the dog likes the man.').

test_owlswrl(256, 'Mary\'s brother is John.').
test_owlswrl(257, 'Mary\'s brother is rich and is taller than John\'s father.').
test_owlswrl(258, 'John likes Mary\'s sister.').
test_owlswrl(259, 'Everybody likes something.').
test_owlswrl(260, 'Every man is a brother of somebody.').
test_owlswrl(261, 'Everybody who somebody likes does not hate a dog or is not an alien.').
test_owlswrl(262, 'Every man is taller than 3 women.').

% BUG: does not give an OWL representation (but "John writes every book." does).
test_owlswrl(263, 'John is an author of every book.').

test_owlswrl(264, 'John does not see what?').

% BUG: fails and gives the wrong error message "Yes/no queries not supported".
test_owlswrl(265, 'A man does not see what?').

test_owlswrl(266, 'John does not see a man who sees what?').

% Think about these:
test_owlswrl(267, 'John does not make everything that Mary wants.').
test_owlswrl(268, 'Who does not make everything that Mary wants?').

test_owlswrl(269, 'John is a man or is something and is nothing.').

% Measurement nouns (currently not implemented)
test_owlswrl(270, 'Every man has at least 3kg of rice.').
test_owlswrl(271, 'Every man has at most 3kg of rice.').
test_owlswrl(272, 'John has at least 3kg of rice.').
test_owlswrl(273, 'John has at most 3kg of rice.').
test_owlswrl(274, 'Every man has at least 3kg of apples.').
test_owlswrl(275, 'Every man has at most 3kg of apples.').
test_owlswrl(276, 'John has at least 3kg of apples.').
test_owlswrl(277, 'John has at most 3kg of apples.').

% Unsupported question with 2 query words
test_owlswrl(278, 'What likes what?').
test_owlswrl(279, 'Which man likes which woman?').

% Complex toplevel box
test_owlswrl(280, 'John has more than 2 cats that Mary likes.').
test_owlswrl(281, 'John has at least 2 cats that at least 3 dogs like.').
test_owlswrl(282, 'John knows at least 2 rich men.').
test_owlswrl(283, 'There are more than 3 rich men.').

test_owlswrl(284, 'A man likes Mary that likes the man.').
test_owlswrl(285, 'A man likes a woman that likes the man.').
test_owlswrl(286, 'A man owns at least 2 cars that a woman likes.'). % BUG

:- if( \+ current_module('clex_ape')).
main :-
	set_up_lexicon,
	time(test_owlswrl).

:- endif.

test_owlswrl :-
	forall(test_owlswrl(ID, Text), (
		clear_messages,
		acetext_to_drs(Text, _, _, Drs, _Messages),
		drs_to_owlswrl:drs_to_owlswrl(Drs, 'http://attempto.ifi.uzh.ch/ontologies/owlswrl/test', Text, OwlFss),
		get_messages(OwlMessages),
		with_output_to(atom(OwlMessagesPp), maplist(writeln, OwlMessages)),
		format('~w: ~w~n~w~n', [ID, Text, OwlMessagesPp]),
		ignore(owlswrl_to_fss(OwlFss)), nl,
		output_xml(OwlFss)
		)
	).


output_xml(Owlswrl) :-
	catch(
		(
			owlswrl_to_xml(Owlswrl, OwlXml),
			xmlterm_to_xmlatom(OwlXml, OwlXmlAtom),
			%store_as_file(ID, OwlXmlAtom),
			write(OwlXmlAtom), nl, nl
		),
		CatchType,
		(
			writeq(CatchType), nl
		)
	).


store_as_file(ID, Atom) :-
	concat_atom(['testruns/owlswrl/test_owlswrl', ID, '.owl'], Path),
	tell(Path),
	write(Atom),
	told.


set_up_lexicon :-
	% Read the large lexicon into ulex
	setup_call_cleanup(
	open('clex_lexicon.pl', read, Stream),
	    read_ulex(Stream),
	    close(Stream)),
	% Override some entries
	asserta(ulex:noun_sg(apple, 'iri|http://www.example.org/words#apple', neutr)),
	asserta(ulex:pn_sg('Bill', iri('http://www.example.org/words#Bill'), neutr)).
