
:- module(stanford_parser,
	  [lc_parse/3
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_declarations').

:- use_module('$REGULUS/PrologLib/utilities').

%---------------------------------------------------------------

:- dynamic complete_edge/3, incomplete_edge/5, completed_edge/4.
:- dynamic prediction/2, empty_agenda/5.

%---------------------------------------------------------------

lc_parse(Cat, Words, Parse):-
	lc_parse_init(Cat),
	parse(Words, 0, To),
	extract_parse(Cat, 0, To, Parse).

lc_parse_init(Cat) :-
	retractall(complete_edge(_From, _Mother, _To)),
	retractall(completed_edge(_From, _Mother, _Cat_IndexPair, _To)),
	retractall(incomplete_edge(_To, _Sought, _Mother, _Cat_IndexPair, _From)),
	retractall(prediction(_Index, _Cat)),
	retractall(empty_agenda(_Sought, _Mother, _Found, _From, _To)),
	initialize_chart(Cat).
	
initialize_chart(Cat):-
	assert(prediction(0, Cat)),
	allow_empty(0).
   
%---------------------------------------------------------------

parse([Word|RestWords], From, To):-
	Next is From + 1,
	scan(Word, From, Next),
	(RestWords = [NextWord|_] -> true ; NextWord = null_word),
	chart_closure(Next,NextWord),
	generate_new_predictions(Next),
	allow_empty(Next),
	parse(RestWords, Next, To).
parse([], LastIndex, LastIndex).

scan(Word, From, To):-
	assert(complete_edge(From, Word, To)),
	assert(completed_edge(From, Word, Word-From, To)).

chart_closure(To, NextWord):-
	complete_edge(From, Cat, To),
	match_completes(Cat, From, To, NextWord),
	fail.
chart_closure(_, _).  

predicted(From, Mother):-
	reachable_cat(Higher, Mother),
	prediction(From, Higher).

match_completes(Cat, From, To, NextWord):-
	incomplete_edge(From, [Cat|RestSought], Mother, _Cat_IndexPair, MotherFrom),
	match_edge(RestSought, Mother, Cat-From, MotherFrom, To, NextWord),
	fail.
match_completes(Cat, From, To, NextWord):-
	lookup_rule(Cat, Mother, RestSought),
	predicted(From, Mother),
	match_edge(RestSought, Mother, Cat-From, From, To, NextWord),
        fail.

match_edge([NextCat|RestSought], Mother, Pair, MotherFrom, To, NextWord):-
	is_reachable_word(NextCat, NextWord),
	assert_general(incomplete_edge(To, [NextCat|RestSought], Mother, Pair, MotherFrom)).
match_edge([], Mother, Pair, MotherFrom, To, NextWord):-
	assert_general(completed_edge(MotherFrom, Mother, Pair, To)),
	Mother = (M/_),
	Mother1 = (M/_),
	assert_general(complete_edge(MotherFrom, Mother1, To)),
	match_completes(Mother1, MotherFrom, To, NextWord).

generate_new_predictions(To):-
	incomplete_edge(To, [FirstCat|_], _, _Mother, _From),
	assert_general(prediction(To, FirstCat)),
	fail.
generate_new_predictions(_To).

is_reachable_word(Word, Word):-
	atomic(Word),
	!.
is_reachable_word(NextCat, NextWord):-
	user:reachable_word(NextCat, NextWord).
is_reachable_word(NextCat, _NextWord):-
	user:reaches_gap(NextCat).


allow_empty(To):-
	user:known_empty(Empty),
	predicted(To, Empty),
	assert_general(empty_agenda([], Empty, Empty-To, To, To)),
	fail.
allow_empty(To):-
	parse_empties(To).


parse_empties(To):-
	retract(empty_agenda(Sought, Category, Pair, From, To)),
	!,
	process_agenda_item(Sought, Category, Pair, From, To),
	parse_empties(To).
parse_empties(_To).

process_agenda_item([],Mother,Pair,From,To) :-
	assert_general(completed_edge(From, Mother,Pair,To)),
	MotherCat/_LF = Mother,
	NewMother = MotherCat/_,
	assert_general(complete_edge(From,NewMother,To)),
	incomplete_edge(From, [NewMother|RestIEsought],IEmother,_,IEfrom),
	assert(empty_agenda(RestIEsought,IEmother,NewMother-From,IEfrom,To)),
	fail.
process_agenda_item([],Cat,_PriorFrom,From,To) :-
	lookup_rule(Cat, Mother, RestSought),
	predicted(From, Mother),
	assert(empty_agenda(RestSought, Mother, Cat-From, From, To)),
	fail.

process_agenda_item([FirstSought|RestSought],Mother,Pair,From,To) :-
	assert_general(incomplete_edge(To, [FirstSought|RestSought],Mother,Pair,From)),
	assert(prediction(To, FirstSought)),
	(   complete_edge(To,FirstSought,CEto),
	    assert(empty_agenda(RestSought,Mother,FirstSought-To,From,CEto))
        ;   user:known_empty(Empty),
	    predicted(To, Empty),
	    assert(empty_agenda([], Empty, Empty-To, To, To))),
	fail.


process_agenda_item(_Sought,_Mother,_Found,_From,_To).

extract_parse(SemCat,From,To,[SemCat|Parses]) :-
	(atomic(SemCat) ->
	    Parses = [];
	    (completed_edge(From, SemCat,Pair, To),
	     extract_parse_list(SemCat, Pair, From, To,[], Parses))).

% MR, Dec 19 2003
% This clause doesn't appear to handle correctly the case where an empty constituent
% has an empty daughter - this is possible in the general Regulus grammar.
% Replace with revised version below.
%extract_parse_list(Mother, Empty-From, From, From, Partial, [['-----']|Partial]):-
%	!,
%	completed_edge(From, Mother, Empty-From, From).
extract_parse_list(Mother, Daughter-From, From, From, Partial, [['-----']|Partial]):-
	!,
	extract_parse_empty(From, Mother, Daughter).
extract_parse_list(_Mother, Daughter-From, From, To, Partial, [FirstParse|Partial]):-
	!,
%	completed_edge(From, Daughter, _Pair, To),
	complete_edge(From, Daughter, To),
	extract_parse(Daughter, From, To, FirstParse).

extract_parse_list(Mother, Daughter-PriorFrom, From, To, Partial, Parses):-
	incomplete_edge(PriorFrom, [Daughter|_], Mother, Pair, From),
	extract_parse(Daughter, PriorFrom, To, FirstParse),
	extract_parse_list(Mother, Pair, From, PriorFrom, [FirstParse|Partial], Parses).

extract_parse_empty(From, Mother, Daughter) :-
	completed_edge(From, Mother, Daughter-From, From),
	Mother = Daughter.
extract_parse_empty(From, Mother, Daughter) :-
	completed_edge(From, Mother, Daughter-From, From),
	Mother \== Daughter,
	extract_parse_empty(From, Daughter, _).	
    
%---------------------------------------------------------------

reachable_cat((Cat1/_Sem1), (Cat2/_Sem2)):-
	user:reachable_cat_helper(Cat1, Cat2).

lookup_rule(First, Mother, Rest):-
	atomic(First),
	!,
	user:indexed_rule(First, Mother, Rest).
lookup_rule((Cat/Sem), Mother, Rest):-
	user:indexed_rule(Cat, Sem, Mother, Rest).

%---------------------------------------------------------------

assert_general(A) :-
	\+ A,
	!,
	assert(A).

assert_general(A) :-
	numbervars(A, 0, _),
	A,
	!,
	fail.

assert_general(A) :-
	copy_term(A,A1),
	clause(A1,true,Ref),
	clause(A2,true,Ref),
	numbervars(A2,0,_),
	A = A2,
	erase_safe(clause(A2,true,Ref),Ref),
	fail.

assert_general(A) :-
	assert(A).

