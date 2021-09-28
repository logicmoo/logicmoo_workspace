:- module(nsm_dict,[
		    switch_makedict/1,
		    parse_dict_entry_header/2,
		    assert_dict_variable/4,
		    assert_dict_entry/3,
		    assert_found_morph/3,
		    parse_SIL_record/3
		   ]).

:- use_module(messages).
:- use_module(utils).

:- dynamic(found_morph/3).
:- dynamic(dict_var/2).
:- dynamic(dict_entry/2).

/** <module> NSM Dictionary parser

This module allows parsing of NSM lexical definitions, like this one,
slightly adapted from Goddard, Semantic Analysis, p. 204

==
someone (X) went =

before X was somewhere
X wanted to be somewhere else
because of this X moved for some time
because of this after this X was not in this place any more
X was somewhere else
==

The tag =|@E|= (uppercase E) in an NSM input file turns on
_|dictionary making|_ modality, which enables processing a definition
header and body, and storing the resulting NSM formulas in
a lexical database.

After a @E tag, the parser accepts a record in _|SIL standard format|_.

The NSM-style definition must be the *last* entry in such a record, and
is introduced by the special tag =|\@|=.

If you don't need to store any additional information, the only 
SIL tag you will need is =|\lx|=, followed by the lexical entry.

You can also link the lexical entry to a dictionary entry in the NSM-DALIA
grammar; the tag =|\link|= is used for this task.

The previous example would be written like this :

==
@E 
\lx go
\link m(eng:e,"go",move(go)).
\@
someone (X) went =

before X was somewhere
X wanted to be somewhere else
because of this X moved for some time
because of this after this X was not in this place any more
X was somewhere else
==


==
@E 
\lx go
\link m(eng:e,"go",move(go)).
\ref Goddard 1998 p. 204
\@
someone (X) went =
before X was somewhere
X wanted to be somewhere else
because of this X moved for some time
because of this after this X was not in this place any more
X was somewhere else
==



@tbd What to do of dictionary entries once asserted.
*/	

%%	switch_makedict(+N:int) is det
%
%	Argument N is either 1 or 0, to turn respectively
%	on or off _|dictionary-making|_ modality.
switch_makedict(1) :-
	retractall(global_par:makedict(_)),
	asserta(global_par:makedict(yes)).
switch_makedict(0) :-	
	retractall(global_par:makedict(_)),
	asserta(global_par:makedict(no)).

%%	parse_dict_entry_header(+Record, -NextCharCode) is det
%
%	Parses the header of a duictionary entry. First, 
%	get_dict_entry/2 is invoked, which unifies Word with
%	the dictionary entry we are defining. 
%	
%	Then, get_comment_and_link/4 is called, which gets the optional 
%       comment and link of the entry. The Comment field will contain
%       a reference to the source from which the analysis is taken. The
%       Link contains the NSM-PROLOG representation of the
%       entry being defined.
%       
parse_dict_entry_header(["@sent":Sent|Record],C2) :-
	skipwhite(69,C),
	parse_SIL_record(C,Record,C1),
	get_intro_sent(C1,Sent,C2).


%%	assert_dict_variable(+Lang,+Var,+Value,+WhatIs) is det
%
%	Adds a variable to the language lexicon. Variables
%	encountered in the introductory line of an NSM dictionary
%	entry definition are so asserted before the parsing
%	of the body of the entry begins. In this way, these variables
%	are available in the body of the definition as normal
%	lexical items, stored as an m/4 predicate. 
%	
%	After the lexical entry has been parsed, variables are
%	removed from the lexicon (see retract_dict_info/1 and
%	retract_variables/1).
%	
assert_dict_variable(Lang,Var,Value,WhatIs) :-
	asserta(dict_var(Var,Value)),
	grammar:m(Lang,Cat,_,WhatIs),
	asserta(grammar:m(_,Cat,Var,Value)).


%%	assert_dict_entry(+Lang,+Record,+Paraphrase) is det
%
%	Asserts a dictionary entry after the NSM definition has been
%	parsed.
%	
assert_dict_entry(Lang,["@sent":Sent,"lx":Word|Record],Paraphrase) :-	
	nsmdalia:parse_sentence(Lang,Sent,IntroSent,_Rest),	
	findall(dict_var(Var,Value),
		dict_var(Var,Value),
		VarList),	
	find_molecule_level(Level),
	assertz(dict_entry(Lang,["lx":Word,
				 "@S":IntroSent,
				 "@M":Level, 
				 "@V":VarList,
				 "@P":Paraphrase 
				|Record])),
	retract_dict_info(VarList).

%%	assert_found_morph(+Class,+Shape,+Exponent) is det
%
%	This procedure is invoked by the morphological
%	parser (dg_morph:dg_morph_parse_word_aux/4) and 
%	morphophon:find_morph/7 only when _dictionary making_ 
%	mode is on. Asserts all the found morphemes for successive
%	molecule-level checking
assert_found_morph(group,[],_) :- !.
assert_found_morph(Class,Shape,Exponent) :-
	found_morph(Class,Shape,Exponent),
	!.
assert_found_morph(Class,Shape,Exponent) :-
	assertz(found_morph(Class,Shape,Exponent)).

%%	retract_dict_info(+List) is det
%
%	After the body of an entry of an NSM dictionary has been 
%	parsed, variables and parsing information are retracted.
%	
retract_dict_info(List):-
	retractall(found_morph(_,_,_)),
	retractall(dict_var(_,_)),
	retract_variables(List).

%%	retract_variables(+Variables:list) is det
%
%	Retracts the variables used in a dictionary entry after the
%	entry has been parsed.
%	
retract_variables([]).
retract_variables([dict_var(Var,Value)|List]) :-
	retract(grammar:m(_,name_lit,Var,Value)),
	retract_variables(List).


find_molecule_level(N) :-
	findall(Shape,
		found_morph(_,Shape,_),
		List),
	find_molecule_level_aux(0,M,List),
	!,
	N is M + 1.


find_molecule_level_aux(M,M,[]) :- !.
find_molecule_level_aux(M,M1,[Morph|List]) :-
	dict_entry(_Lang,Morph,Level,_Comment,_VarList,_Sent,_IntroSent,_Paraphrase),
	Level > M,
	!,
	find_molecule_level_aux(Level,M1,List).
find_molecule_level_aux(M,M1,[_Morph|List]) :-
	find_molecule_level_aux(M,M1,List).


/*
get_dict_entry(Word,C) :-
	get_code(C1),
	read_ident(Word,C1,C2),
	skip_whites(C2,C).

get_comment_and_link(C,Comment,Link,C2) :-
	get_comment_or_link(C,Comment,Link,C1),
	get_comment_or_link(C1,Comment,Link,C2),
	empty_string(Comment),
	empty_string(Link).
	


get_comment_or_link(64,_Comment,Link,C1) :-
	!,
	get_link(Link,C1).
get_comment_or_link(60,Comment,_Link,C1) :-
	!,
	get_comment(60,Comment,C1).
get_comment_or_link(C,[],[],C).

empty_string([]) :- !.
empty_string(_).



get_link(Link,C1) :-
	read(Link),
	skip_whites(10,C1).

get_comment(60,Comment,C1) :-
	!,
	get_code(C),
	get_comment(C,Comment,C1).
get_comment(62,[],C1) :-
	!,
	get_code(C),
	skip_whites(C,C1).
get_comment(C,[C|Comment],C1) :-
	get_code(C2),
	get_comment(C2,Comment,C1).
*/


get_intro_sent(61,[],C1) :- 
	!,
	get_code(C1).
get_intro_sent(end_of_file,[],end_of_file) :-
	!,
	notify_missing("=","dictionary definition header").
get_intro_sent(C,[C|Sent],C1) :-
	get_code(C2),
	get_intro_sent(C2,Sent,C1).


%%	parse_SIL_record(+CharCode,-Record,-NextCharCode) is det
%
%	Parses a record in _|SIL standard format|_.
%	
parse_SIL_record(92,[Slot:Filler|Record],C) :-
	get_code(C1),
	read_ident(Slot,C1,C2),
	parse_SIL_record_aux(Slot,Filler,Record,C2,C).
parse_SIL_record(C,[],C).

parse_SIL_record_aux("@",[],[],C,C1) :- 
	skipwhite(C,C1),!.
parse_SIL_record_aux(_Slot,Filler,Record,C2,C) :-
	skipwhite(C2,C3),
	get_SIL_field(nonewline,C3,Filler,C4),
	skipwhite(C4,C5),
	parse_SIL_record(C5,Record,C).


get_SIL_field(newline,92,[],92) :- !.
get_SIL_field(newline,C,[C|Text],C1) :- 
	!,
	get_code(C2),
	get_SIL_field(nonewline,C2,Text,C1).
get_SIL_field(_NewLine,10,[10|Text],C2) :-
	!,
	get_code(C1),
	get_SIL_field(newline,C1,Text,C2).
get_SIL_field(_NewLine,13,[13|Text],C2) :-
	!,
	get_code(C1),
	get_SIL_field(newline,C1,Text,C2).
get_SIL_field(NewLine,C,[C|Text],C2) :-
	get_code(C1),
	get_SIL_field(NewLine,C1,Text,C2).
