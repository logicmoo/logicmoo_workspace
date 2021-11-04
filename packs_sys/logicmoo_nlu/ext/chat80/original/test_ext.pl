% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or ensure_loadedd in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

%:- module(baseKB,[]).

:- use_module(library(statistics)).

%:- autoload_all.
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_nlu)).
:- if(\+ prolog_load_context(reloading, true)).
:- use_module(library(logicmoo_clif)).
:- endif.
%:- xlisting(lock_predicate/1).
%:- autoload_all.


:- module(parser_chat80).
:- '$set_source_module'(parser_chat80).

sample_set80([ does, object6, action5, object7, ? ]).
sample_set80([ is, object7, in, object6, ? ]).
sample_set80([ does, mexico, action1, the, united_states, ? ]).
sample_set80([ is, the, attrib1, of, object5, greater, than, value1, ? ]).
sample_set80([ does, the, attrib1, of, object5, exceed, value2, million, ? ]).
sample_set80([ is, the, attrib1, of, object5, value3, ? ]).
sample_set80([ does, the, attrib1, of, object5, exceed, the, attrib1, of, object4, ? ]).
sample_set80([ is, spain, action1ed, by, the, object8, ? ]).
sample_set80([ does, the, object3, action1, spain, ? ]).
sample_set80([ is, the, object9, in, object11swis, ? ]).
sample_set80([ is, the, object10, in, object2, ? ]).
sample_set80([ what, type3s, are, there, ? ]).
sample_set80([ does, object12, action1, object5, ? ]).
sample_set80([ what, is, the, property1, of, upper, volta, ? ]).
sample_set80([ where, is, the, largest, type1, ? ]).
sample_set80([ which, type1s, are, objectian2, ? ]).
sample_set80([ which, type1, '''', s, property1, is, london, ? ]).
sample_set80([ which, is, the, largest, african, type1, ? ]).
sample_set80([ how, large, is, the, smallest, object6n, type1, ? ]).
sample_set80([ what, is, the, type2, that, action1s, african, type1s, and, that, action1s, asian, type1s, ? ]).
sample_set80([ what, are, the, property1s, of, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ which, type1s, are, action1ed, by, two, type6s, ? ]).
sample_set80([ how, many, type1s, does, the, danube, flow, through, ? ]).
sample_set80([ how, many, type1s, does, the, object1, action3, through, ? ]).
sample_set80([ what, is, the, total, area, of, type1s, south, of, the, equator, and, not, in, australasia, ? ]).
sample_set80([ what, is, the, total, attrib1, of, type1s, south, of, the, object1, and, not, in, object2, ? ]).
sample_set80([ what, is, the, total, attrib1, of, type1s, abverb2, of, the, object1, and, not, in, object2, ? ]).
sample_set80([ what, is, the, total, area, of, type1s, south, of, the, object1, and, not, in, object2, ? ]).
sample_set80([ what, is, the, average, area, of, the, type1s, in, each, type5, ? ]).
sample_set80([ is, there, more, than, one, type1, in, each, type5, ? ]).
sample_set80([ is, there, some, type2, that, does, not, action1, any, type1, ? ]).	       
sample_set80([ what, are, the, type1s, from, which, a, type3, flows, into, the, object1, ? ]).
sample_set80([ what, are, the, type5s, no, type1, in, which, action5s, more, than, two, type4s, whose, attrib1, exceeds, (1), million, ? ]).
sample_set80([ which, type1, action1ing, the, object14, action1s, a, type1, that, is, action1ed, by, a, type1, whose, attrib1, exceeds, the, attrib1, of, object4, ? ]).
sample_set80([ which, type1s, have, an, attrib1, exceeding, (10), million, ? ]).
sample_set80([ which, type1s, with, a, attrib1, exceeding, (10), million, action1, the, object3, ? ]).
sample_set80([ what, percentage, of, type1s, action1, each, type2, ? ]).
sample_set80([ what, type1s, are, there, in, object2, ? ]).
sample_set80([ what, are, the, areas, of, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, flow, through, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, flow, through, each, type1, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, through, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, through, each, type1, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type3s, that, action5, each, type1, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, property1s, of, the, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, are, the, type4s, in, type1s, action1ing, the, object1, ? ]).
sample_set80([ what, type4s, do, the, type1s, action1ing, the, object1, action5, ? ]).
sample_set80("iraq action1s iran?").
sample_set80("iraq does action1 iran?").
sample_set80("iraq did action1 iran?").
%sample_set80("iraq will action1 iran?").
sample_set80("iraq is action1ing iran?").
sample_set80("iraq was action1ing iran?").
sample_set80("iran is action1ed by iraq?").
%sample_set80("iraq has action1 iran?").
%sample_set80("iraq has a action1 object5").
% X flows into Dest from Origin
sample_set80("the object9 flows into object15?").
sample_set80("the object9 flows to object15?").
sample_set80("the object9 flows in object15?").
sample_set80("the object9 flows from object15?").
sample_set80("the object9 flows at object15?").
sample_set80("the object9 flows through object15?").
sample_set80("the object9 flows into object11swis?").
sample_set80("the object9 flows to object11swis?").
sample_set80("the object9 flows in object11swis?").
sample_set80("the object9 flows from object11swis?").
sample_set80("the object9 flows at object11swis?").
sample_set80("the object9 flows through object11swis?").


chat80_test("does america contain new york ?").
chat80_test("is new york in america ?").
chat80_test("does mexico border the united states ?").
chat80_test("the population of china greater than 200 million ?").
chat80_test("the population of china exceed 1000 million ?").
chat80_test("the population of china 840 million ?").
chat80_test("does the population of china exceed the population of india ?").
chat80_test("is spain bordered by the pacific ?").
chat80_test("does the atlantic border spain ?").
chat80_test("is the rhine in switzerland ?").
chat80_test("is the united kingdom in europe ?").
chat80_test("what rivers are there ?").
chat80_test("does afghanistan border china ?").
chat80_test("what is the capital of upper volta ?").
chat80_test("where is the largest country ?").
chat80_test("which countries are european ?").
chat80_test("which country ' s capital is london ?").
chat80_test("which is the largest african country ?").
chat80_test("how large is the smallest american country ?").
chat80_test("what is the ocean that borders african countries and that borders asian countries ?").
chat80_test("what are the capitals of the countries bordering the baltic ?").
chat80_test("which countries are bordered by two seas ?").
chat80_test("how many countries does the danube flow through ?").
chat80_test("what is the total area of countries south of the equator and not in australasia ?").
chat80_test("what is the average area of the countries in each continent ?").
chat80_test("is there more than one country in each continent ?").
chat80_test("is there some ocean that does not border any country ?").
chat80_test("what are the countries from which a river flows into the black sea ?").
chat80_test("are the continents no country in which contains more than two cities whose population exceeds 1 million ?").
chat80_test("which country bordering the mediterranean borders a country that is bordered by a country whose population exceeds the population of india ?").
chat80_test("countries have a population exceeding 10 million ?").
chat80_test("countries with a population exceeding 10 million border the atlantic ?").
chat80_test("what percentage of countries border each ocean ?").
chat80_test("what countries are there in europe ?").
chat80_test("what are the areas of the countries bordering the baltic ?").
chat80_test("iraq borders iran?").
chat80_test("iraq does border iran?").
chat80_test("iraq did border iran?").
chat80_test("iraq will border iran?").
chat80_test("iraq is bordering iran?").
chat80_test("iraq was bordering iran?").
chat80_test("iran is bordered by iraq?").

% text_drs_eval(Evaluation, Id, Text, DRS, LHSs, Timestamp, Author, Comment).
:- ensure_loaded(lang_model).


%:- autoload_all.
:- use_module(library(logicmoo_common)).
%:- xlisting(lock_predicate/1).
%:- autoload_all.


%:- module(baseKB).

%:- include(load).


:- fixup_exports.

