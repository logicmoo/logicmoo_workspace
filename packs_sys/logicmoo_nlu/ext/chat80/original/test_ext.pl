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
|	This program may Be used, copied, altered or ensure_loadedd in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/


:- use_module(library(statistics)).

%:- autoload_all.
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_nlu)).
:- use_module(library(logicmoo_clif)).
%:- xlisting(lock_predicate/1).
%:- autoload_all.


:- module(parser_chat80).
:- '$set_source_module'(parser_chat80).

add_c80(B):- any_to_string(B,S),add_history1(c80(S)).

c80(B):- prolog_load_context(term,_), !,add_c80(B).
c80(Text):-
  cls, tracing ~= on,  
  any_to_string(Text,S),!,
  c81(S).

c81(S):- 
 mpred_test_mok(into_lexical_segs(S,U)),
 forall(deepen_pos(sentence80(E,U,[],[],[])),dmsg(E)),
 fail.
c81(S):- locally(set_prolog_flag(gc,false),time(process(debug,S))).


gp_africa(Result):-
  setOf(Size:City, []^(       
       database80(ti(city,City)),
       database80(trans_pred(spatial,contain,africa,City)) ,
       database80(count_pred(spatial,population,City,Size)),
       database80(exceeds(Size,_Other))), List),
   database80(aggregate80(max,List,Result)).

:- add_history1(ensure_loaded(geography/load_kb)).
:- add_history1(test_chat80).

:- forall(chat_80_ed(_,B,_),add_c80(B)).
:- c80("how many rivers are in poland ?").
:- c80("iran is bordered by iraq?").
:- c80("what city in africa has the greatest population?").
:- c80("do oceans border any country?").
:- c80("is the population of china greater than india's population?").
:- c80("what is the total area of nations that can border iraq?").
:- c80("what is the total area of nations that should border iraq?").
:- c80("what is the total area of nations that are bordered by iraq?").
:- c80("what ocean does not border any country ?").
:- c80("what oceans should border any country?").
:- c80("is china a country?").
:- fixup_exports.

:- if(\+ prolog_load_context(reloading, true)).
:- initialization(prolog).
:- endif.
