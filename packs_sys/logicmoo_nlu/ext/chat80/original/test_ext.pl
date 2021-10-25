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


:- use_module(library(statistics)).

%:- autoload_all.
:- use_module(library(logicmoo_common)).
%:- xlisting(lock_predicate/1).
%:- autoload_all.


:- module(parser_chat80).
:- '$set_source_module'(parser_chat80).

process80(Text):- 
  cls, tracing ~= on,  
  any_to_string(Text,S),!,
  time(process(debug,S)).

add_process80(B):- any_to_string(B,S),add_history(process80(S)).

:- forall(chat_80_ed(_,B,_),add_process80(B)).
:- add_process80("how many rivers are in poland ?").
:- add_process80("iran is bordered by iraq?").
:- add_process80("what city in africa has the greatest population?").
   

:- add_process80("what is the total area of nations that are bordered by iraq?").
:- add_history(ensure_loaded(geography/load_kb)).

:- add_history(test_chat80).
:- fixup_exports.
