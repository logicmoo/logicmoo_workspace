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
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/
%:- '$set_source_module'(baseKB).
:- trace_or_throw('$set_typein_module'(baseKB)).

:- ensure_loaded(tlxgproc).	% XG generator


:- load_plus_xg_file('clone.xg').
:- load_plus_xg_file('lex.xg').
:- compile_xg_clauses.
% :- list('newg.pl').
:- include(xgrun).	% XG runtimes
% :- include(newg).		% clone + lex
:- include(clotab).	% attachment tables
:- include(newdict).	% syntactic dictionary
:- include(slots).	% fits arguments into predicates
:- include(scopes).	% quantification and scoping
:- include(templa).	% semantic dictionary
:- include(qplan).	% query planning
:- include(talkr).	% query evaluation
:- include(ndtabl).	% relation info.
:- include(readin).	% sentence80 input
:- include(ptree).	% print trees
:- include(aggreg).	% aggregation operators
:- include(world0).     	% data base
:- include(rivers).
:- include(cities).
:- include(countries).
:- include(contain).
:- include(borders).
:- include(newtop).	% top level




bad_chat80 :-
  told,
  told,
   repeat,
   prompt(_,'Question: '),
   trace,readin80:read_sent(P),
      control80(report,P),
      end80(user).

