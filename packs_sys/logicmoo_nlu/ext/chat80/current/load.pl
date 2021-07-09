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



:-  ensure_loaded(xgproc).	% XG generator




:-  context_module(CM), chat80_impl_dir(Where), load_plus_xg_file(CM, logicmoo_nlu_ext(Where/'clone.xg')).
:-  context_module(CM), chat80_impl_dir(Where), load_plus_xg_file(CM, logicmoo_nlu_ext(Where/'lex.xg')).

:- compile_xg_clauses.
% :- xg_listing('newg.pl').

% :- decl_mpred_hybrid(person/1).

% :- list('newg.pl').
:-  ensure_loaded(xgrun).	% XG runtimes
  % ensure_loaded((newg)).		% clone + lex

:- ensure_loaded(clotab).	% attachment tables

:- if(false).
:- include(newdict).	% syntactic dictionary
:- else.
:-  ensure_loaded(newdict).	% syntactic dictionary
:- endif.


%  ensure_loaded(logicmoo_nlu_ext(/newdict_regress)).	e syntactic dictionary

:-ensure_loaded(slots).	% fits arguments into predicates
:-ensure_loaded(scopes).	% quantification and scoping
%:-ensure_loaded((templa)).	% semantic dictionary
:-ensure_loaded(qplan).	% query planning
:-ensure_loaded(talkr).	% query evaluation
%:-ensure_loaded((ndtabl)).	% relation info.
:-use_module(readin).	% sentence80 input
:-ensure_loaded(ptree).	% print trees
:-ensure_loaded(aggreg).	% aggregation operators
:-ensure_loaded(world0).     	% data base
%:-ensure_loaded((world0)).     	% data base
% testing
:-ensure_loaded(newtop). 	% top level

/*
:- ensure_loaded((rivers)).
:- ensure_loaded((cities)).
:- ensure_loaded((countries)).
:- ensure_loaded((contain)).
:- ensure_loaded((borders)).
*/

