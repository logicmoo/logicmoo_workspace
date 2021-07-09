% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/


:- ensure_loaded(xgrun).	% XG runtimes

:- ensure_loaded(xgproc).
:- ensure_loaded(library(logicmoo_common)).

:- multifile( rel_spatial/3 ).
:- discontiguous( rel_spatial/3 ).
:- dynamic( rel_spatial/3 ).


:- multifile( value_units/2 ).
:- multifile( ti/2 ).
:- discontiguous( value_units/2 ).
:- discontiguous( ti/2 ).
:- dynamic( value_units/2 ).
:- dynamic( ti/2 ).



:- discontiguous( trans_spatial/5 ).
:- discontiguous( direct_spatial/5 ).
:- discontiguous( done_by_rel/5 ).
:- multifile( trans_spatial/5 ).
:- multifile( direct_spatial/5 ).
:- multifile( done_by_rel/5 ).
:- dynamic( trans_spatial/5 ).
:- dynamic( direct_spatial/5 ).
:- dynamic( done_by_rel/5 ).

%:- ensure_loaded(newg).		% clone + lex
:-  load_plus_xg_file('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/clone.xg').
:-  load_plus_xg_file('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/lex.xg').


% :- expects_dialect(pfc).
:-op(600,xfy,--).

:- include(countries).
:- include(world0).     	% data base
:- include(rivers).
:- include(cities).
:- include(contain).
:- include(borders).

%:- include(clotab).	% attachment tables
:- include(newdict).	% syntactic dictionary
:- include(slots).	% fits arguments into predicates
:- include(scopes).	% quantification and scoping

:- include(templa).	% semantic dictionary

:- use_module(qplan).	% query planning
:- include(talkr).	% query evaluation
:- include(ndtabl).	% relation info.
:- include(readin).	% sentence input
:- include(ptree).	% print trees
:- include(aggreg).	% aggregation operators

:- include(newtop).	% top level


:- fixup_exports.
