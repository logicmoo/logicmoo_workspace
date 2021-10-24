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

multifile_dynamic_discontiguous(P):- multifile(P),dynamic(P),discontiguous(P).


:- multifile_dynamic_discontiguous(chat80/2).
:- multifile_dynamic_discontiguous(clex_verb80/4).
:- multifile_dynamic_discontiguous(count_pred/4).
:- multifile_dynamic_discontiguous(measure_pred/4).
:- multifile_dynamic_discontiguous(position_pred/4).
:- multifile_dynamic_discontiguous(talkdb_talk_db/6).
:- multifile_dynamic_discontiguous(ti_subclass/2).
:- multifile_dynamic_discontiguous(type_conversion/2).
:- multifile_dynamic_discontiguous(verb_form_db/5).
:- multifile_dynamic_discontiguous(verb_type_db/3).
:- multifile_dynamic_discontiguous(name_template_lf0/2).

:- multifile_dynamic_discontiguous(adjunction_LF/4).
:- multifile_dynamic_discontiguous(intrans_LF/6).
:- multifile_dynamic_discontiguous(thing_LF/6).
:- multifile_dynamic_discontiguous(thing_LF_access/6).


% :- expects_dialect(pfc).
:-op(600,xfy,--).

:- ensure_loaded(templa).	% semantic dictionary


%:- ensure_loaded(newg).		% clone + lex
:-  load_plus_xg_file('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/clone.xg').
:-  load_plus_xg_file('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/lex.xg').


:- ensure_loaded(geography/load_kb).
:- ensure_loaded(world0).     	% data base
:- ensure_loaded(rivers).
:- ensure_loaded(cities).
:- ensure_loaded(contain).
:- ensure_loaded(borders).

%:- ensure_loaded(clotab).	% attachment tables
:- ensure_loaded(newdict).	% syntactic dictionary
:- ensure_loaded(slots).	% fits arguments into predicates
:- ensure_loaded(scopes).	% quantification and scoping


:- use_module(qplan).	% query planning
:- ensure_loaded(talkr).	% query evaluation
:- ensure_loaded(ndtabl).	% relation info.
:- ensure_loaded(readin).	% sentence input
:- ensure_loaded(ptree).	% print trees
:- ensure_loaded(aggreg).	% aggregation operators

:- ensure_loaded(newtop).	% top level

:- fixup_exports.

%:- break.
