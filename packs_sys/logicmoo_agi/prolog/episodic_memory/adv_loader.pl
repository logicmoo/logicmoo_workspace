/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/

:- use_module(library(logicmoo_common)).
:- '$set_source_module'(mu).
% :- ensure_loaded(adv_loader).


% :- user:ensure_loaded(library(parser_sharing)).


:- ensure_loaded(adv_debug).
:- ensure_loaded(adv_help).
:- ensure_loaded(adv_util).
:- ensure_loaded(adv_io).

:- ensure_loaded(adv_model).
:- ensure_loaded(adv_percept).

:- ensure_loaded(adv_inst).
:- ensure_loaded(adv_edit).

:- ensure_loaded(adv_behaviour_tree).

%:- ensure_loaded(adv_axiom).
:- ensure_loaded(adv_implies).

%:- ensure_loaded(adv_abdemo).

:- ensure_loaded(adv_examine).
:- ensure_loaded(adv_action).
:- ensure_loaded(adv_agent).
:- ensure_loaded(adv_floyd).
:- ensure_loaded(adv_physics).
:- ensure_loaded(adv_plan).

:- ensure_loaded(adv_functors).
:- ensure_loaded(adv_eng2txt).
:- ensure_loaded(adv_log2eng).
:- ensure_loaded(adv_eng2cmd).

%:- ensure_loaded(adv_lexicon).

:- ensure_loaded(adv_quasiquote).

:- ensure_loaded(adv_state).

:- ensure_loaded(adv_portray).
:- ensure_loaded(adv_data).

:- ensure_loaded(adv_plugins).
