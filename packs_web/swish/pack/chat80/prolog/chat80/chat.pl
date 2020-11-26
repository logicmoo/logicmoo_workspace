/* @(#)chat.pl	24.1 2/23/88 */

/*
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/

% This file compiles all of Chat-80

/* SWI-Prolog modifications:

   - include library Quintus for enhanced compatibility
   - put discontiguous between brackets
   - rename plus/3 and index/1 to be my_plus; my_index
   - remove last/2: system predicate with equivalent definition.
*/

:- use_module(library(quintus), [no_style_check/1]).
:- op(1150, fx, [(mode), (public)]).

:- no_style_check(single_var).
:- no_style_check((discontiguous)).

:- consult(chatops).

:- consult(readin).		% sentence input, ASCII VERSION
:- consult(ptree).		% print trees
:- consult(xgrun).		% XG runtimes
:- consult(newg).		% clone + lex
:- consult(clotab).		% attachment tables
:- consult(newdic).		% syntactic dictionary
:- consult(slots).		% fits arguments into predicates
:- consult(scopes).		% quantification and scoping
:- consult(templa).		% semantic dictionary
:- consult(qplan).		% query planning
:- consult(talkr).		% query evaluation
:- consult(ndtabl).		% relation info.
:- consult(aggreg).		% aggregation operators
:- consult(world0).		% geographic data base
:- consult(rivers).
:- consult(cities).
:- consult(countr).
:- consult(contai).
:- consult(border).
:- consult(chattop).		% top level control


save_chat :-
   qsave_program(chat, [goal(hi)]).

