
% :- use_module(library(logicmoo_arc)).
:- module(logicmoo_arc,[]).
:- use_module(library(logicmoo_common)).
:- set_prolog_flag(trill_term_expansion,false).
:- locally(set_prolog_flag(trill_term_expansion,false),ensure_loaded(library(kaggle_arc/arc_module))).



