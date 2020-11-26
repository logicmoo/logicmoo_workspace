/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles

*/
:- module(logicmoo_mud, [ ]).


% ==============================================
% [Required] Load the Logicmoo User System
% ==============================================
:- ensure_loaded(library(logicmoo_lib)).

:- if( \+ exists_source(library(nomic_mu)) ;  \+ exists_source(library(marty_white/adv_main))).
:- add_pack_path(packs_sys).
:- if( \+ exists_source(library(nomic_mu)) ;  \+ exists_source(library(marty_white/adv_main))).
% :- pack_install(prologmud_I7).
:- endif.
:- endif.


:- if( \+ app_argv('--noworld')).

:- current_prolog_flag(access_level,WAS),!,
   set_prolog_flag(access_level,user),
   use_module(library(nomic_mu)),   
   set_prolog_flag(access_level,WAS).

:- runtime_boot(mu:srv_mu).

:- endif.


:- fixup_exports.



