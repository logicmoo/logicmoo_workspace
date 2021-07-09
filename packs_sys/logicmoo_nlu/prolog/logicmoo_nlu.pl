#!/usr/bin/env swipl

:- module(logicmoo_nlu,[]).

% ==============================================
% [Required] Load the Logicmoo User System
% ==============================================
:- ensure_loaded(library(logicmoo_lib)).

/*
:- if( \+ exists_source(library('logicmoo_nlu/nl_pipeline.pl'))).
:- add_pack_path(packs_xtra).
:- endif.


:- if( \+ exists_source(library('logicmoo_nlu/parser_sharing.pl'))).
:- add_pack_path(packs_sys).
:- endif.
*/

:- use_module(library(logicmoo_nlu/nl_pipeline)).
:- use_module(library(logicmoo_nlu/parser_sharing)).
:- use_module(library(logicmoo_nlu/parser_pldata)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).
:- use_module(library(logicmoo_nlu/parser_chat80)).
:- use_module(library(logicmoo_nlu/parser_e2c)).

:- current_prolog_flag(access_level,WAS),!,
   set_prolog_flag(access_level,user),
   reexport(library(logicmoo_nlu/nl_pipeline)),
   set_prolog_flag(access_level,WAS).

%:- break.

/*
% :- system:ensure_loaded(pack(logicmoo_nlu/prolog/logicmoo_nlu/parser_chat80)).
%:- system:ensure_loaded(pack(logicmoo_nlu/prolog/logicmoo_nlu/parser_pldata)).

:- use_module(library(logicmoo_nlu/parser_sharing)).
:- use_module(library(logicmoo_nlu/parser_tokenize)).

:- if(exists_source(library(logicmoo_nlu))).
% 
:- use_module(library(logicmoo_nlu)).
%:- use_module(library(logicmoo_nlu/parser_tokenize)).
%:- use_module(library(logicmoo_nlu/nl_pipeline)).
:- else.
% :- system:ensure_loaded(pack(logicmoo_nlu/prolog/logicmoo_nlu/parser_sharing)).
:- if(exists_source(pack(logicmoo_nlu/ext/pldata/nl_iface))).
:- ensure_loaded(pack(logicmoo_nlu/ext/pldata/nl_iface)).
:- ensure_loaded(library(nldata/nl_iface)).
:- else.
:- if(exists_source(library(nldata/nl_iface))).
% being in user is just to help debugging from console
%:- user:ensure_loaded(library(nldata/nl_iface)).
:- endif.
:- endif.
:- load_wordnet.
:- endif.

*/


%:- break.

