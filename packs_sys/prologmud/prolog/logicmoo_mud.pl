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

%:- if( \+ exists_source(library(prologmud/mud_startup))).
%:- add_pack_path(packs_sys).
%:- endif.

set_prologmud_home_dir:- exists_source(prologmud(mud_loader)),!.
set_prologmud_home_dir:- 
   must((absolute_file_name(library('prologmud/'),Dir,[file_type(directory),access(read)]),
   nonvar(Dir), asserta(user:file_search_path(prologmud,Dir)))).

:- if(\+ exists_source(prologmud(mud_loader))).
:- set_prologmud_home_dir,!.
:- assertion(exists_source(prologmud(mud_loader))).
:- endif.

% start_runtime_mud:-!.
% start_runtime_mud:- baseKB:lst, listing(mudAtLoc).

% ensure_mud_startup:- !.
ensure_mud_startup:- 
   set_prologmud_home_dir,
   baseKB:ensure_loaded(prologmud(mud_loader)),
   current_prolog_flag(access_level,WAS),!,
   set_prolog_flag(access_level,user),
   baseKB:ensure_loaded(prologmud(mud_startup)),
   set_prolog_flag(access_level,WAS).

:- fixup_exports.


:- if( \+ app_argv('--nomud')).
:- ensure_mud_startup.
:- endif.



