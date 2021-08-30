:- module(logicmoo_cycqlf,[]).

% :- unload_file(logicmoo_cycqlf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PACK LOADER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(logicmoo_packs)).
:- reexport(library('logicmoo/common_logic/common_logic_utils.pl')).

/*

:- use_module(library(prolog_pack)).
:- if( \+ prolog_pack:current_pack(logicmoo_base)).
:- multifile(user:file_search_path/2).
:-   dynamic(user:file_search_path/2).
:- prolog_load_context(directory,Dir),
   absolute_file_name('../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- initialization(attach_packs,now).
:- pack_list_installed.
:- endif.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD CYC KB LOADER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- if( \+ app_argv('--nokb'))

:- ensure_loaded(library('pldata/plkb7166/kb7166')).
:- set_prolog_flag(verbose_load,full).
:- kb7166:kb7166_qcompile.
%:- break.

:- if( app_argv1('--kb') ).
:- kb7166:kb7166_qconsult.
:- endif.

:- set_prolog_flag(verbose_load,normal).
qsave_kb7166:- qsave_program('kb7166.prc',[class(runtime)]).

:- endif.

