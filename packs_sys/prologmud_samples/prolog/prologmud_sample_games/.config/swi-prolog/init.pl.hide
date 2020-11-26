:- use_module(library(prolog_autoload)).
:- use_module(library(prolog_pack)).
:- user:[library('theme/auto.pl')].
:- multifile(user:file_search_path/2).
:-   dynamic(user:file_search_path/2).


:- if( \+ exists_source(library(sldnfdraw))).
:- attach_packs('/opt/logicmoo_workspace/packs_lib').
:- endif.
:- if( \+ exists_source(library(pfc))).
:- attach_packs('/opt/logicmoo_workspace/packs_sys').
:- endif.
:- if( \+ exists_source(library(lps_syntax))).
:- attach_packs('/opt/logicmoo_workspace/packs_web').
:- endif.

:- if(false).
:- if(false).


% =====================
% Enable History
% =====================
:- if(\+ current_predicate(setup_hist0/0)).
:- if(exists_source(library(editline))).
%:- use_module(library(editline)).
:- else.
:- if(exists_source(library(readline))).
:- use_module(library(readline)).
:- endif.
:- endif.
setup_hist0:-  '$toplevel':setup_history.
:- setup_hist0.
:- endif.

% =====================
% Easier to trace while access_level system
% =====================
:- '$hide'('$toplevel':restore_debug).
:- '$hide'('$toplevel':save_debug).
:- '$hide'('$toplevel':residue_vars/2).
:- '$hide'('system':deterministic/1).
:- '$hide'(toplevel_call/2).
:- '$hide'('$toplevel':'$query_loop'/0).

% =====================
% System metapredicates
% =====================
:- meta_predicate '$syspreds':bit(2,?,?).
:- meta_predicate '$bags':findnsols_loop(*,*,0,*,*).
:- meta_predicate '$bags':findall_loop(*,0,*,*).
:- meta_predicate '$attvar':unfreeze(0).
:- meta_predicate '$attvar':run_crv(0,*,*,*).
:- meta_predicate '$expand':expand_term_list(4,*,*,*,*).
:- meta_predicate '$parms':cached_library_directory(*,0,*).
:- meta_predicate '$toplevel':residue_vars(0,-).
:- meta_predicate '$toplevel':toplevel_call(0).
:- meta_predicate '$toplevel':run_initialize(0,*).
% :- meta_predicate '$toplevel':run_init_goal(0,*).
% :- meta_predicate '$attvar':uhook(*,0,*,*).
% :- meta_predicate '$attvar':uhook(*,0,*).
:- meta_predicate '$toplevel':'$execute_goal2'(0,*).


% =====================
% Add Pack Directories
% =====================
:- use_module(library(prolog_pack)).

:- endif.


:- multifile(user:file_search_path/2).
:-   dynamic(user:file_search_path/2).
dir_from(Rel,Y):-
    ((getenv('LOGICMOO_WS',Dir);
     prolog_load_context(directory,Dir);
     '~/logicmoo_workspace'=Dir;
     '/opt/logicmoo_workspace/'=Dir)),
    absolute_file_name(Rel,Y,[relative_to(Dir),file_type(directory),file_errors(fail)]),
    exists_directory(Y),!.
add_pack_path(Rel):- 
   dir_from(Rel,Y),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).

:- if( \+ exists_source(library(logicmoo_common))).
:- add_pack_path(packs_sys).
:- endif.

:- add_pack_path(packs_usr).
:- add_pack_path(packs_web).
:- add_pack_path(packs_xtra).
:- add_pack_path(packs_lib).

attach_packs_n_utils:-
   attach_packs,
   use_module(library(logicmoo_common)).
:- initialization(attach_packs_n_utils,now).

:- pack_list_installed.

:- endif.



