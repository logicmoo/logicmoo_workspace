/*
< module > Adds autoloading of LogicMOO Utilities predicates
% ===================================================================
    File:         'logicmoo_utils).'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_utils).' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   TeamSPoon
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.prologmoo.com
    SCM:           https://github.com/TeamSPoon/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

:- if( \+ exists_source(library(logicmoo_utils))).

:- if((prolog_load_context(directory,Dir),
   multifile(user:file_search_path/2),
   dynamic(user:file_search_path/2),
   (( \+ user:file_search_path(library,Dir)) ->asserta(user:file_search_path(library,Dir));true))).
:- endif.
:- endif.
:- if((set_prolog_flag(logicmoo_utils_file,(exists_source(library(subclause_expansion)),
       reexport(library(subclause_expansion)))))).
:- endif.
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(logicmoo_utils_file,[logicmoo_utils_test_msg/0]).
:- endif.

pack_ensure(_).

packs_ensure :- maplist([W]>>pack_ensure(W),
[
dictoo,
eggdrop,
gvar_syntax,
instant_prolog_docs,
logicmoo_base,
logicmoo_utils,
predicate_streams,
prologmud,
s_expression,
slack_prolog,
tabling_dra]).


% ======================================================
% Add Extra pack-ages directory
% ======================================================
:- if( \+ exists_source(library(logicmoo_utils))).
% :- initialization(attach_packs,now).
:- if( \+ exists_source(pack(logicmoo_base/prolog/logicmoo/logicmoo_utils))).
:- add_file_search_path_safe(pack,'../../').
% :- initialization(attach_packs,now).
:- endif.
:- endif.
% ======================================================
% Save a directory of *this* file into logicmoo(..)
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- if( \+ exists_source(library(logicmoo_user))).
:- add_file_search_path_safe(library,'./').
:- sanity(exists_source(library(logicmoo_user))).
:- endif.



:- if( \+ current_predicate(system:each_call_cleanup/3)).
%:- reexport(library('supp')).
:- endif.

% ======================================================
% Included separated logicmoo util files
% ======================================================

:- set_prolog_flag(system:generate_debug_info, true).
:- set_prolog_flag(generate_debug_info, true).
 % :- set_prolog_flag(subclause_expansion,false).

:- system:reexport(library(logicmoo_common)).
:- reexport(library(logicmoo_utils_all)).
:- reexport(library('predicate_streams')).
%%:- reexport(library('engines')).
%%:- reexport(library('help')).

% :- baseKB:reexport(library(logicmoo_swilib)).

:- forall((current_module(M),M\==baseKB),assert_if_new(baseKB:mtProlog(M))).


% ======================================================
% Add Utils files to autoloads
% ======================================================
:- add_library_search_path('./logicmoo/util/',[ 'logicmoo_util_*']).

% ======================================================
% Pre-release Sanity tests
% ======================================================
:- dynamic(baseKB:logicmoo_scan_autoloads/0).
:- dynamic(baseKB:logicmoo_pre_release/0).


%  logicmoo_pre_release() is semidet.
%
% Hook To [baseKB:logicmoo_pre_release/0] For Module Logicmoo_utils.
% Logicmoo Pre Release.
%
baseKB:logicmoo_pre_release.

:- if(baseKB:logicmoo_pre_release).
/*
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(false), max_depth(1000), attributes(ignore),spacing(next_argument)]).
:- set_prolog_flag(verbose_autoload, true).
:- set_prolog_flag(debug, true).
:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(backtrace_show_lines, true).
:- set_prolog_flag(debugger_show_context,true).

:- if(current_prolog_flag(gui,true)).
:- guitracer.
%:- notrace(trace).
:- notrace.
:- endif.

*/


% baseKB:logicmoo_scan_autoloads() is semidet.
%
% Hook To [baseKB:logicmoo_scan_autoloads/0] For Module Logicmoo_utils.
% Logicmoo Scan Autoloads.
%
baseKB:logicmoo_scan_autoloads:-false.

:- endif.
% ======================================================
% Pre-release Should check if autoloading messes up anything
% ======================================================

:- if(baseKB:logicmoo_scan_autoloads).
%:- set_prolog_flag(verbose_autoload, false).
%:- autoload([verbose(false)]).
%:- set_prolog_flag(verbose_autoload, true).
:- endif.

% ======================================================
% Create one big logicmoo_utils module
% ======================================================
:- multifile((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
:- dynamic((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
%user:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).


:- thread_local logicmoo_utils_test_tl/0.
logicmoo_utils_test_tl:- throw(writeln("BADDDDDDD! locally/2 did not redefine this")).
logicmoo_utils_test_msg:- locally((
 logicmoo_utils_test_tl:- writeln("% locally worked! ")),logicmoo_utils_test_tl).
:- export(logicmoo_utils_test_msg/0).
/*
% the next are loaded idomaticaly later (if needed)
% %:- reexport(library('clause_expansion')).
% %:- reexport(library('attvar_reader')).
% %:- reexport(library('ctx_frame')).
% %:- reexport(library('dra')).
% %:- reexport(library('bb_gvar')).
% %:- reexport(library('bb_env')).
% %:- reexport(library('dcg')).
% %:- reexport(library('varfunctors')).
% %:- reexport(library('structs')).
% %:- reexport(library('supp')).
*/
/*
:- listing(locally/2).
*/
% :- rtrace.
% :- logicmoo_utils_test_msg.
 % :- set_prolog_flag(subclause_expansion,true).
%:- set_how_virtualize_file(bodies).
% .


