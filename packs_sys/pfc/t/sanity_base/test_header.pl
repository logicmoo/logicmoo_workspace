
:- if(\+ current_prolog_flag(pfc_version,2.0)).

:- include(test_header_1_8).

:- else.

:- if(current_prolog_flag(test_header,_)).

:- wdmsg(reload_of_test_header).

:- use_module(library(pfc)).

:- mpred_reset.

:- else.


% runtype: default = pfc
:- if(current_prolog_flag(runtime_testing_module,_)->true;
  set_prolog_flag(runtime_testing_module,test_header)).
:- endif.



:- if(( \+ current_prolog_flag(test_header,_),set_prolog_flag(test_header,loaded))).


:- if((prolog_load_context(module,user), \+ current_module(pfc_lib))).
:- module(header_sane,[test_header_include/0]).
test_header_include.
:- endif.

%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 0). % 1 = default
:- set_prolog_flag(runtime_debug, 3). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(logicmoo_message_hook,dumpst).

:- endif.

:- if(\+ exists_source(library(logicmoo_utils_all))).
:-  prolog_load_context(directory,X),absolute_file_name('../../..',O,[relative_to(X),file_type(directory)]),
    attach_packs(O).
:- endif.

:- if(\+ exists_source(library(pfc_lib))).
:-  prolog_load_context(directory,X),absolute_file_name('../../prolog',O,[relative_to(X),file_type(directory)]),
    asserta(user:file_search_path(library,O)).
:- endif.



:- if(exists_source(library(editline))).
:- use_module(library(editline)).
:- else.
:- if(exists_source(library(readerline))).
:- use_module(library(readline)).
:- endif.
:- endif.

:-  '$toplevel':setup_history.



%:- set_stream(user_input,tty(false)).


%:- endif. % current_prolog_flag(test_header,_).

:- ensure_loaded(library(pfc)).

:- prolog_load_context(source,File),!,
   ignore((((atom_contains(File,'.pfc')-> sanity(is_pfc_file) ; sanity( \+ is_pfc_file))))),!.

%:- '$current_source_module'(W), '$set_typein_module'(W).

:- mpred_trace_exec.


%:- set_prolog_flag(debug, true).
%:- set_prolog_flag(gc, false).

/*
:- must(
 ((fileAssertMt(Mt2),
(defaultAssertMt(Mt1),
    %fileAssertMt(Mt2),
   source_module(Mt3))),
  sanity((Mt1==Mt2,Mt1==Mt3)))).


*/

:- ensure_loaded(library(pfc_test)).

:- endif.
