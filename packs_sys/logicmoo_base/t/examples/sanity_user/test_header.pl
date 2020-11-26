

:- if(current_prolog_flag(test_header,_)).

:- wdmsg(reload_of_test_header).

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

:- if(exists_source(library(editline))).
:- use_module(library(editline)).
:- else.
:- if(exists_source(library(editline))).
:- use_module(library(readline)).
:- endif.
:- endif.
:-  '$toplevel':setup_history.



%:- set_stream(user_input,tty(false)).


:- use_module(library(pfc_lib)).
%:- if(( \+ current_module(pfc_lib) )).
:- use_module(library(pfc)).
%:- endif.


:- ensure_loaded(library(pfc_test)).

%:- endif. % current_prolog_flag(test_header,_).


:- prolog_load_context(source,File),!,
   ignore((((atom_contains(File,'.pfc')-> sanity(pfc_lib:is_pfc_file) ; sanity( \+ pfc_lib:is_pfc_file))))),!.

:- mpred_trace_exec.


:- set_prolog_flag(debug, true).
:- set_prolog_flag(gc, false).

:- '$current_source_module'(W), '$set_typein_module'(W).

:- sanity(
 ((fileAssertMt(Mt2),
(defaultAssertMt(Mt1),
    %fileAssertMt(Mt2),
   source_module(Mt3))),
  sanity((Mt1==Mt2,Mt1==Mt3)))).






