
% doing these inside of if/endifs so they dont appear before module/2
:- if((prolog_load_context(source,S),format(user_error,'~N~q,~n',[running(S)]))). :- endif.
:- if(( \+ current_prolog_flag(test_module,_),set_prolog_flag(test_module,baseKB),assert(baseKB:this_is_baseKB))). :- endif.

:- if(set_prolog_flag(pfc_version,v(2,0,0))). :- endif.


:- if(current_prolog_flag(loaded_test_header,_)). 

:- wdmsg(reload_of_test_header).
:- mpred_reset.
% runtype: default = pfc
:- else.
:- if(( \+ current_prolog_flag(loaded_test_header,_),set_prolog_flag(loaded_test_header,loaded))).  :- endif.
:- if(( \+ current_prolog_flag(runtime_testing_module,_), set_prolog_flag(runtime_testing_module,test_header))). :- endif.

:- if((prolog_load_context(module,user), \+ current_prolog_flag(runtime_testing_module,user))).
% writes a temp header file and include/1s it
:- if(( tmp_file(swi, Dir), make_directory(Dir),working_directory(OLD,Dir),asserta(t_l:old_pwd(OLD,Dir)),current_prolog_flag(test_module,Module),open('module_header.pl',write,OS),
  format(OS,'\n:- module(~q,[test_header_include/0]).\n test_header_include. ',[Module]),close(OS))). :- endif.
:- include('module_header.pl').
:- retract(t_l:old_pwd(OLD,Delete)),working_directory(_,OLD),delete_directory_and_contents(Delete).
:- endif.

:- if(( \+ exists_source(library(logicmoo_utils_all)), 
   prolog_load_context(directory,X),absolute_file_name('../../..',O,[relative_to(X),file_type(directory)]), attach_packs(O))).  
:- endif.
:- if(use_module(library(logicmoo_utils))). :-endif.
:- if(( \+ exists_source(library(pfc_lib)), 
   prolog_load_context(directory,X),absolute_file_name('../../prolog',O,[relative_to(X),file_type(directory)]),asserta(user:file_search_path(library,O)))).
:- endif.
% sets up history for interactive testing
:- if((ignore(exists_source(library(editline))->use_module(library(editline))
       ;(exists_source(library(readerline)),use_module(library(readline)))),
   '$toplevel':setup_history)). :- endif.

%:- mpred_trace_exec.
:- prolog_load_context(source,File),!,
   ignore((((sub_atom(File,_,_,_,'.pfc')
   -> (set_prolog_flag(is_pfc_file_dialect,true))
   ; set_prolog_flag(is_pfc_file_dialect,false))))),!.

:- current_prolog_flag(is_pfc_file_dialect,true)-> sanity(is_pfc_file) ; true.
%   ; current_prolog_flag(is_pfc_file_dialect,false)-> sanity( \+ is_pfc_file) ; true.
%:- set_prolog_flag(debug, true).
%:- set_prolog_flag(gc, false).
%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 0). % 1 = default
:- set_prolog_flag(runtime_debug, 3). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(logicmoo_message_hook,dumpst).
%:- use_module(library(junit)).
%:- ensure_loaded(library(pfc)).
%:- abolish(j_u:junit_prop/3).
%:- dynamic(j_u:junit_prop/3).
%:- ensure_loaded(library(pfc_lib)).
:- ensure_loaded(library(pfc_test)).
:- prolog_load_context(source,SF),add_test_info(testsuite,file,SF).

/*
:- must(
 ((fileAssertMt(Mt2),
(defaultAssertMt(Mt1),
    %fileAssertMt(Mt2),
   source_module(Mt3))),
  sanity((Mt1==Mt2,Mt1==Mt3)))).


*/
% system:term_expansion( (begin_of_file), [] ):- current_prolog_flag(is_pfc_file_dialect,true).
system:term_expansion(I,P,O,PO):- junit_term_expansion(I,O),P=PO.
system:goal_expansion(I,P,O,PO):- junit_goal_expansion(I,O),P=PO.

:- endif.

:- set_prolog_flag(color_term, true).
:- set_stream(current_output,tty(true)),set_output(user_output),set_stream(current_output,tty(true)).

%:- '$current_source_module'(W), '$set_typein_module'(W).
:- if((current_prolog_flag(test_module,Module)->module(Module);true)). :- endif.
:- if((current_prolog_flag(is_pfc_file_dialect,true),ensure_loaded(library(pfc_lib)),mpred_trace_exec)). :- endif.
