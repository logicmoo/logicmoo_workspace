
% doing these inside of if/endifs so they dont appear before module/2
:- if((prolog_load_context(source,S),format(user_error,'~N~q,~n',[running(S)]))).
:- endif.

:- if(set_prolog_flag(pfc_version,v(2,0,0))).
:- endif.

:- if(\+ current_prolog_flag(pfc_version,v(2,_,_))).
:- include(test_header_1_8).
:- else.

:- if(current_prolog_flag(test_header,_)).
:- wdmsg(reload_of_test_header).
:- mpred_reset.
% runtype: default = pfc
:- else.

:- if(current_prolog_flag(runtime_testing_module,_)->true;
  set_prolog_flag(runtime_testing_module,test_header)).
:- endif.


:- if(( \+ current_prolog_flag(test_module,_),set_prolog_flag(test_module,baseKB))). :- endif.


:- if(( \+ current_prolog_flag(test_header,_),set_prolog_flag(test_header,loaded))).


:- if((prolog_load_context(module,user), \+ current_module(pfc_lib))).
% writes a temp header file and include/1s it
:- if((current_prolog_flag(test_module,Module),open('/tmp/logicmoo_testing/module_header.pl',write,OS),
  format(OS,'\n:- module(~q,[test_header_include/0]).\n test_header_include. ',[Module]),close(OS))). :- endif.
:- include('/tmp/logicmoo_testing/module_header.pl').
:- else.
:- current_prolog_flag(test_module,Module),module(Module).
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

:- use_module(library(logicmoo_utils)).

:- if(\+ exists_source(library(pfc_lib))).
:-  prolog_load_context(directory,X),absolute_file_name('../../prolog',O,[relative_to(X),file_type(directory)]),
    asserta(user:file_search_path(library,O)).
:- endif.

% sets up history for interactive testing
:- if(exists_source(library(editline))).
:- use_module(library(editline)).
:- else.
:- if(exists_source(library(readerline))).
:- use_module(library(readline)).
:- endif.
:- endif.
:-  '$toplevel':setup_history.

/*
:- ensure_loaded(library(pfc_lib)).

:- prolog_load_context(source,File),!,
   ignore((((sub_atom(File,_,_,_,'.pfc')
   -> (sanity(is_pfc_file),set_prolog_flag(is_pfc_file_dialect,true))
   ; sanity( \+ is_pfc_file)),set_prolog_flag(is_pfc_file_dialect,false)))),!.

*/

:- ensure_loaded(library(pfc_test)).
:- use_module(library(logicmoo_clif)).

:- dmsg(this_test_might_need(:- use_module(library(logicmoo_plarkc)))).


:- op(1199,fx,('==>')).
:- op(1190,xfx,('::::')).
:- op(1180,xfx,('==>')).
:- op(1170,xfx,'<==>').
:- op(1160,xfx,('<-')).
:- op(1150,xfx,'=>').
:- op(1140,xfx,'<=').
:- op(1130,xfx,'<=>').
%:- op(1100,fx,('nesc')).
:- op(300,fx,'-').
:- op(300,fx,'~').
:- op(600,yfx,'&').
:- op(600,yfx,'v').
:- op(1075,xfx,'<-').
:- op(350,xfx,'xor').

%:- cls.

   

%:- '$current_source_module'(W), '$set_typein_module'(W).

%:- mpred_trace_exec.



/*
:- must(
 ((fileAssertMt(Mt2),
(defaultAssertMt(Mt1),
    %fileAssertMt(Mt2),
   source_module(Mt3))),
  sanity((Mt1==Mt2,Mt1==Mt3)))).


*/
:- abolish(j_u:junit_prop/2).
:- dynamic(j_u:junit_prop/2).

testing_complete:- j_u:junit_prop(shown_testing_complete,true),!.
testing_complete:- asserta(j_u:junit_prop(shown_testing_complete,true)),
  listing(j_u:junit_prop/2).

:- prolog_load_context(source,SF),asserta(j_u:junit_prop(file,SF)).

% system:term_expansion( (begin_of_file), [] ):- current_prolog_flag(is_pfc_file_dialect,true).
system:term_expansion( (:- break), [] ):- getenv(keep_going,'-k'). 
system:term_expansion( (end_of_file), [] ):- source_location(F,_),j_u:junit_prop(file,SF), testing_complete, fail.

:- at_halt(testing_complete).

:- endif.


:- set_prolog_flag(debug, true).
:- set_prolog_flag(gc, false).
%:- set_prolog_flag(debug, false).
%:- set_prolog_flag(gc, true).

:- '$current_source_module'(W), '$set_typein_module'(W).
:- prolog_load_context(source,File),unload_file(File).



