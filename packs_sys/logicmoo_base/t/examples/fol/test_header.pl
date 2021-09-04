% sets up history for interactive testing
:- if((ignore(exists_source(library(editline))->use_module(library(editline))
       ;(exists_source(library(readerline)),use_module(library(readline)))),
   '$toplevel':setup_history)). :- endif.

% Load SWI Utils
:- if(( \+ exists_source(library(logicmoo_utils_all)), 
   prolog_load_context(directory,X),absolute_file_name('../../..',O,[relative_to(X),file_type(directory)]), attach_packs(O))).  
:- endif.
:- if(use_module(library(logicmoo_utils))). :-endif.

% Load PFC
:- if(set_prolog_flag(pfc_version,v(2,0,0))). :- endif.
:- if(( \+ exists_source(library(pfc_lib)), 
   prolog_load_context(directory,X),absolute_file_name('../../prolog',O,[relative_to(X),file_type(directory)]),asserta(user:file_search_path(library,O)))).
:- endif.
:- if(ensure_loaded(library(pfc_lib))). :-endif.

% Load CLIF
:- if((use_module(library(logicmoo_clif)))). :-endif.

% doing these inside of if/endifs so they dont appear before module/2
:- if((prolog_load_context(source,S),format(user_error,'~N~q,~n',[running(S)]))). :- endif.
:- if(( \+ current_prolog_flag(test_module,_),set_prolog_flag(test_module,baseKB),assert(baseKB:this_is_baseKB))). :- endif.
:- if(( \+ current_prolog_flag(test_typein_module,_), set_prolog_flag(test_typein_module,baseKB))). :- endif.

:- if(current_prolog_flag(loaded_test_header,_)). 
:- wdmsg(reload_of_test_header).
:- mpred_reset.
:- else.
:- if(( \+ current_prolog_flag(loaded_test_header,_),set_prolog_flag(loaded_test_header,loaded))).  :- endif.
:- if((prolog_load_context(module,user), 
  \+ current_prolog_flag(test_module,user),
  \+ current_prolog_flag(test_module,baseKB))).
% writes a temp header file and include/1s it
:- if(( tmp_file(swi, Dir), make_directory(Dir),working_directory(OLD,Dir),asserta(t_l:old_pwd(OLD,Dir)),current_prolog_flag(test_module,Module),open('module_header.pl',write,OS),
  format(OS,'\n:- module(~q,[test_header_include/0]).\n test_header_include. ',[Module]),close(OS))). :- endif.
:- include('module_header.pl').
:- retract(t_l:old_pwd(OLD,Delete)),working_directory(_,OLD),delete_directory_and_contents(Delete).
:- endif.
:- endif.


%:- mpred_trace_exec.
:- prolog_load_context(source,File),!,
   ignore((((sub_atom(File,_,_,_,'.pfc')
   -> (sanity(is_pfc_file),set_prolog_flag(is_pfc_file_dialect,true))
   ; nop((sanity( \+ is_pfc_file),set_prolog_flag(is_pfc_file_dialect,false))))))),!.

%:- set_prolog_flag(debug, true).
%:- set_prolog_flag(gc, false).
%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 0). % 1 = default
:- set_prolog_flag(runtime_debug, 3). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(logicmoo_message_hook,dumpst).
:- ensure_loaded(library(pfc)).

%:- '$current_source_module'(W), '$set_typein_module'(W).
:- if((current_prolog_flag(test_module,Module)->module(Module);true)). :- endif.
:- ensure_loaded(library(pfc_test)).
:- if(at_halt(system:halt_junit)). :- endif.
:- prolog_load_context(source,Src),add_test_info(testsuite,file,Src).
:- prolog_load_context(source,Src),set_prolog_flag(test_src,Src).

clif_op_decls((
 op(1199,fx,('==>')),
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,('<==>')),
 op(1160,xfx,('<-')),
 op(1150,xfx,('=>')),
 op(1140,xfx,('<=')),
 op(1130,xfx,'<=>'),
 op(1120,xfx,'<->'),
% op(1100,fx,('nesc')),
 op(600,yfx,('&')),
 op(600,yfx,('v')),
 op(350,xfx,('xor')),
 op(300,fx,('-')),
 op(300,fx,('~')))).

:- clif_op_decls(OPS),call(OPS).
:- clif_op_decls(OPS),call(baseKB:OPS).

:- dmsg(this_test_might_need(:- use_module(library(logicmoo_plarkc)))).

