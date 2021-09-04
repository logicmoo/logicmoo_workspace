% This file is mostly all inside if/endifs so it doesnt interfere with `module/2`


:- if( \+ current_module(logicmoo_clif)).


% Load Editline/Readline
:- if( \+ current_module(prolog_history)).
:- if((ignore(exists_source(library(editline))->use_module(library(editline))
       ;(exists_source(library(readline)),use_module(library(readline)))),
   '$toplevel':setup_history)). :- endif.
:- endif.

% Load SWI Utils
:- if(( \+ exists_source(library(logicmoo_utils)), 
   prolog_load_context(directory,X),absolute_file_name('../../../..',O,[relative_to(X),file_type(directory)]), attach_packs(O))).  
:- endif.
:- if(use_module(library(logicmoo_utils))). :-endif.

% Load PFC
:- if(set_prolog_flag(pfc_version,v(2,0,0))). :- endif.
:- if(ensure_loaded(library(pfc_lib))). :-endif.

% Load CLIF
:- if((use_module(library(logicmoo_clif)))). :-endif.

:- endif. % \+ current_module(logicmoo_clif)

:- if(assert_if_new((clifops:clif_op_decls((
 op(1199,fx,('==>')), op(1190,xfx,('::::')), op(1180,xfx,('==>')), op(1170,xfx,('<==>')), op(1160,xfx,('<-')),
 op(1150,xfx,('=>')), op(1140,xfx,('<=')), op(1130,xfx,'<=>'),
 op(1120,xfx,'<->'),
 op(600,yfx,('&')), op(600,yfx,('v')),op(350,xfx,('xor')), op(300,fx,('-')),
 op(300,fx,('~'))))))).   :- endif.


:- if((prolog_load_context(source,S),format(user_error,'~N~q,~n',[running(S)]))). :- endif.
:- if(( \+ current_prolog_flag(test_module,_),set_prolog_flag(test_module,baseKB),assert(baseKB:this_is_baseKB))). :- endif.
:- if(( \+ current_prolog_flag(test_typein_module,_), set_prolog_flag(test_typein_module,baseKB))). :- endif.

:- if(current_prolog_flag(loaded_test_header,_)). 
:- wdmsg(reload_of_test_header).
:- mpred_reset.
:- else.
:- if(( \+ current_prolog_flag(loaded_test_header,_),set_prolog_flag(loaded_test_header,loaded))).  :- endif.

:- if(prolog_load_context(module,user)).
:- if(( \+ current_prolog_flag(test_module,user), \+ current_prolog_flag(test_module,baseKB))).
% writes a temp header file and include/1s it
:- if(( tmp_file(swi, Dir), make_directory(Dir),working_directory(OLD,Dir),asserta(t_l:old_pwd(OLD,Dir)),current_prolog_flag(test_module,Module),open('module_header.pl',write,OS),
  format(OS,'\n:- module(~q,[test_header_include/0]).\n test_header_include. ',[Module]),close(OS))). :- endif.
:- include('module_header.pl').
:- retract(t_l:old_pwd(OLD,Delete)),working_directory(_,OLD),delete_directory_and_contents(Delete).
:- endif.
:- endif. % prolog_load_context(module,user)
:- endif. % current_prolog_flag(loaded_test_header,_)

:- if((current_prolog_flag(test_module,Module), '$set_source_module'(Module))). :- endif.
:- if((current_prolog_flag(test_module,Module), clifops:clif_op_decls(OPS), call(Module:OPS))). :- endif.

:- if((prolog_load_context(source,File),!,
   ignore((((sub_atom(File,_,_,_,'.pfc')
   -> (sanity(is_pfc_file),set_prolog_flag(is_pfc_file_dialect,true))
   ; nop((sanity( \+ is_pfc_file),set_prolog_flag(is_pfc_file_dialect,false))))))))).  
:- endif.

:- if((
 %set_prolog_flag(debug, true),
 %set_prolog_flag(gc, false),
 %set_prolog_flag(runtime_speed,0), % 0 = dont care
 set_prolog_flag(runtime_speed, 0), % 1 = default
 set_prolog_flag(runtime_debug, 3), % 2 = important but dont sacrifice other features for it
 set_prolog_flag(runtime_safety, 3),  % 3 = very important
 set_prolog_flag(unsafe_speedups, false),
 set_prolog_flag(logicmoo_message_hook,dumpst),
 %mpred_trace_exec,
 true)).
:- endif.

% :- if(('$current_source_module'(W), '$set_typein_module'(W))). :- endif.
:- if((current_prolog_flag(test_typein_module,Module), '$set_typein_module'(Module), module(Module))). :- endif.
:- if((current_prolog_flag(test_typein_module,Module), clifops:clif_op_decls(OPS), call(Module:OPS))). :- endif.

:- if((ensure_loaded(library(pfc_test)))). :- endif.
:- if((prolog_load_context(source,Src),set_prolog_flag(test_src,Src))). :- endif.
:- if((prolog_load_context(source,Src),add_test_info(testsuite,file,Src))). :- endif.
:- if(at_halt(system:halt_junit)). :- endif.

:- if((prolog_load_context(source,File),(atom_contains(File,'.clif')))).

:- use_module(library(logicmoo_clif)).

:-assert(t_l:each_file_term(must_kif_process_after_rename)).

% install_constant_renamer_until_eof:-  
  %call_on_eof(show_missing_renames), 
%  set_prolog_flag_until_eof(do_renames,term_expansion).

:- set_prolog_flag(runtime_debug, 0). 
:- use_module(library(logicmoo_clif)).
:- set_prolog_flag(runtime_debug, 3). 

:- set_prolog_flag(do_renames,term_expansion).
:- ((prolog_load_context(source,File), atom_contains(File,'.clif')) ->
   (current_stream(File, read, Stream),with_lisp_translation(Stream,must_kif_process_after_rename)); true).

%:- call(call,((asserta(((system:goal_expansion(Here,Loc,_,_):- dmsg(s_goal_expansion(Here,Loc)),trace,fail))),
%   asserta(((system:term_expansion(Here,Loc,_,_):- dmsg(s_term_expansion(Here,Loc)),trace,fail)))))).

:- else.       % end clif file


:- endif.


