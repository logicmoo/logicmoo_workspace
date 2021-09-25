

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
:- expects_dialect(pfc).
%:- endif.


:- ensure_loaded(library(logicmoo_test)).

%:- endif. % current_prolog_flag(test_header,_).


:- prolog_load_context(source,File),!,
   ignore((((atom_contains(File,'.pfc') 
  -> sanity(pfc_lib:is_pfc_file) ; sanity( \+ pfc_lib:is_pfc_file))))),!.

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




:- fav_debug.
:- set_prolog_flag(gc, true).

% :- endif.


:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- fixup_exports.


% :- set_prolog_IO(user_input,user_output,user_error).

:- if((prolog_load_context(source,File),(atom_contains(File,'.clif')))).

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


