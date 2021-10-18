/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
:- module(swi_backport,
   [
      rtrace/1,  % Non-interactive tracing
      rtrace_break/1,  % Interactive tracing
      quietly/1,  % Non-det notrace/1
      restore_trace/1, % After call restor tracer
      rtrace/0, % Start non-intractive tracing
      srtrace/0, % Start non-intractive tracing at System level
      nortrace/0, % Stop non-intractive tracing
      push_tracer/0,pop_tracer/0,reset_tracer/0, % Reset Tracer to "normal"
      on_x_debug/1, % Non-intractive tracing when exception occurs 
      on_f_rtrace/1, % Non-intractive tracing when failure occurs 
      maybe_leash/1, % Set leash only when it makes sense
      maybe_leash/0,
      non_user_console/0,
      ftrace/1, % rtrace showing only failures
      push_guitracer/0,pop_guitracer/0
   ]).

:- set_module(class(library)).

:-ensure_loaded(('cyc_pl/cyc.pl')).

/*
:- if( predicate_property(cyc:debugFmt(_), defined)).
:- abolish(cyc:debugFmt/1).
:- endif.

:- if( predicate_property(cyc:debugFmt(_,_), defined)).
:- abolish(cyc:debugFmt/2).
:- else.
:- endif.
:- if(\+ predicate_property(nop(_),defined)).
:- endif.
:- if( \+ predicate_property(term_to_string(_,_),defined)).
:- endif.
%:- if(\+ predicate_property(alldiscontiguous(),defined)).
%:- endif.

*/

cls:-shell(cls).

:-asserta(user:library_directory('/opt/logicmoo_workspace/packs_sys/logicmoo_utils/prolog/')).

term_to_string(I,IS):- error_catch(string_to_atom(IS,I),_,fail),!.
term_to_string(I,IS):- term_to_atom(I,A),string_to_atom(IS,A),!.
alldiscontiguous:-!.
cyc:debugFmt(Stuff):-once(lmdebugFmt(Stuff)).
cyc:debugFmt(F,A):-once(lmdebugFmt(F,A)).

nop(_).

string_parse_structure_opts_547(Parser, _In, _M, Options,Options2):-
	sgml:set_parser_options(Parser, Options, Options1),
	Options2=Options1.

:- module_transparent(setup_call_cleanup/3).

setup_call_cleanup(X,Y,Z):- X,!,call_cleanup(Y,Z).
% atomic_list_concat_aiml(X,Y,Z):- atomic_list_concat_aiml(X,Y,Z).



:- meta_predicate('$with_unlocked_pred_local'(:,0)).
'$with_unlocked_pred_local'(MP,Goal):- strip_module(MP,M,P),Pred=M:P,
   (predicate_property(Pred,foreign)-> true ;
  (
 ('$get_predicate_attribute'(Pred, system, OnOff)->true;throw('$get_predicate_attribute'(Pred, system, OnOff))),
 (==(OnOff,0) -> Goal ;
 setup_call_cleanup('$set_predicate_attribute'(Pred, system, 0),
   catch(Goal,E,throw(E)),'$set_predicate_attribute'(Pred, system, 1))))).

:- meta_predicate(totally_hide(:)).
totally_hide(MP):- strip_module(MP,M,P),Pred=M:P,
   % (current_prolog_flag(runtime_debug,N), N>2) -> unhide(Pred) ; 
  '$with_unlocked_pred_local'(Pred,
   (('$set_predicate_attribute'(Pred, trace, 0),'$set_predicate_attribute'(Pred, hide_childs, 1)))).

:-totally_hide(totally_hide/1).



%:- totally_hide(quietly/1).


%! rtrace is det.
%
% Start RTracer.
%

rtrace:- start_rtrace,trace.

:- 'totally_hide'(rtrace/0).

start_rtrace:-
      leash(-all),
      assert(t_l:rtracing),
      set_prolog_flag(access_level,system),
      push_guitracer,
      set_prolog_flag(gui_tracer,false),
      visible(+all),
      visible(+exception),
      maybe_leash(+exception).

:- 'totally_hide'(start_rtrace/0).

%! srtrace is det.
%
% Start RTracer.
%
srtrace:- notrace, set_prolog_flag(access_level,system), rtrace.

:- totally_hide(srtrace/0).



%! nortrace is det.
%
% Stop Tracer.
%
stop_rtrace:- 
  notrace,
  maybe_leash(+all),
  visible(+all),
  maybe_leash(+exception),
  retractall(t_l:rtracing),
  !.

:- 'totally_hide'(stop_rtrace/0).
:- system:import(stop_rtrace/0).

nortrace:- stop_rtrace,ignore(pop_tracer).

:- totally_hide(nortrace/0).


:- thread_local('$leash_visible'/2).

%! restore_trace( :Goal) is det.
%
% restore  Trace.
%
%! restore_trace( :Goal) is nondet.
%
% restore  Trace.
%
restore_trace(Goal):- 
  setup_call_cleanup(
   push_leash_visible,
   scce_orig(push_tracer,Goal,pop_tracer),
   restore_leash_visible).

restore_trace0(Goal):- 
  '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   scce_orig(restore_leash_visible,
   ((Goal*-> (push_leash_visible, '$leash'(_, OldL),'$visible'(_, OldV)) ; fail)),
   ('$leash'(_, OldL),'$visible'(_, OldV))).

:- totally_hide(system:'$leash'/2).
:- totally_hide(system:'$visible'/2).

push_leash_visible:- notrace((('$leash'(OldL0, OldL0),'$visible'(OldV0, OldV0), asserta('$leash_visible'(OldL0,OldV0))))).
restore_leash_visible:- notrace((('$leash_visible'(OldL1,OldV1)->('$leash'(_, OldL1),'$visible'(_, OldV1));true))).

% restore_trace(Goal):- setup_call_cleanup(get_trace_reset(Reset),Goal,notrace(Reset)).
:- totally_hide(restore_trace/0).



%! push_guitracer is nondet.
%
% Save Guitracer.
%
push_guitracer:-  notrace(ignore(((current_prolog_flag(gui_tracer, GWas);GWas=false),asserta(t_l:wasguitracer(GWas))))).
:- totally_hide(push_guitracer/0).


%! pop_guitracer is nondet.
%
% Restore Guitracer.
%
pop_guitracer:- notrace(ignore(((retract(t_l:wasguitracer(GWas)),set_prolog_flag(gui_tracer, GWas))))).
:- totally_hide(pop_guitracer/0).

%! maybe_leash( +Flags) is det.
%
% Only leashes interactive consoles
%
maybe_leash(Some):- notrace((maybe_leash->leash(Some);true)).
:- totally_hide(maybe_leash/1).

maybe_leash:- notrace((\+ current_prolog_flag(runtime_must,keep_going), \+ non_user_console)).

non_user_console:- !,fail.
non_user_console:- \+ stream_property(current_input, tty(true)),!.
non_user_console:- \+ stream_property(current_input,close_on_abort(false)).



rtrace(Goal):- notrace(tracing)-> rtrace0((trace,Goal)) ; 
  setup_call_cleanup(current_prolog_flag(debug,WasDebug),
   rtrace0((trace,Goal)),(set_prolog_flag(debug,WasDebug),notrace(stop_rtrace))).
rtrace0(Goal):-
 setup_call_cleanup(notrace((current_prolog_flag(debug,O),rtrace)),
   (trace,Goal,notrace,deterministic(YN),
     (YN == true->!;next_rtrace)),
     notrace(set_prolog_flag(debug,O))).


scce_orig(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 


set_leash_vis(OldL,OldV):- '$leash'(_, OldL),'$visible'(_, OldV),!.
:- totally_hide(set_leash_vis/2).

next_rtrace:- (nortrace;(rtrace,trace,notrace(fail))).
:- 'totally_hide'(next_rtrace/0).



:- '$hide'(rtrace/1).
:- '$hide'(rtrace0/1).
:- '$set_predicate_attribute'(rtrace/1, hide_childs, 1).
:- '$set_predicate_attribute'(rtrace0/1, hide_childs, 0).

%! get_trace_reset( ?Reset) is det.
%
% Get Tracer.
%
get_trace_reset((notrace,set_prolog_flag(debug,WasDebug),CC3,'$visible'(_, OldV),'$leash'(_, OldL),RestoreTrace)):- 
     (notrace(tracing) -> (notrace,RestoreTrace = trace) ; RestoreTrace = notrace),
     '$leash'(OldL, OldL),'$visible'(OldV, OldV),
     (current_prolog_flag(debug,true)->WasDebug=true;WasDebug=false),     
     (current_prolog_flag(gui_tracer, GWas)->CC3=set_prolog_flag(gui_tracer, GWas);CC3=true),!,
     RestoreTrace.

:- totally_hide(get_trace_reset/1).
:- totally_hide(get_trace_reset/1).



%! push_tracer is det.
%
% Push Tracer.
%
push_tracer:- get_trace_reset(Reset)->asserta(t_l:tracer_reset(Reset)).
:- totally_hide(push_tracer/0).

%! pop_tracer is det.
%
% Pop Tracer.
%
pop_tracer:- notrace((retract(t_l:tracer_reset(Reset))->Reset;true)).
:- totally_hide(pop_tracer/0).

%! reset_tracer is det.
%
% Reset Tracer.
%
reset_tracer:- ignore((t_l:tracer_reset(Reset)->Reset;true)).
:- totally_hide(reset_tracer/0).

:- '$hide'(quietly/1).
%:- if_may_hide('totally_hide'(notrace/1,  hide_childs, 1)).
%:- if_may_hide('totally_hide'(notrace/1)).
:- totally_hide(system:tracing/0).
:- totally_hide(system:notrace/0).
:- totally_hide(system:notrace/1).
:- totally_hide(system:trace/0).



/**
 * Simulation of some SWI-Prolog conditional compilation.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */


% sys_stack(-List)
:- private sys_stack/1.
:- thread_local sys_stack/1.

:- private sys_push_stack/1.
sys_push_stack(C) :-
   retract(sys_stack(L)), !,
   assertz(sys_stacK([C|L])).
sys_push_stack(C) :-
   assertz(sys_stack([C])).

% sys_peek_stack
:- private sys_peek_stack/0.
sys_peek_stack :-
   sys_stack([off|_]).

% sys_pop_stack
:- private sys_pop_stack/0.
sys_pop_stack :-
   retract(sys_stack([_,X|L])), !,
   assertz(sys_stack([X|L])).
sys_pop_stack :-
   retract(sys_stack([_])), !.
sys_pop_stack :-
   throw(error(syntax_error(unbalanced_directive),_)).

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
user:term_expansion((:- if(C)), unit) :- !,
   (  sys_peek_stack
   -> sys_push_stack(off)
   ;  C
   -> sys_push_stack(on)
   ;  sys_push_stack(off)).
user:term_expansion((:- elif(C)), unit) :- !,
   (  sys_peek_stack
   -> D = off
   ;  D = on), sys_pop_stack,
   (  sys_peek_stack
   -> sys_push_stack(off)
   ;  D = off, C
   -> sys_push_stack(on)
   ;  sys_push_stack(off)).
user:term_expansion((:- else), unit) :- !,
   (  sys_peek_stack
   -> D = off
   ;  D = on), sys_pop_stack,
   (  sys_peek_stack
   -> sys_push_stack(off)
   ;  D = off
   -> sys_push_stack(on)
   ;  sys_push_stack(off)).
user:term_expansion((:- endif), unit) :- !, sys_pop_stack.
user:term_expansion(unit, _) :- !, fail.
user:term_expansion(_, unit) :- sys_peek_stack, !.

















% WAS EOF
:- module_transparent(nortrace/0).

:-thread_local(t_l:rtracing/0).
:-thread_local(t_l:tracer_reset/1).
:-thread_local(t_l:wasguitracer/1).
:-thread_local(t_l:wastracer/1).

:- 'meta_predicate'(call_call(0)).
call_call(G):-call(G).


:- meta_predicate
   rtrace(0),
   restore_trace(0),
   on_x_debug(0),
   on_f_rtrace(0),  
   
   rtrace_break(0),
   quietly(0),
   ftrace(0).

%! on_f_rtrace( :Goal) is det.
%
% If :Goal fails trace it 
%


% on_f_rtrace(Goal):-  Goal *-> true; ((nortrace,notrace,debugCallWhy(failed(on_f_rtrace(Goal)),Goal)),fail).

on_f_rtrace(Goal):-  Goal *-> true; (rtrace(Goal),debugCallWhy(on_f_rtrace(Goal),Goal)).



%! on_x_debug( :Goal) is det.
%
% If there If Is an exception in :Goal then rtrace.
%
on_x_debug(Goal):- 
 ((( tracing; t_l:rtracing),maybe_leash(+exception))) 
  -> Goal
   ;
   (catchv(Goal,E,(ignore(debugCallWhy(on_x_debug(E,Goal),Goal)),throw(E)))).



unhide(Pred):- '$set_predicate_attribute'(Pred, trace, true),mpred_trace_childs(Pred).


:- multifile(user:prolog_exception_hook/4).
:- dynamic(user:prolog_exception_hook/4).
:- module_transparent(user:prolog_exception_hook/4).

% Make sure interactive debugging is turned back on

user:prolog_exception_hook(error(_, _),_, _, _) :- leash(+all),fail.

user:prolog_exception_hook(error(_, _),_, _, _) :- fail, 
   notrace((  reset_tracer ->
     maybe_leash ->
     t_l:rtracing ->
     leash(+all),
     fail)).

%! quietly( :Goal) is nondet.
%
% Unlike notrace/1, it allows nondet tracing 
%
% But also may be break when excpetions are raised during Goal.
%

% Version 1
quietly(Goal):- \+ tracing,!,call(Goal).
quietly(Goal):- notrace,call_cleanup(Goal,trace).

% version 2 
quietly2(Goal):- \+ tracing -> Goal ; (notrace,call_cleanup(scce_orig(notrace,Goal,trace),trace)).

% version 3 
% quietly(Goal):- !, Goal.  % for overiding
quietly3(Goal):- \+ tracing -> Goal ; 
 (notrace,
  (((Goal,deterministic(YN))) *->
     (YN == yes -> trace ; (trace;(notrace,fail)));
  (trace,!,notrace(fail)))).



deterministically_must(G):- call(call,G),deterministic(YN),true,
  (YN==true -> true; 
     ((wdmsg(failed_deterministically_must(G)),(!)))),!.



%! rtrace( :Goal) is nondet.
%
% Trace a goal non-interactively until the first exception on
%  total failure
%
% ?- rtrace(member(X,[1,2,3])).
%    Call: (9) [lists] lists:member(_7172, [1, 2, 3])    
%    Unify: (9) [lists] lists:member(_7172, [1, 2, 3])   
%    Call: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], 1, 1)     
%    Exit: (10) [lists] lists:member_([2, 3], 1, 1)      
%    Exit: (9) [lists] lists:member(1, [1, 2, 3])        
% X = 1 ;                                                
%    Redo: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], _7172, 1) 
%    Call: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], 2, 2)        
%    Exit: (11) [lists] lists:member_([3], 2, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 2, 1)      
%    Exit: (9) [lists] lists:member(2, [1, 2, 3])        
% X = 2 ;                                                
%    Redo: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], _7172, 2)    
%    Call: (12) [lists] lists:member_([], _7172, 3)      
%    Unify: (12) [lists] lists:member_([], 3, 3)         
%    Exit: (12) [lists] lists:member_([], 3, 3)          
%    Exit: (11) [lists] lists:member_([3], 3, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 3, 1)      
%    Exit: (9) [lists] lists:member(3, [1, 2, 3])        
% X = 3.                                                 
%                                                        
%  ?- rtrace(fail).                                      
%    Call: (9) [system] fail                             
%    Fail: (9) [system] fail                             
% ^  Redo: (8) [rtrace] rtrace:rtrace(user:fail)
% false.

/*
  ?- rtrace((member(X,[writeln(1),throw(good),writen(failed)]),X)).
   Call: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Unify: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Call: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (10) [lists] lists:member(writeln(1), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] writeln(1)
1
   Exit: (10) [system] writeln(1)
X = writeln(1) ;
   Redo: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Call: (12) [lists] lists:member_([writen(failed)], _13424, throw(good))
   Unify: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], throw(good), writeln(1))
   Exit: (10) [lists] lists:member(throw(good), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] throw(good)
ERROR: Unhandled exception: good
*/



%! rtrace_break( :Goal) is nondet.
%
% Trace a goal non-interactively and break on first exception 
% or on total failure
%
rtrace_break(Goal):- \+ maybe_leash, !, rtrace(Goal).
rtrace_break(Goal):- stop_rtrace,trace,debugCallWhy(rtrace_break(Goal),Goal).
%:- totally_hide(rtrace_break/1).
:- '$set_predicate_attribute'(rtrace_break/1, hide_childs, false).





%! ftrace( :Goal) is nondet.
%
% Functor Trace.
%
ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   maybe_leash(-all),maybe_leash(+exception),trace,Goal)).



:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

:- use_module(library(logicmoo_util_common)).
:- fixup_exports.
:- totally_hide('$toplevel':save_debug).
:- totally_hide('$toplevel':toplevel_call/1).
:- totally_hide('$toplevel':residue_vars(_,_)).
:- totally_hide('$toplevel':save_debug).
:- totally_hide('$toplevel':no_lco).


