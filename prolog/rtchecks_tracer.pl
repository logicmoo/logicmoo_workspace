/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(rtchecks_tracer, [trace_rtc/1,
                            do_trace_rtc/1]).

:- use_module(library(mapargs)).
:- use_module(library(assrt_lib)).
:- use_module(library(apply)).
:- use_module(library(context_values)).
:- use_module(system:library(rtchecks_rt)).
:- use_module(system:library(rtchecks_tracer_rt)).
:- use_module(library(rtchecks_utils)).
:- use_module(library(implementation_module)).
:- use_module(library(intercept)).
:- use_module(library(ontrace)).
:- use_module(library(static_strip_module)).
:- use_module(library(resolve_calln)).
:- use_module(library(ctrtchecks)).

:- dynamic
    rtc_scanned/1,
    rtc_break/2,
    rtc_state/3.

:- meta_predicate
    trace_rtc(0),
    do_trace_rtc(0).

trace_rtc(Goal) :-
    call_rtc(do_trace_rtc(Goal)).

do_trace_rtc(Goal) :-
    get_rtcheck_body(Goal, RTChecks),
    call_inoutex(RTChecks, setup_trace, cleanup_trace).

rtcheck_lib(rtchecks_rt).
rtcheck_lib(metaprops).
rtcheck_lib(globprops).
rtcheck_lib(typeprops).

builtin_spec(M, G, S) :-
    predicate_property(M:G, meta_predicate(S)),
    once(arg(_, S, 0 )),
    ( predicate_property(M:G, imported_from(IM))
    ->( IM = system
      ->true
      ; \+ rtcheck_lib(IM),
	predicate_property(system:G, imported_from(IM))
      )
    ; M = system
    ->true
    ).

:- meta_predicate get_rtcheck_body(0, -).
get_rtcheck_body(M:Call, RTChecks) :-
    rtcheck_body(M, Call, RTChecks).

rtcheck_body(_, G, G) :- var(G), !.
rtcheck_body(_, M:G, M:R) :- !,
    rtcheck_body(M, G, R).
rtcheck_body(M, G, R) :-
    builtin_spec(M, G, S), !,
    functor(G, F, A),
    functor(R, F, A),
    mapargs(rtcheck_body_meta_arg(M), S, G, R).
rtcheck_body(M, G, rtcheck_call(M:G)).

rtcheck_body_meta_arg(M, _, 0, G, R) :- !, rtcheck_body(M, G, R).
rtcheck_body_meta_arg(_, _, _, R, R).

:- multifile user:prolog_trace_interception/4.

setup_trace :-
    foldl(ontrace:port_mask, [unify], 0, Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    asserta((user:prolog_trace_interception(Port, Frame, _, Action) :-
            rtcheck_port(Port, Frame, Action)),
            Ref),
    asserta(rtc_state(Visible, Leash, Ref)),
    trace.

cleanup_trace :-
    retract(rtc_state(Visible, Leash, Ref)),
    cleanup_trace(state(Visible, Leash, Ref)),
    retractall(rtc_scanned(_)),
    forall(retract(rtc_break(Clause, PC)),
           ignore('$break_at'(Clause, PC, false))).

black_list_caller(M:F/A) :-
    functor(H, F, A),
    black_list_caller(M, H).

black_list_caller(M, _) :- black_list_module(M).
black_list_caller(metaprops, _).

black_list_callee(M, _) :- black_list_module(M).
black_list_callee(M, Call) :-
    predicate_property(M:'$rtchecked'(_), defined),
    M:'$rtchecked'(Call).
black_list_callee(system, Call) :- black_list_callee_system(Call).
black_list_callee(metaprops, Call) :-
    \+ memberchk(Call, [check(_), trust(_), true(_), false(_)]).

black_list_callee_system(catch(_, _, _)).
black_list_callee_system(setup_call_cleanup(_, _, _)).
black_list_callee_system(setup_call_catcher_cleanup(_, _, _, _)).
black_list_callee_system(callable(_)).
black_list_callee_system(atom(_)).

black_list_module(assrt_lib).
black_list_module(send_check).
black_list_module(ctrtchecks).
black_list_module(rtchecks_rt).
black_list_module(rtchecks_tracer).
black_list_module(rtchecks_utils).
black_list_module(context_values).
black_list_module('$expand').
black_list_module('$messages').
black_list_module(intercept).
black_list_module(ontrace).
black_list_module(expansion_module).
black_list_module(globprops).
black_list_module(typeprops).

white_list_meta(system, Call) :- \+ functor(Call, call, _).

skip_predicate(rtchecks_utils:handle_rtcheck(_)).
skip_predicate(rtchecks_rt:_).

pp_assr(check(_), _).
pp_assr(trust(_), _).
pp_assr(true( _), _).
pp_assr(false(_), _).

:- public rtcheck_port/3.

% rtcheck_port(_,_,skip) :- !.
rtcheck_port(Port, Frame, Action) :-
    tracing,
    ( current_prolog_flag(gui_tracer, true)
    ->print_message(information,
                    format("gui_tracer activated, rtchecks disabled", [])),
      cleanup_trace,
      visible(+cut_call),
      Action = up
    ; rtcheck_port_(Port, Frame, Action)
    ).

rtcheck_port_(unify, Frame, Action) :-
    prolog_frame_attribute(Frame, clause, Clause),
    setup_clause_bpt(Clause, Action), !.
rtcheck_port_(_, Frame, Action) :-
    prolog_frame_attribute(Frame, goal, Goal),
    ( skip_predicate(Goal)
    ->Action = skip
    ; Action = continue
    ).
%    writeln(user_error, rtcheck_port_(Action->Goal)).

call_instr(i_usercall0 ).
call_instr(i_usercalln(_)).

call_instr_param(i_call(  PI), PI).
call_instr_param(i_depart(PI), PI).

setup_clause_bpt(Clause, Action) :-
    ( rtc_scanned(Clause)
    ->Action = skip
    ; assertz(rtc_scanned(Clause)),
      clause_property(Clause, module(CM)),
      \+ black_list_module(CM),
      '$break_pc'(Clause, PC, _NextPC1),
      '$fetch_vm'(Clause, PC, _NextPC2, TInstr),
      ( ( call_instr(TInstr)
        ->nth_clause(M:Goal, _, Clause),
          \+ prop_asr(Goal, M, _, prop, _, _, _)
        ; call_instr_param(TInstr, PI),
          ( PI=LM:F/A
          ->functor(Goal, F, A),
            implementation_module(LM:Goal, M)
          ; PI=F/A
          ->functor(Goal, F, A),
            implementation_module(CM:Goal, M)
          ),
          \+ black_list_callee(M, Goal),
          once(( rtchecks_tracer:pp_assr(Goal, M)
               ; current_assertion(rt, Goal, M, _)
               ; white_list_meta(M, Goal),
                 predicate_property(M:Goal, meta_predicate(S)),
                 once(arg(_, S, 0 ))
               ))
        )
      ->'$break_at'(Clause, PC, true),
        assertz(rtc_break(Clause, PC)),
        fail
      )
    ).

:- multifile
    prolog:break_hook/6.

:- public system:'$rat_trap'/5.
:- meta_predicate system:'$rat_trap'(0, +, +, +, +).
system:( '$rat_trap'(RTChecks, Goal, Caller, Clause, PC) :-
       rtchecks_tracer:( intercept(with_value(RTChecks, '$current_goal', Goal),
                                   assrchk(asr, Error),
                                   '$rat_trap_handle'(Caller, Clause, PC, Error)))).

:- '$hide'('$rat_trap_handle'/4).
'$rat_trap_handle'(Caller, Clause, PC, Error) :-
    ( retract(rtc_break(Clause, PC))
    ->ignore('$break_at'(Clause, PC, false))
    ; true
    ),
    send_signal(assrchk(ppt(Caller, clause_pc(Clause, PC)), Error)).

% prolog:break_hook(Clause, PC, FR, FBR, Expr, _) :-
%     tracing,
%     clause_property(Clause, predicate(PI)),
%     writeln(user_error, prolog:break_hook(Clause:PI, PC, FR, FBR, Expr, _)),
%     % backtrace(50),
%     fail.
prolog:break_hook(Clause, PC, FR, _, call(Goal0), Action) :-
    tracing,
    \+ current_prolog_flag(gui_tracer, true),
    rtc_break(Clause, PC),
    prolog_frame_attribute(FR, context_module, FCM),
    clause_property(Clause, predicate(Caller)),
    ( \+ black_list_caller(Caller)
    ->resolve_calln(Goal0, Goal1),
      static_strip_module(Goal1, FCM, Goal, CM),
      implementation_module(CM:Goal, IM),
      ( \+ black_list_callee(IM, Goal)
      ->( nb_current('$current_goal', CurrGoal),
          CurrGoal =@= CM:Goal
        ->Action = continue
        ; get_rtcheck_body(CM:Goal, RTChecks),
          '$fetch_vm'(Clause, PC, NPC, _VMI),
          Action = call('$rat_trap'(RTChecks, CM:Goal, Caller, Clause, NPC))
        )
      ; Action = continue
      )
    ; Action = continue
    ).
