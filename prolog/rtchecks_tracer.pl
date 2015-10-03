:- module(rtchecks_tracer, [trace_rtc/1,
			    do_trace_rtc/1]).

:- use_module(library(implementation_module)).
:- use_module(library(static_strip_module)).
:- use_module(library(apply)).
:- use_module(library(ontrace)).
:- use_module(assertions(assrt_lib)).
:- use_module(rtchecks(rtchecks_rt)).
:- use_module(rtchecks(rtchecks_gen)).
:- use_module(rtchecks(rtchecks_eval)).
:- use_module(rtchecks(rtchecks_utils)).
:- use_module(library(intercept)).

:- dynamic
    rtc_scanned/1,
    rtc_break/2,
    rtc_state/3.

:- meta_predicate
    trace_rtc(0),
    do_trace_rtc(0).

trace_rtc(Goal) :-
    call_rtc(do_trace_rtc(Goal)).

do_trace_rtc(CM:Goal) :-
    generate_rtchecks(_, CM, Goal, RTCGoal),
    call_inoutex(RTCGoal,
		 setup_trace,
		 cleanup_trace).

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
    forall(retract(rtc_state(Visible, Leash, Ref)),
	   ontrace:cleanup_trace(state(Visible, Leash, Ref))),
    retractall(rtc_scanned(_)),
    forall(retract(rtc_break(Clause, PC)),
	   ignore('$break_at'(Clause, PC, false))).

black_list_caller(M:F/A) :-
    functor(H, F, A),
    black_list_caller(H, M).

black_list_caller(M, _) :- black_list_module(M).
black_list_caller(nativeprops, _).

black_list_callee(M, _) :- black_list_module(M).
black_list_callee(system, Call) :- black_list_callee_system(Call).
black_list_callee(nativeprops, Call) :-
    \+ memberchk(Call, [check(_), trust(_), true(_), false(_)]).

black_list_callee_system(catch(_, _, _)).
black_list_callee_system(setup_call_cleanup(_, _, _)).
black_list_callee_system(setup_call_catcher_cleanup(_, _, _, _)).
black_list_callee_system(callable(_)).
black_list_callee_system(atom(_)).

black_list_module(assrt_lib).
black_list_module(rtchecks_tr).
black_list_module(rtchecks_rt).
black_list_module(rtchecks_eval).
black_list_module(rtchecks_tracer).
black_list_module(rtchecks_utils).
black_list_module(rtchecks_basic).
black_list_module(rtchecks_send).
black_list_module('$expand').
black_list_module(complete_dict).
black_list_module(basicprops).
black_list_module(apply_dict).
black_list_module(exceptions).
black_list_module(intercept).
black_list_module(compact_list).
black_list_module(exceptions_db).
black_list_module(hiordlib).
black_list_module(ontrace).
black_list_module(expansion_module).

white_list_meta(system, Call) :- \+ functor(Call, call, _).

skip_predicate(rtchecks_utils:handle_rtcheck(_)).

pp_assr(check(_), _).
pp_assr(trust(_), _).
pp_assr(true( _), _).
pp_assr(false(_), _).

:- public rtcheck_port/3.

% rtcheck_port(_,_,skip) :- !.
rtcheck_port(Port, Frame, Action) :-
    ( current_prolog_flag(gui_tracer, true)
    ->print_message(information,
		    format("gui_tracer activated, rtchecks tracer will be disabled", [])),
      cleanup_trace,
      visible(+cut_call),
      Action = up
    ; rtcheck_port_(Port, Frame, Action)
    ).

rtcheck_port_(unify, Frame, Action) :-
    prolog_frame_attribute(Frame, clause, Clause),
    setup_clause_bpt(Clause, Action), !.
rtcheck_port_(_, Frame, Action) :-
    ( prolog_frame_attribute(Frame, goal, Goal),
      \+ skip_predicate(Goal)
    ->Action = continue
    ; Action = skip
    ).

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
	  \+ assertion_head_body(Goal, M, _, prop, _, _, _, _, _, _)
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
	       ; current_assertion(Goal, rtcheck, _, _, _, _,
				   _, _, _, _, _, _, _, _, _, M)
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

clause_location(Clause, PC, Loc) :-
    '$clause_term_position'(Clause, PC, List),
    ontrace:clause_subloc(Clause, List, Loc).

:- multifile
    prolog:message_location//1,
    prolog:break_hook/6.

prolog:message_location(clause_pc(Clause, PC)) -->
    {clause_location(Clause, PC, Loc)},
    prolog:message_location(Loc).

:- public rat_trap/3.
:- meta_predicate rat_trap(0, +, +).
rat_trap(Goal, Clause, PC) :-
    intercept(Goal, Error,
	      ( ( retract(rtc_break(Clause, PC))
		->ignore('$break_at'(Clause, PC, false))
		; true
		),
		send_signal(Error)
	      )).

% prolog:break_hook(Clause, PC, FR, FBR, Expr, _) :-
%     clause_property(Clause, predicate(PI)),
%     writeln(user_error, prolog:break_hook(Clause:PI, PC, FR, FBR, Expr, _)),
%     fail.
prolog:break_hook(Clause, PC, FR, _, call(Goal0), Action) :-
    \+ current_prolog_flag(gui_tracer, true),
    rtc_break(Clause, PC),
    prolog_frame_attribute(FR, context_module, FCM),
    clause_property(Clause, predicate(PI)),
    ( \+ black_list_caller(PI)
    ->static_strip_module(Goal0, Goal, CM, FCM),
      implementation_module(CM:Goal, IM),
      ( \+ black_list_callee(IM, Goal)
      ->generate_rtchecks(clause_pc(Clause, PC), CM, Goal, RTChecks),
	( Goal == RTChecks
	->Action = continue
	; % Action = call(M:RTChecks)
	  Action = call(rtchecks_tracer:rat_trap(RTChecks, Clause, PC))
	)
      ; Action = continue
      )
    ; Action = continue
    ).
