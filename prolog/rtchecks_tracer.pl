:- module(rtchecks_tracer, [trace_rtc/1,
			    do_trace_rtc/1]).

:- use_module(library(swi/rtchecks)). % Proper load of ciao dialect modules
:- use_module(library(implementation_module)).
:- use_module(library(static_strip_module)).
:- use_module(library(maplist_dcg)).
:- use_module(library(ontrace)).
:- use_module(library(assertions/assrt_lib)).
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

do_trace_rtc(Goal) :-
    call_inoutex(Goal,
		 setup_trace,
		 cleanup_trace).

:- multifile user:prolog_trace_interception/4.

setup_trace :-
    maplist_dcg(ontrace:port_mask, [unify], 0, Mask),
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
black_list_module(native_props).
black_list_module(basic_props).
black_list_module(apply_dict).
black_list_module(exceptions).
black_list_module(intercept).
black_list_module(compact_list).
black_list_module(exceptions_db).
black_list_module(hiordlib).
black_list_module(system).
black_list_module(ontrace).
black_list_module(maplist_dcg).
black_list_module(expansion_module).

skip_predicate(rtchecks_utils:handle_rtcheck(_)).

pp_assr(check(_), _).
pp_assr(trust(_), _).

:- public rtcheck_port/3.

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

setup_clause_bpt(Clause, Action) :-
    ( rtc_scanned(Clause)
    ->Action = skip
    ; assertz(rtc_scanned(Clause)),
      clause_property(Clause, module(CM)),
      \+ black_list_module(CM),
      '$break_pc'(Clause, PC, _NextPC1),
      '$fetch_vm'(Clause, PC, _NextPC2, TInstr),
      ( ( memberchk(TInstr, [i_usercall0, i_usercalln(_N)])
	->nth_clause(M:Goal, _, Clause),
	  \+ assertion_head_body(Goal, M, _, prop, _, _, _, _, _, _)
	; memberchk(TInstr, [i_call(PI), i_depart(PI)]),
	  ( PI=M:F/A
	  ->functor(Goal, F, A)
	  ; PI=F/A
	  ->functor(Goal, F, A),
	    implementation_module(CM:Goal, M)
	  ),
	  \+ black_list_module(M),
	  once(( pp_assr(Goal, M)
	       ; current_assertion(Goal, rtcheck, _, _, _, _,
				   _, _, _, _, _, _, _, _, _, M)
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
rat_trap(Goal, Clause, PC) :-
    intercept(Goal, Error,
	      ( ( retract(rtc_break(Clause, PC))
		->ignore('$break_at'(Clause, PC, false))
		; true
		),
		send_signal(Error)
	      )).

% prolog:break_hook(Clause, PC, FR, FBR, Expr, _) :-
%     writeln(user_error, prolog:break_hook(Clause, PC, FR, FBR, Expr, _)),
%     fail.
prolog:break_hook(Clause, PC, FR, _, call(Goal0), Action) :-
    \+ current_prolog_flag(gui_tracer, true),
    rtc_break(Clause, PC),
    prolog_frame_attribute(FR, context_module, CM),
    ( black_list_module(CM)
    ->Action = continue
    ; static_strip_module(Goal0, Goal, M, CM),
      ( black_list_module(M)
      ->Action = continue
      ; generate_rtchecks(clause_pc(Clause, PC), M, Goal, RTChecks),
	( Goal == RTChecks
	->Action = continue
	; % Action = call(M:RTChecks)
	  Action = call(rtchecks_tracer:rat_trap(M:RTChecks, Clause, PC))
	)
      )
    ).
