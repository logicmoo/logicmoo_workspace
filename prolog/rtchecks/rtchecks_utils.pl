:- module(rtchecks_utils,
	  [handle_rtcheck/1,
	   call_rtc/1, save_rtchecks/1, load_rtchecks/1, rtcheck_error/1,
	   ctime_t/1]).

:- use_module(library(swi/assertions)).
:- use_module(library(swi/basicprops)).
:- use_module(library(swi/plprops)).
:- use_module(library(lists)).
:- use_module(library(intercept)).
:- use_module(library(prolog_codewalk),  []). % for message_location
:- use_module(library(filtered_backtrace)).
:- use_module(rtchecks(compact_list)).

filtered_backtrace:no_backtrace_clause_hook(_, rtchecks_utils).
filtered_backtrace:no_backtrace_clause_hook(_, rtchecks_tracer).
filtered_backtrace:no_backtrace_clause_hook(_, rtchecks_send).
filtered_backtrace:no_backtrace_clause_hook(_, rtchecks_rt).
filtered_backtrace:no_backtrace_clause_hook(_, intercept).
filtered_backtrace:no_backtrace_clause_hook(_, native_props).
filtered_backtrace:no_backtrace_clause_hook(_, send_check).
filtered_backtrace:no_backtrace_clause_hook(_, plprops).

tracertc :-
    filtered_backtrace(100).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains some useful predicates to
	facilitate work with run-time checks.").

:- doc(handle_rtcheck/1, "Predicate that processes a rtcheck exception.").

:- prop rtcheck_error/1 + type #
	"Specifies the format of a run-time check exception.".

rtcheck_error(rtcheck(Type, _Pred, Dict, PropValues, Locs)) :-
	rtcheck_type(Type),
	list(Dict),
	keylist(PropValues),
	list(Locs).

:- prop rtcheck_type/1 + type # "Specifies the type of run-time errors.".

rtcheck_type(comp).
rtcheck_type(pp_check).
rtcheck_type(success).
rtcheck_type(compat).
rtcheck_type(compatpos).
rtcheck_type(calls).

:- pred handle_rtcheck/1 : rtcheck_error.

handle_rtcheck(RTCheck) :-
	check_to_messages(rtcheck, RTCheck, Messages, []),
	pretty_messages(Messages).

pretty_messages(Messages0) :-
	compact_list(Messages0, Messages),
	print_message(error, ciao_messages(Messages)).

:- multifile
	prolog:error_message_signal//1,
	prolog:error_message//1,
	prolog:message//1.

prolog:error_message_signal(RTCheck) -->
    check_to_messages(rtcheck, RTCheck).

prolog:error_message(unintercepted_signal(Signal)) -->
	( prolog:error_message_signal(Signal) -> []
	; ['unintercepted signal: ~p'-[Signal]]
	).

prolog:message(ciao_messages(Messages)) -->
	Messages.

prolog:message(acheck(checks(Time), RTChecks)) -->
	foldl(check_to_messages(Time), RTChecks).

rtc_message_location(Pos) -->
    { Pos = loc(Src, Ln, _)
    ->Loc = file(Src, Ln, -1, _)
    ; Loc = Pos
    },
    {'$push_input_context'(rtchecks)},
    prolog:message_location(Loc),
    {'$pop_input_context'}.
    
position_to_message(posloc(Pred, Loc)) -->
    rtc_message_location(Loc),
    ['Failure of ~q'-[Pred]],
    ( {Loc = clause_pc(Clause, _PC)}
    ->{clause_property(Clause, predicate(Caller))},
      [' in ~q.'-[Caller]]
    ; ['.']
    ).
position_to_message(asrloc(Loc)) -->
    rtc_message_location(Loc).
position_to_message(pploc(Loc)) -->
    rtc_message_location(Loc).

select_defined(_=V) :- nonvar(V).

:- prop ctime_t/1 is type.

ctime_t(ctcheck).
ctime_t(rtcheck).

:- pred check_time_msg(+ctime_t, ?atm).

check_time_msg(rtcheck, 'Run-Time').
check_time_msg(ctcheck, 'Compile-Time').

:- pred check_to_messages(+ctime_t,
			  +rtcheck_error,
			  ?list,
			  ?Messages:list) #
"Converts a run-time check in a message or a list of messages.
~w is the tail."-[Messages].

check_to_messages(Time,
		  rtcheck(Type, Pred, Dict, PropValues0, Positions)) -->
    { gtrace,
      pairs_keys_values(PropValues0, Props, Values0),
      append(Values0, Values1),
      include(select_defined, Values1, Values2),
      sort(Values2, Values),
      writeln(user_error, Dict)
    },
    foldl(position_to_message, Positions),
    {check_time_msg(Time, TimeMsg)},
    ['~w failure in assertion for ~q.~n'-[TimeMsg, Pred],
     '\tIn *~w*, unsatisfied properties: ~n\t\t~q.'-[Type, Props]
    ],
    ( {Values = []}
    ->[]
    ; ['~n\tBecause: ~n\t\t~q.'-[Values]]
    ).

:- meta_predicate call_rtc(0).

:- pred call_rtc/1 : callable # "This predicate calls a goal and if an
	rtcheck signal is intercepted, an error message is shown and
	the execution continues. Alternatively, it is re-raised as an
	exception depending on the flag rtchecks_abort_on_error
	value.".

call_rtc(Goal) :-
	Error = rtcheck(_Type, _Pred, _Dict, _PropValues, _Poss),
	( current_prolog_flag(rtchecks_abort_on_error, yes) ->
	  intercept(Goal, Error, throw(Error)) % rethrow signal as exception
	; intercept(Goal, Error, (handle_rtcheck(Error), tracertc))
	).

:- dynamic rtcheck_db/1.

:- meta_predicate save_rtchecks(0).

:- pred save_rtchecks/1 : callable # "Asserts in rtcheck_db/1 all the
	run-time check exceptions thrown by the goal.".

save_rtchecks(Goal) :-
	RTError = rtcheck(_Type, _Pred, _Dict, _PropValues, _Poss),
	intercept(Goal, RTError, assertz(rtcheck_db(RTError))).

:- pred load_rtchecks/1 => list(rtcheck_error) # "retract the
	rtcheck_db/1 facts and return them in a list.".

load_rtchecks(RTChecks) :-
	findall(RTCheck, retract(rtcheck_db(RTCheck)), RTChecks).
