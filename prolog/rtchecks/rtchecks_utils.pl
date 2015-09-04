:- module(rtchecks_utils,
	  [handle_rtcheck/1, pretty_messages/1, rtcheck_to_messages/3,
	   ctcheck_to_messages/3, check_to_messages/4, call_rtc/1,
	   save_rtchecks/1, load_rtchecks/1, rtcheck_error/1,
	   ctime_t/1, message_info_t/1],
	  [assertions, basicmodes, nativeprops, regtypes, hiord]).

:- use_module(library(aggregates)).
:- use_module(library(hiordlib)).
:- use_module(library(lists)).
:- use_module(library(sort), [keylist/1]).
:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(write), []).
:- use_module(library(debugger(debugger_lib))).
tracertc.
:- endif.
:- if(current_prolog_flag(dialect, swi)).
:-  abolish(send_signal/1),
    abolish(intercept/3).
:- use_module(library(intercept)).
:- use_module(library(prolog_codewalk),  []). % for message_location
:- use_module(library(filtered_backtrace)).

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

:- endif.
:- use_module(rtchecks(compact_list)).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains some useful predicates to
	facilitate work with run-time checks.").

:- doc(handle_rtcheck/1, "Predicate that processes a rtcheck exception.").

:- regtype rtcheck_error/1 #
	"Specifies the format of a run-time check exception.".

rtcheck_error(rtcheck(Type, _Pred, Dict, PropValues, Locs)) :-
	rtcheck_type(Type),
	list(Dict),
	keylist(PropValues),
	list(Locs).

:- regtype rtcheck_type/1 # "Specifies the type of run-time errors.".

rtcheck_type(comp).
rtcheck_type(pp_check).
rtcheck_type(success).
rtcheck_type(compat).
rtcheck_type(compatpos).
rtcheck_type(calls).

:- pred handle_rtcheck/1 : rtcheck_error.

handle_rtcheck(RTCheck) :-
	check_to_messages(RTCheck, rtcheck, Messages, []),
	pretty_messages(Messages).

:- if(current_prolog_flag(dialect, ciao)).
pretty_messages(Messages) :-
	push_prolog_flag(write_strings, on),
	compact_list(Messages, Messages1),
	messages(Messages1),
	fail.
pretty_messages(_) :-
	pop_prolog_flag(write_strings).
:- endif.

:- regtype message_info_t/1.

message_info_t(message_lns(_, _, _)).
message_info_t(message(_, _)).
message_info_t(message(_)).

:- if(current_prolog_flag(dialect, swi)).

pretty_messages(Messages0) :-
	compact_list(Messages0, Messages),
	print_message(error, ciao_messages(Messages)).

:- multifile
	prolog:error_message_signal//1,
	prolog:error_message//1,
	prolog:message//1.

prolog:error_message_signal(RTCheck) -->
	{check_to_messages(RTCheck, rtcheck, Messages, [])},
	map(Messages, ciao_message).

prolog:error_message(unintercepted_signal(Signal)) -->
	( prolog:error_message_signal(Signal) -> []
	; ['unintercepted signal: ~p'-[Signal]]
	).

prolog:message(ciao_messages(Messages)) -->
	map(Messages, ciao_message).

time_to_message_pred(ctcheck, ctcheck_to_messages).
time_to_message_pred(rtcheck, rtcheck_to_messages).

prolog:message(acheck(checks(Time), RTChecks)) -->
	{ time_to_message_pred(Time, Pred),
	  map(RTChecks, Pred, Messages, [])
	},
	map(Messages, ciao_message).

swi_message(Text) --> map(Text, message_to_swi), [nl].

:- pred ciao_message(+Message:message_info_t, list, list).

ciao_message(message_lns(Pos, _, Text)) -->
    { Pos = loc(Src, Ln, _)
    ->Loc = file(Src, Ln, -1, _)
    ; Loc = Pos
    },
    {'$push_input_context'(rtchecks)},
    prolog:message_location(Loc),
    {'$pop_input_context'},
    swi_message(Text).
ciao_message(message(_, Text)) --> swi_message(Text).
ciao_message(message(Text))    --> swi_message(Text).

message_to_swi(T)       --> {var(T)}, !, ['~w'-[T]].
message_to_swi('\n')    --> !, [nl].
message_to_swi(A)       --> {atom(A)}, !, [A].
message_to_swi($$(M))   --> !, ['~s'-[M]].
message_to_swi({M})     --> !, ['~p'-[M]].
message_to_swi(''({M})) --> !, ['~q'-[M]].
message_to_swi(''(M))   --> !, ['~q'-[M]].
message_to_swi(~~(M))   --> !, ['~w'-[M]].
message_to_swi([](M))   --> !, map(M, message_to_swi).
message_to_swi(T)       --> !, ['~w'-[T]].
:- endif.

position_to_message(posloc(Pred, Loc),
		    message_lns(Loc, error, ['Failure of ', ''(Pred)|Tail])) :-
    ( Loc = clause_pc(Clause, PC)
    ->clause_property(Clause, predicate(Caller)),
      Tail = [' in ', ''(Caller), '.']
    ; Tail = ['.']
    ).
position_to_message(asrloc(Loc),
		    message_lns(Loc, error, [])).
position_to_message(pploc(Loc),
		    message_lns(Loc, error, [])).

:- use_module(library(varnames/apply_dict)).
:- use_module(library(varnames/complete_dict)).
:- export(pretty_prop/3).
pretty_prop(Prop, Dict0, PrettyProp) :-
	complete_dict(Prop, Dict0, [], EDict),
	append(Dict0, EDict, Dict),
	apply_dict(Prop, Dict, yes, PrettyProp).

:- use_module(library(terms_vars)).

pretty_attributes(Term, Attrs) :-
	varset(Term, Vars),
	map(Vars, pretty_attribute, Attrs, []).

pretty_attribute(Var) -->
	( {get_attribute(Var, Attr)} ->
	  [attach_attribute(Var, Attr)]
	; []
	).

select_defined(Term, SDict0, SDict) :-
	( Term=(_N=V),
	  var(V) ->
	  SDict = SDict0
	; SDict0 = [Term|SDict]
	).

:- regtype ctime_t/1.

ctime_t(ctcheck).
ctime_t(rtcheck).

:- pred check_time_msg(+ctime_t, ?atm).

check_time_msg(rtcheck, 'Run-Time').
check_time_msg(ctcheck, 'Compile-Time').

:- pred check_to_messages(+RTCheck   :rtcheck_error,
			  +Time      :ctime_t,
			  ?Messages0 :list(message_info_t),
			  ?Messages  :list(message_info_t))
# "Converts a run-time check in a message or a list of messages.
   @var{Messages} is the tail.".

check_to_messages(rtcheck(Type, Pred0, Dict, PropValues0, Positions0),
		  Time, Messages0, Messages) :-
	pairs_keys_values(PropValues0, Props0, Values0),
	append(Values0, Values1),
	pretty_attributes(Values1, Atts),
	sort(Values1, Values2),
	map(Values2, select_defined, Values3, Atts),
	pretty_prop(t(Pred0, Props0, Dict, Values3, Positions0), Dict,
	    t(Pred, Props, _, Values, Positions)),
	map(Positions, position_to_message, PosMessages0),
	reverse(PosMessages0, PosMessages),
	check_time_msg(Time, TimeMsg),
	Text = [TimeMsg, ' failure in assertion for ', ''({Pred}), '.', '\n',
		'\tIn *', Type, '*, unsatisfied properties: ', '\n',
		'\t\t', ''({Props}), '.'|Text0],
	( Values = []
	->Text0 = Text1
	; Text0 = ['\n', '\tBecause: ','\n',
		   '\t\t', ''({Values}), '.'|Text1]
	),
	( select(message_lns(Loc, MessageType, Text2),
		 PosMessages, PosMessages1)
	->(Text2 == [] -> Text1 = [] ; Text1 = [' ', '\n'|Text2]),
	  Message = message_lns(Loc, MessageType, Text)
	; Text1 = [],
	  Message = message(error, Text),
	  PosMessages1 = PosMessages
	),
	append([Message|PosMessages1], Messages, Messages0).


rtcheck_to_messages(RTCheck) --> check_to_messages(RTCheck, rtcheck).

ctcheck_to_messages(CTCheck) --> check_to_messages(CTCheck, ctcheck).

:- meta_predicate call_rtc(goal).

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

:- data rtcheck_db/1.


:- meta_predicate save_rtchecks(goal).

:- pred save_rtchecks/1 : callable # "Asserts in rtcheck_db/1 all the
	run-time check exceptions thrown by the goal.".

save_rtchecks(Goal) :-
	RTError = rtcheck(_Type, _Pred, _Dict, _PropValues, _Poss),
	intercept(Goal, RTError, assertz_fact(rtcheck_db(RTError))).

:- pred load_rtchecks/1 => list(rtcheck_error) # "retract the
	rtcheck_db/1 facts and return them in a list.".

load_rtchecks(RTChecks) :-
	findall(RTCheck, retract_fact(rtcheck_db(RTCheck)), RTChecks).
