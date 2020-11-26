:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(pre_process_for_paraphrasing,
	[pre_process_for_paraphrasing/2
	]).


%======================================================================

:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%======================================================================

pre_process_for_paraphrasing(DialogueMove, DialogueMove1) :-
	add_day_of_week_to_datime(DialogueMove, DialogueMove1),
	!.
pre_process_for_paraphrasing(DialogueMove, DialogueMove1) :-
	format('~NError: bad call: ~w~n', [pre_process_for_paraphrasing(DialogueMove, DialogueMove1)]),
	fail.

add_day_of_week_to_datime(V, V) :-
	var(V),
	!.
add_day_of_week_to_datime(A, A) :-
	atomic(A),
	!.
add_day_of_week_to_datime(tense_information=interval(datime(Y, Mo, D, H, Mi, S),
						     datime(Y1, Mo1, D1, H1, Mi1, S1)),
			  tense_information=interval(datime(irrelevant, Y, Mo, D, H, Mi, S),
						     datime(irrelevant, Y1, Mo1, D1, H1, Mi1, S1))) :-
	!.
add_day_of_week_to_datime(datime(generic, generic, generic, H, Mi, S),
			  datime(generic, generic, generic, generic, H, Mi, S)) :-
	!.
add_day_of_week_to_datime(datime(Y, Mo, D, H, Mi, S),
			  datime(DOW, Y, Mo, D, H, Mi, S)) :-
	number(Y), number(Mo), number(D),
	dayofweek_for_date(datime(Y, Mo, D, 0, 0, 0), DOW),
	!.
add_day_of_week_to_datime(T, T1) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	add_day_of_week_to_datime_args(N, T, T1),
	!.

add_day_of_week_to_datime_args(I, _T, _T1) :-
	I =< 0,
	!.
add_day_of_week_to_datime_args(I, T, T1) :-
	I >= 1,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	add_day_of_week_to_datime(Arg, Arg1),
	I1 is I - 1,
	!,
	add_day_of_week_to_datime_args(I1, T, T1).


			   