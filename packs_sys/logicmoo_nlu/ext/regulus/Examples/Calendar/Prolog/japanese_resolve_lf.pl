 
:- module(resolve_lf,
	[resolve_lf/4]
    ).

%======================================================================

:- use_module('$REGULUS/Examples/Calendar/Prolog/japanese_ellipsis_classes').

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/japanese_calendar_utils').
 
%======================================================================

resolve_lf(InLF, InState, OutLF, Substitutions) :-
	is_elliptical_representation(InLF),
	member(lf=PreviousLF, InState),
	resolve_lf1(PreviousLF, InLF, OutLF, Substitutions),
	!.
resolve_lf(InLF, _InState, OutLF, Substitutions) :-
	OutLF = InLF,
	Substitutions = [trivial],
	!.

resolve_lf1(PreviousLF, InLF, OutLF, Substitutions) :-
	extract_content_from_elliptical_representation(InLF, Content),
	resolve_lf2(PreviousLF, Content, OutLF, Substitutions-[]),
	Substitutions \== [],
	!.
resolve_lf1(_PreviousLF, InLF, _OutLF, _Substitutions) :-
	format('~N*** Warning: unable to resolve possibly elliptical expression ~w~n', [InLF]),
	fail.

resolve_lf2(Previous, New, New, [Substitution | Out]-Out) :-
	in_ellipsis_class(Class, _Example1, Previous),
	in_ellipsis_class(Class, _Example2, New),
	Substitution = ( Previous --> New ),
	!.
resolve_lf2(Previous, _New, Out, Substitutions-Substitutions) :-
	\+ compound(Previous),
	Previous = Out,
	!.
resolve_lf2(Previous, New, Out, SubstitutionsIn-SubstitutionsOut) :-
	functor(Previous, F, N),
	functor(Out, F, N),
	resolve_lf2_args(N, Previous, New, Out, SubstitutionsIn-SubstitutionsOut).
 
resolve_lf2_args(0, _Previous, _New, _Out, SubstitutionsIn-SubstitutionsIn).
resolve_lf2_args(I, Previous, New, Out, SubstitutionsIn-SubstitutionsOut) :-
	I > 0,
	arg(I, Previous, PreviousArg),
	arg(I, Out, OutArg),
	resolve_lf2(PreviousArg, New, OutArg, SubstitutionsIn-SubstitutionsNext),
	I1 is I - 1,
	!,
	resolve_lf2_args(I1, Previous, New, Out, SubstitutionsNext-SubstitutionsOut).

is_elliptical_representation(LF) :-
	LF = [[question, 
	       [[manner, dou],
		[_Prep, _Term],
		[tense, present],
		[verb, desu]]]].
is_elliptical_representation(LF) :-
	LF = [[UttType,
	       form(present,
		    [[desu],
		     [subject, _Term]])
	      ]],
	member(UttType, [question, dcl]).

extract_content_from_elliptical_representation(LF, Term) :-
	LF = [[UttType,
	       form(present,
		    [[desu],
		     [subject, Term]])
	      ]],
	member(UttType, [question, dcl]),
	!.
extract_content_from_elliptical_representation(LF, Term) :-
	LF = [[question, 
	       [[manner, dou],
		[subject, Term],
		[tense, present],
		[verb, desu]]]],
	!.
extract_content_from_elliptical_representation(LF, [Prep, Term]) :-
	LF = [[question, 
	       [[manner, dou],
		[Prep, Term],
		[tense, present],
		[verb, desu]]]],
	!.






	

