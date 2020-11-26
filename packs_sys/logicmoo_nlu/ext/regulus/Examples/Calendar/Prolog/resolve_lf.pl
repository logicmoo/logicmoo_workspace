 
:- module(resolve_lf,
	[resolve_lf/4]
    ).

%======================================================================

:- use_module('$REGULUS/Examples/Calendar/Prolog/ellipsis_classes').

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').
 
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

% [[elliptical,term(name,pierrette,[])]]
is_elliptical_representation(LF) :-
	member([elliptical, _Body], LF).

extract_content_from_elliptical_representation(LF, [[Prep1, Term1]]) :-
	member([elliptical, [[Prep1, Term1]]], LF),
	term_date_or_time_in_pp(Term1),
	!.

extract_content_from_elliptical_representation(LF, [[Prep1, Term1], [Prep2, Term2]]) :-
	member([elliptical, [[Prep1, Term1], [Prep2, Term2]]], LF),
	term_date_or_time_in_pp(Term1),
	term_date_or_time_in_pp(Term2),
	!.

% [[on_date,term(null,monday,[])]]
extract_content_from_elliptical_representation(LF, [[Prep, Term]]) :-
	member([elliptical, [[Prep, Term]]], LF),
	term_date_or_time_in_pp(Term),
	%prep(Prep, _Type),
	!.

% [[elliptical,term(name,pierrette,[])]]
extract_content_from_elliptical_representation(LF, Term) :-
	member([elliptical, Term], LF),
	term_date_or_time(Term),
	!.

term_date_or_time(Term) :-
	(   functor(Term, term, 3)
	;
	    functor(Term, date, 2)
	;
	    functor(Term, date, 3)
	;
	    functor(Term, time, 3) 
	).

term_date_or_time_in_pp(Term) :-
	(   functor(Term, term, 3)
	;
	    functor(Term, date, 2)
	;
	    functor(Term, date, 3)
	;
	    functor(Term, time, 3)
	;
	    atom(Term)
	).

	



	

