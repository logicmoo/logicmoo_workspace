:- module(lf_rewrite_runtime,
	  [rewrite_lf/2]
    ).

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%======================================================================

rewrite_lf(LFIn, LFOut) :-
	copy_term(LFIn, LF1),
	make_ground(LF1),
	extract_context(LF1, []-Context),
	rewrite_lf1(LF1, LF2, Context),
	unground(LF2, LFOut),
	!.
rewrite_lf(LFIn, LFOut) :-
	format('~N*** Error: bad call: ~w~n', [rewrite_lf(LFIn, LFOut)]),
	fail.

%----------------------------------------------------------------------

extract_context(V, In-In) :-
	var(V),
	!.
extract_context(Atom, In-In) :-
	atomic(Atom),
	!.
extract_context(quant(_Q, _V, Rest, Scope), In-Out) :-
	append(In, Rest, Next1),
	extract_context(Rest, Next1-Next2),
	extract_context(Scope, Next2-Out),
	!.
extract_context(Term, In-Out) :-
	compound(Term),
	functor(Term, _F, N),
	extract_context_args(N, Term, In-Out),
	!.

extract_context_args(I, _Term, In-In) :-
	I < 1,
	!.
extract_context_args(I, Term, In-Out) :-
	arg(I, Term, Arg),
	extract_context(Arg, In-Next),
	I1 is I - 1,
	!,
	extract_context_args(I1, Term, Next-Out).

%----------------------------------------------------------------------

rewrite_lf1(V, V, _Context) :-
	var(V),
	!.
rewrite_lf1(Atom, Atom, _Context) :-
	atomic(Atom),
	!.
rewrite_lf1(Form, Form1, Context) :-
	user:lf_rewrite(Form, Form1, Conds),
	conditions_match_context(Conds, Context),
	!.
rewrite_lf1(quant(Q, V, Rest, Scope),
	    quant(Q, V, Rest1, Scope1),
	    Context) :-
	rewrite_lf_restriction(Rest, Rest1, Context),
	rewrite_lf1(Scope, Scope1, Context),
	!.
rewrite_lf1(Term, Term1, Context) :-
	compound(Term),
	functor(Term, F, N),
	functor(Term1, F, N),
	rewrite_lf1_args(N, Term, Term1, Context),
	!.

%--------------------------------------------------------

rewrite_lf1_args(I, _Term, _Term1, _Context) :-
	I < 1,
	!.
rewrite_lf1_args(I, Term, Term1, Context) :-
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	rewrite_lf1(Arg, Arg1, Context),
	I1 is I - 1,
	!,
	rewrite_lf1_args(I1, Term, Term1, Context).

%--------------------------------------------------------

rewrite_lf_restriction(Atom, Atom, _Context) :-
	atomic(Atom),
	!.
rewrite_lf_restriction([F | R], [F1 | R1], Context) :-
	rewrite_lf_restriction_element(F, F1, Context),
	!,
	rewrite_lf_restriction(R, R1, Context).

rewrite_lf_restriction_element(Form, Form1, Context) :-
	user:lf_rewrite(Form, Form1, Conds),
	conditions_match_context(Conds, Context),
	!.
rewrite_lf_restriction_element(Other, Other, _Context) :-
	!.

%--------------------------------------------------------

conditions_match_context(true, _Context) :-
	!.
conditions_match_context(P, Context) :-
	member(P, Context),
	!.
conditions_match_context((P, Q), Context) :-
	!,
	conditions_match_context(P, Context),
	conditions_match_context(Q, Context).
conditions_match_context((P ; Q), Context) :-
	!,
	(   conditions_match_context(P, Context)
	;
	    conditions_match_context(Q, Context)
	).
