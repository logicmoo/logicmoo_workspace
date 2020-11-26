:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(pre_process_for_paraphrasing,
	[pre_process_for_paraphrasing/2
	]).


%======================================================================

:- use_module('$REGULUS/PrologLib/utilities').
%:- use_module(library(lists)).

%======================================================================

pre_process_for_paraphrasing(DialogueMove, DialogueMove1) :-
	copy_term(DialogueMove, DialogueMove1),
	make_ground_using_letters(DialogueMove1),
	!.
pre_process_for_paraphrasing(DialogueMove, DialogueMove1) :-
	format('~N*** Error: bad call: ~w~n', [pre_process_for_paraphrasing(DialogueMove, DialogueMove1)]),
	fail.

%----------------------------------------------------------------------

make_ground_using_letters(Term) :-
	letter_list(InList),
	make_ground_using_letters(Term, InList-_),
	!.
make_ground_using_letters(Term) :-
	format('~N*** Error: bad call: ~w~n', [make_ground_using_letters(Term)]),
	fail.

letter_list(['A', 'B', 'C', 'D', 'E',
	     'F', 'G', 'H', 'I', 'J',
	     'K', 'L', 'M', 'N', 'O',
	     'P', 'Q', 'R', 'S', 'T']).

make_ground_using_letters(Var, [F | R]-R) :-
	var(Var),
	Var = F,
	!.
make_ground_using_letters(A, In-In) :-
	atomic(A),
	!.
make_ground_using_letters(Term, In-Out) :-
	compound(Term),
	functor(Term, _F, N),
	make_ground_using_letters_args(1, N, Term, In-Out),
	!.

make_ground_using_letters_args(I, N, _Term, In-In) :-
	I > N,
	!.
make_ground_using_letters_args(I, N, Term, In-Out) :-
	arg(I, Term, Arg),
	make_ground_using_letters(Arg, In-Next),
	I1 is I + 1,
	!,
	make_ground_using_letters_args(I1, N, Term, Next-Out).

