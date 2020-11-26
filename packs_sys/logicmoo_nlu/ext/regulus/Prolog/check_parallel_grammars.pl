
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(check_parallel_grammars,
	  [check_parallel_grammars/2]
      ).

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%-----------------------------------------------------------------------------------

check_parallel_grammars(File1, File2) :-
	format('~N--- Checking parallel grammars: ~w vs ~w~n~n', [File1, File2]),
	safe_absolute_file_name(File1, AbsFile1),
	safe_absolute_file_name(File2, AbsFile2),
	safe_prolog_file_to_list_printing_statistics(AbsFile1, List1),
	safe_prolog_file_to_list_printing_statistics(AbsFile2, List2),
	(   check_parallel_grammar_lists(List1, List2) ->
	    format('~N~n--- Grammars appear to be parallel~n~n', [])
	;
	    otherwise ->
	    format('~N~n*** Error: grammars do not appear to be parallel~n~n', [])
	).

check_parallel_grammar_lists([], []) :-
	!.
check_parallel_grammar_lists([F | _], []) :-
	 format('~N*** Error: mismatched item in first file: ~w~n~n', [F]),
	 fail.
check_parallel_grammar_lists([], [F | _]) :-
	 format('~N*** Error: mismatched item in second file: ~w~n~n', [F]),
	 fail.
check_parallel_grammar_lists([F | R], [F1 | R1]) :-
	 (   check_parallel_grammar_list_items(F, F1) ->
	     true
	 ;
	     otherwise ->
	     format('~N~n*** Error: mismatched items:~n~n', []),
	     prettyprint(F),
	     format('~N~n', []),
	     prettyprint(F1),
	     format('~N~n', []),
	     fail
	 ),
	 !,
	 check_parallel_grammar_lists(R, R1).

check_parallel_grammar_list_items(F, F) :-
	!.
check_parallel_grammar_list_items(Item1, Item2) :-
	terms_same_except_for_atoms(Item1, Item2).

terms_same_except_for_atoms(V1, V2) :-
	var(V1),
	var(V2),
	!.
terms_same_except_for_atoms(X, Y) :-
	nonvar(X),
	nonvar(Y),
	X = Y,
	!.
terms_same_except_for_atoms(X, Y) :-
	atomic(X),
	atomic(Y),
	!.
terms_same_except_for_atoms(Term1, Term2) :-
	compound(Term1),
	compound(Term2),
	functor(Term1, F, N),
	functor(Term2, F, N),
	terms_same_except_for_atoms_args(N, Term1, Term2).

terms_same_except_for_atoms_args(I, _Term1, _Term2) :-
	I < 1,
	!.
terms_same_except_for_atoms_args(I, Term1, Term2) :-
	I >= 1,
	arg(I, Term1, Arg1),
	arg(I, Term2, Arg2),
	terms_same_except_for_atoms(Arg1, Arg2),
	I1 is I - 1,
	!,
	terms_same_except_for_atoms_args(I1, Term1, Term2).

