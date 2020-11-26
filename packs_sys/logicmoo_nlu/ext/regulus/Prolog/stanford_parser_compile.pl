
:- module(stanford_parser_compile,
	  [build_reach_tables/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/progress').
:- use_module('$REGULUS/Prolog/regulus_declarations').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

:- dynamic reachable_cat_helper/2, reachable_word/2, reaches_gap/1.

%---------------------------------------------------------------

build_reach_tables(OutputFile) :-
	build_reach_tables1,

	format('~N-- Writing out tables~n', []),
	write_tables_to_file(OutputFile).

%---------------------------------------------------------------

write_tables_to_file(OutputFile) :-
	open_regulus_file(OutputFile, write, S),
	write_tables_to_stream(S),
	close(S).

write_tables_to_stream(S) :-
	write_reachable_cat_helper_to_stream(S),
	write_reachable_word_to_stream(S),
	write_reaches_gap_to_stream(S),
	write_known_empty_to_stream(S),
	write_indexed_rule_3_to_stream(S),
	write_indexed_rule_4_to_stream(S).

write_reachable_cat_helper_to_stream(S) :-
	reachable_cat_helper(X, Y),
	portray_clause(S, reachable_cat_helper(X, Y)),
	fail.
write_reachable_cat_helper_to_stream(S) :-
	portray_clause(S, ( reachable_cat_helper(_X, _Y) :- fail )).

write_reachable_word_to_stream(S) :-
	reachable_word(X, Y),
	portray_clause(S, reachable_word(X, Y)),
	fail.
write_reachable_word_to_stream(S) :-
	portray_clause(S, ( reachable_word(_X, _Y) :- fail )).

write_reaches_gap_to_stream(S) :-
	reaches_gap(X),
	portray_clause(S, reaches_gap(X)),
	fail.
write_reaches_gap_to_stream(S) :-
	portray_clause(S, ( reaches_gap(_X) :- fail )).

write_known_empty_to_stream(S) :-
	user:(Mother ---> []),
	portray_clause(S, known_empty(Mother)),
	fail.
write_known_empty_to_stream(S) :-
	portray_clause(S, ( known_empty(_X) :- fail )).

write_indexed_rule_3_to_stream(S) :-
	user:(Mother ---> Children),
	Children = [First|Rest],
	atomic(First),
	portray_clause(S, indexed_rule(First, Mother, Rest)),
	fail.
write_indexed_rule_3_to_stream(S) :-
	portray_clause(S, ( indexed_rule(_X, _Y, _Z) :- fail )).

write_indexed_rule_4_to_stream(S) :-
	user:(Mother ---> Children),
	Children = [First|Rest],
	First = Cat/Sem,
	portray_clause(S, indexed_rule(Cat, Sem, Mother, Rest)),
	fail.
write_indexed_rule_4_to_stream(S) :-
	portray_clause(S, ( indexed_rule(_X, _Y, _Z, _W) :- fail )).

%---------------------------------------------------------------

build_reach_tables1 :-
	format('~N-- Building reachable_cat~n', []),
	retractall(reachable_cat_helper(_,_)),
	user:(Mother ---> RHS),
	(Cat/_Sem) = Mother,
	functor(Cat,Functor,Arity),
	functor(Cat1,Functor,Arity),
	((assert_general(reachable_cat_helper(Cat1,Cat1)), fail);
%	((assert_general(reachable_cat(Cat1,Sem1,Cat1,Sem1)), fail);
%	((assert_general(reachable_cat(Cat/Sem1,Cat/Sem1)), fail);
	 ([(Daughter/_)|_] = RHS,
	  build_reach_table_helper(Cat1,Daughter))). % This should always fail eventually.
build_reach_tables1 :-
	add_progress_line(reachable_cat),
	format('~N-- Building reachable_word~n', []),
	retractall(reachable_word(_,_)),
	user:(Mother ---> [Daughter|_]),
	atomic(Daughter),
	\+ user:(Daughter ---> _),
	reachable_cat(Ancestor,Mother),
	assert_general(reachable_word(Ancestor,Daughter)),
	fail.	
build_reach_tables1 :-
	add_progress_line(reachable_word),
	format('~N-- Building reaches_gap~n', []),
	retractall(reaches_gap(_)),
	user:(Mother ---> []),
	assert_general(reaches_gap(Mother)),
	reachable_cat(Ancestor,Mother),
	assert_general(reaches_gap(Ancestor)),
	fail.	
build_reach_tables1 :-
	add_progress_line(reachable_gap).

build_reach_table_helper(Ancestor,Descendant) :-
	\+ \+ user:(Descendant/_ ---> _),
	cutoff_term(Ancestor,Ancestor1,[]),
	cutoff_term(Descendant,Descendant1,[]),
	assert_general(reachable_cat_helper(Ancestor1,Descendant1)),
	user:(Descendant1/_ ---> [Descendant2/_|_]),
	build_reach_table_helper(Ancestor1,Descendant2).

cutoff_term(Var,Var,_) :-
	var(Var),
	!.
cutoff_term(Term,PrunedTerm,FunctorList) :-
	functor(Term,Functor,Arity),
	(memberchk(Functor,FunctorList), Functor \== '.' ->
	    PrunedTerm = _;
	    (functor(PrunedTerm,Functor,Arity),
	     cutoff_args(Arity,Term,PrunedTerm,[Functor|FunctorList]))).

cutoff_args(0,_Term,_PrunedTerm,_FunctorList) :-
	!.
cutoff_args(ArgCount,Term,PrunedTerm,FunctorList) :-
	arg(ArgCount,Term,TermArg),
	arg(ArgCount,PrunedTerm,PrunedTermArg),
	cutoff_term(TermArg,PrunedTermArg,FunctorList),
	NewArgCount is ArgCount - 1,
	cutoff_args(NewArgCount,Term,PrunedTerm,FunctorList).

cutoff_list([], []).
cutoff_list([First|Rest], [CutoffFirst|CutoffRest]):-
	cutoff_term(First, CutoffFirst, []),
	cutoff_list(Rest, CutoffRest).
	
%---------------------------------------------------------------

reachable_cat((Cat1/_Sem1), (Cat2/_Sem2)):-
	reachable_cat_helper(Cat1, Cat2).

%---------------------------------------------------------------

assert_general(A) :-
	\+ A,
	!,
	assert(A).

assert_general(A) :-
	numbervars(A, 0, _),
	A,
	!,
	fail.

assert_general(A) :-
	copy_term(A,A1),
	clause(A1,true,Ref),
	clause(A2,true,Ref),
	numbervars(A2,0,_),
	A = A2,
	erase_safe(clause(A2,true,Ref),Ref),
	fail.

assert_general(A) :-
	assert(A).

