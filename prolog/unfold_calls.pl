:- module(unfold_calls,
	  [unfold_calls/4]).

:- use_module(library(implementation_module)).
:- use_module(library(qualify_meta_goal)).

:- multifile
    unfold_call_hook/4.

:- meta_predicate unfold_calls(+, +, 2, -).
unfold_calls(Goal, CM, IsUnfold, Calls) :-
    implementation_module(CM:Goal, M),
    findall(Call, ( unfold_call(Goal, CM, IsUnfold, [], Call),
		    \+ M:Goal =@= Call
		  ), Calls).

unfold_call(Goal, _, _, _, _) :- var(Goal), !, fail.
unfold_call(true, _, _, _, _) :- !, fail.
unfold_call(CM:Goal, _, IsUnfold, NonUnfoldL, Call) :-
    nonvar(Goal), !,
    unfold_call(Goal, CM, IsUnfold, NonUnfoldL, Call).
unfold_call(Goal0, CM, IsUnfold, NonUnfoldL, Call) :-
    CMGoal0 = CM:Goal0,
    implementation_module(CMGoal0, M),
    qualify_meta_goal(CMGoal0, Goal),
    ( unfold_call_hook(Goal, M, CM, Goal2)
    *->
      unfold_call(Goal2, CM, IsUnfold, NonUnfoldL, Call)
    ; \+ \+ memberchk(M:Goal, NonUnfoldL)
    ->Call = CM:Goal
    ; \+ call(IsUnfold, Goal, M),
      \+ ( predicate_property(CM:Goal, meta_predicate(S)),
	   arg(_, S, 0 )
	 )
    ->Call = CM:Goal
    ; ( nth_clause(CM:Goal, _Idx, Ref),
	clause(M:Head, Body, Ref),
	Body \== call(Head)
      *->
	clause_property(Ref, module(BM)),
	( subsumes_term(Head, Goal)
	->Goal = Head,
	  unfold_call(Body, BM, IsUnfold, NonUnfoldL, Call)
	; \+ Head \= Goal
	->copy_term(Goal, Head),
	  unfold_call(Body, BM, IsUnfold, [M:Head|NonUnfoldL], Call)
				% Abstraction to get more info
	)
      ; predicate_property(CM:Goal, meta_predicate(Spec)),
	( arg(N, Spec, S),
	  arg(N, Goal, A),
	  S = 0,
	  unfold_call(A, CM, IsUnfold, NonUnfoldL, Call)
	)
      )
    ).
