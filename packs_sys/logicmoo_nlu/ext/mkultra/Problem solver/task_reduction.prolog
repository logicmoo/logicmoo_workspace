%%
%% Goal reduction
%%

task_reduction(Task, Reduction) :-
   canonical_form_of_task(Task, Canonical),
   matching_strategies(Strategies, Canonical),
   selected_reduction(Canonical, Reduction, Strategies).

selected_reduction(_, S, [S]).
selected_reduction(Task, resolve_match_failure(Task), [ ]) :-
   emit_grain("match fail", 10).
selected_reduction(Task, resolve_conflict(Task, Strategies), Strategies) :-
   emit_grain("match conflict", 10).

%%
%% Canonical forms
%%

canonical_form_of_task(Task, Canon) :-
   normalize_task(Task, Normalized),
   canonical_form_of_task(Normalized, Canon),
   !.
canonical_form(Task, Task).

%%
%% Standard normalizations
%%

normalize_task(if(Condition, Then, _Else),
	  Then) :-
   Condition,
   !.
normalize_task(if(_, _, Else),
	  Else).
normalize_task(cases(CaseList),
	       S) :-
   member(C:S, CaseList),
   C,
   !.

normalize_task(begin(A, B),
	       (A, B)).
normalize_task(begin(A, B, C),
	       (A, B, C)).
normalize_task(begin(A, B, C, D),
	       (A, B, C, D)).
normalize_task(begin(A, B, C, D, E),
	       (A, B, C, D, E)).
normalize_task(begin(A, B, C, D, E, F),
	       (A, B, C, D, E, F)).xs


%%
%% Matching strategies to tasks
%%

matching_strategies(Strategies, Task) :-
   all(S,
       matching_strategy(S, Task),
       Strategies).

%% matching_strategy(-S, +Task)
%  S is a strategy for Task.
matching_strategy(S, Task) :-
   (personal_strategy(Task, S) ; strategy(Task, S)),
   emit_grain("Default", 3),
   \+ veto_strategy(Task).

%%
%% Debugging tools
%%

%% show_decomposition(+Task)
%  Prints the series of decompositions of Task until it reaches a point where
%  it's not further decomposable, or the decomposition is non-unique.
show_decomposition(Task) :-
   writeln(Task),
   task_reduction(Task, Reduced),
   show_decomposition_aux(Reduced).

show_decomposition_aux(resolve_match_failure(_)) :-
   writeln('-> no further decompositions possible').
show_decomposition_aux(resolve_conflict(_, ListOfReductions)) :-
   writeln('->'),
   forall(member(R, ListOfReductions),
	  begin(write('   '),
		writeln(R))).
show_decomposition_aux(UniqueDecomposition) :-
   write('-> '),
   writeln(UniqueDecomposition),
   task_reduction(UniqueDecomposition, Reductions),
   show_decomposition_aux(Reductions).
