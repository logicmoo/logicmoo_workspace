:- module(compact_goal, [compact_goal/2]).

% compact_goal/2 is the opposite of expand_goal/2, and is intended to improve
% display of goals that where expanded by the compiler. It is only approximated
% and should not be used in other context.
%
compact_goal(M:Goal, N:Compact) :- !,
    strip_module(M:Goal, N, Head),
    compact_goal(Head, Compact).
compact_goal(Goal, Compact) :-
    ( member(Pattern-Into,
	     [(\+ (G, \+E))           -forall(G, E),
	      forall(retract(F), true)-retractall(F)]),
      subsumes_term(Pattern, Goal)
    ->Goal = Pattern,
      Goal2 = Into,
      compact_goal(Goal2, Compact)
    ; Compact = Goal
    ).

prolog:called_by(\+ (F, \+ E), system, M, CL) :-
    nonvar(F),
    F = retract(Fact),
    E==true,
    ( prolog:called_by(retractall(Fact), system, M, CL)
    ->true
    ; CL = [true]		% Skip walk meta arguments
    ).
