%% this is a planning problem from C. Elkan,  "Incremental Approximate
%% Planning: Abductive Default Reasoning, AAAI Spring Symposium on
%% Automated Abduction, 1990.

%% holds(proposition,state).
%% causes(action,state,new_state).
%% can(action,state).


%% initial state of the world.
holds(in(christian,arena),s0).
holds(in(lion,cage),s0).
holds(in(trainer,cage),s0).

%% rules for how the world evolves:

holds(P,do(S,A)) :- causes(A,S,P).

holds(P,do(S,A)) :- nonvar(S), holds(P,S), \+ cancels(A,S,P).

% if you are eating an X, then the X is where you are.

holds(in(X,H),S) :-
  holds(eats(lion,X),S),
  holds(in(lion,H),S).


%% the effects of actions:

causes(pounce(lion,X),S,eats(lion,X)) :- can(pounce(lion,X),S).

causes(jump(X),S,in(X,arena)) :-
  can(jump(X),S),
  holds(in(X,cage),S).

cancels(drop(X,Y),S,eats(X,Y)) :- can(drop(X,Y),S).

%% preconditions on actions.

can(pounce(X,Y),S) :- 
  holds(in(X,L),S),
  holds(in(Y,L),S),
  X\==Y,
  \+ holds(eats(X,_),S).

can(jump(lion),S) :- holds(eats(lion,trainer),S).

can(drop(X,Y),S) :- holds(eats(X,Y),S).


query :- holds(eats(lion,christian),State).
