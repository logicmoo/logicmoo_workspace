:- lib(fd).
:- lib(chr).
:- chr2pl(fluent_ecl),  [fluent_ecl].
:- local plus/3.

holds(F, [F|_]).
holds(F, Z) :- nonvar(Z), Z=[F1|Z1], \+ F==F1, holds(F, Z1).

holds(F, [F|Z], Z).
holds(F, Z, [F1|Zp]) :- nonvar(Z), Z=[F1|Z1], \+ F==F1, holds(F, Z1, Zp).

minus(Z, [], Z).
minus(Z, [F|Fs], Zp) :-
   ( \+ not_holds(F, Z) -> holds(F, Z, Z1) ;
     \+ holds(F, Z)     -> Z1 = Z
                         ; cancel(F, Z, Z1), not_holds(F, Z1) ),
   minus(Z1, Fs, Zp).

plus(Z, [], Z).
plus(Z, [F|Fs], Zp) :-
   ( \+ holds(F, Z)     -> Z1=[F|Z] ;
     \+ not_holds(F, Z) -> Z1=Z
                         ; cancel(F, Z, Z2), not_holds(F, Z2), Z1=[F|Z2] ),
   plus(Z1, Fs, Zp).

update(Z1, ThetaP, ThetaN, Z2) :-
   minus(Z1, ThetaN, Z), plus(Z, ThetaP, Z2).

knows(F, Z) :- \+ not_holds(F, Z).

knows_not(F, Z) :- \+ holds(F, Z).

knows_val(X, F, Z) :- holds(F, Z), \+ nonground(X).

execute(E, Z1, Z2) :-
   E = [] -> Z2 = Z1
   ;
   E = [A|P] -> execute(P, Z1, Z), execute(A, Z, Z2)
   ;
   elementary_action(E) -> perform(E, SV), state_update(Z1, E, Z2, SV)
   ;
   execute_compound_action(E, Z1, Z2).

:- op(950, xfy, #).
:- dynamic(plan_search_best/2).

plan(Proc, Plan, Z0) :-
   write('Planning ...'), nl,
   assert(plan_search_best(void,0)),
   plan_search(Proc, Z0),
   plan_search_best(Plan,_),
   retract(plan_search_best(Plan,_)),
   Plan \= void.

plan_search(Proc, Z0) :-
   do(Proc, [], Plan, Z0),
   plan_cost(Proc, Plan, Cost),
   plan_search_best(BestPlan,BestCost),
   ( BestPlan \= void -> Cost < BestCost
                       ; true ),
   retract(plan_search_best(BestPlan,BestCost)),
   assert(plan_search_best(Plan,Cost)),
   fail
   ;
   true.

do(E, S0, S, Z0) :-
   E = [] -> S=S0
   ;
   E = [E1|L] -> do(E1, S0, S1, Z0),
                 do(L, S1, S, Z0)
   ;
   E = (E1#E2) -> ( do(E1, S0, S, Z0) ; do(E2, S0, S, Z0) )
   ;
   plan_proc(E, E1) -> do(E1, S0, S, Z0)
   ;
   E = ?(P) -> P =.. [Pred|Args],
               append(Args, [S0,Z0], ExtArgs),
               P1 =.. [Pred|ExtArgs],
               call(P1),
	       S = S0
   ;
   elementary_action(E) -> S = [E|S0]
   ;
   compound_action(E) -> S = [E|S0].

res([], Z, Z).
res([A|S], Z0, Z) :-
   res(S, Z0, Z1),
   state_update(Z1, A, Z, _).

knows(F, S, Z0) :- \+ ( res(S, Z0, Z), not_holds(F,Z) ).

knows_not(F, S, Z0) :- \+ ( res(S, Z0, Z), holds(F,Z) ).

:- dynamic(knowledge_value_sit/1).

knows_val(X, F, S, Z0) :-
   res(S, Z0, Z) ->
     knows_val(X, F, Z),
     assert(knowledge_value_sit(X)),
     fail.
knows_val(X, F, S, Z0) :-
   knowledge_value_sit(X),
   retract(knowledge_value_sit(X)),
   \+ ( res(S, Z0, Z), not_holds(F, Z) ).

