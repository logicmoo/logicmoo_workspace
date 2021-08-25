:-use_module(library(cnf)).
:-use_module(library(minisat)).
:- use_module(library(lists)).

de_morgan(A,-A):-
  var(A),!.
de_morgan(A+D,-A*D1):-!,
  de_morgan(D,D1).
de_morgan(A*D,-A+D1):-!,
  de_morgan(D,D1).
  
give_me_C(H,D,Var0,Var0):-
  member(corr(H,D),Var0),!.
give_me_C(H,C,Var0,[corr(H,C)|Var0]).
  
  
test(L1,L2):-
  build_f(L1,L2,F),
  cnf(F,Cnf),
  sat(Cnf).
  

build_f([L1],[L2],F1*(-F2)):-
  build_f1(L1,F1,[],Var1),
  build_f1(L2,F2,Var1,Var).

build_f1(and(L),F,Var0,Var):-!,
  build_and(L,F,Var0,Var).
build_f1(or(L),F,Var0,Var):-!,
  build_or(L,F,Var0,Var).
build_f1(H,C,Var0,Var):-
  give_me_C(H,C,Var0,Var).
  
build_and([H|T],F,Var0,Var):-
  T==[],
  build_f1(H,F,Var0,Var).

build_and([H|T],F1 * F2,Var0,Var):-
  build_f1(H,F1,Var0,Var1),
  build_and(T,F2,Var1,Var).
  
build_or([H|T],F,Var0,Var):-
  T==[],
  build_f1(H,F,Var0,Var).

build_or([H|T],F1 + F2,Var0,Var):-
  build_f1(H,F1,Var0,Var1),
  build_or(T,F2,Var1,Var).  
