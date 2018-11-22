:- module(test_bddem,
  [test_bddem/0]).
:- use_module(library(plunit)).
:- ensure_loaded(library(bddem)).

:- rand_seed(100).

test_bddem:-
    run_tests([
    prob,em,
    sampling
    ]).


v1_0(Env,R,BDD):-
  add_var(Env,[0.4,0.6],R,V),equality(Env,V,0,BDD).


:- begin_tests(prob, []).

:-ensure_loaded(library(bddem)).

test(one):-
  init(Env),
  v1_0(Env,0,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=0.4.

test(and):-
  init(Env),
  v1_0(Env,0,BDD1),
  v1_0(Env,0,BDD2),
  and(Env,BDD1,BDD2,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=0.4*0.4.

test(or):-
  init(Env),
  v1_0(Env,0,BDD1),
  v1_0(Env,0,BDD2),
  or(Env,BDD1,BDD2,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=0.4+0.4-0.4*0.4.

test(nor):-
  init(Env),
  v1_0(Env,0,BDD1),
  v1_0(Env,0,BDD2),
  or(Env,BDD1,BDD2,BDDN),
  bdd_not(Env,BDDN,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=1-(0.4+0.4-0.4*0.4).



:- end_tests(prob).

:- begin_tests(em, []).

:-ensure_loaded(library(bddem)).

test(one):-
  init_em(Cont),
  ex1(Cont,BDD1),
  ex1(Cont,BDD2),
  ex2(Cont,BDD3),
  em(Cont,[2,2],[[BDD1,1.0],[BDD2,1.0],[BDD3,1.0]],0.0001,0.001,100,LL,Par,ExP),
  writeln(LL),
  writeln(Par),
  writeln(ExP),
  end_em(Cont),
  abs(LL)<  1e-4.

ex1(Cont,BDD):-
  init_ex(Cont,Env),
  v1_0(Env,0,B0),
  v1_0(Env,1,B1),
  or(Env,B0,B1,BDD),
  end_ex(Cont).

ex2(Cont,BDD):-
  init_ex(Cont,Env),
  v1_0(Env,0,B00),
  v1_0(Env,1,B1),
  bdd_not(Env,B00,B0),
  or(Env,B0,B1,BDD),
  end_ex(Cont).

test(one_dir):-
  init_em(Cont),
  ex1(Cont,BDD1),
  ex1(Cont,BDD2),
  ex2(Cont,BDD3),
  initial_values(Cont,1.0),
  em(Cont,[2,2],[[BDD1,1.0],[BDD2,1.0],[BDD3,1.0]],0.0001,0.001,100,LL,Par,ExP),
  writeln(LL),
  writeln(Par),
  writeln(ExP),
  end_em(Cont),
  abs(LL)<  1e-3.

:- end_tests(em).

:- begin_tests(sampling, []).

:-ensure_loaded(library(bddem)).
:- use_module(library(apply)).
relatively_close_to(V,T,E):-
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow=<V,
	V=<THigh.

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow=<V,
	V=<THigh.


average([H|T],Av):-
  sum_list([H|T],Sum),
  length([H|T],N),
  Av is Sum/N.

variance(L,Av,Var):-
  average(L,Av),
  maplist(sq_diff(Av),L,LS), 
  average(LS,Var).

std_dev(L,Av,Dev):-
  variance(L,Av,Var),
  root(Var,Dev).

root(Var,Dev):-
  Dev is sqrt(Var).

sq_diff(Av,A,S):-
  S is (A-Av)^2.

is0(0).

is1(1).

is2(2).

is3(3).

test(gamma):-
  findall(S,(between(1,10000,_),gamma_sample(1,2,S)),V),
  variance(V,M,Var),
  writeln(mean(M)),
  writeln(var(Var)),
  relatively_close_to(M,1*2,0.2),
  relatively_close_to(Var,1*2*2,0.2).

test(gauss):-
  findall(S,(between(1,10000,_),gauss_sample(1,2,S)),V),
  variance(V,M,Var),
  writeln(mean(M)),
  writeln(var(Var)),
  relatively_close_to(M,1,0.1),
  relatively_close_to(Var,2,0.1).


test(uniform):-
  findall(S,(between(1,10000,_),uniform_sample(S)),V),
  variance(V,M,Var),
  writeln(mean(M)),
  writeln(var(Var)),
  relatively_close_to(M,0.5,0.1),
  relatively_close_to(Var,1/12,0.1).

test(dirichlet):-
  findall(S,(between(1,10000,_),dirichlet_sample([1,1,1],S)),_V).

test(dirichlet1):-
  findall(S,(between(1,10000,_),dirichlet_sample([1,1,1,1],D),discrete_sample(D,S)),V),
  check_sample(V).

test(dirichlet2):-
  findall(S,(between(1,10000,_),dirichlet_sample([2,2,2,2],D),discrete_sample(D,S)),V),
  check_sample(V).

test(sdirichlet1):-
  findall(S,(between(1,10000,_),symmetric_dirichlet_sample(1,4,D),discrete_sample(D,S)),V),
  check_sample(V).

test(sdirichlet2):-
  findall(S,(between(1,10000,_),symmetric_dirichlet_sample(2,4,D),discrete_sample(D,S)),V),
  check_sample(V).

test(discrete):-
  findall(S,(between(1,10000,_),discrete_sample([0.25,0.25,0.25,0.25],S)),V),
  check_sample(V).

check_sample(V):-
  partition(is0,V,L0,_),
  partition(is1,V,L1,_),
  partition(is2,V,L2,_),
  partition(is3,V,L3,_),
  length(L0,N0),
  length(L1,N1),
  length(L2,N2),
  length(L3,N3),
  writeln(N0),
  writeln(N1),
  writeln(N2),
  writeln(N3),
  relatively_close_to(N0,2500,0.1),
  relatively_close_to(N1,2500,0.1),
  relatively_close_to(N2,2500,0.1),
  relatively_close_to(N3,2500,0.1).
:- end_tests(sampling).