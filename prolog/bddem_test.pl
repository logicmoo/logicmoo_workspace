:- module(test_bddem,
  [test_bddem/0]).
:- use_module(library(plunit)).
:- ensure_loaded(library(bddem)).

:- rand_seed(100).

test_bddem:-
    run_tests([
    prob,em
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



:- end_tests(em).
