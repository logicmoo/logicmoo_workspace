:- use_foreign_library(foreign(bddem),install).

%:-dynamic rule_n/1.

%rule_n(0).

compute_prob(Expl,Prob):-
  init_test(_,Env),
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  build_bdd(Env,Expl,BDD),
  ret_prob(Env,BDD,Prob),
  end_test(Env), !.
  
  


build_bdd(Env,[X],BDD):- !,
  %write('1'),nl,
  writel(X),nl,
  bdd_and(Env,X,BDD).
  
build_bdd(Env, [H|T],BDD):-
  %write('2'),nl,
  build_bdd(Env,T,BDDT),
  bdd_and(Env,H,BDDH),
  or(Env,BDDH,BDDT,BDD).
  
build_bdd(Env,[],BDD):- !,
  %write('3'),nl,
  zero(Env,BDD).
  
  
bdd_and(Env,[X],BDDX):-
  %write('bdd_and-1: '),write(X),nl,
  get_prob_ax(X,AxN,Prob),!,
  %write('   '),write(Prob),nl,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.
bdd_and(Env,[_X],BDDX):- !,
  %write('bdd_and-1: '),write(X),nl,write('   1'),nl,
  one(Env,BDDX).
  
bdd_and(Env,[H|T],BDDAnd):-
  %write('bdd_and-2: '),write(H),nl, 
  get_prob_ax(H,AxN,Prob),!,
  %write('   '),write(Prob),nl,
  ProbN is 1-Prob,
  %write('bdd_and-2: ProbN'),nl,
  get_var_n(Env,AxN,[],[Prob,ProbN],VH),
  %write('bdd_and-2: get_var_n'),nl, 
  equality(Env,VH,0,BDDH),
  %write('bdd_and-2: equality'),nl,
  bdd_and(Env,T,BDDT),
  %write('bdd_and-2: bdd_and'),nl,
  and(Env,BDDH,BDDT,BDDAnd).
  
bdd_and(Env,[_H|T],BDDAnd):- !,
  %write('bdd_and-2: '),write(H),nl,write('   1'),nl,
  one(Env,BDDH),
  bdd_and(Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).



  
get_var_n(Env,R,S,Probs,V):-
  ( 
    v(R,S,V) -> 
      true
    ; 
      length(Probs,L),
      %trace,
      add_var(Env,L,Probs,R,V),
      %notrace,
      assert(v(R,S,V))
  ).


get_prob_ax((Ax,_Ind),N,Prob):- !,
  pengine_property(_Pengine1,self(Name)),
  Name:annotationAssertion('https://sites.google.com/a/unife.it/ml/bundle#probability',Ax,literal(ProbA)),
  atom_number(ProbA,Prob),
  ( na(Ax,N) -> 
      true
    ;
      rule_n(N),
      assert(na(Ax,N)),
      retract(rule_n(N)),
      N1 is N + 1,
      assert(rule_n(N1))
  ).
get_prob_ax(Ax,N,Prob):- !,
  pengine_property(_Pengine1,self(Name)),
  Name:annotationAssertion('https://sites.google.com/a/unife.it/ml/bundle#probability',Ax,literal(ProbA)),
  atom_number(ProbA,Prob),
  ( na(Ax,N) -> 
      true 
    ; 
      rule_n(N),
      assert(na(Ax,N)),
      retract(rule_n(N)),
      N1 is N + 1,
      assert(rule_n(N1))
  ).
