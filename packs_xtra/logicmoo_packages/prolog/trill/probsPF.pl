:-use_module(library(lists)).
%:-[-'FML'].
:-load_foreign_files(['bddem'],[],init_my_predicates).

% computes the probability of the formula Expl  
compute_prob(Expl,Prob):-
  init_test(_),
  retractall(v(_,_,_)),
  build_bdd(Expl,BDD), 
  ret_prob(BDD,Prob),
  end_test.
  
% build the BDD
build_bdd([X],BDDX):- \+ is_and(X), \+ is_or(X),bdd_and([X],BDDX),!.
build_bdd([and(F)],BDDF):-
  find_or_in_formula(F,Or),
  build_bdd([or(Or)],BDDOr),
  find_and_in_formula(F,And),
  bdd_and(And,BDDAnd),
  and(BDDAnd,BDDOr,BDDF),!.
build_bdd([and(And)],BDDAnd):- 
  findall(El,(member(El,And),p(El,Prob)), AndNew),
  ( AndNew = [] -> Expl = [] ; (AndNew = [X] -> Expl = [X]; Expl = [and(AndNew)]) ),
  bdd_and(And,BDDAnd),!.
build_bdd([or(F)],BDOr):-
  findall( BDDEl, (member(El,F), build_bdd([El],BDDEl)), BDDList),
  bdd_or(BDDList,BDOr).
  
% boolean operations between BDDs
bdd_or([BDDX],BDDX):- !.
bdd_or([BDDH|BDDT],BDDOr):-
  bdd_or(BDDT,BDDOrT),
  or(BDDH,BDDOrT,BDDOr).
  
bdd_and([X],BDDX):-
  compute_prob_ax(X,Prob),!,
  ProbN is 1-Prob,
  get_var_n([X],[],[Prob,ProbN],VX),
  equality(VX,0,BDDX),!.
bdd_and([X],BDDX):- !,
  one(BDDX).
bdd_and([H|T],BDDAnd):-
  compute_prob_ax(H,Prob),!,
  ProbN is 1-Prob,
  get_var_n([H],[],[Prob,ProbN],VH),
  equality(VH,0,BDDH),
  bdd_and(T,BDDT),
  and(BDDH,BDDT,BDDAnd).
bdd_and([H|T],BDDAnd):- !,
  one(BDDH),
  bdd_and(T,BDDT),
  and(BDDH,BDDT,BDDAnd).
  
% finds the variables
get_var_n(R,S,Probs,V):-
  ( v(R,S,V)-> true; length(Probs,L), add_var(L,Probs,R,V), assert(v(R,S,V)) ).
  

% compute the probability of a given axiom
compute_prob_ax(R,Prob):-
  findall(P, p(R,P),Probs),
  compute_prob_ax1(Probs,Prob).
  
compute_prob_ax1([Prob],Prob):-!.

compute_prob_ax1([Prob1,Prob2],Prob):-!,
  Prob is Prob1+Prob2-(Prob1*Prob2).
  
compute_prob_ax1([Prob1 | T],Prob):-
  compute_prob_ax1(T,Prob0),
  Prob is Prob1 + Prob0 - (Prob1*Prob0).
