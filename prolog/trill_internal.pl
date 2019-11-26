/* trill predicates

This module performs reasoning over probabilistic description logic knowledge bases.
It reads probabilistic knowledge bases in RDF format or in Prolog format, a functional-like
sintax based on definitions of Thea library, and answers queries by finding the set 
of explanations or computing the probability.

[1] http://vangelisv.github.io/thea/

See https://github.com/rzese/trill/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~rzese/software/trill/manual.html for
details.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/

/********************************
  SETTINGS
*********************************/
:- multifile setting_trill/2.
setting_trill(det_rules,[o_rule,and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule,min_rule]).
setting_trill(nondet_rules,[or_rule,max_rule]).

set_up(M):-
  utility_translation:set_up(M),
  M:(dynamic exp_found/2),
  M:(dynamic exp_found/3).

clean_up(M):-
  utility_translation:clean_up(M),
  M:(dynamic exp_found/2),
  M:(dynamic exp_found/3),
  retractall(M:exp_found(_,_)),
  retractall(M:exp_found(_,_,_)).

/***********
  Utilities for queries
 ***********/

% to find all axplanations for probabilistic queries
all_sub_class(M:ClassEx,SupClassEx,Exps):-
  all_unsat(M:intersectionOf([ClassEx,complementOf(SupClassEx)]),Exps).

all_instanceOf(M:ClassEx,IndEx,Exps):-
  findall(Expl,instanceOf(M:ClassEx,IndEx,Expl),Exps).

all_property_value(M:PropEx,Ind1Ex,Ind2Ex,Exps):-
  findall(Expl,property_value(M:PropEx,Ind1Ex,Ind2Ex,Expl),Exps).

all_unsat(M:ConceptEx,Exps):-
  findall(Expl,unsat_internal(M:ConceptEx,Expl),Exps).


all_inconsistent_theory(M:Exps):-
  findall(Expl,inconsistent_theory(M:Expl),Exps).


compute_prob_and_close(M,Exps,Prob):-
  compute_prob(M,Exps,Prob).

% checks the explanation
check_and_close(_,Expl,Expl):-
  dif(Expl,[]).


find_expls(M,LABoxes,Q,E):-%gtrace,
  length(LABoxes,N),!,
  find_expls(M,N,LABoxes,Q,E).

% checks if an explanations was already found
find_expls(M,N,[],[C,I],E):-
  findall(CPsS-Exp,M:exp_found([C,I,CPsS],Exp),Expl),
  findall(CPs,member(CPs-_,Expl),CPsL0),
  sort(CPsL0,CPsL),gtrace,
  combine_expls_from_nondet_rules(CPsL,Expl,N,E).

% checks if an explanations was already found (instance_of version)
find_expls(M,_,[ABox|_T],[C,I],E):-
  clash(M,ABox,EL0),
  member(E0,EL0),
  sort(E0,E),
  findall(CP-N,member(cp(CP,N),E),CPs),
  (dif(CPs,[]) ->
    (sort(CPs,CPsS),
    findall(Exp,M:exp_found([C,I,CPsS],Exp),Expl),
    not_already_found(M,Expl,[C,I,CPsS],E),
    assert(M:exp_found([C,I,CPsS],E)),
    fail
    )
    ;
    (findall(Exp,M:exp_found([C,I],Exp),Expl),
     not_already_found(M,Expl,[C,I],E),
     assert(M:exp_found([C,I],E))
    )
  ).

% checks if an explanations was already found (property_value version)
find_expls(M,_,[(ABox,_)|_T],[PropEx,Ind1Ex,Ind2Ex],E):-
  find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),Es),ABox),
  member(E,Es),
  findall(Exp,M:exp_found([PropEx,Ind1Ex,Ind2Ex],Exp),Expl),
  not_already_found(M,Expl,[PropEx,Ind1Ex,Ind2Ex],E),
  assert(M:exp_found([PropEx,Ind1Ex,Ind2Ex],E)).

find_expls(M,N,[_ABox|T],Query,Expl):-
  %\+ length(T,0),
  find_expls(M,N,T,Query,Expl).

combine_expls_from_nondet_rules([],_Expl,_N,[]).

combine_expls_from_nondet_rules([CPs|_T],Expl,_N,E):-
  member(CP-N,CPs),
  findall(Exp,member(CP-N-Exp,Expl),Expls),
  length(Expls,N),
  flatten(Expls,ExplsF),
  sort(ExplsF,E).

combine_expls_from_nondet_rules([_CP|T],Expl,N,E):-
  combine_expls_from_nondet_rules(T,Expl,N,E).

not_already_found(_M,[],_Q,_E):-!.

not_already_found(_M,[H|_T],_Q,E):-
  subset(H,E),!,
  fail.

not_already_found(M,[H|_T],Q,E):-
  subset(E,H),!,
  retract(M:exp_found(Q,H)).

not_already_found(M,[_H|T],Q,E):-
  not_already_found(M,T,Q,E).

/****************************/

/****************************
  TABLEAU ALGORITHM
****************************/

% --------------
findClassAssertion4OWLNothing(_M,ABox,Expl):-
  findClassAssertion('http://www.w3.org/2002/07/owl#Nothing',_Ind,Expl,ABox).


/***********
  rules
************/

/*
  unfold_rule
  ===========
*/

% ----------------
% unionOf, intersectionOf, subClassOf, negation, allValuesFrom, someValuesFrom, exactCardinality(min and max), maxCardinality, minCardinality
:- multifile find_neg_class/2.

find_neg_class(exactCardinality(N,R,C),unionOf([maxCardinality(NMax,R,C),minCardinality(NMin,R,C)])):-
  NMax is N - 1,
  NMin is N + 1.

find_neg_class(minCardinality(N,R,C),maxCardinality(NMax,R,C)):-
  NMax is N - 1.

find_neg_class(maxCardinality(N,R,C),minCardinality(NMin,R,C)):-
  NMin is N + 1.

%-----------------
:- multifile find_sub_sup_class/3.

%role for concepts exactCardinality
find_sub_sup_class(M,exactCardinality(N,R),exactCardinality(N,S),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).

%concept for concepts exactCardinality
find_sub_sup_class(M,exactCardinality(N,R,C),exactCardinality(N,R,D),Ax):-
  find_sub_sup_class(M,C,D,Ax).

%role for concepts exactCardinality
find_sub_sup_class(M,exactCardinality(N,R,C),exactCardinality(N,S,C),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).

%role for concepts maxCardinality
find_sub_sup_class(M,maxCardinality(N,R),maxCardinality(N,S),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).

%concept for concepts maxCardinality
find_sub_sup_class(M,maxCardinality(N,R,C),maxCardinality(N,R,D),Ax):-
  find_sub_sup_class(M,C,D,Ax).

%role for concepts maxCardinality
find_sub_sup_class(M,maxCardinality(N,R,C),maxCardinality(N,S,C),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).

%role for concepts minCardinality
find_sub_sup_class(M,minCardinality(N,R),minCardinality(N,S),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).

%concept for concepts minCardinality
find_sub_sup_class(M,minCardinality(N,R,C),minCardinality(N,R,D),Ax):-
  find_sub_sup_class(M,C,D,Ax).

%role for concepts minCardinality
find_sub_sup_class(M,minCardinality(N,R,C),minCardinality(N,S,C),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).

/* ************* */

/***********
  update abox
  utility for tableau
************/
modify_ABox(_,ABox0,sameIndividual(LF),Expl1,[(sameIndividual(L),Expl)|ABox]):-
  ( find((sameIndividual(L),Expl0),ABox) ->
  	( sort(L,LS),
  	  sort(LF,LFS),
  	  LS = LFS,!,
  	  absent(Expl0,Expl1,Expl),
  	  delete(ABox0,[(sameIndividual(L),Expl0)],ABox)
  	)
  ;
  	(ABox = ABox0,Expl = Expl1)
  ).

modify_ABox(_,ABox0,C,Ind,Expl1,[(classAssertion(C,Ind),Expl)|ABox]):-
  ( find((classAssertion(C,Ind),Expl0),ABox0) ->
    ( absent(Expl0,Expl1,Expl),
      delete(ABox0,(classAssertion(C,Ind),Expl0),ABox)
    )
  ;
    (ABox = ABox0,Expl = Expl1)
  ).

modify_ABox(_,ABox0,P,Ind1,Ind2,Expl1,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox]):-
  ( find((propertyAssertion(P,Ind1,Ind2),Expl),ABox0) ->
    ( absent(Expl0,Expl1,Expl),
      delete(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl0),ABox)
    )
  ;
    (ABox = ABox0,Expl = Expl1)
  ).

/* ************* */

% -------------------
notDifferentIndividuals(M,X,Y,ABox):-
  \+ inAssertDifferentIndividuals(M,X,Y),
  \+ inABoxDifferentIndividuals(X,Y,ABox).

% --------------

inAssertDifferentIndividuals(M,differentIndividuals(X),differentIndividuals(Y)):-
  !,
  M:differentIndividuals(LI),
  member(X0,X),
  member(X0,LI),
  member(Y0,Y),
  member(Y0,LI).

inAssertDifferentIndividuals(M,X,sameIndividual(Y)):-
  !,
  M:differentIndividuals(LI),
  member(X,LI),
  member(Y0,Y),
  member(Y0,LI).

inAssertDifferentIndividuals(M,sameIndividual(X),Y):-
  !,
  M:differentIndividuals(LI),
  member(X0,X),
  member(X0,LI),
  member(Y,LI).

inAssertDifferentIndividuals(M,X,Y):-
  M:differentIndividuals(LI),
  member(X,LI),
  member(Y,LI).

% ------------------

inABoxDifferentIndividuals(sameIndividual(X),sameIndividual(Y),ABox):-
  !,
  find((differentIndividuals(LI),_),ABox),
  member(X0,X),
  member(X0,LI),
  member(Y0,Y),
  member(Y0,LI).

inABoxDifferentIndividuals(X,sameIndividual(Y),ABox):-
  !,
  find((differentIndividuals(LI),_),ABox),
  member(X,LI),
  member(Y0,Y),
  member(Y0,LI).

inABoxDifferentIndividuals(sameIndividual(X),Y,ABox):-
  !,
  find((differentIndividuals(LI),_),ABox),
  member(X0,X),
  member(X0,LI),
  member(Y,LI).

inABoxDifferentIndividuals(X,Y,ABox):-
  find((differentIndividuals(LI),_),ABox),
  member(X,LI),
  member(Y,LI).

% --------------------

listIntersection([],_,[]).

listIntersection([HX|TX],LCY,TI):-
  \+ member(HX,LCY),
  listIntersection(TX,LCY,TI).

listIntersection([HX|TX],LCY,[HX|TI]):-
  member(HX,LCY),
  listIntersection(TX,LCY,TI).

% ---------------

findExplForClassOf(LC,LI,ABox0,Expl):-
  member(C,LC),
  member(I,LI),
  findClassAssertion(C,I,Expl,ABox0).
%  member((classAssertion(C,I),Expl),ABox0).

/* ************ */


/*  absent
  =========
*/
absent(Expl0,Expl1,Expl):- % Expl0 already present expls, Expl1 new expls to add, Expl the combination of two lists
  absent0(Expl0,Expl1,Expl),!.

%------------------
absent0(Expl0,Expl1,Expl):-
  absent1(Expl0,Expl1,Expl,Added),
  dif(Added,0).

absent1(Expl,[],Expl,0).

absent1(Expl0,[H|T],[H|Expl],1):-
  absent2(Expl0,H),!,
  absent1(Expl0,T,Expl,_).

absent1(Expl0,[_|T],Expl,Added):-
  absent1(Expl0,T,Expl,Added).
  
absent2([H],Expl):-
  length([H],1),
  subset(H,Expl) -> fail ; true.

absent2([H|_T],Expl):-
  subset(H,Expl),!,
  fail.

absent2([_|T],Expl):-
  absent2(T,Expl).

/* **************** */

/*
  build_abox
  ===============
*/

/*build_abox(M,ABox):-
  findall((classAssertion(Class,Individual),[classAssertion(Class,Individual)]),classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),[propertyAssertion(Property,Subject, Object)]),propertyAssertion(Property,Subject, Object),LPA),
  findall((propertyAssertion(Property,Subject,Object),[subPropertyOf(SubProperty,Property,Subject,Object),propertyAssertion(SubProperty,Subject,Object)]),subPropertyOf(SubProperty,Property),LSPA),
  new_abox(ABox0),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox).
*/

build_abox(M,(ABox,Tabs)):-
  retractall(M:final_abox(_)),
  findall((classAssertion(Class,Individual),[[classAssertion(Class,Individual)]]),M:classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),[[propertyAssertion(Property,Subject, Object)]]),M:propertyAssertion(Property,Subject, Object),LPA),
  % findall((propertyAssertion(Property,Subject,Object),[subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)]),subProp(M,SubProperty,Property,Subject,Object),LSPA),
  findall(nominal(NominalIndividual),M:classAssertion(oneOf(_),NominalIndividual),LNA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  create_tabs(LCA,Tabs0,Tabs1),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox3),
  add_all(LNA,ABox3,ABox4),
  findall((differentIndividuals(Ld),[[differentIndividuals(Ld)]]),M:differentIndividuals(Ld),LDIA),
  add_all(LDIA,ABox4,ABox5),
  create_tabs(LDIA,Tabs1,Tabs2),
  create_tabs(LPA,Tabs2,Tabs3),
  create_tabs(LSPA,Tabs3,Tabs4),
  findall((sameIndividual(L),[[sameIndividual(L)]]),M:sameIndividual(L),LSIA),
  merge_all(M,LSIA,ABox5,Tabs4,ABox6,Tabs),
  add_nominal_list(ABox6,Tabs,ABox),
  !.


/* ********** */

/**********************

Explanation Management

***********************/

initial_expl(_M,[]):-!.

empty_expl(_M,[]):-!.

and_f_ax(M,Axiom,F0,F):-
  and_f(M,[[Axiom]],F0,F).

and_f(_M,[],[],[]):- !.

and_f(_M,[],L,L):- !.

and_f(_M,L,[],L):- !.

and_f(_M,L1,L2,F):-
  and_f1(L1,L2,[],F).

and_f1([],_,L,L).

and_f1([H1|T1],L2,L3,L):-
  and_f2(H1,L2,L12),
  append(L3,L12,L4),
  and_f1(T1,L2,L4,L).

and_f2(_,[],[]):- !.

and_f2(L1,[H2|T2],[H|T]):-
  append(L1,H2,H),
  and_f2(L1,T2,T).


/**********************

 TRILL Probability Computation

***********************/

get_bdd_environment(_M,Env):-
  init(Env).

clean_environment(_M,Env):-
  end(Env).


build_bdd(M,Env,[X],BDD):- !,
  bdd_and(M,Env,X,BDD).

build_bdd(M,Env, [H|T],BDD):-
  build_bdd(M,Env,T,BDDT),
  bdd_and(M,Env,H,BDDH),
  or(Env,BDDH,BDDT,BDD).

build_bdd(_M,Env,[],BDD):- !,
  zero(Env,BDD).


bdd_and(M,Env,[X],BDDX):-
  get_prob_ax(M,X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_and(_M,Env,[_X],BDDX):- !,
  one(Env,BDDX).

bdd_and(M,Env,[H|T],BDDAnd):-
  get_prob_ax(M,H,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VH),
  equality(Env,VH,0,BDDH),
  bdd_and(M,Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).
  
bdd_and(M,Env,[_H|T],BDDAnd):- !,
  one(Env,BDDH),
  bdd_and(M,Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).

