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


/***********
  Utilities for queries
 ***********/

% to find all axplanations for probabilistic queries
all_sub_class(ClassEx,SupClassEx,Exps):-
  all_unsat(intersectionOf([ClassEx,complementOf(SupClassEx)]),Exps).

all_instanceOf(ClassEx,IndEx,Exps):-
  findall(Expl,instanceOf(ClassEx,IndEx,Expl),Exps).

all_property_value(PropEx,Ind1Ex,Ind2Ex,Exps):-
  findall(Expl,property_value(PropEx,Ind1Ex,Ind2Ex,Expl),Exps).

all_unsat(ConceptEx,Exps):-
  findall(Expl,unsat_internal(ConceptEx,Expl),Exps).


all_inconsistent_theory(Exps):-
  findall(Expl,inconsistent_theory(Expl),Exps).


% checks the explanation
check_and_close(Expl,Expl):-
  dif(Expl,[]).


% checks if an explanations was already found
find_expls(_M,[],_,[]).

% checks if an explanations was already found (instance_of version)
find_expls(M,[ABox|_T],[C,I],E):-
  clash(ABox,E0),
  sort(E0,E),
  findall(Exp,M:exp_found([C,I],Exp),Expl),
   not_already_found(M,Expl,[C,I],E),
   assert(M:exp_found([C,I],E)).

% checks if an explanations was already found (property_value version)
find_expls(M,[(ABox,_)|_T],[PropEx,Ind1Ex,Ind2Ex],E):-
  find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),E),ABox),
  findall(Exp,M:exp_found([PropEx,Ind1Ex,Ind2Ex],Exp),Expl),
  not_already_found(M,Expl,[PropEx,Ind1Ex,Ind2Ex],E),
  assert(M:exp_found([PropEx,Ind1Ex,Ind2Ex],E)).

find_expls(M,[_ABox|T],Query,Expl):-
  \+ length(T,0),
  find_expls(M,T,Query,Expl).

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
findClassAssertion4OWLNothing(ABox,Expl):-
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
find_sub_sup_class(exactCardinality(N,R),exactCardinality(N,S),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%concept for concepts exactCardinality
find_sub_sup_class(exactCardinality(N,R,C),exactCardinality(N,R,D),Ax):-
  find_sub_sup_class(C,D,Ax).

%role for concepts exactCardinality
find_sub_sup_class(exactCardinality(N,R,C),exactCardinality(N,S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%role for concepts maxCardinality
find_sub_sup_class(maxCardinality(N,R),maxCardinality(N,S),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%concept for concepts maxCardinality
find_sub_sup_class(maxCardinality(N,R,C),maxCardinality(N,R,D),Ax):-
  find_sub_sup_class(C,D,Ax).

%role for concepts maxCardinality
find_sub_sup_class(maxCardinality(N,R,C),maxCardinality(N,S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%role for concepts minCardinality
find_sub_sup_class(minCardinality(N,R),minCardinality(N,S),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%concept for concepts minCardinality
find_sub_sup_class(minCardinality(N,R,C),minCardinality(N,R,D),Ax):-
  find_sub_sup_class(C,D,Ax).

%role for concepts minCardinality
find_sub_sup_class(minCardinality(N,R,C),minCardinality(N,S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

/* ************* */

/***********
  update abox
  utility for tableau
************/
modify_ABox(ABox,C,Ind,Expl,[(classAssertion(C,Ind),Expl)|ABox]):-
  absent(classAssertion(C,Ind),Expl,ABox).

modify_ABox(ABox,P,Ind1,Ind2,Expl,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox]):-
  absent(propertyAssertion(P,Ind1,Ind2),Expl,ABox).

/* ************* */

% -------------------
notDifferentIndividuals(X,Y,ABox):-
  \+ inAssertDifferentIndividuals(X,Y),
  \+ inABoxDifferentIndividuals(X,Y,ABox).

% --------------

inAssertDifferentIndividuals(differentIndividuals(X),differentIndividuals(Y)):-
  !,
  get_trill_current_module(Name),
  Name:differentIndividuals(LI),
  member(X0,X),
  member(X0,LI),
  member(Y0,Y),
  member(Y0,LI).

inAssertDifferentIndividuals(X,sameIndividual(Y)):-
  !,
  get_trill_current_module(Name),
  Name:differentIndividuals(LI),
  member(X,LI),
  member(Y0,Y),
  member(Y0,LI).

inAssertDifferentIndividuals(sameIndividual(X),Y):-
  !,
  get_trill_current_module(Name),
  Name:differentIndividuals(LI),
  member(X0,X),
  member(X0,LI),
  member(Y,LI).

inAssertDifferentIndividuals(X,Y):-
  get_trill_current_module(Name),
  Name:differentIndividuals(LI),
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
absent(propertyAssertion(P,Ind1,Ind2),Expl,ABox):-
  \+ absent1(propertyAssertion(P,Ind1,Ind2),Expl,ABox),!.

absent(classAssertion(C,Ind),Expl,ABox):-
  \+ absent1(classAssertion(C,Ind),Expl,ABox),!.

absent(sameIndividual(L),Expl,ABox):-
  \+ absent1(sameIndividual(L),Expl,ABox),!.


%------------------
absent1(Ax,Expl,ABox):-
  find((Ax,Expl0),ABox),
  subset(Expl0,Expl),!.

absent1(sameIndividual(L),Expl,ABox):-
  find((sameIndividual(LF),Expl0),ABox),
  permutation(L,LF),
  subset(Expl0,Expl),!.

/* **************** */

/*
  build_abox
  ===============
*/

/*build_abox(ABox):-
  findall((classAssertion(Class,Individual),[classAssertion(Class,Individual)]),classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),[propertyAssertion(Property,Subject, Object)]),propertyAssertion(Property,Subject, Object),LPA),
  findall((propertyAssertion(Property,Subject,Object),[subPropertyOf(SubProperty,Property,Subject,Object),propertyAssertion(SubProperty,Subject,Object)]),subPropertyOf(SubProperty,Property),LSPA),
  new_abox(ABox0),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox).
*/

build_abox((ABox,Tabs)):-
  get_trill_current_module(Name),
  findall((classAssertion(Class,Individual),[classAssertion(Class,Individual)]),Name:classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),[propertyAssertion(Property,Subject, Object)]),Name:propertyAssertion(Property,Subject, Object),LPA),
  % findall((propertyAssertion(Property,Subject,Object),[subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)]),subProp(Name,SubProperty,Property,Subject,Object),LSPA),
  findall(nominal(NominalIndividual),Name:classAssertion(oneOf(_),NominalIndividual),LNA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  create_tabs(LCA,Tabs0,Tabs1),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox3),
  add_all(LNA,ABox3,ABox4),
  findall((differentIndividuals(Ld),[differentIndividuals(Ld)]),Name:differentIndividuals(Ld),LDIA),
  add_all(LDIA,ABox4,ABox5),
  create_tabs(LDIA,Tabs1,Tabs2),
  create_tabs(LPA,Tabs2,Tabs3),
  create_tabs(LSPA,Tabs3,Tabs4),
  findall((sameIndividual(L),[sameIndividual(L)]),Name:sameIndividual(L),LSIA),
  merge_all(LSIA,ABox5,Tabs4,ABox6,Tabs),
  add_nominal_list(ABox6,Tabs,ABox),
  !.


/* ********** */

/**********************

Explanation Management

***********************/

empty_expl([]).

and_f_ax(Axiom,F0,F):-
  and_f([Axiom],F0,F).

and_f(Expl1,Expl2,Expl):-
  append(Expl1,Expl2,ExplT),
  list_to_set(ExplT,Expl).


/**********************

 TRILL Probability Computation

***********************/

get_bdd_environment(NV,Env):-
  init_test(NV,Env).

clean_environment(Env):-
  end_test(Env).


build_bdd(Env,[X],BDD):- !,
  bdd_and(Env,X,BDD).

build_bdd(Env, [H|T],BDD):-
  build_bdd(Env,T,BDDT),
  bdd_and(Env,H,BDDH),
  or(Env,BDDH,BDDT,BDD).

build_bdd(Env,[],BDD):- !,
  zero(Env,BDD).


bdd_and(Env,[X],BDDX):-
  get_prob_ax(X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_and(Env,[_X],BDDX):- !,
  one(Env,BDDX).

bdd_and(Env,[H|T],BDDAnd):-
  get_prob_ax(H,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VH),
  equality(Env,VH,0,BDDH),
  bdd_and(Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).
  
bdd_and(Env,[_H|T],BDDAnd):- !,
  one(Env,BDDH),
  bdd_and(Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).

