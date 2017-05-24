/* trillp predicates

This module performs reasoning over probabilistic description logic knowledge bases.
It reads probabilistic knowledge bases in RDF format or in TRILL format, a functional-like
sintax, and answers queries by finding the set of explanations or computing the probability.

See https://github.com/rzese/trill/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~rzese/software/trill/manual.html for
details.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/

%:- use_module(library(clpb)).

/********************************
  SETTINGS
*********************************/
:- multifile setting_trill/2.
setting_trill(det_rules,[and_rule,unfold_rule,add_exists_rule,forall_rule,exists_rule]).
setting_trill(nondet_rules,[or_rule]).

/*****************************
  MESSAGES
******************************/
:- multifile prolog:message/1.

prolog:message(or_in_or) -->
  [ 'Boolean formula wrongly built: or in or' ].

prolog:message(and_in_and) -->
  [ 'Boolean formula wrongly built: and in and' ].

/****************************
  QUERY PREDICATES
*****************************/

/***********
  Utilities for queries
 ***********/

% adds the query into the ABox
add_q(ABox,Query,ABox0):-
  get_bdd_environment(Env),
  one(Env,BDD),
  add(ABox,(Query,BDD),ABox0).


% to find all axplanations for probabilistic queries
all_sub_class(ClassEx,SupClassEx,Exps):-
  sub_class(ClassEx,SupClassEx,Exps).

all_instanceOf(ClassEx,IndEx,Exps):-
  instanceOf(ClassEx,IndEx,Exps).

all_property_value(PropEx,Ind1Ex,Ind2Ex,Exps):-
  property_value(PropEx,Ind1Ex,Ind2Ex,Exps).

all_unsat(ConceptEx,Exps):-
  unsat(ConceptEx,Exps).


all_inconsistent_theory(Exps):-
  inconsistent_theory(Exps).




% checks if an explanations was already found
find_expls([],_,[]).

% checks if an explanations was already found (instance_of version)
find_expls([ABox|T],[C,I],E):-
  clash(ABox,E0),!,
  find_expls(T,[C,I],E1),
  or_f(E0,E1,E).

% checks if an explanations was already found (property_value version)
find_expls([(ABox,_)|T],[PropEx,Ind1Ex,Ind2Ex],E):-
  find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),E0),ABox),!,
  find_expls(T,[PropEx,Ind1Ex,Ind2Ex],E1),
  or_f(E0,E1,E).
  

find_expls([_ABox|T],Query,Expl):-
  \+ length(T,0),
  find_expls(T,Query,Expl).

/****************************/

/****************************
  TABLEAU ALGORITHM
****************************/

% --------------
findClassAssertion4OWLNothing(ABox,Expl):-
  findall(Expl1,findClassAssertion('http://www.w3.org/2002/07/owl#Nothing',_Ind,Expl1,ABox),Expls),
  dif(Expls,[]),
  or_all(Expls,Expl).

or_all([],[]).

or_all([H|T],Expl):-
  or_all(T,Expl1),
  or_f(H,Expl1,Expl).

/* ************* */

/***********
  update abox
  utility for tableau
************/
modify_ABox(ABox0,C,Ind,L0,[(classAssertion(C,Ind),Expl)|ABox]):-
  findClassAssertion(C,Ind,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(L0,Expl1,Expl),
  delete(ABox0,(classAssertion(C,Ind),Expl1),ABox).
  
  
modify_ABox(ABox0,C,Ind,L0,[(classAssertion(C,Ind),L0)|ABox0]).

modify_ABox(ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox]):-
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(L0,Expl1,Expl),
  delete(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl1),ABox).
  
  
modify_ABox(ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),L0)|ABox0]).

/* ************* */


/*
  build_abox
  ===============
*/

build_abox((ABox,Tabs)):-
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  get_trill_current_module(Name),
  findall(1,Name:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',_,_),NAnnAss),length(NAnnAss,NV),
  get_bdd_environment(NV,Env),
  findall((classAssertion(Class,Individual),BDDCA),(Name:classAssertion(Class,Individual),bdd_and(Env,[classAssertion(Class,Individual)],BDDCA)),LCA),
  findall((propertyAssertion(Property,Subject, Object),BDDPA),(Name:propertyAssertion(Property,Subject, Object),bdd_and(Env,[propertyAssertion(Property,Subject, Object)],BDDPA)),LPA),
  % findall((propertyAssertion(Property,Subject,Object),*([subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)])),subProp(Name,SubProperty,Property,Subject,Object),LSPA),
  findall(nominal(NominalIndividual),Name:classAssertion(oneOf(_),NominalIndividual),LNA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  create_tabs(LCA,Tabs0,Tabs1),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox3),
  add_all(LNA,ABox3,ABox4),
  findall((differentIndividuals(Ld),BDDDIA),(Name:differentIndividuals(Ld),bdd_and(Env,[differentIndividuals(Ld)],BDDDIA)),LDIA),
  add_all(LDIA,ABox4,ABox5),
  create_tabs(LDIA,Tabs1,Tabs2),
  create_tabs(LPA,Tabs2,Tabs3),
  create_tabs(LSPA,Tabs3,Tabs4),
  findall((sameIndividual(L),BDDSIA),(Name:sameIndividual(L),bdd_and(Env,[sameIndividual(L)],BDDSIA)),LSIA),
  merge_all(LSIA,ABox5,Tabs4,ABox6,Tabs),
  add_nominal_list(ABox6,Tabs,ABox),
  !.

/**********************

Explanation Management

***********************/

and_f_ax(Axiom,BDD0,BDD):-
  get_bdd_environment(Env),
  bdd_and(Env,[Axiom],BDDAxiom),
  and_f(BDDAxiom,BDD0,BDD).

% and between two BDDs
and_f([],BDD,BDD):- !.

and_f(BDD,[],BDD):- !.

and_f(BDD0,BDD1,BDD):-
  get_bdd_environment(Env),
  and(Env,BDD0,BDD1,BDD).


% or between two formulae
or_f([],BDD,BDD):- !.
  
or_f(BDD,[],BDD):- !.
  
or_f(BDD0,BDD1,BDD):-
  get_bdd_environment(Env),
  or(Env,BDD0,BDD1,BDD).


/**********************

TRILLP SAT TEST

***********************/

test(L1,L2,F):-
  %build_f(L1,L2,F),
  %sat(F).
  or_f(L1,L2,F),
  dif(L2,F).

test(BDD):-
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  get_bdd_environment(10,Env),
  one(Env,BDD1),
  and_f_ax1(a,BDD1,BDD2),
  and_f_ax1(b,BDD2,BDDFirst),
  one(Env,BDD3),
  and_f_ax1(a,BDD3,BDD4),
  and_f_ax1(c,BDD4,BDDSecond),
  one(Env,BDD5),
  and_f_ax1(a,BDD5,BDDThird),
  test(BDDSecond,BDDFirst,BDDAll1),
  test(BDDFirst,BDDAll1,BDD),
  end_test(Env), !.

and_f_ax1(Axiom,BDD0,BDD):-
  get_bdd_environment(Env),
  atom_number('0.6',ProbA),
  ProbB is 1 - ProbA,
  ( na(Axiom,N) ->
      true
    ;
      rule_n(N),
      assert(na(Axiom,N)),
      retract(rule_n(N)),
      N1 is N + 1,
      assert(rule_n(N1))
  ),
  get_var_n(Env,N,[],[ProbA,ProbB],VX),
  equality(Env,VX,0,BDDAxiom),!,
  and_f(BDDAxiom,BDD0,BDD).

/**********************

 TRILLP Probability Computation

***********************/

:- thread_local trillpbdd_environment/1.

get_bdd_environment(_NV,Env):-
  trillpbdd_environment(Env),!.

get_bdd_environment(NV,Env):-
  init_test(NV,Env),
  assert(trillpbdd_environment(Env)).

get_bdd_environment(Env):-
  trillpbdd_environment(Env),!.

clean_environment(Env):-
  end_test(Env),
  retractall(trillpbdd_environment(_)).


build_bdd(_Env,BDD,BDD).

bdd_and(Env,[X],BDDX):-
  get_prob_ax(X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_and(Env,[_X],BDDX):- !,
  one(Env,BDDX).
