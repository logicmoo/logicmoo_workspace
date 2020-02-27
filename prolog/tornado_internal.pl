/* tornado predicates

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
setting_trill(det_rules,[and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule]).
setting_trill(nondet_rules,[or_rule]).

set_up(M):-
  utility_translation:set_up(M),
  M:(dynamic exp_found/2, keep_env/0, tornado_bdd_environment/1).

clean_up(M):-
  utility_translation:clean_up(M),
  M:(dynamic exp_found/2, keep_env/0, tornado_bdd_environment/1),
  retractall(M:exp_found(_,_)),
  retractall(M:keep_env),
  retractall(M:tornado_bdd_environment(_)).

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

% to find all axplanations for probabilistic queries
all_sub_class_int(M:ClassEx,SupClassEx,Exps):-
  assert(M:keep_env),
  sub_class(M:ClassEx,SupClassEx,Exps),!.

all_sub_class_int(M:_,_,Exps):-
  empty_expl(M,Exps).

all_instanceOf_int(M:ClassEx,IndEx,Exps):-
  assert(M:keep_env),
  instanceOf(M:ClassEx,IndEx,Exps),!.
  
all_instanceOf_int(M:_,_,Exps):-
  empty_expl(M,Exps).

all_property_value_int(M:PropEx,Ind1Ex,Ind2Ex,Exps):-
  assert(M:keep_env),
  property_value(M:PropEx,Ind1Ex,Ind2Ex,Exps),!.
  
all_property_value_int(M:_,_,_,Exps):-
  empty_expl(M,Exps).

all_unsat_int(M:ConceptEx,Exps):-
  assert(M:keep_env),
  unsat(M:ConceptEx,Exps),!.

all_unsat_int(M:_,Exps):-
  empty_expl(M,Exps).

all_inconsistent_theory_int(M:Exps):-
  assert(M:keep_env),
  inconsistent_theory(M:Exps),!.

all_inconsistent_theory_int(M:Exps):-
  empty_expl(M,Exps).


compute_prob_and_close(M,Exps,Prob):-
  compute_prob(M,Exps,Prob),
  retractall(M:keep_env),!.

% checks the explanation
check_and_close(M,Expl,Expl):-
  M:keep_env,!.

check_and_close(M,Expl,dot(Dot)):-
  get_bdd_environment(M,Env),
  create_dot_string(Env,Expl,Dot),
  clean_environment(M,Env).



% checks if an explanations was already found
find_expls(M,[],_,BDD):-
  get_bdd_environment(M,Env),
  one(Env,BDD),!.

% checks if an explanations was already found (instance_of version)
find_expls(M,[ABox|T],[C,I],E):-
  findall(E0,clash(M,ABox,E0),Expls0),!,
  or_all_f(M,Expls0,Expls1),
  find_expls(M,T,[C,I],E1),
  and_f(M,Expls1,E1,E).

% checks if an explanations was already found (property_value version)
find_expls(M,[(ABox,_)|T],[PropEx,Ind1Ex,Ind2Ex],E):-
  findall(E0,find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),E0),ABox),Expls0),!,
  or_all_f(M,Expls0,Expls1),
  find_expls(M,T,[PropEx,Ind1Ex,Ind2Ex],E1),
  and_f(M,Expls1,E1,E).
  

find_expls(M,[_ABox|T],Query,Expl):-
  \+ length(T,0),
  find_expls(M,T,Query,Expl).


/****************************/

/****************************
  TABLEAU ALGORITHM
****************************/

% --------------
findClassAssertion4OWLNothing(M,ABox,Expl):-
  findall(Expl1,findClassAssertion('http://www.w3.org/2002/07/owl#Nothing',_Ind,Expl1,ABox),Expls),
  dif(Expls,[]),
  or_all_f(M,Expls,Expl).

/* ************* */

/***********
  update abox
  utility for tableau
************/
modify_ABox(M,ABox0,C,Ind,L0,[(classAssertion(C,Ind),Expl)|ABox]):-
  findClassAssertion(C,Ind,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(M,L0,Expl1,Expl),
  delete(ABox0,(classAssertion(C,Ind),Expl1),ABox).
  
  
modify_ABox(_,ABox0,C,Ind,L0,[(classAssertion(C,Ind),L0)|ABox0]).

modify_ABox(M,ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox]):-
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(M,L0,Expl1,Expl),
  delete(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl1),ABox).
  
  
modify_ABox(_,ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),L0)|ABox0]).

/* ************* */


/*
  build_abox
  ===============
*/

build_abox(M,(ABox,Tabs)):-
  retractall(M:final_abox(_)),
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  get_bdd_environment(M,Env),
  findall((classAssertion(Class,Individual),BDDCA),(M:classAssertion(Class,Individual),bdd_and(M,Env,[classAssertion(Class,Individual)],BDDCA)),LCA),
  findall((propertyAssertion(Property,Subject, Object),BDDPA),(M:propertyAssertion(Property,Subject, Object),dif('http://www.w3.org/2000/01/rdf-schema#comment',Property),bdd_and(M,Env,[propertyAssertion(Property,Subject, Object)],BDDPA)),LPA),
  % findall((propertyAssertion(Property,Subject,Object),*([subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)])),subProp(M,SubProperty,Property,Subject,Object),LSPA),
  findall(nominal(NominalIndividual),M:classAssertion(oneOf(_),NominalIndividual),LNA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  create_tabs(LCA,Tabs0,Tabs1),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox3),
  add_all(LNA,ABox3,ABox4),
  findall((differentIndividuals(Ld),BDDDIA),(M:differentIndividuals(Ld),bdd_and(M,Env,[differentIndividuals(Ld)],BDDDIA)),LDIA),
  add_all(LDIA,ABox4,ABox5),
  create_tabs(LDIA,Tabs1,Tabs2),
  create_tabs(LPA,Tabs2,Tabs3),
  create_tabs(LSPA,Tabs3,Tabs4),
  findall((sameIndividual(L),BDDSIA),(M:sameIndividual(L),bdd_and(M,Env,[sameIndividual(L)],BDDSIA)),LSIA),
  merge_all(M,LSIA,ABox5,Tabs4,ABox6,Tabs),
  add_owlThing_list(M,ABox6,Tabs,ABox),
  !.

/**********************

Explanation Management

***********************/

initial_expl(M,BDD):-
  get_bdd_environment(M,Env),
  zero(Env,BDD).

empty_expl(M,BDD):-
  get_bdd_environment(M,Env),
  one(Env,BDD).

and_f_ax(M,Axiom,BDD0,BDD):-
  get_bdd_environment(M,Env),
  bdd_and(M,Env,[Axiom],BDDAxiom),
  and_f(M,BDDAxiom,BDD0,BDD).

% and between two BDDs
and_f(_,[],BDD,BDD):- !.

and_f(_,BDD,[],BDD):- !.

and_f(M,BDD0,BDD1,BDD):-
  get_bdd_environment(M,Env),
  and(Env,BDD0,BDD1,BDD).


% or between two formulae
or_all_f(M,[],BDD):-
  initial_expl(M,BDD),!.

or_all_f(M,[H|T],Expl):-
  or_all_f(M,T,Expl1),
  or_f(M,H,Expl1,Expl),!.

or_f(_,[],BDD,BDD):- !.
  
or_f(_,BDD,[],BDD):- !.
  
or_f(M,BDD0,BDD1,BDD):-
  get_bdd_environment(M,Env),
  or(Env,BDD0,BDD1,BDD).


/**********************

TRILLP SAT TEST

***********************/

test(M,L1,L2,F):-
  %build_f(L1,L2,F),
  %sat(F).
  or_f(M,L1,L2,F),
  dif(L2,F).


/**********************

Choice Points Management

***********************/

get_choice_point_id(_,0).

create_choice_point(_,_,_,_,_,0).

add_choice_point(_,_,Expl,Expl):- !.


/**********************

 TORNADO Probability Computation

***********************/

get_bdd_environment(M,Env):- 
  M:tornado_bdd_environment(Env),!.

get_bdd_environment(M,Env):-
  init(Env),
  M:assert(tornado_bdd_environment(Env)).

clean_environment(M,Env):-
  end(Env),
  retractall(M:tornado_bdd_environment(_)).

build_bdd(_,Env,[],BDD):- !,
  zero(Env,BDD).

build_bdd(_,_Env,BDD,BDD).

bdd_and(M,Env,[X],BDDX):-
  get_prob_ax(M,X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_and(_M,Env,[_X],BDDX):- !,
  one(Env,BDDX).
