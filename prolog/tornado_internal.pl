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
setting_trill(det_rules,[unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule]). %and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule
setting_trill(nondet_rules,[or_rule]).

set_up(M):-
  utility_translation:set_up(M),
  M:(dynamic exp_found/2, keep_env/0, tornado_bdd_environment/1).

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
all_sub_class(M:ClassEx,SupClassEx,Exps):-
  assert(M:keep_env),
  sub_class(M:ClassEx,SupClassEx,Exps),!.

all_sub_class(M:_,_,Exps):-
  empty_expl(M,Exps).

all_instanceOf(M:ClassEx,IndEx,Exps):-
  assert(M:keep_env),
  instanceOf(M:ClassEx,IndEx,Exps),!.
  
all_instanceOf(M:_,_,Exps):-
  empty_expl(M,Exps).

all_property_value(M:PropEx,Ind1Ex,Ind2Ex,Exps):-
  assert(M:keep_env),
  property_value(M:PropEx,Ind1Ex,Ind2Ex,Exps),!.
  
all_property_value(M:_,_,_,Exps):-
  empty_expl(M,Exps).

all_unsat(M:ConceptEx,Exps):-
  assert(M:keep_env),
  unsat(M:ConceptEx,Exps),!.

all_unsat(M:_,Exps):-
  empty_expl(M,Exps).

all_inconsistent_theory(M:Print,Exps):-
  assert(M:keep_env),
  inconsistent_theory(M:Print,Exps),!.

all_inconsistent_theory(M:_,Exps):-
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
  zero(Env,BDD),!.

% checks if an explanations was already found (instance_of version)
find_expls(M,[ABox|T],[C,I],E):-
  clash(M,ABox,E0),!,
  find_expls(M,T,[C,I],E1),
  or_f(M,E0,E1,E).

% checks if an explanations was already found (property_value version)
find_expls(M,[(ABox,_)|T],[PropEx,Ind1Ex,Ind2Ex],E):-
  find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),E0),ABox),!,
  find_expls(M,T,[PropEx,Ind1Ex,Ind2Ex],E1),
  or_f(M,E0,E1,E).
  

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
  or_all(M,Expls,Expl).

or_all(_M,[],[]).

or_all(M,[H|T],Expl):-
  or_all(M,T,Expl1),
  or_f(M,H,Expl1,Expl).

/* ************* */

/***********
  update abox
  utility for tableau
************/

modify_ABox(M,ABox0,C,Ind,L0,Expl,[(classAssertion(C,Ind),Expl)|ABox]):-
  findClassAssertion(C,Ind,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(M,L0,Expl1,Expl),
  delete(ABox0,(classAssertion(C,Ind),Expl1),ABox).
  
modify_ABox(_,ABox0,C,Ind,L0,L0,[(classAssertion(C,Ind),L0)|ABox0]).

modify_ABox(M,ABox0,C,Ind,L0,false,Added,ABox1):-
  modify_ABox(M,ABox0,C,Ind,L0,Added,ABox1).


modify_ABox(M,ABox0,P,Ind1,Ind2,L0,Expl,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox]):-
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox0),!,
  dif(L0,Expl1),
  test(M,L0,Expl1,Expl),
  delete(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl1),ABox).
  
  
modify_ABox(_,ABox0,P,Ind1,Ind2,L0,L0,[(propertyAssertion(P,Ind1,Ind2),L0)|ABox0]).

/* ************* */

/***********
  update abox
  utility for tableau
************/

get_hierarchy_from_class(M,Class,H4C):-
  hierarchy(M:H),
  get_hierarchy(H,Class,H4CE),!,
  get_bdd_environment(M,Env),
  hier_build_bdd(M,Env,H4CE,H4C).

/* ************* */

/*
  build_abox
  ===============
*/

start_bdd(M,Env):-
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  %findall(1,M:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',_,_),NAnnAss),length(NAnnAss,NV),
  get_bdd_environment(M,Env).

build_abox_int(M,(ABox,Tabs)):-
  start_bdd(M,Env),
  findall((classAssertion(Class,Individual),BDDCA),(M:classAssertion(Class,Individual),bdd_and(M,Env,[classAssertion(Class,Individual)],BDDCA)),LCA),
  findall((propertyAssertion(Property,Subject, Object),BDDPA),(M:propertyAssertion(Property,Subject, Object),bdd_and(M,Env,[propertyAssertion(Property,Subject, Object)],BDDPA)),LPA),
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
  add_nominal_list(ABox6,Tabs,ABox),
  !.

/**********************

Explanation Management

***********************/

initial_expl(M,BDD):-
  get_bdd_environment(M,Env),
  one(Env,BDD).

empty_expl(M,BDD):-
  get_bdd_environment(M,Env),
  zero(Env,BDD).

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
or_f(_,[],BDD,BDD):- !.
  
or_f(_,BDD,[],BDD):- !.
  
or_f(M,BDD0,BDD1,BDD):-
  get_bdd_environment(M,Env),
  or(Env,BDD0,BDD1,BDD).



% init an explanation with one axiom
ax2ex(M,Ax,BDDAxiom):-
  get_bdd_environment(M,Env),
  bdd_and(M,Env,[Ax],BDDAxiom).

/**********************

Hierarchy Explanation Management

***********************/

hier_initial_expl(_M,[]):-!.

hier_empty_expl(_M,[]):-!.

hier_and_f(_M,[],[],[]):- !.

hier_and_f(_M,[],L,L):- !.

hier_and_f(_M,L,[],L):- !.

hier_and_f(_M,L1,L2,F):-
  hier_and_f1(L1,L2,[],F).

hier_and_f1([],_,L,L).

hier_and_f1([H1|T1],L2,L3,L):-
  hier_and_f2(H1,L2,L12),
  append(L3,L12,L4),
  hier_and_f1(T1,L2,L4,L).

hier_and_f2(_,[],[]):- !.

hier_and_f2(L1,[H2|T2],[H|T]):-
  append(L1,H2,H),
  hier_and_f2(L1,T2,T).

hier_or_f_check(_M,Or1,Or2,Or):-absent(Or1,Or2,Or,_).

hier_or_f(_M,Or1,Or2,Or):-
  append(Or1,Or2,Or0),
  sort(Or0,Or).

hier_ax2ex(_M,Ax,[[Ax]]):- !.

/*  absent
  =========
*/
absent(Expl0,Expl1,Expl,Added):- % Expl0 already present expls, Expl1 new expls to add, Expl the combination of two lists
  absent0(Expl0,Expl1,Expl,Added),!.

%------------------
absent0(Expl0,Expl1,Expl,Added):-
  absent1(Expl0,Expl1,Expl,Added),
  dif(Added,[]).

absent1(Expl,[],Expl,[]).

absent1(Expl0,[H|T],[H|Expl],[H|Added]):-
  absent2(Expl0,H),!,
  absent1(Expl0,T,Expl,Added).

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

/**********************

TRILLP SAT TEST

***********************/

test(M,L1,L2,F):-
  %build_f(L1,L2,F),
  %sat(F).
  or_f(M,L1,L2,F),
  dif(L2,F).


/**********************

 TORNADO Probability Computation

***********************/

get_bdd_environment(M,Env):- 
  M:tornado_bdd_environment(Env),!.

get_bdd_environment(M,Env):-
  init(Env),
  M:assert(tornado_bdd_environment(Env)).

get_bdd_environment(M,Env):-
  M:tornado_bdd_environment(Env),!.

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


/**********************

 TORNADO Probability Computation

***********************/
hier_build_bdd(_M,_Env,[],[]):- !.

hier_build_bdd(M,Env, [C-H|T],[C-BDDH|BDDT]):-
  hier_build_bdd_int(M,Env,H,BDDH),
  hier_build_bdd(M,Env,T,BDDT).

hier_build_bdd_int(_M,Env,[],BDD):- !,
  zero(Env,BDD).

hier_build_bdd_int(M,Env,[X],BDD):- !,
  hier_bdd_and(M,Env,X,BDD).

hier_build_bdd_int(M,Env, [H|T],BDD):-
  hier_build_bdd_int(M,Env,T,BDDT),
  hier_bdd_and(M,Env,H,BDDH),
  or(Env,BDDH,BDDT,BDD).

hier_build_bdd_int(_M,Env,[],BDD):- !,
  zero(Env,BDD).


hier_bdd_and(M,Env,[X],BDDX):-
  get_prob_ax(M,X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

hier_bdd_and(_M,Env,[_X],BDDX):- !,
  one(Env,BDDX).

hier_bdd_and(M,Env,[H|T],BDDAnd):-
  get_prob_ax(M,H,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VH),
  equality(Env,VH,0,BDDH),
  hier_bdd_and(M,Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).
  
hier_bdd_and(M,Env,[_H|T],BDDAnd):- !,
  one(Env,BDDH),
  hier_bdd_and(M,Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).
