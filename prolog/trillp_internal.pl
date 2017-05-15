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

:- use_module(library(clpb)).

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
  (Expl1 == [] -> 
     Expl = L0
   ;
     (test(L0,Expl1),or_f(L0,Expl1,Expl))
  ),
  delete(ABox0,(classAssertion(C,Ind),Expl1),ABox).
  
  
modify_ABox(ABox0,C,Ind,L0,[(classAssertion(C,Ind),L0)|ABox0]).

modify_ABox(ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox]):-
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox0),!,
  dif(L0,Expl1),
  (Expl1 == [] -> 
     Expl = L0
   ;
     (test(L0,Expl1),or_f(L0,Expl1,Expl))
  ),
  delete(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl1),ABox).
  
  
modify_ABox(ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),L0)|ABox0]).

/* ************* */


/*
  build_abox
  ===============
*/

build_abox((ABox,Tabs)):-
  get_trill_current_module(Name),
  findall((classAssertion(Class,Individual),*([classAssertion(Class,Individual)])),Name:classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),*([propertyAssertion(Property,Subject, Object)])),Name:propertyAssertion(Property,Subject, Object),LPA),
  % findall((propertyAssertion(Property,Subject,Object),*([subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)])),subProp(Name,SubProperty,Property,Subject,Object),LSPA),
  findall(nominal(NominalIndividual),Name:classAssertion(oneOf(_),NominalIndividual),LNA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  create_tabs(LCA,Tabs0,Tabs1),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox3),
  add_all(LNA,ABox3,ABox4),
  findall((differentIndividuals(Ld),*([differentIndividuals(Ld)])),Name:differentIndividuals(Ld),LDIA),
  add_all(LDIA,ABox4,ABox5),
  create_tabs(LDIA,Tabs1,Tabs2),
  create_tabs(LPA,Tabs2,Tabs3),
  create_tabs(LSPA,Tabs3,Tabs4),
  findall((sameIndividual(L),*([sameIndividual(L)])),Name:sameIndividual(L),LSIA),
  merge_all(LSIA,ABox5,Tabs4,ABox6,Tabs),
  add_nominal_list(ABox6,Tabs,ABox),
  !.

/**********************

Formula Management

***********************/

and_f_ax(Axiom,F0,F):-
  and_f(*([Axiom]),F0,F).

% and between two formulae
and_f([],[],[]):-!.

and_f([],F,F):-!.

and_f(F,[],F):-!.

% absorption for subformula (a * (b + (c * d))) * (c * d * e) = a * c * d * e
and_f(*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(*(AO1),O1),
  subset(AO1,A2),!,
  delete(A1,+(O1),A11),
  and_f(*(A11),*(A2),*(A)).
% (a * (b + c + e)) * (c * d) = a * c * d
and_f(*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(X,O1),
  member(X,A2),!,
  delete(A1,+(O1),A11),
  and_f(*(A11),*(A2),*(A)).
% absorption for subformula (c * d * e)  (a * (b + (c * d))) = c * d * e * a
and_f(*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(*(AO2),O2),
  subset(AO2,A1),!,
  delete(A2,+(O2),A21),
  and_f(*(A1),*(A21),*(A)).
% (c * d) * (a * (b + c + e)) = c * d * a
and_f(*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(X,O2),
  member(X,A1),!,
  delete(A2,+(O2),A21),
  and_f(*(A1),*(A21),*(A)).
% (a * b) * (a * c) = a * b * c
and_f(*(A1),*(A2),*(A)):-!,
  append(A1,A2,A0),
  sort(A0,A).

% absorption x * (x + y) = x
and_f(*(A1),+(O1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
and_f(*(A1),+(O1),*(A)):-
  append(A1,[+(O1)],A).

% absorption x * (x + y) = x
and_f(+(O1),*(A1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
and_f(+(O1),*(A1),*(A)):-
  append([+(O1)],A1,A).

and_f(+(O1),+(O2),*([+(O1),+(O2)])).


% or between two formulae
or_f([],[],[]):-!.

or_f([],F,F):-!.

or_f(F,[],F):-!.

% absorption for subformula (a + (b * (c + d))) + (c + d + e) = a + c + d + e
or_f(+(A1),+(A2),+(A)):-
  member(*(O1),A1),
  member(+(AO1),O1),
  subset(AO1,A2),!,
  delete(A1,*(O1),A11),
  or_f(+(A11),+(A2),+(A)).
% (a + (b * c * e)) + (c + d) = a + c + d
or_f(+(A1),+(A2),+(A)):-
  member(*(O1),A1),
  member(X,O1),
  member(X,A2),!,
  delete(A1,*(O1),A11),
  or_f(+(A11),+(A2),+(A)).
% absorption for subformula (c + d + e)  (a + (b * (c + d))) = c + d + e + a
or_f(+(A1),+(A2),+(A)):-
  member(*(O2),A2),
  member(+(AO2),O2),
  subset(AO2,A1),!,
  delete(A2,*(O2),A21),
  or_f(+(A1),+(A21),+(A)).
% (c + d) + (a + (b * c * e)) = c + d + a
or_f(+(A1),+(A2),+(A)):-
  member(*(O2),A2),
  member(X,O2),
  member(X,A1),!,
  delete(A2,*(O2),A21),
  or_f(+(A1),+(A21),+(A)).
% (a + b) + (a + c) = a + b + c
or_f(+(A1),+(A2),+(A)):-!,
  append(A1,A2,A0),
  sort(A0,A).

% absorption x + (x * y) = x
or_f(*(A1),+(O1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
or_f(*(A1),+(O1),*(A)):-
  append(A1,[+(O1)],A).

% absorption x + (x * y) = x
or_f(+(O1),*(A1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
or_f(+(O1),*(A1),*(A)):-
  append([+(O1)],A1,A).

or_f(*(O1),*(O2),+([*(O1),*(O2)])).


/**********************

TRILLP SAT TEST

***********************/

test(L1,L2):-
  %build_f(L1,L2,F),
  %sat(F).
  create_formula(L1,L2,F),
  sat(F).

create_formula(L1,L2,(F1*(~(F2)))):-
  variabilize_formula(L1,F1,[],Vars),
  variabilize_formula(L2,F2,Vars,_).

variabilize_formula([],[],V,V).

variabilize_formula(*(L),*(F),V0,V1):-
  variabilize_formula(L,F,V0,V1).

variabilize_formula(+(L),+(F),V0,V1):-
  variabilize_formula(L,F,V0,V1).

variabilize_formula(~(L),~(F),V0,V1):-
  variabilize_formula(L,F,V0,V1).

variabilize_formula([H|T],[HV|TV],V0,V1):-
  not_bool_op(H),
  member((H-HV),V0),!,
  variabilize_formula(T,TV,V0,V1).

variabilize_formula([H|T],[HV|TV],V0,V1):-
  not_bool_op(H),!,
  variabilize_formula(T,TV,[(H-HV)|V0],V1).

variabilize_formula([H|T],[HV|TV],V0,V2):-
  variabilize_formula(H,HV,V0,VH),
  append(VH,V0,V1),
  variabilize_formula(T,TV,V1,V2).

not_bool_op(H):-
  \+bool_op(H).

bool_op(+(_)):-!.
bool_op(*(_)):-!.
bool_op(~(_)):-!.

/**********************

 TRILLP Probability Computation

***********************/

build_bdd(Env,*(F),BDD):-
  bdd_and(Env,F,BDD).

build_bdd(Env,+(F),BDD):-
  bdd_or(Env,F,BDD).


bdd_and(Env,[+(X)],BDDX):-!,
  bdd_or(Env,X,BDDX).

bdd_and(_Env,[*(_X)],_BDDX):-
  write('error: *([*(_)])'),
  print_message(error,and_in_and),!,false.

bdd_and(Env,[X],BDDX):-
  get_prob_ax(X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_and(Env,[_X],BDDX):- !,
  one(Env,BDDX).

bdd_and(Env,[+(H)|T],BDDAnd):-!,
  bdd_or(Env,H,BDDH),
  bdd_and(Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).

bdd_and(_Env,[*(_H)|_T],_BDDX):-
  write('error: *([*(_)|_])'),
  print_message(error,and_in_and),!,false.

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


bdd_or(Env,[*(X)],BDDX):-!,
  bdd_and(Env,X,BDDX).

bdd_or(_Env,[+(_X)],_BDDX):-
  write('error: +([+(_)])'),
  print_message(error,or_in_or),!,false.

bdd_or(Env,[X],BDDX):-
  get_prob_ax(X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_or(Env,[_X],BDDX):- !,
  zero(Env,BDDX).

bdd_or(Env,[*(H)|T],BDDAnd):-!,
  bdd_and(Env,H,BDDH),
  bdd_or(Env,T,BDDT),
  or(Env,BDDH,BDDT,BDDAnd).

bdd_or(_Env,[+(_H)|_T],_BDDX):-
  write('error: +([+(_)|_])'),
  print_message(error,or_in_or),!,false.

bdd_or(Env,[H|T],BDDAnd):-
  get_prob_ax(H,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VH),
  equality(Env,VH,0,BDDH),
  bdd_or(Env,T,BDDT),
  or(Env,BDDH,BDDT,BDDAnd).
  
bdd_or(Env,[_H|T],BDDAnd):- !,
  zero(Env,BDDH),
  bdd_or(Env,T,BDDT),
  or(Env,BDDH,BDDT,BDDAnd).

