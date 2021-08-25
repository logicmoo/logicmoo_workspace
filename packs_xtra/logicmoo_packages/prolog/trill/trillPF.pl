:- use_module(library(lists)).
:- use_module(library(dgraphs)).
:- use_module(library(rbtrees)).
:- use_module(library(tries)).
:-[-'FML'].
:-[-'probsPF'].
:-[-'satPF'].

:- yap_flag(unknown,fail).

/***********
  Queries
  - with and without explanations -
  - with and without probability -
 ***********/
prob_sub_class(Class,SupClass,Expl,Prob):-
  unsat(intersectionOf([Class,complementOf(SupClass)]),Expl),
  compute_prob(Expl,Prob). 
  
prob_sub_class(Class,SupClass,Prob):-
  unsat(intersectionOf([Class,complementOf(SupClass)]),Expl),
  compute_prob(Expl,Prob). 
  
sub_class(Class,SupClass,Expl):-
  unsat(intersectionOf([Class,complementOf(SupClass)]),Expl).
  
sub_class(Class,SupClass):-
  unsat(intersectionOf([Class,complementOf(SupClass)])).

  

prob_instanceOf(Class,Ind,Expl,Prob):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(complementOf(Class),Ind),[]),ABox0),
  findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  %writel(L),
  find_expls(L,[],Expl),
  compute_prob(Expl,Prob).
prob_instanceOf(Class,Ind,Expl,Prob):-
  write('Inconsistent ABox'). 
  
prob_instanceOf(Class,Ind,Prob):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(complementOf(Class),Ind),[]),ABox0),
  findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  %writel(L),
  find_expls(L,[],Expl),
  compute_prob(Expl,Prob).
prob_instanceOf(Class,Ind,Prob):-
  write('Inconsistent ABox'). 
 
instanceOf(Class,Ind,Expl):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(complementOf(Class),Ind),[]),ABox0),
  findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl).
instanceOf(Class,Ind,Expl):-
  write('Inconsistent ABox').
instanceOf(Class,Ind):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(complementOf(Class),Ind),[]),ABox0),
  apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),!,
  clash((ABox1,Tabs1),_).
instanceOf(Class,Ind):-
  write('Inconsistent ABox').

  

prob_unsat(Concept,Expl,Prob):-
  retractall(ind(_)),
  assert(ind(2)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(Concept,1),[]),ABox0),
  findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl),
  compute_prob(Expl,Prob).
prob_unsat(Concept,Expl,Prob):-
  write('Inconsistent ABox').
  
prob_unsat(Concept,Prob):-
  retractall(ind(_)),
  assert(ind(2)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(Concept,1),[]),ABox0),
  findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl),
  compute_prob(Expl,Prob).
prob_unsat(Concept,Prob):-
  write('Inconsistent ABox').
  
unsat(Concept,Expl):-
  retractall(ind(_)),
  assert(ind(2)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(Concept,1),[]),ABox0),
  findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  %writel(L),
  find_expls(L,[],Expl).
unsat(Concept,Expl):-
  write('Inconsistent ABox').
unsat(Concept):-
  retractall(ind(_)),
  assert(ind(2)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(Concept,1),[]),ABox0),
  apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),!,
  clash((ABox1,Tabs1),_).
unsat(Concept):-
  write('Inconsistent ABox').


prob_inconsistent_theory(Expl,Prob):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  findall((ABox1,Tabs1),apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl),
  compute_prob(Expl,Prob).  
  
prob_inconsistent_theory(Prob):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  findall((ABox1,Tabs1),apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl),
  compute_prob(Expl,Prob).

inconsistent_theory(Expl):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  findall((ABox1,Tabs1),apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl).
  
inconsistent_theory:-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),!,
  clash((ABox1,Tabs1),_).
inconsistent_theory:-
  write('Inconsistent!').


find_expls([],Expl,Expl):-!.
find_expls([ABox|T],[],Expl):- !,
  clash(ABox,E),
  find_expls(T,E,Expl).
find_expls([ABox|T],Expl0,Expl):-
  clash(ABox,E),
  %append(Expl0,E,Expl1),
  %and_di_formuleC(Expl0,E,Expl1),
  %Expl1 = and([Expl0,E]),
  and_f(Expl0,E,Expl1),
  find_expls(T,Expl1,Expl).

find_clash((ABox0,Tabs0),Expl2):-
  apply_rules((ABox0,Tabs0),(ABox,Tabs)),
  clash((ABox,Tabs),Expl).
  
%-------------
% clash managing
% previous version, manages only one clash at time
% need some tricks in some rules for managing the cases of more than one clash
% TO IMPROVE!
%------------

clash((ABox,Tabs),Expl):-
  %write('clash 1'),nl,
  find((classAssertion(C,Ind),Expl1),ABox),
  find((classAssertion(complementOf(C),Ind),Expl2),ABox), 
  and_f(Expl1,Expl2,Expl).
clash((ABox,Tabs),Expl):-
  %write('clash 2'),nl,
  find((sameIndividuals(LS),Expl1),ABox),
  find((differentIndividuals(LD),Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  X\==Y,
  and_f(Expl1,Expl2,Expl).
clash((ABox,Tabs),Expl):-
  %write('clash 3'),nl,
  find((classAssertion(C,sameIndividuals(L1)),Expl1),ABox),
  find((classAssertion(complementOf(C),sameIndividuals(L2)),Expl2),ABox),
  member(X,L1),
  member(X,L2),!,
  and_f(Expl1,Expl2,Expl).
clash((ABox,Tabs),Expl):-
  %write('clash 4'),nl,
  find((classAssertion(C,Ind1),Expl1),ABox),
  find((classAssertion(complementOf(C),sameIndividuals(L2)),Expl2),ABox),
  member(Ind1,L2),
  and_f(Expl1,Expl2,Expl).
clash((ABox,Tabs),Expl):-
  %write('clash 5'),nl,
  find((classAssertion(C,sameIndividuals(L1)),Expl1),ABox),
  find((classAssertion(complementOf(C),Ind2),Expl2),ABox),
  member(Ind2,L1),
  and_f(Expl1,Expl2,Expl).
clash((ABox,Tabs),Expl):-
  %write('clash 6'),nl,
  find((classAssertion(maxCardinality(N,S,C),Ind),Expl1),ABox),
  s_neighbours(Ind,S,(ABox,Tabs),SN),
  individual_class_C(SN,C,ABox0,SNC),
  length(SNC,LSS),
  LSS @> N,
  make_expl(Ind,S,SNC,Expl1,ABox,Expl).
clash((ABox,Tabs),Expl):-
  %write('clash 7'),nl,
  find((classAssertion(maxCardinality(N,S),Ind),Expl1),ABox),
  s_neighbours(Ind,S,(ABox,Tabs),SN),
  length(SN,LSS),
  LSS @> N,
  make_expl(Ind,S,SN,Expl1,ABox,Expl).

% --------------
make_expl(_,_,[],Expl1,ABox,Expl1).
make_expl(Ind,S,[H|T],Expl1,ABox,Expl):-
  find((propertyAssertion(S,Ind,H),Expl2),ABox),
  make_expl(Ind,S,T,Expl1,ABox,Expl1),
  and_f(Expl2,Expl1,Expl).
  
  
% -------------
% rules application
% -------------
apply_all_rules(ABox0,ABox2):-
  apply_nondet_rules([or_rule],ABox0,ABox1), 
  (ABox0=ABox1 -> 
  ABox2=ABox1;
  apply_all_rules(ABox1,ABox2)).
  
apply_det_rules([],ABox,ABox).
apply_det_rules([H|T],ABox,ABox1):-
  C=..[H,ABox,ABox1],
  %nl,write(H),
  call(C),
  %write(H),nl,
  %write("...done"),
  !.
apply_det_rules([_|T],ABox,ABox1):-
  apply_det_rules(T,ABox,ABox1).

apply_nondet_rules([],ABox,ABox1):-
  apply_det_rules([and_rule,unfold_rule,add_exists_rule,forall_rule,exists_rule],ABox,ABox1).
apply_nondet_rules([H|T],ABox,ABox1):-
  C=..[H,ABox,L],
  %nl,write(H),
  call(C),
  %write(H),nl,
  %write("...done"),
  member(ABox1,L),
  ABox \= ABox1,!.
apply_nondet_rules([_|T],ABox,ABox1):-
  apply_nondet_rules(T,ABox,ABox1).
  

/**********************

/*=====================================================*/
modify_ABox( (ABox0, Tabs0) , C, Ind, L0, [(classAssertion(C,Ind),Expl)|ABox]):-
  find( (classAssertion(C,Ind), Expl1) ,ABox0),!,
  L0 \== Expl1,
  (Expl1 == [] -> 
     Expl = L0;
     (build_f(L0,Expl1,F),cnf(F,Cnf),sat(Cnf),or_f(L0,Expl1,Expl))
  ),
  
  delete(ABox0, (classAssertion(C,Ind), Expl1), ABox).
  
  
modify_ABox( (ABox0, Tabs0) , C, Ind, L0, [(classAssertion(C,Ind),L0)|ABox0]).

/*=====================================================*/


/*==========================================

	RULES
	
	
  ==========================================
*/


/*
  add_exists_rule
  
  looks up for a role that links 2 individuals, if it find it, it searches a subclass axiom
  in the KB that contains 'someValuesFrom(R,C)' where R is the role which links together the 2 individuals
  and C is a class in which the 2nd individual belongs to.
  
  This rule hasn't a corresponding rule in the tableau
  ========================
*/
add_exists_rule((ABox,Tabs),(ABox1,Tabs)):-
  find((propertyAssertion(R,Ind1,Ind2),Expl1),ABox),
  find((classAssertion(C,Ind2),Expl2),ABox),
  existsInKB(R,C),
  and_f(Expl1,Expl2,Expl),
  modify_ABox( (ABox,Tabs) , someValuesFrom(R,C), Ind1, Expl,ABox1).
  
existsInKB(R,C):-
  subClassOf(A,B),
  member(someValuesFrom(R,C),[A,B]).  
existsInKB(R,C):-
  equivalentClasses(L),
  member(someValuesFrom(R,C),L).
/* *************** */ 

/*
  and_rule
  =================
*/
and_rule((ABox0,Tabs0),(ABox,Tabs0)):-
  find((classAssertion(intersectionOf(LC),Ind),Expl),ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  scan_and_list(LC,Ind,Expl,ABox0,Tabs0,ABox,0).
  

%----------------
scan_and_list([],_Ind,_Expl,ABox,Tabs,ABox,Mod):-
  Mod\=0.
  
scan_and_list([C|T],Ind,Expl0,ABox0, Tabs0,ABox1,_Mod):-
  modify_ABox( (ABox0,Tabs0) , C, Ind, Expl0, ABox),
  scan_and_list(T,Ind,Expl0,ABox, Tabs0, ABox1,1).
  
scan_and_list([_C|T],Ind,Expl,ABox0,Tabs0,ABox,Mod):-
  scan_and_list(T,Ind,Expl,ABox0, Tabs0, ABox,Mod).
/* ************* */


/*
  or_rule
  ===============
*/
or_rule((ABox0,Tabs0),L):-
  find((classAssertion(unionOf(LC),Ind),Expl),ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  findall((ABox1,Tabs0),scan_or_list(LC,Ind,Expl,ABox0,Tabs0, ABox1),L),
  L\=[],!.

%---------------
scan_or_list([],_Ind,_Expl,ABox,Tabs,ABox).

scan_or_list([C|_T],Ind,Expl,ABox, Tabs, ABox1):-
  modify_ABox( (ABox,Tabs) , C, Ind, Expl, ABox1).

scan_or_list([_C|T],Ind,Expl,ABox0,Tabs, ABox):-
  scan_or_list(T,Ind,Expl,ABox0, Tabs,ABox).
/* ***************+ */

/*
  exists_rule
  ==================
*/
exists_rule((ABox0,Tabs0),([(propertyAssertion(R,Ind1,Ind2),Expl),
    (classAssertion(C,Ind2),Expl)|ABox0],Tabs)):-
  find((classAssertion(someValuesFrom(R,C),Ind1),Expl),ABox0),
  \+ blocked(Ind1,(ABox0,Tabs0)),
  \+ connected_individual(R,C,Ind1,ABox0),
  new_ind(Ind2),
  add_edge(R,Ind1,Ind2,Tabs0,Tabs).
  

%---------------
connected_individual(R,C,Ind1,ABox):-
  find((propertyAssertion(R,Ind1,Ind2),_Expl1),ABox),
  find((classAssertion(C,Ind2),_Expl2),ABox).
  
/* ************ */

/*
  forall_rule
  ===================
*/
forall_rule((ABox,Tabs),(ABox1,Tabs)):-
  find((classAssertion(allValuesFrom(R,C),Ind1),Expl1),ABox),
  \+ indirectly_blocked(Ind1,(ABox,Tabs)),
  find((propertyAssertion(R,Ind1,Ind2),Expl2),ABox),
  and_f(Expl1,Expl2,Expl),
  modify_ABox( (ABox,Tabs) , C, Ind2, Expl, ABox1).
/* ************ */

/*
  unfold_rule
  ===========
*/
unfold_rule((ABox0,Tabs),(ABox1,Tabs)):-
  find((classAssertion(C,Ind),Expl),ABox0),
  find_sub_sup_class(C,D,Ax),
  and_f([Ax],Expl,AxL),
  modify_ABox( (ABox0,Tabs) ,D,Ind,AxL,ABox),
  add_nominal(D,Ind,ABox,ABox1).
  
/* -- unfold_rule
      takes a class C1 in which Ind belongs, find a not atomic class C
      that contains C1 (C is seen as list of classes), controls if
      the individual Ind belongs to all those classes, if yes find a class D (if exists)
      that is the superclass or an equivalent class of C and adds D to label af Ind
      - for managing tableau with more than one clash -
      correspond to the ce_rule
   --
*/
unfold_rule((ABox0,Tabs),(ABox1,Tabs)):-
  find((classAssertion(C1,Ind),_),ABox0),
  find_not_atomic(C1,C,L),
  find_all(Ind,L,ABox0,L1),
  find_sub_sup_class(C,D,Ax),
  and_f([Ax],L1,AxL1),
  modify_ABox( (ABox0,Tabs) ,D,Ind,AxL1,ABox),
  add_nominal(D,Ind,ABox,ABox1).
  
  
/* -- unfold_rule
 *    control propertyRange e propertyDomain
 * --
 */

unfold_rule((ABox0,Tabs),(ABox1,Tabs)):-
  find_class_prop_range_domain(Ind,D,Expl,(ABox0,Tabs)),
  modify_ABox( (ABox0,Tabs) ,D,Ind,Expl,ABox),
  add_nominal(D,Ind,ABox,ABox1).
  
/*
 * -- unfold_rule
 *    manage the negation
 * --
 */

unfold_rule((ABox0,Tabs),(ABox1,Tabs)):-
  find((classAssertion(complementOf(C),Ind),Expl),ABox0),
  find_neg_class(C,D),
  Ax = complementOf(C),
  and_f([Ax],Expl,AxL),
  modify_ABox( (ABox0,Tabs) ,D,Ind,AxL,ABox),
  add_nominal(D,Ind,ABox,ABox1).

% ----------------
% add_nominal

add_nominal(D,Ind,ABox0,ABox):-
  ((D = oneOf(_),
    \+ member(nominal(Ind),ABox0))
    ->
   ABox = [nominal(Ind)|ABox0]
    ;
   ABox = ABox0
  ).
    
% ----------------
% unionOf, intersectionOf, subClassOf, negation, allValuesFrom, someValuesFrom, exactCardinality(min and max), maxCardinality, minCardinality

find_neg_class(unionOf(L),intersectionOf(NL)):-
  neg_list(L,NL).

find_neg_class(intersectionOf(L),unionOf(NL)):-
  neg_list(L,NL).

/*
find_neg_class(subClassOf(C,D),intersectionOf(C,ND)):-
  neg_class(D,ND).
*/

find_neg_class(complementOf(C),C).

find_neg_class(allValuesFrom(R,C),someValuesFrom(R,NC)):-
  neg_class(C,NC).

find_neg_class(someValuesFrom(R,C),allValuesFrom(R,NC)):-
  neg_class(C,NC).

find_neg_class(exactCardinality(N,R,C), unionOf([maxCardinality(NMax,R,C),minCardinality(NMin,R,C)])):-
  NMax is N - 1,
  NMin is N + 1.

find_neg_class(minCardinality(N,R,C),maxCardinality(NMax,R,C)):-
  NMax is N - 1.

find_neg_class(maxCardinality(N,R,C),minCardinality(NMin,R,C)):-
  NMin is N + 1.

% ---------------

neg_class(complementOf(C),C):- !.

neg_class(C,complementOf(C)).

% ---------------

neg_list([],[]).

neg_list([H|T],[complementOf(H)|T1]):-
  neg_list(T,T1).

neg_list([complementOf(H)|T],[H|T1]):-
  neg_list(T,T1).

% ----------------

find_class_prop_range_domain(Ind,D,[propertyRange(R,D)|ExplPA],(ABox,Tabs)):-
  find((propertyAssertion(R,_,IndL),ExplPA),ABox),
  indAsList(IndL,L),
  member(Ind,L),
  propertyRange(R,D).

find_class_prop_range_domain(Ind,D,[propertyDomain(R,D)|ExplPA],(ABox,Tabs)):-
  find((propertyAssertion(R,IndL,_),ExplPA),ABox),
  indAsList(IndL,L),
  member(Ind,L),
  propertyDomain(R,D).
	

%-----------------
find_sub_sup_class(C,D,subClassOf(C,D)):-
  subClassOf(C,D).

find_sub_sup_class(C,D,equivalentClasses(L)):-
  equivalentClasses(L),
  member(C,L),
  member(D,L),
  C\==D.

%--------------------
find_not_atomic(C,intersectionOf(L1),L1):-
  subClassOf(A,B),
  member(intersectionOf(L1),[A,B]),
  member(C,L1).

find_not_atomic(C,unionOf(L1),[C]):-
  subClassOf(A,B),
  member(unionOf(L1),[A,B]),
  member(C,L1).

find_not_atomic(C,intersectionOf(L),L):-
  intersectionOf(L),
  member(C,L).

/*find_not_atomic(C,unionOf(L),L):-
  unionOf(L),
  member(C,L).
*/

find_not_atomic(C,intersectionOf(L1),L1):-
  equivalentClasses(L),
  member(intersectionOf(L1),L),
  member(C,L1).

find_not_atomic(C,unionOf(L1),[C]):-
  equivalentClasses(L),
  member(unionOf(L1),L),
  member(C,L1).

% -----------------------

find_all(Ind,[H|T],ABox,LT):-
  T = [],!,
  find((classAssertion(H,Ind),LT),ABox).%,
    
  
find_all(Ind,[H|T],ABox,LT):-
  find((classAssertion(H,Ind),Expl1),ABox),
  find_all(Ind,T,ABox,LE),
  and_f(Expl1,LE,LT).
/* ************* */



/*  absent
  =========
*/
absent(classAssertion(C,Ind),Expl,(ABox,Tabs)):-
  \+ absent1(classAssertion(C,Ind),Expl,ABox),!.

absent(sameIndividuals(L),Expl,(ABox,Tabs)):-
  \+ absent1(sameIndividuals(L),Expl,ABox),!.


%------------------
absent1(Ax,Expl,ABox):-
  find((Ax,Expl0),ABox),
  subset(Expl0,Expl),!.

absent1(sameIndividuals(L),Expl,ABox):-
  find((sameIndividuals(LF),Expl0),ABox),
  permutation(L,LF),
  subset(Expl0,Expl),!.

/* **************** */

/*
 * nominal/2, blockable/2, blocked/2, indirectly_blocked/2 and safe/3
 *
 */

nominal(Inds,(ABox,_)):-
  find((nominal(Ind)),ABox),
  member(Ind,Inds).

% ----------------

blockable(Ind,(ABox,_)):-
  ( find((nominal(Ind)),ABox)
    ->
    false
    ;
    true ).

% ---------------

blocked(Ind,(ABox,T)):-
  check_block(Ind,(ABox,T)).

/*

  control for block an individual
  
*/

check_block(Ind,(ABox,(T,RBN,RBR))):-
  blockable(Ind,(ABox,(T,RBN,RBR))),
  dgraph_transpose(T,T1),
  ancestor(Ind,T,A),
  dgraph_neighbors(Ind,T1,N),
  check_block1(Ind,A,N,(ABox,(T1,RBN,RBR))),!.
  
check_block(Ind,(ABox,(T,RBN,RBR))):-
  blockable(Ind,(ABox,(T,RBN,RBR))),
  dgraph_transpose(T,T1),
  dgraph_neighbours(Ind,T1,N),
  check_block2(N,(ABox,(T,RBN,RBR))),!.
  

check_block1(Indx,A,N,(ABox,(T,RBN,RBR))):-
  member(Indx0,N),
  member(Indy,A),
  member(Indy0,A),
  dgraph_neighbours(Indy,T,N2),
  member(Indy0,N2),
  rb_lookup((Indx0,Indx),V,RBN),
  rb_lookup((Indy0,Indy),V2,RBN),
  member(R,V),
  member(R,V2),
  same_label(Indx,Indy0,ABox),
  same_label(Indx0,Indy,ABox),
  all_node_blockable(Indx0,Indy0,(ABox,(T,RBN,RBR))),!.

check_block2([],_).

check_block2([H|Tail],(ABox,(T,RBN,RBR))):-
  blocked(H,(ABox,(T,RBN,RBR))),
  check_block2(Tail,(ABox,(T,RBN,RBR))).

%---------------
indirectly_blocked(Ind,(ABox,(T,RBN,RBR))):-
  dgraph_transpose(T,T1),
  dgraph_neighbours(Ind,T1,N),
  member(A,N),
  blocked(A,(ABox,(T,RBN,RBR))),!.
  
%---------------------
/*
  An R-neighbour y of a node x is safe if
  (i)  x is blockable or
  (ii) x is a nominal node and y is not blocked.
*/

safe(Ind,R,(ABox,(T,RBN,RBR))):-
  rb_lookup(R,V,RBR),
  member((X,Ind),V),
  blockable(X,(ABox,(T,RBN,RBR))),!.
  
safe(Ind,R,(ABox,(T,RBN,RBR))):-
  rb_lookup(R,V,RBR),
  member((X,Ind),V),
  nominal(X,(ABox,(T,RBN,RBR))),!,
  \+ blocked(Ind,(ABox,(T,RBN,RBR))).

/* ***** */

/*
 subset
 check if first argument is subset of second argument
 =================
*/
subset([],_).

subset([H|T],L):-
  member(H,L),
  subset(T,L).

/*
 writel
 write a list one element at each line
 ==========
*/
writel([]):-!.

writel([T|C]):-
  write(T),nl,
  writel(C).

/*
 writeABox
 ==========
*/
writeABox((ABox,_)):-
  writel(ABox).


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
  findall((classAssertion(Class,Individual),[classAssertion(Class,Individual)]),classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),[propertyAssertion(Property,Subject, Object)]),propertyAssertion(Property,Subject, Object),LPA),
  findall((propertyAssertion(Property,Subject,Object),[subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)]),subProp(SubProperty,Property),LSPA),
  findall(nominal(NominalIndividual),classAssertion(oneOf(_),NominalIndividual),LNA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  create_tabs(LCA,Tabs0,Tabs1),
  add_all(LCA,ABox0,ABox1),
  add_all(LPA,ABox1,ABox2),
  add_all(LSPA,ABox2,ABox3),
  add_all(LNA,ABox3,ABox4),
  findall((differentIndividuals(Ld),[differentIndividuals(Ld)]),differentIndividuals(Ld),LDIA),
  add_all(LDIA,ABox4,ABox5),
  create_tabs(LDIA,Tabs1,Tabs2),
  create_tabs(LPA,Tabs2,Tabs3),
  create_tabs(LSPA,Tabs3,Tabs4),
  findall((sameIndividuals(L),[sameIndividuals(L)]),sameIndividuals(L),LSIA),
  merge_all(LSIA,ABox5,Tabs4,ABox6,Tabs),
  add_nominal_list(ABox6,Tabs,ABox),
  !.

%---------------
subProp(SubProperty,Property,Subject,Object):-
  subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object).

%--------------

add_nominal_list(ABox0,(T,_,_),ABox):-
  dgraph_vertices(T,NomListIn),
  prepare_nom_list(NomListIn,NomListOut),
  add_all(NomListOut,ABox0,ABox).
  
%--------------

prepare_nom_list([],[]).

prepare_nom_list([H|T],[(nominal(H))|T1]):-
  prepare_nom_list(T,T1).
%--------------

/*
  add_all(L1,L2,LO).
  add in L2 all item of L1
*/
add_all([],A,A).

add_all([H|T],A0,A):-
  add(A0,H,A1),
  add_all(T,A1,A).

/* ************** */

/*
  initialize the tableau
  tableau is composed of:
  	a directed graph T => tableau without label
  	a red black tree RBN => each node is a couple of ind that contains the label for the edge
  	a red black tree RBR => each node a property that contains the couples of ind
*/
new_tabs((T,ItR,RtI)):-
  rb_new(ItR),
  rb_new(RtI),
  dgraph_new(T).

create_tabs([],G,G).

create_tabs([(propertyAssertion(P,S,O),_Expl)|T],Tabl0,Tabl):-
  add_edge(P,S,O,Tabl0,Tabl1),
  create_tabs(T,Tabl1,Tabl).
  
create_tabs([(differentIndividuals(Ld),_Expl)|Tail],(T0,RBN,RBR),(T,RBN,RBR)):-
  dgraph_add_vertices(T0,Ld,T).

create_tabs([(classAssertion(_,I),_Expl)|Tail],(T0,RBN,RBR),(T,RBN,RBR)):-
  dgraph_add_vertex(T0,I,T1),
  create_tabs(Tail,(T1,RBN,RBR),(T,RBN,RBR)).

/*
  add edge to tableau
  
*/

add_edge(P,S,O,(T0,ItR0,RtI0),(T1,ItR1,RtI1)):-
  add_node_to_tree(P,S,O,ItR0,ItR1),
  add_role_to_tree(P,S,O,RtI0,RtI1),
  add_edge_to_tabl(P,S,O,T0,T1).
  
add_node_to_tree(P,S,O,RB0,RB1):-
  rb_lookup((S,O),V,RB0),
  \+ member(P,V),
  rb_update(RB0,(S,O),[P|V],RB1).

add_node_to_tree(P,S,O,RB0,RB0):-
  rb_lookup((S,O),V,RB0),
  member(P,V).

add_node_to_tree(P,S,O,RB0,RB1):-
  \+ rb_lookup([S,O],_,RB0),
  rb_insert(RB0,(S,O),[P],RB1).

add_role_to_tree(P,S,O,RB0,RB1):-
  rb_lookup(P,V,RB0),
  \+ member((S,O),V),
  rb_update(RB0,P,[(S,O)|V],RB1).

add_role_to_tree(P,S,O,RB0,RB0):-
  rb_lookup(P,V,RB0),
  member((S,O),V).

add_role_to_tree(P,S,O,RB0,RB1):-
  \+ rb_lookup(P,_,RB0),
  rb_insert(RB0,P,[(S,O)],RB1).

add_edge_to_tabl(R,Ind1,Ind2,T0,T0):-
  dgraph_edge(Ind1,Ind2,T0),!.

add_edge_to_tabl(R,Ind1,Ind2,T0,T1):-
  dgraph_add_edge(T0,Ind1,Ind2,T1).

/* 
  remove edge from tableau
*/

remove_node_to_tree(P,S,O,RB,RB):-
  rb_lookup((S,O),V,RB),
  \+ member(P,V).

remove_node_to_tree(P,S,O,RB0,RB1):-
  rb_lookup((S,O),V,RB0),
  member(P,V),
  remove(V,P,V1),
  V1\==[],
  rb_update(RB0,(S,O),V1,RB1).

remove_node_to_tree(P,S,O,RB0,RB1):-
  rb_lookup((S,O),V,RB0),
  member(P,V),
  remove(V,P,V1),
  V1==[],
  rb_delete(RB0,(S,O),RB1).

remove_all_node_to_tree(P,S,O,RB0,RB1):-
  rb_lookup((S,O),_,RB0),
  rb_delete(RB0,(S,O),RB1).

remove_all_node_to_tree(P,S,O,RB0,RB1):-
  \+ rb_lookup((S,O),_,RB0).

remove_role_to_tree(P,S,O,RB,RB):-
  rb_lookup(P,V,RB),
  \+ member((S,O),V).

remove_role_to_tree(P,S,O,RB0,RB1):-
  rb_lookup(P,V,RB0),
  member((S,O),V),
  delete(V,(S,O),V1),
  V1\==[],
  rb_update(RB0,P,V1,RB1).

remove_role_to_tree(P,S,O,RB0,RB1):-
  rb_lookup(P,V,RB0),
  member((S,O),V),
  delete(V,(S,O),V1),
  V1==[],
  rb_delete(RB0,P,RB1).

remove_edge_to_table(P,S,O,T,T):-
  \+ dgraph_edge(S,O,T).

remove_edge_to_table(P,S,O,T0,T1):-
  dgraph_edge(S,O,T),
  dgraph_del_edge(T0,S,O,T1).

remove_node_to_table(S,T0,T1):-
  dgraph_del_vertex(T0,S,T1).

/*
 * merge node in tableau
 */

merge_tabs(X,Y,(T0,RBN0,RBR0),(T,RBN,RBR)):-
  (dgraph_neighbours(X,T0,LSX0)->assign(LSX0,LSX);assign([],LSX)),
  (dgraph_neighbours(Y,T0,LSY0)->assign(LSY0,LSY);assign([],LSY)),
  dgraph_transpose(T0,TT),
  (dgraph_neighbours(X,TT,LPX0)->assign(LPX0,LPX);assign([],LPX)),
  (dgraph_neighbours(Y,TT,LPY0)->assign(LPY0,LPY);assign([],LPY)),
  flatten([X,Y],L0),
  remove_duplicates(L0,L),
  set_predecessor(L,X,LPX,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),!,
  set_successor(L,X,LSX,(T1,RBN1,RBR1),(T2,RBN2,RBR2)),!,
  set_predecessor(L,Y,LPY,(T2,RBN2,RBR2),(T3,RBN3,RBR3)),!,
  set_successor(L,Y,LSY,(T3,RBN3,RBR3),(T4,RBN4,RBR4)),!,
  remove_nodes(X,Y,(T4,RBN4,RBR4),(T,RBN,RBR)).

remove_nodes(X,Y,Tabs0,Tabs):-
  remove_node(X,Tabs0,Tabs1),
  remove_node(Y,Tabs1,Tabs).

remove_node(X,(T0,RBN0,RBR0),(T,RBN,RBR)):-
  (dgraph_neighbours(X,T0,LS0)->assign(LS0,LS);assign([],LS)),
  dgraph_transpose(T0,TT),
  (dgraph_neighbours(X,TT,LP0)->assign(LP0,LP);assign([],LP)),
  remove_node1(X,LS,RBN0,RBR0,RBN1,RBR1),
  remove_node2(X,LP,RBN1,RBR1,RBN,RBR),
  (dgraph_vertices(T0,VS),member(X,VS)->dgraph_del_vertices(T0,[X],T);assign(T0,T)).

remove_node1(_,[],RBN,RBR,RBN,RBR).

remove_node1(X,[H|T],RBN0,RBR0,RBN,RBR):-
  rb_lookup((X,H),V,RBN0),
  remove_edges(V,X,H,RBR0,RBR1),
  remove_all_node_to_tree(_,X,H,RBN0,RBN1),
  remove_node1(X,T,RBN1,RBR1,RBN,RBR).

remove_node2(_,[],RBN,RBR,RBN,RBR).

remove_node2(X,[H|T],RBN0,RBR0,RBN,RBR):-
  rb_lookup((H,X),V,RBN0),
  remove_edges(V,H,X,RBR0,RBR1),
  remove_all_node_to_tree(_,H,X,RBN0,RBN1),
  remove_node1(X,T,RBN1,RBR1,RBN,RBR).

remove_edges([],_,_,RBR,RBR).

remove_edges([H|T],S,O,RBR0,RBR):-
  remove_role_to_tree(H,S,O,RBR0,RBR1),
  remove_edges(T,S,O,RBR1,RBR).
	

set_predecessor(NN,_,[],Tabs,Tabs).

set_predecessor(NN,X,[H|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  rb_lookup((H,X),LR,RBN0),
  set_predecessor1(NN,H,LR,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_predecessor(NN,X,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_predecessor1(NN,H,[],Tabs,Tabs).

set_predecessor1(NN,H,[R|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  add_edge(R,H,NN,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_predecessor1(NN,H,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_successor(NN,X,[],Tabs,Tabs).

set_successor(NN,X,[H|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  rb_lookup((X,H),LR,RBN0),
  set_successor1(NN,H,LR,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_successor(NN,X,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_successor1(NN,H,[],Tabs,Tabs).

set_successor1(NN,H,[R|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  add_edge(R,NN,H,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_successor1(NN,H,L,(T1,RBN1,RBR1),(T,RBN,RBR)).
	  

/* merge node in (ABox,Tabs) */

merge_all([],ABox,Tabs,ABox,Tabs).

merge_all([(sameIndividuals(H),Expl)|T],ABox0,Tabs0,ABox,Tabs):-
  find_same(H,ABox0,L,ExplL),
  L\==[],!,
  merge_all1(H,L,ABox0,Tabs0,ABox1,Tabs1),
  flatten([H,L],L0),
  remove_duplicates(L0,L1),
  append(Expl,ExplL,ExplT),
  add(ABox1,(sameIndividuals(L1),ExplT),ABox2),
  delete(ABox2,(sameIndividuals(L),ExplL),ABox3),
  retract_sameIndividuals(L),
  merge_all(T,ABox3,Tabs1,ABox,Tabs).

merge_all1([],_,ABox,Tabs,ABox,Tabs).

merge_all1([H|T],L,ABox0,Tabs0,ABox,Tabs):-
  \+ member(H,L),
  merge(H,L,(ABox0,Tabs0),(ABox1,Tabs1)),
  merge_all1(T,[H|L],ABox1,Tabs1,ABox,Tabs).

merge_all1([H|T],L,ABox0,Tabs0,ABox,Tabs):-
  member(H,L),
  merge_all1(T,L,ABox0,Tabs0,ABox,Tabs).

merge_all([(sameIndividuals(H),Expl)|T],ABox0,Tabs0,ABox,Tabs):-
  find_same(H,ABox0,L,_),
  L==[],!,
  merge_all2(H,ABox0,Tabs0,ABox1,Tabs1),
  add(ABox1,(sameIndividuals(H),Expl),ABox2),
  merge_all(T,ABox2,Tabs1,ABox,Tabs).

merge_all2([X,Y|T],ABox0,Tabs0,ABox,Tabs):-
  merge(X,Y,(ABox0,Tabs0),(ABox1,Tabs1)),
  merge_all1(T,[X,Y],ABox1,Tabs1,ABox,Tabs).

find_same(H,ABox,L,Expl):-
  find((sameIndividuals(L),Expl),ABox),
  member(X,L),
  member(X,H),!.

find_same(H,ABox,[],[]).

/*
 * merge
 */
merge(sameIndividuals(L),Y,(ABox0,Tabs0),(ABox,Tabs)):-
  !,
  merge_tabs(L,Y,Tabs0,Tabs),
  merge_abox(L,Y,[],ABox0,ABox).

merge(X,sameIndividuals(L),(ABox0,Tabs0),(ABox,Tabs)):-
  !,
  merge_tabs(X,L,Tabs0,Tabs),
  merge_abox(X,L,[],ABox0,ABox).

merge(X,Y,(ABox0,Tabs0),(ABox,Tabs)):-
  !,
  merge_tabs(X,Y,Tabs0,Tabs),
  merge_abox(X,Y,[],ABox0,ABox).

/* abox as a list */

new_abox([]).

 
/* add El to ABox */
add(ABox,El,[El|ABox]).

assign(L,L).
/*
  find & control (not find)
*/
find(El,ABox):-
  member(El,ABox).

control(El,ABox):-
  \+ find(El,ABox).

/*
  merge node in ABox
*/

merge_abox(X,Y,_,[],[]).

merge_abox(X,Y,Expl0,[(classAssertion(C,Ind),ExplT)|T],[(classAssertion(C,sameIndividuals(L)),[sameIndividuals(L)|Expl])|ABox]):-
  flatten([X,Y],L0),
  remove_duplicates(L0,L),
  member(Ind,L),!,
  append(Expl0,ExplT,Expl),
  merge_abox(X,Y,Expl0,T,ABox).

merge_abox(X,Y,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,sameIndividuals(L),Ind2),[sameIndividuals(L)|Expl])|ABox]):-
  flatten([X,Y],L0),
  remove_duplicates(L0,L),
  member(Ind1,L),!,
  append(Expl0,ExplT,Expl),
  merge_abox(X,Y,Expl0,T,ABox).

merge_abox(X,Y,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,Ind1,sameIndividuals(L)),[sameIndividuals(L)|Expl])|ABox]):-
  flatten([X,Y],L0),
  remove_duplicates(L0,L),
  member(Ind2,L),!,
  append(Expl0,ExplT,Expl),
  merge_abox(X,Y,Expl0,T,ABox).

merge_abox(X,Y,Expl0,[H|T],[H|ABox]):-
  merge_abox(X,Y,Expl0,T,ABox).

/* end of abox a s list */

/* abox as a red-black tree */
/*new_abox(T):-
  rb_new(T).

add(A,(Ass,Ex),A1):-
  rb_insert(A,(Ass,Ex),[],A1).

find((Ass,Ex),A):-
  rb_lookup((Ass,Ex),_,A).
*/
/* end of abox as a rb tree */

/*
  creation of a new individual
  
*/
new_ind(I):-
  retract(ind(I)),
  I1 is I+1,
  assert(ind(I1)).



/*
  same label for two individuals
  
*/

same_label(X,Y,ABox):-
  \+ different_label(X,Y,ABox),
  !.

/*

 different label in two individuals
 
*/

different_label(X,Y,ABox):-
  find((classAssertion(C,X),_),ABox),
  \+ find((classAssertion(C,Y),_),ABox).

different_label(X,Y,ABox):-
  find((classAssertion(C,Y),_),ABox),
  \+ find((classAssertion(C,X),_),ABox).
  

/*
  all nodes in path from X to Y are blockable?
  
*/

all_node_blockable(X,Y,(ABox,(T,RBN,RBR))):-
  dgraph_min_path(X,Y,T,P,_),
  all_node_blockable1(P,(ABox,(T,RBN,RBR))).

all_node_blockable1([],_).

all_node_blockable1([H|Tail],(ABox,(T,RBN,RBR))):-
  blockable(H,(ABox,(T,RBN,RBR))),
  all_node_blockable1(Tail,(ABox,(T,RBN,RBR))).
  
/*
 find all ancestor of a node
 
*/
ancestor(Ind,T,AN):-
  dgraph_transpose(T,T1),
  ancestor1([Ind],T1,[],AN).

ancestor1([],_,A,A).

ancestor1([Ind|Tail],T,A,AN):-
  dgraph_neighbours(Ind,T,AT),
  add_all_n(AT,Tail,Tail1),
  add_all_n(A,AT,A1),
  ancestor1(Tail1,T,A1,AN).

%-----------------
/*

 add_all_n(L1,L2,LO)
 add in L2 all item of L1 without duplicates
 
*/
 
add_all_n([],A,A).

add_all_n([H|T],A,AN):-
  \+ member(H,A),
  add_all_n(T,[H|A],AN).

add_all_n([H|T],A,AN):-
  member(H,A),
  add_all_n(T,A,AN).
/* *************** */

/*
 retract_sameIndividuals(L)
 ==========
*/
retract_sameIndividuals(L):-
  retract(sameIndividuals(L)).

retract_sameIndividuals(L):-
  \+ retract(sameIndividuals(L)).
/* ****** */

/*
  find all S neighbours (S is a role)
*/
s_neighbours(Ind1,S,(ABox,(_,_,RBR)),SN):-
  rb_lookup(S,VN,RBR),
  s_neighbours1(Ind1,VN,SN1),
  s_neighbours2(SN1,SN1,SN,ABox).

s_neighbours(Ind1,S,(_,_,RBR),[]):-
  \+ rb_lookup(S,VN,RBR).

s_neighbours1(_,[],[]).

s_neighbours1(Ind1,[(Ind1,Y)|T],[Y|T1]):-
  s_neighbours1(Ind1,T,T1).

s_neighbours1(Ind1,[(X,Y)|T],T1):-
  X\==Ind1,
  s_neighbours1(Ind1,T,T1).
  
s_neighbours2(_,[],[],_).

s_neighbours2(SN,[H|T],[H|T1],ABox):-
  s_neighbours2(SN,T,T1,ABox),
  \+ same_ind(T1,H,ABox).

s_neighbours2(SN,[H|T],T1,ABox):-
  s_neighbours2(SN,T,T1,ABox),
  same_ind(T1,H,ABox).

%-----------------
same_ind(SN,H,ABox):-
  sameIndividuals(SI),
  member(H,SI),
  member(H2,SI),
  member(H2,SN),
  H\==H2.

same_ind(SN,H,ABox):-
  find((sameIndividuals(SI),_),ABox),
  member(H,SI),
  member(H2,SI),
  member(H2,SN),
  H\==H2.

/* ************* */

/*
 s_predecessors
 ==============
 find all S-predecessor of Ind
*/

s_predecessors(Ind1,S,(ABox,(_,_,RBR)),SN):-
  rb_lookup(S,VN,RBR),
  s_predecessors1(Ind1,VN,SN1),
  s_predecessors2(SN1,SN1,SN,ABox).

s_predecessors(Ind1,S,(ABox,(_,_,RBR)),[]):-
  \+ rb_lookup(S,VN,RBR).

s_predecessors1(_,[],[]).

s_predecessors1(Ind1,[(Y,Ind1)|T],[Y|T1]):-
  s_predecessors1(Ind1,T,T1).

s_predecessors1(Ind1,[(X,Y)|T],T1):-
  Y\==Ind1,
  s_predecessors1(Ind1,T,T1).

s_predecessors2(_,[],[],_).

s_predecessors2(SN,[H|T],[H|T1],ABox):-
  s_predecessors2(SN,T,T1,ABox),
  \+ same_ind(T1,H,ABox).

s_predecessors2(SN,[H|T],T1,ABox):-
  s_predecessors2(SN,T,T1,ABox),
  same_ind(T1,H,ABox).

/* ********** */
