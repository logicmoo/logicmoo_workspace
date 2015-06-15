
:- module(trill,[sub_class/2, sub_class/3, prob_sub_class/3,
                 instanceOf/2, instanceOf/3, prob_instanceOf/3,
                 unsat/1, unsat/2, prob_unsat/2,
                 inconsistent_theory/0, inconsistent_theory/1, prob_inconsistent_theory/1,
                 load_theory/1, check_query_args/1] ).

%:- set_prolog_flag(unknow,fail).

/*:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_model')).*/
:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(library(rbtrees)).
:- use_module(library(pengines)).
%:- use_module(library(tries)).
%:- load_foreign_files(['cplint-swi'],[],init_my_predicates).
%:-use_foreign_library(bddem,install).
:-['trillProbComputation'].

:- thread_local
	ind/1.

%:- yap_flag(unknown,fail).
:- multifile
	owl2_model:axiom/1,
        owl2_model:classAssertion/2,
        owl2_model:propertyAssertion/3,
        owl2_model:subPropertyOf/2,
        owl2_model:subClassOf/2,
        owl2_model:equivalentClasses/1,
        owl2_model:differentIndividuals/1,
        owl2_model:sameIndividual/1,
        owl2_model:intersectionOf/1,
        owl2_model:unionOf/1,
        owl2_model:propertyRange/2,
        owl2_model:propertyDomain/2,
        owl2_model:annotationAssertion/3,
        owl2_model:exactCardinality/2,
        owl2_model:exactCardinality/3,
        owl2_model:maxCardinality/2,
        owl2_model:maxCardinality/3,
        owl2_model:minCardinality/2,
        owl2_model:minCardinality/3.


load_theory(Name):-
  [Name].

check_query_args([H|T]) :-
  atomic(H),!,
  get_trill_current_module(Name),
  Name:axiom(A),
  A =.. [_|L],
  flatten(L,L1),
  member(H,L1),!,
  check_query_args(T).
  
check_query_args([H|T]) :-
  \+ atomic(H),!,
  H =.. [_|L],
  flatten(L,L1),
  check_query_args(L1),
  check_query_args(T).

check_query_args([]).

/***********
  Queries
  - with and without explanations -
 ***********/
sub_class(Class,SupClass,Expl):-
  ( check_query_args([Class,SupClass]) ->
	unsat(intersectionOf([Class,complementOf(SupClass)]),Expl)
    ;
    	Expl = ["IRIs not existent"],!
  ).
  %subClassOf(Class,SupClass).

sub_class(Class,SupClass):-
  check_query_args([Class,SupClass]),
  unsat(intersectionOf([Class,complementOf(SupClass)])).

instanceOf(Class,Ind,Expl):-
  ( check_query_args([Class,Ind]) ->
	retractall(ind(_)),
  	assert(ind(1)),
  	build_abox((ABox,Tabs)),
  	\+ clash((ABox,Tabs),_),!,
  	add(ABox,(classAssertion(complementOf(Class),Ind),[]),ABox0),
  	%findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
  	findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  	find_expls(L,[],Expl),
  	Expl \= []
    ;
    	Expl = ["IRIs not existent"],!
  ).

instanceOf(_,_,_):-
  write('Inconsistent ABox').

instanceOf(Class,Ind):-
  check_query_args([Class,Ind]),
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(complementOf(Class),Ind),[]),ABox0),
  %findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
  apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),!,
  clash((ABox1,Tabs1),_).

instanceOf(_,_):-
  write('Inconsistent ABox').

unsat(Concept,Expl):-
  retractall(ind(_)),
  assert(ind(2)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(Concept,1),[]),ABox0),
  %findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
  findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl),
  Expl \= [].

unsat(_,_):-
  write('Inconsistent ABox').

unsat(Concept):-
  retractall(ind(_)),
  assert(ind(2)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  add(ABox,(classAssertion(Concept,1),[]),ABox0),
  %findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
  apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),!,
  clash((ABox1,Tabs1),_).

unsat(_):-
  write('Inconsistent ABox').

inconsistent_theory(Expl):-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  findall((ABox1,Tabs1),apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,[],Expl),
  Expl \= [].

inconsistent_theory:-
  retractall(ind(_)),
  assert(ind(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),!,
  clash((ABox1,Tabs1),_).

inconsistent_theory:-
  write('Inconsistent!').

prob_instanceOf(Class,Ind,P):-
  ( check_query_args([Class,Ind]) ->
  	all_instanceOf(Class,Ind,Exps),
%  (Exps \= [] ->
%    build_formula(Exps,FormulaE,[],VarE),
%    (FormulaE \= [] -> 
%      var2numbers(VarE,0,NewVarE),
%      write(NewVarE),nl,write(FormulaE),
%      compute_prob(NewVarE,FormulaE,P,0)
%    ;
%      P = 1)
%  ;
%    P = 0).  
%  writel(Exps),nl,
  	compute_prob(Exps,P)
  ;
  	P = ["IRIs not existent"],!
  ).

prob_sub_class(Class,SupClass,P):-
  ( check_query_args([Class,SupClass]) ->
  	all_sub_class(Class,SupClass,Exps),
%  (Exps \= [] ->
%    build_formula(Exps,FormulaE,[],VarE),
%    (FormulaE \= [] -> 
%      var2numbers(VarE,0,NewVarE),
%      compute_prob(NewVarE,FormulaE,P,0)
%    ;
%      P = 1)
%  ;
%    P = 0).
  	compute_prob(Exps,P)
  ;
  	P = ["IRIs not existent"],!
  ).
  
prob_unsat(Concept,P):-
  all_unsat(Concept,Exps),
  compute_prob(Exps,P).
    
prob_inconsistent_theory(P):-
  all_inconsistent_theory(Exps),
  compute_prob(Exps,P).

find_expls([],Expl,Expl).

find_expls([ABox|T],Expl0,Expl):-
  clash(ABox,E),
  append(Expl0,E,Expl1),
  find_expls(T,Expl1,Expl).


all_sub_class(Class,SupClass,LE):-
  all_unsat(intersectionOf([Class,complementOf(SupClass)]),LE).

all_instanceOf(Class,Ind,LE):-
  findall(Expl,instanceOf(Class,Ind,Expl),LE).

all_unsat(Concept,LE):-
  findall(Expl,unsat(Concept,Expl),LE).
  

all_inconsistent_theory(LE):-
  findall(Expl,inconsistent_theory(Expl),LE).


/*
find_clash((ABox0,Tabs0),Expl2):-
  apply_rules((ABox0,Tabs0),(ABox,Tabs)),
  clash((ABox,Tabs),Expl).
*/

%-------------
% clash managing
% previous version, manages only one clash at time
% need some tricks in some rules for managing the cases of more than one clash
% TO IMPROVE!
%------------
clash((ABox,_),Expl):-
  %write('clash 1'),nl,
  find((classAssertion(C,Ind),Expl1),ABox),
  find((classAssertion(complementOf(C),Ind),Expl2),ABox),
  append(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 2'),nl,
  find((sameIndividual(LS),Expl1),ABox),
  find((differentIndividuals(LD),Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  X\==Y,
  append(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 3'),nl,
  find((classAssertion(C,sameIndividual(L1)),Expl1),ABox),
  find((classAssertion(complementOf(C),sameIndividual(L2)),Expl2),ABox),
  member(X,L1),
  member(X,L2),!,
  append(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 4'),nl,
  find((classAssertion(C,Ind1),Expl1),ABox),
  find((classAssertion(complementOf(C),sameIndividual(L2)),Expl2),ABox),
  member(Ind1,L2),
  append(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 5'),nl,
  find((classAssertion(C,sameIndividual(L1)),Expl1),ABox),
  find((classAssertion(complementOf(C),Ind2),Expl2),ABox),
  member(Ind2,L1),
  append(Expl1,Expl2,Expl).

clash((ABox,Tabs),Expl):-
  %write('clash 6'),nl,
  find((classAssertion(maxCardinality(N,S,C),Ind),Expl1),ABox),
  s_neighbours(Ind,S,(ABox,Tabs),SN),
  individual_class_C(SN,C,ABox,SNC),
  length(SNC,LSS),
  LSS @> N,
  make_expl(Ind,S,SNC,Expl1,ABox,Expl2),
  flatten(Expl2,Expl3),
  list_to_set(Expl3,Expl).

clash((ABox,Tabs),Expl):-
  %write('clash 7'),nl,
  find((classAssertion(maxCardinality(N,S),Ind),Expl1),ABox),
  s_neighbours(Ind,S,(ABox,Tabs),SN),
  length(SN,LSS),
  LSS @> N,
  make_expl(Ind,S,SN,Expl1,ABox,Expl2),
  flatten(Expl2,Expl3),
  list_to_set(Expl3,Expl).

% --------------
make_expl(_,_,[],Expl1,_,Expl1).

make_expl(Ind,S,[H|T],Expl1,ABox,[Expl2|Expl]):-
  find((propertyAssertion(S,Ind,H),Expl2),ABox),
  make_expl(Ind,S,T,Expl1,ABox,Expl).


% -------------
% rules application
% -------------
apply_all_rules(ABox0,ABox2):-
  apply_nondet_rules([or_rule,max_rule],
                  ABox0,ABox1), 
  (ABox0=ABox1 -> 
  ABox2=ABox1;
  apply_all_rules(ABox1,ABox2)).
  
apply_det_rules([],ABox,ABox).

apply_det_rules([H|_],ABox,ABox1):-
  C=..[H,ABox,ABox1],
  call(C),!.

apply_det_rules([_|T],ABox,ABox1):-
  apply_det_rules(T,ABox,ABox1).


apply_nondet_rules([],ABox,ABox1):-
  apply_det_rules([o_rule,and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule,min_rule],ABox,ABox1).

apply_nondet_rules([H|_],ABox,ABox1):-
  C=..[H,ABox,L],
  call(C),!,
  member(ABox1,L),
  ABox \= ABox1.

apply_nondet_rules([_|T],ABox,ABox1):-
  apply_nondet_rules(T,ABox,ABox1).
  

/**********************
   old version for the rules application

apply_rules_0((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules((ABox0,Tabs0),(ABox,Tabs)).

apply_rules((ABox0,Tabs0),(ABox,Tabs)):-
  %writel(ABox0),nl,
  %apply_rules1((ABox0,Tabs0),(ABox,Tabs)).
  apply_rules1_1((ABox0,Tabs0),(ABox,Tabs)).

apply_rules1_1((ABox0,Tabs0),(ABox,Tabs)):-
  %write('o_rule: '),nl,
  o_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules1_1((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules1((ABox0,Tabs0),(ABox,Tabs)).

apply_rules1((ABox0,Tabs0),(ABox,Tabs)):-
  %write('and_rule: '),nl,
  and_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules1((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules2((ABox0,Tabs0),(ABox,Tabs)).

apply_rules2((ABox0,Tabs0),(ABox,Tabs)):-
  %write('exists_rule: '),nl,
  exists_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules2((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules3((ABox0,Tabs0),(ABox,Tabs)).

apply_rules3((ABox0,Tabs0),(ABox,Tabs)):-
  %write('forall_rule: '), nl,
  forall_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules3((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules3_1((ABox0,Tabs0),(ABox,Tabs)).

apply_rules3_1((ABox0,Tabs0),(ABox,Tabs)):-
  %write('forall_plus_rule: '),nl,
  forall_plus_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules3_1((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules4((ABox0,Tabs0),(ABox,Tabs)).

apply_rules4((ABox0,Tabs0),(ABox,Tabs)):-
  %write('min_rule: '),nl,
  min_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules4((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules5((ABox0,Tabs0),(ABox,Tabs)).

apply_rules5((ABox0,Tabs0),(ABox,Tabs)):-
  %write('unfold_rule: '),nl,
  unfold_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules5((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules6((ABox0,Tabs0),(ABox,Tabs)).

apply_rules6((ABox0,Tabs0),(ABox,Tabs)):-
  %write('add_exists_rule: '),nl,
  add_exists_rule((ABox0,Tabs0),(ABox1,Tabs1)),!,
  %writel(ABox1),nl,
  %write('apllyied'),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules6((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules7((ABox0,Tabs0),(ABox,Tabs)).

apply_rules7((ABox0,Tabs0),(ABox,Tabs)):-
  %write('or_rule: '),nl,
  or_rule((ABox0,Tabs0),L),!,
  member((ABox1,Tabs1),L),
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules7((ABox0,Tabs0),(ABox,Tabs)):-
  apply_rules8((ABox0,Tabs0),(ABox,Tabs)).

apply_rules8((ABox0,Tabs0),(ABox,Tabs)):-
  %write('max_rule: '),nl,
  max_rule((ABox0,Tabs0),L),!,
  member((ABox1,Tabs1),L),
  %write('apllyied'),nl,
  %writel(ABox1),nl,
  apply_rules((ABox1,Tabs1),(ABox,Tabs)).

apply_rules8((ABox,Tabs),(ABox,Tabs)).

*/

/*
  add_exists_rule
  ========================
*/
add_exists_rule((ABox,Tabs),([(classAssertion(someValuesFrom(R,C),Ind1),Expl)|ABox],Tabs)):-
  find((propertyAssertion(R,Ind1,Ind2),Expl1),ABox),
  find((classAssertion(C,Ind2),Expl2),ABox),
  existsInKB(R,C),
  append(Expl1,Expl2,ExplT),
  list_to_set(ExplT,Expl),
  absent(classAssertion(someValuesFrom(R,C),Ind1),Expl,(ABox,Tabs)).
  
existsInKB(R,C):-
  get_trill_current_module(Name),
  Name:subClassOf(A,B),
  member(someValuesFrom(R,C),[A,B]).
  
existsInKB(R,C):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
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
scan_and_list([],_Ind,_Expl,ABox,_Tabs,ABox,Mod):-
  Mod\=0.

scan_and_list([C|T],Ind,Expl,ABox0, Tabs0,[(classAssertion(C,Ind),Expl)|ABox],_Mod):-
  absent(classAssertion(C,Ind),Expl,(ABox0,Tabs0)),!,
  scan_and_list(T,Ind,Expl,ABox0, Tabs0, ABox,1).

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
scan_or_list([],_Ind,_Expl,ABox,_Tabs,ABox).

scan_or_list([C|_T],Ind,Expl,ABox, Tabs, [(classAssertion(C,Ind),Expl)|ABox]):-
  absent(classAssertion(C,Ind),Expl,(ABox,Tabs)).

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
forall_rule((ABox,Tabs),([(classAssertion(C,Ind2),Expl)|ABox],Tabs)):-
  find((classAssertion(allValuesFrom(R,C),Ind1),Expl1),ABox),
  \+ indirectly_blocked(Ind1,(ABox,Tabs)),
  find((propertyAssertion(R,Ind1,Ind2),Expl2),ABox),
  append(Expl1,Expl2,ExplT),
  list_to_set(ExplT,Expl), %list_to_set(ExplT,ExplT1),
  %check_chain(R,Ind1,Ind2,ExplT1,ExplT1,Expl),
  absent(classAssertion(C,Ind2),Expl,(ABox,Tabs)).
  
%------------------
check_chain(_,_,_,[],Ne,Ne):-
  !.

check_chain(R,Ind1,Ind2,[subPropertyOf(R,S,Ind3,Ind1)|T],Ne,Ne4):-
  is_transitive(R,S,Ne),
  copy_s(subPropertyOf(R,S,Ind3,Ind1),subPropertyOf(R,S,Ind3,Ind2),Ne,Ne2),
  check_chain(R,Ind1,Ind2,T,Ne2,Ne3),!,
  check_chain(S,Ind3,Ind2,Ne3,Ne3,Ne4).

check_chain(R,Ind1,Ind2,[transitive(R,[Ind3,Ind1])|T],Ne,Ne3):-
  copy_s(transitive(R,[Ind3,Ind1]),transitive(R,[Ind3,Ind1,Ind2]),Ne,Ne2),
  check_chain(R,Ind1,Ind2,T,Ne2,Ne3),!.

check_chain(R,Ind1,Ind2,[transitive(R,[Ind3,Ind4,Ind1])|T],Ne,Ne3):-
  copy_s(transitive(R,[Ind3,Ind4,Ind1]),transitive(R,[Ind3,Ind4,Ind2]),Ne,Ne2),
  check_chain(R,Ind1,Ind2,T,Ne2,Ne3),!.

check_chain(R,Ind1,Ind2,[_H|T],Ne,Ne1):-
  check_chain(R,Ind1,Ind2,T,Ne,Ne1),!.

% ------------------
is_transitive(R,_S,Ne):-
  %member(transitive(R,_),Ne),!.
  member(transitive(R),Ne),!.

is_transitive(_R,S,Ne):-
  %member(transitive(S,_),Ne),!.
  member(transitive(S),Ne),!.
% ------------------
copy_s(_,_,[],[]).

copy_s(AxO,AxN,[AxO|T],[AxN|T1]):-
  copy_s(AxO,AxN,T,T1).

copy_s(AxO,AxN,[H|T],[H|T1]):-
  copy_s(AxO,AxN,T,T1).
/* ************** */

/*
  forall_plus_rule
  =================
*/
forall_plus_rule((ABox,Tabs),([(classAssertion(allValuesFrom(R,C),Ind2),Expl)| ABox],Tabs)):-
  find((classAssertion(allValuesFrom(S,C),Ind1),Expl1),ABox),
  \+ indirectly_blocked(Ind1,(ABox,Tabs)),
  find((propertyAssertion(R,Ind1,Ind2),Expl2),ABox),
  find_sub_sup_trans_role(R,S,Ind1,Ind2,Expl3),
  append(Expl1,Expl2,ExplT),
  append(ExplT,Expl3,ExplT1),
  list_to_set(ExplT1,Expl),
  absent(classAssertion(allValuesFrom(R,C),Ind2),Expl,(ABox,Tabs)).
  
% --------------
find_sub_sup_trans_role(R,S,_Ind1,_Ind2,[subPropertyOf(R,S),transitive(R)]):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S),
  Name:transitiveProperty(R).

find_sub_sup_trans_role(R,S,_Ind1,_Ind2,[subPropertyOf(R,S)]):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S),
  \+ Name:transitiveProperty(R).
/* ************ */

/*
  unfold_rule
  ===========
*/
unfold_rule((ABox0,Tabs),([(classAssertion(D,Ind),[Ax|Expl])|ABox],Tabs)):-
  find((classAssertion(C,Ind),Expl),ABox0),
  find_sub_sup_class(C,D,Ax),
  absent(classAssertion(D,Ind),[Ax|Expl],(ABox0,Tabs)),
  add_nominal(D,Ind,ABox0,ABox).

/* -- unfold_rule
      takes a class C1 in which Ind belongs, find a not atomic class C
      that contains C1 (C is seen as list of classes), controls if
      the individual Ind belongs to all those classes, if yes find a class D (if exists)
      that is the superclass or an equivalent class of C and adds D to label af Ind
      - for managing tableau with more than one clash -
   --
*/
unfold_rule((ABox0,Tabs),([(classAssertion(D,Ind),[Ax|Expl1])|ABox],Tabs)):-
  find((classAssertion(C1,Ind),_Expl),ABox0),
  find_not_atomic(C1,C,L),
  find_all(Ind,L,ABox0,Expl1),
  find_sub_sup_class(C,D,Ax),
  absent(classAssertion(D,Ind),[Ax|Expl1],(ABox0,Tabs)),
  add_nominal(D,Ind,ABox0,ABox).

/* -- unfold_rule
 *    control propertyRange e propertyDomain
 * --
 */
unfold_rule((ABox0,Tabs),([(classAssertion(D,Ind),Expl)|ABox],Tabs)):-
  find_class_prop_range_domain(Ind,D,Expl,(ABox0,Tabs)),
  absent(classAssertion(D,Ind),Expl,(ABox0,Tabs)),
  add_nominal(D,Ind,ABox0,ABox).
 
/*
 * -- unfold_rule
 *    manage the negation
 * --
 */
unfold_rule((ABox0,Tabs),([(classAssertion(D,Ind),[complementOf(C)|Expl])|ABox],Tabs)):-
  find((classAssertion(complementOf(C),Ind),Expl),ABox0),
  find_neg_class(C,D),
  absent(classAssertion(D,Ind),[complementOf(C)|Expl],(ABox0,Tabs)),
  add_nominal(D,Ind,ABox0,ABox).
  
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

find_neg_class(subClassOf(C,D),intersectionOf(C,ND)):-
  neg_class(D,ND).

find_neg_class(complementOf(C),C).

find_neg_class(allValuesFrom(R,C),someValuesFrom(R,NC)):-
  neg_class(C,NC).

find_neg_class(someValuesFrom(R,C),allValuesFrom(R,NC)):-
  neg_class(C,NC).

find_neg_class(exactCardinality(N,R,C),unionOf([maxCardinality(NMax,R,C),minCardinality(NMin,R,C)])):-
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

find_class_prop_range_domain(Ind,D,[propertyRange(R,D)|ExplPA],(ABox,_Tabs)):-
  find((propertyAssertion(R,_,IndL),ExplPA),ABox),
  indAsList(IndL,L),
  member(Ind,L),
  get_trill_current_module(Name),
  Name:propertyRange(R,D).

find_class_prop_range_domain(Ind,D,[propertyDomain(R,D)|ExplPA],(ABox,_Tabs)):-
  find((propertyAssertion(R,IndL,_),ExplPA),ABox),
  indAsList(IndL,L),
  member(Ind,L),
  get_trill_current_module(Name),
  Name:propertyDomain(R,D).
	

%-----------------
find_sub_sup_class(C,D,subClassOf(C,D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D).

find_sub_sup_class(C,D,equivalentClasses(L)):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(C,L),
  member(D,L),
  C\==D.
  
/*******************
 managing the concept (C subclassOf Thing)
 this implementation doesn't work well in a little set of cases
 TO IMPROVE!
 *******************

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  subClassOf(A,B),
  member(C,[A,B]),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  classAssertion(C,_),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  equivalentClasses(L),
  member(C,L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  unionOf(L),
  member(C,L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  equivalentClasses(L),
  member(someValuesFrom(_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  equivalentClasses(L),
  member(allValuesFrom(_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  equivalentClasses(L),
  member(minCardinality(_,_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  equivalentClasses(L),
  member(maxCardinality(_,_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  equivalentClasses(L),
  member(exactCardinality(_,_,C),L),!.

*/

%--------------------

find_not_atomic(C,intersectionOf(L1),L1):-
  get_trill_current_module(Name),
  Name:subClassOf(A,B),
  member(intersectionOf(L1),[A,B]),
  member(C,L1).

find_not_atomic(C,unionOf(L1),L1):-
  get_trill_current_module(Name),
  Name:subClassOf(A,B),
  member(unionOf(L1),[A,B]),
  member(C,L1).

find_not_atomic(C,intersectionOf(L),L):-
  get_trill_current_module(Name),
  Name:intersectionOf(L),
  member(C,L).

find_not_atomic(C,unionOf(L),L):-
  get_trill_current_module(Name),
  Name:unionOf(L),
  member(C,L).

find_not_atomic(C,intersectionOf(L1),L1):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(intersectionOf(L1),L),
  member(C,L1).

find_not_atomic(C,unionOf(L1),L1):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(unionOf(L1),L),
  member(C,L1).

% -----------------------
find_all(_,[],_,[]).
  
find_all(Ind,[H|T],ABox,ExplT):-
  find((classAssertion(H,Ind),Expl1),ABox),
  find_all(Ind,T,ABox,Expl),
  append(Expl,Expl1,Expl2),
  list_to_set(Expl2,ExplT).


/* ************* */

/*
  ce_rule
  =============
*/
ce_rule((ABox0,(T,RBN,RBR)),(ABox,(T,RBN,RBR))):-
  find_not_sub_sup_class(Ax,UnAx),
  vertices(T,Inds),
  apply_ce_to(Inds,Ax,UnAx,ABox0,ABox,(T,RBN,RBR),C),
  C @> 0.
  

% ------------------
find_not_sub_sup_class(subClassOf(C,D),unionOf(complementOf(C),D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D),
  \+ atomic(C).


find_not_sub_sup_class(equivalentClasses(L),unionOf(L1)):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(C,L),
  \+ atomic(C),
  copy_neg_c(C,L,L1).

%-------------------------
copy_neg_c(_,[],[]).

copy_neg_c(C,[C|T],[complementOf(C)|T1]):-
  !,
  copy_neg_c(C,T,T1).

copy_neg_c(C,[H|T],[H|T1]):-
  copy_neg_c(C,T,T1).

%---------------------
apply_ce_to([],_,_,ABox,ABox,_,0).

apply_ce_to([Ind|T],Ax,UnAx,ABox0,[(classAssertion(UnAx,Ind),[Ax])|ABox],Tabs,C):-
  \+ indirectly_blocked(Ind,(ABox0,Tabs)),
  absent(classAssertion(UnAx,Ind),[Ax],(ABox0,Tabs)),!,
  apply_ce_to(T,Ax,UnAx,ABox0,ABox,Tabs,C0),
  C is C0 + 1.
  
apply_ce_to([_Ind|T],Ax,UnAx,ABox0,ABox,Tabs,C):-
  apply_ce_to(T,Ax,UnAx,ABox0,ABox,Tabs,C).

/* **************** */

/*
  min_rule
  =============
*/
min_rule((ABox,Tabs),([(differentIndividuals(NI),Expl)|ABox1],Tabs1)):-
  find((classAssertion(minCardinality(N,S),Ind1),Expl),ABox),
  \+ blocked(Ind1,(ABox,Tabs)),
  s_neighbours(Ind1,S,(ABox,Tabs),SN),
  safe_s_neigh(SN,S,(ABox,Tabs),SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh(NoI,S,Ind1,Expl,NI,ABox,Tabs,ABox1,Tabs1).
  

min_rule((ABox,Tabs),([(differentIndividuals(NI),Expl)|ABox1],Tabs1)):-
  find((classAssertion(minCardinality(N,S,C),Ind1),Expl),ABox),
  \+ blocked(Ind1,(ABox,Tabs)),
  s_neighbours(Ind1,S,(ABox,Tabs),SN),
  safe_s_neigh(SN,S,(ABox,Tabs),SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh_C(NoI,S,C,Ind1,Expl,NI,ABox,Tabs,ABox1,Tabs1).

% ----------------------
min_rule_neigh(0,_,_,_,[],ABox,Tabs,ABox,Tabs).
  
min_rule_neigh(N,S,Ind1,Expl,[Ind2|NI],ABox,Tabs,[(propertyAssertion(S,Ind1,Ind2),Expl)|ABox2],Tabs2):-
  N > 0,
  NoI is N-1,
  new_ind(Ind2),
  add_edge(S,Ind1,Ind2,Tabs,Tabs1),
  check_block(Ind2,([(propertyAssertion(S,Ind1,Ind2),Expl)|ABox],Tabs)),
  min_rule_neigh(NoI,S,Ind1,Expl,NI,ABox,Tabs1,ABox2,Tabs2).
  
%----------------------
min_rule_neigh_C(0,_,_,_,_,[],ABox,Tabs,ABox,Tabs).
  
min_rule_neigh_C(N,S,C,Ind1,Expl,[Ind2|NI],ABox,Tabs,[(propertyAssertion(S,Ind1,Ind2),Expl),
                                          (classAssertion(C,Ind2),[propertyAssertion(S,Ind1,Ind2)|Expl])|ABox2],Tabs2):-
  N > 0,
  NoI is N-1,
  new_ind(Ind2),
  add_edge(S,Ind1,Ind2,Tabs,Tabs1),
  check_block(Ind2,([(propertyAssertion(S,Ind1,Ind2),Expl)|ABox],Tabs)),
  min_rule_neigh_C(NoI,S,C,Ind1,Expl,NI,ABox,Tabs1,ABox2,Tabs2).
  
%---------------------
safe_s_neigh([],_,_,[]).

safe_s_neigh([H|T],S,Tabs,[H|ST]):-
  safe(H,S,Tabs),
  safe_s_neigh(T,S,Tabs,ST).
/* **************** */

/*
  max_rule
  ================
*/
max_rule((ABox0,Tabs0),L):-
  find((classAssertion(maxCardinality(N,S,C),Ind),Expl),ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  s_neighbours(Ind,S,(ABox0,Tabs0),SN),
  individual_class_C(SN,C,ABox0,SNC),
  length(SNC,LSS),
  LSS @> N,
  findall((ABox1,Tabs1),scan_max_list(S,SNC,Ind,Expl,ABox0,Tabs0, ABox1,Tabs1),L),
  L\=[],
  !.

max_rule((ABox0,Tabs0),L):-
  find((classAssertion(maxCardinality(N,S),Ind),Expl),ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  s_neighbours(Ind,S,(ABox0,Tabs0),SN),
  length(SN,LSS),
  LSS @> N,
  findall((ABox1,Tabs1),scan_max_list(S,SN,Ind,Expl,ABox0,Tabs0, ABox1,Tabs1),L),
  L\=[],
  !.
%---------------------

scan_max_list(S,SN,Ind,Expl,ABox0,Tabs0,ABox,Tabs):-
  member(YI,SN),
  member(YJ,SN),
  check_individuals_not_equal(YI,YJ,ABox0),
  find((propertyAssertion(S,Ind,YI),ExplYI),ABox0),
  find((propertyAssertion(S,Ind,YJ),ExplYJ),ABox0),
  append(ExplYI,ExplYJ,Expl0),
  append(Expl,Expl0,ExplT),
  merge_all([(sameIndividual([YI,YJ]),ExplT)],ABox0,Tabs0,ABox,Tabs).

%--------------------
check_individuals_not_equal(X,Y,ABox):-
  X\==Y,
  \+ same_ind([X],Y,ABox).
%--------------------
individual_class_C([],_,_,[]).

individual_class_C([H|T],C,ABox,[H|T1]):-
  find((classAssertion(C,H),_),ABox),
  individual_class_C(T,C,ABox,T1).

individual_class_C([H|T],C,ABox,T1):-
  \+ find((classAssertion(C,H),_),ABox),
  individual_class_C(T,C,ABox,T1).
/* *************** */

/*
 o_rule
 ============
*/

o_rule((ABox0,Tabs0),([(sameIndividual(LI),ExplC)|ABox],Tabs)):-
  find((classAssertion(oneOf([C]),X),ExplX),ABox0),
  find((classAssertion(oneOf([D]),Y),ExplY),ABox0),
  containsCommon(C,D),
  X\==Y,
  notDifferentIndividuals(X,Y,ABox0),
  nominal(C,(ABox0,Tabs0)),
  indAsList(X,LX),
  indAsList(Y,LY),
  append(ExplX,ExplY,ExplC),
  merge(X,Y,(ABox0,Tabs0),(ABox,Tabs)),
  flatten([LX,LY],LI0),
  list_to_set(LI0,LI),
  absent(sameIndividual(LI),ExplC,(ABox0,Tabs0)).
  
containsCommon(L1,L2):-
  member(X,L1),
  member(X,L2),!.
% -------------------

indAsList(sameIndividual(L),L):- 
  retract_sameIndividual(L),!.

indAsList(X,[X]):-
  atomic(X).

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
  member((classAssertion(C,I),Expl),ABox0).

/* ************ */


/*  absent
  =========
*/
absent(classAssertion(C,Ind),Expl,(ABox,_Tabs)):-
  \+ absent1(classAssertion(C,Ind),Expl,ABox),!.

absent(sameIndividual(L),Expl,(ABox,_Tabs)):-
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
  transpose(T,T1),
  ancestor(Ind,T,A),
  neighbours(Ind,T1,N),
  check_block1(Ind,A,N,(ABox,(T1,RBN,RBR))),!.
  
check_block(Ind,(ABox,(T,RBN,RBR))):-
  blockable(Ind,(ABox,(T,RBN,RBR))),
  transpose(T,T1),
  neighbours(Ind,T1,N),
  check_block2(N,(ABox,(T,RBN,RBR))),!.
  

check_block1(Indx,A,N,(ABox,(T,RBN,RBR))):-
  member(Indx0,N),
  member(Indy,A),
  member(Indy0,A),
  neighbours(Indy,T,N2),
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
  transpose(T,T1),
  neighbours(Ind,T1,N),
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
 check if L is subset of L1
 =================

subset([],_).

subset([H|T],L):-
  member(H,L),
  subset(T,L).
*/

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
  get_trill_current_module(Name),
  findall((classAssertion(Class,Individual),[classAssertion(Class,Individual)]),Name:classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),[propertyAssertion(Property,Subject, Object)]),Name:propertyAssertion(Property,Subject, Object),LPA),
  findall((propertyAssertion(Property,Subject,Object),[subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)]),subProp(Name,SubProperty,Property,Subject,Object),LSPA),
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

%---------------
subProp(Name,SubProperty,Property,Subject,Object):-
  Name:subPropertyOf(SubProperty,Property),Name:propertyAssertion(SubProperty,Subject,Object).

%--------------

add_nominal_list(ABox0,(T,_,_),ABox):-
  vertices(T,NomListIn),
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
new_tabs(([],ItR,RtI)):-
  rb_new(ItR),
  rb_new(RtI).

create_tabs([],G,G).

create_tabs([(propertyAssertion(P,S,O),_Expl)|T],Tabl0,Tabl):-
  add_edge(P,S,O,Tabl0,Tabl1),
  create_tabs(T,Tabl1,Tabl).
  
create_tabs([(differentIndividuals(Ld),_Expl)|T],(T0,RBN,RBR),(T,RBN,RBR)):-
  add_vertices(T0,Ld,T).

create_tabs([(classAssertion(_,I),_Expl)|Tail],(T0,RBN,RBR),(T,RBN,RBR)):-
  add_vertices(T0,[I],T1),
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

add_edge_to_tabl(_R,Ind1,Ind2,T0,T0):-
  graph_edge(Ind1,Ind2,T0),!.

add_edge_to_tabl(_R,Ind1,Ind2,T0,T1):-
  add_edges(T0,Ind1-Ind2,T1).

/*
  check for an edge
*/
graph_edge(Ind1,Ind2,T0):-
  edges(T0, Edges),
  member(Ind1-Ind2, Edges),!.

graph_edge(_,_,_).

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

remove_all_node_to_tree(_P,S,O,RB0,RB1):-
  rb_lookup((S,O),_,RB0),
  rb_delete(RB0,(S,O),RB1).

remove_all_node_to_tree(_P,S,O,RB0,_RB1):-
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

remove_edge_to_table(_P,S,O,T,T):-
  \+ graph_edge(S,O,T).

remove_edge_to_table(_P,S,O,T0,T1):-
  graph_edge(S,O,T0),
  del_edges(T0,[S-O],T1).

remove_node_to_table(S,T0,T1):-
  del_vertices(T0,[S],T1).

/*
 * merge node in tableau
 */

merge_tabs(X,Y,(T0,RBN0,RBR0),(T,RBN,RBR)):-
  (neighbours(X,T0,LSX0)->assign(LSX0,LSX);assign([],LSX)),
  (neighbours(Y,T0,LSY0)->assign(LSY0,LSY);assign([],LSY)),
  transpose(T0,TT),
  (neighbours(X,TT,LPX0)->assign(LPX0,LPX);assign([],LPX)),
  (neighbours(Y,TT,LPY0)->assign(LPY0,LPY);assign([],LPY)),
  flatten([X,Y],L0),
  list_to_set(L0,L),
  set_predecessor(L,X,LPX,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),!,
  set_successor(L,X,LSX,(T1,RBN1,RBR1),(T2,RBN2,RBR2)),!,
  set_predecessor(L,Y,LPY,(T2,RBN2,RBR2),(T3,RBN3,RBR3)),!,
  set_successor(L,Y,LSY,(T3,RBN3,RBR3),(T4,RBN4,RBR4)),!,
  remove_nodes(X,Y,(T4,RBN4,RBR4),(T,RBN,RBR)).

remove_nodes(X,Y,Tabs0,Tabs):-
  remove_node(X,Tabs0,Tabs1),
  remove_node(Y,Tabs1,Tabs).

remove_node(X,(T0,RBN0,RBR0),(T,RBN,RBR)):-
  (neighbours(X,T0,LS0)->assign(LS0,LS);assign([],LS)),
  transpose(T0,TT),
  (neighbours(X,TT,LP0)->assign(LP0,LP);assign([],LP)),
  remove_node1(X,LS,RBN0,RBR0,RBN1,RBR1),
  remove_node2(X,LP,RBN1,RBR1,RBN,RBR),
  (vertices(T0,VS),member(X,VS)->del_vertices(T0,[X],T);assign(T0,T)).

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
	

set_predecessor(_NN,_,[],Tabs,Tabs).

set_predecessor(NN,X,[H|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  rb_lookup((H,X),LR,RBN0),
  set_predecessor1(NN,H,LR,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_predecessor(NN,X,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_predecessor1(_NN,_H,[],Tabs,Tabs).

set_predecessor1(NN,H,[R|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  add_edge(R,H,NN,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_predecessor1(NN,H,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_successor(_NN,_X,[],Tabs,Tabs).

set_successor(NN,X,[H|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  rb_lookup((X,H),LR,RBN0),
  set_successor1(NN,H,LR,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_successor(NN,X,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_successor1(_NN,_H,[],Tabs,Tabs).

set_successor1(NN,H,[R|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  add_edge(R,NN,H,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_successor1(NN,H,L,(T1,RBN1,RBR1),(T,RBN,RBR)).
	  

/* merge node in (ABox,Tabs) */

merge_all([],ABox,Tabs,ABox,Tabs).

merge_all([(sameIndividual(H),Expl)|T],ABox0,Tabs0,ABox,Tabs):-
  find_same(H,ABox0,L,ExplL),
  L\==[],!,
  merge_all1(H,L,ABox0,Tabs0,ABox1,Tabs1),
  flatten([H,L],L0),
  list_to_set(L0,L1),
  append(Expl,ExplL,ExplT),
  add(ABox1,(sameIndividual(L1),ExplT),ABox2),
  delete(ABox2,(sameIndividual(L),ExplL),ABox3),
  retract_sameIndividual(L),
  merge_all(T,ABox3,Tabs1,ABox,Tabs).
  
merge_all([(sameIndividual(H),Expl)|T],ABox0,Tabs0,ABox,Tabs):-
  find_same(H,ABox0,L,_),
  L==[],!,
  merge_all2(H,ABox0,Tabs0,ABox1,Tabs1),
  add(ABox1,(sameIndividual(H),Expl),ABox2),
  merge_all(T,ABox2,Tabs1,ABox,Tabs).
  
merge_all1([],_,ABox,Tabs,ABox,Tabs).

merge_all1([H|T],L,ABox0,Tabs0,ABox,Tabs):-
  \+ member(H,L),
  merge(H,L,(ABox0,Tabs0),(ABox1,Tabs1)),
  merge_all1(T,[H|L],ABox1,Tabs1,ABox,Tabs).

merge_all1([H|T],L,ABox0,Tabs0,ABox,Tabs):-
  member(H,L),
  merge_all1(T,L,ABox0,Tabs0,ABox,Tabs).



merge_all2([X,Y|T],ABox0,Tabs0,ABox,Tabs):-
  merge(X,Y,(ABox0,Tabs0),(ABox1,Tabs1)),
  merge_all1(T,[X,Y],ABox1,Tabs1,ABox,Tabs).

find_same(H,ABox,L,Expl):-
  find((sameIndividual(L),Expl),ABox),
  member(X,L),
  member(X,H),!.

find_same(_H,_ABox,[],[]).

/*
 * merge
 */
merge(sameIndividual(L),Y,(ABox0,Tabs0),(ABox,Tabs)):-
  !,
  merge_tabs(L,Y,Tabs0,Tabs),
  merge_abox(L,Y,[],ABox0,ABox).

merge(X,sameIndividual(L),(ABox0,Tabs0),(ABox,Tabs)):-
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

merge_abox(_X,_Y,_,[],[]).

merge_abox(X,Y,Expl0,[(classAssertion(C,Ind),ExplT)|T],[(classAssertion(C,sameIndividual(L)),[sameIndividual(L)|Expl])|ABox]):-
  flatten([X,Y],L0),
  list_to_set(L0,L),
  member(Ind,L),!,
  append(Expl0,ExplT,Expl),
  merge_abox(X,Y,Expl0,T,ABox).

merge_abox(X,Y,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,sameIndividual(L),Ind2),[sameIndividual(L)|Expl])|ABox]):-
  flatten([X,Y],L0),
  list_to_set(L0,L),
  member(Ind1,L),!,
  append(Expl0,ExplT,Expl),
  merge_abox(X,Y,Expl0,T,ABox).

merge_abox(X,Y,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,Ind1,sameIndividual(L)),[sameIndividual(L)|Expl])|ABox]):-
  flatten([X,Y],L0),
  list_to_set(L0,L),
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
  graph_min_path(X,Y,T,P),
  all_node_blockable1(P,(ABox,(T,RBN,RBR))).

all_node_blockable1([],_).

all_node_blockable1([H|Tail],(ABox,(T,RBN,RBR))):-
  blockable(H,(ABox,(T,RBN,RBR))),
  all_node_blockable1(Tail,(ABox,(T,RBN,RBR))).
  
/*
  find a path in the graph
*/
graph_min_path(X,Y,T,P):-
  findall(Path, graph_min_path1(X,Y,T,Path), Ps),
  min_length(Ps,P).

graph_min_path1(X,Y,T,[X,Y]):-
  member(X-L,T),
  member(Y,L).
  
graph_min_path1(X,Y,T,[X|P]):-
  member(X-L,T),
  member(Z,L),
  graph_min_path1(Z,Y,T,P).
  
min_length([H],H).

min_length([H|T],MP):-
  min_length(T,P),
  length(H,N),
  length(P,NP), 
  (N<NP ->
     MP= H
   ;
     MP= P).
/*
 find all ancestor of a node
 
*/
ancestor(Ind,T,AN):-
  transpose(T,T1),
  ancestor1([Ind],T1,[],AN).

ancestor1([],_,A,A).

ancestor1([Ind|Tail],T,A,AN):-
  neighbours(Ind,T,AT),
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
 retract_sameIndividual(L)
 ==========
*/
retract_sameIndividual(L):-
  retract(sameIndividual(L)).

retract_sameIndividual(L):-
  \+ retract(sameIndividual(L)).
/* ****** */

/*
  find all S neighbours (S is a role)
*/
s_neighbours(Ind1,S,(ABox,(_,_,RBR)),SN):-
  rb_lookup(S,VN,RBR),
  s_neighbours1(Ind1,VN,SN1),
  s_neighbours2(SN1,SN1,SN,ABox).

s_neighbours(_Ind1,S,(_,_,RBR),[]):-
  \+ rb_lookup(S,_VN,RBR).

s_neighbours1(_,[],[]).

s_neighbours1(Ind1,[(Ind1,Y)|T],[Y|T1]):-
  s_neighbours1(Ind1,T,T1).

s_neighbours1(Ind1,[(X,_Y)|T],T1):-
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
same_ind(SN,H,_ABox):-
  get_trill_current_module(Name),
  Name:sameIndividual(SI),
  member(H,SI),
  member(H2,SI),
  member(H2,SN),
  H\==H2.

same_ind(SN,H,ABox):-
  find((sameIndividual(SI),_),ABox),
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

s_predecessors(_Ind1,S,(_ABox,(_,_,RBR)),[]):-
  \+ rb_lookup(S,_VN,RBR).

s_predecessors1(_,[],[]).

s_predecessors1(Ind1,[(Y,Ind1)|T],[Y|T1]):-
  s_predecessors1(Ind1,T,T1).

s_predecessors1(Ind1,[(_X,Y)|T],T1):-
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

/* *************
   Probability computation
   
   ************* */

/*
build_formula([],[],Var,Var).

build_formula([D|TD],TF,VarIn,VarOut):-
        build_term(D,[],[],VarIn,Var1),!,
        build_formula(TD,TF,Var1,VarOut).

build_formula([D|TD],[F|TF],VarIn,VarOut):-
        build_term(D,[],F,VarIn,Var1),
        build_formula(TD,TF,Var1,VarOut).

build_term([],F,F,Var,Var).

build_term([(Ax,S)|TC],F0,F,VarIn,VarOut):-!,
  (p_x(Ax,_)->
    (nth0_eq(0,NVar,VarIn,(Ax,S))->
      Var1=VarIn
    ;
      append(VarIn,[(Ax,S)],Var1),
      length(VarIn,NVar)
    ),
    build_term(TC,[[NVar,0]|F0],F,Var1,VarOut)
  ;
    (p(Ax,_)->
      (nth0_eq(0,NVar,VarIn,(Ax,[]))->
        Var1=VarIn
      ;
        append(VarIn,[(Ax,[])],Var1),
        length(VarIn,NVar) 
      ),
      build_term(TC,[[NVar,0]|F0],F,Var1,VarOut)
    ;
      build_term(TC,F0,F,VarIn,VarOut)
    )
  ).

build_term([Ax|TC],F0,F,VarIn,VarOut):-!,
  (p(Ax,_)->
    (nth0_eq(0,NVar,VarIn,(Ax,[]))->
      Var1=VarIn
    ;
      append(VarIn,[(Ax,[])],Var1),
      length(VarIn,NVar) 
    ),
    build_term(TC,[[NVar,0]|F0],F,Var1,VarOut)
  ;
    build_term(TC,F0,F,VarIn,VarOut)
  ).
*/

/* nth0_eq(PosIn,PosOut,List,El) takes as input a List,
an element El and an initial position PosIn and returns in PosOut
the position in the List that contains an element exactly equal to El
*/

/*
nth0_eq(N,N,[H|_T],El):-
        H==El,!.

nth0_eq(NIn,NOut,[_H|T],El):-
        N1 is NIn+1,
        nth0_eq(N1,NOut,T,El).

*/
/* var2numbers converts a list of couples (Rule,Substitution) into a list
of triples (N,NumberOfHeadsAtoms,ListOfProbabilities), where N is an integer
starting from 0 */
/*
var2numbers([],_N,[]).

var2numbers([(R,_S)|T],N,[[N,2,[Prob,Prob1,0.3,0.7]]|TNV]):-
        (p(R,_);p_x(R,_)),
        compute_prob_ax(R,Prob),!,
        Prob1 is 1-Prob,
        N1 is N+1,
        var2numbers(T,N1,TNV).

     
compute_prob_ax(R,Prob):-
  findall(P, p(R,P),Probs),
  compute_prob_ax1(Probs,Prob).
  
compute_prob_ax1([Prob],Prob):-!.

compute_prob_ax1([Prob1,Prob2],Prob):-!,
  Prob is Prob1+Prob2-(Prob1*Prob2).
  
compute_prob_ax1([Prob1 | T],Prob):-
  compute_prob_ax1(T,Prob0),
  Prob is Prob1 + Prob0 - (Prob1*Prob0).

*/  
  
  
/**************/
get_trill_current_module('translate_rdf'):-
  pengine_self(_Name),!.
get_trill_current_module('owl2_model'):- !.
/**************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(trill:init_test(_,_)).
sandbox:safe_primitive(trill:ret_prob(_,_,_)).
sandbox:safe_primitive(trill:end_test(_)).
sandbox:safe_primitive(trill:one(_,_)).
sandbox:safe_primitive(trill:zero(_,_)).
sandbox:safe_primitive(trill:and(_,_,_,_)).
sandbox:safe_primitive(trill:or(_,_,_,_)).
%sandbox:safe_primitive(trill:bdd_not(_,_,_)).
sandbox:safe_primitive(trill:get_var_n(_,_,_,_,_)).
sandbox:safe_primitive(trill:add_var(_,_,_,_,_)).
sandbox:safe_primitive(trill:equality(_,_,_,_)).


sandbox:safe_primitive(trill:sub_class(_,_)).
sandbox:safe_primitive(trill:sub_class(_,_,_)).
sandbox:safe_primitive(trill:prob_sub_class(_,_,_)).
sandbox:safe_primitive(trill:instanceOf(_,_)).
sandbox:safe_primitive(trill:instanceOf(_,_,_)).
sandbox:safe_primitive(trill:prob_instanceOf(_,_,_)).
sandbox:safe_primitive(trill:unsat(_)).
sandbox:safe_primitive(trill:unsat(_,_)).
sandbox:safe_primitive(trill:prob_unsat(_,_)).
sandbox:safe_primitive(trill:inconsistent_theory).
sandbox:safe_primitive(trill:inconsistent_theory(_)).
sandbox:safe_primitive(trill:prob_inconsistent_theory(_)).
sandbox:safe_primitive(trill:load_theory(_)).
sandbox:safe_primitive(trill:check_query_args(_)).

