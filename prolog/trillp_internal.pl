/* trillp predicates

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

:- use_module(library(clpb)).

/********************************
  SETTINGS
*********************************/
:- multifile setting_trill/2.
setting_trill(det_rules,[and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule]).
setting_trill(nondet_rules,[or_rule]).

set_up(M):-
  utility_translation:set_up(M),
  M:(dynamic exp_found/2).

clean_up(M):-
  utility_translation:clean_up(M),
  M:(dynamic exp_found/2),
  retractall(M:exp_found(_,_)).

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

% findall
find_n_explanations(M,QueryType,QueryArgs,Expls,_,Opt):- % This will not check the arg max_expl as TRILLP returns a pinpointing formula
  find_single_explanation(M,QueryType,QueryArgs,Expls,Opt),!.

find_n_explanations(_,_,_,Expls,_,_):-
  empty_expl(_,Expls).


compute_prob_and_close(M,Exps,Prob):-
  compute_prob(M,Exps,Prob).

% checks the explanation
check_and_close(_,Expl,Expl):-
  dif(Expl,[]).


% checks if an explanations was already found
find_expls(_M,[],_,[]):-!.

% checks if an explanations was already found (instance_of version)
find_expls(M,[Tab|T],[C,I],E):-
  get_clashes(Tab,Clashes),
  findall(E0,(member(Clash,Clashes),clash(M,Clash,Tab,E0)),Expls0),!,
  dif(Expls0,[]),
  or_all_f(M,Expls0,Expls1),
  find_expls(M,T,[C,I],E1),
  and_f(M,Expls1,E1,E),!.

% checks if an explanations was already found (property_value version)
find_expls(M,[Tab|T],[PropEx,Ind1Ex,Ind2Ex],E):-
  get_abox(Tab,ABox),
  findall(E0,find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),E0),ABox),Expls0),!,
  dif(Expls0,[]),
  or_all_f(M,Expls0,Expls1),
  find_expls(M,T,[PropEx,Ind1Ex,Ind2Ex],E1),
  and_f(M,Expls1,E1,E),!.
  

find_expls(M,[_Tab|T],Query,Expl):-
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
modify_ABox(M,Tab0,C,Ind,L0,Tab):-
  get_abox(Tab0,ABox0),
  findClassAssertion(C,Ind,Expl1,ABox0),!,
  dif(L0,Expl1),
  ((dif(L0,[]),subset(L0,Expl1)) -> 
     Expl = L0
   ;
     (subset(Expl1,L0) -> fail 
      ;
        (test(M,L0,Expl1),or_f(M,L0,Expl1,Expl))
     )
  ),
  remove_from_abox(ABox0,(classAssertion(C,Ind),Expl1),ABox),
  set_abox(Tab0,[(classAssertion(C,Ind),Expl)|ABox],Tab).
  
  
modify_ABox(M,Tab0,C,Ind,L0,Tab):-
  add_clash_to_tableau(M,Tab0,C-Ind,Tab1),
  get_abox(Tab0,ABox0),
  set_abox(Tab1,[(classAssertion(C,Ind),L0)|ABox0],Tab).

modify_ABox(M,Tab0,P,Ind1,Ind2,L0,Tab):-
  get_abox(Tab0,ABox0),
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox0),!,
  dif(L0,Expl1),
  ((dif(L0,[]),subset(L0,Expl1)) -> 
     Expl = L0
   ;
     % L0 is the new explanation, i.e. the \psi and Expl1 is the old label of an essertion
     (test(M,L0,Expl1),or_f(M,L0,Expl1,Expl))
  ),
  remove_from_abox(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl1),ABox),
  set_abox(Tab0,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox],Tab).
  
  
modify_ABox(_,Tab0,P,Ind1,Ind2,L0,Tab):-
  get_abox(Tab0,ABox0),
  set_abox(Tab0,[(propertyAssertion(P,Ind1,Ind2),L0)|ABox0],Tab).

/* ************* */


/*
  build_abox
  ===============
*/

build_abox(M,Tableau):-
  retractall(M:final_abox(_)),
  findall((classAssertion(Class,Individual),*([classAssertion(Class,Individual)])),M:classAssertion(Class,Individual),LCA),
  findall((propertyAssertion(Property,Subject, Object),*([propertyAssertion(Property,Subject, Object)])),(M:propertyAssertion(Property,Subject, Object),dif('http://www.w3.org/2000/01/rdf-schema#comment',Property)),LPA),
  % findall((propertyAssertion(Property,Subject,Object),*([subPropertyOf(SubProperty,Property),propertyAssertion(SubProperty,Subject,Object)])),subProp(M,SubProperty,Property,Subject,Object),LSPA),
  findall(nominal(NominalIndividual),M:classAssertion(oneOf(_),NominalIndividual),LNA),
  findall((differentIndividuals(Ld),*([differentIndividuals(Ld)])),M:differentIndividuals(Ld),LDIA),
  new_abox(ABox0),
  new_tabs(Tabs0),
  init_tableau(ABox0,Tabs0,Tableau0),
  append([LCA,LDIA,LPA],CreateTabsList),
  create_tabs(CreateTabsList,Tableau0,Tableau1),
  append([LCA,LPA,LNA,LDIA],AddAllList),
  add_all_to_tableau(M,AddAllList,Tableau1,Tableau2),
  findall((sameIndividual(L),*([sameIndividual(L)])),M:sameIndividual(L),LSIA),
  merge_all_individuals(M,LSIA,Tableau2,Tableau3),
  add_owlThing_list(M,Tableau3,Tableau),
  !.

/**********************

Explanation Management

***********************/

initial_expl(_M,[]):-!.

empty_expl(_M,[]):-!.

and_f_ax(M,Axiom,F0,F):-
  and_f(M,*([Axiom]),F0,F),!.

% and between two formulae
and_f(_,[],[],[]):-!.

and_f(_,[],F,F):-!.

and_f(_,F,[],F):-!.

% absorption for subformula (a * (b + (c * d))) * (c * d * e) = a * c * d * e
and_f(M,*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(*(AO1),O1),
  subset(AO1,A2),!,
  delete(A1,+(O1),A11),
  and_f(M,*(A11),*(A2),*(A)).
% (a * (b + c + e)) * (c * d) = a * c * d
and_f(M,*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(X,O1),
  member(X,A2),!,
  delete(A1,+(O1),A11),
  and_f(M,*(A11),*(A2),*(A)).
% absorption for subformula (c * d * e)  (a * (b + (c * d))) = c * d * e * a
and_f(M,*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(*(AO2),O2),
  subset(AO2,A1),!,
  delete(A2,+(O2),A21),
  and_f(M,*(A1),*(A21),*(A)).
% (c * d) * (a * (b + c + e)) = c * d * a
and_f(M,*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(X,O2),
  member(X,A1),!,
  delete(A2,+(O2),A21),
  and_f(M,*(A1),*(A21),*(A)).
% (a * b) * (a * c) = a * b * c
and_f(_M,*(A1),*(A2),*(A)):-!,
  append(A1,A2,A0),
  sort(A0,A).

% absorption x * (x + y) = x
and_f(_M,*(A1),+(O1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
and_f(_M,*(A1),+(O1),*(A)):-!,
  append(A1,[+(O1)],A).

% absorption x * (x + y) = x
and_f(_M,+(O1),*(A1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
and_f(_M,+(O1),*(A1),*(A)):-!,
  append([+(O1)],A1,A).

and_f(_M,+(O1),+(O2),*([+(O1),+(O2)])).
/*
and_f(M,[],[],[]):-!.
and_f(M,[],F2,F2):-!.
and_f(M,F1,[],F1):-!.
and_f(M,*(L1),*(L2),*(L)):-!,
  flatten([L1,L2],L0),
  remove_duplicates(L0,L).
and_f(M,+(L1),*(L2),*(L)):-!,
  flatten([L2,+(L1)],L0),
  remove_duplicates(L0,L).
and_f(M,*(L1),+(L2),*(L)):-!,
  flatten([L1,+(L2)],L0),
  remove_duplicates(L0,L).  
and_f(M,+(L1),+(L2),*(L)):-!,
  flatten([+(L1),+(L2)],L0),
  remove_duplicates(L0,L).
and_f(M,[El],[*(L2)],*(L)):-!,gtrace,
  flatten([El,L2],L0),
  remove_duplicates(L0,L).  
and_f(M,[El],[+(L2)],*(L)):-!,gtrace,
  flatten([El,+(L2)],L0),
  remove_duplicates(L0,L).
and_f(M,[*(L1)],[El],*(L)):-!,gtrace,
  flatten([El,L1],L0),
  remove_duplicates(L0,L).  
and_f(M,[+(L1)],[El],*(L)):-!,gtrace,
  flatten([El,+(L1)],L0),
  remove_duplicates(L0,L).  
and_f(M,[El1],[El2],*(L)):-gtrace,
  flatten([El1,El2],L0),
  remove_duplicates(L0,L).
*/

/*
% or between two formulae
or_f(M,[],[],[]):-!.

or_f(M,[],F,F):-!.

or_f(M,F,[],F):-!.

% absorption for subformula (a + (b * (c + d))) + (c + d + e) = a + c + d + e
or_f(M,+(A1),+(A2),+(A)):-
  member(*(O1),A1),
  member(+(AO1),O1),
  subset(AO1,A2),!,
  delete(A1,*(O1),A11),
  or_f(M,+(A11),+(A2),+(A)).
% (a + (b * c * e)) + (c + d) = a + c + d
or_f(M,+(A1),+(A2),+(A)):-
  member(*(O1),A1),
  member(X,O1),
  member(X,A2),!,
  delete(A1,*(O1),A11),
  or_f(M,+(A11),+(A2),+(A)).
% absorption for subformula (c + d + e)  (a + (b * (c + d))) = c + d + e + a
or_f(M,+(A1),+(A2),+(A)):-
  member(*(O2),A2),
  member(+(AO2),O2),
  subset(AO2,A1),!,
  delete(A2,*(O2),A21),
  or_f(M,+(A1),+(A21),+(A)).
% (c + d) + (a + (b * c * e)) = c + d + a
or_f(M,+(A1),+(A2),+(A)):-
  member(*(O2),A2),
  member(X,O2),
  member(X,A1),!,
  delete(A2,*(O2),A21),
  or_f(M,+(A1),+(A21),+(A)).
% (a + b) + (a + c) = a + b + c
or_f(M,+(A1),+(A2),+(A)):-!,
  append(A1,A2,A0),
  sort(A0,A).

% absorption x + (x * y) = x
or_f(M,*(A1),+(O1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
or_f(M,*(A1),+(O1),*(A)):-
  append(A1,[+(O1)],A).

% absorption x + (x * y) = x
or_f(M,+(O1),*(A1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
or_f(M,+(O1),*(A1),*(A)):-
  append([+(O1)],A1,A).

or_f(M,*(O1),*(O2),+([*(O1),*(O2)])).
*/

/*
* Cleans a complex formula A by removing sub-formulae which are permutation or subset 
* of other formulae contained in A
*/
val_min(F,LO):-
  formule_gen(F,LF),
  val_min2(LF,LO).
  
val_min2(L,LO):-
  val_min0(L,L,LSov),
  val_min1(L,L,LPer),
  remove_duplicates(LPer,LPer1),
  differenceFML(LSov,LPer1,LD),
  differenceFML(L,LD,LO).

val_min0([],_,[]):-!.
val_min0([X|T],L,[X|L2]):-
  member(Y,L),
  dif(Y,X),
  subset(Y,X),!,
  val_min0(T,L,L2).
val_min0([_|T],L,L2):-
  val_min0(T,L,L2).  
  
val_min1([],_,[]):-!.
val_min1([X|T],L,[Y|L2]):-
  member(Y,L),
  dif(Y,X),
  is_permutation(X,Y),!,
  val_min1(T,L,L2).
val_min1([_|T],L,L2):-
  val_min1(T,L,L2).

  
% difference between formulae
differenceFML([],_,[]).
differenceFML([T|Tail],L2,[T|Other]):- \+ member(T,L2),!,differenceFML(Tail,L2,Other).
differenceFML([_|C],L2,Diff):- differenceFML(C,L2,Diff).

% intersection between formulae
intersectionFML([],_,[]).
intersectionFML([T|C],L2,[T|Resto]):- member(T,L2),!,intersectionFML(C,L2,Resto).
intersectionFML([_|C],L2,LInt):- intersectionFML(C,L2,LInt).

%
is_or(+(_)).
is_and(*(_)).


find_and_in_formula(F,And):- findall( X, (member(X,F), \+ is_or(X)), And).
find_or_in_formula(F,Or):- member(+(Or),F),!.
  


% develops a formula
formule_gen([],[]):- !.
formule_gen(FC,F):-findall(XRD, (formula_gen(FC,X), remove_duplicates(X,XRD)), FCD), remove_duplicates(FCD,F).

formula_gen([],[]):-!.
formula_gen([X],[X]):- \+ is_and(X), \+ is_or(X),!.
formula_gen([*(FC)],F):-
  find_or_in_formula(FC,Or),!,
  find_and_in_formula(FC,And),
  member(X,Or),
  formula_gen([X],XF),
  append(And,XF,F).
formula_gen([*(FC)],And):- 
  find_and_in_formula(FC,And),!.
formula_gen([+(FC)],F):- 
  member(X,FC),
  formula_gen([X],XF),
  append([],XF,F).

% Decomposes a fomula
formule_decomp([],[],[],[],[],[],[]):- !.
formule_decomp([],[+(_F2)],[],[],[],[],[]):- !.
formule_decomp([*(F1)],[],AndF1,[],[],AndF1,[]):-
  find_and_in_formula(F1,AndF1),!.
formule_decomp([],[*(F2)],[],AndF2,[],[],AndF2):-
  find_and_in_formula(F2,AndF2),!.
formule_decomp([*(F1)],[*(F1)],AndF1,AndF1,AndF1,[],[]):- 
  find_and_in_formula(F1,AndF1),!.   
formule_decomp([*(F1)],[+(_F2)],AndF1,[],[],AndF1,[]):-
  find_and_in_formula(F1,AndF1),!.  
formule_decomp([*(F1)],[*(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2):-
  find_and_in_formula(F1,AndF1),
  find_and_in_formula(F2,AndF2),
  intersectionFML(AndF1,AndF2,AndUguali),
  differenceFML(AndF1,AndUguali,AndDiversiF1),
  differenceFML(AndF2,AndUguali,AndDiversiF2),!. 
formule_decomp([El1],[+(_F2)],[El1],[],[],[El1],[]):- !.
formule_decomp([El1],[*(F2)],[El1],AndF2,AndUguali,AndDiversiF1,AndDiversiF2):-
  find_and_in_formula(F2,AndF2),
  intersectionFML([El1],AndF2,AndUguali),
  differenceFML([El1],AndUguali,AndDiversiF1),
  differenceFML(AndF2,AndUguali,AndDiversiF2),!.   
formule_decomp([*(F1)],[El2],AndF1,[El2],AndUguali,AndDiversiF1,AndDiversiF2):-
  find_and_in_formula(F1,AndF1),
  intersectionFML(AndF1,[El2],AndUguali),
  differenceFML(AndF1,AndUguali,AndDiversiF1),
  differenceFML([El2],AndUguali,AndDiversiF2),!.   
formule_decomp([],[El2],[],[El2],[],[],[El2]):- !.
formule_decomp([El1],[],[El1],[],[],[El1],[]):- !.
formule_decomp([El],[El],[El],[El],[El],[],[]):- !.
formule_decomp([El1],[El2],[El1],[El2],[],[El1],[El2]):- !.
  
 
%computes a compact formula strarting from 2 formulae 
/*
or_f(M,[],[],[]):-!.

or_f(M,[],F,F):-!.

or_f(M,F,[],F):-!.
*/

or_all_f(_M,[H],H):-!.

or_all_f(M,[H|T],Expl):-
  or_all_f(M,T,Expl1),
  or_f(M,H,Expl1,Expl),!.

or_f(_M,F1,F2,F):-
  or_f_int([F1],[F2],[F]).

or_f_int([*(FC1)],[FC2],OrF):- !,
  findall( +(X), (member(+(X),FC1)), Or), length(Or,Length), 
  ( (Length > 1) ->
     (OrF = [+([*(FC1),FC2])])
   ;
     (formule_gen([*(FC1)],F1), or_scan(F1,[FC2],OrF))
  ).

or_f_int(FC1,FC2,OrF):- formule_gen(FC1,F1), or_scan(F1,FC2,OrF).

or_scan([],F2,F2):-!.
or_scan([T|C],F2,OrF):- ( T = [_] -> NT = T ; NT = [*(T)] ), or_between_formule(NT,F2,OrF1),or_scan(C,OrF1,OrF).
  
%computes a compact formula strarting from 2 formulae
or_between_formule(F1,[],F1):- !.
or_between_formule([],F2,F2):- !.
or_between_formule(F,F,F):- !.
/*
or_between_formule(F1,F2,F2):-
  nl,write('Zeresimo caso'),nl,
  formule_gen(F1,F1F),
  formule_gen(F2,F2F),
  findall(X1, (member(X,F1F),is_permutation(X,X1),member(X1,F2F)), Ris), is_permutation(Ris,F2F),!.
*/
or_between_formule(F1,[+(F2)],OrF):-
  formule_decomp(F1,[+(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2),
  or_between_formule1(F1,[+(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,F2,OrF),!. 
or_between_formule(F1,[*(F2)],OrF):-
  formule_decomp(F1,[*(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2),
  ( find_or_in_formula(F2,OrF2) -> true ; OrF2 = [] ),
  or_between_formule1(F1,[*(F2)],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,OrF2,OrF),!.
or_between_formule(F1,[El2],OrF):-
  formule_decomp(F1,[El2],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2),
  or_between_formule1(F1,[El2],AndF1,AndF2,AndUguali,AndDiversiF1,AndDiversiF2,[],OrF).  
  
or_between_formule1(_F1,_F2,_AndF1,_AndF2,AndUguali,[],_AndDiversiF2,_OrF2,OrF):-
  %nl,write('First case'),nl,
  !,
  ( AndUguali = [_] -> append([],AndUguali,OrF) ; OrF = [*(AndUguali)]).
or_between_formule1(_F1,_F2,_AndF1,_AndF2,AndUguali,_AndDiversiF1,[],[],OrF):-
  %nl,write('2nd case'),nl,
  !,
  ( AndUguali = [_] -> append([],AndUguali,OrF) ; OrF = [*(AndUguali)] ).
or_between_formule1(_F1,_F2,_AndF1,_AndF2,[],AndDiversiF1,AndDiversiF2,[],OrF):-
  %nl,write('3rd case'),nl,
  dif(AndDiversiF1,[]), dif(AndDiversiF2,[]),!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,NAndF1) ; NAndF1 = *(AndDiversiF1) ),
  ( AndDiversiF2 = [_] -> append([],AndDiversiF2,NAndF2) ; NAndF2 = *(AndDiversiF2) ),
  flatten([NAndF1, NAndF2], Or),
  OrF = [+(Or)].
or_between_formule1(_F1,_F2,_AndF1,_AndF2,AndUguali,AndDiversiF1,AndDiversiF2,[],OrF):-
  %nl,write('4th case'),nl,
  dif(AndDiversiF1,[]), dif(AndDiversiF2,[]), dif(AndUguali,[]),!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,NAndF1) ; NAndF1 = *(AndDiversiF1) ),
  ( AndDiversiF2 = [_] -> append([],AndDiversiF2,NAndF2) ; NAndF2 = *(AndDiversiF2) ),
  flatten([NAndF1, NAndF2], Or),
  flatten([AndUguali, +(Or)], And),
  OrF = [*(And)].
or_between_formule1(F1,_F2,_AndF1,_AndF2,[],AndDiversiF1,[],OrF2,OrF):-
  %nl,write('5th case'),nl,
  dif(AndDiversiF1,[]), dif(OrF2,[]),!,
  find_compatible_or(AndDiversiF1,OrF2,OrF2C,OrF2NC),
  ( dif(OrF2C,[]) -> (or_f_int(OrF2C,F1,OrFC), flatten([OrFC, OrF2NC], NOr) ) ; append(F1, OrF2,NOr) ),
  OrF = [+(NOr)].  
or_between_formule1(_F1,_F2,_AndF1,_AndF2,AndUguali,AndDiversiF1,[],OrF2,OrF):-
  %nl,write('6th case'),nl,
  dif(AndDiversiF1,[]), dif(AndUguali,[]), dif(OrF2,[]),!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,AndDiversiF1N) ; AndDiversiF1N = [*(AndDiversiF1)] ),
  find_compatible_or(AndDiversiF1,OrF2,OrF2C,OrF2NC),
  ( dif(OrF2C,[]) -> (or_f_int(OrF2C,AndDiversiF1N,OrFC),flatten([OrFC, OrF2NC], NOr) ) ; append(AndDiversiF1N, OrF2,NOr) ),
  flatten([AndUguali, +(NOr)], And),
  OrF = [*(And)].
or_between_formule1(_F1,_F2,_AndF1,_AndF2,[],AndDiversiF1,AndDiversiF2,OrF2,OrF):-
  %nl,write('7th case'),nl,
  dif(AndDiversiF1,[]), dif(AndDiversiF2,[]), dif(OrF2,[]),!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,AndDiversiF1N) ; AndDiversiF1N = *(AndDiversiF1) ),
  flatten([AndDiversiF2, +(OrF2)], AndF2N),
  NOrF2 = *(AndF2N),
  flatten([AndDiversiF1N, NOrF2], And),
  OrF = [+(And)].
or_between_formule1(_F1,_F2,_AndF1,_AndF2,AndUguali,AndDiversiF1,AndDiversiF2,OrF2,OrF):-
  %nl,write('8th case'),nl,
  dif(AndDiversiF1,[]), dif(AndDiversiF2,[]), dif(AndUguali,[]), dif(OrF2,[]),!,
  ( AndDiversiF1 = [_] -> append([],AndDiversiF1,AndDiversiF1N) ; AndDiversiF1N = *(AndDiversiF1) ),
  flatten([AndDiversiF2, +(OrF2)], AndF2N),
  NOrF2 = *(AndF2N),
  flatten([AndDiversiF1N, NOrF2], AndDiversi),
  flatten([AndUguali, +(AndDiversi)], And),
  OrF = [*(And)].

%optimization for 5th and 6th cases
find_compatible_or(F1,OrF2,OrF2C,OrF2NC):-  
  findall(  Y, ( member(Y,OrF2), ( Y = *(YN) -> find_and_in_formula(YN,AndY) ; AndY = [Y] ), intersectionFML(F1,AndY,I), dif(I,[]),!), OrF2C),
  differenceFML(OrF2,OrF2C,OrF2NC).
  
remove_duplicates(A,C):-sort(A,C).

/**********************

TRILLP SAT TEST

***********************/
/*
L1 is the \psi
L2 is the old label of the assertion, e.g. lab(n : D) in the unfold rule
I check if L1*(~L2) is satisfiable with sat/2. If it is satifiable it means that L1 does not model L2, i.e. \psi \not\models L2

*/
test(_M,L1,L2):-
  %build_f(L1,L2,F),
  %sat(F).
  create_formula(L1,L2,F),
  sat(F).

create_formula(L1,L2,(F1*(~(F2)))):-
  dif(L1,[]), dif(L2,[]),
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
  append(VH,V0,V10),
  sort(V10,V1),
  variabilize_formula(T,TV,V1,V2).

not_bool_op(H):-
  \+bool_op(H).

bool_op(+(_)):-!.
bool_op(*(_)):-!.
bool_op(~(_)):-!.


/**********************

Choice Points Management

***********************/

get_choice_point_id(_,0).

create_choice_point(_,_,_,_,_,0).

add_choice_point(_,_,Expl,Expl):- !.

/**********************

 TRILLP Probability Computation

***********************/

get_bdd_environment(_M,Env):-
  init(Env).

clean_environment(_M,Env):-
  end(Env).

build_bdd(_M,Env,[],BDD):-
  zero(Env,BDD).

build_bdd(M,Env,*(F),BDD):-
  bdd_and(M,Env,F,BDD).

build_bdd(M,Env,+(F),BDD):-
  bdd_or(M,Env,F,BDD).


bdd_and(M,Env,[+(X)],BDDX):-!,
  bdd_or(M,Env,X,BDDX).

bdd_and(_,_Env,[*(_X)],_BDDX):-
  write('error: *([*(_)])'),
  print_message(error,and_in_and),!,false.

bdd_and(M,Env,[X],BDDX):-
  get_prob_ax(M,X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_and(_M,Env,[_X],BDDX):- !,
  one(Env,BDDX).

bdd_and(M,Env,[+(H)|T],BDDAnd):-!,
  bdd_or(M,Env,H,BDDH),
  bdd_and(M,Env,T,BDDT),
  and(Env,BDDH,BDDT,BDDAnd).

bdd_and(_,_Env,[*(_H)|_T],_BDDX):-
  write('error: *([*(_)|_])'),
  print_message(error,and_in_and),!,false.

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


bdd_or(M,Env,[*(X)],BDDX):-!,
  bdd_and(M,Env,X,BDDX).

bdd_or(_,_Env,[+(_X)],_BDDX):-
  write('error: +([+(_)])'),
  print_message(error,or_in_or),!,false.

bdd_or(M,Env,[X],BDDX):-
  get_prob_ax(M,X,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VX),
  equality(Env,VX,0,BDDX),!.

bdd_or(_M,Env,[_X],BDDX):- !,
  one(Env,BDDX).

bdd_or(M,Env,[*(H)|T],BDDAnd):-!,
  bdd_and(M,Env,H,BDDH),
  bdd_or(M,Env,T,BDDT),
  or(Env,BDDH,BDDT,BDDAnd).

bdd_or(_,_Env,[+(_H)|_T],_BDDX):-
  write('error: +([+(_)|_])'),
  print_message(error,or_in_or),!,false.

bdd_or(M,Env,[H|T],BDDAnd):-
  get_prob_ax(M,H,AxN,Prob),!,
  ProbN is 1-Prob,
  get_var_n(Env,AxN,[],[Prob,ProbN],VH),
  equality(Env,VH,0,BDDH),
  bdd_or(M,Env,T,BDDT),
  or(Env,BDDH,BDDT,BDDAnd).
  
bdd_or(M,Env,[_H|T],BDDAnd):- !,
  zero(Env,BDDH),
  bdd_or(M,Env,T,BDDT),
  or(Env,BDDH,BDDT,BDDAnd).

