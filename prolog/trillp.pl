/** <module> trillp

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

:- module(trillp,[sub_class/2, sub_class/3, prob_sub_class/3,
                 instanceOf/2, instanceOf/3, prob_instanceOf/3,
                 property_value/3, property_value/4, prob_property_value/4,
                 unsat/1, unsat/2, prob_unsat/2,
                 inconsistent_theory/0, inconsistent_theory/1, prob_inconsistent_theory/1,
                 axiom/1, add_kb_prefix/2, add_kb_prefixes/1, add_axiom/1, add_axioms/1, remove_kb_prefix/2, remove_kb_prefix/1, remove_axiom/1, remove_axioms/1,
                 load_kb/1, load_owl_kb/1] ).

:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(library(rbtrees)).
:- use_module(library(dif)).
:- use_module(library(clpb)).
:- use_module(library(pengines)).
:- use_module(library(sandbox)).



:- use_foreign_library(foreign(bddem),install).

:- style_check(-discontiguous).

:- multifile
	owl2_model:axiom/1,
	owl2_model:class/1,
	owl2_model:annotationProperty/1,
	owl2_model:namedIndividual/1,
	owl2_model:objectProperty/1,
	owl2_model:dataProperty/1,
	owl2_model:transitiveProperty/1,
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


:- thread_local
	ind/1,
	exp_found/2.

/********************************
  SETTINGS
*********************************/
setting_trillp(det_rules,[and_rule,unfold_rule,add_exists_rule,forall_rule,exists_rule]).
setting_trillp(nondet_rules,[or_rule]).

/********************************
  LOAD KNOWLEDGE BASE
*********************************/
/**
 * load_kb(++FileName:kb_file_name) is det
 *
 * The predicate loads the knowledge base contained in the given file. 
 * The knowledge base must be defined in TRILL format, to use also OWL/RDF format
 * use the predicate owl_rdf/1.
 */
load_kb(FileName):-
  user:consult(FileName).

/**
 * load_owl_kb(++FileName:kb_file_name) is det
 *
 * The predicate loads the knowledge base contained in the given file. 
 * The knowledge base must be defined in pure OWL/RDF format.
 */
load_owl_kb(FileName):-
  load_owl(FileName).

/*****************************/

/*****************************
  UTILITY PREDICATES
******************************/
%defined in translate_rdf
:- multifile add_kb_prefix/2, add_kb_prefixes/1, add_axiom/1, add_axioms/1,
             remove_kb_prefix/2, remove_kb_prefix/1, remove_axiom/1, remove_axioms/1.

/**
 * add_kb_prefix(++ShortPref:string,++LongPref:string) is det
 *
 * This predicate registers the alias ShortPref for the prefix defined in LongPref.
 * The empty string '' can be defined as alias.
 */

/**
 * add_kb_prefixes(++Prefixes:list) is det
 *
 * This predicate registers all the alias prefixes contained in Prefixes.
 * The input list must contain pairs alias=prefix, i.e., [('foo'='http://example.foo#')].
 * The empty string '' can be defined as alias.
 */

/**
 * add_axiom(++Axiom:axiom) is det
 *
 * This predicate adds the given axiom to the knowledge base.
 * The axiom must be defined following the TRILL syntax.
 */

/**
 * add_axioms(++Axioms:list) is det
 *
 * This predicate adds the axioms of the list to the knowledge base.
 * The axioms must be defined following the TRILL syntax.
 */

/**
 * remove_kb_prefix(++ShortPref:string,++LongPref:string) is det
 *
 * This predicate removes from the registered aliases the one given in input.
 */

/**
 * remove_kb_prefix(++Name:string) is det
 *
 * This predicate takes as input a string that can be an alias or a prefix and 
 * removes the pair containing the string from the registered aliases.
 */

/**
 * remove_axiom(++Axiom:axiom) is det
 *
 * This predicate removes the given axiom from the knowledge base.
 * The axiom must be defined following the TRILL syntax.
 */

/**
 * remove_axioms(++Axioms:list) is det
 *
 * This predicate removes the axioms of the list from the knowledge base.
 * The axioms must be defined following the TRILL syntax.
 */

/**
 * axiom(?Axiom:axiom) is det
 *
 * This predicate searches in the loaded knowledge base axioms that unify with Axiom.
 */
axiom(Axiom):-
  get_trill_current_module(Name),
  Name:axiom(Axiom).

/*****************************
  MESSAGES
******************************/
:- multifile prolog:message/1.

prolog:message(iri_not_exists) -->
  [ 'IRIs not existent' ].

prolog:message(inconsistent) -->
  [ 'Inconsistent ABox' ].

prolog:message(consistent) -->
  [ 'Consistent ABox' ].

/****************************
  QUERY PREDICATES
*****************************/

/***********
  Queries
  - with and without explanations -
 ***********/
/**
 * instanceOf(++Class:concept_description,++Ind:individual_name,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns one explanation for the instantiation of the individual to the given class.
 * The returning explanation is a set of axioms.
 * The predicate fails if the individual does not belong to the class.
 */
instanceOf(Class,Ind,Expl):-
  ( check_query_args([Class,Ind],[ClassEx,IndEx]) *->
	retractall(exp_found(_,_)),
	retractall(trillan_idx(_)),
  	assert(trillan_idx(1)),
  	build_abox((ABox,Tabs)),
  	(  \+ clash((ABox,Tabs),_) *->
  	    (
  	    	add(ABox,(classAssertion(complementOf(ClassEx),IndEx),[]),ABox0),
	  	findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
  		find_expls(L,[ClassEx,IndEx],Expl),
  		dif(Expl,[])
  	    )
  	 ;
  	    print_message(warning,inconsistent),!,false
  	)
    ;
    	print_message(warning,iri_not_exists),!,false
  ).

/**
 * instanceOf(++Class:concept_description,++Ind:individual_name) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns true if the individual belongs to the class, false otherwise.
 */
instanceOf(Class,Ind):-
  (  check_query_args([Class,Ind],[ClassEx,IndEx]) *->
	(
	  retractall(exp_found(_,_)),
	  retractall(trillan_idx(_)),
	  assert(trillan_idx(1)),
	  build_abox((ABox,Tabs)),
	  (  \+ clash((ABox,Tabs),_) *->
	      (
	        add(ABox,(classAssertion(complementOf(ClassEx),IndEx),[]),ABox0),
	        apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),!,
	  	clash((ABox1,Tabs1),_),!
	      )
	    ;
	      print_message(warning,inconsistent),!,false
	  )
	)
    ;
        print_message(warning,iri_not_exists),!,false
  ).

/**
 * property_value(++Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns one explanation for the fact Ind1 is related with Ind2 via Prop.
 * The returning explanation is a set of axioms.
 * The predicate fails if the two individual are not Prop-related.
 */
property_value(Prop, Ind1, Ind2,Expl):-
  ( check_query_args([Prop,Ind1,Ind2],[PropEx,Ind1Ex,Ind2Ex]) *->
	retractall(exp_found(_,_)),
	retractall(trillan_idx(_)),
  	assert(trillan_idx(1)),
  	build_abox((ABox,Tabs)),
  	(  \+ clash((ABox,Tabs),_) *->
  	    (
  	    	findall((ABox1,Tabs1),apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),L),
  		find_expls(L,[PropEx,Ind1Ex,Ind2Ex],Expl),
  		dif(Expl,[])
  	    )
  	 ;
  	    print_message(warning,inconsistent),!,false
  	)
    ;
    	print_message(warning,iri_not_exists),!,false
  ).

/**
 * property_value(++Prop:property_name,++Ind1:individual_name,++Ind2:individual_name) is det
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns true if the two individual are Prop-related, false otherwise.
 */
property_value(Prop, Ind1, Ind2):-
  (  check_query_args([Prop,Ind1,Ind2],[PropEx,Ind1Ex,Ind2Ex]) *->
	(
	  retractall(exp_found(_,_)),
	  retractall(trillan_idx(_)),
	  assert(trillan_idx(1)),
	  build_abox((ABox,Tabs)),
	  (  \+ clash((ABox,Tabs),_) *->
	      (
	        apply_all_rules((ABox,Tabs),(ABox1,_Tabs1)),!,
	  	find((propertyAssertion(PropEx,Ind1Ex,Ind2Ex),_),ABox1),!
	      )
	    ;
	      print_message(warning,inconsistent),!,false
	  )
	)
    ;
        print_message(warning,iri_not_exists),!,false
  ).

/**
 * sub_class(++Class:concept_description,++SupClass:concept_description,-Expl:list)
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * one explanation for the subclass relation between Class and SupClass.
 * The returning explanation is a set of axioms.
 * The predicate fails if there is not a subclass relation between the two classes.
 */
sub_class(Class,SupClass,Expl):-
  ( check_query_args([Class,SupClass],[ClassEx,SupClassEx]) *->
	unsat_internal(intersectionOf([ClassEx,complementOf(SupClassEx)]),Expl)
    ;
    	print_message(warning,iri_not_exists),!,false
  ).
  

/**
 * sub_class(++Class:concept_description,++SupClass:concept_description) is det
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * true if Class is a subclass of SupClass, and false otherwise.
 */
sub_class(Class,SupClass):-
  ( check_query_args([Class,SupClass],[ClassEx,SupClassEx]) *->
        unsat_internal(intersectionOf([ClassEx,complementOf(SupClassEx)])),!
    ;
        print_message(warning,iri_not_exists),!,false
  ).

/**
 * unsat(++Concept:concept_description,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns one explanation for the unsatisfiability of the concept.
 * The returning explanation is a set of axioms.
 * The predicate fails if the concept is satisfiable.
 */
unsat(Concept,Expl):-
  (check_query_args([Concept],[ConceptEx]) *->
  	unsat_internal(ConceptEx,Expl)
    ;
    	print_message(warning,iri_not_exists),!,false
   ).

% ----------- %
unsat_internal(Concept,Expl):-
  retractall(exp_found(_,_)),
  retractall(trillan_idx(_)),
  assert(trillan_idx(2)),
  build_abox((ABox,Tabs)),
  ( \+ clash((ABox,Tabs),_) *->
     (
     	add(ABox,(classAssertion(Concept,trillan(1)),[]),ABox0),
	%findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
	findall((ABox1,Tabs1),apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),L),
	find_expls(L,['unsat',Concept],Expl),
	dif(Expl,[])
     )
    ;
     print_message(warning,inconsistent),!,false
  ).
% ----------- %

/**
 * unsat(++Concept:concept_description) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns true if the concept is unsatisfiable, false otherwise.
 */
unsat(Concept):-
  (check_query_args([Concept],[ConceptEx]) *->
  	unsat_internal(ConceptEx)
    ;
    	print_message(warning,iri_not_exists),!,false
   ).

% ----------- %
unsat_internal(Concept):-
  retractall(exp_found(_,_)),
  retractall(trillan_idx(_)),
  assert(trillan_idx(2)),
  build_abox((ABox,Tabs)),
  ( \+ clash((ABox,Tabs),_) *->
     (
     	add(ABox,(classAssertion(Concept,trillan(1)),[]),ABox0),
  	%findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
  	apply_all_rules((ABox0,Tabs),(ABox1,Tabs1)),!,
  	clash((ABox1,Tabs1),_),!
     )
    ;
     print_message(warning,inconsistent),!,false
  ).
% ----------- %

/**
 * inconsistent_theory(-Expl:list)
 *
 * This predicate returns one explanation for the inconsistency of the loaded knowledge base.
 * The returning explanation is a set of axioms.
 * The predicate fails if the knowledge base is consistent.
 */
inconsistent_theory(Expl):-
  retractall(exp_found(_,_)),
  retractall(trillan_idx(_)),
  assert(trillan_idx(1)),
  build_abox((ABox,Tabs)),
  findall((ABox1,Tabs1),apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),L),
  find_expls(L,['inconsistent','kb'],Expl),
  dif(Expl,[]).

/**
 * inconsistent_theory
 *
 * This predicate returns true if the knowledge base is inconsistent, false otherwise.
 */
inconsistent_theory:-
  retractall(exp_found(_,_)),
  retractall(trillan_idx(_)),
  assert(trillan_idx(1)),
  build_abox((ABox,Tabs)),
  \+ clash((ABox,Tabs),_),!,
  apply_all_rules((ABox,Tabs),(ABox1,Tabs1)),!,
  clash((ABox1,Tabs1),_),!.

inconsistent_theory:-
  print_message(warning,consistent).

/**
 * prob_instanceOf(++Class:concept_description,++Ind:individual_name,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns the probability of the instantiation of the individual to the given class.
 */
prob_instanceOf(Class,Ind,Prob):-
  ( check_query_args([Class,Ind],[ClassEx,IndEx]) *->
  	instanceOf(ClassEx,IndEx,Exps),
  	compute_prob(Exps,Prob)
  ;
  	print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_property_value(++Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns the probability of the fact Ind1 is related with Ind2 via Prop.
 */
prob_property_value(Prop, Ind1, Ind2,Prob):-
  ( check_query_args([Prop,Ind1,Ind2],[PropEx,Ind1Ex,Ind2Ex]) *->
  	property_value(PropEx,Ind1Ex,Ind2Ex,Exps),
  	compute_prob(Exps,Prob)
  ;
  	print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_sub_class(++Class:concept_description,++SupClass:class_name,--Prob:double) is det
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns 
 * the probability of the subclass relation between Class and SupClass.
 */
prob_sub_class(Class,SupClass,Prob):-
  ( check_query_args([Class,SupClass],[ClassEx,SupClassEx]) *->
  	sub_class(ClassEx,SupClassEx,Exps),
  	compute_prob(Exps,Prob)
  ;
  	print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_unsat(++Concept:concept_description,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns the probability of the unsatisfiability
 * of the concept.
 */
prob_unsat(Concept,Prob):-
  ( check_query_args([Concept],[ConceptEx]) *->
    unsat(ConceptEx,Exps),
    compute_prob(Exps,Prob)
  ;
    print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_inconsistent_theory(--Prob:double) is det
 *
 * This predicate returns the probability of the inconsistency of the loaded knowledge base.
 */
prob_inconsistent_theory(Prob):-
  inconsistent_theory(Exps),
  compute_prob(Exps,Prob).


% expands query arguments using prefixes and checks their existence in the kb
check_query_args(L,LEx) :-
  get_trill_current_module(Name),
  Name:ns4query(NSList),
  expand_all_ns(L,NSList,LEx), %from translate_rdf module
  check_query_args_presence(LEx,Name).

check_query_args_presence([],_).

check_query_args_presence([H|T],Name) :-
  atomic(H),!,
  find_atom_in_axioms(Name,H),%!,
  check_query_args_presence(T,Name).

check_query_args_presence([H|T],Name) :-
  \+ atomic(H),!,
  H =.. [_|L],
  flatten(L,L1),
  check_query_args_presence(L1,Name),
  check_query_args_presence(T,Name).

% looks for presence of atoms in kb's axioms
find_atom_in_axioms(Name,H):-
  Name:axiom(A),
  A =.. [_|L],
  flatten(L,L1),
  member(H,L1),!.

find_atom_in_axioms(Name,H):-
  (
    (
      ( Name:class(A) ; Name:annotationProperty(A) ; Name:namedIndividual(A) ; Name:objectProperty(A) ;
        Name:dataProperty(A)
      ),
      L=[A]
    )
   ;(
      ( Name:classAssertion(A,B) ; Name:subPropertyOf(A,B) ; Name:subClassOf(A,B) ; Name:propertyRange(A,B) ;
        Name:propertyDomain(A,B) ; Name:exactCardinality(A,B) ; Name:maxCardinality(A,B) ; Name:minCardinality(A,B)
      ),
      L=[A,B]
    )
   ;
    (
      ( Name:propertyAssertion(A,B,C) ; Name:annotationAssertion(A,B,C) ; Name:exactCardinality(A,B,C) ;
        Name:maxCardinality(A,B,C) ; Name:minCardinality(A,B,C)
      ),
      L=[A,B,C]
    )
   ;
    Name:equivalentClasses(L)
   ;
    Name:differentIndividuals(L)
   ;
    Name:sameIndividual(L)
   ;
    Name:intersectionOf(L)
   ;
    Name:unionOf(L)
  ),
  flatten(L,L1),
  member(H,L1),!.

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

/**************
  FIND FUNCTIONS
***************/

findClassAssertion(C,Ind,Expl1,ABox):-
	find((classAssertion(C,Ind),Expl1),ABox).

findPropertyAssertion(R,Ind1,Ind2,Expl1,ABox):-
	find((propertyAssertion(R,Ind1,Ind2),Expl1),ABox).


/****************************
  TABLEAU ALGORITHM
****************************/

%-------------
% clash managing
% previous version, manages only one clash at time
% need some tricks in some rules for managing the cases of more than one clash
% TO IMPROVE!
%------------
clash((ABox,_),Expl):-
  %write('clash 1'),nl,
  findClassAssertion(C,Ind,Expl1,ABox),
  findClassAssertion(complementOf(C),Ind,Expl2,ABox),
  and_f(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 2'),nl,
  find((sameIndividual(LS),Expl1),ABox),
  find((differentIndividuals(LD),Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  dif(X,Y),
  and_f(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 3'),nl,
  findClassAssertion(C,sameIndividual(L1),Expl1,ABox),
  findClassAssertion(complementOf(C),sameIndividual(L2),Expl2,ABox),
  member(X,L1),
  member(X,L2),!,
  and_f(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 4'),nl,
  findClassAssertion(C,Ind1,Expl1,ABox),
  findClassAssertion(complementOf(C),sameIndividual(L2),Expl2,ABox),
  member(Ind1,L2),
  and_f(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 5'),nl,
  findClassAssertion(C,sameIndividual(L1),Expl1,ABox),
  findClassAssertion(complementOf(C),Ind2,Expl2,ABox),
  member(Ind2,L1),
  and_f(Expl1,Expl2,Expl).

clash((ABox,_),Expl):-
  %write('clash 6'),nl,
  findall(Expl1,findClassAssertion("http://www.w3.org/2002/07/owl#Nothing",_Ind,Expl1,ABox),Expls),
  dif(Expls,[]),
  or_all(Expls,Expl).
  
/*
clash((ABox,Tabs),Expl):-
  %write('clash 7'),nl,
  findClassAssertion(maxCardinality(N,S,C),Ind,Expl1,ABox),
  s_neighbours(Ind,S,(ABox,Tabs),SN),
  individual_class_C(SN,C,ABox,SNC),
  length(SNC,LSS),
  LSS @> N,
  make_expl(Ind,S,SNC,Expl1,ABox,Expl2),
  flatten(Expl2,Expl3),
  list_to_set(Expl3,Expl).

clash((ABox,Tabs),Expl):-
  %write('clash 8'),nl,
  findClassAssertion(maxCardinality(N,S),Ind,Expl1,ABox),
  s_neighbours(Ind,S,(ABox,Tabs),SN),
  length(SN,LSS),
  LSS @> N,
  make_expl(Ind,S,SN,Expl1,ABox,Expl2),
  flatten(Expl2,Expl3),
  list_to_set(Expl3,Expl).
*/

% --------------
or_all([],[]).

or_all([H|T],Expl):-
  or_all(T,Expl1),
  or_f(H,Expl1,Expl).

/*
make_expl(_,_,[],Expl1,_,Expl1).

make_expl(Ind,S,[H|T],Expl1,ABox,[Expl2|Expl]):-
  findPropertyAssertion(S,Ind,H,Expl2,ABox),
  make_expl(Ind,S,T,Expl1,ABox,Expl).
*/


% -------------
% rules application
% -------------
apply_all_rules(ABox0,ABox):-
  setting_trillp(det_rules,Rules),
  apply_det_rules(Rules,ABox0,ABox1),
  (ABox0=ABox1 *->
  ABox=ABox1;
  apply_all_rules(ABox1,ABox)).

apply_det_rules([],ABox0,ABox):-
  setting_trillp(nondet_rules,Rules),
  apply_nondet_rules(Rules,ABox0,ABox).

apply_det_rules([H|_],ABox0,ABox):-
  %C=..[H,ABox,ABox1],
  call(H,ABox0,ABox),!.

apply_det_rules([_|T],ABox0,ABox):-
  apply_det_rules(T,ABox0,ABox).


apply_nondet_rules([],ABox,ABox).

apply_nondet_rules([H|_],ABox0,ABox):-
  %C=..[H,ABox,L],
  call(H,ABox0,L),!,
  member(ABox,L),
  dif(ABox0,ABox).

apply_nondet_rules([_|T],ABox0,ABox):-
  apply_nondet_rules(T,ABox0,ABox).


/***********
  rules
************/
/*
  add_exists_rule
  
  Looks up for a role that links 2 individuals, if it find it, it searches a subclass axiom
  in the KB that contains 'someValuesFrom(R,C)' where R is the role which links the 2 individuals
  and C is a class in which the 2nd individual belongs to.
  
  This rule hasn't a corresponding rule in the tableau
  ========================
*/
add_exists_rule((ABox0,Tabs),(ABox,Tabs)):-
  findPropertyAssertion(R,Ind1,Ind2,Expl1,ABox0),
  findClassAssertion(C,Ind2,Expl2,ABox0),
  existsInKB(R,C),
  and_f(Expl1,Expl2,Expl),
  modify_ABox(ABox0,someValuesFrom(R,C),Ind1,Expl,ABox).

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
  findClassAssertion(intersectionOf(LC),Ind,Expl,ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  scan_and_list(LC,Ind,Expl,ABox0,Tabs0,ABox,0).


%----------------
scan_and_list([],_Ind,_Expl,ABox,_Tabs,ABox,Mod):-
  dif(Mod,0).

scan_and_list([C|T],Ind,Expl,ABox0,Tabs0,ABox,_Mod):-
  modify_ABox(ABox0,C,Ind,Expl,ABox1),!,
  scan_and_list(T,Ind,Expl,ABox1,Tabs0,ABox,1).

scan_and_list([_C|T],Ind,Expl,ABox0,Tabs0,ABox,Mod):-
  scan_and_list(T,Ind,Expl,ABox0,Tabs0,ABox,Mod).
/* ************* */

/*
  or_rule
  ===============
*/
or_rule((ABox0,Tabs0),L):-
  findClassAssertion(unionOf(LC),Ind,Expl,ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  not_ind_intersected_union(Ind,LC,ABox0),
  length(LC,NClasses),
  findall((ABox1,Tabs0),scan_or_list(LC,NClasses,Ind,Expl,ABox0,Tabs0,ABox1),L),
  dif(L,[]),!.

not_ind_intersected_union(Ind,LC,ABox):-
  \+ ind_intersected_union(Ind,LC,ABox).

ind_intersected_union(Ind,LC,ABox) :-
  findClassAssertion(C,Ind,_,ABox),
  member(C,LC),!.
%---------------
scan_or_list([C],1,Ind,Expl,ABox0,_Tabs,ABox):- !,
  modify_ABox(ABox0,C,Ind,Expl,ABox).

scan_or_list([C|_T],_NClasses,Ind,Expl,ABox0,_Tabs,ABox):-
  modify_ABox(ABox0,C,Ind,Expl,ABox).

scan_or_list([_C|T],NClasses,Ind,Expl,ABox0,Tabs,ABox):-
  NC is NClasses - 1,
  scan_or_list(T,NC,Ind,Expl,ABox0,Tabs,ABox).
/* **************** */

/*
  exists_rule
  ==================
*/
exists_rule((ABox0,Tabs0),([(propertyAssertion(R,Ind1,Ind2),Expl),
    (classAssertion(C,Ind2),Expl)|ABox0],Tabs)):-
  findClassAssertion(someValuesFrom(R,C),Ind1,Expl,ABox0),
  \+ blocked(Ind1,(ABox0,Tabs0)),
  \+ connected_individual(R,C,Ind1,ABox0),
  new_ind(Ind2),
  add_edge(R,Ind1,Ind2,Tabs0,Tabs).


%---------------
connected_individual(R,C,Ind1,ABox):-
  findPropertyAssertion(R,Ind1,Ind2,_,ABox),
  findClassAssertion(C,Ind2,_,ABox).

/* ************ */

/*
  forall_rule
  ===================
*/
forall_rule((ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(allValuesFrom(R,C),Ind1,Expl1,ABox0),
  \+ indirectly_blocked(Ind1,(ABox0,Tabs)),
  findPropertyAssertion(R,Ind1,Ind2,Expl2,ABox0),
  and_f(Expl1,Expl2,Expl),
  modify_ABox(ABox0,C,Ind2,Expl,ABox).

/* ************** */

/*
  unfold_rule
  ===========
*/

unfold_rule((ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(C,Ind,Expl,ABox0),
  find_sub_sup_class(C,D,Ax),
  and_f([Ax],Expl,AxL),
  modify_ABox(ABox0,D,Ind,AxL,ABox1),
  add_nominal(D,Ind,ABox1,ABox).

/* -- unfold_rule
      takes a class C1 in which Ind belongs, find a not atomic class C
      that contains C1 (C is seen as list of classes), controls if
      the individual Ind belongs to all those classes, if yes it finds a class D (if exists)
      that is the superclass or an equivalent class of C and adds D to label of Ind
      - for managing tableau with more than one clash -
      correspond to the ce_rule
   --
*/
unfold_rule((ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(C1,Ind,Expl,ABox0),
  find_not_atomic(C1,C,L),
  ( C = unionOf(_) -> Expl1 = Expl ; find_all(Ind,L,ABox0,Expl1)),
  find_sub_sup_class(C,D,Ax),
  and_f([Ax],Expl1,AxL1),
  modify_ABox(ABox0,D,Ind,AxL1,ABox1),
  add_nominal(D,Ind,ABox1,ABox).

/* -- unfold_rule
 *    control propertyRange e propertyDomain
 * --
 */
unfold_rule((ABox0,Tabs),(ABox,Tabs)):-
  find_class_prop_range_domain(Ind,D,Expl,(ABox0,Tabs)),
  modify_ABox(ABox0,D,Ind,Expl,ABox1),
  add_nominal(D,Ind,ABox1,ABox).

/*
 * -- unfold_rule
 *    manage the negation
 * --
 */
unfold_rule((ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(complementOf(C),Ind,Expl,ABox0),
  find_neg_class(C,D),
  and_f([complementOf(C)],Expl,AxL),
  modify_ABox(ABox0,D,Ind,AxL,ABox1),
  add_nominal(D,Ind,ABox1,ABox).

% ----------------
% add_nominal

add_nominal(D,Ind,ABox0,ABox):-
  ((D = oneOf(_),
    \+ member(nominal(Ind),ABox0))
    *->
   (
     ABox1 = [nominal(Ind)|ABox0],
     (member((classAssertion('Thing',Ind),_E),ABox1)
     ->
     ABox = ABox1
     ;
     ABox = [(classAssertion('Thing',Ind),[])|ABox1]
     )
   )
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

/*
find_neg_class(exactCardinality(N,R,C),unionOf([maxCardinality(NMax,R,C),minCardinality(NMin,R,C)])):-
  NMax is N - 1,
  NMin is N + 1.

find_neg_class(minCardinality(N,R,C),maxCardinality(NMax,R,C)):-
  NMax is N - 1.

find_neg_class(maxCardinality(N,R,C),minCardinality(NMin,R,C)):-
  NMin is N + 1.
*/

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
  findPropertyAssertion(R,_,IndL,ExplPA,ABox),
  indAsList(IndL,L),
  member(Ind,L),
  get_trill_current_module(Name),
  Name:propertyRange(R,D).

find_class_prop_range_domain(Ind,D,[propertyDomain(R,D)|ExplPA],(ABox,_Tabs)):-
  findPropertyAssertion(R,IndL,_,ExplPA,ABox),
  indAsList(IndL,L),
  member(Ind,L),
  get_trill_current_module(Name),
  Name:propertyDomain(R,D).


%-----------------
% subClassOf
find_sub_sup_class(C,D,subClassOf(C,D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D).

%equivalentClasses
find_sub_sup_class(C,D,equivalentClasses(L)):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(C,L),
  member(D,L),
  dif(C,D).

%concept for concepts allValuesFrom
find_sub_sup_class(allValuesFrom(R,C),allValuesFrom(R,D),subClassOf(C,D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D).

%role for concepts allValuesFrom
find_sub_sup_class(allValuesFrom(R,C),allValuesFrom(S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%concept for concepts someValuesFrom
find_sub_sup_class(someValuesFrom(R,C),someValuesFrom(R,D),subClassOf(C,D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D).

%role for concepts someValuesFrom
find_sub_sup_class(someValuesFrom(R,C),someValuesFrom(S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

/*
%role for concepts exactCardinality
find_sub_sup_class(exactCardinality(N,R),exactCardinality(N,S),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%concept for concepts exactCardinality
find_sub_sup_class(exactCardinality(N,R,C),exactCardinality(N,R,D),subClassOf(C,D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D).

%role for concepts exactCardinality
find_sub_sup_class(exactCardinality(N,R,C),exactCardinality(N,S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%role for concepts maxCardinality
find_sub_sup_class(maxCardinality(N,R),maxCardinality(N,S),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%concept for concepts maxCardinality
find_sub_sup_class(maxCardinality(N,R,C),maxCardinality(N,R,D),subClassOf(C,D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D).

%role for concepts maxCardinality
find_sub_sup_class(maxCardinality(N,R,C),maxCardinality(N,S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%role for concepts minCardinality
find_sub_sup_class(minCardinality(N,R),minCardinality(N,S),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).

%concept for concepts minCardinality
find_sub_sup_class(minCardinality(N,R,C),minCardinality(N,R,D),subClassOf(C,D)):-
  get_trill_current_module(Name),
  Name:subClassOf(C,D).

%role for concepts minCardinality
find_sub_sup_class(minCardinality(N,R,C),minCardinality(N,S,C),subPropertyOf(R,S)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(R,S).
*/

/*******************
 managing the concept (C subclassOf Thing)
 this implementation doesn't work well in a little set of cases
 TO IMPROVE!
 *******************/
/*
find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:subClassOf(A,B),
  member(C,[A,B]),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:classAssertion(C,_),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(C,L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:unionOf(L),
  member(C,L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(someValuesFrom(_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(allValuesFrom(_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(minCardinality(_,_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(maxCardinality(_,_,C),L),!.

find_sub_sup_class(C,'Thing',subClassOf(C,'Thing')):-
  get_trill_current_module(Name),
  Name:equivalentClasses(L),
  member(exactCardinality(_,_,C),L),!.

*/

%--------------------
% looks for not atomic concepts descriptions containing class C
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
% puts together the explanations of all the concepts found by find_not_atomic/3
find_all(_,[],_,[]).

find_all(Ind,[H|T],ABox,ExplT):-
  findClassAssertion(H,Ind,Expl1,ABox),
  find_all(Ind,T,ABox,Expl2),
  and_f(Expl1,Expl2,ExplT).


% ------------------------
%  unfold_rule to unfold roles
% ------------------------
% sub properties
unfold_rule((ABox0,Tabs),(ABox,Tabs)):-
  findPropertyAssertion(C,Ind1,Ind2,Expl,ABox0),
  find_sub_sup_property(C,D,Ax),
  and_f([Ax],Expl,AxL),
  modify_ABox(ABox0,D,Ind1,Ind2,AxL,ABox1),
  add_nominal(D,Ind1,ABox1,ABox2),
  add_nominal(D,Ind2,ABox2,ABox).

%inverseProperties
unfold_rule((ABox0,Tabs),(ABox,Tabs)):-
  findPropertyAssertion(C,Ind1,Ind2,Expl,ABox0),
  find_inverse_property(C,D,Ax),
  and_f([Ax],Expl,AxL),
  modify_ABox(ABox0,D,Ind2,Ind1,AxL,ABox1),
  add_nominal(D,Ind1,ABox1,ABox2),
  add_nominal(D,Ind2,ABox2,ABox).

%-----------------
% subPropertyOf
find_sub_sup_property(C,D,subPropertyOf(C,D)):-
  get_trill_current_module(Name),
  Name:subPropertyOf(C,D).

%equivalentProperties
find_sub_sup_property(C,D,equivalentProperties(L)):-
  get_trill_current_module(Name),
  Name:equivalentProperties(L),
  member(C,L),
  member(D,L),
  dif(C,D).

%-----------------
%inverseProperties
find_inverse_property(C,D,inverseProperties(C,D)):-
  get_trill_current_module(Name),
  Name:inverseProperties(C,D).

find_inverse_property(C,D,inverseProperties(D,C)):-
  get_trill_current_module(Name),
  Name:inverseProperties(D,C).

/* ************* */

/***********
  update abox
  utility for tableau
************/
modify_ABox(ABox0,C,Ind,L0,[(classAssertion(C,Ind),Expl)|ABox]):-
  findClassAssertion(C,Ind,Expl1,ABox0),!,
  L0 \== Expl1,gtrace,
  (Expl1 == [] -> 
     Expl = L0
   ;
     (test(L0,Expl1),or_f(L0,Expl1,Expl))
  ),
  delete(ABox0,(classAssertion(C,Ind),Expl1),ABox).
  
  
modify_ABox(ABox0,C,Ind,L0,[(classAssertion(C,Ind),L0)|ABox0]).

modify_ABox(ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),Expl)|ABox]):-
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox0),!,
  L0 \== Expl1,gtrace,
  (Expl1 == [] -> 
     Expl = L0
   ;
     (test(L0,Expl1),or_f(L0,Expl1,Expl))
  ),
  delete(ABox0,(propertyAssertion(P,Ind1,Ind2),Expl1),ABox).
  
  
modify_ABox(ABox0,P,Ind1,Ind2,L0,[(propertyAssertion(P,Ind1,Ind2),L0)|ABox0]).

/* ************* */

% -------------------

indAsList(sameIndividual(L),L):-
  retract_sameIndividual(L),!.

indAsList(X,[X]):-
  atomic(X).



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
    *->
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
  transpose_ugraph(T,T1),
  ancestor(Ind,T,A),
  neighbours(Ind,T1,N),
  check_block1(Ind,A,N,(ABox,(T1,RBN,RBR))),!.

check_block(Ind,(ABox,(T,RBN,RBR))):-
  blockable(Ind,(ABox,(T,RBN,RBR))),
  transpose_ugraph(T,T1),
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
  transpose_ugraph(T,T1),
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

prepare_nom_list([H|T],[(nominal(H)),(classAssertion('Thing',H),[])|T1]):-
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
  	a red black tree RBN => each node is a pair of inds that contains the label for the edge
  	a red black tree RBR => each node is a property that contains the pairs of inds
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
  add_edges(T0,[Ind1-Ind2],T1).

/*
  check for an edge
*/
graph_edge(Ind1,Ind2,T0):-
  edges(T0, Edges),
  member(Ind1-Ind2, Edges),!.

%graph_edge(_,_,_).

/*
  remove edges and nodes from tableau

  To remove a node from the tableau use remove_node(Node,Tabs0,Tabs)
*/

% remove_all_nodes_from_tree(Property,Subject,Object,RBN0,RBN)
% removes from RBN the pair key-values with key (Subject,Object)
% key (Subject,Object) exists
remove_all_nodes_from_tree(_P,S,O,RB0,RB1):-
  rb_lookup((S,O),_,RB0),
  rb_delete(RB0,(S,O),RB1).

% key (Subject,Object) does not exist
remove_all_nodes_from_tree(_P,S,O,RB0,_RB1):-
  \+ rb_lookup((S,O),_,RB0).
% ----------------

% remove_role_from_tree(Property,Subject,Object,RBR0,RBR)
% remove in RBR the pair (Subject,Object) from the value associated with key Property
% pair (Subject,Object) does not exist for key Property
remove_role_from_tree(P,S,O,RB,RB):-
  rb_lookup(P,V,RB),
  \+ member((S,O),V).

% pair (Subject,Object) exists for key Property but it is not the only pair associated to it
remove_role_from_tree(P,S,O,RB0,RB1):-
  rb_lookup(P,V,RB0),
  member((S,O),V),
  delete(V,(S,O),V1),
  dif(V1,[]),
  rb_update(RB0,P,V1,RB1).

% pair (Subject,Object) exists for key Property and it is the only pair associated to it
remove_role_from_tree(P,S,O,RB0,RB1):-
  rb_lookup(P,V,RB0),
  member((S,O),V),
  delete(V,(S,O),V1),
  V1==[],
  rb_delete(RB0,P,RB1).
% ----------------

% remove_edge_from_table(Property,Subject,Object,Tab0,Tab)
% removes from T the edge from Subject to Object
remove_edge_from_table(_P,S,O,T,T):-
  \+ graph_edge(S,O,T).

remove_edge_from_table(_P,S,O,T0,T1):-
  graph_edge(S,O,T0),
  del_edges(T0,[S-O],T1).
% ----------------

% remove_node_from_table(Subject,Tab0,Tab)
% removes from T the node corresponding to Subject
remove_node_from_table(S,T0,T1):-
  del_vertices(T0,[S],T1).

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

/*
 * merge node in tableau
 */

merge_tabs(X,Y,(T0,RBN0,RBR0),(T,RBN,RBR)):-
  (neighbours(X,T0,LSX0)*->assign(LSX0,LSX);assign([],LSX)),
  (neighbours(Y,T0,LSY0)*->assign(LSY0,LSY);assign([],LSY)),
  transpose_ugraph(T0,TT),
  (neighbours(X,TT,LPX0)*->assign(LPX0,LPX);assign([],LPX)),
  (neighbours(Y,TT,LPY0)*->assign(LPY0,LPY);assign([],LPY)),
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

% Collects all the connected in input (LP, predecessor) or in output (LS, successor) for node X
% removes from RBN (remove_all_nodes_from_tree) all the pairs key-value where the key contains node X (pairs (X,Ind1) and (Ind1,X))
% and from RBR (remove_edges->remove_role_from_tree) all the pairs containing X from the values of the roles entering in or exiting from X
remove_node(X,(T0,RBN0,RBR0),(T,RBN,RBR)):-
  (neighbours(X,T0,LS0)*->assign(LS0,LS);assign([],LS)),
  transpose_ugraph(T0,TT),
  (neighbours(X,TT,LP0)*->assign(LP0,LP);assign([],LP)),
  remove_node1(X,LS,RBN0,RBR0,RBN1,RBR1),
  remove_node2(X,LP,RBN1,RBR1,RBN,RBR),
  (vertices(T0,VS),member(X,VS)*->del_vertices(T0,[X],T);assign(T0,T)).

remove_node1(_,[],RBN,RBR,RBN,RBR).

remove_node1(X,[H|T],RBN0,RBR0,RBN,RBR):-
  rb_lookup((X,H),V,RBN0),
  remove_edges(V,X,H,RBR0,RBR1),
  remove_all_nodes_from_tree(_,X,H,RBN0,RBN1),
  remove_node1(X,T,RBN1,RBR1,RBN,RBR).

remove_node2(_,[],RBN,RBR,RBN,RBR).

remove_node2(X,[H|T],RBN0,RBR0,RBN,RBR):-
  rb_lookup((H,X),V,RBN0),
  remove_edges(V,H,X,RBR0,RBR1),
  remove_all_nodes_from_tree(_,H,X,RBN0,RBN1),
  remove_node1(X,T,RBN1,RBR1,RBN,RBR).

remove_edges([],_,_,RBR,RBR).

remove_edges([H|T],S,O,RBR0,RBR):-
  remove_role_from_tree(H,S,O,RBR0,RBR1),
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

/* merge node in (ABox,Tabs) */

merge_all([],ABox,Tabs,ABox,Tabs).

merge_all([(sameIndividual(H),Expl)|T],ABox0,Tabs0,ABox,Tabs):-
  find_same(H,ABox0,L,ExplL),
  dif(L,[]),!,
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

/* end of abox a s list */

/*
  creation of a new individual

*/
new_ind(trillan(I)):-
  retract(trillan_idx(I)),
  I1 is I+1,
  assert(trillan_idx(I1)).

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
  findClassAssertion(C,X,_,ABox),
  \+ findClassAssertion(C,Y,_,ABox).

different_label(X,Y,ABox):-
  findClassAssertion(C,Y,_,ABox),
  \+ findClassAssertion(C,X,_,ABox).


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
  (N<NP *->
     MP= H
   ;
     MP= P).
/*
 find all ancestor of a node

*/
ancestor(Ind,T,AN):-
  transpose_ugraph(T,T1),
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
  get_trill_current_module(N),
  retract(N:sameIndividual(L)).

retract_sameIndividual(L):-
  get_trill_current_module(N),
  \+ retract(N:sameIndividual(L)).
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
  dif(X,Ind1),
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
  dif(H,H2).

same_ind(SN,H,ABox):-
  find((sameIndividual(SI),_),ABox),
  member(H,SI),
  member(H2,SI),
  member(H2,SN),
  dif(H,H2).

/* ************* */

/*
 s_predecessors
 ==============
 find all S-predecessor of Ind
*/

s_predecessors(Ind1,S,(ABox,(_,_,RBR)),SN):-
  rb_lookup(S,VN,RBR),
  s_predecessors1(Ind1,VN,SN1),
  s_predecessors2(SN1,SN,ABox).

s_predecessors(_Ind1,S,(_ABox,(_,_,RBR)),[]):-
  \+ rb_lookup(S,_VN,RBR).

s_predecessors1(_,[],[]).

s_predecessors1(Ind1,[(Y,Ind1)|T],[Y|T1]):-
  s_predecessors1(Ind1,T,T1).

s_predecessors1(Ind1,[(_X,Y)|T],T1):-
  dif(Y,Ind1),
  s_predecessors1(Ind1,T,T1).

s_predecessors2([],[],_).

s_predecessors2([H|T],[H|T1],ABox):-
  s_predecessors2(T,T1,ABox),
  \+ same_ind(T1,H,ABox).

s_predecessors2([H|T],T1,ABox):-
  s_predecessors2(T,T1,ABox),
  same_ind(T1,H,ABox).

/* ********** */

/**********************

TRILLP PINPOINTING FORMULA MANAGEMENT

***********************/

% and between two formulae
and_f([],[],[]):-!.

and_f([],F,F):-!.

and_f(F,[],F):-!.

and_f(*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(*(AO1),O1),
  subset(AO1,A2),!,
  delete(A1,+(O1),A11),
  and_f(*(A11),*(A2),*(A)).
and_f(*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(X,O1),
  member(X,A2),!,
  delete(A1,+(O1),A11),
  and_f(*(A11),*(A2),*(A)).
and_f(*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(*(AO2),O2),
  subset(AO2,A1),!,
  delete(A2,+(O2),A21),
  and_f(*(A1),*(A21),*(A)).
and_f(*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(X,O2),
  member(X,A1),!,
  delete(A2,+(O2),A21),
  and_f(*(A1),*(A21),*(A)).
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

or_f(*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(*(AO1),O1),
  subset(AO1,A2),!,
  delete(A1,+(O1),A11),
  or_f(*(A11),*(A2),*(A)).
or_f(*(A1),*(A2),*(A)):-
  member(+(O1),A1),
  member(X,O1),
  member(X,A2),!,
  delete(A1,+(O1),A11),
  or_f(*(A11),*(A2),*(A)).
or_f(*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(*(AO2),O2),
  subset(AO2,A1),!,
  delete(A2,+(O2),A21),
  or_f(*(A1),*(A21),*(A)).
or_f(*(A1),*(A2),*(A)):-
  member(+(O2),A2),
  member(X,O2),
  member(X,A1),!,
  delete(A2,+(O2),A21),
  or_f(*(A1),*(A21),*(A)).
or_f(*(A1),*(A2),*(A)):-!,
  append(A1,A2,A0),
  sort(A0,A).

% absorption x * (x + y) = x
or_f(*(A1),+(O1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
or_f(*(A1),+(O1),*(A)):-
  append(A1,[+(O1)],A).

% absorption x * (x + y) = x
or_f(+(O1),*(A1),*(A1)):-
  member(X,A1),
  member(X,O1),!.
or_f(+(O1),*(A1),*(A)):-
  append([+(O1)],A1,A).

or_f(+(O1),+(O2),*([+(O1),+(O2)])).


/**********************

TRILLP SAT TEST

***********************/

test(L1,L2):-
  %build_f(L1,L2,F),
  %sat(F).
  sat((L1*(~(L2)))).

build_f([L1],[L2],(F1*(~(F2)))):-
  build_f1(L1,F1,[],Var1),
  build_f1(L2,F2,Var1,_Var).

build_f1(*(L),F,Var0,Var):-!,
  build_and(L,F,Var0,Var).
build_f1(+(L),F,Var0,Var):-!,
  build_or(L,F,Var0,Var).
build_f1(H,C,Var0,Var):-
  give_me_C(H,C,Var0,Var).
  
build_and([H|T],F,Var0,Var):-
  T==[],
  build_f1(H,F,Var0,Var).

build_and([H|T],F1 * F2,Var0,Var):-
  build_f1(H,F1,Var0,Var1),
  build_and(T,F2,Var1,Var).
  
build_or([H|T],F,Var0,Var):-
  T==[],
  build_f1(H,F,Var0,Var).

build_or([H|T],F1 + F2,Var0,Var):-
  build_f1(H,F1,Var0,Var1),
  build_or(T,F2,Var1,Var).

give_me_C(H,D,Var0,Var0):-
  member(corr(H,D),Var0),!.
give_me_C(H,C,Var0,[corr(H,C)|Var0]).

/**********************

TRILLP COMPUTEPROB

***********************/

:- thread_local
	%get_var_n/5,
        rule_n/1,
        na/2,
        v/3.

%rule_n(0).

compute_prob(Expl,Prob):-
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  get_trill_current_module(Name),
  findall(1,Name:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',_,_),NAnnAss),length(NAnnAss,NV),
  init_test(NV,Env),
  build_bdd(Env,Expl,BDD),
  ret_prob(Env,BDD,Prob),
  end_test(Env), !.



build_bdd(Env,[X],BDD):- 
  \+ is_and(X), \+ is_or(X),!,
  bdd_and(Env,[X],BDD).

build_bdd(Env,[*(F)],BDDF):-
  find_or_in_formula(F,Or),
  build_bdd(Env,[+(Or)],BDDOr),
  find_and_in_formula(F,And),
  bdd_and(Env,And,BDDAnd),
  and(Env,BDDAnd,BDDOr,BDDF),!.
build_bdd(Env,[*(And)],BDDAnd):- 
  get_trill_current_module(Name),
  findall(El,(member(El,And),
   Name:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',El,_)), AndNew),
  ( AndNew = [] -> Expl = [] ; (AndNew = [X] -> Expl = [X]; Expl = [*(AndNew)]) ),
  bdd_and(Env,And,BDDAnd),!.
build_bdd(Env,[+(F)],BDOr):-
  findall( BDDEl, (member(El,F), build_bdd(Env,[El],BDDEl)), BDDList),
  bdd_or(Env,BDDList,BDOr).

build_bdd(Env,[],BDD):- !,
  zero(Env,BDD).

bdd_or(Env,[BDDX],BDDX):- !.
bdd_or(Env,[BDDH|BDDT],BDDOr):-
  bdd_or(Env,BDDT,BDDOrT),
  or(Env,BDDH,BDDOrT,BDDOr).

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




get_var_n(Env,R,S,Probs,V):-
  (
    v(R,S,V) ->
      true
    ;
      length(Probs,L),
      add_var(Env,L,Probs,R,V),
      assert(v(R,S,V))
  ).


get_prob_ax((Ax,_Ind),N,Prob):- !,
  compute_prob_ax(Ax,Prob),
  ( na(Ax,N) ->
      true
    ;
      rule_n(N),
      assert(na(Ax,N)),
      retract(rule_n(N)),
      N1 is N + 1,
      assert(rule_n(N1))
  ).
get_prob_ax(Ax,N,Prob):- !,
  compute_prob_ax(Ax,Prob),
  ( na(Ax,N) ->
      true
    ;
      rule_n(N),
      assert(na(Ax,N)),
      retract(rule_n(N)),
      N1 is N + 1,
      assert(rule_n(N1))
  ).

compute_prob_ax(Ax,Prob):-
  get_trill_current_module(Name),
  findall(ProbA,(Name:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',Ax,literal(ProbAT)),atom_number(ProbAT,ProbA)),Probs),
  compute_prob_ax1(Probs,Prob).

compute_prob_ax1([Prob],Prob):-!.

compute_prob_ax1([Prob1,Prob2],Prob):-!,
  Prob is Prob1+Prob2-(Prob1*Prob2).

compute_prob_ax1([Prob1 | T],Prob):-
  compute_prob_ax1(T,Prob0),
  Prob is Prob1 + Prob0 - (Prob1*Prob0).

/************************/

/**************/
/*get_trill_current_module('translate_rdf'):-
  pengine_self(_Name),!.*/
%get_trill_current_module(Name):-
%  pengine_self(Name),!.
get_trill_current_module('owl2_model'):- !.
/**************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(trillp:init_test(_,_)).
sandbox:safe_primitive(trillp:ret_prob(_,_,_)).
sandbox:safe_primitive(trillp:end_test(_)).
sandbox:safe_primitive(trillp:one(_,_)).
sandbox:safe_primitive(trillp:zero(_,_)).
sandbox:safe_primitive(trillp:and(_,_,_,_)).
sandbox:safe_primitive(trillp:or(_,_,_,_)).
sandbox:safe_primitive(trillp:bdd_not(_,_,_)).
sandbox:safe_primitive(trillp:get_var_n(_,_,_,_,_)).
sandbox:safe_primitive(trillp:add_var(_,_,_,_,_)).
sandbox:safe_primitive(trillp:equality(_,_,_,_)).


sandbox:safe_primitive(trillp:sub_class(_,_)).
sandbox:safe_primitive(trillp:sub_class(_,_,_)).
sandbox:safe_primitive(trillp:prob_sub_class(_,_,_)).
sandbox:safe_primitive(trillp:instanceOf(_,_)).
sandbox:safe_primitive(trillp:instanceOf(_,_,_)).
sandbox:safe_primitive(trillp:prob_instanceOf(_,_,_)).
sandbox:safe_primitive(trillp:property_value(_,_,_)).
sandbox:safe_primitive(trillp:property_value(_,_,_,_)).
sandbox:safe_primitive(trillp:prob_property_value(_,_,_,_)).
sandbox:safe_primitive(trillp:unsat(_)).
sandbox:safe_primitive(trillp:unsat(_,_)).
sandbox:safe_primitive(trillp:prob_unsat(_,_)).
sandbox:safe_primitive(trillp:inconsistent_theory).
sandbox:safe_primitive(trillp:inconsistent_theory(_)).
sandbox:safe_primitive(trillp:prob_inconsistent_theory(_)).
sandbox:safe_primitive(trillp:axiom(_)).
sandbox:safe_primitive(trillp:add_kb_prefix(_,_)).
sandbox:safe_primitive(trillp:add_axiom(_)).
sandbox:safe_primitive(trillp:add_axioms(_)).
sandbox:safe_primitive(trillp:load_kb(_)).
sandbox:safe_primitive(trillp:load_owl_kb(_)).

:- use_module(translate_rdf).

:- if(\+pengine_self(_Name)).
:- trill.
:- endif.

