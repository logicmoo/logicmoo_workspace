/** <module> trill

This module performs reasoning over probabilistic description logic knowledge bases.
It reads probabilistic knowledge bases in RDF format or in Prolog format, a functional-like
sintax based on definitions of Thea library, and answers queries by finding the set 
of explanations or computing the probability.

[1] http://vangelisv.github.io/thea/

See https://github.com/rzese/trill/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~rzese/software/trill/manual.html for
details.

@version 5.2.1
@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/

:- module(trill,[sub_class/2, sub_class/3, prob_sub_class/3, sub_class/4,
                 instanceOf/2, instanceOf/3, prob_instanceOf/3, instanceOf/4,
                 property_value/3, property_value/4, prob_property_value/4, property_value/5,
                 unsat/1, unsat/2, prob_unsat/2, unsat/3,
                 inconsistent_theory/0, inconsistent_theory/1, prob_inconsistent_theory/1, inconsistent_theory/2,
                 axiom/1, add_kb_prefix/2, add_kb_prefixes/1, add_axiom/1, add_axioms/1, remove_kb_prefix/2, remove_kb_prefix/1, remove_axiom/1, remove_axioms/1,
                 load_kb/1, load_owl_kb/1, load_owl_kb_from_string/1, init_trill/1] ).

:- meta_predicate sub_class(:,+).
:- meta_predicate sub_class(:,+,-).
:- meta_predicate sub_class(:,+,-,+).
:- meta_predicate prob_sub_class(:,+,-).
:- meta_predicate instanceOf(:,+).
:- meta_predicate instanceOf(:,+,-).
:- meta_predicate instanceOf(:,+,-,+).
:- meta_predicate prob_instanceOf(:,+,-).
:- meta_predicate property_value(:,+,+).
:- meta_predicate property_value(:,+,+,-).
:- meta_predicate property_value(:,+,+,-,+).
:- meta_predicate prob_property_value(:,+,+,-).
:- meta_predicate unsat(:).
:- meta_predicate unsat(:,-).
:- meta_predicate unsat(:,-,+).
:- meta_predicate prob_unsat(:,-).
:- meta_predicate inconsistent_theory(:).
:- meta_predicate inconsistent_theory(:,+).
:- meta_predicate prob_inconsistent_theory(:).
:- meta_predicate axiom(:).
:- meta_predicate add_kb_prefix(:,+).
:- meta_predicate add_kb_prefixes(:).
:- meta_predicate add_axiom(:).
:- meta_predicate add_axioms(:).
:- meta_predicate remove_kb_prefix(:,+).
:- meta_predicate remove_kb_prefix(:).
:- meta_predicate remove_axiom(:).
:- meta_predicate remove_axioms(:).
:- meta_predicate load_kb(+).
:- meta_predicate load_owl_kb(+).
:- meta_predicate load_owl_kb_from_string(+).
:- meta_predicate set_algorithm(:).
:- meta_predicate init_trill(+).

:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(library(rbtrees)).
:- use_module(library(dif)).
:- use_module(library(pengines)).
:- use_module(library(sandbox)).

:- reexport(library(bddem)).

:- style_check(-discontiguous).


/********************************
  SETTINGS
*********************************/
:- multifile setting_trill/2.

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

/**
 * load_owl_kb_from_string(++KB:string) is det
 *
 * The predicate loads the knowledge base contained in the given string. 
 * The knowledge base must be defined in pure OWL/RDF format.
 */
load_owl_kb_from_string(String):-
  load_owl_from_string(String).

/*****************************/

/*****************************
  UTILITY PREDICATES
******************************/
%defined in utility_translation
:- multifile add_kb_prefix/2, add_kb_prefixes/1, add_axiom/1, add_axioms/1,
             remove_kb_prefix/2, remove_kb_prefix/1, remove_axiom/1, remove_axioms/1.

/**
 * add_kb_prefix(:ShortPref:string,++LongPref:string) is det
 *
 * This predicate registers the alias ShortPref for the prefix defined in LongPref.
 * The empty string '' can be defined as alias.
 */

/**
 * add_kb_prefixes(:Prefixes:list) is det
 *
 * This predicate registers all the alias prefixes contained in Prefixes.
 * The input list must contain pairs alias=prefix, i.e., [('foo'='http://example.foo#')].
 * The empty string '' can be defined as alias.
 */

/**
 * add_axiom(:Axiom:axiom) is det
 *
 * This predicate adds the given axiom to the knowledge base.
 * The axiom must be defined following the TRILL syntax.
 */

/**
 * add_axioms(:Axioms:list) is det
 *
 * This predicate adds the axioms of the list to the knowledge base.
 * The axioms must be defined following the TRILL syntax.
 */

/**
 * remove_kb_prefix(:ShortPref:string,++LongPref:string) is det
 *
 * This predicate removes from the registered aliases the one given in input.
 */

/**
 * remove_kb_prefix(:Name:string) is det
 *
 * This predicate takes as input a string that can be an alias or a prefix and 
 * removes the pair containing the string from the registered aliases.
 */

/**
 * remove_axiom(:Axiom:axiom) is det
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
 * axiom(:Axiom:axiom) is det
 *
 * This predicate searches in the loaded knowledge base axioms that unify with Axiom.
 */
:- multifile axiom/1.
/*axiom(M:Axiom):-
  M:ns4query(NSList),
  expand_all_ns(M,[Axiom],NSList,[AxiomEx]),
  M:axiom(AxiomEx).*/

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
 * instanceOf(:Class:concept_description,++Ind:individual_name,-Expl:list,-Expl:list,++Assert_ABox:boolean)
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns one explanation for the instantiation of the individual to the given class.
 * The returning explanation is a set of axioms.
 * The predicate fails if the individual does not belong to the class.
 * If Assert_ABox is set to true the list of aboxes is asserted with the predicate final_abox/1.
 */
instanceOf(M:Class,Ind,Expl,Assert_ABox):-
  ( check_query_args(M,[Class,Ind],[ClassEx,IndEx]) ->
  	set_up(M),
	retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
	retractall(M:trillan_idx(_)),
  	assert(M:trillan_idx(1)),
  	build_abox(M,(ABox,Tabs)),
  	(  \+ clash(M,(ABox,Tabs),_) ->
  	    (
  	    	add_q(M,ABox,classAssertion(complementOf(ClassEx),IndEx),ABox0),
      findall((ABox1,Tabs1),apply_all_rules(M,(ABox0,Tabs),(ABox1,Tabs1)),L),
      (Assert_ABox==true -> (writeln('Asserting ABox...'), M:assert(final_abox(L)), writeln('Done. Asserted in final_abox/1...')) ; true),
  		find_expls(M,L,[ClassEx,IndEx],Expl1),
  		check_and_close(M,Expl1,Expl)
  	    )
  	 ;
  	    print_message(warning,inconsistent),!,false
  	)
    ;
    	print_message(warning,iri_not_exists),!,false
  ).

/**
 * instanceOf(:Class:concept_description,++Ind:individual_name)
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns one explanation for the instantiation of the individual to the given class.
 * The returning explanation is a set of axioms.
 * The predicate fails if the individual does not belong to the class.
 */
instanceOf(M:Class,Ind,Expl):-
  instanceOf(M:Class,Ind,Expl,false).

/**
 * instanceOf(:Class:concept_description,++Ind:individual_name) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns true if the individual belongs to the class, false otherwise.
 */
instanceOf(M:Class,Ind):-
  (  check_query_args(M,[Class,Ind],[ClassEx,IndEx]) ->
	(
	  set_up(M),
	  retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
	  retractall(M:trillan_idx(_)),
	  assert(M:trillan_idx(1)),
	  build_abox(M,(ABox,Tabs)),
	  (  \+ clash(M,(ABox,Tabs),_) ->
	      (
	        add_q(M,ABox,classAssertion(complementOf(ClassEx),IndEx),ABox0),
	        apply_all_rules(M,(ABox0,Tabs),(ABox1,Tabs1)),!,
	  	clash(M,(ABox1,Tabs1),_),!
	      )
	    ;
	      print_message(warning,inconsistent),!,false
	  )
	)
    ;
        print_message(warning,iri_not_exists),!,false
  ).

/**
 * property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,-Expl:list,++Assert_ABox:boolean)
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns one explanation for the fact Ind1 is related with Ind2 via Prop.
 * The returning explanation is a set of axioms.
 * The predicate fails if the two individual are not Prop-related.
 * If Assert_ABox is set to true the list of aboxes is asserted with the predicate final_abox/1.
 */
property_value(M:Prop, Ind1, Ind2,Expl,Assert_ABox):-
  ( check_query_args(M,[Prop,Ind1,Ind2],[PropEx,Ind1Ex,Ind2Ex]) ->
	set_up(M),
	retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
	retractall(M:trillan_idx(_)),
  	assert(M:trillan_idx(1)),
  	build_abox(M,(ABox,Tabs)),
  	(  \+ clash(M,(ABox,Tabs),_) ->
  	    (
  	    	findall((ABox1,Tabs1),apply_all_rules(M,(ABox,Tabs),(ABox1,Tabs1)),L),
          (Assert_ABox==true -> (writeln('Asserting ABox...'), M:assert(final_abox(L)), writeln('Done. Asserted in final_abox/1...')) ; true),
  		find_expls(M,L,[PropEx,Ind1Ex,Ind2Ex],Expl1),
  		check_and_close(M,Expl1,Expl)
  	    )
  	 ;
  	    print_message(warning,inconsistent),!,false
  	)
    ;
    	print_message(warning,iri_not_exists),!,false
  ).

/**
 * property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns one explanation for the fact Ind1 is related with Ind2 via Prop.
 * The returning explanation is a set of axioms.
 * The predicate fails if the two individual are not Prop-related.
 */
property_value(M:Prop, Ind1, Ind2,Expl):-
  property_value(M:Prop, Ind1, Ind2,Expl,false).

/**
 * property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name) is det
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns true if the two individual are Prop-related, false otherwise.
 */
property_value(M:Prop, Ind1, Ind2):-
  (  check_query_args(M,[Prop,Ind1,Ind2],[PropEx,Ind1Ex,Ind2Ex]) ->
	(
	  set_up(M),
	  retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
	  retractall(M:trillan_idx(_)),
	  assert(M:trillan_idx(1)),
	  build_abox(M,(ABox,Tabs)),
	  (  \+ clash(M,(ABox,Tabs),_) ->
	      (
	        apply_all_rules(M,(ABox,Tabs),(ABox1,_Tabs1)),!,
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
 * sub_class(:Class:concept_description,++SupClass:concept_description,-Expl:list,-Expl:list,++Assert_ABox:boolean)
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * one explanation for the subclass relation between Class and SupClass.
 * The returning explanation is a set of axioms.
 * The predicate fails if there is not a subclass relation between the two classes.
 * If Assert_ABox is set to true the list of aboxes is asserted with the predicate final_abox/1.
 */
sub_class(M:Class,SupClass,Expl,Assert_ABox):-
  ( check_query_args(M,[Class,SupClass],[ClassEx,SupClassEx]) ->
	unsat_internal(M:intersectionOf([ClassEx,complementOf(SupClassEx)]),Expl,Assert_ABox)
    ;
    	print_message(warning,iri_not_exists),!,false
  ).
  
/**
 * sub_class(:Class:concept_description,++SupClass:concept_description,-Expl:list)
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * one explanation for the subclass relation between Class and SupClass.
 * The returning explanation is a set of axioms.
 * The predicate fails if there is not a subclass relation between the two classes.
 */
sub_class(M:Class,SupClass,Expl):-
  sub_class(M:Class,SupClass,Expl,false).

/**
 * sub_class(:Class:concept_description,++SupClass:concept_description) is det
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * true if Class is a subclass of SupClass, and false otherwise.
 */
sub_class(M:Class,SupClass):-
  ( check_query_args(M,[Class,SupClass],[ClassEx,SupClassEx]) ->
        unsat_internal(M:intersectionOf([ClassEx,complementOf(SupClassEx)])),!
    ;
        print_message(warning,iri_not_exists),!,false
  ).

/**
 * unsat(:Concept:concept_description,-Expl:list,++Assert_ABox:boolean)
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns one explanation for the unsatisfiability of the concept.
 * The returning explanation is a set of axioms.
 * The predicate fails if the concept is satisfiable.
 * If Assert_ABox is set to true the list of aboxes is asserted with the predicate final_abox/1.
 */
unsat(M:Concept,Expl,Assert_ABox):-
  (check_query_args(M,[Concept],[ConceptEx]) ->
  	unsat_internal(M:ConceptEx,Expl,Assert_ABox)
    ;
    	print_message(warning,iri_not_exists),!,false
   ).

/**
 * unsat(:Concept:concept_description,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns one explanation for the unsatisfiability of the concept.
 * The returning explanation is a set of axioms.
 * The predicate fails if the concept is satisfiable.
 */
unsat(M:Concept,Expl):-
  unsat(M:Concept,Expl,false).

% ----------- %
unsat_internal(M:Concept,Expl,Assert_ABox):-
  set_up(M),
  retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
  retractall(M:trillan_idx(_)),
  assert(M:trillan_idx(2)),
  build_abox(M,(ABox,Tabs)),
  ( \+ clash(M,(ABox,Tabs),_) ->
     (
     	add_q(M,ABox,classAssertion(Concept,trillan(1)),ABox0),
	%findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
	findall((ABox1,Tabs1),apply_all_rules(M,(ABox0,Tabs),(ABox1,Tabs1)),L),
  (Assert_ABox==true -> (writeln('Asserting ABox...'), M:assert(final_abox(L)), writeln('Done. Asserted in final_abox/1...')) ; true),
	find_expls(M,L,['unsat',Concept],Expl1),
	check_and_close(M,Expl1,Expl)
     )
    ;
     print_message(warning,inconsistent),!,false
  ).

unsat_internal(M:Concept,Expl):-
  unsat_internal(M:Concept,Expl,false).
% ----------- %

/**
 * unsat(:Concept:concept_description) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns true if the concept is unsatisfiable, false otherwise.
 */
unsat(M:Concept):-
  (check_query_args(M,[Concept],[ConceptEx]) ->
  	unsat_internal(M:ConceptEx)
    ;
    	print_message(warning,iri_not_exists),!,false
   ).

% ----------- %
unsat_internal(M:Concept):-
  set_up(M),
  retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
  retractall(M:trillan_idx(_)),
  assert(M:trillan_idx(2)),
  build_abox(M,(ABox,Tabs)),
  ( \+ clash(M,(ABox,Tabs),_) ->
     (
     	add_q(M,ABox,classAssertion(Concept,trillan(1)),ABox0),
  	%findall((ABox1,Tabs1),apply_rules_0((ABox0,Tabs),(ABox1,Tabs1)),L),
  	apply_all_rules(M,(ABox0,Tabs),(ABox1,Tabs1)),!,
  	clash(M,(ABox1,Tabs1),_),!
     )
    ;
     print_message(warning,inconsistent),!,false
  ).
% ----------- %

/**
 * inconsistent_theory(:Expl:list,++Assert_ABox:boolean)
 *
 * This predicate returns one explanation for the inconsistency of the loaded knowledge base.
 * If Assert_ABox is set to true the list of aboxes is asserted with the predicate final_abox/1.
 */
inconsistent_theory(M:Expl,Assert_ABox):-
  set_up(M),
  retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
  retractall(M:trillan_idx(_)),
  assert(M:trillan_idx(1)),
  build_abox(M,(ABox,Tabs)),
  % Without prior search of clashes in order to find all the possible clashes after expansion
  findall((ABox1,Tabs1),apply_all_rules(M,(ABox,Tabs),(ABox1,Tabs1)),L),
  (Assert_ABox==true -> (writeln('Asserting ABox...'), M:assert(final_abox(L)), writeln('Done. Asserted in final_abox/1...')) ; true),
  find_expls(M,L,['inconsistent','kb'],Expl1),
  check_and_close(M,Expl1,Expl).

/**
 * inconsistent_theory(:Expl:list)
 *
 * This predicate returns one explanation for the inconsistency of the loaded knowledge base.
 */
inconsistent_theory(M:Expl):-
  inconsistent_theory(M:Expl,false).

/**
 * inconsistent_theory
 *
 * This predicate returns true if the loaded knowledge base is inconsistent, otherwise it fails.
 */
inconsistent_theory:-
  get_trill_current_module(M),
  set_up(M),
  retractall(M:exp_found(_,_)),retractall(M:exp_found(_,_,_)),
  retractall(M:trillan_idx(_)),
  assert(M:trillan_idx(1)),
  build_abox(M,(ABox,Tabs)),
  ( (clash(M,(ABox,Tabs),_),!) -> true
    ;
      (apply_all_rules(M,(ABox,Tabs),(ABox1,Tabs1)),!,
       clash(M,(ABox1,Tabs1),_),!)
  ).

/**
 * prob_instanceOf(:Class:concept_description,++Ind:individual_name,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns the probability of the instantiation of the individual to the given class.
 */
prob_instanceOf(M:Class,Ind,Prob):-
  ( check_query_args(M,[Class,Ind],[ClassEx,IndEx]) ->
  	all_instanceOf(M:ClassEx,IndEx,Exps),
  	compute_prob_and_close(M,Exps,Prob)
  ;
  	print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns the probability of the fact Ind1 is related with Ind2 via Prop.
 */
prob_property_value(M:Prop, Ind1, Ind2,Prob):-
  ( check_query_args(M,[Prop,Ind1,Ind2],[PropEx,Ind1Ex,Ind2Ex]) ->
  	all_property_value(M:PropEx,Ind1Ex,Ind2Ex,Exps),
  	compute_prob_and_close(M,Exps,Prob)
  ;
  	print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_sub_class(:Class:concept_description,++SupClass:class_name,--Prob:double) is det
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns 
 * the probability of the subclass relation between Class and SupClass.
 */
prob_sub_class(M:Class,SupClass,Prob):-
  ( check_query_args(M,[Class,SupClass],[ClassEx,SupClassEx]) ->
  	all_sub_class(M:ClassEx,SupClassEx,Exps),
  	compute_prob_and_close(M,Exps,Prob)
  ;
  	print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_unsat(:Concept:concept_description,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns the probability of the unsatisfiability
 * of the concept.
 */
prob_unsat(M:Concept,Prob):-
  ( check_query_args(M,[Concept],[ConceptEx]) ->
    all_unsat(M:ConceptEx,Exps),
    compute_prob_and_close(M,Exps,Prob)
  ;
    print_message(warning,iri_not_exists),!,false
  ).

/**
 * prob_inconsistent_theory(:Prob:double) is det
 *
 * If the knowledge base is inconsistent, this predicate returns the probability of the inconsistency.
 */
prob_inconsistent_theory(M:Prob):-
  all_inconsistent_theory(M:Exps),
  compute_prob_and_close(M,Exps,Prob).

/***********
  Utilities for queries
 ***********/

% adds the query into the ABox
add_q(M,ABox,Query,ABox0):-
  empty_expl(M,Expl),
  add(ABox,(Query,Expl),ABox0).

% expands query arguments using prefixes and checks their existence in the kb
check_query_args(M,L,LEx) :-
  M:ns4query(NSList),
  expand_all_ns(M,L,NSList,false,LEx), %from utility_translation module
  check_query_args_presence(M,LEx).

check_query_args_presence(_M,[]):-!.

check_query_args_presence(M,[H|T]) :-
  nonvar(H),
  atomic(H),!,
  find_atom_in_axioms(M,H),%!,
  check_query_args_presence(M,T).

check_query_args_presence(M,[H|T]) :-
  nonvar(H),
  \+ atomic(H),!,
  H =.. [_|L],
  flatten(L,L1),
  check_query_args_presence(M,L1),
  check_query_args_presence(M,T).

/*
check_query_args_presence(M,[_|T]):-
  check_query_args_presence(M,T).
*/

% looks for presence of atoms in kb's axioms
find_atom_in_axioms(M,H):-
  M:kb_atom(L1),
  member(H,L1),!.

/****************************/

/**************
  FIND FUNCTIONS
***************/

findClassAssertion(C,Ind,Expl1,ABox):-
  (
    is_list(Ind) ->
    (
      find((classAssertion(C,sameIndividual(Ind)),Expl1),ABox)
    ) ;
    (
      find((classAssertion(C,Ind),Expl1),ABox)
    )
  ).

findPropertyAssertion(R,Ind1,Ind2,Expl1,ABox):-
	(
    is_list(Ind1) ->
    (
      Ind1S=sameIndividual(Ind1)
    ) ;
    (
      Ind1S=Ind1
    )
  ),
  (
    is_list(Ind2) ->
    (
      Ind2S=sameIndividual(Ind2)
    ) ;
    (
      Ind2S=Ind2
    )
  ),
  find((propertyAssertion(R,Ind1S,Ind2S),Expl1),ABox).


/****************************
  TABLEAU ALGORITHM
****************************/

/*
find_clash(M,(ABox0,Tabs0),Expl2):-
  apply_rules((ABox0,Tabs0),(ABox,Tabs)),
  clash(M,(ABox,Tabs),Expl).
*/

%-------------
% clash managing
% previous version, manages only one clash at time
% need some tricks in some rules for managing the cases of more than one clash
% TO IMPROVE!
%------------
clash(M,(ABox,_),Expl):-
  %write('clash 1'),nl,
  findClassAssertion(C,Ind,Expl1,ABox),
  findClassAssertion(complementOf(C),Ind,Expl2,ABox),
  and_f(M,Expl1,Expl2,Expl).

clash(M,(ABox,_),Expl):-
  %write('clash 2'),nl,
  find((sameIndividual(LS),Expl1),ABox),
  find((differentIndividuals(LD),Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  dif(X,Y),
  and_f(M,Expl1,Expl2,Expl).

clash(M,(ABox,_),Expl):-
  %write('clash 3'),nl,
  findClassAssertion(C,sameIndividual(L1),Expl1,ABox),
  findClassAssertion(complementOf(C),sameIndividual(L2),Expl2,ABox),
  member(X,L1),
  member(X,L2),!,
  and_f(M,Expl1,Expl2,Expl).

clash(M,(ABox,_),Expl):-
  %write('clash 4'),nl,
  findClassAssertion(C,Ind1,Expl1,ABox),
  findClassAssertion(complementOf(C),sameIndividual(L2),Expl2,ABox),
  member(Ind1,L2),
  and_f(M,Expl1,Expl2,Expl).

clash(M,(ABox,_),Expl):-
  %write('clash 5'),nl,
  findClassAssertion(C,sameIndividual(L1),Expl1,ABox),
  findClassAssertion(complementOf(C),Ind2,Expl2,ABox),
  member(Ind2,L1),
  and_f(M,Expl1,Expl2,Expl).

clash(M,(ABox,_),Expl):-
  %write('clash 6'),nl,
  findClassAssertion4OWLNothing(M,ABox,Expl).

clash(M,(ABox,_),Expl):-
  %write('clash 7'),nl,
  M:disjointClasses(L), % TODO use hierarchy
  member(C1,L),
  member(C2,L),
  dif(C1,C2),
  findClassAssertion(C1,Ind,Expl1,ABox),
  findClassAssertion(C2,Ind,Expl2,ABox),
  and_f(M,Expl1,Expl2,ExplT),
  and_f_ax(M,disjointClasses(L),ExplT,Expl).

clash(M,(ABox,_),Expl):-
  %write('clash 8'),nl,
  M:disjointUnion(Class,L), % TODO use hierarchy
  member(C1,L),
  member(C2,L),
  dif(C1,C2),
  findClassAssertion(C1,Ind,Expl1,ABox),
  findClassAssertion(C2,Ind,Expl2,ABox),
  and_f(M,Expl1,Expl2,ExplT),
  and_f_ax(M,disjointUnion(Class,L),ExplT,Expl).

/*
clash(M,(ABox,Tabs),Expl):-
  %write('clash 9'),nl,
  findClassAssertion(maxCardinality(N,S,C),Ind,Expl1,ABox),
  s_neighbours(M,Ind,S,(ABox,Tabs),SN),
  individual_class_C(SN,C,ABox,SNC),
  length(SNC,LSS),
  LSS @> N,
  make_expl(M,Ind,S,SNC,Expl1,ABox,Expl).

clash(M,(ABox,Tabs),Expl):-
  %write('clash 10'),nl,
  findClassAssertion(maxCardinality(N,S),Ind,Expl1,ABox),
  s_neighbours(M,Ind,S,(ABox,Tabs),SN),
  length(SN,LSS),
  LSS @> N,
  make_expl(Ind,S,SN,Expl1,ABox,Expl).


% --------------

make_expl(_,_,_,[],Expl,_,Expl).

make_expl(M,Ind,S,[H|T],Expl0,ABox,Expl):-
  findPropertyAssertion(S,Ind,H,Expl2,ABox),
  and_f(M,Expl2,Expl0,Expl1),
  make_expl(M,Ind,S,T,Expl1,ABox,Expl).
*/

% -------------
% rules application
% -------------
apply_all_rules(M,ABox0,ABox):-
  setting_trill(det_rules,Rules),
  apply_det_rules(M,Rules,ABox0,ABox1),
  (ABox0=ABox1 ->
  ABox=ABox1;
  apply_all_rules(M,ABox1,ABox)).

apply_det_rules(M,[],ABox0,ABox):-
  setting_trill(nondet_rules,Rules),
  apply_nondet_rules(M,Rules,ABox0,ABox).

apply_det_rules(M,[H|_],ABox0,ABox):-
  %C=..[H,ABox,ABox1],
  call(H,M,ABox0,ABox),!.

apply_det_rules(M,[_|T],ABox0,ABox):-
  apply_det_rules(M,T,ABox0,ABox).


apply_nondet_rules(_,[],ABox,ABox).

apply_nondet_rules(M,[H|_],ABox0,ABox):-
  %C=..[H,ABox,L],
  call(H,M,ABox0,L),!,
  member(ABox,L),
  dif(ABox0,ABox).

apply_nondet_rules(M,[_|T],ABox0,ABox):-
  apply_nondet_rules(M,T,ABox0,ABox).


/*
apply_all_rules(M,ABox0,ABox):-
  apply_nondet_rules([or_rule,max_rule],
                  ABox0,ABox1),
  (ABox0=ABox1 ->
  ABox=ABox1;
  apply_all_rules(M,ABox1,ABox)).

apply_det_rules([],ABox,ABox).
apply_det_rules([H|_],ABox0,ABox):-
  %C=..[H,ABox,ABox1],
  once(call(H,ABox0,ABox)).
apply_det_rules([_|T],ABox0,ABox):-
  apply_det_rules(T,ABox0,ABox).
apply_nondet_rules([],ABox0,ABox):-
  apply_det_rules([o_rule,and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule,min_rule],ABox0,ABox).
apply_nondet_rules([H|_],ABox0,ABox):-
  %C=..[H,ABox,L],
  once(call(H,ABox0,L)),
  member(ABox,L),
  dif(ABox0,ABox).
apply_nondet_rules([_|T],ABox0,ABox):-
  apply_nondet_rules(T,ABox0,ABox).
*/

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
add_exists_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findPropertyAssertion(R,Ind1,Ind2,Expl1,ABox0),
  findClassAssertion(C,Ind2,Expl2,ABox0),
  existsInKB(M,R,C),
  and_f(M,Expl1,Expl2,Expl),
  modify_ABox(M,ABox0,someValuesFrom(R,C),Ind1,Expl,ABox).

existsInKB(M,R,C):-
  M:subClassOf(A,B),
  member(someValuesFrom(R,C),[A,B]).

existsInKB(M,R,C):-
  M:equivalentClasses(L),
  member(someValuesFrom(R,C),L).

/* *************** */ 

/*
  and_rule
  =================
*/
and_rule(M,(ABox0,Tabs0),(ABox,Tabs0)):-
  findClassAssertion(intersectionOf(LC),Ind,Expl,ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  scan_and_list(M,LC,Ind,Expl,ABox0,Tabs0,ABox,0).


%----------------
scan_and_list(_M,[],_Ind,_Expl,ABox,_Tabs,ABox,Mod):-
  dif(Mod,0).

scan_and_list(M,[C|T],Ind,Expl,ABox0,Tabs0,ABox,_Mod):-
  modify_ABox(M,ABox0,C,Ind,Expl,ABox1),!,
  scan_and_list(M,T,Ind,Expl,ABox1,Tabs0,ABox,1).

scan_and_list(M,[_C|T],Ind,Expl,ABox0,Tabs0,ABox,Mod):-
  scan_and_list(M,T,Ind,Expl,ABox0,Tabs0,ABox,Mod).
/* ************* */

/*
  or_rule
  ===============
*/
or_rule(M,(ABox0,Tabs0),L):- 
  findClassAssertion(unionOf(LC),Ind,Expl,ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  %not_ind_intersected_union(Ind,LC,ABox0),
  % length(LC,NClasses),
  get_choice_point_id(M,ID),
  scan_or_list(M,LC,0,ID,Ind,Expl,ABox0,Tabs0,L),
  dif(L,[]),
  create_choice_point(M,Ind,or,unionOf(LC),LC,_),!. % last variable whould be equals to ID

not_ind_intersected_union(Ind,LC,ABox):-
  \+ ind_intersected_union(Ind,LC,ABox).

ind_intersected_union(Ind,LC,ABox) :-
  findClassAssertion(C,Ind,_,ABox),
  member(C,LC),!.
%---------------
scan_or_list(_,[],_,_,_,_,_,_,[]):- !.

scan_or_list(M,[C|T],N0,CP,Ind,Expl0,ABox0,Tabs,[(ABox,Tabs)|L]):-
  add_choice_point(M,cpp(CP,N0),Expl0,Expl),
  modify_ABox(M,ABox0,C,Ind,Expl,ABox),
  N is N0 + 1,
  scan_or_list(M,T,N,CP,Ind,Expl0,ABox0,Tabs,L).

/* **************** */

/*
  exists_rule
  ==================
*/
exists_rule(M,(ABox0,Tabs0),([(propertyAssertion(R,Ind1,Ind2),Expl),
    (classAssertion(C,Ind2),Expl)|ABox0],Tabs)):-
  findClassAssertion(someValuesFrom(R,C),Ind1,Expl,ABox0),
  \+ blocked(Ind1,(ABox0,Tabs0)),
  \+ connected_individual(R,C,Ind1,ABox0),
  new_ind(M,Ind2),
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
forall_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(allValuesFrom(R,C),Ind1,Expl1,ABox0),
  \+ indirectly_blocked(Ind1,(ABox0,Tabs)),
  findPropertyAssertion(R,Ind1,Ind2,Expl2,ABox0),
  and_f(M,Expl1,Expl2,Expl),
  modify_ABox(M,ABox0,C,Ind2,Expl,ABox).

/* ************** */

/*
  forall_plus_rule
  =================
*/
forall_plus_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(allValuesFrom(S,C),Ind1,Expl1,ABox0),
  \+ indirectly_blocked(Ind1,(ABox0,Tabs)),
  findPropertyAssertion(R,Ind1,Ind2,Expl2,ABox0),
  find_sub_sup_trans_role(M,R,S,Expl3),
  and_f(M,Expl1,Expl2,ExplT),
  and_f(M,ExplT,Expl3,Expl),
  modify_ABox(M,ABox0,allValuesFrom(R,C),Ind2,Expl,ABox).

% --------------
find_sub_sup_trans_role(M,R,S,Expl):-
  M:subPropertyOf(R,S),
  M:transitiveProperty(R),
  initial_expl(M,EExpl),
  and_f_ax(M,subPropertyOf(R,S),EExpl,Expl0),
  and_f_ax(M,transitive(R),Expl0,Expl).

find_sub_sup_trans_role(M,R,S,Expl):-
  M:subPropertyOf(R,S),
  \+ M:transitiveProperty(R),
  initial_expl(M,EExpl),
  and_f_ax(M,subPropertyOf(R,S),EExpl,Expl).
/* ************ */

/*
  unfold_rule
  ===========
*/

unfold_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(C,Ind,Expl,ABox0),
  find_sub_sup_class(M,C,D,Ax),
  and_f_ax(M,Ax,Expl,AxL),
  modify_ABox(M,ABox0,D,Ind,AxL,ABox1),
  add_nominal(M,D,Ind,ABox1,ABox).

/* -- unfold_rule
      takes a class C1 in which Ind belongs, find a not atomic class C
      that contains C1 (C is seen as list of classes), controls if
      the individual Ind belongs to all those classes, if yes it finds a class D (if exists)
      that is the superclass or an equivalent class of C and adds D to label of Ind
      - for managing tableau with more than one clash -
      correspond to the ce_rule
   --
*/
unfold_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(C1,Ind,Expl,ABox0),
  find_not_atomic(M,C1,C,L),
  ( C = unionOf(_) -> Expl1 = Expl ; find_all(M,Ind,L,ABox0,Expl1)),
  find_sub_sup_class(M,C,D,Ax),
  and_f_ax(M,Ax,Expl1,AxL1),
  modify_ABox(M,ABox0,D,Ind,AxL1,ABox1),
  add_nominal(M,D,Ind,ABox1,ABox).

/* -- unfold_rule
 *    control propertyRange e propertyDomain
 * --
 */
unfold_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  find_class_prop_range_domain(M,Ind,D,Expl,(ABox0,Tabs)),
  modify_ABox(M,ABox0,D,Ind,Expl,ABox1),
  add_nominal(M,D,Ind,ABox1,ABox).

/*
 * -- unfold_rule
 *    manage the negation
 * --
 */
unfold_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findClassAssertion(complementOf(C),Ind,Expl,ABox0),
  find_neg_class(C,D),
  and_f_ax(M,complementOf(C),Expl,AxL),
  modify_ABox(M,ABox0,D,Ind,AxL,ABox1),
  add_nominal(M,D,Ind,ABox1,ABox).

% ----------------
% add_nominal

add_nominal(M,D,Ind,ABox0,ABox):-
  ((D = oneOf(_),
    \+ member(nominal(Ind),ABox0))
    ->
   (
     ABox1 = [nominal(Ind)|ABox0],
     (member((classAssertion('http://www.w3.org/2002/07/owl#Thing',Ind),_E),ABox1)
     ->
     ABox = ABox1
     ;
     (empty_expl(M,Expl),ABox = [(classAssertion('http://www.w3.org/2002/07/owl#Thing',Ind),Expl)|ABox1])
     )
   )
    ;
   ABox = ABox0
  ).

% ----------------
% unionOf, intersectionOf, subClassOf, negation, allValuesFrom, someValuesFrom, exactCardinality(min and max), maxCardinality, minCardinality
:- multifile find_neg_class/2.

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

find_class_prop_range_domain(M,Ind,D,Expl,(ABox,_Tabs)):-
  findPropertyAssertion(R,_,IndL,ExplPA,ABox),
  indAsList(IndL,L),
  member(Ind,L),
  M:propertyRange(R,D),
  and_f_ax(M,propertyRange(R,D),ExplPA,Expl).

find_class_prop_range_domain(M,Ind,D,Expl,(ABox,_Tabs)):-
  findPropertyAssertion(R,IndL,_,ExplPA,ABox),
  indAsList(IndL,L),
  member(Ind,L),
  M:propertyDomain(R,D),
  and_f_ax(M,propertyDomain(R,D),ExplPA,Expl).


%-----------------
:- multifile find_sub_sup_class/4.

% subClassOf
find_sub_sup_class(M,C,D,subClassOf(C,D)):-
  M:subClassOf(C,D).

%equivalentClasses
find_sub_sup_class(M,C,D,equivalentClasses(L)):-
  M:equivalentClasses(L),
  member(C,L),
  member(D,L),
  dif(C,D).

%concept for concepts allValuesFrom
find_sub_sup_class(M,allValuesFrom(R,C),allValuesFrom(R,D),Ax):-
  find_sub_sup_class(M,C,D,Ax).

%role for concepts allValuesFrom
find_sub_sup_class(M,allValuesFrom(R,C),allValuesFrom(S,C),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).

%concept for concepts someValuesFrom
find_sub_sup_class(M,someValuesFrom(R,C),someValuesFrom(R,D),Ax):-
  find_sub_sup_class(M,C,D,Ax).

%role for concepts someValuesFrom
find_sub_sup_class(M,someValuesFrom(R,C),someValuesFrom(S,C),subPropertyOf(R,S)):-
  M:subPropertyOf(R,S).


/*******************
 managing the concept (C subclassOf Thing)
 this implementation doesn't work well in a little set of cases
 TO IMPROVE!
 *******************/
/*
find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:subClassOf(A,B),
  member(C,[A,B]),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:classAssertion(C,_),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:equivalentClasses(L),
  member(C,L),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:unionOf(L),
  member(C,L),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:equivalentClasses(L),
  member(someValuesFrom(_,C),L),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:equivalentClasses(L),
  member(allValuesFrom(_,C),L),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:equivalentClasses(L),
  member(minCardinality(_,_,C),L),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:equivalentClasses(L),
  member(maxCardinality(_,_,C),L),!.

find_sub_sup_class(M,C,'http://www.w3.org/2002/07/owl#Thing',subClassOf(C,'http://www.w3.org/2002/07/owl#Thing')):-
  M:equivalentClasses(L),
  member(exactCardinality(_,_,C),L),!.

*/

%--------------------
% looks for not atomic concepts descriptions containing class C
find_not_atomic(M,C,intersectionOf(L1),L1):-
  M:subClassOf(A,B),
  member(intersectionOf(L1),[A,B]),
  member(C,L1).

find_not_atomic(M,C,unionOf(L1),L1):-
  M:subClassOf(A,B),
  member(unionOf(L1),[A,B]),
  member(C,L1).

/*
find_not_atomic(M,C,intersectionOf(L),L):-
  M:intersectionOf(L),
  member(C,L).

find_not_atomic(M,C,unionOf(L),L):-
  M:unionOf(L),
  member(C,L).
*/

find_not_atomic(M,C,intersectionOf(L1),L1):-
  M:equivalentClasses(L),
  member(intersectionOf(L1),L),
  member(C,L1).

find_not_atomic(M,C,unionOf(L1),L1):-
  M:equivalentClasses(L),
  member(unionOf(L1),L),
  member(C,L1).

% -----------------------
% puts together the explanations of all the concepts found by find_not_atomic/3
find_all(_M,_,[],_,[]).

find_all(M,Ind,[H|T],ABox,ExplT):-
  findClassAssertion(H,Ind,Expl1,ABox),
  find_all(M,Ind,T,ABox,Expl2),
  and_f(M,Expl1,Expl2,ExplT).


% ------------------------
%  unfold_rule to unfold roles
% ------------------------
% sub properties
unfold_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findPropertyAssertion(C,Ind1,Ind2,Expl,ABox0),
  find_sub_sup_property(M,C,D,Ax),
  and_f_ax(M,Ax,Expl,AxL),
  modify_ABox(M,ABox0,D,Ind1,Ind2,AxL,ABox1),
  add_nominal(M,D,Ind1,ABox1,ABox2),
  add_nominal(M,D,Ind2,ABox2,ABox).

%inverseProperties
unfold_rule(M,(ABox0,Tabs),(ABox,Tabs)):-
  findPropertyAssertion(C,Ind1,Ind2,Expl,ABox0),
  find_inverse_property(M,C,D,Ax),
  and_f_ax(M,Ax,Expl,AxL),
  modify_ABox(M,ABox0,D,Ind2,Ind1,AxL,ABox1),
  add_nominal(M,D,Ind1,ABox1,ABox2),
  add_nominal(M,D,Ind2,ABox2,ABox).

%-----------------
% subPropertyOf
find_sub_sup_property(M,C,D,subPropertyOf(C,D)):-
  M:subPropertyOf(C,D).

%equivalentProperties
find_sub_sup_property(M,C,D,equivalentProperties(L)):-
  M:equivalentProperties(L),
  member(C,L),
  member(D,L),
  dif(C,D).

%-----------------
%inverseProperties
find_inverse_property(M,C,D,inverseProperties(C,D)):-
  M:inverseProperties(C,D).

find_inverse_property(M,C,D,inverseProperties(D,C)):-
  M:inverseProperties(D,C).

%inverseProperties
find_inverse_property(M,C,C,symmetricProperty(C)):-
  M:symmetricProperty(C).

/* ************* */

/*
  ce_rule
  =============
*/
ce_rule(M,(ABox0,(T,RBN,RBR)),(ABox,(T,RBN,RBR))):-
  find_not_sub_sup_class(M,Ax,UnAx),
  vertices(T,Inds),
  apply_ce_to(M,Inds,Ax,UnAx,ABox0,ABox,(T,RBN,RBR),C),
  C @> 0.


% ------------------
find_not_sub_sup_class(M,subClassOf(C,D),unionOf(complementOf(C),D)):-
  M:subClassOf(C,D),
  \+ atomic(C).


find_not_sub_sup_class(M,equivalentClasses(L),unionOf(L1)):-
  M:equivalentClasses(L),
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
apply_ce_to(_M,[],_,_,ABox,ABox,_,0).

apply_ce_to(M,[Ind|T],Ax,UnAx,ABox0,ABox,Tabs,C):-
  \+ indirectly_blocked(Ind,(ABox0,Tabs)),
  modify_ABox(M,ABox0,UnAx,Ind,[Ax],ABox1),!,
  apply_ce_to(M,T,Ax,UnAx,ABox1,ABox,Tabs,C0),
  C is C0 + 1.

apply_ce_to(M,[_Ind|T],Ax,UnAx,ABox0,ABox,Tabs,C):-
  apply_ce_to(M,T,Ax,UnAx,ABox0,ABox,Tabs,C).

/* **************** */

/*
  min_rule
  =============
*/
min_rule(M,(ABox,Tabs),([(differentIndividuals(NI),Expl)|ABox1],Tabs1)):-
  findClassAssertion(minCardinality(N,S),Ind1,Expl,ABox),
  \+ blocked(Ind1,(ABox,Tabs)),
  s_neighbours(M,Ind1,S,(ABox,Tabs),SN),
  safe_s_neigh(SN,S,(ABox,Tabs),SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh(M,NoI,S,Ind1,Expl,NI,ABox,Tabs,ABox1,Tabs1).


min_rule(M,(ABox,Tabs),([(differentIndividuals(NI),Expl)|ABox1],Tabs1)):-
  findClassAssertion(minCardinality(N,S,C),Ind1,Expl,ABox),
  \+ blocked(Ind1,(ABox,Tabs)),
  s_neighbours(M,Ind1,S,(ABox,Tabs),SN),
  safe_s_neigh(SN,S,(ABox,Tabs),SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh_C(M,NoI,S,C,Ind1,Expl,NI,ABox,Tabs,ABox1,Tabs1).

% ----------------------
min_rule_neigh(_,0,_,_,_,[],ABox,Tabs,ABox,Tabs).

min_rule_neigh(M,N,S,Ind1,Expl,[Ind2|NI],ABox,Tabs,[(propertyAssertion(S,Ind1,Ind2),Expl)|ABox2],Tabs2):-
  N > 0,
  NoI is N-1,
  new_ind(M,Ind2),
  add_edge(S,Ind1,Ind2,Tabs,Tabs1),
  check_block(Ind2,([(propertyAssertion(S,Ind1,Ind2),Expl)|ABox],Tabs)),
  min_rule_neigh(M,NoI,S,Ind1,Expl,NI,ABox,Tabs1,ABox2,Tabs2).

%----------------------
min_rule_neigh_C(_,0,_,_,_,_,[],ABox,Tabs,ABox,Tabs).

min_rule_neigh_C(M,N,S,C,Ind1,Expl,[Ind2|NI],ABox,Tabs,[(propertyAssertion(S,Ind1,Ind2),Expl),
                                          (classAssertion(C,Ind2),[propertyAssertion(S,Ind1,Ind2)|Expl])|ABox2],Tabs2):-
  N > 0,
  NoI is N-1,
  new_ind(M,Ind2),
  add_edge(S,Ind1,Ind2,Tabs,Tabs1),
  check_block(Ind2,([(propertyAssertion(S,Ind1,Ind2),Expl)|ABox],Tabs)),
  min_rule_neigh_C(M,NoI,S,C,Ind1,Expl,NI,ABox,Tabs1,ABox2,Tabs2).

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
max_rule(M,(ABox0,Tabs0),L):- 
  findClassAssertion(maxCardinality(N,S,C),Ind,Expl0,ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  s_neighbours(M,Ind,S,(ABox0,Tabs0),SN),
  individual_class_C(SN,C,ABox0,SNC),
  length(SNC,LSS),
  LSS @> N,
  get_choice_point_id(M,ID),%gtrace,
  scan_max_list(M,maxCardinality(N,S,C),S,C,SNC,ID,Ind,Expl0,ABox0,Tabs0,L),!. % last variable whould be equals to ID

max_rule(M,(ABox0,Tabs0),L):-
  findClassAssertion(maxCardinality(N,S),Ind,Expl0,ABox0),
  \+ indirectly_blocked(Ind,(ABox0,Tabs0)),
  s_neighbours(M,Ind,S,(ABox0,Tabs0),SN),
  length(SN,LSS),
  LSS @> N,
  get_choice_point_id(M,ID),
  scan_max_list(M,maxCardinality(N,S),S,'http://www.w3.org/2002/07/owl#Thing',SN,ID,Ind,Expl0,ABox0,Tabs0,L),!. 
%---------------------

scan_max_list(M,MaxCardClass,S,C,SN,CP,Ind,Expl,ABox0,Tabs0,Tab_list):-
  create_couples_for_merge(SN,[],Ind_couples), % MAYBE check_individuals_not_equal(M,YI,YJ,ABox0), instead of dif
  length(Ind_couples,NChoices),
  (
    NChoices @> 1 -> (FirstChoice = -1) ; (FirstChoice = 0)
  ),
  create_list_for_max_rule(M,Ind_couples,FirstChoice,CP,Ind,S,C,Expl,ABox0,Tabs0,Tab_list),
  dif(Tab_list,[]),
  create_choice_point(M,Ind,mr,MaxCardClass,Ind_couples,_). % last variable whould be equals to ID

create_couples_for_merge([],Ind_couples,Ind_couples).

create_couples_for_merge([H|T],Ind_couples0,Ind_couples):-
  create_couples_for_merge_int(H,T,Ind_couples0,Ind_couples1),
  create_couples_for_merge(T,Ind_couples1,Ind_couples).

create_couples_for_merge_int(_,[],Ind_couples,Ind_couples).

create_couples_for_merge_int(I,[H|T],Ind_couples0,Ind_couples):-
  create_couples_for_merge_int(I,T,[I-H|Ind_couples0],Ind_couples).

create_list_for_max_rule(_,[],_,_,_,_,_,_,_,_,[]).

create_list_for_max_rule(M,[YI-YJ|Ind_couples],N0,CP,Ind,S,C,Expl0,ABox0,Tabs0,[(ABox,Tabs)|Tab_list]):-
  findPropertyAssertion(S,Ind,YI,ExplYI,ABox0),
  findPropertyAssertion(S,Ind,YJ,ExplYJ,ABox0),
  findClassAssertion(C,YI,ExplCYI,ABox0),
  findClassAssertion(C,YJ,ExplCYJ,ABox0),
  and_f(M,ExplYI,ExplYJ,ExplS0),
  and_f(M,ExplS0,ExplCYI,ExplS1),
  and_f(M,ExplS1,ExplCYJ,ExplC0),
  and_f(M,ExplC0,Expl0,ExplT0),
  (
    dif(N0,-1) ->
    (
      add_choice_point(M,cpp(CP,N0),ExplT0,ExplT),
      N is N0 + 1
    ) ;
    (
      ExplT = ExplT0,
      N = N0
    )
  ),
  flatten([YI,YJ],LI),
  merge_all(M,[(sameIndividual(LI),ExplT)],ABox0,Tabs0,ABox,Tabs),
  create_list_for_max_rule(M,Ind_couples,N,CP,Ind,S,C,Expl0,ABox0,Tabs0,Tab_list).

/*
scan_max_list(M,S,SN,CP,Ind,Expl,ABox0,Tabs0,YI-YJ,ABox,Tabs):-
  member(YI,SN),
  member(YJ,SN),
  % genero cp
  check_individuals_not_equal(M,YI,YJ,ABox0),
  findPropertyAssertion(S,Ind,YI,ExplYI,ABox0),
  findPropertyAssertion(S,Ind,YJ,ExplYJ,ABox0),
  and_f(M,ExplYI,ExplYJ,Expl0),
  and_f(M,Expl0,Expl,ExplT0),
  add_choice_point(M,cpp(CP,N0),ExplT0,ExplT),
  merge_all(M,[(sameIndividual([YI,YJ]),ExplT)],ABox0,Tabs0,ABox,Tabs).
*/
%--------------------

separate_merged_ind_from_tab([],[],[]).

separate_merged_ind_from_tab([I1-I2-Tab|L0],[I1-I2|LCP],[Tab|L]):-
  separate_merged_ind_from_tab(L0,LCP,L).

%--------------------
check_individuals_not_equal(M,X,Y,ABox):-
  dif(X,Y),
  \+ same_ind(M,[X],Y,ABox).
%--------------------
individual_class_C([],_,_,[]).

individual_class_C([H|T],C,ABox,[H|T1]):-
  findClassAssertion(C,H,_,ABox),!,
  individual_class_C(T,C,ABox,T1).

individual_class_C([_H|T],C,ABox,T1):-
  %\+ findClassAssertion(C,H,_,ABox),
  individual_class_C(T,C,ABox,T1).
/* *************** */

/*
 o_rule
 ============
*/

o_rule(M,(ABox0,Tabs0),([(sameIndividual(LI),ExplC)|ABox],Tabs)):-
  findClassAssertion(oneOf([C]),X,ExplX,ABox0),
  findClassAssertion(oneOf([D]),Y,ExplY,ABox0),
  containsCommon(C,D),
  dif(X,Y),
  notDifferentIndividuals(M,X,Y,ABox0),
  nominal(C,(ABox0,Tabs0)),
  indAsList(X,LX),
  indAsList(Y,LY),
  and_f(M,ExplX,ExplY,ExplC),
  merge(M,X,Y,ExplC,(ABox0,Tabs0),(ABox,Tabs)),
  flatten([LX,LY],LI0),
  list_to_set(LI0,LI),
  absent(sameIndividual(LI),ExplC,ABox0).

containsCommon(L1,L2):-
  member(X,L1),
  member(X,L2),!.
% -------------------

/* ************* */

/***********
  update abox
  utility for tableau
************/

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

%---------------
subProp(M,SubProperty,Property,Subject,Object):-
  M:subPropertyOf(SubProperty,Property),M:propertyAssertion(SubProperty,Subject,Object).

%--------------

add_nominal_list(M,ABox0,(T,_,_),ABox):-
  vertices(T,NomListIn),
  prepare_nom_list(M,NomListIn,NomListOut),
  add_all(NomListOut,ABox0,ABox).

%--------------

prepare_nom_list(_,[],[]).

prepare_nom_list(M,[literal(_)|T],T1):-!,
  prepare_nom_list(M,T,T1).

prepare_nom_list(M,[H|T],[(nominal(H)),(classAssertion('http://www.w3.org/2002/07/owl#Thing',H),Expl)|T1]):-
  empty_expl(M,Expl),
  prepare_nom_list(M,T,T1).
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

  add_edge(Property,Subject,Object,Tab0,Tab)
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
merge(M,sameIndividual(L),Y,Expl,(ABox0,Tabs0),(ABox,Tabs)):-
  !,
  merge_tabs(L,Y,Tabs0,Tabs),
  merge_abox(M,L,Y,Expl,ABox0,ABox).

merge(M,X,sameIndividual(L),Expl,(ABox0,Tabs0),(ABox,Tabs)):-
  !,
  merge_tabs(X,L,Tabs0,Tabs),
  merge_abox(M,X,L,Expl,ABox0,ABox).

merge(M,X,Y,Expl,(ABox0,Tabs0),(ABox,Tabs)):-
  !,
  merge_tabs(X,Y,Tabs0,Tabs),
  merge_abox(M,X,Y,Expl,ABox0,ABox).

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

% TODO update
merge_abox(M,X,Y,Expl0,ABox0,ABox):-
  flatten([X,Y],L0),
  list_to_set(L0,L),
  merge_abox(M,L,Expl0,ABox0,ABox).


merge_abox(_M,_L,_,[],[]).

merge_abox(M,L,Expl0,[(classAssertion(C,Ind),ExplT)|T],[(classAssertion(C,sameIndividual(L)),Expl)|ABox]):-
  member(Ind,L),!,
  and_f(M,Expl0,ExplT,Expl),
  %and_f_ax(M,sameIndividual(L),Expl1,Expl),
  merge_abox(M,L,Expl0,T,ABox).

merge_abox(M,L,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,sameIndividual(L),Ind2),Expl)|ABox]):-
  member(Ind1,L),!,
  and_f(M,Expl0,ExplT,Expl),
  %and_f_ax(M,sameIndividual(L),Expl1,Expl),
  merge_abox(M,L,Expl0,T,ABox).

merge_abox(M,L,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,Ind1,sameIndividual(L)),Expl)|ABox]):-
  member(Ind2,L),!,
  and_f(M,Expl0,ExplT,Expl),
  %and_f_ax(M,sameIndividual(L),Expl1,Expl),
  merge_abox(M,L,Expl0,T,ABox).

merge_abox(M,L,Expl0,[H|T],[H|ABox]):-
  merge_abox(M,L,Expl0,T,ABox).

/* merge node in (ABox,Tabs) */

merge_all(_,[],ABox,Tabs,ABox,Tabs).

merge_all(M,[(sameIndividual(H),Expl)|T],ABox0,Tabs0,ABox,Tabs):-
  find_same(H,ABox0,L,ExplL),
  dif(L,[]),!,
  merge_all1(M,H,Expl,L,ABox0,Tabs0,ABox1,Tabs1),
  flatten([H,L],L0),
  list_to_set(L0,L1),
  and_f(M,Expl,ExplL,ExplT),
  add(ABox1,(sameIndividual(L1),ExplT),ABox2),
  delete(ABox2,(sameIndividual(L),ExplL),ABox3),
  retract_sameIndividual(L),
  merge_all(M,T,ABox3,Tabs1,ABox,Tabs).

merge_all(M,[(sameIndividual(H),Expl)|T],ABox0,Tabs0,ABox,Tabs):-
  find_same(H,ABox0,L,_),
  L==[],!,
  merge_all2(M,H,Expl,ABox0,Tabs0,ABox1,Tabs1),
  add(ABox1,(sameIndividual(H),Expl),ABox2),
  merge_all(M,T,ABox2,Tabs1,ABox,Tabs).

merge_all1(_M,[],_,_,ABox,Tabs,ABox,Tabs).

merge_all1(M,[H|T],Expl,L,ABox0,Tabs0,ABox,Tabs):-
  \+ member(H,L),
  merge(M,H,L,Expl,(ABox0,Tabs0),(ABox1,Tabs1)),
  merge_all1(M,T,Expl,[H|L],ABox1,Tabs1,ABox,Tabs).

merge_all1(M,[H|T],Expl,L,ABox0,Tabs0,ABox,Tabs):-
  member(H,L),
  merge_all1(M,T,Expl,L,ABox0,Tabs0,ABox,Tabs).



merge_all2(M,[X,Y|T],Expl,ABox0,Tabs0,ABox,Tabs):-
  merge(M,X,Y,Expl,(ABox0,Tabs0),(ABox1,Tabs1)),
  merge_all1(M,T,Expl,[X,Y],ABox1,Tabs1,ABox,Tabs).

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
new_ind(M,trillan(I)):-
  retract(M:trillan_idx(I)),
  I1 is I+1,
  assert(M:trillan_idx(I1)).

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
  (N<NP ->
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
s_neighbours(M,Ind1,S,(ABox,(_,_,RBR)),SN):-
  rb_lookup(S,VN,RBR),
  s_neighbours1(Ind1,VN,SN1),
  s_neighbours2(M,SN1,SN1,SN,ABox).

s_neighbours(_,_Ind1,S,(_,_,RBR),[]):-
  \+ rb_lookup(S,_VN,RBR).

s_neighbours1(_,[],[]).

s_neighbours1(Ind1,[(Ind1,Y)|T],[Y|T1]):-
  s_neighbours1(Ind1,T,T1).

s_neighbours1(Ind1,[(X,_Y)|T],T1):-
  dif(X,Ind1),
  s_neighbours1(Ind1,T,T1).
  
s_neighbours2(_,_,[],[],_).

s_neighbours2(M,SN,[H|T],[H|T1],ABox):-
  s_neighbours2(M,SN,T,T1,ABox),
  \+ same_ind(M,T1,H,ABox).

s_neighbours2(M,SN,[H|T],T1,ABox):-
  s_neighbours2(M,SN,T,T1,ABox),
  same_ind(M,T1,H,ABox).

%-----------------
same_ind(M,SN,H,_ABox):-
  M:sameIndividual(SI),
  member(H,SI),
  member(H2,SI),
  member(H2,SN),
  dif(H,H2).

same_ind(_,SN,H,ABox):-
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

s_predecessors(M,Ind1,S,(ABox,(_,_,RBR)),SN):-
  rb_lookup(S,VN,RBR),
  s_predecessors1(Ind1,VN,SN1),
  s_predecessors2(M,SN1,SN,ABox).

s_predecessors(_M,_Ind1,S,(_ABox,(_,_,RBR)),[]):-
  \+ rb_lookup(S,_VN,RBR).

s_predecessors1(_,[],[]).

s_predecessors1(Ind1,[(Y,Ind1)|T],[Y|T1]):-
  s_predecessors1(Ind1,T,T1).

s_predecessors1(Ind1,[(_X,Y)|T],T1):-
  dif(Y,Ind1),
  s_predecessors1(Ind1,T,T1).

s_predecessors2(_M,[],[],_).

s_predecessors2(M,[H|T],[H|T1],ABox):-
  s_predecessors2(M,T,T1,ABox),
  \+ same_ind(M,T1,H,ABox).

s_predecessors2(M,[H|T],T1,ABox):-
  s_predecessors2(M,T,T1,ABox),
  same_ind(M,T1,H,ABox).

/* ********** */

/* *************
   Probability computation
   Old version

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

/**********************

 Probability Computation

***********************/

:- thread_local
	%get_var_n/5,
        rule_n/1,
        na/2,
        v/3.

%rule_n(0).

compute_prob(M,Expl,Prob):-
  retractall(v(_,_,_)),
  retractall(na(_,_)),
  retractall(rule_n(_)),
  assert(rule_n(0)),
  %findall(1,M:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',_,_),NAnnAss),length(NAnnAss,NV),
  get_bdd_environment(M,Env),
  build_bdd(M,Env,Expl,BDD),
  ret_prob(Env,BDD,Prob),
  clean_environment(M,Env), !.

get_var_n(Env,R,S,Probs,V):-
  (
    v(R,S,V) ->
      true
    ;
      %length(Probs,L),
      add_var(Env,Probs,R,V),
      assert(v(R,S,V))
  ).


get_prob_ax(M,(Ax,_Ind),N,Prob):- !,
  compute_prob_ax(M,Ax,Prob),
  ( na(Ax,N) ->
      true
    ;
      rule_n(N),
      assert(na(Ax,N)),
      retract(rule_n(N)),
      N1 is N + 1,
      assert(rule_n(N1))
  ).
get_prob_ax(M,Ax,N,Prob):- !,
  compute_prob_ax(M,Ax,Prob),
  ( na(Ax,N) ->
      true
    ;
      rule_n(N),
      assert(na(Ax,N)),
      retract(rule_n(N)),
      N1 is N + 1,
      assert(rule_n(N1))
  ).

compute_prob_ax(M,Ax,Prob):-
  findall(ProbA,(M:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',Ax,literal(ProbAT)),atom_number(ProbAT,ProbA)),Probs),
  compute_prob_ax1(Probs,Prob).

compute_prob_ax1([Prob],Prob):-!.

compute_prob_ax1([Prob1,Prob2],Prob):-!,
  Prob is Prob1+Prob2-(Prob1*Prob2).

compute_prob_ax1([Prob1 | T],Prob):-
  compute_prob_ax1(T,Prob0),
  Prob is Prob1 + Prob0 - (Prob1*Prob0).

/************************/

unload_all_algorithms :-
  unload_file(library(trill_internal)),
  unload_file(library(trillp_internal)),
  unload_file(library(tornado_internal)).

set_algorithm(M:trill):-
  unload_all_algorithms,
  consult(library(trill_internal)),
  clean_up(M),!.

set_algorithm(M:trillp):-
  unload_all_algorithms,
  consult(library(trillp_internal)),
  clean_up(M),!.

set_algorithm(M:tornado):-
  unload_all_algorithms,
  consult(library(tornado_internal)),
  clean_up(M),!.

init_trill(Alg):-
  utility_translation:get_module(M),
  set_algorithm(M:Alg),
  set_up(M),
  trill:add_kb_prefixes(M:[('disponte'='https://sites.google.com/a/unife.it/ml/disponte#'),('owl'='http://www.w3.org/2002/07/owl#')]).

/**************/
/*get_trill_current_module('utility_translation'):-
  pengine_self(_Name),!.*/
%get_trill_current_module(Name):-
%  pengine_self(Name),!.
%get_trill_current_module('utility_translation'):- !.
get_trill_current_module(M):-
  utility_translation:get_module(M).
/**************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(trill:get_var_n(_,_,_,_,_)).


/*
sandbox:safe_primitive(trill:sub_class(_,_)).
sandbox:safe_primitive(trill:sub_class(_,_,_)).
sandbox:safe_primitive(trill:prob_sub_class(_,_,_)).
sandbox:safe_primitive(trill:instanceOf(_,_)).
sandbox:safe_primitive(trill:instanceOf(_,_,_)).
sandbox:safe_primitive(trill:prob_instanceOf(_,_,_)).
sandbox:safe_primitive(trill:property_value(_,_,_)).
sandbox:safe_primitive(trill:property_value(_,_,_,_)).
sandbox:safe_primitive(trill:prob_property_value(_,_,_,_)).
sandbox:safe_primitive(trill:unsat(_)).
sandbox:safe_primitive(trill:unsat(_,_)).
sandbox:safe_primitive(trill:prob_unsat(_,_)).
sandbox:safe_primitive(trill:inconsistent_theory).
sandbox:safe_primitive(trill:inconsistent_theory(_)).
sandbox:safe_primitive(trill:prob_inconsistent_theory(_)).
sandbox:safe_primitive(trill:axiom(_)).
sandbox:safe_primitive(trill:add_kb_prefix(_,_)).
sandbox:safe_primitive(trill:add_kb_prefixes(_)).
sandbox:safe_primitive(trill:add_axiom(_)).
sandbox:safe_primitive(trill:add_axioms(_)).
sandbox:safe_primitive(trill:load_kb(_)).
sandbox:safe_primitive(trill:load_owl_kb(_)).
*/

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(trill:sub_class(_,_),[]).
sandbox:safe_meta(trill:sub_class(_,_,_),[]).
sandbox:safe_meta(trill:sub_class(_,_,_,_),[]).
sandbox:safe_meta(trill:prob_sub_class(_,_,_),[]).
sandbox:safe_meta(trill:instanceOf(_,_),[]).
sandbox:safe_meta(trill:instanceOf(_,_,_),[]).
sandbox:safe_meta(trill:instanceOf(_,_,_,_),[]).
sandbox:safe_meta(trill:prob_instanceOf(_,_,_),[]).
sandbox:safe_meta(trill:property_value(_,_,_),[]).
sandbox:safe_meta(trill:property_value(_,_,_,_),[]).
sandbox:safe_meta(trill:property_value(_,_,_,_,_),[]).
sandbox:safe_meta(trill:prob_property_value(_,_,_,_),[]).
sandbox:safe_meta(trill:unsat(_),[]).
sandbox:safe_meta(trill:unsat(_,_),[]).
sandbox:safe_meta(trill:unsat(_,_,_),[]).
sandbox:safe_meta(trill:prob_unsat(_,_),[]).
sandbox:safe_meta(trill:inconsistent_theory,[]).
sandbox:safe_meta(trill:inconsistent_theory(_),[]).
sandbox:safe_meta(trill:inconsistent_theory(_,_),[]).
sandbox:safe_meta(trill:prob_inconsistent_theory(_),[]).
sandbox:safe_meta(trill:axiom(_),[]).
sandbox:safe_meta(trill:add_kb_prefix(_,_),[]).
sandbox:safe_meta(trill:add_kb_prefixes(_),[]).
sandbox:safe_meta(trill:remove_kb_prefix(_,_),[]).
sandbox:safe_meta(trill:remove_kb_prefix(_),[]).
sandbox:safe_meta(trill:add_axiom(_),[]).
sandbox:safe_meta(trill:add_axioms(_),[]).
sandbox:safe_meta(trill:load_kb(_),[]).
sandbox:safe_meta(trill:load_owl_kb(_),[]).

:- use_module(library(utility_translation)).

user:term_expansion((:- trill),[]):-
  utility_translation:get_module(M),
  set_algorithm(M:trill),
  set_up(M),
  trill:add_kb_prefixes(M:[('disponte'='https://sites.google.com/a/unife.it/ml/disponte#'),('owl'='http://www.w3.org/2002/07/owl#')]).

user:term_expansion((:- trillp),[]):-
  utility_translation:get_module(M),
  set_algorithm(M:trillp),
  set_up(M),
  trill:add_kb_prefixes(M:['disponte'='https://sites.google.com/a/unife.it/ml/disponte#','owl'='http://www.w3.org/2002/07/owl#']).

user:term_expansion((:- tornado),[]):-
  utility_translation:get_module(M),
  set_algorithm(M:tornado),
  set_up(M),
  trill:add_kb_prefixes(M:['disponte'='https://sites.google.com/a/unife.it/ml/disponte#','owl'='http://www.w3.org/2002/07/owl#']).

