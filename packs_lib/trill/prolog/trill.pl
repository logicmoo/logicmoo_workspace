/** <module> trill

This module performs reasoning over probabilistic description logic knowledge bases.
It reads probabilistic knowledge bases in RDF format or in Prolog format, a functional-like
sintax based on definitions of Thea library, and answers queries by finding the set 
of explanations or computing the probability.

[1] http://vangelisv.github.io/thea/

See https://github.com/rzese/trill/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~rzese/software/trill/manual.html for
details.

@version 6.0.2
@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/

:- module(trill,[sub_class/2, sub_class/3, prob_sub_class/3, sub_class/4, all_sub_class/3,
                 instanceOf/2, instanceOf/3, prob_instanceOf/3, instanceOf/4, all_instanceOf/3,
                 property_value/3, property_value/4, prob_property_value/4, property_value/5, all_property_value/4,
                 unsat/1, unsat/2, prob_unsat/2, unsat/3, all_unsat/2,
                 inconsistent_theory/0, inconsistent_theory/1, prob_inconsistent_theory/1, inconsistent_theory/2, all_inconsistent_theory/1,
                 axiom/1, kb_prefixes/1, add_kb_prefix/2, add_kb_prefixes/1, add_axiom/1, add_axioms/1, remove_kb_prefix/2, remove_kb_prefix/1, remove_axiom/1, remove_axioms/1,
                 load_kb/1, load_owl_kb/1, load_owl_kb_from_string/1, init_trill/1,
                 set_tableau_expansion_rules/2] ).

:- meta_predicate sub_class(:,+).
:- meta_predicate sub_class(:,+,-).
:- meta_predicate sub_class(:,+,-,+).
:- meta_predicate all_sub_class(:,+,-).
:- meta_predicate prob_sub_class(:,+,-).
:- meta_predicate instanceOf(:,+).
:- meta_predicate instanceOf(:,+,-).
:- meta_predicate instanceOf(:,+,-,+).
:- meta_predicate all_instanceOf(:,+,-).
:- meta_predicate prob_instanceOf(:,+,-).
:- meta_predicate property_value(:,+,+).
:- meta_predicate property_value(:,+,+,-).
:- meta_predicate property_value(:,+,+,-,+).
:- meta_predicate all_property_value(:,+,+,-).
:- meta_predicate prob_property_value(:,+,+,-).
:- meta_predicate unsat(:).
:- meta_predicate unsat(:,-).
:- meta_predicate unsat(:,-,+).
:- meta_predicate all_unsat(:,-).
:- meta_predicate prob_unsat(:,-).
:- meta_predicate inconsistent_theory(:).
:- meta_predicate inconsistent_theory(:,+).
:- meta_predicate all_inconsistent_theory(:).
:- meta_predicate prob_inconsistent_theory(:).
:- meta_predicate axiom(:).
:- meta_predicate kb_prefixes(:).
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
:- meta_predicate set_tableau_expansion_rules(:,+).

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
:- multifile setting_trill_default/2.

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

:- multifile kb_prefixes/1.

/**
 * set_tableau_expansion_rules(:DetRules:list,++NondetRules:list) is det
 * 
 * This predicate set the rules as taken in input, maintaining order and number of rules.
 * DetRules is the list of deterministic rules, NondetRules the list of non-deterministic ones.
 */
set_tableau_expansion_rules(M:DetRules,NondetRules):-
  retractall(M:setting_trill(det_rules,_)),
  retractall(M:setting_trill(nondet_rules,_)),
  assert(M:setting_trill(det_rules,DetRules)),
  assert(M:setting_trill(nondet_rules,NondetRules)).

/*****************************
  MESSAGES
******************************/
:- multifile prolog:message/1.

prolog:message(iri_not_exists(IRIs)) -->
  [ 'IRIs not existent: ~w' -[IRIs] ].

prolog:message(inconsistent) -->
  [ 'Inconsistent ABox' ].

prolog:message(consistent) -->
  [ 'Consistent ABox' ].

prolog:message(wrong_number_max_expl) -->
  [ 'max_expl option can take integer values or "all"' ].

prolog:message(timeout_reached) -->
  [ 'Timeout reached' ].

/*****************************
  QUERY OPTIONS
******************************/

/** 
 * query_option(+OptList:list,+Option:term,?Value:term)
 * 
 * Check if the option defined by Option is in OptList and returns the option Value.

 * Options can be:
 * - assert_abox(Boolean) if Boolean is set to true the list of aboxes is asserted with the predicate final_abox/1
 * - return_prob(Prob) if present the probability of the query is computed and unified with Prob
 * - return_single_prob(Boolean) must be used with return_prob(Prob). It forces TRILL to return the probability of each single explanation
 * - max_expl(Value) to limit the maximum number of explanations to find. Value must be an integer. The predicate will return a list containing at most Value different explanations
 * - time_limit(Value) to limit the time for the inference. The predicate will return the explanations found in the time allowed. Value is the number of seconds allowed for the search of explanations 
*/
query_option(OptList,Option,Value):-
  Opt=..[Option,Value],
  memberchk(Opt,OptList).

/****************************
  QUERY PREDICATES
*****************************/

execute_query(M,QueryType,QueryArgsNC,Expl,QueryOptions):-
  check_query_args(M,QueryArgsNC,QueryArgs,QueryArgsNotPresent),
  ( dif(QueryArgsNotPresent,[]) ->
    (print_message(warning,iri_not_exists(QueryArgsNotPresent)),!,false) ; true
  ),
  find_explanations(M,QueryType,QueryArgs,Expl,QueryOptions),
  ( query_option(QueryOptions,return_prob,Prob) ->
    (
      compute_prob_and_close(M,Expl,Prob),
      (query_option(QueryOptions,return_single_prob,false) -> true ; !)
    ) ; true
  ).


% Execution monitor
find_explanations(M,QueryType,QueryArgs,Expl,QueryOptions):-
  % TODO call_with_time_limit
  ( query_option(QueryOptions,assert_abox,AssertABox) -> Opt=[assert_abox(AssertABox)] ; Opt=[]),
  ( query_option(QueryOptions,max_expl,N) -> 
    MonitorNExpl = N
    ; 
    ( ( query_option(QueryOptions,return_prob,_) -> MonitorNExpl=all ; MonitorNExpl=bt) ) % if return_prob is present and no max_expl force to find all the explanations
  ),
  ( query_option(QueryOptions,time_limit,MonitorTimeLimit) ->
    find_n_explanations_time_limit(M,QueryType,QueryArgs,Expl,MonitorNExpl,MonitorTimeLimit,Opt)
    ;
    find_n_explanations(M,QueryType,QueryArgs,Expl,MonitorNExpl,Opt)
  ).

find_n_explanations_time_limit(M,QueryType,QueryArgs,Expl,MonitorNExpl,MonitorTimeLimit,Opt):-
  retractall(M:setting_trill(timeout,_)),
  get_time(Start),
  Timeout is Start + MonitorTimeLimit,
  assert(M:setting_trill(timeout,Timeout)),
  find_n_explanations(M,QueryType,QueryArgs,Expl,MonitorNExpl,Opt),
  get_time(End),
  (End<Timeout -> true ; print_message(warning,timeout_reached)).



find_single_explanation(M,QueryType,QueryArgs,Expl,Opt):-
  set_up_reasoner(M),
  build_abox(M,Tableau,QueryType,QueryArgs), % will expand the KB without the query
  (absence_of_clashes(Tableau) ->  % TODO if QueryType is inconsistent no check
    (
      add_q(M,QueryType,Tableau,QueryArgs,Tableau0),
      set_up_tableau(M),
      findall(Tableau1,expand_queue(M,Tableau0,Tableau1),L),
      (query_option(Opt,assert_abox,true) -> (writeln('Asserting ABox...'), M:assert(final_abox(L)), writeln('Done. Asserted in final_abox/1...')) ; true),
      find_expls(M,L,QueryArgs,Expl1),
      check_and_close(M,Expl1,Expl)
    )
  ;
    print_message(warning,inconsistent),!,false
  ).

/*************
 
  Monitor predicates

**************/

check_time_monitor(M):-
  M:setting_trill(timeout,Timeout),!,
  get_time(Now),
  Timeout<Now. % I must stop

/* *************** */

set_up_reasoner(M):-
  set_up(M),
  retractall(M:exp_found(_,_)),
  retractall(M:exp_found(_,_,_)),
  retractall(M:trillan_idx(_)),
  assert(M:trillan_idx(1)).

set_up_tableau(M):-
  % TO CHANGE move to KB loading
  %setting_trill_default(det_rules,DetRules),
  %setting_trill_default(nondet_rules,NondetRules),
  %set_tableau_expansion_rules(M:DetRules,NondetRules). 
  prune_tableau_rules(M).

% instanceOf
add_q(M,io,Tableau0,[ClassEx,IndEx],Tableau):- !,
  neg_class(ClassEx,NClassEx),
  add_q(M,Tableau0,classAssertion(NClassEx,IndEx),Tableau1),
  add_clash_to_tableau(M,Tableau1,NClassEx-IndEx,Tableau2),
  update_expansion_queue_in_tableau(M,NClassEx,IndEx,Tableau2,Tableau).

% property_value
add_q(M,pv,Tableau0,[PropEx,Ind1Ex,Ind2Ex],Tableau):-!,
  neg_class(PropEx,NPropEx), %use of neg_class to negate property
  add_q(M,Tableau0,propertyAssertion(NPropEx,Ind1Ex,Ind2Ex),Tableau1),
  add_clash_to_tableau(M,Tableau1,NPropEx-Ind1Ex-Ind2Ex,Tableau2),
  update_expansion_queue_in_tableau(M,PropEx,Ind1Ex,Ind2Ex,Tableau2,Tableau).


% sub_class
add_q(M,sc,Tableau0,[SubClassEx,SupClassEx],Tableau):- !,
  neg_class(SupClassEx,NSupClassEx),
  query_ind(QInd),
  add_q(M,Tableau0,classAssertion(intersectionOf([SubClassEx,NSupClassEx]),QInd),Tableau1),
  utility_translation:add_kb_atoms(M,class,[intersectionOf([SubClassEx,NSupClassEx])]), % This is necessary to correctly prune expansion rules
  add_owlThing_ind(M,Tableau1,QInd,Tableau2),
  add_clash_to_tableau(M,Tableau2,intersectionOf([SubClassEx,NSupClassEx])-QInd,Tableau3),
  update_expansion_queue_in_tableau(M,intersectionOf([SubClassEx,NSupClassEx]),QInd,Tableau3,Tableau).

% unsat
add_q(M,un,Tableau0,['unsat',ClassEx],Tableau):- !,
  query_ind(QInd),
  add_q(M,Tableau0,classAssertion(ClassEx,QInd),Tableau1),
  add_owlThing_ind(M,Tableau1,QInd,Tableau2),
  add_clash_to_tableau(M,Tableau2,ClassEx-QInd,Tableau3),
  update_expansion_queue_in_tableau(M,ClassEx,QInd,Tableau3,Tableau).

% inconsistent_theory
add_q(_,it,Tableau,['inconsistent','kb'],Tableau):- !. % Do nothing

/*
  Auxiliary predicates to extract the det of individuals connected to the query
*/

% Find the individuals directly connected to the given one
gather_connected_individuals(M,Ind,ConnectedInds):-
  find_successors(M,Ind,SuccInds),
  find_predecessors(M,Ind,PredInds),
  append(SuccInds,PredInds,ConnectedInds).

find_successors(M,Ind,List) :- findall(ConnectedInd, (M:propertyAssertion(_,Ind,ConnectedInd)), List).
find_predecessors(M,Ind,List) :- findall(ConnectedInd, (M:propertyAssertion(_,ConnectedInd,Ind)), List).

intersect([H|_], List) :- member(H, List), !.
intersect([_|T], List) :- intersect(T, List).

% Recursively gather all the connected individuals, i.e., isolate the relevant fragment of the KB.
%scan_connected_individuals(M,IndividualsToCheck,IndividualsChecked,IndividualsSet0,IndividualsSet).
scan_connected_individuals(_,[],_,IndividualsSet0,IndividualsSet):-
  sort(IndividualsSet0,IndividualsSet).

scan_connected_individuals(M,[H|IndividualsToCheck],IndividualsChecked,IndividualsSet0,IndividualsSet):-
  memberchk(H,IndividualsChecked),!,
  scan_connected_individuals(M,IndividualsToCheck,IndividualsChecked,IndividualsSet0,IndividualsSet).


scan_connected_individuals(M,[H|IndividualsToCheck0],IndividualsChecked,IndividualsSet0,IndividualsSet):-
  gather_connected_individuals(M,H,NewIndividualsToCheck),
  append(IndividualsSet0,NewIndividualsToCheck,IndividualsSet1),
  append(IndividualsToCheck0,NewIndividualsToCheck,IndividualsToCheck),
  scan_connected_individuals(M,IndividualsToCheck,[H|IndividualsChecked],IndividualsSet1,IndividualsSet).


% Builds the list of individuals conneted given the query type
collect_individuals(M,io,[_,IndEx],IndividualsSet):-
  scan_connected_individuals(M,[IndEx],[],[IndEx],IndividualsSet).

collect_individuals(M,pv,[_,Ind1Ex,Ind2Ex],IndividualsSet):-
  scan_connected_individuals(M,[Ind1Ex,Ind2Ex],[],[Ind1Ex,Ind2Ex],IndividualsSet).

collect_individuals(_,sc,[_,_],[QInd]):- % It is not necessary to check the KB as the individual of the query is a new fresh individual not included in the KB.
  query_ind(QInd).

collect_individuals(_,un,['unsat',_],[QInd]):- % It is not necessary to check the KB as the individual of the query is a new fresh individual not included in the KB.
  query_ind(QInd).

collect_individuals(_,it,['inconsistent','kb'],[]):-!.

/*
  check the KB atoms to consider only the necessary expansion rules, pruning the useless ones
*/
prune_tableau_rules(M):-
  M:kb_atom(KBA),
  Classes=KBA.class,
  setting_trill_default(det_rules,DetRules),
  prune_tableau_rules(Classes,DetRules,PrunedDetRules),
  setting_trill_default(nondet_rules,NondetRules),
  prune_tableau_rules(Classes,NondetRules,PrunedNondetRules),
  set_tableau_expansion_rules(M:PrunedDetRules,PrunedNondetRules).

add_tableau_rules_from_class(M,someValuesFrom(_,_)):-
  M:setting_trill(det_rules,Rules),
  memberchk(exists_rule,Rules),!.

add_tableau_rules_from_class(M,C):-
  M:kb_atom(KBA),
  Classes=KBA.class,
  setting_trill_default(det_rules,DetRules),
  prune_tableau_rules([C|Classes],DetRules,PrunedDetRules),
  setting_trill_default(nondet_rules,NondetRules),
  prune_tableau_rules([C|Classes],NondetRules,PrunedNondetRules),
  set_tableau_expansion_rules(M:PrunedDetRules,PrunedNondetRules).

% o_rule,and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule,min_rule,or_rule,max_rule,ch_rule
prune_tableau_rules(_,[],[]).

prune_tableau_rules(KBA,[o_rule|TR],[o_rule|PTR]):-
  memberchk(oneOf(_),KBA),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[and_rule|TR],[and_rule|PTR]):-
  memberchk(intersectionOf(_),KBA),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[unfold_rule|TR],[unfold_rule|PTR]):-
  !,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[add_exists_rule|TR],[add_exists_rule|PTR]):-
  !,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[forall_rule|TR],[forall_rule|PTR]):-
  memberchk(allValuesFrom(_,_),KBA),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[forall_plus_rule|TR],[forall_plus_rule|PTR]):-
  memberchk(allValuesFrom(_,_),KBA),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[exists_rule|TR],[exists_rule|PTR]):-
  memberchk(someValuesFrom(_,_),KBA),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[min_rule|TR],[min_rule|PTR]):-
  (memberchk(minCardinality(_,_),KBA); memberchk(minCardinality(_,_,_),KBA);memberchk(exactCardinality(_,_),KBA);memberchk(exactCardinality(_,_,_),KBA)),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[or_rule|TR],[or_rule|PTR]):-
  memberchk(unionOf(_),KBA),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[max_rule|TR],[max_rule|PTR]):-
  (memberchk(maxCardinality(_,_),KBA); memberchk(maxCardinality(_,_,_),KBA);memberchk(exactCardinality(_,_),KBA);memberchk(exactCardinality(_,_,_),KBA)),!,
  prune_tableau_rules(KBA,TR,PTR).


prune_tableau_rules(KBA,[ch_rule|TR],[ch_rule|PTR]):-
  (memberchk(maxCardinality(_,_),KBA); memberchk(maxCardinality(_,_,_),KBA);memberchk(exactCardinality(_,_),KBA);memberchk(exactCardinality(_,_,_),KBA)),!,
  prune_tableau_rules(KBA,TR,PTR).

prune_tableau_rules(KBA,[_|TR],PTR):-
  prune_tableau_rules(KBA,TR,PTR).

/***********
  Queries
  - with and without explanations -
 ***********/
/**
 * instanceOf(:Class:concept_description,++Ind:individual_name,-Expl:list,-Expl:list,++Opt:list)
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns one explanation for the instantiation of the individual to the given class.
 * The returning explanation is a set of axioms.
 * The predicate fails if the individual does not belong to the class.
 * Opt is a list containing settings for the execution of the query. 
  */
instanceOf(M:Class,Ind,Expl,Opt):-
  execute_query(M,io,[Class,Ind],Expl,Opt).
  

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
  instanceOf(M:Class,Ind,Expl,[]).

/**
 * all_instanceOf(:Class:concept_description,++Ind:individual_name)
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns all the explanations for the instantiation of the individual to the given class.
 * The returning explanations are in the form if a list (set) of set of axioms.
 * The predicate fails if the individual does not belong to the class.
 */
all_instanceOf(M:Class,Ind,Expl):-
  execute_query(M,io,[Class,Ind],Expl,[max_expl(all)]),
  empty_expl(M,EExpl),
  dif(Expl,EExpl).

/**
 * instanceOf(:Class:concept_description,++Ind:individual_name) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns true if the individual belongs to the class, false otherwise.
 */
instanceOf(M:Class,Ind):-
  execute_query(M,io,[Class,Ind],_,[max_expl(1)]),!.

/**
 * property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,-Expl:list,++Opt:list)
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns one explanation for the fact Ind1 is related with Ind2 via Prop.
 * The returning explanation is a set of axioms.
 * The predicate fails if the two individual are not Prop-related. * 
 * Opt is a list containing settings for the execution of the query. 
 * Settings can be:
 * - assert_abox(Boolean) if Boolean is set to true the list of aboxes is asserted with the predicate final_abox/1
 * - return_prob(Prob) if present the probability of the query is computed and unified with Prob
 */
property_value(M:Prop, Ind1, Ind2,Expl,Opt):-
  execute_query(M,pv,[Prop, Ind1, Ind2],Expl,Opt).

/**
 * property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns one explanation for the fact Ind1 is related with Ind2 via Prop.
 * The returning explanation is a set of axioms.
 * The predicate fails if the two individual are not Prop-related.
 */
property_value(M:Prop, Ind1, Ind2,Expl):-
  property_value(M:Prop, Ind1, Ind2,Expl,[]).

/**
 * all_property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns all the explanations for the fact Ind1 is related with Ind2 via Prop.
 * The returning explanations are in the form if a list (set) of set of axioms.
 * The predicate fails if the individual does not belong to the class.
 */
all_property_value(M:Prop, Ind1, Ind2,Expl):-
  execute_query(M,pv,[Prop, Ind1, Ind2],Expl,[max_expl(all)]),
  empty_expl(M,EExpl),
  dif(Expl,EExpl).

/**
 * property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name) is det
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns true if the two individual are Prop-related, false otherwise.
 */
property_value(M:Prop, Ind1, Ind2):-
  execute_query(M,pv,[Prop, Ind1, Ind2],_,[max_expl(1)]),!.

/**
 * sub_class(:Class:concept_description,++SupClass:concept_description,-Expl:list,++Opt:list)
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * one explanation for the subclass relation between Class and SupClass.
 * The returning explanation is a set of axioms.
 * The predicate fails if there is not a subclass relation between the two classes.
 * Opt is a list containing settings for the execution of the query. 
 * Settings can be:
 * - assert_abox(Boolean) if Boolean is set to true the list of aboxes is asserted with the predicate final_abox/1
 * - return_prob(Prob) if present the probability of the query is computed and unified with Prob
 */
sub_class(M:Class,SupClass,Expl,Opt):-
  execute_query(M,sc,[Class,SupClass],Expl,Opt).
  
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
  sub_class(M:Class,SupClass,Expl,[]).

/**
 * all_sub_class(:Class:concept_description,++SupClass:concept_description,-Expl:list)
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * all the explanations for the subclass relation between Class and SupClass.
 * The returning explanations are in the form if a list (set) of set of axioms.
 * The predicate fails if the individual does not belong to the class.
 */
all_sub_class(M:Class,SupClass,Expl):-
  execute_query(M,sc,[Class,SupClass],Expl,[max_expl(all)]).

/**
 * sub_class(:Class:concept_description,++SupClass:concept_description) is det
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns
 * true if Class is a subclass of SupClass, and false otherwise.
 */
sub_class(M:Class,SupClass):-
  execute_query(M,sc,[Class,SupClass],_,[max_expl(1)]),!.

/**
 * unsat(:Concept:concept_description,-Expl:list,++Opt:list)
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns one explanation for the unsatisfiability of the concept.
 * The returning explanation is a set of axioms.
 * The predicate fails if the concept is satisfiable.
 * Opt is a list containing settings for the execution of the query. 
 * Settings can be:
 * - assert_abox(Boolean) if Boolean is set to true the list of aboxes is asserted with the predicate final_abox/1
 * - return_prob(Prob) if present the probability of the query is computed and unified with Prob
 */
unsat(M:Concept,Expl,Opt):-
  execute_query(M,un,[Concept],Expl,Opt).

/**
 * unsat(:Concept:concept_description,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns one explanation for the unsatisfiability of the concept.
 * The returning explanation is a set of axioms.
 * The predicate fails if the concept is satisfiable.
 */
unsat(M:Concept,Expl):-
  unsat(M:Concept,Expl,[]).

/**
 * all_unsat(:Concept:concept_description,-Expl:list)
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns all the explanations for the unsatisfiability of the concept.
 * The returning explanations are in the form if a list (set) of set of axioms.
 * The predicate fails if the individual does not belong to the class.
 */
all_unsat(M:Concept,Expl):-
  execute_query(M,un,[Concept],Expl,[max_expl(all)]),
  empty_expl(M,EExpl),
  dif(Expl,EExpl).

/**
 * unsat(:Concept:concept_description) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns true if the concept is unsatisfiable, false otherwise.
 */
unsat(M:Concept):-
execute_query(M,un,[Concept],_,[max_expl(1)]),!.

/**
 * inconsistent_theory(:Expl:list,++Opt:list)
 *
 * This predicate returns one explanation for the inconsistency of the loaded knowledge base.
 * Opt is a list containing settings for the execution of the query. 
 * Settings can be:
 * - assert_abox(Boolean) if Boolean is set to true the list of aboxes is asserted with the predicate final_abox/1
 * - return_prob(Prob) if present the probability of the query is computed and unified with Prob
 */
inconsistent_theory(M:Expl,Opt):-
  execute_query(M,it,[],Expl,Opt).

/**
 * inconsistent_theory(:Expl:list)
 *
 * This predicate returns one explanation for the inconsistency of the loaded knowledge base.
 */
inconsistent_theory(M:Expl):-
  inconsistent_theory(M:Expl,[]).

/**
 * all_inconsistent_theory(:Expl:list)
 *
 * This predicate returns all the explanations for the inconsistency of the loaded knowledge base.
 * The returning explanations are in the form if a list (set) of set of axioms.
 * The predicate fails if the individual does not belong to the class.
 */
all_inconsistent_theory(M:Expl):-
  execute_query(M,it,[],Expl,[max_expl(all)]),
  empty_expl(M,EExpl),
  dif(Expl,EExpl).

/**
 * inconsistent_theory
 *
 * This predicate returns true if the loaded knowledge base is inconsistent, otherwise it fails.
 */
inconsistent_theory:-
  get_trill_current_module(M),
  execute_query(M,it,[],_,[max_expl(1)]),!.

/**
 * prob_instanceOf(:Class:concept_description,++Ind:individual_name,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition
 * of a complex concept as a ground term and name or the full URI of an individual and
 * returns the probability of the instantiation of the individual to the given class.
 */
prob_instanceOf(M:Class,Ind,Prob):-
  instanceOf(M:Class,Ind,_,[return_prob(Prob)]).

/**
 * prob_property_value(:Prop:property_name,++Ind1:individual_name,++Ind2:individual_name,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a property and of two individuals
 * and returns the probability of the fact Ind1 is related with Ind2 via Prop.
 */
prob_property_value(M:Prop, Ind1, Ind2,Prob):-
  property_value(M:Prop, Ind1, Ind2,_,[return_prob(Prob)]).

/**
 * prob_sub_class(:Class:concept_description,++SupClass:class_name,--Prob:double) is det
 *
 * This predicate takes as input two concepts which can be given by the name or the full URI 
 * of two a simple concept or the definition of a complex concept as a ground term and returns 
 * the probability of the subclass relation between Class and SupClass.
 */
prob_sub_class(M:Class,SupClass,Prob):-
  sub_class(M:Class,SupClass,_,[return_prob(Prob)]).

/**
 * prob_unsat(:Concept:concept_description,--Prob:double) is det
 *
 * This predicate takes as input the name or the full URI of a class or the definition of 
 * a complex concept as a ground term and returns the probability of the unsatisfiability
 * of the concept.
 */
prob_unsat(M:Concept,Prob):-
  unsat(M:Concept,_,[return_prob(Prob)]).

/**
 * prob_inconsistent_theory(:Prob:double) is det
 *
 * If the knowledge base is inconsistent, this predicate returns the probability of the inconsistency.
 */
prob_inconsistent_theory(M:Prob):-
  inconsistent_theory(M:_,[return_prob(Prob)]).

/***********
  Utilities for queries
 ***********/

% adds the query into the ABox
add_q(M,Tableau0,Query,Tableau):-
  query_empty_expl(M,Expl),
  add_to_tableau(Tableau0,(Query,Expl),Tableau1),
  create_tabs([(Query,Expl)],Tableau1,Tableau).


% initialize an empty explanation for the query with the query placeholder 'qp' in teh choicepoint list
query_empty_expl(M,Expl):-%gtrace,
  empty_expl(M,EExpl),
  add_choice_point(M,qp,EExpl,Expl).


% expands query arguments using prefixes and checks their existence in the kb
% returns the non-present arguments
check_query_args(M,QA,QAEx,NotEx):-
  check_query_args_int(M,QA,QAExT,NotEx),!,
  ( length(QA,1) -> 
    QAEx = ['unsat'|QAExT]
    ;
    ( length(QA,0) -> QAEx = ['inconsistent','kb'] ; QAEx = QAExT)
  ).

check_query_args_int(_,[],[],[]).

check_query_args_int(M,[H|T],[HEx|TEx],NotEx):-
  check_query_args(M,[H],[HEx]),!,
  check_query_args_int(M,T,TEx,NotEx).

check_query_args_int(M,[H|T],TEx,[H|NotEx]):-
  check_query_args_int(M,T,TEx,NotEx).

% expands query arguments using prefixes and checks their existence in the kb
check_query_args(M,L,LEx) :-
  M:ns4query(NSList),
  expand_all_ns(M,L,NSList,false,LEx), %from utility_translation module
  check_query_args_presence(M,LEx).

check_query_args_presence(_M,[]):-!.

check_query_args_presence(M,['http://www.w3.org/2002/07/owl#Thing'|T]) :-
  check_query_args_presence(M,T).

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
  ( member(H,L1.class) ; member(H,L1.objectProperty) ; member(H,L1.individual) ; member(H,L1.dataProperty) ; member(H,L1.annotationProperty) ; member(H,L1.datatype) ),!.

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
:- multifile clash/4.

clash(M,owlnothing,Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 6'),nl,
  findClassAssertion4OWLNothing(M,ABox,Expl).

clash(M,C-Ind,Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 1'),nl,
  findClassAssertion(C,Ind,Expl1,ABox),
  neg_class(C,NegC),
  findClassAssertion(NegC,Ind,Expl2,ABox),
  and_f(M,Expl1,Expl2,Expl).

clash(M,sameIndividual(LS),Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 2.a'),nl,
  find((sameIndividual(LS),Expl1),ABox),
  find((differentIndividuals(LD),Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  dif(X,Y),
  and_f(M,Expl1,Expl2,Expl).

clash(M,differentIndividuals(LS),Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 2.b'),nl,
  find((differentIndividuals(LS),Expl1),ABox),
  find((sameIndividual(LD),Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  dif(X,Y),
  and_f(M,Expl1,Expl2,Expl).

clash(M,C-sameIndividual(L1),Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 3'),nl,
  findClassAssertion(C,sameIndividual(L1),Expl1,ABox),
  neg_class(C,NegC),
  findClassAssertion(NegC,sameIndividual(L2),Expl2,ABox),
  samemember(L1,L2),!,
  and_f(M,Expl1,Expl2,Expl).

samemember(L1,L2):-
  member(X,L1),
  member(X,L2),!.

clash(M,C-Ind1,Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 4'),nl,
  findClassAssertion(C,Ind1,Expl1,ABox),
  neg_class(C,NegC),
  findClassAssertion(NegC,sameIndividual(L2),Expl2,ABox),
  member(Ind1,L2),
  and_f(M,Expl1,Expl2,Expl).

clash(M,C-sameIndividual(L1),Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 5'),nl,
  findClassAssertion(C,sameIndividual(L1),Expl1,ABox),
  neg_class(C,NegC),
  findClassAssertion(NegC,Ind2,Expl2,ABox),
  member(Ind2,L1),
  and_f(M,Expl1,Expl2,Expl).

clash(M,C1-Ind,Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 7'),nl,
  M:disjointClasses(L), % TODO use hierarchy
  member(C1,L),
  member(C2,L),
  dif(C1,C2),
  findClassAssertion(C1,Ind,Expl1,ABox),
  findClassAssertion(C2,Ind,Expl2,ABox),
  and_f(M,Expl1,Expl2,ExplT),
  and_f_ax(M,disjointClasses(L),ExplT,Expl).

clash(M,C1-Ind,Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 8'),nl,
  M:disjointUnion(Class,L), % TODO use hierarchy
  member(C1,L),
  member(C2,L),
  dif(C1,C2),
  findClassAssertion(C1,Ind,Expl1,ABox),
  findClassAssertion(C2,Ind,Expl2,ABox),
  and_f(M,Expl1,Expl2,ExplT),
  and_f_ax(M,disjointUnion(Class,L),ExplT,Expl).

clash(M,P-Ind1-Ind2,Tab,Expl):-
  get_abox(Tab,ABox),
  %write('clash 11'),nl,
  findPropertyAssertion(P,Ind1,Ind2,Expl1,ABox),
  neg_class(P,NegP), % use of neg_class with a property
  findPropertyAssertion(NegP,Ind1,Ind2,Expl2,ABox),
  and_f(M,Expl1,Expl2,Expl).


/*
clash(M,Tab,Expl):-
  %write('clash 9'),nl,
  findClassAssertion(maxCardinality(N,S,C),Ind,Expl1,ABox),
  s_neighbours(M,Ind,S,Tab,SN),
  get_abox(Tab,ABox),
  individual_class_C(SN,C,ABox,SNC),
  length(SNC,LSS),
  LSS @> N,
  make_expl(M,Ind,S,SNC,Expl1,ABox,Expl).

clash(M,Tab,Expl):-
  %write('clash 10'),nl,
  findClassAssertion(maxCardinality(N,S),Ind,Expl1,ABox),
  s_neighbours(M,Ind,S,Tab,SN),
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


:- multifile check_clash/3.

check_clash(_,'http://www.w3.org/2002/07/owl#Nothing'-_,_):-
  %write('clash 6'),nl,
  !.

check_clash(_,C-Ind,Tab):-
  get_abox(Tab,ABox),
  %write('clash 1'),nl,
  neg_class(C,NegC),
  findClassAssertion(NegC,Ind,_,ABox),!.
  
check_clash(_,sameIndividual(LS),Tab):-
  get_abox(Tab,ABox),
  %write('clash 2.a'),nl,
  find((differentIndividuals(LD),_Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  dif(X,Y),!.
  
check_clash(_,differentIndividuals(LS),Tab):-
  get_abox(Tab,ABox),
  %write('clash 2.b'),nl,
  find((sameIndividual(LD),_Expl2),ABox),
  member(X,LS),
  member(Y,LS),
  member(X,LD),
  member(Y,LD),
  dif(X,Y),!.
  
check_clash(_,C-sameIndividual(L1),Tab):-
  get_abox(Tab,ABox),
  %write('clash 3'),nl,
  neg_class(C,NegC),
  findClassAssertion(NegC,sameIndividual(L2),_Expl2,ABox),
  member(X,L1),
  member(X,L2),!.
  
check_clash(_,C-Ind1,Tab):-
  get_abox(Tab,ABox),
  %write('clash 4'),nl,
  neg_class(C,NegC),
  findClassAssertion(NegC,sameIndividual(L2),_Expl2,ABox),
  member(Ind1,L2),!.
  
check_clash(_,C-sameIndividual(L1),Tab):-
  get_abox(Tab,ABox),
  %write('clash 5'),nl,
  neg_class(C,NegC),
  findClassAssertion(NegC,Ind2,_,ABox),
  member(Ind2,L1),!.
  
check_clash(M,C1-Ind,Tab):-
  get_abox(Tab,ABox),
  %write('clash 7'),nl,
  M:disjointClasses(L), % TODO use hierarchy
  member(C1,L),
  member(C2,L),
  dif(C1,C2),
  findClassAssertion(C2,Ind,_,ABox),!.
  
check_clash(M,C1-Ind,Tab):-
  get_abox(Tab,ABox),
  %write('clash 8'),nl,
  M:disjointUnion(_Class,L), % TODO use hierarchy
  member(C1,L),
  member(C2,L),
  dif(C1,C2),
  findClassAssertion(C2,Ind,_,ABox),!.

check_clash(_,P-Ind1-Ind2,Tab):-
  get_abox(Tab,ABox),
  %write('clash 11'),nl,
  neg_class(P,NegP),  % use of neg_class with a property
  findPropertyAssertion(NegP,Ind1,Ind2,_,ABox),!.

% -------------
% rules application
% -------------
expand_queue(M,Tab,Tab):-
  test_end_expand_queue(M,Tab),!.

expand_queue(M,Tab0,Tab):-
  extract_from_expansion_queue(Tab0,EA,Tab1),!,
  apply_all_rules(M,Tab1,EA,Tab2),
  % update_queue(M,T,NewExpQueue),
  expand_queue(M,Tab2,Tab).


test_end_expand_queue(M,_):-
  check_time_monitor(M),!.

test_end_expand_queue(_,Tab):-
  expansion_queue_is_empty(Tab).

%expand_queue(M,ABox0,[_EA|T],ABox):-
%  expand_queue(M,ABox0,T,ABox).

apply_all_rules(M,Tab0,EA,Tab):-
  M:setting_trill(det_rules,Rules),
  apply_det_rules(M,Rules,Tab0,EA,Tab1),
  (test_end_apply_rule(M,Tab0,Tab1) ->
  Tab=Tab1;
  apply_all_rules(M,Tab1,EA,Tab)).

apply_det_rules(M,[],Tab0,EA,Tab):-
  M:setting_trill(nondet_rules,Rules),
  apply_nondet_rules(M,Rules,Tab0,EA,Tab).

apply_det_rules(M,[H|_],Tab0,EA,Tab):-
  %C=..[H,Tab,Tab1],
  call(H,M,Tab0,EA,Tab),!.

apply_det_rules(M,[_|T],Tab0,EA,Tab):-
  apply_det_rules(M,T,Tab0,EA,Tab).


apply_nondet_rules(_,[],Tab,_EA,Tab).

apply_nondet_rules(M,[H|_],Tab0,EA,Tab):-
  %C=..[H,Tab,L],
  call(H,M,Tab0,EA,L),!,
  member(Tab,L),
  dif(Tab0,Tab).

apply_nondet_rules(M,[_|T],Tab0,EA,Tab):-
  apply_nondet_rules(M,T,Tab0,EA,Tab).


test_end_apply_rule(M,_,_):-
  check_time_monitor(M),!.

test_end_apply_rule(_,Tab0,Tab1):-
  same_tableau(Tab0,Tab1).

/*
apply_all_rules(M,Tab0,Tab):-
  apply_nondet_rules([or_rule,max_rule],
                  Tab0,Tab1),
  (Tab0=Tab1 ->
  Tab=Tab1;
  apply_all_rules(M,Tab1,Tab)).

apply_det_rules([],Tab,Tab).
apply_det_rules([H|_],Tab0,Tab):-
  %C=..[H,Tab,Tab1],
  once(call(H,Tab0,Tab)).
apply_det_rules([_|T],Tab0,Tab):-
  apply_det_rules(T,Tab0,Tab).
apply_nondet_rules([],Tab0,Tab):-
  apply_det_rules([o_rule,and_rule,unfold_rule,add_exists_rule,forall_rule,forall_plus_rule,exists_rule,min_rule],Tab0,Tab).
apply_nondet_rules([H|_],Tab0,Tab):-
  %C=..[H,Tab,L],
  once(call(H,Tab0,L)),
  member(Tab,L),
  dif(Tab0,Tab).
apply_nondet_rules([_|T],Tab0,Tab):-
  apply_nondet_rules(T,Tab0,Tab).
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
add_exists_rule(M,Tab0,[R,Ind1,Ind2],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(C,Ind2,Expl2,ABox),
  (unifiable(C,someValuesFrom(_,_),_)->false;
  ( %existsInKB(M,R,C),
    add_tableau_rules_from_class(M,someValuesFrom(R,C)),
    findPropertyAssertion(R,Ind1,Ind2,Expl1,ABox),
    and_f(M,Expl1,Expl2,Expl),
    modify_ABox(M,Tab0,someValuesFrom(R,C),Ind1,Expl,Tab)
  )).

add_exists_rule(M,Tab0,[C,Ind2],Tab):-
  (unifiable(C,someValuesFrom(_,_),_)->false;
  ( get_abox(Tab0,ABox),
    findPropertyAssertion(R,Ind1,Ind2,Expl1,ABox),
    %existsInKB(M,R,C),
    add_tableau_rules_from_class(M,someValuesFrom(R,C)),
    findClassAssertion(C,Ind2,Expl2,ABox),
    and_f(M,Expl1,Expl2,Expl),
    modify_ABox(M,Tab0,someValuesFrom(R,C),Ind1,Expl,Tab)
  )).


/*
existsInKB(M,R,C):-
  M:subClassOf(A,B),
  member(someValuesFrom(R,C),[A,B]).

existsInKB(M,R,C):-
  M:equivalentClasses(L),
  member(someValuesFrom(R,C),L).
*/

/* *************** */ 

/*
  and_rule
  =================
*/
and_rule(M,Tab0,[intersectionOf(LC),Ind],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(intersectionOf(LC),Ind,Expl,ABox),
  \+ indirectly_blocked(Ind,Tab0),
  scan_and_list(M,LC,Ind,Expl,Tab0,Tab,0).


%----------------
scan_and_list(_M,[],_Ind,_Expl,Tab,Tab,Mod):-
  dif(Mod,0).

scan_and_list(M,[C|T],Ind,Expl,Tab0,Tab,_Mod):-
  modify_ABox(M,Tab0,C,Ind,Expl,Tab1),!,
  scan_and_list(M,T,Ind,Expl,Tab1,Tab,1).

scan_and_list(M,[_C|T],Ind,Expl,Tab0,Tab,Mod):-
  scan_and_list(M,T,Ind,Expl,Tab0,Tab,Mod).
/* ************* */

/*
  or_rule
  ===============
*/
or_rule(M,Tab0,[unionOf(LC),Ind],L):- 
  get_abox(Tab0,ABox),
  findClassAssertion(unionOf(LC),Ind,Expl,ABox),
  \+ indirectly_blocked(Ind,Tab0),
  %not_ind_intersected_union(Ind,LC,ABox),
  % length(LC,NClasses),
  get_choice_point_id(M,ID),
  scan_or_list(M,LC,0,ID,Ind,Expl,Tab0,L),
  dif(L,[]),
  create_choice_point(M,Ind,or,unionOf(LC),LC,_),!. % last variable whould be equals to ID

not_ind_intersected_union(Ind,LC,ABox):-
  \+ ind_intersected_union(Ind,LC,ABox).

ind_intersected_union(Ind,LC,ABox) :-
  member(C,LC),
  findClassAssertion(C,Ind,_,ABox),!.
%---------------
scan_or_list(_,[],_,_,_,_,_,[]):- !.

scan_or_list(M,[C|T],N0,CP,Ind,Expl0,Tab0,[Tab|L]):-
  add_choice_point(M,cpp(CP,N0),Expl0,Expl),
  modify_ABox(M,Tab0,C,Ind,Expl,Tab),
  N is N0 + 1,
  scan_or_list(M,T,N,CP,Ind,Expl0,Tab0,L).

/* **************** */

/*
  exists_rule
  ==================
*/
exists_rule(M,Tab0,[someValuesFrom(R,C),Ind1],Tab):-
  get_abox(Tab0,ABox0),
  findClassAssertion(someValuesFrom(R,C),Ind1,Expl,ABox0),
  \+ blocked(Ind1,Tab0),
  \+ connected_individual(R,C,Ind1,ABox0),
  new_ind(M,Ind2),
  add_edge(R,Ind1,Ind2,Tab0,Tab1),
  add_owlThing_ind(M,Tab1,Ind2,Tab2),
  modify_ABox(M,Tab2,C,Ind2,Expl,Tab3),
  modify_ABox(M,Tab3,R,Ind1,Ind2,Expl,Tab).



%---------------
connected_individual(R,C,Ind1,ABox):-
  findPropertyAssertion(R,Ind1,Ind2,_,ABox),
  findClassAssertion(C,Ind2,_,ABox).

/* ************ */

/*
  forall_rule
  ===================
*/
forall_rule(M,Tab0,[allValuesFrom(R,C),Ind1],Tab):-
  get_abox(Tab0,ABox),
  findPropertyAssertion(R,Ind1,Ind2,Expl2,ABox),
  \+ indirectly_blocked(Ind1,Tab0),
  findClassAssertion(allValuesFrom(R,C),Ind1,Expl1,ABox),
  and_f(M,Expl1,Expl2,Expl),
  modify_ABox(M,Tab0,C,Ind2,Expl,Tab).

forall_rule(M,Tab0,[R,Ind1,Ind2],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(allValuesFrom(R,C),Ind1,Expl1,ABox),
  \+ indirectly_blocked(Ind1,Tab0),
  findPropertyAssertion(R,Ind1,Ind2,Expl2,ABox),
  and_f(M,Expl1,Expl2,Expl),
  modify_ABox(M,Tab0,C,Ind2,Expl,Tab).

/* ************** */

/*
  forall_plus_rule
  =================
*/
forall_plus_rule(M,Tab0,[allValuesFrom(S,C),Ind1],Tab):-
  get_abox(Tab0,ABox),
  findPropertyAssertion(R,Ind1,Ind2,Expl2,ABox),
  find_sub_sup_trans_role(M,R,S,Expl3),
  findClassAssertion(allValuesFrom(S,C),Ind1,Expl1,ABox),
  \+ indirectly_blocked(Ind1,Tab0),
  and_f(M,Expl1,Expl2,ExplT),
  and_f(M,ExplT,Expl3,Expl),
  modify_ABox(M,Tab0,allValuesFrom(R,C),Ind2,Expl,Tab).

forall_plus_rule(M,Tab0,[R,Ind1,Ind2],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(allValuesFrom(S,C),Ind1,Expl1,ABox),
  \+ indirectly_blocked(Ind1,Tab0),
  findPropertyAssertion(R,Ind1,Ind2,Expl2,ABox),
  find_sub_sup_trans_role(M,R,S,Expl3),
  and_f(M,Expl1,Expl2,ExplT),
  and_f(M,ExplT,Expl3,Expl),
  modify_ABox(M,Tab0,allValuesFrom(R,C),Ind2,Expl,Tab).

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

unfold_rule(M,Tab0,[C,Ind],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(C,Ind,Expl,ABox),
  find_sub_sup_class(M,C,D,Ax),
  and_f_ax(M,Ax,Expl,AxL),
  modify_ABox(M,Tab0,D,Ind,AxL,Tab1),
  add_nominal(M,D,Ind,Tab1,Tab).

/* -- unfold_rule
      takes a class C1 in which Ind belongs, find a not atomic class C
      that contains C1 (C is seen as list of classes), controls if
      the individual Ind belongs to all those classes, if yes it finds a class D (if exists)
      that is the superclass or an equivalent class of C and adds D to label of Ind
      - for managing tableau with more than one clash -
      correspond to the ce_rule
   --
*/
unfold_rule(M,Tab0,[C1,Ind],Tab):-
  find_not_atomic(M,C1,C,L),
  get_abox(Tab0,ABox),
  ( C = unionOf(_) -> findClassAssertion(C1,Ind,Expl,ABox)
   ; find_all(M,Ind,L,ABox,Expl)),
  %find_sub_sup_class(M,C,D,Ax),
  %and_f_ax(M,Ax,Expl1,AxL1),
  modify_ABox(M,Tab0,C,Ind,Expl,Tab1),
  add_nominal(M,C,Ind,Tab1,Tab).

/* -- unfold_rule
 *    control propertyRange e propertyDomain
 * --
 */
unfold_rule(M,Tab0,[P,S,O],Tab):-
  get_abox(Tab0,ABox),
  find_class_prop_range_domain(M,P,S,O,Ind,D,Expl,ABox),
  modify_ABox(M,Tab0,D,Ind,Expl,Tab1),
  add_nominal(M,D,Ind,Tab1,Tab).

/*
 * -- unfold_rule
 *    manage the negation
 * --
 */
unfold_rule(M,Tab0,[complementOf(C),Ind],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(complementOf(C),Ind,Expl,ABox),
  find_neg_class(C,D),
  and_f_ax(M,complementOf(C),Expl,AxL),
  modify_ABox(M,Tab0,D,Ind,AxL,Tab1),
  add_nominal(M,D,Ind,Tab1,Tab).

% ----------------
% add_nominal

add_nominal(M,D,Ind,Tab0,Tab):-
  get_abox(Tab0,ABox0),
  ((D = oneOf(_),
    \+ member(nominal(Ind),ABox0))
    ->
   (
     ABox1 = [nominal(Ind)|ABox0],
     (member((classAssertion('http://www.w3.org/2002/07/owl#Thing',Ind),_E),ABox1)
     ->
     set_abox(Tab0,ABox1,Tab)
     ;
     (empty_expl(M,Expl),set_abox(Tab0,[(classAssertion('http://www.w3.org/2002/07/owl#Thing',Ind),Expl)|ABox1],Tab))
     )
   )
    ;
  set_abox(Tab0,ABox0,Tab)
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

find_class_prop_range_domain(M,P,S,O,O,D,Expl,ABox):-
  findPropertyAssertion(P,S,O,ExplPA,ABox),
  %ind_as_list(IndL,L),
  %member(Ind,L),
  M:propertyRange(P,D),
  and_f_ax(M,propertyRange(P,D),ExplPA,Expl).

find_class_prop_range_domain(M,P,S,O,S,D,Expl,ABox):-
  findPropertyAssertion(P,S,O,ExplPA,ABox),
  %ind_as_list(IndL,L),
  %member(Ind,L),
  M:propertyDomain(P,D),
  and_f_ax(M,propertyDomain(P,D),ExplPA,Expl).


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
find_not_atomic(M,C,Ax,LC):-
  M:subClassOf(A,B),
  find_not_atomic_int(C,[A,B],Ax,LC).

find_not_atomic(M,C,Ax,LC):-
  M:equivalentClasses(L),
  find_not_atomic_int(C,L,Ax,LC).

/*
find_not_atomic(M,C,unionOf(L1),L1):-
  M:subClassOf(A,B),
  member(unionOf(L1),[A,B]),
  member(C,L1).

find_not_atomic(M,C,unionOf(L1),L1):-
  M:equivalentClasses(L),
  member(unionOf(L1),L),
  member(C,L1).
*/

find_not_atomic_int(C,LC0,intersectionOf(L1),L1):-
  member(intersectionOf(L1),LC0),
  member(C,L1).

find_not_atomic_int(C,LC0,Ax,LC):-
  member(intersectionOf(L1),LC0),
  find_not_atomic_int(C,L1,Ax,LC).

find_not_atomic_int(C,LC0,unionOf(L1),L1):-
  member(unionOf(L1),LC0),
  member(C,L1).

find_not_atomic_int(C,LC0,Ax,LC):-
  member(unionOf(L1),LC0),
  find_not_atomic_int(C,L1,Ax,LC).




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
unfold_rule(M,Tab0,[C,Ind1,Ind2],Tab):-
  get_abox(Tab0,ABox),
  findPropertyAssertion(C,Ind1,Ind2,Expl,ABox),
  find_sub_sup_property(M,C,D,Ax),
  and_f_ax(M,Ax,Expl,AxL),
  modify_ABox(M,Tab0,D,Ind1,Ind2,AxL,Tab1),
  add_nominal(M,D,Ind1,Tab1,Tab2),
  add_nominal(M,D,Ind2,Tab2,Tab).

%inverseProperties
unfold_rule(M,Tab0,[C,Ind1,Ind2],Tab):-
  get_abox(Tab0,ABox),
  findPropertyAssertion(C,Ind1,Ind2,Expl,ABox),
  find_inverse_property(M,C,D,Ax),
  and_f_ax(M,Ax,Expl,AxL),
  modify_ABox(M,Tab0,D,Ind2,Ind1,AxL,Tab1),
  add_nominal(M,D,Ind1,Tab1,Tab2),
  add_nominal(M,D,Ind2,Tab2,Tab).

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
ce_rule(M,Tab0,Tab):-
  get_tabs(Tab0,(T,_,_)),
  find_not_sub_sup_class(M,Ax,UnAx),
  vertices(T,Inds),
  apply_ce_to(M,Inds,Ax,UnAx,Tab0,Tab,C),
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
apply_ce_to(_M,[],_,_,Tab,Tab,0).

apply_ce_to(M,[Ind|T],Ax,UnAx,Tab0,Tab,C):-
  \+ indirectly_blocked(Ind,Tab),
  modify_ABox(M,Tab0,UnAx,Ind,[Ax],Tab1),!,
  apply_ce_to(M,T,Ax,UnAx,Tab1,Tab,C0),
  C is C0 + 1.

apply_ce_to(M,[_Ind|T],Ax,UnAx,Tab0,Tab,C):-
  apply_ce_to(M,T,Ax,UnAx,Tab0,Tab,C).

/* **************** */

/*
  min_rule
  =============
*/
min_rule(M,Tab0,[minCardinality(N,S),Ind1],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(minCardinality(N,S),Ind1,Expl,ABox),
  \+ blocked(Ind1,Tab0),
  s_neighbours(M,Ind1,S,Tab0,SN),
  safe_s_neigh(SN,S,Tab0,SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh(M,NoI,S,Ind1,Expl,NI,Tab0,Tab1),
  modify_ABox(M,Tab1,differentIndividuals(NI),Expl,Tab).


min_rule(M,Tab0,[minCardinality(N,S,C),Ind1],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(minCardinality(N,S,C),Ind1,Expl,ABox),
  \+ blocked(Ind1,Tab0),
  s_neighbours(M,Ind1,S,Tab0,SN),
  safe_s_neigh_C(SN,S,C,Tab0,ABox,SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh_C(M,NoI,S,C,Ind1,Expl,NI,Tab0,Tab1),
  modify_ABox(M,Tab1,differentIndividuals(NI),Expl,Tab).

min_rule(M,Tab0,[exactCardinality(N,S),Ind1],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(exactCardinality(N,S),Ind1,Expl,ABox),
  \+ blocked(Ind1,Tab0),
  s_neighbours(M,Ind1,S,Tab0,SN),
  safe_s_neigh(SN,S,Tab0,SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh(M,NoI,S,Ind1,Expl,NI,Tab0,Tab1),
  modify_ABox(M,Tab1,differentIndividuals(NI),Expl,Tab).


min_rule(M,Tab0,[exactCardinality(N,S,C),Ind1],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(exactCardinality(N,S,C),Ind1,Expl,ABox),
  \+ blocked(Ind1,Tab0),
  s_neighbours(M,Ind1,S,Tab0,SN),
  safe_s_neigh_C(SN,S,C,Tab0,SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh_C(M,NoI,S,C,Ind1,Expl,NI,Tab0,Tab1),
  modify_ABox(M,Tab1,differentIndividuals(NI),Expl,Tab).

min_rule(M,(ABox,Tabs),([(differentIndividuals(NI),Expl)|ABox1],Tabs1)):-
  findClassAssertion(exactCardinality(N,S),Ind1,Expl,ABox),
  \+ blocked(Ind1,(ABox,Tabs)),
  s_neighbours(M,Ind1,S,(ABox,Tabs),SN),
  safe_s_neigh(SN,S,(ABox,Tabs),SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh(M,NoI,S,Ind1,Expl,NI,ABox,Tabs,ABox1,Tabs1).


min_rule(M,(ABox,Tabs),([(differentIndividuals(NI),Expl)|ABox1],Tabs1)):-
  findClassAssertion(exactCardinality(N,S,C),Ind1,Expl,ABox),
  \+ blocked(Ind1,(ABox,Tabs)),
  s_neighbours(M,Ind1,S,(ABox,Tabs),SN),
  safe_s_neigh(SN,S,(ABox,Tabs),SS),
  length(SS,LSS),
  LSS @< N,
  NoI is N-LSS,
  min_rule_neigh_C(M,NoI,S,C,Ind1,Expl,NI,ABox,Tabs,ABox1,Tabs1).

% ----------------------
min_rule_neigh(_,0,_,_,_,[],Tab,Tab).

min_rule_neigh(M,N,S,Ind1,Expl,[Ind2|NI],Tab0,Tab):-
  N > 0,
  NoI is N-1,
  new_ind(M,Ind2),
  add_edge(S,Ind1,Ind2,Tab0,Tab1),
  add_owlThing_ind(M,Tab1,Ind2,Tab2),
  modify_ABox(M,Tab2,S,Ind1,Ind2,Expl,Tab3),
  %check_block(Ind2,Tab3),
  min_rule_neigh(M,NoI,S,Ind1,Expl,NI,Tab3,Tab).

%----------------------
min_rule_neigh_C(_,0,_,_,_,_,[],Tab,Tab).

min_rule_neigh_C(M,N,S,C,Ind1,Expl,[Ind2|NI],Tab0,Tab):-
  N > 0,
  NoI is N-1,
  new_ind(M,Ind2),
  add_edge(S,Ind1,Ind2,Tab0,Tab1),
  add_owlThing_ind(M,Tab1,Ind2,Tab2),
  modify_ABox(M,Tab2,S,Ind1,Ind2,Expl,Tab3),
  modify_ABox(M,Tab3,C,Ind2,[propertyAssertion(S,Ind1,Ind2)|Expl],Tab4),
  %check_block(Ind2,Tab4),
  min_rule_neigh_C(M,NoI,S,C,Ind1,Expl,NI,Tab4,Tab).

%---------------------
safe_s_neigh([],_,_,[]):-!.

safe_s_neigh([H|T],S,Tab,[H|ST]):-
  safe(H,S,Tab),
  safe_s_neigh(T,S,Tab,ST).

safe_s_neigh_C(L,S,C,Tab,LT):-
  get_abox(Tab,ABox),
  safe_s_neigh_C(L,S,C,Tab,ABox,LT).

safe_s_neigh_C([],_,_,_,_,_,[]):-!.

safe_s_neigh_C([H|T],S,C,Tab,ABox,[H|ST]):-
  safe(H,S,Tab),
  findClassAssertion(C,H,_,ABox),!,
  safe_s_neigh_C(T,S,C,Tab,ABox,ST).

/* **************** */

/*
  max_rule
  ================
*/
max_rule(M,Tab0,[maxCardinality(N,S),Ind],L):-
  get_abox(Tab0,ABox),
  findClassAssertion(maxCardinality(N,S),Ind,Expl0,ABox),
  \+ indirectly_blocked(Ind,Tab0),
  s_neighbours(M,Ind,S,Tab0,SN),
  length(SN,LSS),
  LSS @> N,
  get_choice_point_id(M,ID),
  scan_max_list(M,maxCardinality(N,S),S,'http://www.w3.org/2002/07/owl#Thing',SN,ID,Ind,Expl0,Tab0,ABox,L),!. 

max_rule(M,Tab0,[maxCardinality(N,S,C),Ind],L):-
  get_abox(Tab0,ABox),
  findClassAssertion(maxCardinality(N,S,C),Ind,Expl0,ABox),
  \+ indirectly_blocked(Ind,Tab0),
  s_neighbours(M,Ind,S,Tab0,SN),
  individual_class_C(SN,C,ABox,SNC),
  length(SNC,LSS),
  LSS @> N,
  get_choice_point_id(M,ID),%gtrace,
  scan_max_list(M,maxCardinality(N,S,C),S,C,SNC,ID,Ind,Expl0,Tab0,ABox,L),!. % last variable whould be equals to ID

%---------------------

max_rule(M,Tab0,[exactCardinality(N,S),Ind],L):-
  get_abox(Tab0,ABox),
  findClassAssertion(exactCardinality(N,S),Ind,Expl0,ABox),
  \+ indirectly_blocked(Ind,Tab0),
  s_neighbours(M,Ind,S,Tab0,SN),
  length(SN,LSS),
  LSS @> N,
  get_choice_point_id(M,ID),
  scan_max_list(M,exactCardinality(N,S),S,'http://www.w3.org/2002/07/owl#Thing',SN,ID,Ind,Expl0,Tab0,ABox,L),!. 

max_rule(M,Tab0,[exactCardinality(N,S,C),Ind],L):-
  get_abox(Tab0,ABox),
  findClassAssertion(exactCardinality(N,S,C),Ind,Expl0,ABox),
  \+ indirectly_blocked(Ind,Tab0),
  s_neighbours(M,Ind,S,Tab0,SN),
  individual_class_C(SN,C,ABox,SNC),
  length(SNC,LSS),
  LSS @> N,
  get_choice_point_id(M,ID),%gtrace,
  scan_max_list(M,exactCardinality(N,S,C),S,C,SNC,ID,Ind,Expl0,Tab0,ABox,L),!. % last variable whould be equals to ID

  max_rule(M,Tab0,[S,Ind,_],L):-
    get_abox(Tab0,ABox),
    findClassAssertion(exactCardinality(N,S),Ind,Expl0,ABox),
    \+ indirectly_blocked(Ind,Tab0),
    s_neighbours(M,Ind,S,Tab0,SN),
    length(SN,LSS),
    LSS @> N,
    get_choice_point_id(M,ID),
    scan_max_list(M,exactCardinality(N,S),S,'http://www.w3.org/2002/07/owl#Thing',SN,ID,Ind,Expl0,Tab0,ABox,L),!. 
  
  max_rule(M,Tab0,[S,Ind,_],L):-
    get_abox(Tab0,ABox),
    findClassAssertion(exactCardinality(N,S,C),Ind,Expl0,ABox),
    \+ indirectly_blocked(Ind,Tab0),
    s_neighbours(M,Ind,S,Tab0,SN),
    individual_class_C(SN,C,ABox,SNC),
    length(SNC,LSS),
    LSS @> N,
    get_choice_point_id(M,ID),%gtrace,
    scan_max_list(M,exactCardinality(N,S,C),S,C,SNC,ID,Ind,Expl0,Tab0,ABox,L),!. % last variable whould be equals to ID

%---------------------

scan_max_list(M,MaxCardClass,S,C,SN,CP,Ind,Expl,Tab0,ABox,Tab_list):-
  create_couples_for_merge(SN,[],Ind_couples), % MAYBE check_individuals_not_equal(M,YI,YJ,ABox), instead of dif
  length(Ind_couples,NChoices),
  (
    NChoices @> 1 -> (FirstChoice = -1) ; (FirstChoice = 0)
  ),
  create_list_for_max_rule(M,Ind_couples,FirstChoice,CP,Ind,S,C,Expl,Tab0,ABox,Tab_list),
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

create_list_for_max_rule(M,[YI-YJ|Ind_couples],N0,CP,Ind,S,C,Expl0,Tab0,ABox,[Tab|Tab_list]):-
  findPropertyAssertion(S,Ind,YI,ExplYI,ABox),
  findPropertyAssertion(S,Ind,YJ,ExplYJ,ABox),
  findClassAssertion(C,YI,ExplCYI,ABox),
  findClassAssertion(C,YJ,ExplCYJ,ABox),
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
  merge_all_individuals(M,[(sameIndividual(LI),ExplT)],Tab0,Tab),
  create_list_for_max_rule(M,Ind_couples,N,CP,Ind,S,C,Expl0,Tab0,ABox,Tab_list).

/*
scan_max_list(M,S,SN,CP,Ind,Expl,ABox0,Tabs0,YI-YJ,ABox,Tabs):-
  member(YI,SN),
  member(YJ,SN),
  % generate cp
  check_individuals_not_equal(M,YI,YJ,ABox0),
  findPropertyAssertion(S,Ind,YI,ExplYI,ABox0),
  findPropertyAssertion(S,Ind,YJ,ExplYJ,ABox0),
  and_f(M,ExplYI,ExplYJ,Expl0),
  and_f(M,Expl0,Expl,ExplT0),
  add_choice_point(M,cpp(CP,N0),ExplT0,ExplT),
  merge_all_individuals(M,[(sameIndividual([YI,YJ]),ExplT)],ABox0,Tabs0,ABox,Tabs).
*/

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
  ch_rule
  ================
*/
ch_rule(M,Tab0,[maxCardinality(N,S,C),Ind1],L):-
  get_abox(Tab0,ABox),
  findPropertyAssertion(S,Ind1,Ind2,Expl2,ABox),
  \+ indirectly_blocked(Ind1,Tab0),
  findClassAssertion(maxCardinality(N,S,C),Ind1,Expl1,ABox),
  absent_c_not_c(Ind2,C,ABox),
  and_f(M,Expl1,Expl2,Expl),
  get_choice_point_id(M,ID),%gtrace,
  neg_class(C,NC),
  scan_or_list(M,[C,NC],0,ID,Ind2,Expl,Tab0,L),
  dif(L,[]),
  create_choice_point(M,Ind2,ch,maxCardinality(N,S,C),[C,NC],_),!. % last variable whould be equals to ID

ch_rule(M,Tab0,[exactCardinality(N,S,C),Ind1],L):-
  get_abox(Tab0,ABox),
  findPropertyAssertion(S,Ind1,Ind2,Expl2,ABox),
  \+ indirectly_blocked(Ind1,Tab0),
  findClassAssertion(exactCardinality(N,S,C),Ind1,Expl1,ABox),
  absent_c_not_c(Ind2,C,ABox),
  and_f(M,Expl1,Expl2,Expl),
  get_choice_point_id(M,ID),%gtrace,
  neg_class(C,NC),
  scan_or_list(M,[C,NC],0,ID,Ind2,Expl,Tab0,L),
  dif(L,[]),
  create_choice_point(M,Ind2,ch,exactCardinality(N,S,C),[C,NC],_),!. % last variable whould be equals to ID

  ch_rule(M,Tab0,[S,Ind1,Ind2],L):-
    get_abox(Tab0,ABox),
    findClassAssertion(maxCardinality(N,S,C),Ind1,Expl1,ABox),
    \+ indirectly_blocked(Ind1,Tab0),
    findPropertyAssertion(S,Ind1,Ind2,Expl2,ABox),
    absent_c_not_c(Ind2,C,ABox),
    and_f(M,Expl1,Expl2,Expl),
    get_choice_point_id(M,ID),%gtrace,
    neg_class(C,NC),
    scan_or_list(M,[C,NC],0,ID,Ind2,Expl,Tab0,L),
    dif(L,[]),
    create_choice_point(M,Ind2,ch,maxCardinality(N,S,C),[C,NC],_),!. % last variable whould be equals to ID
  
  ch_rule(M,Tab0,[S,Ind1,Ind2],L):-
    get_abox(Tab0,ABox),
    findClassAssertion(exactCardinality(N,S,C),Ind1,Expl1,ABox),
    \+ indirectly_blocked(Ind1,Tab0),
    findPropertyAssertion(S,Ind1,Ind2,Expl2,ABox),
    absent_c_not_c(Ind2,C,ABox),
    and_f(M,Expl1,Expl2,Expl),
    get_choice_point_id(M,ID),%gtrace,
    neg_class(C,NC),
    scan_or_list(M,[C,NC],0,ID,Ind2,Expl,Tab0,L),
    dif(L,[]),
    create_choice_point(M,Ind2,ch,exactCardinality(N,S,C),[C,NC],_),!. % last variable whould be equals to ID

%---------------------

absent_c_not_c(Ind,C,ABox) :-
  \+ is_there_c_not_c(Ind,C,ABox).

is_there_c_not_c(Ind,C,ABox) :-
 findClassAssertion(C,Ind,_,ABox),!.

is_there_c_not_c(Ind,C,ABox) :-
  neg_class(C,NC),
  findClassAssertion(NC,Ind,_,ABox),!.

/* *************** */

/*
 o_rule
 ============
*/

o_rule(M,Tab0,[oneOf(C),X],Tab):-
  get_abox(Tab0,ABox),
  findClassAssertion(oneOf(C),X,ExplX,ABox),
  findClassAssertion(oneOf(D),Y,ExplY,ABox),
  containsCommon(C,D),
  dif(X,Y),
  notDifferentIndividuals(M,X,Y,ABox),
  nominal(C,Tab),
  ind_as_list(X,LX),
  ind_as_list(Y,LY),
  and_f(M,ExplX,ExplY,ExplC),
  merge(M,X,Y,ExplC,Tab0,Tab),
  flatten([LX,LY],LI0),
  sort(LI0,LI),
  absent(sameIndividual(LI),ExplC,ABox).

containsCommon(L1,L2):-
  member(X,L1),
  member(X,L2),!.
% -------------------

/* ************* */

/***********
  utility for sameIndividual
************/

ind_as_list(sameIndividual(L),L):-
  retract_sameIndividual(L),!.

ind_as_list(X,[X]):-
  atomic(X).

list_as_sameIndividual(L0,sameIndividual(L)):-
  list_as_sameIndividual_int(L0,L1),
  sort(L1,L).

list_as_sameIndividual_int([],[]).

list_as_sameIndividual_int([sameIndividual(L0)|T0],L):-
  !,
  append(L0,T0,L1),
  list_as_sameIndividual_int(L1,L).

list_as_sameIndividual_int([H|T0],[H|T]):-
  list_as_sameIndividual_int(T0,T).


find_same(H,ABox,sameIndividual(L),Expl):-
  find((sameIndividual(L),Expl),ABox),
  member(X,L),
  member(X,H),!.

find_same(_H,_ABox,[],[]).


/*
 retract_sameIndividual(L)
 ==========
*/
retract_sameIndividual(sameIndividual(L)):-
  !,
  retract_sameIndividual(L).

retract_sameIndividual(L):-
  get_trill_current_module(N),
  retract(N:sameIndividual(L)).

retract_sameIndividual(L):-
  get_trill_current_module(N),
  \+ retract(N:sameIndividual(L)).
/* ****** */

/*
 * nominal/2, blockable/2, blocked/2, indirectly_blocked/2 and safe/3
 *
 */

nominal(Inds,Tab):-
  get_abox(Tab,ABox),
  find((nominal(Ind)),ABox),
  member(Ind,Inds).

% ----------------

blockable(Ind,Tab):-
  get_abox(Tab,ABox),
  ( find((nominal(Ind)),ABox)
    ->
    false
    ;
    true ).

% ---------------

blocked(Ind,Tab):-
  check_block(Ind,Tab).

/*

  control for blocking an individual

*/

check_block(Ind,Tab):-
  blockable(Ind,Tab),
  get_tabs(Tab,(T,_,_)),
  transpose_ugraph(T,T1),
  ancestor(Ind,T,A),
  neighbours(Ind,T1,N),
  check_block1(Ind,A,N,Tab),!.

check_block(Ind,Tab):-
  blockable(Ind,Tab),
  get_tabs(Tab,(T,_,_)),
  transpose_ugraph(T,T1),
  neighbours(Ind,T1,N),
  check_block2(N,Tab),!.


check_block1(Indx,A,N,Tab):-
  member(Indx0,N),
  member(Indy,A),
  member(Indy0,A),
  get_tabs(Tab,(T,RBN,_)),
  neighbours(Indy,T,N2),
  member(Indy0,N2),
  rb_lookup((Indx0,Indx),V,RBN),
  rb_lookup((Indy0,Indy),V2,RBN),
  member(R,V),
  member(R,V2),
  get_abox(Tab,ABox),
  same_label(Indx,Indy0,ABox),
  same_label(Indx0,Indy,ABox),
  all_node_blockable(Indx0,Indy0,Tab),!.

%check_block2([],_).

check_block2([H|Tail],Tab):-
  blocked(H,Tab),
  check_block2(Tail,Tab).

%---------------
indirectly_blocked(Ind,Tab):-
  get_tabs(Tab,(T,_RBN,_RBR)),
  transpose_ugraph(T,T1),
  neighbours(Ind,T1,N),
  member(A,N),
  blocked(A,Tab),!.

%---------------------
/*
  An R-neighbour y of a node x is safe if
  (i)  x is blockable or
  (ii) x is a nominal node and y is not blocked.
*/

safe(Ind,R,Tab):-
  get_tabs(Tab,(_,_,RBR)),
  rb_lookup(R,V,RBR),
  get_parent(X,Ind,V),
  blockable(X,Tab),!.

safe(Ind,R,Tab):-
  get_tabs(Tab,(_,_,RBR)),
  rb_lookup(R,V,RBR),
  get_parent(X,Ind,V),
  nominal(X,Tab),!,
  \+ blocked(Ind,Tab).

get_parent(X,Ind,[(X,Ind)|_T]):-!.

get_parent(X,Ind,[(X,LI)|_T]):-
  is_list(LI),
  member(Ind,LI),!.

get_parent(X,Ind,[_|T]):-
  get_parent(X,Ind,T).


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
writeABox(Tab):-
  get_abox(Tab,ABox),
  writel(ABox).


/*
  build_abox
  ===============
*/

%---------------
subProp(M,SubProperty,Property,Subject,Object):-
  M:subPropertyOf(SubProperty,Property),M:propertyAssertion(SubProperty,Subject,Object).

%--------------

add_owlThing_ind(M,Tab0,Ind,Tab):-
  prepare_nom_list(M,[Ind],NomListOut),
  add_all_to_tableau(M,NomListOut,Tab0,Tab).

add_owlThing_list(M,Tab0,Tab):- % TODO
  get_tabs(Tab0,(T,_,_)),
  vertices(T,NomListIn),
  prepare_nom_list(M,NomListIn,NomListOut),
  add_all_to_tableau(M,NomListOut,Tab0,Tab).

%--------------

prepare_nom_list(_,[],[]).

prepare_nom_list(M,[literal(_)|T],T1):-!,
  prepare_nom_list(M,T,T1).

prepare_nom_list(M,[H|T],[(classAssertion('http://www.w3.org/2002/07/owl#Thing',H),Expl)|T1]):-
  initial_expl(M,Expl),
  prepare_nom_list(M,T,T1).
%--------------


/* merge nodes in (ABox,Tabs) */

merge_all_individuals(_,[],Tab,Tab).

merge_all_individuals(M,[(sameIndividual(H),Expl)|T],Tab0,Tab):-
  get_abox(Tab0,ABox0),
  find_same(H,ABox0,L,ExplL),
  dif(L,[]),!,
  merge_all1(M,H,Expl,L,Tab0,Tab1),
  list_as_sameIndividual([H,L],SI), %TODO
  %flatten([H,L],L0),
  %sort(L0,SI),
  and_f(M,Expl,ExplL,ExplT),
  add_to_tableau(Tab1,(SI,ExplT),Tab2),
  remove_from_tableau(Tab2,(sameIndividual(L),ExplL),Tab3),
  retract_sameIndividual(L),
  merge_all_individuals(M,T,Tab3,Tab).

merge_all_individuals(M,[(sameIndividual(H),Expl)|T],Tab0,Tab):-
  %get_abox(Tab0,ABox0),
  %find_same(H,ABox0,L,_),
  %L==[],!,
  merge_all2(M,H,Expl,Tab0,Tab1),
  add_to_tableau(Tab1,(sameIndividual(H),Expl),Tab2),
  merge_all_individuals(M,T,Tab2,Tab).

merge_all1(_M,[],_,_,Tab,Tab).

merge_all1(M,[H|T],Expl,L,Tab0,Tab):-
  \+ member(H,L),
  merge(M,H,L,Expl,Tab0,Tab1),
  merge_all1(M,T,Expl,[H|L],Tab1,Tab).

merge_all1(M,[H|T],Expl,L,Tab0,Tab):-
  member(H,L),
  merge_all1(M,T,Expl,L,Tab0,Tab).



merge_all2(M,[X,Y|T],Expl,Tab0,Tab):-
  merge(M,X,Y,Expl,Tab0,Tab1),
  merge_all1(M,T,Expl,[X,Y],Tab1,Tab).



/*
  creation of the query anon individual

*/
query_ind(trillan(0)).

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

all_node_blockable(X,Y,Tab):-
  get_tabs(Tab,(T,_,_)),
  graph_min_path(X,Y,T,P),
  all_node_blockable1(P,Tab).

all_node_blockable1([],_).

all_node_blockable1([H|Tail],Tab):-
  blockable(H,Tab),
  all_node_blockable1(Tail,Tab).

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
  find all S neighbours (S is a role)
*/
s_neighbours(M,Ind1,S,Tab,SN):- %gtrace,
  get_tabs(Tab,(_,_,RBR)),
  rb_lookup(S,VN,RBR),!,
  s_neighbours1(Ind1,VN,SN0),
  flatten(SN0,SN1),
  get_abox(Tab,ABox),
  s_neighbours2(M,SN1,SN1,SN,ABox),!.

s_neighbours(_,_Ind1,_S,_Tab,[]). % :-
%  get_tabs(Tab,(_,_,RBR)),
%  \+ rb_lookup(S,_VN,RBR).

s_neighbours1(_,[],[]).

s_neighbours1(Ind1,[(Ind1,Y)|T],[Y|T1]):-
  s_neighbours1(Ind1,T,T1).

s_neighbours1(Ind1,[(X,_Y)|T],T1):-
  dif(X,Ind1),
  s_neighbours1(Ind1,T,T1).
  
s_neighbours2(_,_,[],[],_).

s_neighbours2(M,SN,[H|T],[H|T1],ABox):-
  s_neighbours2(M,SN,T,T1,ABox),
  not_same_ind(M,T1,H,ABox),!.

s_neighbours2(M,SN,[_H|T],T1,ABox):-
  s_neighbours2(M,SN,T,T1,ABox).
  %same_ind(M,T1,H,ABox).


%-----------------

not_same_ind(M,SN,H,_ABox):-
  M:differentIndividuals(SI),
  member(H,SI),
  member(H2,SI),
  member(H2,SN),
  dif(H,H2),!.

not_same_ind(_,SN,H,ABox):-
  find((differentIndividuals(SI),_),ABox),
  member(H,SI),
  member(H2,SI),
  member(H2,SN),
  dif(H,H2),!.

not_same_ind(M,SN,H,ABox):-
  \+ same_ind(M,SN,H,ABox),!.

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

s_predecessors(M,Ind1,S,Tab,SN):-
  get_tabs(Tab,(_,_,RBR)),
  rb_lookup(S,VN,RBR),
  s_predecessors1(Ind1,VN,SN1),
  get_abox(Tab,ABox),
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
  %findall(1,M:annotationAssertion('http://ml.unife.it/disponte#probability',_,_),NAnnAss),length(NAnnAss,NV),
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
  findall(ProbA,(M:annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',Ax,literal(ProbAT)),atom_number(ProbAT,ProbA)),ProbsOld), % Retro-compatibility
  findall(ProbA,(M:annotationAssertion('http://ml.unife.it/disponte#probability',Ax,literal(ProbAT)),atom_number(ProbAT,ProbA)),ProbsNew),
  append(ProbsNew, ProbsOld, Probs),
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
  utility_translation:set_up_kb_loading(M),
  trill:add_kb_prefixes(M:[('disponte'='http://ml.unife.it/disponte#'),('owl'='http://www.w3.org/2002/07/owl#')]).

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





% ==========================================================================================================
% TABLEAU MANAGER
% ==========================================================================================================

% ======================================================================
% As Dict
% ======================================================================

/* getters and setters for Tableau */

get_abox(Tab,ABox):-
  ABox = Tab.abox.

set_abox(Tab0,ABox,Tab):-
  Tab = Tab0.put(abox,ABox).

get_tabs(Tab,Tabs):-
  Tabs = Tab.tabs.

set_tabs(Tab0,Tabs,Tab):-
  Tab = Tab0.put(tabs,Tabs).

get_clashes(Tab,Clashes):-
  Clashes = Tab.clashes.

set_clashes(Tab0,Clashes,Tab):-
  Tab = Tab0.put(clashes,Clashes).

absence_of_clashes(Tab):-
  get_clashes(Tab,Clashes),
  Clashes=[].

get_expansion_queue(Tab,ExpansionQueue):-
  ExpansionQueue = Tab.expq.

set_expansion_queue(Tab0,ExpansionQueue,Tab):-
  Tab = Tab0.put(expq,ExpansionQueue).

extract_from_expansion_queue(Tab0,EA,Tab):-
  get_expansion_queue(Tab0,EQ0),
  extract_from_expansion_queue_int(EQ0,EA,EQ),
  set_expansion_queue(Tab0,EQ,Tab).

extract_from_expansion_queue_int([[],[EA|T]],EA,[[],T]).

extract_from_expansion_queue_int([[EA|T],NonDetQ],EA,[T,NonDetQ]).

expansion_queue_is_empty(Tab):-
  get_expansion_queue(Tab,EQ),
  empty_expansion_queue(EQ).

empty_expansion_queue([[],[]]).

same_tableau(Tab1,Tab2):-
  get_abox(Tab1,ABox),
  get_abox(Tab2,ABox),
  get_tabs(Tab1,Tabs),
  get_tabs(Tab2,Tabs).

/* initializers */

/**
 * new_tabelau(-EmptyTableaus:dict)
 * 
 * Initialize an empty tableau.
 */
new_tableau(tableau{abox:ABox, tabs:Tabs, clashes:[], expq:ExpansionQueue}):-
  new_abox(ABox),
  new_tabs(Tabs),
  empty_expansion_queue(ExpansionQueue).


/**
 * init_tabelau(+ABox:abox, +Tabs:tableau, -InitializedTableaus:dict)
 * 
 * Initialize a tableau with the lements given in input.
 */
init_tableau(ABox,Tabs,tableau{abox:ABox, tabs:Tabs, clashes:[], expq:ExpansionQueue}):-
  empty_expansion_queue(ExpansionQueue).

/**
 * init_tabelau(+ABox:abox, +Tabs:tableau, +ExpansionQueue:expansion_queue, -InitializedTableaus:dict)
 * 
 * Initialize a tableau with the lements given in input.
 */
init_tableau(ABox,Tabs,ExpansionQueue,tableau{abox:ABox, tabs:Tabs, clashes:[], expq:ExpansionQueue}).


% ======================================================================
% As List (missing Expansion Queue!)
% ======================================================================
/*

get_abox([ABox,_,_],ABox).

set_abox([_,Tabs,C],ABox,[ABox,Tabs,C]).

get_tabs([_,Tabs,_],Tabs).

set_tabs([ABox,_,C],Tabs,[ABox,Tabs,C]).

get_clashes([_,_,Clashes],Clashes).

set_clashes([ABox,Tabs,_],Clashes,[ABox,Tabs,Clashes]).



new_tableau([ABox,Tabs,[]]):-
  new_abox(ABox),
  new_tabs(Tabs).



init_tableau(ABox,Tabs,[ABox,Tabs,[]]).

*/



% ===================================
% ABOX
% ===================================

/* abox as a list */

new_abox([]).

 
/* add El to ABox */
add_to_tableau(Tableau0,El,Tableau):-
  get_abox(Tableau0,ABox0),
  add_to_abox(ABox0,El,ABox),
  set_abox(Tableau0,ABox,Tableau).

remove_from_tableau(Tableau0,El,Tableau):-
  get_abox(Tableau0,ABox0),
  remove_from_abox(ABox0,El,ABox),
  set_abox(Tableau0,ABox,Tableau).

add_clash_to_tableau(M,Tableau0,ToCheck,Tableau):-
  check_clash(M,ToCheck,Tableau0),!,
  get_clashes(Tableau0,Clashes0),
  add_to_clashes(Clashes0,ToCheck,Clashes),
  set_clashes(Tableau0,Clashes,Tableau).

add_clash_to_tableau(_,Tableau,_,Tableau).

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


add_to_abox(ABox,El,[El|ABox]).

remove_from_abox(ABox0,El,ABox):-
  delete(ABox0,El,ABox).


add_to_clashes(Clashes,'http://www.w3.org/2002/07/owl#Nothing'-_,[owlnothing|Clashes]):-!.

add_to_clashes(Clashes,El,[El|Clashes]).

remove_from_abox(Clashes0,El,Clashes):-
  delete(Clashes0,El,Clashes).

/*
  add_all_to_tableau(M,L1,L2,LO).
  add in L2 all item of L1
*/
add_all_to_tableau(M,L,Tableau0,Tableau):-
  get_abox(Tableau0,ABox0),
  get_clashes(Tableau0,Clashes0),
  add_all_to_abox_and_clashes(M,L,Tableau0,ABox0,ABox,Clashes0,Clashes),
  set_abox(Tableau0,ABox,Tableau1),
  set_clashes(Tableau1,Clashes,Tableau).

add_all_to_abox_and_clashes(_,[],_,A,A,C,C).

add_all_to_abox_and_clashes(M,[(classAssertion(C,I),Expl)|T],Tab,A0,A,C0,C):-
  check_clash(M,C-I,Tab),!,
  add_to_abox(A0,(classAssertion(C,I),Expl),A1),
  add_to_clashes(C0,C-I,C1),
  add_all_to_abox_and_clashes(M,T,A1,A,C1,C).

add_all_to_abox_and_clashes(M,[(sameIndividual(LI),Expl)|T],Tab,A0,A,C0,C):-
  check_clash(M,sameIndividual(LI),Tab),!,
  add_to_abox(A0,(sameIndividual(LI),Expl),A1),
  add_to_clashes(C0,sameIndividual(LI),C1),
  add_all_to_abox_and_clashes(M,T,A1,A,C1,C).

add_all_to_abox_and_clashes(M,[(differentIndividuals(LI),Expl)|T],Tab,A0,A,C0,C):-
  check_clash(M,differentIndividuals(LI),Tab),!,
  add_to_abox(A0,(differentIndividuals(LI),Expl),A1),
  add_to_clashes(C0,differentIndividuals(LI),C1),
  add_all_to_abox_and_clashes(M,T,A1,A,C1,C).

add_all_to_abox_and_clashes(M,[H|T],Tab,A0,A,C0,C):-
  add_to_abox(A0,H,A1),
  add_all_to_abox_and_clashes(M,T,Tab,A1,A,C0,C).

add_all_to_abox([],A,A).

add_all_to_abox([H|T],A0,A):-
  add_to_abox(A0,H,A1),
  add_all_to_abox(T,A1,A).

/* ************** */



% ===================================
% EXPANSION QUEUE
% ===================================



% ------------
% Utility for rule application
% ------------
update_expansion_queue_in_tableau(M,C,Ind,Tab0,Tab):-
  get_expansion_queue(Tab0,ExpansionQueue0),
  update_expansion_queue(M,C,Ind,ExpansionQueue0,ExpansionQueue),
  set_expansion_queue(Tab0,ExpansionQueue,Tab).

update_expansion_queue_in_tableau(M,P,Ind1,Ind2,Tab0,Tab):-
  get_expansion_queue(Tab0,ExpansionQueue0),
  update_expansion_queue(M,P,Ind1,Ind2,ExpansionQueue0,ExpansionQueue),
  set_expansion_queue(Tab0,ExpansionQueue,Tab).



update_expansion_queue(_,unionOf(L),Ind,[DQ,NDQ0],[DQ,NDQ]):-!,
  delete(NDQ0,[unionOf(L),Ind],NDQ1),
  append(NDQ1,[[unionOf(L),Ind]],NDQ).

update_expansion_queue(_,maxCardinality(N,S,C),Ind,[DQ,NDQ0],[DQ,NDQ]):-!,
  delete(NDQ0,[maxCardinality(N,S,C),Ind],NDQ1),
  append(NDQ1,[[maxCardinality(N,S,C),Ind]],NDQ).

update_expansion_queue(_,maxCardinality(N,S),Ind,[DQ,NDQ0],[DQ,NDQ]):-!,
  delete(NDQ0,[maxCardinality(N,S),Ind],NDQ1),
  append(NDQ1,[[maxCardinality(N,S),Ind]],NDQ).

update_expansion_queue(_,exactCardinality(N,S,C),Ind,[DQ,NDQ0],[DQ,NDQ]):-!,
  delete(NDQ0,[exactCardinality(N,S,C),Ind],NDQ1),
  append(NDQ1,[[exactCardinality(N,S,C),Ind]],NDQ).

update_expansion_queue(_,exactCardinality(N,S),Ind,[DQ,NDQ0],[DQ,NDQ]):-!,
  delete(NDQ0,[exactCardinality(N,S),Ind],NDQ1),
  append(NDQ1,[[exactCardinality(N,S),Ind]],NDQ).

update_expansion_queue(_,C,Ind,[DQ0,NDQ],[DQ,NDQ]):-!,
  delete(DQ0,[C,Ind],DQ1),
  append(DQ1,[[C,Ind]],DQ).

update_expansion_queue(_,P,Ind1,Ind2,[DQ0,NDQ],[DQ,NDQ]):-!,
  delete(DQ0,[P,Ind1,Ind2],DQ1),
  append(DQ1,[[P,Ind1,Ind2]],DQ).


init_expansion_queue(LCA,LPA,EQ):-
  empty_expansion_queue(EmptyEQ),
  add_classes_expqueue(LCA,EmptyEQ,EQ0),
  add_prop_expqueue(LPA,EQ0,EQ).

add_classes_expqueue([],EQ,EQ).

add_classes_expqueue([(classAssertion(C,I),_)|T],EQ0,EQ):-
  update_expansion_queue(_,C,I,EQ0,EQ1),
  add_classes_expqueue(T,EQ1,EQ).

add_prop_expqueue([],EQ,EQ).

add_prop_expqueue([(propertyAssertion(P,S,O),_)|T],EQ0,EQ):-
  update_expansion_queue(_,P,S,O,EQ0,EQ1),
  add_prop_expqueue(T,EQ1,EQ).




% ===================================
% TABS
% ===================================

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

/*
  adds nodes and edges to tabs given axioms
*/
create_tabs(L,Tableau0,Tableau):-
  get_tabs(Tableau0,Tabs0),
  create_tabs_int(L,Tabs0,Tabs),
  set_tabs(Tableau0,Tabs,Tableau).


create_tabs_int([],G,G).
  
create_tabs_int([(propertyAssertion(P,S,O),_Expl)|T],Tabl0,Tabl):-
  add_edge_int(P,S,O,Tabl0,Tabl1),
  create_tabs_int(T,Tabl1,Tabl).
  
create_tabs_int([(differentIndividuals(Ld),_Expl)|Tail],(T0,RBN,RBR),Tabs):-
  add_vertices(T0,Ld,T1),
  create_tabs_int(Tail,(T1,RBN,RBR),Tabs).

create_tabs_int([(classAssertion(_,I),_Expl)|Tail],(T0,RBN,RBR),Tabs):-
  add_vertices(T0,[I],T1),
  create_tabs_int(Tail,(T1,RBN,RBR),Tabs).


/*
  add edge to tableau

  add_edge(Property,Subject,Object,Tab0,Tab)
*/
add_edge(P,S,O,Tableau0,Tableau):-
  get_tabs(Tableau0,Tabs0),
  add_edge_int(P,S,O,Tabs0,Tabs),
  set_tabs(Tableau0,Tabs,Tableau).

add_edge_int(P,S,O,(T0,ItR0,RtI0),(T1,ItR1,RtI1)):-
  add_node_to_tree(P,S,O,ItR0,ItR1),
  add_role_to_tree(P,S,O,RtI0,RtI1),
  add_edge_to_tabl(P,S,O,T0,T1).

add_node_to_tree(P,S,O,RB0,RB1):-
  rb_lookup((S,O),V,RB0),
  \+ member(P,V),!,
  rb_update(RB0,(S,O),[P|V],RB1).

add_node_to_tree(P,S,O,RB0,RB0):-
  rb_lookup((S,O),V,RB0),
  member(P,V),!.

add_node_to_tree(P,S,O,RB0,RB1):-
  \+ rb_lookup([S,O],_,RB0),!,
  rb_insert(RB0,(S,O),[P],RB1).

add_role_to_tree(P,S,O,RB0,RB1):-
  rb_lookup(P,V,RB0),
  \+ member((S,O),V),!,
  rb_update(RB0,P,[(S,O)|V],RB1).

add_role_to_tree(P,S,O,RB0,RB0):-
  rb_lookup(P,V,RB0),
  member((S,O),V),!.

add_role_to_tree(P,S,O,RB0,RB1):-
  \+ rb_lookup(P,_,RB0),!,
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





% ===================================
% FUNCTIONS ON ABOX AND TABS
% ===================================

/*
 * merge
 * 
 * Implement the Merge operation of the tableau. Merge two individuals
 */
% The first three are needed because T in tabs:(T,RBN,RBR) saves sameIndividuals
% as a list instead of a single individual sameIndividual(L).
% The addition of sameIndividual is made after, during the update of the ABox.
% TODO: it could be improved!
/*
merge(M,sameIndividual(LX),sameIndividual(LY),Expl,Tableau0,Tableau):-
  !,
  get_tabs(Tableau0,Tabs0),
  merge_tabs(L,Y,Tabs0,Tabs),
  get_abox(Tableau0,ABox0),
  merge_abox(M,L,Y,Expl,ABox0,ABox),
  set_tabs(Tableau0,Tabs,Tableau1),
  set_abox(Tableau1,ABox,Tableau).

merge(M,sameIndividual(L),Y,Expl,Tableau0,Tableau):-
  !,
  get_tabs(Tableau0,Tabs0),
  merge_tabs(L,Y,Tabs0,Tabs),
  get_abox(Tableau0,ABox0),
  merge_abox(M,L,Y,Expl,ABox0,ABox),
  set_tabs(Tableau0,Tabs,Tableau1),
  set_abox(Tableau1,ABox,Tableau).
*/

merge(M,X,Y,Expl,Tableau0,Tableau):-
  !,
  get_tabs(Tableau0,Tabs0),
  merge_tabs(X,Y,Tabs0,Tabs),
  get_abox(Tableau0,ABox0),
  flatten([X,Y],L0),
  sort(L0,L),
  list_as_sameIndividual(L,SI),
  get_clashes(Tableau0,Clashes0),
  merge_abox(M,L,SI,Expl,ABox0,ABox,ClashesToCheck),
  set_abox(Tableau0,ABox,Tableau1),
  check_merged_classes(M,ClashesToCheck,Tableau1,NewClashes),
  update_clashes_after_merge(M,L,SI,Tableau1,Clashes0,ClashesAM),
  append(NewClashes,ClashesAM,Clashes),
  set_tabs(Tableau1,Tabs,Tableau2),
  set_clashes(Tableau2,Clashes,Tableau3),
  get_expansion_queue(Tableau3,ExpQ0),
  update_expansion_queue_after_merge(L,SI,ExpQ0,ExpQ),
  set_expansion_queue(Tableau3,ExpQ,Tableau).


/*
 * merge node in tableau. X and Y single individuals
 */

merge_tabs(X,Y,(T0,RBN0,RBR0),(T,RBN,RBR)):-
  (neighbours(X,T0,LSX0)*->assign(LSX0,LSX);assign([],LSX)),
  (neighbours(Y,T0,LSY0)*->assign(LSY0,LSY);assign([],LSY)),
  transpose_ugraph(T0,TT),
  (neighbours(X,TT,LPX0)*->assign(LPX0,LPX);assign([],LPX)),
  (neighbours(Y,TT,LPY0)*->assign(LPY0,LPY);assign([],LPY)),
  % list_as_sameIndividual([X,Y],SI), %TODO
  flatten([X,Y],L0),
  sort(L0,SI),
  set_predecessor(SI,X,LPX,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),!,
  set_successor(SI,X,LSX,(T1,RBN1,RBR1),(T2,RBN2,RBR2)),!,
  set_predecessor(SI,Y,LPY,(T2,RBN2,RBR2),(T3,RBN3,RBR3)),!,
  set_successor(SI,Y,LSY,(T3,RBN3,RBR3),(T4,RBN4,RBR4)),!,
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
  add_edge_int(R,H,NN,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_predecessor1(NN,H,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_successor(_NN,_X,[],Tabs,Tabs).

set_successor(NN,X,[H|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  rb_lookup((X,H),LR,RBN0),
  set_successor1(NN,H,LR,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_successor(NN,X,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

set_successor1(_NN,_H,[],Tabs,Tabs).

set_successor1(NN,H,[R|L],(T0,RBN0,RBR0),(T,RBN,RBR)):-
  add_edge_int(R,NN,H,(T0,RBN0,RBR0),(T1,RBN1,RBR1)),
  set_successor1(NN,H,L,(T1,RBN1,RBR1),(T,RBN,RBR)).

/*
  merge node in ABox
*/

% TODO update
merge_abox(_M,_L,_,_,[],[],[]).

merge_abox(M,L,SI,Expl0,[(classAssertion(C,Ind),ExplT)|T],[(classAssertion(C,SI),Expl)|ABox],[C-SI|CTC]):-
  member(Ind,L),!,
  and_f(M,Expl0,ExplT,Expl),
  %and_f_ax(M,sameIndividual(L),Expl1,Expl),
  merge_abox(M,L,SI,Expl0,T,ABox,CTC).

merge_abox(M,L,SI,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,SI,Ind2),Expl)|ABox],CTC):-
  member(Ind1,L),!,
  and_f(M,Expl0,ExplT,Expl),
  %and_f_ax(M,sameIndividual(L),Expl1,Expl),
  merge_abox(M,L,SI,Expl0,T,ABox,CTC).

merge_abox(M,L,SI,Expl0,[(propertyAssertion(P,Ind1,Ind2),ExplT)|T],[(propertyAssertion(P,Ind1,SI),Expl)|ABox],CTC):-
  member(Ind2,L),!,
  and_f(M,Expl0,ExplT,Expl),
  %and_f_ax(M,sameIndividual(L),Expl1,Expl),
  merge_abox(M,L,SI,Expl0,T,ABox,CTC).

merge_abox(M,L,SI,Expl0,[H|T],[H|ABox],CTC):-
  merge_abox(M,L,SI,Expl0,T,ABox,CTC).


/*
  check for new clashes due to merge
 */

check_merged_classes(_,[],_,[]).

check_merged_classes(M,[ToCheck|TC],Tab,[ToCheck|NewClashes]):-
  check_clash(M,ToCheck,Tab),!,
  check_merged_classes(M,TC,Tab,NewClashes).

check_merged_classes(M,[_ToCheck|TC],Tab,NewClashes):-
  check_merged_classes(M,TC,Tab,NewClashes).

/*
 update clashes ofter merge
 substitute ind in clashes with sameIndividual
 */

update_clashes_after_merge(M,L,SI,Tableau,Clashes0,Clashes):-
  update_clashes_after_merge(M,L,SI,Tableau,Clashes0,Clashes,0).

% if last argument is 0 -> need to theck clash for sameIndividual/differentIndividual
% if there is no clash (check_clash returns false), backtrack to (*)
update_clashes_after_merge(M,_,SI,Tableau,[],[SI],0):-
  check_clash(M,SI,Tableau),!.

% (*)
update_clashes_after_merge(_,_,_,_,[],[],_).

update_clashes_after_merge(M,L,SI,Tableau,[sameIndividual(LC)|TC0],[SI|TC],0):-
  memberchk(I,L),
  memberchk(I,LC),!,
  update_clashes_after_merge(M,L,SI,Tableau,TC0,TC,1).

update_clashes_after_merge(M,L,SI,Tableau,[C-I|TC0],[C-SI|TC],UpdatedSI):-
  memberchk(I,L),!,
  update_clashes_after_merge(M,L,SI,Tableau,TC0,TC,UpdatedSI).

update_clashes_after_merge(M,L,SI,Tableau,[C-sameIndividual(LOld)|TC0],[C-SI|TC],UpdatedSI):-
  memberchk(I,L),
  memberchk(I,LOld),!,
  update_clashes_after_merge(M,L,SI,Tableau,TC0,TC,UpdatedSI).

update_clashes_after_merge(M,L,SI,Tableau,[Clash|TC0],[Clash|TC],UpdatedSI):-
  update_clashes_after_merge(M,L,SI,Tableau,TC0,TC,UpdatedSI).




/*
 update expansion queue ofter merge
 substitute ind in expansion queue with sameIndividual
 */
update_expansion_queue_after_merge(L,SI,[ExpQD0,ExpQND0],[ExpQD,ExpQND]):-
  update_expansion_queue_after_merge_int(L,SI,ExpQD0,ExpQD),
  update_expansion_queue_after_merge_int(L,SI,ExpQND0,ExpQND).

update_expansion_queue_after_merge_int(_,_,[],[]).

update_expansion_queue_after_merge_int(L,SI,[[C,I]|TC0],[[C,IN]|TC]):-
  substitute_individual(L,I,SI,IN),
  update_expansion_queue_after_merge_int(L,SI,TC0,TC).

update_expansion_queue_after_merge_int(L,SI,[[P,S,O]|TC0],[[P,SN,ON]|TC]):-
  substitute_individual(L,S,SI,SN),
  substitute_individual(L,O,SI,ON),
  update_expansion_queue_after_merge_int(L,SI,TC0,TC).

substitute_individual(L,sameIndividual(LSI),SI,SI):-
  memberchk(I,L),
  memberchk(I,LSI),!.

substitute_individual(_,I,_,I):-!.



% ==================================================================================================================

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
sandbox:safe_meta(trill:all_sub_class(_,_,_),[]).
sandbox:safe_meta(trill:prob_sub_class(_,_,_),[]).
sandbox:safe_meta(trill:instanceOf(_,_),[]).
sandbox:safe_meta(trill:instanceOf(_,_,_),[]).
sandbox:safe_meta(trill:instanceOf(_,_,_,_),[]).
sandbox:safe_meta(trill:all_instanceOf(_,_,_),[]).
sandbox:safe_meta(trill:prob_instanceOf(_,_,_),[]).
sandbox:safe_meta(trill:property_value(_,_,_),[]).
sandbox:safe_meta(trill:property_value(_,_,_,_),[]).
sandbox:safe_meta(trill:property_value(_,_,_,_,_),[]).
sandbox:safe_meta(trill:all_property_value(_,_,_,_),[]).
sandbox:safe_meta(trill:prob_property_value(_,_,_,_),[]).
sandbox:safe_meta(trill:unsat(_),[]).
sandbox:safe_meta(trill:unsat(_,_),[]).
sandbox:safe_meta(trill:unsat(_,_,_),[]).
sandbox:safe_meta(trill:all_unsat(_,_),[]).
sandbox:safe_meta(trill:prob_unsat(_,_),[]).
sandbox:safe_meta(trill:inconsistent_theory,[]).
sandbox:safe_meta(trill:inconsistent_theory(_),[]).
sandbox:safe_meta(trill:inconsistent_theory(_,_),[]).
sandbox:safe_meta(trill:all_inconsistent_theory(_),[]).
sandbox:safe_meta(trill:prob_inconsistent_theory(_),[]).
sandbox:safe_meta(trill:axiom(_),[]).
sandbox:safe_meta(trill:kb_prefixes(_),[]).
sandbox:safe_meta(trill:add_kb_prefix(_,_),[]).
sandbox:safe_meta(trill:add_kb_prefixes(_),[]).
sandbox:safe_meta(trill:remove_kb_prefix(_,_),[]).
sandbox:safe_meta(trill:remove_kb_prefix(_),[]).
sandbox:safe_meta(trill:add_axiom(_),[]).
sandbox:safe_meta(trill:add_axioms(_),[]).
sandbox:safe_meta(trill:load_kb(_),[]).
sandbox:safe_meta(trill:load_owl_kb(_),[]).
sandbox:safe_meta(trill:set_tableau_expansion_rules(_,_),[]).

/*

:- use_module(library(utility_translation)).

user:term_expansion((:- trill),[]):-
  utility_translation:get_module(M),
  set_algorithm(M:trill),
  set_up(M),
  utility_translation:set_up_kb_loading(M),
  trill:add_kb_prefixes(M:[('disponte'='http://ml.unife.it/disponte#'),('owl'='http://www.w3.org/2002/07/owl#')]).

user:term_expansion((:- trillp),[]):-
  utility_translation:get_module(M),
  set_algorithm(M:trillp),
  set_up(M),
  utility_translation:set_up_kb_loading(M),
  trill:add_kb_prefixes(M:['disponte'='http://ml.unife.it/disponte#','owl'='http://www.w3.org/2002/07/owl#']).

user:term_expansion((:- tornado),[]):-
  utility_translation:get_module(M),
  set_algorithm(M:tornado),
  set_up(M),
  utility_translation:set_up_kb_loading(M),
  trill:add_kb_prefixes(M:['disponte'='http://ml.unife.it/disponte#','owl'='http://www.w3.org/2002/07/owl#']).

*/

