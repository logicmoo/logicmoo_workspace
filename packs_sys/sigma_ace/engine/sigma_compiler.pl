

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

end_of_file.


:-ensure_loaded('sigma_image.pl').


ti:-make,import_ado_cache.

import_ado_cache:-import_ado_cache('Merge').
import_ado_cache(KBName):-
	makeKnowledgeBase(KBName,KBName),
	showKnowledgeBase,
	getCurrentKnowledgeBase(KB),
	format('~nImporting ~w.~n~n',[KB]),!,
	before_ado_cache(KBName,KB),
	import_ado_cache(KBName,KB),
	after_ado_cache(KBName,KB).


import_ado_cache_all_on:-
	setOption(traceOutput,yes),
	setOption(prover,decider),
	setOption(translationMode,flatRelational),
	setOption(decider,on).
	
import_ado_cache(KBName,KB):-
	sigmaCache(_, surface, Axiom, Vars, KBName, Context, Tracking, User, Status),%trace,
	ado_cache_sigma_assertKIF(Axiom,Tracking,Context,KB,KBName),
	fail.
	
import_ado_cache(KBName,KB):-
	format('~n% Done importing ~w.~n~n',[KBName]),!.
	

before_ado_cache(KBName,KB):-
	retractall(shown_template(_)),
	tell(fookb).

after_ado_cache(KBName,KB):-
	listing(shown_template),
	told,
	predicate_property(shown_template(_), number_of_clauses(N)),
	format('~q clauses ~n',[N]).

ado_cache_sigma_assertKIF(Axiom,Tracking,Context,KB,KBName):-
	sigma_assertKIF(Axiom,Tracking,Context,KB,KBName),!.  % Puts it into SigmaHL % TODO

	
:-dynamic(shown_template/1).

show_template(Surface,Axiom,Result):-shown_template(Result),!.

show_template(Surface,Axiom,Result):-
	format('~nSurf: ~q~nResult:  ~q~n ~q~n~n',[Surface,Axiom,Result]),
	assert(shown_template(Result)),!.       

make_template(Var,Var):-isSlot(Var),!.
make_template(Term,OTerm):-compound(Term),
	Term=..[P|List],
	hlPredicateAttribute(logicalConnective,P),!,
	maplist(make_template,List,OList),
	OTerm=..[P|OList],!.
make_template(Term,OTerm):-compound(Term),
	Term=..List,!,
	maplist(make_template,List,OList),
	OTerm=..OList,!.
make_template(P,P):-member(P,[holds,instance]),!.
make_template(P,Q):-predicate_table(P,Q),!.
make_template(P,Q):-
	flag(pred_num,X,X+1),
	atom_concat('p',X,Q),
	asserta(predicate_table(P,Q)),!.
	
write_cl_of2(Tracking,Axiom):-
	translate(Axiom,Clauses),
	format('~n% ~w  ~q~n',[Tracking,Axiom]),
	list_clauses_out(Clauses).

list_clauses_out([]).
list_clauses_out([H|T]):-
	list_clause_out(H),
	list_clauses_out(T).

list_clause_out(true).
list_clause_out(cl([H],[])):-format('fact(~q).~n',[H]).
list_clause_out(cl([],H)):-format('disj(~q).~n',[H]).
list_clause_out(H):-format('~q.~n',[H]).

cmp_is_a_fuction(X):-atom(X),atom_concat(_,'Fn',X).


sigma_assertKIF(Axiom):-				   
	getCurrentKnowledgeBase(KB),
	sigma_assertKIF(Axiom,KB).

sigma_assertKIF(Axiom,KB):-
	idGen(Tracking),
	sigma_assertKIF(Axiom,Tracking,[],KBName).	

sigma_assertKIF(Axiom,Context,KB):-
	idGen(Tracking),
	sigma_assertKIF(Axiom,Tracking,[Context],KBName).	


sigma_assertKIF(Axiom,Tracking,Context,KBName):-
	sigma_assertKIF(Axiom,Tracking,Context,KBName,KBName).

% ===============================================================	
% sigma_assertKIF(Axiom,Tracking,Context,KBObj,KBName)
% ===============================================================	

sigma_assertKIF(Axiom,Tracking,Context,KB,KBName):-
	preprocessAxiom(Axiom,AxiomO),!,
	sigma_assertKIF2(AxiomO,Tracking,Context,KB,KBName).


preprocessAxiom(Axiom,Axiom).  % TODO
'VariableArityRelation'(disjointDecomposition).
'VariableArityRelation'(exhaustiveDecomposition).
'VariableArityRelation'(partition).

 
% Uses Specialization

sigma_assertKIF2(Axiom,Tracking,Context,KB,KBName):-
	sigma_assertKIF_implicit(Axiom),!.
sigma_assertKIF2(Axiom,Tracking,Context,KB,KBName):-
	sigma_assertKIF_fast(Axiom,Tracking,Context,KB,KBName),!.
sigma_assertKIF2(Axiom,Tracking,Context,KB,KBName):-
	sigma_assertKIF_slow(Axiom,Tracking,Context,KB,KBName),!.

sigma_assertKIF2(Axiom,Tracking,Context,KB,KBName):-
        sigma_numbervars(Surface,1,_),!,
	getAssertionClauses(KB,Ctx,Axiom,Clauses,_,Flags),!,
	write_surf_axioms(Axiom,Clauses).

write_surf_axioms(Surface,and(A,B)):-!,
	write_surf_axioms(Surface,A),!,
	write_surf_axioms(Surface,B).
	
write_surf_axioms(Surface,CL):-!,
	flag(pred_num,_,0),
	retractall(predicate_table(_,_)),!,
	unnumbervars(Surface:CL,OSurface:OCL),	!,
	sigma_numbervars(OSurface:OCL,0,_),
	make_template(OCL,Template),!,
	show_template(OSurface,OCL,Template),!.



%        format('Uncompiled: ~q~n',[C]),
 %       ignore((C=[],write(Axiom),nl,nl)),

% ===============================================================	
% Fast/Slow Specializations
% ===============================================================      
sigma_assertKIF_fast(KIF,Tracking,Context,KB,KBName):-
	sigma_assertKIF_mapping(KIF,Tracking,Context,KB,Mapped),
	invokeMapping(Mapped),!.

sigma_assertKIF_mapping(KIF,Tracking,Context,KB,Mapped):-
	mapping_nt(KIF,Context,KB,Mapped).

mapping_nt(KIF,Context,KB,Mapped):-
	mapping_nt(KIF,Mapped).
	
invokeMapping(undefined).
invokeMapping([A|B]):-!,invokeMapping(A),invokeMapping(B).
invokeMapping((A,B)):-!,invokeMapping(A),invokeMapping(B).
invokeMapping((A;B)):-!,invokeMapping(A);invokeMapping(B).
invokeMapping('+'(Axiom)):-!,sigma_assertKIF(Axiom).
invokeMapping(Slowmapped):-Slowmapped.

sigma_assertKIF_slow(KIF,Tracking,Context,KB,KBName):-
	sigma_assertKIF_slowmapping(KIF,Tracking,Context,KB,Slowmapped),
	invokeMapping(Slowmapped),!.

sigma_assertKIF_slowmapping(KIF,Tracking,Context,KB,Slowmapped):-
	slowmapping_nt(KIF,Context,KB,Slowmapped).

slowmapping_nt(KIF,Context,KB,Slowmapped):-
	slowmapping_nt(KIF,Slowmapped).


% ===============================================================	
% Mapped Specializations
% ===============================================================      

mapping_nt(KIF,Context,KB,Mapped):-
	mapping_nt(KIF,Mapped). % TODO 

% instance/2 
mapping_nt(instance(CName1,CName2),assert_ind(CName1,CName2)).

% subclass/2
mapping_nt(subclass(A, B),defprimconcept(A,[supers([B])])).

% disjoint/2
mapping_nt(disjoint(CName1,CName2),defprimconcept(CName2,not(CName1))).    

% subrelation/2	   %TODO unclear
mapping_nt(subrelation(A, B),defprimconcept(A,B)).

% range/2	   % TODO unclear
mapping_nt(range(A, B),(defprimrole(A,B),defprimrole(B))).

% rangeSubclass/2	   % TODO unclear
mapping_nt(range(A, B),defprimrole(A,B)).

% ===============================================================	
% Slowmapped Specializations
% ===============================================================      

slowmapping_nt(KIF,Context,KB,Slowmapped):-
	slowmapping_nt(KIF,Slowmapped). % TODO 

% inverse/2 for Functions
slowmapping_nt(inverse(CName1,CName2),defrole(CName1,inverse(CName2))):-cmp_is_a_fuction(CName1).

% inverse/2 for ELSE (Predicates)
slowmapping_nt(inverse(CName1,CName2),defprimconcept(CName1,inverse(CName2))).

% Not instance/2 
slowmapping_nt(not(instance(CName1,CName2)),assert_ind(CName1,not(CName2))).

% Catches mapping_info Functions
slowmapping_nt(Term,assert_ind(A,B,R)):-compound(Term),functor(Term,R,N),cmp_is_a_fuction(R),
	mapping_info_relation(R,N),
	Term=..[R,A,B].

% Catches mapping_info Predicates
slowmapping_nt(Term,assert_ind(A,B,R)):-compound(Term),functor(Term,R,N),
	mapping_info_relation(R,N),
	Term=..[R,A,B].

% Catches new Functions %TODO
slowmapping_nt(Term,assert_ind(A,B,R)):-compound(Term),functor(Term,R,N),cmp_is_a_fuction(R),
	assert(mapping_info_relation(R,N)),
	Term=..[R,A,B].

% Catches new Relations %TODO
slowmapping_nt(Term,assert_ind(A,B,R)):-compound(Term),functor(Term,R,N),
	assert(mapping_info_relation(R,N)),
	Term=..[R,A,B].

:-dynamic(mapping_info_relation/2).

mapping_info_relation(representsInLanguage,2).
mapping_info_relation(expressedInLanguage,2).
mapping_info_relation(equivalentContentInstance,2).
mapping_info_relation(subsumesContentClass,2).
mapping_info_relation(equivalentContentClass,2).


mapping_info_relation(documentation,2).
mapping_info_relation(age,2).
mapping_info_relation(publishes,2).
mapping_info_relation(authors,2).
mapping_info_relation(internalRelatedConcept,2).
mapping_info_relation(relatedInternalConcept,2).
mapping_info_relation(synonymousExternalConcept,2).
mapping_info_relation(subsumedExternalConcept,2).
mapping_info_relation(subsumingExternalConcept,2).
mapping_info_relation(subsumesContentInstance,2).
mapping_info_relation(starts,2).
mapping_info_relation(meetsTemporally,2).
mapping_info_relation(finishes,2).
mapping_info_relation(before,2).
mapping_info_relation(identityElement,2).
mapping_info_relation(trichotomizingOn,2).
mapping_info_relation(holdsDuring,2).
mapping_info_relation(deathTime,2).
mapping_info_relation(birthTime,2).
mapping_info_relation(agent,2).
mapping_info_relation(attribute,2).
mapping_info_relation(part,2).
mapping_info_relation(surface,2).
mapping_info_relation(patient,2).
mapping_info_relation(superficialPart,2).
mapping_info_relation(connected,2).
mapping_info_relation(orientation,2).
mapping_info_relation(between,2).
mapping_info_relation(containsInformation,2).
mapping_info_relation(employs,2).
mapping_info_relation(occupiesPosition,2).
mapping_info_relation(member,2).
mapping_info_relation(familyRelation,2).
mapping_info_relation(connectsEngineeringComponents,2).
mapping_info_relation(connectedEngineeringComponents,2).
mapping_info_relation(engineeringSubcomponent,2).
mapping_info_relation(hasPurpose,2).
mapping_info_relation(version,2).
mapping_info_relation(component,2).
mapping_info_relation(date,2).
mapping_info_relation(greaterThan,2).
mapping_info_relation(inhabits,2).
mapping_info_relation(effector,2).
mapping_info_relation(mother,2).
mapping_info_relation(father,2).
mapping_info_relation(siblings,2).
mapping_info_relation(parent,2).
mapping_info_relation(wants,2).
mapping_info_relation(inScopeOfInterest,2).
mapping_info_relation(time,2).
mapping_info_relation(result,2).
mapping_info_relation(instrument,2).
mapping_info_relation(origin,2).
mapping_info_relation(destination,2).
mapping_info_relation(length,2).
mapping_info_relation(path,2).
mapping_info_relation(subProcess,2).
mapping_info_relation(located,2).
mapping_info_relation(during,2).
mapping_info_relation(overlapsSpatially,2).
mapping_info_relation(meetsSpatially,2).
mapping_info_relation(properPart,2).
mapping_info_relation(fills,2).
mapping_info_relation(properlyFills,2).
mapping_info_relation(completelyFills,2).
mapping_info_relation(partiallyFills,2).
mapping_info_relation(hole,2).
mapping_info_relation(interiorPart,2).
mapping_info_relation(overlapsPartially,2).
mapping_info_relation(partlyLocated,2).
mapping_info_relation(connects,2).
mapping_info_relation(duration,2).
mapping_info_relation(lessThan,2).
mapping_info_relation(lessThanOrEqualTo,2).
mapping_info_relation(resource,2).
mapping_info_relation(cooccur,2).
mapping_info_relation(earlier,2).
mapping_info_relation(overlapsTemporally,2).
mapping_info_relation(temporallyBetweenOrEqual,2).
mapping_info_relation(beforeEq,2).
mapping_info_relation(temporallyBetween,2).
mapping_info_relation(temporalPart,2).
mapping_info_relation(frequency,2).
mapping_info_relation(monetaryValue,2).
mapping_info_relation(measure,2).			
mapping_info_relation(larger,2).
mapping_info_relation(pathLength,2).
mapping_info_relation(graphPart,2).
mapping_info_relation(greaterThanOrEqualTo,2).
mapping_info_relation(arcWeight,2).
mapping_info_relation(subGraph,2).
mapping_info_relation(links,2).
mapping_info_relation(element,2).
mapping_info_relation(subset,2).
mapping_info_relation(uses,2).
mapping_info_relation(subPlan,2).
mapping_info_relation(realization,2).
mapping_info_relation(represents,2).
mapping_info_relation(possesses,2).
mapping_info_relation(crosses,2).
mapping_info_relation(penetrates,2).
mapping_info_relation(traverses,2).
mapping_info_relation(exactlyLocated,2).
mapping_info_relation(holdsObligation,2).
mapping_info_relation(confersObligation,2).
mapping_info_relation(capability,2).
mapping_info_relation(holdsRight,2).
mapping_info_relation(confersRight,2).
mapping_info_relation(hasSkill,2).
mapping_info_relation(exploits,2).
mapping_info_relation(entails,2).
mapping_info_relation(copy,2).
mapping_info_relation(distributes,2).
mapping_info_relation(reflexiveOn,2).
mapping_info_relation(equivalenceRelationOn,2).
mapping_info_relation(totalOrderingOn,2).
mapping_info_relation(partialOrderingOn,2).
mapping_info_relation(irreflexiveOn,2).
mapping_info_relation(closedOn,2).
mapping_info_relation(true,2).
mapping_info_relation(knows,2).
mapping_info_relation(believes,2).
mapping_info_relation(needs,2).
mapping_info_relation(manner,2).
mapping_info_relation(piece,2).

% ===============================================================	
% Undefined Specializations
% =============================================================== 

% successorAttributeClosure/2
mapping_nt(successorAttributeClosure(A, B),undefined).

% disjointDecomposition/N
mapping_nt(disjointDecomposition(Class,List),undefined).
% exhaustiveDecomposition/N
mapping_nt(exhaustiveDecomposition(Class,List),undefined).
% partition/N
mapping_nt(partition(Class,List),undefined).




% contraryProperty/2
mapping_nt(contraryProperty(A, B),undefined).

% subAttribute/2
mapping_nt(subAttribute(A, B),undefined).

% successorAttribute/2
mapping_nt(successorAttribute(A, B),undefined).

% domain/3
mapping_nt(domain(A, B, C),undefined).

% 'include-context'/1
mapping_nt('include-context'(A),undefined).

% domainSubclass/3
mapping_nt(domainSubclass(A, B, C),undefined).

% singleValued/2
mapping_nt(singleValued(A, B),undefined).

% range/2
mapping_nt(range(A, B),undefined).

% starts/2
mapping_nt(starts(A, B),undefined).

% equal/2
mapping_nt(equal(A, B),undefined).

% meetsTemporally/2
mapping_nt(meetsTemporally(A, B),undefined).

% finishes/2
mapping_nt(finishes(A, B),undefined).

% before/2
mapping_nt(before(A, B),undefined).

% internalRelatedConcept/2
mapping_nt(internalRelatedConcept(A, B),undefined).

% identityElement/2
mapping_nt(identityElement(A, B),undefined).

% trichotomizingOn/2
mapping_nt(trichotomizingOn(A, B),undefined).

% holdsDuring/2
mapping_nt(holdsDuring(A, B),undefined).

% deathTime/2
mapping_nt(deathTime(A, B),undefined).

% birthTime/2
mapping_nt(birthTime(A, B),undefined).

% agent/2
mapping_nt(agent(A, B),undefined).

% attribute/2
mapping_nt(attribute(A, B),undefined).

% member/2
mapping_nt(member(A, B),undefined).

% version/2
mapping_nt(version(A, B),undefined).

% greaterThan/2
mapping_nt(greaterThan(A, B),undefined).

% destination/2
mapping_nt(destination(A, B),undefined).

% length/2
mapping_nt(length(A, B),undefined).

% path/2
mapping_nt(path(A, B),undefined).

% subProcess/2
mapping_nt(subProcess(A, B),undefined).

% lessThan/2
mapping_nt(lessThan(A, B),undefined).

% lessThanOrEqualTo/2
mapping_nt(lessThanOrEqualTo(A, B),undefined).

% larger/2
mapping_nt(larger(A, B),undefined).

% greaterThanOrEqualTo/2
mapping_nt(greaterThanOrEqualTo(A, B),undefined).

% links/2
mapping_nt(links(A, B),undefined).

% element/2
mapping_nt(element(A, B),undefined).

% subset/2
mapping_nt(subset(A, B),undefined).

% subPlan/2
mapping_nt(subPlan(A, B),undefined).

% entails/2
mapping_nt(entails(A, B),undefined).

% copy/2
mapping_nt(copy(A, B),undefined).

% distributes/2
mapping_nt(distributes(A, B),undefined).

% reflexiveOn/2
mapping_nt(reflexiveOn(A, B),undefined).

% equivalenceRelationOn/2
mapping_nt(equivalenceRelationOn(A, B),undefined).

% totalOrderingOn/2
mapping_nt(totalOrderingOn(A, B),undefined).

% partialOrderingOn/2
mapping_nt(partialOrderingOn(A, B),undefined).

% irreflexiveOn/2
mapping_nt(irreflexiveOn(A, B),undefined).

% closedOn/2
mapping_nt(closedOn(A, B),undefined).

% valence/2
mapping_nt(valence(A, B),undefined).

% true/2
mapping_nt(true(A, B),undefined).

% ===============================================================	
% Implied Axioms
% ===============================================================
sigma_assertKIF_implicit(Axiom):-fail.


% ===============================================================	
% Asking Axioms
% ===============================================================
sigma_askKIF(Axiom).
sigma_askKIF(Axiom,KB).
sigma_askKIF(Axiom,Context,KB).


cmp_print(X):-atom(X),!,write(X).
cmp_print(X):-print(X).
cmp_write(X):-write(X).
cmp_writeq(X):-writeq(X).
cmp_nl:-nl.
cmp_format(X):-format(X).
cmp_format(X,Y):-format(X,Y).
cmp_format(S,X,Y):-format(S,X,Y).




/**********************************************************************
 *
 * @(#) kb.pll 1.24@(#)
 *
 */


/**********************************************************************
 *
 * getCurrentKnowledgeBase(KBName)
 * gets the name of the current knowledgeBase
 *
 */

getCurrentKnowledgeBase(KBName) :-
	currentKnowledgeBase(KB),
	knowledgeBase(KBName,KB,_),
	!.

/**********************************************************************
 *
 * makeKnowledgeBase(+Name,+Comment)
 * creates new knowledgeBase with name Name. Comment can be any string
 * Name will become the current knowledgeBase.
 *
 */

makeKnowledgeBase(Name,Comment) :-
	getTwoRandomNumbers(RT,CT),
	FirstChar is 97 + (CT mod 26),
	Runtime   is (RT mod 10000),
	atom_codes(Runtime,RTChars),
	atom_codes(KBIdentifier,[FirstChar|RTChars]),
	asserta(knowledgeBase(Name,kb(KBIdentifier),Comment)),
	retractall(currentKnowledgeBase(_)),
	asserta(currentKnowledgeBase(kb(KBIdentifier))),
	!.

/**********************************************************************
 *
 * showKnowledgeBase(+Name)
 * 
 */

showKnowledgeBase :- 
	getCurrentKnowledgeBase(Name),
	showKnowledgeBase(Name),
	!.

showKnowledgeBase(KBName) :-
	knowledgeBase(KBName,Name,Comment),
	cmp_write('Knowledge base '), 
	cmp_write(KBName), 
	cmp_nl,
	cmp_write('('),
	cmp_write(Comment),
	cmp_write(')'),
	cmp_nl,
	showModalAxioms(Name),
	showDefprimconcept(Name),
	showDefconcept(Name),
	showDefprimrole(Name),
	showDefrole(Name),
	showDefhl_closed(Name),
	showAssertConcept(Name),
	showAssertRole(Name),
	showFDW(Name),
	!.

showModalAxioms(Name) :-
	modalAxioms(Name,CTX,user,K,C,MOp,A),
	once((nonvar(A) ; (A = C))),
	cmp_write('        '), 
	cmp_write('     modalAxioms('), cmp_write(CTX), cmp_write(','), cmp_write(K), cmp_write(','),
	cmp_write(MOp), cmp_write(','), cmp_write(A), cmp_write(')\n'),
	fail.
showModalAxioms(_) :-
	!.
showAssertConcept(Name) :-
	clause(conceptElement(Name,CTX,_,user,A,C,Ax),_),
	cmp_write(Ax), cmp_write(':     assert_ind('), cmp_write(CTX), cmp_write(','),
	cmp_write(A), cmp_write(','), cmp_write(C), cmp_write(')\n'),
	fail.
showAssertConcept(_) :-
	!.
showAssertRole(Name) :-
	clause(roleElement(Name,CTX,_,user,A,B,R,Ax),_),
	cmp_write(Ax), cmp_write(':     assert_ind('), cmp_write(CTX), cmp_write(','),
	cmp_write(A), cmp_write(','), cmp_write(B), cmp_write(','), cmp_write(R), cmp_write(')\n'),
	fail.
showAssertRole(_) :-
	!.
showDefconcept(Name) :-
	conceptEqualSets(Name,user,CTX,CN,CT,Ax),
	cmp_write(Ax), cmp_write(':     defconcept('), cmp_write(CTX), cmp_write(','),
	cmp_write(CN), cmp_write(','), cmp_write(CT), cmp_write(')\n'),
	fail.
showDefconcept(_Name) :-
	!.
showDefprimconcept(Name) :-
	conceptSubsets(Name,user,CTX,CN,CT,Ax),
	cmp_write(Ax), cmp_write(': defprimconcept('), cmp_write(CTX), cmp_write(','),
	cmp_write(CN), cmp_write(','), cmp_write(CT), cmp_write(')\n'),
	fail.
showDefprimconcept(_Name) :-
	!.
showDefrole(Name) :-
	roleEqualSets(Name,user,CTX,CN,CT,Ax),
	cmp_write(Ax), cmp_write(':        defrole('), cmp_write(CTX), cmp_write(','),
	cmp_write(CN), cmp_write(','), cmp_write(CT), cmp_write(')\n'),
	fail.
showDefrole(_Name) :-
	!.
showDefprimrole(Name) :-
	roleSubsets(Name,user,CTX,CN,CT,Ax),
	cmp_write(Ax), cmp_write(':    defprimrole('), cmp_write(CTX), cmp_write(','),
	cmp_write(CN), cmp_write(','), cmp_write(CT), cmp_write(')\n'),
	fail.
showDefprimrole(_Name) :-
	!.
showDefhl_closed(Name) :-
	hl_closed(Name,CTX,X,Y,R),
	cmp_write('axiom   '), cmp_write(':     defhl_closed('), cmp_write(CTX), cmp_write(','),
	cmp_write(X), cmp_write(','), cmp_write(Y), cmp_write(','), cmp_write(R), cmp_write(')'),
	cmp_nl,
	fail.
showDefhl_closed(_Name) :-
	!.


/**********************************************************************
 *
 * removeKnowledgeBase(+Name)
 *
 */

removeKnowledgeBase :-
	getCurrentKnowledgeBase(KBName),
	!,
	removeKnowledgeBase(KBName).


removeKnowledgeBase(Name) :-
	clearKnowledgeBase(Name),
	retractall(knowledgeBase(Name,_,_)),
	retract(currentKnowledgeBase(Name)),
	asserta(currentKnowledgeBase(kb(e0))),
	!.
removeKnowledgeBase(_Name) :-
	% if we get here, Name was not the current environemt
	!.

/***********************************************************************
 *
 * clearKnowledgeBase(Name)
 *
 */

clearKnowledgeBase :- 
	getCurrentKnowledgeBase(KBName),
	clearKnowledgeBase(KBName),
	!.

clearKnowledgeBase(KBName) :-
	knowledgeBase(KBName,KB,_),
	retractCompiledPredicates(KB),
	retractall(KB,instance/9),
	retractall(KB,kb_in/10),
	retractall(KB,eq/9),
	retractall(KB,constraint/8),
	retractall(KB,hl_holds/5),
	retractall(KB,hl_closed/5),
	retractall(KB,compiledPredicate/2),
	retractall(KB,conceptElement/7),
	retractall(KB,conceptEqualSets/6),
	retractall(KB,conceptHierarchy/3),
	retractall(KB,conceptName/4),
	retractall(KB,conceptSubsets/6),
	retractall(KB,knowledgeBase/3),
	retractall(KB,given_change/4),
	retractall(KB,given_inflLink/4),
	retractall(KB,modalAxioms/7),
	retractall(KB,roleHl_attributes/5),
	retractall(KB,roleDefault/4),
	retractall(KB,roleDefNr/4),
	retractall(KB,roleDomain/4),
	retractall(KB,roleElement/8),
	retractall(KB,roleEqualSets/6),
	retractall(KB,roleHierarchy/3),
	retractall(KB,roleName/4),
	retractall(KB,roleNr/5),
	retractall(KB,roleRange/4),
	retractall(KB,roleSubsets/6),
	retractall(sub(_,KB,_,_,_)),
	retractall(succ(_,KB,_,_,_)),
	retractall(KB,abductiveDerivation/3),
	retractall(KB,consistencyDerivation/3),
	retractall(KB,hypothesis/1),
	retractall(KB,inconsistencyCheck/3),
	retractall(KB,option/2),
	retractall(nsub(_,KB,_,_,_)),
	retractall(KB,nsub3/2),
	retractall(KB,sub3/2),
	retractall(KB,succ3/2),
	!.

/**********************************************************************
 *
 * retractCompiledPredicates(+KB)
 * if the knowledgeBase KB contains compiled predicates, then for each
 * compiled predicate Pred there is a fact 
 *                    compilePredicate(KB,Pred/Arity).
 * So when the knowledgeBase is to be removed, we just abolish the 
 * compiled predicates.
 *
 */

retractCompiledPredicates(KB) :-
	compiledPredicate(KB,Pred/Arity),
	abolish(Pred/Arity),
	fail.
retractCompiledPredicates(_) :-
	!.


/**********************************************************************
 *
 * initKnowledgeBase(Name)
 *
 */

initKnowledgeBase :- 
	getCurrentKnowledgeBase(KBName),
	initKnowledgeBase(KBName),
	!.

initKnowledgeBase(KBName) :-
	clearKnowledgeBase(KBName),
	setOption(queryCaching,on),
	knowledgeBase(KBName,KB,_),
	assert(theory(KB,
	[
        forall(P,forall(X,forall(Y,forall(Z,'=>'(and([equal(X,Z), instance([],P,pair(Z,Y))]),instance([],P,pair(X,Y))))))),
        forall(P,forall(X,forall(Y,forall(Z,'=>'(and([equal(Y,Z), instance([],P,pair(X,Z))]),instance([],P,pair(X,Y))))))),
        forall(A,forall(X,forall(Y,'=>'(and([equal(X,Y), instance([],A,X)]),instance([],A,Y))))),
	forall(X,forall(Y,'=>'(equal(X,Y),equal(Y,X)))),
	forall(X,equal(X,X)),
	forall(X,forall(CTX,instance(CTX,'Entity',X)))])),
	assertInRules(KB),
	% Assert equality axioms
	assertEqRule(KB,1),
	% Assert 'Entity' role
%	assertEqRule(KB,2),
	assertEqRule(KB,3),
	% Proof by hypothesis for roles (Test 14.07.92)
	assertEqRule(KB,4),
	% Assert 'Entity' concept
	assertInRule(KB,1),
	% Assert 'bot' concept
	assertInRule(KB,2),
	% Proof by hypothesis for concepts
	assertInRule(KB,3),
	% Assert X instance some(r,c) => X instance atleast(1,r)
%	gensym(axiom,AN11),
%	assertInRule(KB,3,AN11),
	% Assert X instance atleast(1,r) => X instance some(r,'Entity')
%	assertInRule(KB,4,AN11),
	% Assert X instance atmost(0,r) => X instance all(r,c)
%	gensym(axiom,AN10),
%	assertInRule(KB,1,AN10),
	% Assert X instance all(r,'bot') => X instance atmost(0,r)
%	assertInRule(KB,2,AN10),
	% Assert not('Entity') law
	% necessary for inconsistent knowledge bases?
	% bad influence on runtime!
%	assertInRule(KB,4),
	% Assert double negation laws
	gensym(axiom,AN6),
%	assertInRule(KB,5,AN6),
%	assertInRule(KB,6,AN6),
	% Concrete domains
	gensym(axiom,AN7),
%	assertInRule(KB,7,AN7),
%	assertInRule(KB,8,AN7),
%	assertInRule(KB,9,AN7),
	%%  Abductive Reasoning
	% Proof by abductive hypothesis
	assertAbductionRule(KB,1),
	% Proof by abduction
	assertAbductionRule(KB,2),
	% Meta Reasoning
%	metaReasoning,
	% Assert concept hierarchy
	assertz(conceptHierarchy(KB,[],hl_node(['Entity'],[]))),
	assertz(conceptName(KB,[],[],'Entity')),
	assertz(conceptName(KB,[],[],'bot')),
	% Assert role hierarchy
	assertz(roleHierarchy(KB,[],hl_node(['Entity'],[]))),
	assertz(roleName(KB,[],[],'Entity')),
	initFuncdep,
	!.

/**********************************************************************
 *
 * assertInRules(+KB)
 * asserts the clauses for the instance predicate which is used to 
 * construct goals instance the user interface. In general the instance clauses
 * just call corresponding kb_in clauses. The kb_in clauses result
 * from the translation of terminological and assertional axioms.
 *
 * !! Remember: Changes to this clauses have to be reflected instance the
 *              definition of the compileKB predicate.
 *
 */

assertInRules(KB) :-
	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
		 ifOption(traceOutput,yes,(length(CALL,Depth), cmp_format('trying ~d  instance(~w,~w)~n',[Depth,CN,CON]))),
	kb_in(KB,pr(5),Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
		 ifOption(traceOutput,yes,(length(CALL,Depth), cmp_format('succeeded ~d  instance(~w,~w)~n',[Depth,CN,CON]))))),
% There are no kb_in clauses with priority 4 at the moment (07.10.92)
%	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%	kb_in(KB,pr(4),Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT))),
	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp([or(H1),rl(H2),fl(H3)]),ab(noAb),call(CALL),PT) :-
		 clashInHyp(H2), !, fail)),
	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
		 (CN \== 'Entity', CN \== bot, CN \== not('Entity'), CN \== not(bot),
	kb_in(KB,pr(3),Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT)))),
% There are no kb_in clauses with priority 2 at the moment (07.10.92)
%	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%	kb_in(KB,pr(2),Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT))),
	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
		 (CN \== 'Entity',CN \== bot, CN \== not('Entity'), CN \== not(bot),
	kb_in(KB,pr(1),Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT)))),
% Experimental code (07.10.92 uh)
% It might be useful to have global information about the failure of
% derivations. With the code hl_below such a failure is used to assert to
% hypothesis that the negation of the goal is true.
%	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%		 (nonvar(CON), nonvar(CN), 
%		  \+ hypothesis(instance(KB,modal(CTX),CN,CON,ab(D),PT)),
%		  getNegatedConcept(CN,C1),
%		  assertz(hypothesis(instance(KB,modal(CTX),C1,CON,ab(D),assume))),
%		  fail))),
% There are no kb_in clauses with priority 0 at the moment (07.10.92)
%	assertz((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%	kb_in(KB,pr(0),Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT))),
	!.

assertEqRule(KB,1) :-
	gensym(axiom,AN1),
	gensym(rule,RN1),
	constructEqHead(KB,rn(AN1,RN1,user,lInR),W1,app((F:R),X),F,R,X,HYPS,AB,CALLS,equality,EqHead1),
	constructMLCall(KB,rn(AX,_RN4,_S4,_O4),bodyMC(W1),headMC(W1),normal(R),X,HYPS,AB,CALLS,PT3,InHead2),
	assertz(EqHead1),
%       old code (uh 06.05.94)
%	assertz((EqHead1 :- cCS(CALLS,true),  simple_term(X))),
%-----------------------------------------------------------------
%       The following would be correct
%	assertz((EqHead1 :- cCS(CALLS,true),  InHead2)),
%       old code (uh 20.08.92)
%	assertz((eq(KB,rn(AN1,RN1,user,lInR),modal(CTX),X,X,hyp(HYPS),
%                   ab(_AB),call(CALLS),proved(eq(CTX,X,X,hyp(HYPS),
%                   basedOn(true)))) :- 
%                (cCS(CALLS,true)))),
	!.
assertEqRule(KB,2) :-
	Role1 =.. ['Entity',_X,_Y],
	gensym(axiom,AN3),
	gensym(rule,RN3),
	constructMLHead(KB,rn(AN3,RN3,user,lInR),_CTX,_,Role1,
			_HYPS,_D,_CALLS,tbox,InHeadR),
	assertz((InHeadR)),
	!.
assertEqRule(KB,3) :-
	gensym(axiom,AN20),
	gensym(rule,RN20),
	constructEqHead(KB,rn(AN20,RN20,user,lInR),_W1,_Y,_F,'Entity',_X,_HYPS,_D,_CALLS,tbox,EqHead20),
	assertz(EqHead20),
	!.
assertEqRule(KB,4) :-
	gensym(axiom,AN21),
	gensym(rule,RN21),
	HYPS = [or(H1),rl(H2),fl(H3)],
	constructEqHead(KB,rn(AN21,RN21,user,lInR),W1,Y,F,R,X,HYPS,D,_CALLS,tbox,EqHead20),
	constructEqMark(rn(_AN21,_RN21,_,_),W1,Y,F,R,X,_HYPS2,D,_CALLS2,EqMark20),
	assertz((EqHead20 :- append(H1,H2,H), member(EqMark20,H))),
	!.


assertInRule(KB,1) :-
	% For all X: X instance 'Entity'
	% Priority 5 (high priority)
	gensym(axiom,AN2),
	gensym(rule,RN2),
	constructKBHead(KB,pr(5),rn(AN2,RN2,user,lInR),_W1,'Entity',X,
			_HYPS,_D,_CALLS,tbox,InHead),
	assertz((InHead :- isGround_copy(X,X))),
	!.
assertInRule(KB,2) :-
	% For all X: X instance not(bot) 
	% What is actually needed is the equivalence of 'Entity' and not(bot).
	% So we need
	% For all X: X instance 'Entity' if X instance not(bot)
	% is subsumed by assertInRule(KB,1).
	% For all X: X instance not('Entity') if X instance bot
	% This rule will not be asserted.
	% For all X: X instance bot if X instance not('Entity')
	% is subsumed by assertInRule(KB,4).
	% For all X: X instance not(bot) if X instance 'Entity'.
	% is subsumed by assertInRule(KB,2), i.e. the rule we will
	% assert now.
	% Priority 5 (high priority)
	gensym(axiom,AN4),
	gensym(rule,RN4),
	constructKBHead(KB,pr(5),rn(AN4,RN4,user,lInR),_W1,not(bot),X,
	                _HYPS,_D,_CALLS,tbox,InHead1),
	assertz(InHead1),
	!.
assertInRule(KB,3) :-
	% For all X: X instance C if (X instance C) is a hypothesis
	% Priority 5 (high priority)
	gensym(axiom,AN4),
	gensym(rule,RN4),
	HYPS = [or(H1),rl(H2),fl(H3)],
	constructInHead(KB,rn(_AN5,_RN5,_S5,_O5),CTX,C,X,_HYPS,_D1,_CALLS1,_,InHead1),
	constructKBHead(KB,pr(5),rn(AN4,RN4,system,lInR),CTX,C,X,
	                HYPS,_D,_CALLS2,usingHyp(InHead1),InHead2),
	assertz((InHead2 :- append(H1,H2,H), member(InHead1,H))),
	constructMLMark(InHead1,Mark1),
	assertz((InHead2 :- (append(H1,H2,H), member(Mark1,H)) ; memberDML(Mark1,H3))),
	!.
assertInRule(KB,4) :-
	% For all X: X instance not('Entity') => X instance C 
	% Priority 1 (low priority)
	% necessary for inconsistent knowledge bases ?
	gensym(axiom,AN7),
	gensym(rule,RN7),
	constructKBHead(KB,pr(1),rn(AN7,RN7,system,lInR),CTX,_C,X,
	                HYPS,D,CALLS,PT3,InHead3),
	constructMLCall(KB,rn(AN7,_RN7,_S7,_O7),bodyMC(CTX),headMC(CTX),
                        not('Entity'),X,HYPS,D,CALLS,PT3,L3),
 	constructMLMark(InHead3,Mark3),
	assertz((InHead3 :- cCS(CALLS,Mark3), L3)),
	!.

assertInRule(KB,1,AN10) :- 
	% Assert x instance atmost(0,r) => x instance all(r,c)
	% Priority 1 (low priority)
	gensym(rule,Rule),
	ruleName(AN10,Rule,system,lInR,RN),
	convertInConsequence(KB,pr(1),RN,_CTX,W,all(R,C),X,
	                     HYPS,AB,CALLS,PT,(EqLiteral,InHead)),
	constructMLMark(InHead,Mark),
	convertInAntecedent(KB,rn(AN10,system,lInR),bodyMC(W),headMC(W),
			    atmost(0,R),X,HYPS,AB,CALLS,PT,Body),
	asserta((InHead :- (nonvar(C), (cCS(CALLS,Mark), once((EqLiteral, Body)))))),
	!.
assertInRule(KB,2,AxiomName) :-
	% Assert x instance all(r,'bot') => x instance atmost(0,r)
	% Priority 1 (low priority)
	convertInAntecedent(KB,rn(AxiomName,_O,_Orientation),
                            bodyMC(W1),headMC(W1),all(R,'bot'),X,
			    HYPS,AB,CALLS,PT1,Body),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,system,lInR,RN1),
	convertInConsequence(KB,pr(1),RN1,_CTX,W1,
			     atmost(0,R),X,HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (nonvar(R),(cCS(CALLS,Mark1), once(Body))))),
	!.
assertInRule(KB,3,AxiomName) :-
	% Assert x instance some(R,'Entity') => x instance atleast(1,R)
	% Priority 1 (low priority)
	convertInAntecedent(KB,rn(AxiomName,system,lInR),
			    bodyMC(W1),headMC(W1),
			    some(R,'Entity'),X,HYPS,AB,CALLS,PT1,Body),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,system,lInR,RN1),
	convertInConsequence(KB,pr(1),RN1,_CTX,W1,
			     atleast(1,R),X,HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (nonvar(R), cCS(CALLS,Mark1), once(Body)))),
	!.
assertInRule(KB,4,AxiomName) :-
	% Assert x instance atleast(1,R) => x instance some(R,'Entity')
	% Priority 1 (low priority)
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,system,lInR,RN1),
	convertInConsequence(KB,pr(1),RN1,_CTX,W1,some(R,'Entity'),X,
	                     HYPS,AB,CALLS,PT1,(EqLiteral, InHead1)),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(KB,rn(AxiomName,system,lInR),
	                    bodyMC(W1),headMC(W1),
			    atleast(1,R),X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), once((EqLiteral, Body))))),
	!.
assertInRule(KB,5,AN6) :-
	% For all X: X instance C => X instance not(not(C))
	% Priority 1 (low priority)
	gensym(rule,RN6),
	constructKBHead(KB,pr(1),rn(AN6,RN6,system,lInR),CTX,not(not(C)),X,
	                HYPS,D,CALLS,PT4,Consequence3),
	constructMLCall(KB,rn(AN6,_RN9,_S9,_O9),bodyMC(CTX),headMC(CTX),C,X,
	                HYPS,D,CALLS,PT4,Antecedent4),
	constructMLMark(Consequence3,AxiomHead3),
	assertz((Consequence3 :- cCS(CALLS,AxiomHead3), Antecedent4)),
	!.
assertInRule(KB,6,AN6) :-
	% For all X: X instance not(not(C)) => X instance C 
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(KB,pr(1),rn(AN6,RN8,system,lInR),CTX,C,X,
	                HYPS,D,CALLS,PT3,Consequence4),
	constructMLCall(KB,rn(AN6,_RN7,_S7,_O7),bodyMC(CTX),headMC(CTX),
			not(not(C)),X,HYPS,D,CALLS,PT3,Antecedent3),
	constructMLMark(Consequence4,AxiomHead4),
	assertz((Consequence4 :- cCS(CALLS,AxiomHead4), Antecedent3)),
	!.
assertInRule(KB,7,AN7) :-
	% For all X: X instance set(S2) and cmp_subset(S2,S1) => X instance S1
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(KB,pr(1),rn(AN7,RN8,system,lInR),CTX,set(S1),X,
	                HYPS,D,CALLS,PT2,Consequence1),
	constructMLCall(KB,rn(AN7,_RN2,_S2,_O2),bodyMC(CTX),headMC(CTX),
			set(S2),X,HYPS,D,CALLS,PT2,Antecedent2),
	L1 = cmp_subset(S2,S1),
	constructMLMark(Consequence1,AxiomHead1),
	assertz((Consequence1 :- cCS(CALLS,AxiomHead1), (Antecedent2, L1))),
	!.
assertInRule(KB,8,AN7) :-
	% For all X: X instance set(S2) and X instance set(S3) and 
	%            cmp_intersection(S2,S3,S1) => X instance S1
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(KB,pr(1),rn(AN7,RN8,system,lInR),CTX,set(S1),X,
	                HYPS,D,CALLS,and([PT2,PT3]),Consequence1),
	constructMLCall(KB,rn(AN7,_RN2,_S2,_O2),bodyMC(CTX),headMC(CTX),
			set(S2),X,HYPS,D,CALLS,PT2,Antecedent2),
	constructMLCall(KB,rn(AN7,_RN3,_S3,_O3),bodyMC(CTX),headMC(CTX),
			set(S3),X,HYPS,D,CALLS,PT3,Antecedent3),
	L1 = cmp_intersection([S2,S3],S1),
	constructMLMark(Consequence1,AxiomHead1),
	assertz((Consequence1 :- cCS(CALLS,AxiomHead1), (Antecedent3, (Antecedent2, L1)))),
	!.
assertInRule(KB,9,AN7) :-
	% For all X: X instance set(S2) and X instance set(S3) and 
	%            cmp_intersection(S2,S3,S1) => X instance S1
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(KB,pr(1),rn(AN7,RN8,system,lInR),CTX,not(set(S1)),X,
	                HYPS,D,CALLS,and([PT2,PT3]),Consequence1),
	constructMLCall(KB,rn(AN7,_RN2,_S2,_O2),bodyMC(CTX),headMC(CTX),
			set(S2),X,HYPS,D,CALLS,PT2,Antecedent2),
	constructMLCall(KB,rn(AN7,_RN3,_S3,_O3),bodyMC(CTX),headMC(CTX),
			set(S3),X,HYPS,D,CALLS,PT3,Antecedent3),
	L1 = cmp_subtract(S2,S3,S1),
	constructMLMark(Consequence1,AxiomHead1),
	assertz((Consequence1 :- cCS(CALLS,AxiomHead1), (Antecedent3, (Antecedent2, L1)))),
	!.


assertAbductionRule(KB,1) :-
	% Proof by abductive hypothesis
	gensym(axiom,AN1),
	gensym(rule,RN1),
	constructInHead(KB,rn(_AN2,_RN2,_S2,_O2),CTX,C,X,
	                _HYPS1,_D,_CALLS1,_,InHead1),
	constructMLHead(KB,rn(AN1,RN1,system,lInR),CTX,C,X,_HYPS2,D1,_CALLS2,usingAbHyp(instance(CTX,C,X)),InHead2),
	assertz((InHead2 :- memberDML(InHead1,D1))),
	!.
assertAbductionRule(KB,2) :-
	% Proof by abduction
	gensym(axiom,AN1),
	gensym(rule,RN1),
	constructInHead(KB,rn(_AN2,_RN2,_S2,_O2),CTX,C,X,_HYPS,_D0,_CALLS3,_,InHead2),
	constructMLHead(KB,rn(AN1,RN1,system,lInR),CTX,C1,X,
	                HYPS,D1,CALLS,usingAbHyp(instance(CTX,C,X)),InHead1),
	L1 = normalizeNot(C1,C),
	L2 = not(memberDML(InHead2,D1)),
	L3 = addDefaultML(InHead2,D1),
	assertz((InHead1 :- L1, L2, L3)),
	!.


	

/**********************************************************************
 *
 * switchToKnowledgeBase(Name)
 *
 */

switchToKnowledgeBase(Name) :-
	knowledgeBase(Name,KB,_),
	retractall(currentKnowledgeBase(_)),
	asserta(currentKnowledgeBase(KB)),
	!.

/**********************************************************************
 *
 * saveKnowledgeBase(FileName)
 * 
 */

saveKnowledgeBase(FileName) :-
	getCurrentKnowledgeBase(KBName),
	saveKnowledgeBase(KBName,FileName).

saveKnowledgeBase(KBName,FileName) :-
	tell(FileName),
	writeKnowledgeBase(KBName),
	told,
	% to be implemented
	!.

writeKnowledgeBase(KBName) :-
	knowledgeBase(KBName,KB,C),
	cmp_writeq(knowledgeBase(KBName,KB,C)), cmp_write('.\n'),
%	cmp_write(':- dynamic(constraint/8).\n'),
%	cmp_write(':- dynamic(instance/9).\n'),
%	cmp_write(':- dynamic(kb_in/10).\n'),
%	cmp_write(':- dynamic(hl_holds/5).\n'),
%	cmp_write(':- dynamic(eq/9).\n'),
%	cmp_write(':- dynamic(conceptHierarchy/3).\n'),
%	cmp_write(':- dynamic(roleHierarchy/3).\n'),
%	cmp_write(':- dynamic(conceptEqualSets/6).\n'),
%	cmp_write(':- dynamic(conceptSubsets/6).\n'),
%	cmp_write(':- dynamic(roleEqualSets/6).\n'),
%	cmp_write(':- dynamic(roleSubsets/6).\n'),
%	cmp_write(':- dynamic(conceptName/4).\n'),
%	cmp_write(':- dynamic(roleName/4).\n'),
%	cmp_write(':- dynamic(falsum/2).\n'),
%	cmp_write(':- dynamic(inconsistencyCheck/3).\n'),
%	cmp_write(':- dynamic(conceptElement/6).\n'),
%	cmp_write(':- dynamic(roleElement/7).\n'),
%	cmp_write(':- dynamic(modalAxioms/6).\n'),
	writeall(instance(KB,_A0,_B0,_C0,_D0,_E0,_F0,_G0,_H0)),
	writeall(kb_in(KB,_A1,_B1,_C1,_D1,_E1,_F1,_G1,_H1,_I1)),
	writeall(eq(KB,_A11,_B11,_C11,_D11,_E11,_F11,_G11,_H11)),
	writeall(constraint(KB,_A12,_B12,_C12,_D12,_E12,_F12,_G12)),
	writeall(hl_holds(KB,_A17,_B17,_C17,_D17)),
	writeall(hl_closed(KB,_A18,_B18,_C18,_D18)),
	writeall(compiledPredicate(KB,_A19)),
	writeall(conceptElement(KB,_A14,_B14,_C14,_D14,_E14,_F14)),
	writeall(conceptEqualSets(KB,_A4,_B4,_C4,_D4,_E4)),
	writeall(conceptHierarchy(KB,_A2,_B2)),
	writeall(conceptName(KB,_A8,_B8,_C8)),
	writeall(conceptSubsets(KB,_A5,_B5,_C5,_D5,_E5)),
	writeall(KB,given_change/4),
	writeall(KB,given_inflLink/4),
	writeall(modalAxioms(KB,_A16,_B16,_C16,_D16,_E16,_F16)),
	writeall(KB,roleHl_attributes/5),
	writeall(KB,roleDefault/4),
	writeall(KB,roleDefNr/4),
	writeall(KB,roleDomain/4),
	writeall(KB,roleElement/8),
	writeall(KB,roleEqualSets/6),
	writeall(KB,roleHierarchy/3),
	writeall(KB,roleName/4),
	writeall(KB,roleNr/5),
	writeall(KB,roleRange/4),
	writeall(KB,roleSubsets/6),
	writeall(succ(_A17,KB,_B17,_C17)),
	writeall(sub(_A18,KB,_B18,_C18)),
	!.

writeall(Head) :-
	clause(Head,Body),   
	once((
		sigma_numbervars((Head,Body),0,_),
		cmp_format('~q.\n',[((Head :- Body))]))),
	fail.
writeall(_) :- !.

writeall(KB,Pred/Arity) :-  
	constructHead(KB,Pred/Arity,Head),
	clause(Head,Body),
	once((
		sigma_numbervars((Head,Body),0,_),
		cmp_format('~q.\n',[((Head :- Body))]))),
	fail.
writeall(_,_) :- !.


constructHead(KB,Pred/Arity,Head) :-
	constructArguments(KB,Arity,[],Arguments),
	Head =.. [Pred|Arguments],
	!.
constructArguments(KB,1,Args,[KB|Args]) :-
	!.
constructArguments(KB,N,Args,Arguments) :-
	M is (N - 1),
	constructArguments(KB,M,[X|Args],Arguments),
	!.

/**********************************************************************
 *
 * loadKnowledgeBase(FileName)
 * 
 */

loadKnowledgeBase(FileName) :-	see(FileName),
	read(knowledgeBase(KBName,KB,C)),
	(removeKnowledgeBase(KBName) ; true),
	asserta(knowledgeBase(KBName,KB,C)),
	repeat,
	read(Clause),
	assertClause(Clause),
	seen,
	!.
loadKnowledgeBase(FileName) :-
	seen,
	!,
	fail.
loadKnowledgeBase(FileName,KBName) :-
	see(FileName),
	read(knowledgeBase(_KBName2,KB,C)),
	(removeKnowledgeBase(KBName) ; true),
	assertz(knowledgeBase(KBName,KB,C)),
	repeat,
	read(Clause),
	assertClause(Clause),
	seen,
	!.
loadKnowledgeBase(_FileName,_KBName) :-
	seen,
	!, 
	fail.

assertClause('end_of_file') :-
	!.
assertClause(Clause) :-
	assertz(Clause),
	fail.

/**********************************************************************
 *
 * copyKnowledgeBase(Name1,Name2)
 * copies knowledgeBase Name1 to knowledgeBase Name2.
 *
 */

copyKnowledgeBase(Name2) :-
	getCurrentKnowledgeBase(Name1),
	copyKnowledgeBase(Name1,Name2).

copyKnowledgeBase(Name1,Name2) :-
	knowledgeBase(Name1,KB1,Comment),
	makeKnowledgeBase(Name2,Comment),
	% !! This knowledgeBase mustn't be initializes because
	% the clauses asserted usually during initialization
	% will also be copied from knowledgeBase Name1.
	knowledgeBase(Name2,KB2,_),
	term_expansion(copy,on,KB1,KB2),
	!,
	copyAll(KB1,KB2,instance/9),
	copyAll(KB1,KB2,kb_in/10),
	copyAll(KB1,KB2,eq/9),
	copyAll(KB1,KB2,constraint/8),
	copyAll(KB1,KB2,hl_holds/5),
	copyAll(KB1,KB2,hl_closed/5),
	copyAll(KB1,KB2,compiledPredicate/2),
	copyAll(KB1,KB2,conceptElement/7),
	copyAll(KB1,KB2,conceptEqualSets/6),
	copyAll(KB1,KB2,conceptHierarchy/3),
	copyAll(KB1,KB2,conceptName/4),
	copyAll(KB1,KB2,conceptSubsets/6),
	copyAll(KB1,KB2,knowledgeBase/3),
	copyAll(KB1,KB2,given_change/4),
	copyAll(KB1,KB2,given_inflLink/4),
	copyAll(KB1,KB2,modalAxioms/7),
	copyAll(KB1,KB2,roleHl_attributes/5),
	copyAll(KB1,KB2,roleDefault/4),
	copyAll(KB1,KB2,roleDefNr/4),
	copyAll(KB1,KB2,roleDomain/4),
	copyAll(KB1,KB2,roleElement/8),
	copyAll(KB1,KB2,roleEqualSets/6),
	copyAll(KB1,KB2,roleHierarchy/3),
	copyAll(KB1,KB2,roleName/4),
	copyAll(KB1,KB2,roleNr/5),
	copyAll(KB1,KB2,roleRange/4),
	copyAll(KB1,KB2,roleSubsets/6),
%	copyAll(KB1,KB2,sub/4),
%	copyAll(KB1,KB2,succ/4),
%	copyAll(KB1,KB2,option/2),
%	copyAll(KB1,KB2,nsub/4),
	term_expansion(copy,off,KB1,KB2),
	!.

term_expansion(copy,on,KB1,KB2) :-
	abolish(term_expansion/2),
	assertz((term_expansion((Head :- Body),(Head1 :- Body1)) :-
	term_expansion(Head,Head1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L, Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L; Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion(\+Atom,\+Atom1) :-
	term_expansion(Atom,Atom1))),
	assertz((term_expansion(once(Body1),once(Body2)) :-
		term_expansion(Body1,Body2))),
	assertz((term_expansion(call(Body1),call(Body2)) :-
		 term_expansion(Body1,Body2))),
	assertTermExpansionClause(instance/9,KB1,KB2),
	assertTermExpansionClause(kb_in/10,KB1,KB2),
	assertTermExpansionClause(eq/9,KB1,KB2),
	assertTermExpansionClause(constraint/8,KB1,KB2),
	assertTermExpansionClause(hl_holds/5,KB1,KB2),
	assertTermExpansionClause(hl_closed/5,KB1,KB2),
	assertTermExpansionClause(compiledPredicate/2,KB1,KB2),
	assertTermExpansionClause(conceptElement/7,KB1,KB2),
	assertTermExpansionClause(conceptEqualSets/6,KB1,KB2),
	assertTermExpansionClause(conceptHierarchy/3,KB1,KB2),
	assertTermExpansionClause(conceptName/4,KB1,KB2),
	assertTermExpansionClause(conceptSubsets/6,KB1,KB2),
	assertTermExpansionClause(knowledgeBase/3,KB1,KB2),
	assertTermExpansionClause(given_change/4,KB1,KB2),
	assertTermExpansionClause(given_inflLink/4,KB1,KB2),
	assertTermExpansionClause(modalAxioms/7,KB1,KB2),
	assertTermExpansionClause(roleHl_attributes/5,KB1,KB2),
	assertTermExpansionClause(roleDefault/4,KB1,KB2),
	assertTermExpansionClause(roleDefNr/4,KB1,KB2),
	assertTermExpansionClause(roleDomain/4,KB1,KB2),
	assertTermExpansionClause(roleElement/8,KB1,KB2),
	assertTermExpansionClause(roleEqualSets/6,KB1,KB2),
	assertTermExpansionClause(roleHierarchy/3,KB1,KB2),
	assertTermExpansionClause(roleName/4,KB1,KB2),
	assertTermExpansionClause(roleNr/5,KB1,KB2),
	assertTermExpansionClause(roleRange/4,KB1,KB2),
	assertTermExpansionClause(roleSubsets/6,KB1,KB2),
	assertTermExpansionClause(sub/4,KB1,KB2),
	assertTermExpansionClause(succ/4,KB1,KB2),
	assertz((term_expansion(succ(X1,KB1,X3,X4),
				succ(X1,KB2,X3,X4)))),
	assertz((term_expansion(sub(X1,KB1,X3,X4),
				sub(X1,KB2,X3,X4)))),
	assertz(term_expansion(X,X)),
	!.
term_expansion(copy,off,_KB1,_KB2) :-
	abolish(term_expansion/2),
	!.


assertTermExpansionClause(Pred/Arity,KB1,KB2) :-
	constructArguments(KB,Arity,[],[KB1|Arguments]),
	Head1 =.. [Pred|[KB1|Arguments]],
	Head2 =.. [Pred|[KB2|Arguments]],
	assertz((term_expansion(Head1,Head2))),
	!.

expandTerm(A,B) :-
	expand_term(A,B),
	!.

copyall(KB1,_KB2,Pred,Args) :-
	Head1 =.. [Pred,KB1|Args],
	clause(Head1,Body1),
	expandTerm((Head1,Body1),(Head2,Body2)),
	assertz((Head2 :- Body2)),
	fail.
copyall(_,_,_,_) :- !.

copyAll(KB1,_KB2,Pred/Arity) :-
	constructHead(KB1,Pred/Arity,Head1),
	clause(Head1,Body1),
	expandTerm((Head1,Body1),(Head2,Body2)),
	assertz((Head2 :- Body2)),
	fail.
copyAll(_,_,_) :- !.

/**********************************************************************
 *
 * renameKnowledgeBase(Name1,Name2)
 * renames knowledgeBase Name1 to knowledgeBase Name2.
 *
 */

renameKnowledgeBase(Name1,Name2) :-
	retract(knowledgeBase(Name1,KB,C)),
	asserta(knowledgeBase(Name2,KB,C)),
	% to be implemented
	!.


						

/**********************************************************************
 *
 * @(#) dynamicDef.pll 1.19@(#)
 *
 */


% !! Remember: Any changes to the following list should be carefully
%              reflected instance     clearKnowledgeBase
%                       and     saveKnowledgeBase.

% The following predicates belong to the translated terminologial 
% axioms.
:- multifile(instance/9).
:- dynamic(instance/9).
:- multifile(kb_in/10).
:- dynamic(kb_in/10).
:- multifile(eq/9).
:- dynamic(eq/9).
:- multifile(constraint/8).
:- dynamic(constraint/8).
:- multifile(hl_holds/5).
:- dynamic(hl_holds/5).
% The following predicates are used for additional informations about
% the terminology and the world description.
:- multifile(axiom/3).
:- dynamic(axiom/3).
:- multifile(hl_closed/5).
:- dynamic(hl_closed/5).
:- multifile(compiledPredicate/2).
:- dynamic(compiledPredicate/2).
:- multifile(conceptElement/7).
:- dynamic(conceptElement/7).
:- multifile(conceptEqualSets/6).
:- dynamic(conceptEqualSets/6).
:- multifile(conceptHierarchy/3).
:- dynamic(conceptHierarchy/3).
:- multifile(conceptName/4).
:- dynamic(conceptName/4).
:- multifile(conceptSubsets/6).
:- dynamic(conceptSubsets/6).
:- multifile(knowledgeBase/3).
:- dynamic(knowledgeBase/3).
:- multifile(given_change/4).
:- dynamic(given_change/4).
:- multifile(given_inflLink/4).
:- dynamic(given_inflLink/4).
:- multifile(modalAxioms/7).
:- dynamic(modalAxioms/7).
:- multifile(roleHl_attributes/5).
:- dynamic(roleHl_attributes/5).
:- multifile(roleDefault/4).
:- dynamic(roleDefault/4).
:- multifile(roleDefNr/4).
:- dynamic(roleDefNr/4).
:- multifile(roleDomain/4).
:- dynamic(roleDomain/4).
:- multifile(roleElement/8).
:- dynamic(roleElement/8).
:- multifile(roleEqualSets/6).
:- dynamic(roleEqualSets/6).
:- multifile(roleHierarchy/3).
:- dynamic(roleHierarchy/3).
:- multifile(roleName/4).
:- dynamic(roleName/4).
:- multifile(roleNr/5).
:- dynamic(roleNr/5).
:- multifile(roleRange/4).
:- dynamic(roleRange/4).
:- multifile(roleSubsets/6).
:- dynamic(roleSubsets/6).
:- multifile(query/6).
:- dynamic(query/6).
:- multifile(sub/5).
:- dynamic(sub/5).
:- multifile(succ/5).
:- dynamic(succ/5).
% The following predicates are used during computations only.
:- multifile(abductiveDerivation/3).
:- dynamic(abductiveDerivation/3).
:- multifile(consistencyDerivation/3).
:- dynamic(consistencyDerivation/3).
:- multifile(hypothesis/1).
:- dynamic(hypothesis/1).
:- multifile(inconsistencyCheck/3).
:- dynamic(inconsistencyCheck/3).
:- multifile(option/2).
:- dynamic(option/2).
:- multifile(nsub/5).
:- dynamic(nsub/5).
:- multifile(nsub3/2).
:- dynamic(nsub3/2).
:- multifile(sub3/2).
:- dynamic(sub3/2).
:- multifile(succ3/2).
:- dynamic(succ3/2).
:- multifile(value/2).
:- dynamic(value/2).
% Predicates which are no longer needed
%:- multifile(falsum/2).
%:- dynamic(falsum/2).
%:- multifile(numb/1).
%:- dynamic(numb/1).
:- op(1200,xfx,<-).
:- op(500,fx,~).
/**********************************************************************
 *
 * @(#) lib.pll 1.22@(#)
 *
 */

/*
member(X, [X|_]    ).
member(X, [_|L]) :-
        member(X, L).
*/

/**********************************************************************
 *
 * COUNTER
 *
 */

/**********************************************************************
 *
 * setCounter(+Counter,+Value)
 * creates a new counter Counter with value Value.
 *
 */

setCounter(Counter,N) :-
        asserta(value(Counter,N)),
        !.

/**********************************************************************
 *
 * addCounter(+Counter,+Value)
 * adds Value to the current value of counter Counter.
 *
 */
 
addCounter(Counter,N) :-
        retract(value(Counter,M)),
        Sum is N + M,
        asserta(value(Counter,Sum)),
        !.

/**********************************************************************
 *
 * getCounter(+Counter,-Value)
 * retrieves the current value Value of counter Counter.
 *
 */

getCounter(Counter,N) :-
        value(Counter,N),
        !.

/**********************************************************************
 *
 * writes(+List)
 * put each character instance List.
 *
 */

writes([]) :- !.
writes([H|T]) :- put(H), writes(T).

/***********************************************************************
 *
 * printTime(+G)
 * execute goal G and report the runtime the execution needed.
 * Only available for SICStus Prolog and Quintus Prolog.
 *
 */

printTime(G) :-
	(currentProlog(poplog) ; currentProlog(macprolog)),
	!,
	call(G),
	!.
printTime(G) :-
	!,
	getRuntime(T0),
	printTime(G,T0).

printTime(G,T0) :-
	call(G),
	getRuntime(T1),
	T is T1 - T0,
	cmp_format('Total runtime ~3d sec.~n', [T]).
printTime(_,T0) :-
	getRuntime(T1),
	T is T1 - T0,
	cmp_format('Total runtime ~3d sec.~n', [T]),
	!,
	fail.

/**********************************************************************
 *
 * simple_term(X) 
 * it contrast to the usage instance the Quintus Prolog user manual we
 * call a term `simple` if it is either an atom or a variable.
 * This predicate succeeds iff X is a simple term instance this sense.
 *
 */

simple_term(X) :-(isSlot(X);atomic(X)),!.

/**********************************************************************
 *
 * LIBRARY HANDLING
 *
 */

loadLibraries(sicstus) :-
	assertz((gensym(Prefix, V) :-
	isSlot(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	atom_codes(Prefix,P1),
	atom_codes(N,N1),
	append(P1,N1,V1),
	atom_codes(V,V1),
	!)),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(runtime,[RT,CT]))),
	assertz((getRuntime(RT) :-
	statistics(runtime,[RT|_]))),
	assertz((append([],L2,L2))),
	assertz((append([A1|L1],L2,[A1|L3]) :-
	append(L1,L2,L3))),
	assertz((not(Goal) :- call(\+ Goal))),
	assertz((once(Goal) :- Goal, !)),
	assertz((onceOrMore(Goal) :- option(allProofs,yes), Goal)),
	assertz((onceOrMore(Goal) :- not(option(allProofs,yes)), Goal, !)),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	!.
loadLibraries(eclipse) :-
	assertz((gensym(Prefix, V) :-
	isSlot(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	atom_codes(Prefix,P1),
	atom_codes(N,N1),
	append(P1,N1,V1),
	atom_codes(V,V1),
	!)),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(runtime,[RT,CT]))),
	assertz((getRuntime(RT) :-
	statistics(times,[RT|_]))),
	assertz((append([],L2,L2))),
	assertz((append([A1|L1],L2,[A1|L3]) :-
	append(L1,L2,L3))),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	assertz((onceOrMore(Goal) :- option(allProofs,yes), Goal)),
	assertz((onceOrMore(Goal) :- not(option(allProofs,yes)), Goal, !)),
	!.

loadLibraries(swiprolog) :-
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	assertz((portray(not(F)) :- display(not(F)))),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(cputime,RT1), RT is (ceil(RT1 * 100000)) mod 100000, statistics(atoms,CT))),
	assertz((getRuntime(RT) :-
	statistics(cputime,RT1), RT is ceil(RT1 * 1000))),
	index(kb_in(1,0,0,0,1,1,0,0,0,0)),
	index(eq(1,0,0,1,1,0,0,0,0)),
	index(constraint(1,0,0,1,0,0,0,0)),
/*
	assertz((retractall(Head) :- retract(Head), fail)),
	assertz((retractall(Head) :- retract((Head :- _Body)), fail)),
	assertz((retractall(_))),
*/
	assertz((onceOrMore(Goal) :- option(allProofs,yes), Goal)),
	assertz((onceOrMore(Goal) :- not(option(allProofs,yes)), Goal, !)),
	!.

loadLibraries(poplog) :-
	op(600,xfy,':'),
	assertz((gensym(Prefix, V) :-
	isSlot(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	atom_codes(Prefix,P1),
	atom_codes(N,N1),
	append(P1,N1,V1),
	atom_codes(V,V1),
	!)),
	assertz((append([],L2,L2))),
	assertz((append([A1|L1],L2,[A1|L3]) :-
	append(L1,L2,L3))),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	assertz((once(Goal) :- Goal, !)),
	assertz((onceOrMore(Goal) :- option(allProofs,yes), Goal)),
	assertz((onceOrMore(Goal) :- not(option(allProofs,yes)), Goal, !)),
	assertz((saveSIGMAHL(F) :- save_program(F))),
	!.
loadLibraries(quintus) :-
	op(300,fx,'~'),
	assertz((gensym(Prefix, V) :-
	isSlot(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	atom_codes(Prefix,P1),
	atom_codes(N,N1),
	append(P1,N1,V1),
	atom_codes(V,V1),
	!)),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(runtime,[RT,CT]))),
	assertz((getRuntime(RT) :-
	statistics(runtime,[RT|_]))),
	assertz((not(Goal) :- call(\+ Goal))),
	assertz((once(Goal) :- Goal, !)),
	assertz((onceOrMore(Goal) :- option(allProofs,yes), Goal)),
	assertz((onceOrMore(Goal) :- not(option(allProofs,yes)), Goal, !)),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	assertz((saveSIGMAHL(F) :- save_program(F))),
	!.
loadLibraries(macprolog) :-
	op(600,xfy,':'),
	!.

loadLibraries(xsbprolog) :-
	op(300,fx,'~'),
%	consult(setof),
%       compile(setof),
%        import setof/3, bagof/3,  findall/3, sort/2, keysort/2,'^'/2 from setof,
	!.

testForMacprolog(others) :-
	current_op(_X,_Y,':'),
	!,
	fail.
testForMacprolog(macprolog) :-
	unknown(_X,fail),
	!.

getLibraries :-
	testForMacprolog(_),
	!,
	asserta(currentProlog(macprolog)),
	
	loadLibraries(macprolog).
getLibraries :-
	current_op(1190,fx,delay),
	!,
	sicstus,
	asserta(currentProlog(eclipse)),
	set_flag(variable_names,off),
	loadLibraries(eclipse).

getLibraries :-
	current_op(_X,_Y,?),
	style_check(-singleton),
	!,
	asserta(currentProlog(swiprolog)),
	style_check(-discontiguous),
	loadLibraries(swiprolog).

getLibraries :-
	setof((X,Y),prolog_flag(X,Y),L),
	member((single_var,_Z),L),
	!,
	asserta(currentProlog(quintus)),
	
	prolog_flag(single_var,_,off),
	loadLibraries(quintus).
getLibraries :-
	current_op(_X,hy,_Y),
	loadLibraries(xsbprolog),
	!.
getLibraries :-
	prolog_flag(_X,_Y),
	!,
	asserta(currentProlog(sicstus)),
	
	prolog_flag(single_var_warnings,_,off),
	(prolog_flag(compiling,_,fastcode) ; true),
	prolog_flag(unknown,_,fail),
%	asserta(foreign_file('int.o',[int_init])),
%	asserta(foreign(int_init,int_init)),
%	load_foreign_files(['int.o'],[]),
%	int_init,
	loadLibraries(sicstus).
getLibraries :-
	tell('v1'), version, told,
	!,
	asserta(currentProlog(poplog)),
	
	loadLibraries(poplog).

/**********************************************************************
 *
 * OPTIONS
 *
 */


/***********************************************************************
 *
 * setOption(+Option,+Set)
 * set option Option to value Set.
 *
 */

setOption(Option,Set) :-
	retractall(option(Option,_)),
	asserta(option(Option,Set)),
	!.

/**********************************************************************
 *
 * ifOption(+Option,+Set,+Goal)
 * executes Goal if the current value of Option is Set otherwise
 * the predicate suceeds.
 *
 */

ifOption(Option,Set,Goal) :-
	option(Option,Set),
	call(Goal),
	!.
ifOption(_,_,_) :-
	!.

retractall(KB,Pred/Arity) :-
	constructHead(KB,Pred/Arity,Head),
	retractall(Head), 
	!.


:- getLibraries.




/**********************************************************************
 *
 * @(#) sets.pll 1.1@(#)
 *
 */

%   member(?Element, ?Set)
%   is true when Set is a list, and Element occurs instance it.  It may be used
%   to test for an element or to enumerate all the elements by backtracking.
%   Indeed, it may be used to generate the Set!

/*
member(X, [X|_]    ).
member(X, [_,X|_]  ).
member(X, [_,_,X|_]).
member(X, [_,_,_|L]) :-
        member(X, L).
*/
%   reverseList(+List1,-List2
%   reverses the list List1 to get List2

reverseList([],[]) :- !.
reverseList([H|T],L2) :-
	reverseList(T,L1),
	append(L1,[H],L2),
	!.

%   cmp_memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs instance a known Set.  In return for this limited use, it
%   is more efficient than member/2 when it is applicable.

cmp_memberchk(X, L) :- 
	nonvar(X), 
	nonvar(L),
	memberchk1(X,L).

memberchk1(X, [X|_]    ) :- !.
memberchk1(X, [_,X|_]  ) :- !.
memberchk1(X, [_,_,X|_]) :- !.
memberchk1(X, [_,_,_|L]) :-
	memberchk1(X, L).

%   nonmember(+Element, +Set)
%   means that Element does not occur instance Set.  It does not make sense
%   to instantiate Element instance any way, as there are infinitely many
%   terms which do not occur instance any given set.  Nor can we generate
%   Set; there are infinitely many sets not containing a given Element.
%   Read it as "the given Element does not occur instance the given list Set".
%   This code was suggested by Bruce Hakami; seven versions of this
%   operation were benchmarked and this found to be the fastest.
%   The old code was for DEC-10 Prolog, which did not compile (\+)/1.

nonmember(Element, Set) :-
	nonvar(Element),
	nonvar(Set),
	not(member(Element, Set)).

%   cmp_intersection(+Set1, +Set2, ?Intersection)
%   is true when all three arguments are lists representing sets,
%   and Intersection contains every element of Set1 which is also
%   an element of Set2, the order of elements instance Intersection
%   boneg the same as instance Set1.  That is, Intersection represents
%   the cmp_intersection of the sets represented by Set1 and Set2.
%   If Set2 is a partial list, Intersection will be empty, which
%   is not, of course, correct.  If Set1 is a partial list, this
%   predicate will run away on backtracking.  Set1 and Set2 should
%   both be proper lists, but this is not checked.  Duplicates instance
%   Set1 may survive instance Intersection.  It is worthy of note that
%   if Set1 is an ordset, Intersection is an ordset, despite Set2.

cmp_intersection([], _, []).
cmp_intersection([Element|Elements], Set, Intersection) :-
	cmp_memberchk(Element, Set),
	!,
	Intersection = [Element|Rest],
	cmp_intersection(Elements, Set, Rest).
cmp_intersection([_|Elements], Set, Intersection) :-
	cmp_intersection(Elements, Set, Intersection).



%   cmp_intersection(+ListOfSets, ?Intersection)
%   is true when Intersection is the cmp_intersection of all the sets instance
%   ListOfSets.  The order of elements instance Intersection is taken from
%   the first set instance ListOfSets.  This has been turned inside out to
%   minimise the storage turnover.

cmp_intersection([Set|Sets], Intersection) :-
	intersection1(Set, Sets, Intersection).

intersection1([], _, []).
intersection1([Element|Elements], Sets, Intersection) :-
	memberchk_all(Sets, Element),
	!,
	Intersection = [Element|Rest],
	intersection1(Elements, Sets, Rest).
intersection1([_|Elements], Sets, Intersection) :-
	intersection1(Elements, Sets, Intersection).

memberchk_all([], _).
memberchk_all([Set|Sets], Element) :-
	cmp_memberchk(Element, Set),
	memberchk_all(Sets, Element).

%   cmp_subtract(+Set1, +Set2, ?Difference)
%   is like intersect, but this time it is the elements of Set1 which
%   *are* instance Set2 that are deleted.  Note that duplicated Elements of
%   Set1 which are not instance Set2 are retained instance Difference.


cmp_subtract([], _, []).
cmp_subtract([Element|Elements], Set, Difference) :-
	cmp_memberchk(Element, Set),
	!,
	cmp_subtract(Elements, Set, Difference).
cmp_subtract([Element|Elements], Set, [Element|Difference]) :-
	cmp_subtract(Elements, Set, Difference).

%   cmp_union(+Set1, +Set2, ?Union)
%   is true when cmp_subtract(Set1,Set2,Diff) and append(Diff,Set2,Union),
%   that is, when Union is the elements of Set1 that do not occur instance
%   Set2, followed by all the elements of Set2.

cmp_union([], Union, Union).
cmp_union([Element|Elements], Set, Union) :-
	cmp_memberchk(Element, Set),
	!,
	cmp_union(Elements, Set, Union).
cmp_union([Element|Elements], Set, [Element|Union]) :-
	cmp_union(Elements, Set, Union).

%   cmp_union(+ListOfSets, ?Union)
%   is true when Union is the cmp_union of all sets instance ListOfSets.


cmp_union([],[]).
cmp_union([Set1],Set1).
cmp_union([Set1,Set2|Sets],Union) :-
	cmp_union(Set1,Set2,Set),
	cmp_union([Set|Sets],Union).


%   list_to_set(+List, ?Set)
%   is true when List and Set are lists, and Set has the same elements
%   as List instance the same order, except that it contains no duplicates.
%   The two are thus equal considered as sets.  If you really want to
%   convert a list to a set, list_to_ord_set is faster, but this way
%   preserves as much of the original ordering as possible.
%   If List contains several copies of an element X, only the LAST
%   copy of X is retained.  If you want to convert a List to a Set,
%   retaining the FIRST copy of repeated elements, call
%	symdiff([], List, Set)
		 /*
list_to_set([], []).
list_to_set([Head|Tail], Set) :-
	cmp_memberchk(Head, Tail),
	!,
	list_to_set(Tail, Set).
list_to_set([Head|Tail], [Head|Set]) :-
	list_to_set(Tail, Set).
		   */

%   deletoneList(+List, +Kill, ?Residue)
%   is true when List is a list, instance which Kill may or may not occur, and
%   Residue is a copy of List with all elements equal to Kill deleted.
%   To extract a single copy of Kill, use select(Kill, List, Residue).
%   If List is not proper, deletoneList/3 will FAIL.  Kill and the elements of
%   List should be sufficiently instantiated for \= to be sound.

deletoneList(-, _, _) :- !, fail.		% reject partial lists
deletoneList([], _, []).
deletoneList([Kill|Tail], Kill, Residue) :- !,
	deletoneList(Tail, Kill, Residue).
deletoneList([Head|Tail], Kill, [Head|Residue]) :-
    %	Head \= Kill,
	deletoneList(Tail, Kill, Residue).


cmp_subset([],_S2) :- !.
cmp_subset([E1|S1],S2) :-
	not(not(member(E1,S2))),
	cmp_subset(S1,S2),
	!.

equalset(S1,S2) :-
	cmp_subset(S1,S2),
	cmp_subset(S2,S1),
	!.

%----------------------------------------------------------------------	
%   Module : lists

%   perm(+List, ?Perm)
%   is true when List and Perm are permutations of each other.  The main
%   use of perm/2 is to generate permutations.  You should not use this
%   predicate instance new progractx; use permutation/2 instead.  List must be
%   a proper list.  Perm may be partly instantiated.

perm([], []).
perm([X|Xs], Ys1) :-
	perm(Xs, Ys),
	insert(Ys, X, Ys1).


insert(L, X, [X|L]).
insert([H|T], X, [H|L]) :-
	insert(T, X, L).

%   permutation(?List, ?Perm)
%   is true when List and Perm are permuations of each other.
%   Unlike perm/2, it will work even when List is not a proper list.
%   It even acts instance a marginally sensible way when Perm isn't proper
%   either, but it will still backtrack forever.
%   Be careful: this is quite efficient, but the number of permutations of an
%   N-element list is N!, and even for a 7-element list that is 5040.

permutation(List, Perm) :-
	permutation(List, Perm, Perm).

permutation([], [], []).
permutation([X|Xs], Ys1, [_|Zs]) :-
	permutation(Xs, Ys, Zs),
	insert(Ys, X, Ys1).

/**********************************************************************
 *
 * @(#) abduction.pll 1.2@(#)
 *
 */

getAbductionHyps(L,[]) :-
	isSlot(L),
	!.
getAbductionHyps([],[]) :-
	!.
getAbductionHyps([instance(KB,RN,modal(CTX),C,X,A1,A2,A3,A4)|L1],
	[instance(KB,RN,modal(CTX),C,X,A1,A2,A3,A4)|L2]) :-
	!,
	getAbductionHyps(L1,L2).

doMinimalityCheck(GL1,[instance(KB,RN,modal(CTX),C,X,_A1,_A2,_A3,_A4)|GL2]) :-
	append(GL1,GL2,GL),
	HYPS = [or(GL),rl([]),fl(H3)],
	constructMLCall(KB,rn(_AX3,_RN3,_S3,_O3),bodyMC(CTX),headMC(CTX),
	                C,X,HYPS,[],CALLS,_PT35,Goal),
	not(Goal),
	doMinimalityCheck([instance(KB,RN,modal(CTX),C,X,_A1,_A2,_A3,_A4)|GL1],
	                   GL2),
	!.
doMinimalityCheck(_GL1,[]) :-
	!.
	

doConsistencyCheck(GL1,[instance(KB,RN,modal(CTX),C,X,_A1,_A2,_A3,_A4)|GL2]) :-
	append(GL1,GL2,GL),
	HYPS = [or(GL),rl([]),fl(H3)],
	normalizeNot(not(C),C1),
	constructMLCall(KB,rn(_AX3,_RN3,_S3,_O3),bodyMC(CTX),headMC(CTX),
	                C1,X,HYPS,[],CALLS,_PT35,Goal),
	not(Goal),
	doConsistencyCheck([instance(KB,RN,modal(CTX),C,X,_A1,_A2,_A3,_A4)|GL1],
	                   GL2),
	!.
doConsistencyCheck(_GL1,[]) :-
	!.
	

	
/**********************************************************************
 *
 * @(#) callStack.pll 1.5@(#)
 *
 */

/**********************************************************************
 *
 * THE CALL STACK 
 * is a list of elements of the following form:
 * - true 
 * - instance(rn(AX,RN,_,_),modal(CTX),C,X,hyp(HYPS))
 * - eq(rn(AX,RN,_,_),modal(CTX),X,Y,hyp(HYPS))
 * - constraint(rn(AX,RN,_,_),CTX,(card,app(_F:R,X),Rel,N),hyp(HYPS))
 *
 */

/***********************************************************************
 * 
 * cCS(+CallStack,Call)
 * succeeds if the 'Entity' call on CallStack is not already contained
 * elsewhere instance CallStack and Call is not already contained instance CallStack.
 * This predicate is used to prevent nontermination.
 *
 */

cCS([],_) :- !.
cCS(IL,A1) :-
%	cmp_print('trying '), cmp_print(A1), cmp_nl,
	noAxiom(A1,IL),
%	IL = [I1|IL1],
	noDouble(IL),
%	printAxiom(A1), cmp_nl,
%	cmp_print('------------------------------------------------------------'),
%	cmp_nl,
	not(clashCS([A1|IL])),
	!.

testEqualAbductiveHypotheses(D1,D2) :-
	currentKnowledgeBase(KB),
	abductiveDerivation(KB,D1,HL1),
	abductiveDerivation(KB,D2,HL2),
	!,
	equalset(HL1,HL2),
	!.
testEqualAbductiveHypotheses(_D1,_D2) :-
	!.

testEqualHypotheses(H1,H2) :-
%	equalset(H1,H2),
	!.


functionalSubterm(Term,Term) :-
	!.
functionalSubterm(app(F:R,Term),Term) :-
	not(atomic(Term)),
	!.
functionalSubterm(app(F:R,Term1),Term2) :-
	not(atomic(Term2)),
	functionalSubterm(Term1,Term2),
	!.

% To prove instance(C,X) it is not allowed to use another instance-clause generated 
% from the same axiom
sameAxiom(AX,_RN1,CTX1,instance(C1,X1),HYPS1,D1,
          instance(rn(AX,_RN2,_,_),modal(CTX2),C2,X2,hyp(HYPS2),ab(D2))) :- 
	not(not(functionalSubterm(X1,X2))),
	not(not(C1 = C2)),
	testEqualAbductiveHypotheses(D1,D2),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,instance(_C,X1),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),X2,_,hyp(HYPS2))) :- 
	nonvar(X1),
	nonvar(X2),
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,instance(_C,X1),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),_,X2,hyp(HYPS2))) :- 
	not(not(functionalSubterm(X1,X2))),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
% To prove instance(C,X) it is not allowed to use a constraint-clause generated 
% from the same axiom
sameAxiom(AX,_RN1,CTX1,instance(_C,X1),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),CTX2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :- 
	not(not(functionalSubterm(X1,X2))),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,instance(_C,X1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),CTX2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :- 
	not(not(functionalSubterm(X1,X2))),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,eq(X1,_Y),HYPS1,_D1,
          instance(rn(AX,_RN2,_,_),modal(CTX2),_C,X2,hyp(HYPS2),ab(_D2))) :- 
	nonvar(X1),
	nonvar(X2),
	not(not(functionalSubterm(X1,X2))),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,eq(_X,Y1),HYPS1,_D1,
          instance(rn(AX,_RN2,_,_),modal(CTX2),_C,Y2,hyp(HYPS2),ab(_D2))) :- 
	not(not(functionalSubterm(Y1,Y2))),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,eq(X1,Y1),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),X2,Y2,hyp(HYPS2))) :- 
	not(not(functionalSubterm(X1,X2))),
	not(not(functionalSubterm(Y1,Y2))),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,eq(X1,app(_Y1)),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),CTX2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :-   
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), 
	!.
sameAxiom(AX,_RN1,CTX1,eq(_X,app(_F1:R1,Y1)),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),CTX2,(card,app(_F2:R2,Y2),_Rel,_N),hyp(HYPS2))) :-
	not(not(R1 = R2)),
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,eq(X1,app(_Y1)),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),CTX2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :-   
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), 
	!.
sameAxiom(AX,_RN1,CTX1,eq(_X,app(_F1:R1,Y1)),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),CTX2,(card,app(_F2:R2,Y2),_Rel,_N),hyp(HYPS2))) :-
	not(not(R1 = R2)),
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,eq(_X,Y1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),CTX2,(card,app(_,Y2),_Rel,_N),hyp(HYPS2))) :-
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,c(X1,_,_),HYPS1,_D1,
          instance(rn(AX,_RN2,_,_),modal(CTX2),_,X2,hyp(HYPS2),ab(_D2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,c(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,c(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,c(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),_,app(_,X2),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,c(X1,R1,Rel1),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),CTX2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,c(X1,R1,Rel1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),CTX2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,sc(X1,_,_),HYPS1,_D1,
          instance(rn(AX,_RN2,_,_),modal(CTX2),_,X2,hyp(HYPS2),ab(_D2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,sc(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,sc(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,sc(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(CTX2),_,app(_,X2),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,sc(X1,R1,Rel1),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),CTX2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(AX,_RN1,CTX1,sc(X1,R1,Rel1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),CTX2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(CTX1,CTX2), !.
sameAxiom(_AX,_,_,_,_,_,_) :- !, fail.

equalWorlds(W1,W2) :-
	isSlot(W1),
	isSlot(W2),
	!.
equalWorlds(W1,W2) :-
	isSlot(W1),
	nonvar(W2),
	!,
	fail.
equalWorlds(W1,W2) :-
	isSlot(W2),
	nonvar(W1),
	!,
	fail.
equalWorlds([],[]) :-
	!.
equalWorlds(app(_F:m(_MOp,_A),_W1),[]) :-
	!,
	fail.
equalWorlds([],app(_F:m(_MOp,_A),_W2)) :-
	!,
	fail.
equalWorlds(app(F1:m(MOp,A1),W1),app(F2:m(MOp,A2),W2)) :-
	A1 == A2,
%	isSlot(F1),
%	isSlot(F2),
%       not(not(F1 = F2)),
	!,
	equalWorlds(W1,W2).
equalWorlds(app(F1:m(MOp,A1),_W1),app(F2:m(MOp,A2),_W2)) :-
	A1 == A2,
	nonvar(F1),
	nonvar(F2),
	not(not(F1 = F2)),
	!.
equalWorlds(_W1,_W2) :-
	!,
	fail.
	

noAxiom(true,_) :- !.
noAxiom(_,[]) :- !.
noAxiom(instance(rn(AX,RN,_,_),modal(CTX),C,X,hyp(HYPS),ab(D)),[C1|CL]) :-
	not(sameAxiom(AX,RN,CTX,instance(C,X),HYPS,D,C1)),
	noAxiom(instance(rn(AX,RN,_,_),modal(CTX),C,X,hyp(HYPS),ab(D)),CL).
noAxiom(eq(rn(AX,RN,_,_),modal(CTX),X,Y,hyp(HYPS)),[C1|CL]) :-
	not(sameAxiom(AX,RN,CTX,eq(X,Y),HYPS,_D,C1)),
	noAxiom(eq(rn(AX,RN,_,_),modal(CTX),X,Y,hyp(HYPS)),CL).
noAxiom(constraint(rn(AX,RN,_,_),CTX,(card,app(_F:R,X),Rel,N),hyp(HYPS)),[C1|CL]) :-
	not(sameAxiom(AX,RN,CTX,c(X,R,Rel),HYPS,_D,C1)),
	noAxiom(constraint(rn(AX,RN,_,_),CTX,(card,app(_,X),Rel,N),hyp(HYPS)),CL).
noAxiom(solveConstraint(rn(AX,RN,_,_),CTX,(card,app(_F:R,X),Rel,N),hyp(HYPS)),[C1|CL]) :-
	not(sameAxiom(AX,RN,CTX,sc(X,R,Rel),HYPS,_D,C1)),
	noAxiom(solveConstraint(rn(AX,RN,_,_),CTX,(card,app(_,X),Rel,N),hyp(HYPS)),CL).

noDouble([instance(rn(AX,RN,_,_),modal(CTX),not(C),X,hyp(_HYPS1),ab(D))|IL]) :-
	!,
	not((member(instance(rn(AX1,RN1,_,_),modal(CTX),not(C),X1,hyp(_HYPS2),ab(D)),IL), functionalSubterm(X,X1))),
	not((member(instance(rn(AX2,RN2,_,_),modal(CTX),C,X,hyps(_HYPS3),ab(noAb)),IL),functionalSubterm(X,X1))),
	!,
	noDouble(IL).
noDouble([instance(rn(AX,RN,_,_),modal(CTX),C,X,hyp(_HYPS1),ab(D))|IL]) :-
	!,
	not((member(instance(rn(AX1,RN1,_,_),modal(CTX),C,X1,hyp(_HYPS2),ab(D)),IL), functionalSubterm(X,X1))),
	not((member(instance(rn(AX2,RN2,_,_),modal(CTX),not(C),X1,hyps(_HYPS3),ab(noAb)),IL),functionalSubterm(X,X1))),
	!,
	noDouble(IL).
noDouble([eq(rn(AX,RN,_,_),modal(CTX),X,Y,hyp(_HYPS1))|IL]) :-
	!,
	not(member(eq(rn(AX1,RN1,_,_),modal(CTX),X,Y,hyp(_HYPS2)),IL)),
	!,
	noDouble(IL).
noDouble([constraint(rn(AX,RN,_,_),CTX,(card,app(_F:R,X),Rel,N),hyp(_HYPS1))|IL]) :-
	!,
	not(member(constraint(rn(AX1,RN1,_,_),CTX,(card,app(_F:R,X),Rel,N),hyp(_HYPS2)),IL)),
	!,
	noDouble(IL).
noDouble([solveConstraint(rn(AX,RN,_,_),CTX,(card,app(_F:R,X),Rel,N),hyp(_HYPS1))|IL]) :-
	!,
	not(member(solveConstraint(rn(AX1,RN1,_,_),CTX,(card,app(_F:R,X),Rel,N),hyp(_HYPS2)),IL)),
	!,
	noDouble(IL).
noDouble([solveConstraint(KB1,RL1,noEquivalence,hyp(_HYPS1))|IL]) :-
	!,
	not(member(solveConstraint(KB1,RL1,noEquivalence,hyp(_HYPS2)),IL)),
	!,
	noDouble(IL).
noDouble([I1|IL]) :-
	not(member(I1,IL)),
	!,
	noDouble(I1,IL).
noDouble([]) :-
	!.

printAxiom(solveConstraint(CTX,(card,app((_FF:R),X),Rel,N),hyp(HYPS))) :-
	cmp_print('axiom???'),
	cmp_print('   '),
	cmp_print(solveConstraint(CTX,(app(R,X),Rel,N),hyp(HYPS))),
	!.
printAxiom(eq(rn(AX,RN,_,_),modal(CTX),Y,app((_FF:R),X),hyp(HYPS))) :-
	cmp_print(rn(AX,RN)),
	cmp_print('   '),
	cmp_print(eq(CTX,Y,app(R,X),hyp(HYPS))),
	!.
printAxiom(instance(rn(AX,RN,_,_),modal(_CTX),CN,CON,hyp(HYP))) :-
	cmp_print(rn(AX,RN)),
	cmp_print('   '),
	cmp_print(instance(CN,CON,hyp(HYP))),
	!.
printAxiom(constraint(rn(AX,RN,_,_),CTX,(card,app((_FF:R),X),Rel,N),hyp(HYPS))) :-
	cmp_print(rn(AX,RN)),
	cmp_print('   '),
	cmp_print(constraint(CTX,(app(R,X),Rel,N),hyp(HYPS))),
	!.
printAxiom(true) :-
	!.

/**********************************************************************
 *
 * clashCS(+CL)
 * succeeds if CL is a clash, i.e. it obeys one of the following 
 * conditions:
 * - it contains instance(bot,X) for some X.
 * - it contains both instance(A,X) and instance(not(A),X) for some A and some X.
 *
 */

last([],[],_) :-
	!,
	fail.
last([L1],[],L1) :-
	!.
last([L1|LL1],[L1|LL2],Last) :-
	last(LL1,LL2,Last),
	!.

generateClashGoal(CS1,Goal) :-
	last(CS1,CS2,instance(rn(AX,RN,S,O),modal(W1),C,X,hyp(HYPS))),
	getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,_),
	constructMLHead(KB,rn(_AX1,_RN1,user,_O1),W1,C1,X,CS1,noAb,[],_,Goal),
	!.

		
clashCS(CL) :-
	retract(clashTest(possible)),
	assertz(clashTest(impossible)),
	generateClashGoal(CL,Goal),
	!,
	doClashTest(Goal).
clashCS(_CL) :-
	!,
	fail.
	
doClashTest(InHead1) :-
	call(InHead1),
	InHead1 = instance(KB,_,modal(W1),C1,X,hyp(HYP),ab(_),call(_CALL),_),
	atomic(X),
	normalizeNot(not(C1),C2),
	constructMLHead(KB,rn(_AX2,_RN2,_S2,_O2),W1,C2,X,HYP,noAb,[],_,InHead2),
	call(InHead2),
	cmp_print('Clash test succeeded for\n'),
	cmp_print(HYP), cmp_nl,
	cmp_print('and\n'),
	cmp_print(InHead1), cmp_nl,
	cmp_nl,
	retract(clashTest(impossible)),
	assertz(clashTest(possible)),
	!.
doClashTest(Goal) :-
	% the clash goal has failed, so there is no clash
	cmp_print('Clash test succeeded for\n'),
	cmp_print(HYP), cmp_nl,
	cmp_nl,
	retract(clashTest(impossible)),
	assertz(clashTest(possible)),
	!,
	fail.




% clashCS(CL) :-
% 	clashTest(possible),
% 	member(instance(rn(_,_,_,_),modal(_CTX),bot,_X,hyp(_HYPS1)),CL),
% 	!.
% clashCS(CL) :-
% 	clashTest(possible),
% 	member(instance(rn(_,_,_,_),modal(CTX),not(A),X,hyp(_HYPS1)),CL),
% 	member(instance(rn(_,_,_,_),modal(CTX),A,X,hyp(_HYPS2)),CL),
% 	!.
% clashCS(CL) :-
% 	clashTest(possible),
% 	member(constraint(rn(_,_,_,_),CTX,
% 			  (card,app(_F1:R,X),'>=',N1),hyp(_HYPS1)),CL),
% 	member(constraint(rn(_,_,_,_),CTX,
% 			  (card,app(_F2:R,X),'=<',N2),hyp(_HYPS2)),CL),
% 	number(N1),
% 	number(N2),
% 	N1 > N2,
% 	!.
% clashCS(CL) :-
% 	member(constraint(rn(_,_,_,_),CTX,
% 			  (card,app(_F1:R,X),'=<',N1),hyp(_HYPS1)),CL),
% 	number(N1),
% 	countAllRoleFillersInCS(CTX,R,X,CL,N2),
% 	N2 > N1,
% 	!.
% 		
% 		 
% countAllRoleFillersInCS(CTX,R,X,CL,N) :-
% 	getAllRoleFillersInCS(CTX,R,X,CL,[],RF),
% 	length(RF,N).
% 
% getAllRoleFillersInCS(_CTX,_R,_X,[],RF,RF) :-
% 	!.
% getAllRoleFillersInCS(CTX,R,X,
%    [eq(rn(_,_,_,_),modal(CTX),Y,app(_F:R,X),hyp(_HYPS))|CL],RF1,RF2) :-
% 	nonvar(Y),
% 	nonvar(X),
% 	atomic(Y),
% 	not(member((X,Y),RF1)),
% 	!,
% 	getAllRoleFillersInCS(CTX,R,CL,[(X,Y)|RF1],RF2).
% getAllRoleFillersInCS(CTX,R,X,[_|CL],RF1,RF2) :-
% 	getAllRoleFillersInCS(CTX,R,X,CL,RF1,RF2),
% 	!.

/**********************************************************************
 *
 * @(#) clash.pll 1.2@(#)
 *
 */

clashInHyp(CL) :-
	member(instance(_,modal(_CTX),bot,_X,hyp(_HYPS1),ab(_)),CL),
	!.
clashInHyp(CL) :-
	member(instance(_N2,modal(CTX2),A,X,hyp(_HYPS2),ab(_D2)),CL),
	atomic(A),
	member(instance(_N1,modal(CTX1),not(A),X,hyp(_HYPS1),ab(_D1)),CL),
	not(not(CTX1 = CTX2)),
	!.
% clashInHyp(CL) :-
% 	member(constraint(rn(_,_,_,_),CTX,
% 			  (card,app(_F1:R,X),'>=',N1),hyp(_HYPS1)),CL),
% 	member(constraint(rn(_,_,_,_),CTX,
% 			  (card,app(_F2:R,X),'=<',N2),hyp(_HYPS2)),CL),
% 	number(N1),
% 	number(N2),
% 	N1 > N2,
% 	!.
% clashInHyp(CL) :-
% 	member(constraint(rn(_,_,_,_),CTX,
% 			  (card,app(_F1:R,X),'=<',N1),hyp(_HYPS1)),CL),
% 	number(N1),
% 	countAllRoleFillersInCS(CTX,R,X,CL,N2),
% 	N2 > N1,
% 	!.
% 		
% 		 
% countAllRoleFillersInCS(CTX,R,X,CL,N) :-
% 	getAllRoleFillersInCS(CTX,R,X,CL,[],RF),
% 	length(RF,N).
% 
% getAllRoleFillersInCS(_CTX,_R,_X,[],RF,RF) :-
% 	!.
% getAllRoleFillersInCS(CTX,R,X,
%    [eq(rn(_,_,_,_),modal(CTX),Y,app(_F:R,X),hyp(_HYPS))|CL],RF1,RF2) :-
% 	nonvar(Y),
% 	nonvar(X),
% 	atomic(Y),
% 	not(member((X,Y),RF1)),
% 	!,
% 	getAllRoleFillersInCS(CTX,R,CL,[(X,Y)|RF1],RF2).
% getAllRoleFillersInCS(CTX,R,X,[_|CL],RF1,RF2) :-
% 	getAllRoleFillersInCS(CTX,R,X,CL,RF1,RF2),
% 	!.
% 
/**********************************************************************
 *
 * @(#) classifier.pll 1.12@(#)
 *
 */

/***********************************************************************
 *
 * subsumes(+Name1,+Name2)
 * Parameter: Name1     concept or role name
 *            Name2     concept or role name
 * true iff Name1 subsumes Name2 instance modal context []
 * (so Name1 and Name2 must both be concept names or role names).
 *
 */

subsumes(N1,N2) :-
	getCurrentKnowledgeBase(KBName),
	subsumes(KBName,[],N1,N2).

/***********************************************************************
 *
 * subsumes(+CTX,+Name1,+Name2)
 * Parameter: CTX        modal context
 *            Name1     concept or role name
 *            Name2     concept or role name
 * true iff Name1 subsumes Name2 (so Name1 and Name2 must both be
 * concept names or role names).
 *
 */

subsumes(CTX,N1,N2) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	currentKnowledgeBase(KB),
	clause(conceptName(KB,_CTX1,_W1,N1),_),
	clause(conceptName(KB,_CTX2,_W2,N2),_),
	!,
	subsumes(concepts,KB,CTX,N1,N2).
subsumes(CTX,N1,N2) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	currentKnowledgeBase(KB),
	clause(roleName(KB,_CTX1,_W1,N1),_),
	clause(roleName(KB,_CTX2,_W2,N2),_),
	subsumes(roles,KB,CTX,N1,N2).

subsumes(KBName,CTX,N1,N2) :-
	knowledgeBase(KBName,KB,_),
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	clause(conceptName(KB,_CTX1,_W1,N1),_),
	clause(conceptName(KB,_CTX2,_W2,N2),_),
	!,
	subsumes(concepts,KB,CTX,N1,N2).
subsumes(KBName,CTX,N1,N2) :-
	knowledgeBase(KBName,KB,_),
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	currentKnowledgeBase(KB),
	clause(roleName(KB,_CTX1,_W1,N1),_),
	clause(roleName(KB,_CTX2,_W2,N2),_),
	subsumes(roles,KB,CTX,N1,N2).

subsumes(concepts,KB,CTX,C,D) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	constructMLHead(KB,_RN1,W1,D,aaa,_HYPS,noAb,_CALLS,abox,InHeadD),
	asserta((InHeadD :- call(G1))),
 	getQuery(KB,W1,C,aaa,Exp,InHeadC),
%	convertToGoal(KB,_RN2,CTX,C,aaa,[or([]),rl([]),fl(_DML1)],noAb,[],
%		      _PT2,InHeadC),
	call((call(G1), InHeadC)),
	retract((InHeadD :- _Body)).
subsumes(concepts,KB,CTX,_C,D) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,_G1],_),
	constructMLHead(KB,_RN1,W1,D,aaa,_HYPS,noAb,_CALLS,abox,InHeadD),
	retract((InHeadD :- _Body)),
	!,
	fail.
subsumes(roles,KB,CTX,R,S) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(skolem,SF),
	constructEqHead(KB,_RN1,W1,bbb,SF,S,aaa,_HYPS,noAb,_CALLS,abox,InHeadS),
	asserta((InHeadS :- call(G1))),
	constructEqHead(KB,_RN2,W1,bbb,_FF,R,aaa,[or([]),rl([]),fl(_DML1)],
			noAb,[],_PT2,InHeadR),
	call((G1, InHeadR)),
	retract((InHeadS :- _Body)).
subsumes(roles,KB,CTX,_R,S) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,_G1],_),
	constructEqHead(KB,_RN2,W1,bbb,_FF,S,aaa,_HYPS,noAb,_CALLS,_,InHeadS),
	retract((InHeadS :- _Body)),
	!,
	fail.

/***********************************************************************
 *
 * classified(+CTX,+Name)
 * succeeds iff Name is already instance the appropriate subsumption hierarchy 
 * instance modal context CTX.
 *
 */

classified(KB,CTX,Concept) :-
	clause(conceptName(KB,_CTX1,_W1,Concept),_),
	!,
	conceptHierarchy(KB,CTX,Tree),
	search(Concept,Tree).
classified(KB,CTX,Role) :-
	clause(roleName(KB,_CTX1,_W1,Role),_),
	roleHierarchy(KB,CTX,Tree),
	search(Role,Tree).


search(Concept,hl_node(CL,_NL)) :-
	member(Concept,CL),
	!.
search(Concept,hl_node(_CL,NL)) :-
	searchSubtrees(Concept,NL),
	!.

searchSubtrees(_Concept,[]) :-
	!,
	fail.
searchSubtrees(Concept,[N1|_]) :-
	search(Concept,N1),
	!.
searchSubtrees(Concept,[_|NL]) :-
	searchSubtrees(Concept,NL).


search(Concept,hl_node(CL,NL),[hl_node(CL,NL)]) :-
	member(Concept,CL),
	!.
search(Concept,hl_node(_CL,NL),T1) :-
	searchSubtrees(Concept,NL,T1),
	!.

searchSubtrees(_Concept,[],[]) :-
	!.
searchSubtrees(Concept,[N1|NL],T2) :-
	search(Concept,N1,T1),
	searchSubtrees(Concept,NL,TL),
	append(T1,TL,T2),
	!.

/***********************************************************************
 *
 * classify
 * compute the subsumption hierarchy 
 * side effects: 
 * asserts a clause
 *               conceptHierarchy(CTX,Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 * This is now done using the new classification algorithm written
 * by .
 *
 */

classify :-
	newClassify.
classify(Arg1) :-
	newClassify(Arg1).
classify(KBName,CTX) :-
	newClassify(KBName,CTX).

/***********************************************************************
 *
 * classify(+NewConcept)
 * adds concept NewConcept to the subsumption hierarchy instance the modal
 * context [].
 * side effects: 
 * asserts a clause
 *               conceptHierarchy([],Tree)
 * or            roleHierachy([],Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 *
 */

classify(KBName,NewConcept) :-
	knowledgeBase(KBName,KB,_),
	atomic(NewConcept),
	clause(conceptName(KB,_CTX1,_W2,NewConcept),_), % _CTX1 might be [] ?
	classify(concepts,[],NewConcept).
classify(KBName,NewRole) :-
	knowledgeBase(KBName,KB,_),
	atomic(NewRole),
	clause(roleName(KB,_CTX1,_W1,NewRole),_), % _CTX1 might be [] ?
	classify(roles,[],NewRole).


/***********************************************************************
 *
 * oldClassify
 * compute the subsumption hierarchy instance the modal context CTX
 * side effects: 
 * asserts a clause
 *               conceptHierarchy(CTX,Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 * This is the original classification algorithm written by 
 * .
 *
 */

oldClassify :-
	getCurrentKnowledgeBase(KBName),
	oldClassify(KBName,[]).

oldClassify(KBName) :-
	knowledgeBase(KBName,_KB,_),
	!,
	oldClassify(KBName,[]).
oldClassify(CTX) :-
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	oldClassify(KBName,CTX).

oldClassify(KBName,CTX) :-
	knowledgeBase(KBName,KB,_),
	clause(conceptName(KB,CTX,_,Concept),_),
	once(classify(concepts,KB,CTX,Concept)),
	fail.
oldClassify(KBName,CTX) :-
	knowledgeBase(KBName,KB,_),
	clause(roleName(KB,CTX,_,Role),_),
	once(classify(roles,KB,CTX,Role)),
	fail.
oldClassify(_,_) :-
	!.
	

/***********************************************************************
 *
 * classify(+CTX,+NewConcept)
 * adds concept NewConcept to the subsumption hierarchy instance the modal
 * context CTX.
 * side effects: 
 * asserts a clause
 *               conceptHierarchy(CTX,Tree)
 * or            roleHierarchy(CTX,Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 *
 */
 
classify(KBName,CTX,NewConcept) :-
	knowledgeBase(KBName,KB,_),
	clause(conceptName(KB,_CTX1,_W1,NewConcept),_), % _CTX1 might be CTX ?
	!,
	classify(concepts,KB,CTX,NewConcept).
classify(KBName,CTX,NewRole) :-
	knowledgeBase(KBName,KB,_),
	clause(roleName(KB,_CTX1,_W1,NewRole),_), % _CTX1 might be CTX ?
	!,
	classify(roles,KB,CTX,NewRole).

classify(concepts,KB,CTX,NewConcept) :-
	classified(KB,CTX,NewConcept),
	!.
classify(roles,KB,CTX,NewRole) :-
	classified(KB,CTX,NewRole),
	!.
classify(concepts,KB,CTX,NewConcept) :-
	retract(conceptHierarchy(KB,CTX,OldTree)),
	classify(concepts,KB,CTX,NewConcept,OldTree,NewTree),
	assertz(conceptHierarchy(KB,CTX,NewTree)).
classify(roles,KB,CTX,NewRole) :-
	retract(roleHierarchy(KB,CTX,OldTree)),
	classify(roles,KB,CTX,NewRole,OldTree,NewTree),
	assertz(roleHierarchy(KB,CTX,NewTree)).

classify(Type,KB,CTX,NewConcept,OldTree,NewTree) :-
	testForSubsumption(Type,KB,CTX,NewConcept,OldTree,NewTree,_Judgement),
	!.


/***********************************************************************
 *
 * testForSubsumption(+Type,+CTX,+NewConcept,+OldTree
 *                    -NewTree,-Judgement)
 * builds a tree representation NewTree of the subsumption hierarchy 
 * Judgement has the following meaning:
 * hl_below  : NewConcept is hl_below  the 'Entity' concept of OldTree
 *          instance this case NewTree is instantiated with the tree which
 *          has NewConcept inserted instance OldTree
 * hl_beside : NewConcept is hl_beside the 'Entity' concept of OldTree
 *          instance this case NewTree is instantiated with the tree which
 *          has NewConcept as 'Entity' concept and all concepts of OldTree
 *          which are subsumed by NewConcept hl_below it
 * hl_above  : NewConcept is hl_above  the 'Entity' concept of OldTree
 *          instance this case NewTree is not instantiated
 * instance     : NewConcept is '<=>' to the 'Entity' concept of OldTree
 *          instance this case NewTree is instantiated with the tree which
 *          has NewConcept inserted instance OldTree
 *
 */

testForSubsumption(Type,KB,CTX,NewConcept,hl_node([ClassifiedConcept|CL],AL),NewTree,Judgement) :-
	once(subsume2(Type,KB,CTX,NewConcept,ClassifiedConcept)), 
	testForEquivalence(Type,KB,CTX,NewConcept,hl_node([ClassifiedConcept|CL],AL),NewTree,Judgement),
	!.
testForSubsumption(Type,KB,CTX,NewConcept,hl_node([ClassifiedConcept|CL],AL),NewTree,hl_below) :-
	% to get here the subsumption test instance the first clause
        % must have failed
	once(subsume2(Type,KB,CTX,ClassifiedConcept,NewConcept)),
	% so only x \instance NewConcept        => x \instance ClassifiedConcept
	% but not x \instance ClassifiedConcept => x \instance NewConcept
	tfsList1(Type,KB,CTX,NewConcept,[ClassifiedConcept|CL],[],AL,
                hl_below([]),hl_beside([]),hl_above([]),NewTree),
	!.
testForSubsumption(Type,KB,CTX,NewConcept,hl_node([ClassifiedConcept|CL],AL),NewTree,hl_beside) :-
	% neither x \instance NewConcept        => x \instance ClassifiedConcept
	% nor     x \instance ClassifiedConcept => x \instance NewConcept
	tfsList2(Type,KB,CTX,NewConcept,[ClassifiedConcept|CL],[],AL,
                hl_below([]),hl_beside([]),hl_above([]),NewTree),
	!.

tfsList1(_Type,_,_CTX,NewConcept,N,_NL1,[],
        hl_below(NL3),hl_beside(NL4),hl_above(NL5),Tree) :-
	buildTree1(NewConcept,N,hl_below(NL3),hl_beside(NL4),hl_above(NL5),Tree),
	!.
tfsList1(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
        hl_below(NL3),hl_beside(NL4),hl_above(NL5),NewTree) :-
	testForSubsumption(Type,KB,CTX,NewConcept,Node1,Tree,Judgement),
	continue1(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
	         hl_below(NL3),hl_beside(NL4),hl_above(NL5),Tree,Judgement,NewTree).

buildTree1(NewConcept,N,hl_below([]),hl_beside(NL2),hl_above(NL3),
	hl_node(N,[hl_node([NewConcept],NL3)|NL2])) :- 
	!.
buildTree1(_NewConcept,N,hl_below(NL1),hl_beside(NL2),hl_above(_),
	hl_node(N,NL)) :-
	cmp_union(NL1,NL2,NL),
	!.
buildTree1(_NewConcept,_N,_,_,_,_) :-
	!,
	fail.

	
continue1(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
         hl_below(NL3),hl_beside(NL4),hl_above(NL5),Tree,hl_below,NewTree) :-
	% NL4 can be non-empty
	% NL5 should be the empty list !
	tfsList1(Type,KB,CTX,NewConcept,N,[Node1|NL1],NL2,
                hl_below([Tree|NL3]),hl_beside(NL4),hl_above(NL5),NewTree),
	!.
continue1(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
         hl_below(NL3),hl_beside(NL4),hl_above(NL5),
         hl_node([NewConcept],[]),hl_beside,NewTree) :-
	tfsList1(Type,KB,CTX,NewConcept,N,[Node1|NL1],NL2,
                hl_below(NL3),hl_beside([Node1|NL4]),hl_above(NL5),NewTree),
	!.
continue1(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
         hl_below(NL3),hl_beside(NL4),hl_above(NL5),
         hl_node([NewConcept],[N1|NL]),hl_beside,NewTree) :-
	cmp_union(NL5,[N1|NL],NL6),
	tfsList1(Type,KB,CTX,NewConcept,N,[Node1|NL1],NL2,
                hl_below(NL3),hl_beside([Node1|NL4]),hl_above(NL6),NewTree),
	!.
continue1(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
         hl_below(NL3),hl_beside(NL4),hl_above(NL5),_Tree,hl_above,NewTree) :-
	tfsList1(Type,KB,CTX,NewConcept,N,[Node1|NL1],NL2,
	        hl_below(NL3),hl_beside(NL4),hl_above([Node1|NL5]),NewTree),
	!.
continue1(_Type,_,_CTX,_NewConcept,N,NL1,[_Node1|NL2],
         hl_below(_NL3),hl_beside(_NL4),hl_above(_NL5),
         Tree,instance,hl_node(N,NL)) :-
        % NL3, NL4 and NL5 can be non-empty
	reverseList(NL1,NL6),
	cmp_union(NL6,[Tree|NL2],NL),
	!.

tfsList2(_Type,_,_CTX,NewConcept,N,_NL1,[],
        hl_below(NL3),hl_beside(NL4),hl_above(NL5),Tree) :-
	buildTree2(NewConcept,N,hl_below(NL3),hl_beside(NL4),hl_above(NL5),Tree),
	!.
tfsList2(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
        hl_below(NL3),hl_beside(NL4),hl_above(NL5),NewTree) :-
	testForSubsumption(Type,KB,CTX,NewConcept,Node1,Tree,Judgement),
	continue2(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
	         hl_below(NL3),hl_beside(NL4),hl_above(NL5),Tree,Judgement,NewTree).

buildTree2(NewConcept,_N,hl_below([]),hl_beside(_NL2),hl_above([]),
	hl_node([NewConcept],[])) :-
	!.
buildTree2(NewConcept,_N,hl_below([]),hl_beside(_NL2),hl_above(NL3),
	hl_node([NewConcept],NL3)) :- 
	!.
buildTree2(_NewConcept,_N,_,_,_,_) :-
	!,
	fail.

	
continue2(_Type,_,_CTX,_NewConcept,_N,_NL1,[_Node1|_NL2],
         hl_below(_NL3),hl_beside(_NL4),hl_above(_NL5),_Tree,hl_below,_NewTree) :-
	!,
	fail.
continue2(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
         hl_below(NL3),hl_beside(NL4),hl_above(NL5),_Tree,hl_beside,NewTree) :-
	tfsList2(Type,KB,CTX,NewConcept,N,[Node1|NL1],NL2,
                hl_below(NL3),hl_beside([Node1|NL4]),hl_above(NL5),NewTree),
	!.
continue2(Type,KB,CTX,NewConcept,N,NL1,[Node1|NL2],
         hl_below(NL3),hl_beside(NL4),hl_above(NL5),_Tree,hl_above,NewTree) :-
	tfsList2(Type,KB,CTX,NewConcept,N,[Node1|NL1],NL2,
	        hl_below(NL3),hl_beside(NL4),hl_above([Node1|NL5]),NewTree),
	!.
continue2(_Type,_,_CTX,_NewConcept,_N,_NL1,[_Node1|_NL2],
         hl_below(_NL3),hl_beside(_NL4),hl_above(_NL5),
         _Tree,instance,hl_node(_N,_NL)) :-
	!,
	fail.

testForEquivalence(Type,KB,CTX,NewConcept,hl_node([ClassifiedConcept|CL],AL),
	           hl_node([NewConcept,ClassifiedConcept|CL],AL),instance) :-
	once(subsume2(Type,KB,CTX,ClassifiedConcept,NewConcept)),
	% so NewConcept = ClassifiedConcept
	!.
testForEquivalence(_Type,_,_CTX,_NewConcept,hl_node([_ClassifiedConcept|_CL],_AL),
	           _,hl_above) :-
	% so only x \instance ClassifiedConcept => x \instance NewConcept
        % but not x \instance NewConcept        => x \instance ClassifiedConcept
	!.

subsume2(Type,KB,CTX,X,Y) :- isSlot(X),!,fail.
subsume2(Type,KB,CTX,X,Y) :- isSlot(Y),!,fail.
subsume2(Type,KB,CTX,X,'Entity') :- !,fail.
subsume2(Type,KB,CTX,bot,X) :- !,fail.
subsume2(Type,KB,CTX,X,bot) :- !.
subsume2(Type,KB,CTX,'Entity',X) :- !.
subsume2(Type,KB,CTX,X,Y) :- 
	sub3(X,Y),
	!.
subsume2(Type,KB,CTX,X,Y) :- 
	nsub3(X,Y),
	!,fail. 
subsume2(Type,KB,CTX,X,Y) :- 
	X \== Y,
	subsumes(Type,KB,CTX,X,Y), 
  	cont4(X,Y),
	!.
subsume2(Type,KB,CTX,X,Y) :- 
	X \== Y,
	cont5a(X,Y),
	!,
	fail.
cont4('Entity',Y).
cont4(X,Y) :- 
	assert1(sub3(X,Y)),
	succ3(Z,X),
	cont4(Z,Y),!.
cont4(X,Y). 
cont5a(bot,X) :- !.
cont5a(X,bot) :- !,fail.
cont5a(X,Y) :-
	assert1(nsub3(X,Y)),
	succ3(Y,Z),
	cont5a(X,Z),!.

assert2(G) :- not(G),assert(G),!.
assert2(G) :-!.

retract2(G) :- retract(G),!.
retract2(G) :- !.

succ2(X,Y) :- succ3(X,Y),!.
succ2(X,bot) :- !.


/***********************************************************************
 *
 * showHierarchy(+Type)
 * Parameter: Type     'concepts' or 'roles'
 * display subsumption hierarchy instance the modal context [].
 *
 */

showHierarchy(Type) :-
	getCurrentKnowledgeBase(KBName),
	showHierarchy(KBName,[],Type).

/***********************************************************************
 *
 * showHierarchy(+KBName,+CTX,+Type)
 * Parameter: KBName   knowledgeBase atom_codes
 *            CTX        modal context
 *            Type      'concepts' or 'roles'
 * display subsumption hierarchy instance the modal context CTX.
 *
 */

showHierarchy(KBName,CTX,concepts) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Tree),
	showDag([],Tree).
showHierarchy(KBName,CTX,roles) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Tree),
	showDag([],Tree).

showHierarchy(KBName,Type) :-
	knowledgeBase(KBName,_,_),
	!,
	showHierarchy(KBName,[],Type).
showHierarchy(CTX,Type) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	showHierarchy(KBName,CTX,Type).

/***********************************************************************
 *
 * getHierarchy(+Type,-H)
 * Parameter: Type     'concepts' or 'roles'
 * instantiates H with the internal representation of the subsumption 
 * hierarchy of Type instance the current knowledgeBase and modal context [].
 *
 */

getHierarchy(Type,H) :-
	getCurrentKnowledgeBase(KBName),
	getHierarchy(KBName,[],Type,H).

/***********************************************************************
 *
 * getHierarchy(+KBName,+CTX,+Type,-H)
 * Parameter: KBName   knowledgeBase atom_codes
 *            CTX        modal context
 *            Type      'concepts' or 'roles'
 * instantiates H with the internal representation of the subsumption 
 * hierarchy of Type instance knowledgeBase KBName and modal context CTX.
 *
 */

getHierarchy(KBName,CTX,concepts,Tree) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Tree).
getHierarchy(KBName,CTX,roles,Tree) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Tree).

getHierarchy(KBName,Type,Tree) :-
	knowledgeBase(KBName,_,_),
	!,
	getHierarchy(KBName,[],Type,Tree).
getHierarchy(CTX,Type,Tree) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	getHierarchy(KBName,CTX,Type,Tree).

/***********************************************************************
 *
 * showDag(+Depth,+Tree)
 * display subtree of the tree representation of the subsumption 
 * hierarchy which is located at depth D, where D is the lenght of
 * the list Depth of minus signs, instance the hierarchy.
 *
 */

showDag(Depth,hl_node(CL,AL)) :-
	writes(Depth),
	writes(" "),
	printClass(CL),
	printArgs([45|Depth],AL).

printClass([C1]) :-
	cmp_print(C1),
	cmp_nl,
	!.
printClass([C1,C2|CL]) :-
	cmp_print(C1),
	writes(" ("),
	printRest([C2|CL]),
	writes(")"),
	cmp_nl.
printRest([]) :- !.
printRest([C1]) :-
	cmp_print(C1).
printRest([C1,C2|CL]) :-
	cmp_print(C1),
	cmp_print(", "),
	printRest([C2|CL]).

printArgs(_Depth,[]) :- !.
printArgs(Depth,[N1|NL]) :-
	showDag(Depth,N1),
	printArgs(Depth,NL).






/**********************************************************************
 *
 * @(#) classifier2.pll 1.42@(#)
 *
 */

init_new_daten :- 
	currentKnowledgeBase(KB),
	init_new_daten(KB).

init_new_daten(KB) :-
        init_succ(_),
	init_sub(_),
	init_nsub(_),
	assert(conceptName1(KB,_,'Entity')),
	assert(roleName1(KB,_,'Entity')),
       	assertz(succ(concepts,KB,_,'Entity',bot)),
	assertz(sub(concepts,KB,_,'Entity',_)),
	assertz(nsub(concepts,KB,_,X,X)),	
	assertz(succ(roles,KB,_,'Entity',bot)),
	assertz(sub(roles,KB,_,'Entity',_)),
	assertz(nsub(roles,KB,_,X,X)),
%	assertz(sub(roles,KB,_,X,X)),
	assertz(sub(roles,KB,_,_,bot)),
	assertz(sub(concepts,KB,_,_,bot)).


init_succ(CTX) :- 
 	currentKnowledgeBase(KB),
        init_succ(KB,CTX),
	!.
init_succ(_CTX).
init_succ(KB,CTX) :- 
	retractall(succ(_,KB,CTX,_,_)),
	!.

init_sub(CTX) :-
	currentKnowledgeBase(KB),
	init_sub(KB,CTX).
init_sub(_CTX).
init_sub(KB,CTX) :- 
	retractall(sub(_,KB,CTX,_,_)),
	!.

init_nsub(CTX) :-
 	currentKnowledgeBase(KB),
	init_nsub(KB,CTX).
init_nsub(_CTX).
init_nsub(KB,CTX) :-
	retractall(nsub(_,KB,CTX,_,_)),
	!.

/********************************************************************/
% Test-functions 

new1 :- newClassify,
	show_dag([]),printStat. 
show :- getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,_),
	showDefconcept(KB),
	showDefprimconcept(KB),
	showDefrole(KB),
        showDefprimrole(KB).

new1(CTX) :- newClassify(CTX).

newClassify :-
	getCurrentKnowledgeBase(KBName),
	newClassify(KBName,[]).

newClassify(KBName) :-
	knowledgeBase(KBName,_KB,_),
	!,
	newClassify(KBName,[]).
newClassify(CTX) :-
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	newClassify(KBName,CTX).

newClassify(KBName,CTX) :-
	knowledgeBase(KBName,KB,_),
	init_new_daten(KB),
	initStat,
	testb(KB,CTX),
	buildOrdering(KB,CTX,CTree,RTree),
	retractall(conceptHierarchy(KB,CTX,_)),
	retractall(roleHierarchy(KB,CTX,_)),
	assert(conceptHierarchy(KB,CTX,CTree)),
	assert(roleHierarchy(KB,CTX,RTree)),
	ifOption(testOutput,yes,printStat),
	!.	
testb(KB,CTX) :-
        not(find_concept(concepts,KB,CTX)),
        not(find_role(roles,KB,CTX)).
	
find_concept(concepts,KB,CTX) :-
	getConceptName(KB,CTX,Concept),
	not(atom_codes(Concept,[99,111,110,99,101,112,116|_])), %string "concept"
	ifOption(testOutput,yes,(cmp_print(Concept), cmp_nl)),
	addCounter(conceptsClassified,1),
	find_concept1(concepts,KB,CTX,Concept).

find_concept1(concepts,KB,CTX,Concept) :-
	conceptEqualSets(KB,user,CTX,Concept,CT,_),
%	once(find_concept2(concepts,KB,CTX,Concept,CT)),
	find_concept2(concepts,KB,CTX,Concept,CT),
 	!,
 	fail.
find_concept1(concepts,KB,CTX,Concept) :-
	conceptSubsets(KB,user,CTX,Concept,CT,_),
%	once(find_pconcept2(concepts,KB,CTX,Concept,CT)),
	find_pconcept2(concepts,KB,CTX,Concept,CT),
 	!,
	fail.
find_concept1(concepts,KB,CTX,Concept) :-
	make_succ2(concepts,KB,CTX,Concept),
	!,
	fail.


find_role(roles,KB,CTX) :-
	getRoleName(KB,CTX,Role),
	not(atom_codes(Role,[114,111,108,101|_])), %string "role"
	addCounter(rolesClassified,1),
	find_role1(roles,KB,CTX,Role).

find_role1(roles,KB,CTX,Role) :-
	roleEqualSets(KB,user,CTX,Role,CT,_),
	once(find_role2(roles,KB,CTX,Role,CT)),
 	fail.
find_role1(roles,KB,CTX,Role) :-
	roleSubsets(KB,user,CTX,Role,CT,_),
	once(find_prole2(roles,KB,CTX,Role,CT)),
	fail.
find_role1(roles,KB,CTX,Role) :-
	make_succ2(roles,KB,CTX,Role),
	!,
	fail.

/***** Entwicklungsecke....
test fuer den trans.abschluss von roleEqualSets,roleSubsets,concept...

find_role1(roles,KB,CTX,Role) :-
	roleEqualSets(KB,user,CTX,Role,CT,_),
	find_role11(roles,KB,CTX,Role,CT),
	!,
	fail.
find_role11(roles,KB,CTX,Role,CT) :-
	find_role2(roles,KB,CTX,Role,CT),
	!,
	roleEqualSets(KB,user,CTX,CT,CT1,_),
	find_role11(roles,KB,CTX,Role,CT1),
	!.
find_role11(roles,KB,CTX,Role,CT) :-
	!.

find_role1(roles,KB,CTX,Role) :-
	roleSubsets(KB,user,CTX,Role,CT,_),
	find_prole11(roles,KB,CTX,Role,CT),
 	!,
	fail.
find_prole11(roles,KB,CTX,Role,CT) :-
	find_prole2(roles,KB,CTX,Role,CT),
	!,
	roleSubsets(KB,user,CTX,CT,CT1,_),
	find_prole11(roles,KB,CTX,Role,CT1),
	!.
find_prole11(roles,KB,CTX,Role,CT) :-
	!.
*/

/*******************************************/
find_concept2(concepts,KB,CTX,Concept,and([X|[R]])) :-
	conceptEqualSets(KB,user,CTX,Concept1,R,_),
	assert_succ(concepts,KB,CTX,X,Concept),
	assert_succ(concepts,KB,CTX,Concept1,Concept),		
	!. 
find_concept2(concepts,KB,CTX,Concept,and(L)) :-
	find_concept21(concepts,KB,CTX,Concept,L),
	!.

find_concept2(concepts,KB,CTX,Concept,and([Concept1|[some(Role,Concept1)]])) :-
	getRoleName(KB,CTX,Role),
	assert_succ(concepts,KB,CTX,Concept1,Concept),	
	!.

find_concept2(concepts,KB,CTX,Concept,some(_Role,Concept1)) :-
	find_concept25(KB,CTX,Concept,Concept1),
	!. 

find_concept2(concepts,KB,CTX,Concept,or([and(L)])) :-
	find_concept26(concepts,KB,CTX,Concept,L),
	!.
	
find_concept2(concepts,KB,CTX,Concept,or([and(L1)|R])) :-
	find_concept3(KB,CTX,Concept,R,L1,Z),
	find_concept31(KB,CTX,Concept,Z),	
	!.
find_concept2(concepts,KB,CTX,Concept,or(L)) :-
	find_concept26(concepts,KB,CTX,Concept,L),
	!.
find_concept2(concepts,KB,CTX,Concept,CT) :-
	getConceptName(KB,CTX,CT),
	find_concept20(concepts,KB,CTX,Concept,CT),
	!.

find_concept20(concepts,KB,CTX,Concept,CT) :-
	(succ(concepts,KB,CTX,Topconcept,Concept),
	subsume1(concepts,KB,CTX,Topconcept,CT)),
	assert1(nsub(concepts,KB,CTX,Concept,CT)),
	assert1(nsub(concepts,KB,CTX,CT,Concept)),
	assert_succ(concepts,KB,CTX,Topconcept,CT),
	!.	
find_concept20(concepts,KB,CTX,Concept,CT) :-
	(succ(concepts,KB,CTX,Topconcept,CT),
	subsume1(concepts,KB,CTX,Topconcept,Concept)),
	assert1(nsub(concepts,KB,CTX,Concept,CT)),
	assert1(nsub(concepts,KB,CTX,CT,Concept)),
	assert_succ(concepts,KB,CTX,Topconcept,Concept),
	!.
find_concept20(concepts,KB,CTX,Concept,CT) :-
	assert1(nsub(concepts,KB,CTX,Concept,CT)),
	assert1(nsub(concepts,KB,CTX,CT,Concept)),
	assert1(succ(concepts,KB,CTX,'Entity',Concept)),
	assert1(succ(concepts,KB,CTX,'Entity',CT)),
	!.

find_concept21(concepts,_KB,_CTX,_Concept,[]) :-
	!.
find_concept21(concepts,KB,CTX,Concept,[X|R]) :-
	getConceptName(KB,CTX,X),
	assert_succ(concepts,KB,CTX,X,Concept),
	find_concept21(concepts,KB,CTX,Concept,R),
	!.
find_concept21(concepts,KB,CTX,Concept,[not(R1)|R]) :-    
	getConceptName(KB,CTX,R1),
	setofOrNil(K,find_concept22(concepts,KB,CTX,Concept,R1,K),L),
	find_concept23(concepts,KB,CTX,Concept,L),
	find_concept21(concepts,KB,CTX,Concept,R),
	!.
find_concept21(concepts,KB,CTX,Concept,[_X|R]) :-
	find_concept21(concepts,KB,CTX,Concept,R),
	!,
	fail.

find_concept22(concepts,KB,CTX,Concept,R1,K) :-
	succ(concepts,KB,CTX,K,R1),
	not(succ(concepts,KB,CTX,K,Concept)).

find_concept23(concepts,_KB,_CTX,_Concept,[]) :-
	!.	
find_concept23(concepts,KB,CTX,Concept,[L1|R1]) :-
	find_concept24(concepts,KB,CTX,Concept,L1),
	find_concept23(concepts,KB,CTX,Concept,R1),
	!.
find_concept24(concepts,KB,CTX,Concept,L1) :-
	succ(concepts,KB,CTX,Top,L1),
	succ(concepts,KB,CTX,Top,K),
	subsume1(concepts,KB,CTX,K,Concept),
	setofOrNil(Nf,succ(concepts,KB,CTX,K,Nf),Lnf),
	make_succ1(concepts,KB,CTX,K,Lnf,Concept),
	!. 
		
find_concept25(KB,CTX,Concept,Concept1) :-
	succ(concepts,KB,CTX,K,Concept1),
	setofOrNil(Nf,succ(concepts,KB,CTX,Concept1,Nf),Lnf),
	make_succ1(concepts,KB,CTX,K,Lnf,Concept),
	fail.
find_concept25(_KB,_CTX,_Concept,_Concept1) :-
	!,
	fail.

find_concept26(concepts,KB,CTX,Concept,[C1|R]) :-
	getConceptName(KB,CTX,C1),
%	subsume1(concepts,KB,CTX,Concept,C1),
	assert_succ(concepts,KB,CTX,Concept,C1),
	find_concept26(concepts,KB,CTX,Concept,R).

find_concept3(_KB,_CTX,_Concept,[],Z,Z) :- 
	!.
find_concept3(KB,CTX,Concept,[and(L1)|R],Z,K) :-
	cmp_intersection(Z,L1,Z1),
	find_concept3(KB,CTX,Concept,R,Z1,K),
	!.

find_concept31(_KB,_CTX,_Concept,[]) :-
	!.
find_concept31(KB,CTX,Concept,[L1|R1]) :-
	assert_succ(concepts,KB,CTX,L1,Concept),
	find_concept31(KB,CTX,Concept,R1),
	!.

% ********************** Primconcepte **************************
% es fehlt noch defprimconcept(_,_,some(_,_..))
%                   "         (_,not(),...)
%                   "         (_,...(),...)
					
find_pconcept2(concepts,KB,CTX,PrimConcept,not(X)) :-
	getConceptName(KB,CTX,X),
	cont1a(concepts,KB,CTX,[],X,PrimConcept),
%	succ(concepts,KB,CTX,Topconcept,X),
	find_pconcept23(KB,CTX,X,PrimConcept,Top),
	assert_succ(concepts,KB,CTX,Top,PrimConcept),
	assert_succ(concepts,KB,CTX,Top,X),
	!.
find_pconcept2(concepts,KB,CTX,Primconcept,and(L)) :-
	find_pconcept24(KB,CTX,Primconcept,L),
	!.
find_pconcept2(concepts,KB,CTX,PrimConcept,some(X,Y)) :-
	find_pconcept21(KB,CTX,PrimConcept,X,Y),
	!.
find_pconcept2(concepts,KB,CTX,PrimConcept,and([X|[not(Y)]])) :-
	getConceptName(KB,CTX,X),
	getConceptName(KB,CTX,Y),
	find_pconcept23(KB,CTX,X,Y,Top),
	assert_succ(concepts,KB,CTX,Top,PrimConcept),	
%	assert1(sub(concepts,KB,CTX,PrimConcept)),
	!.
find_pconcept2(concepts,KB,CTX,Primconcept,or([and(L)])) :-
	find_pconcept26(concepts,KB,CTX,Primconcept,L),
	!.
find_pconcept2(concepts,KB,CTX,Primconcept,or([and(L1)|R])) :-
	find_pconcept3(KB,CTX,Primconcept,R,L1,Z),
	find_pconcept31(KB,CTX,Primconcept,Z),	
	!.
find_pconcept2(concepts,KB,CTX,Primconcept,or(L)) :-
	find_pconcept26(concepts,KB,CTX,Primconcept,L),
	!.
find_pconcept2(concepts,KB,CTX,PrimConcept,CT) :-
	getConceptName(KB,CTX,CT),
	assert1(sub(concepts,KB,CTX,CT,PrimConcept)),
	direct_succ(concepts,KB,CTX,[],CT,PrimConcept,Z,L1),
        contb(concepts,KB,CTX,Z,L1,PrimConcept),
	!.


find_pconcept26(concepts,KB,CTX,Primconcept,[C1|R]) :-
	getConceptName(KB,CTX,C1),
	subsume1(concepts,KB,CTX,C1,Primconcept),
	find_pconcept27(concepts,KB,CTX,Primconcept,C1),
  	find_pconcept26(concepts,KB,CTX,Primconcept,R).

find_pconcept27(concepts,KB,CTX,Primconcept,C1):-
	assert1(sub(concepts,KB,CTX,C1,Primconcept)),
	direct_succ(concepts,KB,CTX,[],C1,PrimConcept,Z,L1),
        contb(concepts,KB,CTX,Z,L1,PrimConcept),
	!.

find_pconcept3(_KB,_CTX,_Primconcept,[],Z,Z) :- 
	!.
find_pconcept3(KB,CTX,Primconcept,[and(L1)|R],Z,K) :-
	cmp_intersection(Z,L1,Z1),
	find_pconcept3(KB,CTX,Primconcept,R,Z1,K),
	!.

find_pconcept31(_KB,_CTX,_Primconcept,[]) :-
	!.
find_pconcept31(KB,CTX,Primconcept,[L1|R1]) :-
	find_pconcept27(concepts,KB,CTX,Primconcept,L1),
	find_pconcept31(KB,CTX,Primconcept,R1),
	!.


find_pconcept21(KB,CTX,PrimConcept,X,or([Y1|[Y2]])) :-
	conceptEqualSets(KB,user,CTX,Concept,some(X,Y1),_),
	conceptEqualSets(KB,user,CTX,Concept1,some(X,Y2),_),
	find_pconcept23(KB,CTX,Concept1,Concept,Top),
	assert_succ(concepts,KB,CTX,Top,PrimConcept),
	!.

find_pconcept23(KB,CTX,X,Y,X) :-
	sub(concepts,KB,CTX,X,Y),
	!.
find_pconcept23(KB,CTX,X,Y,Y) :-
	sub(concepts,KB,CTX,Y,X),
	!.
find_pconcept23(KB,CTX,X,Y,Top) :-
	sub(concepts,KB,CTX,Top,X),
	sub(concepts,KB,CTX,Top,Y).

find_pconcept24(KB,CTX,Primconcept,[X|R]) :-
	getConceptName(KB,CTX,X),
	assert1(sub(concepts,KB,CTX,X,PrimConcept)),
	direct_succ(concepts,KB,CTX,[],X,PrimConcept,Z,L1),
        contb(concepts,KB,CTX,Z,L1,PrimConcept),
	find_pconcept24(KB,CTX,Primconcept,R),
	!.


/*************************************************************************
*                    rollen
*/

find_role2(roles,KB,CTX,Role,CT) :-
	getRoleName(KB,CTX,CT),
	succ(roles,KB,CTX,Toprole,Role),
	assert1(nsub(roles,KB,CTX,Role,Ct)),
	assert1(nsub(roles,KB,CTX,Ct,Role)),
	assert_succ(roles,KB,CTX,Toprole,CT),
	!.	
find_role2(roles,KB,CTX,Role,CT) :-
	getRoleName(KB,CTX,CT),
	succ(roles,KB,CTX,Toprole,CT),
	assert1(nsub(roles,KB,CTX,Role,Ct)),
	assert1(nsub(roles,KB,CTX,Ct,Role)),
	assert_succ(roles,KB,CTX,Toprole,Role),
	!.
find_role2(roles,KB,CTX,Role,CT) :-	
	getRoleName(KB,CTX,CT),
	assert1(nsub(roles,KB,CTX,Role,Ct)),
	assert1(nsub(roles,KB,CTX,Ct,Role)),
	assert_succ(roles,KB,CTX,'Entity',Role),
	assert_succ(roles,KB,CTX,'Entity',CT),
	!.	
find_role2(roles,KB,CTX,Role,and([X|[R]])) :-	
	roleEqualSets(KB,user,CTX,Role1,R,_),
	assert_succ(roles,KB,CTX,X,Role),
	assert_succ(roles,KB,CTX,Role1,Role),	
	!. 
find_role2(roles,KB,CTX,Role,and([X|[R]])) :-	
	getRoleName(KB,CTX,X),
	getRoleName(KB,CTX,R),
	assert_succ(roles,KB,CTX,X,Role),
	assert_succ(roles,KB,CTX,R,Role),
	!.
find_role2(roles,KB,CTX,Role,and([Role1|[some(_,Role1)]])) :-	
	assert_succ(roles,KB,CTX,Role1,Role),	
	!.
find_role2(roles,KB,CTX,Role,or([X|[R]])) :-	
	find_role3(KB,CTX,Role,X,R).
find_role2(roles,KB,CTX,Role,and(L)) :-
	find_role21(roles,KB,CTX,Role,L),
	!.
find_role2(roles,KB,CTX,Role,restr(Role1,_Concept)) :-
	assert_succ(roles,KB,CTX,Role1,Role),
	!.


find_role2(roles,KB,CTX,Role,CT) :-
	CT = or([and(L)]),
	find_role26(roles,KB,CTX,Role,L),
	!.
	
find_role2(roles,KB,CTX,Role,or([and(L1)|R])) :-
	find_role30(KB,CTX,Role,R,L1,Z),
	find_role31(KB,CTX,Role,Z),	
	!.
find_role2(roles,KB,CTX,Role,or(L)) :-
	find_role26(roles,KB,CTX,Role,L),
	!.

find_role26(roles,KB,CTX,Role,[C1|R]) :-
	getRoleName(KB,CTX,C1),
	subsume1(roles,KB,CTX,C1,Role),
	assert_succ(roles,KB,CTX,C1,Role),
	find_role26(roles,KB,CTX,Role,R).

find_role30(_KB,_CTX,_Role,[],Z,Z) :- 
	!.
find_role30(KB,CTX,Role,[and(L1)|R],Z,K) :-
	cmp_intersection(Z,L1,Z1),
	find_role30(KB,CTX,Role,R,Z1,K),
	!.

find_role31(_KB,_CTX,_Role,[]) :-
	!.
find_role31(KB,CTX,Role,[L1|R1]) :-
	assert_succ(roles,KB,CTX,L1,Role),
	find_role31(KB,CTX,Role,R1),
	!.


find_role3(KB,CTX,Role,X,R) :-
	X = and([X1|[R1]]),
	subsume1(roles,KB,CTX,Role,R1),
	find_role2(roles,KB,CTX,Role,X).
find_role3(KB,CTX,Role,X,R) :-
	R = and([X1|[R1]]),
	subsume1(roles,KB,CTX,Role,R1),
	find_role2(roles,KB,CTX,Role,X).

find_role21(roles,_KB,_CTX,_Role,[]) :-
	!.
find_role21(roles,KB,CTX,Role,[X|R]) :-
	getRoleName(KB,CTX,X),
	assert_succ(roles,KB,CTX,X,Role),
	find_role21(roles,KB,CTX,Role,R),
	!.
find_role21(roles,KB,CTX,Role,[not(R1)|_R]) :-
	getRoleName(KB,CTX,R1),
	setofOrNil(K,find_role22(roles,KB,CTX,Role,R1,K),L),
	find_role23(roles,KB,CTX,Role,L),
	!.
find_role22(roles,KB,CTX,Role,R1,K) :-
	succ(roles,KB,CTX,K,R1),
	not(succ(roles,KB,CTX,K,Role)).

find_role23(roles,_KB,_CTX,_Role,[]) :-
	!.	
find_role23(roles,KB,CTX,Role,[L1|R1]) :-
	find_role24(roles,KB,CTX,Role,L1),
	find_role23(roles,KB,CTX,Role,R1),
	!.
find_role24(roles,KB,CTX,Role,L1) :-
	succ(roles,KB,CTX,Top,L1),
	succ(roles,KB,CTX,Top,K),
	subsume1(roles,KB,CTX,K,Role),
	setofOrNil(Nf,succ(roles,KB,CTX,K,Nf),Lnf),
	make_succ1(roles,KB,CTX,K,Lnf,Role),
	!.

/******** PrimRollen ************************************************/

find_prole2(roles,KB,CTX,PrimRole,and(L)) :-
	find_prole24(KB,CTX,PrimRole,L),
	!.

find_prole2(roles,KB,CTX,PrimRole,not(X)) :-
	getRoleName(KB,CTX,X),
	cont1a(roles,KB,CTX,X,PrimRole),
	find_prole23(KB,CTX,X,PrimRole,Top),
	assert_succ(roles,KB,CTX,Top,PrimRole),
	assert_succ(roles,KB,CTX,Top,X),
	!.

find_prole2(roles,KB,CTX,PrimRole,CT) :-
	convertCTX(KB,[[],true],CTX,[],[W1,G1],_),
	call(G1),
	getRoleName(KB,CTX,CT),
	assert1(sub(roles,KB,CTX,CT,PrimRole)),
	direct_succ(roles,KB,CTX,[],CT,PrimRole,Z,L1),
              contb(roles,KB,CTX,Z,L1,PrimRole),
	!.

find_prole2(roles,KB,CTX,PrimRole,some(X,Y)) :-
	find_prole21(KB,CTX,PrimRole,X,Y),
	!.

find_prole2(roles,KB,CTX,PrimRole,and([X|[not(Y)]])) :-
	getRoleName(KB,CTX,X),
	getRoleName(KB,CTX,Y),
	find_prole23(KB,CTX,X,Y,Top),
	assert_succ(roles,KB,CTX,Top,PrimRole),	
	!.
	
find_prole2(roles,KB,CTX,Primrole,or([and(L1)|R])) :-
	find_prole3(KB,CTX,Primrole,R,L1,Z),
	find_prole31(KB,CTX,Primrole,Z),	
	!.

find_prole2(roles,KB,CTX,Primrole,or(L)) :-
	find_prole26(roles,KB,CTX,Primrole,L),
	!.


find_prole21(KB,CTX,PrimRole,X,or([Y1|[Y2]])) :-
	roleEqualSets(KB,user,CTX,Role,some(X,Y1),_),
	roleEqualSets(KB,user,CTX,Role1,some(X,Y2),_),
	find_prole23(KB,CTX,Role,Role1,Top),
	assert_succ(roles,KB,CTX,Top,PrimRole),
	!.

find_prole23(KB,CTX,X,Y,X) :-
	sub(roles,KB,CTX,X,Y),
	!.
find_prole23(KB,CTX,X,Y,Y) :-
	sub(roles,KB,CTX,Y,X),
	!.
find_prole23(KB,CTX,X,Y,Top) :-
	sub(roles,KB,CTX,Top,X),sub(roles,KB,CTX,Top,Y).

find_prole24(_KB,_CTX,_PrimRole,[]).
find_prole24(KB,CTX,PrimRole,[X|R]) :-
	getRoleName(KB,CTX,X),
	assert1(sub(roles,KB,CTX,X,PrimRole)),
	direct_succ(roles,KB,CTX,[],X,PrimRole,Z,L1),
              contb(roles,KB,CTX,Z,L1,PrimRole),
	find_prole24(KB,CTX,PrimRole,R),
	!.

find_prole26(roles,KB,CTX,Primrole,[C1|R]) :-
	convertCTX(KB,[[],true],CTX,[],[W1,G1],_),
	call(G1),
	getRoleName(KB,CTX,C1),
	subsume1(roles,KB,CTX,C1,Primrole),
	find_prole27(roles,KB,CTX,Primrole,C1),
  	find_prole26(roles,KB,CTX,Primrole,R).

find_prole27(roles,KB,CTX,Primrole,C1):-
	assert1(sub(roles,KB,CTX,C1,Primrole)),
	direct_succ(roles,KB,CTX,[],C1,Primrole,Z,L1),
              contb(roles,KB,CTX,Z,L1,Primrole),
	!.

find_prole3(_KB,_CTX,_Primrole,[],Z,Z) :- 
	!.
find_prole3(KB,CTX,Primrole,[and(L1)|R],Z,K) :-
	cmp_intersection(Z,L1,Z1),
	find_prole3(KB,CTX,Primrole,R,Z1,K),
	!.

find_prole31(_KB,_CTX,_Primrole,[]) :-
	!.
find_prole31(KB,CTX,Primrole,[L1|R1]) :-
	find_prole27(roles,KB,CTX,Primrole,L1),
	find_prole31(KB,CTX,Primrole,R1),
	!.

/****************************************************************/

make_succ(CTX) :-           
	currentKnowledgeBase(KB),            
              not(make_succ(concepts,KB,CTX)),
	not(make_succ(roles,KB,CTX)),!.
make_succ(concepts,KB,CTX) :-        
	getConceptName(KB,CTX,NewConcept),
	ifOption(testOutput,yes,(cmp_print(NewConcept),cmp_nl)),
	make_succ2(concepts,KB,CTX,NewConcept),
	fail.
make_succ(roles,KB,CTX) :-
	getRoleName(KB,CTX,NewRole),
	ifOption(testOutput,yes,(cmp_print(NewRole),cmp_nl)),
	make_succ2(roles,KB,CTX,NewRole),
       	fail.
make_succ2(Type,KB,CTX,NewConcept) :- 
	NewConcept \== 'Entity',!,
	NewConcept \== bot,!,
%	put_succ(Type,KB,CTX,NewConcept),
	direct_succ(Type,KB,CTX,[],'Entity',NewConcept,X,L),
	contb(Type,KB,CTX,X,L,NewConcept),
	!.

contb(_Type,_KB,_CTX,[],_L,_NewConcept) :- 
        !.

contb(Type,KB,CTX,[X|R],L,NewConcept) :-
        setofOrNil(Y,contc(Type,KB,CTX,X,Y,L),L1),
	list_to_set(L1,L2),
        make_succ1(Type,KB,CTX,X,L2,NewConcept),
        !,
        contb(Type,KB,CTX,R,L,NewConcept).
contb(Type,KB,CTX,X,L,NewConcept) :-
	list_to_set(L,L1),	
        make_succ1(Type,KB,CTX,X,L1,NewConcept),
        !.

contc(Type,KB,CTX,X,Y,L) :-
        sub(Type,KB,CTX,X,Y),
	member(Y,L).

put_succ(Type,KB,CTX,New) :-
	put_below(Type,KB,CTX,[],'Entity',New,IL),
	assert_succs(IL,New,Type,KB,CTX),
	retract_succs(Type,KB,CTX,New).

assert_succs([],New,_Type,_KB,_CTX).
assert_succs([hl_below(Old)|IL],New,Type,KB,CTX) :-
%	not((succ1(Type,KB,CTX,Old,X),member(hl_below(X),IL))),  % ?????
	succ1(Type,KB,CTX,Old,X),
	not(member(hl_below(X),IL)),  % ?????
	!,
	assert1(succ(Type,KB,CTX,Old,New)),
	assert_succs(IL,New,New,Type,KB,CTX).
assert_succs([hl_below(_)|IL],New,New,Type,KB,CTX) :-
	assert_succs(IL,New,New,Type,KB,CTX).
assert_succs([atop(Old)|IL],New,New,Type,KB,CTX) :-
	assert1(succ(Type,KB,CTX,New,Old)),
	assert_succs(IL,New,New,Type,KB,CTX).
assert_succs([aside(_)|IL],New,New,Type,KB,CTX) :-
	assert_succs(IL,New,New,Type,KB,CTX).

retract_succs(Type,KB,CTX,New) :-
	succ1(Type,KB,CTX,C1,New),
	succ1(Type,KB,CTX,New,C2),
	succ1(Type,KB,CTX,C1,C2),
	retract1(succ(Type,KB,CTX,C1,C2)),
	fail.
retract_succs(_Type,_KB,_CTX,_New) :-
	!.

put_below(Type,KB,CTX,Done,Old,New,[hl_below(Old)|IL]) :-
	subsume1(Type,KB,CTX,Old,New),
	!,
	put_where(Type,KB,CTX,[Old|Done],Old,New,IL).
put_below(Type,KB,CTX,Done,Old,New,[atop(Old)]) :-
	subsume1(Type,KB,CTX,New,Old),
	!.
put_below(Type,KB,CTX,Done,Old,New,[aside(Old)]) :-
	setofOrNil(Y,(succ1(Type,KB,CTX,Old,Y), not(member(Y,[Old|Done]))),L),
	put_aside_list(Type,KB,CTX,[Old|Done],L,New,IL).

put_where(Type,KB,CTX,Done,Old,New,[]) :-
	subsume1(Type,KB,CTX,New,Old),
	!,
	succ(concepts,KB,CTX,Topconcept,Old),
	assert1(nsub(concepts,KB,CTX,Old,New)),
	assert1(nsub(concepts,KB,CTX,New,Old)),
	assert_succ(concepts,KB,CTX,Topconcept,New).
put_where(Type,KB,CTX,Done,Old,New,IL) :-
	setofOrNil(Y,(succ1(Type,KB,CTX,Old,Y), not(member(Y,[Old|Done]))),L),
	put_below_list(Type,KB,CTX,Done,L,New,IL).

put_aside_list(Type,KB,CTX,Done,[Old|L],New,[atop(Old)|IL]) :-
	subsume1(Type,KB,CTX,New,Old),
	put_aside_list(Type,KB,CTX,[Old|Done],L,New,IL).
put_aside_list(Type,KB,CTX,Done,[Old|L],New,IL) :-
	setofOrNil(Y,(succ1(Type,KB,CTX,Old,Y), not(member(Y,[Old|Done]))),L1),
	put_aside_list(Type,KB,CTX,[Old|Done],L1,New,IL1),
	put_aside_list(Type,KB,CTX,[Old|Done],L,New,IL2),
	cmp_union(IL1,IL2,IL).
put_aside_list(Type,KB,CTX,Done,[],New,[]).

put_below_list(Type,KB,CTX,Done,[Old|L],New,IL) :-
	put_below(Type,KB,CTX,Done,Old,New,IL1),
	put_below_list(Type,KB,CTX,[Old|Done],L,New,IL2),
	cmp_union(IL1,IL2,IL).
put_below_list(Type,KB,CTX,Done,[],New,[]).


direct_succ(_Type,_KB,_CTX,_Done,bot,_X,_,[]) :- 
	fail.
direct_succ(Type,KB,CTX,Done,X,NewConcept,Z,L1) :-
	subsume1(Type,KB,CTX,X,NewConcept),
	setofOrNil(Y,(succ1(Type,KB,CTX,X,Y), not(member(Y,[X|Done]))),L),
	!,
	check(Type,KB,CTX,[X|Done],L,_,X,NewConcept,Z,L1),
	!.
direct_succ(Type,KB,CTX,Done,X,NewConcept,Z,L1) :- 
	setofOrNil(Y,(succ1(Type,KB,CTX,X,Y), not(member(Y,[X|Done]))),L),
	check(Type,KB,CTX,[X|Done],L,_,'Entity',NewConcept,Z,L1),
	!.

check(Type,KB,CTX,Done,[Y|L],L2,X,NewConcept,Z,L1) :-
        subsume1(Type,KB,CTX,Y,NewConcept),
	!,
        direct_succ(Type,KB,CTX,Done,Y,NewConcept,Z1,L10),
	!,        	 				 	 	
	conta(Type,KB,CTX,[Y|Done],L,L2,X,NewConcept,Z1,L10,Z,L1),
        !.
check(Type,KB,CTX,Done,[Y|L],L2,X,NewConcept,Z,L1) :-
	!,
	check(Type,KB,CTX,[Y|Done],L,[Y|L2],X,NewConcept,Z,L1).
check(Type,KB,CTX,Done,[],L2,X,NewConcept,X,L1) :-
	check1(Type,KB,CTX,Done,L2,NewConcept,L1),
	!.

conta(_Type,_KB,_CTX,_Done,[],_L2,_X,_NewConcept,Z1,L10,Z1,L10) :-
        !.
conta(Type,KB,CTX,Done,L,L2,X,NewConcept,Z1,L10,Z,L1) :-
        check(Type,KB,CTX,Done,L,L2,X,NewConcept,Z2,L11),
 	union1(Z1,Z2,Za),
	delete1(Za,'Entity',Z),
	union1(L10,L11,L1),
        !.

check1(_,_,_,_,[],_,[]) :- !.
check1(Type,KB,CTX,Done,[Y|L],NewConcept,[Y|L1]) :-
	subsume1(Type,KB,CTX,NewConcept,Y),
	!,
	check1(Type,KB,CTX,[Y|Done],L,NewConcept,L1).
check1(Type,KB,CTX,Done,[Y|L],NewConcept,L1) :-
	not(member(Y,Done)),
	setofOrNil(Z,succ1(Type,KB,CTX,Y,Z),L2),
	check1(Type,KB,CTX,[Y|Done],L2,NewConcept,L3),
	check1(Type,KB,CTX,[Y|Done],L,NewConcept,L4),
	cmp_union(L3,L4,L5),
	deletoneList(L5,'Entity',L1),
	!.
check1(Type,KB,CTX,Done,[Y|L],NewConcept,L1) :-
	check1(Type,KB,CTX,[Y|Done],L,NewConcept,L1),
	!.

%% make_succ1(Type,KB,CTX,X,[Y|L],NewConcept) :- 
%% 	not(succ(Type,KB,CTX,NewConcept,Y)),
%%         retract1(succ(Type,KB,CTX,X,Y)),
%% %	assert1(succ(Type,KB,CTX,NewConcept,Y)),
%%         assert_succ(Type,KB,CTX,NewConcept,Y),
%% 	!,
%% 	make_succ1(Type,KB,CTX,X,L,NewConcept). 
%% make_succ1(Type,KB,CTX,X,[Y|L],NewConcept) :- 
%% % 	assert1(succ(Type,KB,CTX,x,NewConcept)),
%%         assert_succ(Type,KB,CTX,X,NewConcept),
%% 	!,
%% 	make_succ1(Type,KB,CTX,X,L,NewConcept).
%% make_succ1(Type,KB,CTX,X,[],NewConcept) :- 
%% %	assert1(succ(Type,KB,CTX,X,NewConcept)),
%%         assert_succ(Type,KB,CTX,X,NewConcept),
%% 	!.

make_succ1(Type,KB,CTX,X,[Y|L],NewConcept) :- 
	succ(Type,KB,CTX,X,Y),
	!,
	retract1(succ(Type,KB,CTX,X,Y)),
	assert1(succ(Type,KB,CTX,NewConcept,Y)),
	make_succ1(Type,KB,CTX,X,L,NewConcept).
make_succ1(Type,KB,CTX,X,[Y|L],NewConcept) :- 
	!,
	assert1(succ(Type,KB,CTX,NewConcept,Y)),
	make_succ1(Type,KB,CTX,X,L,NewConcept).
make_succ1(Type,KB,CTX,X,[],NewConcept) :- 
	assert_succ(Type,KB,CTX,X,NewConcept),
 	!.


/****************  practical funktions ******************************/	

subsume1(_Type,_KB,_CTX,X,_Y) :- isSlot(X),!,fail.
subsume1(_Type,_KB,_CTX,_X,Y) :- isSlot(Y),!,fail.
subsume1(_Type,_KB,_CTX,_X,'Entity') :- !,fail.
subsume1(_Type,_KB,_CTX,bot,_X) :- !,fail.
subsume1(_Type,_KB,_CTX,_X,[]) :- !.
subsume1(_Type,_KB,_CTX,_X,bot) :- !.
subsume1(_Type,_KB,_CTX,'Entity',_X) :- !.
subsume1(Type,KB,CTX,X,Y) :- 
	sub(Type,KB,CTX,X,Y),
	!.
subsume1(Type,KB,CTX,X,Y) :- 
	nsub(Type,KB,CTX,X,Y),
	!,
	fail. 
subsume1(Type,KB,CTX,X,Y) :- 
	X \== Y,
	addCounter(Type,1),
	ifOption(testOutput,yes,(getRuntime(T1),cmp_format('~3d: ~a >= ~a',[T1,X,Y]))),
	subsumes(Type,KB,CTX,X,Y), 
	ifOption(testOutput,yes,(getRuntime(T1),cmp_format(' succeeded (~3d)~n',[T1]))),
	cont(Type,KB,CTX,[],X,Y),
	!.
subsume1(Type,KB,CTX,X,Y) :- 
	X \== Y,
	ifOption(testOutput,yes,(getRuntime(T1),cmp_format(' failed (~3d)~n',[T1]))),
	cont1a(Type,KB,CTX,[],X,Y),
	!,
	fail.

cont(_Type,_KB,_CTX,_,'Entity',_Y).
cont(Type,KB,CTX,Done,X,Y) :- 
	assert1(sub(Type,KB,CTX,X,Y)),
	succ1(Type,KB,CTX,Z,X),
	not(member(Z,Done)),
	cont(Type,KB,CTX,[Z|Done],Z,Y),!.
cont(_Type,_KB,_CTX,_,_X,_Y). 
cont1a(_Type,_KB,_CTX,_,bot,_X) :- 
	!.
cont1a(_Type,_KB,_CTX,_,_X,bot) :- 
	!,fail.
cont1a(_Type,_KB,_CTX,Done,X,_Y) :-
       member(X,Done), 
       !.
cont1a(Type,KB,CTX,Done,X,Y) :-
	assert1(nsub(Type,KB,CTX,X,Y)),
	succ1(Type,KB,CTX,X,Z),
	cont1a(Type,KB,CTX,[X|Done],Z,Y),
	!.

delete1([X|R],'Entity',Z) :-
	deletoneList([X|R],'Entity',Z),
	!.
delete1(_X,'Entity',_Z) :-
	!.

union1([],[],[]).
union1([X|R],[Y|R1],Z):-
	cmp_union([X|R],[Y|R1],Z),
	!.
union1([X|R],Y,Z) :-
	cmp_union([X|R],[Y],Z),
	!.
union1([X],Y,Z) :-
	cmp_union([X],[Y],Z),
	!.
union1(X,[Y],Z) :-
	cmp_union([X],[Y],Z),
	!.
union1(X,[Y|R],Z) :-
	cmp_union([X],[Y|R],Z),
	!.
union1(X,Y,Z) :-
	cmp_union([X],[Y],Z),
	!.
assert1(G) :- 
	not(G),
	assert(G),
	!.
assert1(_G) :-
	!.

assert_succ(_Type,_KB,_CTX,X,X) :-
	!.
/*
assert_succ(Type,KB,CTX,X,RorC) :-
	assert1(succ(Type,KB,CTX,X,RorC)),
	cont(Type,KB,CTX,[],X,RorC),
	!.
*/
assert_succ(Type,KB,CTX,X,RorC) :-
	cont(Type,KB,CTX,[],X,RorC),
	not((sub(Type,KB,CTX,X,Y),not(isSlot(Y)),sub(Type,KB,CTX,Y,RorC),Y \== RorC)),
	assert1(succ(Type,KB,CTX,X,RorC)),
	!.
assert_succ(_Type,_KB,_CTX,_X,_RorC).


retract1(G) :- 
	retract(G),
	!.
retract1(_G) :- 
	!.

succ1(Type,KB,CTX,X,Y) :- 
	succ(Type,KB,CTX,X,Y).
%	!.
succ1(_Type,_KB,_CTX,_X,bot).
% 	:-  !.

/*****************************************************************************/
/***************** cmp_print and statistic - functions ***************************/
newShowHierarchy :-
	show_dag.

show_dag :-
	currentKnowledgeBase(KB),
	show_dag(KB,[]).
show_dag(CTX) :-
	currentKnowledgeBase(KB),
	show_dag(KB,CTX).
show_dag(KB,CTX) :-
	!,
	cmp_print('Concepts\n'),
        not(show_dag(concepts,KB,CTX,'Entity',[])),cmp_nl,cmp_nl,
	cmp_print('Roles\n'),
	not(show_dag(roles,KB,CTX,'Entity',[])).
show_dag(_Type,_KB,_CTX,bot,_) :- !,fail.
show_dag(Type,KB,CTX,Node,L) :-
	writes(L),
	cmp_print(Node),cmp_nl,
	succ(Type,KB,CTX,Node,N),
  	show_dag(Type,KB,CTX,N,[45|L]),
	fail.

initStat :-
	!,
	setCounter(subsumptionTests,0),
	setCounter(concepts,0),
	setCounter(roles,0),
	setCounter(conceptsClassified,0),
	setCounter(rolesClassified,0),
	getRuntime(T0),
	setCounter(runtime,T0),
	!.
getStat(CN,CST,RN,RST,T) :-
	!,
	getRuntime(T1),
	getCounter(subsumptionTests,ST),
	getCounter(concepts,CST),
	getCounter(conceptsClassified,CN),
	getCounter(roles,RST),
	getCounter(rolesClassified,RN),
	getCounter(runtime,T0),
	T is T1 - T0,
	!.
printStat :-
	!,
	getStat(CN,CST,RN,RST,T),
	cmp_format('Concepts classified:         ~d~n',CN),
	cmp_format('Subsumption tests performed: ~d~n',CST),
	cmp_format('Roles    classified:         ~d~n',RN),
	cmp_format('Subsumption tests performed: ~d~n',RST),
	cmp_format('Total runtime:               ~3d sec.~2n',T),
	!.

buildOrdering(KB,CTX,CTree,RTree) :- 
	buildOrdering(concepts,KB,CTX,'Entity',[],CTree),
	buildOrdering(roles,KB,CTX,'Entity',[],RTree),
	!.


buildOrdering(Type,KB,CTX,bot,_Done,hl_node([bot|EquivClass],[])) :-
	!,
	setofOrNil(Z2,(succ(Type,KB,CTX,bot,Z2),succ(Type,KB,CTX,Z2,bot)),EquivClass),
	!.
buildOrdering(Type,KB,CTX,Concept1,Done,hl_node([Concept1|EquivClass],SubtreeList)) :-
	setofOrNil(Z1,succ(Type,KB,CTX,Concept1,Z1),S1),
	setofOrNil(Z2,(succ(Type,KB,CTX,Concept1,Z2),succ(Type,KB,CTX,Z2,Concept1)),EquivClass),
	successorSet(S1,EquivClass,Succ),
	append(Done,[Concept1|EquivClass],Done1),
	buildOrderingList(Type,KB,CTX,Succ,Done1,SubtreeList).

buildOrderingList(_Type,_KB,_CTX,[],_Done,[]) :-
	!.
buildOrderingList(Type,KB,CTX,[C1|CL],Done,SubtreeList) :-
	member(C1,Done),
	!,
	buildOrderingList(Type,KB,CTX,CL,Done,SubtreeList).
buildOrderingList(Type,KB,CTX,[C1|CL],Done,[Subtree|SubtreeList]) :-
	buildOrdering(Type,KB,CTX,C1,Done,Subtree),
	buildOrderingList(Type,KB,CTX,CL,Done,SubtreeList),
	!.

successorSet(S1,EquivClass,S2) :-
	successor_set(S1,EquivClass,S3),
	((S3 \== [], S2 = S3) ; (S2 = [bot])),
	!.

successor_set([],_,[]) :-
	!.
successor_set([C1|CL],EquivClass,S2) :-
	member(C1,EquivClass),
	!,
	successor_set(CL,EquivClass,S2).
successor_set([bot|CL],EquivClass,S2) :-
	!,
	successor_set(CL,EquivClass,S2).
successor_set([C1|CL],EquivClass,[C1|S2]) :-
	successor_set(CL,EquivClass,S2).
/**********************************************************************
 *
 * @(#) compileKB.pll 1.9@(#)
 *
 */

/**********************************************************************
 *
 * compileKnowledgeBase(FileName)
 * 
 */

compileKnowledgeBase(FileName) :-
	see(FileName),
	read(knowledgeBase(KBName,_KB,_Comment)),
	seen,
	compileKnowledgeBase(FileName,KBName),
	!.
compileKnowledgeBase(FileName) :-
	% Some file handling error has occured
	seen,
	!, 
	fail.

compileKnowledgeBase(FileName,KBName) :-
	see(FileName),
	read(knowledgeBase(_KBName,KB,Comment)),
	(removeKnowledgeBase(KBName) ; true),
	termExpansion(on,KB,CPList),
	tell('compile.tmp'),
	cmp_write((:- dynamic(constraint/8))), cmp_write('.\n'),
	cmp_write((:- dynamic(numb/1))), cmp_write('.\n'),
%	cmp_write((:- dynamic(instance/9))), cmp_write('.\n'),
%	cmp_write((:- dynamic(kb_in/10))), cmp_write('.\n'),
	cmp_write((:- dynamic(falsum/2))), cmp_write('.\n'),
%	cmp_write((:- dynamic(conceptName/4))), cmp_write('.\n'),
%	cmp_write((:- dynamic(roleName/4))), cmp_write('.\n'),
%	cmp_write((:- dynamic(conceptEqualSets/6))), cmp_write('.\n'),
	cmp_write((:- dynamic(conceptSubsets/6))), cmp_write('.\n'),
%	cmp_write((:- dynamic(eq/9))), cmp_write('.\n'),
	cmp_write((:- dynamic(inconsistencyCheck/3))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleEqualSets/6))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleSubsets/6))), cmp_write('.\n'),
	cmp_write((:- dynamic(conceptElement/7))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleElement/8))), cmp_write('.\n'),
	cmp_write((:- dynamic(hl_closed/5))), cmp_write('.\n'),
	cmp_write((:- dynamic(sub/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(succ/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(nsub/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(sub3/2))), cmp_write('.\n'),
	cmp_write((:- dynamic(succ3/2))), cmp_write('.\n'),
	cmp_write((:- dynamic(nsub3/2))), cmp_write('.\n'),
	cmp_write((:- dynamic(abductiveDerivation/3))), cmp_write('.\n'),
	cmp_write((:- dynamic(consistencyDerivation/3))), cmp_write('.\n'),
	cmp_write((:- dynamic(hypothesis/1))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleDomain/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleRange/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleDefault/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleNr/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleDefNr/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(roleHl_attributes/5))), cmp_write('.\n'),
%	cmp_write((:- dynamic(given_inflLink/4))), cmp_write('.\n'),
%	cmp_write((:- dynamic(given_change/4))), cmp_write('.\n'),
	cmp_write((:- dynamic(value/2))), cmp_write('.\n'),
	cmp_write((:- dynamic(option/2))), cmp_write('.\n'),
%	cmp_write((:- dynamic(knowledgeBase/3))), cmp_write('.\n'),
%	cmp_write((:- dynamic(conceptHierarchy/3))), cmp_write('.\n'),
%	cmp_write((:- dynamic(roleHierarchy/3))), cmp_write('.\n'),
	cmp_write((:- dynamic(modalAxiom/6))), cmp_write('.\n'),
%	cmp_write((:- dynamic(hl_holds/5))), cmp_write('.\n'),
	cmp_write((:- dynamic(compiledPredicate/2))), cmp_write('.\n'),
	cmp_writeq((:- asserta(knowledgeBase(KBName,KB,Comment)))), cmp_write('.\n'),
	cmp_writeq((:- retractall(currentKnowledgeBase(_)))), cmp_write('.\n'),
	cmp_writeq((:- asserta(currentKnowledgeBase(KB)))), cmp_write('.\n'),
	writeCompiledPredicateFactsToFile(KB,CPList),
	expand_term((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),
                        ab(D),call(CALL),PT) :-
	                   kb_in(KB,pr(5),Name,modal(CTX),CN,CON,hyp(HYP),
                                 ab(D),call(CALL),PT)),
		    InClause1),
	cmp_writeq(InClause1), cmp_write('.\n'),
	expand_term((instance(KB,Name,modal(CTX),CN,CON,
                        hyp([or(H1),rl(H2),fl(H3)]),ab(noAb),call(CALL),PT) :-
		           clashInHyp(H2), !, fail),
		    InClause2),
	cmp_writeq(InClause2), cmp_write('.\n'),
	expand_term(instance(KB,X2,X3,X4,X5,X6,X7,X8,X9), Head3),
	cmp_writeq((Head3 :- kb_in(KB,pr(3),X2,X3,X4,X5,X6,X7,X8,X9))),
	cmp_write('.\n'),
	expand_term((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),
                        ab(D),call(CALL),PT) :-
		          (CN \== 'Entity', CN \== bot, CN \== not('Entity'), 
                           CN \== not(bot),
	                   kb_in(KB,pr(3),Name,modal(CTX),CN,CON,hyp(HYP),
                                 ab(D),call(CALL),PT))),
		    InClause4),
	cmp_writeq(InClause4), cmp_write('.\n'),
	expand_term((instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),
                        ab(D),call(CALL),PT) :-
		          (CN \== 'Entity',CN \== bot, CN \== not('Entity'), 
                           CN \== not(bot),
			   kb_in(KB,pr(1),Name,modal(CTX),CN,CON,hyp(HYP),
				 ab(D),call(CALL),PT))),
		    InClause5),
	cmp_writeq(InClause5), cmp_write('.\n'),
	repeat,
	read(Clause),
	treatClause(Clause),
	seen,
	told,
	assertConnectionClauses(KB),
	termExpansion(off,KB),
	compile('compile.tmp'),
	!.
compileKnowledgeBase(FileName,KBName) :-
	% Some file handling error has occured
	seen,
	told,
	!,
	fail.

treatClause('end_of_file') :-
	!.
treatClause((:-dynamic Pred/Arity)) :-
%	cmp_write((:-dynamic Pred/Arity)), cmp_write('.\n'),
	!,
	fail.
treatClause((instance(_X1,_X2,_X3,_X4,_X5,_X6,_X7,_X8,_X9) :- _Body)) :-
	!,
	fail.
treatClause(X) :-
	expand_term(X,Y),
	cmp_writeq(Y), cmp_write('.\n'),
	!,
	fail.

writeCompiledPredicateFactsToFile(KB,[]) :-
	!.
writeCompiledPredicateFactsToFile(KB,[Pred/Arity|List]) :-
	cmp_writeq((compiledPredicate(KB,Pred/Arity))),
	cmp_write('.\n'),
	writeCompiledPredicateFactsToFile(KB,List).

assertConnectionClauses(KB) :-
	expand_term(constraint(KB,X2,X3,X4,X5,X6,X7,X8),CompConAtom),
	assertz((constraint(KB,X2,X3,X4,X5,X6,X7,X8) :-
		 CompConAtom)),
	expand_term(eq(KB,X2,X3,X4,X5,X6,X7,X8,X9),CompEqAtom),
	assertz((eq(KB,X2,X3,X4,X5,X6,X7,X8,X9) :-
		 CompEqAtom)),
	expand_term(instance(KB,X2,X3,X4,X5,X6,X7,X8,X9),CompInAtom),
	assertz((instance(KB,X2,X3,X4,X5,X6,X7,X8,X9) :-
		 CompInAtom)),
%	assertz((kb_in(KB,X2,X3,X4,X5,X6,X7,X8,X9,X10) :-
%		 comp_kb_in(KB,X2,X3,X4,X5,X6,X7,X8,X9,X10))),
	expand_term(hl_holds(KB,X2,X3,X4,X5),CompRelAtom),
	assertz((hl_holds(KB,X2,X3,X4,X5) :-
		 CompRelAtom)),
	!.

termExpansion(on,kb(Id),
              [CompCon/8,CompEq/9,CompIn/9,CompKb_in/10,CompRel/6]) :-
	% Generate the names for the compiled instance, kb_in, constraint, and hl_holds
	% predicates instance knowledgeBase Id.
	atom_codes(Id,IdChars),
	atom_codes(instance,InChars),
	append(InChars,[95,99,95|IdChars],CompInChars),
	atom_codes(CompIn,CompInChars),
	atom_codes(constraint,ConChars),
	append(ConChars,[95,99,95|IdChars],CompConChars),
	atom_codes(CompCon,CompConChars),
	atom_codes(eq,EqChars),
	append(EqChars,[95,99,95|IdChars],CompEqChars),
	atom_codes(CompEq,CompEqChars),
	atom_codes('kb_in',Kb_inChars),
	append(Kb_inChars,[95,99,95|IdChars],CompKb_inChars),
	atom_codes(CompKb_in,CompKb_inChars),
	atom_codes('hl_holds',RelChars),
	append(RelChars,[95,99,95|IdChars],CompRelChars),
	atom_codes(CompRel,CompRelChars),
	% Abolish any previously asserted clauses for the 
	% compiled predicades
	abolish(CompCon/8),
	abolish(CompEq/9),
	abolish(CompIn/9),
	abolish(CompKb_in/10),
	abolish(CompRel/6),
	% Generate the atoms for these predicates 
	CompConAtom =.. [CompCon|[X4,X1,X2,X3,X5,X6,X7,X8]],
	CompEqAtom =.. [CompEq|[X4-X5,X1,X2,X3,X6,X7,X8,X9]],
	CompInAtom =.. [CompIn|[X4-X5,X1,X2,X3,X6,X7,X8,X9]],
	CompKb_inAtom =.. [CompKb_in|[X5-X6,X1,X2,X3,X4,X7,X8,X9,X10]],
	CompRelAtom =.. [CompRel|[X1,X2,X3,X4,X5,X6]],
	% Assert the term_expansion rules needed to translate the
	% interpreted clauses into compiled clauses.
	abolish(term_expansion/2),
	assertz((term_expansion((Head :- Body),(Head1 :- Body1)) :-
	term_expansion(Head,Head1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L, Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L; Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion(\+Atom,\+Atom1) :-
	term_expansion(Atom,Atom1))),
	assertz((term_expansion(constraint(X1,X2,X3,X4,X5,X6,X7,X8),
				CompConAtom))),
	assertz((term_expansion(eq(X1,X2,X3,X4,X5,X6,X7,X8,X9),
				CompEqAtom))),
	assertz((term_expansion(instance(X1,X2,X3,X4,X5,X6,X7,X8,X9),
				CompInAtom))),
	assertz((term_expansion(kb_in(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10),
				CompKb_inAtom))),
	assertz((term_expansion(hl_holds(X1,X2,X3,X4,X5),
				CompRelAtom))),
	assertz((term_expansion(once(Body1),once(Body2)) :-
		term_expansion(Body1,Body2))),
	assertz((term_expansion(call(Body1),call(Body2)) :-
		 term_expansion(Body1,Body2))),
	assertz(term_expansion(X,X)),
	!.
termExpansion(off,_) :-
	abolish(term_expansion/2),
	!.

/**********************************************************************
 *
 * @(#) cnf.pll 1.4@(#)
 *
 */

/***********************************************************************
 *
 * negate(+C1,-C2)
 * C2 is just the term not(C1).
 *
 */

negate(not(C1),C1) :- !.
negate(C1,not(C1)) :- !.

/***********************************************************************
 *
 * invert(+R1,-R2)
 * R2 is just the term inverse(R1).
 *
 */

invert(inverse(R),R) :- !.
invert(R,inverse(R)) :- !.

/***********************************************************************
 *
 * normalizeNot(+C1,-C2)
 * applies the laws
 *      not(and([A,B]))   -> and([not(A),not(B)])
 *      not(or([A,B]))    -> or([not(A),not(B)])
 *      not(not(A))       -> A
 *      not(all(R,C))     -> some(R,not(C))
 *      not(some(R,C))    -> all(R,not(C))
 *      not(atleast(N,R)) -> atmost(N-1,R)
 *      not(atmost(N,R))  -> atleast(N+1,R)
 *      not(b(O,P,C))     -> d(O,P,not(C))
 *      not(d(O,P,C))     -> b(O,P,not(C))
 * to C1 as long as possible to get C2.
 *
 */

normalizeNot(not(and([C1,C2|L1])),or(L3)) :-
	!,
	hop_map(negate,[C1,C2|L1],L2),
	hop_map(normalizeNot,L2,L3).
normalizeNot(not(and([C1])),C3) :-
	negate(C1,C2),
        normalizeNot(C2,C3).
normalizeNot(not(and([])),'bot') :-
	!.
normalizeNot(not(set(L)),not(set(L))) :-
	!.
normalizeNot(not(or([C1,C2|L1])),and(L3)) :-
	!,
	hop_map(negate,[C1,C2|L1],L2),
	hop_map(normalizeNot,L2,L3).
normalizeNot(not(or([C1])),C3) :-
	negate(C1,C2),
        normalizeNot(C2,C3).
normalizeNot(not(or([])),'Entity') :-
	!.
normalizeNot(not(all(R,C1)),some(R,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(some(R,C1)),all(R,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(atleast(N,R)),atmost(M,R)) :-
	M is N-1.
normalizeNot(not(atmost(N,R)),atleast(M,R)) :-
	M is N+1.
normalizeNot(not(b(O,P,C1)),d(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(d(O,P,C1)),b(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(bc(O,P,C1)),dc(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(dc(O,P,C1)),bc(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(b(O,P)),d(O,P)) :-
	!.
normalizeNot(not(d(O,P)),b(O,P)) :-
	!.
normalizeNot(not(bc(O,P)),dc(O,P)) :-
	!.
normalizeNot(not(dc(O,P)),bc(O,P)) :-
	!.
normalizeNot(not(not(C1)),C3) :-
	!,
	normalizeNot(C1,C3).
normalizeNot(not(set([])),'Entity') :- !.
normalizeNot(not('Entity'),bot) :- !.
normalizeNot(not(bot),'Entity') :- !.
normalizeNot(C1,C1).

/***********************************************************************
 *
 * normalizoneverse(+R1,-R2)
 * applies the laws
 *      inverse(and([R,S])) -> and([inverse(R),inverse(S)])
 *      inverse(inverse(R)) -> R
 * to R1 as long as possible to get R2.
 *
 */

normalizoneverse(inverse(and(RL1)),and(RL3)) :-
	hop_map(invert,RL1,RL2),
	hop_map(normalizoneverse,RL2,RL3),
	!.
normalizoneverse(inverse(inverse(R1)),R3) :-
	normalizoneverse(R1,R3).
normalizoneverse(R1,R1).

/***********************************************************************
 *
 * cmp_flatten(+C1,-C2)
 * deletes unnecessary occurrences of `and' and `or' instance C1 to get C2.
 *
 */

cmp_flatten(and(L1),and(L2)) :-
	!,
	hop_map(cmp_flatten,L1,L3),
	flattenAnd([],L3,L2).
cmp_flatten(or(L1),or(L2)) :-
	!,
	hop_map(cmp_flatten,L1,L3),
	flattenOr([],L3,L2).
cmp_flatten(set(L1),set(L1)) :-
	!.
cmp_flatten(all(R1,C1),all(R2,C2)) :-
	cmp_flatten(R1,R2),
	cmp_flatten(C1,C2).
cmp_flatten(some(R1,C1),some(R2,C2)) :-
	cmp_flatten(R1,R2),
	cmp_flatten(C1,C2).
cmp_flatten(atleast(N,R1),atleast(N,R2)) :-
	cmp_flatten(R1,R2).
cmp_flatten(atmost(N,R1),atmost(N,R2)) :-
	cmp_flatten(R1,R2).
cmp_flatten(b(O,P,C1),b(O,P,C2)) :-
	cmp_flatten(C1,C2).
cmp_flatten(d(O,P,C1),d(O,P,C2)) :-
	cmp_flatten(C1,C2).
cmp_flatten(bc(O,P,C1),bc(O,P1,C2)) :-
	cmp_flatten(P,P1),
	cmp_flatten(C1,C2).
cmp_flatten(dc(O,P,C1),dc(O,P1,C2)) :-
	cmp_flatten(P,P1),
	cmp_flatten(C1,C2).
cmp_flatten(not(C1),not(C2)) :-
	!,
	cmp_flatten(C1,C2).
cmp_flatten(inverse(R1),inverse(R2)) :-
	cmp_flatten(R1,R2).
cmp_flatten(C1,C1).


/***********************************************************************
 *
 * flattenAnd(+L1,+L2,-L3)
 * eliminates occurrences of `and' instance L2 to get L2'. L3 is the result
 * of appending L2' to L1.
 *
 */

flattenAnd(L1,[and(L2)|L3],L4) :-
	!,
%	flattenAnd([],L2,L5),
	L5 = L2,
	append(L1,L5,L6),
	flattenAnd(L6,L3,L4).
flattenAnd(L1,[C1|L3],L4) :-
	append(L1,[C1],L6),
	flattenAnd(L6,L3,L4).
flattenAnd(L1,[],L1).

/***********************************************************************
 *
 * flattenOr(+L1,+L2,-L3)
 * eliminates occurrences of `or' instance L2 to get L2'. L3 is the result
 * of appending L2' to L1.
 *
 */
 
flattenOr(L1,[or(L2)|L3],L4) :-
	!,
%	flattenOr([],L2,L5),
	L5 = L2,
	append(L1,L5,L6),
	flattenOr(L6,L3,L4).
flattenOr(L1,[C1|L3],L4) :-
	append(L1,[C1],L6),
	flattenOr(L6,L3,L4).
flattenOr(L1,[],L1).

/***********************************************************************
 *
 * distributeAnd(and(+L1),or(+L2),or(-L3))
 * here or(L3) has the form
 *     or([C_1,...,C_n])
 * where C_i is the result of applying de Morgan's laws to
 * and(L1|[A_i]) 
 * where A_i is the ith element of L2.
 *
 */

distributeAnd(and(L1),or([C2|L2]),or([C3|L4])) :-
	% L3 := L1 and C2
	append(L1,[C2],L3),
	% C3 := deMorganAnd(L3)
	deMorgan(and(L3),C3),
	% build other disjuncts
	distributeAnd(and(L1),or(L2),or(L4)).
distributeAnd(and(_L1),or([]),or([])).

/***********************************************************************
 *
 * distributeOr(or(+L1),and(+L2),and(-L3))
 * here and(L3) has the form
 *     and([C_1,...,C_n])
 * where C_i is the result of applying de Morgan's laws to
 * or(L1|[A_i]) 
 * where A_i is the ith element of L2.
 *
 */
	
distributeOr(or(L1),and([C2|L2]),and([C3|L4])) :-
	% L3 := L1 or C2
	append(L1,[C2],L3),
	% C3 := deMorgan(L3)
	deMorgan(or(L3),C3),
	% build other conjuncts
	distributeOr(or(L1),and(L2),and(L4)).
distributeOr(or(_L1),and([]),and([])).
	
/***********************************************************************
 *
 * deMorganAnd(+L1,+L2,-C1)
 * applies de Morgan's law
 *      and([A,or([B,C])]) -> or([and([A,B]),and([A,C])])
 * to and(L1|L2) as long as possible to get C1.
 *
 */

deMorganAnd(L1,[or(L2)|L3],L4) :-
	append(L1,L3,L5),
	distributeAnd(and(L5),or(L2),L4).
deMorganAnd(L1,[C1|L3],L4) :-
	append(L1,[C1],L5),
	deMorganAnd(L5,L3,L4).
deMorganAnd(L1,[],and(L1)).

/***********************************************************************
 *
 * deMorganOr(+L1,+L2,-C1)
 * applies de Morgan's law
 *      or([A,or([B,C])]) -> and([or([A,B]),or([A,C])])
 * to or(L1|L2) as long as possible to get C1.
 *
 */

deMorganOr(L1,[and(L2)|L3],L4) :-
	append(L1,L3,L5),
	distributeOr(or(L5),and(L2),L4).
deMorganOr(L1,[C1|L3],L4) :-
	append(L1,[C1],L5),
	deMorganOr(L5,L3,L4).
deMorganOr(L1,[],or(L1)).

/***********************************************************************
 *
 * deMorgan(+C1,-C2)
 * applies de Morgan's laws to C1
 *      and([A,or([B,C])]) -> or([and([A,B]),and([A,C])])
 *      or([A,and([B,C])]) -> and([or([A,B]),or([A,C])])
 * as long as possible to get C2.
 *
 */

deMorgan(and(L1),C1) :-
	deMorganAnd([],L1,C1).
deMorgan(or(L1),C1) :-
	deMorganOr([],L1,C1).
deMorgan(C1,C1) :-
	!.

/***********************************************************************
 *
 * cnf(+C1,-C2)
 * C2 is the conjunctive normalform of C1.
 *
 */

cnf(C1,C6) :-
	normalizeNot(C1,C2),
	cmp_flatten(C2,C3),
	normalizoneverse(C3,C4),
	deMorgan(C4,C5),
	cmp_flatten(C5,C6).

/**********************************************************************
 *
 * @(#) conceptFunctions.pll 1.5@(#)
 *
 */

/***********************************************************************
 *
 * memberConcept(+Concept,+Dag)
 * Arguments: Concept     concept name
 *            Dag         subsumption hierarchy
 * checks wether or not Concept occurs instance the subsumption hierarchy.
 *
 */

memberConcept(Concept,Dag) :-
	memberElement(Concept,Dag).

memberConceptSubtrees(Concept,List) :-
	memberElementSubtrees(Concept,List).

/***********************************************************************
 *
 * memberDirectSubConcepts(+Concept,+Dag)
 * Arguments: Concept     concept name
 *            Dag         subsumption hierarchy
 * checks wether or not Concept occurs instance the direct subconcepts of
 * the 'Entity' concept of Dag.
 *
 */

memberDirectSubConcepts(Concept,hl_node(_CL,NL)) :-
	!,
	memberDirectSubElements(Concept,NL).

memberDirectSubConcepts(Concept,List) :-
	memberDirectSubElements(Concept,List).

/***********************************************************************
 *
 * getDirectSuperConcepts(+KBName,+CTX,+Concept,-CL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are direct super concepts
 * of Concept.
 *
 */

getDirectSuperConcepts(KBName,CTX,Concept,CL) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	getDirectSuperElements(Concept,CL,Dag).


/***********************************************************************
 *
 * getAllSuperConcepts(+KBName,+CTX,+Concept,-CL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are super concepts of
 * Concept.
 *
 */

getAllSuperConcepts(KBName,CTX,Concept,CL) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	getAllSuperElements(Concept,CL,Dag).

/***********************************************************************
 *
 * getDirectSubConcepts(+KBName,+CTX,+Concept,-CL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are direct super concepts
 * of Concept.
 *
 */

getDirectSubConcepts(KBName,CTX,Concept,CL) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	getDirectSubElements(Concept,CL,Dag).

/***********************************************************************
 *
 * getAllSubConcepts(+KBName,+CTX,+Concept,-CL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are super concepts of 
 * Concept.
 *
 */

getAllSubConcepts(KBName,CTX,Concept,CL) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	getAllSubElements(Concept,CL,Dag).

/***********************************************************************
 *
 * getConcepts(+CTX,-CL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            CL          list of concept names
 * CL is the list of all concept names instance the subsumption hierarchy.
 *
 */

getConcepts(KBName,CTX,['Entity'|CL]) :-
	getAllSubConcepts(KBName,CTX,'Entity',CL).

/***********************************************************************
 *
 * testDirectSuperConcept(+KBName,+CTX,+Concept1,+Concept2,-Concept)
 * Arguments: KBName        knowledgeBase identifier
 *            CTX             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct superconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct superconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testDirectSuperConcept(KBName,CTX,Concept1,Concept2,Concept) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	testDirectSuperElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * testDirectSubConcept(+KBName,+CTX,+Concept1,+Concept2,-Concept)
 * Arguments: KBName        knowledgeBase identifier
 *            CTX             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct subconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct subconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testDirectSubConcept(KBName,CTX,Concept1,Concept2,Concept) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	testDirectSubElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * testSuperConcept(+KBName,+CTX,+Concept1,+Concept2,-Concept)
 * Arguments: KBName        knowledgeBase identifier
 *            CTX             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct superconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct superconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testSuperConcept(KBName,CTX,Concept1,Concept2,Concept) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	testSuperElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * testSubConcept(+KBName,+CTX,+Concept1,+Concept2,-Concept)
 * Arguments: KBName        knowledgeBase identifier
 *            CTX             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct superconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct superconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testSubConcept(KBName,CTX,Concept1,Concept2,Concept) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Dag),
	testSubElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * getCommonSuperConcepts(+KBName,+CTX,+CL1,-CL2)
 * Arguments: KBName  knowledgeBase identifier
 *            CTX       modal context
 *            CL1      list of concept names
 *            CL2      list of concept names
 * CL2 is the list of all concept names subsuming all concepts instance CL1.
 *
 */

getCommonSuperConcepts(KBName,CTX,CL1,CL2) :-
	hop_map(getAllSuperConcepts,[KBName,CTX],CL1,CLL1),
	cmp_intersection(CLL1,CL2).

/***********************************************************************
 *
 * getCommonSubConcepts(+KBName,+CTX,+CL1,-CL2)
 * Arguments: KBName  knowledgeBase identifier
 *            CTX       modal context
 *            CL1      list of concept names
 *            CL2      list of concept names
 * CL2 is the list of all concept names which are subsumed by all
 * concepts instance CL1.
 *
 */

getCommonSubConcepts(KBName,CTX,CL1,CL2) :-
	hop_map(getAllSubConcepts,[KBName,CTX],CL1,CLL1),
	cmp_intersection(CLL1,CL2).

/***********************************************************************
 *
 * getAllObjects(+KBName,+CTX,+O)
 *
 */

getAllObjects(KBName,CTX,O13) :-
	!,
	knowledgeBase(KBName,KB,_),
	setofOrNil(X1,[C1,AX1]^(conceptElement(KB,CTX,_,user,X1,C1,AX1)),O1),
	setofOrNil(X2,[R2,Y2,AX2]^roleElement(KB,CTX,_,user,X2,Y2,R2,AX2),O2),
	setofOrNil(Y3,[R3,X3,AX3]^roleElement(KB,CTX,_,user,X3,Y3,R3,AX3),O3),
	cmp_union( O1,O2,O12),
	cmp_union(O12,O3,O13),
	!.
/**********************************************************************
 *
 * @(#) constraints.pll 1.9@(#)
 *
 */

/**********************************************************************
 *
 * solveConstraint(CTX,(card,app((FF:R),X),Rel,N),hyp(HYPS),call(CALLS))
 * if Rel is '>=', 
 *    the predicate succeeds if the cardinality of 
 *    app((FF:R),X) instance modal context CTX is greater than N.
 *    If N is a variable, it will be instantiated with the greatest
 *    number M such that the cardinality of  app((FF:R),X) instance modal 
 *    context CTX is provably greater than M.
 * if Rel is '=<', 
 *    the predicate succeeds if the cardinality of 
 *    app((FF:R),X) instance modal context CTX is smaller than N.
 *    If N is a variable, it will be instantiated with the greatest
 *    number M such that the cardinality of  app((FF:R),X) instance modal 
 *    context CTX is provably smaller than M.
 *
 */

solveConstraint(KB,CTX,(card,app((FF:R),X),Rel,N),(PL,EL),hyp(HYPS),ab(D),call(CALLS),PTO) :-
%	cCS(CALLS,true),
%	SolveHead = solveConstraint(AN,CTX,(card,app((FF:R),X),Rel,N),hyp(HYPS)),
	length(CALLS,XXX),
	CALLS1 = CALLS,
	ifOption(traceOutput,yes,(cmp_format('trying ~d  solve(~w(~w)) ~w ~w~n',[XXX,R,X,Rel,N]))),
	collectAllFillers(KB,CTX,R,X,HYPS,D,CALLS1,S),
	computeNumber(S,Rel,(M,EL,PL,PTAbox)),
	continueSolve(KB,CTX,(card,app((FF:R),X),Rel,N),hyp(HYPS),ab(D),call(CALLS1),(M,PTAbox),PT),
	PTO = proved(CTX,card(R,X,Rel,N),basedOn(PT)).
	

computeNumber([],'=<',(noMaxRestriction,[],[],basedOn(noAboxEntries))) :- !.
computeNumber([],'>=',(noMinRestriction,[],[],basedOn(noAboxEntries))) :- !.
computeNumber(S,_Rel,(M,EL,PL,and(PL))) :-
	reduceToSolutionSet(S,EL,PL),
	length(EL,M).

reduceToSolutionSet([],[],[]) :- !.
reduceToSolutionSet([(E1,PT1,_)|L],L2,L3) :-
	member((E1,_PT2,_R2),L),
	!,
	reduceToSolutionSet(L,L2,L3).
reduceToSolutionSet([(E1,PT1,_)|L],[E1|L2],[PT1|L3]) :-
	reduceToSolutionSet(L,L2,L3).


continueSolve(_,_,(card,_,'=<',N),hyp(_),ab(_),call(_),(M,_PTAbox),_) :-
	number(M),
	nonvar(N),
	M > N,
	!,
	fail.
continueSolve(KB,CTX,(card,app((FF:R),X),Rel,M1),hyp(HYPS),ab(D),call(CALLS),(M1,PTAbox),PT3) :-
	number(M1),
	hl_closed(KB,CTX,X,newSkolem,R),
	!.
continueSolve(KB,CTX,(card,app((FF:R),X),Rel,N),hyp(HYPS),ab(D),call(CALLS),(M1,PTAbox),PT3) :-
	member(solveConstraint(KB,RL2,noEquivalence,_),CALLS),
%	member(R,RL2),
	collectAllConstraints(KB,CTX,[R],X,Rel,HYPS,D,CALLS,S),
	findNumberRestriction(Rel,(M1,PTAbox),S,(M3,PT3)),
	!,
	comparison(Rel,M3,N).
continueSolve(KB,CTX,(card,app((FF:R),X),Rel,N),hyp(HYPS),ab(D),call(CALLS),(M1,PTAbox),PT3) :-
	not(member(solveConstraint(KB,RL2,noEquivalence,_),CALLS)),
	bagofOrNil(R1,AX2^RN2^S2^O2^F^PT2^(eq(KB,rn(AX2,RN2,S2,O2),modal(CTX1),app((F:R),X),app((F:R1),X),hyp(HYPS),ab(D),call(CALLS),PT2), nonvar(F)),RL),
	CALLS1 = [solveConstraint(KB,RL,noEquivalence,_)|CALLS],
	collectAllConstraints(KB,CTX,RL,X,Rel,HYPS,D,CALLS1,S),
	findNumberRestriction(Rel,(M1,PTAbox),S,(M3,PT3)),
	!,
	comparison(Rel,M3,N).

collectAllFillers(KB,CTX,R,X,HYPS,D,CALLS,S) :-
	EqLiteral = eqGenerator(KB,AX,RN,S,O,CTX,Y,app((FF:R),X),HYPS,D,CALLS,PT),
	bagof((Y,PT,[KB,CTX,R,X,HYPS,D,CALLS]),AX^RN^S^O^FF^EqLiteral,S),
	!.
collectAllFillers(_,_,_,_,_,_,_,[]) :-
	!.


collectAllConstraints(KB,CTX,RL,X,Rel,HYPS,D,CALLS,S) :-
	C1 = constraint(KB,rn(AX1,RN1,S1,O1),CTX,(card,app((FF:R1),X),Rel,M2),
			hyp(HYPS),ab(D),call(CALLS),PT),
	bagof((M2,PT,[KB,CTX,FF,R1,X,Rel,HYPS,D,CALLS]),AX1^RN1^S1^O1^PT1^FF^AX2^RN2^S2^O2^F^PT2^(member(R1,RL),C1),S),
	!.
collectAllConstraints(_,_CTX,_RL,_X,_Rel,_HYPS,_D,_CALLS,[]) :-
	!.


/**********************************************************************
 * 
 * comparison(+Rel,+M,?N)
 * if N is a variable then N is instantiated with M and the predicate
 * succeeds.
 * if N is a number, the predicates succeeds if then goal Rel(M,N)
 * succeeds.
 *
 */

comparison(_Rel,M3,N) :-
	isSlot(N),
	!,
	N = M3.
comparison(Rel,M3,N) :-
	number(M3), number(N),
	Goal =.. [Rel,M3,N],
	call(Goal).
comparison(>=,noMaxRestriction,N) :-
	!.
comparison(>=,noMinRestriction,N) :-
	!,
	fail.
comparison(=<,noMaxRestriction,N) :-
	!,
	fail.
comparison(=<,noMinRestriction,N) :-
	!.
comparison(>=,M,noMaxRestriction) :-
	!,
	fail.
comparison(>=,M,noMinRestriction) :-
	!.
comparison(=<,M,noMaxRestriction) :-
	!.
comparison(=<,M,noMinRestriction) :-
	!,
	fail.

	
/**********************************************************************
 *
 * findNumberRestristriction(+Rel,+L,-N)
 * if Rel is '=<' then N will be instantiated with the smallest number
 * instance the list of numbers L.
 * if Rel is '>=' then N will be instantiated with the greatest number
 * instance the list of numbers L.
 *
 */

%findNumberRestriction('=<',(noRestriction,PT1),[],(1000000,PT1)) :- !.
%findNumberRestriction('>=',(noRestriction,PT1),[],(0,PT1)) :- !.
%findNumberRestriction('>=',(N,PT1),[],(N,PT1)) :- !.
%findNumberRestriction('=<',(N,PT1),[],(1000000,noConstraintsFound)) :- !.
findNumberRestriction('>=',(N,PT1),[],(N,PT1)) :- !.
findNumberRestriction('=<',(N,PT1),[],(noMaxRestriction,noConstraintsFound)) :- !.
findNumberRestriction(_,(noMinRestriction,_),[(N1,PT2,_)],(N1,PT2)) :- !.
findNumberRestriction(_,(noMaxRestriction,_),[(N1,PT2,_)],(N1,PT2)) :- !.
findNumberRestriction('=<',(M,_PT1),[(N1,PT2,_)],(N1,PT2)) :-
	comparison(=<,N1,M),
	!.
findNumberRestriction('=<',(M,PT1),[(_N1,_,_)],(M,PT1)) :-
	!.
findNumberRestriction('>=',(M,PT1),[(N1,_,_)],(M,PT1)) :-
	comparison(>=,M,N1),
	!.
findNumberRestriction('>=',(_M,_),[(N1,PT2,_)],(N1,PT2)) :-
	!.
findNumberRestriction('>=',(K,PT1),[(N1,_,_)|NL],(N2,PT3)) :-
	findNumberRestriction('>=',(K,PT1,_),NL,(N2,PT3)),
	comparison(>=,N2,N1),
	!.
findNumberRestriction('=<',(K,PT1),[(N1,_,_)|NL],(N2,PT3)) :-
	findNumberRestriction('=<',(K,PT1,_),NL,(N2,PT3)),
	comparison(=<,N2,N1),
	!.
findNumberRestriction(_,_,[(N1,PT1,_)|_NL],(N1,PT1)) :-
	!.


subtractRestrictions(N1,N2,M) :-
	number(N1),
	number(N2),
	!,
	M is N1 - N2.
subtractRestrictions(N1,noMaxRestriction,noMaxRestriction) :-
	!.
subtractRestrictions(noMinRestriction,N2,noMinRestriction) :-
	!.
subtractRestrictions(N1,N2,N1).

addRestrictions(N1,N2,M) :-
	number(N1),
	number(N2),
	!,
	M is N1 + N2.
addRestrictions(noMinRestriction,N2,N2) :-
	number(N2),
	!.
addRestrictions(N1,noMinRestriction,N1) :-
	number(N1),
	!.
addRestrictions(noMaxRestriction,N2,noMaxRestriction) :-
	!.
addRestrictions(N1,noMaxRestriction,noMaxRestriction) :-
	!.
addRestrictions(noMinRestriction,N2,noMinRestriction) :-
	!.







/**********************************************************************
 *
 * @(#) construct.pll 1.13@(#)
 *
 */

inProofTerm(CTX,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	nonvar(AX),
	conceptSubsets(_KB,_user,CTX1,C1,C2,AX),
	PT = proved(instance(CTX1,D,X),usingAxiom(defprimconcept(C1,C2)),basedOn(PT1)),
	!.
inProofTerm(CTX,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	nonvar(AX),
	conceptEqualSets(_KB,_user,CTX1,C1,C2,AX),
	PT = proved(instance(CTX1,D,X),usingAxiom(defconcept(C1,C2)),basedOn(PT1)),
	!.
inProofTerm(CTX,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	nonvar(AX),
	conceptElement(_KB,CTX1,_,user,C1,C2,AX),
	PT = proved(instance(CTX1,D,X),usingAxiom(assert_ind(C1,C2)),basedOn(PT1)),
	!.
inProofTerm(CTX,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	!.

inProofTerm(CTX,Name,D,X,HYPS,PT1,PT) :-
	PT = proved(instance(CTX,Name,D,X),basedOn(PT1)),
	!.
inProofTerm(CTX,D,X,HYPS,PT1,PT) :-
	PT = proved(instance(CTX,D,X),basedOn(PT1)),
	!.

eqProofTerm(CTX,Y,_FF,R,X,HYPS,PT1,PT) :-
	nonvar(R),
	atomic(R),
	!,
	Rel =.. [R,CTX,X,Y],
	PT = proved(Rel,basedOn(PT1)),
	!.
eqProofTerm(CTX,Y,_FF,R,X,HYPS,PT1,PT) :-
	Rel = hl_holds(R,CTX,X,Y),
	PT = proved(Rel,basedOn(PT1)),
	!.
conProofTerm(CTX,R,X,Rel,N,HYPS,PT1,PT) :-
	PT = proved(card(R,CTX,X,Rel,N),basedOn(PT1)),
	!.
/***********************************************************************
 *
 * makeTerm(+TermPieces,-Term)
 *
 */

makeTerm(Term,Term) :-
	isSlot(Term),
	!.
makeTerm(Term,Term) :-
	atomic(Term),
	!.
makeTerm([Functor|ArgList],Term) :-
	hop_map(makeTerm,ArgListTerms,ArgList),
	Term =.. [Functor|ArgListTerms].
makeTerm(Term,Term).

%element(X) :-
%	atomic(X),
%	!.
%element(X) :-
%	isSlot(X),
%	!.
element(_) :- !.

relation(R,RN,X1,Y1) :-
%	nonvar(R),
%	R =.. [RN,X1,Y1].
	T =.. [RN,X1,Y1],
	R = T.

eqGenerator(KB,AX,RN,S,O,CTX,X,Y,HYPS,D,CALLS,PT) :-
	eq(KB,rn(AX,RN,S,O),modal(CTX),X,Y,hyp(HYPS),ab(D),call(CALLS),PT),
	nonvar(X),
	nonvar(Y),
	atomic(X).

gensymbol(object,L,NewSymbol) :-
	gensym(object,NewFunction),
	NewSymbol =.. [NewFunction|L],
	!.
gensymbol(Symbol,L,NewSymbol) :-
	gensym(Symbol,NewSymbol),
	!.


/**********************************************************************
 *
 * ruleName(+AxiomName,+RuleName,+Orientation)
 *
 */

ruleName(AxiomName,RuleName,Origin,Orientation,
	 rn(AxiomName,RuleName,Origin,Orientation)) :- 
	!.

reverseOrientation(lInR,rInL) :- !.
reverseOrientation(rInL,lInR) :- !.


typeOfDefinition(_,_,C,system) :-
	isSlot(C),
	!.
typeOfDefinition(KB,CTX,C,user) :-
	getConceptName(KB,CTX,C),
	!.
typeOfDefinition(_,_,C,system) :-
	atomic(C),
	atom_codes(C,[99,111,110,99,101,112,116|_]),   %string "concept"
	!.
typeOfDefinition(KB,CTX,R,user) :-
	getRoleName(KB,CTX,R),
	!.
typeOfDefinition(_,_,R,system) :-
	atomic(R),
	atom_codes(R,[114,111,108,101|_]),    %string "role"
	!.
typeOfDefinition(KB,CTX,not(C),Type) :-
	!,
	typeOfDefinition(KB,CTX,C,Type).
typeOfDefinition(_,_,normal(C),system) :-
	!.
typeOfDefinition(_,_,not(normal(C)),system) :-
	!.
typeOfDefinition(_,_,_,user) :-
	!.

% somoneterpretation([]).
% somoneterpretation([I1|IL]) :-
% 	call(I1),
% 	somoneterpretation(IL).
% 
% allInterpretation([]) :-
% 	fail.
% allInterpretation([I1|IL]) :-
% 	(call(I1) ; allInterpretation(IL)).
% 
% roleConjunction(X,IL) :-
% 	isSlot(X),
% 	somoneterpretation(IL).
% roleConjunction(X,IL) :-
% 	nonvar(X),
% 	atom_codes(X,[115,107,111,108,101,109|_]),  %string "skolem"
% 	allInterpretation(IL).
% roleConjunction(X,IL) :-
% 	nonvar(X),
% 	not(atom_codes(X,[115,107,111,108,101,109|_])), %string "skolem"
% 	somoneterpretation(IL).


/***********************************************************************
 *
 * convertCTX(KB,+CTX1,+ModalOperator,WVL1,-CTX2,WVL2)
 * Arguments: CTX1                modal context
 *            ModalOperator      modal operator
 *            WVL1               list of free world variables already
 *                               generated during the conversion
 *            CTX2                modal context
 *            WVL2               list of all free world variables 
 *                               generated during the conversion
 * CTX2 is the translation of ModalOperator appended to CTX1.
 *
 */

genagent(X,_,X) :-
	isSlot(X),
	!.
genagent(all,free,_A) :-
	!.
genagent(all,skolemize,A) :-
	gensym(agent,A),
	!.
genagent(A,_,A) :-
	!.

convertCTX(positive,KB,Start,CTX,WVL1,End,WVL2) :-
	!,
	convertCTX(KB,Start,CTX,WVL1,End,WVL2).
convertCTX(negative,KB,Start,CTX1,WVL1,End,WVL2) :-
	!,
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	convertCTX(KB,Start,CTX3,WVL1,End,WVL2).
	

convertCTX(_KB,_,CTX,WVL,[_W1,true],WVL) :-
	isSlot(CTX),
	!.
convertCTX(_KB,[CTX1,Lits1],[],WVL,[CTX1,Lits1],WVL) :-
	!.
convertCTX(KB,[CTX1,Lits1],[d(MOp,A)|L],WVL,[CTX3,Lits3],WVL3) :-
	gensym(wp,WP),
	WPTerm = [WP,WVL],
	genagent(A,skolemize,Agent),
	CTX2 = app(WPTerm:m(MOp,Agent),CTX1),
	convertCTX(KB,[CTX2,Lits1],L,WVL,[CTX3,Lits3],WVL3),
	!.
convertCTX(KB,[CTX1,Lits1],[b(MOp,A)|L],WVL,[CTX3,Lits3],WVL3) :-
	genagent(A,free,Agent),
	Lit = hl_holds(KB,_,m(MOp,Agent),CTX1,CTX2),
	convertCTX(KB,[CTX2,(Lit,Lits1)],L,[CTX2|WVL],[CTX3,Lits3],WVL3),
	!.
convertCTX(KB,[CTX1,Lits1],[dc(MOp,C)|L],WVL,[CTX3,Lits3],WVL3) :-
	gensym(wp,WP),
	WPTerm = [WP,WVL],
	genagent(all,skolemize,Agent),
	CTX2 = app(WPTerm:m(MOp,Agent),CTX1),
	getQuery(KB,CTX1,C,Agent,_Exp,Body),
	convertCTX(KB,[CTX2,(once(Body),Lits1)],L,WVL,[CTX3,Lits3],WVL3),
	!.
convertCTX(KB,[CTX1,Lits1],[bc(MOp,C)|L],WVL,[CTX3,Lits3],WVL3) :-
	genagent(all,free,Agent),
	Lit = hl_holds(KB,_,m(MOp,Agent),CTX1,CTX2),
	getQuery(KB,CTX1,C,Agent,_Exp,Body),
	convertCTX(KB,[CTX2,((once(Body),Lit),Lits1)],L,[CTX2|WVL],[CTX3,Lits3],WVL3),
	!.



/***********************************************************************
 *
 * THE STRUCTURE OF THE IN-CLAUSES
 * 
 * 1) THE HEAD
 *    instance(KB,RN,modal(W),A1,X,hyp(C1),ab(D),call(H1),Exp)
 *    KB is a internal knowledgeBase atom_codes
 *    RN  is a rule atom_codes
 *    W   is a world
 *    A1  is a concept name or the negation of a concept name
 *    X   is a free variable
 *    C1  is a list of clauses --- the hypotheses that can be used
 *    D   is a name identifying a specific abductive derivation
 *    H1  is a list of calls   --- the calls to instance that have already
 *                                 been used
 *    Exp is a explanation term
 * 2) THE BODY
 *
 */

/***********************************************************************
 *
 * constructMLHead(+ModalSequence,
 *                 +ConceptName,+Constraint,
 *                 +Hypotheses,+CallStack,-Inhead)
 * 
 */

constructInHead(KB,Name,CTX,CN,CON,HYP,D,CALL,PT1,InHead) :-
	inProofTerm(CTX,Name,CN,CON,HYP,PT1,PT),
	InHead = instance(KB,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
	!.

constructKBHead(KB,Priority,Name,CTX,CN,CON,HYP,D,CALL,PT1,InHead) :-
	inProofTerm(CTX,Name,CN,CON,HYP,PT1,PT),
	InHead = kb_in(KB,Priority,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
	!.

constructMLHead(KB,Name,CTX,CN,CON,HYP,D,CALL,PT1,InHead) :-
	inProofTerm(CTX,Name,CN,CON,HYP,PT1,PT),
	InHead = kb_in(KB,pr(3),Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
	!.

getKnowledgeBase(kb_in(KB,pr(_),_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),KB).
getKnowledgeBase(instance(KB,_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),KB).
getModalSequence(kb_in(_,pr(_),_,modal(CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),CTX).
getModalSequence(instance(_,_,modal(CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),CTX).
getConceptName(kb_in(_,pr(_),_,modal(_CTX),CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),CN).
getConceptName(instance(_,_,modal(_CTX),CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),CN).
getConstraint(kb_in(_,pr(_),_,modal(_CTX),_CN,CON,hyp(_HYP),ab(_),call(_CALL),_),CON).
getConstraint(instance(_,_,modal(_CTX),_CN,CON,hyp(_HYP),ab(_),call(_CALL),_),CON).
getHypotheses(kb_in(_,pr(_),_,modal(_CTX),_CN,_CON,hyp(HYP),ab(_),call(_CALL),_),HYP).
getHypotheses(instance(_,_,modal(_CTX),_CN,_CON,hyp(HYP),ab(_),call(_CALL),_),HYP).
getCallStack(kb_in(_,pr(_),_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(CALL),_),CALL).
getCallStack(instance(_,_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(CALL),_),CALL).
getExplanation(kb_in(_,pr(_),_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),E),E).
getExplanation(instance(_,_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),E),E).
getInExplanation(kb_in(_,pr(_),_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),
	         proved(I,_)),I).
getInExplanation(instance(_,_,modal(_CTX),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),
	         proved(I,_)),I).

/**********************************************************************
 *
 * constructEqHead(KB,+CTX,+Y,+F,+R,+X,+HYPS,+CALLS,-L)
 *
 */

constructEqHead(KB,Name,CTX,Y,F,R,X,HYPS,D,CALLS,PT1,L) :-
	eqProofTerm(CTX,Y,F,R,X,HYPS,PT1,PT),
	L = eq(KB,Name,modal(CTX),Y,app((F:R),X),hyp(HYPS),ab(D),call(CALLS),PT),
	!.

/**********************************************************************
 *
 * constructEqMark(+CTX,+Y,+F,+R,+X,+HYPS,+CALLS,+AN,-L)
 *
 */

constructEqMark(Name,CTX,Y,F,R,X,HYPS,_D,_CALLS,L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = eq(Name,modal(CTX),Y,app((F:R),X),hyp(H1)),
	!.

/**********************************************************************
 *
 * constructEqCall(KB,+CTX,+Y,+F,+R,+X,+HYPS,+CALLS,+AN,-L)
 *
 */

constructEqCall(KB,rn(AX,RN,_Source,Orientation),bodyMC(CTX1),headMC(CTX2),
	        Y,F,R,X,HYPS,D,CALLS,PT,L) :-
	constructEqMark(rn(AX,RN,_S1,Orientation),CTX2,Y,F,R,X,HYPS,D,CALLS,C1),
	L = eq(KB,rn(_AX2,_RN2,_S2,_O2),modal(CTX1),Y,app((F:R),X),
               hyp(HYPS),ab(D),call([C1|CALLS]),PT),
	!.

/***********************************************************************
 *
 * constructMLMark(+ModalSequence,+ConceptName,+Constraint,
 *                      +AxiomName,-LoopCheck)
 *
 */

constructMLMark(Name,CTX,CN,CON,HYPS,D,LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = instance(Name,modal(CTX),CN,CON,hyp(H1),ab(D)),
	!.

constructMLMark(kb_in(_,Pr,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(_),_),LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = instance(Name,modal(CTX),CN,CON,hyp(H1),ab(D)),
	!.
constructMLMark(instance(_,Pr,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(_),_),LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = instance(Name,modal(CTX),CN,CON,hyp(H1),ab(D)),
	!.
constructMLMark(instance(_,Name,modal(CTX),CN,CON,hyp(HYP),ab(D),call(_),_),LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = instance(Name,modal(CTX),CN,CON,hyp(H1),ab(D)),
	!.

/***********************************************************************
 *
 * constructMLCall(+KB,rn(+AX1,+RN1,+S1,+O1),bodyMC(CTX1),headMC(CTX2), 
 *                 +ConceptName,+Variable,
 *                 +Hypotheses,+DerivationName,+CallStack,+Proofterm,
 *                 -InTerm)
 *
 * The information instance rn(AX1,RN1,S1,O1)  is used instance the following way:
 * AX1, RN1, and O1 is used instance the construction of the MLMark
 * which is added to the call stack. If AX1 is `no' then the MLMark is
 * not added to the call stack at all.
 * S1 is used instance the construction of InHead. If S1 is a variable, any 
 * rule can be used to prove the call. If S1 is `user' then only user
 * provided rules may be used. If S1 is `system' then only system provided
 * rules may be used.
 *
 */

constructMLCall(KB,rn(AX1,RN1,S1,O1),bodyMC(CTX1),headMC(CTX2),
                CN,CON,HYPS,D,CALLS,PT1,InHead) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	constructMLMark(rn(AX1,RN1,_S2,_O2),CTX2,CN,CON,HYPS,D,Mark),
	convert_loop(AX1,CALLS,Mark,CALLS1),
	getNegatedConcept(CN,C2),
	InHeadH = instance(_NameH,modal(CTX1),C2,CON,hyp(_H),ab(_D)),
	getSource(S1,Source),
	InHead = instance(KB,rn(_AX3,_RN3,Source,_O3),modal(CTX1),CN,CON,
                    hyp([or(H1),rl([InHeadH|H2]),fl(H3)]),
                    ab(D),call(CALLS1),PT1),
	!.


getSource(V1,_V2) :-
	isSlot(V1),
	!.
getSource(any,_V2) :-
	!.
getSource(V1,V1) :-
	!.

getNegatedConcept(CN,not(CN)) :-
	isSlot(CN),
	!.
getNegatedConcept(CN,C2) :-
	normalizeNot(not(CN),C2),
	!.

/***********************************************************************
 *
 * constructConHead(KB,+Name,+CTX,+F,+R,+X,+Rel,+N,
 *                  +HYPS,+CALLS,-Literal)
 *
 */

constructConHead(KB,Name,CTX,F,R,X,Rel,N,HYPS,D,CALLS,PT1,L) :-
	conProofTerm(CTX,R,X,Rel,N,HYPS,PT1,PT),
	L = constraint(KB,Name,CTX,(card,app((F:R),X),Rel,N),
                       hyp(HYPS),ab(D),call(CALLS),PT),
	!.

/***********************************************************************
 *
 * constructConMark(+CTX,+F,+R,+X,+Rel,+N,+HYPS,+CALLS,+AN,-Literal)
 *
 */

constructConMark(Name,CTX,F,R,X,Rel,N,HYPS,_D,_CALLS,L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = constraint(Name,CTX,(card,app((F:R),X),Rel,N),hyp(H1)),
	!.

constructConMark(constraint(_,Name,CTX,(card,A,Rel,N),hyp(HYPS),ab(_D),call(_CALLS),_PT),L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = constraint(Name,CTX,(card,A,Rel,N),hyp(H1)),
	!.

/***********************************************************************
 *
 * constructSolveConMark(+CTX,+F,+R,+X,+Rel,+N,+HYPS,+CALLS,+AN,-Literal)
 *
 */

constructSolveConMark(Name,CTX,F,R,X,Rel,N,HYPS,_D,_CALLS,L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = solveConstraint(Name,CTX,(card,app((F:R),X),Rel,N),hyp(H1)),
	!.

constructSolveConMark(constraint(_,Name,CTX,(card,A,Rel,N),hyp(HYPS),ab(_D),call(_CALLS),_PT),L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = solveConstraint(Name,CTX,(card,A,Rel,N),hyp(H1)),
	!.

/***********************************************************************
 *
 * constructConCall(KB,+CTX,+F,+R,+X,+Rel,+N,+HYPS,+CALLS,+AN,-Literal)
 *
 */

constructConCall(KB,bodyMC(CTX1),headMC(CTX2),F,R,X,Rel,N,HYPS,D,CALLS,AN,PT1,L) :-
	constructConMark(CTX2,F,R,X,Rel,N,HYPS,D,CALLS,AN,Mark),
        L = constraint(KB,_Name,CTX1,(card,app((F:R),X),Rel,N),
                       hyp(HYPS),ab(D),call([Mark|CALLS]),PT1),
	!.


addDefaultML(I1,L1) :-
	isSlot(L1),
	!,
	L1 = [I1|_L2],
	!.
addDefaultML(I1,[_|L1]) :-
	addDefaultML(I1,L1),
	!.

memberDML(I1,L) :-
	nonvar(L),
	L = [I1|L2],
	!.
memberDML(I1,L) :-
	nonvar(L),
	L = [_|L2],
	memberDML(I1,L2).


/**********************************************************************
 *
 * getAxiom(+KB,+CTX,AX)
 * succeeds if AX is an axiom instance knowledgeBase KB and modal context
 * CTX.
 *
 */
	
getAxiom(KB,CTX,Ax) :-
	axiom(KB,CTX,Ax).

/**********************************************************************
 *
 * getConceptName(+KB,+CTX,CN)
 * succeeds if CN is a concept name instance knowledgeBase KB and modal context
 * CTX.
 *
 */

getConceptName(KB,CTX1,CN) :-
	convertCTX(negative,KB,[[],true],CTX1,[],[W1,G1],_),
	clause(conceptName(KB,_,W1,CN),_),
	once((call(G1),conceptName(KB,_,W1,CN))).

/**********************************************************************
 *
 * getRoleName(+KB,+CTX,CN)
 * succeeds if CN is a role name instance knowledgeBase KB and modal context
 * CTX.
 *
 */

getRoleName(KB,CTX1,CN) :-
	convertCTX(negative,KB,[[],true],CTX3,[],[W1,G1],_),
	clause(roleName(KB,_,W1,CN),_),
	once((call(G1),roleName(KB,_,W1,CN))).


/**********************************************************************
 *
 * @(#) dag.pll 1.3@(#)
 *
 */

/***********************************************************************
 *
 * memberElement(+Element,+Dag)
 * Parameter: Element     element name
 *            Dag         subsumption hierarchy
 * checks wether or not Element occurs instance the subsumption hierarchy.
 *
 */

memberElement(Element,hl_node(CL,_NL)) :-
	member(Element,CL),
	!.
memberElement(Element,hl_node(_CL,NL)) :-
	memberElementSubtrees(Element,NL),
	!.

memberElementSubtrees(_Element,[]) :-
	!,
	fail.
memberElementSubtrees(Element,[N1|_NL]) :-
	memberElement(Element,N1),
	!.
memberElementSubtrees(Element,[_N1|NL]) :-
	memberElementSubtrees(Element,NL).

/***********************************************************************
 *
 * memberDirectSubElements(+Element,+Dag)
 * Parameter: Element     element name
 *            Dag         subsumption hierarchy
 * checks wether or not Element occurs instance the direct subelements of
 * the 'Entity' element of Dag.
 *
 */

memberDirectSubElements(Element,hl_node(_CL,NL)) :-
	!,
	memberDirectSubElements(Element,NL).

memberDirectSubElements(_Element,[]) :-
	!,
	fail.
memberDirectSubElements(Element,[hl_node(CL,_NL1)|_NL]) :-
	member(Element,CL),
	!.
memberDirectSubElements(Element,[_N1|NL]) :-
	memberDirectSubElements(Element,NL).

/***********************************************************************
 *
 * getDirectSuperElements(+Element,-CL,+Dag)
 * Parameter: Dag         subsumption hierarchy
 *            Element     element name
 *            CL          list of element names
 * CL is the list of all element names which are direct super elements
 * of Element.
 *
 */

getDirectSuperElements(Element,CL,hl_node(CL,NL)) :-
	memberDirectSubElements(Element,hl_node(CL,NL)),
	!.
getDirectSuperElements(Element,CL,hl_node(_,NL)) :-
	getDirectSuperElements(Element,CL,NL).

getDirectSuperElements(_Element,[],[]) :-
	!.
getDirectSuperElements(Element,CL,[N1|NL]) :-
	getDirectSuperElements(Element,CL1,N1),
	getDirectSuperElements(Element,CL2,NL),
	cmp_union(CL1,CL2,CL).

/***********************************************************************
 *
 * getAllSuperElements(+Element,-CL,+Dag)
 * Parameter: Element     element name
 *            CL          list of element names
 *            Dag         subsumption hierarchy
 * CL is the list of all element names which are super elements of
 * Element.
 *
 */

getAllSuperElements(Element,CL,Dag) :-
	getAllSuperElements(Element,CL,[],Dag).

getAllSuperElements(Element,CL1,CL1,hl_node(CL,_NL)) :-
	member(Element,CL),
	!.
getAllSuperElements(Element,CL3,CL1,hl_node(CL,NL)) :-
	cmp_union(CL,CL1,CL2),
	getAllSuperElements(Element,CL3,CL2,NL).

getAllSuperElements(_Element,[],_CL1,[]) :-
	!.
getAllSuperElements(Element,CL2,CL1,[N1|NL]) :-
	getAllSuperElements(Element,CL3,CL1,N1),
	getAllSuperElements(Element,CL4,CL1,NL),
	cmp_union(CL3,CL4,CL2).


/***********************************************************************
 *
 * getDirectSubElements(+Element,-CL,+Dag)
 * Parameter: Element     element name
 *            CL          list of element names
 *            Dag         subsumption hierarchy
 * CL is the list of all element names which are direct sub elements
 * of Element.
 *
 */

getDirectSubElements(Element,CL1,hl_node(CL,NL)) :-
	member(Element,CL),
	!,
	getSubElements(CL1,NL).
getDirectSubElements(Element,CL1,hl_node(_CL,NL)) :-
	getDirectSubElements(Element,CL1,NL).

getDirectSubElements(_Element,[],[]) :-
	!.
getDirectSubElements(Element,CL,[N1|NL]) :-
	getDirectSubElements(Element,CL1,N1),
	getDirectSubElements(Element,CL2,NL),
	cmp_union(CL1,CL2,CL).

getSubElements([],[]) :-
	!.
getSubElements(CL,[hl_node(CL1,_)|NL]) :-
	getSubElements(CL2,NL),
	cmp_union(CL1,CL2,CL).


/***********************************************************************
 *
 * getAllSubElements(+Dag,+Element,-CL,+Dag)
 * Parameter: Element     element name
 *            CL          list of element names
 *            Dag         subsumption hierarchy
 * CL is the list of all element names which are sub elements of 
 * Element
 *
 */

getAllSubElements(Element,CL1,hl_node(CL,NL)) :-
	member(Element,CL),
	!,
	getElements(CL1,NL).
getAllSubElements(Element,CL1,hl_node(_CL,NL)) :-
	getAllSubElements(Element,CL1,NL),
	!.

getAllSubElements(_Element,[],[]) :-
	!.
getAllSubElements(Element,CL,[N1|NL1]) :-
	getAllSubElements(Element,CL2,N1),
	getAllSubElements(Element,CL3,NL1),
	cmp_union(CL2,CL3,CL).

/***********************************************************************
 *
 * getElements(-CL,+Dag)
 * Parameter: CL     list of element names
 *            Dag    subsumption hierarchy
 * CL is the list of all element names instance the subsumption hierarchy.
 *
 */

getElements(CL,hl_node(CL1,NL)) :-
	getElements(CL2,NL),
	cmp_union(CL1,CL2,CL).
getElements([],[]) :-
	!.
getElements(CL,[N1|NL]) :-
	getElements(CL1,N1),
	getElements(CL2,NL),
	cmp_union(CL1,CL2,CL).


/***********************************************************************
 *
 * testDirectSuperElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct superelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct superelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testDirectSuperElement(Element1,Element2,Element1,hl_node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberDirectSubElements(Element2,hl_node(CL,NL)).
testDirectSuperElement(Element1,Element2,Element2,hl_node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberDirectSubElements(Element1,hl_node(CL,NL)).

/***********************************************************************
 *
 * testDirectSubElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct subelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct subelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testDirectSubElement(Element1,Element2,Element2,hl_node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberDirectSubElements(Element2,hl_node(CL,NL)).
testDirectSubElement(Element1,Element2,Element1,hl_node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberDirectSubElements(Element1,hl_node(CL,NL)).


/***********************************************************************
 *
 * testSuperElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct superelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct superelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testSuperElement(Element1,Element2,Element1,hl_node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberElementSubtrees(Element2,NL).
testSuperElement(Element1,Element2,Element2,hl_node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberElementSubtrees(Element1,NL).


/***********************************************************************
 *
 * testSubElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct superelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct superelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testSubElement(Element1,Element2,Element1,hl_node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberElementSubtrees(Element1,NL).
testSubElement(Element1,Element2,Element2,hl_node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberElementSubtrees(Element2,NL).


/***********************************************************************
 *
 * getCommonSuperElements(+CL1,-CL2,+Dag)
 * Parameter: CL1      list of element names
 *            CL2      list of element names
 *            Dag      subsumption hierarchy
 * CL2 is the list of all element names subsuming all elements instance CL1.
 *
 */

getCommonSuperElements(CL1,CL2,Dag) :-
	hop_map(getAllSuperElements,[Dag],CL1,CLL1),
	cmp_intersection(CLL1,CL2).

/***********************************************************************
 *
 * getCommonSubElements(+CL1,-CL2,Dag)
 * Parameter: CL1      list of element names
 *            CL2      list of element names
 *            Dag      subsumption hierarchy
 * CL2 is the list of all element names which are subsumed by all
 * elements instance CL1.
 *
 */

getCommonSubElements(CL1,CL2,Dag) :-
	hop_map(getAllSubElements,[Dag],CL1,CLL1),
	cmp_intersection(CLL1,CL2).




/**********************************************************************
 *
 * @(#) examples.pll 1.5@(#)
 *
 */

example(1) :-
	makeKnowledgeBase('ex1','von HJO'),
	initKnowledgeBase,
	defconcept(fatherAcademic,and([male,some(child,academic)])),
	defconcept(grandfatherAcademic,and([male,some(child,fatherAcademic)])),
	assert_ind(tom,tim,child),
	assert_ind(tim,mike,child),
	assert_ind(mike,male),
	assert_ind(mike,academic),
	assert_ind(tim,male),
	assert_ind(tom,male).
%%% Example  2:
%%% KRIS-Example
% setof(C,ask(elementOf(mary,C)),L)
% gives L = ['Entity',grandparent,parent,parent_with_sons_only,
%            parent_with_two_children,person] 
% instance Total runtime 12.167 sec. (05.06.92)
example(2) :-
	makeKnowledgeBase('ex2','krisExample'),
	initKnowledgeBase,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defconcept(males,some(sex,male)),
	defconcept(females,some(sex,female)),
	defprimconcept(person,some(sex,or([male,female]))),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,some(sex,female)])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,some(sex,male))])),
	defconcept(parent_with_two_children,and([parent,atleast(2,child)])),
	assert_ind(tom,father),
	assert_ind(tom,peter,child),
	assert_ind(tom,harry,child),
	assert_ind(mary,parent_with_sons_only),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example  3:
% inconsistent([])
% succeeds instance Total runtime 0.000 sec. (05.06.92)
example(3) :-
	makeKnowledgeBase('ex3','Inconsistence'),
	initKnowledgeBase,
	defprimconcept(parent_with_one_child,atmost(1,child)),
	assert_ind(mary,parent_with_one_child),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example  4:
% Modal Operators
example(4) :-
	makeKnowledgeBase('ex4','Inconsistence'),
	initKnowledgeBase,
	defconcept([b(believe,a1)],c1,b(want,a2,car)),
	defconcept([b(believe,a1)],c2,b(want,a3,car)),
	defprimconcept([b(believe,a1)],c1,c2),
	assert_ind([b(believe,a1)],audi,c1).
%%% Example  5:
% subsumes([],c1,c2).
% fails    instance Total runtime 0.050 sec. (05.06.92)
% subsumes([],c2,c1).
% succeeds instance Total runtime 0.050 sec. (05.06.92)
example(5) :-
	makeKnowledgeBase('ex5','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,and([all(r,a),all(and([r,q]),b)])),
	defconcept(c2,all(and([r,q]),and([a,b]))).
%%% Example  6:
% subsumes([],c1,c2).
% fails    instance Total runtime 0.033 sec. (05.06.92)
% subsumes([],c2,c1).
% succeeds instance Total runtime 0.033 sec. (05.06.92)
example(6) :-
	makeKnowledgeBase('ex6','Subsumption'),
	initKnowledgeBase,
	defrole(r1,and([r,q])),
	defconcept(d0,and([a,b])),
	defconcept(d1,all(r,a)),
	defconcept(d2,all(r1,b)),
	defconcept(c1,and([d1,d2])),
	defconcept(c2,all(r1,d0)).
%%% Example  7:
example(7) :-
	makeKnowledgeBase('ex7','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,atleast(3,r)),
	defconcept(c2,and([all(and([r,p]),a),all(and([r,q]),not(a)),atleast(2,and([r,p])),atleast(2,and([r,q]))])).
%%% Example  8;
% ask(elementOf(tom,heterosexual))
% succeeds instance Total runtime 0.033 sec. (05.06.92)
example(8) :-
	makeKnowledgeBase('ex8','Disjunction of complementary concepts'),
	initKnowledgeBase,
	defprimconcept(male),
	defconcept(female,not(male)),
	defconcept(heterosexual,or([male,female])).
%%% Example  9:
% Variation of the KRIS-Example
% ask(elementOf(chris,male))
% succeeds instance Total runtime 0.000 sec. (05.06.92)
example(9) :-
	makeKnowledgeBase('ex9','Variation of the KRIS example'),
	initKnowledgeBase,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defprimconcept(person,or([male,female])),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,female])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,male)])),
	defconcept(parent_with_two_children,and([parent,atleast(2,child)])),
	assert_ind(tom,father),
	assert_ind(tom,peter,child),
	assert_ind(tom,harry,child),
	assert_ind(mary,parent_with_sons_only),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example 10:
% ask(elementOf(tom,c2)) 
% succeeds instance Total runtime 0.017 sec. (05.06.92)
example(10) :-
	makeKnowledgeBase('ex10','Inverse Role'),
	initKnowledgeBase,
	defrole(r2,inverse(r1)),
	defconcept(c1,all(r1,c2)),
	defconcept(c3,some(r2,c1)),
	assert_ind(tom,c3).
%%% Example 11:
% inconsistent([])
% succeeds instance Total runtime 0.034 sec. (05.06.92)
example(11) :-
	makeKnowledgeBase('ex11','Inconsistence'),
	initKnowledgeBase,
	defconcept(c1,and([atleast(2,child),atmost(1,child)])),
	assert_ind(tom,c1).
%%% Example 12:
% subsumes([],c1,c2)
% succeeds instance Total runtime 0.050 sec. (05.06.92)
% subsumes([],c2,c1)
% fails    instance Total runtime 0.200 sec. (05.06.92)
example(12) :-
	makeKnowledgeBase('ex12','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,and([person,atleast(2,child)])),
	defconcept(c2,and([person,atleast(3,restr(child,lawyer))])).
%%% Example 13:
% subsumes([],c1,c2)
% succeeds instance Total runtime 0.117 sec. (05.06.92)
example(13) :-
	makeKnowledgeBase('ex13','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,and([person,atmost(4,restr(child,doctor))])),
	defconcept(c2,and([person,female,atmost(3,child)])).
%%% Example 14:
% subsumes([],c1,c2)
% succeeds ???
% subsumes([],c2,c1)
% succeeds instance Total runtime 0.250 sec. (06.06.92)
example(14) :-
	makeKnowledgeBase('ex14','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,atmost(0,restr(r,and([atleast(3,s1),atleast(4,s2)])))),
	defconcept(c2,all(restr(r,atleast(2,s1)),atmost(2,s2))).
%%% Example 15:
% subsumes([],c2,c1)
% succeeds instance Total runtime 0.067 sec. (05.06.92)
example(15) :-
	makeKnowledgeBase('ex15','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,and([person,all(friend,doctor),all(restr(friend,doctor),atleast(1,speciality))])),
	defconcept(c2,and([person,all(friend,atleast(1,speciality))])).
%%% Example 16:
% subsumes([],c2,c1)
% succeeds instance Total runtime 0.450 sec. (06.06.92)
example(16) :-
	makeKnowledgeBase('ex16','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,and([atleast(1,restr(child,lawyer)),atleast(1,restr(child,doctor))])),
	defconcept(c2,or([atleast(2,child),atleast(1,restr(child,and([lawyer,doctor])))])).
%%% Example 17:
% subsumes([],c2,c1)
% succeeds instance Total runtime 0.034 sec. (05.06.92)
example(17) :-
	makeKnowledgeBase('ex17','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,some(and([child,friend]),doctor)),
	defconcept(c2,and([some(child,doctor),some(friend,doctor)])).
%%% Example 18:
% ask(elementOf(mary,c4))
% succeeds instance Total runtime 0.117 sec. (05.06.92)
example(18) :-
	makeKnowledgeBase('ex18','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c3,and([atmost(4,child),atleast(2,restr(child,female))])),
	defconcept(c4,atmost(2,restr(child,female))),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,peter,child),
	assert_ind(mary,tom,child),
	assert_ind(mary,c3).
%%% Example 19
% ask(elementOf(amy,female))
% succeeds instance Total runtime 0.067 sec. (06.06.92)
example(19) :-
	makeKnowledgeBase('ex19','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c5,and([atmost(2,restr(child,male))])),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,tom,child),
	assert_ind(mary,peter,child),
	assert_ind(mary,amy,child),
	assert_ind(mary,c5).
%%% Example 20
% inconsistent([])
% succeeds instance Total runtime 5.167 sec. (05.06.92)
example(20) :-
	makeKnowledgeBase('ex20','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c5,and([atmost(2,restr(child,male)),atmost(1,restr(child,female))])),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,tom,child),
	assert_ind(mary,peter,child),
	assert_ind(mary,amy,child),
	assert_ind(mary,jane,child),
	assert_ind(mary,c5).
%%% Example 21
% ask(elementOf(betty,female))
example(21) :-
	makeKnowledgeBase('ex21','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c1,and([atmost(1,restr(teacher,male)),atmost(1,restr(teacher,female))])),
	defconcept(c2,and([atmost(2,restr(teacher,male)),atmost(1,restr(teacher,female))])),
	assert_ind(tom,c1),
	assert_ind(sue,c1),
	assert_ind(david,c2),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,peter,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher),
	assert_ind(david,betty,teacher),
	assert_ind(david,chris,teacher),
	assert_ind(david,peter,teacher).
%%% Example 22
% ask(elementOf(amy,female))
% should succeeds
% but fails instance the current implementation
example(22) :-
	makeKnowledgeBase('ex22','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defconcept(male,not(female)),
	defrole(maleTeacher,restr(teacher,male)),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 23
% is a variant of example 23 with user provided names for the 
% restricted roles.
% ask(elementOf(amy,female))
% should succeeds
% but fails instance the current implementation
example(23) :-
	makeKnowledgeBase('ex23','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defconcept(male,not(female)),
	defprimrole(maleTeacher,teacher),
	defprimrole(femaleTeacher,teacher),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 24
% ask(elementOf(audi,c3))
% succeeds instance Total runtime 1.634 sec. (24.06.92)
example(24) :-
	makeKnowledgeBase('ex24','Modal operators'),
	initKnowledgeBase,	
	modalAxioms(kd45,believe,a1),
	defconcept(c1,b(believe,a1,auto)),
	defconcept(c3,b(believe,a1,c1)),
	defconcept([b(believe,a1)],c1,b(believe,a1,auto)),
	defconcept([b(believe,a1)],c3,b(believe,a1,c1)),
	assert_ind(audi,c1).
%%% Example 25
% not(ask(elementOf(audi,c3)))
% succeeds instance Total runtime 0.033 sec. (24.06.92)
example(25) :-
	makeKnowledgeBase('ex25','Modal operators'),
	initKnowledgeBase,	
	modalAxioms(kd45,believe,a1),
	defconcept([b(believe,a1)],c1,b(believe,a1,auto)),
	defconcept([b(believe,a1)],c3,b(believe,a1,c1)),
	assert_ind(audi,c1).
%%% Example 26
% subsumes([],c2,c1)
% succeeds instance Total runtime 0.034 sec. (24.06.92)
% not(subsumes([],c1,c2))
% succeeds instance Total runtime 1.333 sec. (24.06.92)
example(26) :-
	makeKnowledgeBase('ex27','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,atmost(0,r)),
	defconcept(c2,all(r,c5)).
%%% Example 27
% subsumes([],c2,c1) 
% succeeds instance Total runtime 0.067 sec. (24.06.92)
% not(subsumes([],c1,c2))
% succeeds
example(27) :-
	makeKnowledgeBase('ex28','Subsumption'),
	initKnowledgeBase,
	defconcept(c1,not(some(r,'Entity'))),
	defconcept(c2,all(r,c5)).
%%% Example 28
% ask(ex28,[b(believe,john)],elementOf(audi,auto),P)
% succeeds
example(28) :-
	makeKnowledgeBase('ex28','Modal operators'),
	initKnowledgeBase,	
	modalAxioms(kd45,believe,a1),
	modalAxioms(kd45,believe,all),
	defprimconcept(auto),
	assert_ind([b(believe,all)],audi,auto).
%%% Example 29
% is a variant of example 23 with a more restricted definition of c1
% ask(elementOf(amy,female))
% should succeeds
% but fails instance the current implementation
example(29) :-
	makeKnowledgeBase('ex29','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defconcept(male,not(female)),
	defprimrole(teacher),
	defrole(maleTeacher,restr(teacher,male)),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(2,femaleTeacher)])),
	assert_ind(tom,c1),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
example(30) :-
	makeKnowledgeBase('ex30','Number restrictions'),
	initKnowledgeBase,
	defprimconcept(female),
	defrole(maleTeacher,restr(teacher,not(female))),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 31
% First test example for defhl_closed
% ask(elementOf(tom,onlyMaleChildren))
% succeeds
example(31) :-
	makeKnowledgeBase('ex31','defhl_closed'),
	initKnowledgeBase,
	defconcept(onlyMaleChildren,all(child,male)),
	assert_ind(tom,peter,child),
	assert_ind(tom,chris,child),
	assert_ind(tom,tim,child),
	assert_ind(peter,male),
	assert_ind(chris,male),
	assert_ind(tim,male),
	defhl_closed(tom,_Y,child).
%%% Example 32
% First test example for abduction
% abduce(elementOf(robin,male),H,E)
% abduce(elementOf(robin,female),H,E)
example(32) :-
	makeKnowledgeBase('ex32','abduction'),
	initKnowledgeBase,
	defconcept(male,not(female)).
%%% Example 33
% Second test example for abduction
% abduce(elementOf(nixon,dove),H,E)
% abduce(elementOf(nixon,hawk),H,E)
% gives unexpected results!!!
example(33) :-
	makeKnowledgeBase('ex33','abduction'),
	initKnowledgeBase,
	defconcept(c1,and([quaker,normalQuaker])),
	defconcept(c2,and([republican,normalRepublican])),
	defprimconcept(c1,dove),
	defprimconcept(c2,hawk),
	assert_ind(nixon,quaker),
	assert_ind(nixon,republican).
%%% Example 34
% The following gives an inconsistent specification of
% the penguin - bird problem. So
% inconsistent(ex34)
% succeeds
example(34) :-
	makeKnowledgeBase('ex34',abduction),
	initKnowledgeBase,
	defprimconcept(penguin,and([bird,not(fly)])),
	defprimconcept(bird,fly),
	assert_ind(tweety,penguin),
	assert_ind(john,bird).
%%% Example 35
% This is a consistent specification of the penguin - bird problem.
% abduce(ex35,[],elementOf(john,fly),H,E).
% succeeds with
% H = [instance(kb(e1),rn(_7982,_7983,_7984,_7985),modal([]),normalBird,john,
%         hyp(_7989),ab(_7991),call(_7993),
%         proved(instance([],normalBird,john),hyp(_7989),basedOn(_8005)))],
% E = proved(instance([],fly,john),hyp([]),
%            basedOn(and([proved(instance([],bird,john),hyp([]),basedOn(abox)),
%                         proved(instance([],normalBird,john),hyp([]),
%     basedOn(usingAbHyp(instance(kb(e1),rn(_7525,_7526,_7527,_7528),modal([]),
%                           normalBird,john,hyp(_7532),ab(_7534),call(_7536),
%                           proved(instance([],normalBird,john),hyp(_7532),
%                           basedOn(_7548))))))])))
% and
% abduce(ex35,[],elementOf(tweety,fly),H,E).
% fails
example(35) :-
	makeKnowledgeBase('ex35',abduction),
	initKnowledgeBase,
	defprimconcept(penguin,and([bird,not(normalBird)])),
	defprimconcept(and([bird,normalBird]),fly),
	assert_ind(tweety,penguin),
	assert_ind(john,bird).
%%% Example 36
% Variant of example 33 giving the expected results:
% abduce(ex36,[],elementOf(nixon,dove),H,E).
% succeeds with
% H = [instance(kb(e4),rn(_8077,_8078,_8079,_8080),modal([]),
%         normalQuaker,nixon,hyp(_8084),ab(_8086),call(_8088),
%         proved(instance([],normalQuaker,nixon),hyp(_8084),basedOn(_8100)))],
% E = proved(instance([],dove,nixon),hyp([]),
%        basedOn(and([proved(instance([],quaker,nixon),hyp([]),basedOn(abox)),
%                     proved(instance([],normalQuaker,nixon),hyp([]),
%           basedOn(usingAbHyp(instance(kb(e4),rn(_7620,_7621,_7622,_7623),
%                   modal([]),normalQuaker,nixon,hyp(_7627),ab(_7629),
%                   call(_7631),proved(instance([],normalQuaker,nixon),
%                   hyp(_7627),basedOn(_7643))))))]))) 
% and
% abduce(ex36,[],elementOf(nixon,hawk),H,E).
% succeeds with
% H = [instance(kb(e4),rn(_8077,_8078,_8079,_8080),modal([]),
%         normalRepublican,nixon, hyp(_8084),ab(_8086),call(_8088),
%         proved(instance([],normalRepublican,nixon),hyp(_8084),basedOn(_8100)))],
% E = proved(instance([],dove,nixon),hyp([]),
%        basedOn(and([proved(instance([],republican,nixon),hyp([]),basedOn(abox)),
%                     proved(instance([],normalRepublican,nixon),hyp([]),
%           basedOn(usingAbHyp(instance(kb(e4),rn(_7620,_7621,_7622,_7623),
%                   modal([]),normalRepublican,nixon,hyp(_7627),ab(_7629),
%                   call(_7631),proved(instance([],normalRepublican,nixon),
%                   hyp(_7627),basedOn(_7643))))))]))) 
example(36) :-
	makeKnowledgeBase('ex36','abduction'),
	initKnowledgeBase,
	defprimconcept(and([quaker,normalQuaker]),dove),
	defprimconcept(and([republican,normalRepublican]),hawk),
	assert_ind(nixon,quaker),
	assert_ind(nixon,republican).
%%% Example 37
example(37) :-
	makeKnowledgeBase('ex37','abduction'),
	initKnowledgeBase,
	defprimconcept(rained_last_night,grass_is_wet),
	defprimconcept(sprinkler_was_on,grass_is_wet),
	defprimconcept(grass_is_wet,shoes_are_wet).
%%% Example 38
% ask(elementOf(ideaste,c2))
% should succeed
example(38) :-
	makeKnowledgeBase('ex38','disjunctive_information'),
	initKnowledgeBase,
	assert_ind(ideaste,oedipus,hasChild),
	assert_ind(oedipus,polyneikes,hasChild),
	assert_ind(ideaste,polyneikes,hasChild),
	assert_ind(polyneikes,thersandros,hasChild),
	assert_ind(oedipus,fatherMurderer),
	assert_ind(thersandros,not(fatherMurderer)),
	defconcept(c1,and([fatherMurderer,some(hasChild,not(fatherMurderer))])),
	defconcept(c2,some(hasChild,c1)).
%%% Example 39
% ask(elementOf(lucky,female))
% succeeds
example(39) :-
	makeKnowledgeBase('ex39','negation_as_failure'),
	initKnowledgeBase,
	defrole(parentOf,inverse(childOf)),
	defconcept(male,not(female)),
	defprimconcept(and([some(parentOf,'Entity'),naf(not(female))]),female),
	assert_ind(mary,lucky,childOf).
%%% Example 40
% ask(elementOf(peter,richPerson))
% succeeds.
% After
% assert_ind(peter,poorPerson)
% the query
% ask(elementOf(peter,richPerson))
% fails
example(40) :-
	makeKnowledgeBase('ex40','negation_as_failure'),
	initKnowledgeBase,
	defprimconcept(and([doctor,naf(not(richPerson))]),richPerson),
	defconcept(poorPerson,not(richPerson)),
	assert_ind(peter,doctor).
%%% Example 41
% ask(elementOf(tom,richPerson))
% succeeds.
% After 
% assert_ind(tom,poorPerson)
% the query
% ask(elementOf(tom,richPerson))
% fails
example(41) :-
	makeKnowledgeBase('ex41','negation_as_failure'),
	initKnowledgeBase,
	defrole(doctorParentOf,restr(inverse(childOf),doctor)),
	defrole(childOfDoctor,inverse(r1)),
	defprimconcept(and([some(doctorParentOf,'Entity'),naf(not(richPerson))]),richPerson),
	defconcept(poorPerson,not(richPerson)),
	assert_ind(chris,doctor),
	assert_ind(chris,tom,childOf).
%%% Example 42
% ask(elementOf(audi,fourWheels))
% succeeds.
% After
% assert_ind(audi,fiveWheels)
% the query
% ask(elementOf(audi,fourWheels))
% fails
example(42) :-
	makeKnowledgeBase('ex42','negation_as_failure'),
	initKnowledgeBase,
	defconcept(fourWheels,and([atleast(4,wheels),atmost(4,wheels)])),
	defconcept(fiveWheels,and([atleast(5,wheels),atmost(5,wheels)])),
	defprimconcept(and([car,naf(not(fourWheels))]),fourWheels),
	assert_ind(audi,car).
%%% Example 43
example(43) :-
	makeKnowledgeBase('ex43','concrete_domains'),
	initKnowledgeBase,
	defconcept(colors,set([b,y,r])),
	defconcept(blueOrYellow,set([b,y])),
	defconcept(red,set([r])),
	defconcept(blue,set([b])),
	defconcept(yellow,set([y])),
	defconcept(redOrYellow,set([r,y])),
	defconcept(blueOrRed,set([b,r])),
	defconcept(yellowOrBlue,set([y,b])).
%%% Example 44
% subsumes(c2,c1)
% should succeed
example(44) :-
	makeKnowledgeBase('ex44','concrete_domains'),
	initKnowledgeBase,
	defconcept(c1,set([a,b])),
	defconcept(c2,set([a,b,c])).
%%% Example 45
example(45) :-
	makeKnowledgeBase('ex45','concrete_domains'),
	initKnowledgeBase,
	defconcept(c1,set([a,b,c])),
	defconcept(c2,set([a,b])),
	defconcept(nc2,not(c2)).
%%% Example 46
% An insufficient specification of 
% The bmw is either yellow, blue, or red but not yellow. 
% ask(elementOf(bmw,c3))
% fails
example(46) :-
	makeKnowledgeBase('ex46','concrete_domains'),
	initKnowledgeBase,
	defconcept(c1,some(hasCol,set([yellow,blue,red]))),
	defconcept(c2,some(hasCol,not(set([yellow])))),
	defconcept(c3,some(hasCol,set([blue,red]))),
	assert_ind(bmw,c1),
	assert_ind(bmw,c2).
%%% Example 47
% A correct specification of
% The bmw is either yellow, blue, or red but not yellow. 
% ask(elementOf(bmw,c3))
% succeeds
example(47) :-
	makeKnowledgeBase('ex47','concrete_domains'),
	initKnowledgeBase,
	defconcept(c1,and([some(hasCol,set([yellow,blue,red])),all(hasCol,set([yellow,blue,red]))])),
	defconcept(c2,some(hasCol,not(set([yellow])))),
	defconcept(c3,some(hasCol,set([blue,red]))),
	assert_ind(bmw,c1),
	assert_ind(bmw,c2).
example(48) :-
	makeKnowledgeBase('ex48','concrete_concepts'),
	initKnowledgeBase,
	defconcept(oneSpouse,and([atleast(1,spouse),atmost(1,spouse)])),
	assert_ind(m1,oneSpouse),
	defprimconcept(some(inverse(spouse),set([m1])),set([g0,g1,g2])),
	assert_ind(g0,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g0])),set([m1,g1,g2])),
	assert_ind(g1,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g1])),set([m1,g0,g2])),
	assert_ind(g2,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g2])),set([m1,g0,g1])),
	defconcept(zeroSH,and([atleast(0,sh),atmost(0,sh)])),
	defconcept(oneSH,and([atleast(1,sh),atmost(1,sh)])),
	defconcept(twoSH,and([atleast(2,sh),atmost(2,sh)])),
	assert_ind(g0,zeroSH),
	assert_ind(g1,oneSH),
	assert_ind(g2,twoSH),
	defprimconcept(and([some(inverse(sh),set([m1])),set([m1])]),bot),
	defprimconcept(and([some(inverse(sh),set([g0])),set([g0])]),bot),
	defprimconcept(and([some(inverse(sh),set([g1])),set([g1])]),bot),
	defprimconcept(and([some(inverse(sh),set([g2])),set([g2])]),bot),
	defprimconcept(and([some(inverse(spouse),set([m1])),some(inverse(sh),set([m1]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g0])),some(inverse(sh),set([g0]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g1])),some(inverse(sh),set([g1]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g2])),some(inverse(sh),set([g2]))]),bot),
%	defconcept(some(sh,set([m1])),some(inverse(sh),set([m1]))),
%	defconcept(some(sh,set([g0])),some(inverse(sh),set([g0]))),
%	defconcept(some(sh,set([g1])),some(inverse(sh),set([g1]))),
%	defconcept(some(sh,set([g2])),some(inverse(sh),set([g2]))).
	defrole(sh,inverse(sh)),
	defrole(spouse,inverse(spouse)).
%%% Example 49
% ask(elementOf(p,c4))
% should fail
example(49) :-
	makeKnowledgeBase('ex49','defaults'),
	initKnowledgeBase,
	defconcept(c4,and([c5,c6])),
	defprimconcept(and([c0,naf(not(c2))]),c5),
	defprimconcept(and([c0,naf(not(c3))]),c6),
	defconcept(c1,or([not(c2),not(c3)])),
	assert_ind(p,c0),
	assert_ind(p,c1).
example(50) :-
	makeKnowledgeBase('ex50','complete_or'),
	initKnowledgeBase,
	defprimconcept(c1,c0),
	defprimconcept(not(c1),c0).
example(51) :-
	makeKnowledgeBase('ex51','functional_dependencies'),
	initKnowledgeBase,
	def(posInfl(f,d)),
	def(posInfl(h,f)),
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)),
	def(negInfl(b,e)),
	def(negInfl(e,d)),
	def(posInfl(g,e)),
	def(posInfl(a,g)),
	def(increase(a)).
example(52) :-
	makeKnowledgeBase('ex52','functional_dependencies'),
	initKnowledgeBase,
	def(increase(hasCubicCapacity)),
	def(negInfl(withRebate,hasPrice)),
	def(posInfl(hasPrice,hasOverallCost)),
	def(posInfl(hasCubicCapacity,hasListPrice)),
	def(posInfl(hasListPrice,hasPrice)),
	def(posInfl(hasCubicCapacity,hasFuelConsumption)),
	def(posInfl(hasFuelConsumption,hasOverallCost)),
	def(posInfl(hasCubicCapacity,hasMaxSpeed)),
	def(negInfl(hasCatConverter,hasMaxSpeed)),
	def(posInfl(hasCatConverter,hasFuelConsumption)),
	def(posInfl(hasCubicCapacity,hasWeight)),
	def(negInfl(hasWeight,hasMaxSpeed)).
example(53) :-
	makeKnowledgeBase('ex53','functional_dependencies'),
	initKnowledgeBase,
	def(increase(hasCubicCapacity)),
	def(infl(withRebate,hasPrice,-1.0)),
	def(infl(hasPrice,hasOverallCost,1.0)),
	def(infl(hasCubicCapacity,hasListPrice,1.2)),
	def(infl(hasListPrice,hasPrice,1.0)),
	def(infl(hasCubicCapacity,hasFuelConsumption,0.8)),
	def(infl(hasFuelConsumption,hasOverallCost,1.0)),
	def(infl(hasCubicCapacity,hasHorsePower,1.0)),
	def(infl(hasHorsePower,hasFuelConsumption,1.0)),
	def(infl(hasHorsePower,hasMaxSpeed,1.0)),
	def(infl(hasFuelType,hasMaxSpeed,0.8)),
	def(infl(hasCatConverter,hasHorsePower,-0.5)),
	def(infl(hasCubicCapacity,hasWeight,0.5)),
	def(infl(hasWeight,hasHorsePower,-1.0)).
example(54) :-
	makeKnowledgeBase('ex54','functional_dependencies'),
	initKnowledgeBase,
	def(negInfl(a,b)),
	def(posInfl(b,e)),
	def(posInfl(e,d)),
	def(negInfl(g,e)),
	def(negInfl(a,g)).
%
%	Apart from the notation identical to ex54.
%
example(55) :-
	makeKnowledgeBase('ex55','functional_dependencies'),
	initKnowledgeBase,
	def(infl(a,b,1.0)),
	def(infl(b,e,1.0)),
	def(infl(e,d,1.0)),
	def(infl(g,e,1.0)),
	def(infl(a,g,-1.0)).
example(56) :-
	makeKnowledgeBase('ex56','functional_dependencies'),
	initKnowledgeBase,
	def(infl(a,b,1.0)),
	def(infl(b,e,1.0)),
	def(infl(e,d,1.0)),
	def(infl(g,e,1.0)),
	def(infl(a,g,-1.0)),
	def(infl(f,g,0.5)),
	def(infl(f,h,-0.5)),
	def(infl(h,d,0.3)).
example(57) :-
	makeKnowledgeBase('ex57','functional_dependencies'),
	initKnowledgeBase,
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)).
example(58) :- 
	makeKnowledgeBase('ex58','functional_dependencies'),
	initKnowledgeBase,
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)),
	def(infl(e,b,-1.0)),
	def(infl(e,c,0.5)).
example(59) :-
	sigma_defKnowlegeBase('mybox','sb.lit'),
	sigma_initenv,
	sigma_primconcept(person),
	sigma_primconcept(woman,[supers([person])]),
	sigma_primconcept(man,[supers([person])]),
	sigma_disjoint(man,woman),
	sigma_primelemrole(child,'domain-range'(parent,person,person)),
	sigma_defconcept(parent,[supers([person]),
                              nr(child,1,30,2)]),
	sigma_defconcept(mother,[supers([parent,woman])]),
	sigma_defconcept(father,[supers([parent,man])]),
	sigma_defconcept(granni,[supers([grandparent,mother])]),
	sigma_defelem(harry,[element(parent)]),
	sigma_defelem(mary,[element(mother), 
                         irole(child, 
                               iname('marys-child'),
                               [nr(1,30,2), vr(harry)])]).
example(60) :-
	makeKnowledgeBase('ex60','Modal operators'),
	initKnowledgeBase,	
	modalAxioms(kd45,believe,peter),
	defprimconcept([b(believe,peter)],doctor,richPerson),
	assert_ind([b(believe,peter)],tom,doctor).
%%% Example 61
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,nest))
% deduce(elementOf(tweety,not(emu)))
% deduce(elementOf(tweety,not(cuckoo)))
% succeed
example(61) :-
	makeKnowledgeBase('ex61','Defaults and the lottery paradox'),
	initKnowledgeBase,	
	defprimconcept(and([bird,naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(not(nest))]), nest),
	defprimconcept(emu,not(fly)),
	defprimconcept(cuckoo,not(nest)),
	assert_ind(tweety,bird).
%%% Example 62
% deduce(elementOf(tweety,bird))
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,nest))
% consistent([])
% succeed
% deduce(elementOf(tweety,not(emu)))
% deduce(elementOf(tweety,emu))
% deduce(elementOf(tweety,not(cuckoo)))
% deduce(elementOf(tweety,cuckoo))
% deduce(elementOf(tweety,not(bird)))
% fail
example(62) :-
	makeKnowledgeBase('ex62','Defaults and the lottery paradox'),
	initKnowledgeBase,	
	defprimconcept(and([bird,naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(not(nest))]), nest),
	defprimconcept(emu,not(fly)),
	defprimconcept(cuckoo,not(nest)),
	defconcept(bird,or([emu,cuckoo])),
	assert_ind(tweety,bird).
%%% Example 63
% deduce(elementOf(tweety,bird))
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,nest))
% deduce(elementOf(tweety,sparrow))
% deduce(elementOf(tweety,not(emu)))
% deduce(elementOf(tweety,not(cuckoo)))
% consistent([])
% succeed
example(63) :-
	makeKnowledgeBase('ex63','Defaults and the lottery paradox'),
	initKnowledgeBase,	
	defprimconcept(and([bird,naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(not(nest))]), nest),
	defprimconcept(emu,not(fly)),
	defprimconcept(cuckoo,not(nest)),
	defconcept(bird,or([sparrow,emu,cuckoo])),
	assert_ind(tweety,bird).
%%% Example 64
% deduce(elementOf(peter,leftHandUsable))
% deduce(elementOf(peter,rightHandUsable))
% deduce(elementOf(peter,oneHandUsable))
% succeed
% deduce(elementOf(peter,bothHandsUsable))
% deduce(elementOf(peter,not(bothHandsUsable))
% fail
example(64) :-
	makeKnowledgeBase('ex64','Defaults and the lottery paradox'),
	initKnowledgeBase,	
	defprimconcept(naf(leftHandBroken),leftHandUsable),
	defprimconcept(naf(rightHandBroken),rightHandUsable),
	defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
	defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
	defconcept(bothHandsUsable,and([leftHandUsable,rightHandUsable])),
	assert_ind(peter,oneHandBroken).
%%% Example 65
% deduce(elementOf(peter,leftHandUsable))
% can prove leftHandUsable by default because
% cannot prove leftHandBroken because
% can prove oneHandBroken but
% cannot prove not(rightHandBroken) because
% cannot prove rightHandUsable because
% can prove rightHandBroken because
% can prove oneHandBroken and
% can prove not(leftHandBroken) because
% can prove leftHandUsable by default because
% cannot prove leftHandBroken because the loop check prevents
%                                     the application of any axiom
% deduce(elementOf(peter,rightHandUsable))
% deduce(elementOf(peter,not(bothHandsUsable))
% succeed
% deduce(elementOf(peter,bothHandsUsable))
% deduce(elementOf(peter,oneHandUsable))
% cannot prove oneHandUsable becauce
% (cannot prove leftHandUsable because
%  can prove leftHandBroken because
%  oneHandBroken is a fact and
%  (can prove not(rightHandBroken) because
%   can prove rightHandUsable by default because
%   cannot prove rightHandBroken because
%   can prove oneHandBroken but 
%   cannot prove not(leftHandBroken) because
%   cannot prove leftHandUsable because the loop check prevents
%                                       the application of any axiom))
% and it is also not possible possible to prove rightHandUsable
% for similar reasons
% deduce(elementOf(peter,not(oneHandUsable)))
% fail
example(65) :-
	makeKnowledgeBase('ex65','Defaults and the lottery paradox'),
	initKnowledgeBase,	
	defprimconcept(naf(leftHandBroken),leftHandUsable),
	defprimconcept(naf(rightHandBroken),rightHandUsable),
	defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
	defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
	defconcept(bothHandsUsable,and([leftHandUsable,rightHandUsable])),
	defprimconcept(leftHandBroken,not(leftHandUsable)),
	defprimconcept(rightHandBroken,not(rightHandUsable)),
	assert_ind(peter,oneHandBroken).
%%% Example 66
% deduce(elementOf(peter,leftHandUsable))
% deduce(elementOf(peter,rightHandUsable))
% deduce(elementOf(peter,oneHandUsable))
% deduce(elementOf(peter,not(bothHandsUsable))
% succeed
% deduce(elementOf(peter,bothHandsUsable))
% deduce(elementOf(peter,not(oneHandUsable)))
% fail
example(66) :-
	makeKnowledgeBase('ex66','Defaults and the lottery paradox'),
	initKnowledgeBase,	
	defprimconcept(naf(leftHandBroken),leftHandUsable),
	defprimconcept(naf(rightHandBroken),rightHandUsable),
	defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
	defconcept(oneHandUsable,or([naf(not(leftHandUsable)),naf(not(rightHandUsable))])),
	defconcept(bothHandsUsable,and([leftHandUsable,rightHandUsable])),
	defprimconcept(leftHandBroken,not(leftHandUsable)),
	defprimconcept(rightHandBroken,not(rightHandUsable)),
	assert_ind(peter,oneHandBroken).
%%% Example 67
example(67) :-
	makeKnowledgeBase('ex67','Defaults and the lottery paradox'),
        initKnowledgeBase,        
        defprimconcept(naf(leftHandBroken),leftHandUsable),
        defprimconcept(naf(rightHandBroken),rightHandUsable),
        defprimconcept(leftHandBroken,not(leftHandUsable)),
        defprimconcept(rightHandBroken,not(rightHandUsable)),
        defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
        defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
        defconcept(bothHandsBroken,and([leftHandBroken,rightHandBroken])),
        assert_ind(peter,oneHandBroken),
        assert_ind(peter,not(bothHandsBroken)).
example(68) :-
	makeKnowledgeBase('ex68','Defaults and the lottery paradox'),
        initKnowledgeBase,        
        defprimconcept(naf(bot),leftHandUsable),
        defprimconcept(naf(bot),rightHandUsable),
        defprimconcept(leftHandBroken,not(leftHandUsable)),
        defprimconcept(rightHandBroken,not(rightHandUsable)),
        defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
        defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
        defconcept(bothHandsBroken,and([leftHandBroken,rightHandBroken])),
        assert_ind(peter,oneHandBroken),
        assert_ind(peter,not(bothHandsBroken)).
%%% Example 69
% deduce(elementOf(tweety,bird))
% succeeds
% deduce(elementOf(tweety,not(bird)))
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,not(fly)))
% deduce(elementOf(tweety,nest))
% deduce(elementOf(tweety,not(nest)))
% fail
example(69) :-
	makeKnowledgeBase('ex69','Defaults and the lottery paradox'),
	initKnowledgeBase,	
	defprimconcept(and([bird,naf(exception),naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(exception),naf(not(nest))]), nest),
	defprimconcept(emu,exception),
	defprimconcept(cuckoo,exception),
	defconcept(bird,or([emu,cuckoo])),
	assert_ind(tweety,bird).
%%% Example 70
% deduce(elementOf(a,clearTop))
% deduce(elementOf(a,not(clearTop)))
% fail
% deduce(elementOf(b,clearTop))
% deduce(elementOf(b,clearTop))
% succeed
example(70) :-
	makeKnowledgeBase('ex70','Defaults and '$existential' quantification'),
	initKnowledgeBase,
	defconcept(blocked,some(on,'Entity')),
	defprimconcept(and([block,naf(blocked)]),clearTop),
	assert_ind(a,block),
	assert_ind(b,block),
	assert_ind(c,block),
	assert_ind(a,b,on).
example(71) :-
	makeKnowledgeBase('ex71','PRACMA'),
	initKnowledgeBase,
	defprimconcept(sporttyp),
	defprimconcept(envtyp),
	sigma_primconcept([b(believe,pk)],carwish,[supers([car])]),
	sigma_primelemrole([bc(want,sporttyp)],has_tyre,'domain-range'(carwish,broad_tyre,broad_tyre)),
	sigma_primelemrole([bc(believe,sporttyp)],speed,'domain-range'('2cv',low,low)),
	sigma_primelemrole([bc(want,envtyp)],has_part,'domain-range'(carwish,cat_conv,cat_conv)),
	sigma_primelemrole([bc(believe,envtyp)],speed,'domain-range'('2cv',fast,fast)),
%	modalAxioms(kd45,believe,pk),	
%	modalAxioms(kd45,want,pk),	
	modalAxioms(kd45,believe,concept(sporttyp)),
	modalAxioms(kd45,want,concept(sporttyp)),
	modalAxioms(kd45,believe,concept(envtyp)),
	modalAxioms(kd45,want,concept(envtyp)).
example(72) :-
	makeKnowledgeBase('ex72','DEMO'),
	initKnowledgeBase,
	defprimconcept(sporttyp),
	defprimconcept(umwelttyp),
	modalAxioms([b(believe,pv)],k,want,concept(sporttyp)),
	modalAxioms([b(believe,pv)],k,want,concept(umwelttyp)),
	modalAxioms(kd45,believe,all),
	sigma_primconcept([b(believe,all)], vw, [supers([auto])]),
	sigma_primconcept([b(believe,all)], opel, [supers([auto])]),
	assert_ind([b(believe,all)],polo,vw),
	assert_ind([b(believe,all)],manta,opel),
	sigma_defconcept([b(believe,pv),bc(want,sporttyp)],wunsch_auto,[supers([auto,hatSpoiler])]),
	% Anmerkung:
        % In SIGMAHL ist es m"oglich, mehrere sich erg"anzende Definitionen 
        % f"ur one Konzept (hier z.B. vw) zu haben. Damit one Konzept im
        % Verlauf des Dialogs st"andig zu verfoneern. 
	sigma_primconcept([b(believe,pv),bc(believe,sporttyp)],vw,[supers([langsam])]),
	defprimconcept([b(believe,pv),bc(believe,sporttyp)],and([auto,hatKat,naf(not(langsam))]),langsam),
	sigma_defconcept([b(believe,pv),bc(want,umwelttyp)],wunsch_auto,[supers([auto,hatKat])]),
	sigma_primconcept([b(believe,pv),bc(believe,umwelttyp)],vw,[supers([not(langsam)])]),
	assert_ind([b(believe,pv)],pk,sporttyp),
        % Anmerkung:
	% Bei der folgenden Definition reicht es nicht zu sagen, da\3
        % polo one Auto ist oder das alle glauben, da\3 polo one Auto ist,
        % da man durchaus Sachen im want haben kann, die der Realit"at 
        % widersprechen. Deshalb mu\3 pk wollen, da\3 polo one auto ist.
	assert_ind([b(believe,pv),b(want,pk)],polo,auto).
        % Demo:
        %
        % setof(C,ask([b(believe,pk)],elementOf(polo,C)),L).
        % L = [auto,langsam,'Entity',vw,not(bot)]
        % Zun"achst erbt hier der pk vom b(believe,all), den Glauben, da\3
        % polo one vw und damit one auto ist. Vom b(believe,sporttyp) erbt 
        % er, da\3 vw's langsam sind, womit auch der polo langsam ist.
        % 
        % setof(C,ask([b(believe,pk)],elementOf(manta,C)),L)
        % L = [auto,opel,'Entity',not(bot)]
        % Da es sich bei dem manta um oneen opel handelt, wird zun"achst
        % nicht angenommen, da\3 der manta langsam ist.
        %
        % assert_ind([b(believe,pv),b(believe,pk)],manta,hatKat)
        % Hiermit haben wir festgelegt, da\3 der pk glaubt, da\3 der manta
        % oneen Katalysator hat. Nun erbt der pk vom sporttyp aber auch
        % die Regel, da\3 Autos mit Katalysatoren normalerweise langsam sind.
        % Dies f"uhrt bei der Wiederholung der letzten Anfrage zu folgendem
        % Ergebnis:
        %
        % setof(C,ask([b(believe,pk)],elementOf(manta,C)),L)
        % L = [auto,hatKat,langsam,opel,'Entity',not(bot)]
        %
        % Wir k"onnen neben der Deduktion auf Abduktion verwenden:
        %
        % abduce([b(want,pk)],H,elementOf(polo,wunsch_auto),E).
        % E = proved(instance(app(_A:m(want,pk),[]),wunsch_auto,polo),
        %     basedOn(and([proved(instance(app(_A:m(want,pk),[]),auto,polo),
        %     basedOn(abox)),
        %     proved(instance(app(_A:m(want,pk),[]),hatSpoiler,polo),
        %     basedOn(usingAbHyp(instance(app(_A:m(want,pk),[]),hatSpoiler,polo))))]))),
        % H = [instance(app(_B:m(want,pk),[]),hatSpoiler,polo)]          
        % D.h. pk will den polo als Wunschauto, wenn der polo oneen Spoiler hat.
        %
        % Nun erhalten wir von PRACMA die Anweisungen zu oneem 
        % Stereotypwechsel:
        %
        % delete_ind(pk,sporttyp)
        % assert_ind(pk,umwelttyp)
        %
        % Dadurch "andern sich die Anfrageergebnisse wie folgt:
        %
        % setof(C,ask([b(believe,pk)],elementOf(polo,C)),L).
        % L = [auto,'Entity',vw,not(bot),not(langsam)]
        %
        % Der polo geh"ort nun zu den nicht langsamen Autos, da umwelttypen
        % genau dies glauben.
        % 
        % setof(C,ask([b(believe,pk)],elementOf(manta,C)),L).
        % L = [auto,hatKat,opel,'Entity',not(bot)]
        % 
        % Der Manta hat zwar immernoch oneen Katalysator, ist aber trotzdem
        % nicht langsam, da umwelttypen nicht glauben, da\3 Katalysatoren one
        % Auto langsam machen.
        %
        % Wir k"onnen auch instance diesem Fall fragen, unter welchen Uctxt"anden
        % pk den polo f"ur sone Wunschauto halten w"urde:
        %
        % abduce([b(want,pk)],H,elementOf(polo,wunsch_auto),E).
        % E = proved(instance(app(_A:m(want,pk),[]),wunsch_auto,polo),
        %     basedOn(and([proved(instance(app(_A:m(want,pk),[]),auto,polo),
        %     basedOn(abox)),
        %     proved(instance(app(_A:m(want,pk),[]),hatKat,polo),
        %     basedOn(usingAbHyp(instance(app(_A:m(want,pk),[]),hatKat,polo))))]))),
        % H = [instance(app(_B:m(want,pk),[]),hatKat,polo)]
        %
        % Wie erwartet, soll das Wunschauto von pk nun oneen Katalysator haben.
example(73) :-
	assert_ind([b(believe,all)],polo,vw),
	defprimconcept([b(believe,pv),bc(believe,sporttyp)],vw,langsam),
	assert_ind([b(believe,pv)],pk,sporttyp),
	modalAxioms(kd45,believe,pv),
	modalAxioms([b(believe,pv)],kd45,believe,concept(sporttyp)).
example(74) :-
	makeKnowledgeBase('ex74','DEMO'),
	initKnowledgeBase,
	defprimconcept(sporttyp),
	defprimconcept(umwelttyp),
	modalAxioms([b(glaubt,pv)],k,will,concept(sporttyp)),
	modalAxioms([b(glaubt,pv)],k,will,concept(umwelttyp)),
	modalAxioms(kd45,glaubt,all),
	sigma_primconcept([b(glaubt,all)], vw, [supers([auto])]),
	sigma_primconcept([b(glaubt,all)], opel, [supers([auto])]),
	assert_ind([b(glaubt,all)],polo,vw),
	assert_ind([b(glaubt,all)],manta,opel),
	defprimconcept([b(glaubt,pv),bc(will,sporttyp)],and([auto,or([hatSpoiler,hatSchiebedach])]),wunschAuto),
	defprimconcept([b(glaubt,pv),bc(glaubt,sporttyp)],vw,langsam),
	defprimconcept([b(glaubt,pv),bc(glaubt,sporttyp)],and([auto,hatKat,naf(not(langsam))]),langsam),
	defconcept([b(glaubt,pv),bc(will,umwelttyp)],wunschAuto,and([auto,hatKat])),
	defprimconcept([b(glaubt,pv),bc(glaubt,umwelttyp)],vw,not(langsam)),
	assert_ind([b(glaubt,pv)],pk,sporttyp),
	assert_ind([b(glaubt,pv),b(will,pk)],polo,auto).
example(75) :-
	makeKnowledgeBase('ex75','DEMO'),
	initKnowledgeBase,
	defprimconcept(racer),
	defprimconcept(creeper),
	modalAxioms([b(believe,ps)],k,want,concept(racer)),
	modalAxioms([b(believe,ps)],k,want,concept(creeper)),
	modalAxioms(kd45,believe,all),
	defprimconcept([b(believe,all)], vw, car),
	defprimconcept([b(believe,all)], bmw, car),
	assert_ind([b(believe,all)],beetle,vw),
	assert_ind([b(believe,all)],'bmw735',bmw),
	defprimconcept([b(believe,ps),bc(want,racer)],and([car,or([has_spoiler,has_sliding_roof])]),dream_car),
	defprimconcept([b(believe,ps),bc(believe,racer)],vw,slow),
	defprimconcept([b(believe,ps),bc(believe,racer)],and([car,has_cat_conv,naf(not(slow))]),slow),
	defconcept([b(believe,ps),bc(want,creeper)],dream_car,and([car,has_cat_conv])),
	defprimconcept([b(believe,ps),bc(believe,creeper)],vw,not(slow)),
	assert_ind([b(believe,ps)],pc,racer),
	assert_ind([b(believe,ps),b(want,pc)],beetle,car).
example(76) :-
	makeKnowledgeBase('ex76','SETHEO'),
	initKnowledgeBase,
	defprimconcept(racer),
	defprimconcept(creeper),
	modalAxioms([b(believe,ps)],k,want,concept(racer)),
	modalAxioms([b(believe,ps)],k,want,concept(creeper)),
	modalAxioms(kd45,believe,all),
	defprimconcept([b(believe,all)], vw, car),
	defprimconcept([b(believe,all)], bmw, car),
	assert_ind([b(believe,all)],beetle,vw),
	assert_ind([b(believe,all)],'bmw735',bmw),
	defprimconcept([b(believe,ps),bc(want,racer)],and([car,or([has_spoiler,has_sliding_roof])]),dream_car),
	defprimconcept([b(believe,ps),bc(believe,racer)],vw,slow),
	defconcept([b(believe,ps),bc(want,creeper)],dream_car,and([car,has_cat_conv])),
	defprimconcept([b(believe,ps),bc(believe,creeper)],vw,not(slow)),
	assert_ind([b(believe,ps)],pc,racer),
	assert_ind([b(believe,ps),b(want,pc)],beetle,car).
%%% Example 77
% the results for 
% deduce(elementOf(mary,grandparent),E)
% should be a cmp_subset of the results for
% abduce(H,elementOf(mary,grandparent),E).
example(77) :-
	makeKnowledgeBase('ex77','smallKRISExample'),
	initKnowledgeBase,
	defconcept(parent,and([person,some(child,person)])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	assert_ind(tom,father),
	assert_ind(mary,parent),
	assert_ind(mary,tom,child).
example(78) :-
	makeKnowledgeBase('beispiel8.0','Original von Baldes'),
	initKnowledgeBase,
	defprimconcept([],ps_ueber100,'Entity'),
	defprimconcept([],altesBaujahr,'Entity'),
	defprimconcept([],porsche,'Entity'),
	defprimconcept([],ford,'Entity'),
	defprimconcept([],fiat,'Entity'),
	defprimconcept([],porsche,leistungsstarkesAuto),
	defprimconcept([],leistungsstarkesAuto,verbrauchtvielBenzin),
	defprimconcept([],altesModell,verbrauchtvielBenzin),
	defprimconcept([],schlechteReifen,verbrauchtvielBenzin),
	defconcept([],leistungsstarkerMotor,ps_ueber100),
	defconcept([],leistungsstarkesAuto,leistungsstarkerMotor),
	assert_ind([],auto5,porsche),
	assert_ind([],auto3,schlechteReifen),
	assert_ind([],auto3,altesModell),
	assert_ind([],auto3,fiat),
	assert_ind([],auto2,ps_ueber100),
	assert_ind([],auto2,ford),
	assert_ind([],auto1,ps_ueber100),
	assert_ind([],auto1,porsche).
example(79) :-
	makeKnowledgeBase('beispiel8.1','Minimalbeispiel'),
	initKnowledgeBase,
	assert_ind([],auto3,schlechteReifen),
	assert_ind([],auto3,altesModell),
	defprimconcept([],altesModell,verbrauchtvielBenzin),
	defprimconcept([],schlechteReifen,verbrauchtvielBenzin).
example(80) :-
	sigma_defKnowlegeBase('mybox','sb.lit'),
	sigma_initenv,
	sigma_primconcept(person),
	sigma_primconcept(woman,[supers([person])]),
	sigma_primconcept(man,[supers([person])]),
	sigma_disjoint(man,woman),
	sigma_primelemrole(child,'domain-range'(parent,person,person)),
	sigma_defconcept(parent,[supers([person]),
                              nr(child,1,30,2)]),
	sigma_defconcept(mother,[supers([parent,woman])]),
	sigma_defconcept(father,[supers([parent,man])]),
	sigma_defconcept(granni,[supers([grandparent,mother])]).
example(81) :-
	modalAxioms([b(believe,ps)],kd45,believe,pc),
	modalAxioms(kd45,believe,ps),
	assert_ind([b(believe,ps),b(believe,pc)],audi,car).
example(82) :-
	assert_ind(car1,and([car,
                     atleast(1,restr(hasPart,seat)),
                     all(restr(hasPart,seat),
                         some(hasHl_attribute,heatable))])).
example(83) :-
	makeKnowledgeBase('ext0','modProv test 1'),
	initKnowledgeBase,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defprimconcept(person,some(sex,or([male,female]))).
example(84) :-
	makeKnowledgeBase('ext1','modProv test 2'),
	initKnowledgeBase,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defprimconcept(person,some(sex,or([male,female]))),
	defconcept(parent,and([person,some(child,person)])).
example(85) :-
	makeKnowledgeBase('ext1','modProv test 2'),
	initKnowledgeBase,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defprimconcept(person,some(sex,or([male,female]))),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,some(sex,female)])),
	defconcept(father,and([parent,not(mother)])).
example(86) :-
	makeKnowledgeBase('ext2','modProv test 3'),
	initKnowledgeBase,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defprimconcept(person,some(sex,or([male,female]))),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,some(sex,female)])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,some(sex,male))])).
example(87) :-
	makeKnowledgeBase('ext3','modProv test 4'),
	initKnowledgeBase,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defconcept(males,some(sex,male)),
	defconcept(females,some(sex,female)),
	defprimconcept(person,some(sex,or([male,female]))),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,some(sex,female)])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,some(sex,male))])),
	assert_ind(tom,father),
	assert_ind(tom,peter,child),
	assert_ind(tom,harry,child),
	assert_ind(mary,parent_with_sons_only),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
example(88) :-
	makeKnowledgeBase('person','person.tbox from KRIS'),
	initKnowledgeBase,
	defprimconcept(female,not(male)),
	defconcept(man,and([human,male])),
	defconcept(woman,and([human,female])),
	defconcept(mother,and([woman,some(child,human)])),
	defconcept(father,and([man,some(hild,human)])),
	defconcept(parent,or([mother,father])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(father_with_sons_only,and([father,all(child,male)])).
example(89) :-
	makeKnowledgeBase('fss','Knowledge base FSS'),
	initKnowledgeBase,
	defprimconcept(sbone),
	defprimconcept(lexicon,sbone),
	defprimconcept(fss,sbone),
	defprimconcept(pointing,fss),
	defprimconcept(vague_p,pointing),
	defprimconcept(standard_p,pointing),
	defprimconcept(encircling_p,pointing),
	defprimconcept(exact_p,pointing),
	defprimconcept(time,fss),
	defprimconcept(moment,time),
	defprimconcept(period,time),
	defprimconcept(interval,time),
	defprimconcept(yearly,interval),
	defprimconcept(jaehrlich,and([yearly, lexicon])),
	defprimconcept(weekly,interval),
	defprimconcept(woechentlich,and([lexicon, weekly])),
	defprimconcept(daily,interval),
	defconcept(taeglich,and([daily,lexicon])),
	defprimconcept(monthly,interval),
	defconcept(monat,and([monthly, lexicon])),
	defprimconcept(speech_act, fss),
	defprimconcept(order,speech_act),
	defprimconcept(assertion,speech_act),
	defprimconcept(question,speech_act),
	defprimconcept(interjection,speech_act),
	defprimconcept(determiner,fss),
	defconcept(d,and([determiner, lexicon])),
	defprimconcept(indefinite,determiner),
	defconcept(one,and([indefinite,lexicon])),
	defprimconcept(cardinal,indefinite),
	defconcept(number35,and([cardinal, lexicon])),
	defprimconcept(number50,and([cardinal,lexicon])),
	defconcept(fuenfunddreissig,and([cardinal, lexicon])),
	defprimconcept(interrogative,determiner),
	defprimconcept(definite,determiner),
	defprimconcept(demonstrative,definite),
	defconcept(der,and([definite, lexicon])),
	defconcept(die,and([definite, lexicon])),
	defprimconcept(possessive,definite),
	defconcept(dem,and([lexicon, definite])),
	defconcept(mone,and([lexicon, determiner])),
	defprimconcept(property_filler, fss),
	defprimconcept(adjective_property,property_filler),
	defprimconcept(truth_value,adjective_property),
	defprimrole(truth_mod),
	defconcept(abstract_thing,and([thing,some(truth_mod,'Entity'),some(truth_mod,'Entity')])),
	defprimconcept(atom_codes,abstract_thing),
	defconcept(thing,and([property_filler,
	some(det,determiner),
	some(deitic_mpd,pointing),
	some(named,atom_codes)])),
	defconcept(predicate,and([fss,
	some(subject,thing),
	some(purpose,predicate),
	some(time,time),
	some(illoc,speech_act),
	some(cause,predicate),
	some(result,thing),
	some(location,thing)])),
	defprimconcept(state,and([predicate,some(time,period)])),
	defconcept(human,and([animate,some(volition,volitional_sq)])),
	defconcept(geographical_object,and([inanimate,some(origin_mod,origin)])),
	defprimconcept(wohn,and([lexicon, state,
	some(agent,human),
	some(location_wohn,location)])),
	defprimconcept(qualitative,adjective_property),
	defprimconcept(quality,qualitative),
	defprimconcept(colour, quality),
	defconcept(concrete_thing,and([thing,some(colour_mod,colour)])),
	defprimconcept(relation,adjective_property),
	defconcept(individual,and([concrete_thing,some(relative_mod,relation)])),
	defconcept(mass_noun,concrete_thing),
	defprimconcept(material,mass_noun),
	defconcept(inanimate,and([individual,some(material_mod,material)])),
	defprimconcept(weight,quality),
	defconcept(motion,and([action,some(destination,geographical_object),
	some(source,geographical_object)])),
	defconcept(motion_by_means,and([motion,some(means,touchable_object)])),
	defconcept(fahr,and([lexicon, motion_by_means])),
	defconcept(geh,and([lexicon, motion])),
	defconcept(treff,and([lexicon, action])),
	defconcept(werf,and([lexicon, action, some(concerned,'Entity')])),
	defconcept(causative,action),
	defconcept(productive,and([causative,some(result,thing)])),
	defconcept(cmp_write,and([productive,some(concerned,thing)])),
	defprimconcept(canvas,touchable_object),
	defconcept(enter,and([cmp_write,some(location_enter,canvas)])),
	defconcept(arbeit,and([lexicon, action])),
	defprimconcept(repeat,action),
	defprimconcept(wiederhol,and([repeat, lexicon])),
	defconcept(transaction,and([action,some(beneficative,human)])),
	defconcept(schenk,and([lexicon,transaction,some(concerned,thing)])),
	defprimconcept(zahl,and([lexicon, transaction])),
	defconcept(kauf,and([lexicon,transaction,some(concerned,thing)])),
	defprimconcept(reason,action),
	defprimconcept(verursach,and([lexicon, reason])),
	defconcept(unterricht,and([predicate, lexicon])),
	defprimconcept(property,predicate),
	defconcept(value_property,and([property,some(measure,abstract_thing)])),
	defconcept(kost,and([cost, lexicon])),
	defprimconcept(deducte,property),
	defprimconcept(absetzbar,and([deducte, lexicon])),
	defprimconcept(sone,and([lexicon, property])),
	defprimconcept(besitzen,and([lexicon, property])),
	defprimconcept(haben,and([lexicon, property,
	some(has_property_haben,thing)])),
	defprimconcept(origin,adjective_property),
	defprimconcept(state_q,qualitative),
	defprimconcept(volitional_sq,state_q),
	defprimconcept(physical_sq,state_q),
	defprimconcept(klone,and([lexicon, physical_sq])),
	defprimconcept(gross,and([lexicon, physical_sq])),
	defprimconcept(rot,and([colour, lexicon])),
	defprimconcept(worth,quality),
	defprimconcept(voelklingen_name),
	defprimconcept(gi,and([abstract_thing, lexicon])),
	defconcept(indication_of_quantity,
	and([abstract_thing,
	some(quantity,cardinal)])),
	defconcept(dm,and([indication_of_quantity, lexicon])),
	defprimconcept(schroneer,and([profession, lexicon])),
	defprimconcept(informatiker,and([profession, lexicon])),
	defprimconcept(action_content,and([abstract_thing, action])),
	defprimconcept(motion_content,and([action_content, motion])),
	defprimconcept(motion_by_means_content,and([motion_by_means, action_content])),
	defprimconcept(cost,abstract_thing),
	defprimconcept(kosten,and([lexicon, cost])),
	defprimconcept(geld,and([lexicon, cost])),
	defprimconcept(profession,abstract_thing),
	defprimconcept(tax_action,abstract_thing),
	defprimconcept(steuerhandlung,and([lexicon, tax_action])),
	defprimconcept(number,abstract_thing),
	defconcept(fahrt,and([thing, lexicon])),
	defconcept(das,and([thing, lexicon])),
	defconcept(animate,and([individual,some(physis_mod,physical_sq)])),
	defprimconcept(frau,and([human, lexicon])),
	defconcept(mann,and([human, lexicon])),
	defconcept(junge,and([human, lexicon])),
	defprimconcept(sie,and([lexicon, human])),
	defprimconcept(person,and([lexicon, human])),
	defconcept(karl,and([lexicon, human])),
	defconcept(ich,and([lexicon, human])),
	defconcept(peter,and([lexicon, human])),
	defprimconcept(plant,animate),
	defconcept(animal,and([animate,some(volition,volitional_sq)])),
	defprimconcept(town,geographical_object),
	defconcept(voelklingen,and([town, lexicon])),
	defprimconcept(saarbruecken,and([town, lexicon])),
	defprimconcept(dudweiler,and([town, lexicon])),
	defprimconcept(saarlouis,and([lexicon, town])),
	defprimconcept(berlin,and([lexicon, town])),
	defprimconcept(wald,and([geographical_object, lexicon])),
	defprimconcept(hier,and([geographical_object, lexicon])),
	defprimconcept(ort,and([lexicon, geographical_object])),
	defprimconcept(information,inanimate),
	defprimconcept(string,information),
	defprimconcept(system,and([human, inanimate])),
	defconcept(vehicle,and([touchable_object,some(worth_mod,worth)])),
	defprimconcept(fahrrad,and([vehicle, lexicon])),
	defprimconcept(bus,and([vehicle, lexicon])),
	defprimconcept(motorrad,and([vehicle, lexicon])),
	defprimconcept(buch,and([lexicon, touchable_object])),
	defprimconcept(kochbuch,and([lexicon, touchable_object])),
	defprimconcept(result,touchable_object),
	defprimconcept(auktion,and([lexicon, thing])),
	defprimconcept(was,and([lexicon, thing])).
example(90) :-
	example(89),
	assert_ind(birthday,yearly),
	assert_ind(hello,speech_act),
	assert_ind(plane1,thing).
/**********************************************************************
 *
 * @(#) fdwAbduce.pll 1.1@(#)
 *
 */

aux_abduce(KB,World,[],change(Y,_),Ws) :-
	bagof(W,Z^changingInfl(KB,World,Z,Y,W),Ws),
	!.

aux_abduce(_,_,[],_,[]).

aux_abduce(KB,World,[change(X,Wx)|Hs],change(Y,_),[Wy|Ws]) :-
	infl(KB,World,X,Y,Wxy),
	not(given_change(KB,World,X,_)),
	weightOf_change(Wx,Wxy,Wy),
	aux_abduce(KB,World,Hs,change(Y,_),Ws).

aux2_abduce(_,_,_,[]).

aux2_abduce(KBName,CTX,change(X,Wx),[Change|Changes]) :-
	nonvar(Change),
	abduce(KBName,CTX,change(X,Wx),Change,[]),
	abduce(KBName,CTX,change(X,Wx),Changes,[]).

/***********************************************************************
 *
 * default_change(+-Change,+-WeightedChange)
 *
 *	Change is of the form 
 *		increase(+-X), noChange(+-X), decrease(+-X).
 *
 *	WeightedChange is of the form
 *		change(+-X,+-W)
 *	with W boneg the default weight associated with Change.
 */

default_change(increase(X),change(X,W)) :-
	nonvar(W),
	W > 0.0.

default_change(decrease(X),change(X,W)) :-
	nonvar(W),
	W < 0.0.

default_change(noChange(X),change(X,W)) :-
	nonvar(W),
	W = 0.0.

default_change(increase(X),change(X,1.0)).

default_change(decrease(X),change(X,-1.0)).

default_change(noChange(X),change(X,0.0)).

/***********************************************************************
 *
 * default_changes(+-Changes,+-WeightedChanges)
 *
 *	Changes is a list of
 *		increase(+-X), noChange(+-X), decrease(+-X)
 *	predicates.
 *
 *	WeightedChanges is a list of
 *		change(+-X,+-W)
 *	predicates where W boneg the default weight associated 
 *	with the appropriate Changes predicates.
 */

default_changes([],[]).

default_changes([Change|Changes],[WeightedChange|WeightedChanges]) :-
	default_change(Change,WeightedChange),
	default_changes(Changes,WeightedChanges).










/**********************************************************************
 *
 * @(#) fdwUserInterface.pll 1.1@(#)
 *
 */


/***********************************************************************
 *
 * initFuncdep
 *
 *	asserts default given_ clauses which prevent errors if the user
 *	has not (implicitly) defined any given_ clauses.
 */

initFuncdep :-
	assertz((given_inflLink(_,_,_,_) :- !, fail)),
	assertz((given_change(_,_,_,_) :- !, fail)).
	

/***********************************************************************
 *
 * initialize, initialise
 *
 *	Similar to initialize instance
 *	~hustadt/pop/sigmahl/sigmahl-0.0.6/userInterface.pll
 */

% For those of us who prefer the alternative spelling
initialise :-
	initialize.

/***********************************************************************
 *
 * listFDW
 *
 *	lists the internal representation of the information defined by 
 *	the user.
 */

listFDW :-
	listing(given_inflLink),
	listing(given_change).

/***********************************************************************
 *
 * showFDW
 *
 *	displays the user defined information instance the knowledge base.
 *	(Similar to showKnowledgeBase.)
 */

showFDW :-
        getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,Comment),
        cmp_print('Functional Dependencies: '),
        cmp_print(KBName),
        cmp_print(' ('),
        cmp_print(Comment),
        cmp_print(')'),
        cmp_nl,
	showFDW(KB).

showFDW(KB) :-
	showInfl(KB),
	showChange(KB).

showInfl(KB) :-
	given_inflLink(KB,World,app(_,W,X),Y),
	cmp_print(World),
	cmp_print('infl('),
	cmp_print(X), cmp_print(','), cmp_print(Y), cmp_print(','), cmp_print(W), cmp_print(').'),
	cmp_nl,
	fail.

showInfl(_).

showChange(KB) :-
	given_change(KB,World,X,W),
	cmp_print(World),
	cmp_print('change('),
	cmp_print(X), cmp_print(','), cmp_print(W), cmp_print(').'),
	cmp_nl,
	fail.

showChange(_).

showFD :-
        getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,Comment),
        cmp_print('Functional Dependencies: '),
        cmp_print(KBName),
        cmp_print(' ('),
        cmp_print(Comment),
        cmp_print(')'),
        cmp_nl,
	showFD(KB).

showFD(KB) :-
	showPosInfl(KB),
	showNegInfl(KB),
	showNoInfl(KB),
	showIncrease(KB),
	showDecrease(KB),
	showNoChange(KB).

showPosInfl(KB) :-
	given_inflLink(KB,World,app(_,W,X),Y),
	W > 0.0,
	cmp_print(World),
	cmp_print('posInfl('),
	cmp_print(X), cmp_print(','), cmp_print(Y), cmp_print(').'),
	cmp_nl,
	fail.

showPosInfl(_).

showNegInfl(KB) :-
	given_inflLink(KB,World,app(_,W,X),Y),
	W < 0.0,
	cmp_print(World),
	cmp_print('negInfl('),
	cmp_print(X), cmp_print(','), cmp_print(Y), cmp_print(').'),
	cmp_nl,
	fail.

showNegInfl(_).

showNoInfl(KB) :-
	given_inflLink(KB,World,app(_,0.0,X),Y),
	cmp_print(World),
	cmp_print('noInfl('),
	cmp_print(X), cmp_print(','), cmp_print(Y), cmp_print(').'),
	cmp_nl,
	fail.

showNoInfl(_).

showIncrease(KB) :-
	given_change(KB,World,X,W),
	W > 0.0,
	cmp_print(World),
	cmp_print('increase('),
	cmp_print(X), cmp_print(').'),
	cmp_nl,
	fail.

showIncrease(_).

showDecrease(KB) :-
	given_change(KB,World,X,W),
	W < 0.0,
	cmp_print(World),
	cmp_print('decrease('),
	cmp_print(X), cmp_print(').'),
	cmp_nl,
	fail.

showDecrease(_).

showNoChange(KB) :-
	given_change(KB,World,X,0.0),
	cmp_print(World),
	cmp_print('noChange('),
	cmp_print(X), cmp_print(').'),
	cmp_nl,
	fail.

showNoChange(_).

/***********************************************************************
 *
 * def(+KBName,+CTX,:+Fact)
 *
 *	Fact is one of 
 *		infl(+X,+Y,+W),
 *		posInfl(+X,+Y), negInfl(+X,+Y), noInfl(+X,+Y),
 *		change(+X,+W),
 *		increase(+X), decrease(+X).
 *
 *	This predicate is used to update the knowledge base of
 *	information about the functional dependencies. The definition
 *	of multiple influences between hl_attributes and multiple changes 
 *	on an hl_attribute are prevented. 
 *
 *	Note that, X and Y denote roles/hl_attributes (of cars) and W
 *	denotes the weight of X influencing Y or W denotes the weight
 *	of change of an hl_attribute. 
 *
 *	posInfl is assigned the weight 1.0, negInfl the weight -1.0 and 
 *	noInfl the weight 0.0. The weights for increase, decrease and
 *	noChange are 1.0, -1.0 and 0.0, respectively.
 */

def(KBName,CTX,infl(X,Y,W)) :-
	get_KB_World(KBName,CTX,KB,World),
	atomic(X),
	assertNames(KB,World,X,role),
	atomic(Y),
	assertNames(KB,World,Y,role),
	wellDefined_InflWeight(W),
	not(given_inflLink(KB,World,app(_,_,X),Y)),
	gensym(sk,F),
	asserta(given_inflLink(KB,World,app(F,W,X),Y)).

def(KBName,CTX,change(X,W)) :-
	get_KB_World(KBName,CTX,KB,World),
	atomic(X),
	assertNames(KB,World,X,role),
	wellDefined_ChangeWeight(W),
	not(given_change(KB,World,X,_)),
	asserta(given_change(KB,World,X,W)).

def(KBName,CTX,posInfl(X,Y)) :-
	def(KBName,CTX,infl(X,Y,1.0)).

def(KBName,CTX,negInfl(X,Y)) :-
	def(KBName,CTX,infl(X,Y,-1.0)).

def(KBName,CTX,noInfl(X,Y)) :-
	def(KBName,CTX,infl(X,Y,0.0)).

def(KBName,CTX,increase(X)) :-
	def(KBName,CTX,change(X,1.0)).

def(KBName,CTX,decrease(X)) :-
	def(KBName,CTX,change(X,-1.0)).

def(KBName,CTX,noChange(X)) :-
	def(KBName,CTX,change(X,0.0)).

/***********************************************************************
 *
 * def(:+Fact)
 *
 *	calls def(+KBName,+CTX,:+Fact) with default knowledgeBase KBName 
 *	and empty modal sequence.
 */

def(Fact) :-
        getCurrentKnowledgeBase(KBName),
	def(KBName,[],Fact).

/***********************************************************************
 *
 * def(+KBName,:+Fact)
 *
 *	calls def(+KBName,+CTX,:+Fact) with default knowledgeBase KBName 
 *	and empty modal sequence.
 */

def(KBName,Fact) :-
        knowledgeBase(KBName,_,_),
	def(KBName,[],Fact).

/***********************************************************************
 *
 * def(+CTX,:+Fact)
 *
 *	calls def(+KBName,+CTX,:+Fact) with default knowledgeBase KBName 
 *	and empty modal sequence.
 */

def(CTX,Fact) :-
        nonvar(CTX),
        (CTX = [] ; CTX = [_|_]),
        !,
        getCurrentKnowledgeBase(KBName),
	def(KBName,[],Fact).

/***********************************************************************
 *
 * undef(+KBName,+CTX,:+-Fact)
 *
 *	retracts all facts matching Fact.
 */

undef(KBName,CTX,infl(X,Y,W)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_inflLink(KB,CTX,app(_,W,X),Y)),
	fail.
undef(KBName,CTX,change(X,W)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_change(KBName,CTX,X,W)),
	fail.
undef(KBName,CTX,posInfl(X,Y)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_inflLink(KB,CTX,app(_,1.0,X),Y)),
	fail.
undef(KBName,CTX,negInfl(X,Y)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_inflLink(KB,CTX,app(_,-1.0,X),Y)),
	fail.
undef(KBName,CTX,noInfl(X,Y)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_inflLink(KB,CTX,app(_,0.0,X),Y)),
	fail.
undef(KBName,CTX,increase(X)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_change(KB,CTX,X,1.0)),
	fail.
undef(KBName,CTX,decrease(X)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_change(KB,CTX,X,-1.0)),
	fail.
undef(KBName,CTX,noChange(X)) :-
        knowledgeBase(KBName,KB,_),
	retract(given_change(KB,CTX,X,0.0)),
	fail.
undef(_,_,_).

/***********************************************************************
 *
 * undef(:+-Fact)
 *
 *	retracts all facts matching Fact instance default knowledgeBase and
 *	default modal context.
 */

undef(Fact) :-
        getCurrentKnowledgeBase(KBName),
	undef(KBName,[],Fact).

/***********************************************************************
 *
 * undef(+KBName,:+-Fact)
 *
 *	retracts all facts matching Fact instance default knowledgeBase and
 *	default modal context.
 */

undef(KBName,Fact) :-
        knowledgeBase(KBName,_,_),
	!,
	undef(KBName,[],Fact).

/***********************************************************************
 *
 * undef(+CTX,:+-Fact)
 *
 *	retracts all facts matching Fact instance default knowledgeBase and
 *	default modal context.
 */

undef(CTX,Fact) :-
        (CTX = [] ; CTX = [_|_]),
        !,
        getCurrentKnowledgeBase(KBName),
	undef(KBName,[],Fact).

/***********************************************************************
 *
 * get_KB_World(+KBName,+CTX,-KB,-World),
 *
 */

get_KB_World(KBName,CTX,KB,World) :-
	knowledgeBase(KBName,KB,_),
        convertCTX(KB,[[],true],CTX,[],[World,G1],_),
        call(G1).

/**********************************************************************
 *
 * @(#) folToClause.pll 1.2@(#)
 *
 */
%---------------------------------------------------------------------------!

% Module:       folToClause.pll
% Purpose:      Translation of first-order logic formulae to clauses
%---------------------------------------------------------------------------!
%
%
% Syntax of first-order logic formulae
% ====================================
% Atom    => instance(X,ConceptName)
%            eq(PrologTerm,PrologTerm)
%            hl_holds(RoleName,PrologTerm,PrologTerm)
%
% Formula =>   Atom
%            | and([Formula,Formula])
%            | or([Formula,Formula])
%            | not(Formula)
%            | '=>'(Formula,Formula)
%            | '<=>'(Formula,Formula)
%            | forall(PrologVar,Formula)
%            | exists(PrologVar,Formula)
%> % Syntax of clauses
% =================
% Clause  => cl(Head,Body)
% Head    =>   []
%            | [Atom|Head]
% Body    =>   []
%            | [Atom|Body]
%----------------------------------------------------------------------
% translate(+F,-C)
% Parameter: F   First-order formula
%            C   Clause

translate(X,Clauses) :-
	implout(X,X1),
	negin(X1,X2),
	skolem(X2,X3,[]),
	univout(X3,X4),
	conjn(X4,X5),
	cmp_clausify(X5,Clauses,[]).


%----------------------------------------------------------------------
% implout(+F1,-F2)
% Arguments: F1   First-order formula
%            F2   First-order formula
% removes implications and equivalences instance F1 resulting instance F2
%

implout(and(A,B),O):-!,
	implout(and([A,B]),O).

implout(or(A,B),O):-!,
	implout(or([A,B]),O).

%implout(instance(A,B),O):-!,implout(instance([],B,A),O).
	
implout('<=>'(P,Q),and([or([not(P1),Q1]),or([P1,not(Q1)])])) :-
	option(translationMode,flatRelational),
	!,
	implout(P,P1),
	implout(Q,Q1).
implout('<=>'(P,Q),or([and([P1,Q1]),and([not(P1),not(Q1)])])) :-
	!,
	implout(P,P1),
	implout(Q,Q1).
implout('=>'(P,Q),or([not(P1),Q1])) :-
	!,
	implout(P,P1),
	implout(Q,Q1).
implout(forall(X,P),forall(X,P1)) :-
	!,
	implout(P,P1).
implout(exists(X,P),exists(X,P1)) :-
	!,
	implout(P,P1).
implout(and(L),and(L1)) :-
	!,
	map(implout,L,L1).
implout(or(L),or(L1)) :-
	!,
	map(implout,L,L1).
implout(not(P),not(P1)) :-
	!,
	implout(P,P1).
implout(P,P).

%----------------------------------------------------------------------
% negin(+F1,-F2)
% Parameter: F1   First-order formula
%            F2   First-order formula
% computes the negation normal form of F1 
%
%  

negin(P,P):-isSlot(P),!.
negin(P,P):-not(compound(P)),!.

negin(not(P),P1) :-
	!,
	neg(P,P1).
negin(forall(X,P),forall(X,P1)) :-
	!,
	negin(P,P1).
negin(exists(X,P),exists(X,P1)) :-
	!,
	negin(P,P1).
negin(and(L),and(L1)) :-
	!,
	map(negin,L,L1).
negin(or(L),or(L1)) :-
	!,
	map(negin,L,L1).
negin(P,P).

%----------------------------------------------------------------------
% neg(+F1,-F2)
% Parameter: F1   First-order formula
%            F2   First-order formula
% negates formula F1 to get F2

neg(P,P):-isSlot(P),!.
neg(P,P):-not(compound(P)),!.

neg(not(P),P1) :-
	!,
	negin(P,P1).
neg(forall(X,P),exists(X,P1)) :-
	!,
	neg(P,P1).
neg(exists(X,P),forall(X,P1)) :-
	!,
	neg(P,P1).
neg(and(L),or(L1)) :-
	!,
	map(neg,L,L1).
neg(or(L),and(L1)) :-
	!,
	map(neg,L,L1).
neg(P,not(P)).

%----------------------------------------------------------------------
% skolem(+F1,-F2,*Vars)
% Parameter: F1     First-order formula
%            F2     First-order formula
%            Vars   List of variables
% F2 is the skolemized form of F1.
%
% 

skolem(forall(X,P),forall(X,P1),Vars) :-
	!,skolem(P,P1,[X|Vars]).
	
skolem(exists(X,P),P2,Vars) :-
	!,
	skolem(P,P1,Vars),
	getSkolemGen(P,F), %gensym(f,F),
	Sk =.. [F|Vars],
	cmp_subst(P1,P2,X,Sk).
	
skolem(and(L),and(L1),Vars) :-
	!,
	map(skolem,[Vars],L,L1).
skolem(or(L),or(L1),Vars) :-
	!,
	map(skolem,[Vars],L,L1).
skolem(P,P,_).


%----------------------------------------------------------------------
% cmp_subst(+F1,-F2,+X,+Sk)
% Parameter: F1     First-order formula
%            F2     First-order formula
%            X      Variable that will be substituted
%            Sk     Skolem term 
% substitutes Sk for X instance formula F1.
% 
% 

cmp_subst(T1,T2,X,Sk) :-
	(atomic(T1) ; isSlot(T1)),
	T1 == X,
	!,
	T2 = Sk.
cmp_subst(T1,T2,X,_Sk) :-
	(atomic(T1) ; isSlot(T1)),
	not(T1 == X),
	!,
	T2 = T1.
cmp_subst(forall(Y,P),forall(Y,P),X,_Sk) :-
	X == Y,
	!.
cmp_subst(forall(Y,P),forall(Y,P1),X,Sk) :-
	!,
	cmp_subst(P,P1,X,Sk).
cmp_subst(exists(Y,P),exists(Y,P),X,_Sk) :-
	X == Y,
	!.
cmp_subst(exists(Y,P),exists(Y,P1),X,Sk) :-
	!,
	cmp_subst(P,P1,X,Sk).
cmp_subst(and(L),and(L1),X,Sk) :-
	!,
	map(cmp_subst,[X,Sk],L,L1).
cmp_subst(or(L),or(L1),X,Sk) :-
	!,
	map(cmp_subst,[X,Sk],L,L1).
cmp_subst(not(P),not(P1),X,Sk) :-
	!,
	cmp_subst(P,P1,X,Sk).
cmp_subst(T1,T2,X,Sk) :-
	!,
	T1 =.. [F|Args],
	map(cmp_subst,[X,Sk],Args,Args1),
	T2 =.. [F|Args1].

%----------------------------------------------------------------------
% univout(+F1,-F1)
% Parameter: F1   First-order formula
%            F2   First-order formula
% removes quantifiers
%
% 

univout(forall(_X,P),P1) :-
	!,
	univout(P,P1).
univout(and(L),and(L1)) :-
	!,
	map(univout,L,L1).
univout(or(L),or(L1)) :-
	!,
	map(univout,L,L1).
univout(P,P).

%----------------------------------------------------------------------
% conjn(+F1,-F2)
% Parameter: F1   First-order formula
%            F2   First-order formula
% computes the conjunctive normal form of F1
%
% 

conjn(and(L),R) :-
	!,
	map(conjn,L,L1),
	conjn1(and(L1),R).
conjn(or(L),R) :-
	!,
	map(conjn,L,L1),
	conjn1(or(L1),R).
conjn(P,P).

conjn1(or([and([P,Q]),R]),and([P1,Q1])) :-
	!,
	conjn(or([P,R]),P1),
	conjn(or([Q,R]),Q1).
conjn1(or([R,and([P,Q])]),and([P1,Q1])) :-
	!,
	conjn(or([P,R]),P1),
	conjn(or([Q,R]),Q1).
conjn1(P,P).

cmp_clausify(and([P,Q]),C1,C2) :-
	!,
	cmp_clausify(P,C1,C3),
	cmp_clausify(Q,C3,C2).
cmp_clausify(P,[cl(A,B)|Cs],Cs) :-
	cmp_inclause(P,A,[],B,[]),
	!.
cmp_clausify(_,C,C).

cmp_inclause(or([P,Q]),A,A1,B,B1) :-
	!,
	cmp_inclause(P,A2,A1,B2,B1),
	cmp_inclause(Q,A,A2,B,B2).
cmp_inclause(not(P),A,A,B1,B) :-
	!,
	not(memq(P,A)),
	cmp_putin(P,B,B1).
cmp_inclause(P,A1,A,B,B) :-
	not(memq(P,B)),
	cmp_putin(P,A,A1).

cmp_putin(X,[],[X]) :-
	!.
cmp_putin(X,[Y|L],L) :-
	X == Y,
	!.
cmp_putin(X,[Y|L],[Y|L1]) :-
	cmp_putin(X,L,L1).


	
memq(_X,[]) :-
	!,
	fail.
memq(X,[Y|_L]) :-
	X == Y,
	!.
memq(X,[_|L]) :-
	memq(X,L).
	
	


/**********************************************************************
 *
 * @(#) hop.pll 1.4@(#)
 *
 */

/***********************************************************************
 * 
 * hop_map(+Pred,+InList,-OutList)
 * calls Pred(X,Y) for all X instance InList and collects all Y to get
 * OutList.
 *
 */

hop_map(_Pred,[],[]) :- !.
hop_map(Pred,[H1|T1],[H2|T2]) :-
	Clause =.. [Pred,H1,H2],
	call(Clause),
	hop_map(Pred,T1,T2).

/***********************************************************************
 * 
 * hop_map(+Pred,+[A1,...,An],+InList,-OutList)
 * calls Pred(A1,...,An,X,Y) for all X instance InList and collects all Y to 
 * get OutList.
 *
 */

hop_map(_Pred,_Args,[],[]) :- !.
hop_map(Pred,Args,[H1|T1],[H2|T2]) :-
	Clause =.. [Pred|[H1,H2|Args]],
	call(Clause),
	hop_map(Pred,Args,T1,T2).

mapGoal(_Goal,_X,[]) :-
	!.
mapGoal(Goal,X,[(Y,_PTY)|L1]) :-
	not(not((X = Y, call(Goal)))),
	mapGoal(Goal,X,L1).
	
/**********************************************************************
 *
 * try(+G)
 * calls G and succeeds even if G fails.
 *
 */

try(G)  :- call(G).
try(_G) :- true.


/**********************************************************************
 *
 * doboth(G1,G2)
 * calls G1 and if G1 succeeds G2 is called.
 *
 */

doboth(G1,G2) :-
	call(G1),
	call(G2).

/**********************************************************************
 *
 * tell(GoalList)
 * calls all the goals given instance argument GoalList which is either a
 * list of PROLOG goals or a single PROLOG goal.
 *
 */

callList([]) :-
	!.
callList([G1|GL]) :-
	!,
	call(G1),
	callList(GL).
callList(G1) :-
	call(G1).


/**********************************************************************
 *
 * toList(+ListTerm,-List)
 * converts a LISP-like list ListTerm into a PROLOG-like list List
 *
 */

tolist(nil,[]) :- !.
tolist(cons(A,L1),[A|L2]) :-
	tolist(L1,L2).

/**********************************************************************
 *
 * setofOrNil(A,B,C)
 * succeeds with the same result as setof(A,B,C) if setof(A,B,C) 
 * succeeds. Otherwise C will be instantiated with the empty list.
 *
 */

setofOrNil(A,B,C) :-
	setof(A,B,C),
	!.
setofOrNil(A,B,[]) :-
	!.

/**********************************************************************
 *
 * bagofOrNil(A,B,C)
 * succeeds with the same result as bagof(A,B,C) if bagof(A,B,C) 
 * succeeds. Otherwise C will be instantiated with the empty list.
 *
 */

bagofOrNil(A,B,C) :-
	setof(A,B,C),
	!.
bagofOrNil(A,B,[]) :-
	!.

/**********************************************************************
 *
 * @(#) infl.pll 1.5@(#)
 *
 */

/***********************************************************************
 *
 * getInflDescription(+KB,+World,-DescriptY,+-Y)
 *
 *	Given a role Y this predicate returns its description instance terms
 *	of the least hl_attribute on which Y depends as a possible chain 
 *	of influence relationships. DescriptY is an hl_attribute or it 
 *	is of the form app(sk_,W,Z) where W denotes a weight.
 *
 *	This predicate is now superfluous. Its replacement is the one
 *	with three arguments.
 */

getInflDescription(KB,World,app(F,W,DescriptZ),Y) :-
	given_inflLink(KB,World,app(F,W,Z),Y),
	getInflDescription(KB,World,DescriptZ,Z).

getInflDescription(KB,World,X,X) :-
	atom(X),
	!,
	not(given_inflLink(KB,World,_,X)).

getInflDescription(KB,World,X,X) :-
	isSlot(X),
	!.

/***********************************************************************
 *
 * getInflDescription(+KB,+World,-DescriptY,+-X,+-Y)
 *
 *	Given a role Y this predicate returns its description instance terms
 *	of the hl_attribute X on which Y depends as a chain of influence 
 *	relationships. DescriptY is an hl_attribute or it is of the form 
 *	app(sk_,W,Z) where W denotes a weight.
 */

getInflDescription(KB,World,app(F,W,DescriptZ),X,Y) :-
	given_inflLink(KB,World,app(F,W,Z),Y),
	getInflDescription(KB,World,DescriptZ,X,Z).

getInflDescription(KB,World,X,X,X) :-
	!.

/***********************************************************************
 *
 * test_inflLink(+-X,+-Y,+-W)
 *
 *	X is either an hl_attribute/role or it is of the form
 *	app(sk_,_,_).
 *
 *	Y is of the form app(+-F,1.0,+Z) or app(+-F,-1.0,+Z).
 *
 *	This predicate tests for influence of weight W between X and Y.
 */

test_inflLink(X,Y,W) :-
	nonvar(X),
	Y = app(F,W,X),
	!,
	nonvar(F).

test_inflLink(X,Y,W) :-
	isSlot(X),
	nonvar(Y),
	Y = app(F,W,X),
	atom(X),
	!.

test_inflLink(X,Y,W) :-
	isSlot(X),
	isSlot(Y),
	!,
	fail.

test_inflLink(X,app(F,W1,Y),W) :-
	test_inflLink(X,Y,W2),
	weightOf_ChainedInfl(W1,W2,W).

/***********************************************************************
 *
 * inflLink(+KB,+World,+-X,+-Y,+-W)
 *
 *	tests if hl_attribute X influences hl_attribute Y with weight W.
 *
 *	Note: the output for uninstantiated X and/or Y depends on the
 *	order of the literals instance the body of infl.
 */

inflLink(KB,World,X,Y,W) :-
	wellDefined_hl_attribute(KB,World,X),
	wellDefined_hl_attribute(KB,World,Y),
	(isSlot(W);
	wellDefined_InflWeight(W)),
	getInflDescription(KB,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

inflLink(KB,World,X,Y,W) :-
	isSlot(X),
	wellDefined_hl_attribute(KB,World,Y),
	(isSlot(W);
	wellDefined_InflWeight(W)),
	getInflDescription(KB,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

inflLink(KB,World,X,Y,W) :-
	wellDefined_hl_attribute(KB,World,X),
	isSlot(Y),
	(isSlot(W);
	wellDefined_InflWeight(W)),
	getInflDescription(KB,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

inflLink(KB,World,X,Y,W) :-
	isSlot(X),
	isSlot(Y),
	(isSlot(W);
	wellDefined_InflWeight(W)),
	getInflDescription(KB,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

/***********************************************************************
 *
 * leastInfl(+KB,+World,+-X,+-Y)
 *
 *	succeeds if X is a least hl_attribute influencing Y.
 */

leastInfl(KB,World,X,Y) :-
	getInflDescription(KB,World,_,X,Y),
	not(X = Y),
	not(given_inflLink(KB,World,_,X)).

/***********************************************************************
 *
 * leastInfls(+KB,+World,+-Xs,+Y)
 *
 *	collects the least hl_attributes influencing Y instance Xs.
 */

leastInfls(KB,World,Xs,Y) :-
	setof(X,leastInfl(KB,World,X,Y),Xs).

/***********************************************************************
 *
 * greatestInfl(+KB,+World,+-X,+-Y)
 *
 *	succeeds if Y is a greatest hl_attribute influenced by X.
 */

greatestInfl(KB,World,X,Y) :-
	getInflDescription(KB,World,_,X,Y),
	not(X = Y),
	not(given_inflLink(KB,World,app(_,_,Y),_)).

/***********************************************************************
 *
 * greatestInfls(+KB,+World,+X,+-Ys)
 *
 *	collects the greatest hl_attributes influenced by X instance Ys.
 */

greatestInfls(KB,World,X,Ys) :-
	setof(Y,greatestInfl(KB,World,X,Y),Ys).

/***********************************************************************
 *
 * infl(+KB,+World,+-X,+-Y,+-W)
 *
 *	computes the cumulative weight W of all the influence links 
 *	between the hl_attributes X and Y.
 */

infl(KB,World,X,Y,W) :-
	bagof(Weight,inflLink(KB,World,X,Y,Weight),Weights),
	weightOf_TotalInfl(Weights,W).

/***********************************************************************
 *
 * maxPosInfl(+KB,+World,+-X,+-Y,+-Wmax)
 *
 *	succeeds if Wmax is the greatest weight with which X influences 
 *	Y positively.
 */

maxPosInfl(KB,World,X,Y,WMax) :-
	isSlot(X),
	bagof(W,Z^posInfl(KB,World,Z,Y,W),Ws),
	sigma_max(Ws,WMax,wellDefined_InflWeight),
	posInfl(KB,World,X,Y,WMax).

maxPosInfl(KB,World,X,Y,WMax) :-
	isSlot(Y),
	bagof(W,Z^posInfl(KB,World,X,Z,W),Ws),
	sigma_max(Ws,WMax,wellDefined_InflWeight),
	posInfl(KB,World,X,Y,WMax).

maxPosInfl(KB,World,X,Y,WMax) :-
	nonvar(X),
	nonvar(Y),
	posInfl(KB,World,X,Y,WMax).

/***********************************************************************
 *
 * maxNegInfl(+KB,+World,+-X,+-Y,+-WMin)
 *
 *	succeeds if WMin is the greatest weight with which X influences 
 *	Y negatively.
 */

maxNegInfl(KB,World,X,Y,WMin) :-
	isSlot(X),
	bagof(W,Z^negInfl(KB,World,Z,Y,W),Ws),
	sigma_min(Ws,WMin,wellDefined_InflWeight),
	negInfl(KB,World,X,Y,WMin).

maxNegInfl(KB,World,X,Y,WMin) :-
	isSlot(Y),
	bagof(W,Z^negInfl(KB,World,X,Z,W),Ws),
	sigma_min(Ws,WMin,wellDefined_InflWeight),
	negInfl(KB,World,X,Y,WMin).

maxNegInfl(KB,World,X,Y,WMin) :-
	nonvar(X),
	nonvar(Y),
	negInfl(KB,World,X,Y,WMin).

/***********************************************************************
 *
 * posInfl(+KB,+World,+-X,+-Y)
 *
 *	succeeds if hl_attribute X influences hl_attribute Y positively.
 */

posInfl(KB,World,X,Y) :-
	infl(KB,World,X,Y,W),
	W > 0.0.

/***********************************************************************
 *
 * posInfl(+KB,+World,+-X,+-Y,+-W)
 *
 *	succeeds if hl_attribute X influences hl_attribute Y positively with
 *	weight W.
 */

posInfl(KB,World,X,Y,W) :-
	infl(KB,World,X,Y,W),
	W > 0.0.

/***********************************************************************
 *
 * negInfl(+KB,+World,+-X,+-Y)
 *
 *	succeeds if hl_attribute X influences hl_attribute Y negatively.
 */

negInfl(KB,World,X,Y) :-
	infl(KB,World,X,Y,W),
	W < 0.0.

/***********************************************************************
 *
 * negInfl(+KB,+World,+-X,+-Y,+-W)
 *
 *	succeeds if hl_attribute X influences hl_attribute Y negatively with
 *	weight W.
 */

negInfl(KB,World,X,Y,W) :-
	infl(KB,World,X,Y,W),
	W < 0.0.

/***********************************************************************
 *
 * noInfl(+KB,+World,+-X,+-Y)
 *
 *	succeeds if the cumulative influence between the hl_attributes X and
 *	Y is 0.0.
 */

noInfl(KB,World,X,Y) :-
	infl(KB,World,X,Y,0.0).

/***********************************************************************
 *
 * simultInfl(+KB,+World,+-Xs,+-Y,+-W)
 *
 *	checks if the list Xs is well-defined (that is, is Xs a SET of
 *	independent hl_attributes) and computes the total weight W of the 
 *	hl_attributes instance the list Xs simultaneously influencing hl_attribute Y.
 */

simultInfl(KB,World,Xs,Y,W) :-
	nonvar(Xs),
	wellDefined_setOfHl_attributes(KB,World,Xs),
	aux_simultInfl(KB,World,Xs,Y,Ws),
	weightOf_SimultInfl(Ws,W).

simultInfl(KB,World,Xs,Y,W) :-
	isSlot(Xs),
	!,
	leastInfl(KB,World,Xs,Y),
	aux_simultInfl(KB,World,Xs,Y,Ws),
	wellDefined_setOfHl_attributes(KB,World,Xs),
	weightOf_SimultInfl(Ws,W).

aux_simultInfl(_,_,[],_,[]).

aux_simultInfl(KB,World,[X|Xs],Y,[W|Ws]) :-
	infl(KB,World,X,Y,W),
	aux_simultInfl(KB,World,Xs,Y,Ws).

aux_simultInfl(KB,World,[X|Xs],Y,Ws) :-
	not(getInflDescription(KB,World,_,X,Y)),
	aux_simultInfl(KB,World,Xs,Y,Ws).

/***********************************************************************
 *
 * simultPosInfl(+KB,+World,+-Xs,+-Y)
 *
 *	succeeds if the simultaneous influence of the hl_attributes instance the
 *	list Xs on the hl_attribute Y is positive.
 */

simultPosInfl(KB,World,Xs,Y) :-
	simultInfl(KB,World,Xs,Y,W),
	W > 0.0.

/***********************************************************************
 *
 * simultNegInfl(+KB,+World,+-Xs,+-Y)
 *
 *	succeeds if the simultaneous influence of the hl_attributes instance the
 *	list Xs on the hl_attribute Y is positive.
 */

simultNegInfl(KB,World,Xs,Y) :-
	simultInfl(KB,World,Xs,Y,W),
	W < 0.0.

/***********************************************************************
 *
 * simultNoInfl(+KB,+World,+-Xs,+-Y)
 *
 *	succeeds if the simultaneous influence of the hl_attributes instance the
 *	list Xs on the hl_attribute Y is positive.
 */

simultNoInfl(KB,World,Xs,Y) :-
	simultInfl(KB,World,Xs,Y,0.0).

/***********************************************************************
 *
 * change(+KB,+World,+-Y,+-Wy)
 *
 *	determines the change instance Y.
 */

change(KB,World,Y,Wy) :-
	bagof(W,X^changingInfl(KB,World,X,Y,W),Ws),
	weightOf_SimultChange(Ws,Wy).

change(KB,World,Y,Wy) :-
	given_change(KB,World,Y,Wy).

/***********************************************************************
 *
 * changingInfl(+KB,+World,+-X,+-Y,+-Wy)
 *
 *	succeeds if the influencing hl_attribute X of Y changes. Wy is the
 *	weight of the resulting change instance Y.
 */

changingInfl(KB,World,X,Y,Wy) :-
	infl(KB,World,X,Y,Wxy),
	given_change(KB,World,X,Wx),
	weightOf_change(Wx,Wxy,Wy).

/***********************************************************************
 *
 * increase(+KB,+World,+-Y)
 *
 *	succeeds if hl_attribute Y increases.
 */

increase(KB,World,Y) :-
	change(KB,World,Y,W),
	W > 0.0.
%	change(KB,World,Y,1.0).

/***********************************************************************
 *
 * decrease(+KB,+World,+-Y)
 *
 *	succeeds if hl_attribute Y decreases.
 */

decrease(KB,World,Y) :-
	change(KB,World,Y,W),
	W < 0.0.
%	change(KB,World,Y,-1.0).

/***********************************************************************
 *
 * noChange(+KB,+World,+-Y)
 *
 *	succeeds if hl_attribute Y does not change (i.e. there is neither
 *	an increase nor a decrease).
 */

noChange(KB,World,Y) :-
	change(KB,World,Y,0.0).

/***********************************************************************
 *
 * wellDefined_hl_attribute(+KBName,+World,+X)
 *
 *	Is X an hl_attribute?
 *
 *	Note: At the moment this clause succeeds if X is an atom. We
 *	may want to do more verifying here.
 */

wellDefined_hl_attribute(KB,World,X) :-
	atom(X),
	roleName(KB,_CTX,World,X),
	!.

/***********************************************************************
 *
 * wellDefined_setOfHl_attributes(+KB,+World,+Xs)
 *
 *	Succeeds if Xs is a variable or if Xs is a SET of independent
 *	(with respect to the influence relationship) atoms.
 *
 *	Note: In the current implementation the independence is NOT 
 *	verified.
 */

wellDefined_setOfHl_attributes(KB,World,Xs) :-
	isSet(Xs),
	noInflLinks(KB,World,Xs).

/***********************************************************************
 *
 * isSet(+L)
 *
 *	Succeeds if L is a SET.
 */

isSet([]) :-
	!.
isSet([E|L]) :-
	not(member(E,L)),
	isSet(L).


/***********************************************************************
 *
 * noInflLinks(+KB,+World,+Xs)
 *
 *	Succeeds if there are no links among any of the hl_attributes instance Xs.
 *
 *	NOTE: The complexity is quadratic, namely (n-1)^2, 
 *	where n is the length of Xs.
 */

noInflLinks(KB,World,[X|Xs]) :-
	wellDefined_hl_attribute(KB,World,X),
	aux_noInflLinks(KB,World,X,Xs),
	noInflLinks(KB,World,Xs).

noInflLinks(_,_,[]).

aux_noInflLinks(KB,World,X,[Y|Ys]) :-
	not(getInflDescription(KB,World,_,X,Y)),
	not(getInflDescription(KB,World,_,Y,X)),
	aux_noInflLinks(KB,World,X,Ys).

aux_noInflLinks(_,_,_,[]).

/***********************************************************************
 *
 * wellDefined_SimultChanges(+Changes)
 *
 *	Succeeds if Changes is a list of changes of the form
 *	change(X,W) instance which no X occurs more than once.
 */

wellDefined_SimultChanges(Changes) :-
	hl_attributes(Changes,Xs),
	isSet(Xs),
	!.

/***********************************************************************
 *
 * hl_attributes(+Changes,+-Xs)
 *
 *	Generates a list Xs of hl_attributes X from the list Changes.
 *	The elements of Changes are of the form change(X,W).
 */

hl_attributes([],[]).

hl_attributes([change(X,_)|Changes],[X|Xs]) :-
	hl_attributes(Changes,Xs).

/***********************************************************************
 *
 * wellDefined_InflWeight(+W)
 *
 *	Is the given weight W of influence well-defined?
 */

wellDefined_InflWeight(W) :-
	float(W),
	!.
wellDefined_InflWeight(W) :-
	integer(W),
	!.
%	W >= -1.0,
%	W =< 1.0.

/***********************************************************************
 *
 * weightOf_ChainedInfl(+-W1,+-W2,+-W)
 *
 *	defines the weight W of chained influences with weights W1 and
 *	W2.
 *	W is given by f(W1,W2) where instance this implementation f is
 *	multiplication.
 */

weightOf_ChainedInfl(W1,W2,W) :-
	product(W1,W2,W,wellDefined_ChangeWeight).

/***********************************************************************
 *
 * weightOf_TotalInfl(+Ws,+-W)
 *
 *	computes the the total weight W from the Ws. 
 *	Here, W is the sum of the Ws.
 *	We could have just as well chosen W to be the arithmetic
 *	mean of the Ws.
 *	Which is better remains open for the moment.
 */

weightOf_TotalInfl(Ws,W) :-
	sum(Ws,W,wellDefined_InflWeight).
%	arithm_Mean(Ws,W,wellDefined_InflWeight).

/***********************************************************************
 *
 * weightOf_SimultInfl(+-Ws,+-W)
 *
 *	computes the weight W of a list of simultaneous influences from
 *	different hl_attributes with weights specified instance Ws.
 *	Here, W is the sum of the Ws.
 *	We could have just as well chosen W to be the arithmetic
 *	mean of the Ws.
 *	Which is better remains open for the moment.
 */

weightOf_SimultInfl(Ws,W) :-
	sum(Ws,W,wellDefined_InflWeight).
%	arithm_Mean(Ws,W,wellDefined_InflWeight).

/***********************************************************************
 *
 * wellDefined_ChangeWeight(+W)
 *
 *	Is the given weight W of change well-defined?
 */

wellDefined_ChangeWeight(W) :-
	float(W),
	!.
wellDefined_ChangeWeight(W) :-
	integer(W),
	!.
%	W >= -1.0,
%	W =< 1.0.

/***********************************************************************
 *
 * weightOf_change(+-Wx,+-Wxy,-+Wy)
 *
 *	succeeds if Wy = f(Wx,Wxy) for a given f. 
 *	I chose f to be multiplication.
 */

weightOf_change(Wx,Wxy,Wy) :-
	product(Wx,Wxy,Wy,wellDefined_ChangeWeight).

/***********************************************************************
 *
 * weightOf_SimultChange(+Ws,+-W)
 *
 *	computes the weight W of the change resulting from
 *	simultaneous changes with weights Ws.
 *	W is the sum over the Ws.
 */

weightOf_SimultChange(Ws,W) :-
	sum(Ws,W,wellDefined_ChangeWeight).

/***********************************************************************
 *
 * arithm_Mean([+-Value|+Values],+-Mean,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name for
 *	checking whether each of the values is well-defined this clause
 *	computes the arithmetical mean (Mean) over Values.
 *	Provided Mean is given the first value may be a variable.
 */

arithm_Mean([],0.0,IsWellDefName) :-
	!.

arithm_Mean([Value|Values],Mean,IsWellDefName) :-
	isSlot(Value),
	!,
	length([Value|Values],N),
	Sum is Mean * N,
	sum([Value|Values],Sum,IsWellDefName).

arithm_Mean(Values,Mean,IsWellDefName) :-
	sum(Values,Sum,IsWellDefName),
	length(Values,N),
	Mean is Sum / N.

/***********************************************************************
 *
 * sum([+-Value|+Values],+-Sum,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause computes the sum (Sum) of the values.
 *	Provided Sum is given the first value may be a variable.
 */

sum([Value|Values],Sum,IsWellDefName) :-
	isSlot(Value),
	!,
	IsWellDef =.. [IsWellDefName,Sum],
	IsWellDef,
	sum(Values,VSum,IsWellDefName),
	Value is Sum - VSum.

sum([Value|Values],Sum,IsWellDefName) :-
	IsWellDef =.. [IsWellDefName,Value],
	IsWellDef,
	sum(Values,VSum,IsWellDefName),
	Sum is Value + VSum.

sum([],0.0,_).

/***********************************************************************
 *
 * product(+Factor1,+Factor2,+-Product,+IsWellDefName)
 * product(+-Factor1,+Factor2,+Product,+IsWellDefName)
 * product(+Factor1,+-Factor2,+Product,+IsWellDefName)
 *
 *	Given two values (Factor1 and Factor2) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause computes the product (Product) of the 
*	values.
 */

product(Factor1,Factor2,Product,IsWellDefName) :-
	IsWellDef1 =.. [IsWellDefName,Factor1],
	IsWellDef1,
	IsWellDef2 =.. [IsWellDefName,Factor2],
	IsWellDef2,
	Product is Factor1 * Factor2,
	!.

product(Factor1,Factor2,Product,IsWellDefName) :-
	IsWellDef1 =.. [IsWellDefName,Factor2],
	IsWellDef1,
	IsWellDef2 =.. [IsWellDefName,Product],
	IsWellDef2,
	Factor1 is Product / Factor2,
	!.

product(Factor1,Factor2,Product,IsWellDefName) :-
	IsWellDef1 =.. [IsWellDefName,Factor1],
	IsWellDef1,
	IsWellDef2 =.. [IsWellDefName,Product],
	IsWellDef2,
	Factor2 is Product / Factor1,
	!.

/***********************************************************************
 *
 * max([+-Value|+Values],+-Max,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause determines the maximum (Max) of the 
 *	values.
 *
 * max(+Value1,+Value2,+-Max)
 *
 *	returns the bigger value of Value1 and Value2 instance Max.
 */

sigma_max([Max],Max,_) :-
	!.

sigma_max([Value|Values],Max,IsWellDefName) :-
	IsWellDef =.. [IsWellDefName,Value],
	IsWellDef,
	sigma_max(Values,VMax,IsWellDefName),
	lub(Value,VMax,Max).

lub(Value1,Value2,Value1) :-
	Value1 > Value2,
	!.

lub(Value1,Value2,Value2).

/***********************************************************************
 *
 * min([+-Value|+Values],+-Min,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause determines the minimum (Min) of the 
 *	values.
 *
 * min(+Value1,+Value2,+-Min)
 *
 *	returns the smaller value of Value1 and Value2 instance Min.
 */

sigma_min([Min],Min,_) :-
	!.

sigma_min([Value|Values],Min,IsWellDefName) :-
	IsWellDef =.. [IsWellDefName,Value],
	IsWellDef,
	sigma_min(Values,VMin,IsWellDefName),
	glb(Value,VMin,Min).

glb(Value1,Value2,Value1) :-
	Value1 < Value2,
	!.

glb(Value1,Value2,Value2).

/**********************************************************************
 *
 * @(#) klone.pll 1.10@(#)
 *
 */

/***********************************************************************
 *
 * defprimconcept(+KnowledgeBase,+Left)
 * Parameter: ConceptName       concept name
 * defines the concept ConceptName instance modal context [].
 *
 */

defprimconcept(Left) :-
	getCurrentKnowledgeBase(KBName),
	defprimconcept(KBName,[],Left).


/***********************************************************************
 *
 * defprimconcept(+KnowledgeBase,+Left)
 * Parameter: ConceptName       concept name
 * defines the concept ConceptName instance modal context [].
 *
 */

defprimconcept(KBName,Left) :-
	knowledgeBase(KBName,_,_),
	!,
	defprimconcept(KBName,[],Left).
defprimconcept(CTX,Left) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	defprimconcept(KBName,CTX,Left).
defprimconcept(Left,Right) :-
	getCurrentKnowledgeBase(KBName),
	defprimconcept(KBName,[],Left,Right).
	

/***********************************************************************
 *
 * defprimconcept(+KnowledgeBase,+CTX,+Left)
 * Parameter: ModalSequence     modal context
 *            ConceptName       concept name
 * defines the concept ConceptName instance modal context ModalSequence.
 *
 */

defprimconcept(KBName,CTX,Left) :-
	knowledgeBase(KBName,KB,_),
	(CTX = [] ; CTX = [_|_]),
	atomic(Left),
	assertz(conceptSubsets(KB,user,CTX,Left,'Entity',noAxiom)),
	assertz(axiom(KB,CTX,defprimconcept(CTX,Left,'Entity'))),
	assertNames(KB,CTX,Left,concept),
	!.

defprimconcept(CTX,Left,Right) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	defprimconcept(KBName,CTX,Left,Right).

/***********************************************************************
 *
 * defprimconcept(+KnowledgeBase,+Left,+Right)
 * Parameter: ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be a cmp_subset of the concept
 * ConceptTerm instance modal context [].
 *
 */

defprimconcept(KBName,Left,Right) :-
	knowledgeBase(KBName,_,_),
	defprimconcept(KBName,[],Left,Right).

/***********************************************************************
 *
 * defprimconcept(+KnowledgeBase,+ModalSequence,+Left,+Right)
 * Parameter: ModalSequence     modal context
 *            ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be a cmp_subset of the concept
 * ConceptTerm instance modal context ModalSequence.
 *
 */

defprimconcept(KBName,CTX,L,R) :-
	knowledgeBase(KBName,KB,_),
%	nonvar(CTX),
	cnf(L,Left),
	cnf(R,Right),
	assertNames(KB,CTX,Left,concept),
	assertNames(KB,CTX,Right,concept),
	assertz(axiom(KB,CTX,defprimconcept(CTX,L,R))),
	unfold(KB,[(user,concept,Left,Right)],[(_Origin,_,L1,_,R1)|DL]),
	gensym(axiom,AxiomName1),
	assertz(conceptSubsets(KB,user,CTX,Left,Right,AxiomName1)),
	typeOfDefinition(KB,CTX,L1,Origin),
	assertConceptLInR(KB,rn(AxiomName1,Origin,lInR),CTX,L1,R1),
	defList(KB,CTX,DL,_),
	negate(R1,NotRight1),
	cnf(NotRight1,NotRight),
	negate(L1,NotLeft1),
	cnf(NotLeft1,NotLeft),
	notClauseL(KB,CTX,NotRight,NotLeft).


notClauseL(KB,CTX,Left,Right) :-
	% assertz that Left is included instance Right
	unfold(KB,[(user,concept,Left,Right)],[(_O,_,Concept1,C3,Concept2)|DL2]),
	defPositiveList(KB,CTX,DL2),
	gensym(axiom,AxiomName2),
	typeOfDefinition(KB,CTX,Concept1,O),
	assertz(conceptSubsets(KB,user,CTX,Concept1,C3,AxiomName2)),
	assertConceptLInR(KB,rn(AxiomName2,O,lInR),CTX,Concept1,Concept2).
/* 
notClauseL(KB,CTX,Left,Right) :-
	% assertz that Left is included instance Right
	atomic(Left),
	!,
	unfold(KB,[(user,concept,Left,Right)],[(_O,_,Concept1,C3,Concept2)|DL2]),
	defPositiveList(KB,CTX,DL2),
	gensym(axiom,AxiomName2),
	typeOfDefinition(KB,CTX,Concept1,O),
	assertConceptLInR(KB,rn(AxiomName2,O,lInR),CTX,Concept1,Concept2),
	assertz(conceptSubsets(KB,user,CTX,Concept1,C3,AxiomName2)).
notClauseL(KB,CTX,Left,Right) :-
	atomic(Right),
	!,
	gensym(concept,Concept1),
	unfold(KB,[(system,concept,Concept1,Left)],DL2),
	defPositiveList(KB,CTX,DL2),
	gensym(axiom,AxiomName2),
	assertConceptLInR(KB,rn(AxiomName2,system,lInR)CTX,Concept1,Right),
	assertz(conceptSubsets(KB,system,CTX,Concept1,Concept2,AxiomName2)).
notClauseL(KB,CTX,Left,Right) :-
	!,
	gensym(concept,Concept1),
	gensym(concept,Concept2),
	unfold(KB,[(system,concept,Concept1,Left),(system,concept,Concept2,Right)],DL2),
	defPositiveList(KB,CTX,DL2),
	gensym(axiom,AxiomName2),
	assertConceptLInR(KB,rn(AxiomName,system,lInR),CTX,Concept1,Concept2),
	assertz(conceptSubsets(KB,system,CTX,Concept1,Concept2,AxiomName2)).
*/

notClausesLR(KB,CTX,Left,Right,DL2) :-
	unfold(KB,[(system,concept,Left,Right)],DL2),
	defPositiveList(KB,CTX,DL2).

/***********************************************************************
 *
 * defconcept(+ConceptName,+ConceptTerm)
 * Parameter: ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be '<=>' to the concept
 * ConceptTerm instance modal context [].
 *
 */ 

defconcept(ConceptName,ConceptTerm) :-
	getCurrentKnowledgeBase(KBName),
	defconcept(KBName,[],ConceptName,ConceptTerm).

defconcept(CTX,CN,CT) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	defconcept(KBName,CTX,CN,CT),
	!.
defconcept(KBName,ConceptName,ConceptTerm) :-
	nonvar(KBName),
	knowledgeBase(KBName,_,_),
	!,
	defconcept(KBName,[],ConceptName,ConceptTerm).

/***********************************************************************
 *
 * defconcept(+ModalSequence,+ConceptName,+ConceptTerm)
 * Parameter: ModalSequence     modal context
 *            ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be '<=>' to the concept
 * ConceptTerm instance modal context ModalSequence.
 *
 */ 

defconcept(KBName,CTX,CT1,CT2) :-
	nonvar(KBName),
	knowledgeBase(KBName,KB,_),
	cnf(CT1,ConceptTerm1),
	cnf(CT2,ConceptTerm2),
	assertNames(KB,CTX,ConceptTerm1,concept),
	assertNames(KB,CTX,ConceptTerm2,concept),
	assertz(axiom(KB,CTX,defconcept(CTX,CT1,CT2))),
	unfold(KB,[(user,concept,ConceptTerm1,ConceptTerm2)],DL),
	defList(KB,CTX,DL,_).

defPositiveList(_,_,[]) :- !.
defPositiveList(KB,CTX,[(Origin,concept,ConceptName,CTO,ConceptTerm)|DL]) :-
	gensym(axiom,AxiomName),
	assertz(conceptEqualSets(KB,Origin,CTX,ConceptName,CTO,AxiomName)),
	assertConceptRInL(KB,rn(AxiomName,Origin,rInL),CTX,ConceptName,ConceptTerm),
	assertConceptLInR(KB,rn(AxiomName,Origin,lInR),CTX,ConceptName,ConceptTerm),
	defPositiveList(KB,CTX,DL).
defPositiveList(KB,CTX,[(_Origin,role,RN,_RTO,RT)|RDL]) :-
	gensym(axiom,AxiomName),
	assertRoleLInR(KB,CTX,RN,RT,AxiomName),
	assertRoleRInL(KB,CTX,RN,RT,AxiomName),
	defPositiveList(KB,CTX,RDL).

defList(_,_,[],[]) :- !.
defList(KB,CTX,[(Origin,concept,ConceptName,CTO,ConceptTerm)|DL],
        NeededDL3) :-
	gensym(axiom,AxiomName),
	assertz(conceptEqualSets(KB,Origin,CTX,ConceptName,CTO,AxiomName)),
	assertConceptRInL(KB,rn(AxiomName,Origin,rInL),CTX,ConceptName,ConceptTerm),
	assertConceptLInR(KB,rn(AxiomName,Origin,lInR),CTX,ConceptName,ConceptTerm),
	negate(ConceptTerm,NotRight1),
	cnf(NotRight1,NotRight),
	negate(ConceptName,NotLeft1),
	cnf(NotLeft1,NotLeft),
	notClausesLR(KB,CTX,NotRight,NotLeft,NeededDL1),
	defList(KB,CTX,DL,NeededDL2),
	append(NeededDL1,NeededDL2,NeededDL3).
defList(KB,CTX,[(Origin,role,RN,RTO,RT)|RDL],NeededDL) :-
	gensym(axiom,AxiomName),
	assertz(roleEqualSets(KB,Origin,CTX,RN,RTO,AxiomName)),
	assertRoleLInR(KB,CTX,RN,RT,AxiomName),
	assertRoleRInL(KB,CTX,RN,RT,AxiomName),
	defList(KB,CTX,RDL,NeededDL).

undefList(_,_,[]) :- !.
undefList(KBName,CTX,[(Origin,concept,ConceptName,CTO,ConceptTerm)|DL]) :-
	undefconcept(KBName,CTX,ConceptName,CTO),
	undefList(KB,CTX,DL).
undefList(KBName,CTX,[(Origin,role,RN,RTO,RT)|RDL]) :-
	undefrole(KBName,CTX,RN,RTO),
	undefList(KB,CTX,RDL).

/***********************************************************************
 *
 * assert_ind(+ModalSequence,+ABoxElement,+ConceptTerm)
 * Parameter: ModalSequence     modal context
 *            ABoxElement       name of ABox element
 *            ConceptTerm       concept term
 * adds ABoxElement to Concept instance modal context ModalSequence.
 *
 */

assert_ind(X,CT) :-
	getCurrentKnowledgeBase(KBName),
	assert_ind(KBName,[],X,CT).

assert_ind(KBName,X,CT) :-
	knowledgeBase(KBName,_,_),
	!,
	assert_ind(KBName,[],X,CT).
assert_ind(CTX,X,CT) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	assert_ind(KBName,CTX,X,CT).

assert_ind(X,Y,R) :-
	getCurrentKnowledgeBase(KBName),
	assert_ind(KBName,X,Y,R).

assert_ind(KBName,CTX,X,C) :-
	knowledgeBase(KBName,KB,_),
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	!,
	atomic(X),
	gensym(axiom,AxiomName),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,user,lInR,RN1),
	defineEquivalentConceptname(KBName,CTX,C,assert,C1),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	asserta((conceptElement(KB,CTX,W1,user,X,C1,AxiomName) :- call(G1))),
	assertz(axiom(KB,CTX,assert_ind(CTX,X,C1))),
	constructMLHead(KB,RN1,W1,C1,X,_HYPS,_D,_CALLS,abox,InHead),
	asserta((InHead :- call(G1))),
	assertNames(KB,CTX,C1,concept).

/***********************************************************************
 *
 * assert_ind(+ModalSequence,+ABoxElement1,+ABoxElement2,+Role)
 * Parameter: ModalSequence     modal context
 *            ABoxElement1      name of ABox element
 *            ABoxElement2      name of ABox element
 *            Role              role name
 * adds the pair (ABoxElement1,ABoxElement2) to Role instance modal context
 * ModalSequence.
 *
 */

assert_ind(KBName,X,Y,R) :-
	knowledgeBase(KBName,_,_),
	!,
	atomic(X),
	atomic(Y),
	!,
	assert_ind(KBName,[],X,Y,R).
assert_ind(CTX,X,Y,R) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	!,
	atomic(X),
	atomic(Y),
	!,
	assert_ind(KBName,CTX,X,Y,R).

assert_ind(KBName,CTX,X,Y,RT) :-
	knowledgeBase(KBName,KB,_),
	atomic(X),
	atomic(Y),
	defineEquivalentRolename(KBName,CTX,RT,assert,R),
	Role1 =.. [R,X,Y],
	asserta(Role1),
%	Role2 =.. [R,X,Y],
	gensymbol(skolem,[X,Y],SF),
	gensym(axiom,AX),
	gensym(rule,RN),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	constructEqHead(KB,rn(AX,RN,user,lInR),W1,Y,SF,R,X,_,_D,CALLS,abox,EqLiteral),
	asserta((EqLiteral :- (cCS(CALLS,true), call(G1)))),
	assertNames(KB,CTX,R,role),
	assertz(axiom(KB,CTX,assert_ind(CTX,X,Y,R))),
	asserta((roleElement(KB,CTX,W1,user,X,Y,R,AX) :- call(G1))).


/***********************************************************************
 *
 * defprimrole(+RN)
 * Parameter: RN        role name
 * defines the role RN instance modal context [].
 * 
 */

defprimrole(Role) :-
	currentKnowledgeBase(KB),
	!,
	assertNames(KB,[],Role,role),
	asserta(roleSubsets(KB,user,[],Role,'Entity',noAxiom)).

defprimrole(KBName,Role) :-
	knowledgeBase(KBName,KB,_),
	!,
	assertNames(KB,[],Role,role),
	asserta(axiom(KB,[],defprimrole([],Role,'Entity'))),
	asserta(roleSubsets(KB,user,[],Role,'Entity',noAxiom)).

/***********************************************************************
 *
 * defprimrole(+CTX,+RN)
 * Parameter: CTX        modal context
 *            RN        role name
 * defines the role RN instance modal context CTX.
 * 
 */

defprimrole(CTX,Role) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	!,
	currentKnowledgeBase(KB),
	assertNames(KB,CTX,Role,role),
	asserta(axiom(KB,CTX,defprimrole(CTX,Role,'Entity'))),
	asserta(roleSubsets(KB,user,CTX,Role,'Entity',noAxiom)).

defprimrole(R1,R2) :-
	getCurrentKnowledgeBase(KBName),
	defprimrole(KBName,[],R1,R2).

/***********************************************************************
 *
 * defprimrole(+RN,+Role)
 * Parameter: RN        role name
 *            Role      role term
 * defines the role RN to be a cmp_subset of the role Role instance modal
 * context [].
 * 
 */

defprimrole(KBName,RN,Role) :-
	knowledgeBase(KBName,_,_),
	atomic(RN),
	!,
	defprimrole(KBName,[],RN,Role).
defprimrole(CTX,RN,Role) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	atomic(RN),
	!,
	defprimrole(KBName,CTX,RN,Role).

/***********************************************************************
 *
 * defprimrole(+CTX,+RN,+Role)
 * Parameter: CTX        modal context
 *            RN        role name
 *            Role      role term
 * defines the role RN to be a cmp_subset of the role Role instance modal
 * context CTX.
 *
 */

defprimrole(KBName,CTX,RN,Role) :-
	knowledgeBase(KBName,KB,_),
	atomic(RN),
	assertNames(KB,CTX,RN,role),
	assertNames(KB,CTX,Role,role),
	unfold(KB,[(user,role,RN,Role)],[(user,role,RN,_,RT)|RDL]),
	gensym(axiom,AxiomName),
	asserta(axiom(KB,CTX,defprimrole(CTX,RN,Role))),
	asserta(roleSubsets(KB,user,CTX,RN,Role,AxiomName)),
	assertRoleLInR(KB,CTX,RN,RT,AxiomName),
	defList(KB,CTX,RDL,_).

/***********************************************************************
 *
 * defrole(+RN,+Role)
 * Parameter: RN        role name
 *            Role      role term
 * defines role RN to be '<=>' to the role Role instance modal context
 * [].
 *
 */

defrole(RN,Role) :-
	getCurrentKnowledgeBase(KBName),
	defrole(KBName,[],RN,Role).

defrole(CTX,RN,Role) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	defrole(KBName,[],RN,Role).
defrole(KBName,RN,Role) :-
	nonvar(KBName),
	knowledgeBase(KBName,_,_),
	defrole(KBName,[],RN,Role).

/***********************************************************************
 *
 * defrole(+CTX,+RN,+Role)
 * Parameter: CTX        modal context
 *            RN        role name
 *            Role      role term
 * defines the role RN to be '<=>' to the role Role instance modal
 * context CTX.
 *
 */

defrole(KBName,CTX,RN,Role) :-
	knowledgeBase(KBName,KB,_),
	atomic(RN),
	unfold(KB,[(user,role,RN,Role)],RDL),
	assertNames(KB,CTX,RN,role),
	assertNames(KB,CTX,Role,role),
	asserta(axiom(KB,CTX,defrole(CTX,RN,Role))),
	defList(KB,CTX,RDL,_).

/**********************************************************************
 *
 * defdisjoint(KBName,CTX,ConceptList)
 * Parameter: KBName         knowledgeBase atom_codes
 *            CTX              modal context
 *            ConceptList     list of concept names
 *
 */

defdisjoint(CL) :-
	getCurrentKnowledgeBase(KBName),
	defdisjoint(KBName,[],CL),
	!.

defdisjoint(KBName,CL) :-
	knowledgeBase(KBName,_,_),
	defdisjoint(KBName,[],CL),
	!.
defdisjoint(CTX,CL) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	defdisjoint(KBName,CTX,CL),
	!.

defdisjoint(_KBName,_CTX,[]) :-
	!.
defdisjoint(KBName,CTX,[C1|CL]) :-
	defdisjoint(KBName,CTX,C1,CL),
	defdisjoint(KBName,CTX,CL),
	!.

defdisjoint(_KBName,_CTX,_C1,[]) :-
	!.
defdisjoint(KBName,CTX,C1,[C2|CL]) :-
	defprimconcept(KBName,CTX,C1,not(C2)),
	defdisjoint(KBName,CTX,C1,CL).


/**********************************************************************
 *
 * defhl_closed(KBName,CTX,X,Y,R)
 *
 */

defhl_closed(X,Y,R) :-
	getCurrentKnowledgeBase(KBName),
	defhl_closed(KBName,[],X,Y,R),
	!.

defhl_closed(KBName,X,Y,R) :-
	knowledgeBase(KBName,_,_),
	defhl_closed(KBName,[],X,Y,R),
	!.
defhl_closed(CTX,X,Y,R) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	defhl_closed(KBName,CTX,X,Y,R),
	!.

defhl_closed(KBName,CTX,X,Y,R) :-
	knowledgeBase(KBName,KB,_),
	assertz(hl_closed(KB,CTX,X,Y,R)),
	!.

/***********************************************************************
 *
 * assertNames(+Type,+CTX,+CT)
 * Arguments: Type   'concept' or 'role'
 *            CTX     modal context
 *            T      concept or role term
 * asserts for each concept name CN instance T a fact
 *            conceptName(CN)
 * and for each role name RN instance T a fact
 *            roleName(RN)
 * These facts are used to distinguish concept and role names introduced 
 * by the user from those introduced by the system.
 *
 */

assertNames(KB,CTX,CT,Type) :-
	namesInTerm(CT,CNL1,Type),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	hop_map(assertName,[KB,CTX,W1,G1],CNL1,_).

assertName((concept,CN1),alreadyAsserted,KB,CTX,W1,G1) :-
% If the concept name is already asserted with identical modal sequence, 
% then we do nothing.
	clause(conceptName(KB,CTX,_,CN1),_),
	!.
assertName((role,CN1),alreadyAsserted,KB,CTX,W1,G1) :-
% If the role name is already asserted with identical modal sequence, 
% then we do nothing.
	clause(roleName(KB,CTX,_,CN1),_),
	!.
assertName((concept,CN1),newAsserted,KB,CTX,W1,G1) :-
% Otherwise we assert the concept name
% Remember: The fact that the concept name is not already asserted with
% identical modal sequence does not mean that we are not already able to 
% deduce that the concept name is present instance the modal context corresponding
% to the modal sequence.
	assertz((conceptName(KB,CTX,W1,CN1) :- G1)),
	!.
assertName((role,CN1),newAsserted,KB,CTX,W1,G1) :-
% Otherwise we assert the role name
% Remember: The fact that the role name is not already asserted with
% identical modal sequence does not mean that we are not already able to 
% deduce that the role name is present instance the modal context corresponding
% to the modal sequence.
	assertz((roleName(KB,CTX,W1,CN1) :- G1)),
	!.
	
/***********************************************************************
 *
 * namesInTerm(+T1,-TL+Type) 
 * Arguments: T1     concept or role term
 *            TL     list of pairs (Type1,Name)
 *            Type   'concept' or 'role'
 * TL is the list of all concept and role names instance T1.
 *
 */

namesInTerm(and(CTL),CNL,Type) :-
	hop_map(namesInTerm,[Type],CTL,CNLL),
	cmp_union(CNLL,CNL),
	!.
namesInTerm(or(CTL),CNL,Type) :-
	hop_map(namesInTerm,[Type],CTL,CNLL),
	cmp_union(CNLL,CNL),
	!.
namesInTerm(some(R,C),L,_) :-
	namesInTerm(R,L1,role),
	namesInTerm(C,L2,concept),
	append(L1,L2,L).
namesInTerm(all(R,C),L,_) :-
	namesInTerm(C,L1,concept),
	namesInTerm(R,L2,role),
	append(L1,L2,L).
namesInTerm(restr(R,C),L,_) :-
	namesInTerm(C,L1,concept),
	namesInTerm(R,L2,role),
	append(L1,L2,L).
namesInTerm(atleast(_N,R),L,_) :-
	namesInTerm(R,L,role).
namesInTerm(atmost(_N,R),L,_) :-
	namesInTerm(R,L,role).
namesInTerm(inverse(R),L,Type) :-
	namesInTerm(R,L,Type).
namesInTerm(not(C),L,Type) :-
	namesInTerm(C,L,Type).
namesInTerm(naf(C),L,Type) :-
	setOption(queryCaching,off),
	namesInTerm(C,L,Type).
namesInTerm(b(_O,_P,C),L,Type) :-
	namesInTerm(C,L,Type).
namesInTerm(d(_O,_P,C),L,Type) :-
	namesInTerm(C,L,Type).
namesInTerm(bc(_O,D,C),L,Type) :-
	namesInTerm(D,L1,Type),
	namesInTerm(C,L2,Type),
	append(L1,L2,L).
namesInTerm(dc(_O,D,C),L,Type) :-
	namesInTerm(D,L1,Type),
	namesInTerm(C,L2,Type),
	append(L1,L2,L).
namesInTerm(set(_L),[],_Type) :-
	!.
namesInTerm(L,[(Type,L)],Type) :-
	atomic(L),
	!.



/**********************************************************************
 *
 * @(#) kloneToFOL.pll 1.12@(#)
 *
 */
%------------------------------------------------------------------------------

% Module:       malcToFOL.pll
% Purpose:      Translation of modal KL-ONE terms to first-order logic 
%               formulae to clauses
%------------------------------------------------------------------------------

axiomToClause(MC,VL,T1,Op,T2,C) :-
	axiomToFOL(MC,VL,T1,Op,T2,F),
	forallQuantify(VL,F,F1),
	translate(F1,C1),
	clausesToSystemSyntax(C1,C).

forallQuantify([],F,F) :-
	!.
forallQuantify([X|VL],F1,forall(X,F2)) :-
	forallQuantify(VL,F1,F2).

%----------------------------------------------------------------------
% axiomToFOL(+MODALCONTEXT,+VARLIST,+TERM1,+OPERATOR,+TERM2,-FORMULA)
% If OPERATOR is ''<=>'', then FORMULA is the translation of the 
% equivalence of TERM1 and TERM2 instance MODALCONTEXT.
% If OPERATOR is ''=>'', then FORMULA is the translation of the
% implication of TERM2 by TERM1 instance MODALCONTEXT.

axiomToFOL(MC,VL,_,instance,C,F) :-
	!,
	option(translationMode,TM),
	malcToFOL(TM,U,VL,C,F1),
	modalContextToFOL(MC,[],U,F1,F).
axiomToFOL(MC,VL,C1,Op,C2,F) :-
	option(translationMode,TM),
	malcToFOL(TM,U,VL,C1,F1),
	malcToFOL(TM,U,VL,C2,F2),
	F3 =.. [Op,F1,F2],
	modalContextToFOL(MC,[],U,F3,F).

modalContextToFOL([],V,V,F,F) :-
	!.
modalContextToFOL([b(O,A)|MC],U1,V,F3,
	          forall(U2,'=>'(hl_holds(X1,X2,U1,U2),F4))) :-
	convertCTX(e1,[U1,true],[b(O,A)],[],[U2,(hl_holds(e1,X1,X2,U1,U2), true)],_),
	modalContextToFOL(MC,U2,V,F3,F4).
modalContextToFOL([d(O,A)|MC],U1,V,F3,F4) :-
	convertCTX(e1,[U1,true],[d(O,A)],[],
                  [app(W1 : m(O,A), U1),true],_),
	modalContextToFOL(MC,app(typed(W1,m(O,A)),U1),V,F3,F4).
modalContextToFOL([bc(O,C)|MC],U1,V,F3,
	          forall(A,forall(U2,'=>'(and([F1,hl_holds(X1,m(O,A),U1,U2)]),F4)))) :-
	convertCTX(e1,[U1,true],[bc(O,C)],[],
	          [U2,((once(_G),hl_holds(e1,X1,m(O,A),U1,U2)),true)],_),
	option(translationMode,TM),
	malcToFOL(TM,U1,[A],C,F1),
	modalContextToFOL(MC,U2,V,F3,F4).
modalContextToFOL([dc(O,C)|MC],U1,V,F3,
	          forall(V,and([F1,F4]))) :-
	convertCTX(e1,[U1,true],[dc(O,C)],[],
	          [app(W1 : m(O,A), U1),_G],_),
	option(translationMode,TM),
	malcToFOL(TM,U1,[A],C,F1),
	modalContextToFOL(MC,[app(typed(W1,m(O,A)), U1)],V,F3,F4).


    
%----------------------------------------------------------------------
% malcToFOL(functional,+WORLD,+VARLIST,+TERM,-FORMULA)
% translates TERM instance WORLD for VARLIST into the first-order logic 
% formula FORMULA.
%
% 

malcToFOL(Trans,U,[X],and([C1]),F1) :-
	malcToFOL(Trans,U,[X],C1,F1).
malcToFOL(Trans,U,[X],and([C1|CL]),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X],C1,F1),
	malcToFOL(Trans,U,[X],and(CL),F2).
malcToFOL(Trans,U,[X],or([C1]),F1) :-
	malcToFOL(Trans,U,[X],C1,F1).
malcToFOL(Trans,U,[X],or([C1|CL]),
	   or([F1,F2])) :-
	malcToFOL(Trans,U,[X],C1,F1),
	malcToFOL(Trans,U,[X],or(CL),F2).
malcToFOL(Trans,U,[X],set(L),F) :-
	!,
	setDisjunction(X,L,F).
malcToFOL(Trans,U,[X],not(C),not(F)) :-
	malcToFOL(Trans,U,[X],C,F),
	!.
malcToFOL(Trans,U,[X],naf(C),naf(F)) :-
	malcToFOL(Trans,U,[X],C,F),
	!.
malcToFOL(Trans,U,[X],all(R,C),
	   forall(Y,'=>'(F1,F2))) :-
	malcToFOL(Trans,U,[X,Y],R,F1),
	malcToFOL(Trans,U,[Y],C,F2).
malcToFOL(relational,U,[X],some(R,C),
	   exists(Y,and([F1,F2]))) :-
	malcToFOL(relational,U,[X,Y],R,F1),
	malcToFOL(relational,U,[Y],C,F2).
malcToFOL(flatRelational,U,[X],some(R,C),
	   exists(Y,and([F1,F2]))) :-
	malcToFOL(flatRelational,U,[X,Y],R,F1),
	malcToFOL(flatRelational,U,[Y],C,F2).
malcToFOL(functional,U,[X],some(R,C),
	   exists(F,F2)) :-
	malcToFOL(functional,U,[app(fun(F,R),X)],C,F2).
malcToFOL(Trans,U,[X],atleast(N,R),F) :-
	nrToFOL(Trans,U,[X],atleast(N,R),F).
malcToFOL(Trans,U,[X],atmost(N,R),F) :-
	nrToFOL(Trans,U,[X],atmost(N,R),F).
malcToFOL(Trans,U,[X],b(O,A,C2),
           forall(V,'=>'(hl_holds(O,A,U,V),F))) :-
	malcToFOL(Trans,V,[X],C2,F).    
malcToFOL(relational,U,[X],d(O,A,C2),
	   exists(V,and([hl_holds(O,A,U,V),F]))) :-
	malcToFOL(relational,V,[X],C2,F).
malcToFOL(flatRelational,U,[X],d(O,A,C2),
	   exists(V,and([hl_holds(O,A,U,V),F]))) :-
	malcToFOL(relational,V,[X],C2,F).
malcToFOL(functional,U,[X],d(O,A,C2),
	   exists(W1,F)) :-
	convertCTX(e1,[U,true],[d(O,A)],[],[app(W1 : m(O,A), U), true],_),
	malcToFOL(functional,app(typed(W1,m(O,A)),U),[X],C2,F).
malcToFOL(Trans,U,[X],bc(O,C1,C2),
	   forall(A,forall(V,'=>'(and([F1,hl_holds(O,A,U,V)]),F2)))) :-
        malcToFOL(Trans,U,[A],C1,F1),
	malcToFOL(Trans,V,[X],C2,F2).
malcToFOL(relational,U,[X],dc(O,C1,C2),
	   forall(A,exists(V,and([and([F1,hl_holds(O,A,U,V)]),F2])))) :-
	malcToFOL(relational,U,[A],C1,F1),
	malcToFOL(relational,V,[X],C2,F2).
malcToFOL(flatRelational,U,[X],dc(O,C1,C2),
	   forall(A,exists(V,and([and([F1,hl_holds(O,A,U,V)]),F2])))) :-
	malcToFOL(flatRelational,U,[A],C1,F1),
	malcToFOL(flatRelational,V,[X],C2,F2).
malcToFOL(functional,U,[X],dc(O,C1,C2),
	   forall(A,exists(W1,and([F1,F2])))) :-
	convertCTX(e1,[U,true],[d(O,A)],[],[app(W1 : m(O,A), U), true],_),
	malcToFOL(functional,U,[A],C1,F1),
	malcToFOL(functional,app(typed(W1,m(O,A)),U),[X],C2,F2).
malcToFOL(Trans,U,[X],A,F) :-
	atomic(A),
	F =.. [instance,U,A,X].
malcToFOL(Trans,U,[X,Y],inverse(R),F) :-
	malcToFOL(Trans,U,[Y,X],R,F).
malcToFOL(Trans,U,[X,Y],and([R1]),F) :-
	!,
	malcToFOL(Trans,U,[X,Y],R1,F).
malcToFOL(Trans,U,[X,Y],and([R1|RL]),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X,Y],R1,F1),
	malcToFOL(Trans,U,[X,Y],and(RL),F2).
malcToFOL(Trans,U,[X,Y],or([R1]),F) :-
	!,
	malcToFOL(Trans,U,[X,Y],R1,F).
malcToFOL(Trans,U,[X,Y],or([R1|RL]),
	   or([F1,F2])) :-
	malcToFOL(Trans,U,[X,Y],R1,F1),
	malcToFOL(Trans,U,[X,Y],or(RL),F2).
malcToFOL(Trans,U,[X,Y],restrict(R,C),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X,Y],R,F1),
	malcToFOL(Trans,U,[Y],C,F2).
malcToFOL(Trans,U,[X,Y],restr(R,C),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X,Y],R,F1),
	malcToFOL(Trans,U,[Y],C,F2).
malcToFOL(relational,U,[X,Y],P,F) :-
	atomic(P),
	F =.. [instance,U,P,pair(X,Y)].
malcToFOL(flatRelational,U,[X,Y],P,F) :-
	atomic(P),
	F =.. [instance,U,P,X,Y].
malcToFOL(functional,U,[X,Y],P,equal(Y,app(fun(F,P),X))) :-
	atomic(P),
	atomic(X),
	gensym(f,F),
	!.
malcToFOL(functional,U,[X,Y],P,equal(Y,app(fun(F,P),X))) :-
	atomic(P),
	isSlot(X),
	!.
	
	

%----------------------------------------------------------------------
% nrToFOL([+VAR],+NUMBERRESTRICTION,-FORMULA)
% translates NUMBERRESTRICTION into FORMULA for variable VAR.
%
% 

nrToFOL(Trans,U,[X],atmost(0,R),forall(Y,not(F1))) :-
	!,
	malcToFOL(Trans,U,[X,Y],R,F1).
nrToFOL(Trans,U,[X],atmost(M,R),F) :-
	% M >= 1
	N is M+1,
	nVars(N,VarList),
	relConjunction(U,X,VarList,R,F1),
	eqDisjunction(VarList,F2),
	quantify(forall,VarList,'=>'(F1,F2),F).
nrToFOL(Trans,_U,[_X],atleast(0,_R),true) :-
	!.
nrToFOL(Trans,U,[X],atleast(1,R),exists(Y,F1)) :-
	!,
	malcToFOL(Trans,U,[X,Y],R,F1).
nrToFOL(Trans,U,[X],atleast(N,R),F) :-
	nVars(N,VarList),
	relConjunction(U,X,VarList,R,F1),
	neqConjunction(VarList,F2),
	quantify(exists,VarList,and([F1,F2]),F).

%----------------------------------------------------------------------
% quantify(+QUANTIFIER,+VARLIST,+MATRIX,-FORMULA)
% FORMULA is a formula with a quantifier prefix consisting only of
% quantifiers equal to QUANTIFIER and containing all variables instance 
% VARLIST and the matrix of FORMULA is MATRIX.
%
% 

quantify(forall,[Y1],F,forall(Y1,F)) :-
	!.
quantify(forall,[Y1|YL],F,forall(Y1,F3)) :-
	quantify(forall,YL,F,F3).
quantify(exists,[Y1],F,exists(Y1,F)) :-
	!.
quantify(exists,[Y1|YL],F,exists(Y1,F3)) :-
	quantify(exists,YL,F,F3).

%----------------------------------------------------------------------
% setDisjunction(+VAR,+CONSTLIST,-FORMULA)
% FORMULA is a disjunction containing equalities for any pair (VAR,CONST) 
% where CONST is an element of CONSTLIST. CONSTLIST must include atleast
% one element.
%
% 

setDisjunction(X,[],false) :-
	!.
setDisjunction(X,[A],eq(X,A)) :-
	!.
setDisjunction(X,[A|L],or([eq(X,A),F])) :-
	setDisjunction(X,L,F).

%----------------------------------------------------------------------
% eqDisjunction(+VARLIST,-FORMULA)
% FORMULA is a disjunction containing equalities for any pair
% of variables that can be build using variables instance VARLIST. VARLIST
% must include at least 2 variables.
%
% 

eqDisjunction([Y1,Y2],F1) :-
	!,
	eqDisjunction(Y1,[Y2],F1),
	!.
eqDisjunction([Y1|YL],or([F1,F2])) :-
	eqDisjunction(Y1,YL,F1),
	eqDisjunction(YL,F2).

eqDisjunction(Y1,[Y2],equal(Y1,Y2)) :-
	!.
eqDisjunction(Y1,[Y2|YL],or([equal(Y1,Y2),F2])) :-
	eqDisjunction(Y1,YL,F2).

%----------------------------------------------------------------------
% neqConjunction(+VARLIST,-FORMULA)
% FORMULA is a conjunction containing inequalities for any pair
% of variables that can be build using variables instance VARLIST. VARLIST
% must include at least 2 variables.
%
% 

neqConjunction([Y1,Y2],F1) :-
	!,
	neqConjunction(Y1,[Y2],F1),
	!.
neqConjunction([Y1|YL],and([F1,F2])) :-
	neqConjunction(Y1,YL,F1),
	neqConjunction(YL,F2).

neqConjunction(Y1,[Y2],not(equal(Y1,Y2))) :-
	!.
neqConjunction(Y1,[Y2|YL],and([not(equal(Y1,Y2)),F2])) :-
	neqConjunction(Y1,YL,F2).

%----------------------------------------------------------------------
% relConjunction(+VAR,+VARLIST,+ROLETERM,-FORMULA)
% FORMULA is a conjunction containing the translation of ROLETERM for
% any pair that can be build taking VAR and an element of VARLIST.
%
% 

relConjunction(_U,_X,[],_,true) :-
	!.
relConjunction(U,X,[Y1],R,F) :-
	!,
	option(translationMode,TM),
	malcToFOL(TM,U,[X,Y1],R,F).
relConjunction(U,X,[Y1|YL],R,and([F1,F2])) :-
	option(translationMode,TM),
	malcToFOL(TM,U,[X,Y1],R,F1),
	relConjunction(U,X,YL,R,F2).

%----------------------------------------------------------------------
% nVars(+N,-VARLIST)
% VARLIST is a list of N fresh Prolog variables.
%
% 

nVars(0,[]) :-
	!.
nVars(N,[_Y1|VL]) :-
	N >= 1,
	!,
	M is N-1,
	nVars(M,VL).
nVars(_,[]).

%----------------------------------------------------------------------


printSystemSyntax(CL) :-
	option(prover,P),
	printSystemSyntax(P,CL).

printSystemSyntax(S,[(false <- T1)|CL]) :-
	!,
	cmp_write('<- '),
	printSystemSyntax(S,T1),
	cmp_write('.'),
	cmp_nl,
	printSystemSyntax(CL).
printSystemSyntax(S,[(H1 <- true)|CL]) :-
	!,
	printSystemSyntax(S,H1),
	cmp_write(' '),
	cmp_write('<- '),
	cmp_write('.'),
	cmp_nl,
	printSystemSyntax(CL).
printSystemSyntax(scan,[C1|CL]) :-
	!,
	printSystemSyntax(scanClause,C1),
	printSystemSyntax(scan,CL).
printSystemSyntax(scanClause,[-(C1),C2|CL]) :-
	!,
	cmp_write('-'),
	printSystemSyntax(scanAtom,C1),
	cmp_write(' | '),
	printSystemSyntax(scanClause,[C2|CL]).
printSystemSyntax(scanClause,[C1,C2|CL]) :-
	!,
	printSystemSyntax(scanAtom,C1),
	cmp_write(' | '),
	printSystemSyntax(scanClause,[C2|CL]).
printSystemSyntax(scanClause,[-(C1)]) :-
	!,
	cmp_write('-'),
	printSystemSyntax(scanAtom,C1),
	cmp_write('.'),
	cmp_nl.
printSystemSyntax(scanClause,[C1]) :-
	!,
	printSystemSyntax(scanAtom,C1),
	cmp_write('.'),
	cmp_nl.
printSystemSyntax(scanAtom,app(P,L)) :-
	!,
	cmp_print(P),
	cmp_write('('),
	cmp_print(L),
	cmp_write(')').
printSystemSyntax(scanAtom,C1) :-
	!,
	cmp_print(C1).
printSystemSyntax(otter,[C1|CL]) :-
	!,
	printSystemSyntax(otterClause,C1),
	printSystemSyntax(otter,CL).
printSystemSyntax(otterClause,[-(C1),C2|CL]) :-
	!,
	cmp_write('-'),
	cmp_print(C1),
	cmp_write(' | '),
	printSystemSyntax(otterClause,[C2|CL]).
printSystemSyntax(otterClause,[C1,C2|CL]) :-
	!,
	cmp_print(C1),
	cmp_write(' | '),
	printSystemSyntax(otterClause,[C2|CL]).
printSystemSyntax(otterClause,[-(C1)]) :-
	!,
	cmp_write('-'),
	cmp_print(C1),
	cmp_write('.'), 
	cmp_nl,
	printSystemSyntax(otter,CL).
printSystemSyntax(S,[C1|CL]) :-
	!,
	cmp_print(C1),
	cmp_write('.'),
	cmp_nl,
	printSystemSyntax(S,CL).
printSystemSyntax(otterClause,[]) :-
	cmp_write('.'),
	cmp_nl,
	!.
printSystemSyntax(_,[]) :-
	!.
printSystemSyntax(_,CL) :-
	cmp_print(CL).


%----------------------------------------------------------------------
% clausesToNHProlog(+CLAUSES,-NHCLAUSES)
% translates CLAUSES which are given instance abstract syntax to NHCLAUSES
% which are instance near-Horn Prolog syntax.
%
% 


literalsToNHProlog([H1,H2|HL],(H1,HL2)) :-
	literalsToNHProlog([H2|HL],HL2).
literalsToNHProlog([H1],H1) :-
	!.
literalsToNHProlog([],true) :-
	!.

implicationToNHProlog(HL1,[],HL1) :-
	!.
implicationToNHProlog(HL1,TL1,(HL1 :- TL1)) :-
	!.

%----------------------------------------------------------------------
% clausesToLOP(+CLAUSES,-NHCLAUSES)
% translates CLAUSES which are given instance abstract syntax to NHCLAUSES
% which are instance LOP syntax.
%
% 


clausesToSystemSyntax(CL1,CL2) :-
	option(prover,setheo),
	clausesToSystemSyntax('LOP',CL1,CL2).
clausesToSystemSyntax(CL1,CL2) :-
	option(prover,otter),
	clausesToSystemSyntax(otter,CL1,CL2).
clausesToSystemSyntax(CL1,CL1) :-
	option(prover,decider),
	!.

clausesToSystemSyntax('NHProlog',[cl(HL,TL)|CL],[C2|CL2]) :-
	literalsToNHProlog(HL,HL1),
	literalsToNHProlog(TL,TL1),
	implicationToNHProlog(HL1,TL1,C2),
	clausesToSystemSyntax('NHProlog',CL,CL2).
clausesToSystemSyntax('LOP',[cl(HL0,TL0)|CL],[C2|CL2]) :-
	clauseToSequent(cl(HL0,TL0),HL,TL),
	literalsToLOP(succedent,HL,HL1),
	literalsToLOP(antecedent,TL,TL1),
	implicationToLOP(HL1,TL1,C2),
	clausesToSystemSyntax('LOP',CL,CL2).
clausesToSystemSyntax(otter,[cl(HL0,TL0)|CL],[C2|CL2]) :-
	map(negateLiterals,TL0,TL1),
	append(HL0,TL1,L2),
	literalsToOtter(L2,C2),
	clausesToSystemSyntax(otter,CL,CL2).
clausesToSystemSyntax(_,[],[]).


literalsToOtter([~H1,H2|HL],[- H1| HL2]) :-
	literalsToOtter([H2|HL],HL2).
literalsToOtter([H1,H2|HL],[H1| HL2]) :-
	literalsToOtter([H2|HL],HL2).
literalsToOtter([~H1],[- H1]) :-
	!.
literalsToOtter([H1],[H1]) :-
	!.
literalsToOtter([],true) :-
	!.

clauseToSequent(cl([],TL),HL1,[]) :-
	!,
	map(negateLiterals,TL,HL1).
clauseToSequent(cl(HL,TL),HL,TL) :-
	!.

negateLiterals(~L,L) :-
	!.
negateLiterals(L,~L) :-
	!.

literalsToLOP(antecedent,[H1,H2|HL],(H1,HL2)) :-
	literalsToLOP(antecedent,[H2|HL],HL2).
literalsToLOP(succedent,[H1,H2|HL],(H1;HL2)) :-
	literalsToLOP(succedent,[H2|HL],HL2).
literalsToLOP(_,[H1],H1) :-
	!.
literalsToLOP(_,[],true) :-
	!.

implicationToLOP(HL1,true,(HL1 <- true)) :-
	!.
implicationToLOP(HL1,TL1,(HL1 <- TL1)) :-
	!.


%----------------------------------------------------------------------
% envToFOL(+CLAUSES,-NHCLAUSES)
% translates CLAUSES which are given instance abstract syntax to NHCLAUSES
% which are instance LOP syntax.
%
% 

envToFOL(Name,CL) :-
	option(translationMode,flatRelational),
	!,
	translateModalAxioms(Name,CL1),
	translateAxioms(Name,CL2),
	append(CL1,CL2,CL),
	!.
envToFOL(Name,CL) :-
	theory(Name,CLI0),
	map(translateFOLtoClause,CLI0,CLI1),
	clausesToSystemSyntax(CLI1,CL0),
	translateModalAxioms(Name,CL1),
	translateAxioms(Name,CL2),
	append(CL0,CL1,CL01),
	append(CL01,CL2,CL),
	!.

translateModalAxiom([],[]) :-
	!.
translateModalAxiom([[CTX,KClass,MOp,concept(C)]|L1],CL) :-
	!,
	malcToFOL(functional,U,[A],C,F1),
	translateClass(KClass,MOp,C,[A],F2),
	modalContextToFOL(CTX,[],U,forall(A,'=>'(F1,F2)),F3),
	translate(F3,C1),
	clausesToSystemSyntax(C1,CL1),
	translateModalAxiom(L1,CL2),
	append(CL1,CL2,CL).
translateModalAxiom([[CTX,KClass,MOp,all]|L1],CL) :-
	!,
	translateClass(KClass,MOp,all,[A],F2),
	modalContextToFOL(MC,[],U,forall(A,F2),F3),
	translate(F3,C1),
	clausesToSystemSyntax(C1,CL1),
	translateModalAxiom(L1,CL2),
	append(CL1,CL2,CL).
translateModalAxiom([[CTX,KClass,MOp,A]|L1],CL) :-
	!,
	translateClass(KClass,MOp,C,[A],F2),
	modalContextToFOL(MC,[],U,forall(A,F2),F3),
	translate(F3,C1),
	clausesToSystemSyntax(C1,CL1),
	translateModalAxiom(L1,CL2),
	append(CL1,CL2,CL).

translateClass(kd45,MOp,C,[A],
	hl_holds(C,m(MOp,A),B,app(typed(F,m(MOp,A)),D))) :-
	!.
translateClass(k,   MOp,C,[A],
        hl_holds(C,m(MOp,A),B,app(typed(F,m(MOp,A)),B))) :-
	!.
translateClass(kd5, MOp,C,[A],
        and([hl_holds(C,m(MOp,A),app(typed(F1,m(MOp,A)),U),app(typed(F2,m(MOp,A)),V)),
             hl_holds(C,m(MOp,A),U,app(typed(F2,m(MOp,A)),U))])) :-
	!.


translateModalAxioms(Name,CL) :-
	setofOrNil([CTX,KClass,MOp,Concept],
                   [X1,B1]^clause(modalAxioms(Name,CTX,user,KClass,Concept,MOp,X1),B1),
		   L1),
	translateModalAxiom(L1,CL).

translateAxiom([],[]) :-
	!.
translateAxiom([[A1,A2,A3,A4,A5]|L1],CL) :-
	axiomToClause(A1,A2,A3,A4,A5,CL1),
	translateAxiom(L1,CL2),
	append(CL1,CL2,CL).

translateFOLtoClause(F1,CL) :-
	translate(F1,[CL]).

translateAxioms(Name,CL) :-
	setofOrNil([CTX,[A],_,instance,C],
	           [X1,Ax,B1]^clause(conceptElement(Name,CTX,X1,user,A,C,Ax),B1),
		   L1),
	translateAxiom(L1,CL1),
	setofOrNil([CTX,[A,B],_,instance,R],
	           [X1,Ax,B1]^clause(roleElement(Name,CTX,X1,user,A,B,R,Ax),B1),
		   L2),
	translateAxiom(L2,CL2),
	setofOrNil([CTX,[X],CT1,'<=>',CT2],
	           [Ax,B1]^clause(conceptEqualSets(Name,user,CTX,CT1,CT2,Ax),B1),
		   L3),
	translateAxiom(L3,CL3),
	setofOrNil([CTX,[X],CT1,'=>',CT2],
	           [Ax,B1]^clause(conceptSubsets(Name,user,CTX,CT1,CT2,Ax),B1),
		   L4),
	translateAxiom(L4,CL4),
	setofOrNil([CTX,[X,Y],RN,'<=>',RT],
	           [Ax,B1]^clause(roleEqualSets(Name,user,CTX,RN,RT,Ax),B1),
		   L5),
	translateAxiom(L5,CL5),
	setofOrNil([CTX,[X,Y],RN,'=>',RT],
	           [Ax,B1]^clause(roleSubSets(Name,user,CTX,RN,RT,Ax),B1),
		   L6),
	translateAxiom(L6,CL6),
%	hl_closed(Name,CTX,X,Y,R),
	append(CL1,CL2,CL12),
	append(CL12,CL3,CL13),
	append(CL13,CL4,CL14),
	append(CL14,CL5,CL15),
	append(CL15,CL6,CL),
	!.


/**********************************************************************
 *
 * @(#) modal.pll 1.7@(#)
 *
 */

modalAxioms(KName,MOp,A) :-
	getCurrentKnowledgeBase(KBName),
	modalAxioms(KBName,[],KName,MOp,A).

/**********************************************************************
 *
 * genclass(+Agent,-Class) 
 * classifies Agent to distinguish axioms for a modal operator behaving
 * equally for all agents from axioms for a mutual modal operator.
 *
 */

genclass(_,_,A,A,every,true) :-
	isSlot(A),
	!.
genclass(_,_,all,_,all,true) :-
	!.
genclass(KB,[W1,G1],concept(C),Agent,C,(Body,G1)) :-
	!,
	getQuery(KB,W1,C,Agent,_Exp,Body),
	!.
genclass(_,[W1,G1],A,A,some,G1) :-
	!.

/**********************************************************************
 * 
 * assertMA(+Class,+Head,+WorldGoal,+Goal)
 * asserts the appropriate clause for the given Class. If Class is all,
 * i.e. we are dealing with a mutual modal operator, no world checks 
 * have to be done. Otherwise WorldGoal has to be added to the body of
 * the clause.
 *
 */

assertMA(A1,hl_holds(KB,every,m(MOp,A1),X,Y), WG, G) :-
	isSlot(A1),
	asserta((hl_holds(KB,every,m(MOp,A1),X,Y) :- (WG, G))),
	!.
assertMA(all,hl_holds(KB,all,m(MOp,A),X,Y), WG, G) :-
	asserta((hl_holds(KB,all,m(MOp,A),X,Y) :-  G)),
	!.
assertMA(A,hl_holds(KB,some,m(MOp,A),X,Y), WG, G) :-
	asserta((hl_holds(KB,some,m(MOp,A),X,Y) :- (WG, G))),
	!.
assertMA(concept(_),hl_holds(KB,D,m(MOp,A),X,Y), WG, G) :-
	asserta((hl_holds(KB,D,m(MOp,A),X,Y) :- (WG, G))),
	!.


/**********************************************************************
 *
 * modalAxioms(+KBName,+KripkeClass,+MOp,+Agent)
 * asserts the modal axioms for the modal operator MOp and agent Agent
 * instance knowledgeBase KBName for KripkeClass.
 *
 */
	
modalAxioms(CTX,KName,MOp,A1) :-
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	modalAxioms(KBName,CTX,KName,MOp,A1).
modalAxioms(KBName,KName,MOp,A1) :-
	knowledgeBase(KBName,_,_),
	modalAxioms(KBName,[],KName,MOp,A1).

modalAxioms(KBName,CTX,k,MOp,A1) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	genclass(KB,[W1,G1],A1,A,C,Goal),
	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
	assertMA(A1,
                 hl_holds(KB,C,m(MOp,A),U,app(_FF:m(MOp,A),U)), 
		 (not(not(world(KB,m(MOp,A),U,V)))), 
		 (normal(KB,U), Goal)),
	asserta(modalAxioms(KB,CTX,user,k,A1,MOp,A)),
	!.
modalAxioms(KBName,CTX,kd45,MOp,A1) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	genclass(KB,[W1,G1],A1,A,C,Goal),
	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
	assertMA(A1,
	         hl_holds(KB,C,m(MOp,A),U,app(_FF:m(MOp,A),V)), 
		 (not(not(world(KB,m(MOp,A),U,V)))), 
		 (normal(KB,U), Goal)),
	asserta(modalAxioms(KB,CTX,user,kd45,A1,MOp,A)),
	!.
modalAxioms(KBName,CTX,kd4e,MOp,A) :-
	modalAxioms(KBName,kd45,MOp,A).
modalAxioms(KBName,CTX,kd5,MOp,A1) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	genclass(KB,[W1,G1],A1,A,C,Goal),
	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
	assertMA(A1,
	         hl_holds(KB,C,m(MOp,A),app(_F1:m(MOp,A),U),app(_F2:m(MOp,A),V)), 
		 ((world(KB,m(MOp,A),U,V), not(U == []))), 
		 Goal),
	assertMA(A1,
	         hl_holds(KB,C,m(MOp,A),U,app(_F2:m(MOp,A),U)), 
		 true, 
		 Goal),
	asserta(modalAxioms(KB,CTX,user,kd5,A1,MOp,A)),
	!.
modalAxioms(KBName,CTX,kd4,MOp,A1) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	genclass(KB,[W1,G1],A1,A,C,Goal),
	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
	assertMA(A1,hl_holds(KB,C,m(MOp,A),U,app(_F1:m(MOp,A),U)), Goal),
	assertMA(A1,hl_holds(KB,C,m(MOp,A),U,app(_F1:m(MOp,A),V)), (world(KB,m(MOp,A),U,V), (hl_holds(KB,_,m(MOp,A),U,V), Goal))),
	asserta(modalAxioms(KB,CTX,user,k4,A1,MOp,A)),
	!.
modalAxioms(KBName,CTX,kt,MOp,A1) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	genclass(KB,[W1,G1],A1,A,C,Goal),
	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
	assertMA(A1,hl_holds(KB,C,m(MOp,A),U,app(_F1:m(MOp,A),U)), Goal),
	assertMA(A1,hl_holds(KB,C,m(MOp,A),U,U), Goal),
	asserta(modalAxioms(KB,CTX,user,kt,A1,MOp,A)),
	!.

%% new code
%% uh 17.04.94
%% modalAxioms(KBName,CTX,k,MOp,A1) :-
%% 	knowledgeBase(KBName,KB,_),
%% 	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
%% 	genclass(KB,[W1,G1],A1,A,C,Goal),
%% 	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
%% 	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
%% 	assertMA(A1,
%%                  hl_holds(KB,C,m(MOp,A),U,app(_FF:m(MOp,A),U)), 
%% 		 (not(not(world(KB,m(MOp,A),W1,U)))),
%% 		 (normal(KB,U), Goal)),
%% 	asserta(modalAxioms(KB,CTX,user,k,A1,MOp,A)),
%% 	!.
%% modalAxioms(KBName,CTX,kd45,MOp,A1) :-
%% 	knowledgeBase(KBName,KB,_),
%% 	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
%% 	genclass(KB,[W1,G1],A1,A,C,Goal),
%% 	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
%% 	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
%% 	assertMA(A1,
%% 	         hl_holds(KB,C,m(MOp,A),V,app(_FF:m(MOp,A),W)), 
%% 		 (not(not((world(KB,m(MOp,A),U,V),
%% 		 world(KB,m(MOp,A),U,W),
%% 		 world(KB,m(MOp,_),[],U))))),
%% 		 true),
%% %	assertMA(A1,
%% %                 hl_holds(KB,C,m(MOp,A),U,app(_FF:m(MOp,A),U)), 
%% %		 (not(not(world(KB,m(MOp,A),U,V)))), 
%% %		 (normal(KB,U), Goal)),
%% 	asserta(modalAxioms(KB,CTX,user,kd45,A1,MOp,A)),
%% 	!.
%% modalAxioms(KBName,CTX,kd4e,MOp,A) :-
%% 	modalAxioms(KBName,kd45,MOp,A).
%% modalAxioms(KBName,CTX,kd5,MOp,A1) :-
%% 	knowledgeBase(KBName,KB,_),
%% 	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
%% 	genclass(KB,[W1,G1],A1,A,C,Goal),
%% 	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
%% 	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
%% 	assertMA(A1,
%% 	         hl_holds(KB,C,m(MOp,A),app(_F1:m(MOp,A),U),app(_F2:m(MOp,A),V)), 
%% 		 ((world(KB,m(MOp,A),W1,U), 
%% 		   (world(KB,m(MOp,A),W1,V), not(U == [])))), 
%% 		 Goal),
%% 	assertMA(A1,
%% 	         hl_holds(KB,C,m(MOp,A),U,app(_F2:m(MOp,A),U)), 
%% 		 (world(KB,m(MOp,A),W1,U)), 
%% 		 Goal),
%% 	asserta(modalAxioms(KB,CTX,user,kd5,A1,MOp,A)),
%% 	!.
%% modalAxioms(KBName,CTX,kd4,MOp,A1) :-
%% 	knowledgeBase(KBName,KB,_),
%% 	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
%% 	genclass(KB,[W1,G1],A1,A,C,Goal),
%% 	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
%% 	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
%% 	assertMA(A1,
%%                  hl_holds(KB,C,m(MOp,A),U,app(_F1:m(MOp,A),U)), 
%% 		 (world(KB,m(MOp,A),W1,U)),
%% 		 Goal),
%% 	assertMA(A1,
%%                  hl_holds(KB,C,m(MOp,A),U,app(_F1:m(MOp,A),V)), 
%% 	         (world(KB,m(MOp,A),W1,U), 
%% 		  (world(KB,m(MOp,A),W1,V), hl_holds(KB,_,m(MOp,A),U,V))),
%% 		 Goal),
%% 	asserta(modalAxioms(KB,CTX,user,k4,A1,MOp,A)),
%% 	!.
%% modalAxioms(KBName,CTX,kt,MOp,A1) :-
%% 	knowledgeBase(KBName,KB,_),
%% 	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
%% 	genclass(KB,[W1,G1],A1,A,C,Goal),
%% 	retractall(hl_holds(KB,C,m(MOp,A),_,_)),
%% 	retractall(modalAxioms(KB,CTX,user,_,A1,MOp,A)),
%% 	assertMA(A1,
%% 	         hl_holds(KB,C,m(MOp,A),U,app(_F1:m(MOp,A),U)), 
%% 		 (world(KB,m(MOp,A),W1,U)),
%% 		 Goal),
%% 	assertMA(A1,
%% 	         hl_holds(KB,C,m(MOp,A),U,U), 
%% 		 (world(KB,m(MOp,A),W1,U)),
%% 		 Goal),
%% 	asserta(modalAxioms(KB,CTX,user,kt,A1,MOp,A)),
%% 	!.

/**********************************************************************
 *
 * normal(+KBName,+World)
 * succeeds if World is normal, i.e. has a successor.
 *
 */

normal(_,_).

/**********************************************************************
 *
 * world(+KBName,m(+MOp,+Agent),+WorldSequence) 
 * checks wether or not WorldSequence is a sequence of worlds for
 * modal operator MOp and agent Agent.
 *
 */

world(_KB,m(_MOp,_A),U,U).
world(KB,m(MOp,A),U,app(_FF:m(MOp,A),V)) :-
	world(KB,m(MOp,A),U,V).

worldDepth(W,0) :-
	isSlot(W),
	!.
worldDepth([],0).
worldDepth(app(_FF:m(_MOp,_A),V),N) :-
	worldDepth(V,M),
	N is M+1.

/* append([], List, List).
append([Head|Tail], List, [Head|Rest]) :- 
        append(Tail, List, Rest).

member(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).

not(Goal) :- 
	call(\+ Goal).
*/

setofOrNil(A,B,C) :-
        (setof(A,B,C) -> true ; C = []).


/**********************************************************************
 *
 * OPTIONS
 *
 */

/***********************************************************************
 *
 * setOption(+Option,+Set)
 * set option Option to value Set.
 *
 */

setOption(Option,Set) :-
        retractall(option(Option,_)),
        asserta(option(Option,Set)).

/**********************************************************************
 *
 * ifOption(+Option,+Set,+Goal)
 * executes Goal if the current value of Option is Set otherwise
 * the predicate suceeds.
 *
 */

ifOption(Option,Set,Goal) :-
        (option(Option,Set) ->
	    call(Goal)
	;
	    true
	).

retractall(KB,Pred/Arity) :-
        constructHead(KB,Pred/Arity,Head),
        retractall(Head).


modFindClause([],[]).
modFindClause([CL|T],[CL|Erg1]) :-
	% nur holen was man braucht
	modPositiv(CL,[instance(_,_,X)]),
	% tests vorziehen
	isSlot(X),
	modNegativ(CL,[instance(_,_,Y)]),
	isSlot(Y), 
	X == Y,!, % nicht da der cut?
	modProofTree(CL,_PT1),
	modFindClause(T,Erg1).
modFindClause([CL|T],[CL|Erg1]) :-
	modPositiv(CL,[instance(_,_,X1,Y1)]),
	isSlot(X1),
	isSlot(Y1),	
	modNegativ(CL,[instance(_,_,X2,Y2)]),
	isSlot(X2),
	isSlot(Y2),
	X1 == X2,
	Y1 == Y2,!, % hier cut?
	modProofTree(CL,_PT1),
	modFindClause(T,Erg1).
modFindClause([_|T],Erg) :-
	modFindClause(T,Erg).


%---------------------------------------------------------------------------
% modInference(+CLAUSELIST1,-BOOL,-CLAUSELIST2,+LEVEL)
% BOOL is `false' if CLAUSELIST1 is consistent. Otherwise, BOOL is `true'
% and CLAUSELIST2 is a list of clauses which have exactly one positive 
% literal and are a consequence of CLAUSELIST1.
% modInference is a end-recursive predicate.  % falsch, wegen den cuts
% LEVEL is the level of recursion % that has been reached.
%
%      
%    

modInference(CL1,Bool,CL,stat(N3,N4)) :-
	modForwardSubsumption(CL1,[],[],[],CL2),
	ifOption(workingOutput,yes,(length(CL2,N2), cmp_write('Starting with '), cmp_print(N2), cmp_write(' clauses: \n'),printClauseList(CL2),cmp_nl)),
	modInference([],CL2,CL3,Bool,1),
	length(CL3,N3),
	modFindClause(CL3,CL4),
	length(CL4,N4),
	(option(decider,on) ->
	    CL = CL4
	;
	    CL = CL3
	).

modInference(Set1,Set2,[],Bool,_) :-
% 	append(Set1,Set2,Set3), nicht unn"otig Terme nur zum lesen aufbauen
	(member(cl([],[],_Pt),Set1) ->
	    true
	;
	    member(cl([],[],_Pt),Set2)
	),
	!,
	Bool = false.
modInference(Old,New,Result,Bool,Level) :-
	ifOption(workingOutput,yes,(cmp_write('Starting modInference on level '),cmp_print(Level),cmp_nl)),
	ifOption(compare,yes,(cmp_print('Clauses: \n'),printClauseList(New))),
	modConsequences(product,Old,New,C1),
	modConsequences(combination,New,New,C2),
	append(Old,New,OldClauses),
	append(C1,C2,NewClauses),
%	ifOption(workingOutput,yes,(length(NewClauses,N1), cmp_write('Consequences: '),cmp_print(N1),cmp_nl)),
	ifOption(traceOutput,yes,(cmp_write('Starting subsumption tests.\n'),cmp_write('Old clauses: '),printClauseList(OldClauses),cmp_nl,cmp_write('New clauses: '),printClauseList(NewClauses),cmp_nl)),
	modForwardSubsumption(NewClauses,OldClauses,[],RemainingOldClauses,NonSubsumedClauses),
	modInferenceCont(RemainingOldClauses,NonSubsumedClauses,Result,Bool,Level).

modInferenceCont(RemainingOldClauses,[],RemainingOldClauses,true,_) :-
	ifOption(compare,yes,(cmp_print('Final Clauses: \n'),printClauseList(RemainingOldClauses))).
modInferenceCont(RemainingOldClauses,NonSubsumedClauses,Result,Bool,Level) :-
	NonSubsumedClauses \== [],
	ifOption(traceOutput,yes,(length(RemainingOldClauses,N2), cmp_write('Remaining old clauses: '),cmp_print(N2),cmp_nl,printClauseList(RemainingOldClauses),cmp_nl)),
        ifOption(traceOutput,yes,(length(NonSubsumedClauses,N3),cmp_write('Remaining new clauses: '),cmp_print(N3),cmp_nl,printClauseList(NonSubsumedClauses),cmp_nl)),
	M is Level+1,
	modInference(RemainingOldClauses,NonSubsumedClauses,Result,Bool,M).

%---------------------------------------------------------------------------
% modDelete(+KB,+CTX,+CONCEPTROLENAME,+CLAUSELLIST,-LIST)
% LIST is the result of removing CONCEPTROLENAME of CLAUSELLIST 
%
%      

modDelete(CR,CList,EList) :-
	modDelete([],CR,CList,EList).

modDelete(CTX,CR,CList,EList) :-
	currentKnowledgeBase(KB),
	modDelete(KB,CTX,CR,CList,EList).

modDelete(KB,_CTX,CR,CList,EList) :-
	currentKnowledgeBase(KB), % warum 2mal hinteroneander?
	(conceptName(KB,_,_,CR);roleName(KB,_,_,CR)),
	ifOption(prover,Prover,true),
	setOption(prover,nil),
	modDelete1(CList,CR,EList), % 1st 2nd argument swapped
	setOption(prover,Prover).

% first argument indexing -> 1st 2nd swapped
modDelete1([],_CR,[]).
modDelete1([Cl|Cr],CR,Out) :-
	modProofTree(Cl,PTree),
	collect(PTree,List),
	not(modMember(CR,List)),
	% hier soll wohl cut hin, oder?
	!,
	Out = [Cl|Er], % postpone output unification after the cut!
	modDelete1(CR,Cr,Er). % jetzt ist es tailrec
modDelete1([_Cl|Cr],CR,Er) :-
	modDelete1(CR,Cr,Er).

%---------------------------------------------------------------------------
% modSubsumes(+KB,+CTX,+CONCEPTROLENAME1,+CONCEPTROLENAME2,+CLAUSELLIST)
% succeed if CONCEPTROLENAME1 subsumes CONCEPTROLENAME2
%
%      

modSubsumes(CR1,CR2,CList) :-
	modSubsumes([],CR1,CR2,CList).

modSubsumes(CTX,_CR1,_CR2,CList) :-
	currentKnowledgeBase(KB),
	modSubsumes(KB,CTX,_CR,CList,_EList).

modSubsumes(KB,_CTX,CR1,CR2,CList) :-
	currentKnowledgeBase(KB), % wieder; warum 2x currentKB
	% ist das exclusiv?
	% wenn ja, dann un"otiger choicepoint
	(conceptName(KB,_,_,CR1);roleName(KB,_,_,CR1)),
	(conceptName(KB,_,_,CR2);roleName(KB,_,_,CR2)),
	ifOption(prover,Prover,true),
	setOption(prover,nil),
	modSubsumes1(CR1,CR2,CList),  % 1st 2nd swapped (tailrec)
	setOption(prover,Prover).

modSubsumes1([Cl|_Cr],CR1,CR2) :-
	modPositiv(Cl,P),
	modPositiv(Cl,N),
	collect(P,List1),
	collect(N,List2),
	modMember(CR1,List1),
	modMember(CR2,List2),
	!.
% modSubsumes1(List,CR1,CR2) :-
modSubsumes1([_|List],CR1,CR2) :-
	cmp_write('modified modSubsumes1\n'),
	modSubsumes1(List,CR1,CR2). % das ist onee Endlos-Schleife
% vermutlich [_|List] im Kopf ?

%---------------------------------------------------------------------------
% modElementOf(+KB,+CTX,+CONCEPTROLENAME1,+CONCEPTROLENAME2,+CLAUSELLIST)
%
%      

modElementOf(KBName,CTX,X,CT,CList) :-
	currentKnowledgeBase(KB),
	knowledgeBase(KBName,KB,_),	
	modSuccList(CT,KBName,CTX,CList,SuccList), % 3rd arg becomes 1st
	modMember(X,SuccList).

modSuccList([],_KBName,_CTX,_CList,Out) :- !,Out = [].
modSuccList([CT|CR],KBName,CTX,CList,SuccList) :-	
	modSuccList(KBName,CTX,CT,CList,SuccList1),
	modSuccList(KBName,CTX,CR,CList,SuccList2),
	cmp_union(SuccList1,SuccList2,SuccList),
	!.
modSuccList(CT,_KBName,_CTX,CList,SuccList) :-	
	findall(NC,(member(El,CList),modPositiv(El,CT),modNegativ(El,NC)),SuccList),
	!.

modAsk(CR,CList) :-
	modAsk([],CR,CList).

modAsk(CTX,CR,CList) :-
	currentKnowledgeBase(KB),
	knowledgeBase(KBName,KB,_),
	modAsk(KBName,CTX,CR,CList).

modAsk(KBName,CTX,elementOf(X,CT),_CList) :-
	currentKnowledgeBase(KB),
	knowledgeBase(KBName,KB,_),	
% das wird irgendwie immer 2x mal gemacht, warum?
% das ??? ist nicht von mir
%       ???	
        defineEquivalentConceptname(KBName,CTX,CT,query,C),
        convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
        getNegatedConcept(C,C1),
        constructMLMark(rn(_AN5,_RN5,_S5,_O5),W1,C1,X,_HYPS,_D1,_InHead1),
        getQuery(KB,W1,C,X,_Exp,Goal),
        performQuery(X,G1,Goal),
        allowedAnswerConcept(KB,C).
%       ????   


%---------------------------------------------------------------------------
% modAnd(+BOOL1,+BOOL2,-BOOL3)
% BOOL3 is the logical conjunction of the boolean values BOOL1 and BOOL2.
%
%      

modAnd(false,_,false).
modAnd(true,X,X).


%---------------------------------------------------------------------------
% modJoin(+LIST1,+LIST2,-LIST3)
% If LIST1 and LIST2 are not syntactically equal, LIST3 is the result of 
% appending LIST2 to LIST1. Otherwise, LIST3 is LIST1.
%
%      

modJoin(CL1,CL2,CL1) :-
	CL1 == CL2,
	!.
modJoin(CL1,CL2,CL3) :-
	append(CL1,CL2,CL3).

%---------------------------------------------------------------------------
% modPositiv(+Clause,-Positiv) 
% Returns the positiv of the clause
% modNegativ(+Clause,-Negativ) 
% Returns the negativ of the clause
% modProofTree(+Clause,-ProovTree)
% Returns the proof-tree of the clause
%
%      

% vermeide unn"otige Variablen, niemals cut (nur onee Klausel)
modPositiv(C,P) :- arg(1,C,P).
modNegativ(C,N) :- arg(2,C,N).
modProofTree(cl(_,_,char(PT,_,_)),PT).
modProofTree(cl(_,_,PT),PT) :-
	atomic(PT).
modNewArgOfClause(cl(_,_,char(_,Arg1,Arg2)),Arg1,Arg2).


printClauseList([]) :- cmp_nl.
printClauseList([C|CL]) :-
	cmp_write('Clause: '),
	cmp_print(C),
	cmp_nl,
	printClauseList(CL).

% Klauseln aus RList sollen mit allen Klauseln aus LList resolviert werden
% zwei cuts instance oneer Klausel => fast immer was nicht ok
% cut am Angang definitiv unn"otig
modConsequences(Type,LList,RList,Consequences) :-
	ifOption(traceOutput,yes,(cmp_write('Starting modConsequences.\n'))),
	ifOption(traceOutput,yes,(length(LList,N1), length(RList,N2), cmp_write('Starting outer loop with '),cmp_print(N2),cmp_write(' on '), cmp_print(N1),cmp_write(' clauses.\n'))),
	modSetfactor(LList,Factors),
	ifOption(traceOutput,yes,(length(Factors,N3), cmp_write('Number of factors '),cmp_print(N3),cmp_nl)),
        modRemoveTaut(Factors,NonTautologicalFactors),
	all_maxlits_all_clauses(LList,LListMax),
	all_maxlits_all_clauses(RList,RListMax),
	modSetproduct0(Type,LListMax,RListMax,Resolvents),
	ifOption(traceOutput,yes,(length(Resolvents,N4), cmp_write('Number of resolvents '), cmp_print(N4),cmp_nl)),
	modRemoveTaut(Resolvents,NonTautologicalResolvents),
	append(NonTautologicalFactors,NonTautologicalResolvents,Consequences).

%---------------------------------------------------------------------------
% all_maxlits_all_clauses(+CLAUSELIST1,ECLAUSELIST2)
% ECLAUSELIST2 will be a list of clauses where each clause is of the form
% cl(PLits,NLits,PT)-maxlits(MPLits,MNLits)
% such that MPLits is the list of maximal positive literals instance the clause,
% and MNLits is the list of maximal negative literals instance the clause.
%
%      

all_maxlits_all_clauses([],[]).
all_maxlits_all_clauses([Clause|List],[Clause-MaxLits|ListMax]) :-
	find_maximal_literals(Clause,MaxLits),
	ifOption(traceOutput,yes,(cmp_write('Maximal literals '), cmp_print(MaxLits), cmp_write(' instance '), cmp_print(Clause), cmp_nl)),
	all_maxlits_all_clauses(List,ListMax).
	
%---------------------------------------------------------------------------
% modSetfactor(+CLAUSES,-FACTORS)
% FACTORS is the list of all factors of clauses instance CLAUSES
% with respect to order-restricted factoring.
%
%      
%     
% Description: 

modSetfactor([],[]).
modSetfactor([Clause|ClauseList],FactorList) :-
        setofOrNil(Factor,maximalFactor(Clause,Factor),FactorList1),
	% !, % setof is det!
	modSetfactor(ClauseList,FactorList2), % nicht tailrek (acc passing)
	append(FactorList1,FactorList2,FactorList).

%---------------------------------------------------------------------------
% modSetproduct0(+CLAUSELIST1,+CLAUSELIST2,-RESOLVENTS)
% RESOLVENTS is the list of all resolvents of clauses instance CLAUSELIST1
% and CLAUSELIST2 with respect to order-restricted resolution.
%
%      
%     
% Description: 

modSetproduct0(_,[], _, []) :-
	ifOption(traceOutput,yes,(cmp_nl,cmp_write('Ended outer loop.\n'))).
modSetproduct0(product,[Head-MLitsHead|Tail], Set, Result) :-
%	ifOption(traceOutput,yes,(length(Set,N),cmp_write('Starting inner loop for '),cmp_print(Head),cmp_write(' with '),cmp_print(N),cmp_nl)),
        modSetproduct(Set, Head, MLitsHead, Result1),
        modSetproduct0(product,Tail, Set, Result2),
	append(Result1,Result2, Result).
modSetproduct0(combination,[Head-MLitsHead|Tail], [C1|Set], Result) :-
	ifOption(workingOutput,yes,(cmp_write('Computing all resolvents with '),cmp_print(Head),cmp_nl)),
        modSetproduct([C1|Set], Head, MLitsHead, Result1),
        modSetproduct0(combination,Tail, Set, Result2),
	append(Result1,Result2, Result).
 
%---------------------------------------------------------------------------
% modSetproduct(+CLAUSELIST1,+CLAUSE,+MAXLITS,-RESOLVENTS)
% RESOLVENTS is the list of all resolvents of clauses instance CLAUSELIST1 with 
% CLAUSE (which contains MAXLITS as maximal literals) with respect to 
% order-restricted resolution.
%
%      
%     
% Description: 

modSetproduct([], _X, _, []).
%	ifOption(traceOutput,yes,(cmp_nl,cmp_write('Ended inner loop for '),cmp_print(X),cmp_nl)).
modSetproduct([Head-MLitsHead|Tail], X, MLitsX, Erg) :-
%% Removed uh 03.02.95
%%	not(modClEqual(Head,X)),
	setofOrNil(Res3,maximalResolvent(X,MLitsX,Head,MLitsHead,Res3),Resolvents),
        modSetproduct(Tail, X, MLitsX, TailX),
	append(Resolvents,TailX,Erg).


%---------------------------------------------------------------------------
% find_maximal_literals(+CLAUSE1,maxlits(+MPLITS,+MNLITS))
% MPLITS will be the list of maximal positive literals instance CLAUSE1,
% MNLITS will be the list of maximal negative literals instance CLAUSE1.
%
%      

find_maximal_literals(Clause,maxlits(MlitsP,MlitsN)) :-
	modPositiv(Clause,P),
	setofOrNil(ML,maximal_literal(P,Clause,ML),MlitsP),
	modNegativ(Clause,N),
	setofOrNil(ML,maximal_literal(N,Clause,ML),MlitsN).

maximal_literal(Ls,C,Lit) :-
	member(Lit,Ls),
	modMaximalLiteral(Lit,C).

modRemoveEqualLit([],[]) :-
	!.
modRemoveEqualLit([cl(PTerm,NTerm,PTree)|Cl],[cl(C1,C2,PTree)|CLR]) :-
	modRemoveEqualLit1(PTerm,C1),
	modRemoveEqualLit1(NTerm,C2),
	!,
	modRemoveEqualLit(Cl,CLR).
modRemoveEqualLit1([H],[H]) :-
	!.
modRemoveEqualLit1([Head|Tail],Erg) :-
	modMember(Head,Tail),
	!,
	modRemoveEqualLit1(Tail,Erg).
modRemoveEqualLit1([Head|Tail],[Head|Erg]) :-
	modRemoveEqualLit1(Tail,Erg).


%---------------------------------------------------------------------------
% modBackwardSubsumption(+CLAUSELIST1,+CLAUSELIST2,-CLAUSELIST3)
% CLAUSELIST3 will be the list of all clauses instance CLAUSELIST1 which are not
% subsumed by clauses instance CLAUSELIST2.
%
%      

modBackwardSubsumption([],_NewClauses,[]).
modBackwardSubsumption([OldClause|OldClauses],NewClauses,AllClauses) :-
	modFindSubsumingClause(NewClauses,OldClause),!,
	modBackwardSubsumption(OldClauses,NewClauses,AllClauses).
modBackwardSubsumption([OldClause|OldClauses],NewClauses,[OldClause|AllClauses]) :-
	modBackwardSubsumption(OldClauses,NewClauses,AllClauses).

%---------------------------------------------------------------------------
% modForwardSubsumption(+CLAUSELIST1,+CLAUSELIST2,-CLAUSELIST3)
% CLAUSELIST3 will be the list of all clauses instance CLAUSELIST2 which are not
% subsumed by clauses instance CLAUSELIST1.
%
%      

modForwardSubsumption([],OldClauses,ProductiveNewClauses,OldClauses,ProductiveNewClauses) :-
	ifOption(traceOutput,yes,cmp_nl).
modForwardSubsumption([NewClause|NewClauses],OldClauses0,ProductiveNewClauses,OldClauses1,NewClauses1) :-
	ifOption(workingOutput,yes,(cmp_write('S'),ttyflush)),	
	(
	    modFindSubsumingClause(ProductiveNewClauses,NewClause)
	;
	    modFindSubsumingClause(OldClauses0,NewClause)
	),
	!,
	modForwardSubsumption(NewClauses,OldClauses0,ProductiveNewClauses,OldClauses1,NewClauses1).
modForwardSubsumption([NewClause|NewClauses],OldClauses0,ProductiveNewClauses0,OldClauses2,NewClauses2) :-
%	ifOption(traceOutput,yes,(cmp_write('B('),ttyflush)),
	modBackwardSubsumption(OldClauses0,[NewClause],OldClauses1),
	modBackwardSubsumption(ProductiveNewClauses0,[NewClause],ProductiveNewClauses1),
%	ifOption(traceOutput,yes,(cmp_write(')B'),ttyflush)),
	modForwardSubsumption(NewClauses,OldClauses1,[NewClause|ProductiveNewClauses1],OldClauses2,NewClauses2).


%---------------------------------------------------------------------------
% modExtractCR(+CLAUSE1,-CLAUSE2)
% Clause2 is the list of Concept-Role-Name instance CLAUSE1
%
%  

modExtract(Cl,P1,N1) :-
	modPositiv(Cl,P),
	modNegativ(Cl,N),
	modExtractCR(P,P1),
	modExtractCR(N,N1).

modExtractCR([],[]) :-
	!.
modExtractCR([H|T],[L1|L]) :-
	modExtractCR1(H,L1),
	modExtractCR(T,L),
	!.
modExtractCR1(instance(_,L1,_),L1).
modExtractCR1(instance(_,L1,_,_),L1).

modExtractSkolem(Pos,Neg,Skol) :-
	modExtractSkolem(Pos,Skol1),
	modExtractSkolem(Neg,Skol2),
	modUnion(Skol1,Skol2,Skol).
modExtractSkolem([],[]).
modExtractSkolem([H|T],L) :-
	modExtractSkolem1(H,L1),	
	modExtractSkolem(T,L2),
	modUnion(L1,L2,L).

modExtractSkolem1(instance(_,_,Var),[]) :-
	(isSlot(Var);atom(Var)).
modExtractSkolem1(instance(_,_,Var),[S]) :-
	nonvar(Var),
	functor(Var,S,_).
modExtractSkolem1(instance(_,_,Var,Var1),S) :-
	((isSlot(Var1);atom(Var1)) ->
	    S = [];
	    (functor(Var1,S1,_),S = [S1])).


modSubsumes(C,D) :-
%	ifOption(traceOutput,yes,(cmp_write('Test if '), cmp_print(C), cmp_write(' subsumes '), cmp_print(D), cmp_nl)),
	modNewArgOfClause(C,[PTC,NTC],Skol1),  % ?
	modNewArgOfClause(D,[PTD,NTD],Skol2),  % ?
	cmp_subset(PTC,PTD),cmp_subset(NTC,NTD),  % ?
	cmp_subset(Skol1,Skol2),  % ?
	modPositiv(D,PD),modNegativ(D,ND),
%	modPositiv(C,PC),modNegativ(C,NC),
%	copy_term(cl(PC,NC),cl(PC0,NC0)),
	modPositiv(C,PC0),modNegativ(C,NC0),
	isGround_copy(cl(PD,ND),cl(PD0,ND0)),
	ifOption(traceOutput,yes,(cmp_write('Ground Term D: '),cmp_print(D), cmp_nl)),
	!,
	not(not((newStillman(PC0,PD0),newStillman(NC0,ND0)))),
	ifOption(traceOutput,yes,(cmp_write('Answer is positive.\n'))).
%       stillman(NC0,ND0,1,1).
%	stillman(PC0,PD0,1,1),

my_stillman(C1,C2,D1,D2) :-
	((newStillman(C1,D1),newStillman(C2,D2)) ->
	    (not(not((newStillman(C1,D1),newStillman(C2,D2)))) ->
		true;
		(cmp_write('stillman'),cmp_write(C1),cmp_write(C2),cmp_write(D1),cmp_write(D2),cmp_nl,fail));
            (not(not((newStillman(C1,D1),newStillman(C2,D2)))) ->
		(cmp_write('stillman'),cmp_write(C1),cmp_write(C2),cmp_write(D1),cmp_write(D2),cmp_nl,fail);
		fail)).
	

stillman(ArgC,ArgD,I,J) :-
	length(ArgD,K),
	not(J > K),
	!,
	stillman1(A,I,J,K,ArgC,ArgD),
	((length(ArgC,I);(I1 is I+1,stillman(ArgC,ArgD,I1,1))) ->
	    true;
	    (A1 is  A + 1,stillman(ArgC,ArgD,I,A1))).
stillman1(A,I,A,K,ArgC,ArgD) :-
	not(A > K),
	!,
	((modNth(I,ArgC,LC),modNth(A,ArgD,LC)) ->
	    NA = A;
	    (A1 is A+1,stillman1(NA,I,A1,K,ArgC,ArgD))).

newStillman([],ArgD) :- !.
newStillman([LitC|R],ArgD) :-
	newStillman1(LitC,ArgD),
	!,
	newStillman(R,ArgD).

newStillman1(LitC,[LitD|R]) :-
	((LitC = LitD) ->
	    !;
	    newStillman1(LitC,R)).

modNth(1,[H|_T],H) :- !.
modNth(Nr,[_|T],E) :-
	Nr1 is Nr - 1,
	modNth(Nr1,T,E).


%---------------------------------------------------------------------------
% modFindSubsumingClause(+CLAUSE1,+CLAUSELIST)
% true iff CLAUSELIST contains a clause subsuming CLAUSE1.
%
%      

%modFindSubsumingClause(_Clause,[]) :-
%	!,
%	ifOption(traceOutput,yes,(cmp_write('found nothing.\n'))),
%	fail.

%modFindSubsumingClause([Clause2|ClauseList],Clause1) :-
%	modProofTree(Clause2,PT2),
%	modProofTree(Clause1,PT1),
%	not(cmp_subset(PT2,PT1)),
%	!,
%	(modFindSubsumingClause(ClauseList,Clause1)->
%	    true;
%	    modSubsumes(Clause2,Clause1)).

modFindSubsumingClause([Clause2|_ClauseList],Clause1) :-
%	modNewArgOfClause(Clause2,[PosL2,NegL2],Skol2),   % ?
%	modNewArgOfClause(Clause1,[PosL1,NegL1],Skol1),  % ?
%	cmp_subset(PosL2,PosL1), cmp_subset(NegL2,NegL1),  % ?
%	cmp_subset(Skol2,Skol1),  % ?
%	!,  % ?
	modSubsumes(Clause2,Clause1).
	ifOption(traceOutput,yes,(cmp_write('Subsumption:\n'),cmp_print(Clause2),cmp_write(' subsumes \n'),cmp_print(Clause1),cmp_nl)).

modFindSubsumingClause([_Clause2|ClauseList],Clause1) :-
	modFindSubsumingClause(ClauseList,Clause1).


%---------------------------------------------------------------------------
% modClSubsumes(+CLAUSE1,+CLAUSE2)
% true iff CLAUSE1 subsumes CLAUSE2.
%
%      
%     
%               					
% Description: 


modOldSubsumes(CL1,CL2) :-
	modSubsumes_maybe(CL1,CL2),
	isGround_copy(CL2,cl(P21,N21,PT21)),
	% The clause cl(P21,N21,PT21) is negated by turning it into cl(N21,P21,PT21)
	modLiteralsToClauses(cl(N21,P21,PT21),ClauseList),
%	ifOption(traceOutput,yes,(cmp_write('Test if '), cmp_print(cl(P1,N1)), cmp_write(' subsumes '), cmp_print(cl(P2,N2)), cmp_nl)),
%	cmp_write('?'),ttyflush,
	modSubsumptionTest(ClauseList,[CL1],Result),
	Result == true.
%	cmp_write('S'),ttyflush.
%	ifOption(traceOutput,yes,(cmp_write('Answer is positive.\n'))),

modSubsumes_maybe(CL1,CL2) :-
	modPositiv(CL1,P1),
	modNegativ(CL1,N1),
	modPositiv(CL2,P2),
	modNegativ(CL2,N2),
	all_maybe_unifiable(P1,P2),
	all_maybe_unifiable(N1,N2).

all_maybe_unifiable([],_).
all_maybe_unifiable([Lit|Lits],ULits) :-
	one_maybe_unifiable(ULits,Lit),
	all_maybe_unifiable(Lits,ULits).

one_maybe_unifiable([Lit0|_Lits],Lit1) :-
	maybe_unifiable(Lit0,Lit1),!.
one_maybe_unifiable([_Lit0|Lits],Lit1) :-
	one_maybe_unifiable(Lits,Lit1).

%---------------------------------------------------------------------------
% modSubsumptionTest(+CLAUSELIST1,+CLAUSELIST2,-RESULT)
% tries to derive the empty clause from CLAUSELIST1 and CLAUSELIST2
% (see the description of a subsumption algorithm instance [Chang and Lee, 1973]
% RESULT is instantiated by `true' if the empty clause can be derived.
% Otherwise, RESULT is instantiated by `false'.
%
%      
% Description: 

modSubsumptionTest(_W,U,Result) :-
	member(cl([],[],_),U),
	!,
	% cl(P1,N1,_) subsumes cl(P2,N2,_)
	Result = true.
modSubsumptionTest(_W,[],Result) :-
	!,
	% cl(P1,N1,_) does not subsume cl(P2,N2,_)
	Result = false.
modSubsumptionTest(W,U1,Result) :-
	modAllResolvents(W,U1,U2),
	modSubsumptionTest(W,U2,Result).

%---------------------------------------------------------------------------
% modAllResolvents(+CLAUSELIST1,+CLAUSELIST2,-CLAUSELIST3)
% CLAUSELIST3 is the list all resolvents of all pairs of clauses CLAUSE1 and 
% CLAUSE2 where CLAUSE1 is taken from CLAUSELIST1 and CLAUSE2 is taken from 
% CLAUSELIST2.
%
%      
% Description: 

modAllResolvents(CL1,CL2,Res) :-
	modAllResolvents(CL1,CL2,[],Res).

modAllResolvents([],_,Res,Res).
modAllResolvents([C|CL1],CL2,Res0,Res2) :-
	modAllResolvents1(CL2,C,Res0,Res1),
	modAllResolvents(CL1,CL2,Res1,Res2).

modAllResolvents1([],_,Res,Res).
modAllResolvents1([C1|CL1],C2,Res0,Res2) :-
	setofOrNil(C3,classicalResolvent(C1,C2,C3),U2),
	append(U2,Res0,Res1),
	modAllResolvents1(CL1,C2,Res1,Res2).
	

classicalResolvent(cl(P1,N1,char(PT1,A1,A2)),cl(P2,N2,char(PT2,B1,B2)),cl(E1,E2,char(PT3,[C1,C2],C3))) :-
	find_maybe_unifiable(P1,N2,L1,L2),
	modReplaceVar(variants(L1,L2,P1,N1,P2,N2),variants(L11,L21,P11,N11,P21,N21)),
	modUnify(L11,L21),
	modSubtract(P11,[L11],C),
	modSubtract(N21,[L21],D), 
	modUnion(C,P21,E1),
	modUnion(D,N11,E2),
	modUnion(PT1,PT2,PT3),
	modExtractSkolem(E1,E2,C3),
	modExtractCR(E1,C1),
	modExtractCR(E2,C2),
	!.
classicalResolvent(cl(P1,N1,char(PT1,A1,A2)),cl(P2,N2,char(PT2,B1,B2)),cl(E1,E2,char(PT3,[C1,C2],C3))) :-
	find_maybe_unifiable(P2,N1,L1,L2),
	modReplaceVar(variants(L1,L2,P1,N1,P2,N2),variants(L11,L21,P11,N11,P21,N21)),
	modUnify(L11,L21),
	modSubtract(P21,[L21],C),
	modSubtract(N11,[L11],D), 
	modUnion(C,P11,E1),
	modUnion(D,N21,E2),
	modUnion(PT1,PT2,PT3),
	modExtractCR(E1,C1),
	modExtractCR(E2,C2),
	modExtractSkolem(E1,E2,C3),
	!.

%---------------------------------------------------------------------------
% modLiteralsToClauses(CLAUSE1,CLAUSELIST)
% negate the clause CLAUSE1 and turns each literal instance CLAUSE1 into a 
% clause. The resulting list of clauses is CLAUSELIST.
%
%      
% Description: 

modLiteralsToClauses(Cl,PCL) :-
	modPositiv(Cl,P),
	modNegativ(Cl,N),
	modPositiveLiteralsToClauses(P,PCL,NCL),
	modNegativeLiteralsToClauses(N,NCL).

% holes (difference lists!)
modPositiveLiteralsToClauses([],Tail,Tail).
modPositiveLiteralsToClauses([H|T],[cl([H],[],_)|T1],Tail) :-
	modPositiveLiteralsToClauses(T,T1,Tail).

modNegativeLiteralsToClauses([],[]).
modNegativeLiteralsToClauses([H|T],[cl([],[H],_)|T1]) :-
	modNegativeLiteralsToClauses(T,T1).


%---------------------------------------------------------------------------
% modClReplaceVarWithAtom(CLAUSE,VARLIST,NUMBER)
% CLAUSE is a clause and VARLIST is a list of VAR1-VAR2 where VAR1 
% and VAR2 are Prolog variables, such that VAR2 occurs instance CLAUSE. Every such 
% variable VAR2 is unified with a fresh constant. As a side effect, a isGround 
% element of CLAUSE is computed.
% To generate a fresh constant, the integer NUMBER is used and incremented
% every time it has been used.
%
%      
% Description: 

modClReplaceVarWithAtom([],_C,_).
modClReplaceVarWithAtom([_-New|T],C,M) :-
	genconstant(M,New,N),
	modClReplaceVarWithAtom(T,C,N).

genconstant(M,V,N) :-
	N is M+1,
	atom_codes(M,N1),
	atom_codes(V,[0's|N1]). %'

modRemoveTaut([],[]) :-
	!.
modRemoveTaut([CL1|CL],CLN) :-
	modPositiv(CL1,PL),
	modNegativ(CL1,NL),
	modProofTree(CL1,_PT),
%	ifOption(traceOutput,yes,(cmp_write('Trying to remove '),cmp_print(cl(PL,NL,PT)))),
	member(P,PL),
	member(N,NL),
	not(not(modVariant(P,N))),
	!,
%	ifOption(traceOutput,yes,(cmp_write(' succeeded.\n'))),
	modRemoveTaut(CL,CLN).
modRemoveTaut([CL1|CL],[CL1|CLN]) :-
	!,
%	ifOption(traceOutput,yes,(cmp_write(' failed.\n'))),
	modRemoveTaut(CL,CLN).

%---------------------------------------------------------------------------
% maximalFactor(+CLAUSE1,-CLAUSE2)
% CLAUSE2 is a factor of CLAUSE1
%
%      
%     
% Description: 

maximalFactor(cl(PLL,NLL,PT),cl(C1,C2,PT)) :-
	modFactorLiteralList(cl(PLL,NLL,PT),PLL,C1), 
	modFactorLiteralList(cl(PLL,NLL,PT),NLL,C2), 
	% check whether the C1 is actually a factor of PLL, i.e. C1 has to
        % be shorther than PLL. Do the same for NLL and C2.
	length(PLL,LengthPLL),
	length(NLL,LengthNLL),
	length(C1,LengthC1),
	length(C2,LengthC2),
	hasChanged(LengthPLL,LengthC1,LengthNLL,LengthC2).

hasChanged(L1,NL1,_L2,_NL2) :-
	L1 \== NL1,
	!.
hasChanged(_L1,_NL1,L2,NL2) :-
	L2 \== NL2,
	!.


modFactorLiteralList(cl(PLL,NLL,PT),[H|T],LiteralList) :-
	memberAndPos(L1,[H|T],PosL1),
	modMaximalLiteral(L1,cl(PLL,NLL,PT)),
	memberAndPos(L2,[H|T],PosL2),
	PosL1 \== PosL2,
	modFactorLiterals([H|T],L1,L2,LiteralList),
	!.
modFactorLiteralList(_,LiteralList,LiteralList) :-
	!.
 
modFactorLiterals(LiteralList,Literal1,Literal2,ResultList) :-
	modUnify(Literal1,Literal2),
	modSubtract(LiteralList,[Literal2],DifferenceList),
	modUnion(DifferenceList,[Literal1],ResultList),
	!.

memberAndPos(X,[X|_],0).
memberAndPos(X,[_|L],N) :-
	memberAndPos(X,L,M),		       
	N is M+1.

%---------------------------------------------------------------------------
% maximalResolvent(+CLAUSE1,+MLits1,+CLAUSE2,+MLits2,-CLAUSE3)
% CLAUSE3 is a resolvent of CLAUSE1 and CLAUSE2 such that the resolvent 
% literals are maximal instance the respective clauses.
%
%      
%     
% Description: 

maximalResolvent(CL1,ML1,CL2,ML2,cl(E1,E2,char(PT3,[CLEP,CLEN],Ar2))) :-
	arg(1,ML1,MP1),
	arg(2,ML2,MN2),
%	cmp_write('from '),cmp_write(MP1),cmp_write(' and '),cmp_write(MN2),cmp_nl,
	find_maybe_unifiable(MP1,MN2,L1,L2),   
	modPositiv(CL1,P1),
	modPositiv(CL2,P2),
	modNegativ(CL1,N1),
	modNegativ(CL2,N2),
	modProofTree(CL1,PT1),
	modProofTree(CL2,PT2),
	modUnion(PT1,PT2,PT3),
	modReplaceVar(variants(L1,L2,P1,N1,P2,N2),variants(L11,L21,P11,N11,P21,N21)),
	L11 = L21,
%	modUnify(L11,L21),
	modSubtract(P11,[L11],C),
	modSubtract(N21,[L21],D), 
	modUnion(C,P21,E1),
	modUnion(D,N11,E2),
	modExtractCR(E1,CLEP),
	modExtractCR(E2,CLEN),
	modExtractSkolem(E1,E2,Ar2),
	ifOption(traceOutput,yes,(cmp_write('Resolved '), cmp_print(CL1), cmp_nl, cmp_write(' and '),cmp_print(CL2),cmp_nl,cmp_write(' on '), cmp_print(L1), cmp_write(' and '), cmp_print(L2), cmp_nl, cmp_write('getting '), cmp_print(cl(E1,E2,char(PT3,[CLEP,CLEN],Ar2))),cmp_nl)).
%	cmp_write('selected '),cmp_write(L1),cmp_write(' with '),cmp_write(L2),cmp_nl,
%	cmp_write('P'),ttyflush.
maximalResolvent(CL1,ML1,CL2,ML2,cl(E1,E2,char(PT3,[CLEP,CLEN],Ar2))) :-
	arg(2,ML1,MN1),
	arg(1,ML2,MP2),
%	cmp_write('from '),cmp_write(MN1),cmp_write(' and '),cmp_write(MP2),cmp_nl,
	find_maybe_unifiable(MN1,MP2,L1,L2),   
	modPositiv(CL1,P1),
	modPositiv(CL2,P2),
	modNegativ(CL1,N1),
	modNegativ(CL2,N2),
	modProofTree(CL1,PT1),
	modProofTree(CL2,PT2),
	modUnion(PT1,PT2,PT3),
	modReplaceVar(variants(L1,L2,P1,N1,P2,N2),variants(L11,L21,P11,N11,P21,N21)),
	L11 = L21,
%	modUnify(L11,L21),
	modSubtract(P21,[L21],C),
	modSubtract(N11,[L11],D), 
	modUnion(C,P11,E1),
	modUnion(D,N21,E2),
	modExtractCR(E1,CLEP),
	modExtractCR(E2,CLEN),
	modExtractSkolem(E1,E2,Ar2),
	ifOption(traceOutput,yes,(cmp_write('Resolved '), cmp_print(CL1), cmp_nl, cmp_write(' and '),cmp_print(CL2),cmp_nl,cmp_write(' on '), cmp_print(L1), cmp_write(' and '), cmp_print(L2), cmp_nl, cmp_write('getting '), cmp_print(cl(E1,E2,char(PT3,[CLEP,CLEN],Ar2))),cmp_nl)).
%	cmp_write('from '),cmp_write(MN1),cmp_write(' and '),cmp_write(MP2),cmp_nl,
%	cmp_write('selected '),cmp_write(L1),cmp_write(' with '),cmp_write(L2),cmp_nl,
%	cmp_write('N'),ttyflush.


%---------------------------------------------------------------------------
% modMaximalLiteral(+LITERAL,+CLAUSE)
% true iff LITERAL covers all literals instance CLAUSE.
%
%      
%     
% Description: 

modMaximalLiteral(L1,cl(PLL,NLL,_PT)) :-
	modMaximalLiteralList(PLL,L1),
	modMaximalLiteralList(NLL,L1).

modMaximalLiteralList([],_A).
modMaximalLiteralList([H|T],A) :-
	modLiteralCovers(A,H),
	modMaximalLiteralList(T,A).

modLiteralCovers(A,B) :-
	functor(A,_,AA),
	functor(B,_,AB),
	modLiteralCovers(A,B,AA,AB).

modLiteralCovers(_A,_B,_,0) :- !.
modLiteralCovers(A,B,AA,AB) :-
	arg(AB,B,SB),
	modLiteralCoversSB(A,SB,AA),
	AB1 is AB-1,
	modLiteralCovers(A,B,AA,AB1).
	
modLiteralCoversSB(A,SB,AA) :-
	AA >= 1,
	arg(AA,A,SA),
	(do_CoversTermtest(SA,SB) ->
	    true
	;
	    AA1 is AA-1,
	    modLiteralCoversSB(A,SB,AA1)
	).

do_CoversTermtest(T1,T2) :-
	modCovers(T1,T2).
do_CoversTermtest(T1,T2) :-
	modSubterm(T2,T1).


modCovers(X,Y) :-  
	isSlot(X),!,
	X == Y.
modCovers(Clause1,Clause2) :-
%       t = f(t1,...,tn) , s is isSlot and s = ti for some i ,i=1...n.
	isSlot(Clause2),
	!,
	functor(Clause1,_,A),
	var_equal_subterm(A,Clause1,Clause2).
modCovers(Clause1,Clause2) :-
%       t= f(t1,..,tn)  s= g(t1,..tm)  , n>=m>=0
	functor(Clause1,_,A1),
	functor(Clause2,_,A2),
	A1 >= A2,
	var_equal_subterms(A2,Clause1,Clause2),
	!.
modCovers(Clause1,Clause2) :-
%       t=s
	nonvar(Clause2),
	nonvar(Clause1),
	not(not(Clause1 = Clause2)).


var_equal_subterm(A,Term,Var) :-
	A >= 1,
	arg(A,Term,V),
	(Var == V -> 
	    true 
	;
	    A1 is A-1,
	    var_equal_subterm(A1,Term,Var)
	).

var_equal_subterms(0,_Term0,_Term1) :- !.
var_equal_subterms(A,Term0,Term1) :-
	A >= 1,
	arg(A,Term0,V0),
	arg(A,Term1,V1),
	(V0 == V1 -> 
	    true 
	;
	    A1 is A-1,
	    var_equal_subterms(A1,Term0,Term1)
	).
 
modSubterm(A1,A2) :-
        A1 == A2,
        !.
modSubterm(A1,A2) :-
	nonvar(A2),
	functor(A2,_,A),
	modSubterm(A1,A2,A).

modSubterm(A1,A2,A) :-
	A >= 1,
	arg(A,A2,SA2),	
	(modSubterm(A1,SA2) ->
	    true
	;
	    AM1 is A - 1,
	    modSubterm(A1,A2,AM1)
	).


modUnify(X,Y) :-
	unify(X,Y).

unify(X, Y):-unify_with_occurs_check(X,Y).

/*
unify(X, Y) :-
        isSlot(X),isSlot(Y),!,
        X = Y.        
unify(X, Y) :-
        isSlot(X),!,
        occurs_check(Y, X),  
        X = Y.
unify(X, Y) :-
        isSlot(Y),!,
        occurs_check(X, Y),
        X = Y.
unify(X, Y) :-
        atomic(X),!,
        X = Y.
unify(X, Y) :-
        functor(X, F, N),
        functor(Y, F, N),
        unify(N, X, Y).
*/
        
unify(0, _, _) :- !.
unify(N, X, Y) :-
        arg(N, X, Xn),arg(N, Y, Yn),
        unify(Xn, Yn),
        M is N-1,
        !,
        unify(M, X, Y).

occurs_check(Term, Var) :-
        isSlot(Term),!,
        Term \== Var.
occurs_check(Term, Var) :-
        functor(Term, _, Arity),
        occurs_check(Arity, Term, Var).

occurs_check(0, _, _) :- !.
occurs_check(N, Term, Var) :-
        arg(N, Term, Arg),
        occurs_check(Arg, Var),
        M is N-1,
        !,
        occurs_check(M, Term, Var).

maybe_unifiable(T0,_T1) :-
	isSlot(T0),!.
maybe_unifiable(_T0,T1) :-
	isSlot(T1),!.
maybe_unifiable(T0,T1) :-
	functor(T0,F,A),
	functor(T1,F,A),
	maybe_unifiable(T0,T1,A).

maybe_unifiable(_T0,_T1,0) :- !.
maybe_unifiable(T0,T1,A) :- 
	arg(A,T0,ST0),
	arg(A,T1,ST1),
	maybe_unifiable(ST0,ST1),
	A1 is A-1,
	maybe_unifiable(T0,T1,A1).

% finds two literals that may be unifiable one from each list
find_maybe_unifiable([Lit0|_Lits0],Lits1,Lit0,U1) :-
	find_maybe_unifiable(Lits1,Lit0,U1).
find_maybe_unifiable([_Lit0|Lits0],Lits1,U0,U1) :-
	find_maybe_unifiable(Lits0,Lits1,U0,U1).
	
find_maybe_unifiable([Lit1|_Lits1],Lit0,Lit1) :-
	maybe_unifiable(Lit1,Lit0).
find_maybe_unifiable([_|Lits1],Lit0,U1) :-
	find_maybe_unifiable(Lits1,Lit0,U1).

% finds two maximal literals that may be unifiable one from each list
% first collect maximal literals from the second list (that may have a least one unifiable partner)
find_max_maybe_unifiable(Lits0,CL0,Lits1,CL1,U0,U1) :-
	collect_maximals(Lits1,CL1,Lits0,MLits1),
	find_max_maybe_unifiable(Lits0,CL0,MLits1,U0,U1).

collect_maximals([],_,_,[]).
collect_maximals([Lit1|Lits1],CL1,Lits0,MLits2) :-
	((modMaximalLiteral(Lit1,CL1),maybe_unifiablelist(Lits0,Lit1)) ->
	    MLits2 = [Lit1|MLits1]
	;
	    MLits2 = MLits1
	),
	collect_maximals(Lits1,CL1,MLits1).

maybe_unifiablelist([Lit0|Lits0],Lit1) :-
	(maybe_unifiable(Lit0,Lit1) ->
	    true
	;
	    maybe_unifiablelist(Lits0,Lit1)
	).
	

find_max_maybe_unifiable([Lit0|_Lits0],CL0,Lits1,U0,U1) :-
	modMaximalLiteral(Lit0,CL0),
	find_max_maybe_unifiable(Lits1,Lit0,U0,U1).
find_max_maybe_unifiable([_Lit0|Lits0],CL0,Lits1,U0,U1) :-
	find_max_maybe_unifiable(Lits0,CL0,Lits1,U0,U1).
	
find_max_maybe_unifiable([Lit1|_Lits1],Lit0,Lit0,Lit1) :-
	maybe_unifiable(Lit1,Lit0).
find_max_maybe_unifiable([_|Lits1],Lit0,U0,U1) :-
	find_max_maybe_unifiable(Lits1,Lit0,U0,U1).

 
%---------------------------------------------------------------------------
% modReplaceVar(+TERM1,-TERM2)
% TERM2 is a variant of TERM1, i.e. all (Prolog) variables instance T1 are replaced
% by fresh variables.
%
%      
% Description: 

modReplaceVar(T1,T2) :-
	modcopy_term(T1,T2). 
%	modReplaceVar(T1,T2,[],_).

%---------------------------------------------------------------------------
% modReplaceVar(+TERM1,-TERM2,+VARLIST1,-VARLIST2)
% TERM2 is a variant of TERM1, i.e. all (Prolog) variables instance T1 are replaced
% by fresh variables. VARLIST2 contains a list of lists [VAR1,VAR2], where 
% VAR1 is a variable occurring instance TERM1 which is replaced by VAR2 instance TERM2.
% VARLIST1 contains a similar list containing such replacement lists which
% have been computed for other parts of the term.
%
%      
% Description: 

modReplaceVar(T1,T2,VarList1,VarList2) :-
	isSlot(T1),
	!,
	modReplaceVarmember(VarList1,T1,T2,VarList2).
modReplaceVar(T1,T2,VarList1,VarList2) :-
	atomic(T1),
	!,
	T1 = T2,
	VarList1 = VarList2.
% Lists special seectx to pay off
modReplaceVar([E0|R0],[E1|R1],VarList1,VarList3) :-
	!,
	modReplaceVar(E0,E1,VarList1,VarList2),
	modReplaceVarList(R0,R1,VarList2,VarList3).
modReplaceVar(T1,T2,VarList1,VarList2) :-
	functor(T1,F,A),
	functor(T2,F,A),
	modReplaceVar(T1,T2,VarList1,VarList2,A).

modReplaceVarList([],[],VarList,VarList).
modReplaceVarList([E0|R0],[E1|R1],VarList1,VarList3) :-
	modReplaceVar(E0,E1,VarList1,VarList2),
	modReplaceVarList(R0,R1,VarList2,VarList3).

modReplaceVar(_T1,_T2,VarList,VarList,0) :- !.
modReplaceVar(T1,T2,VarList1,VarList3,A) :-
	arg(A,T1,ST1),
	arg(A,T2,ST2),
	modReplaceVar(ST1,ST2,VarList1,VarList2),
	A1 is A-1,
	modReplaceVar(T1,T2,VarList2,VarList3,A1).


%---------------------------------------------------------------------------
% modReplaceVarmember(+VARLIST1,+VAR1,-VAR2,-VARLIST2)
% VAR1 is a (Prolog) variable. If VAR1 is contained instance VARLIST1, i.e.
% there is a list [VAR1,VAR3] instance VARLIST1, then VAR3 will be VAR3 and
% VARLIST2 is equal to VARLIST1. Otherwise, VAR2 is a fresh variable and
% VARLIST2 is VARLIST1 extended by [VAR1,VAR2].
%
%      
% Description: 

% pb
% [A,B] -> A-B (Assoziationsliste)
% 3rd is 1st arg

modReplaceVarmember([],A,B,[A-B]).
modReplaceVarmember([A3-A4|T1],A1,A2,[A3-A4|T2]) :-
	(A1 == A3 ->
	    A2 = A4,
	    T1 = T2
	;
	    modReplaceVarmember(T1,A1,A2,T2)
	).


modNotEqual(Arg1,Arg1) :-
	!,fail.
modNotEqual(_,_).


modUnion(A,[],A) :-
	!.
modUnion([],A,A) :-
	!.
modUnion([],[],[]).
modUnion([X|R],[Y|R1],Z):-
	modUnion1([X|R],[Y|R1],Z),
	!.
modUnion([X|R],Y,Z) :-
	modUnion1([X|R],[Y],Z),
	!.
modUnion([X],Y,Z) :-
	modUnion1([X],[Y],Z),
	!.
modUnion(X,[Y],Z) :-
	modUnion1([X],[Y],Z),
	!.
modUnion(X,[Y|R],Z) :-
	modUnion1([X],[Y|R],Z),
	!.
modUnion(X,Y,Z) :-
	modUnion1([X],[Y],Z),
	!.
modUnion1([], Union, Union).
modUnion1([Element|Elements], Set, Union) :-
	memberVarList(Element, Set),
	!,
	modUnion1(Elements, Set, Union).
modUnion1([Element|Elements], Set, [Element|Union]) :-
	modUnion1(Elements, Set, Union).


modClUnion(false,_,false).
modClUnion(_,false,false).
modClUnion([],Union,Union).
modClUnion([El|Els],Set,Union) :-
	modClmember(El,Set),
	!,
	modClUnion(Els,Set,Union).
modClUnion([El|Els],Set,[El|Union]) :-
	modClUnion(Els,Set,Union).

modCl_list_to_set([], []) :-
	ifOption(traceOutput,yes,(cmp_print('modCl_list_to_set done.\n'))).
modCl_list_to_set([Head|Tail], Set) :-
	ifOption(traceOutput,yes,(length([Head|Tail],N),cmp_print(N),cmp_print(' '))),
	modClmember(Head, Tail),
	!,
	modCl_list_to_set(Tail, Set).
modCl_list_to_set([Head|Tail], Set) :-
	isSlot(Head),
	!,	
	modCl_list_to_set(Tail,Set),
	!.
modCl_list_to_set([Head|Tail], [Head|Set]) :-
	modCl_list_to_set(Tail, Set).
	
modClmember(Er,[H|_T]) :-
	modClEqual(Er,H),
	!.
modClmember(Er,[_|T]) :-
	modClmember(Er,T),
	!.

modClEqual(CL1,CL2) :-
	modPositiv(CL1,P1),
	modPositiv(CL2,P2),
	modNegativ(CL1,N1),
	modNegativ(CL2,N2),
	modEqualset(P1,P2),
	modEqualset(N1,N2),
	!.

modEqualset(A, B) :-
        modSubset(A, B),
        modSubset(B, A), !.

modVarEqualSet(L1,L2) :-
	modVarSubset(L1,L2),
	modVarSubset(L2,L1),
	!.
modVarSubset([],_) :- !.
modVarSubset([H1|T],List) :-
	memberVarList(H1,List),
	modVarSubset(T,List),
	!.

modSubset([], _) :- !.
modSubset([B|C], A) :-
	member(L,A),
        not(not(modVariant(B,L))),
	!,
        modSubset(C, A), 
	!.


modVariant(A,B) :-
	isSlot(A),
	isSlot(B),
	!,
	A = B.
modVariant(A,B) :-
	isSlot(A),
	nonvar(B),
	!,
	fail.
modVariant(A,B) :-
	nonvar(A),
	isSlot(B),
	!,
	fail.
modVariant(A,B) :-
	atomic(A),
	atomic(B),
	!,
	A == B.
modVariant(A,B) :-
	A =.. [F|TLA],
	B =.. [F|TLB],
	!,
	modVariantList(TLA,TLB).
modVariantList([],[]) :-
	!.
modVariantList([A|TLA],[B|TLB]) :-
	modVariant(A,B),
	modVariantList(TLA,TLB).


modnonmember([_,_],[]) :-
	!.
modnonmember([X,Y],[[X1,Y1]|_]) :-
	modClEqual(X,X1),
	modClEqual(Y,Y1),
	!,fail.
modnonmember([X,Y],[[X1,Y1]|_]) :-
	modClEqual(X,Y1),
	modClEqual(Y,X1),
	!,fail.
modnonmember([X,Y],[_|T]) :-
	modnonmember([X,Y],T),
	!.

modMember(X,Y) :-
        nonvar(Y),
        X==Y.
modMember(_X,Y) :-
        isSlot(Y),
        !,fail.
modMember(X, [Y|_]    ) :-
        nonvar(Y),
        X=Y.
modMember(X, [_,Y|_]  ) :-
        nonvar(Y),
        X=Y.
modMember(X, [_,_,Y|_]) :-
        nonvar(Y),
        X==Y.
modMember(X, [_,_,_|L]) :-
        modMember(X, L).


%%% no variables ...
%%%

mod_list_to_set([], []).
mod_list_to_set([Head|Tail], Set) :-
	memberVarList(Head, Tail),
	!,
	mod_list_to_set(Tail, Set).
mod_list_to_set([Head|Tail], Set) :-
	isSlot(Head),
	mod_list_to_set(Tail,Set),
	!.
mod_list_to_set([Head|Tail], [Head|Set]) :-
	mod_list_to_set(Tail, Set).

mod_deletoneList(L1,L2,L3) :-
	deletoneList(L1,L2,L3),
	!.
mod_deletoneList(L1,_,L1) :-
	!.

%%% memberVarList(X,Y)
%%% true if X instance VariableList Y

memberVarList(X,[Y|_]) :-
	X == Y,!.
memberVarList(X, [_|T]    ) :-
	memberVarList(X,T).


%%% modCollect(X,Y) 
%%% Y exists of all Variables instance X

modCollect([],[]) :-
	!.
modCollect([X|R],L) :-
	isSlot(X),
	modCollect(R,L1),
	cmp_union([X],L1,L),
	!.
modCollect([X|R],L) :-
	(atom(X);number(X)),	
	modCollect(R,L),
	!.
modCollect([[X1|R1]|R],L) :-
	isSlot(X1),
	modCollect(R1,L1),
	modCollect(R,L2),
	cmp_union(L1,L2,L3),
	cmp_union(L3,[X1],L),
	!.
modCollect([[X|R1]|R],L) :-
	(atom(X);number(X)),
	modCollect(R1,L1),
	modCollect(R,L3),
	cmp_union(L1,L3,L),
	!.
modCollect([[X1|R1]|R],L) :-
	X1 =.. X2,
	modCollect(X2,L2),
	modCollect(R1,L1),
	modCollect(R,L3),
	cmp_union(L1,L2,L4),
	cmp_union(L4,L3,L),
	!.
modCollect([X|R],L3) :-
	X =.. X1,
	modCollect(X1,L1),
	modCollect(R,L2),
	cmp_union(L1,L2,L3),
	!.
modCollect(X,[X]) :-
	isSlot(X),
	!.
modCollect(X,[]) :-
	(atom(X);number(X)),
	!.
modCollect(X,L1) :-
	X =.. X1,
	modCollect(X1,L1),!.


% modSubtract1(List,Kill,Res)
% deletes all clauses instance List which are instance Kill 
%

modSubtract1([], _, []).
modSubtract1([Element|Elements], Set, Difference) :-
%	ifOption(traceOutput,yes,(length([Element|Elements],N),cmp_write('Trying to cmp_subtract '),cmp_print(Element),cmp_write(' with '),cmp_print(N))),
	modClmember(Element,Set),
        !,
%	ifOption(traceOutput,yes,(cmp_write(' succeeded.\n'))),
        modSubtract1(Elements, Set, Difference).
modSubtract1([Element|Elements], Set, [Element|Difference]) :-
%	ifOption(traceOutput,yes,(cmp_write(' failed.\n'))),
        modSubtract1(Elements, Set, Difference).

% modSubtract(List,Kill,Res)
% deletes all variables instance List which are instance Kill 
%

modSubtract([], _, []).
modSubtract([Element|Elements], Set, Difference) :-
        memberVarList(Element, Set),
        !,
        modSubtract(Elements, Set, Difference).
modSubtract([Element|Elements], Set, [Element|Difference]) :-
        modSubtract(Elements, Set, Difference).

not_subtract(L1,L2,L3) :-
	cmp_subtract(L1,L2,L3),
	!,fail.
not_subtract(_,_,_) :-
	!.


% pb
modcopy_term(T0,T1) :-
	copy_term(T0,T1). % Builtin instance SICStus


isGround_copy(T0,T1) :-
	isGround_copy(T0,T1,[],_,0,_).

isGround_copy(T0,T1,Subst0,Subst1,N0,N1) :-
	isSlot(T0),!,
	do_subst(Subst0,T0,T1,Subst1,N0,N1).
isGround_copy(T0,T0,Subst0,Subst0,N0,N0) :-
	atomic(T0),!.
isGround_copy(T0,T1,Subst0,Subst1,N0,N1) :-
	functor(T0,F,A),
	functor(T1,F,A),
	isGround_copy(A,T0,T1,Subst0,Subst1,N0,N1).

isGround_copy(0,_T0,_T1,Subst,Subst,N,N) :- !.
isGround_copy(A,T0,T1,Subst0,Subst2,N0,N2) :- 
	arg(A,T0,ST0),
	arg(A,T1,ST1),
	isGround_copy(ST0,ST1,Subst0,Subst1,N0,N1),
	A1 is A -1,
	isGround_copy(A1,T0,T1,Subst1,Subst2,N1,N2).

do_subst([],T0,T1,[T0-T1],N0,N1) :-
	genconstant(N0,T1,N1).
do_subst([V-C|Subst0],T0,T1,Subst1,N0,N1) :-
	V == T0,!,
	Subst1 = [V-C|Subst0],
	N0 = N1,
	T1 = C.
do_subst([V-C|Subst0],T0,T1,[V-C|Subst1],N0,N1) :-
	do_subst(Subst0,T0,T1,Subst1,N0,N1).



testtime(N) :-
	timeit(test(N,_,_)).

profileit(N) :-
	compile('~barth/Prolog/SICStus/Tools/profiler'),
	compile('/HG/local/knowRep/sigmahl/src/sigmahl/modProv'),
	test1(N,_,_),
	print_profile.


modTranslatoneDecide([],[]) :-
        !.
modTranslatoneDecide([cl(ListH,ListR,ProofTree)|T],Erg) :-
        append(ListH,ListR,List),
        collect(List,ErList),
        (modMember(equal,ErList) ->
            modTranslatoneDecide(T,Erg);
            (modTranslatoneDecide(T,Cl2),
            append([cl(ListH,ListR,ProofTree)],Cl2,Erg))),
        !.


modKBToFOL(Name,CL) :-
	option(translationMode,flatRelational),
	!,
	modTranslateModalAxioms(Name,CL1),
	modTranslateAxioms(Name,CL2),
	append(CL10,CL2,CL),
	!.
modTranslateModalAxioms(Name,CL) :-
	setofOrNil([CTX,KClass,MOp,Concept],
                   [X1,B1]^clause(modalAxioms(Name,CTX,user,KClass,Concept,MOp,X1),B1),
		   L1),
	modTranslateModalAxiom(L1,CL).

modTranslateModalAxiom([],[]) :-
	!.
modTranslateModalAxiom([[CTX,KClass,MOp,concept(C)]|L1],CL) :-
	!,
	malcToFOL(functional,U,[A],C,F1),
	translateClass(KClass,MOp,C,[A],F2),
	modalContextToFOL(CTX,[],U,forall(A,'=>'(F1,F2)),F3),
	translate(F3,C1),
	clausesToSystemSyntax(C1,CL1),
	modPurCl2Cl(CL1,C,CL10),
	modTranslateModalAxiom(L1,CL2),
	append(CL10,CL2,CL).
modTranslateModalAxiom([[CTX,KClass,MOp,all]|L1],CL) :-
	!,
	translateClass(KClass,MOp,all,[A],F2),
	modalContextToFOL(MC,[],U,forall(A,F2),F3),
	translate(F3,C1),
	clausesToSystemSyntax(C1,CL1),
	modPurCl2Cl(CL1,all,CL10),
	modTranslateModalAxiom(L1,CL2),
	append(CL10,CL2,CL).
modTranslateModalAxiom([[CTX,KClass,MOp,A]|L1],CL) :-
	!,
	translateClass(KClass,MOp,C,[A],F2),
	modalContextToFOL(MC,[],U,forall(A,F2),F3),
	translate(F3,C1),
	clausesToSystemSyntax(C1,CL1),
	modPurCl2Cl(CL1,A,CL10),
	modTranslateModalAxiom(L1,CL2),
	append(CL10,CL2,CL).


modTranslateAxioms(Name,CL) :-
	setofOrNil([CTX,[A],_,instance,C],
	           [X1,Ax,B1]^clause(conceptElement(Name,CTX,X1,user,A,C,Ax),B1),
		   L1),
	modTranslateAxiom(L1,CL1),
	setofOrNil([CTX,[A,B],_,instance,R],
	           [X1,Ax,B1]^clause(roleElement(Name,CTX,X1,user,A,B,R,Ax),B1),
		   L2),
	modTranslateAxiom(L2,CL2),
	setofOrNil([CTX,[X],CT1,'<=>',CT2],
	           [Ax,B1]^clause(conceptEqualSets(Name,user,CTX,CT1,CT2,Ax),B1),
		   L3),
	modTranslateAxiom(L3,CL3),
	setofOrNil([CTX,[X],CT1,'=>',CT2],
	           [Ax,B1]^clause(conceptSubsets(Name,user,CTX,CT1,CT2,Ax),B1),
		   L4),
	modTranslateAxiom(L4,CL4),
	setofOrNil([CTX,[X,Y],RN,'<=>',RT],
	           [Ax,B1]^clause(roleEqualSets(Name,user,CTX,RN,RT,Ax),B1),
		   L5),
	modTranslateAxiom(L5,CL5),
	setofOrNil([CTX,[X,Y],RN,'=>',RT],
	           [Ax,B1]^clause(roleSubSets(Name,user,CTX,RN,RT,Ax),B1),
		   L6),
	modTranslateAxiom(L6,CL6),
%	hl_closed(Name,CTX,X,Y,R),
	append(CL1,CL2,CL12),
	append(CL12,CL3,CL13),
	append(CL13,CL4,CL14),
	append(CL14,CL5,CL15),
	append(CL15,CL6,CL),
	!.


modTranslateAxiom([],[]) :-
	!.
modTranslateAxiom([[A1,A2,A3,A4,A5]|L1],CL) :-
	modAxiomToClause(A1,A2,A3,A4,A5,CL1),
	modTranslateAxiom(L1,CL2),
	append(CL1,CL2,CL).

modAxiomToClause(MC,VL,T1,Op,T2,CL) :-
	modExpand(VL,T1,TN1,TL1),
	modExpand(VL,T2,TN2,TL2),
 	axiomToFOL(MC,VL,TN1,Op,TN2,F),
	modAxiomsToClause(MC,TL1,CL1),
	modAxiomsToClause(MC,TL2,CL2),
	forallQuantify(VL,F,F1),
	translate(F1,C1),
	clausesToSystemSyntax(C1,C),
	modPurCl2Cl(C,T1,Cl),
	modPurCl2Cl(CL1,T1,CL10),
	modPurCl2Cl(CL2,T1,CL20),
	append(CL10,CL20,CL3),
	append(CL3,Cl,CL).

modPurCl2Cl([],VL,[]) :- !.
modPurCl2Cl(cl(Pos,Neg),VL,[cl(Pos,Neg,char(VL1,[CRP,CRN],Skol))]) :-
	((isSlot(VL);VL = [_|_]) ->
	    VL1 = VL;
	    VL1 = [VL]),
	modExtractCR(Pos,CRP),
	modExtractCR(Neg,CRN),
	modExtractSkolem(Pos,Neg,Skol),
	!.
modPurCl2Cl([PCl|TCl],VL,Cl) :-
	modPurCl2Cl(PCl,VL,C1),
	modPurCl2Cl(TCl,VL,C2),
	append(C1,C2,Cl),
	!.
modPurCl2Cl([_PCl|TCl],VL,C) :-
	modPurCl2Cl(TCl,VL,C),
	!.
	
modAxiomsToClause(MC,[],[]) :-
	!.
modAxiomsToClause(MC,[T|TL],CL3) :-
	T = (VL,T1,Op,T2),
	axiomToFOL(MC,VL,T1,Op,T2,F),
	forallQuantify(VL,F,F1),
	translate(F1,C1),
	clausesToSystemSyntax(C1,CL1),
	modAxiomsToClause(MC,TL,CL2),
	append(CL1,CL2,CL3).

modExpand(_,V,V,[]) :-
	isSlot(V),
	!.
modExpand(_,V,V,[]) :-
	atom(V),
	!.
modExpand(VL,and([C1]),and([F1]),TL) :-
	modExpand(VL,C1,F1,TL).
modExpand(VL,and([C1|CL]), and([F1|F2]),TL) :-
	modExpand(VL,C1,F1,TL1),
	modExpand([X],and(CL),and(F2),TL2),
	append(TL1,TL2,TL).
modExpand(VL,or([C1]),or([F1]),TL) :-
	modExpand(VL,C1,F1,TL).
modExpand(VL,or([C1|CL]), or([F1|F2]),TL) :-
	modExpand(VL,C1,F1,TL1),
	modExpand([X],or(CL),or(F2),TL2),
	append(TL1,TL2,TL).
modExpand([X],set(L),F) :-
	!,
	setDisjunction(X,L,F).
modExpand(VL,not(C),not(F),TL) :-
	modExpand(VL,C,F,TL),
	!.
modExpand(VL,naf(C),naf(F),TL) :-
	modExpand(VL,C,F,TL),
	!.
modExpand([X],all(R,C),C1,[([X],C1,'<=>',all(R,D))|TL]) :-
	gensym(concept,C1),
	modExpand([X],C,D,TL).
modExpand([X],some(R,C),C1,[([X],C1,'<=>',some(R,D))|TL]) :-
	gensym(concept,C1),
	modExpand([X],C,D,TL).
modExpand([X],atleast(N,R),atleast(N,R),[]) :-
	!.
modExpand([X],atmost(N,R),atmost(N,R),[]) :-
	!.
modExpand([X],b(O,A,C2),
           forall(V,'=>'(hl_holds(O,A,U,V),F))) :-
	malcToFOL(Trans,V,[X],C2,F).    
modExpand([X],d(O,A,C2),undefined,[]) :-
	!.
modExpand([X],d(O,A,C2),undefined,[]) :-
	!.
modExpand([X],d(O,A,C2),undefined,[]) :-
	!.
modExpand([X],bc(O,C1,C2),undefined,[]) :-
	!.
modExpand([X],dc(O,C1,C2),undefined,[]) :-
	!.
modExpand([X],dc(O,C1,C2),undefined,[]) :-
	!.
modExpand([X],dc(O,C1,C2),undefined,[]) :- 
	!.
modExpand([X,Y],inverse(R),inverse(S),TL) :-
	modExpand([Y,X],R,S,TL),
	!.
modExpand([X,Y],restrict(R,C),restrict(S,D),TL) :-
	modExpand([X,Y],R,S,TL1),
	modExpand([Y],C,D,TL2),
	append(TL1,TL2,TL),
	!.

      

test1(K,Bool,CL) :-
        example(K),
	!,
        knowledgeBase(_,KB,_),
        KB \== kb(e0),
	setOption(prover,decider),
	setOption(translationMode,flatRelational),	
	setOption(decider,on),
        modKBToFOL(KB,K2),
        getRuntime(T0),
        length(K2,N0),
	modInference(K2,Bool,CL,stat(N1,N2)),
	length(CL,N3),
	getRuntime(T1),
        T is T1 - T0,
        cmp_format("Total runtime:                         ~t~3d~45| sec.~n",T),
        cmp_format("Number of clauses    at the beginning: ~t~d~41|~n",N0),
        cmp_format("Number of clauses    at the end:       ~t~d~41|~n",N1),
        cmp_format("Number of SH clauses at the end:       ~t~d~41|~n",N2),
        !.
 
test(K,Bool,CL) :-
        getRuntime(T0),
	setOption(prover,decider),
	setOption(translationMode,flatRelational),
	setOption(decider,on),
	clauseSet(K,K2),
        length(K2,N0),
	modInference(K2,Bool,CL,stat(N1,N2)),
	length(CL,N3),
	getRuntime(T1),
        T is T1 - T0,
        cmp_format("Total runtime:                      ~t~3d~45| sec.~n",T),
        cmp_format("Number of clauses at the beginning: ~t~d~41|~n",N0),
        cmp_format("Number of persistent clauses:       ~t~d~41|~n",N1),
        cmp_format("Number of persistent horn clauses:  ~t~d~41|~n",N2),
        cmp_format("Number of clauses at the end     : ~t~d~41|~n",N3),
	!.

% Clause set 83
% generated from example 83
% Total runtime 0.620 sec. on a SPARC 10/512
clauseSet(83,
[cl([],[instance([],concept52,_9400),instance([],sex,_9400,_9841),instance([],concept49,_9841)],c01),
cl([instance([],sex,_9400,f16(_9400)),instance([],concept52,_9400)],[],c02),
cl([instance([],concept49,f16(_9400)),instance([],concept52,_9400)],[],c03),
cl([],[instance([],concept52,_9400),instance([],person,_9400)],c04),
cl([instance([],sex,_9433,f17(_9433))],[instance([],concept53,_9433)],c05),
cl([instance([],male,f17(_9433)),instance([],female,f17(_9433))],[instance([],concept53,_9433)],c06),
cl([instance([],concept53,_9433)],[instance([],male,_12722),instance([],sex,_9433,_12722)],c07),
cl([instance([],concept53,_9433)],[instance([],female,_12722),instance([],sex,_9433,_12722)],c08),
cl([instance([],concept53,_9433)],[instance([],person,_9433)],c09),
cl([],[instance([],male,_9468),instance([],female,_9468)],c10),
cl([],[instance([],female,_9496),instance([],male,_9496)],c11),
cl([instance([],'Entity',_9524)],[instance([],male,_9524)],c12)]).
% Clause set 84
% generated from example 84
% 14.03.95: Total runtime 1.067 sec.
%           Total runtime 1.290 sec on a SPARC 10/512
% Number of clauses at the beginning:     9
% Number of persistent clauses:           9
% Number of persistent horn clauses:      3
% Number of clauses at the end     :      3
clauseSet(84,
[cl([instance([],child,_9408,f6(_9408))],[instance([],concept21,_9408)],c01),
cl([instance([],person,f6(_9408))],[instance([],concept21,_9408)],c02),
cl([instance([],concept21,_9408)],[instance([],child,_9408,_9701),instance([],person,_9701)],c03),
cl([instance([],person,_9371)],[instance([],parent,_9371)],c04),
cl([instance([],concept21,_9371)],[instance([],parent,_9371)],c05),
cl([instance([],parent,_9371)],[instance([],person,_9371),instance([],concept21,_9371)],c0),
cl([],[instance([],concept22,_13682),instance([],sex,_13682,_14123),instance([],concept15,_14123)],c07),
cl([instance([],sex,_13682,f7(_13682)),instance([],concept22,_13682)],[],c08),
cl([instance([],concept15,f7(_13682)),instance([],concept22,_13682)],[],c09),
cl([],[instance([],concept22,_13682),instance([],person,_13682)],c10),
cl([instance([],sex,_13715,f8(_13715))],[instance([],concept23,_13715)],c11),
cl([instance([],male,f8(_13715)),instance([],female,f8(_13715))],[instance([],concept23,_13715)],c12),
cl([instance([],concept23,_13715)],[instance([],male,_132779),instance([],sex,_13715,_132779)],c13),
cl([instance([],concept23,_13715)],[instance([],female,_132779),instance([],sex,_13715,_132779)],c14),
cl([instance([],concept23,_13715)],[instance([],person,_13715)],c15),
cl([],[instance([],male,_13750),instance([],female,_13750)],c16),
cl([],[instance([],female,_13778),instance([],male,_13778)],c17),
cl([instance([],'Entity',_13806)],[instance([],male,_13806)],c18)]).
% Clause set 85
% generated from example 85
% 14.03.95: Total runtime 10.450 sec.
%           Total runtime 21.090 sec. on a SPARC 10/512 
% Number of clauses at the beginning:    18
% Number of persistent clauses:          31
% Number of persistent horn clauses:     10
% Number of clauses at the end     :     10
clauseSet(85,
[
cl([instance([],parent,_36965)],[instance([],father,_36965)],c01),
cl([],[instance([],mother,_36965),instance([],father,_36965)],c02),
cl([instance([],father,_36965),instance([],mother,_36965)],[instance([],parent,_36965)],c03),
cl([instance([],sex,_37012,f1(_37012))],[instance([],concept11,_37012)],c04),
cl([instance([],female,f1(_37012))],[instance([],concept11,_37012)],c05),
cl([instance([],concept11,_37012)],[instance([],sex,_37012,_37029),instance([],female,_37029)],c06),
cl([instance([],parent,_36966)],[instance([],mother,_36966)],c07),
cl([instance([],concept11,_36966)],[instance([],mother,_36966)],c08),
cl([instance([],mother,_36966)],[instance([],parent,_36966),instance([],concept11,_36966)],c09),
cl([instance([],child,_37114,f2(_37114))],[instance([],concept12,_37114)],c10),
cl([instance([],person,f2(_37114))],[instance([],concept12,_37114)],c11),
cl([instance([],concept12,_37114)],[instance([],child,_37114,_37131),instance([],person,_37131)],c12),
cl([instance([],person,_36967)],[instance([],parent,_36967)],c13),
cl([instance([],concept12,_36967)],[instance([],parent,_36967)],c14),
cl([instance([],parent,_36967)],[instance([],person,_36967),instance([],concept12,_36967)],c15),
cl([],[instance([],concept13,_37221),instance([],sex,_37221,_37238),instance([],concept1,_37238)],c16),
cl([instance([],sex,_37221,f3(_37221)),instance([],concept13,_37221)],[],c17),
cl([instance([],concept1,f3(_37221)),instance([],concept13,_37221)],[],c18),
cl([],[instance([],concept13,_37221),instance([],person,_37221)],c19),
cl([instance([],sex,_37222,f4(_37222))],[instance([],concept14,_37222)],c20),
cl([instance([],male,f4(_37222)),instance([],female,f4(_37222))],[instance([],concept14,_37222)],c21),
cl([instance([],concept14,_37222)],[instance([],male,_37313),instance([],sex,_37222,_37313)],c22),
cl([instance([],concept14,_37222)],[instance([],female,_37313),instance([],sex,_37222,_37313)],c23),
cl([instance([],concept14,_37222)],[instance([],person,_37222)],c24),
cl([],[instance([],male,_37223),instance([],female,_37223)],c25),
cl([],[instance([],female,_37224),instance([],male,_37224)],c26),
cl([instance([],'Entity',_37225)],[instance([],male,_37225)],c27)]).
% Clause set 86
% generated from example 86
% 14.03.95: Total runtime   302.114 sec.
% 28.02.95: Total runtime  1867.817 sec. on a SPARC 10/512
% 01.01.95: Total runtime 15330.417 sec.
% Number of clauses at the beginning:    42
% Number of persistent clauses:         171
% Number of persistent horn clauses:     28
clauseSet(86,
[cl([instance([],concept43,_10062)],[instance([],concept42,_9576),instance([],child,_9576,_10062)],c01),
cl([instance([],child,_9576,f9(_9576)),instance([],concept42,_9576)],[],c02),
cl([instance([],concept42,_9576)],[instance([],concept43,f9(_9576))],c03),
cl([instance([],sex,_9576,f10(_9576))],[instance([],concept43,_9576)],c04),
cl([instance([],male,f10(_9576))],[instance([],concept43,_9576)],c05),
cl([instance([],concept43,_9576)],[instance([],sex,_9576,_12299),instance([],male,_12299)],c06),
cl([instance([],parent,_9371)],[instance([],parent_with_sons_only,_9371)],c07),
cl([instance([],concept42,_9371)],[instance([],parent_with_sons_only,_9371)],c08),
cl([instance([],parent_with_sons_only,_9371)],[instance([],parent,_9371),instance([],concept42,_9371)],c09),
cl([instance([],child,_132002,f11(_132002))],[instance([],concept44,_132002)],c10),
cl([instance([],parent,f11(_132002))],[instance([],concept44,_132002)],c11),
cl([instance([],concept44,_132002)],[instance([],child,_132002,_132295),instance([],parent,_132295)],c12),
cl([instance([],parent,_9409)],[instance([],grandparent,_9409)],c13),
cl([instance([],concept44,_9409)],[instance([],grandparent,_9409)],c14),
cl([instance([],grandparent,_9409)],[instance([],parent,_9409),instance([],concept44,_9409)],c15),
cl([instance([],parent,_9444)],[instance([],father,_9444)],c16),
cl([],[instance([],mother,_9444),instance([],father,_9444)],c17),
cl([instance([],father,_9444),instance([],mother,_9444)],[instance([],parent,_9444)],c18),
cl([instance([],sex,_137853,f12(_137853))],[instance([],concept45,_137853)],c19),
cl([instance([],female,f12(_137853))],[instance([],concept45,_137853)],c20),
cl([instance([],concept45,_137853)],[instance([],sex,_137853,_138146),instance([],female,_138146)],c21),
cl([instance([],parent,_9478)],[instance([],mother,_9478)],c22),
cl([instance([],concept45,_9478)],[instance([],mother,_9478)],c23),
cl([instance([],mother,_9478)],[instance([],parent,_9478),instance([],concept45,_9478)],c24),
cl([instance([],child,_142032,f13(_142032))],[instance([],concept46,_142032)],c25),
cl([instance([],person,f13(_142032))],[instance([],concept46,_142032)],c26),
cl([instance([],concept46,_142032)],[instance([],child,_142032,_142325),instance([],person,_142325)],c27),
cl([instance([],person,_9513)],[instance([],parent,_9513)],c28),
cl([instance([],concept46,_9513)],[instance([],parent,_9513)],c29),
cl([instance([],parent,_9513)],[instance([],person,_9513),instance([],concept46,_9513)],c30),
cl([],[instance([],concept47,_146356),instance([],sex,_146356,_164367),instance([],concept24,_164367)],c31),
cl([instance([],sex,_146356,f14(_146356)),instance([],concept47,_146356)],[],c32),
cl([instance([],concept24,f14(_146356)),instance([],concept47,_146356)],[],c3),
cl([],[instance([],concept47,_146356),instance([],person,_146356)],c34),
cl([instance([],sex,_146389,f15(_146389))],[instance([],concept48,_146389)],c35),
cl([instance([],male,f15(_146389)),instance([],female,f15(_146389))],[instance([],concept48,_146389)],c36),
cl([instance([],concept48,_146389)],[instance([],male,_167248),instance([],sex,_146389,_167248)],c37),
cl([instance([],concept48,_146389)],[instance([],female,_167248),instance([],sex,_146389,_167248)],c38),
cl([instance([],concept48,_146389)],[instance([],person,_146389)],c39),
cl([],[instance([],male,_146424),instance([],female,_146424)],c40),
cl([],[instance([],female,_146452),instance([],male,_146452)],c41),
cl([instance([],'Entity',_146480)],[instance([],male,_146480)],c42)]).
% Clause set 87
% generated from example 87
clauseSet(87,
[cl([instance([],concept43,_10062)],[instance([],concept42,_9576),instance([],child,_9576,_10062)],c01),
cl([instance([],child,_9576,f9(_9576)),instance([],concept42,_9576)],[],c02),
cl([instance([],concept42,_9576)],[instance([],concept43,f9(_9576))],c03),
cl([instance([],sex,_9576,f10(_9576))],[instance([],concept43,_9576)],c04),
cl([instance([],male,f10(_9576))],[instance([],concept43,_9576)],c05),
cl([instance([],concept43,_9576)],[instance([],sex,_9576,_12299),instance([],male,_12299)],c06),
cl([instance([],parent,_9371)],[instance([],parent_with_sons_only,_9371)],c07),
cl([instance([],concept42,_9371)],[instance([],parent_with_sons_only,_9371)],c08),
cl([instance([],parent_with_sons_only,_9371)],[instance([],parent,_9371),instance([],concept42,_9371)],c09),
cl([instance([],child,_132002,f11(_132002))],[instance([],concept44,_132002)],c10),
cl([instance([],parent,f11(_132002))],[instance([],concept44,_132002)],c11),
cl([instance([],concept44,_132002)],[instance([],child,_132002,_132295),instance([],parent,_132295)],c12),
cl([instance([],parent,_9409)],[instance([],grandparent,_9409)],c13),
cl([instance([],concept44,_9409)],[instance([],grandparent,_9409)],c14),
cl([instance([],grandparent,_9409)],[instance([],parent,_9409),instance([],concept44,_9409)],c15),
cl([instance([],parent,_9444)],[instance([],father,_9444)],c16),
cl([],[instance([],mother,_9444),instance([],father,_9444)],c17),
cl([instance([],father,_9444),instance([],mother,_9444)],[instance([],parent,_9444)],c18),
cl([instance([],sex,_137853,f12(_137853))],[instance([],concept45,_137853)],c19),
cl([instance([],female,f12(_137853))],[instance([],concept45,_137853)],c20),
cl([instance([],concept45,_137853)],[instance([],sex,_137853,_138146),instance([],female,_138146)],c21),
cl([instance([],parent,_9478)],[instance([],mother,_9478)],c22),
cl([instance([],concept45,_9478)],[instance([],mother,_9478)],c23),
cl([instance([],mother,_9478)],[instance([],parent,_9478),instance([],concept45,_9478)],c24),
cl([instance([],child,_142032,f13(_142032))],[instance([],concept46,_142032)],c25),
cl([instance([],person,f13(_142032))],[instance([],concept46,_142032)],c26),
cl([instance([],concept46,_142032)],[instance([],child,_142032,_142325),instance([],person,_142325)],c27),
cl([instance([],person,_9513)],[instance([],parent,_9513)],c28),
cl([instance([],concept46,_9513)],[instance([],parent,_9513)],c29),
cl([instance([],parent,_9513)],[instance([],person,_9513),instance([],concept46,_9513)],c30),
cl([],[instance([],concept47,_146356),instance([],sex,_146356,_164367),instance([],concept24,_164367)],c31),
cl([instance([],sex,_146356,f14(_146356)),instance([],concept47,_146356)],[],c32),
cl([instance([],concept24,f14(_146356)),instance([],concept47,_146356)],[],c3),
cl([],[instance([],concept47,_146356),instance([],person,_146356)],c34),
cl([instance([],sex,_146389,f15(_146389))],[instance([],concept48,_146389)],c35),
cl([instance([],male,f15(_146389)),instance([],female,f15(_146389))],[instance([],concept48,_146389)],c36),
cl([instance([],concept48,_146389)],[instance([],male,_167248),instance([],sex,_146389,_167248)],c37),
cl([instance([],concept48,_146389)],[instance([],female,_167248),instance([],sex,_146389,_167248)],c38),
cl([instance([],concept48,_146389)],[instance([],person,_146389)],c39),
cl([],[instance([],male,_146424),instance([],female,_146424)],c40),
cl([],[instance([],female,_146452),instance([],male,_146452)],c41),
cl([instance([],'Entity',_146480)],[instance([],male,_146480)],c42),
cl([instance([],father,tom)],[],c43),
cl([instance([],child,tom,peter)],[],c44),
cl([instance([],child,tom,harry)],[],c45),
cl([instance([],child,mary,tom)],[],c46),
cl([instance([],child,mary,chris)],[],c47),
cl([instance([],parent_with_sons_only,mary)],[],c48)]).
% Clause set 88
% generated from example 88
% 14.03.95: Total runtime  943.049 sec.
% 28.02.95: Total runtime 1966.120 sec. on a SPARC 10/512
clauseSet(88,
[cl([instance([],male,_B)],[instance([],concept16,_A),instance([],child,_A,_B)],c1),cl([instance([],child,_A,f1(_A)),instance([],concept16,_A)],[],c01),
cl([instance([],concept16,_A)],[instance([],male,f1(_A))],c02),
cl([instance([],father,_C)],[instance([],father_with_sons_only,_C)],c03),
cl([instance([],concept16,_C)],[instance([],father_with_sons_only,_C)],c04),
cl([instance([],father_with_sons_only,_C)],[instance([],father,_C),instance([],concept16,_C)],c05),
cl([instance([],child,_D,f2(_D))],[instance([],concept17,_D)],c06),
cl([instance([],parent,f2(_D))],[instance([],concept17,_D)],c07),
cl([instance([],concept17,_D)],[instance([],child,_D,_E),instance([],parent,_E)],c08),
cl([instance([],parent,_F)],[instance([],grandparent,_F)],c09),
cl([instance([],concept17,_F)],[instance([],grandparent,_F)],c10),
cl([instance([],grandparent,_F)],[instance([],parent,_F),instance([],concept17,_F)],c11),
cl([instance([],mother,_G),instance([],father,_G)],[instance([],parent,_G)],c12),
cl([instance([],parent,_G)],[instance([],mother,_G)],c13),
cl([instance([],parent,_G)],[instance([],father,_G)],c14),
cl([instance([],hild,_H,f3(_H))],[instance([],concept18,_H)],c15),
cl([instance([],human,f3(_H))],[instance([],concept18,_H)],c16),
cl([instance([],concept18,_H)],[instance([],hild,_H,_I),instance([],human,_I)],c17),
cl([instance([],man,_J)],[instance([],father,_J)],c18),
cl([instance([],concept18,_J)],[instance([],father,_J)],c19),
cl([instance([],father,_J)],[instance([],man,_J),instance([],concept18,_J)],c20),
cl([instance([],child,_K,f4(_K))],[instance([],concept19,_K)],c21),
cl([instance([],human,f4(_K))],[instance([],concept19,_K)],c22),
cl([instance([],concept19,_K)],[instance([],child,_K,_L),instance([],human,_L)],c23),
cl([instance([],woman,_M)],[instance([],mother,_M)],c24),
cl([instance([],concept19,_M)],[instance([],mother,_M)],c25),
cl([instance([],mother,_M)],[instance([],woman,_M),instance([],concept19,_M)],c26),
cl([instance([],human,_N)],[instance([],woman,_N)],c27),
cl([instance([],female,_N)],[instance([],woman,_N)],c28),
cl([instance([],woman,_N)],[instance([],human,_N),instance([],female,_N)],c29),
cl([instance([],human,_O)],[instance([],man,_O)],c30),
cl([instance([],male,_O)],[instance([],man,_O)],c31),
cl([instance([],man,_O)],[instance([],human,_O),instance([],male,_O)],c32),
cl([],[instance([],male,_P),instance([],female,_P)],c33),
cl([],[instance([],female,_Q),instance([],male,_Q)],c34)]).
% Clause set fssTBox
% generated from the fss knowledge base
clauseSet(fssTBox,
[cl([instance([],touchable_object,_A)],[instance([],vehicle,_A)],c001),
cl([instance([],worth_mod,_A,f8(_A))],[instance([],vehicle,_A)],c002),
cl([instance([],worth,f8(_A))],[instance([],vehicle,_A)],c003),
cl([instance([],vehicle,_A)],[instance([],touchable_object,_A),instance([],worth_mod,_A,_B),instance([],worth,_B)],c004),
cl([instance([],town,_C)],[instance([],voelklingen,_C)],c005),
cl([instance([],lexicon,_C)],[instance([],voelklingen,_C)],c006),
cl([instance([],voelklingen,_C)],[instance([],town,_C),instance([],lexicon,_C)],c007),
cl([instance([],animate,_D)],[instance([],animal,_D)],c008),
cl([instance([],volition,_D,f9(_D))],[instance([],animal,_D)],c009),
cl([instance([],volitional_sq,f9(_D))],[instance([],animal,_D)],c010),
cl([instance([],animal,_D)],[instance([],animate,_D),instance([],volition,_D,_E),instance([],volitional_sq,_E)],c011),
cl([instance([],lexicon,_F)],[instance([],peter,_F)],c012),
cl([instance([],human,_F)],[instance([],peter,_F)],c013),
cl([instance([],peter,_F)],[instance([],lexicon,_F),instance([],human,_F)],c014),
cl([instance([],lexicon,_G)],[instance([],ich,_G)],c015),
cl([instance([],human,_G)],[instance([],ich,_G)],c016),
cl([instance([],ich,_G)],[instance([],lexicon,_G),instance([],human,_G)],c017),
cl([instance([],lexicon,_H)],[instance([],karl,_H)],c018),
cl([instance([],human,_H)],[instance([],karl,_H)],c019),
cl([instance([],karl,_H)],[instance([],lexicon,_H),instance([],human,_H)],c020),
cl([instance([],human,_I)],[instance([],junge,_I)],c021),
cl([instance([],lexicon,_I)],[instance([],junge,_I)],c022),
cl([instance([],junge,_I)],[instance([],human,_I),instance([],lexicon,_I)],c023),
cl([instance([],human,_J)],[instance([],mann,_J)],c024),
cl([instance([],lexicon,_J)],[instance([],mann,_J)],c025),
cl([instance([],mann,_J)],[instance([],human,_J),instance([],lexicon,_J)],c026),
cl([instance([],individual,_K)],[instance([],animate,_K)],c027),
cl([instance([],physis_mod,_K,f10(_K))],[instance([],animate,_K)],c028),
cl([instance([],physical_sq,f10(_K))],[instance([],animate,_K)],c029),
cl([instance([],animate,_K)],[instance([],individual,_K),instance([],physis_mod,_K,_L),instance([],physical_sq,_L)],c030),
cl([instance([],thing,_M)],[instance([],das,_M)],c031),
cl([instance([],lexicon,_M)],[instance([],das,_M)],c032),
cl([instance([],das,_M)],[instance([],thing,_M),instance([],lexicon,_M)],c033),
cl([instance([],thing,_N)],[instance([],fahrt,_N)],c034),
cl([instance([],lexicon,_N)],[instance([],fahrt,_N)],c035),
cl([instance([],fahrt,_N)],[instance([],thing,_N),instance([],lexicon,_N)],c036),
cl([instance([],indication_of_quantity,_O)],[instance([],dm,_O)],c037),
cl([instance([],lexicon,_O)],[instance([],dm,_O)],c038),
cl([instance([],dm,_O)],[instance([],indication_of_quantity,_O),instance([],lexicon,_O)],c039),
cl([instance([],abstract_thing,_P)],[instance([],indication_of_quantity,_P)],c040),
cl([instance([],quantity,_P,f11(_P))],[instance([],indication_of_quantity,_P)],c041),
cl([instance([],cardinal,f11(_P))],[instance([],indication_of_quantity,_P)],c042),
cl([instance([],indication_of_quantity,_P)],[instance([],abstract_thing,_P),instance([],quantity,_P,_Q),instance([],cardinal,_Q)],c043),
cl([instance([],cost,_R)],[instance([],kost,_R)],c044),
cl([instance([],lexicon,_R)],[instance([],kost,_R)],c045),
cl([instance([],kost,_R)],[instance([],cost,_R),instance([],lexicon,_R)],c046),
cl([instance([],property,_S)],[instance([],value_property,_S)],c047),
cl([instance([],measure,_S,f12(_S))],[instance([],value_property,_S)],c048),
cl([instance([],abstract_thing,f12(_S))],[instance([],value_property,_S)],c049),
cl([instance([],value_property,_S)],[instance([],property,_S),instance([],measure,_S,_T),instance([],abstract_thing,_T)],c050),
cl([instance([],predicate,_U)],[instance([],unterricht,_U)],c051),
cl([instance([],lexicon,_U)],[instance([],unterricht,_U)],c052),
cl([instance([],unterricht,_U)],[instance([],predicate,_U),instance([],lexicon,_U)],c053),
cl([instance([],lexicon,_V)],[instance([],kauf,_V)],c054),
cl([instance([],transaction,_V)],[instance([],kauf,_V)],c055),
cl([instance([],concerned,_V,f13(_V))],[instance([],kauf,_V)],c056),
cl([instance([],thing,f13(_V))],[instance([],kauf,_V)],c057),
cl([instance([],kauf,_V)],[instance([],lexicon,_V),instance([],transaction,_V),instance([],concerned,_V,_W),instance([],thing,_W)],c058),
cl([instance([],lexicon,_X)],[instance([],schenk,_X)],c059),
cl([instance([],transaction,_X)],[instance([],schenk,_X)],c060),
cl([instance([],concerned,_X,f14(_X))],[instance([],schenk,_X)],c061),
cl([instance([],thing,f14(_X))],[instance([],schenk,_X)],c062),
cl([instance([],schenk,_X)],[instance([],lexicon,_X),instance([],transaction,_X),instance([],concerned,_X,_Y),instance([],thing,_Y)],c063),
cl([instance([],action,_Z)],[instance([],transaction,_Z)],c064),
cl([instance([],beneficative,_Z,f15(_Z))],[instance([],transaction,_Z)],c065),
cl([instance([],human,f15(_Z))],[instance([],transaction,_Z)],c066),
cl([instance([],transaction,_Z)],[instance([],action,_Z),instance([],beneficative,_Z,_A1),instance([],human,_A1)],c067),
cl([instance([],lexicon,_B1)],[instance([],arbeit,_B1)],c068),
cl([instance([],action,_B1)],[instance([],arbeit,_B1)],c069),
cl([instance([],arbeit,_B1)],[instance([],lexicon,_B1),instance([],action,_B1)],c070),
cl([instance([],cmp_write,_C1)],[instance([],enter,_C1)],c071),
cl([instance([],location_enter,_C1,f16(_C1))],[instance([],enter,_C1)],c072),
cl([instance([],canvas,f16(_C1))],[instance([],enter,_C1)],c073),
cl([instance([],enter,_C1)],[instance([],cmp_write,_C1),instance([],location_enter,_C1,_D1),instance([],canvas,_D1)],c074),
cl([instance([],productive,_E1)],[instance([],cmp_write,_E1)],c075),
cl([instance([],concerned,_E1,f17(_E1))],[instance([],cmp_write,_E1)],c076),
cl([instance([],thing,f17(_E1))],[instance([],cmp_write,_E1)],c077),
cl([instance([],cmp_write,_E1)],[instance([],productive,_E1),instance([],concerned,_E1,_F1),instance([],thing,_F1)],c078),
cl([instance([],causative,_G1)],[instance([],productive,_G1)],c079),
cl([instance([],result,_G1,f18(_G1))],[instance([],productive,_G1)],c080),
cl([instance([],thing,f18(_G1))],[instance([],productive,_G1)],c081),
cl([instance([],productive,_G1)],[instance([],causative,_G1),instance([],result,_G1,_H1),instance([],thing,_H1)],c082),
cl([instance([],action,_I1)],[instance([],causative,_I1)],c083),
cl([instance([],causative,_I1)],[instance([],action,_I1)],c084),
cl([instance([],lexicon,_J1)],[instance([],werf,_J1)],c085),
cl([instance([],action,_J1)],[instance([],werf,_J1)],c086),
cl([instance([],concerned,_J1,f19(_J1))],[instance([],werf,_J1)],c087),
cl([instance([],'Entity',f19(_J1))],[instance([],werf,_J1)],c088),
cl([instance([],werf,_J1)],[instance([],lexicon,_J1),instance([],action,_J1),instance([],concerned,_J1,_K1),instance([],'Entity',_K1)],c089),
cl([instance([],lexicon,_L1)],[instance([],treff,_L1)],c090),
cl([instance([],action,_L1)],[instance([],treff,_L1)],c091),
cl([instance([],treff,_L1)],[instance([],lexicon,_L1),instance([],action,_L1)],c092),
cl([instance([],lexicon,_M1)],[instance([],geh,_M1)],c093),
cl([instance([],motion,_M1)],[instance([],geh,_M1)],c094),
cl([instance([],geh,_M1)],[instance([],lexicon,_M1),instance([],motion,_M1)],c095),
cl([instance([],lexicon,_N1)],[instance([],fahr,_N1)],c096),
cl([instance([],motion_by_means,_N1)],[instance([],fahr,_N1)],c097),
cl([instance([],fahr,_N1)],[instance([],lexicon,_N1),instance([],motion_by_means,_N1)],c098),
cl([instance([],motion,_O1)],[instance([],motion_by_means,_O1)],c099),
cl([instance([],means,_O1,f20(_O1))],[instance([],motion_by_means,_O1)],c100),
cl([instance([],touchable_object,f20(_O1))],[instance([],motion_by_means,_O1)],c101),
cl([instance([],motion_by_means,_O1)],[instance([],motion,_O1),instance([],means,_O1,_P1),instance([],touchable_object,_P1)],c102),
cl([instance([],action,_Q1)],[instance([],motion,_Q1)],c103),
cl([instance([],destination,_Q1,f21(_Q1))],[instance([],motion,_Q1)],c104),
cl([instance([],geographical_object,f21(_Q1))],[instance([],motion,_Q1)],c105),
cl([instance([],source,_Q1,f22(_Q1))],[instance([],motion,_Q1)],c106),
cl([instance([],geographical_object,f22(_Q1))],[instance([],motion,_Q1)],c107),
cl([instance([],motion,_Q1)],[instance([],action,_Q1),instance([],destination,_Q1,_R1),instance([],geographical_object,_R1),instance([],source,_Q1,_S1),instance([],geographical_object,_S1)],c108),
cl([instance([],individual,_T1)],[instance([],inanimate,_T1)],c109),
cl([instance([],material_mod,_T1,f23(_T1))],[instance([],inanimate,_T1)],c110),
cl([instance([],material,f23(_T1))],[instance([],inanimate,_T1)],c111),
cl([instance([],inanimate,_T1)],[instance([],individual,_T1),instance([],material_mod,_T1,_U1),instance([],material,_U1)],c112),
cl([instance([],concrete_thing,_V1)],[instance([],mass_noun,_V1)],c113),
cl([instance([],mass_noun,_V1)],[instance([],concrete_thing,_V1)],c114),
cl([instance([],concrete_thing,_W1)],[instance([],individual,_W1)],c115),
cl([instance([],relative_mod,_W1,f24(_W1))],[instance([],individual,_W1)],c116),
cl([instance([],relation,f24(_W1))],[instance([],individual,_W1)],c117),
cl([instance([],individual,_W1)],[instance([],concrete_thing,_W1),instance([],relative_mod,_W1,_X1),instance([],relation,_X1)],c118),
cl([instance([],thing,_Y1)],[instance([],concrete_thing,_Y1)],c119),
cl([instance([],colour_mod,_Y1,f25(_Y1))],[instance([],concrete_thing,_Y1)],c120),
cl([instance([],colour,f25(_Y1))],[instance([],concrete_thing,_Y1)],c121),
cl([instance([],concrete_thing,_Y1)],[instance([],thing,_Y1),instance([],colour_mod,_Y1,_Z1),instance([],colour,_Z1)],c122),
cl([instance([],inanimate,_A2)],[instance([],geographical_object,_A2)],c123),
cl([instance([],origin_mod,_A2,f26(_A2))],[instance([],geographical_object,_A2)],c124),
cl([instance([],origin,f26(_A2))],[instance([],geographical_object,_A2)],c125),
cl([instance([],geographical_object,_A2)],[instance([],inanimate,_A2),instance([],origin_mod,_A2,_B2),instance([],origin,_B2)],c126),
cl([instance([],animate,_C2)],[instance([],human,_C2)],c127),
cl([instance([],volition,_C2,f27(_C2))],[instance([],human,_C2)],c128),
cl([instance([],volitional_sq,f27(_C2))],[instance([],human,_C2)],c129),
cl([instance([],human,_C2)],[instance([],animate,_C2),instance([],volition,_C2,_D2),instance([],volitional_sq,_D2)],c130),
cl([instance([],fss,_E2)],[instance([],predicate,_E2)],c131),
cl([instance([],subject,_E2,f28(_E2))],[instance([],predicate,_E2)],c132),
cl([instance([],thing,f28(_E2))],[instance([],predicate,_E2)],c133),
cl([instance([],purpose,_E2,f29(_E2))],[instance([],predicate,_E2)],c134),
cl([instance([],predicate,f29(_E2))],[instance([],predicate,_E2)],c135),
cl([instance([],time,_E2,f30(_E2))],[instance([],predicate,_E2)],c136),
cl([instance([],time,f30(_E2))],[instance([],predicate,_E2)],c137),
cl([instance([],illoc,_E2,f31(_E2))],[instance([],predicate,_E2)],c138),
cl([instance([],speech_act,f31(_E2))],[instance([],predicate,_E2)],c139),
cl([instance([],cause,_E2,f32(_E2))],[instance([],predicate,_E2)],c140),
cl([instance([],predicate,f32(_E2))],[instance([],predicate,_E2)],c141),
cl([instance([],result,_E2,f33(_E2))],[instance([],predicate,_E2)],c142),
cl([instance([],thing,f33(_E2))],[instance([],predicate,_E2)],c143),
cl([instance([],location,_E2,f34(_E2))],[instance([],predicate,_E2)],c144),
cl([instance([],thing,f34(_E2))],[instance([],predicate,_E2)],c145),
cl([instance([],predicate,_E2)],[instance([],fss,_E2),instance([],subject,_E2,_F2),instance([],thing,_F2),instance([],purpose,_E2,_G2),instance([],predicate,_G2),instance([],time,_E2,_H2),instance([],time,_H2),instance([],illoc,_E2,_I2),instance([],speech_act,_I2),instance([],cause,_E2,_J2),instance([],predicate,_J2),instance([],result,_E2,_K2),instance([],thing,_K2),instance([],location,_E2,_L2),instance([],thing,_L2)],c146),
cl([instance([],property_filler,_M2)],[instance([],thing,_M2)],c147),
cl([instance([],det,_M2,f35(_M2))],[instance([],thing,_M2)],c148),
cl([instance([],determiner,f35(_M2))],[instance([],thing,_M2)],c149),
cl([instance([],deitic_mpd,_M2,f36(_M2))],[instance([],thing,_M2)],c150),
cl([instance([],pointing,f36(_M2))],[instance([],thing,_M2)],c151),
cl([instance([],named,_M2,f37(_M2))],[instance([],thing,_M2)],c152),
cl([instance([],atom_codes,f37(_M2))],[instance([],thing,_M2)],c153),
cl([instance([],thing,_M2)],[instance([],property_filler,_M2),instance([],det,_M2,_N2),instance([],determiner,_N2),instance([],deitic_mpd,_M2,_O2),instance([],pointing,_O2),instance([],named,_M2,_P2),instance([],atom_codes,_P2)],c154),
cl([instance([],thing,_Q2)],[instance([],abstract_thing,_Q2)],c155),
cl([instance([],truth_mod,_Q2,f38(_Q2))],[instance([],abstract_thing,_Q2)],c156),
cl([instance([],'Entity',f38(_Q2))],[instance([],abstract_thing,_Q2)],c157),
cl([instance([],truth_mod,_Q2,f39(_Q2))],[instance([],abstract_thing,_Q2)],c158),
cl([instance([],'Entity',f39(_Q2))],[instance([],abstract_thing,_Q2)],c159),
cl([instance([],abstract_thing,_Q2)],[instance([],thing,_Q2),instance([],truth_mod,_Q2,_R2),instance([],'Entity',_R2),instance([],truth_mod,_Q2,_S2),instance([],'Entity',_S2)],c160),
cl([instance([],lexicon,_T2)],[instance([],mone,_T2)],c161),
cl([instance([],determiner,_T2)],[instance([],mone,_T2)],c162),
cl([instance([],mone,_T2)],[instance([],lexicon,_T2),instance([],determiner,_T2)],c163),
cl([instance([],lexicon,_U2)],[instance([],dem,_U2)],c164),
cl([instance([],definite,_U2)],[instance([],dem,_U2)],c165),
cl([instance([],dem,_U2)],[instance([],lexicon,_U2),instance([],definite,_U2)],c166),
cl([instance([],definite,_V2)],[instance([],die,_V2)],c167),
cl([instance([],lexicon,_V2)],[instance([],die,_V2)],c168),
cl([instance([],die,_V2)],[instance([],definite,_V2),instance([],lexicon,_V2)],c169),
cl([instance([],definite,_W2)],[instance([],der,_W2)],c170),
cl([instance([],lexicon,_W2)],[instance([],der,_W2)],c171),
cl([instance([],der,_W2)],[instance([],definite,_W2),instance([],lexicon,_W2)],c172),
cl([instance([],cardinal,_X2)],[instance([],fuenfunddreissig,_X2)],c173),
cl([instance([],lexicon,_X2)],[instance([],fuenfunddreissig,_X2)],c174),
cl([instance([],fuenfunddreissig,_X2)],[instance([],cardinal,_X2),instance([],lexicon,_X2)],c175),
cl([instance([],cardinal,_Y2)],[instance([],number35,_Y2)],c176),
cl([instance([],lexicon,_Y2)],[instance([],number35,_Y2)],c177),
cl([instance([],number35,_Y2)],[instance([],cardinal,_Y2),instance([],lexicon,_Y2)],c178),
cl([instance([],indefinite,_Z2)],[instance([],one,_Z2)],c179),
cl([instance([],lexicon,_Z2)],[instance([],one,_Z2)],c180),
cl([instance([],one,_Z2)],[instance([],indefinite,_Z2),instance([],lexicon,_Z2)],c181),
cl([instance([],determiner,_A3)],[instance([],d,_A3)],c182),
cl([instance([],lexicon,_A3)],[instance([],d,_A3)],c183),
cl([instance([],d,_A3)],[instance([],determiner,_A3),instance([],lexicon,_A3)],c184),
cl([instance([],monthly,_B3)],[instance([],monat,_B3)],c185),
cl([instance([],lexicon,_B3)],[instance([],monat,_B3)],c186),
cl([instance([],monat,_B3)],[instance([],monthly,_B3),instance([],lexicon,_B3)],c187),
cl([instance([],daily,_C3)],[instance([],taeglich,_C3)],c188),
cl([instance([],lexicon,_C3)],[instance([],taeglich,_C3)],c189),
cl([instance([],taeglich,_C3)],[instance([],daily,_C3),instance([],lexicon,_C3)],c190),
cl([instance([],sbone,_D3)],[instance([],lexicon,_D3)],c191),
cl([instance([],sbone,_E3)],[instance([],lexicon,_E3)],c192),
cl([instance([],'Entity',_F3)],[instance([],sbone,_F3)],c193),
cl([instance([],lexicon,_G3)],[instance([],was,_G3)],c194),
cl([instance([],thing,_G3)],[instance([],was,_G3)],c195),
cl([instance([],lexicon,_H3)],[instance([],was,_H3)],c196),
cl([instance([],thing,_H3)],[instance([],was,_H3)],c197),
cl([instance([],lexicon,_I3)],[instance([],auktion,_I3)],c198),
cl([instance([],thing,_I3)],[instance([],auktion,_I3)],c199),
cl([instance([],lexicon,_J3)],[instance([],auktion,_J3)],c200),
cl([instance([],thing,_J3)],[instance([],auktion,_J3)],c201),
cl([instance([],touchable_object,_K3)],[instance([],result,_K3)],c202),
cl([instance([],touchable_object,_L3)],[instance([],result,_L3)],c203),
cl([instance([],lexicon,_M3)],[instance([],kochbuch,_M3)],c204),
cl([instance([],touchable_object,_M3)],[instance([],kochbuch,_M3)],c205),
cl([instance([],lexicon,_N3)],[instance([],kochbuch,_N3)],c206),
cl([instance([],touchable_object,_N3)],[instance([],kochbuch,_N3)],c207),
cl([instance([],lexicon,_O3)],[instance([],buch,_O3)],c208),
cl([instance([],touchable_object,_O3)],[instance([],buch,_O3)],c209),
cl([instance([],lexicon,_P3)],[instance([],buch,_P3)],c210),
cl([instance([],touchable_object,_P3)],[instance([],buch,_P3)],c211),
cl([instance([],vehicle,_Q3)],[instance([],motorrad,_Q3)],c212),
cl([instance([],lexicon,_Q3)],[instance([],motorrad,_Q3)],c213),
cl([instance([],vehicle,_R3)],[instance([],motorrad,_R3)],c214),
cl([instance([],lexicon,_R3)],[instance([],motorrad,_R3)],c215),
cl([instance([],vehicle,_S3)],[instance([],bus,_S3)],c216),
cl([instance([],lexicon,_S3)],[instance([],bus,_S3)],c217),
cl([instance([],vehicle,_T3)],[instance([],bus,_T3)],c218),
cl([instance([],lexicon,_T3)],[instance([],bus,_T3)],c219),
cl([instance([],vehicle,_U3)],[instance([],fahrrad,_U3)],c220),
cl([instance([],lexicon,_U3)],[instance([],fahrrad,_U3)],c221),
cl([instance([],vehicle,_V3)],[instance([],fahrrad,_V3)],c222),
cl([instance([],lexicon,_V3)],[instance([],fahrrad,_V3)],c223),
cl([instance([],human,_W3)],[instance([],system,_W3)],c224),
cl([instance([],inanimate,_W3)],[instance([],system,_W3)],c225),
cl([instance([],human,_X3)],[instance([],system,_X3)],c226),
cl([instance([],inanimate,_X3)],[instance([],system,_X3)],c227),
cl([instance([],information,_Y3)],[instance([],string,_Y3)],c228),
cl([instance([],information,_Z3)],[instance([],string,_Z3)],c229),
cl([instance([],inanimate,_A4)],[instance([],information,_A4)],c230),
cl([instance([],inanimate,_B4)],[instance([],information,_B4)],c231),
cl([instance([],lexicon,_C4)],[instance([],ort,_C4)],c232),
cl([instance([],geographical_object,_C4)],[instance([],ort,_C4)],c233),
cl([instance([],lexicon,_D4)],[instance([],ort,_D4)],c234),
cl([instance([],geographical_object,_D4)],[instance([],ort,_D4)],c235),
cl([instance([],geographical_object,_E4)],[instance([],hier,_E4)],c236),
cl([instance([],lexicon,_E4)],[instance([],hier,_E4)],c237),
cl([instance([],geographical_object,_F4)],[instance([],hier,_F4)],c238),
cl([instance([],lexicon,_F4)],[instance([],hier,_F4)],c239),
cl([instance([],geographical_object,_G4)],[instance([],wald,_G4)],c240),
cl([instance([],lexicon,_G4)],[instance([],wald,_G4)],c241),
cl([instance([],geographical_object,_H4)],[instance([],wald,_H4)],c242),
cl([instance([],lexicon,_H4)],[instance([],wald,_H4)],c243),
cl([instance([],lexicon,_I4)],[instance([],berlin,_I4)],c244),
cl([instance([],town,_I4)],[instance([],berlin,_I4)],c245),
cl([instance([],lexicon,_J4)],[instance([],berlin,_J4)],c246),
cl([instance([],town,_J4)],[instance([],berlin,_J4)],c247),
cl([instance([],lexicon,_K4)],[instance([],saarlouis,_K4)],c248),
cl([instance([],town,_K4)],[instance([],saarlouis,_K4)],c249),
cl([instance([],lexicon,_L4)],[instance([],saarlouis,_L4)],c250),
cl([instance([],town,_L4)],[instance([],saarlouis,_L4)],c251),
cl([instance([],town,_M4)],[instance([],dudweiler,_M4)],c252),
cl([instance([],lexicon,_M4)],[instance([],dudweiler,_M4)],c253),
cl([instance([],town,_N4)],[instance([],dudweiler,_N4)],c254),
cl([instance([],lexicon,_N4)],[instance([],dudweiler,_N4)],c255),
cl([instance([],town,_O4)],[instance([],saarbruecken,_O4)],c256),
cl([instance([],lexicon,_O4)],[instance([],saarbruecken,_O4)],c257),
cl([instance([],town,_P4)],[instance([],saarbruecken,_P4)],c258),
cl([instance([],lexicon,_P4)],[instance([],saarbruecken,_P4)],c259),
cl([instance([],geographical_object,_Q4)],[instance([],town,_Q4)],c260),
cl([instance([],geographical_object,_R4)],[instance([],town,_R4)],c261),
cl([instance([],animate,_S4)],[instance([],plant,_S4)],c262),
cl([instance([],animate,_T4)],[instance([],plant,_T4)],c263),
cl([instance([],lexicon,_U4)],[instance([],person,_U4)],c264),
cl([instance([],human,_U4)],[instance([],person,_U4)],c265),
cl([instance([],lexicon,_V4)],[instance([],person,_V4)],c266),
cl([instance([],human,_V4)],[instance([],person,_V4)],c267),
cl([instance([],lexicon,_W4)],[instance([],sie,_W4)],c268),
cl([instance([],human,_W4)],[instance([],sie,_W4)],c269),
cl([instance([],lexicon,_X4)],[instance([],sie,_X4)],c270),
cl([instance([],human,_X4)],[instance([],sie,_X4)],c271),
cl([instance([],human,_Y4)],[instance([],frau,_Y4)],c272),
cl([instance([],lexicon,_Y4)],[instance([],frau,_Y4)],c273),
cl([instance([],human,_Z4)],[instance([],frau,_Z4)],c274),
cl([instance([],lexicon,_Z4)],[instance([],frau,_Z4)],c275),
cl([instance([],abstract_thing,_A5)],[instance([],number,_A5)],c276),
cl([instance([],abstract_thing,_B5)],[instance([],number,_B5)],c277),
cl([instance([],lexicon,_C5)],[instance([],steuerhandlung,_C5)],c278),
cl([instance([],tax_action,_C5)],[instance([],steuerhandlung,_C5)],c279),
cl([instance([],lexicon,_D5)],[instance([],steuerhandlung,_D5)],c280),
cl([instance([],tax_action,_D5)],[instance([],steuerhandlung,_D5)],c281),
cl([instance([],abstract_thing,_E5)],[instance([],tax_action,_E5)],c282),
cl([instance([],abstract_thing,_F5)],[instance([],tax_action,_F5)],c283),
cl([instance([],abstract_thing,_G5)],[instance([],profession,_G5)],c284),
cl([instance([],abstract_thing,_H5)],[instance([],profession,_H5)],c285),
cl([instance([],lexicon,_I5)],[instance([],geld,_I5)],c286),
cl([instance([],cost,_I5)],[instance([],geld,_I5)],c287),
cl([instance([],lexicon,_J5)],[instance([],geld,_J5)],c288),
cl([instance([],cost,_J5)],[instance([],geld,_J5)],c289),
cl([instance([],lexicon,_K5)],[instance([],kosten,_K5)],c290),
cl([instance([],cost,_K5)],[instance([],kosten,_K5)],c291),
cl([instance([],lexicon,_L5)],[instance([],kosten,_L5)],c292),
cl([instance([],cost,_L5)],[instance([],kosten,_L5)],c293),
cl([instance([],abstract_thing,_M5)],[instance([],cost,_M5)],c294),
cl([instance([],abstract_thing,_N5)],[instance([],cost,_N5)],c295),
cl([instance([],motion_by_means,_O5)],[instance([],motion_by_means_content,_O5)],c296),
cl([instance([],action_content,_O5)],[instance([],motion_by_means_content,_O5)],c297),
cl([instance([],motion_by_means,_P5)],[instance([],motion_by_means_content,_P5)],c298),
cl([instance([],action_content,_P5)],[instance([],motion_by_means_content,_P5)],c299),
cl([instance([],action_content,_Q5)],[instance([],motion_content,_Q5)],c300),
cl([instance([],motion,_Q5)],[instance([],motion_content,_Q5)],c301),
cl([instance([],action_content,_R5)],[instance([],motion_content,_R5)],c302),
cl([instance([],motion,_R5)],[instance([],motion_content,_R5)],c303),
cl([instance([],abstract_thing,_S5)],[instance([],action_content,_S5)],c304),
cl([instance([],action,_S5)],[instance([],action_content,_S5)],c305),
cl([instance([],abstract_thing,_T5)],[instance([],action_content,_T5)],c306),
cl([instance([],action,_T5)],[instance([],action_content,_T5)],c307),
cl([instance([],profession,_U5)],[instance([],informatiker,_U5)],c308),
cl([instance([],lexicon,_U5)],[instance([],informatiker,_U5)],c309),
cl([instance([],profession,_V5)],[instance([],informatiker,_V5)],c310),
cl([instance([],lexicon,_V5)],[instance([],informatiker,_V5)],c311),
cl([instance([],profession,_W5)],[instance([],schroneer,_W5)],c312),
cl([instance([],lexicon,_W5)],[instance([],schroneer,_W5)],c313),
cl([instance([],profession,_X5)],[instance([],schroneer,_X5)],c314),
cl([instance([],lexicon,_X5)],[instance([],schroneer,_X5)],c315),
cl([instance([],abstract_thing,_Y5)],[instance([],gi,_Y5)],c316),
cl([instance([],lexicon,_Y5)],[instance([],gi,_Y5)],c317),
cl([instance([],abstract_thing,_Z5)],[instance([],gi,_Z5)],c318),
cl([instance([],lexicon,_Z5)],[instance([],gi,_Z5)],c319),
cl([instance([],'Entity',_A6)],[instance([],voelklingen_name,_A6)],c320),
cl([instance([],quality,_B6)],[instance([],worth,_B6)],c321),
cl([instance([],quality,_C6)],[instance([],worth,_C6)],c322),
cl([instance([],colour,_D6)],[instance([],rot,_D6)],c323),
cl([instance([],lexicon,_D6)],[instance([],rot,_D6)],c324),
cl([instance([],colour,_E6)],[instance([],rot,_E6)],c325),
cl([instance([],lexicon,_E6)],[instance([],rot,_E6)],c326),
cl([instance([],lexicon,_F6)],[instance([],gross,_F6)],c327),
cl([instance([],physical_sq,_F6)],[instance([],gross,_F6)],c328),
cl([instance([],lexicon,_G6)],[instance([],gross,_G6)],c329),
cl([instance([],physical_sq,_G6)],[instance([],gross,_G6)],c330),
cl([instance([],lexicon,_H6)],[instance([],klone,_H6)],c331),
cl([instance([],physical_sq,_H6)],[instance([],klone,_H6)],c332),
cl([instance([],lexicon,_I6)],[instance([],klone,_I6)],c333),
cl([instance([],physical_sq,_I6)],[instance([],klone,_I6)],c334),
cl([instance([],state_q,_J6)],[instance([],physical_sq,_J6)],c335),
cl([instance([],state_q,_K6)],[instance([],physical_sq,_K6)],c336),
cl([instance([],state_q,_L6)],[instance([],volitional_sq,_L6)],c337),
cl([instance([],state_q,_M6)],[instance([],volitional_sq,_M6)],c338),
cl([instance([],qualitative,_N6)],[instance([],state_q,_N6)],c339),
cl([instance([],qualitative,_O6)],[instance([],state_q,_O6)],c340),
cl([instance([],adjective_property,_P6)],[instance([],origin,_P6)],c341),
cl([instance([],adjective_property,_Q6)],[instance([],origin,_Q6)],c342),
cl([instance([],lexicon,_R6)],[instance([],haben,_R6)],c343),
cl([instance([],property,_R6)],[instance([],haben,_R6)],c344),
cl([instance([],concept108,_R6)],[instance([],haben,_R6)],c345),
cl([instance([],lexicon,_S6)],[instance([],haben,_S6)],c346),
cl([instance([],property,_S6)],[instance([],haben,_S6)],c347),
cl([instance([],has_property_haben,_S6,f40(_S6))],[instance([],haben,_S6)],c348),
cl([instance([],thing,f40(_S6))],[instance([],haben,_S6)],c349),
cl([instance([],lexicon,_T6)],[instance([],besitzen,_T6)],c350),
cl([instance([],property,_T6)],[instance([],besitzen,_T6)],c351),
cl([instance([],lexicon,_U6)],[instance([],besitzen,_U6)],c352),
cl([instance([],property,_U6)],[instance([],besitzen,_U6)],c353),
cl([instance([],lexicon,_V6)],[instance([],sone,_V6)],c354),
cl([instance([],property,_V6)],[instance([],sone,_V6)],c355),
cl([instance([],lexicon,_W6)],[instance([],sone,_W6)],c356),
cl([instance([],property,_W6)],[instance([],sone,_W6)],c357),
cl([instance([],deducte,_X6)],[instance([],absetzbar,_X6)],c358),
cl([instance([],lexicon,_X6)],[instance([],absetzbar,_X6)],c359),
cl([instance([],deducte,_Y6)],[instance([],absetzbar,_Y6)],c360),
cl([instance([],lexicon,_Y6)],[instance([],absetzbar,_Y6)],c361),
cl([instance([],property,_Z6)],[instance([],deducte,_Z6)],c362),
cl([instance([],property,_A7)],[instance([],deducte,_A7)],c363),
cl([instance([],predicate,_B7)],[instance([],property,_B7)],c364),
cl([instance([],predicate,_C7)],[instance([],property,_C7)],c365),
cl([instance([],lexicon,_D7)],[instance([],verursach,_D7)],c366),
cl([instance([],reason,_D7)],[instance([],verursach,_D7)],c367),
cl([instance([],lexicon,_E7)],[instance([],verursach,_E7)],c368),
cl([instance([],reason,_E7)],[instance([],verursach,_E7)],c369),
cl([instance([],action,_F7)],[instance([],reason,_F7)],c370),
cl([instance([],action,_G7)],[instance([],reason,_G7)],c371),
cl([instance([],lexicon,_H7)],[instance([],zahl,_H7)],c372),
cl([instance([],transaction,_H7)],[instance([],zahl,_H7)],c373),
cl([instance([],lexicon,_I7)],[instance([],zahl,_I7)],c374),
cl([instance([],transaction,_I7)],[instance([],zahl,_I7)],c375),
cl([instance([],repeat,_J7)],[instance([],wiederhol,_J7)],c376),
cl([instance([],lexicon,_J7)],[instance([],wiederhol,_J7)],c377),
cl([instance([],repeat,_K7)],[instance([],wiederhol,_K7)],c378),
cl([instance([],lexicon,_K7)],[instance([],wiederhol,_K7)],c379),
cl([instance([],action,_L7)],[instance([],repeat,_L7)],c380),
cl([instance([],action,_M7)],[instance([],repeat,_M7)],c381),
cl([instance([],touchable_object,_N7)],[instance([],canvas,_N7)],c382),
cl([instance([],touchable_object,_O7)],[instance([],canvas,_O7)],c383),
cl([instance([],quality,_P7)],[instance([],weight,_P7)],c384),
cl([instance([],quality,_Q7)],[instance([],weight,_Q7)],c385),
cl([instance([],mass_noun,_R7)],[instance([],material,_R7)],c386),
cl([instance([],mass_noun,_S7)],[instance([],material,_S7)],c387),
cl([instance([],adjective_property,_T7)],[instance([],relation,_T7)],c388),
cl([instance([],adjective_property,_U7)],[instance([],relation,_U7)],c389),
cl([instance([],quality,_V7)],[instance([],colour,_V7)],c390),
cl([instance([],quality,_W7)],[instance([],colour,_W7)],c391),
cl([instance([],qualitative,_X7)],[instance([],quality,_X7)],c392),
cl([instance([],qualitative,_Y7)],[instance([],quality,_Y7)],c393),
cl([instance([],adjective_property,_Z7)],[instance([],qualitative,_Z7)],c394),
cl([instance([],adjective_property,_A8)],[instance([],qualitative,_A8)],c395),
cl([instance([],lexicon,_B8)],[instance([],wohn,_B8)],c396),
cl([instance([],state,_B8)],[instance([],wohn,_B8)],c397),
cl([instance([],concept50,_B8)],[instance([],wohn,_B8)],c398),
cl([instance([],concept51,_B8)],[instance([],wohn,_B8)],c399),
cl([instance([],lexicon,_C8)],[instance([],wohn,_C8)],c400),
cl([instance([],state,_C8)],[instance([],wohn,_C8)],c401),
cl([instance([],agent,_C8,f41(_C8))],[instance([],wohn,_C8)],c402),
cl([instance([],human,f41(_C8))],[instance([],wohn,_C8)],c403),
cl([instance([],location_wohn,_C8,f42(_C8))],[instance([],wohn,_C8)],c404),
cl([instance([],location,f42(_C8))],[instance([],wohn,_C8)],c405),
cl([instance([],predicate,_D8)],[instance([],state,_D8)],c406),
cl([instance([],concept41,_D8)],[instance([],state,_D8)],c407),
cl([instance([],predicate,_E8)],[instance([],state,_E8)],c408),
cl([instance([],time,_E8,f43(_E8))],[instance([],state,_E8)],c409),
cl([instance([],period,f43(_E8))],[instance([],state,_E8)],c410),
cl([instance([],abstract_thing,_F8)],[instance([],atom_codes,_F8)],c411),
cl([instance([],abstract_thing,_G8)],[instance([],atom_codes,_G8)],c412),
cl([instance([],adjective_property,_H8)],[instance([],truth_value,_H8)],c413),
cl([instance([],adjective_property,_I8)],[instance([],truth_value,_I8)],c414),
cl([instance([],property_filler,_J8)],[instance([],adjective_property,_J8)],c415),
cl([instance([],property_filler,_K8)],[instance([],adjective_property,_K8)],c416),
cl([instance([],fss,_L8)],[instance([],property_filler,_L8)],c417),
cl([instance([],fss,_M8)],[instance([],property_filler,_M8)],c418),
cl([instance([],definite,_N8)],[instance([],possessive,_N8)],c419),
cl([instance([],definite,_O8)],[instance([],possessive,_O8)],c420),
cl([instance([],definite,_P8)],[instance([],demonstrative,_P8)],c421),
cl([instance([],definite,_Q8)],[instance([],demonstrative,_Q8)],c422),
cl([instance([],determiner,_R8)],[instance([],definite,_R8)],c423),
cl([instance([],determiner,_S8)],[instance([],definite,_S8)],c424),
cl([instance([],determiner,_T8)],[instance([],interrogative,_T8)],c425),
cl([instance([],determiner,_U8)],[instance([],interrogative,_U8)],c426),
cl([instance([],cardinal,_V8)],[instance([],number50,_V8)],c427),
cl([instance([],lexicon,_V8)],[instance([],number50,_V8)],c428),
cl([instance([],cardinal,_W8)],[instance([],number50,_W8)],c429),
cl([instance([],lexicon,_W8)],[instance([],number50,_W8)],c430),
cl([instance([],indefinite,_X8)],[instance([],cardinal,_X8)],c431),
cl([instance([],indefinite,_Y8)],[instance([],cardinal,_Y8)],c432),
cl([instance([],determiner,_Z8)],[instance([],indefinite,_Z8)],c433),
cl([instance([],determiner,_A9)],[instance([],indefinite,_A9)],c434),
cl([instance([],fss,_B9)],[instance([],determiner,_B9)],c435),
cl([instance([],fss,_C9)],[instance([],determiner,_C9)],c436),
cl([instance([],speech_act,_D9)],[instance([],interjection,_D9)],c437),
cl([instance([],speech_act,_E9)],[instance([],interjection,_E9)],c438),
cl([instance([],speech_act,_F9)],[instance([],question,_F9)],c439),
cl([instance([],speech_act,_G9)],[instance([],question,_G9)],c440),
cl([instance([],speech_act,_H9)],[instance([],assertion,_H9)],c441),
cl([instance([],speech_act,_I9)],[instance([],assertion,_I9)],c442),
cl([instance([],speech_act,_J9)],[instance([],order,_J9)],c443),
cl([instance([],speech_act,_K9)],[instance([],order,_K9)],c444),
cl([instance([],fss,_L9)],[instance([],speech_act,_L9)],c445),
cl([instance([],fss,_M9)],[instance([],speech_act,_M9)],c446),
cl([instance([],interval,_N9)],[instance([],monthly,_N9)],c447),
cl([instance([],interval,_O9)],[instance([],monthly,_O9)],c448),
cl([instance([],interval,_P9)],[instance([],daily,_P9)],c449),
cl([instance([],interval,_Q9)],[instance([],daily,_Q9)],c450),
cl([instance([],lexicon,_R9)],[instance([],woechentlich,_R9)],c451),
cl([instance([],weekly,_R9)],[instance([],woechentlich,_R9)],c452),
cl([instance([],lexicon,_S9)],[instance([],woechentlich,_S9)],c453),
cl([instance([],weekly,_S9)],[instance([],woechentlich,_S9)],c454),
cl([instance([],interval,_T9)],[instance([],weekly,_T9)],c455),
cl([instance([],interval,_U9)],[instance([],weekly,_U9)],c456),
cl([instance([],yearly,_V9)],[instance([],jaehrlich,_V9)],c457),
cl([instance([],lexicon,_V9)],[instance([],jaehrlich,_V9)],c458),
cl([instance([],yearly,_W9)],[instance([],jaehrlich,_W9)],c459),
cl([instance([],lexicon,_W9)],[instance([],jaehrlich,_W9)],c460),
cl([instance([],interval,_X9)],[instance([],yearly,_X9)],c461),
cl([instance([],interval,_Y9)],[instance([],yearly,_Y9)],c462),
cl([instance([],time,_Z9)],[instance([],interval,_Z9)],c463),
cl([instance([],time,_A10)],[instance([],interval,_A10)],c464),
cl([instance([],time,_B10)],[instance([],period,_B10)],c465),
cl([instance([],time,_C10)],[instance([],period,_C10)],c466),
cl([instance([],time,_D10)],[instance([],moment,_D10)],c467),
cl([instance([],time,_E10)],[instance([],moment,_E10)],c468),
cl([instance([],fss,_F10)],[instance([],time,_F10)],c469),
cl([instance([],fss,_G10)],[instance([],time,_G10)],c470),
cl([instance([],pointing,_H10)],[instance([],exact_p,_H10)],c471),
cl([instance([],pointing,_I10)],[instance([],exact_p,_I10)],c472),
cl([instance([],pointing,_J10)],[instance([],encircling_p,_J10)],c473),
cl([instance([],pointing,_K10)],[instance([],encircling_p,_K10)],c474),
cl([instance([],pointing,_L10)],[instance([],standard_p,_L10)],c475),
cl([instance([],pointing,_M10)],[instance([],standard_p,_M10)],c476),
cl([instance([],pointing,_N10)],[instance([],vague_p,_N10)],c477),
cl([instance([],pointing,_O10)],[instance([],vague_p,_O10)],c478),
cl([instance([],fss,_P10)],[instance([],pointing,_P10)],c479),
cl([instance([],fss,_Q10)],[instance([],pointing,_Q10)],c480),
cl([instance([],sbone,_R10)],[instance([],fss,_R10)],c481),
cl([instance([],sbone,_S10)],[instance([],fss,_S10)],c482),
cl([instance([],sbone,_T10)],[instance([],fss,_T10)],c483),
cl([instance([],sbone,_U10)],[instance([],fss,_U10)],c484)]).
% Clause set fss
% generated from the fss knowledge base
% 14.03.95: Total runtime  943.049 sec.
clauseSet(fss,L) :-
	clauseSet(fssTBox,L90),
	append(L90,
	       [cl([instance([],yearly,birthday)],[],c485),
%		cl([instance([],speech_act,hello)],[],c486),
%		cl([instance([],question,where)],[],c487),
%		cl([instance([],question,when)],[],c488),
		cl([instance([],question,how)],[],c489),
		cl([instance([],thing,car1)],[],c490),
		cl([],[instance([],abstract_thing,boat1)],c491),
		cl([instance([],cost,boat1)],[],c492),
		cl([instance([],thing,plane1)],[],c493)],L).
clauseSet(fss1,
[cl([instance([],touchable_object,_A)],[instance([],vehicle,_A)],c001),
cl([instance([],worth_mod,_A,f8(_A))],[instance([],vehicle,_A)],c002),
cl([instance([],worth,f8(_A))],[instance([],vehicle,_A)],c003),
cl([instance([],vehicle,_A)],[instance([],touchable_object,_A),instance([],worth_mod,_A,_B),instance([],worth,_B)],c004),
cl([instance([],town,_C)],[instance([],voelklingen,_C)],c005)]).
/**********************************************************************
 *
 * @(#) roleFunctions.pll 1.2@(#)
 *
 */

/***********************************************************************
 *
 * getDirectFatherRoles(+KBName,+CTX,+Role,-RL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are direct father roles
 * of Role.
 *
 */

getDirectFatherRoles(KBName,CTX,Role,RL) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Dag),
	getDirectSuperElements(Role,RL,Dag).

/***********************************************************************
 *
 * getAllFatherRoles(+KBName,+CTX,+Role,-RL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are father roles of
 * Role
 *
 */

getAllFatherRoles(KBName,CTX,Role,RL) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Dag),
	getAllSuperElements(Role,RL,[],Dag).

/***********************************************************************
 *
 * getDirectSonRoles(+KBName,+CTX,+Role,-RL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are direct father roles
 * of Role
 *
 */

getDirectSonRoles(KBName,CTX,Role,RL) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Dag),
	getDirectSubElements(Role,RL,Dag).

/***********************************************************************
 *
 * getAllSonRoles(+KBName,+CTX,+Role,-RL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are father roles of 
 * Role
 *
 */

getAllSonRoles(KBName,CTX,Role,RL) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Dag),
	getAllSubElements(Role,RL,Dag).

/***********************************************************************
 *
 * getRoles(+KBName,+CTX,-RL)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX     modal context
 *            RL     list of role names
 * RL is the list of all role names instance the subsumption hierarchy.
 *
 */

getRoles(KBName,CTX,['Entity'|RL]) :-
	getAllSonRoles(KBName,CTX,'Entity',RL).

/***********************************************************************
 *
 * testDirectFatherRole(+KBName,+CTX,+Role1,+Role2,-Role)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct father role of Role2
 * or
 * Role is Role2 iff Role2 is a direct father role of Role1
 * otherwise
 * the predicate fails
 *
 */

testDirectFatherRole(KBName,CTX,Role1,Role2,Role) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Dag),
	testDirectSuperElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * testDirectSonRole(+KBName,+CTX,+Role1,+Role2,-Role)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct subrole of Role2
 * or
 * Role is Role2 iff Role2 is a direct subrole of Role1
 * otherwise
 * the predicate fails
 *
 */

testDirectSonRole(KBName,CTX,Role1,Role2,Role) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Dag),
	testDirectSuperElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * testFatherRole(+KBName,+CTX,+Role1,+Role2,-Role)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct father role of Role2
 * or
 * Role is Role2 iff Role2 is a direct father role of Role1
 * otherwise
 * the predicate fails
 *
 */

testFatherRole(KBName,CTX,Role1,Role2,Role) :-
	knowledgeBase(KBName,KB,_),
	roleHierarchy(KB,CTX,Dag),
	testSuperElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * testSonRole(+KBName,+CTX,+Role1,+Role2,-Role)
 * Arguments: KBName     knowledgeBase identifier
 *            CTX          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct father role of Role2
 * or
 * Role is Role2 iff Role2 is a direct father role of Role1
 * otherwise
 * the predicate fails
 *
 */

testSonRole(KB,CTX,Role1,Role2,Role) :-
	roleHierarchy(KB,CTX,Dag),
	testSubElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * getCommonFatherRoles(+KBName,+CTX,RL1,RL2)
 * Arguments: KBName  knowledgeBase identifier
 *            CTX       modal context
 *            RL1      list of role names
 *            RL2      list of role names
 * RL2 is the list of all role names subsuming all roles instance RL1.
 *
 */

getCommonFatherRoles(KBName,CTX,RL1,RL2) :-
	hop_map(getAllFatherRoles,[KBName,CTX],RL1,RLL1),
	cmp_intersection(RLL1,RL2).

/***********************************************************************
 *
 * getCommonSonRoles(+KBName,+CTX,RL1,RL2)
 * Arguments: KBName  knowledgeBase identifier
 *            CTX       modal context
 *            RL1      list of role names
 *            RL2      list of role names
 * RL2 is the list of all role names which are subsumed by all
 * roles instance RL1.
 *
 */

getCommonSonRoles(KBName,CTX,RL1,RL2) :-
	hop_map(getAllSonRoles,[KBName,CTX],RL1,RLL1),
	cmp_intersection(RLL1,RL2).

/**********************************************************************
 *
 * @(#) revision.pll 1.38@(#)
 *
 */

/**********************************************************************
 *
 * undefconcept(+CTX,+CN)
 *
 */


undefconcept(CN) :-
	getCurrentKnowledgeBase(KBName),
	undefconcept(KBName,[],CN).

undefconcept(KBName,CN) :-
	knowledgeBase(KBName,_,_),
	!,
	undefconcept(KBName,[],CN).
undefconcept(CTX,CN) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	undefconcept(KBName,CTX,CN).
undefconcept(CN,CT) :-
	getCurrentKnowledgeBase(KBName),
	undefconcept(KBName,[],CN,CT).

undefconcept(KBName,CTX,CN) :-

	knowledgeBase(KBName,KB,_),
	(CTX = [] ; CTX = [_|_]),
	!,
	undefConcept(KB,CTX,CN).
undefconcept(CTX,CN,CT) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	undefconcept(KBName,CTX,CN,CT).
undefconcept(KBName,CN,CT) :-
	undefconcept(KBName,[],CN,CT).

undefconcept(KBName,CTX,CN,CT) :-
	knowledgeBase(KBName,KB,_),
	conceptEqualSets(KB,_user,CTX,CN,CT,AX1),
	retractall(axiom(KB,CTX,defconcept(CTX,CN,CT))),
 	retractall(kb_in(KB,_,rn(AX1,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
 	cmp_retractAllProlog(query(KB,CTX,CN,_CT,_PT,_)),
	retractall(eq(KB,rn(AX1,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX1,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX1,_,_,_),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX1,_,_,_),_,_,_,_,_,_)),
	change_classifier(KBName,CTX,CN,CT),
	retract(conceptEqualSets(KB,_user,CTX,CN,CT,AX1)),
	conceptEqualSets(KB,_,CTX,_,not(CN),AX2),
 	retractall(kb_in(KB,_,rn(AX2,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
 	cmp_retractAllProlog(query(KB,CTX,CN,_CT,_PT,_)),
	retractall(eq(KB,rn(AX2,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX2,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX2,_,_,_),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX2,_,_,_),_,_,_,_,_,_)),
	!.

undefConcept(KB,CTX,CN) :-
	conceptEqualSets(KB,user,_,CN,_,AX),
	retractall(axiom(KB,CTX,defconcept(CTX,CN,_CT1))),
 	retractall(kb_in(KB,_,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],_,_),_,_))),
   	cmp_retractAllProlog(query(KB,CTX,CN,_CT,_PT,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(KBName,CTX,CN,CT),
	retractall(conceptEqualSets(KB,user,CTX,CN,_CT,AX)),
	fail,
	!.
undefConcept(KB,CTX,CN) :-
	conceptEqualSets(KB,system,_,_,not(CN),AX),
	retractall(axiom(KB,CTX,defconcept(CTX,not(CN),_CT1))),
 	retractall(kb_in(KB,_,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],_,_),_,_))),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(conceptEqualSets(KB,system,CTX,_,_,AX)),
   	cmp_retractAllProlog(query(KB,CTX,not(CN),_CT,_PT,_)),
	fail,
	!.
undefConcept(_KB,_CTX,_CN) :-
	!.

cmp_retractAllProlog(kb_in(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)) :-
	clause(kb_in(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_)),_),
	retractall(kb_in(KB,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
	fail.
cmp_retractAllProlog(kb_in(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)).

cmp_retractAllProlog(instance(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)) :-
	clause(instance(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_)),_),
	member(rn(AX,_,_,_),[Name]),	
	retractall(instance(KB,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
	fail.
cmp_retractAllProlog(instance(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)) :-
	retractall(instance(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(instance(_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)).

cmp_retractAllProlog(query(KB,CTX,CN,_CT,_PT,_PT1)) :-
	query(KB,CTX,CN1,CT,PT,PT1),
	collect(PT,Liste),
	member(CN,Liste),
	retractall(query(KB,CTX,CN1,CT,PT,PT1)),
	fail.
cmp_retractAllProlog(query(KB,CTX,CN,_CT,_PT,_PT1)).

/**********************************************************************
 *
 * undefrole(+CTX,+CN)
 *
 */
undefrole(RN) :-
	getCurrentKnowledgeBase(KBName),
	undefrole(KBName,[],RN).

undefrole(KBName,RN) :-
	knowledgeBase(KBName,_,_),
	!,
	undefrole(KBName,[],RN).
undefrole(CTX,RN) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	undefrole(KBName,CTX,RN).
undefrole(RN,RT) :-
	getCurrentKnowledgeBase(KBName),
	undefrole(KBName,[],RN,RT).

undefrole(KBName,CTX,RN) :-
	knowledgeBase(KBName,KB,_),
	(CTX = [] ; CTX = [_|_]),
	!,
	undefRole(KB,CTX,RN).
undefrole(CTX,RN,RT) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	undefrole(KBName,CTX,RN,RT).
undefrole(KBName,RN,RT) :-
	undefrole(KBName,[],RN,RT).
undefrole(KBName,CTX,RN,RT) :-
	knowledgeBase(KBName,KB,_),
	roleEqualSets(KB,_user,CTX,RN,RT,AX),
	retractall(axiom(KB,CTX,defrole(CTX,RN,RT))),
 	retractall(kb_in(KB,_,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),

 	cmp_retractAllProlog(query(KB,CTX,CN,_CT,_PT,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(KBName,CTX,RN,RT),
	retract(roleEqualSets(KB,_user,CTX,RN,RT,AX)),
	!.
undefRole(KB,CTX,RN) :-
	roleEqualSets(KB,user,CTX,RN,_,AX),
	retractall(axiom(KB,CTX,defrole(CTX,RN,_RT1))),
 	retractall(kb_in(KB,_,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(KBName,CTX,RN,_),
	retractall(roleEqualSets(KB,user,CTX,RN,_RT,AX)),
	fail,
	!.
undefRole(_KB,_CTX,_RN) :-
	!.

/**********************************************************************
 *
 * undefprimconcept(+CTX,+CN)
 *
 */

undefprimconcept(CN) :-
	getCurrentKnowledgeBase(KBName),
	undefprimconcept(KBName,CN,_).
undefprimconcept(CN,CT) :-
	getCurrentKnowledgeBase(KBName),
	!,
	undefprimconcept(KBName,[],CN,CT).

undefprimconcept(KBName,CN,CT) :-
	knowledgeBase(KBName,_,_),
	!,
	undefprimconcept(KBName,[],CN,CT).
undefprimconcept(CTX,CN,CT) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	undefprimconcept(KBName,CTX,CN,CT).

undefprimconcept(KBName,CTX,CN,CT) :-	
	knowledgeBase(KBName,KB,_),
	conceptSubsets(KB,_user,CTX,CN,CT,AX),
	retractall(axiom(KB,CTX,defprimconcept(CTX,CN,CT))),
 	retractall(kb_in(KB,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
 	cmp_retractAllProlog(query(KB,CTX,CN,_CT,_PT,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,lInR),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,lInR),_,_,_,_,_,_)),
	change_classifier(KBName,CTX,CN,CT),
	retract(conceptSubsets(KB,_user,CTX,CN,CT,AX)),
	!.
/**********************************************************************
 *
 * undefprimrole(+CTX,+CN)
 *
 */
undefprimrole(RN) :-
	getCurrentKnowledgeBase(KBName),
	undefprimrole(KBName,[],RN).

undefprimrole(KBName,RN) :-
	knowledgeBase(KBName,_,_),
	!,
	undefprimrole(KBName,[],RN).
undefprimrole(CTX,RN) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	undefprimrole(KBName,CTX,RN).
undefprimrole(RN,RT) :-
	getCurrentKnowledgeBase(KBName),
	undefprimrole(KBName,[],RN,RT).

undefprimrole(KBName,CTX,RN) :-
	knowledgeBase(KBName,KB,_),
	(CTX = [] ; CTX = [_|_]),
	!,
	undefprimRole(KB,CTX,RN).
undefprimrole(CTX,RN,RT) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	undefprimrole(KBName,CTX,RN,RT).
undefprimrole(KBName,RN,RT) :-
	undefprimrole(KBName,[],RN,RT).
undefprimrole(KBName,CTX,RN,RT) :-
	knowledgeBase(KBName,KB,_),

	roleSubsets(KB,_user,CTX,RN,RT,AX),
	retractall(axiom(KB,CTX,defprimrole(CTX,RN,RT))),
 	retractall(kb_in(KB,_,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
 	cmp_retractAllProlog(query(KB,CTX,RN,_RT,_PT,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,lInR),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,lInR),_,_,_,_,_,_)),
	change_classifier(KBName,CTX,RN,RT),
	retract(roleSubsets(KB,_user,CTX,RN,RT,AX)),
	!.
undefprimRole(KB,CTX,RN) :-
	roleSubsets(KB,user,CTX,RN,_,AX),
	retractall(axiom(KB,CTX,defprimrole(CTX,RN,_RT1))),
 	cmp_retractAllProlog(query(KB,CTX,RN,_RT,_PT,_)),
 	retractall(kb_in(KB,_,rn(AX,_,_,_),_,_,_,_,_,_,proved(instance([],Name,_),_,_))),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(KB,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(KB,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(KBName,CTX,RN,_),
	retractall(roleSubsets(KB,user,CTX,RN,_RT,AX)),
	fail,
	!.

/** ist instance arbeit    */


delete_ind(X,C) :-
	completeParameter([(X,C)],KBName,CTX,_,_),
	delete_ind(KBName,CTX,X,C).
delete_ind(KBName,X,C) :-
	knowledgeBase(KBName,_,_),
	!,
	delete_ind(KBName,[],X,C).
delete_ind(CTX,X,C) :-
	(CTX = []; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	delete_ind(KBName,CTX,X,C).
delete_ind(X,Y,R) :-
	getCurrentKnowledgeBase(KBName),
	delete_ind(KBName,[],X,Y,R).
delete_ind(KBName,CTX,X,C) :-
	knowledgeBase(KBName,KB,_),
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	!,
	atomic(X),
	retractall(axiom(KB,CTX,assert_ind(CTX,X,C))),
	cmp_retractAllProlog(query(KB,CTX,_C,_X,_PT,_)),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	constructMLHead(KB,rn(_,_,user,lInR),W1,C,X,_HYPS,_D,_CALLS,abox,InHead),
	not(not((retract((InHead :- call(user:G1))) ;
 	 retract((InHead :- call(G1)))))),
	not(not((retract((conceptElement(KB,_,W1,_,X,C,_) :- call(user:G1))) ;
	 retract((conceptElement(KB,_,W1,_,X,C,_) :- call(G1)))))).
%	 retractall(((InHead :- call(user:G1)))),
%	 retractall(((InHead :- call(G1)))),
%	 retractall(((conceptElement(KB,_,W1,_,X,C,_) :- call(user:G1)))),
%	 retractall(((conceptElement(KB,_,W1,_,X,C,_) :- call(G1)))).
delete_ind(P1,X,Y,R) :-
	completeParameter([(X,Y,R)],KBName,CTX,_,_),
	delete_ind(KBName,CTX,X,Y,R).
delete_ind(KBName,CTX,X,Y,R) :-
	knowledgeBase(KBName,KB,_),
	atomic(X),
	atomic(Y),
	atomic(R),
	retractall(axiom(KB,CTX,assert_ind(CTX,X,Y,R))),
	Role1 =.. [R,X,Y],
	retract(Role1),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	constructEqHead(KB,rn(_,_,user,lInR),W1,Y,_FF,R,X,_,_D,CALLS,abox,EqLiteral),
	not(not((retract((EqLiteral :- (cCS(CALLS,true), call(user:G1))));
	 retract((EqLiteral :- (cCS(CALLS,true), call(G1))))))),
	not(not((retract((roleElement(KB,_,W1,I,X,Y,R,_) :- call(user:G1))) ;
	 retract((roleElement(KB,_,W1,I,X,Y,R,_) :- call(G1)))))).
%	retractall((EqLiteral :- (cCS(CALLS,true), call(user:G1)))),
%	retractall((EqLiteral :- (cCS(CALLS,true), call(G1)))),
%	retractall((roleElement(KB,_,W1,X,Y,R,_) :- call(user:G1))),
%	retractall((roleElement(KB,_,W1,X,Y,R,_) :- call(G1))).

	
/***
*    change_classifier veraendert die Sub.Hierarchie nach undefconcept      
***/
change_classifier(CN,CT) :-
	change_classifier([],CN,CT),
	!.
change_classifier(CTX,CN,CT) :-
	getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,_),
	change_classifier(KBName,CTX,CN,CT),
	!.

change_classifier(KBName,CTX,CN,CT) :-
	knowledgeBase(KBName,KB,_),
	getConceptName(KB,[],CN),
	getAllSubConcepts(KBName,CTX,CN,CL1),
	find_definition(CN,CL2),
 	union1(CL1,CL2,CL),
% removed test output
% uh 31.05.94
% cmp_print(CL),
	change_classifier1(KB,CTX,CN,CT,CL),
	delete_hierarchy(concepts,KB,CTX,CN),
	change_classifier2(KB,CTX,CT,CL),
	!.
change_classifier(KBName,CTX,RN,CT) :-
	knowledgeBase(KBName,KB,_),
	getRoleName(KB,[],RN),
	getAllSonRoles(KBName,CTX,RN,CL1),
	find_definition(RN,CL2),
 	union1(CL1,CL2,CL),
% removed test output
% uh 31.05.94
% cmp_print(CL),
	change_classifier1(KB,CTX,RN,CT,CL),
	delete_hierarchy(roles,KB,CTX,RN),
	change_classifier2(KB,CTX,CT,CL),
	!.
change_classifier(KBName,CTX,CN,CT).

change_classifier1(KB,CTX,CR,CT,[]) :-
	!.
change_classifier1(KB,CTX,CR,CT,[H|T]) :-
	getConceptName(KB,CTX,H),
	(H \== 'Entity',H \== bot),
	delete_hierarchy(concepts,KB,CTX,H),
	change_classifier1(KB,CTX,CR,CT,T),
	!.
change_classifier1(KB,CTX,CR,CT,[H|T]) :-
	getRoleName(KB,CTX,H),
	(H \== 'Entity',H \== bot),
	delete_hierarchy(roles,KB,CTX,H),
	change_classifier1(KB,CTX,CR,CT,T),
	!.
change_classifier1(KB,CTX,CR,CT,[H|T]) :-
	change_classifier1(KB,CTX,CR,CT,T),
	!.

change_classifier2(KB,CTX,CT,[]) :-
	!.
change_classifier2(KB,CTX,CT,[H|T]) :-
	getConceptName(KB,CTX,H),
	not(make_succ2(concepts,KB,CTX,H)),
	change_classifier2(KB,CTX,CT,T),
	!.
change_classifier2(KB,CTX,CT,[H|T]) :-
	getRoleName(KB,CTX,H),
	not(make_succ2(roles,KB,CTX,H)),
	change_classifier2(KB,CTX,CT,T),
	!.
change_classifier2(KB,CTX,CT,[H|T]) :-
	change_classifier2(KB,CTX,CT,T),
	!.
delete_hierarchy(Type,KB,CTX,CR) :-
        clause(succ(Type,KB,CTX,CR,SC),_),
	clause(succ(Type,KB,CTX,PC,CR),_),
	subsume1(Type,KB,CTX,PC,SC),
	assert_succ(Type,KB,CTX,PC,SC),
	fail.
delete_hierarchy(Type,KB,CTX,CR) :-
	retractall(succ(Type,KB,CTX,CR,_)),
	retractall(succ(Type,KB,CTX,_,CR)),
	retractall(sub(Type,KB,CTX,CR,_)),
	retractall(sub(Type,KB,CTX,_,CR)),
	retractall(nsub(Type,KB,CTX,CR,_)),
	retractall(nsub(Type,KB,CTX,_,CR)),
	!.
	
/*****************************************************************************
*      find_definition   sucht alle concepte die irgendwie mit dem 	     *
* 			  zu loeschenden concept verbunden sind.             *
*****************************************************************************/

find_definition(Concept,Liste) :-
	find_definition([],Concept,Liste).
find_definition(CTX,Concept,Liste) :-
	getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,_),
	find_definition(KB,CTX,Concept,Liste),
	!.
find_definition(KB,CTX,Concept,Liste) :-
	getConceptName(KB,CTX,Concept),
	find_conceptEqualSets(KB,_user,CTX,Concept,CT1,_),
	collect(CT1,List1),
	find_conceptSubsets(KB,_user,CTX,Concept,CT2,_),
	collect(CT2,List2),
	union1(List1,List2,List3),
	find_Def_concept(CTX,Concept,List_of_Concepts),
	union1(List3,List_of_Concepts,Liste),
	!.
find_definition(KB,CTX,Role,Liste) :-
	getRoleName(KB,CTX,Role),
	find_roleEqualSets(KB,_user,CTX,Role,CT1,_),
	collect(CT1,List1),
	find_roleSubsets(KB,_user,CTX,Role,CT2,_),
	collect(CT2,List2),
	union1(List1,List2,List3),
	find_Def_role(CTX,Role,List_of_Roles),
	union1(List3,List_of_Roles,Liste),
	!.

find_conceptEqualSets(KB,_user,CTX,Concept,CT1,_) :-
	conceptEqualSets(KB,_user,CTX,Concept,CT1,_),
	!.
find_conceptEqualSets(KB,_user,CTX,Concept,[],_).
find_conceptSubsets(KB,_user,CTX,Concept,CT2,_) :-
	conceptSubsets(KB,_user,CTX,Concept,CT2,_),
	!.
find_conceptSubsets(KB,_user,CTX,Concept,[],_).

find_roleEqualSets(KB,_user,CTX,Role,CT1,_) :-
	roleEqualSets(KB,_user,CTX,Role,CT1,_),
	!.
find_roleEqualSets(KB,_user,CTX,Role,[],_).
find_roleSubsets(KB,_user,CTX,Role,CT2,_) :-
	roleSubsets(KB,_user,CTX,Role,CT2,_),
	!.
find_roleSubsets(KB,_user,CTX,Role,[],_).


find_Def_concept(CTX,Concept,List_of_Concepts) :-
	getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,_),
	find_Def_concept(KB,CTX,Concept,List_of_Concepts).
% Removed redundant code 
% 15.02.93 uh
% find_Def_concept(CTX,Concept,List_of_Concepts) :-
%	getCurrentKnowledgeBase(KBName),
%	knowledgeBase(KBName,KB,_),
%	find_Def_concept(KB,CTX,Concept,List_of_Concepts).
find_Def_concept(KB,CTX,Concept,List_of_Concepts) :-
	setofOrNil(CN,find_Def_concept1(KB,CTX,Concept,CN),List_of_Concepts),
	!.

find_Def_concept1(KB,CTX,Concept,CN) :-
	conceptEqualSets(KB,_user,CTX,CN,CT,AX),
	atom(CN),
	not(atom_codes(CN,[99,111,110,99,101,112,116|_])),  %string "concept"
	collect(CT,Liste),
	member(Concept,Liste).
find_Def_concept1(KB,CTX,Concept,CN) :-
	conceptSubsets(KB,_user,CTX,CN,CT,AX),
	atom(CN),
	not(atom_codes(CN,[99,111,110,99,101,112,116|_])),      %string "concept"
	collect(CT,Liste),
	member(Concept,Liste).
find_Def_role(CTX,Role,List_of_Roles) :-
	getCurrentKnowledgeBase(KBName),
	knowledgeBase(KBName,KB,_),
	find_Def_role(KB,CTX,Role,List_of_Roles).
find_Def_role(KB,CTX,Role,List_of_Roles) :-
	setofOrNil(CN,find_Def_role1(KB,CTX,Role,CN),List_of_Roles),
	!.
find_Def_role1(KB,CTX,Role,CN) :-
	roleEqualSets(KB,_user,CTX,CN,CT,AX),
	atom(CN),
	not(atom_codes(CN,[99,111,110,99,101,112,116|_])),	    %string "concept"
	collect(CT,Liste),
	member(Role,Liste).
find_Def_role1(KB,CTX,Role,CN) :-
	roleSubsets(KB,_user,CTX,CN,CT,AX),
	atom(CN),
	not(atom_codes(CN,[99,111,110,99,101,112,116|_])),     %string "concept"
	collect(CT,Liste),
	member(Role,Liste).
/****************************************************************************/
/*       collect      sammelt aus oneer bel.verschachtelten kette von      
*/
/*                    ausdruecken alle Concept'e oder Role's .             
*/
/***************************************************************************/	

collect(CT,Liste) :-
	collect1(CT,L),
	(option(prover,Decider) ->
	    Liste = L;
	    collect2(L,Liste)),
	!.
collect2([],[]).
collect2([H|T],[H|L]) :-
	currentKnowledgeBase(KB),
	nonvar(H),
	(clause(conceptName(KB,_,_,H),_);clause(roleName(KB,_,_,H),_)),
	collect2(T,L),
	!.
collect2([H|T],L) :-
	collect2(T,L),
	!.
collect1([],L) :-
	!.
collect1([X|R],L) :-
	(atom(X);number(X)),
	collect1(R,L1),
	union1(X,L1,L),
	!.
collect1([X|R],L) :-
%	option(prover,_Decider),
	isSlot(X),
	collect1(R,L),
	!.
collect1([[X1|R1]|R],L) :-
	(atom(X1);number(X1)),
	collect1(R1,L1),
	collect1(R,L2),
	union1(L1,L2,L3),
	union1(L3,X1,L),
	!.
collect1([[X1|R1]|R],L) :-
	X1 =.. X2,
	collect1(X2,L2),
	collect1(R1,L1),
	collect1(R,L3),
	union1(L1,L2,L4),
	union1(L4,L3,L),
	!.
collect1([X|R],L3) :-
	X =.. X1,
	collect1(X1,L1),
	collect1(R,L2),
	union1(L1,L2,L3),
	!.
collect1(X,[X|L]) :-
	(atom(X);number(X)),
	currentKnowledgeBase(KB),
	(clause(conceptName(KB,_,_,X),_);clause(roleName(KB,_,_,X),_)),
	!.
collect1(X,[]) :-
%	option(prover,_Decider),
	isSlot(X),
	!.

collect1(X,[L|L1]) :-
	X =.. X1,
	collect1(X1,L1),!.

delete_conceptName(C) :-
	getCurrentKnowledgeBase(KBName),
	delete_conceptName(KBName,_CTX,C).
delete_conceptName(CTX,C) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	delete_conceptName(KBName,CTX,C).
delete_conceptName(KBName,C) :-
	delete_conceptName(KBName,_CTX,C).
delete_conceptName(KBName,CTX,C) :-
	knowledgeBase(KBName,KB,_),
	retractall(conceptName(KB,CTX,_W,C)),
	!.

delete_roleName(C) :-
	getCurrentKnowledgeBase(KBName),
	delete_roleName(KBName,_CTX,C).
delete_roleName(CTX,C) :-
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	delete_roleName(KBName,CTX,C).
delete_roleName(KBName,C) :-
	delete_roleName(KBName,_CTX,C).
delete_roleName(KBName,CTX,R) :-
	knowledgeBase(KBName,KB,_),
	retractall(roleName(KB,CTX,_W,R)),
	!.
/**********************************************************************
 *
 * @(#) sb2.pll 1.20@(#)

 *
 */

/*-------------------------------------------------------------------------------
 * Der Konstruktor: sigma_defKnowlegeBase(EName,Comment)
 * generiert one neues KnowledgeBase und bindet dies ans uebergebene Symbol EName,
 * EName wird also zu current knowledgeBase.
 * Comment kann irgendone string sone.
 *-----------------------------------------*/

  
  sigma_defKnowlegeBase(EName,Comment):- makeKnowledgeBase(EName,Comment).


/*-------------------------------------------------------------------------------
 * sigma_initenv
 * loescht den Inhalt onees knowledgeBase, und initialisiert den instance sigma_defenv schon
 * definierten EName als Tbox-KnowledgeBase.
 *----------------------------------------*/

 sigma_initenv:- 
	initKnowledgeBase.
 sigma_initKnowlegeBase(KBName) :- 
	initKnowledgeBase(KBName).


/*-------------------------------------------------------------------------------
 * Der 4-stellige Hilfskonstruktor make__primconcept mit folgenden Parametern:
 * (KBName,CTX,CName1,supers(Liste_von_Konzepte))  
 * definiert one Konzept CName1 als Subkonzept von den instance der Liste auftretenden 
 * Superkonzepten instance knowledgeBase KBName und modal context CTX.
 *-------------------------------------------*/


make_primconcept(KBName,CTX,CName1,supers([])).
make_primconcept(KBName,CTX,CName1,supers([X|T])):-
		  defprimconcept(KBName,CTX,CName1,X),
		  make_primconcept(KBName,CTX,CName1,supers(T)).

/*------------------------------------------------------------------------------
 * make_primconcept(KBName,CTX,CName1,restr-inh(RName1,restricts(RName2,
 *                                                     range(CName2,CNameDef))))
 * 
 * definiert one Konzept CName1, an dem onee Rolle RName2  auf den Rollenfueller
 * CName2 (range) restrigniert wird.
 * Und zwar instance evironment=KBName und modal context CTX. 
 * Der neuen Rolle wird das Symbol RName1 zugewiesen,
 * die Domain-Menge dieser neuen Rolle (CNameDom) ist gerade onee Teilmenge von 
 * CName1.
 *----------------------------------------*/
make_primconcept(KBName,CTX,CName1,
                'restr-inh'(RName1,restricts(RName2,range(CName2,CNameDef)))):-
	make_primconcept(KBName,CTX,CName1,
	                 restrict_inh(RName1, restricts(RName2 ,
				 		        range(CName2,CNameDef)))).
make_primconcept(KBName,CTX,CName1,
                'restrict-inh'(RTerm1,restricts(RTerm2,range(CName2,CNameDef)))):-
       make_primconcept(KBName,CTX,CName1,
                        restrict_inh(RTerm1, restricts(RTerm2,
						       range(CName2,CNameDef)))).

make_primconcept(KBName,CTX,CName1,restrict_inh(RTerm1, restricts(RTerm2 ,
						     range(CName2,CNameDef)))):-
	expand_role(KBName,CTX,RTerm1,RName1,_,_,_),
	expand_role(KBName,CTX,RTerm2,RName2,_,_,_),
	knowledgeBase(KBName,KB,_),
	defrole(KBName,CTX,RName1 , restr(RName2 , CName2)),
	gensym(concept,CNameDom),
	defconcept(KBName,CTX,CNameDom ,some(RName2 ,'Entity')),
        defprimconcept(KBName,CTX,CNameDom ,CName1),
%	defprimconcept(KBName,CTX,and([some(inverse(RName1),'Entity'),
%				       naf(not(CNameDef))]),CNameDef),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleDomain(KB,W1,RName1,CNameDom) :- G1)),
	assertz((roleRange(KB,W1,RName1,CName2) :- G1)),
	assertz((roleDefault(KB,W1,RName1,CNameDef) :- G1)),
	assertz((roleTripel(KB,W1,RName1,CNameDom,CName2,CNameDef))).
   
/*----------------------------------------------------------------------------
 * make_primconcept(KBName,CTX,CName1, nr(RName1,MinNr,MaxNr,DefNr))
 * definiert one Konzept CName1, an dem onee "number restriction" stattfindet:
 * die Rolle RName1 wird bzgl. ihrer Kardinalitaet restrigniert auf
 * (MinNr,MaxNr,DefNr),
 * und zwar instance  knowledgeBase=KBName und modal context=CTX,
 * die Dom-Menge der Rolle RName1 ist dann one CNameDom als Teilmenge von CName1.
 *-----------------------------------------*/


make_primconcept(KBName,CTX,CName1 , nr(RTerm1, MinNr,MaxNr,DefNr)):-
	knowledgeBase(KBName,KB,_),
	expand_role(KBName,CTX,RTerm1,RName1,CNameDomT,CName2T,CNameDefT),
	gensym(concept,CNameDom),
	defconcept(KBName,CTX,CNameDom, and([atleast(MinNr,RName1),atmost(MaxNr,RName1)])),
	defconcept(KBName,CTX,CNameDom, some(RName1,'Entity')), 
	defprimconcept(KBName,CTX,CNameDom , CName1),
%	gensym(concept,CNameDef),
%	defconcept(KBName,CTX,CNameDef, and([atleast(DefNr,RName1),atmost(DefNr,RName1)])),
%	defprimconcept(KBName,CTX,and([some(inverse(RName1)),naf(not(CNameDef))]),CNameDef),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleNr(KB,W1,RName1,MinNr,MaxNr) :- G1)),
	assertz((roleDefNr(KB,W1,RName1,DefNr) :- G1)),
	assertz((roleAll(KB,W1,Rname1,CNameDomT,CName2T,CNameDefT,MinNr,MaxNr,DefNr))).



/*------------------------------------------------------------------------------
 *sigma_primconcept(CName)
 * definiert one generelles Konzept CName instance modal context [].
 *---------------------------------------*/


 sigma_primconcept(CName):- 
	getCurrentKnowledgeBase(KBName),
	sigma_primconcept(KBName,[],CName).


/*-------------------------------------------------------------------------------
 * Der 2-stellige Konstruktor sigma_primconcept mit jeweils folgenden Parametern:
 * (KBName,CName1), oder (CTX,CName1) definiert one generelles Konzept CName1
 * instance invironment KBName und modal context [], bzw. instance current KnowledgeBase und
 * modal context CTX.
 *----------------------------------------*/
  
sigma_primconcept(KBName,Left) :-
	knowledgeBase(KBName,_,_),
	!,
	sigma_primconcept(KBName,[],Left),
	!.


sigma_primconcept(CTX,Left) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_primconcept(KBName,CTX,Left),
	!.



/*-----------------------------------------------------------------------------
 * Der 2-stellige Konstruktor sigma_primconcept(CName1,SpecListe) hat als Argumente one
 * CName1 und onee Liste von bis jetzt onegef"uhrten Pr"adikaten wie:
 * [supers(Liste von concepts) , restr-inh(...) , nr(...)], es werden dann jeweils die
 * entsprechenden Hilfskonstruktoren mit dem onezelnen Listenelement als aktueller Parameter 
 * aktiviert bzw. aufgerufen und zwar instance current knowledgeBase und [] als modal context.
 *-----------------------------------------*/

sigma_primconcept(CName1,[]) :-
	!.
sigma_primconcept(CName1,[X|T]):-
	getCurrentKnowledgeBase(KBName),
	sigma_primconcept(KBName,[],CName1,[X|T]),
	!.



/*-----------------------------------------------------------------------------
 * Der 3-stellige Konstruktor sigma_primconcept(A,CName1,SpecListe) funktioniert analog
 * nur instance A= KnowledgeBase und [] als modal context, bzw. instance current knowledgeBase und
 * A=CTX als modal context.
 *-----------------------------------------*/

sigma_primconcept(A,CName1,[]) :-
	!.
sigma_primconcept(A,CName1,[X|T]):-
	knowledgeBase(A,_,_),
	!,
	sigma_primconcept(A,[],CName1,[X|T]),
	!.
sigma_primconcept(A,CName1,[X|T]):-
	(A = [] ; A = [_|_]),
	getCurrentKnowledgeBase(KBName),
	!,
	sigma_primconcept(KBName,A,CName1,[X|T]).

/*-------------------------------------------------------------------------------
 * Der 3-stellige Konstruktor von  sigma_primconcept mit folgenden Parametern:
 * (KnowledgeBase,CTX,CName1)  
 * definiert one Konzept CName1 instance knowledgeBase KBName und modal context CTX. 
 *-------------------------------------------*/

sigma_primconcept(KBName,CTX,CName):-
	   knowledgeBase(KBName,KB,_),
	   (CTX = [] ; CTX = [_|_]),
	   defprimconcept(KBName,CTX,CName),
	   !.

/*-----------------------------------------------------------------------------
 * Der 4-stellige Konstruktor sigma_primconcept(KBName,CTX,CName1,SpecListe) wird aktiviert 
 * mit expliziter Angabe von KnowledgeBase=KBName und modal context =CTX.
 *-----------------------------------------*/

sigma_primconcept(KBName,CTX,CName1,[]) :-
	!.
sigma_primconcept(KBName,CTX,CName1,[X|T]):-
	make_primconcept(KBName,CTX,CName1,X),
        sigma_primconcept(KBName,CTX,CName1,T),
	!.

/*------------------------------------------------------------------------------
 * make_defconcept(KBName,CTX,CName1,restr-inh(RName1,restricts(RName2,
 *                                          range(CName2,CNameDef))),CNameDom)
 * 
 * definiert one Konzept CName1, an dem onee Rolle RName2  auf den Rollenfueller
 * CName2 (range) restrigniert wird.
 * Und zwar instance evironment=KBName und modal context CTX. 
 * Der neuen Rolle wird das Symbol RName1 zugewiesen,
 * die Domain-Menge dieser neuen Rolle (CNameDom) ist gerade onee Teilmenge von 
 * CName1.
 *----------------------------------------*/

make_defconcept(KBName,CTX,CName1,restrict_inh(RName1, restricts(RName2 ,
				     range(CName2,CNameDef))),CNameDom):-
	make_defconcept(KBName,CTX,CName1,'restr-inh'(RName1, restricts(RName2 ,
                        range(CName2,CNameDef))),CNameDom).
make_defconcept(KBName,CTX,CName1,'restrict-inh'(RName1, restricts(RName2 ,
				     range(CName2,CNameDef))),CNameDom):-
	make_defconcept(KBName,CTX,CName1,'restr-inh'(RName1, restricts(RName2 ,
                        range(CName2,CNameDef))),CNameDom).

make_defconcept(KBName,CTX,CName1,'restr-inh'(RName1, restricts(RName2 ,
				     range(CName2,CNameDef))),CNameDom):-
	knowledgeBase(KBName,KB,_),
	defrole(KBName,CTX,RName1 , restr(RName2 , CName2)),
	gensym(concept,CNameDom),
	defconcept(KBName,CTX,CNameDom ,some(RName2 ,'Entity')),
%	defprimconcept(KBName,CTX,and([some(inverse(RName1),'Entity'),
%				       naf(not(CNameDef))]),CNameDef),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleDomain(KB,CTX,RName1,CNameDom) :- G1)),
	assertz((roleRange(KB,CTX,RName1,CName2) :- G1)),
	assertz((roleDefault(KB,CTX,RName1,CNameDef) :- G1)),
	assertz((roleTripel(KB,CTX,RName1,CNameDom,CName2,CNameDef))).
    
/*----------------------------------------------------------------------------
 * make_defconcept(KBName,CTX,CName1, nr(RName1,MinNr,MaxNr,DefNr),CNameDom)
 * definiert one Konzept CName1, an dem onee "number restriction" stattfindet:
 * die Rolle RName1 wird bzgl. ihrer Kardinalitaet restrigniert auf
 * (MinNr,MaxNr,DefNr),
 * und zwar instance  knowledgeBase=KBName und modal context=CTX.
 *-----------------------------------------*/


make_defconcept(KBName,CTX,CName1 , nr(RTerm, MinNr,MaxNr,DefNr),CNameDom):-
	knowledgeBase(KBName,KB,_),
	expand_role(KBName,CTX,RTerm,RName1,CNameDomT,CNameT,CNameDefT),
	gensym(concept,CNameDom),
	defconcept(KBName,CTX,CNameDom, and([atleast(MinNr,RName1),atmost(MaxNr,RName1)])),
%	defconcept(KBName,CTX,CNameDom, some(RName1,'Entity')), 
%	gensym(concept,CNameDef),
%	defconcept(KBName,CTX,CNameDef, and([atleast(DefNr,RName1),atmost(DefNr,RName1)])),
%	defprimconcept(KBName,CTX,and([some(inverse(RName1)),naf(not(CNameDef))]),CNameDef),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleNr(KB,W1,RName1,MinNr,MaxNr) :- G1)),
	assertz((roleDefNr(KB,W1,RName1,DefNr) :- G1)),
	assertz((roleAll(KB,W1,RName1,CNameDomT,CNameT,CNameDefT,MinNr,MaxNr,DefNr) :- G1)).

make_defconcept(KBName,CTX,CName1 , necres(RTerm, nec),CNameDom):-
	!,
	knowledgeBase(KBName,KB,_),
	expand_role(KBName,CTX,RTerm,RName1,_,_,_),
	gensym(concept,CNameDom),
	defconcept(KBName,CTX,CNameDom,atleast(1,RName1)),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleDomain(KB,W1,RName1,CNameDom) :- G1)),
	!.
make_defconcept(KBName,CTX,CName1 , necres(RTerm, _),CNameDom):-
	!,
	knowledgeBase(KBName,KB,_),
	expand_role(KBName,CTX,RTerm,RName1,_,_,_),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleDomain(KB,W1,RName1,CNameDom) :- G1)),
	!.


expand_role(_,_,RTerm,RTerm,_,_,_) :-
	atomic(RTerm),
	!.
expand_role(KBName,CTX1,RTerm,RName1,CName1,CName2,CNameDef) :-
	RTerm = sigma_primelemrole(KBName,CTX2,RName1, 'domain-range'(CName1,CName2,CNameDef)),
	!,
	append(CTX1,CTX2,CTX3),
	sigma_primelemrole(KBName,CTX3,RName1, 'domain-range'(CName1,CName2,CNameDef)).
expand_role(KBName,CTX1,RTerm,RName1,CName1,CName2,CNameDef) :-
	RTerm = sigma_primelemrole(CTX2,RName1, 'domain-range'(CName1,CName2,CNameDef)),
	!,
	append(CTX1,CTX2,CTX3),
	sigma_primelemrole(CTX3,RName1, 'domain-range'(CName1,CName2,CNameDef)).
expand_role(KBName,CTX1,RTerm,RName1,CName1,CName2,CNameDef) :-
	RTerm = sigma_primelemrole(RName1, 'domain-range'(CName1,CName2,CNameDef)),
	!,
	sigma_primelemrole(CTX1,RName1, 'domain-range'(CName1,CName2,CNameDef)).
expand_role(_,_,RTerm,RTerm,_,_,_).


/*-----------------------------------------------------------------------------
 * elem(I,Liste,H) nimmt das I-te Element oneer Liste und sieht so aus:
 *-----------------------------------------*/


elem(1,[H|T],H).
elem(I,[H|T],X):- Hilf is I-1,
		  elem(Hilf,T,X).


/*-----------------------------------------------------------------------------
 * Der 2-stellige Konstruktor sigma_defconcept(CName1,SpecListe) hat als Argumente one
 * CName1 und onee Liste von bis jetzt onegef"uhrten Pr"adikaten wie:
 * [supers(Liste von concepts) , restr-inh(...) , nr(...)], es werden dann jeweils die
 * entsprechenden Hilfskonstruktoren mit dem onezelnen Listenelement als aktueller Parameter 
 * aktiviert bzw. aufgerufen und zwar instance current knowledgeBase und [] als modal context.
 *-----------------------------------------*/

sigma_defconcept(C1,SpecList):- 
	getCurrentKnowledgeBase(KBName),
	sigma_defconcept(KBName,[],C1,SpecList),
	!.

/*-----------------------------------------------------------------------------
 * Der 3-stellige Konstruktor sigma_defconcept(A,CName1,SpecListe) funktioniert analog
 * nur instance A= KnowledgeBase und [] als modal context, bzw. instance current knowledgeBase und
 * A=CTX als modal context.
 *-----------------------------------------*/


sigma_defconcept(A,C1,SpecList) :- 
	knowledgeBase(A,_,_),
	!,
	sigma_defconcept(A,[],C1,SpecList),
	!.
sigma_defconcept(A,C1,SpecList) :-
	(A = [] ; A = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_defconcept(KBName,A,C1,SpecList),
	!.

/*-----------------------------------------------------------------------------
 * Der 4-stellige Konstruktor sigma_defconcept(KBName,CTX,CName1,SpecListe) wird aktiviert 
 * mit expliziter Angabe von KnowledgeBase=KBName und modal context =CTX.
 *-----------------------------------------*/


sigma_defconcept(KBName,CTX,C1,SpecList) :-
	sigma_defconcept(KBName,CTX,C1,SpecList,[]).

sigma_defconcept(KBName,CTX,C1,[],[]) :-
	!.
sigma_defconcept(KBName,CTX,C1,[],CList) :-
	!,
	defconcept(KBName,CTX,C1,and(CList)),
	!.
sigma_defconcept(KBName,CTX,C1,[supers(L)|SpecList],CList1) :-
	!,
	append(L,CList1,CList2),
	sigma_defconcept(KBName,CTX,C1,SpecList,CList2).
sigma_defconcept(KBName,CTX,C1,[Spec1|SpecList],CList1) :-
	make_defconcept(KBName,CTX,C1,Spec1,Concept),
	sigma_defconcept(KBName,CTX,C1,SpecList,[Concept|CList1]).
sigma_defconcept(KBName,CTX,C1,supers(L),[]) :-
	!,
	defconcept(KBName,CTX,C1,and(L)).

/*---------------------------------------------------------------------------
 * sigma_primelemrole(RName1,domain-range(CName1,CName2,CNameDef))
 * definiert onee neue generelle Rolle RName1 mit CName1 als domain, CName2 
 * als range und CNameDef als "default value restriction" instance modal context [].
 *------------------------------------------*/


sigma_primelemrole(RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	getCurrentKnowledgeBase(KBName),
	sigma_primelemrole(KBName,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).

/*---------------------------------------------------------------------------
 * sigma_primelemrole(X,RName1,domain-range(CName1,CName2,CNameDef))
 * definiert onee neue generelle Rolle RName1 mit CName1 als domain, CName2 
 * als range und CNameDef als "default value restriction" instance modal context []
 * und X=knowledgeBase bzw. instance modal context X=CTX und current knowledgeBase.
 *------------------------------------------*/


sigma_primelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	knowledgeBase(X,_,_),
	sigma_primelemrole(X,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).
sigma_primelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	(X = [] ; X = [_|_]),
	getCurrentKnowledgeBase(KBName),
	sigma_primelemrole(KBName,X,RName1, 'domain-range'(CName1,CName2,CNameDef)).

/*---------------------------------------------------------------------------
 * sigma_primelemrole(KBName,CTX,RName1,domain-range(CName1,CName2,CNameDef))
 * definiert onee neue generelle Rolle RName1 mit CName1 als domain, CName2 
 * als range und CNameDef als "default value restriction" instance modal context CTX 
 * und knowledgeBase KBName.
 *------------------------------------------*/

sigma_primelemrole(KBName,CTX,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	knowledgeBase(KBName,KB,_),
	defprimconcept(KBName,CTX,CName1,some(RName1,'Entity')),
	defprimconcept(KBName,CTX,some(inverse(RName1),'Entity'),CName2),
%	defprimconcept(ENvName,CTX,and([CName2,naf(not(CNameDef))]),CNameDef),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleDomain(KB,W1,RName1,CName1) :- G1)),
	assertz((roleRange(KB,W1,RName1,CName2) :- G1)),
	assertz((roleDefault(KB,W1,RName1,CNameDef) :- G1)),
	assertz((roleTripel(KB,W1,RName1,CName1,CName2,CNameDef) :- G1)),
	!.

/*----------------------------------------------------------------------------
 * sigma_defelemrole(RName1,restricts(RName2,range(CName1,CNameDef)))
 * restrigniert die Rolle RName2 bzgl. des Rollenfuellers, auf CName1,
 * instance modal context [] und current knowledgeBase.
 * (CNameDef gibt den Default-Rollenfueller an).
 *------------------------------------------*/

sigma_defelemrole(RName1, restricts(RName2, range(CName1,CNameDef))):-
	getCurrentKnowledgeBase(KBName),
	sigma_defelemrole(KBName,[],RName1,restricts(RName2,range(CName1,CNameDef))),
	!.

/*----------------------------------------------------------------------------
 * sigma_defelemrole(X,RName1,restricts(RName2,range(CName1,CNameDef)))
 * restrigniert die Rolle RName2 bzgl. des Rollenfuellers, auf CName1,
 * instance modal context [] und X=knowledgeBase bzw. instance modal context X=CTX und 
 * current knowledgeBase.
 *------------------------------------------*/


sigma_defelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
	knowledgeBase(X,_,_),
	!,
	sigma_defelemrole(X,[],RName1, restricts(RName2, range(CName1,CNameDef))).
sigma_defelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
	(X = [] ; X = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_defelemrole(KBName,X,RName1, restricts(RName2, range(CName1,CNameDef))).

/*----------------------------------------------------------------------------
 * sigma_defelemrole(KBName,CTX,RName1,restricts(RName2,range(CName1,CNameDef)))
 * restrigniert die Rolle RName2 bzgl. des Rollenfuellers, auf CName1,
 * instance modal context CTX und  knowledgeBase KBName. 
 *------------------------------------------*/


sigma_defelemrole(KBName,CTX,RName1, restricts(RName2, range(CName1,CNameDef))):-
	knowledgeBase(KBName,KB,_),
	defrole(KBName,CTX,RName1,restr(RName2,CName1)),
%	defprimconcept(KBName,CTX,and([some(inverse(RName1),'Entity'),
%                                      naf(not(CNameDef))]),CNameDef),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleRange(KB,CTX,RName1,CName1) :- G1)),
	assertz((roleDefault(KB,CTX,RName1,CNameDef) :- G1)),
	!.



/*----------------------------------------------------------------------------
 * sigma_disjoint(CName1,CName2)
 * definiert generelle Konzepte CName1,CName2 als disjunkt.
 *-------------------------------------------*/

		     
sigma_disjoint(CName1,CName2):- 
%	defprimconcept(CName1,not(CName2)),
	defprimconcept(CName2,not(CName1)),
	!.


sigma_disjoint(X,CName1,CName2):- 
%	defprimconcept(X,CName1,not(CName2)),
	defprimconcept(X,CName2,not(CName1)),
	!.


sigma_disjoint(KBName,CTX,CName1,CName2):- 
%	defprimconcept(KBName,CTX,CName1,not(CName2)),
	defprimconcept(KBName,CTX,CName2,not(CName1)),
	!.



/*----------------------------------------------------------------------------
 * sigma_expand(CName1,CName2)
 * erlaubt die Erweiterung der Definition onees bereits existierenden Konzeptes,
 * der Konstruktor fuegt zu oneem bestehenden Konzept one weiteres "Vaterkonzept
 * hinzu, die Moeglichkeit der Erweiterung ist also analog zur Definition der 
 * Konzepte.
 *------------------------------------------*/

 /*? sigma_expand(CName1,CName2):- defprimconcept(CName1,CName2)??*/






make_irole(KBName,CTX,ICName1,irole(RName,iname(IRName),
                                    nr(MinNr,MaxNr,DefNr))):-
	knowledgeBase(KBName,KB,_),
%       defprimrole(KBName,CTX,IRName,restr(RName,
%	                                    and([atleast(MinNr,RName),
%			  		    atmost(MaxNr,RName),
%					    some(inverse(RName),'Entity')]))),
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	assertz((roleNr(KB,W1,IRName,MinNr,MaxNr) :- G1)),
	assertz((roleDefNr(KB,W1,IRName,DefNr) :- G1)).
                           


make_irole(KBName,CTX,ICName1,irole(RName,iname(IRName),vr(ICName2))) :-
        knowledgeBase(KBName,KB,_),
%	gensym(concept,CName2),
%       defprimrole(KBName,CTX,IRName, restr(RName,
%                 			     CName2)),
	getRoleDomain(KB,CTX,RName,CDom),
	getRoleRange(KB,CTX,RName,CRang),
	!,
	consistCheck(KBName,CTX,ICName1,CDom),
	!,
	consistCheck(KBName,CTX,ICName2,CRang),
	!,
%	consistCheck(KBName,CTX,ICName2,CName2),
	assert_ind(KBName,CTX,ICName1,ICName2,IRName).

constructRestriction(RName,[],['Entity']) :-
	!.
constructRestriction(RName,[nr(MinNr,MaxNr,DefNr)|L1],
                     [atleast(MinNr,'Entity'), atmost(MaxNr,'Entity') | L2]) :-
	constructRestriction(RName,L1,L2),
	!.
constructRestriction(RName,[vr(ICName2)|L1],[ICName2|L2]) :-
	constructRestriction(RName,L1,L2),
	!.
     
 /*-------------------------------------------------------------------------------
  * make_defelem(ICName,element(CName))
  * individualisiert one Konzept CName mit dem Instanz-Namen ICName vom Typ string,
  * d.h. es wird das Abox-Element ICName zu Konzept hinzugefuegt und zwar instance modal
  * context [] bzw CTX.
  *-----------------------------------------*/


consistCheck(KB,CTX,IC,Concept) :- 
% vor dem Test die Normalform von not(Concept) mittels
% normalizeNot(not(Concept),NotConcept)
% bilden und dann
% sigma_ask(KB,CTX,(element(IC,NotConcept)))
% aufrufen
			 normalizeNot(not(Concept),NotConcept),
			 sigma_ask(KB,CTX,(element(IC,NotConcept))),
			 cmp_nl,
			 cmp_write('--- impossible --- '),
			 !,
			 fail.

			    



consistCheck(KB,CTX,IC,Concept) :-
			          !.



make_defelem(KBName,CTX,ICName,element(CName)):-
	  consistCheck(KBName,CTX,ICName,CName),
	  assert_ind(KBName,CTX,ICName,CName),
	  !.





/*--------------------------------------------------------------------------------
 * make_defelem(KBName,CTX,ICName1,irole(RName,iname(IRName),[nr(MinNr,MaxNr,DefNr),vr(ICName2)]))
 * erzeugt onee Instanz ICName1 fuer one Konzept, an dem auch die Rolle RName
 * individualisiert wird mit dem Instanznamen IRName,
 * der Kardinalitaet der indiv. Rolle nr(MinNr,MaxNr,DefNr),
 * dem Rollenfueller der indiv. Rolle vr(ICName2),
 * Und zwar instance modal context CTX und knowledgeBase=KBName.
 *------------------------------------------*/
 



make_defelem(KBName,CTX,ICName1,irole(RName,iname(IRName),SpecList)) :-
	constructRestriction(IRName,SpecList,RestrList),
	defprimrole(KBName,CTX,IRName,restr(RName,and(RestrList))),
        make_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),SpecList)).

make_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),[])).
make_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),[X|T])) :-
	make_irole(KBName,CTX,ICName1,irole(RName,iname(IRName),X)),
	make_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),T)).


/*--------------------------------------------------------------------------------
 * sigma_defelem(ICName1,ISpecListe)
 * ISpecListe=[element(...),irole(iname(...)nr(...)vr(...))]
 * erzeugt onee Instanz ICName1 fuer one Konzept, an dem auch die Rolle RName
 * individualisiert werden kann mit dem Instanznamen IRName,
 * der Kardinalitaet der indiv. Rolle nr(MinNr,MaxNr,DefNr),
	* dem Rollenfueller der indiv. Rolle vr(ICName2),
 * Und zwar instance modal context [] und current knowledgeBase.
 * Domain dieser Rolle ist onee Teilmenge von indiv. Konzept ICName1.
 *------------------------------------------*/



sigma_defelem(ICName1,SpecListe) :-
	getCurrentKnowledgeBase(KBName),
	sigma_defelem(KBName,[],ICName1,SpecListe).



/*-----------------------------------------------------------------------------
 *Wie oben, nur instance A=knowledgeBase und modal context [], bzw. instance current knowledgeBase
 *und modal context A=CTX.
 *-----------------------------------------*/


sigma_defelem(A,ICName1,[]).

sigma_defelem(A,ICName1,SpecList) :-
	knowledgeBase(A,_,_),
	!,
	sigma_defelem(A,[],ICName1,SpecList).
sigma_defelem(A,ICName1,SpecList) :-
	(A = [] ; A = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_defelem(KBName,A,ICName1,SpecList).


/*-----------------------------------------------------------------------------
 *Wie oben, nur explizit instance knowledgeBase KBName und modal context CTX.
 *-----------------------------------------*/


sigma_defelem(KBName,CTX,ICName1,[]).
 
sigma_defelem(KBName,CTX,ICName1,[X|T]):-
	make_defelem(KBName,CTX,ICName1,X),
	sigma_defelem(KBName,CTX,ICName1,T). 


/************************ sigma_ask-Selektoren fuer die TBox **********************/
 

sigma_ask(Q) :-
	getCurrentKnowledgeBase(KBName),
	sigma_ask(KBName,[],Q).

sigma_ask(E,Q) :-
	knowledgeBase(E,_,_),
	!,
	sigma_ask(E,[],Q).
sigma_ask(M,Q) :-
	(M = [] ; M = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_ask(KBName,M,Q).

/*----------------------------------------------------------------------
 *
 * sigma_ask(supers(CName1,CName2)))
 *
 * ueberprueft, ob CName1 one direktes Subkonzept von CName2 ist.
 *
 *
 *-----------------------------------------------------------------------*/


 sigma_ask(KBName,CTX,(supers(CName1,CName2))) :-
	getDirectSuperConcepts(KBName,CTX,CName1,CL),
	member(CName2,CL).

/*---------------------------------------------------------------------------
 * sigma_ask(KBName,CTX,(supers*(CName1,CName2)))
 *
 * ueberprueft auch transitive supers-Beziehungen (Subsumptionsbeziehungen) zwischen
 * Subkonzept CName1 und Superkonzept CName2.
 *-----------------------------------------*/

 sigma_ask(KBName,CTX,(supers*(CName1,CName2))) :-
	getAllSuperConcepts(KBName,CTX,CName1,CL),
	member(CName2,CL).


/*------------------------------------------------------------------------------
 * sigma_ask(KBName,CTX,(role(RName,CNameDom,CNameRan)))
 *
 * dieser Selektor dient zum Zugriff bzw. zur Anfrage nach Rollenbeziehungen
 * zwischen zwei Konzepten, oder aber auch nach Domain und Range oneer Rolle.
 *--------------------------------------------*/

 sigma_ask(KBName,CTX,(role(RName,Subconcept,CNameRan))) :-
	  knowledgeBase(KBName,KB,_),
	sigma_ask(KBName,CTX,(supers*(Subconcept,Superconcept))),
	getRoleDomain(KB,CTX,RName,Superconcept),
	getRoleRange(KB,CTX,RName,CNameRan).

 sigma_ask(KBName,CTX,(role(RName,CNameDom,CNameRan))) :-
	  knowledgeBase(KBName,KB,_),
	  !,
	getRoleDomain(KB,CTX,RName,CNameDom),
	getRoleRange(KB,CTX,RName,CNameRan).



/*-----------------------------------------------------------------------------
 * sigma_ask(KBName,CTX,(roleDef(RName,CNameDef)))
 *
 * dient zur Anfrage bzgl. DefaultRange oneer Rolle.
 *--------------------------------------------*/


 sigma_ask(KBName,CTX,(roleDef(RName,CNameDef))) :-   
	  knowledgeBase(KBName,KB,_),
	  !,
	getRoleDefault(KB,CTX,RName,CNameDef).

/*----------------------------------------------------------------------------
 * sigma_ask(KBName,CTX,(roleNr(RName,MinNr,MaxNr)))
 *
 * der Selektor dient zum Zugriff auf die Number Restriction oneer Rolle RName
 * am oneem Konzept CName.
 *-------------------------------------------*/


 sigma_ask(KBName,CTX,(roleNr(RName,MinNr,MaxNr))) :-  
	  knowledgeBase(KBName,KB,_),
	  !,
	getRoleNr(KB,CTX,RName,MinNr,MaxNr).
				       
                                       
              

/*----------------------------------------------------------------------------
 * sigma_ask(KBName,CTX,(roleDefNr(RName,DefNr)))
 *
 * der Selektor dient zum Zugriff auf die Default- Number Restriction oneer Rolle
 * RName am oneem Konzept CName.
 *-------------------------------------------*/


 sigma_ask(KBName,CTX,(roleDefNr(RName,DefNr))) :-  
	  knowledgeBase(KBName,KB,_),
	  !,
	getRoleDefNr(KB,CTX,RName,DefNr).



/*----------------------------------------------------------------------------
 ************************* sigma_ask-Selektoren fuer die ABox ***********************
 *
 * sigma_ask(KBName,CTX,(element(ICName,CName)))
 *
 * ermoeglicht den Zugriff Initial.-Beziehung oneer Instanz ICName zum entsprech-
 * ende generellen Konzept CName.
 *------------------------------------------*/


 sigma_ask(KBName,CTX,(element(ICName,CName))) :- 
	ask(KBName,CTX,elementOf(ICName,CName),_).


sigma_ask(KBName,CTX,(hl_attributes(CN,Hl_attribute,Value))) :-
	hl_attribute(concept,KBName,CTX,CN,[Hl_attribute,Value]).
sigma_ask(KBName,CTX,(hl_attributes(CN,RN,Hl_attribute,Value))) :-
	hl_attribute(role,KBName,CTX,[CN,RN],[Hl_attribute,Value]).

sigma_ask(KBName,CTX,(subrole(Subrole,Superrole))) :-
	getDirectFatherRoles(KBName,CTX,Subrole,RL),
	member(Superrole,RL).

sigma_ask(KBName,CTX,(irole(R,X,Y))) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	EqLiteral = eq(KB,rn(_AX,_RN,_S,_O),modal(W1),Y,CON,A1,X,Pc,call(_CALLS),ab(noAb),Exp,Gd,Od,L1,L2,Anc1,Anc2,Y,_PT),
%	EqLiteral = eq(KB,rn(_AX,_RN,_S,_O),modal(W1),Y,app((FF:R),X),hyp(_HYPS),ab(noAb),call(_CALLS),_PT),
	call(G1),
	call(EqLiteral).


%----------------------------------------------------------------------
% sigma_ask(KBName,CTX,(allRoles(+CName,-Info)))
% Arguments: Cname ConceptName
%	     Info is a list consisting of lists with elements:
%               Rnames role-atom_codes
%	        Cnames domain 
% 	        Min    Minimalnr. der role
%              	Max    Maximalnr. der role
%           	Defnr  Defaultnr. der role  
%
% liefert alle rolen,domains,Minnr,Maxnr,Defnr die von oneem konzept ausgehen
% bsp : sigma_ask(kb,ctx,allRoles(golf,X))    X = ((has_part golf windshield 1 1 1)(consumes golf gasoline 1 1 1))
% 


sigma_ask(KBName,CTX,(allRoles(CName,Info_list))) :-
	setofOrNil(Info,[RName,CNameRan,Info]^(sigma_ask(KBName,CTX,(role(RName,CName,CNameRan))),
	 	 sigma_ask(KBName,CTX,(allRoles(RName,CName,CNameRan,Info)))),Info_list).
	
sigma_ask(KBName,CTX,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan,Min,Max,Defnr]))) :-
	sigma_ask(KBName,CTX,(roleNr(RName,Min,Max))),
	sigma_ask(KBName,CTX,(roleDefNr(RName,Defnr))),
	!.
sigma_ask(KBName,CTX,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan]))) :-
	!.


%----------------------------------------------------------------------
% sigma_fact(KBName,CTX,(all_roles(+CName,-Info)))
% Arguments: Cname ConceptName
%	     Info is a list consisting of lists with elements:
%               Rnames role-atom_codes
%	        Cnames domain 
% 	        Min    Minimalnr. der role
%              	Max    Maximalnr. der role
%           	Defnr  Defaultnr. der role  
%
% liefert alle rolen,domains,Minnr,Maxnr,Defnr die von oneem konzept ausgehen
% bsp : sigma_fact(kb,ctx,allRoles(golf,X))    X = ((has_part golf windshield 1 1 1)(consumes golf gasoline 1 1 1))
% 


sigma_fact(KBName,CTX,(allRoles(CName,Info_list))) :-
	setofOrNil(Info,[RName,CNameRan]^(sigma_fact(KBName,CTX,(role(RName,CName,CNameRan))),
	 	 sigma_fact(KBName,CTX,(allRoles(RName,CName,CNameRan,Info)))),Info_list).
	
sigma_fact(KBName,CTX,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan,Min,Max,Defnr]))) :-
	sigma_fact(KBName,CTX,(roleNr(RName,Min,Max))),
	sigma_fact(KBName,CTX,(roleDefNr(RName,Defnr))),
	!.
sigma_fact(KBName,CTX,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan]))) :-
	!.


%----------------------------------------------------------------------
% sigma_ask(KBName,CTX,(direct_super_role(+-RName,+-CNameDom,+-CNameRan,+-SRName,+-SCNameDom,+-SCNameRan))) :-
% Arguments: Rname RoleName
%	     CNameDom Domain
%            CNameRan Range 
%	     SRName Super-Role-Name
% 	     SRNameDom Super-Role_domain
%            SRNameRan Super-Role-range
%
% ist erfuellt wenn (SRName SCNameDom SCNameRan) die direkte super-rolle von (RName CNameDom CNameRan) ist
%
% 

sigma_ask(KBName,CTX,(direct_super_role(RName,CNameDom,CNameRan,RName,SCNameDom,CNameRan))) :-
	isSlot(CNameDom),
	getDirectSubConcepts(KBName,CTX,SCNameDom,CL),
	member(CNameDom,CL),
	!.

sigma_ask(KBName,CTX,(direct_super_role(RName,CNameDom,CNameRan,RName,SCNameDom,CNameRan))) :-
	sigma_ask(KBName,CTX,(supers(CNameDom,SCNameDom))),
	!.
%	sigma_ask(KBName,CTX,(role(SRName,SCNameDom,SCNameRan))),	
%	!.


%----------------------------------------------------------------------
% sigma_ask(KBName,CTX,(risa(+-IRName,+-ICNameDom,+-ICNameRan,+-GRName,+-GCNameDom,+-GCNameRan))) :-
% Arguments: IRname    individ.-RoleName
%	     ICNameDom individ.-Domain
%            ICNameRan individ.-Range 
%	     GRName    genereller Super-Role-Name
% 	     GRNameDom genereller Super-Role_domain
%            GRNameRan genereller Super-Role-range
% ist erfuellt wenn (GRName GCNameDom GCNameRan) die generelle - rolle von (IRName ICNameDom ICNameRan) ist
%  
% 

sigma_ask(KBName,CTX,(risa(IRName,ICNameDom,ICNameRan,GRName,GCNameDom,GCNameRan))) :-
	knowledgeBase(KBName,KB,_),
	getConceptElement(KB,CTX,ICNameDom,GCNameDom),
	sigma_ask(KBName,CTX,role(GRName,GCNameDom,GCNameRan)).

sigma_ask(KBName,CTX,(risa(IRName,ICNameDom,ICNameRan,IRName,GCNameDom,ICNameRan))) :-
	knowledgeBase(KBName,KB,_),
	getConceptElement(KB,CTX,ICNameDom,GCNameDom),
	!.

%----------------------------------------------------------------------
% sigma_ask(KBName,CTX,(rall(+-RName,+-CNameDom,+-CNameRan,CNameDef,+-MinNr,+-MaxNr,+-DefNr))) :-
% Arguments: Rname    RoleName
%	     CNameDom RoleDomain
%            CNameRan RoleRange 
%	     CNameDef ConceptNameDef.
% 	     MinNR    Minim.Nr
%            MaxNr    Maxi. Nr
% 
% ist erfuellt wenn 
%


sigma_ask(KBName,CTX1,(rall(RName,CNameDom,CNameRan,CNameDef,MinNr,MaxNr,DefNr))) :-
	knowledgeBase(KBName,KB,_),
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	convertCTX(negative,KB,[[],true],CTX3,[],[W1,G1],_),
	call(G1),
	roleAll(KB,W1,RName,CNameDom,CNameRan,CNameDef,MinNr,MaxNr,DefNr),
	!.


/*-----------------------------------------------------------------------------
 */

sigma_hl_attributes(CN,AList) :-
	getCurrentKnowledgeBase(KBName),
	sigma_assert_hl_attributes(concept,KBName,[],CN,AList).
sigma_hl_attributes(A1,CN,AList) :-
	knowledgeBase(A1,_,_),
	!,
	sigma_assert_hl_attributes(concept,A1,[],CN,AList).
sigma_hl_attributes(A1,CN,AList) :-
	(A1 = [] ; A1 = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_assert_hl_attributes(concept,KBName,A1,CN,AList).
sigma_hl_attributes(CN,R,AList) :-
	getCurrentKnowledgeBase(KBName),
	sigma_assert_hl_attributes(role,KBName,[],[CN,R],AList).
sigma_hl_attributes(A1,A2,CN,AList) :-
	knowledgeBase(A1,_,_),
	(A2 = [] ; A2 = [_|_]),
	!,
	sigma_assert_hl_attributes(concept,A1,A2,CN,AList).
sigma_hl_attributes(KBName,CN,RN,AList) :-
	sigma_assert_Hl_attributes(role,KBName,[],[CN,RN],AList).
sigma_hl_attributes(KBName,CTX,CN,RN,AList) :-	
	sigma_assert_hl_attributes(role,KBName,CTX,[CN,RN],AList).

sigma_assert_hl_attributes(Type,KBName,CTX,Spec,[]) :-
	!.
sigma_assert_hl_attributes(Type,KBName,CTX,Spec,[Pair|AList]) :-
	assertz(hl_attribute(Type,KBName,CTX,Spec,Pair)),
	sigma_assert_hl_attributes(Type,KBName,CTX,Spec,AList).

/*------------------------------------------------------------------------------
 * Die 2- bis 4-stellige Funktion sigma_unprimconcept([KB],[CTX],CN,SpecListe) erwartet 
 * als letztes Argument  onee Liste von Konstruktoren: supers,restr-inh,nr, die irgendwann
 * mit sigma_primconcept bzgl. CN definiert wurde.
 * Die Fkt. sigma_unprimconcept eliminiert die entsprechenden Subsumption-Beziehungen wieder 
 * und l"oscht ebenfalls Onetr"age der Rollen wie: roleDomain,roleNr,etc..
 *-----------------------------*/

 

 unmake_primconcept(KBName,CTX,CN,supers([])) :- !.

 unmake_primconcept(KBName,CTX,CN,supers([X|T])) :-
			    knowledgeBase(KBName,KB,_),
			    conceptSubsets(KB,_user,CTX,CN,X,AX),
			    undefprimconcept(KBName,CTX,CN,X),
			    unmake_primconcept(KBName,CTX,CN,supers(T)).

unmake_primconcept(KBName,CTX,CName1,
		   'restr-inh'(R1,restricts(R2,range(CName2,CNameDef)))):-
          unmake_primconcept(KBName,CTX,CName1,
			    restrict_inh(R1, restricts(R2 ,
							   range(CName2,CNameDef)))).



unmake_primconcept(KBName,CTX,CName1,
		   'restrict-inh'(R1,restricts(R2,range(CName2,CNameDef)))) :-
        unmake_primconcept(KBName,CTX,CName1,
			 restrict_inh(R1, restricts(R2 ,
							range(CName2,CNameDef)))).
 


unmake_primconcept(KBName,CTX,CName1,
	           restrict_inh(R1,restricts(R2,range(CName2,CNameDef)))) :-
	knowledgeBase(KBName,KB,_),
	!,
	getRoleDomain(KB,CTX,R1,CNameDom),
	!,
	getRoleRange(KB,CTX,R1,CName2),
	undefprimconcept(KBName,CTX,CNameDom,CName1),
	retract((roleDomain(KB,CTX,R1,CNameDom) :- _)),
	retract((roleRange(KB,CTX,R1,CName2) :- _)),
	retract((roleDefault(KB,CTX,R1,CNameDef) :- _)),
	retract((roleTripel(KB,CTX,R1,CNameDom,CName2,CNameDef))),
        !.



unmake_primconcept(KBName,CTX,CName1 , nr(R1, MinNr,MaxNr,DefNr)) :-
	knowledgeBase(KBName,KB,_),
	!,
	getRoleNr(KB,CTX,R1,MinNr,MaxNr),
	!,
	getRoleDefNr(KB,CTX,R1,DefNr),
	conceptEqualSets(KB,_user,CTX,CNameDom,
			and([atleast(MinNr,R1),atmost(MaxNr,R1)]),AX),
        undefconcept(KBName,CTX,CNameDom,and([atleast(MinNr,R1),
	 				      atmost(MaxNr,R1)])),
        undefconcept(KBName,CTX,CNameDom,some(R1,'Entity')),
        undefprimconcept(KBName,CTX,CNameDom,CName1),
        retract((roleNr(KB,CTX,R1,MinNr,MaxNr) :- _)),
	retract((roleDefNr(KB,CTX,R1,DefNr) :- _)),
	!.



/*------------------------------------------------------------------------
 * sigma_unprimconcept(KBName,CTX,CName1,SpecList)
 *
 *------------------------------------------*/


sigma_unprimconcept(CName1,[]) :- !.

sigma_unprimconcept(CName1,[X|T]) :-
			    getCurrentKnowledgeBase(KBName),
			    sigma_unprimconcept(KBName,[],CName1,[X|T]),
			    !.



sigma_unprimconcept(A,CName1,[X|T]) :-
			     knowledgeBase(A,_,_),
			     !,
			     sigma_unprimconcept(A,[],CName1,[X|T]),
			     !.


sigma_unprimconcept(A,CName1,[X|T]) :-
			     (A=[] ; A=[_,_]),
			     getCurrentKnowledgeBase(KBName),
			     !,
			     sigma_unprimconcept(KBName,A,CName1,[X|T]).



sigma_unprimconcept(KBName,CTX,CName1,[]) :- !.

sigma_unprimconcept(KBName,CTX,CName1,[X|T]) :-
			     unmake_primconcept(KBName,CTX,CName1,X),
			     sigma_unprimconcept(KBName,CTX,CName1,T),
			     !.




/*------------------------------------------------------------------------------
 * Die 4-stellige Fkt. unmake_defconcept(KB,CTX,CN,supers(List_of_concepts)) erwartet
 * als letztes Argument  onee Liste (Menge) von Konzepten. Onee Obermenge dieser Menge wurde
 * irgendwann instance AND-Verkn"upfung mit CN "aquivalent gemacht und zwar instance der Form:
 *
 *    sigma_defconcept(CN, [supers([C1,C2,...,Cn]),..]) ----->   defconcept(CN,and([C1,C2,..,Cn])).    
 *
 *-------------------------------------------------*/



unmake_defconcept(KBName,CTX,CName1,restrict_inh(RName1, restricts(RName2 ,
				    range(CName2,CNameDef))),CNameDom):-
        unmake_defconcept(KBName,CTX,CName1,'restr-inh'(RName1, restricts(RName2 ,
			  range(CName2,CNameDef))),CNameDom).


unmake_defconcept(KBName,CTX,CName1,'restrict-inh'(RName1, restricts(RName2 ,
					range(CName2,CNameDef))),CNameDom):-
        unmake_defconcept(KBName,CTX,CName1,'restr-inh'(RName1, restricts(RName2 ,
			  range(CName2,CNameDef))),CNameDom).


unmake_defconcept(KBName,CTX,CName1,'restr-inh'(RName1, restricts(RName2 ,
				  range(CName2,CNameDef))),CNameDom):-

        knowledgeBase(KBName,KB,_),
	!,
	roleEqualSets(KB,_user,CTX,RName1,restr(RName2,CName2)),
	undefrole(KBName,CTX,RName1,restr(RName2,CName2)),
	!,
	getRoleDomain(KB,CTX,RName1,CNameDom),
	!,
	conceptEqualSets(KB,_usr,CTX,CNameDom,some(RName2,'Entity')),
	undefconcept(CNameDom,some(RName2,'Entity')),
        retract((roleDomain(KB,CTX,RName1,CNameDom) :- _)),
	retract((roleRange(KB,CTX,RName1,CName2) :- _)),
	retract((roleDefault(KB,CTX,RName1,CNameDef) :- _)),
	retract((roleTripel(KB,CTX,RName1,CNameDom,CName2,CNameDef))),
	retract((roleAll(KB,CTX,RName1,CNameDom,CName2,CNameDef,_,_,_))),
	!.


unmake_defconcept(KBName,CTX,CName1, nr(RTerm,MinNr,MaxNr,DefNr),CNameDom) :-
	knowledgeBase(KBName,KB,_),
        unexpand_role(KBName,CTX,RTerm,R1),
	!,
	conceptEqualSets(KB,_usr,CTX,CNameDom,and([atleast(MinNr,R1),
					           atmost(MaxNr,R1)]),AX),
        undefconcept(KBName,CTX,CNameDom,and([atleast(MinNr,R1),
						   atmost(MaxNr,R1)])),
	!,
	conceptEqualSets(KB,_usr,CTX,CNameDom,some(R1,'Entity')),
        undefconcept(KBName,CTX,CNameDom,some(R1,'Entity')),       
	retract((roleNr(KB,CTX,R1,MinNr,MaxNr) :- _)),
	retract((roleDefNr(KB,CTX,R1,DefNr) :- _)),
	!.
        						   
        
	    
unmake_defconcept(KBName,CTX,CName1, necres(RTerm, nec),CNameDom):-
	!,
	knowledgeBase(KBName,KB,_),
	unexpand_role(KBName,CTX,RTerm,R1),
	conceptEqualSets(KB,_usr,CTX,CNameDom,atleast(1,R1),AX),
	undefconcept(KBName,CTX,CNameDom,atleast(1,R1)),
	retract((roleDomain(KB,CTX,R1,CNameDom) :- _)),
	!.



unmake_defconcept(KBName,CTX,CName1 , necres(RTerm, _),CNameDom):-
        !,
	knowledgeBase(KBName,KB,_),
	unexpand_role(KBName,CTX,RTerm,R1),
	retract((roleDomain(KB,CTX,RName1,CNameDom) :- _)),
	!.


/*---------------------------------------------------------------------
 * unexpand_role(KBName,CTX,RTerm,R1)
 *
 *-------------------------------------*/


unexpand_role(_,_,RTerm,RTerm) :-
	 atomic(RTerm),
	 !.

unexpand_role(KBName,CTX,RTerm,R1) :-
         RTerm = sigma_unprimelemrole(KBName,CTX,R1, 'domain-range'(C1,C2,CNameDef)),
         !,
	 call(RTerm).
	   


unexpand_role(KBName,CTX,RTerm,R1) :-
         RTerm = sigma_unprimelemrole(CTX,R1, 'domain-range'(C1,C2,CNameDef)),
         !,
	 call(RTerm).


unexpand_role(KBName,CTX,RTerm,R1) :-
         RTerm = sigma_unprimelemrole(R1, 'domain-range'(C1,C2,CNameDef)),
         !,
	 call(RTerm).



unexpand_role(_,_,RTerm,RTerm).



/**********************************************************************
 *
 * sigma_undefconcept(KBName,CTX,CName1,SpecList)
 *
 *
 */

sigma_undefconcept(CName1,[]):- !.

sigma_undefconcept(CName1,SpecList) :-
         getCurrentKnowledgeBase(KBName),
	 sigma_undefconcept(KBName,[],CName1,SpecList),
	 !.



sigma_undefconcept(A,CName1,SpecList) :-
         knowledgeBase(A,_,_),
       	 !,
	 sigma_undefconcept(A,[],CName1,SpecList),
	 !.


sigma_undefconcept(A,CName1,SpecList) :-
         (A=[] ; A=[_,_]),
         getCurrentKnowledgeBase(KBName),
         !,
         sigma_undefconcept(KBName,A,CName1,SpecList).


sigma_undefconcept(KBName,CTX,CName1,SpecList) :-
           sigma_undefconcept(KBName,CTX,CName1,SpecList,[]).


sigma_undefconcept(KBName,CTX,CName1,[],[]) :- !.

sigma_undefconcept(KBName,CTX,CName1,[],HList) :-
	   knowledgeBase(KBName,KB,_),
	   !,
	   conceptEqualSets(KB,_user,CTX,CName1,and(HList),AX),
	   undefconcept(KBName,CTX,CName1,and(HList)),
	   !.


sigma_undefconcept(KBName,CTX,CName1,[supers(L)|SpecList],CL1) :-
	   !,
	   append(L,CL1,CL2),
	   sigma_undefconcept(KBName,CTX,CName1,SpecList,CL2).

sigma_undefconcept(KBName,CTX,CName1,[Spec1|SpecList],CL1) :-
           unmake_defconcept(KBName,CTX,CName1,Spec1,Concept),
           sigma_undefconcept(KBName,CTX,CName1,SpecList,[Concept|CL1]).



sigma_undefconcept(KBName,CTX,CName1,supers(L),[]) :-
           knowledgeBase(KBName,KB,_),
	   !,
	   conceptEqualSets(KB,_user,CTX,CName1,and(L),AX),
	   undefconcept(KBName,CTX,CName1,and(L)).



/**********************************************************************
 *
 * sigma_unprimelemrole(KBName,CTX,RName1, 'domain-range'(CName1,CName2,CNameDef)) *
 *
 */


sigma_unprimelemrole(RName1, 'domain-range'(CName1,CName2,CNameDef)):-
           getCurrentKnowledgeBase(KBName),
	   sigma_unprimelemrole(KBName,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).



sigma_unprimelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	   getCurrentKnowledgeBase(X),
	   sigma_unprimelemrole(X,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).


sigma_unprimelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	   (X = [] ; X = [_|_]),
	   getCurrentKnowledgeBase(KBName),
	   sigma_unprimelemrole(KBName,X,RName1, 'domain-range'(CName1,CName2,CNameDef)).




sigma_unprimelemrole(KBName,CTX,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	   knowledgeBase(KBName,KB,_),
	   !,
	   conceptSubsets(KB,_usr,CTX,CName1,some(RName1,'Entity')),
	   undefprimconcept(KBName,CTX,CName1,some(RName1,'Entity')),
	   !,
	   conceptSubsets(KB,_usr,CTX,some(inverse(RName1),'Entity')),
	   undefprimconcept(KBName,CTX,some(inverse(RName1),'Entity'),CName2),
	   retract((roleDomain(KB,CTX,RName1,CName1) :- _)),
	   retract((roleRange(KB,CTX,RName1,CName2) :- _)),
	   retract((roleDefault(KB,CTX,RName1,CNameDef) :- _)),
	   retract((roleTripel(KB,CTX,RName1,CName1,CName2,CNameDef))),
	   retract((roleAll(KB,CTX,RName1,CName1,CName2,CNameDef,_,_,_))),
	   !.


/**********************************************************************
 *
 * sigma_undefelemrole(RName1,restricts(RName2,range(CName1,CNameDef)))
 * 
 */


sigma_undefelemrole(RName1, restricts(RName2, range(CName1,CNameDef))):-
        getCurrentKnowledgeBase(KBName),
	sigma_undefelemrole(KBName,[],RName1, restricts(RName2, range(CName1,CNameDef))),
	!.


sigma_undefelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
	getCurrentKnowledgeBase(X),
	!,
	sigma_undefelemrole(X,[],RName1, restricts(RName2, range(CName1,CNameDef))),
        !.


sigma_undefelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
        (X = [] ; X = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_undefelemrole(KBName,X,RName1, restricts(RName2, range(CName1,CNameDef))),
	!.


sigma_undefelemrole(KBName,CTX,RName1, restricts(RName2, range(CName1,CNameDef))) :-
        knowledgeBase(KBName,KB,_),
	!,
	roleEqualSets(KB,_user,CTX,RName1,restr(RName2,CName1)),
	undefrole(KBName,CTX,RName1,restr(RName2,CName1)),
	!,
	roleRange(KB,CTX,RName1,CName1),
	!,
	roleDefault(KB,CTX,RName1,CNameDef),
	retract((roleRange(KB,CTX,RName1,CName1) :- _)),
	retract((roleDefault(KB,CTX,RName1,CNameDef) :- _)),
	!.

/*----------------------------------------------*/


unmake_irole(KBName,CTX,ICName1,irole(RName,iname(IRName),
				      nr(MinNr,MaxNr,DefNr))):-
        knowledgeBase(KBName,KB,_),
	!,
	roleSubsets(KB,_user,CTX,IRName,restr(RName,and([atleast(MinNr,RName),
				            	    atmost(MaxNr,RName),
						    some(inverse(RName),'Entity')]))),

	undefprimrole(KBName,CTX,IRName,restr(RName,
					      and([atleast(MinNr,RName),
					      atmost(MaxNr,RName),
					      some(inverse(RName),'Entity')]))),
        !,
	getRoleNr(KB,CTX,IRName,MinNr,MaxNr),
	!,
	getRoleDefNr(KB,CTX,IRName,DefNr),
	retract((roleNr(KB,CTX,IRName,MinNr,MaxNr) :- _)),
	retract((roleDefNr(KB,CTX,IRName,DefNr) :- _)),
	!.

unmake_irole(KBName,CTX,ICName1,irole(RName,iname(IRName),vr(ICName2))) :-
	knowledgeBase(KBName,KB,_),
	!,
	roleSubsets(KB,_user,CTX,IRName,restr(RName,CName2)),
	undefprimrole(KBName,CTX,IRName, restr(RName,CName2)),
        delete_ind(KBName,CTX,ICName1,ICName2,IRName).

unmake_defelem(KBName,CTX,ICName,element(CName)):-
	delete_ind(KBName,CTX,ICName,CName),
	!.

unmake_defelem(KBName,CTX,ICName1,irole(RName,iname(IRName),SpecList)) :-
	constructRestriction(IRName,SpecList,RestrList),
	!,
	roleSubsets(KB,_user,CTX,IRName,restr(RName,and(RestrList))),
	undefprimrole(KBName,CTX,IRName,restr(RName,and(RestrList))),
	unmake_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),SpecList)).


unmake_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),[])) :-
	!.
unmake_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),[X|T])) :-
	unmake_irole(KBName,CTX,ICName1,irole(RName,iname(IRName),X)),
	unmake_defelem_list(KBName,CTX,ICName1,irole(RName,iname(IRName),T)).



/**********************************************************************
 *
 * sigma_undefelem(KBName,CTX,ICName1,[X|T])
 *
 */

sigma_undefelem(ICName1,SpecListe) :-
	getCurrentKnowledgeBase(KBName),
	sigma_undefelem(KBName,[],ICName1,SpecListe).



sigma_undefelem(A,ICName1,SpecList) :-
        knowledgeBase(A,_,_),
	!,
	sigma_undefelem(A,[],ICName1,SpecList).

sigma_undefelem(A,ICName1,SpecList) :-
	(A = [] ; A = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName),
	sigma_undefelem(KBName,A,ICName1,SpecList).


sigma_undefelem(KBName,CTX,ICName1,[]) :-
	!.

sigma_undefelem(KBName,CTX,ICName1,[X|T]):-
	unmake_defelem(KBName,CTX,ICName1,X),
        sigma_undefelem(KBName,CTX,ICName1,T).


/**********************************************************************
 *
 * sigma_fact(KBName,CTX,elementOf(X,C),P)
 *
 */

sigma_fact(P1) :-
	completeParameter([P1],KBName,CTX,Query,Proof),
	sigma_fact(KBName,CTX,Query,Proof).
sigma_fact(P1,P2) :-
	completeParameter([P1,P2],KBName,CTX,Query,Proof),
	sigma_fact(KBName,CTX,Query,Proof).
sigma_fact(P1,P2,P3) :-
	completeParameter([P1,P2,P3],KBName,CTX,Query,Proof),
	sigma_fact(KBName,CTX,Query,Proof).

sigma_fact(KBName,CTX,element(X,C),Exp) :-
	retractall(hypothesis(_)),
 	knowledgeBase(KBName,KB,_),
 	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
 	getNegatedConcept(C,C1),
 	getFactQuery(KB,W1,C,X,Exp,Goal),
 	call((call(G1), Goal)),
 	atomic(X),
	allowedAnswerConcept(KB,C).
sigma_fact(KBName,CTX,(role(RName,CNameDom,CNameRan)),proved(fact,basedOn(tbox))) :-
	knowledgeBase(KBName,KB,_),
	!,
	getRoleDomain(KB,CTX,RName,CNameDom),
	getRoleRange(KB,CTX,RName,CNameRan).
sigma_fact(KBName,CTX,(hl_attributes(CN,Hl_attribute,Value)),proved(fact,basedOn(tbox))) :-
	hl_attribute(concept,KBName,CTX,CN,[Hl_attribute,Value]).
sigma_fact(KBName,CTX,(hl_attributes(CN,RN,Hl_attribute,Value)),proved(fact,basedOn(tbox))) :-
	hl_attribute(role,KBName,CTX,[CN,RN],[Hl_attribute,Value]).
sigma_fact(KBName,CTX,irole(R,X,Y),Exp) :-
	retractall(hypothesis(_)),
	knowledgeBase(KBName,KB,_),
	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	getFactQuery(KB,W1,Y,R,X,Exp,Goal),
	call(G1), 
	call(Goal).
	
getFactQuery(KB,W1,C0,X,Exp,Goal) :-
	constructMLHead(KB,_RN1,W1,C0,CON,A1,X,Pc,_CALLS,_D,Exp,Gd,Od,L1,L2,Anc1,Anc2,Y,PT,InHead),
%	constructMLHead(KB,_RN1,W1,C0,X,_HYPS,_D,_CALLS,PT,InHead),
	getExplanation(InHead,Exp),
	Goal = (clause(InHead,Body), ((PT == abox), call(Body))),
	!.
getFactQuery(KB,W1,Y,R,X,PT,Goal) :-
	constructEqHead(KB,_RN1,W1,Y,CON,A1,X,Pc,_CALLS,_D,Exp,Gd,Od,L1,L2,Anc1,Anc2,Y,PT,EqHead),
%	constructEqHead(KB,_RN1,W1,Y,_F,R,X,_HYPS,_D,_CALLS,PT,EqHead),
	Goal = (clause(EqHead,Body), ((PT == abox), call(Body))),
	!.

getRoleDomain(KB,CTX1,RName,CDom) :-
	convertCTX(negative,KB,[[],true],CTX1,[],[W1,G1],_),
	clause(roleDomain(KB,W1,RName,CDom),_),
	once((call(G1),roleDomain(KB,W1,RName,CDom))).

getRoleRange(KB,CTX1,RName,CRange) :-
	convertCTX(negative,KB,[[],true],CTX1,[],[W1,G1],_),
	clause(roleRange(KB,W1,RName,CRange),_),
	once((call(G1),roleRange(KB,W1,RName,CRange))).

getRoleDefault(KB,CTX1,RName,CNameDef) :-
	convertCTX(negative,KB,[[],true],CTX1,[],[W1,G1],_),
	clause(roleDefault(KB,W1,RName,CNameDef),_),
	once((call(G1),roleDefault(KB,W1,RName,CNameDef))).

getRoleNr(KB,CTX1,RName,MinNr,MaxNr) :- 
	convertCTX(negative,KB,[[],true],CTX1,[],[W1,G1],_),
	clause(roleNr(KB,W1,RName,MinNr,MaxNr),_),
	once((call(G1),roleNr(KB,W1,RName,MinNr,MaxNr))).

getRoleDefNr(KB,CTX1,R1,DefNr) :-
	convertCTX(negative,KB,[[],true],CTX1,[],[W1,G1],_),
	clause(roleDefNr(KB,W1,R1,DefNr),_),
	once((call(G1),roleDefNr(KB,W1,R1,DefNr))).

getRoleTripel(KB,CTX1,RName1,CNameDomT,CNameT,CNameDefT) :-
	convertCTX(KB,[[],true],CTX1,[],[W1,G1],_),
	clause(roleTripel(KB,W1,RName1,CNameDomT,CNameT,CNameDefT),_),
	once((call(G1),roleTripel(KB,W1,RName1,CNameDomT,CNameT,CNameDefT))).

getConceptElement(KB,CTX1,X,C) :-
	convertCTX(KB,[[],true],CTX1,[],[W1,G1],_),
	clause(conceptElement(KB,_W1,_,X,C,_),_),
	once((call(G1),conceptElement(KB,W1,_,X,C,_))).
/**********************************************************************
 *
 * @(#) tellConcept.pll 1.12@(#)
 *
 */

/***********************************************************************
 *
 * assertConceptRInL(+ModalSequence,+CT1,+CT2,+AxiomName)
 * asserts membership clauses for the inclusion of CT2 into CT1 instance 
 * modal context ModalSequence.
 *
 */

assertConceptRInL(KB,Name,CTX,CT1,CT2) :-
	assertConceptLInR(KB,Name,CTX,CT2,CT1).

/***********************************************************************
 *
 * assertConceptLInR(+ModalSequence,+ConceptName,+Constraint,+AxiomName)
 * asserts membership clauses for the inclusion of ConceptName into
 * Constraint instance modal context ModalSequence.
 *
 */

assertConceptLInR(_KB,rn(_,_,_),_CTX,_CN,or([])) :- 
	!.
assertConceptLInR(KB,rn(AxiomName,Source,Orientation),CTX,CN,or([CT1|CTs])) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	hop_map(negate,[CT1|CTs],NCTs),
	convertInAntecedentList(KB,rn(AxiomName,_AnySource1,Orientation),
                                bodyMC(W1),headMC(W1),NCTs,X,
				HYPS,AB,CALLS,PTL,INCTs),
	convertInAntecedent(KB,rn(AxiomName,_AnySource2,Orientation),
                            bodyMC(W1),headMC(W1),
			    CN,X,HYPS,AB,CALLS,PT1,Body),
	assertOrConceptLInR(KB,rn(AxiomName,Source,Orientation),
                            CTX,PT1,Body,W1,G1,X,HYPS,AB,CALLS,
                            or([CT1|CTs]),[],[],PTL,INCTs).
assertConceptLInR(_KB,rn(_,_,_),_CTX,_CN,and([])) :-
	!.
assertConceptLInR(KB,rn(AxiomName,O2,Orientation2),CTX,CN1,and([CN2|CTs])) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation2),
                            bodyMC(W1),headMC(W1),
			    CN1,X,HYPS,AB,CALLS,PT1,Body),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O2,Orientation2,RN2),
	convertInConsequence(KB,pr(3),RN2,CTX,W1,CN2,X,HYPS,AB,CALLS,PT1,InHead2),
	constructMLMark(InHead2,Mark2),
%	asserta((InHead2 :- (cCS(CALLS,Mark2), (call(G1), onceOrMore(Body))))),
	asserta((InHead2 :- (cCS(CALLS,Mark2), (call(G1), Body)))),
	assertConceptLInR(KB,rn(AxiomName,O2,Orientation2),CTX,CN1,and(CTs)),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,set(Set1)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,set(Set1),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), onceOrMore(Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,not(set(Set1))) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,not(set(Set1)),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), onceOrMore(Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,not(D)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,not(D),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), onceOrMore(Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,naf(D)) :-
	% instance the consequence not and naf have the same meaning
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,not(D),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), onceOrMore(Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,all(R,D)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,all(R,D),X,HYPS,AB,CALLS,PT1,
	                     (EqLiteral, InHead1)),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (EqLiteral, Body))))),
	gensym(rule,RuleName2),
	ruleName(AxiomName,RuleName2,O,Orientation,RN2),
	convertInConsequence(KB,pr(3),RN2,CTX,W1,not(normal(R)),X,
			     HYPS,AB,CALLS,PT2,InHead2),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W1),not(D),aaa,HYPS,AB,CALLS,PT1,Body2),
        constructMLMark(InHead2,Mark2),
        asserta((InHead2 :- (cCS(CALLS,Mark2), Body2))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,some(R,set([X]))) :-
	nonvar(X),
	!,
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName3),
	ruleName(AxiomName,RuleName3,user,lInR,RN3),
	constructEqHead(KB,RN3,W1,X,SF1,R,Y,HYPS,AB,CALLS,PT1,EqLiteral2),
	constructEqMark(RN3,W1,X,SF1,R,Y,HYPS,AB,CALLS,EqMark2),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W1),CN,Y,HYPS,AB,CALLS,PT1,Body),
	asserta((EqLiteral2 :- cCS(CALLS,EqMark2), Body)),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,some(R,D)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName1),
	ruleName(AxiomName,RuleName1,O,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,some(R,D),X,
	                     HYPS,AB,CALLS,PT1,(EqLiteral, InHead1)),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (EqLiteral, Body))))),
	gensym(rule,RuleName2),
	ruleName(AxiomName,RuleName2,system,Orientation,RN2),
	convertInConsequence(KB,pr(3),RN2,CTX,W1,normal(R),X,
			     HYPS,AB,CALLS,PT2,InHead2),
	constructMLMark(InHead2,Mark2),
	asserta((InHead2 :- cCS(CALLS,Mark2), (call(G1), Body))),
%% experimental code
%%  03.06.94
%% x instance CN => R(X,sk(X)) for some skolem function sk
%	gensymbol(skolem,[X],SF1),
%	gensymbol(object,[X],SO1),
%	gensym(rule,RuleName3),
%	ruleName(AxiomName,RuleName3,user,lInR,RN3),
%	constructEqHead(KB,RN3,W1,SO1,SF1,R,X,HYPS,AB,CALLS,PT1,EqLiteral2),
%	constructEqMark(RN3,W1,SO1,SF1,R,X,HYPS,AB,CALLS,EqMark2),
%	asserta((EqLiteral2 :- cCS(CALLS,EqMark2), Body)),
%% experimental code
%%  03.06.94
%% x instance CN => D(sk(X))   for some skolem function sk
%	gensym(rule,RuleName4),
%	ruleName(AxiomName,RuleName4,O,Orientation,RN4),
%	convertInConsequence(KB,pr(3),RN4,CTX,W1,D,SO1,HYPS,AB,CALLS,PT1,InHead4),
%	constructEqCall(KB,rn(AN4,RN4,_S4,_O4),bodyMC(W1),headMC(W1),Y,SF1,R,X,HYPS,AB,CALLS,PT4,EqCall4),
%	constructMLMark(InHead4,Mark4),
%	asserta((InHead4 :- (cCS(CALLS,Mark4), (call(G1), (EqCall4, Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,_S,Orientation),CTX,CN,atleast(N,R)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	typeOfDefinition(KB,CTX,R,S1),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,S1,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,atleast(N,R),X,
			     HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), onceOrMore(Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,atmost(N,R)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	typeOfDefinition(KB,CTX,R,O1),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O1,Orientation,RN1),
	convertInConsequence(KB,pr(3),RN1,CTX,W1,atmost(N,R),X,
			     HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), onceOrMore((call(G1), Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,b(MOp,P1,D)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	genagent(P1,free,P),
	C1 = hl_holds(KB,_,m(MOp,P),W1,W2),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body),
	constructMLHead(KB,RN1,W2,D,X,HYPS,AB,CALLS,and([C1,PT1]),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (C1, Body))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,bc(MOp,C,D)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	genagent(P1,free,P),
	C1 = hl_holds(KB,_,m(MOp,P),W1,W2),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource2,Orientation),
			    bodyMC(W1),headMC(W2),C,P,HYPS,AB,CALLS,PT2,Body2),
	constructMLHead(KB,RN1,W2,D,X,HYPS,AB,CALLS,and([C1,PT1,PT2]),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (C1, (Body1, Body2)))))),
	!.
assertConceptLInR(KB,rn(AxiomName,_S1,Orientation),CTX,CN,d(MOp,P1,D)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],WVL),
	gensym(rule,RuleName),
	typeOfDefinition(KB,CTX,D,S2),
	ruleName(AxiomName,RuleName,S2,Orientation,RN1),
	gensym(wp,WP),
	WPTerm =.. [WP,WVL],
	genagent(P1,skolemize,P),
	W2 = app(WPTerm:m(MOp,P),W1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body),
	constructMLHead(KB,RN1,W2,D,X,HYPS,AB,CALLS,
			PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), Body)))),
	!.
assertConceptLInR(KB,rn(AxiomName,_S1,Orientation),CTX,CN,dc(MOp,C,D)) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],WVL),
	gensym(rule,RuleName),
	typeOfDefinition(KB,CTX,D,S2),
	ruleName(AxiomName,RuleName,S2,Orientation,RN1),
	gensym(wp,WP),
	WPTerm =.. [WP,WVL],
	genagent(P1,skolemize,P),
	W2 = app(WPTerm:m(MOp,P),W1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body1),
	convertInAntecedent(KB,rn(AxiomName,_AnySource2,Orientation),
                            bodyMC(W1),headMC(W2),C,P,HYPS,AB,CALLS,PT2,Body2),
	constructMLHead(KB,RN1,W2,D,X,HYPS,AB,CALLS,
			and([PT1,PT2]),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (Body1, Body2))))),
	!.
assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,ConceptTerm) :-
	assertConceptLInR(KB,rn(AxiomName,O,Orientation),CTX,CN,and([ConceptTerm])).

/***********************************************************************
 * 
 * assertOrConceptLInR(KB,CTX,InHead,or([CT1|CTs]),[INCT|CTs]),AxiomName).
 *
 */

assertOrConceptLInR(_KB,rn(_,_,_),_CTX,_PT1,_InHead,_W1,_G1,_X,_HYPS,_,_CALLS,
		    or([]),_FPTL,_First,_PTL,_INCTs) :-
	!.
assertOrConceptLInR(KB,rn(AxiomName,O,Orientation),
                    CTX,PT2,InHead2,W1,G1,X,HYPS,AB,CALLS,or([CT1|CTs]),
	            FPTL,First,[PT1|PTL1],[INCT1|INCTs]) :-
	append([InHead2|First],INCTs,L1),
	append([PT2|FPTL],PTL1,PTL),
%	hop_map(getInExplanation,L1,PTL),
	constructBody(L1,Body),
	gensym(rule,RuleName),
	typeOfDefinition(KB,CTX,CT1,S1),
	ruleName(AxiomName,RuleName,S1,Orientation,RN1),
	constructMLHead(KB,RN1,W1,CT1,X,HYPS,AB,CALLS,and(PTL),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), onceOrMore((call(G1), Body))))),
	append(First,[INCT1],L2),
	append(FPTL,[PT1],FPTL2),
	!,
	assertOrConceptLInR(KB,rn(AxiomName,O,Orientation),
                            CTX,PT2,InHead2,W1,G1,X,HYPS,AB,CALLS,or(CTs),
                            FPTL2,L2,PTL1,INCTs).

constructBody([],true) :-
	!.
constructBody([I1],I1) :-
	!.
constructBody([I1|IL],(I1,B1)) :-
	constructBody(IL,B1).


/***********************************************************************
 *
 * convertAndList(+ModalSequence,+ConceptTermList,
 *                   -X,-CallStack,-InTerCTXtructure,+AxiomName)
 *
 */

convertAndList(_,_,_,_,[],_X,_HYPS,_,_CALLS,true,[]).
convertAndList(KB,Name,MC1,MC2,[CTerm],X,HYPS,AB,CALLS,InCTerm,[PT1]) :-
	convertInAntecedent(KB,Name,MC1,MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm).
convertAndList(KB,Name,MC1,MC2,[CTerm|CTermList],X,HYPS,AB,CALLS,
	       (InCTerm,InCTermList),[PT1|PTL]) :-
	convertInAntecedent(KB,Name,MC1,MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
	convertAndList(KB,Name,MC1,MC2,CTermList,X,
		       HYPS,AB,CALLS,InCTermList,PTL).

/***********************************************************************
 *
 * convertOrList(+ModalSequence,+ConceptTermList,
 *                   -X,-CallStack,-InTerCTXtructure,+AxiomName)
 *
 */

convertOrList(_,_,_,[],_X,_HYPS,_AB,_CALLS,_AX,false,[]).
convertOrList(KB,Name,MC1,MC2,[CTerm],X,HYPS,AB,CALLS,InCTerm,[PT1]) :-
	convertInAntecedent(KB,Name,MC1,MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
	!.
convertOrList(KB,Name,bodyMC(W1),MC2,[CTerm|CTermList],X,
	      HYPS,AB,CALLS,
              (InCTerm ; (InCTermList)),[PT1|PTL]) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	Name = rn(AX,_S,_O),
	convertInAntecedent(KB,Name,bodyMC(W1),MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
	convertOrList(KB,Name,bodyMC(W1),MC2,CTermList,X,
		      HYPS,AB,CALLS,InCTermList,PTL).
%convertOrList(KB,Name,bodyMC(W1),MC2,[CTerm|CTermList],X,
% 	      HYPS,AB,CALLS,
%               (InCTerm ; (NewH1 = [HypTerm|H1], InCTermList)),[PT1|PTL]) :-
% 	HYPS = [or(H1),rl(H2),fl(H3)],
% 	Name = rn(AX,_S,_O),
% 	convertInAntecedent(KB,Name,bodyMC(W1),MC2,
%                             CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
% 	negate(CTerm,CTermN),
% 	constructMLHead(KB,rn(AX,_RN1,_S1,_O1),W1,CTermN,X,
% 			HYPS,AB,_CALLS,_,HypMLTerm),
% 	constructMLMark(HypMLTerm,HypTerm),
% 	convertOrList(KB,Name,bodyMC(W1),MC2,CTermList,X,
%		      [or(NewH1),rl(H2),fl(H3)],AB,CALLS,InCTermList,PTL).

/***********************************************************************
 *
 * convertInAntecedent(KB,+ModalSequence,+ConceptTerm,-Variable,
 *              -Hypotheses,-CallStack,+AxiomName,-InLiteral)
 *
 */

convertInAntecedent(KB,rn(AX,S1,_O),MC1,MC2,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	(isSlot(D) ; atomic(D)),
	!,
	constructMLCall(KB,rn(AX,_RN1,S1,_O1),MC1,MC2,
	                D,X,HYPS,AB,CALLS,PT1,InHead).
convertInAntecedent(KB,rn(AX,Source,_O),MC1,MC2,set(S1),X,HYPS,AB,CALLS,PT1,Body) :-
	constructMLCall(KB,rn(AX,_RN1,Source,_O1),MC1,MC2,
	                set(S1),X,HYPS,AB,CALLS,PT1,InHead1),
	Body = ((nonvar(S1), (nongeneric(X), member(X,S1))) ; InHead1),
	!.
convertInAntecedent(KB,rn(AX,Source,_O),MC1,MC2,not(set(S1)),X,HYPS,AB,CALLS,PT1,Body) :-
	constructMLCall(KB,rn(AX,_RN1,Source,_O1),MC1,MC2,
	                not(set(S1)),X,HYPS,AB,CALLS,PT1,InHead1),
	Body = ((nonvar(S1), (atomic(X), (nongeneric(X), not(member(X,S1)))) ; InHead1)),
	!.
convertInAntecedent(KB,Name,MC1,MC2,and(L),X,HYPS,AB,CALLS,and(PTL),Body) :-
	convertAndList(KB,Name,MC1,MC2,L,X,HYPS,AB,CALLS,Body,PTL),
	!.
convertInAntecedent(KB,Name,MC1,MC2,or(L),X,HYPS,AB,CALLS,or(PTL),Body) :-
	convertOrList(KB,Name,MC1,MC2,L,X,HYPS,AB,CALLS,Body,PTL),
	!.
convertInAntecedent(KB,rn(AX,S1,_O1),MC1,MC2,
	            not(D),X,HYPS,AB,CALLS,PT1,Body) :-
	constructMLCall(KB,rn(AX,_RN,S1,_O2),MC1,MC2,
	                not(D),X,HYPS,AB,CALLS,PT1,InHead),
	Body = InHead,
	!.
convertInAntecedent(KB,rn(AX,S1,_O1),bodyMC(CTX1),MC2,
	            naf(D),X,HYPS,AB,CALLS,PT1,Body) :-
	% instance the antecedent `x instance naf(D) is provable' means 
	% `x instance D is not provable'
	atomic(D),
	!,
	HYPS = [or(H1),rl(H2),fl(H3)],
	NewHYPS = [or(H1),rl([]),fl(H3)],
	constructMLHead(KB,rn(AX,_RN3,_S3,_O3),CTX1,naf(D),X,
			HYPS,AB,_CALLS,_,DefaultMLTerm),
	convertInAntecedent(KB,rn(AX,S1,_O1),bodyMC(CTX1),MC2,D,X,NewHYPS,
			    AB,CALLS,PT2,BodyD),
	PT1 = byDefault(instance(CTX1,not(D),X),hyp(NewHYPS),basedOn([])),
	constructMLMark(DefaultMLTerm,DefaultTerm),
	L1 = addDefaultML(DefaultTerm,H3),
	constructMLMark(BodyD,MarkD),
	Body = (member(MarkD,HYPS) ; (nongeneric(X), (not(member(DefaultMLTerm,CALLS)), (not(BodyD), nongeneric(X), L1)))),
	!.
convertInAntecedent(KB,rn(AX,S1,_O1),bodyMC(CTX1),MC2,
	            naf(D),X,HYPS,AB,CALLS,PT1,Body) :-
	% instance the antecedent `x instance naf(D) is provable' means 
	% `x instance D is not provable'
	HYPS = [or(H1),rl(H2),fl(H3)],
	NewHYPS = [or(H1),rl([]),fl(H3)],
	convertInAntecedent(KB,rn(AX,S1,_O1),bodyMC(CTX1),MC2,D,X,NewHYPS,
			    AB,CALLS,PT2,BodyD),
	constructMLMark(BodyD,MarkD),
	normalizeNot(not(D),D1),
	PT1 = byDefault(instance(CTX1,D1,X),hyp(NewHYPS),basedOn([])),
	constructMLHead(KB,rn(AX,_RN3,_S3,_O3),CTX1,D1,X,
			HYPS,AB,_CALLS,_,DefaultMLTerm),
	constructMLMark(DefaultMLTerm,DefaultTerm),
	L1 = addDefaultML(DefaultTerm,H3),
%	L1 = asserta(hypothesis(instance(KB,modal(CTX1),D1,X,hyp(HYPS),ab(AB),PT1))),
	Body = (nongeneric(X), (not(BodyD), nongeneric(X), L1)),
	!.
convertInAntecedent(KB,rn(AX,S1,_O1),MC1,MC2,
	            all(R,D),X,HYPS,AB,CALLS,or([and([PT2,PT1]),PT3]),
                    ((EqLiteral, Body); (InHead2; (C1, (C2, C3))))) :-
	% create a new skolem constant 
	gensymbol(skolem,[X,Y],SF),
	% construct equational literal
	constructEqCall(KB,rn(AX,_RN1,_S2,_O2),MC1,MC2,Y,SF,R,X,HYPS,AB,CALLS,PT2,EqLiteral),
	convertInAntecedent(KB,rn(AX,S1,_O3),MC1,MC2,D,Y,HYPS,AB,CALLS,PT1,Body),
	constructMLCall(KB,rn(AX,_RN4,_S4,_O4),MC1,MC2,not(normal(R)),X,HYPS,AB,CALLS,PT3,InHead2),
	MC1 = bodyMC(W1),
	C1 = hl_closed(KB,CTX,X,_,R),
	C2 = collectAllFillers(KB,W1,R,X,HYPS,D,CALLS,S),
	C3 = mapGoal(Body,Y,S),
	!.
convertInAntecedent(KB,rn(AX,S1,_O1),MC1,MC2,
 	            some(R,D),X,HYPS,AB,CALLS,and([PT2,PT1]),(EqLiteral, onceOrMore((Body, isGround(Y,true))))) :-
	/* construct equational literal */
	constructEqCall(KB,rn(AX,_RN1,S1,_O2),MC1,MC2,Y,_FF,R,X,
			HYPS,AB,CALLS,PT2,EqLiteral),
	convertInAntecedent(KB,rn(AX,_S2,_O3),MC1,MC2,D,Y,
			    HYPS,AB,CALLS,PT1,Body),
	!.
convertInAntecedent(KB,rn(AX,_S,_O1),bodyMC(CTX1),headMC(CTX2),
	            atleast(N,R),X,HYPS,AB,CALLS,PT1,Body) :-
	% construct equational literal
	constructSolveConMark(rn(AX,_RN1,_S1,_O2),CTX2,_FF1,R,X,'>=',N,
			 HYPS,AB,CALLS,Mark),
	Body = solveConstraint(KB,CTX1,(card,app((_FF2:R),X),'>=',N),_,
			       hyp(HYPS),ab(AB),call([Mark|CALLS]),PT1),
	!.
convertInAntecedent(KB,rn(AX,_S,_O),bodyMC(CTX1),headMC(CTX2),
	            atmost(N,R),X,HYPS,AB,CALLS,PT1,Body) :-
	/* construct right term */
        constructSolveConMark(rn(AX,_RN1,_S1,_O1),CTX2,_FF1,R,X,'=<',N,
			 HYPS,AB,CALLS,Mark),
	Body = solveConstraint(KB,CTX1,(card,app((_FF2:R),X),'=<',N),_,
			       hyp(HYPS),ab(AB),call([Mark|CALLS]),PT1),
	!.
convertInAntecedent(KB,rn(AX,S1,_O),bodyMC(CTXnew),headMC(CTXold),
	            b(MOp,P1,D),X,HYPS,AB,CALLS,PT1,InHead) :-
	/* construct right term */
	gensym(wp,WP),
	genagent(P1,skolemize,P),
	CTX1 = app(WP:m(MOp,P),CTXnew),
        constructMLCall(KB,rn(AX,_RN1,S1,_O1),bodyMC(CTX1),headMC(CTXold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInAntecedent(KB,rn(AX,S1,_O),bodyMC(CTXnew),headMC(CTXold),
	            bc(MOp,C,D),X,HYPS,AB,CALLS,and([PT1,PT2]),(InHead, Body)) :-
	/* construct right term */
	gensym(wp,WP),
	genagent(P1,skolemize,P),
	CTX1 = app(WP:m(MOp,P),CTXnew),
        constructMLCall(KB,rn(AX,_RN1,S1,_O1),bodyMC(CTX1),headMC(CTXold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	convertInAntecedent(KB,rn(AX,_RN2,_S2),bodyMC(CTXold),headMC(CTXold),
			    C,P,HYPS,AB,CALLS,PT2,Body),
	!.
convertInAntecedent(KB,rn(AX,S1,_O),bodyMC(CTXnew),headMC(CTXold),
	            d(MOp,P1,D),X,HYPS,AB,CALLS,PT1,InHead) :-
	/* construct right term */
        genagent(P1,free,P),
        CTX1 = app(_FF:m(MOp,P),CTXnew),
        constructMLCall(KB,rn(AX,_RN1,S1,_O1),bodyMC(CTX1),headMC(CTXold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInAntecedent(KB,rn(AX,S1,_O),bodyMC(CTXnew),headMC(CTXold),
	            dc(MOp,C,D),X,HYPS,AB,CALLS,and([PT1,PT2]),(InHead, Body)) :-
	/* construct right term */
        genagent(P1,free,P),
        CTX1 = app(_FF:m(MOp,P),CTXnew),
        constructMLCall(KB,rn(AX,_RN1,S1,_O1),bodyMC(CTX1),headMC(CTXold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	convertInAntecedent(KB,rn(AX,_RN2,_S2),bodyMC(CTXold),headMC(CTXold),
			    C,P,HYPS,AB,CALLS,PT2,Body),
	!.
convertInAntecedent(KB,rn(AX,S,_O),MC1,MC2,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	CON = X,
	constructMLCall(KB,rn(AX,_RN1,S,_O1),MC1,MC2,
	                D,CON,HYPS,AB,CALLS,PT1,InHead),
	!.

convertInAntecedentList(_KB,_,_,_,[],_X,_HYPS,_AB,_CALLS,[],[]) :-
	!.
convertInAntecedentList(KB,Name,MC1,MC2,[NCT],X,HYPS,AB,CALLS,[PT1],[INCT]) :-
	convertInAntecedent(KB,Name,MC1,MC2,NCT,X,HYPS,AB,CALLS,PT1,INCT),
	!.
convertInAntecedentList(KB,Name,MC1,MC2,[NCT|NCTs],X,
                        HYPS,AB,CALLS,[PT1|PTL],[INCT|INCTs]) :-
	convertInAntecedent(KB,Name,MC1,MC2,NCT,X,HYPS,AB,CALLS,PT1,INCT),
	convertInAntecedentList(KB,Name,MC1,MC2,NCTs,X,
				HYPS,AB,CALLS,PTL,INCTs).


/***********************************************************************
 *
 * convertInConsequence(KB,+ModalSequence,+ConceptTerm,-X,
 *               -Hypotheses, -CallStack, +AxiomName,
 *               -InLiteral)
 *
 */

convertInConsequence(KB,Pr,rn(AX,RN,_S,O),CTX,W1,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	(isSlot(D) ; atomic(D)),
	!,
	typeOfDefinition(KB,CTX,D,S2),
	constructKBHead(KB,Pr,rn(AX,RN,S2,O),W1,D,X,HYPS,AB,CALLS,PT1,InHead).
convertInConsequence(KB,Pr,rn(AX,RN,_S,O),CTX,W1,some(R,D),X,
                     HYPS,AB,CALLS,PT1,(EqLiteral, InHead)) :-
	% construct equational literal
	gensymbol(skolem,[X,Y],SF),
	constructEqCall(KB,rn(AX,_RN2,_S2,_O2),bodyMC(W1),headMC(W1),
	                Y,SF,R,X,HYPS,AB,CALLS,PT2,EqLiteral),
	typeOfDefinition(KB,CTX,D,S2),
	convertInConsequence(KB,Pr,rn(AX,RN,S2,O),CTX,W1,D,Y,HYPS,AB,CALLS,
                             and([PT1,PT2]),InHead),
	!.
convertInConsequence(KB,Pr,rn(AX,RN,_S,O),CTX,W1,all(R,D),X,
                     HYPS,AB,CALLS,PT1,((EqCall, isGround(Y,true)), InHead)) :-
	% construct equation literal
	constructEqCall(KB,rn(AX,_RN2,_S2,_O2),bodyMC(W1),headMC(W1),
	                Y,_FF,R,X,HYPS,AB,CALLS,PT2,EqCall),
	typeOfDefinition(KB,CTX,D,S2),
	convertInConsequence(KB,Pr,rn(AX,RN,S2,O),CTX,W1,D,Y,HYPS,AB,CALLS,
                             and([PT1,PT2]),InHead),
	!.
convertInConsequence(KB,_Pr,Name,_CTX,W1,atleast(N,R),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	/* construct role term */
        constructConHead(KB,Name,W1,_FF,R,X,'>=',N,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(KB,_Pr,Name,_CTX,W1,atmost(N,R),X,HYPS,AB,CALLS,PT1,InHead) :-
	/* construct role term */
        constructConHead(KB,Name,W1,_FF,R,X,'=<',N,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(KB,Pr,rn(AX,RN,_S,O),CTX,W1,not(D),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	typeOfDefinition(KB,CTX,D,S2),
	constructKBHead(KB,Pr,rn(AX,RN,S2,O),W1,not(D),X,
			HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(KB,Pr,rn(AX,RN,_S,O),CTX,W1,naf(D),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	% instance the consequence not and naf have the same meaning
	typeOfDefinition(KB,CTX,D,S2),
	constructKBHead(KB,Pr,rn(AX,RN,S2,O),W1,not(D),X,
			HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(KB,Pr,rn(AX,RN,_S,O),CTX,W1,set(Set1),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	typeOfDefinition(KB,CTX,D,S2),
	constructKBHead(KB,Pr,rn(AX,RN,S2,O),W1,set(Set1),X,
			HYPS,AB,CALLS,PT1,InHead),
	!.
%convertInConsequence(KB,rn(AX,RN,_S,O),CTX,W1,b(MOp,P,D),X,
%                      HYPS,AB,CALLS,PT1,InHead) :-
%	gensym(wp,WP),
%	CTX1 = app(WP:m(MOp,P),W1),
%	typeOfDefinition(KB,CTX,D,S2),
%	constructMLHead(KB,rn(AX,RN,S2,O),CTX1,D,X,HYPS,AB,CALLS,PT1,InHead),
%	!.
%convertInConsequence(KB,rn(AX,RN,_S,O),CTX,W1,d(MOp,P,D),X,HYPS,CALLS,PT1,InHead) :-
%	CTX1 = app(WP:m(MOp,P),W1),
%	typeOfDefinition(KB,CTX,D,S2),
%	constructMLHead(KB,rn(AX,RN,S2,O),CTX1,D,X,HYPS,AB,CALLS,PT1,InHead),
%	!.
convertInConsequence(KB,Pr,rn(AX,RN,_S,O),CTX,W1,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	/* add loop check to control list */
        CON = X,
	typeOfDefinition(KB,CTX,D,S2),
	constructKBHead(KB,Pr,rn(AX,RN,S2,O),W1,D,CON,HYPS,AB,CALLS,PT1,InHead).

convertInConsequenceList(_KB,_Pr,_Name,_CTX,[],_X,_HYPS,_AB,_CALLS,no,[]) :-
	!.
convertInConsequenceList(KB,Pr,Name,CTX,[NCT],X,HYPS,AB,CALLS,[INCT]) :-
	convertInConsequence(KB,Pr,Name,CTX,NCT,X,HYPS,AB,CALLS,INCT),
	!.
convertInConsequenceList(KB,Pr,Name,CTX,[NCT|NCTs],X,
                         HYPS,AB,CALLS,[INCT|INCTs]) :-
	convertInConsequence(KB,Pr,Name,CTX,NCT,X,HYPS,AB,CALLS,INCT),
	convertInConsequenceList(KB,Pr,Name,CTX,NCTs,X,HYPS,AB,CALLS,INCTs).

/***********************************************************************
 *
 * convert_loop(LoopTerm,+CALLS,+Constraint,-CALLS)
 *
 */

convert_loop(no,CALLS,_,CALLS).
convert_loop(_,CALLS,CON,[CON|CALLS]).

convertToGoal(KB,RN,CTX1,CN,X,HYPS,AB,CALLS,PT,G) :-
	convertCTX(negative,KB,[[],true],CTX1,[],[W1,G1],_),
	getQuery(KB,W1,CN,X,PT,G),
%	G = call((instance(KB,RN,modal(W1),CN,X,hyp(HYPS),ab(AB),call(CALLS),PT), G1)),
	!.

	
nongeneric(X) :-
	isSlot(X),
	!.
nongeneric(aaa) :-
	!,
	fail.
nongeneric(_) :-
	!.

isGround(Result) :-ground(Result).


isGround(T,Result) :-
	isSlot(T),
	!,
	Result = false.
isGround(T,Result) :-
	atomic(T),
	!,
	Result = true.
isGround(T,Result) :-
	T =.. [F|Args],
	map(isGround,Args,Results),
	member(false,Results),
	!,
	Result = false.
isGround(T,true) :-
	!.
/**********************************************************************
 *
 * @(#) tellRole.pll 1.8@(#)
 *
 */

/***********************************************************************
 *
 * assertRoleLInR(KB,+CTX,+RN,+RT,+AN)
 *
 */

assertRoleLInR(KB,CTX,R1,inverse(R2),AN) :-
	!,
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN),
	ruleName(AN,RN,user,lInR,Name),
	constructEqHead(KB,Name,W1,X,inverse(F),R2,app((F:R1),X),HYPS,AB,CALLS,true,EqLiteral1), 
%	asserta((EqLiteral1 :- G1)),
	constructEqHead(KB,Name,W1,X,F,R2,app((inverse(F):R1),X),HYPS,AB,CALLS,true,EqLiteral2), 
%	asserta((EqLiteral2 :- G1)),
	gensym(rule,RN3),
	constructEqHead(KB,rn(AN,RN3,user,rInL),W1,X,inverse(F),inverse(R2),
			Y,HYPS,AB,CALLS,PT1,EqLiteral3),
	constructEqCall(KB,rn(AN,RN3,_S3,_O3),bodyMC(W1),headMC(W1),X,F,R1,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral4),
%	asserta((EqLiteral3 :- cCS(CALLS,true), (call(G1), EqLiteral4))),
	gensym(rule,RN4),
	constructEqHead(KB,rn(AN,RN4,user,rInL),W1,Y,inverse(F1),R2,X,HYPS,AB,CALLS,
			PT2,EqLiteral5),
	constructEqMark(rn(AN,RN4,_S3,_O3),W1,X,F1,R1,Y,HYPS,AB,CALLS,EqMark2),
	constructEqCall(KB,rn(AN,RN4,_S3,_O3),bodyMC(W1),headMC(W1),
			X,F1,R1,Y,HYPS,AB,CALLS,PT2,EqLiteral6),
	asserta((EqLiteral5 :- cCS(CALLS,EqMark2), (call(G1), EqLiteral6))),
	true.
assertRoleLInR(KB,CTX,R1,and(RL),AN) :-
	!,
	assertAndConstraintLInR(KB,CTX,R1,and(RL),AN),
        assertAndRoleLInR(KB,CTX,R1,and(RL),AN).
assertRoleLInR(KB,CTX,R1,restr(R2,C),AN) :-
	!,
	assertRoleLInRRestr1(KB,CTX,R1,restr(R2,C),AN),
	assertRoleLInRRestr3(KB,CTX,R1,restr(R2,C),AN),
	getComplementRole(KB,CTX,R1,restr(R2,C),R3,restr(R2,CNF)),
	assertRoleLInRRestr4(KB,CTX,R1,restr(R2,C),R3,restr(R2,CNF),AN).
assertRoleLInR(KB,CTX,R1,R2,AN) :-
	!,
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF1),
	gensym(rule,RN1),
	ruleName(AN,RN1,user,lInR,Name1),
	constructEqHead(KB,Name1,W1,Y,SF1,R2,X,HYPS,AB,CALLS,PT1,EqLiteral2),
	constructEqMark(rn(AN,RN1,_S2,_O2),W1,Y,SF1,R2,X,HYPS,AB,CALLS,EqMark2),
	constructEqCall(KB,rn(AN,RN1,_S3,_O3),bodyMC(W1),headMC(W1),Y,_FF,R1,X,HYPS,AB,CALLS,PT1,EqLiteral1),
	asserta((EqLiteral2 :- (cCS(CALLS,EqMark2), (call(G1), EqLiteral1)))),
	gensymbol(skolem,[X,Y],SF2),
	gensym(rule,RN2),
	constructConHead(KB,rn(AN,RN2,user,lInR),W1,SF2,R2,X,'>=',N,
                         HYPS,AB,CALLS,PT1,C2),
	constructConMark(C2,Mark2),
	constructSolveConMark(rn(AN,RN2,_S4,_O4),W1,_FF3,R1,X,'>=',N,HYPS,AB,CALLS,Mark1),
	C1 = solveConstraint(KB,W1,(card,app((_FF:R1),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT1),
	asserta((C2 :- (cCS(CALLS,Mark2), (call(G1), C1)))),
	gensym(rule,RN5),
	gensym(skolem,SF3),
	constructConHead(KB,rn(AN,RN5,user,lInR),W1,SF3,R1,X,'=<',N,
                         HYPS,AB,CALLS,PT1,C4),
	constructConMark(C4,Mark4),
	constructSolveConMark(rn(AN,RN5,_S6,_O6),W1,_FF4,R2,X,'=<',N,HYPS,AB,CALLS,Mark5),
	C5 = solveConstraint(KB,CTX,(card,app((_FF2:R2),X),'=<',N),_,hyp(HYPS),ab(AB),call([Mark5|CALLS]),PT1),
	asserta((C4 :- (cCS(CALLS,Mark4), (call(G1), C5)))).
	
/**********************************************************************
 *
 * assertRoleLInRRestr1(+CTX,+R1,restr(+R2,+C),+AN)
 * handles the case R1 is included instance restr(R2,C).
 * asserts the constraints and membership clauses describing the 
 * relationship of R1 and R2.
 *
 */

assertRoleLInRRestr1(KB,CTX,R1,restr(R2,C),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN1),
	typeOfDefinition(KB,CTX,C,S1),
	constructMLHead(KB,rn(AN,RN1,S1,lInR),W1,C,Y,HYPS,AB,CALLS,PT1,InHead),
	constructMLMark(InHead,InMark),
% uh (06.04.94)
% adding InMark to the call stack is wrong
%	constructEqCall(KB,rn(AN,RN1,_S2,_O2),bodyMC(W1),headMC(W1),
%	                Y,F,R1,X,HYPS,AB,[InMark|CALLS],PT1,EqLiteral11),
% new version:
	constructEqCall(KB,rn(AN,RN1,_S2,_O2),bodyMC(W1),headMC(W1),
	                Y,F,R1,X,HYPS,AB,[InMark|CALLS],PT1,EqLiteral11),
	asserta((InHead :- (cCS(CALLS,InMark), (call(G1), (EqLiteral11, isGround(Y,true)))))),
	gensym(skolem,SF),
	gensym(rule,RN2),
	typeOfDefinition(KB,CTX,C,S2),
	constructEqHead(KB,rn(AN,RN2,S2,lInR),W1,Y,SF,R2,X,
                        HYPS,AB,CALLS,PT2,EqLiteral2),
	constructEqMark(rn(AN,RN2,_S3,_O3),W1,Y,SF,R2,X,HYPS,AB,CALLS,EqMark2),
% uh (06.04.94)
% adding EqMark2 to the call stack is wrong
%	constructEqCall(KB,rn(AN,RN2,_S4,_O4),bodyMC(W1),headMC(W1),
%                        Y,F,R1,X,HYPS,AB,[EqMark2|CALLS],PT2,EqLiteral21),
% new version:
	constructEqCall(KB,rn(AN,RN2,_S4,_O4),bodyMC(W1),headMC(W1),
                        Y,F,R1,X,HYPS,AB,CALLS,PT2,EqLiteral21),
	asserta((EqLiteral2 :- (cCS(CALLS,EqMark2), (call(G1), EqLiteral21)))),
	!.


assertRoleLInRRestr3(KB,CTX,R1,restr(R2,C),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN1),
	typeOfDefinition(KB,CTX,C,S1),
	constructConHead(KB,rn(AN,RN1,S1,lInR),W1,G,R2,X,'>=',N,
	                 HYPS,AB,CALLS,and([PT2,PT4]),C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN,_RN2,_S2,_O2),
                         W1,_FF1,R1,X,'>=',N,HYPS,AB,CALLS,Mark2),
	C2 = solveConstraint(KB,W1,(card,app((F:R1),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	asserta((C1 :- (cCS(CALLS,Mark1), (call(G1), C2)))),
	gensym(rule,RN3),
	constructConHead(KB,rn(AN,RN3,S1,lInR),W1,G,R1,X,'=<',N,
                         HYPS,AB,CALLS,PT4,C3),
	constructConMark(C3,Mark3),
	constructSolveConMark(rn(AN,RN3,_S4,_O4),
                         W1,_FF3,R2,X,'=<',N,HYPS,AB,CALLS,Mark4),
	C4 = solveConstraint(KB,W1,(card,app((F:R2),X),'=<',N),_,hyp(HYPS),ab(AB),call([Mark4|CALLS]),PT4),
	asserta((C3 :- (cCS(CALLS,Mark3), (call(G1), C4)))).


/**********************************************************************
 *
 * assertRoleLInRRestr2(KB,+CTX,+R1,restr(+R2,+C1),
 *                          +R3,restr(+R2,+C2),+AN)
 * handles the case R1 is included instance restr(R2,C).
 * asserts the constraints describing the relationship between 
 * R1 = restr(R2,C1), R3 = restr(R2,not(C1)) and R2.
 *
 */

assertRoleLInRRestr4(KB,CTX,R1,restr(R2,_C),R3,restr(R2,_CNF),AN1) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(KB,rn(AN1,RN,user,lInR),W1,_FF,R1,X,'=<',N1,
	                 HYPS,AB,CALLS,and([PT2,PT3]),C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN1,RN,_S2,_O2),W1,FF1,R2,X,'=<',N2,HYPS,AB,CALLS,Mark2),
	C2 = solveConstraint(KB,W1,(card,app((FF1:R2),X),'=<',N2),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	constructSolveConMark(rn(AN1,RN,_S3,_O3),W1,FF2,R3,X,'>=',N3,HYPS,AB,CALLS,Mark3),
	C3 = solveConstraint(KB,W1,(card,app((FF2:R3),X),'>=',N3),_,hyp(HYPS),ab(AB),call([Mark3|CALLS]),PT3),
	asserta((C1 :- (cCS(CALLS,Mark1), (call(G1), (C2, (C3, (subtractRestrictions(N2,N3,M), comparison('=<',M,N1)))))))),
	!.




/***********************************************************************
 *
 * assertAndRoleLInR(+CTX,+Lit,+X,+Y,+RT,+CALLS,+AN)
 *
 */

assertAndRoleLInR(_,_CTX,_,and([]),_AN) :-
	!.
assertAndRoleLInR(KB,CTX,R1,and([R2|RL]),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(skolem,SF),
	gensym(rule,RN),
	constructEqHead(KB,rn(AN,RN,user,lInR),
                        W1,Y,SF,R2,X,HYPS,AB,CALLS,PT1,EqLiteral2),
	constructEqMark(rn(AN,RN,_S1,_O1),W1,Y,SF,R2,X,HYPS,AB,CALLS,EqMark2),
% uh (06.04.94)
% adding EqMark2 to the call stack is wrong
%	constructEqCall(KB,rn(AN,RN,_S2,_O2),bodyMC(W1),headMC(W1),Y,_F,R1,X,
%	                HYPS,AB,[EqMark2|CALLS],PT1,EqLiteral1),
% new version:
	constructEqCall(KB,rn(AN,RN,_S2,_O2),bodyMC(W1),headMC(W1),Y,_F,R1,X,
	                HYPS,AB,CALLS,PT1,EqLiteral1),
	asserta((EqLiteral2 :- (cCS(CALLS,EqMark2), (call(G1), EqLiteral1)))),
	assertAndRoleLInR(KB,CTX,R1,and(RL),AN).

/***********************************************************************
 *
 * assertAndConstraintLInR(+CTX,+RN,+RT,+AN)
 *
 */

assertAndConstraintLInR(_,_CTX,_,and([]),_AN) :-
	!.
assertAndConstraintLInR(KB,CTX,R1,and([R2|RL]),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN),
	ruleName(AN,RN,user,lInR,Name),
	constructConHead(KB,Name,W1,_FF,R1,X,Rel,N,HYPS,AB,CALLS,PT2,C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF2,R2,X,Rel,N,HYPS,AB,CALLS,Mark2),	
	gensymbol(skolem,[X],SF),
	C2 = solveConstraint(KB,W1,(card,app((SF:R2),X),Rel,N),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	asserta((C1 :- cCS(CALLS,Mark1), (call(G1), C2))),
	assertAndConstraintLInR(KB,CTX,R1,and(RL),AN).

/***********************************************************************
 *
 * assertAndConstraintRInL(+CTX,+RN,+RT,+AN)
 *
 */

assertAndConstraintRInL(_,_CTX,_,and([]),_AN) :-
	!.
assertAndConstraintRInL(KB,CTX,R1,and([R2|RL]),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(KB,rn(AN,RN,user,rInL),W1,_FF,R2,X,'>=',N,
	                 HYPS,AB,CALLS,PT2,C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF1,R1,X,'>=',N,HYPS,AB,CALLS,Mark2),
	gensymbol(skolem,[X],SF),
	C2 = solveConstraint(KB,W1,(card,app((SF:R1),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	asserta((C1 :- cCS(CALLS,Mark1), (call(G1), C2))),
	assertAndConstraintRInL(KB,CTX,R1,and(RL),AN).


/***********************************************************************
 *
 * assertRoleRInL(KB,+CTX,+RN,+RT,+AN)
 *
 */

assertRoleRInL(KB,CTX,R1,inverse(R2),_AN) :-
	!,
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN1),
	constructEqHead(KB,rn(AN,RN1,user,rInL),W1,X,F,R1,
                        app((inverse(F):R2),X),HYPS,AB,CALLS,
			true,EqLiteral1),
%	asserta((EqLiteral1 :- call(G1))),
	gensym(rule,RN2),
	constructEqHead(KB,rn(AN,RN2,user,rInL),
                        W1,X,inverse(F),R1,app((F:R2),X),HYPS,AB,CALLS,
			true,EqLiteral2),
%	asserta((EqLiteral2 :- call(G1))),
	gensym(rule,RN3),
	constructEqHead(KB,rn(AN,RN3,user,rInL),W1,Y,inverse(F),inverse(R2),
			X,HYPS,AB,CALLS,PT1,EqLiteral3),
	constructEqCall(KB,rn(AN,RN3,_S3,_O3),bodyMC(W1),headMC(W1),X,F,R1,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral4),
%	asserta((EqLiteral3 :- cCS(CALLS,true), (call(G1), EqLiteral4))),
	gensym(rule,RN4),
	constructEqHead(KB,rn(AN,RN4,user,rInL),W1,Y,inverse(F1),R1,X,HYPS,AB,CALLS,
			PT2,EqLiteral5),
	
        constructEqMark(rn(AN,RN4,_S3,_O3),W1,X,F1,R2,Y,HYPS,AB,CALLS,EqMark2),
	constructEqCall(KB,rn(AN,RN4,_S3,_O3),bodyMC(W1),headMC(W1),
			X,F1,R2,Y,HYPS,AB,CALLS,PT2,EqLiteral6),
	asserta((EqLiteral5 :- cCS(CALLS,EqMark2), (call(G1), EqLiteral6))).
assertRoleRInL(KB,CTX,R1,restr(R2,C), AN) :-
	!,
	assertRoleRInLRestr1(KB,CTX,R1,restr(R2,C),AN),
	getComplementRole(KB,CTX,R1,restr(R2,C),R3,restr(R2,CNF)),
	assertRoleRInLRestr2(KB,CTX,R1,restr(R2,C),R3,restr(R2,CNF),AN),
	assertRoleRInLRestr3(KB,CTX,R1,restr(R2,C),AN),
	assertRoleRInLRestr4(KB,CTX,R1,restr(R2,C),R3,restr(R2,CNF),AN).
assertRoleRInL(KB,CTX,R1,and(RL),AN) :-
	!,
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF),
	gensym(rule,RN1),
	roleBody(KB,W1,and(RL),X,Y,HYPS,AB,CALLS,AN,Body,PTL),
	constructEqHead(KB,rn(AN,RN1,user,rInL),
                        W1,Y,SF,R1,X,HYPS,AB,CALLS,and([PTL]),EqLiteral1),
	constructEqMark(rn(AN,RN1,_S2,_O2),W1,Y,SF,R1,X,HYPS,AB,CALLS,EqMark1),
	asserta((EqLiteral1 :- (cCS(CALLS,EqMark1), (call(G1), Body)))),
	assertAndConstraintRInL(KB,CTX,R1,and(RL),AN).
assertRoleRInL(KB,CTX,R1,R2,AN) :-
	!,
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF),
	gensym(rule,RN1),
	constructEqHead(KB,rn(AN,RN1,user,rInL),W1,X,SF,R1,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral1),
	constructEqMark(rn(AN,RN1,_S2,_O2),W1,X,SF,R1,Y,HYPS,AB,CALLS,EqMark1),
	constructEqCall(KB,rn(AN,RN1,_S3,_O3),bodyMC(W1),headMC(W1),X,_F,R2,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral2),
	asserta((EqLiteral1 :- (cCS(CALLS,EqMark1), (call(G1), EqLiteral2)))),
	gensym(rule,RN2),
	constructConHead(KB,rn(AN,RN2,user,rInL),W1,_FF5,R1,X,'>=',N,
	                 HYPS,AB,CALLS,PT1,C2),
	constructConMark(C2,Mark2),
	constructSolveConMark(rn(AN,RN2,_S4,_O4),W1,_FF3,R2,X,'>=',N,HYPS,AB,CALLS,Mark1),
	C1 = solveConstraint(KB,W1,(card,app((_FF:R2),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT1),
	asserta((C2 :- (cCS(CALLS,Mark2), (call(G1), C1)))),
	gensym(rule,RN5),
	gensym(skolem,SF3),
	constructConHead(KB,rn(AN,RN5,user,rInL),W1,SF3,R2,X,'=<',N,
	                 HYPS,AB,CALLS,PT5,C4),
	constructConMark(C4,Mark4),
	constructSolveConMark(rn(AN,RN5,_S6,_O6),W1,_FF4,R1,X,'=<',N,HYPS,AB,CALLS,Mark5),
	C5 = solveConstraint(KB,W1,(card,app((_FF2:R1),X),'=<',N),_,hyp(HYPS),ab(AB),call([Mark5|CALLS]),PT5),
	asserta((C4 :- (cCS(CALLS,Mark4), (call(G1), C5)))).

/**********************************************************************
 *
 * getComplementRole(+CTX,restr(+R2,C),-R3,restr(+R2,-CNF))
 * CNF is the normalform of not(C).
 * If there is already a role name R for restr(R2,CNF) then R3
 * will be instantiated with R.
 * If there is no role name for restr(R2,CNF) then a role name R
 * is generated, clauses for R will be provided, and R3 will be
 * instantiated with R.
 *
 */
 
getComplementRole(KB,CTX,_R1,restr(R2,C),R3,restr(R2,CNF)) :-
	negate(C,CN),
	cnf(CN,CNF),
	roleEqualSets(KB,system,CTX,R3,restr(R2,CNF),_AX),
	!.
getComplementRole(KB,CTX,_R1,restr(R2,C),R3,restr(R2,CNF)) :-
	gensym(role,R3),
	negate(C,CN),
	cnf(CN,CNF),
	gensym(axiom,AN),
	asserta(roleEqualSets(KB,system,CTX,R3,restr(R2,CNF),AN)),
	assertRoleLInRRestr1(KB,CTX,R3,restr(R2,CNF),AN),
	assertRoleLInRRestr3(KB,CTX,R3,restr(R2,CNF),AN),
	assertRoleRInLRestr1(KB,CTX,R3,restr(R2,CNF),AN),
%	assertRoleRInLRestr2(KB,CTX,R1,restr(R2,CNF),R3,restr(R2,C),AN),
	assertRoleRInLRestr3(KB,CTX,R3,restr(R2,CNF),AN),
	!.
	

/**********************************************************************
 *
 * assertRoleRInLRestr1(+CTX,+R1,restr(+R2,C),+AN)
 * handles the case restr(R2,C) is included instance R1.
 * asserts the constraints and membership clauses describing the 
 * relationship of R1 and R2.
 *
 */

assertRoleRInLRestr1(KB,CTX,R1,restr(R2,C),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF),
	gensym(rule,RN),
	constructEqHead(KB,rn(AN,RN,user,rInL),
                        W1,Y,SF,R1,X,HYPS,AB,CALLS,and([PTEq2,PTIn]),EqLiteral1),
	constructEqMark(rn(AN,RN,_S1,_O1),
                        W1,Y,SF,R1,X,HYPS,AB,CALLS,EqMark1),
	constructEqCall(KB,rn(AN,RN,_S2,_O2),
                        bodyMC(W1),headMC(W1),Y,_FF,R2,X,HYPS,AB,CALLS,PTEq2,EqLiteral2),
	constructMLCall(KB,rn(AN,RN,_S3,_O3),
                        bodyMC(W1),headMC(W1),C,Y,HYPS,AB,CALLS,PTIn,InHead),
	asserta((EqLiteral1 :- (cCS(CALLS,EqMark1), (call(G1), (EqLiteral2, (isGround(Y,true), onceOrMore(InHead))))))).


assertRoleRInLRestr2(KB,CTX,R1,restr(R2,_C),R3,restr(R2,_CNF),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(KB,rn(AN,RN,user,rInL),W1,_FF,R1,X,'>=',N1,
	                 HYPS,AB,CALLS,and([PT2,PT3]),D1),
	constructConMark(D1,Mark1),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF2,R2,X,'>=',_N2,HYPS,AB,CALLS,Mark2),
	D2 = solveConstraint(KB,W1,(card,app((_FF3:R2),X),'>=',N2),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	constructSolveConMark(rn(AN,RN,_S3,_O3),W1,_FF4,R3,X,'=<',_N3,HYPS,AB,CALLS,Mark3),
	D3 = solveConstraint(KB,W1,(card,app((_FF5:R3),X),'=<',N3),_,hyp(HYPS),ab(AB),call([Mark3|CALLS]),PT3),
	asserta((D1 :- (cCS(CALLS,Mark1), (call(G1), (D2, (D3, (subtractRestrictions(N2,N3,M), comparison('>=',M,N1)))))))),
	!.
	
	
assertRoleRInLRestr3(KB,CTX,R1,restr(R2,C),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	negate(C,CN),
	cnf(CN,CNF),
	gensym(rule,RN),
	typeOfDefinition(KB,W1,CNF,S),
	constructMLHead(KB,rn(AN,RN,S,user),
                        W1,CNF,Y,HYPS,AB,CALLS,and([PT1,PT2]),InHead),
	constructMLMark(InHead,Mark),
%	constructConMark(rn(AN,RN,_S2,_O2),W1,_FF2,R2,X,'>=',_N2,
%	                 HYPS,CALLS,Mark2),
%	D2 = solveConstraint(KB,CTX,(card,app((_FF4:R2),X),'>=',N2),(M2,L2),
%	                     hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT3),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF1,R1,X,'=<',_N1,
	                 HYPS,AB,CALLS,Mark1),
	D1 = solveConstraint(KB,W1,(card,app((_FF3:R1),X),'=<',N1),(M1,L1),
	                     hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT2),
	constructEqCall(KB,rn(AN,RN,_S3,_O3),bodyMC(W1),headMC(W1),
                        Y,_FF,R2,X,HYPS,AB,CALLS,PT1,EqLiteral2),
% Removed this rule for test purposes
% uh 03.10.92
%	asserta((InHead :- (cCS(CALLS,Mark), (call(G1), (EqLiteral2, (isGround(Y,true), (D1, (not(member((Y,_),L1)), N1 = M1)))))))),
	!.

assertRoleRInLRestr4(KB,CTX,R1,restr(R2,_C),R3,restr(R2,_CNF),AN) :-
	convertCTX(positive,KB,[[],true],CTX,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(KB,rn(AN,RN,user,rInL),W1,_FF2,R2,X,Rel,N2,
	                 HYPS,AB,CALLS,and([PT1,PT3]),D2),
	constructConMark(D2,Mark2),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF1,R1,X,Rel,_N1,
	                 HYPS,AB,CALLS,Mark1),
	D1 = solveConstraint(KB,W1,(card,app((_FF4:R1),X),Rel,N1),_,
	                     hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT1),
	constructSolveConMark(rn(AN,RN,_S3,_O3),W1,_FF3,R3,X,Rel,_N3,
	                 HYPS,AB,CALLS,Mark3),
	D3 = solveConstraint(KB,W1,(card,app((_FF5:R3),X),Rel,N3),_,
	                     hyp(HYPS),ab(AB),call([Mark3|CALLS]),PT3),
	asserta((D2 :- (cCS(CALLS,Mark2), (call(G1), (D1, (D3, (addRestrictions(N1,N3,M), comparison(Rel,M,N2)))))))),
	!.
	

/***********************************************************************
 *
 * roleBody(+CTX,+RT,+X,+Y,+CALLS,+AN,-Body)
 *
 */

roleBody(_,_CTX,and([]),_X,_Y,_,_,_,_,true,[]) :- 
	!.
roleBody(KB,CTX,and([R1]),X,Y,HYPS,AB,CALLS,AN,Body,[PT1]) :-
	constructEqCall(KB,rn(AN,_RN,_S1,_O1),bodyMC(CTX),headMC(CTX),Y,_FF,R1,X,HYPS,AB,CALLS,PT1,Body),
	!.
roleBody(KB,CTX,and([R1|RL]),X,Y,HYPS,AB,CALLS,AN,(EqLiteral, Body),[PT1|PTL]) :-
	constructEqCall(KB,rn(AN,_RN1,_S1,_O1),
                        bodyMC(CTX),headMC(CTX),Y,_FF,R1,X,HYPS,AB,CALLS,PT1,EqLiteral),
	roleBody(KB,CTX,and(RL),X,Y,HYPS,AB,CALLS,AN,Body,PTL).










/**********************************************************************
 *
 * @(#) testSigmahl.pll 1.6@(#)
 *
 */

testSigmahl :-
	testAllSigmahlExamples(1),
	!.

testSigmahl(N) :-
	testAllSigmahlExamples(N),
	!.

testAllSigmahlExamples(87) :-
	cmp_print('Test complete\n\n'),
	!.
testAllSigmahlExamples(N) :-
	initialize,
	cmp_print('Example '), cmp_print(N), cmp_nl, example(N),
	once(testSigmahlExample(N)),
	M is N + 1,
	testAllSigmahlExamples(M).

testSigmahlExample(1) :-	
	cmp_print('No goal for this example\n').
testSigmahlExample(2) :-
	printTime(setof(C,E^deduce(ex2,[],elementOf(mary,C),E),L1)), cmp_print(L1), cmp_nl,
	printTime(setof(D,F^deduce(ex2,[],elementOf(tom,D),F),L2)), cmp_print(L2), cmp_nl.
testSigmahlExample(3) :-
	tryGoal(inconsistent(ex3)),
	cmp_print('Failure expected instance example 3.\n').
testSigmahlExample(4) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(5) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testSigmahlExample(6) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testSigmahlExample(7) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(8) :-
	tryGoal(deduce(elementOf(tom,heterosexual))).
testSigmahlExample(9) :-
	tryGoal(deduce(elementOf(chris,male))).
testSigmahlExample(10) :-
	tryGoal(deduce(elementOf(tom,c2))).
testSigmahlExample(11) :-
	tryGoal(inconsistent(ex11)),
	cmp_print('Failure expected instance example 11.\n').
testSigmahlExample(12) :-
	tryGoal(subsumes([],c1,c2)),
	cmp_print('Failure expected instance example 12.\n'),
	tryGoal(not(subsumes([],c2,c1))),
	cmp_print('Failure expected instance example 12.\n').
testSigmahlExample(13) :-
	tryGoal(subsumes([],c1,c2)).
testSigmahlExample(14) :-
%	initialize, cmp_print('Example 14\n'), example(14),
%	tryGoal(subsumes([],c2,c1)),
	!.
testSigmahlExample(15) :-
	tryGoal(subsumes([],c2,c1)),
	cmp_print('Failure expected instance example 15.\n').
testSigmahlExample(16) :-
	tryGoal(subsumes([],c2,c1)).
testSigmahlExample(17) :-
	tryGoal(subsumes([],c2,c1)).
testSigmahlExample(18) :-
	tryGoal(deduce(elementOf(mary,c4))).
testSigmahlExample(19) :-
	tryGoal(deduce(elementOf(amy,female))),
	cmp_print('Failure expected instance example 19.\n').
testSigmahlExample(20) :-
	tryGoal(inconsistent(ex20)),
	cmp_print('Failure expected instance example 20.\n').
testSigmahlExample(21) :-
	cmp_print('No goal for this example\n'),
% 	deduce(elementOf(betty,female)),
	!.
testSigmahlExample(22) :-
% 	deduce(elementOf(amy,female)),
	cmp_print('No goal for this example\n').
testSigmahlExample(23) :-
% 	deduce(elementOf(amy,female))
	cmp_print('No goal for this example\n').
testSigmahlExample(24) :-
	tryGoal(deduce(elementOf(audi,c3))).
testSigmahlExample(25) :-
	tryGoal(not(deduce(elementOf(audi,c3)))).
testSigmahlExample(26) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)),
	cmp_print('Failure expected instance example 26.\n').
testSigmahlExample(27) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)),
	cmp_print('Failure expected instance example 27.\n').
testSigmahlExample(28) :-
	tryGoal(deduce([b(believe,john)],elementOf(audi,auto),_P)).
testSigmahlExample(29) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(30) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(31) :-
	tryGoal(deduce(elementOf(tom,onlyMaleChildren))).
testSigmahlExample(32) :-
	tryGoal(abduce(_H1,elementOf(robin,male),_E1)),
	tryGoal(abduce(_H2,elementOf(robin,female),_E2)).
testSigmahlExample(33) :-
	tryGoal(abduce(_H3,elementOf(nixon,dove),_E3)),
	tryGoal(abduce(_H4,elementOf(nixon,hawk),_E4)).
testSigmahlExample(34) :-
	tryGoal(inconsistent(ex34)).
testSigmahlExample(35) :-
	tryGoal(abduce(ex35,[],_H5,elementOf(john,fly),_E5)),
	tryGoal(not(abduce(ex35,[],_H8,elementOf(tweety,fly),_E8))).
testSigmahlExample(36) :-
	tryGoal(abduce(ex36,[],_H6,elementOf(nixon,dove),_E6)),
	tryGoal(abduce(ex36,[],_H7,elementOf(nixon,hawk),_E7)).
testSigmahlExample(37) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(38) :-
	tryGoal(deduce(elementOf(ideaste,c2))).
testSigmahlExample(39) :-
	tryGoal(deduce(elementOf(lucky,female))),
	tryGoal(assert_ind(lucky,male)),
	tryGoal(consistent([])).
testSigmahlExample(40) :-
	tryGoal(deduce(elementOf(peter,richPerson))),
	tryGoal(assert_ind(peter,poorPerson)),
	tryGoal(consistent([])),
	tryGoal(not(subsumes(richPerson,doctor))).
testSigmahlExample(41) :-
	tryGoal(deduce(elementOf(tom,richPerson))),
	tryGoal(assert_ind(tom,poorPerson)),
	tryGoal(consistent([])).
testSigmahlExample(42) :-
	tryGoal(deduce(elementOf(audi,fourWheels))),
	tryGoal(assert_ind(audi,fiveWheels)),
	tryGoal(consistent([])).
testSigmahlExample(43) :-
	tryGoal(deduce(elementOf(r,red))),
	tryGoal(deduce(elementOf(r,redOrYellow))),
	tryGoal(deduce(elementOf(r,colors))).
testSigmahlExample(44) :-
	tryGoal(subsumes(c2,c12)),
	cmp_print('Failure expected instance example 44.\n').
testSigmahlExample(45) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(46) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(47) :-
	tryGoal(deduce(elementOf(bmw,c3))),
	cmp_print('Failure expected instance example 47.\n').
testSigmahlExample(48) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(49) :-
	tryGoal(not(deduce(elementOf(p,c4)))).
testSigmahlExample(50) :-
	tryGoal(deduce(elementOf(peter,c0))).
testSigmahlExample(51) :-
	tryGoal(deduce(posInfl(a,d))),
	tryGoal(deduce(posInfl(b,d))),
	tryGoal(bagof(Y1,deduce(posInfl(a,Y1)),Y1s)),
	tryGoal(verifySolution(Y1s,[b,c,d,g])),
	tryGoal(deduce(infl(a,d,1.0))),
	tryGoal(bagof((X1,W1),deduce(infl(X1,e,W1)),X1W1Pairs)),
	tryGoal(verifySolution(X1W1Pairs,[(a,0.0),(b,-1.0),(g,1.0)])),
	tryGoal(deduce(simultInfl([a,h],d,2.0))),
	tryGoal(deduce(change(d,1.0))),
	tryGoal(bagof(X2,deduce(increase(X2)),X2s)),
	tryGoal(verifySolution(X2s,[b,c,d,g,a])).
testSigmahlExample(52) :-
	tryGoal(deduce(negInfl(withRebate,hasOverallCost))),
	tryGoal(deduce(posInfl(hasListPrice,hasOverallCost))),
	tryGoal(deduce(posInfl(hasCubicCapacity,hasPrice))),
	tryGoal(deduce(posInfl(hasCubicCapacity,hasOverallCost))),
	tryGoal(deduce(posInfl(hasCatConverter,hasOverallCost))),
	tryGoal(deduce(simultInfl([hasCubicCapacity,hasCatConverter],hasOverallCost,3.0))),
	tryGoal(deduce(simultInfl([hasCubicCapacity,hasCatConverter],hasMaxSpeed,-1.0))),
	tryGoal(deduce(leastInfl(hasCubicCapacity,hasMaxSpeed))),
	tryGoal(deduce(leastInfls([hasCatConverter,hasCubicCapacity],hasMaxSpeed))),
	tryGoal(deduce(maxPosInfl(hasCubicCapacity,hasOverallCost,2.0))),
	tryGoal(bagof((X1,W1),deduce(maxNegInfl(X1,hasMaxSpeed,W1)),X1W1Pairs)),
	tryGoal(verifySolution(X1W1Pairs,[(hasCatConverter,-1.0),(hasWeight,-1.0)])),
	tryGoal(bagof(X2,deduce(increase(X2)),X2s)),
	tryGoal(verifySolution(X2s,[hasFuelConsumption,hasListPrice,hasOverallCost,hasPrice,hasWeight,hasCubicCapacity])),
	tryGoal(bagof((X3,W3),(deduce(leastInfl(X3,hasMaxSpeed)),abduce(change(X3,W3),change(hasMaxSpeed,1.0))),X3W3s)),
	tryGoal(verifySolution(X3W3s,[(hasCatConverter,-1.0)])).
testSigmahlExample(53) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(54) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(55) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(56) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(57) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(58) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(59) :-
	tryGoal(sigma_ask(element(harry,parent))),
	tryGoal(sigma_ask(element(harry,person))),
	printTime(setof((X,Y),sigma_ask(role(child,X,Y)),L1)), cmp_print(L1), cmp_nl,
	printTime(setof(X,sigma_ask(roleDef(child,X)),L2)), cmp_print(L2), cmp_nl,
	printTime(setof((X,Y),sigma_ask(roleNr('marys-child',X,Y)),L3)), cmp_print(L3), cmp_nl,
	printTime(setof(X,sigma_ask(roleDefNr('marys-child',X)),L4)), cmp_print(L4), cmp_nl.
testSigmahlExample(60) :-
	tryGoal(deduce(ex60,[b(believe,peter)],elementOf(tom,richPerson),E)),
	tryGoal(assert_ind([b(believe,peter)],tom,not(richPerson))),
	tryGoal(inconsistent([b(believe,peter)])).
testSigmahlExample(61) :-
	tryGoal(deduce(elementOf(tweety,fly))),
	tryGoal(deduce(elementOf(tweety,nest))),
	tryGoal(deduce(elementOf(tweety,not(emu)))),
	tryGoal(deduce(elementOf(tweety,not(cuckoo)))),
	tryGoal(consistent([])).
testSigmahlExample(62) :-
	tryGoal(deduce(elementOf(tweety,fly))),
	tryGoal(deduce(elementOf(tweety,nest))),
	tryGoal(not(deduce(elementOf(tweety,not(emu))))),
	tryGoal(not(deduce(elementOf(tweety,not(cuckoo))))),
	tryGoal(not(deduce(elementOf(tweety,emu)))),
	tryGoal(not(deduce(elementOf(tweety,cuckoo)))),
	tryGoal(consistent([])).
testSigmahlExample(63) :-
	tryGoal(deduce(elementOf(tweety,fly))),
	tryGoal(deduce(elementOf(tweety,nest))),
	tryGoal(deduce(elementOf(tweety,not(emu)))),
	tryGoal(deduce(elementOf(tweety,not(cuckoo)))),
	tryGoal(deduce(elementOf(tweety,sparrow))),
	tryGoal(consistent([])).
testSigmahlExample(64) :-
	tryGoal(deduce(elementOf(peter,leftHandUsable))),
	tryGoal(deduce(elementOf(peter,rightHandUsable))),
	tryGoal(deduce(elementOf(peter,oneHandUsable))),
	tryGoal(deduce(elementOf(peter,bothHandsUsable))),
	tryGoal(not(deduce(elementOf(peter,not(bothHandsUsable))))).
testSigmahlExample(65) :-
	tryGoal(deduce(elementOf(peter,leftHandUsable))),
        tryGoal(deduce(elementOf(peter,rightHandUsable))),
	tryGoal(deduce(elementOf(peter,not(bothHandsUsable)))),
	tryGoal(deduce(elementOf(peter,oneHandUsable))).
testSigmahlExample(66) :-
	tryGoal(deduce(elementOf(peter,leftHandUsable))),
	tryGoal(deduce(elementOf(peter,rightHandUsable))),
	tryGoal(deduce(elementOf(peter,oneHandUsable))),
	tryGoal(deduce(elementOf(peter,not(bothHandsUsable)))),
	tryGoal(not(deduce(elementOf(peter,bothHandsUsable)))),
	tryGoal(not(deduce(elementOf(peter,not(oneHandUsable))))).
testSigmahlExample(67) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(68) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(69) :-
	tryGoal(deduce(elementOf(tweety,bird))),
	tryGoal(not(deduce(elementOf(tweety,not(bird))))),
	tryGoal(not(deduce(elementOf(tweety,fly)))),
	tryGoal(not(deduce(elementOf(tweety,not(fly))))),
	tryGoal(not(deduce(elementOf(tweety,nest)))),
	tryGoal(not(deduce(elementOf(tweety,not(nest))))).
testSigmahlExample(70) :-
	tryGoal(deduce(elementOf(b,clearTop))),
	tryGoal(deduce(elementOf(b,clearTop))),
	tryGoal(not(deduce(elementOf(a,clearTop)))),
	tryGoal(not(deduce(elementOf(a,not(clearTop))))).
testSigmahlExample(71) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(72) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(73) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(74) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(75) :-
	tryGoal(setof(C,ask([b(believe,ps),b(believe,pc)],elementOf(beetle,C)),L1)),
	tryGoal(setof(C,ask([b(believe,ps),b(believe,pc)],elementOf('bmw735',C)),L2)),
	tryGoal(assert_ind([b(believe,ps),b(believe,pc)],'bmw735',has_cat_conv)),
	tryGoal(setof(C,ask([b(believe,ps),b(believe,pc)],elementOf('bmw735',C)),L3)),
	tryGoal(assert_ind([b(believe,ps),b(believe,pc)],'bmw735',not(slow))),
	tryGoal(setof(C,ask([b(believe,ps),b(believe,pc)],elementOf('bmw735',C)),L4)),
	tryGoal(assert_ind([b(believe,ps),b(want,pc)],beetle,not(has_spoiler))),
	tryGoal(assert_ind([b(believe,ps),b(want,pc)],beetle,not(has_sliding_roof))),
	tryGoal(delete_ind([b(believe,ps)],pc,racer)),
	tryGoal(assert_ind([b(believe,ps)],pc,creeper)),
	tryGoal(setof(C,ask([b(believe,ps),b(believe,pc)],elementOf(beetle,C)),L5)),
	tryGoal(ask([b(believe,ps),b(believe,pc),b(believe,pc)],elementOf(beetle,not(slow)))),
	tryGoal(modalAxioms(kd5,believe,all)),
	tryGoal(not(ask([b(believe,ps),b(believe,pc),b(believe,pc)],elementOf(beetle,not(slow))))).
testSigmahlExample(76) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(77) :-
	tryGoal(deduce(elementOf(mary,grandparent),L1)),
	tryGoal(abduce(H,elementOf(mary,grandparent),L2)).
testSigmahlExample(78) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(79) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(80) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(81) :-
	tryGoal(deduce([b(believe,ps),b(believe,pc)],elementOf(audi,car))),
	tryGoal(deduce([b(believe,ps),b(believe,pc),b(believe,pc)],elementOf(audi,car))),
	tryGoal(not(deduce([b(believe,pc),b(believe,pc)],elementOf(audi,car)))),
	tryGoal(not(deduce([b(believe,ps),b(believe,ps)],elementOf(audi,car)))),
	tryGoal(not(deduce([b(believe,pc),b(believe,ps)],elementOf(audi,car)))).
testSigmahlExample(82) :-
	tryGoal(deduce(elementOf(car1,car))),
	tryGoal(deduce(elementOf(car1,atleast(1,restr(hasPart,seat))))),
	tryGoal(deduce(elementOf(car1,all(restr(hasPart,seat),some(hasHl_attribute,heatable))))).
testSigmahlExample(83) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(84) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(85) :-
	cmp_print('No goal for this example\n').
testSigmahlExample(86) :-
	cmp_print('No goal for this example\n').


tryGoal(G) :-
	call(G),
	!,
	cmp_print('Goal '), cmp_print(G), cmp_print(' succeeded\n').
tryGoal(G) :-
	cmp_print('Goal '), cmp_print(G), cmp_print(' failed\n').

/***********************************************************************
 *
 * verifySolution(+TestSol,+ExpectedSol)
 *
 *	prints an error message if TestSol and ExpectedSol do not
 *	match.
 */

verifySolution(TestSol,ExpectedSol) :-
	nonvar(ExpectedSol),
	nonvar(TestSol),
	!,
	TestSol = ExpectedSol.
verifySolution(TestSol,ExpectedSol) :-
	cmp_print('Solutions differ: test solution is '),
	cmp_print(TestSol),
	cmp_print(', while expected solution is '),
	cmp_print(ExpectedSol).

/**********************************************************************
 *
 * @(#) unfold.pll 1.5@(#)
 *
 */

/***********************************************************************
 *
 * unfold(KB,+Type,+CT,+List1,-CN,-List2)
 * Parameter: Type      'concept' or 'role'
 *            CT        concept term
 *            List1     List of tuples (Origin,T,T1,T2)
 *                      where Origin is either 'user' or 'system'
 *                            T      is either 'concept' or 'role'
 *                            T1     is a concept term or role term
 *                            T2     is a concept term or role term
 *            CN        concept name
 *            List2     List of triples (Origin,CN,CT)
 * unfolds concept terms or role terms so that instance List2 for all tuples
 * (O,T,CN,all(R1,C1)), (O,T,CN,and([C1,...,Cn])), 
 * (O,T,RN,and([R1,...,Rn])), ...
 * CN and the Ci are concept names and RN and the Ri are role names.
 *
 * 130892   UH   (c)
 *
 */


unfold(_KB,O,_Side,_Type,CT,DL1,O,CT,DL1) :-
	atomicConcept(CT),
	!.
unfold(_KB,_O,_Side,concept,not(CT),DL1,system,not(CT),DL1) :-
	atomicConcept(CT),
	!.
%unfold(KB,_O,_Side,concept,CT,DL1,user,C,DL1) :-
%	conceptEqualSets(KB,system,_,C,CT,_),
%	clause(conceptName(KB,_,_C),_),
%	!.
%unfold(KB,_O,_Side,concept,CT,DL1,system,C,DL1) :-
%	conceptEqualSets(KB,system,_,C,CT,_),
%	!.
unfold(KB,_O,left,concept,CT,DL1,system,CT1,DL2) :-
	gensym(concept,C),
	unfold(KB,[(system,concept,C,CT)],[(_,concept,C,CT,CT1)|DL]),
	append(DL1,DL,DL2).
unfold(KB,_O,right,concept,CT,DL1,system,C,DL2) :-
	gensym(concept,C),
	unfold(KB,[(system,concept,C,CT)],[(system,concept,C,CT,CT1)|DL]),
	append(DL1,[(system,concept,C,CT,CT1)|DL],DL2).
unfold(KB,_O,_Side,role,RT,DL1,system,R,DL2) :-
	gensym(role,R),
	unfold(KB,[(system,role,R,RT)],[(system,role,R,RT,RT1)|DL]),
	append(DL1,[(system,role,R,RT,RT1)|DL],DL2).

/***********************************************************************
 *
 * unfold(KB,+List1,-List2)
 * Parameter: List1     List of tuples (Origin,Type,T1,T2)
 *                      where Origin is either 'user' or 'system'
 *                            T      is either 'concept' or 'role'
 *                            T1     is a concept or role term
 *                            T2     is a concept or role term
 *            List2     List of tuples (Origin,Type,CN,CT)
 * unfolds concept terms or role terms so that instance List2 for all tuples
 * (O,T,CN,all(R1,C1)), (O,T,CN,and([C1,...,Cn])), 
 * (O,T,RN,and([R1,...,Rn])), ...
 * CN and the Ci are concept names and RN and the Ri are role names.
 *
 * 130892   UH   (c)
 *
 */

unfold(KB,[(Origin,concept,A,all(R,B))|L1],[(NewOrigin,concept,A1,all(R,B),all(R1,B1))|DL3]) :-
	unfold(KB,L1,L2),
	unfold(KB,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
	unfold(KB,Origin,right,concept,B,DL2,_NewOriginB1,B1,DL3),
        !.
unfold(KB,[(Origin,concept,A,some(R,B))|L1],[(NewOrigin,concept,A1,some(R,B),some(R1,B1))|DL3]) :-
	unfold(KB,L1,L2),
	unfold(KB,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
	unfold(KB,Origin,right,concept,B,DL2,_NewOriginB1,B1,DL3),
        !.
unfold(KB,[(Origin,concept,A,atmost(N,R))|L1],[(NewOrigin,concept,A1,atmost(N,R),atmost(N,R1))|DL2]) :-
	integer(N),
	unfold(KB,L1,L2),
	unfold(KB,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
        !.
unfold(KB,[(Origin,concept,A,atleast(N,R))|L1],[(NewOrigin,concept,A1,atleast(N,R),atleast(N,R1))|DL2]) :-
	integer(N),
	unfold(KB,L1,L2),
	unfold(KB,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
        !.
unfold(KB,[(Origin,Type,A,and(L1))|L2],[(NewOrigin,Type,A1,and(L1),and(L3))|L5]) :-
	unfold(KB,L2,DL1),
	unfold(KB,Origin,left,Type,A,DL1,NewOrigin,A1,L4),
	unfoldList(KB,Type,L1,L3,CL3),
	append(L4,CL3,L5),
	!.
unfold(KB,[(Origin,Type,A,set(L1))|L2],[(NewOrigin,Type,A1,set(L1),C)|L4]) :-
	unfold(KB,L2,DL1),
	unfoldSetToConcept(set(L1),C),
	unfold(KB,Origin,left,Type,A,DL1,NewOrigin,A1,L4),
	!.
unfold(KB,[(Origin,concept,A,or(L1))|L2],[(NewOrigin,concept,A1,or(L1),or(L3))|L5]) :-
	unfold(KB,L2,DL1),
	unfold(KB,Origin,left,concept,A,DL1,NewOrigin,A1,L4),
	unfoldList(KB,concept,L1,L3,CL3),
	append(L4,CL3,L5),
	!.
unfold(KB,[(Origin,concept,A,not(B))|L2],[(NewOrigin,concept,A1,not(B),not(B1))|L3]) :-
	unfold(KB,L2,L4),
	unfold(KB,Origin,left,concept,A,L4,NewOrigin,A1,L5),
	unfold(KB,Origin,right,concept,B,L5,_NewOriginB,B1,L3),
	!.
unfold(KB,[(Origin,concept,A,naf(B))|L2],[(NewOrigin,concept,A1,naf(B),naf(B1))|L3]) :-
	unfold(KB,L2,L4),
	unfold(KB,Origin,left,concept,A,L4,NewOrigin,A1,L5),
	unfold(KB,Origin,right,concept,B,L5,_NewOriginB,B1,L3),
	!.
unfold(KB,[(Origin,concept,A,b(O,P,B))|L2],[(NewOrigin,concept,A1,b(O,P,B),b(O,P,B1))|L3]) :-
	unfold(KB,L2,L4),
	unfold(KB,Origin,left,concept,A,L4,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,concept,B,DL1,_NewOriginB1,B1,L3),
	!.
unfold(KB,[(Origin,concept,A,bc(O,C,B))|L2],[(NewOrigin,concept,A1,bc(O,P,B),bc(O,C1,B1))|L5]) :-
	unfold(KB,L2,L4),
	unfold(KB,Origin,left,concept,A,L4,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,concept,C,DL1,_NewOriginB1,C1,L3),
	unfold(KB,Origin,right,concept,B,DL1,_NewOriginB1,B1,L4),
	append(L3,L4,L5),
	!.
unfold(KB,[(Origin,concept,A,dc(O,C,B))|L2],[(NewOrigin,concept,A1,dc(O,P,B),bc(O,C1,B1))|L5]) :-
	unfold(KB,L2,L4),
	unfold(KB,Origin,left,concept,A,L4,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,concept,C,DL1,_NewOriginB1,C1,L3),
	unfold(KB,Origin,right,concept,B,DL1,_NewOriginB1,B1,L4),
	append(L3,L4,L5),
	!.
unfold(KB,[(Origin,concept,A,d(O,P,B))|L2],[(NewOrigin,concept,A1,d(O,P,B1),d(O,P,B1))|L3]) :-
	unfold(KB,L2,L4),
	unfold(KB,Origin,left,concept,A,L4,NewOrigin,A1,DL1),
	unfold(KB,Origin,right,concept,B,DL1,_NewOriginB1,B1,L3),
	!.
unfold(KB,[(Origin,role,RN,restr(R,C))|L1],[(NewOrigin,role,RN1,restr(R,C),restr(R1,C1))|L2]) :-
	unfold(KB,L1,L3),
	unfold(KB,Origin,left,role,RN,L3,NewOrigin,RN1,L4),
	unfold(KB,Origin,right,role,R,L4,_NewOriginR1,R1,L5),
	unfold(KB,Origin,right,concept,C,L5,_NewOriginC1,C1,L2),
        !.
unfold(KB,[(Origin,role,RN,inverse(R))|L1],[(NewOrigin,role,RN1,inverse(R),inverse(R1))|L5]) :-
	unfold(KB,L1,L3),
	unfold(KB,Origin,left,role,RN,L3,NewOrigin,RN1,L4),
	unfold(KB,Origin,right,role,R,L4,_NewOriginR1,R1,L5),
        !.
unfold(_KB,[(Origin,Type,A,B)],[(Origin,Type,A,B,B)]) :-
	atomicConcept(B),
	!.
unfold(_KB,[],[]) :- !.
	

unfoldList(_KB,_,[],[],[]) :- !.
unfoldList(KB,Type,[CT1|CTL1],[CT1|CTL2],DL1) :-
	atomicConcept(CT1),
	unfoldList(KB,Type,CTL1,CTL2,DL1),
	!.
%unfoldList(KB,Type,[CT1|CTL1],[CN|CNL2],DL3) :-
%	conceptEqualSets(KB,system,_,CN,CT1,_),
%	!,
%	unfoldList(KB,Type,CTL1,CNL2,DL3).
unfoldList(KB,Type,[CT1|CTL1],[CN|CNL2],DL1) :-
	gensym(Type,CN),
	unfold(KB,[(system,Type,CN,CT1)],DL),
	unfoldList(KB,Type,CTL1,CNL2,DL3),
	append(DL,DL3,DL1).


/**********************************************************************
 *
 * atomicConcept(+CT) 
 * succeeds if the concept term CT can be regarded as a atomic concept
 * for our translation. In the current implementation variables, 
 * identifiers, singleton sets, and their negation are regarded as 
 * atomic.
 *
 * 130892   UH   (c)
 * 140892   UH   Added clauses for sets and negation of variables
 * 140892   UH   Documented
 *
 */

atomicConcept(CT) :-
	isSlot(CT),
	!.
atomicConcept(CT) :-
	atomic(CT),
	!.
atomicConcept(not(CT)) :-
	isSlot(CT),
	!.
atomicConcept(not(CT)) :-
	atomic(CT),
	!.
atomicConcept(set([E1])) :-
	!.
%atomicConcept(not(set([E1]))) :-
%	!.
	
/**********************************************************************
 *
 * unfoldElementToSet(+Set,-CT)
 * for a given set Set the concept term CT consisting of a disjunktion 
 * of all singleton set contained instance Set is computed.
 *
 * 130892   UH   (c)  
 * 140892   UH   Documented
 *
 */

unfoldElementToSet(E1,set([E1])).

unfoldSetToConcept(set([]),bot) :-
	!.
unfoldSetToConcept(set([E1]),set([E1])) :-
	!.
unfoldSetToConcept(set([E1|L1]),or(L2)) :-
	hop_map(unfoldElementToSet,[E1|L1],L2),
	!.


/**********************************************************************
 *
 * @(#) userInterface.pll 1.46@(#)
 *
 */

/***********************************************************************
 *
 * initialize
 * cleans TBox, ABox, hierarchies, ...
 *
 */

initialize :-
	retractCompiledPredicates(_),
	retractall(_,axiom/3),
	retractall(_,instance/9),
	retractall(_,kb_in/10),
	retractall(_,eq/9),
	retractall(_,constraint/8),
	retractall(_,hl_holds/5),
	retractall(_,hl_closed/5),
	retractall(_,compiledPredicate/2),
	retractall(_,conceptElement/7),
	retractall(_,conceptEqualSets/6),
	retractall(_,conceptHierarchy/3),
	retractall(_,conceptName/4),
	retractall(_,conceptSubsets/6),
	retractall(_,knowledgeBase/3),
	retractall(_,given_change/4),
	retractall(_,given_inflLink/4),
	retractall(_,modalAxioms/7),
	retractall(_,roleHl_attributes/5),
	retractall(_,roleDefault/4),
	retractall(_,roleDefNr/4),
	retractall(_,roleDomain/4),
	retractall(_,roleElement/8),
	retractall(_,roleEqualSets/6),
	retractall(_,roleHierarchy/3),
	retractall(_,roleName/4),
	retractall(_,roleNr/4),
	retractall(_,roleRange/4),
	retractall(_,roleSubsets/6),
	retractall(_,sub/4),
	retractall(_,succ/4),
	retractall(_,abductiveDerivation/3),
	retractall(_,consistencyDerivation/3),
	retractall(_,hypothesis/1),
	retractall(_,inconsistencyCheck/3),
	retractall(_,option/2),
	retractall(sub(_,_,_,_,_)),
	retractall(nsub(_,_,_,_,_)),
	retractall(succ(_,_,_,_,_)),
	retractall(_,sub3/2),
	retractall(_,nsub3/2),
	retractall(_,succ3/2),
	retractall(_,value/2),
	retractall(_,query/6),
	asserta(knowledgeBase(initial,kb(e0),'Initial KnowledgeBase')),
	asserta(currentKnowledgeBase(kb(e0))),
	initKnowledgeBase(initial),
	!.

retractRoles(KB) :-
 	clause(roleName(KB,_CTX,_,RN),_),
 	Head =.. [RN,_,_],
 	retractall(Head),
	fail.
retractRoles(_).


/**********************************************************************
 *
 * loadKB(+FileName)
 * 
 */

loadKB(FileName) :-
	see(FileName),
	repeat,
	read(Goal),
	doFileGoal(Goal),
	!.
loadKB(_) :-
	seen,
	!,
	fail.

loadKB(FileName,KBName) :-
	isSlot(KBName),
	loadKB(FileName),
	% The file FileName should contain a call to makeKnowledgeBase
	% Due to the definition of makeKnowledgeBase the new knowledgeBase
	% should be described by the first knowledgeBase/3 fact instance the
	% database.
	knowledgeBase(KBName,_,_),
	!.


doFileGoal('end_of_file') :-
	seen,
	!.
doFileGoal(Goal) :-
	once(call(Goal)),
	fail.

/**********************************************************************
 *
 * getKB(+Name,-Set)
 * Set contains all terminological and assertional axioms instance 
 * knowledge base Name.
 *
 */

getKB(Set) :- 
	getCurrentKnowledgeBase(Name),
	getKB(Name,Set),
	!.

getKB(KBName,Set08) :-
	knowledgeBase(KBName,Name,_Comment),
	bagofOrNil(Clause1,
                   [CTX,K1,C1,MOp1,A1]^(modalAxioms(Name,CTX,user,K1,C1,MOp1,A1), 
                   Clause1 = modalAxioms(CTX,K1,MOp1,A1)),Set1),
	bagofOrNil(Clause2,
                   [CTX2,W1,G1,A2,C2,Ax2]^(clause(conceptElement(Name,CTX2,W1,user,A2,C2,Ax2),G1),
                   Clause2 = assert_ind(CTX2,A2,C2)),Set2),
	bagofOrNil(Clause3,
                   [CTX3,W1,G1,A3,B3,R3,Ax3]^(clause(roleElement(Name,CTX3,W1,user,A3,B3,R3,Ax3)),
	           Clause3 = assert_ind(CTX3,A3,B3,R3)),Set3),
	bagofOrNil(Clause4,
                   [CTX4,CN4,CT4,Ax4]^(conceptEqualSets(Name,user,CTX4,CN4,CT4,Ax4),
                   Clause4 = defconcept(CTX4,CN4,CT4)),Set4),
	bagofOrNil(Clause5,
                   [CTX5,CN5,CT5,Ax5]^(conceptSubsets(Name,user,CTX5,CN5,CT5,Ax5),
                   Clause5 = defprimconcept(CTX5,CN5,CT5)),Set5),
	bagofOrNil(Clause6,
		   [CTX6,CN6,CT6,Ax6]^(roleEqualSets(Name,user,CTX6,CN6,CT6,Ax6),
                   Clause6 = defrole(CTX6,CN6,CT6)),Set6),
	bagofOrNil(Clause7,
                   [CTX7,CN7,CT7,Ax7]^(roleSubsets(Name,user,CTX7,CN7,CT7,Ax7),
		   Clause7 = defprimrole(CTX7,CN7,CT7)),Set7),
	bagofOrNil(Clause8,
		   [CTX8,X8,Y8,R8]^(hl_closed(Name,CTX8,X8,Y8,R8),
		   Clause8 = defhl_closed(CTX8,X8,Y8,R8)),Set8),
	append(   [],Set1,Set01),
	append(Set01,Set2,Set02),
	append(Set02,Set3,Set03),
	append(Set03,Set4,Set04),
	append(Set04,Set5,Set05),
	append(Set05,Set6,Set06),
	append(Set06,Set7,Set07),
	append(Set07,Set8,Set08),
	!.

/**********************************************************************
 *
 * saveKB(+KBName,+FileName)
 *
 */

saveKB(FileName) :-
	getCurrentKnowledgeBase(KBName),
	!,
	saveKB(KBName,FileName).

saveKB(KBName,FileName) :-
	knowledgeBase(KBName,KB,C),
	tell(FileName),
	cmp_writeq(makeKnowledgeBase(KBName,C)), cmp_write('.\n'),
	cmp_writeq(initKnowledgeBase(KBName)), cmp_write('.\n'),
	transformAndWrite(axiom(KB,_,Axiom),Axiom),
	transformAndWrite(modalAxioms(KB,MC71,user,AS72,AC73,MO74,_A75),
                          modalAxioms(KBName,MC71,AS72,MO74,AC73)),
	transformAndWrite(roleHl_attributes(KB,A71,B71,C71),
			  (knowledgeBase(KBName,NewKB,_), assert(roleHl_attributes(NewKB,A71,B71,C71)))),
	transformAndWrite(roleDefault(KB,A81,B81,C81),
			  (knowledgeBase(KBName,NewKB,_), assert(roleDefault(NewKB,A81,B81,C81)))),
	transformAndWrite(roleDefNr(KB,A91,B91,C91),
			  (knowledgeBase(KBName,NewKB,_), assert(roleDefNr(NewKB,A91,B91,C91)))),
	transformAndWrite(roleDomain(KB,A82,B82,C82),
			  (knowledgeBase(KBName,NewKB,_), assert(roleDomain(NewKB,A82,B82,C82)))),
	transformAndWrite(roleNr(KB,A83,B83,C83),
			  (knowledgeBase(KBName,NewKB,_), assert(roleNr(NewKB,A83,B83,C83)))),
	transformAndWrite(roleRange(KB,A84,B84,C84),
			  (knowledgeBase(KBName,NewKB,_), assert(roleRange(NewKB,A84,B84,C84)))),
        told,
        !.
saveKB(_,_) :-
	told,
	!,
	fail.
			  
transformAndWrite(G1,Var,Test,G2) :-
	clause(G1,_Body),
	call(Test),
	writeeq(G2), cmp_write('.\n'),
	fail.
transformAndWrite(_,_,_,_) :-
	!.

transformAndWrite(G1,G2) :-
	clause(G1,_Body),
	cmp_writeq(G2), cmp_write('.\n'),
	fail.
transformAndWrite(_,_) :-
	!.


/***********************************************************************
 *
 * deduce(KBName,CTX,Query,Proof)
 *
 */

deduce(P1) :-
	completeParameter([P1],KBName,CTX,Query,Proof),
	deduce(KBName,CTX,Query,Proof).
deduce(P1,P2) :-
	completeParameter([P1,P2],KBName,CTX,Query,Proof),
	deduce(KBName,CTX,Query,Proof).
deduce(P1,P2,P3) :-
	completeParameter([P1,P2,P3],KBName,CTX,Query,Proof),
	deduce(KBName,CTX,Query,Proof).

deduce(KBName,CTX,elementOf(X,C),Exp) :-
	option(prover,setheo),
	!,
	deduceSetheo(KBName,CTX,elementOf(X,C),Exp).
/* deduce(KBName,CTX,elementOf(X,C),Exp) :-
	deduceSIGMAHL(KBName,CTX,elementOf(X,C),Exp).
*/
deduce(KBName,CTX,X,Exp) :-
	deduceSIGMAHL(KBName,CTX,X,Exp).
deduceSIGMAHL(KBName,CTX,elementOf(X,C),Exp) :-
	option(queryCaching,on),
	retractall(hypothesis(_)),
 	knowledgeBase(KBName,KB,_),
 	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	clause(query(KB,W1,C,X,Exp,Goal),_).
deduceSIGMAHL(KBName,CTX,elementOf(X,CT),Exp) :-
	retractall(hypothesis(_)),
	removeDefinitionOfConceptname(queryconcept),
	defineEquivalentConceptname(KBName,CTX,CT,query,C),
 	knowledgeBase(KBName,KB,_),
 	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
 	getNegatedConcept(C,C1),
	constructMLMark(rn(_AN5,_RN5,_S5,_O5),W1,C1,X,_HYPS,_D1,InHead1),
 	getQuery(KB,W1,C,X,Exp,Goal),
	performQuery(X,G1,Goal),
	allowedAnswerConcept(KB,C),
% 	anlegen oneer clausel die instance undefconcept wieder geloescht wird...
 	setQuery(KB,W1,CT,X,Exp,Goal).
deduceSIGMAHL(KBName,CTX,roleFiller(X,R1,L,N),Exp) :-
	retractall(hypothesis(_)),
	removeDefinitionOfRolename(queryrole),
%	defineEquivalentRolename(KBName,CTX,R1,R),
	R = R1,
	knowledgeBase(KBName,KB,_),
	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	call(G1),
	defineEquivalentRolename(KBName,CTX,R,query,R1),
	solveConstraint(KB,W1,(card,app((_FF:R1),X),'>=',_N1),(_N,L),
	                hyp([or([]),rl([]),fl([])]),ab(noAb),call([]),Exp),
	nonvar(X),
	length(L,N).
deduceSIGMAHL(KBName,CTX,roleConstraints(X,R1,['>=',N1,'=<',N2,L,N3]),Exp) :-
	retractall(hypothesis(_)),
	removeDefinitionOfRolename(queryrole),
%	defineEquivalentRolename(KBName,CTX,R1,R),
	R = R1,
	knowledgeBase(KBName,KB,_),
	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	call(G1),
	defineEquivalentRolename(KBName,CTX,R,query,R1),
	solveConstraint(KB,W1,(card,app((_FF:R1),X),'>=',N1),(_,L),
	                hyp([or([]),rl([]),fl([])]),ab(noAb),call([]),Exp1),
	solveConstraint(KB,W1,(card,app((_FF:R1),X),'=<',N2),(Exp3,L),
	                hyp([or([]),rl([]),fl([])]),ab(noAb),call([]),Exp2),
	Exp = proved(CTX,and([card(R1,X,>=,N1),
			     card(R1,X,=<,N2),
			     inst(R1,X,L)]),
		     basedOn(and([Exp1,Exp2,Exp3]))),
	nonvar(X),
	length(L,N3).

%----------------------------------------------------------------------
% defineEquivalentRolename(+KBName,+CTX,+RoleTerm,-RoleName)
% Arguments: KBName   knowledgeBase atom_codes
%            CTX        modal context (instance list form)
%            RoleTerm  role term (possibly non-atomic)
%            RoleName  role name
% if RoleTerm is either a variable or a role name, then RoleName is equal to
% RoleTerm. Otherwise, a role RoleName '<=>' to RoleTerm is defined.

defineEquivalentRolename(_,_,R,_,R) :-
	isSlot(R),
	!.
defineEquivalentRolename(_,_,R,_,R) :-
	atomic(R),
	!.
defineEquivalentRolename(KBName,CTX,R,query,queryrole) :-
	defrole(KBName,CTX,queryrole,R),
	!.
defineEquivalentRolename(KBName,CTX,R,_,R1) :-
	gensym(role,R1),
	defrole(KBName,CTX,R1,R),
	!.

%----------------------------------------------------------------------
% defineEquivalentConceptname(+KBName,+CTX,+RoleTerm,-RoleName)
% Arguments: KBName   knowledgeBase atom_codes
%            CTX        modal context (instance list form)
%            ConceptTerm  role term (possibly non-atomic)
%            ConceptName  role name
% if ConceptTerm is either a variable or a concept name, then ConceptName is 
% equal to ConceptTerm. Otherwise, a role ConceptName '<=>' to 
% ConceptTerm is defined.

defineEquivalentConceptname(_,_,C,_,C) :-
	isSlot(C),
	!.
defineEquivalentConceptname(_,_,C,_,C) :-
	atomic(C),
	!.
defineEquivalentConceptname(KBName,CTX,C,query,C1) :-
	C1 = queryconcept,
	defconcept(KBName,CTX,queryconcept,C),
	!.
defineEquivalentConceptname(KBName,CTX,C,_,C1) :-
	gensym(concept,C1),
	defconcept(KBName,CTX,C1,C),
	!.

removeDefinitionOfConceptname(C) :-
	conceptEqualSets(KB,_,CTX,C,_,_),
	knowledgeBase(KBName,KB,_),
	undefconcept(KBName,CTX,C),
	fail.
removeDefinitionOfConceptname(C) :-
	!.


%----------------------------------------------------------------------
% removeDefinitionOfRolename(+KBName,+CTX,+RoleName)
% Arguments: KBName   knowledgeBase atom_codes
%            CTX        modal context (instance list form)
%            RoleTerm  role term (possibly non-atomic)
%            RoleName  role name
% removes the definition of RoleName.

removeDefinitionOfRolename(R) :-
	roleEqualSets(KB,_,CTX,R,_,_),
	knowledgeBase(KBName,KB,_),
	undefrole(KBName,CTX,R),
	fail.
removeDefinitionOfRolename(R) :-
	!.

deduceSetheo(KBName,CTX,elementOf(X,C),Exp) :-
 	knowledgeBase(KBName,KB,_),
 	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	getQuery(KB,CTX,C,X,GL),
	cmp_write('Trying '), cmp_print(X), cmp_write(' instance '), cmp_print(C), cmp_write('.\n'),
	tell('aaa.lop'),
	envToFOL(KB,CL),
	printSystemSyntax(CL),
	printSystemSyntax(GL),
	told,
	shell('/HG/local/provers/setheo/bin.sun4/inwasm -cons -nosgreord aaa',S1),
	S1 = 0,
	shell('/HG/local/provers/setheo/bin.sun4/wasm aaa',S2),
	S2 = 0, 
	shell('/HG/local/provers/setheo/bin.sun4/sam -dr -cons aaa',S3),
	S3 = 0.

deduceDecider(KBName,CTX,elementOf(X,C),Exp) :-
 	knowledgeBase(KBName,KB,_),
 	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
%	getQuery(KB,CTX,C,X,GL),
%	cmp_write('Trying '), cmp_print(X), cmp_write(' instance '), cmp_print(C), cmp_write('.\n'),
%	tell('aaa.lop'),
	setOption(prover,decider),
	setOption(translationMode,relational),	
	envToFOL(KB,CL1),
	modTranslatoneDecide(CL1,CL),
%	printSystemSyntax(CL),
%	printSystemSyntax(GL),
%	told,
	modInferenceClauseList(CL,Exp),	
	!.

setQuery(KB,W1,C,X,Exp,Goal) :-
	not(clause(query(KB,W1,C,X,Exp,_Goal),_)),
	assert(query(KB,W1,C,X,Exp,Goal)),
	!.
setQuery(KB,W1,C,X,Exp,Goal) :-
	!,
	fail.

splitGoal([(instance(A1,B1,C1) <- true)],[(false <- instance(A1,B1,C1))]) :-
	!.
splitGoal([(~ instance(A1,B1,C1) <- true)],[(false <- ~ instance(A1,B1,C1))]) :-
	!.
splitGoal([B|C],[B|D]) :-
	splitGoal(C, D).
splitGoal([],[]).

getQuery(KB,CTX1,C0,X,C3) :-
	isSlot(C0),
	isSlot(X),
	clause(conceptName(KB,_,_,C0),_),
        conceptElement(KB,_,_,user,X,_,_),
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	axiomToFOL(CTX3,[X],_,instance,C0,F),
	translate(F,C1),
	clausesToSystemSyntax(C1,C2),
	splitGoal(C2,C3).
getQuery(KB,CTX1,C0,X,C3) :-
	isSlot(C0),
	isSlot(X),
	clause(conceptName(KB,_,_,C0),_),
        conceptElement(KB,_,_,user,X,_,_),
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	axiomToFOL(CTX3,[X],_,instance,not(C0),F),
	translate(F,C1),
	clausesToSystemSyntax(C1,C2),
	splitGoal(C2,C3).
getQuery(KB,CTX1,C0,X,C3) :-
	isSlot(C0),
	nonvar(X),
	setof(F1,[A1,A2,A3]^clause(conceptName(KB,A1,A2,F1),A3),L1),
	member(C0,L1),
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	axiomToFOL(CTX3,[X],_,instance,C0,F),
	translate(F,C1),
	clausesToSystemSyntax(C1,C2),
	splitGoal(C2,C3).
getQuery(KB,CTX1,C0,X,C3) :-
	isSlot(C0),
	nonvar(X),
	setof(F1,[A1,A2,A3]^clause(conceptName(KB,A1,A2,F1),A3),L1),
	member(C0,L1),
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	axiomToFOL(CTX3,[X],_,instance,not(C0),F),
	translate(F,C1),
	clausesToSystemSyntax(C1,C2),
	splitGoal(C2,C3).
getQuery(KB,CTX1,C0,X,C3) :-
	nonvar(C0),
	isSlot(X),
        conceptElement(KB,_,_,user,X,_,_),
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	axiomToFOL(CTX3,[X],_,instance,C0,F),
	translate(F,C1),
	clausesToSystemSyntax(C1,C2),
	splitGoal(C2,C3).
getQuery(KB,CTX1,C0,X,C3) :-
	nonvar(C0),
	nonvar(X),
	hop_map(negate,CTX1,CTX2),
	hop_map(normalizeNot,CTX2,CTX3),
	axiomToFOL(CTX3,[X],_,instance,C0,F),
	translate(F,C1),
	clausesToSystemSyntax(C1,C2),
	splitGoal(C2,C3).
getQuery(KB,W1,C0,X,Exp,Goal) :-
	isSlot(C0),
	clause(conceptName(KB,_,_,C0),_),
	constructMLCall(KB,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal).

performQuery(X,G1,Goal) :-
	nonvar(X),
	not(option(allProofs,yes)),
	!,
 	once((call((call(G1), Goal)), atomic(X))).
performQuery(X,G1,Goal) :-
	!,
 	call((call(G1), Goal)), 
	atomic(X).

/***********************************************************************
 *
 * deduce(+KBName,+CTX,:+-Info,-E)
 *
 *	If instantiated, Info is one of 
 *		infl(+-X,+-Y,+-W),
 *		posInfl(+-X,+-Y), negInfl(+-X,+-Y), noInfl(+-X,+-Y),
 *		simultInfl(+-Xs,+-Y,+-W), 
 *		simultPosInfl(+-Xs,+-Y), simultNegInfl(+-Xs,+-Y), 
 *		simultNoInfl(+-Xs,+-Y), 
 *		change(+-X,+-W),
 *		increase(+-X), decrease(+-X), noChange(+-X).
 *
 *	Succeeds if Info can be inferred by deduction.
 */

deduce(KBName,CTX,infl(X,Y,W),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	infl(KB,World,X,Y,W).
deduce(KBName,CTX,simultInfl(X,Y,W),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	simultInfl(KB,World,X,Y,W).
deduce(KBName,CTX,leastInfl(X,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	leastInfl(KB,World,X,Y).
deduce(KBName,CTX,leastInfls(Xs,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	leastInfls(KB,World,Xs,Y).
deduce(KBName,CTX,greatestInfl(X,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	greatestInfl(KB,World,X,Y).
deduce(KBName,CTX,greatestInfls(Xs,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	greatestInfls(KB,World,Xs,Y).
deduce(KBName,CTX,maxPosInfl(X,Y,W),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	maxPosInfl(KB,World,X,Y,W).
deduce(KBName,CTX,maxNegInfl(X,Y,W),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	maxNegInfl(KB,World,X,Y,W).
deduce(KBName,CTX,change(Y,W),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	change(KB,World,Y,W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deduce(KBName,CTX,posInfl(X,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	posInfl(KB,World,X,Y).
deduce(KBName,CTX,negInfl(X,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	negInfl(KB,World,X,Y).
deduce(KBName,CTX,simultPosInfl(Xs,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	simultPosInfl(KB,World,Xs,Y).
deduce(KBName,CTX,simultNegInfl(Xs,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	simultNegInfl(KB,World,Xs,Y).
deduce(KBName,CTX,simultNoInfl(Xs,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	simultNoInfl(KB,World,Xs,Y).
deduce(KBName,CTX,noInfl(X,Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	noInfl(KB,World,X,Y).
deduce(KBName,CTX,increase(X),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	increase(KB,World,X).
deduce(KBName,CTX,decrease(X),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	decrease(KB,World,X).
deduce(KBName,CTX,noChange(Y),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	noChange(KB,World,Y).




getQuery(KB,W1,C0,X,Exp,Goal) :-
	isSlot(C0),
	clause(conceptName(KB,_,_,C0),_),
	constructMLCall(KB,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal).
getQuery(KB,W1,C0,X,Exp,Goal) :-
	isSlot(C0),
	clause(conceptName(KB,_,_,C1),_),
	C0 = not(C1),
	constructMLCall(KB,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal).
%getQuery(KB,W1,C0,X,Exp,Goal) :-
% 	isSlot(C0),
%	!,
%	constructMLCall(KB,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
%			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal),
%	!.
getQuery(KB,W1,C0,X,Exp,Goal) :-
	nonvar(C0),
	convertInAntecedent(KB,rn(no,user,_O1),bodyMC(W1),headMC(_),
			    C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal),
	!.

/***********************************************************************
 *
 * abduce(+-Hypothesis,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypothesis Hypothesis.
 */
abduce(Hyps,elementOf(X,Y)) :-
	!,
	getCurrentKnowledgeBase(KBName),
	abduce(KBName,[],Hyps,elementOf(X,Y),_).
abduce(Hypothesis,Consequent) :-
        getCurrentKnowledgeBase(KBName),
	abduce(KBName,[],Hypothesis,Consequent,[]).

/***********************************************************************
 *
 * abduce(+KBName,+-Hypothesis,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypothesis Hypothesis.
 */

abduce(KBName,Hypothesis,elementOf(X,C)) :-
	nonvar(KBName),
	knowledgeBase(KBName,_,_),
	!,
	abduce(KBName,[],elementOf(X,C),_Exp).
abduce(CTX,Hypothesis,elementOf(X,C)) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
        getCurrentKnowledgeBase(KBName),
	!,
	abduce(KBName,CTX,Hypothesis,elementOf(X,C),_Exp).
abduce(Hypothesis,elementOf(X,C),Exp) :-
	getCurrentKnowledgeBase(KBName),
	!,
	abduce(KBName,[],Hypothesis,elementOf(X,C),Exp).
abduce(KBName,Hypothesis,Consequent) :-
        knowledgeBase(KBName,_,_),
	!,
	abduce(KBName,[],Hypothesis,Consequent,[]).
abduce(CTX,Hypothesis,Consequent) :-
	nonvar(CTX),
        (CTX = [] ; CTX = [_|_]),
        getCurrentKnowledgeBase(KBName),
	!,
	abduce(KBName,CTX,Hypothesis,Consequent,[]).

abduce(KBName,Hyps,elementOf(X,Y),Exp) :-
	nonvar(KBName),
	knowledgeBase(KBName,_,_),
	!,
	abduce(KBName,[],Hyps,elementOf(X,Y),Exp).
abduce(CTX,Hyps,elementOf(X,Y),Exp) :-
	nonvar(CTX),
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	!,
	abduce(KBName,CTX,Hyps,elementOf(X,Y),Exp).
abduce(KBName,CTX,Hyps,elementOf(X,Y)) :-
	!,
	abduce(KBName,CTX,Hyps,elementOf(X,Y),_Exp).

abduce(KBName,CTX,Hyps,elementOf(X,C),Exp) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	constructMLCall(KB,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C,X,[or([]),rl([]),fl(_DML1)],D,[],_Exp1,InHead),
	call((call(G1), InHead)),
	ifOption(traceOutput,yes,(cmp_print('Hypotheses '), cmp_print(D), cmp_nl)),
	getConstraint(InHead,X),
	atomic(X),
	allowedAnswerConcept(KB,C),
	getAbductionHyps(D,GL),
	once((doConsistencyCheck([],GL),doMinimalityCheck([],GL))),
	getExplanation(InHead,Exp),
	getAbductionHypotheses(D,Hyps).

getAbductionHypotheses(L,[]) :-
	isSlot(L),
	!.
getAbductionHypotheses([],[]) :-
	!.
getAbductionHypotheses([instance(KB,RN,modal(CTX),C,X,_,_,_,_)|L1],
	                [assert_ind(CTX1,X,C)|L2]) :- 
	!,
	translateModalContext(CTX,CTX1),
	getAbductionHypotheses(L1,L2).
getAbductionHypotheses([C1|L1],
	                [C1|L2]) :- 
	!,
	getAbductionHypotheses(L1,L2).


translateModalContext([],[]) :-
	!.
translateModalContext(app([WP,_]:m(M,A),W),CTX) :-
	isSlot(WP),
	!,
	translateAgent(A,A1),
	translateModalContext(W,CTX2),
	append(CTX2,[d(M,A1)],CTX).
translateModalContext(app([WP,_]:m(M,A),W),CTX) :-
	!,
	translateAgent(A,A1),
	translateModalContext(W,CTX2),
	append(CTX2,[b(M,A1)],CTX).

translateAgent(A,all) :-
	isSlot(A),
	!.
translateAgent(A,A) :-
	!.

/***********************************************************************
 *
 * abduce(+KBName,+CTX,+-change(+-X,+-Wx),+-change(+-Y,+-Wy),[]).
 *
 *	Succeeds if, under the hypothesis of change(+-X,+-Wx), 
 *	change(+-Y,+-Wy) follows.
 */

abduce(KBName,CTX,change(X,Wx),change(Y,Wy),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	wellDefined_ChangeWeight(Wx),
	infl(KB,World,X,Y,Wxy),
	not(given_change(KB,World,X,_)),
	bagof(W,Z^changingInfl(KB,World,Z,Y,W),Ws),
	weightOf_change(Wx,Wxy,Wy1),
	weightOf_SimultChange([Wy1|Ws],Wy).

abduce(KBName,CTX,change(X,Wx),change(Y,Wy),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	isSlot(Wx),
	wellDefined_ChangeWeight(Wy),
	infl(KB,World,X,Y,Wxy),
	not(given_change(KB,World,X,_)),
	bagof(W,Z^changingInfl(KB,World,Z,Y,W),Ws),
	weightOf_SimultChange([Wy1|Ws],Wy),
	weightOf_change(Wx,Wxy,Wy1).

abduce(KBName,CTX,change(X,Wx),change(Y,Wy),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	infl(KB,World,X,Y,Wxy),
	not(given_change(KB,World,_,_)),
	weightOf_change(Wx,Wxy,Wy).

abduce(KBName,CTX,change(X,Wx),change(X,Wx),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	wellDefined_hl_attribute(KB,World,X),
	not(given_change(KB,World,X,_)),
	wellDefined_ChangeWeight(Wx).

/***********************************************************************
 *
 * abduce(+KBName,+CTX,+-Hypothesis,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypothesis Hypothesis.
 *
 *	Hypothesis and Consequent are of the form:
 *		increase(+-X), decrease(+-X), noChange(+-X). 
 */

abduce(KBName,CTX,Hypothesis,Consequent,[]) :-
	isSlot(Hypothesis),
	nonvar(Consequent),
	once(default_change(Consequent,WeightedConsequent)),
	abduce(KBName,CTX,change(X,Wx),WeightedConsequent,[]),
	once(default_change(Hypothesis,change(X,Wx),[])).

abduce(KBName,CTX,Hypothesis,Consequent,[]) :-
	nonvar(Hypothesis),
%	isSlot(Consequent),
	once(default_change(Hypothesis,WeightedHypothesis)),
	abduce(KBName,CTX,WeightedHypothesis,change(Y,Wy),[]),
	once(default_change(Consequent,change(Y,Wy))).

/***********************************************************************
 *
 * abduce(+KBName,+CTX,+Changes,+-change(+-Y,+-W)).
 *
 *	Succeeds if change(+-Y,+-W) follows under the hypotheses of 
 *	Changes.
 *	Changes is a list of changes of the form change(+-X,+W).
 */

abduce(KBName,CTX,Hs,change(Y,W),[]) :-
	get_KB_World(KBName,CTX,KB,World),
	nonvar(Hs),
	aux_abduce(KB,World,Hs,change(Y,W),Ws),
	wellDefined_SimultChanges(Hs),
	weightOf_SimultChange(Ws,W).

/***********************************************************************
 *
 * abduce(+KBName,+CTX,+-Hypotheses,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypotheses Hypotheses.
 *
 *	Hypotheses  is a list of 
 *		increase(+-X), decrease(+-X), noChange(+-X). 
 *	predicates and Consequent is one of these.
 */

abduce(KBName,CTX,Hypotheses,Consequent,[]) :-
	nonvar(Hypotheses),
	nonvar(Consequent),
	once(default_changes(Hypotheses,WeightedHypotheses)),
	once(default_change(Consequent,WeightedConsequent)),
	abduce(KBName,CTX,WeightedHypotheses,WeightedConsequent,[]).

abduce(KBName,CTX,Hypotheses,Consequent,[]) :-
	nonvar(Hypotheses),
%	isSlot(Consequent),
	once(default_changes(Hypotheses,WeightedHypotheses)),
	abduce(KBName,CTX,WeightedHypotheses,change(Y,Wy),[]),
	once(default_change(Consequent,change(Y,Wy))).

/***********************************************************************
 *
 * abduce(+KBName,+CTX,+-Change,+-Changes).
 *
 *	Succeeds if Changes are hold under the hypothesis that Change
 *	holds.
 *
 *	Changes (respectively Change) is a list of changes 
 *	(respectively a change) of the form change(+-X,+-W).
 */

abduce(KBName,CTX,Change,Changes,[]) :-
	nonvar(Changes),
	aux2_abduce(KBName,CTX,Change,Changes).

/***********************************************************************
 *
 * abduce(KBName,CTX,+-Hypothesis,+-Consequents).
 *
 *	Succeeds if Consequents follow under the hypothesis Hypothesis.
 *
 *	Hypothesis is of the form:
 *		increase(+-X), decrease(+-X), noChange(+-X). 
 *	Consequents is a list of these.
 */

abduce(KBName,CTX,Hypothesis,Consequents,[]) :-
	nonvar(Hypothesis),
	nonvar(Consequents),
	once(default_change(Hypothesis,WeightedHypothesis)),
	once(default_changes(Consequents,WeightedConsequents)),
	abduce(KBName,CTX,WeightedHypothesis,WeightedConsequents,[]).

abduce(KBName,CTX,Hypothesis,Consequents,[]) :-
	isSlot(Hypothesis),
	nonvar(Consequents),
	once(default_changes(Consequents,WeightedConsequents)),
	abduce(KBName,CTX,change(X,Wx),WeightedConsequents,[]),
	once(default_change(Hypothesis,change(X,Wx))).





/***********************************************************************
 *
 * allowedAnswerConcept(+C)
 * true iff C is a concept introduced by the user. No concept names
 * introduces by the system or concept terms are allowed as answer.
 *
 */

allowedAnswerConcept(KB,C) :-
	atomic(C),
	!,
	clause(conceptName(KB,_,_,C),_),
	!.
% allowedAnswerConcept(KB,C) :-
%	atomic(C),
%	conceptEqualSets(KB,user,_,C,_,_),
%	!.
% allowedAnswerConcept(KB,C) :-
%	atomic(C),
%	conceptEqualSets(KB,user,_,_,C,_),
%	!.
%allowedAnswerConcept(KB,C) :-
%	atomic(C),
%	conceptSubsets(KB,user,_,C,_,_),
%	!.
%allowedAnswerConcept(KB,C) :-
%	atomic(C),
%	conceptSubsets(KB,user,_,_,C,_).
allowedAnswerConcept(KB,C) :-
	nonvar(C),
	C = not(D),
	!,
	nonvar(D),
	not(D = not(E)),
	!,
	allowedAnswerConcept(KB,D).
allowedAnswerConcept(_,normal(_)) :-
	!,
	fail.
allowedAnswerConcept(_,not(normat(_))) :-
	!,
	fail.
allowedAnswerConcept(KB,C) :-
	not(atomic(C)),
	!.

/***********************************************************************
 *
 * inconsistent(+KBName)
 *
 */


inconsistent :-
	getCurrentKnowledgeBase(KBName),
	inconsistent(KBName,[]).

inconsistent(KBName) :-
	nonvar(KBName),
	knowledgeBase(KBName,_,_),
	inconsistent(KBName,[]).
inconsistent(CTX) :-
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	inconsistent(KBName,CTX).

inconsistent(KBName,CTX) :-
	knowledgeBase(KBName,KB,_),
	convertCTX(negative,KB,[[],true],CTX,[],[W1,G1],_),
	constructMLHead(KB,rn(_AX1,_RN1,user,_O1),W1,C,X,[or([]),rl([]),fl(_DML1)],noAb,[],_,InHead1),
	call((call(G1), InHead1)),
	getConstraint(InHead1,X),
	atomic(X),
	constructMLHead(KB,rn(_AX2,_RN2,_S2,_O2),W1,not(C),X,[or([]),rl([]),fl(_DML1)],noAb,[],_,InHead2),
	call((call(G1), InHead2)).

/***********************************************************************
 *
 * consistent(+KBName)
 *
 */

consistent :-
	not(inconsistent).

consistent(KBName) :-
	nonvar(KBName),
	knowledgeBase(KBName,_,_),
	not(inconsistent(KBName,[])).
consistent(CTX) :-
	(CTX = [] ; CTX = [_|_]),
	getCurrentKnowledgeBase(KBName),
	not(inconsistent(KBName,CTX)).

consistent(KBName,CTX) :-
	not(inconsistent(KBName,CTX)).

/***********************************************************************
 *
 *
 */

metaReasoning :-
	constructMLHead(KB,rn(ti,ti,system,lInR),W1,C,X,
			_HYPS,_D,_CALLS,inconsistency,InHead1),
	constructMLHead(KB,rn(ti,ti,system,lInR),W1,not(C),X,
			_HYPS,_D,_CALLS,inconsistency,InHead2),
	Lit11 = not(inconsistencyCheck(_,_,_)),
	Lit13 = asserta(InHead2),
	Lit14 = asserta(inconsistencyCheck(CTX,C,X)),
	Lit15 = tryInconsistency(CTX,C,X,InHead2),
	assertz((InHead1 :- atomic(C), atomic(X), Lit11, Lit13, Lit14, Lit15)),
	Lit23 = asserta(InHead1),
	Lit24 = asserta(inconsistencyCheck(CTX,C,X)),
	Lit25 = tryInconsistency(CTX,C,X,InHead1),
	assertz((InHead2 :- atomic(C), atomic(X), Lit11, Lit23, Lit24, Lit25)).


tryInconsistency(CTX,C,X,InHead) :-
	inconsistent(CTX),
	!,
	retract(inconsistencyCheck(CTX,C,X)),
	retract(InHead).
tryInconsistency(CTX,C,X,InHead) :-
	!,
	retract(inconsistencyCheck(CTX,C,X)),
	retract(InHead),
	!,
	fail.

/**********************************************************************
 *
 *
 *
 */

% realize(KBName,CTX,X,CL) :-
% 	knowledgeBase(KBName,KB,_),
%	conceptHierarchy(KB,CTX,Tree1),
% 	conceptElement(KB,CTX,_,user,X,C1,_),
%	search(C1,Tree1,TreeList),
%	realizeArgs(KBName,CTX,X,TreeList,CL).
realize(KBName,CTX,X,CL) :-
	setofOrNil(C,deduce(KBName,CTX,elementOf(X,C)),CL).

realizeDag(KBName,CTX,X,hl_node(CL,AL),CL1) :-
	realizeArgs(KBName,CTX,X,AL,CL0),
	realizeNode(KBName,CTX,X,CL,CL0,CL1).

realizeArgs(_,_,_,[],[]) :-
	!.
realizeArgs(KBName,CTX,X,[C|AL],CL3) :-
	realizeDag(KBName,CTX,X,C,CL1),
	realizeArgs(KBName,CTX,X,AL,CL2),
	append(CL1,CL2,CL3).

realizeNode(KBName,CTX,X,_CL,[C0|CL0],[C0|CL0]) :-
	!.
realizeNode(KBName,CTX,X,[C|CL],[],CL1) :-
	deduce(KBName,CTX,elementOf(X,C),_),
	!,
	CL1 = [C|CL].
realizeNode(_,_,_,_,_,[]) :-
	!.

sbAsk(KBName,CTX,X,CL) :-
	knowledgeBase(KBName,KB,_),
	conceptHierarchy(KB,CTX,Tree1),
	askDag(KBName,CTX,X,Tree1,CL).

askDag(KBName,CTX,X,hl_node(CL,AL),CL1) :-
	askArgs(KBName,CTX,X,AL,CL0),
	askNode(KBName,CTX,X,CL,CL0,CL1).

askArgs(_,_,_,[],[]) :-
	!.
askArgs(KBName,CTX,X,[C|AL],CL3) :-
	askDag(KBName,CTX,X,C,CL1),
	askArgs(KBName,CTX,X,AL,CL2),
	append(CL1,CL2,CL3).

askNode(_KBName,_CTX,_esX,CL,[C0|CL0],CL1) :-
	!,
	append([C0|CL0],CL,CL1).
askNode(KBName,CTX,X,[C|CL],[],CL1) :-
	deduce(KBName,CTX,elementOf(X,C),_),
	!,
	CL1 = [C|CL].
askNode(_,_,_,_,_,[]) :-
	!.

/**********************************************************************
 *
 * completeParameter(+ParameterList,-KBName,-CTX,-Query,-Proof)
 * takes a list of parameters ParameterList and instantiates the
 * variables KBName,CTX,Query,Proof correctly.
 *
 */

completeParameter([Query],KBName,[],Query,_Proof) :-
	getCurrentKnowledgeBase(KBName),
	!.
completeParameter([P1,P2],P1,_CTX,P2,_Proof) :-
	isSlot(P1),
	!.
completeParameter([P1,P2],P1,_CTX,P2,_Proof) :-
	nonvar(P1),
	atomic(P1),
	P1 \== [],
	!.
completeParameter([P1,P2],KBName,P1,P2,_Proof) :-
	nonvar(P1),
	(P1 = [] ; P1 = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName).
completeParameter([P1,P2],KBName,[],P1,P2) :-
	nonvar(P1),
	!,
	getCurrentKnowledgeBase(KBName).
completeParameter([P1,P2,P3],P1,P2,P3,_Proof) :-
	isSlot(P1),
	isSlot(P2),
	!.
completeParameter([P1,P2,P3],P1,P2,P3,_Proof) :-
	nonvar(P1),
	atomic(P1),
	P1 \== [],
	isSlot(P2),
	!.
completeParameter([P1,P2,P3],KBName,P1,P2,P3) :-
	nonvar(P1),
	(P1 = [] ; P1 = [_|_]),
	!,
	getCurrentKnowledgeBase(KBName).
completeParameter([P1,P2,P3],P1,P2,P3,_Proof) :-
	nonvar(P2),
	(P2 = [] ; P2 = [_|_]),
	!.
completeParameter([P1,P2,P3],P1,[],P2,P3) :-
	nonvar(P1),
	atomic(P1),
	P1 \== [],
	!.
completeParameter([P1,P2,P3],P1,[],P2,P3) :-
	!.
completeParameter([P1,P2,P3,P4],P1,P2,P3,P4) :-
	!.


/***********************************************************************
 *
 * getCurrentConcepts(-LC)
 *  list of all concepts instance the current knowledge base
 * getCurrentRoles(-LR)
 *  list of all roles instance the current knowledge base
 * getCurrentAxioms(-LAx)
 *  list of all axioms instance the current knowledge base
 * 
 */


getCurrentConcepts(LC) :-
    getCurrentKnowledgeBase(KBName), 
    knowledgeBase(KBName, KB,_), 
    setofOrNil(C,[CTX,W,B]^clause(conceptName(KB,CTX,W,C),B),LC).
getCurrentRoles(LR) :-
    getCurrentKnowledgeBase(KBName), 
    knowledgeBase(KBName, KB,_), 
    setofOrNil(R,[CTX,W,B]^clause(roleName(KB,CTX,W,R),B),LR).
getCurrentAxioms(LAx) :-
    getCurrentKnowledgeBase(KBName), 
    knowledgeBase(KBName, KB,_), 
    setofOrNil(defprimconcept(CTX1,C1,D1),[Ax1,B1]^clause(conceptSubsets(KB,user,CTX1,C1,D1,Ax1),B1),LAx1),
    setofOrNil(defconcept(CTX2,C2,D2),[Ax2,B2]^clause(conceptEqualSets(KB,user,CTX2,C2,D2,Ax2),B2),LAx2),
    setofOrNil(defprimrole(CTX3,R3,S3),[Ax3,B3]^clause(roleSubsets(KB,user,CTX3,R3,S3,Ax3),B3),LAx3),
    setofOrNil(defrole(CTX4,R4,S4),[Ax4,B4]^clause(roleEqualSets(KB,user,CTX4,R4,S4,Ax4),B4),LAx4),
    setofOrNil(assert_ind(CTX5,X5,C5),[Ax5,B5,W5]^clause(conceptElement(KB,CTX5,W5,user,X5,C5,Ax5),B5),LAx5),
    setofOrNil(assert_ind(CTX6,X6,Y6,R6),[Ax6,B6,W6]^clause(roleElement(KB,CTX6,W6,user,X6,Y6,R6,Ax6),B6),LAx6),
    append(LAx1,LAx2,LAx12),
    append(LAx3,LAx4,LAx34),
    append(LAx5,LAx6,LAx56),
    append(LAx12,LAx34,LAx14),
    append(LAx14,LAx56,LAx).


/**********************************************************************
 *
 * @(#) initCall.pll 1.6@(#)
 *
 */

:- cmp_write('% Loading SigmaHL (Version 1.10.1 Feb 2002)\n').
:-setOption(traceOutput,yes).
:- initialize.

