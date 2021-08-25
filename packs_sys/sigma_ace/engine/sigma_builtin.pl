%%% This list can be added to as required.

builtin(T) :-
	nonvar(T),
        functor(T,F,N),
        builtin(F,N).

builtin(!,0).
builtin(true,0).
builtin(fail,0).
builtin(succeed,0).
builtin(trace,0).
builtin(atom,1).
builtin(integer,1).
builtin(number,1).
builtin(atomic,1).
builtin(constant,1).
builtin(functor,3).
builtin(arg,3).
builtin(var,1).
builtin(nonvar,1).
builtin(call,1).
builtin(=,2).
builtin(\=,2).
builtin(==,2).
builtin(\==,2).
builtin(>,2).
builtin(<,2).
builtin(>=,2).
builtin(=<,2).
builtin(is,2).
builtin(display,1).
builtin(write,1).
builtin(nl,0).
builtin(infer_by,_).
builtin(write_proved,_).
builtin(search,_).
builtin(search_cost,_).
builtin(unify,_).
builtin(identical_member,_).
builtin(unifiable_member,_).
builtin(inc_ncalls,0).
% --- compatibility predicates
builtin(justification,_).
builtin(compatible,_).
builtin(model_initialization,_).
% --- variable handling predicates
builtin(herbrand,1).
% hooks handling predicates
builtin(hook1,1).
builtin(hook2,1).
% --- lemma handling predicates
builtin(lemmatize,_).
builtin(dynamic_lemma,_).
builtin(static_lemma,_).
% --- misc
builtin(\+,1).

% --- special purpose predicates
builtin(P,_) :-
	builtin_predicate(P).
builtin_predicate(P) :-
	name(P,L),
        (name('_pl',L1);
	 name('_db',L1)
        ),
        append(_,L1,L).
	/*
	
predicate_property( 
	transparent)
	
*/

% ==================================================
% Intrinsic Keywords (used by compile_conditionals)
% ==================================================

% Gets compiled as Formula connectives 
:-dynamic(hlPredicateAttribute/2).


hlPredicateAttribute(X,'SententialOperator'):-
	hlPredicateAttribute(X,connective).
hlPredicateAttribute(X,connective):-
	hlPredicateAttribute(X,quantifier).

% FOL Quantifiers
hlPredicateAttribute(forall,quantifier).
hlPredicateAttribute(forall,not(exists)).
hlPredicateAttribute(exists,quantifier).
hlPredicateAttribute(exists,not(forall)).

% FOL Quantifiers
hlPredicateAttribute(possible,modop).
hlPredicateAttribute(possible,not(necessary)).
hlPredicateAttribute(necessary,modop).
hlPredicateAttribute(necessary,not(possible)).


hlPredicateAttribute(some,quantifier).
hlPredicateAttribute(some,notimplemented).
hlPredicateAttribute(some,quantifier).
hlPredicateAttribute(some,notimplemented).

% Junctions
hlPredicateAttribute(and,connective).
hlPredicateAttribute(and,not(or)).

hlPredicateAttribute(or,connective).
hlPredicateAttribute(or,not(and)).

% Implication/Equivalence/Entailment
hlPredicateAttribute((=>),connective).
hlPredicateAttribute((<=>),connective).
hlPredicateAttribute((entails),connective).

%Negation
hlPredicateAttribute(not,connective). % Explicit Negation
hlPredicateAttribute(naf,connective). % Negation by failure (prolog)

hlPredicateAttribute(instance,no_holds).

hlPredicateAttribute(instance,dag(subclass)).
hlPredicateAttribute(element,dag(subset)).

hlPredicateAttribute(subclass,no_holds).
hlPredicateAttribute(subrelation,no_holds).

hlPredicateAttribute(X,no_holds):-
	hlPredicateAttribute(X,hierarchical).

hlPredicateAttribute(holds,prefix(holds)).

hlPredicateAttribute(X,hierarchical):-
	hlPredicateAttribute(_,dag(X)).


hlPredicateAttribute(X,hierarchical):-atom(X),atom_concat('sub',_,X).
hlPredicateAttribute(X,'SkolemFunction'):-atom(X),atom_concat(_,'SkFn',X).
hlPredicateAttribute(X,'Function'):-atom(X),atom_concat(_,'Fn',X).

					 

ontology_hlPredicateAttribute(logicalConnective,holdsDuring).
ontology_hlPredicateAttribute(logicalConnective,skolem).
ontology_hlPredicateAttribute(logicalConnective,believes).
ontology_hlPredicateAttribute(logicalConnective,knows).
ontology_hlPredicateAttribute(logicalConnective,desires).
ontology_hlPredicateAttribute(logicalConnective,wants).
ontology_hlPredicateAttribute(logicalConnective,'KappaFn').

ontology_structure_functor('instance').


