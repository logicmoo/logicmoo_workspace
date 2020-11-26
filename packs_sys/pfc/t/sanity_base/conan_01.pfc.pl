#!/usr/bin/env swipl

%  was_module(sanity_ks_two,[]).

%# Test not present yet
:- include(test_header).

/*

((prologHybrid(F),arity(F,A)/is_ftNameArity(F,A))<==>mpred_prop(F,A,prologHybrid)/is_ftNameArity(F,A)).

prologMultiValued(mudDescription(ftTerm,ftText), [predProxyAssert(add_description),predProxyRetract(remove_description),predProxyQuery(query_description)],prologHybrid).

prologHybrid(isEach(mudLastCommand/2,mudNamed/2, mudSpd/2,mudStr/2,typeGrid/3)).

((prologHybrid(F),arity(F,A)/is_ftNameArity(F,A))<==>mpred_prop(F,A,prologHybrid)/is_ftNameArity(F,A)).


:- ain(ttRelationType(rtFOO)).
% :- must((fully_expand( rtFOO(foo/2),O), O = (arity(foo, 2), rtFOO(foo), tPred(foo)))).

:- must((fully_expand( rtFOO(foo/2),O), sub_term(Sub,O),Sub==rtFOO(foo))).

*/




