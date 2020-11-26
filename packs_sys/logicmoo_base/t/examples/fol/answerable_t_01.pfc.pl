#!/usr/bin/env swipl

:- module(t123,[]).

:- include(test_header).


:- test_boxlog(( ~fallacy_t(PROP) => unknown_t(PROP) v false_t(PROP) v true_t(PROP) )).
:- test_boxlog(( ~unknown_t(PROP) => true_t(PROP) v false_t(PROP)  )).
:- test_boxlog(( ~false_t(PROP) => fallacy_t(PROP) v unknown_t(PROP) v true_t(PROP) )).
:- test_boxlog(( answerable_t(PROP) <=> askable_t(PROP) & ~unknown_t(PROP) )).
:- test_boxlog(( answerable_t(PROP) => true_t(PROP) v false_t(PROP)  )).
:- test_boxlog(( askable_t(PROP) <=> ~fallacy_t(PROP) )).
:- test_boxlog(( askable_t(PROP) => true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )).
:- test_boxlog(( askable_t(PROP) v fallacy_t(PROP) )).
:- test_boxlog(( asserted_t(PROP) => true_t(PROP) )).
:- test_boxlog(( fallacy_t(PROP) => false_t(PROP) & true_t(PROP) & ~unknown_t(PROP) & ~possible_t(PROP) )).   
:- test_boxlog(( true_t(PROP) & false_t(PROP) => fallacy_t(PROP) )).
:- test_boxlog(( true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )).

:- test_boxlog(( true_t(PROP) => possible_t(PROP) )).
:- test_boxlog(( possible_t(PROP) => ~false_t(PROP) & ~fallacy_t(PROP)  )).

:- test_boxlog(( ~true_t(PROP) => false_t(PROP) v fallacy_t(PROP) v possible_t(PROP) )).
:- test_boxlog(( false_t(PROP) <=> ~true_t(PROP) & ~possible_t(PROP) & ~unknown_t(PROP) )).
:- test_boxlog(( true_t(PROP) => ~false_t(PROP) & possible_t(PROP) & ~unknown_t(PROP) )).
:- test_boxlog(( ~asserted_t(PROP) => possible_t(PROP) v false_t(PROP) v fallacy_t(PROP) )).
:- test_boxlog(( ~possible_t(PROP) => false_t(PROP) v fallacy_t(PROP) )).
:- test_boxlog(( possible_t(PROP) => ~false_t(PROP) & ~fallacy_t(PROP)  )).            
:- test_boxlog(( unknown_t(PROP) => ~true_t(PROP) & possible_t(PROP) & ~asserted_t(PROP) & ~false_t(PROP) )).
%:- test_boxlog(( ist(MT1,askable_t(PROP))  & genlMt(MT1,MT2) => ist(MT2, (true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )))).
% :- test_boxlog(( ist(MT1,asserted_t(PROP)) & genlMt(MT1,MT2) => ist(MT2,true_t(PROP)) )).



