#!/usr/bin/env lmoo-clif

% :- module(t123,[]).

:- include(test_header).
:- cls.
%:- module(t123).   
%:- '$set_source_module'(t123).

:- set_prolog_flag(logicmoo_modality,full).

:- use_module(library(script_files)).

% :- process_this_script.

:- statistics.



subtest([subtest_assert(tHuman(joe)),
        mpred_test(isa(_,tHeart))]).

subtest([subtest_assert(tHuman(joe)),
        mpred_test(hasOrgan(joe,_))]).

subtest([subtest_assert(tHeart(_)),
        mpred_test(~hasOrgan(jack,_))]).



:- ain(tHuman(iBob)).

testable_quants(X,Fml,Q,Name):-member(QF,[exactly,atmost,atleast]),member(QN,[0,1,2,3,5,10]),Q=..[QF,QN,X,Fml],atom_concat(QF,QN,Name).
testable_quants(X,Fml,Q,Name):-member(QF,[all,exists]),Q=..[QF,X,Fml],atom_concat(QF,'1',Name).
testable_quants(X,Fml,~Q,Name):-member(QF,[all,exists]),Q=..[QF,X,Fml],atom_concat(QF,'0',Name).


:- add_test(exactly3, (all([[Human,tHuman]],exactly(3,[[E3ye,tEye]],hasEye(Human,E3ye))))).
:- add_test(exists1, (all([[Human,tHuman]],exists([[SomeClue,tClue]],hasClue(Human,SomeClue))))).
:- add_test(least1, (all([[Human,tHuman]],atleast(1,[[LeastIdea,tIdea]],hasIdea(Human,LeastIdea))))).
:- add_test(least3, (all([[Human,tHuman]],atleast(3,[[L3Dream,tDream]],hasDream(Human,L3Dream))))).
:- add_test(most1, (all([[Human,tHuman]],atmost(1,[[M1Spouse,tSpouse]],hasSpouse(Human,M1Spouse))))).
:- add_test(exactly1, (all([[Human,tHuman]],exactly(1,[[Heart,tHeart]],hasHeart(Human,Heart))))).
:- add_test(most3, (all([[Human,tHuman]],atmost(3,[[M3Meal,tMeal]],hasMeal(Human,M3Meal))))).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/420 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/sanity_at_least_most_exactly_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/SANITY_AT_LEAST_MOST_EXACTLY_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ASANITY_AT_LEAST_MOST_EXACTLY_01 

