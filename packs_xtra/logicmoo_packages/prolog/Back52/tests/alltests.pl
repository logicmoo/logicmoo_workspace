:- dynamic test/2.
:- op(0,xfy,(.)).

%% jan
:- ['jantest.tbox'].      
:- ['jantest.abox'].
:- ['jantest.iabox'].
:- ['jantest.tbox-abox-rev'].
:- ['jantest.abox-revision'].      
:- ['jantest.ibox-abox-rev'].

%% carsten
:- ['cmk.abox'].

%% jochen
:- ['ibox_tests'].

%% martin
:- ['role_tests'].
:- ['value_tests'].
:- ['revision_tests'].
:- ['macro_tests'].

%% oli
:- ['b5tests'].
:- ['retrievaltest'].
:- ['displaytest'].

%% uwe
:- ['featuretest.pl'].
alltests :-
	retractall(test(_,_)),
	backinit,
   maplist(ignore,[!, jantest, nl,
   !, aboxtest, nl,
   !, aiboxtest, nl,
   !, roletest, nl,
   !, valuetest, nl, 
   !, iboxtest, nl, 
   !, arevisionstest, nl, 
   !, revisionstest2, nl,
   !, iarevisionstest, nl, 
   !, cmktest, nl, 
   !, macrotest, nl,
   !, b5tests,
   !, feature_test, nl]),
        nl, nl, 
	write('*** Test session ended.'),
	write(' The following tests failed (but should not):'),
	nl,
	write_failed_tests.


write_failed_tests :-
	test(Test, failed),
	write('--- '), write(Test), nl,
	fail.
write_failed_tests.

:- op(625,xfy,(.)).

