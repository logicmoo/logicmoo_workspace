
%  was_module(nd_01,[]).
% :- if(rtrace). :-endif.
:- include(library(logicmoo_test_header)).

% :- use_listing_vars.

:- dynamic((p/0,px/0,py/0,pz/0,p1/0,p1x/0,p1y/0,p1z/0,p2/0,p2x/0,p2y/0,p2z/0)).

functor_foo(P,F,A):- ground(P),functor(P,F,A).


:- expects_dialect(pfc).


((p, {member(X,[px,py,pz])}) ==> {writeq(X)}).
p.


((p1) ==> {member(X,[p1x,p1y,p1z])},{writeq(X)}).
p1.

% :-asserta((mpred_core:functor_foo(P,F,A):- trace,ground(P),functor(P,F,A))).

:- set_fc_mode(depth).

((foobar, P0/(copy_term(P0,P),nonvar(P),writeln(start(functor(P,F,A))),functor_foo(P,F,A))) ==> {writeln(done(functor(P,F,A)))}).

/*
% :- rtrace,trace.
((((P0/(copy_term(P0,P),nonvar(P),is_ftNonvar(P),functor(P,F,A), \+ mpred_connective(F), A>1) ==> 
   {lmconf:agenda_slow_op_enqueue(must(ignore(deduceEachArgType(P))))})))).
*/


% :- pp_DB.



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/nd_01.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/ND_01/logicmoo_pfc_test_sanity_base_ND_01_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AND_01 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/532
