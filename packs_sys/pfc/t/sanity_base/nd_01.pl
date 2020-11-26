
%  was_module(nd_01,[]).

:- include(test_header).

% :- use_listing_vars.

:- dynamic((p/0,px/0,py/0,pz/0,p1/0,p1x/0,p1y/0,p1z/0,p2/0,p2x/0,p2y/0,p2z/0)).

functor_foo(P,F,A):- ground(P),functor(P,F,A).


:- begin_pfc.


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


