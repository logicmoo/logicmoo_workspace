:- module(lambdaki, [ (\)/2, (\)/3, (\)/4, (^)/3, (^)/4, (^)/5 ]).
% Very small lambda library (with no free variables and no checking anything)

:- meta_predicate \(1,?), \(2,?,?), \(3,?,?,?).
:- meta_predicate ^(?,0,?), ^(?,1,?,?), ^(?,2,?,?,?).

:- set_prolog_flag(generate_debug_info, false).

\(M:Hats,A1) :- copy_term(Hats,Copy), call(M:Copy,A1).
\(M:Hats,A1,A2) :- copy_term(Hats,Copy), call(M:Copy,A1,A2).
\(M:Hats,A1,A2,A3) :- copy_term(Hats,Copy), call(M:Copy,A1,A2,A3).

^(A1,P,A1) :- call(P).
^(A1,P,A1,A2) :- call(P,A2).
^(A1,P,A1,A2,A3) :- call(P,A2,A3).
