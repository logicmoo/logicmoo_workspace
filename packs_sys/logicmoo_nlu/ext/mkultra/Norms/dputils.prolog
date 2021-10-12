% From the book
% PROLOG PROGRAMMING IN DEPTH
% by Michael A. Covington, Donald Nute, and Andre Vellino
% (Prentice Hall, 1997).
% Copyright 1997 Prentice-Hall, Inc.
% For educational use only
%
% Last modified 8/24/2006


% DPUTILS.PL
%
% Utilities for d-Prolog.
%

:- op(1100,fx,@@).

:- public dlisting/1.
:- public (@@)/1.

%
% @@(+Goal)
%   Tests to see if Goal is strictly or defeasibly
%   derivable from the d-Prolog knowledge base. Goal
%   may not contain an uninstantiated variable.
%

@@ Goal :-
   \+ ground(Goal),
        write('Improper argument for @@.'), nl,
        write('Argument contains an uninstantiated variable.'), 
        nl,
        !,
        fail.

@@ Goal :-
        strict_der(root,Goal),
        contrary(Goal,Contrary),
        Contrary,
        !,
        write('definitely, yes -'), nl,
        write('and definitely, no - contradictory.'), nl.

@@ Goal :-
        strict_der(root,Goal),
        !,
        write('definitely, yes.'), nl.

@@ Goal :-
        contrary(Goal,Contrary),
        Contrary,
        !,
        write('definitely, no.'), nl.

@@ Goal :-
        def_der(root,Goal),
        !,
        write('presumably, yes.'), nl.

@@ Goal :-
        contrary(Goal,Contrary),
        def_der(root,Contrary),
        !,
        write('presumably, no.'), nl.

@@ _ :-
        write('can draw no conclusion.'), nl.


%
% dlisting(+Predicate)
%   Lists all ordinary Prolog clauses, defeasible rules, and
%   defeaters in memory which have heads of the form
%   Predicate(...Arguments...) or of the form 
%   ~ Predicate(...Arguments...).
%

dlisting(Predicate) :-
        listing(Predicate),
        fail.

dlisting(Predicate) :-
        clause(~ Head,Body),
        functor(Head,Predicate,_),
        pprint(~ Head,' :-',Body),
        fail.

dlisting(Predicate) :-
        (Head := Body),
        dfunctor(Head,Predicate,_),
        pprint(Head,' :=',Body),
        fail.

dlisting(Predicate) :-
        (Head :^ Body),
        dfunctor(Head,Predicate,_),
        pprint(Head,' :^',Body),
        fail.

dlisting(Predicate) :-
        clause(incompatible(Clause1,Clause2),Body),
        dfunctor(Clause1,Predicate,_),
        pprint(incompatible(Clause1,Clause2),' :-',Body),
        fail.

dlisting(Predicate) :-
        clause(incompatible(Clause1,Clause2),Body),
        dfunctor(Clause2,Predicate,_),
        pprint(incompatible(Clause1,Clause2),' :-',Body),
        fail.

dlisting(Predicate) :-
        clause(sup(Clause1,Clause2),Body),
        rule_functor(Clause1,Predicate,_),
        pprint(sup(Clause1,Clause2),' :-',Body),
        fail.

dlisting(Predicate) :-
        clause(sup(Clause1,Clause2),Body),
        rule_functor(Clause2,Predicate,_),
        pprint(sup(Clause1,Clause2),' :-',Body),
        fail.

dlisting(_).

%
% dfunctor(+Clause,-Predicate,-Arity)
%   Returns the d-Prolog Predicate of Clause
%   together with its Arity.
%

dfunctor(~ Clause,Predicate,Arity) :-
        functor(Clause,Predicate,Arity),
        !.

dfunctor(Clause,Predicate,Arity):-
        functor(Clause,Predicate,Arity).

%
% rule_functor(+Rule,-Predicate,-Arity)
%   Returns the d-Prolog Predicate of the head of
%   the Rule together with its Arity.
%

rule_functor((Head :- _),Predicate,Arity) :-
        dfunctor(Head,Predicate,Arity),
        !.

rule_functor((Head := _),Predicate,Arity) :-
        dfunctor(Head,Predicate,Arity),
        !.

rule_functor((Head :^ _),Predicate,Arity) :-
        dfunctor(Head,Predicate,Arity),
        !.

%
% pprint(+Head,+Operator,+Body)
%   A formatting routine for printing ordinary Prolog clauses,
%   defeasible rules, and defeaters.
%

pprint(Head,' :-',true) :-
        !,
        write(Head), write('.'), nl.

pprint(Head,Operator,Clause) :-
        write(Head), write(Operator), nl,
        pprint(Clause).

pprint((First,Rest)) :-
        !,
        write('    '), write(First), write(','), nl,
        pprint(Rest).

pprint(Clause) :-
        write('    '), write(Clause), write('.'), nl.

