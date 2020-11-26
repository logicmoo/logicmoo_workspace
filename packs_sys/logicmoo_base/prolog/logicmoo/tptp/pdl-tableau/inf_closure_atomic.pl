%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003,    Renate Schmidt,  University of Manchester
%  Modified 2009-10, Ullrich Hustadt, University of Liverpool
%
% Uses
%
%   p, -p
%   ----- (p must be atomic and X-free)
%   clash
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(inf_closure, 
    [
        inf_closure/3,
	inf_closure/4
    ]).

:- dynamic
        inf_closure/3.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inf_closure(State, not(true), _, Fml) :- !,
    set_satisfiable_flag(0),
    store_inconsistent(Fml),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(true)] ** '), pdl_write(not(true)), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',not(true)]), 
    fail.

% Definitions 7 and 23 in (De Giacomo and Massacci, 2000) give different
% conditions for the simplest form of an inconsistency: Definition 7 only
% recognises an inconsistency between A and not(A) where A is atomic,
% while Definition 23 recognises an inconsistency between F and not(F) where
% F can be an arbitrary formula. However, using Definition 23, gives incorrect
% results for some formulae. Therefore, we only use Definition 7.
inf_closure(State, not(A), [A|_], Fml) :- 
    atomic(A),
    !,
    set_satisfiable_flag(0),
    store_inconsistent(Fml),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(A)-A] ** '), pdl_write(not(A)), pdl_write(' - '), pdl_write(A), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',not(A),A]), 
    fail.

inf_closure(State, A, [not(A)|_], Fml) :- 
    atomic(A),
    !,
    set_satisfiable_flag(0),
    store_inconsistent(Fml),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(A)-A] ** '), pdl_write(not(A)), pdl_write(' - '), pdl_write(A), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',A,not(A)]), 
    fail.

inf_closure(_, _, [], _) :- !.

inf_closure(State, Term, [_|Set], Fml) :- !,
    inf_closure(State, Term, Set, Fml).

