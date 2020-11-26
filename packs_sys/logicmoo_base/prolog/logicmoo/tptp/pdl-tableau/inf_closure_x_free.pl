%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003, Renate Schmidt,  University of Manchester
%  Modified 2009, Ullrich Hustadt, University of Liverpool
%
% Uses
%
%   F, -F
%   ----- (F must be X-free)
%   clash
%
%   X_{<a*>p}, -<a*>p
%   -----------------
%        clash
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
% F can be an arbitrary formula. However, in both cases F is not allowed to
% contain an occurrence of X.
inf_closure(State, not(A), [A|_], Fml) :- 
    x_free(A),
    !,
    set_satisfiable_flag(0),
    store_inconsistent(Fml),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(A)-A] ** '), pdl_write(not(A)), pdl_write(' - '), pdl_write(A), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',not(A),A]), 
    fail.

inf_closure(State, A, [not(A)|_], Fml) :- 
    x_free(A),
    !,
    set_satisfiable_flag(0),
    store_inconsistent(Fml),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(A)-A] ** '), pdl_write(not(A)), pdl_write(' - '), pdl_write(A), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',A,not(A)]), 
    fail.

% The following rule corresponds to condition 2 of Definition 23.
inf_closure(State, x(Y,A), [not(A)|_], Fml) :- 
%    atomic(A),
    !,
    set_satisfiable_flag(0),
    store_inconsistent(Fml),
    pdl_write(State), pdl_write(' ** Unsatisfiable [x(Y,A)-not(A)] ** '), pdl_write(x(Y,A)), pdl_write(' - '), pdl_write(not(A)), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',x(Y,A),not(A)]), 
    fail.

inf_closure(_, _, [], _) :- !.

%% ignore X variables
%inf_closure(State, x(_,_), _) :- !.
%inf_closure(State, not(x(_,_)), _) :- !.

inf_closure(State, Term, [_|Set], Fml) :- !,
    inf_closure(State, Term, Set, Fml).


x_free(dia(_P,F)) :-
	x_free(F),
	!.
x_free(box(_P,F)) :-
	x_free(F),
	!.
x_free(not(F)) :-
	x_free(F),
	!.
x_free(or(F,G)) :-
	x_free(F),
	x_free(G),
	!.
x_free(and(F,G)) :-
	x_free(F),
	x_free(G),
	!.
x_free(A) :-
	atomic(A),
	!.
