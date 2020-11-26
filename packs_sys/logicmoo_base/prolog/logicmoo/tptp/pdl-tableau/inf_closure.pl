%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003, Renate Schmidt, University of Manchester
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(inf_closure, 
    [
        inf_closure/3
    ]).

:- dynamic
        inf_closure/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inf_closure(State, not(true), _) :- !,
    set_satisfiable_flag(0),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(true)] ** '), pdl_write(not(true)), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',not(true)]), 
    fail.

inf_closure(State, not(A), [A|_]) :- !,
    set_satisfiable_flag(0),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(A)-A] ** '), pdl_write(not(A)), pdl_write(' - '), pdl_write(A), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',not(A),A]), 
    fail.

inf_closure(State, A, [not(A)|_]) :- !,
    set_satisfiable_flag(0),
    pdl_write(State), pdl_write(' ** Unsatisfiable [not(A)-A] ** '), pdl_write(A), pdl_write(' - '), pdl_write(not(A)), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',A,not(A)]), 
    fail.

inf_closure(State, x(Y,A), [not(A)|_]) :- !,
    set_satisfiable_flag(0),
    pdl_write(State), pdl_write(' ** Unsatisfiable [x(Y,A)-not(A)] ** '), pdl_write(x(Y,A)), pdl_write(' - '), pdl_write(not(A)), pdl_nl,
    print_proof_step(State, '** Unsatisfiable **', ['clash',x(Y,A),not(A)]), 
    fail.

inf_closure(_, _, []) :- !.

%% ignore X variables
%inf_closure(State, x(_,_), _) :- !.
%inf_closure(State, not(x(_,_)), _) :- !.

inf_closure(State, Term, [_|Set]) :- !,
    inf_closure(State, Term, Set).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
