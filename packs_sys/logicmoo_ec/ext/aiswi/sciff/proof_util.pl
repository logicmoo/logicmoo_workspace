%----------------------------------------------------------
% Utilities for Java/Sicstus interfacing
% 
% Marco ALberti
% Federico Chesani
% Creation: 1 September 2003
% Last modification: 30 August 2005
%----------------------------------------------------------


:- module(proof_util, [
%        printConstraints/0,
%        printConstraints/1,
        setSCV/1,
        latest_happened/2,
        scv/1,
        crea_lista_chars/2
    ]).
        
:- dynamic scv/1.





:- use_module( library(chr)).
:- use_module(library(system)).
:- use_module( library(lists)).
:- use_module(prolog_version).
:- (is_dialect(swi) -> true
   ; use_module( library(codesio))).
:- use_module( library(terms)).
:- use_module( library(clpfd)).
:- use_module( print_clp_constraints).







%------------------------------------------------------------------------------
crea_lista_chars([], []) :- !.
crea_lista_chars([H|T], Result) :-
    crea_lista_chars_vincolo(H, Res1),
    crea_lista_chars(T, Res2),
    append(Res1, Res2, Result).

crea_lista_chars_vincolo(H, Result) :-
    H=..[_A,B,C],
    converti(C, LC),
    converti(B, LB),
    append(LC, "     ", R1),
    append(R1, LB, R2),
    append(R2, "\n", Result).
    
converti(X, LX) :-
    X=..[suspension, N |_],
    !,
    number(N),
    number_to_chars(N, R1),
    append("<c", R1, R2),
    append(R2, ">", LX).
converti(X, L) :-
    X = pending(_),
    !,
    write_to_chars(X, LX),
    term_variables_bag(X, XVAR),
    create_diseq(XVAR, LCLP),
    append(LX, LCLP, L).
converti(X, L) :-
    X = gt_current_time(_,_),
    !,
    write_to_chars(X, LX),
    term_variables_bag(X, XVAR),
    create_diseq(XVAR, LCLP),
    append(LX, LCLP, L).
converti(X, L) :-
    X = inconsistent(_,_),
    !,
    write_to_chars(X, LX),
    term_variables_bag(X, XVAR),
    create_diseq(XVAR, LCLP),
    append(LX, LCLP, L).
converti(X, L) :-
    X = viol(_),
    !,
    write_to_chars(X, LX),
    term_variables_bag(X, XVAR),
    create_diseq(XVAR, LCLP),
    append(LX, LCLP, L).
converti(X, LX) :-
    write_to_chars(X, LX).



create_diseq([], []) :-
    !.
create_diseq([H|T], LL) :-
    create_max(H, LMAX),
    create_min(H, LMIN),
    append(LMIN, LMAX, LH),
    create_diseq(T, LT),
    append(LH, LT, LL).

create_max(X, []) :-
    fd_max(X, sup),
    !.
create_max(X, L) :-
    fd_max(X, N),
    write_to_chars(X, LX),
    append(", ", LX, L1),
    append(L1, " <= ", L2),
    number_to_chars(N, N1),
    append(L2, N1, L).

create_min(X, []) :-
    fd_min(X, inf),
    !.
create_min(X, L) :-
    fd_min(X, N),
    write_to_chars(X, LX),
    append(", ", LX, L1),
    append(L1, " >= ", L2),
    number_to_chars(N, N1),
    append(L2, N1, L).






%------------------------------------------------------------------------------
% PRINTING UTILITIES THROUGH STANDARD OUTPUT
%------------------------------------------------------------------------------
stampa_lista_vincoli([]) :- !.
stampa_lista_vincoli([H|T]):-
        stampa_vincolo(H),
        stampa_lista_vincoli(T).

stampa_vincolo(H) :-
    H=..[_A,B,C],
    print(C),
    write('     '),
    write(B),
    nl.

%------------------------------------------------------------------------------
% UTILITY FOR HOOKING JAVA
%------------------------------------------------------------------------------
setSCV(X) :-
    assert(scv(X)).
    



%------------------------------------------------------------------------------
% ACCESSING THE LATEST HAPPENED FACT...
%------------------------------------------------------------------------------


latest_happened(HPattern, HLatest) :-
	findall_constraints(HPattern, HList),
	latest(HList, HLatest).

latest([], _) :- !.
latest([HTerm#_Id| Tail], HLatest) :-
	latest(Tail, HTerm, HLatest).

latest([], HLatest, HLatest) :- !.
latest([HTerm#_Id| Tail], Partial, HLatest) :-
	get_time(HTerm, T1),
	get_time(Partial, T2),
	T2 > T1, !,
	latest(Tail, Partial, HLatest).
latest([HTerm#_Id| Tail], _Partial, HLatest) :-
	latest(Tail, HTerm, HLatest).

get_time(h(_, T), T).



%------------------------------------------------------------------------------
% COUNTING THE HAPPENED FACTS
%------------------------------------------------------------------------------

count_happened(HPattern, N) :-
	findall_constraints(HPattern, List),
	length(List, N).
