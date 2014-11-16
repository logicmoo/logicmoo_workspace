
:- use_module(library(swi/assertions), except([(test)/1])).
:- use_module(foreign_test_i).

:- begin_tests(foreign).

test(foreign_eq) :-
    eq(2, B),
    assertion(B=:=2).

test(foreign_idx) :-
    L=[1,5,3,4],
    forall(nth0(Idx, L, E),
	   ( idx(L,Idx,D),
	     assertion(D=:=E)
	   )).

test(foreign_numl) :-
    numl(5, L),
    assertion(L==[1.0,2.0,3.0,4.0,5.0]).

test(foreign_arrays) :-
    get_arrays(5,AAA,AA,A),
    show_arrays(AAA,AA,A),
    assertion(A==[0,1,2,3,4]).

test(foreign_typeio) :-
    aa(position(2,4),XX),
    assertion(XX==position(20,40)).

test(foreign_io) :-
    io(_),
    io(2).

test(foreign_sio) :-
    sio(SIO),
    assertion(SIO==510).

test(foreign_sio_neg) :-
    \+ sio(1).

test(foreign_fields) :-
    f(field(2,3,Sum)),
    assertion(Sum==5).

test(foreign_type) :-
    positive_t(_),
    positive_t(2),
    \+positive_t(-2).

test(foreign_cmptype) :-
    fce(contain_extern(1,3),B),
    assertion(B==contain_extern(1, 1)).

test(foreign_typeex, [error(type_error(positive_t, 0 ))]) :-
    fce(contain_extern(1,2),_).

:- end_tests(foreign).
