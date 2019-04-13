
:- use_module(library(assertions)).
:- use_module(foreign_test_i).

:- begin_tests(foreign).

test(enum_example) :-
    f_enum_example(element(a), X, Y, I),
    assertion(I==1),
    assertion(X==Y),
    assertion(X==element(f(g(h)))).
test(union_example) :-
    f_union_example(pair(2,3), X, Y, I),
    assertion(I==2),
    assertion(X==Y),
    assertion(X==d(d{value1:a,value2:[b,c,d]})).

test(fortran1) :-
    fortran1(1.5, X),
    assertion(X =:= 2.25).

test(foreign_eq) :-
    eq(2, B),
    assertion(B=:=2).

test(foreign_idx) :-
    L=[1,5,3,4],
    forall(nth0(Idx, L, E),
           ( idx(L,Idx,D),
             assertion(D=:=E)
           )).

test(foreign_extend) :-
    extend([1,2,3],X),
    assertion(X==[1,2,3,2,4,6]).

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
    once(negative_t(_)),
    negative_t(-2),
    \+negative_t(2).

test(foreign_cmptype) :-
    fce(contain_extern(1,3),B),
    assertion(B==contain_extern(1, 1)).

test(foreign_opaque) :-
    fco(contain_opaque(1,-3),B),
    assertion(B==contain_opaque(1, -1)).

/*
test(foreign_opaqueex, [error(type_error(negative_t, 1))]) :-
    fco(contain_opaque(1,-1), _).
*/

test(foreign_dict1) :-
    fd1(d{listv:[2, 4], value1:a, value2:b},A,B,C),
    assertion([A,B,C]==[a,"b",4]).

test(foreign_dict2) :-
    fd2(A, a, b, 2),
    assertion(A==d{listv:[2,4],value1:a,value2:b}).

test(foreign_dict3) :-
    fd3(A,a,b,[1,2,3]),
    assertion(A==d{listv:[1, 2, 3], value1:a, value2:b}).

test(foreign_dict4) :-
    fd3(A,_,_,_),
    assertion(A==d{}).

test(fimport1) :-
    test_ireverse1([1,2,3], Y),
    assertion(Y==[3,2,1]).

test(fimport2) :-
    test_ireverse2([1,2,3], Y),
    assertion(Y==[3,2,1]).

:- end_tests(foreign).
