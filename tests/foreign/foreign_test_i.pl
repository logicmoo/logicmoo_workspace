:- module(foreign_test_i, [fce/2, aa/2, eq/2, idx/3, numl/2, get_arrays/4,
			   show_arrays/3, io/1, sio/1, f/1, positive_t/1]).

:- use_module(library(swi/assertions)).
:- use_module(library(swi/basicprops)).
:- use_module(library(swi/plprops)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
:- gen_foreign_library('foreign_test_i.so').
:- use_foreign_header('foreign_test.h').
:- use_foreign_source('foreign_test.c').

:- prop positive_t(?int) is (type, foreign(is_positive_t)).

:- prop contain_extern_t/1 is type.
contain_extern_t(contain_extern(Idx, Value)) :-
    int(Idx),
    positive_t(Value).

:- pred fce(+contain_extern_t, -contain_extern_t) is foreign.

:- prop flag_t/1 is type.

flag_t(Value) :- int(Value).

:- prop field_t/1 is type.

field_t(field(A, B, Sum)) :- int(A), int(B), flag_t(Sum).

:- prop position_t/1 is type.

position_t(position(X, Y)) :- int(X), int(Y).

:- prop geometry_t/1 is type.

geometry_t(geometry(P, W, H)) :- position_t(P), int(W), int(H).

:- pred aa(+position_t, -position_t) is foreign(c_aa).

:- pred pp(+int,-int:C) is (foreign(c_pp), returns(C)).

:- pred a(+list(position_t), +position_t) is foreign(c_a).

:- pred eq(+int, -int) is foreign(c_eq).
:- pred idx(+list(num), +int, -num) is foreign(c_idx).
:- pred numl(+int, -list(num)) is (foreign(c_numl), memory_root).

:- pred f(?field_t) is (foreign(c_f), returns_state).
:- pred pq(?position_t) is foreign(c_pq).

:- pred get_arrays(+int,-list(list(list(num))), -list(list(int)), -list(int))
    is (foreign(c_get_arrays), memory_root).

:- pred show_arrays(+list(list(list(num))), +list(list(int)), +list(int))
    is foreign.

:- pred io(int) is foreign(c_io).

:- pred sio(int) is (foreign(c_sio), returns_state, memory_root).

% :- pred u(list(list(list(num))), list(list(int)), list(int), int)
%     is foreign(c_u).

/*
:- true pred s(-list(list(list(num))):LLL, -list(list(int)):LLN, -list(int):LN, -int:N)
    is (foreign(c_s),
	size_of(LLL, LLN),
	size_of(LLN, LN),
	size_of(LN, N)
       ).
*/
