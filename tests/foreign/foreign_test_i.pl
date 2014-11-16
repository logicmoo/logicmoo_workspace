:- module(foreign_test_i, [aa/2, p/1, q/3, r/2, s/4, t/4, io/1, sio/1, f/1]).

:- use_module(library(swi/assertions)).
:- use_module(library(foreign/foreign_interface)).
:- gen_foreign_library('foreign_test_i.so').
:- use_foreign_source('foreign_test.c').

%:- use_module(xif4(xif4)).

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

:- pred p(+int) is foreign(c_p).
:- pred q(+list(num), +int, -num) is foreign(c_q).
:- pred r(+int, -list(num)) is (foreign(c_r), memory_root).

:- pred f(?field_t) is (foreign(c_f), returns_state).
:- pred pq(?position_t) is foreign(c_pq).

:- pred s(-list(list(list(num))), -list(list(int)), -list(int), -int)
    is (foreign(c_s), memory_root).

:- pred t(+list(list(list(num))), +list(list(int)), +list(int), +int)
    is foreign(c_t).

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
