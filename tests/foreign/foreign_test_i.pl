:- module(foreign_test_i, [fce/2, fco/2, aa/2, eq/2, idx/3, numl/2, io/1, f/1,
			   get_arrays/4, show_arrays/3, sio/1, negative_t/1,
			   positive_t/1, fd1/4, fd2/4, fd3/4, extend/2]).

:- use_module(library(swi/assertions)).
:- use_module(library(swi/basicprops)).
:- use_module(library(swi/plprops)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
:- extra_compiler_opts('-O2 -gdwarf-2 -g3 -D__DEBUG_MALLOC__').
:- use_foreign_header(foreign_test).
:- use_foreign_source(foreign_test).
:- gen_foreign_library(.('foreign_test_i.so')).

this_dir(Dir) :-
    context_module(M),
    current_module(M, Path),
    directory_file_path(Dir, _, Path).

:-  ( \+ user:file_search_path('.', _)
    ->this_dir(Dir),
    asserta(user:file_search_path('.', Dir))
    ; true
    ).

:- prop d_t/1 is type.

d_t(Dict) :-
    dict_t(Dict,
	   d{value1:atm,
	     value2:atm,
	     listv:list(int)
	    }).

:- pred [fd1(+d_t,atm,atm,int),
	 fd2(-d_t,+atm,+atm,+int)+memory_root,
	 fd3(d_t,atm,atm,list(int))+memory_root
	] is foreign.

:- prop positive_t/1 is type.
positive_t(N) :- int(N).

:- prop negative_t/1 is (type, foreign(is_negative_t)).

:- prop contain_extern_t/1 is type.
contain_extern_t(contain_extern(Idx, Value)) :-
    int(Idx),
    positive_t(Value).

:- prop contain_opaque_t/1 is type.
contain_opaque_t(contain_opaque(Idx, Value)) :-
    int(Idx),
    negative_t(Value).

:- prop example_t/1 is type.
example_t(example(Name, Value)) :-
    atm(Name),
    num(Value).

:- prop compound_t/1 is type.
compound_t(compound(Idx, Value, Example, Name, PExample)) :-
    int(Idx),
    ptr(Value, int),
    example_t(Example),
    ptr(Name, atm),
    ptr(PExample, example_t).

:- pred fce(+contain_extern_t, -contain_extern_t) is foreign.

:- pred fco(+contain_opaque_t, -contain_opaque_t) is foreign.

:- prop flag_t/1 is type.

flag_t(Value) :- int(Value).

:- prop field_t/1 is type.

field_t(field(A, B, Sum)) :- int(A), int(B), ptr(Sum,flag_t).

:- prop position_t/1 is type.

position_t(position(X, Y)) :- int(X), int(Y).

:- prop geometry_t/1 is type.

geometry_t(geometry(P, W, H)) :- position_t(P), int(W), int(H).

:- pred aa(+position_t, -position_t) is foreign(c_aa).

:- pred pp(+int,-int:C) is (foreign(c_pp), returns(C)).

:- pred a(+list(position_t), +position_t) is foreign(c_a).

:- pred extend(+list(int),-list(int)) is foreign.

:- pred eq(+int, -int) is foreign(c_eq).

:- pred idx(+list(num), +int, -num) is foreign(c_idx).

:- pred numl(+int, -list(num)) is (foreign(c_numl), memory_root).

:- pred f(?field_t) is (foreign(c_f), returns_state, memory_root).
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
