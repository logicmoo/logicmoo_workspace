:- module(foreign_test_i, [fce/2, fco/2, aa/2, eq/2, idx/3, numl/2, io/1, f/1,
                           get_arrays/4, show_arrays/3, sio/1, negative_t/1,
                           positive_t/1, fd1/4, fd2/4, fd3/4, extend/2,
                           test_ireverse1/2, test_ireverse2/2]).

:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -D__DEBUG_MALLOC__').
:- extra_compiler_opts('-O2 -gdwarf-2 -g3').
:- use_foreign_header(foreign_test).
:- use_foreign_source(foreign_test).
:- gen_foreign_library(.(foreign_test_i)).

this_dir(Dir) :-
    context_module(M),
    current_module(M, Path),
    directory_file_path(Dir, _, Path).

:- ( \+ user:file_search_path('.', _)
    ->this_dir(Dir),
    asserta(user:file_search_path('.', Dir))
    ; true
    ).

:- type d_t/1.

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

:- type positive_t/1.
positive_t(N) :- int(N).

:- type union_t/1.
union_t(u(First, Second)) :-
    int(First),
    int(Second).
union_t(num(Number)) :-
    num(Number).
union_t(positive(T)) :-
    positive_t(T).

:- type negative_t/1 is foreign(is_negative_t).

:- type contain_extern_t/1.
contain_extern_t(contain_extern(Idx, Value)) :-
    int(Idx),
    positive_t(Value).

:- type contain_opaque_t/1.
contain_opaque_t(contain_opaque(Idx, Value)) :-
    int(Idx),
    negative_t(Value).

:- type example_t/1.
example_t(example(Name, Value)) :-
    atm(Name),
    num(Value).

:- type compound_t/1.
compound_t(compound(Idx, Value, Example, Name, PExample)) :-
    int(Idx),
    ptr(int, Value),
    example_t(Example),
    ptr(atm, Name),
    ptr(example_t, PExample).

:- pred fce(+contain_extern_t, -contain_extern_t) is foreign.

:- pred fco(+contain_opaque_t, -contain_opaque_t) is foreign.

:- type flag_t/1.

flag_t(Value) :- int(Value).

:- type field_t/1.

field_t(field(A, B, Sum)) :- int(A), int(B), ptr(flag_t, Sum).

:- type position_t/1.

position_t(position(X, Y)) :- int(X), int(Y).

:- type geometry_t/1.

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

:- pred [ireverse1(     +list(int), -list(int)) is (fimport, returns_state, memory_root),
         test_ireverse1(+list(int), -list(int)) is (foreign, memory_root),
         ireverse2(+list(int):LIn,  -list(int)) is (fimport, returns_state, parent(LIn)),
         test_ireverse2(+list(int), -list(int)) is (foreign)].

ireverse1(X,Y) :- reverse(X, Y).

ireverse2(X,Y) :- reverse(X, Y).

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
