:- module(my_nlist_int,
          [my_nlist_int/1, test_nlist/2]).

:- use_module(library(filesex)).
:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% :- extra_compiler_opts('-O2 -gdwarf-2 -g3 -D__DEBUG_MALLOC__').
:- extra_compiler_opts('-O2 -gdwarf-2 -g3').
:- use_foreign_source('pl-my_nlist_int').
:- include_foreign_dir(include).
:- gen_foreign_library(plbin(lib_my_nlist_int)).

:- pred show_term(+term_t) is fimport. % to help to debug in the c side

show_term(Term) :-
    writeq(user_error, Term),
    nl(user_error).

:- type my_nlist_int/1 + tgen.

my_nlist_int(Node) :- int(Node).
my_nlist_int(List) :- list(my_nlist_int, List).

:- pred test_nlist(+my_nlist_int, -my_nlist_int) is (foreign, memory_root).
