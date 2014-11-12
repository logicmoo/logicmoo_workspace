:- module(foreign_props,
	  [foreign/1,
	   foreign/2,
	   returns/2,
	   returns_state/1,
	   memory_root/1,
	   pointer/1,
	   pointer/2]).

:- use_module(library(swi/assertions)).
:- use_module(library(swi/basicprops)).
:- use_module(library(swi/plprops)).

:- prop foreign/1 + no_rtcheck.
:- meta_predicate foreign(0).
foreign(G) :- call(G).

:- prop foreign/2 + no_rtcheck.
:- meta_predicate foreign(0,?).
foreign(G, _) :- call(G).

:- prop returns/2 + no_rtcheck.
:- meta_predicate returns(0,?).
returns(G,_) :- call(G).

:- prop returns_state/1 + no_rtcheck.
:- meta_predicate returns_state(0).
returns_state(G) :- call(G).

:- prop memory_root/1 + no_rtcheck.
:- meta_predicate memory_root(0).
memory_root(G) :- call(G).

:- prop pointer/1 + type.
pointer(I) :- int(I).

:- prop pointer/2 + type.
pointer(I, A) :- int(I), atm(A).

:- prop dict_t/2 + type.
:- meta_predicate dict_t(?, :).
dict_t(Term, M:Desc) :-
    is_dict(Desc, Tag),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Desc, M), Pairs).

dict_kv(Desc, M, Key-Value) :-
    Type =Desc.Key,
    call(M:Type, Value).
