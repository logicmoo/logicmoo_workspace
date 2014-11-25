:- module(foreign_props,
	  [foreign/1,
	   foreign/2,
	   returns/2,
	   returns_state/1,
	   memory_root/1,
	   ptr/1,
	   ptr/2,
	   dict_t/2,
	   dict_t/3]).

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

:- prop ptr/1 + type.
ptr(I) :- int(I).

:- prop ptr/2 + type.
:- meta_predicate ptr(?,1).
ptr(I, A) :- call(A, I).

:- prop dict_t/2 + type.
:- meta_predicate dict_t(?, :).
dict_t(Term, Desc) :-
    dict_t(Term, _, Desc).
    
:- prop dict_t/3 + type.
:- meta_predicate dict_t(?, ?, :).
dict_t(Term, Tag, M:Desc) :-
    dict_create(Dict, Tag, Desc),
    dict_pairs(Term, Tag, Pairs),
    maplist(dict_kv(Dict, M), Pairs).

dict_kv(Desc, M, Key-Value) :-
    Type =Desc.Key,
    call(M:Type, Value).
