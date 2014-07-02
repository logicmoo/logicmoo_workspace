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

:- multifile foreign_interface:generating/0.
:- dynamic   foreign_interface:generating/0.

:- prop foreign/1 + nortcheck.
:- meta_predicate foreign(0).
foreign(G) :- call(G).

:- prop foreign/2 + nortcheck.
:- meta_predicate foreign(0,?).
foreign(G, _) :- call(G).

:- prop returns/2 + nortcheck.
:- meta_predicate returns(0,?).
returns(G,_) :- call(G).

:- prop returns_state/1 + nortcheck.
:- meta_predicate returns_state(0).
returns_state(G) :- call(G).

:- prop memory_root/1 + nortcheck.
:- meta_predicate memory_root(0).
memory_root(G) :- call(G).

:- prop pointer/1 + type.
pointer(I) :- int(I).

:- prop pointer/2 + type.
pointer(I, A) :- int(I), atm(A).
