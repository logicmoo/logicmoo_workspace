
:- module(foo,[a/1, b/1]).

:- set_prolog_flag(pfc_version,1.8).

:- expects_dialect(pfc).

%:- trace.
a(1).
a(2).
a(2) <==> b(1).

:- listing(b/1).


