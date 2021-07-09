
:- module(bar,[]).

:- set_prolog_flag(pfc_version,1.8).

:- expects_dialect(pfc).

a(1).
a(2).

%:- trace.
a(2) <==> b(1).

:- listing(b/1).


