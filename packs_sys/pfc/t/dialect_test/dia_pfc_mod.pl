
:- module(foo,[a/1]).



:- expects_dialect(pfc).
% :- set_fileAssertMt(foo).

a(1).
% :- rtrace.
==> a(2).

% (a =-=> b).
:- trace.
a(2) <==> b(1).
