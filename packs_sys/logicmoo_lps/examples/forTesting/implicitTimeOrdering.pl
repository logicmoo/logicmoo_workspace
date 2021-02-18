
:- expects_dialect(lps).

events e(_), f(_).
actions a(_).

foo from _ if
	e(X) from T, f(Y) from T,
	a(boo) from T1.

if true then 
	foo.

observe e(1) from 2.
observe f(2) from 2.
