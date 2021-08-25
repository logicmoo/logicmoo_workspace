
macroinit :-
	write('*** TEST FILE: macro_tests.pl'), nl,
	backinit,
	backread('backmacros'),
	backmacro(atleast5(R) *= atleast(5,R)),
	backmacro(all1(R,C) *= all(R,C) and atleast(1,R)),
	backmacro(concerns(A,B) *= A and all(r1, B)),
	backmacro(xqval(X) *= all(qualitativeValue,aset(X))),
	c :< anything.

macrotest :-
	macroinit,
	backstate(verbosity = warning),
	macrotest(1),
	macrotest(2),
	macrotest(3),
	macrotest(4).



macrotest(1) :-
	c11 := exactly(5,r),
	c11 ?< atleast(5,r),
	c11 ?< atmost(5,r),
	write('+++ macrotest 1 succeeded'),
	nl,
	!.
macrotest(2) :-
	c21 := some(r,c),
	c22 := some(r) and all(r,c),
	c22 ?< c21,
	write('+++ macrotest 2 succeeded'),
	nl,
	!.

macrotest(3) :-
	c31 := some(r,c),
	c33 := c31 and r:o31,
	o30 :: c33,
	o31 :: c,
	o30 ?: some(r,c),
	o30 ?: c31,
	write('+++ macrotest 3 succeeded'),
	nl,
	!.
macrotest(4) :-
	c41 :< atleast5(r),
	c41 ?< atleast(5,r),
	c42 :< all1(r,c),
	c42 ?< atleast(1,r) and all(r,c),
	c43 := all1(r,atleast5(r)),
	c43 ?< all(r,atleast(3,r)),
	write('+++ macrotest 4 succeeded'),
	nl,
	!.
macrotest(N) :-
	write('--- macrotest '),
	write(N),
	write(' failed'),
	nl,
	assert(test(macrotest(N), failed)).

