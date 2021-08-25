
valuetest :-
	write('*** TEST FILE: value_tests.pl'), nl,
	asettest,
	numbertest.


asettest :-
	asetinit,
	asettbox,
	asetabox,
	asetinit2,
	asettbox(3).

numbertest :-
	numberinit,
	numbertbox,
	numberabox.

asetinit :-
	backinit,
	d_open := attribute_domain,
	d_closed := attribute_domain([a,b,c,d,e,f,g,h,i,j,k]),
	a1 := aset(c .. g,d_closed),
	a21 := aset([a,b,c]),
	a22 := aset([a,b,c],d_open),
	a23 := aset([a,b,c],d_closed),
	aunion1 := a1 union a23,
	aintersection1 := a1 intersection a23.

asetinit2 :-
	backinit,
	a := aset([]), 
	b := aset([a,b,c]),
	c := aset([e,f,g]),
	d := a intersection c,
	e := attribute_domain([a,b,c,d,e,f,g]),
	f := aset(a..c,e) intersection aset(e..f,e).

asettbox :-
	asettbox(1),
	asettbox(2).

asettbox(1) :-
	a1 ?< d_closed,
	aintersection1 ?< a1,
	a22 ?< d_open,
	a21 ?< aset,
	\+ a22 ?< aset,
	\+ a23 ?< aset,
	\+ aunion2 := a21 union a22,
	write('+++ aset tbox test 1 succeeded'),
	nl,
	!.
asettbox(2) :-
	r1aset :< range(aset),
	r1d_closed :< range(d_closed),
	r1d_open :< range(d_open),
	r2aset :< range(aset),
	r1a1 := r1d_closed and range(a1),
	r1aintersection1 := r1a1 and range(a23),
	r1r2aset := r1aset and r2aset,
	rinc := r1aset.r,
	rtrans := trans(r1d_closed),

	r1aintersection1 ?< range(aintersection1),
	rtrans ?< r1d_closed,

	\+ error := inv(r1d_closed),
	write('+++ aset tbox test 2 succeeded'),
	nl,!.

asettbox(3) :-
	a ?< nothing,
	d ?< nothing,
	f ?< nothing,
	write('+++ aset tbox test 3  succeeded'),
	nl,
	!.

asettbox(N) :-
	write('--- aset tbox test '),
	write(N),
	write(' failed'),
	assert(test(aset_tbox_test(N), failed)),
	nl.

asetabox :-
	backstate(verbosity = warning),
	o1 :: r1aset:el1,
	o2 :: r1aset:el2,
	o3 :: r1aset:el3,
	o1 :: r2aset:el1,

	o4 :: r1d_closed:a,
	o4 :: r1d_closed:c,

	o1 ?: atleast(1,r1r2aset),
	o4 ?: atleast(2,r1d_closed),
	o4 ?: atleast(1,r1a1),
	\+ o4 ?: atleast(2,r1a1),
	o4 ?: atleast(1,r1aintersection1),
	\+ o4 ?: atleast(2,r1aintersection1),
	o4 ?: atmost(1,r1aintersection1),
	write('+++ aset abox test succeeded'),
	nl,
	!.
asetabox :-
	write('--- aset abox test failed'),
	assert(test(aset_abox_test, failed)),
	nl,
	!.

numberinit :-
	backinit,
	nr1 := 17,
	nr2 := 0.757,
	nr3 := -10 .. 10,
	nr4 := ge(-10),
	nr5 := le(10),
	nr6 := nr4 intersection nr5,
	nr7 := gt(-5),
	nr8 := lt(5),
	nr9 := nr7 intersection nr8,
	nr10 := nr9 intersection nr6,
	rnr :< range(number),
	rnr3 := rnr and range(nr3),
	rnr6 := rnr and range(nr6),
	nr11 := 1..10 intersection gt(10),
	nr12 := 4..12 intersection lt(4).
numbertbox :-
	nr3 ?< nr6,
	nr6 ?< nr3,
	nr10 ?< nr9,
	nr9 ?< nr10,
	\+ nrn :< -17 .. -7,
	nr11 ?< nothing,
	nr12 ?< nothing,
	write('+++ number tbox test succeeded'),
	nl,
	!.
numbertbox :-
	write('--- number tbox test failed'),
	assert(test(number_tbox_test, failed)),
	nl.

numberabox :-
	backstate(verbosity = warning),
	o1 :: rnr:7,
	o1 ?: atleast(1,rnr),
	o1 ?: atleast(1,rnr3),
	o1 ?: atleast(1,rnr6),
	write('+++ number abox test succeeded'),
	nl,
	!.
numberabox :-
	write('--- number abox test failed'),
	assert(test(number_abox_test, failed)),
	nl.





