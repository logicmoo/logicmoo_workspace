revisiontest :-
	write('*** TEST FILE: revision_tests.pl'), nl,
	tboxrevision,
	nl,
	tboxaboxrevision,
	nl.


tboxrevision :-
	tboxrevinit,
	tboxrev(1),
	tboxrev(2),
	tboxrev(3),
	tboxrev(4),
	tboxrev(5),
	tboxrev(6),
	tboxrev(7),
	tboxrev(8).

tboxaboxrevision :-
	trart(1),
	trart(2),
	trart(3),
	trart(4).


tboxrevinit :-
	backinit,
	r1 :< range(anything),
	r2 :< range(anything),
	r3 :< range(anything),
	c1 :< anything,
	c2 :< anything,
	c3 :< anything,
	c4 :< anything,
	c5 :< anything,
	c6 :< anything,
	c7 :< anything,
	c12 := c1 and c2,
	c31 :< c3,
	c61 :< c6,
	c71 :< c7,
	all1 :< all(r1,c1),
	all31 :< all(r1,c31),
	atl1 := atleast(3,r1),
	atm1 := atmost(3,r1),
	all7 :< all(r1,c7).

tboxrev(1) :-
	\+ c4 ?< c3,
	c4 :< c3,
	c4 ?< c3,
	write('+++ tbox revision 1 succeeded'),
	nl,
	!.

tboxrev(2) :-
	\+ c2 ?< c3,
	\+ c12 ?< c3,
	c12 ?< c1 and c2,
	c1 and c2 ?< c12,
	c2 :< c3,
	c2 ?< c3,
	c12 ?< c3,
	c12 ?< c1 and c2,
	c1 and c2 ?< c12,
	write('+++ tbox revision 2 succeeded'),
	nl,
	!.

tboxrev(3) :-
	all1 ?< all(r1,c1),
	c1 :< c5,
	all1 ?< all(r1,c1),
	all1 ?< all(r1,c5),
	write('+++ tbox revision 3 succeeded'),
	nl,
	!.

tboxrev(4) :-
	c3 :< c5,
	c3 ?< c5,
	c4 ?< c5,
	c31 ?< c5,
	all31 ?< all(r1,c5),
	write('+++ tbox revision 4 succeeded'),
	nl,
	!.
tboxrev(5) :-
	c6 ?< anything,
	c61 ?< anything,
	\+ c6 ?< domain(anything) and range(anything),
	\+ c61 ?< domain(anything) and range(anything),
	\+c6 :< r1,
%	\+ c6 ?< anything,
%	\+ c61 ?< anything,
%	c6 ?< domain(anything) and range(anything),
%	c61 ?< domain(anything) and range(anything),
	write('+++ tbox revision 5 succeeded'),
	nl,
	!.
tboxrev(6) :-
	\+ r1 ?< r3,
	r1 :< r3,
	r1 ?< r3,
	atl1 ?< atleast(3,r3),
	atmost(3,r3) ?< atm1,
	write('+++ tbox revision 6 succeeded'),
	nl,
	!.
tboxrev(7) :-
	c71 ?< c7,
	c71 ?< c7,
	\+ c71 ?< c3,
	c71 :< c3,
	\+ c71 ?< c7,
	c71 ?< c3,
	write('+++ tbox revision 7 succeeded'),
	nl,
	!.
tboxrev(8) :-
	\+ c7 :< r1,
	\+ c7 ?< r1,
	\+ c1 :< c12,
	\+ c1 ?< c12,
	\+ c1 :< all(r,c12),
	\+ c1 ?< all(r,c12),	
	write('+++ tbox revision 8 succeeded'),
	nl,
	!.

tboxrev(N) :-
	write('--- tbox revision '),
	write(N),
	write(' failed'),
	assert(test(tbox_revision_test(N), failed)),
	nl.

trart(1) :- 
	backinit,
	backstate(verbosity = warning),
    %% base tbox
	p1 :< anything,
	p2 :< anything,
	r1 :< domain(anything) and range(anything),
	c3 := p1 and all(r1, p2),
    %% base abox
	o1 :: c3 and r1:o2,
	o1 ?: p1,
	o2 ?: p2,
    %% tbox revision
	c3 := p2 and all(r1, p1),
        o1 ?: p2,
	o2 ?: p1,
        not(o1 ?: p1),
	not(o2 ?: p2),
	write('+++ tbox-abox revision 1 succeeded'),
	nl,
	!.
trart(2) :-
	backinit,
	backstate(verbosity = warning),
    %% base tbox
	p1 :< anything,
	p2 :< anything,
        r1 :< range(p1),
    %% base abox
	o1 :: p1 and r1:o2,
	o1 ?: p1 noibox,
	o2 ?: p1 noibox,
    %% tbox revision
	r1 :< range(p2),
	o1 ?: p1 noibox,
	o2 ?: p2 noibox,             
	not(o2 ?: p1 noibox),        
	write('+++ tbox-abox revision 2 succeeded'),
	nl,
	!.
trart(3) :-
	backinit,
	backstate(verbosity = warning),
    %% base tbox
	p1 :< anything,
	p2 :< anything,
	p3 :< p2,
	p4 :< p2 and not(p3),
	r1 :< range(p3),
	c3 := p1 and all(r1, p2),
    %% base abox
	o1 :: c3 and r1:o2,
	o1 ?: p1 noibox,
	o2 ?: p3 noibox,
    %% tbox revision
	r1 :< range(p4),   %% dependents: [c3]
	o2 ?: p4 noibox,
	not(o2 ?: p3 noibox),
	write('+++ tbox-abox revision 3 succeeded'),
	nl,
	!.


trart(4) :-
	backinit,
	backstate(verbosity = warning),
    %% wie trart(2), nur mit domain
    %% base tbox
	p1 :< anything,
	p2 :< anything,
        r1 :< domain(p1),
    %% base abox
	o1 :: r1:o2,
	o1 ?: p1 noibox,
    %% tbox revision
	r1 :< domain(p2),
	o1 ?: p2 noibox,        
	not(o2 ?: p1 noibox),    
	write('+++ tbox-abox revision 4 succeeded'),
	nl,
	!.

trart(N) :-
	write('--- tbox-abox revision '),
	write(N),
	write(' failed'),
	assert(test(tbox_abox_revision_test(N), failed)),
	nl.


