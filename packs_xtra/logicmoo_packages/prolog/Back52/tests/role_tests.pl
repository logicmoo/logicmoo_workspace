
roletest :-
	write('*** TEST FILE: role_tests.pl'), nl,
	roletboxtest,
	rocotest,
	arevtest.

roletboxtest :-
	backinit,
	rottest(1),
	rottest(2),
	rottest(3),
	rottest(4),
	rottest(inc).

rocotest :-
	rocotest(1),
	rocotest(2),
	rocotest(3),
	rocotest(4).
arevtest :-
	arevtest(1),
	arevtest(2),
	arevtest(4).

rottest(1) :-
	c1 :< anything,
	c2 :< anything,
	r1 :< range(anything),
	r12 := r1 and r2,
	r3 :< domain(c1),
	r4 :< range(c2),
	r5 := r2 and r3 and r4,
	inv_r1 := inv(r1),
	inv_r2 := inv(r2),
	inv_r12 := inv(r12),
	r5 ?< r3,
	\+ r3 ?< r5,
	inv_r12 ?< inv_r1,
	inv(r1) and inv(r2) ?< inv_r12,
	inv_r12 ?< inv(r1) and inv(r2),
	write('+++ role tbox test 1 succeeded'),
	nl,
	!.
rottest(2) :-
	trans_r5 := trans(r5),
	trans_trans_r5 := trans(trans_r5),
	inv_trans_r5 := inv(trans(r5)),
	inv_r5 := inv(r5),       % inv(r5)  intern schon vorhanden!
	trans_inv_r5 := trans(inv_r5),
	trans_r5_and_r6 := trans(r5 and r6),
	trans_r5_and_trans_r6 := trans(r5) and trans(r6),
	r5 ?< trans(r5),
	\+ trans_r5 ?< r5,
	trans_trans_r5 ?< trans_r5,
	inv_trans_r5 ?< trans_inv_r5,
	trans_inv_r5 ?< inv_trans_r5,
	trans_r5_and_r6 ?< trans_r5_and_trans_r6,
	\+ trans_r5_and_trans_r6 ?< trans_r5_and_r6,
	write('+++ role tbox test 2 succeeded'),
	nl,
	!.
rottest(3) :-
	r0_comp_r1 := r0.r1,
	r1_comp_r0 := r1.r0,
	r0_comp_r1_comp_r1_comp_r0 := r0_comp_r1.r1_comp_r0,
	r1_comp_r1_and_r5 := (r1.r1) and r5,
	rcomp := r0.r1_comp_r1_and_r5.r0,
	rcomp ?< r0_comp_r1_comp_r1_comp_r0,
	\+ r0_comp_r1_comp_r1_comp_r0 ?< rcomp,
	r5_comp_r5 := r5.r5,
	write('+++ role tbox test 3 succeeded'),
	nl,
	!.
rottest(4) :-
	inv_r5_comp_inv_r5 := inv_r5.inv_r5,
	inv_r5_comp_r5 := inv(r5_comp_r5),
	inv_r1_comp_r0 := inv(r1_comp_r0),
	inv_r0_comp_inv_r1 := inv(r0).inv(r1),
	inv_r5_comp_inv_r5 ?< inv_r5_comp_r5,
	inv_r5_comp_r5 ?< inv_r5_comp_inv_r5,
	inv_r1_comp_r0 ?< inv_r0_comp_inv_r1,
	inv_r0_comp_inv_r1 ?< inv_r1_comp_r0,
	write('+++ role tbox test 4 succeeded'),
	nl,
	!.

rottest(inc) :-
	backinit,
	c1 :< anything,
	not_c1 :< not(c1),
	r0 :< domain(anything) and range(anything),
	r1 :< domain(c1),
	r2 :< domain(not_c1),
	r3 :< range(c1),
	r4 :< range(not_c1),
	ri1 := r0 and not(r0),
	ri1 ?< nothing noibox,
	ri2 :< domain(c1 and not_c1),
	ri2 ?< nothing noibox,
	ri3 :< range(c1 and not_c1),
	ri3 ?< nothing noibox,
	ri4 := r1 and r2,
	ri4 ?< nothing noibox,
	ri5 := r3 and r4,
	ri5 ?< nothing noibox,
	ri_inv :=inv(r1 and domain(not_c1)),
	ri_inv ?< nothing noibox,
	ri_trans := trans(r4 and range(c1)),
	ri_trans ?< nothing noibox,
	ri_comp1 := inv(ri1).r1,
	ri_comp1 ?< nothing noibox,
	ri_comp2 := r2.ri1,
	ri_comp2 ?< nothing noibox,
	ri_comp3 := r4.r1,
	ri_comp3 ?< nothing noibox,
	write('+++ role_tbox_test incoherence succeeded'),
	nl,
	!.
rottest(N):-
	   write('--- role_tbox_test '),
	   write(N),
	   write(' failed'),
	   assert(test(role_tbox_test(N), failed)),
	   nl.

rocotest(1) :-
	backinit,
	backstate(verbosity = warning),
	c1:<anything,
	c2:<anything,
	r1:<domain(anything) and range(anything),
	r2:<domain(anything) and range(anything),
	r12:=r1 and r2,
	r1domain:=r1 and domain(c1 and c2),
	r1range:=r1 and range(c1 and c2),
	backtell(o0 :: c1),
	backtell(o1 :: c1 and (r1:o0)),
	backtell(o2 :: c1 and (r1:o0)),
	backtell(o3 :: c1 and (r1:o0)),
	backtell(o1 :: c2),
	backtell(o2 :: c1 and (r2:o0)),
	backtell(o0 :: c2),
	backtell(o3 :: c2 and (r2:o0)),

	backask(o1 ?: r1 : o0),
	backask(o1 ?: r1range : o0),
	backask(o2 ?: r1range : o0 and r12 : o0),
	backask(o3 ?: r1domain : o0 and r1range : o0 and r12 : o0),
	\+ backask(o2 ?: atleast(1,r1domain)),
	\+ backask(o1 ?: atleast(1,r12)),
	write('+++ role completion test 1 succeeded'),
	nl,
	!.
rocotest(2) :-
	backinit,
	backstate(verbosity = warning),
	c:<anything,
	r1:<domain(anything) and range(anything),
	ir1:=inv(r1),
	tr1:=trans(r1),
	backtell(o01::c),
	backtell(o02::c),
	backtell(o03::c),
	backtell(o04::c),
	backtell(o05::c),
	backtell(o1::c and (r1:o01)),
	backtell(o1::c and (r1:o02)),
	backtell(o1::c and (r1:o03)),
	backtell(o2::c and (ir1:o04)),
	backtell(o2::c and (ir1:o05)),
	backtell(o2::c and (r1:o1)),

	backask(o04 ?: atleast(5,tr1)),
	backask(o01 ?: atleast(4,inv(tr1))),
	backask(o1 ?: atleast(3,tr1) and atleast(3,inv(tr1))),
	backretrieve(getall(inv(tr1) : o2)),
	backretrieve(getall(tr1 : o2)),
	write('+++ role completion test 2 succeeded'),
	nl,
	!.
rocotest(3) :-
	backinit,
	backstate(verbosity = warning),
	c:<anything,
	r1:<range(anything),
	r2:<range(anything),
	r3:<range(anything),
	ir1:=inv(r1),
	c123:=r1.r2.r3,
	backtell(o01::c),
	backtell(o02::c),
	backtell(o03::c),
	backtell(o04::c),
	backtell(o05::c),
	backtell(o1::c and (r3:o01)),
	backtell(o1::c and (r3:o02)),
	backtell(o1::c and (r3:o03)),
	backtell(o2::c and (ir1:o04)),
	backtell(o2::c and (ir1:o05)),
	backtell(o2::c and (r2:o1)),
	backtell(o3::c and (r1:o2)),

	backretrieve(getall c123 : o01),
	backretrieve(getall inv(c123) : o04),
	backask(o3 ?: atleast(3,c123)),
	write('+++ role completion test 3 succeeded'),
	nl,
	!.
rocotest(4) :-
	backinit,
	backstate(verbosity = warning),
	c:<anything,
	r1:<range(anything),
	r2:<range(anything),
	r3:<range(number),
	c123:=r1.r2.r3,
	backtell(o1::c and r2:(o2 :: c)),
	backtell(o0::c and r1:o1),
	backtell(o2::c and r3:17),

	backask(o0 ?: c123 : 17),
	write('+++ role completion test 4 succeeded'),
	nl,
	!.
rocotest(N):-
	write('--- role completion test '),
	write(N),
	write(' failed'),
	nl, assert(test(role_completion_test(N), failed)).

arevtest(1) :-
	rocotest(1),
	backtell(redescribe(o0 :: c1)),
	backtell(redescribe(o3 :: c1 and c2 and r1:o0)),
	\+ backask(o1 ?: r1range:o0),
	backask(o1 ?: r1domain:o0),
	backask(o3 ?: r1domain:o0),
	\+ backask(o3 ?: r12:o0),
	write('+++ role abox revision test 1 succeeded'),
	nl,
	!.
arevtest(2) :-
	rocotest(2),
	backtell(redescribe(o2 :: c and ir1 : o04)),

	backretrieve(getall(inv(tr1) : o2)),
	backretrieve(getall(tr1 : o2)),
	backask(o1 ?: atleast(3,tr1)),
	\+ backask(o2 ?: atleast(1,tr1)),
	backask(o2 ?: atleast(1,ir1)),
	\+backask(o2 ?: atleast(2,ir1)),
	write('+++ role abox revision test 2 succeeded'),
	nl,
	!.
arevtest(4) :-
	rocotest(4),
	backtell(redescribe(o2::c and r3:15)),
	\+ backask(o0 ?: c123 : 17),
	backask(o0 ?: c123 : 15),
	write('+++ role abox revision test 4 succeeded'),
	nl,
	!.
arevtest(N):-
	write('--- role abox revision test '),
	write(N),
	write(' failed'),
	assert(test(role_abox_revision_test(N), failed)),
	nl.



