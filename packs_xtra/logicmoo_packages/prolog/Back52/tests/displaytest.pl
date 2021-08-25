displaytest('tests/displaytest.pl','display test',53).

displaytest :-
	displaytest(FN,TN,Max),
	start_msg(FN),
	displaytestloop(Max,1,TN),
	res_msg(TN),
	end_msg(FN).


displaytestloop(0,_,_) :- !.
displaytestloop(Rest,N,TN) :-
	(displayaction(N),!;true),
	(displaytest(N) ->
	    succ_msg(N,TN)
	;
	fail_msg(N,TN)),
	M is N+1,
	RestMinus1 is Rest-1,
	displaytestloop(RestMinus1,M,TN).


displayaction(1) :-
	write('*** Loading test data base #1.'),nl,
	backinit,
	conc1 :< oneof([a,b,c,d,e]),
	r :< domain(anything) and range(conc1),
	r1 :< domain(anything) and range(anything),
	r2 :< domain(anything) and range(anything),
	r3 :< domain(anything) and range(anything),
	r4 :< domain(anything) and range(anything),
	rs :< range(string),
	d :< rs:test,
	aset1 := aset([a,b,c,d,e]),
	newasetdom := attribute_domain,
	aset2 := aset([a,b,c,d,e],newasetdom),
	number1 := gt(0),
	o :: d and r:close(a),
	write('*** Testing [self] for unambiguous classes.'),nl,!.

displayaction(7) :-
	write('*** Testing [self] for simple terms.'),nl,!.


displayaction(12) :-
	write('*** Testing [self] for unambiguous instances.'),nl,!.

displayaction(25) :-
	write('*** More tests using getall-expressions.'),nl,!.

displayaction(36) :-
	write('*** Loading test data base #2.'),nl,
	backinit,
	o :< anything,
	p :< anything,
	q :< anything,
	c1 := all(r,c) and atmost(1,r),
	c2 := all(r,c) and atmost(1,r),
	ra :< range(aset),
	rs :< range(string),
	o :: ra: o and rs: o,
	p :: ra: p and rs: p,
	q :: ra: q and rs: q,
	write('*** Testing disambiguation of arguments'),nl.

displayaction(42) :-
	write('*** Testing introduced_as, defined_as and self'),nl,
	write('*** i.e. retrieval functions operating at name level.'),nl.


displaytest(1) :-
	backretrieve([self] for conc1).

displaytest(2) :-
	backretrieve([self] for r).

displaytest(3) :-
	backretrieve([self] for aset1).

displaytest(4) :-
	backretrieve([self] for number1).

displaytest(5) :-
	backretrieve([self] for string).

displaytest(6) :-
	backretrieve([self] for [conc1,r,aset1,number1,string]).


% RETRIEVAL OF SIMPLE TERMS

displaytest(7) :-
	backretrieve([self] for atleast(1,r) and atmost(2,r) and all(r,conc1) and oneof([a,b,c,d,e])).

displaytest(8) :-
	backretrieve([self] for (r1.r2) and inv(r3) and trans(r4)).

displaytest(9) :-
	backretrieve([self] for aset([a,b,c,d]) intersection aset([a,b,c,d,e]) union aset([a,c,e]) without aset([a,d])).

displaytest(10) :-
	backretrieve([self] for gt(5) intersection 10 .. 20 intersection lt(25)).

displaytest(11) :-
	backretrieve([self] for [atleast(1,r) and atmost(2,r) and all(r,conc1)
                           and oneof([a,b,c,d,e]),
			  (r1.r2) and inv(r3) and trans(r4),
			  aset([a,b,c,d]) intersection aset([a,b,c,d,e])
                          union aset([a,c,e]) without aset([a,d]),
			  10 .. 15 intersection ge(5) intersection lt(25)]).


% RETRIEVAL OF UNAMBIGUOUS INSTANCES

displaytest(12) :-
	backretrieve([self] for o).

displaytest(13) :-
	backretrieve([self] for uc_53).

displaytest(14) :-
	backretrieve([self] for uc(53)).
	
displaytest(15) :-
	backretrieve([self] for theknown(d)).

displaytest(16) :-
	backretrieve([self] for a/obj).

displaytest(17) :-
	backretrieve([self] for [a,b,c,d,e]/obj).

displaytest(18) :-
	backretrieve([self] for getall(conc1)).

displaytest(19) :-
	backretrieve([self] for 42).

displaytest(20) :-
	backretrieve([self] for a/aset^obj).

displaytest(21) :-
	backretrieve([self] for [a,b,c,d,e]/aset^obj).

displaytest(22) :-
	backretrieve([self] for getall(aset1)).

displaytest(23) :-
	backretrieve([self] for test/string^obj).

displaytest(24) :-
	backretrieve([self] for getall(string)).


% TESTS ON GETALL	

displaytest(25) :-
	backretrieve([self] for  getall(d)).

displaytest(26) :-
	backretrieve([self] for getall(conc1 or d)).

displaytest(27) :-
	backretrieve([self] for  getall(d and r:(a or b))).

displaytest(28) :-
	backretrieve([self] for  getall(d and r:(someknown(conc1)))).

displaytest(29) :-
	backretrieve([self] for  getall(r)).

displaytest(30) :-
	backretrieve([self] for  getall(newasetdom)).

displaytest(31) :-
	backretrieve([self] for  getall(number1)).

displaytest(32) :-
	backretrieve([self] for  getall(atleast(1,r) and atmost(1,r) and all(r,conc1) and oneof([a,b,c,d,e,o]))).

displaytest(33) :-
	backretrieve([self] for  getall((r1.r2) and inv(r3) and trans(r4))).

displaytest(34) :-
	backretrieve([self] for  getall(aset([a,b,c,d]) intersection aset([a,b,c,d,e]) union aset([a,c,e]) without aset([a,d]))).

displaytest(35) :-
	backretrieve([self] for  getall(gt(5) intersection 10 .. 20 intersection lt(25))).


displaytest(36) :-
	backretrieve(o).

displaytest(37) :-
	backretrieve(o/obj).

displaytest(38) :-
	backretrieve([o/conc,o/obj,o/aset^obj,o/string^obj]).

displaytest(39) :-
	backretrieve([o,p,q]/obj).

displaytest(40) :-
	backretrieve([o/conc,o/aset^obj]/conc).

displaytest(41) :-
	backretrieve([o/aset^obj,o/junk]).


displaytest(42) :-
	backretrieve([c1,c2]).

displaytest(43) :-
	backretrieve(conc(22)).

displaytest(44) :-
	backretrieve(all(r,c) and atmost(1,r)).

displaytest(45) :-
	backretrieve(17).

displaytest(46) :-
	backretrieve(uc_29).

displaytest(47) :-
	backretrieve(o/string^obj).
	

displaytest(48) :-
	backretrieve([introduced_as] for o).

displaytest(49) :-
	backretrieve([introduced_as] for [o,p,q]).

displaytest(50) :-
	backretrieve([introduced_as] for [c1,c2]).


displaytest(51) :-
	backretrieve([defined_as] for o/conc).

displaytest(52) :-
	backretrieve([defined_as] for o/obj).

displaytest(53) :-
	backretrieve([defined_as] for c1).

