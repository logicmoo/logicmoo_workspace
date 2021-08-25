retrievaltest('tests/retrievaltest.pl','retrieval test',54).

retrievaltest :-
	retrievaltest(FN,TN,Max),
	start_msg(FN),
	retrievaltestloop(Max,1,TN),
	res_msg(TN),
	end_msg(FN).


retrievaltestloop(0,_,_) :- !.
retrievaltestloop(Rest,N,TN) :-
	(retrievalaction(N),!;true),
	(retrievaltest(N) ->
	    succ_msg(N,TN)
	;
	fail_msg(N,TN)),
	M is N+1,
	RestMinus1 is Rest-1,
	retrievaltestloop(RestMinus1,M,TN).


retrievalaction(1) :-
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

retrievalaction(7) :-
	write('*** Testing [self] for simple terms.'),nl,!.


retrievalaction(12) :-
	write('*** Testing [self] for unambiguous instances.'),nl,!.

retrievalaction(25) :-
	write('*** More tests using getall-expressions.'),nl,!.

retrievalaction(36) :-
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
	q :: ra: q and rs: q.

retrievalaction(42) :-
	write('*** Testing introduced_as, defined_as and self'),nl,
	write('*** i.e. retrieval functions operating at name level.'),nl.


retrievaltest(1) :-
	backretrieve(X = [self] for conc1),
	X == [[conc1]].

retrievaltest(2) :-
	backretrieve(X = [self] for r),
	X == [[r]].

retrievaltest(3) :-
	backretrieve(X = [self] for aset1),
	X == [[aset1]].

retrievaltest(4) :-
	backretrieve(X = [self] for number1),
	X == [[number1]].

retrievaltest(5) :-
	backretrieve(X = [self] for string),
	X == [[string]].

retrievaltest(6) :-
	backretrieve(X = [self] for [conc1,r,aset1,number1,string]),
	X == [[conc1],[r],[aset1],[number1],[string]].


% RETRIEVAL OF SIMPLE TERMS

retrievaltest(7) :-
	backretrieve(X = [self] for atleast(1,r) and atmost(2,r) and all(r,conc1) and oneof([a,b,c,d,e])),
	X = [[conc(_)]].

retrievaltest(8) :-
	backretrieve(X = [self] for (r1.r2) and inv(r3) and trans(r4)),
	X = [[role(_)]].

retrievaltest(9) :-
	backretrieve(X = [self] for aset([a,b,c,d]) intersection aset([a,b,c,d,e]) union aset([a,c,e]) without aset([a,d])),
	X = [[aset1]].

retrievaltest(10) :-
	backretrieve(X = [self] for gt(5) intersection 10 .. 20 intersection lt(25)),
	X = [[number(_)]].

retrievaltest(11) :-
	backretrieve(X = [self] for [atleast(1,r) and atmost(2,r) and all(r,conc1)
                           and oneof([a,b,c,d,e]),
			  (r1.r2) and inv(r3) and trans(r4),
			  aset([a,b,c,d]) intersection aset([a,b,c,d,e])
                          union aset([a,c,e]) without aset([a,d]),
			  10 .. 15 intersection ge(5) intersection lt(25)]),
	X = [[conc(_)],[role(_)],[aset1],[number(_)]].


% RETRIEVAL OF UNAMBIGUOUS INSTANCES

retrievaltest(12) :-
	backretrieve(X = [self] for o),
	X == [[o]].

retrievaltest(13) :-
	backretrieve(X = [self] for uc_54),
	X == [[o]].

retrievaltest(14) :-
	backretrieve(X = [self] for uc(54)),
	X == [[o]].
	
retrievaltest(15) :-
	backretrieve(X = [self] for theknown(d)),
	X == [[o]].

retrievaltest(16) :-
	backretrieve(X = [self] for a/obj),
	X == [[a]].

retrievaltest(17) :-
	backretrieve(X = [self] for [a,b,c,d,e]/obj),
	X == [[a],[b],[c],[d],[e]].

retrievaltest(18) :-
	backretrieve(X = [self] for getall(conc1)),
	X == [[a]].

retrievaltest(19) :-
	backretrieve(X = [self] for 42),
	X == [[42]].

retrievaltest(20) :-
	backretrieve(X = [self] for a/aset^obj),
	X == [[a]].

retrievaltest(21) :-
	backretrieve(X = [self] for [a,b,c,d,e]/aset^obj),
	X == [[a],[b],[c],[d],[e]].	

retrievaltest(22) :-
	backretrieve(X = [self] for getall(aset1)),
	X == [[e],[d],[c],[b],[a]].

retrievaltest(23) :-
	backretrieve(X = [self] for test/string^obj),
	X == [[test]].

retrievaltest(24) :-
	backretrieve(X = [self] for getall(string)),
	X == [[test]].


% TESTS ON GETALL	

retrievaltest(25) :-
	backretrieve(X = [self] for  getall(d)),
	X == [[o]].

retrievaltest(26) :-
	backretrieve(X = [self] for getall(conc1 or d)),
	X == [[a],[o]].

retrievaltest(27) :-
	backretrieve(X = [self] for  getall(d and r:(a or b))),
	X == [[o]].

retrievaltest(28) :-
	backretrieve(X = [self] for  getall(d and r:(someknown(conc1)))),
	X == [[o]].

retrievaltest(29) :-
	backretrieve(X = [self] for  getall(r)),
	X = [wrong_argument(_)].

retrievaltest(30) :-
	backretrieve(X = [self] for  getall(newasetdom)),
	X == [[e],[d],[c],[b],[a]].

retrievaltest(31) :-
	backretrieve(X = [self] for  getall(number1)),
	X = [wrong_argument(_)].

retrievaltest(32) :-
	backretrieve(X = [self] for  getall(atleast(1,r) and atmost(1,r) and all(r,conc1) and oneof([a,b,c,d,e,o]))),
	X == [[o]].

retrievaltest(33) :-
	backretrieve(X = [self] for  getall((r1.r2) and inv(r3) and trans(r4))),
	X = [wrong_argument(_)].

retrievaltest(34) :-
	backretrieve(X = [self] for  getall(aset([a,b,c,d]) intersection aset([a,b,c,d,e]) union aset([a,c,e]) without aset([a,d]))),
	X == [[e],[d],[c],[b],[a]].

retrievaltest(35) :-
	backretrieve(X = [self] for  getall(gt(5) intersection 10 .. 20 intersection lt(25))),
	X = [wrong_argument(_)].


retrievaltest(36) :-
	backretrieve(X = o),
	X == [wrong_argument(ambigue(o))].

retrievaltest(37) :-
	backretrieve(X = o/obj),
	X == [[o]].

retrievaltest(38) :-
	backretrieve(X = [o/conc,o/obj,o/aset^obj,o/string^obj]),
	X == [[o],[o],[o],[o]].

retrievaltest(39) :-
	backretrieve(X = [o,p,q]/obj),
	X == [[o],[p],[q]].

retrievaltest(40) :-
	backretrieve(X = [o/conc,o/aset^obj]/conc),
	X = [[o],wrong_argument(mismatch(aset^obj,conc))].

retrievaltest(41) :-
	backretrieve(X = [o/aset^obj,o/junk]),
	X = [[o],wrong_argument(wrong_da(junk,o))].


retrievaltest(42) :-
	backretrieve(X = [c1,c2]),
	X == [[c1],[c2]].	

retrievaltest(43) :-
	backretrieve(X = conc(22)),
	X == [[c]].

retrievaltest(44) :-
	backretrieve(X = all(r,c) and atmost(1,r)),
	X == [[c1]].

retrievaltest(45) :-
	backretrieve(X = 17),
	X == [[17]].

retrievaltest(54) :-
	backretrieve(X = uc_30),
	X == [[o]].

retrievaltest(46) :-
	backretrieve(X = uc(30)),
	X == [[o]].

retrievaltest(47) :-
	backretrieve(X = o/string^obj),
	X == [[o]].
	

retrievaltest(48) :-
	backretrieve(X = [introduced_as] for o),
	X == [[[conc]-[string,aset,conc]]].

retrievaltest(49) :-
	backretrieve(X = [introduced_as] for [o,p,q]),
	X == [[[conc]-[string,aset,conc]],[[conc]-[string,aset,conc]],[[conc]-[string,aset,conc]]].

retrievaltest(50) :-
	backretrieve(X = [introduced_as] for [c1,c2]),
	X == [[[conc]-[]],[[conc]-[]]].


retrievaltest(51) :-
	backretrieve(X = [defined_as] for o/conc),
	X == [[o:<anything]].

retrievaltest(52) :-
	backretrieve(X = [defined_as] for o/obj),
	X == [[o::ra:o and rs:o]].

retrievaltest(53) :-
	backretrieve(X = [defined_as] for c1),
	X == [[c1:=all(r,c) and atmost(1,r)]].


