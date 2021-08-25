/**********************************************************************
 *
 * @(#) testMotel.pl 1.4@(#)
 *
 */

testMotel :-
	testAllMotelExamples(1),
	!.

testMotel(N) :-
	testAllMotelExamples(N),
	!.

testAllMotelExamples(61) :-
	print('Test complete'), nl, nl,
	!.
testAllMotelExamples(N) :-
	initialize,
	print('Example '), print(N), nl, example(N),
	once(testMotelExample(N)),
	M is N + 1,
	testAllMotelExamples(M).

testMotelExample(1) :-	
	print('No goal for this example'), nl.
testMotelExample(2) :-
	printTime(setof(C,E^deduce(ex2,[],elementOf(mary,C),E),L1)), print(L1), nl,
	printTime(setof(D,F^deduce(ex2,[],elementOf(tom,D),F),L2)), print(L2), nl.
testMotelExample(3) :-
	tryGoal(inconsistent(ex3)).
testMotelExample(4) :-
	print('No goal for this example'), nl.
testMotelExample(5) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(6) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(7) :-
	print('No goal for this example'), nl.
testMotelExample(8) :-
	tryGoal(deduce(elementOf(tom,heterosexual))).
testMotelExample(9) :-
	tryGoal(deduce(elementOf(chris,male))).
testMotelExample(10) :-
	tryGoal(deduce(elementOf(tom,c2))).
testMotelExample(11) :-
	tryGoal(inconsistent(ex11)).
testMotelExample(12) :-
	tryGoal(subsumes([],c1,c2)),
	tryGoal(not(subsumes([],c2,c1))).
testMotelExample(13) :-
	tryGoal(subsumes([],c1,c2)).
testMotelExample(14) :-
%	initialize, print('Example 14'), nl, example(14),
%	tryGoal(subsumes([],c2,c1)),
	!.
testMotelExample(15) :-
	tryGoal(subsumes([],c2,c1)).
testMotelExample(16) :-
	tryGoal(subsumes([],c2,c1)).
testMotelExample(17) :-
	tryGoal(subsumes([],c2,c1)).
testMotelExample(18) :-
	tryGoal(deduce(elementOf(mary,c4))).
testMotelExample(19) :-
	tryGoal(deduce(elementOf(amy,female))).
testMotelExample(20) :-
	tryGoal(inconsistent(ex20)).
testMotelExample(21) :-
	print('No goal for this example'), nl,
% 	deduce(elementOf(betty,female)),
	!.
testMotelExample(22) :-
% 	deduce(elementOf(amy,female)),
	print('No goal for this example'), nl.
testMotelExample(23) :-
% 	deduce(elementOf(amy,female))
	print('No goal for this example'), nl.
testMotelExample(24) :-
	tryGoal(deduce(elementOf(audi,c3))).
testMotelExample(25) :-
	tryGoal(not(deduce(elementOf(audi,c3)))).
testMotelExample(26) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(27) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(28) :-
	tryGoal(deduce(ex29,[b(believe,john)],elementOf(audi,auto),_P)).
testMotelExample(29) :-
	print('No goal for this example'), nl.
testMotelExample(30) :-
	print('No goal for this example'), nl.
testMotelExample(31) :-
	tryGoal(deduce(elementOf(tom,onlyMaleChildren))).
testMotelExample(32) :-
	tryGoal(abduce(_H1,elementOf(robin,male),_E1)),
	tryGoal(abduce(_H2,elementOf(robin,female),_E2)).
testMotelExample(33) :-
	tryGoal(abduce(_H3,elementOf(nixon,dove),_E3)),
	tryGoal(abduce(_H4,elementOf(nixon,hawk),_E4)).
testMotelExample(34) :-
	tryGoal(inconsistent(ex34)).
testMotelExample(35) :-
	tryGoal(abduce(ex35,[],_H5,elementOf(john,fly),_E5)),
	tryGoal(not(abduce(ex35,[],_H8,elementOf(tweety,fly),_E8))).
testMotelExample(36) :-
	tryGoal(abduce(ex36,[],_H6,elementOf(nixon,dove),_E6)),
	tryGoal(abduce(ex36,[],_H7,elementOf(nixon,hawk),_E7)).
testMotelExample(37) :-
	print('No goal for this example'), nl.
testMotelExample(38) :-
	tryGoal(deduce(elementOf(ideaste,c2))).
testMotelExample(39) :-
	tryGoal(deduce(elementOf(lucky,female))),
	tryGoal(assert_ind(lucky,male)),
	tryGoal(not(deduce(elementOf(lucky,female)))),
	tryGoal(consistent([])).
testMotelExample(40) :-
	tryGoal(deduce(elementOf(peter,richPerson))),
	tryGoal(assert_ind(peter,poorPerson)),
	tryGoal(not(deduce(elementOf(peter,richPerson)))),
	tryGoal(consistent([])),
	tryGoal(not(subsumes(richPerson,doctor))).
testMotelExample(41) :-
	tryGoal(deduce(elementOf(tom,richPerson))),
	tryGoal(assert_ind(tom,poorPerson)),
	tryGoal(not(deduce(elementOf(tom,richPerson)))),
	tryGoal(consistent([])).
testMotelExample(42) :-
	tryGoal(deduce(elementOf(audi,fourWheels))),
	tryGoal(assert_ind(audi,fiveWheels)),
	tryGoal(not(deduce(elementOf(audi,fourWheels)))),
	tryGoal(consistent([])).
testMotelExample(43) :-
	tryGoal(deduce(elementOf(r,red))),
	tryGoal(deduce(elementOf(r,redOrYellow))),
	tryGoal(deduce(elementOf(r,colors))).
testMotelExample(44) :-
	tryGoal(subsumes(c2,c12)).
testMotelExample(45) :-
	print('No goal for this example'), nl.
testMotelExample(46) :-
	print('No goal for this example'), nl.
testMotelExample(47) :-
	tryGoal(deduce(elementOf(bmw,c3))).
testMotelExample(48) :-
	print('No goal for this example'), nl.
testMotelExample(49) :-
	tryGoal(not(deduce(elementOf(p,c4)))).
testMotelExample(50) :-
	tryGoal(deduce(elementOf(peter,c0))).

testMotelExample(51) :-
	tryGoal(deduce(posInfl(a,d))),
	tryGoal(deduce(posInfl(b,d))),
	tryGoal(bagof(Y1,deduce(posInfl(a,Y1)),Y1s)),
	verifySolution(Y1s,[b,c,d,g]),
	tryGoal(deduce(infl(a,d,1.0))),
	tryGoal(bagof((X1,W1),deduce(infl(X1,e,W1)),X1W1Pairs)),
	verifySolution(X1W1Pairs,[(a,0.0),(b,-1.0),(g,1.0)]),
	tryGoal(deduce(simultInfl([a,h],d,2.0))),
	tryGoal(deduce(change(d,1.0))),
	tryGoal(bagof(X2,deduce(increase(X2)),X2s)),
	verifySolution(X2s,[b,c,d,g,a]).

testMotelExample(52) :-
	tryGoal(deduce(negInfl(withRebate,hasOverallCost))),
	tryGoal(deduce(posInfl(hasListPrice,hasOverallCost))),
	tryGoal(deduce(posInfl(hasCubicCapacity,hasPrice))),
	tryGoal(deduce(posInfl(hasCubicCapacity,hasOverallCost))),
	tryGoal(deduce(posInfl(hasCatConverter,hasOverallCost))),
	tryGoal(deduce(simultInfl([hasCubicCapacity,hasCatConverter],hasOverallCost,3.0))),
	tryGoal(deduce(simultInfl([hasCubicCapacity,hasCatConverter],hasMaxSpeed,-1.0))),
	tryGoal(deduce(leastInfl(hasCubicCapacity,hasMaxSpeed))),
	tryGoal(deduce(leastInfls([hasCatConverter,hasCubicCapacity],hasMaxSpeed))),
	tryGoal(deduce(maxPosInfl(hasCubicCapacity,hasOverallCost,2.0))),
	tryGoal(bagof((X1,W1),deduce(maxNegInfl(X1,hasMaxSpeed,W1)),X1W1Pairs)),
	verifySolution(X1W1Pairs,[(hasCatConverter,-1.0),(hasWeight,-1.0)]),
	tryGoal(bagof(X2,deduce(increase(X2)),X2s)),
	verifySolution(X2s,[hasFuelConsumption,hasListPrice,hasOverallCost,hasPrice,hasWeight,hasCubicCapacity]),
	tryGoal(bagof((X3,W3),(deduce(leastInfl(X3,hasMaxSpeed)),abduce(change(X3,W3),change(hasMaxSpeed,1.0))),X3W3s)),
	verifySolution(X3W3s,[(hasCatConverter,-1.0)]).
testMotelExample(53) :-
	print('No goal for this example'), nl.
testMotelExample(54) :-
	print('No goal for this example'), nl.
testMotelExample(55) :-
	print('No goal for this example'), nl.
testMotelExample(56) :-
	print('No goal for this example'), nl.
testMotelExample(57) :-
	print('No goal for this example'), nl.
testMotelExample(58) :-
	print('No goal for this example'), nl.
testMotelExample(59) :-
	tryGoal(sb_ask(isa(harry,parent))),
	tryGoal(sb_ask(isa(harry,person))),
	printTime(setof((X,Y),sb_ask(role(child,X,Y)),L1)), print(L1), nl,
	printTime(setof(X,sb_ask(roleDef(child,X)),L2)), print(L2), nl,
	printTime(setof((X,Y),sb_ask(roleNr('marys-child',X,Y)),L3)), print(L3), nl,
	printTime(setof(X,sb_ask(roleDefNr('marys-child',X)),L4)), print(L4), nl.
testMotelExample(60) :-
	tryGoal(deduce(ex60,[b(believe,peter)],elementOf(tom,richPerson),E)),
	tryGoal(assert_ind([b(believe,peter)],tom,not(richPerson))),
	tryGoal(inconsistent([b(believe,peter)])).


tryGoal(G) :-
	call(G),
	!,
	print('Goal '), print(G), print(' succeeded'), nl.
tryGoal(G) :-
	print('Goal '), print(G), print(' failed'), nl.

/***********************************************************************
 *
 * verifySolution(+TestSol,+ExpectedSol)
 *
 *	prints an error message if TestSol and ExpectedSol do not
 *	match.
 */

verifySolution(TestSol,ExpectedSol) :-
	nonvar(ExpectedSol),
	nonvar(TestSol),
	!,
	TestSol = ExpectedSol.
verifySolution(TestSol,ExpectedSol) :-
	print('Solutions differ: test solution is '),
	print(TestSol),
	print(', while expected solution is '),
	print(ExpectedSol).

