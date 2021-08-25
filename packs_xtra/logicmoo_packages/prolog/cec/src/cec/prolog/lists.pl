/*
 *	file:		lists.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicated related to lists.
 *
 *	history:
 *	891010	js	Added this comment
 *	891106	uh	Moved the definition of
 *			sort_list/3 		divide/5
 *			iter/5			prefix/2
 *			from compute/antecedent3.pl into this file
 *	891117	uh	Added definition of 	maxOfList
 *	900808	js	Added definition of	tupleAsList/2
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

conc(L1,L2,L3):-
	append(L1,L2,L3).


atend(L,A,LA):-
	append(L,[A],LA),!.

last(LX,X):-
	append(_,[X],LX),!.

reverse(L,R):-
	reverse(L,[],R).

reverse([X|Xs],RYs,Zs):-
	reverse(Xs,[X|RYs],Zs).
reverse([],Xs,Xs).



change([_|T],0,V,[V|T]):-!.
change(L,I,_,L):-
	I < 1,
	write('change: negative Index !
'),!.
change([E|T],I,V,[E|TV]):-
	II is I-1,
	change(T,II,V,TV),!.
change([],_,_,[]):-
	write('change: Index too large !
'),!.



select([E|_],0,E):-!.
select(L,I,_):-
	I < 0,
	write('select: negative Index !
'),!.
select([_|T],I,EI):-
	II is I-1,
	select(T,II,EI),!.
select([],_,_):-
	write('select: Index too large !
'),!.

deleteElem([_|R],0,R):-!.
deleteElem(_,I,_):-
	I < 0,
	write('deleteElem: negative Index !
'),
	trace,
	!.
deleteElem([F|T],I,[F|T1]):-
	II is I-1,
	deleteElem(T,II,T1),
	!.
deleteElem([],_,_):-
	write('deleteElem: Index too large !
'),
	trace,
	!.

% Partitionierung von Listen in linke Haelfte, Element, rechte Haelfte
'chooseElem'([F|T],[],F,T).

'chooseElem'([F|T],[F|L],S,R):-
	'chooseElem'(T,L,S,R).

% Anwendung bei member-Funktion
% member(L,E):-
	% 'chooseElem'(L,LT,E,RT).

memb(L,X):-member(X,L).  % built-in predicate in KA-CProlog

union([],S,S):-!.   % only to catch frequent cases
union(S,[],S):-!.   % ------------"---------------
union(S1,S2,S):-
	setof1(X,(member(X,S1);member(X,S2)),S),
	!.

diff(S,[],S):-!.
diff(S1,[X|S2],S):-
	deleteOne(S1,X,S11),
	diff(S11,S2,S),
	!.
diff(S1,[_|S2],S):-
	diff(S1,S2,S).





intersect([],_T,[]).
intersect([X|S],T,[X|U]):-
	append(L,[X|R],T),
	!,
	append(L,R,T1),
	intersect(S,T1,U).
intersect([_|S],T,U):-
	intersect(S,T,U).


min([],_):-
	error("min of empty list undefined",[],min/2),
	trace.
min([X],X).
min([X,Y|L],M):-
	min([Y|L],M1),
	(X>=M1 ->
		M=M1
	;	M=X
	),
	!.


/* position(L,E,P)
 * succeeds if L contains E at position P. The first position is 0.
 */

position([X|_Xs],X,0).
position([_X|Xs],Y,P):-
	position(Xs,Y,PM1),
	P is PM1+1.


% permutation(+List,?PermutedList)
% succeeds once for every permutation of List matching PermutedList

permutation([],[]).
permutation(L,[X|Xs]):-
	deleteOne(L,X,LMX),
	permutation(LMX,Xs).


% deleteOne(+List,?Element,?Rest)
% succeeds once for every Element of List and Rest containing all other
% elements of list

deleteOne([X|Xs],X,Xs).
deleteOne([Y|Xs],X,[Y|Ys]):-
	deleteOne(Xs,X,Ys).


% applyPermutation(+List,+Permutation,-PermutedList)
% reorders List according to Permutation. Permutation is given as a
% list of numbers, not necessarily consecutive. The reordering
% is achieved by sorting the numbers and the list in parallel.
% Example: applyPermutation([a,b,c],[3,1,2],[b,c,a])

applyPermutation(L,P,LP):-
	pairing(P,L,L1),
	sort(L1,L2),
	pairing(_,LP,L2),
	!.



orOnLists(P,[X|Xs],OtherArgs):-
	(	apply((P,OtherArgs),[X])
	;	orOnLists(P,Xs,OtherArgs)).


/* rReduce(+Op,+Zero,+List,-Result)
 * Op:   predicate or lambda expression of arity 3
 * Zero: value for empty list
 * calculates E1 Op (E2 Op (..(En Op Zero)..)) for List=[E1,E2,..,En]
 */

:-dynamic rReduce/4.
rReduce(_,Zero,[],Zero).
rReduce(Op,Zero,[X|Xs],V):-
	rReduce(Op,Zero,Xs,VXs),
	apply(Op,[X,VXs,V]).

flatten(LL,L):-
	rReduce(append,[],LL,L),
	!.

mult([],[],[]).
mult([F|R],[G|S],[(F,G)|RS]):-
	mult(R,S,RS).


listToTuple(L,T):-
	var(L),
	var(T),
	L=[T].

listToTuple(L,(D1,D2)):-
	var(L),
	listToTuple(L1,D1),
	listToTuple(L2,D2),
	append(L1,L2,L).

listToTuple(L,D):-
	var(L),
	(D=true ->
		L=[]
	;	L=[D]
	).

listToTuple([F],F).
listToTuple([F|L],(F,D2)):-
	listToTuple(L,D2).
listToTuple([],true).


multListWithElem([],E,[]):-!.
multListWithElem([F|L],E,[(F,E)|LE]):-
	multListWithElem(L,E,LE),
	!.


multElemWithList([],E,[]):-!.
multElemWithList([F|L],E,[(E,F)|LE]):-
	multElemWithList(L,E,LE),
	!.


% pairing(?X,?Y,?XY)
% succeeds if X,Y and XY are of equal size and if XY contains
% a list of pairs each containing one element of X and one of Y.
% Can be used to construct or take apart the list of pairs.

pairing(L1,L2,L3):-
	pairing(L1,L2,',',L3),
	!.

pairing([],[],_F,[]).
pairing([X|Xs],[Y|Ys],F,[P|Zs]):-
	P=..[F,X,Y],
	pairing(Xs,Ys,F,Zs),!.

genVarList(0,[]):-
	!.
genVarList(N,[_|L]):-
	M is N-1,
	genVarList(M,L),
	!.

genList(0,_,[]):-
	!.
genList(N,E,[E|L]):-
	M is N-1,
	genList(M,E,L),
	!.
	
deleteBegin(0,L,L):-!.
deleteBegin(N,[F|T],TD):-
	M is N-1,
	deleteBegin(M,T,TD),
	!.
deleteBegin(_,X,X).

concAtomNames(A,B,AB):-
	name(A,NA),
	name(B,NB),
	append(NA,NB,NAB),
	name(AB,NAB).

concAtomList([N|RL],NR):-
	concAtomList(RL,R),
	concAtomNames(N,R,NR).
concAtomList([],'').

asList(L,L):-
	list(L),
	!.
asList(X,[X]).


/* tupleAsList(+T,-L)
 */
tupleAsList(T,[E]) :-
	var(T),
	!,
	T=E.
tupleAsList((A,B),[A|L]) :-
	!,
	tupleAsList(B,L).
tupleAsList(A,[A]).


/* listOfNumbers(+N,-Ns)
 * creates the list Ns of numbers from 1 to N
 */
listOfNumbers(N,Ns):-
	listOfNumbers(N,1,Ns),
	!.

listOfNumbers(0,_,[]):-
	!.
listOfNumbers(N,From,[From|Ns]):-
	N1 is N-1,
	From1 is From+1,
	listOfNumbers(N1,From1,Ns),
	!.

/* numberedList(+List,-NumberedList)
 * creates a list of pairs. The first component of the pair is
 * the number of the element in the list, the second is the element
 * itself. Numbering starts at 1.
 */

numberedList(L,LN):-
	length(L,N),
	listOfNumbers(N,Ns),
	pairing(Ns,L,LN),
	!.
asSet([],[]).
asSet([X|Xs],Ys):-
	member(X,Xs),
	!,
	asSet(Xs,Ys).
asSet([X|Xs],[X|Ys]):-
	asSet(Xs,Ys).


list(X):-
	var(X),
	!,
	fail.
list([]).
list([_|B]):-
	list(B).


/* subSequence(S,L)
 * succeeds if S is a subsequence of L, i.e. the elements
 * of S appear in L in the same order as in S, but
 * not necessarily consecutive.
 * can be used to define member and twoMembers:
member(E,L) :-
	subSequence([E],L).
twoMembers(E1,E2,L) :-
	subSequence([E1,E2],L).

subSequence([],[]).
subSequence([E|S],[E|L]) :-
	subSequence(S,L).
subSequence(S,[_|L]) :-
	subSequence(S,L).
 */

/* twoMembers(E1,E2,L)
 * succeeds if E1 appears before E2 in L
 */

twoMembers(E1,E2,[E1|L]) :-
	member(E2,L).
twoMembers(E1,E2,[_E|L]) :-
	twoMembers(E1,E2,L).

/* maxOfList(+L,-E,-N)
 * finds the maximal element E and its position N in list L
 */

maxOfList(L,E,N) :-
	maxOfList(L,1,E,N).

maxOfList([E],I,E,I) :-
	!.
maxOfList([E|L],I,Val,Pos) :-
	I1 is I+1,
	maxOfList(L,I1,E2,I2),
	( E2 > E ->
	    Val=E2,
	    Pos=I2
	;
	    Val=E,
	    Pos=I
	).

/*----------------------------------------------------------------------*/
/* prefix(?Occ1,?Occ2)                                                  */
/* checks if Occ1 is a prefix of Occ2                                   */
/* 08.12.88 no changes */

prefix([],_Occ).
prefix([N1|Occ1Rest],[N1|Occ2Rest]) :-
   prefix(Occ1Rest,Occ2Rest).

/*----------------------------------------------------------------------*/
/* sort_list(+Ordering,+List,-SortedList)				*/
/* computes the sorted list SortedList from List using the partial 	*/
/* ordering Ordering, which must be defined by some clauses.		*/

sort_list(_Rel,[],[]) :- !.
sort_list(_Rel,[E1],[E1]) :- !.
sort_list(Rel,[E1|EList1],SL) :-
   divide(Rel,E1,EList1,EL1,EL2),
   sort_list(Rel,EL1,SEL1),
   sort_list(Rel,EL2,SEL2),
   append(SEL1,[E1|SEL2],SL).

divide(_Rel,_E1,[],[],[]) :- !.
divide(Rel,E1,[E2|EList2],[E2|SEL1],SEL2) :-
   G=..[Rel,E2,E1], G, !,
   divide(Rel,E1,EList2,SEL1,SEL2).
divide(Rel,E1,[E2|EList2],SEL1,[E2|SEL2]) :-
   divide(Rel,E1,EList2,SEL1,SEL2), !.

