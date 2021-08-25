/*
 *	file:		show.pl
 *	version:	1.0
 *	date:		October 30, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the definitions of the predicates
 *	show and showms.
 *
 *	history:
 *	891030	uh	Moved definitions of
 * 			showDBos/0   	    	showDBms/0  	
 *			showDB/0		printTupelsOS/1		
 *			printTupelsMS/1		printTuples/1
 *			hasOverloadedOp/1	relevantOrderSorted/1
 *			nonredundantOS/2
 *			from dbKit.pl into this file
 *	891103 	uh	Moved definitions of
 *			sig/0			show/0
 *			showms/0		show/1
 *			showms/1		show/2
 *			showCStatus/0		unfailingMode/0
 *			showNR/2		showosCStatus/0
 *			into this file
 *	891106 	uh	Moved definitions of
 *			typeRules/0		typeComputingRule/1
 *			orderSortedRules/0	orderSortedRule/1
 *			from pe.pl into this file
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


sig :-
	nl,
	write('Signature :'),
	nl,
	nl,
	clause('ofType'(Op,(O:Type)),true),
	print((Op ofType (O:Type))),
	nl,
	fail.
sig :- !.



show :-
	cont(os,true),
	!,
	showDBos,
	showosCStatus.
show :-
	showms.

showms :-
	showDBms,
	showCStatus.

show(S) :-
	show(S,os).

showms(S) :-
	show(S,ms).

show(S,SpecType) :-
	(world(S) ->
		ss('$$current'),
		rs(S),
		(SpecType == 'os' ->
			show
		;
			showms
		),
		ps('$$current')
	;
		nl,
		write(' *** There exists no specification of that name!'),
		!,
		fail
	).


showCStatus :-
	showCStatus(_).

showCStatus(Complete) :-
	setof1((I,T),Kind^mayBeReducible(I,T,Kind),Red),
	(Red = [] ->
		write('All axioms reduced.')
	;
		showNR('$equation',Red),
		showNR('$rule',Red),
		showNR('$nonoperational equation',Red)
	),
	nl,
	setof1('$maySuperpose'(Ch,I,A,Ty,J,L,T),'$maySuperpose'(Ch,I,A,Ty,J,L,T),S),
	(S = [] ->
		write('All superpositions computed.')
	;
		write('The following superpositions may be nontrivial:'),
		nl,
		print('$list'(S,'; ','','',9))
	),
	nl,
	(%Red = [],
	 S = [] ->
		('$equation'(_,_) ->	
			write('The set of equations is not empty.')
		;	(unfailingMode ->
				write('(Incomplete) unfailing completion mode: no inconsistency detected.')
			;	write('No more equations, the system is
complete.'),
			Complete=complete
			)
		),
		nl
	;
		true
	),
	!.


unfailingMode:-
	'$nonoperational equation'(I,([],E)),
	\+ permutative(E,_),
	\+ lemma(I,'$nonoperational equation',indScheme(_)).



showNR(T,Red) :-
	setof1(I,member((I,T),Red),RE),
	(RE = [] ->

		true
	;
		nl,
		sPrint("The following %s may be reducible: %",[T,RE])
	),
	!.


showosCStatus :-
	setof1((I,T),Kind^mayBeReducible(I,T,Kind),Red),
	(Red = [] ->
		write('All axioms reduced.')
	;
		write('Some axioms may be reducible.')
	),
	nl,
	setof1('$maySuperpose'(Ch,I,A,Ty,J,L,T),'$maySuperpose'(Ch,I,A,Ty,J,L,T),S),
	(S = [] ->
		write('All superpositions computed.')
	;
		write('Some superpositions may be nontrivial.')
	),
	nl,
	(%Red = [],
	 S = [] ->
		('$equation'(_,_) ->	
			write('The set of equations is not empty.')
		;
			write('No more equations, the system is complete.')
		),
		nl
	;
		true
	),
	!.

/*Already recorded*/
/*----------------------------------------------------------------------*
 *	     		      showDB(As)				*
 *----------------------------------------------------------------------*/

showDBos :-
	assign1(show,[]),
	relationTypes(Ts),
	mapP(lambda([T],printTuplesOS(T)),Ts),
	nl.

showDBms :-
	assign1(show,[]),
	relationTypes(Ts),
	mapP(lambda([T],printTuplesMS(T)),Ts).

showDB(As) :-
	assign1(show,As),
	relationTypes(Ts),
	mapP(lambda([T],printTuples(T)),Ts).

printTuplesOS(T):-
	setof1('$tupel'((I,O,[]),Ref,T),
	       Fact^(Fact=..[T,I,O],clause(Fact,true,Ref)),Tupels),
	manyToOrderSorted(Tupels,OrderSortedTupels1),
	manyToOrderSortedPR(Tupels,OrderSortedTupels2),
        pairing(OrderSortedTupels1,OrderSortedTupels2,OrderSortedTupels),
	toProlog(OrderSortedTupels2,[],OSTP,_),
	relevantOrderSorted(OrderSortedTupels,OSTP,OsTupels),
	mapF(lambda(['$tupel'((I,_,_),_,_),F],
		[O]^(F=..[T,I,O],F)),OsTupels,OsEqs),
	nl,
	write('Current order-sorted '),
	print(T),
	write('s'),
	nl,nl,
	mapP(lambda([T_],(print(T_),nl)),OsEqs),
	nl.

printTuplesMS(T):-
	setof1('$tupel'((I,O,[]),Ref,T),
		       Fact^(Fact=..[T,I,O],
			     clause(Fact,true,Ref)
			    ),SortedTupels),
	nl,
	write('Current '),
	print(T),
	write('s'),
	nl,nl,
	mapP(lambda([T_],
			(hasOverloadedOp(T_) ->
			    (showMS:==on,
			     print(T_), nl,
			     showMS:==off)
			    ;
			    print(T_),nl)),
	     SortedTupels),
	nl.

printTuples(T):-
	setof1('$tupel'((I,O,A),Ref,T),
	       Fact^(Fact=..[T,I,O],
		     clause(Fact,true,Ref),
		     setof1((N,V),(attribute(N,T),a(T,I,N,V)),A)
		    ),Tupels),
	sort(Tupels,SortedTupels),
	nl,
	write('Current '),
	print(T),
	write('s'),
	nl,nl,
	mapP(lambda([T_],
			(print(T_),nl)),
		     SortedTupels),
	nl.



hasOverloadedOp('$tupel'((_,Eq,_),_,_)) :-
	termInfo(Eq,(Ops,_)),
	member(Op,Ops),
	( 	'$inj' ofType (Op:_)
	;	O ofType (Op:_), 
		O\=='$ground',
		O ofType (Op1:_), 
		Op\==Op1),
	!.

relevantOrderSorted(Ts,TsP,OTs):- 
	setof1(E,SE^(member((E,SE),Ts),nonredundantOS(SE,TsP)),OTs).


nonredundantOS('$tupel'((I,_,_),_,'$nonoperational equation'),_):-
	kind(I,'$nonoperational equation',injectivityLaw),
	!,
	fail.
nonredundantOS('$tupel'((I,_,_),_,T),_):-
	origin(I,T,orderSortedTrans),
	!,
	fail.
nonredundantOS('$tupel'((_,(_,[X=X]),_),_,_),_):-
	!,
	fail.
nonredundantOS('$tupel'((_,(C,[L=R]),_),_,_),_):-
	(	member((L=R),C)
	;	member((R=L),C)
	),
	!,
	fail.
nonredundantOS('$tupel'((I,OS,_),_,T),TP):-
	member('$tupel'((J,OS,_),_,T),TP),
	I>J,
	!,
	fail.
nonredundantOS(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typeRules:-
	nl,
	setof(R,typeComputingRule(R),Rs),
	write('	'),
	writeList(Rs,'
	'),
	nl,
	!.


typeComputingRule('$rule'(C,[L=R])) :-
	'$rule'(_,(C,[L=R])),
	manyToOrderSorted(L,T),
	manyToOrderSorted(R,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

orderSortedRules:-
	nl,
	setof(R,orderSortedRule(R),Rs),
	write('	'),
	writeList(Rs,'
	'),
	nl,
	!.


orderSortedRule('$rule'(C,[L1=R1])) :-
	'$rule'(_,(C,[L=R])),
	manyToOrderSorted(L,L1),
	manyToOrderSorted(R,R1),
	L1\==R1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ac_ist_AC(O) :-
	ac_ist_A(O),
	ac_ist_C(O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dead code ?
opSet(Es,EOps) :-
	termInfo(Es,[Ops,_]),
	delete(Ops,'=',O1),
	delete(O1,'.',EOps).


showOrientation :-
	setof1('$equation'(I,(C,Eq)),I^C^Eq^'$equation'(I,(C,Eq)),Tupels),
	sort(Tupels,SortedTupels),
	map(getOrientation,SortedTupels,Orientations),
	pairing(SortedTupels,Orientations,OEqs),
	mapP(lambda([T_],
			(print(T_),nl)),
		     OEqs),
	nl.
	
		

getOrientation('$equation'(I,(C,[L = R])),Orientation) :-
	tps_current_ordering(kns_or_neqkns),
	(kns_GT(L,R,extToBy([],[],[])) ->
		(kns_GT(R,L,extToBy([],[],[])) ->
			% both direction possible
			Orientation = bothDirectionsNoExt
		;
			% only left to right possible
			Orientation = leftToRightNoExt

	  	)
	;
		kns_GT(R,L,extToBy([],[],[])),
		% only right to left possible
		Orientation = rightToLeftNoExt

	).
getOrientation('$equation'(I,(C,[L = R])),Orientation) :-
	tps_current_ordering(kns_or_neqkns),
	(kns_GT(L,R,extToBy([],_,_)) ->
		(kns_GT(R,L,extToBy([],_,_)) ->
			% both direction possible
			Orientation = bothDirectionsExt
		;
			% only left to right possible
			Orientation = leftToRightExt

	  	)
	;
		(kns_GT(R,L,extToBy([],_,_)) ->
			% only right to left possible
			Orientation = rightToLeftExt
		;
			% not orientable
			Orientation = notOrientable
		)

	).
getOrientation('$equation'(I,(C,[L = R])),Orientation) :-
	tps_current_ordering(poly(_N)),
	pol_getvars((L,R),[],VarList),
	pol_interpret_term1(L, VarList, LI),
	pol_interpret_term1(R, VarList, RI),
	(pol_tuplegreater(LI,RI) ->
		% only left to right possible
		Orientation = leftToRightNoExt
	;
		(pol_tuplegreater(RI,LI) ->
			% only right to left possible
			Orientation = rightToLeftNoExt
		;
			(LI = RI ->
				% both directions possible
				Orientation = bothDirectionsNoExt
			;
				Orientation = notOrientable
			)
		)
	).
getOrientation('$equation'(I,(C,[L = R])),Orientation) :-
	tps_current_ordering(poly(_N)),
	% there is no interpretation for L or R
	Orientation = incompleteInterpretation.


pol_orderingC(L,R) :-
	pol_getvars((L,R),[],VarList),
	pol_interpret_term1(L, VarList, LI),
	pol_interpret_term1(R, VarList, RI),
	pol_tuplegreater(LI, RI).

pol_interpret_term1(@X, VarList, Result) :-
	!,
	pol_transformvar(@X, VarList, ExpList),
	pol_get_tuplelength(N),
	pol_expand_to_list([(q(1,1), ExpList)], N, Result).
pol_interpret_term1(X, VarList, Result) :-
	functor(X, X, 0),
	!,
	pol_get_op_interpretation1(X, 0, Res1),
	pol_make_zero(VarList, ZeroList),
	pol_map_change_explists(Res1, ZeroList, Result).
pol_interpret_term1(T, VarList, Result) :-
	functor(T, _, N),
	T =.. [Functor|Arguments],
	!,
	pol_map_interpret_term1(Arguments, VarList, ArgInterp),
	pol_get_op_interpretation(Functor, N, FunctInterp),
	pol_map_papply(FunctInterp, ArgInterp, Result).

pol_map_interpret_term1([], _, []).
pol_map_interpret_term1([T1|TList], VarList, [R1|RList]) :-
	pol_interpret_term1(T1, VarList, R1),
	pol_map_interpret_term1(TList, VarList, RList).

pol_get_op_interpretation1(Functor, Arity, Interpretation) :-
	pol_get_current(pol_state(_N,IList,_C,_AC)),
	member(pol_op_interpretation(Functor, Arity, Interpretation),IList),
	!.
pol_get_op_interpretation1(Functor, Arity, Interpretation) :-
	pol_get_current(pol_state(_N,IList,_C,_AC)),
	TypeLength is Arity+1,
	(OF ofType (Functor:T)),
	length(T,TypeLength),
	member(pol_op_interpretation(OF, Arity, Interpretation),IList),
	!.
pol_get_op_interpretation1(F,K,I):-
	nRedComplOp(F),
	listOfNumbers(K,Indexes),
	map(lambda([J,M],[M1]^(varAsMonomial(K,J,M1),M=(q(2,1),M1))),Indexes,IR),
	pol_expand_to_tuple_length(IR,I),
	pol_assert(pol_op_interpretation(F,K,I)),
	!.
