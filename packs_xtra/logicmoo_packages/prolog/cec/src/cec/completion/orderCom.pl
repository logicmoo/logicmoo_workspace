/*
 *	file:		orderCom.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	November 6, 1989
 *	author:		-
 *
 *	description:
 *	This file contains predicates related to termination orderings:
 *	- resetOrient/0 resetOrient/1
 *	- resetOrientation/0 resetOrientation/1
 *	- order/0 (interactively) order/1
 *	- operators/0 operators/1
 *	- greater/0 (interactively) greater/1
 *	- equal/0 (interactively) equal/1
 *	- status/0 (interactively) status/1 status/2
 * 	- setInterpretation/0 (interactively) setInterpretation/1
 *	  setInterpretation/2 ...
 *
 *	history:
 *	891106	uh 	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

resetOrient :-
	pushState(undo),
	resetOrientation.


resetOrient(I) :-
	(eqsExist ->
		true
	;	ctr('$equation'):=0
	),
	i('$rule',(C,[L=R]),I,Ref),
	dispose('$rule',Ref),
	ruleKind(C,L,R,K),
	(K = general ->
		true
	;
		retract(specialLaw(K))
	),
	new('$equation',(C,[L=R]),_,[],_).


resetOrientation :-
	resetOrient(_),
	fail.
resetOrientation.


resetOrientation(P) :-
	cont1(P,yes),
	P :== no,
	('$rule'(_I,_R) ->
		resetOrientation,
		write('All rules have been turned back into equations. '),
		nl
	;
		true
	).
resetOrientation(_P).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

order :-
	tps_current_ordering(Ord),
	transformed_Order(ExtOrd,Ord),
	nl,
	sPrint("The current termination ordering is ""%"".
The following alternative orderings may be selected:",[ExtOrd]),
	prompt1(['
recursive path ordering without equality (after Kapur, Narendran, Sivakumar)','
recursive path ordering (after Kapur, Narendran, Sivakumar)','
polynomial abstraction with tuplelength N','
manual ordering','
"file." for reading from a file','
"no." for no change'],
		[neqkns,kns,'poly<N>',manual,file,no],
		(transformed_Order(ExtNewOrd,NewOrd),
		 member(NewOrd,[neqkns,kns,poly(_N),manual,file,no])),
		ExtNewOrd),
	!,
	((NewOrd == no ; NewOrd == Ord) -> 
		true
	;
		(NewOrd == file ->
			nl,
			write('filename:'),
			read(File),
			nl,
			(fileExists(File) ->
				nl,
				write('Saving the current state...'),
				nl,
				pushState(undo),
				sPrint("Consulting the file %...",[File]),
				nl,
				consult(File),
				resetOrientation(ordering_changed)
			;
				error("failed",[],order)
			)
		;	
			% (NewOrd == poly(N) or NewOrd == kns_or_neqkns or
			%  NewOrd == manual and NewOrd \== Ord
			nl,
			write('Saving the current state...'),
			nl,
			pushState(undo),
			tps_order(NewOrd),
		 	nl,
			sPrint("The termination ordering is now ""%"".",[ExtNewOrd]),
			nl,
	 		(noResetOrientation(Ord,NewOrd) -> 
				true 
			; 
				((NewOrd = poly(_), Ord = poly(_)) ->
					true 
				;
					initialize_ordering
				),
				resetOrientation,
				write('All rules have been turned back into equations. '),
				nl
			)
		)
	),
	!.
order :-
	popState(undo),
	fail.


transformed_Order(Order,Order) :- 
	member(Order,[neqkns,kns,manual,file,no]),
	!.
transformed_Order(Order,poly(Tuplelength)) :-
	(atomic(Order) ; number(Tuplelength)),
	!,
	appendAtoms(poly,Tuplelength,Order),
	number(Tuplelength).
transformed_Order(poly(N),poly(N)) :-
	number(N).
transformed_Order(ExternalOrder,Order) :-
	appendAtoms(poly,Tuplelength,ExternalOrder),
	number(Tuplelength),
	Order = poly(Tuplelength),
	!.


order(NewOrd) :-
%	ancestors(G),
%	G \== [order(NewOrd)],
	tps_current_ordering(Ord),
	transformed_Order(NewOrd,NewOrd1),
	member(NewOrd1,[neqkns,kns,poly(_N),manual]),
	(NewOrd1 \== Ord -> 
		tps_order(NewOrd1),
		(noResetOrientation(Ord,NewOrd1) ->
			true
		;
			((NewOrd1 = poly(_), Ord = poly(_)) ->
				true 
			;
				initialize_ordering
			),
			ordering_changed :== yes
		)
	;
		true
	).
%order(_NewOrd) :-
%	nl,
%	error("It's not allowed to use order/1 interactively.
%	Use instead of order/1 the interactive version order/0.",[],order).
order(poly) :-
	order(poly(1)).


noResetOrientation(_OldOrd,manual).
noResetOrientation(neqkns,kns).
noResetOrientation(kns,neqkns) :-
	\+ kns_eq(_).
noResetOrientation(poly(N),poly(M)) :-
	N =< M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%operators :-
%	tps_operators.

operators :-
	operators(all).

operators(Mod) :-
	tps_operators(Mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init :- 
%	pushState(undo),
%	initialize_ordering,
%	resetOrientation,
%	write('All rules have been turned back into equations. '),
	nl,
	error("This CEC-command doesn't exist anymore.",[],init),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

greater :-
	tps_current_ordering(kns_or_neqkns),
	pushState(undo), 
	!,
	nl,
	write('Please enter a list of the following form:'),
	nl,
	write('[[a, b, c, ... ], [g, h, i, ... ], ... ]'),
	nl,
	write('Meaning: a > b > c > ... and g > h > i > ...'),
	nl,
	nl,
	read(L),
	tps_greater(L).
greater :-
	tps_current_ordering(Ord),
	\+ member(Ord,[kns,neqkns]),
	tps_error(Ord,kns_or_neqkns,greater).


greater(X) :- 
	tps_greater(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equal :-
	tps_current_ordering(kns),
	pushState(undo), 
	!,
	nl,
	write('Please enter a list of the following form:'),
	nl,
	write('[[a, b, c, ... ], [g, h, i, ... ], ... ]'),
	nl,
	write('Meaning: a = b = c = ... and g = h = i = ...'),
	nl,
	nl,
	read(L),
	tps_equal(L).
equal :-
	tps_current_ordering(Ord),
	Ord \== kns,
	tps_error(Ord,kns,equal).


equal(X) :-
	tps_equal(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

status :-
	tps_current_ordering(kns_or_neqkns),
	!,
	pushState(undo), 
	nl,
	write('Please enter [Operator : Status, ...]:'),
	nl,
	nl,
	read(L),
	repeat_status(L).
status :-
	tps_current_ordering(Ord),
	\+ member(Ord,[kns,neqkns]),
	tps_error(Ord,kns_or_neqkns,status).


status(L) :-
	repeat_status(L).


status(Op,St) :-
	tps_status(Op,St).


repeat_status([]).
repeat_status([(Op : St) | L]) :-
	tps_status(Op, St),
	repeat_status(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setInterpretation :-
	tps_current_ordering(poly(_N)),
	pushState(undo), 
	!,
	nl,
	write('Please enter [Op(Variablelist) : interpretation, ...]'),
	nl,
	write('Example : [push(s,e) : s + e^2].'),
	nl,
	nl,
	read(L),
	repeat_setInterpretation(L,_MayChange),
	nl.
setInterpretation :-
	tps_current_ordering(Ord),
	\+(Ord = poly(N)),
	tps_error(Ord,poly(N),setInterpretation).


setInterpretation(L) :-
	repeat_setInterpretation(L,dontChange).



setInterpretation(Op,IP) :-
	repeat_setInterpretation([(Op : IP)],dontChange).


repeat_setInterpretation((Op : IP),C) :-
	repeat_setInterpretation([Op : IP],C).
repeat_setInterpretation([],_) :-
	resetOrientation(ordering_changed).
repeat_setInterpretation([(Op : IP) | L],Changed) :-
	(var(Changed) ->
		C1 = C1
	;	C1 = Changed),
	tps_setInterpretation(Op,IP,Changed),
	(Changed == yes ->
		sPrint("*** new interpretation % for operator %!",[IP,Op]),nl,
		ordering_changed :== yes
	;
		true
	),
	repeat_setInterpretation(L,C1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpretation :- 
	tps_current_ordering(poly(_N)), 
	!,
	ss(beforeChange),
	tps_interpretation(ListOfChanges),
	(ListOfChanges \== [] -> 
%		setOfObjects('$rule',Rs),					% old rs 15.12.88
%		mapP(lambda([(Ref,R)],checkOrientation(R,Ref)),Rs),	% 	"
		resetOrientation,					% new rs 15.12.88
		write('All rules have been turned back into equations. '),%	"
		nl,							%	"
		ss(afterChange),
		ps(beforeChange),
		pushState(undo),
		ps(afterChange)
	;
		ds(beforeChange)
	),
	!.
interpretation :-
	tps_current_ordering(Ord),
	\+(Ord = poly(_N)),
	tps_error(Ord,poly(_N),interpretation),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tuplelength :-
	nl,
	error("This CEC-command doesn't exist anymore.",[],tuplelength),
	!.


tuplelength(NewTupleLength) :-
	order(poly(NewTupleLength)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialize_ordering :-
	tps_init,
	setof1(Op,(ac_ist_C(Op),not(ac_ist_AC(Op))),COps),
    	mapA(tps_declc,COps),
	setof1(Op,ac_ist_AC(Op),ACOps),
	mapA(tps_declac,ACOps).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constructor :-
	pushState(undo),
	!,
	error:==none,
	nl,
	write('Please enter an operator: '),
	read(Op),
	constructor(Op),
	checkConstructors,
	!,
	(cont1(error,none) ->
	    true
	;
	    popState(undo),
	    fail
	),
	nl.

constructor(Op/Arity) :-
	atomic(Op),
	number(Arity),
	(hhConstructor(Op/Arity) ->
		true
	;
		assert(hhConstructor(Op/Arity))
	),
	(setof(OT,osOpDeclsFor(Op/Arity,OT),Ops) ->
		mapP(lambda([OX],enterUnique(hhConstructor(OX))),Ops)
	;
		error("Operator % undeclared.",[Op],constructor)
	).
constructor('$true'):-
	!.
constructor('$true-$pred'):-
	!.
constructor(Op) :-
	atomic(Op),
	(hhConstructor(Op) ->
		true
	;	
		(differentArities(Op)	->
			error("% not unique",[Op],constructor)
			;
			(setof(OT,Type^ofType(Op,(OT:Type)),Ops) ->
			    mapP(lambda([OX],enterUnique(hhConstructor(OX))),Ops),
			    Op ofType (_:TypeList),
			    makeOp(Op,TypeList,OSOp),
			    assert(hhConstructor(OSOp))
			;
			    ofType(O,(Op:Type)) ->
				assert(hhConstructor(Op)),
				makeOp(O,Type,OSOp),
				assert(hhConstructor(OSOp))
			    ;
			    error("Operator % undeclared.",[Op],constructor)
			)
		)
	).

osOpDeclsFor('$true'/0,'$true-$pred').
osOpDeclsFor(Op/Arity,OT) :-
	Op ofType (OT:[_Codomain|ArgTypes]),
	length(ArgTypes,Arity).
