/*
 *	file:		morphisms.pl
 *	version:	1.5
 *	date:		November 8, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicate for the application of a sort and
 *	operator  renaming  (morphism, for short)  to  an object  of a 
 *	specification. Additionally there a predicates for testing the 
 *	syntactical correctness, the injectivity and the admissibility 
 *	of a morphism.
 *
 *	history:
 *	891106	uh	Added this comment
 *	891108	uh 	Moved definition of
 *			isAssoc/1
 *			from rename.pl into this file
 *	891129	uh	Changed definition of
 *			opAssoc/3
 *			to allow the use of disambiguated order-sorted
 *			operators O/N
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*
 *			applyAssoc(T,AT,A)				*
 * applies a operator and sort renaming A to a term T yielding AT	*
 *----------------------------------------------------------------------*/

applyAssoc(T,T,[]):-
	!.
applyAssoc(T,T,([],[])):-			% inserted rs 4.01.89
	!.
applyAssoc(T,T,_) :-
	var(T),
	!.
applyAssoc(T,T,_):-
	db_reference(T),
	!.
applyAssoc(@T,@TNew,(SortAssoc,_OAssoc)) :-	% inserted rs 4.01.89
	varName(T,Name),
	termType(@T,Type),
	!,
	applyAssoc(Type,NewType,SortAssoc),
	mkAtom('%-%',[Name,NewType],TNew),
	!.
applyAssoc(@T,@T,_) :-
	!.
applyAssoc(T,TNew,(SortAssoc,OAssoc)) :-
	atom(T),
	(	member((T,TNew),OAssoc)	% to rename constants
	;	varName(T,Name),	% to rename variables in substitutions
		Name\==T,
		termType(@T,Type),
		applyAssoc(Type,NewType,SortAssoc),
		mkAtom('%-%',[Name,NewType],TNew)),
	!.
applyAssoc(Op/Arity,OpNew/Arity,(_,OAssoc)) :- % inserted uh 08.12.89
	!,
	(member((Op/Arity,OpNew/Arity),OAssoc) ->
		true
	;
		OpNew/Arity = Op/Arity
	),
	!.
applyAssoc(Op/Arity,OpNew/Arity,A) :-
	!,
	(member((Op/Arity,OpNew/Arity),A) ->
		true
	;
		OpNew/Arity = Op/Arity
	),
	!.
applyAssoc(T,TNew,(SAssoc,OAssoc)) :-		% inserted rs 4.01.89
	T=..[O|Ts],
	!,
	mapL(applyAssoc,Ts,ATs,[(SAssoc,OAssoc)]),
	(member((O,AO),OAssoc) ->
		NewO = AO
	;
		NewO = O
	),
	TNew=..[NewO|ATs],
	!.
applyAssoc(T,TNew,A) :-
	T=..[O|Ts],
	mapL(applyAssoc,Ts,ATs,[A]),
	(member((O,AO),A) ->
		NewO = AO
	;
		NewO = O
	),
	TNew=..[NewO|ATs],
	!.

applyAssocA(T,T,[]):-
	!.
applyAssocA(T,T,_) :-
	var(T),
	!.
applyAssocA(O,NewO,A) :-
	atomic(O),
	(	member((O,NewO),A)
	;	NewO = O),
	!.
applyAssocA(Op/Arity,OpNew/Arity,A) :-
	!,
	(member((Op/Arity,OpNew/Arity),A) ->
		true
	;
		OpNew/Arity = Op/Arity
	),
	!.
applyAssocA(T,AT,A) :-
	T=..[O|Ts],
	mapL(applyAssocA,Ts,ATs,[A]),
	AT=..[O|ATs],
	!.


/*----------------------------------------------------------------------*
 *			injective(Assoc)				*
 *----------------------------------------------------------------------*/

injective(Assoc) :-
	member((O,N),Assoc),
	member((O1,N),Assoc),
	O\==O1,
	!,
	fail.
injective(_).


/*----------------------------------------------------------------------*
 *		   opAssoc((Op,NewOp),OpAssoc,SortAssoc)		*
 *		   opBasicAssoc(OpShort,NewShort,OpAssoc)		*
 *----------------------------------------------------------------------*/

% old version (changed uh 29.11.89)
% opAssoc((Op,NewOp),OpAssoc,SortAssoc) :-
% 	(OpShort ofType (Op:T)),
% 	opBasicAssoc(OpShort,NewShort,OpAssoc),
% 	applyAssoc(T,TR,SortAssoc),
% 	uniqueOpName(NewShort,TR,NewOp).
% 
% opBasicAssoc(OpShort,NewShort,OpAssoc) :-
% 	member((OpShort,O),OpAssoc),
% 	(admissibleAssoc(OpShort) ->
% 		NewShort=O
% 	;
%		NewShort=OpShort
%	).
% opBasicAssoc(OpShort,OpShort,OpAssoc) :-
%	not member((OpShort,_),OpAssoc).

% new version (generated uh 29.11.89)
opAssoc((Op,NewOp),OpAssoc,SortAssoc) :-
	(OpShort ofType (Op:T)),
	length(T,N),
	ArgNum is N-1,
	opBasicAssoc(OpShort/ArgNum,NewShort,OpAssoc),
	applyAssoc(T,TR,SortAssoc),
	uniqueOpName(NewShort,TR,NewOp).
opAssoc((OldOp,NewOp),OpAssoc,_SortAssoc) :-
	(OpShort ofType (_Op:T)),
	length(T,N),
	ArgNum is N-1,
	opBasicAssoc(OpShort/ArgNum,NewShort,OpAssoc),
	osOpName(OpShort, ArgNum,OldOp),
	osOpName(NewShort,ArgNum,NewOp).

opBasicAssoc(OpShort/N,NewShort,OpAssoc) :-
	member((OpShort/N,NewOp),OpAssoc),
	(admissibleAssoc(OpShort) ->
		NewShort=NewOp
	;
		true
	),
	!.
opBasicAssoc(OpShort/_,NewShort,OpAssoc) :-
	member((OpShort,NewOp),OpAssoc),
	\+ OpShort = _/_,
	(admissibleAssoc(OpShort) ->
		NewShort=NewOp
	;
		NewShort=OpShort
	).
opBasicAssoc(OpShort/_,OpShort,OpAssoc) :-
	not member((OpShort,_),OpAssoc).


/*----------------------------------------------------------------------*
 *		 	  admissibleAssoc(O)				*
 *----------------------------------------------------------------------*/

admissibleAssoc(O) :-
	(member(O,['$inj']) ->
		error("Operator % has a built-in meaning - not renamed.",[O],renameSpec),
		fail
	;
		true
	).


/*----------------------------------------------------------------------*
 *		 	  completeOpAssoc(O,S,CO)			*
 *----------------------------------------------------------------------*/

completeOpAssoc(O,S,CO) :-
	setof1(A,opAssoc(A,O,S),CO).

auxOpRenaming(OI,OIA):-
	setof1(A,internalOpAssoc(A,OI),OI1),
	append(OI,OI1,OIA),
	!.

internalOpAssoc((O,OR),OI):-
	member((P,P1),OI),
	(_ ofType (P:[_|T])),
	length(T,N),
	mkAtom('$%%',[N,P],O),
	mkAtom('$%%',[N,P1],OR).
internalOpAssoc((O,OR),OI):-
	member((P,P1),OI),
	(_ ofType (P:[_|T])),
	length(T,N),
	mkAtom('$%G%',[N,P],O),
	mkAtom('$%G%',[N,P1],OR).

isAssoc(_,[]).
isAssoc(sorts,[(X,Y)|A]) :-
	atomic(X),
	atomic(Y),
	isAssoc(sorts,A).
isAssoc(operators,[(X,Y)|A]) :-
	(atomic(X) ; (X = F/N, atomic(F), number(N))),
	atomic(Y),
	isAssoc(operators,A).
