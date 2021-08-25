/*
 *	file:		strings.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates related to strings.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

mkString([Char|String],[P|Ps],PS):-
	name('%',[Char]),
	mkString(String,Ps,SPs),
	(atomic(P) ->
		name(P,S)
	;	S=P
	),
		append(S,SPs,PS),
	!.
mkString([Char|String],Ps,[Char|SPs]):-
	mkString(String,Ps,SPs),
	!.
mkString([],_,[]):-
	!.
mkString(_,[],_):-
	nl,
	write(' *** mkString : too few parameters').


mkAtom(A,L,B):-
	name(A,AS),
	mkString(AS,L,BS),
	name(B,BS).

% baseName(FullName,BaseName) <=>
% ---------------------------
%	BaseName is the largest suffix of FullName which doesn't contain
%	a slash

baseName(noorder,noorder) :- !.
baseName(FullName,BaseName) :-
	name(FullName,LFullName),
	baseNameList(LFullName,LBaseName,_),
	name(BaseName,LBaseName).

% baseNameList(CharList1,CharList2,true) <=> 
% --------------------------------------
%	CharList1 == CharList2 and '/' is not element of CharList1
% baseNameList(CharList1,CharList2,false) <=>
% ---------------------------------------
%	CharList2 is the largest suffix of CharList1 which does'nt contain
%	a slash ('/') and CharList2 \== CharList1

baseNameList([],[],true).
baseNameList([Slash|L],BaseName,false) :-
	name('/',[Slash]),
	!,
	baseNameList(L,BaseName,_WithoutSlashes).
baseNameList([C|L],BaseName,WithoutSlashes) :-
	baseNameList(L,LBaseName,WithoutSlashes),
	(WithoutSlashes == true ->
		BaseName = [C|LBaseName]
	;
		BaseName = LBaseName
	).
