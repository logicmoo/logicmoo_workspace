/*
 *	file:		sets.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicaes related to sets.
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

% data type sorted set of pairs (key,info)
% the sorting is according to the ordering on keys

% :-[-'../apply'].

delete([X|Xs],X,Xs):-
	!.
delete([Y|Xs],X,[Y|Ys]):-
	delete(Xs,X,Ys),!.
delete([],_,_):-!.

delete1([X|Xs],Y,Xs):-
	X==Y,
	!.
delete1([Y|Xs],X,[Y|Ys]):-
	delete1(Xs,X,Ys),!.
delete1([],_,_):-!.

insert([(Key,Y)|Ys],(Key1,Y1),[(Key1,Y1),(Key,Y)|Ys]):-
	Key1 @< Key,!.
insert([(Key,Y)|Ys],(Key1,Y1),[(Key,Y)|Ys1]):-
	insert(Ys,(Key1,Y1),Ys1),!.
insert([],X,[X]):-!.
insert([X|Xs],X,[X|Xs]):-!.

merge(S,[],S):-!.
merge([],S,S):-!.
merge([E|Es],[F|Fs],Gs):-
	E=F,
	merge([E|Es],Fs,Gs),
	!.
merge([E|Es],[F|Fs],[E|Gs]):-
	E@<F,
	merge(Es,[F|Fs],Gs),
	!.
merge([E|Es],[F|Fs],[F|Gs]):-
	E@>F,
	merge([E|Es],Fs,Gs),
	!.


filter([],_,[]).
filter([X|Xs],P,[X|Ys]):-
	apply(P,[X]),
	!,
	filter(Xs,P,Ys).
filter([_|Xs],P,Ys):-
	filter(Xs,P,Ys).


:-dynamic split/4.
split(_,[],[],[]).
split(P,[X|Xs],[X|Ys],Zs):-
	apply(P,[X]),
	!,
	split(P,Xs,Ys,Zs).
split(P,[X|Xs],Ys,[X|Zs]):-
	split(P,Xs,Ys,Zs).


setof1(X,P,S):-
	setof(X,P,S1),
	!,
	S=S1.
setof1(_,_,[]).

bagof1(X,P,S):-
	bagof(X,P,S1),
	!,
	S=S1.
bagof1(_,_,[]).

member_s(X,[Y|_]):-
	X==Y,
	!.
member_s(X,[_|Ys]):-
	member_s(X,Ys).


isMember(X,[A-B|_]):-
	A=<X,
	X=<B,
	!.
isMember(X,[_|S]):-
	isMember(X,S).

isInsert([A-B|S],X,[X-B|S]):-
	X is A-1,
	!.
isInsert([A-B|S],X,[X-X,A-B|S]):-
	X < A,
	!.
isInsert([A-B|S],X,S1):-
	X is B+1,
	!,
	merge2Intervals([A-X|S],S1).
isInsert([A-B|S],X,[A-B|S1]):-
	X > B,
	!,
	isInsert(S,X,S1).
isInsert([],X,[X-X]).


merge2Intervals([A-B,C-D|S],[A-D|S]):-
	B=C;
	C is B+1,
	!.
merge2Intervals(X,X):-
	!.

isTranslate([],_,[]).
isTranslate([A-B|S],N,[AN-BN|SN]):-
	AN is A+N,
	BN is B+N,
	isTranslate(S,N,SN).


memberP(X,SetOfSets):-
	member(Set,SetOfSets),
	member(X,Set).

partition([],[],[]).
partition([X|Xs],Ys,[X|Zs]):-
	partition(Xs,Ys,Zs).
partition(Xs,[X|Ys],[X|Zs]):-
	partition(Xs,Ys,Zs).



:- dynamic path/5.
path(_,A,A,_,[]).
path(Rel,A,B,Visited,[C|Path]):-
	apply(Rel,[A,C]),
	\+member(C,Visited),
	path(Rel,C,B,[C|Visited],Path).



:- dynamic reflTransClosure/3.
reflTransClosure(Relation,A,B):-
	path(Relation,A,B,[],_Path).

reflTransClosureTest(R,A,B):-
	reflTransClosure(R,A,B),
	!.

:- dynamic transClosure/3.
transClosure(Relation,A,B):-
	path(Relation,A,B,[],Path),
	length(Path,N),
	N>0.


:-dynamic transClosureTest/3.
transClosureTest(R,A,B):-
	transClosure(R,A,B),
	!.

subset([],_):-
	!.
subset([X|Xs],Y):-
	member(X,Y),
	!,
	subset(Xs,Y),
	!.

intersection_s([],_,[]).
intersection_s([X|Xs],Ys,[X|Zs]):-
	member_s(X,Ys),
	!,
	intersection_s(Xs,Ys,Zs).
intersection_s([X|Xs],Ys,Zs):-
	not member_s(X,Ys),
	intersection_s(Xs,Ys,Zs).


integer(I,N,_):-
	I>N,
	!,
	fail.
integer(From,_To,From).
integer(From,To,N):-
	F is From+1,
	integer(F,To,N).

% tables with deleted-flags

dmember(X,[entry(Flag,X)|_]):-
	var(Flag).
dmember(X,[_|Xs]):-
	dmember(X,Xs).

