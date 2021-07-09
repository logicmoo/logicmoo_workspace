:- module(lazy,
	[	lazy_unfold/5
	,	lazy_unfold/4
   ,  lazy_unfold_finite/4
	,	lazy_seqmap/5
	,	lazy_seqmap/4
   ,  lazy_maplist/2
   ,  lazy_maplist/3
   ,  lazy_maplist/4
   ,  lazy_repeat/2
   ,  lazy/4
	]).

:- meta_predicate
		lazy_seqmap(4,?,?,?,?)
   ,	lazy_seqmap(3,?,?,?)
	,	lazy_unfold(4,?,?,?,?)
	,	lazy_unfold(3,?,?,?)
   ,  lazy_unfold_finite(3,?,?,?)
   ,	lazy_maplist(1,?)
   ,	lazy_maplist(2,?,?)
   ,	lazy_maplist(3,?,?,?)
   ,  lazy(3,?,?,-)
	.

%% lazy(+P:pred(+A,+B,-C), X:A, Y:B, -Z:C) is det
%  Lazy version of call/4, triggered when Z is needed. Goal
%  expansion version allows macro expansion of call(P, ...).
lazy(P,X,Y,Z) :- freeze(Z,call(P,X,Y,Z)).
user:goal_expansion(lazy(P,X,Y,Z), freeze(Z,call(P,X,Y,Z))).

%% lazy_unfold( +P:pred(-A1,-A2,+S,-S), -XX1:list(A1), -XX2:list(A2), +S1:S, -S2:S) is det.
%% lazy_unfold( +P:pred(-A1,+S,-S), -XX1:list(A1), +S1:S, -S2:S) is det.
%
%  Lazily unfold an infinite stream
lazy_unfold(P,[X1|XX],[Y1|YY],S1,S3) :-
	call(P,X1,Y1,S1,S2),
	freeze(YY,lazy:lazy_unfold(P,XX,YY,S2,S3)).

lazy_unfold(P,[X1|XX],S1,S3) :-
	call(P,X1,S1,S2),
	freeze(XX,lazy:lazy_unfold(P,XX,S2,S3)).

%% lazy_unfold_finite( +P:pred(-A1,+S,-S), -XX1:list(A1), +S1:S, -S2:S) is det.
%  Lazily unfold a finite list or infinite stream. If unfolding predicate fails,
%  then a finite list is produced.
lazy_unfold_finite(P,Xs,S1,S3) :-
   (  call(P,X1,S1,S2)
   -> Xs=[X1|XX], freeze(XX,lazy:lazy_unfold_finite(P,XX,S2,S3))
   ;  Xs=[]
   ).

%% lazy_seqmap( +P:pred(A1,A2,S,S), +XX1:list(A1), -XX2:list(A2), S1:S, S2:S) is det.
%% lazy_seqmap( +P:pred(A1,A2,S,S), -XX1:list(A1), -XX2:list(A2), S1:S, S2:S) is nondet.
%% lazy_seqmap( +P:pred(A1,S,S), -XX1:list(A1), S1:S, S2:S) is nondet.
%
%  Lazy versions of dcg_core:seqmap//{2,3} - can succeed for any lenght of list.
%  Computation is frozen on last list, which is to be understood as an 'output'.
lazy_seqmap(_,[],[],S,S).
lazy_seqmap(P,[X1|XX],[Y1|YY],S1,S3) :-
	call(P,X1,Y1,S1,S2),
	freeze(YY,lazy:lazy_seqmap(P,XX,YY,S2,S3)).

lazy_seqmap(_,[],S,S).
lazy_seqmap(P,[X1|XX],S1,S3) :-
	call(P,X1,S1,S2),
	freeze(XX,lazy:lazy_seqmap(P,XX,S2,S3)).

lazy_maplist(P,[X|XX]) :- call(P,X), freeze(XX,lazy:lazy_maplist(P,XX)).
lazy_maplist(_,[]).

lazy_maplist(P,[X|XX],[Y|YY]) :- call(P,X,Y), freeze(YY,lazy:lazy_maplist(P,XX,YY)).
lazy_maplist(_,[],[]).

lazy_maplist(P,[X|XX],[Y|YY],[Z|ZZ]) :- call(P,X,Y,Z), freeze(ZZ,lazy:lazy_maplist(P,XX,YY,ZZ)).
lazy_maplist(_,[],[],[]).

lazy_repeat(X,[X|L]) :- freeze(L,lazy:lazy_repeat(X,L)).
