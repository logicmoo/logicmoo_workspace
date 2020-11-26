/*  Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2011, K.U. Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Binomial Heap imlementation based on
%
%	Functional Binomial Queues
%	James F. King
%	University of Glasgow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(binomialheap,
	[
		empty_q/1,
		insert_q/3,
		insert_list_q/3,
		delete_min_q/3,
		find_min_q/2
	]).

:- use_module(library(lists),[reverse/2]).

% data Tree a = Node a [Tree a]
% type BinQueue a = [Maybe (Tree a)]
% data Maybe a = Zero | One a
% type Item = (Entry,Key)

key(_-Key,Key).

empty_q([]).

meld_q(P,Q,R) :-
	meld_qc(P,Q,zero,R).

meld_qc([],Q,zero,Q) :- !.
meld_qc([],Q,C,R) :- !,
	meld_q(Q,[C],R).
meld_qc(P,[],C,R) :- !,
	meld_qc([],P,C,R).
meld_qc([zero|Ps],[zero|Qs],C,R) :- !,
	R = [C | Rs],
	meld_q(Ps,Qs,Rs).
meld_qc([one(node(X,Xs))|Ps],[one(node(Y,Ys))|Qs],C,R) :- !,
	key(X,KX),
	key(Y,KY),
	( KX < KY ->
		T = node(X,[node(Y,Ys)|Xs])
	;
		T = node(Y,[node(X,Xs)|Ys])
	),
	R = [C|Rs],
	meld_qc(Ps,Qs,one(T),Rs).
meld_qc([P|Ps],[Q|Qs],C,Rs) :-
	meld_qc([Q|Ps],[C|Qs],P,Rs).

insert_q(Q,I,NQ) :-
	meld_q([one(node(I,[]))],Q,NQ).

insert_list_q([],Q,Q).
insert_list_q([I|Is],Q,NQ) :-
	insert_q(Q,I,Q1),
	insert_list_q(Is,Q1,NQ).

min_tree([T|Ts],MT) :-
	min_tree_acc(Ts,T,MT).

min_tree_acc([],MT,MT).
min_tree_acc([T|Ts],Acc,MT) :-
	least(T,Acc,NAcc),
	min_tree_acc(Ts,NAcc,MT).

least(zero,T,T) :- !.
least(T,zero,T) :- !.
least(one(node(X,Xs)),one(node(Y,Ys)),T) :-
	key(X,KX),
	key(Y,KY),
	( KX < KY ->
		T = one(node(X,Xs))
	;
		T = one(node(Y,Ys))
	).

remove_tree([],_,[]).
remove_tree([T|Ts],I,[NT|NTs]) :-
	( T == zero ->
		NT = T
	;
		T = one(node(X,_)),
		( X == I ->
			NT = zero
		;
			NT = T
		)
	),
	remove_tree(Ts,I,NTs).

delete_min_q(Q,NQ,Min) :-
	min_tree(Q,one(node(Min,Ts))),
	remove_tree(Q,Min,Q1),
	reverse(Ts,RTs),
	make_ones(RTs,Q2),
	meld_q(Q2,Q1,NQ).

make_ones([],[]).
make_ones([N|Ns],[one(N)|RQ]) :-
	make_ones(Ns,RQ).

find_min_q(Q,I) :-
	min_tree(Q,one(node(I,_))).


