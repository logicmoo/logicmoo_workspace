/******************************************************************************
    This file is being distributed, by written permission of Quintus 
    Corporation, for use with the BACK system only.  This file may not
    be used with Prolog implementations other than Quintus Prolog except
    (a) as part of the BACK system, or (b) with the written permission
    of Quintus Corporation.  To obtain such written permission, please
    contact:

	Quintus Corporation
	2100 Geng Road
	Palo Alto,
	California  94303
	USA
	415-813-3800
	marketing@quintus.com
******************************************************************************/

%   Module : lists
%   Authors: Bob Welham, Lawrence Byrd, and Richard A. O'Keefe
%   Updated: 10/25/90
%   Defines: list processing utilities
%   SeeAlso: library(flatten)

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(lists_quint, [
	append/2,			%   ListOfLists -> List
	append/5,			%   List x List x List x List x List
	correspond/4,			%   Elem <- List x List -> Elem
	delete/3,			%   List x Elem -> List
	delete/4,			%   List x Elem x Count -> List
	is_list/1,			%   List ->
	keys_and_values/3,		%   KeyValList -> KeyList x ValList
	last/2,				%   List -> Elem
	nextto/3,			%   Elem, Elem <- List
	nmember/3,			%   Elem <- Set -> Integer
	nth0/3,				%   Integer x List -> Elem
	nth0/4,				%   Integer x List -> Elem x List
	nth1/3,				%   Integer x List -> Elem
	nth1/4,				%   Integer x List -> Elem x List
	one_longer/2,			%   List x List ->
	perm/2,				%   List -> List
	perm2/4,			%   Elem x Elem -> Elem x Elem
	permutation/2,			%   List <-> List
	proper_length/2,		%   List -> Length
	remove_dups/2,			%   List -> Set
	rev/2,				%   List -> List
	reverse/2,			%   List -> List
	same_length/2,			%   List x List ->
	same_length/3,			%   List x List x Integer ->
	select/4,			%   Elem x List x Elem -> List
	selectchk/4,			%   Elem x List x Elem -> List
	shorter_list/2,			%   List x List ->
	subseq/3,			%   List -> List x List
	subseq0/2,			%   List -> List
	subseq1/2,			%   List -> List
	sumlist/2,			%   List -> Integer
	transpose/2			%   ListOfLists <-> ListOfLists
   ]).
:- use_module(library(types), [
	should_be/4
   ]).

sccs_id('"@(#)90/10/25 lists.pl    58.1"').

/*  Several of the comments below indicate that a particular predicate
    only works when a particular argument "is a proper list".  In
    general, an instance of a recursively defined data type "foo" is
    said to be "a proper foo" when it is a non-variable and all its
    "foo" components are proper foos.  For example, consider
	:- type tree(K,V) = {} | node(K,V,tree(K,V),tree(K,V)).
    X is said to be "a proper tree" if X is {} or if X is node(_,_,L,R)
    where both L and R are proper trees.  Similarly, X is a proper list
    if and only if X is [] or X is [_|L] where L is a proper list.
    The point is that a recursive procedure working its way down a
    proper whatever will not be creating new structure.  The predicate
    is_list/1 recognises proper lists.

    In general, the predicates in this file are only defined when the
    arguments have types compatible with the :- pred declaration below
    instantiated compatibly with the :- mode declaration below.  Their
    effect on other inputs is not defined.  For example,
	append([], 1, X, 2, Y)
    succeeds, binding X = 1 and Y = 2.  This is rather odd, but since
    the query is ill-typed the behaviour is not an error.
*/
:- mode
	append(+, ?),
	append(?, ?, ?, ?, ?),
	correspond(?, +, +, ?),
	delete(+, +, -),
	delete(+, +, +, -),
	    delete_1(+, +, +, -),
	is_list(?),
	is_list_(+),
	keys_and_values(?, ?, ?),
	last(?, ?),
	    last_1(?, ?, ?),
	nextto(?, ?, ?),
	nmember(?, +, ?),
	nth0(+, +, ?),
	nth0(+, ?, ?, ?),
	nth1(+, +, ?),
	nth1(+, ?, ?, ?),
	one_longer(?, ?),
	perm(+, ?),
	    insert(+, +, ?),
	permutation(?, ?),
	    permutation(?, ?, ?),
	perm2(?,?, ?,?),
	proper_length(+, ?),
	    proper_length(+, +, -),
	remove_dups(+, ?),
	rev(?, ?),
	reverse(?, ?),
	reverse(?, +, ?),
	same_length(?, ?),
	same_length(?, ?, ?),
	    'same length'(+, ?, ?),
	    'same length'(?, ?, +, -),
	select(?, ?, ?, ?),
	selectchk(+, +, ?, ?),
	shorter_list(?, +),
	subseq(?, ?, ?),
	subseq0(+, ?),
	subseq1(+, ?),
	sumlist(+, ?),
	sumlist(+, +, ?).

/* pred
	append(list(list(T)), list(T)),
	append(list(T), list(T), list(T), list(T), list(T)),
	correspond(T, list(T), list(U), U),
	delete(list(T), T, list(T)),
	delete(list(T), T, integer, list(T)),
	    delete_1(list(T), T, integer, list(T)),
	is_list(T),
	keys_and_values(list(pair(T,U)), list(T), list(U)),
	last(T, list(T)),
	    last_1(list(T), T, T),
	nextto(T, T, list(T)),
	nmember(T, list(T), integer),
	nth0(integer, list(T), T),
	nth1(integer, list(T), T),
	    nth0v(list(T), T, integer, integer),
	    nth0i(integer, list(T), T),
	nth0(integer, list(T), T, list(T)),
	nth1(integer, list(T), T, list(T)),
	    nth0i(integer, list(T), T, list(T)),
	    nth0v(list(T), T, integer, integer, list(T)),
	one_longer(list(T), list(T)),
	perm(list(T), list(T)),
	    insert(list(T), T, list(T)),
	permutation(list(T), list(T)),
	    permutation(list(T), list(T), list(T)),
	perm2(T, T, T, T),
	proper_length(list(T), integer),
	    proper_length(list(T), integer, integer),
	remove_dups(list(T), list(T)),
	rev(list(T), list(T)),
	    rev(list(T), list(T), list(T)),
	reverse(list(T), list(T)),
	    reverse(list(T), list(T), list(T), list(T)),
	same_length(list(T), list(T)),
	same_length(list(T), list(T), integer),
	    'same length'(integer, list(T), list(T)),
	    'same length'(list(T), list(T), integer, integer),
	select(T, list(T), T, list(T)),
	selectchk(T, list(T), T, list(T)),
	shorter_list(list(T), list(T)),
	subseq(list(T), list(T), list(T)),
	subseq0(list(T), list(T)),
	subseq1(list(T), list(T)),
	sumlist(list(integer), integer),
	    sumlist(list(integer), integer, integer),
	transpose(list(list(T)), list(list(T))),
	    transpose(list(list(T)), list(list(T)), list(list(T))),
		transpose_1(list(U), list(U)),
		transpose_1(list(list(T)), list(T), list(list(T))).
*/


%   append(+ListOfLists, ?List)
%   is true when ListOfLists is a list [L1,...,Ln] of lists, List is
%   a list, and appending L1, ..., Ln together yields List.  The
%   ListOfLists **must** be a proper list.  (Strictly speaking we
%   should produce an error message if it is not, but this version
%   fails.)  Additionally, either List should be a proper list, or
%   each of L1, ..., Ln should be a proper list.  The behaviour on
%   non-lists is undefined.  ListOfLists must be proper because for
%   any given solution, infinitely many more can be obtained by
%   inserting nils ([]) into ListOfList.

append(-, _) :- !, fail.	% reject partial lists.
append([], []).
append([L|Ls], List0) :-
	append(L, List1, List0),
	append(Ls, List1).



%   append(?Prefix, ?Tail1, ?List1, ?Tail2, ?List2)
%   is true when append(Prefix, Tail1, List1) and append(Prefix, Tail2, List2)
%   are both true.  You could call append/3 twice, but that is order-
%   dependent.  This will terminate if Prefix is a proper list or if
%   either List1 or List2 is a proper list.

append([], List1, List1, List2, List2).
append([H|T], Tail1, [H|List1], Tail2, [H|List2]) :-
	append(T, Tail1, List1, Tail2, List2).



%   correspond(X, Xlist, Ylist, Y)
%   is true when Xlist and Ylist are lists, X is an element of Xlist, Y is
%   an element of Ylist, and X and Y are in similar places in their lists.
%   No relation is implied between other elements of Xlist and Ylist.
%   For a similar predicate without the cut, see select/4 below.
%   New code should use selectchk/4 instead.

correspond(X, [X|_], [Y|_], Y) :- !.
correspond(X, [_|T], [_|U], Y) :-
	correspond(X, T, U, Y).



%   delete(+List, +Kill, ?Residue)
%   is true when List is a list, in which Kill may or may not occur, and
%   Residue is a copy of List with all elements equal to Kill deleted.
%   To extract a single copy of Kill, use select(Kill, List, Residue).
%   If List is not proper, delete/3 will FAIL.  Kill and the elements of
%   List should be sufficiently instantiated for \= to be sound.

delete(-, _, _) :- !, fail.		% reject partial lists
delete([], _, []).
delete([Kill|Tail], Kill, Residue) :- !,
	delete(Tail, Kill, Residue).
delete([Head|Tail], Kill, [Head|Residue]) :-
    %	Head \= Kill,
	delete(Tail, Kill, Residue).


%   delete(+List, +Kill, +Count, ?Residue)
%   is true when List is a list, in which Kill may or may not occur,
%   and Count is a non-negative integer, and Residue is a copy of
%   List with the first Count elements equal to Kill deleted.  If
%   List has fewer than Count elements equal to Count, all of them
%   are deleted.
%   If List is not proper, delete/4 may FAIL.  Kill and the elements of
%   List should be sufficiently instantiated for \= to be sound.

delete(List, Kill, N, Residue) :-
	(   integer(N), nonvar(Kill) ->
	    (   N > 0 ->
		delete_1(List, Kill, N, Residue)
	    ;   N =:= 0 ->
		Residue = List
	    )
	;   should_be(integer, N,   3, delete(List,Kill,N,Residue)),
	    should_be(nonvar, Kill, 2, delete(List,Kill,N,Residue))
	).

delete_1(-, _, _, _) :- !, fail.	% reject partial lists
delete_1([], _, _, []).
delete_1([Kill|Tail], Kill, N, Residue) :- !,
	M is N-1,
	(   M > 0 -> delete_1(Tail, Kill, M, Residue)
	;/* M = 0 */ Residue = Tail
	).
delete_1([Head|Tail], Kill, N, [Head|Residue]) :-
    %	Head \= Kill,
	delete_1(Tail, Kill, N, Residue).



%   is_list(+List)
%   succeeds when List is a proper list.  That is, List is nil ([]) or
%   a cons cell ([Head|Tail]) whose Tail is a proper list.
%   A variable, or a list whose final tail is a variable, will fail this
%   test.

is_list(-) :- !, fail.		% catch & reject variables
is_list([]).
is_list([_|Tail]) :-
	is_list(Tail).



%   keys_and_values(?[K1-V1,...,Kn-Vn], ?[K1,...,Kn], ?[V1,...,Vn])
%   is true when its arguments look like the picture above.  It is meant
%   for splitting a list of Key-Value pairs (such as keysort/2 wants and
%   produces) into separate lists of Keys and of Values.  It may just as
%   well be used for building a list of pairs from a pair of lists.   In
%   fact one usually wants just the keys or just the values, but you can
%   supply _ as the other argument.   For example, suppose you wanted to
%   sort a list without having duplicates removed.  You could do
%	keys_and_values(RawPairs, RawKeys, _),
%	keysort(RawPairs, OrdPairs),
%	keys_and_values(OrdPairs, OrdKeys, _).
%   (In fact this operation is msort/2 and should be available somewhere.)

keys_and_values([], [], []).
keys_and_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
	keys_and_values(Pairs, Keys, Values).



%   last(?Last, +List)
%   is true when List is a List and Last is its last element.
%   The argument order is surprising; if it followed the recommended
%   inputs-before-outputs rule, it would be last(List -> Last).
%   There is also a last(?Fore, ?Last, ?List) in library(listparts),
%   whose argument order matches append/3, and this is consistent with
%   that, but last/3 came after.  "Backwards compatibility" is the excuse.
%   This could be defined as last(X,L) :- append(_, [X], L).

last(Last, [Head|Tail]) :-
	last_1(Tail, Head, Last).		

last_1([], Last, Last).
last_1([Head|Tail], _, Last) :-
	last_1(Tail, Head, Last).



%   nextto(?X, ?Y, +List)
%   is true when X and Y appear side-by-side in List.
%   It could be written as
%	nextto(X, Y, List) :- append(_, [X,Y], List).
%   It may be used to enumerate successive pairs from the list.
%   List should be proper, otherwise nextto/3 will generate it.

nextto(X,Y, [X,Y|_]).
nextto(X,Y, [_|List]) :-
	nextto(X,Y, List).



%   nmember(Elem, List, Index)
%   is true when Elem is the Indexth member of List.  Could be written as
%	nmember(X, L, N) :- append(B, [X|_], L), length(B, M), N is M+1.
%   It may be used to select a particular element, or to find where some
%   given element occurs, or to enumerate the elements and indices together.
%   It is now an alias for nth1/3, which should be used in new programs.

nmember(Element, List, Index) :-
	nth1(Index, List, Element).



%   nth0(?N, ?List, ?Elem)
%   is true when Elem is the Nth member of List, counting the first as
%   element 0.  That is, throw away the first N elements and unify Elem
%   with the next.  E.g. nth0(0, [H|T], H).
%   Either N should be an integer, or List should be proper.

nth0(Index, List, Element) :-
	(   integer(Index) ->		% are we finding Element?
	    Index >= 0,
	    nth0i(Index, List, Element)
	;   var(Index) ->		% or are we finding Index?
	    nth0v(List, Element, 0, Index)
	;   should_be(integer, Index, 1, nth0(Index,List,Element))
	).


%   nth0v: find the Index of an Element in the given List.
%   The Element might occur more than once, find each place.

nth0v([Element|_], Element, Index, Index).
nth0v([_|Tail], Element, M, Index) :-
	N is M+1,
	nth0v(Tail, Element, N, Index).


%   nth0i: find an Element in the given List at a known Index >= 0.

nth0i(N, [Head|Tail], Elem) :-
	(   N =:= 0 -> Elem = Head
	;   M is N-1,			% should be succ(M, N)
	    nth0i(M, Tail, Elem)
	).


%   nth1(?N, ?List, ?Element)
%   is true when Elem is the Nth member of List, counting the first as
%   element 0.  That is, throw away the first N-1 elements and unify Elem
%   with the next element (the Nth).  E.g. nth1(1, [H|T], H).
%   This is just like nth0 except that it counts from 1 instead of 0.
%   Either N should be an integer, or List should be proper.

nth1(Index, List, Element) :-
	(   integer(Index) ->		% are we finding Element?
	    Index >= 1,
	    N is Index-1,
	    nth0i(N, List, Element)
	;   var(Index) ->		% or are we finding Index?
	    nth0v(List, Element, 1, Index)
	;   should_be(integer, Index, 1, nth1(Index,List,Element))
	).



%   nth0(?N, ?List, ?Elem, ?Rest)
%   unifies Elem with the Nth element of List, counting from 0, and Rest
%   with the other elements.  It can be used to select the Nth element
%   of List (yielding Elem and Rest), or to insert Elem *before* the Nth
%   (counting from 0) element of Rest, when it yields List, e.g.
%   nth0(2, List, c, [a,b,d,e]) unifies List with [a,b,c,d,e].
%   This can be seen as inserting Elem *after* the Nth element of Rest
%   if you count from 1 rather than 0.
%   Either N should be an integer, or List or Rest should be proper.

nth0(Index, List, Elem, Rest) :-
	(   integer(Index) ->		% are we finding Elem?
	    Index >= 0,
	    nth0i(Index, List, Elem, Rest)
	;   var(Index) ->		% or are we finding Index?
	    one_longer(List, Rest),
	    nth0v(List, Elem, 0, Index, Rest)
	;   should_be(integer, Index, 1, nth0(Index,List,Elem,Rest))
	).


nth0i(N, [Head|Tail], Elem, Rest) :-
	(   N =:= 0 ->
	    Elem = Head, Rest = Tail
	;   M is N-1,			% succ(M, N); should fail if N < 1
	    Rest = [Head|More],
	    nth0i(M, Tail, Elem, More)
	).


nth0v([Head|Tail], Head, Index, Index, Tail).
nth0v([Head|Tail], Elem, M, Index, [Head|Rest]) :-
	N is M+1,
	nth0v(Tail, Elem, N, Index, Rest).


%   nth1(?N, ?List, ?Elem, ?Rest)
%   unifies Elem with the Nth element of List, counting from 1, and Rest
%   with the other elements.  It can be used to select the Nth element
%   of List (yielding Elem and Rest), or to insert Elem *before* the Nth
%   (counting from 1) element of Rest, when it yields List, e.g.
%   nth1(2, List, b, [a,c,d,e]) unifies List with [a,b,c,d,e].
%   Either N should be an integer, or List or Rest should be proper.

nth1(Index, List, Elem, Rest) :-
	(   integer(Index) ->		% are we finding Elem?
	    Index >= 1,
	    N is Index-1,
	    nth0i(N, List, Elem, Rest)
	;   var(Index) ->		% or are we finding Index?
	    one_longer(List, Rest),
	    nth0v(List, Elem, 1, Index, Rest)
	;   should_be(integer, Index, 1, nth1(Index,List,Elem,Rest))
	).



%   one_longer(Longer, Shorter)
%   is true when length(Longer,N) & length(Shorter,M) & succ(M,N)
%   for some integers M, N.  This is not exported (yet).  It was
%   written to make {nth0,nth1}/4 able to find the index, just as
%   same_length/2 is useful for making things invertible.

one_longer([_|RestLonger], Shorter) :-
	same_length(RestLonger, Shorter).



%   perm(+List, ?Perm)
%   is true when List and Perm are permutations of each other.  The main
%   use of perm/2 is to generate permutations.  You should not use this
%   predicate in new programs; use permutation/2 instead.  List must be
%   a proper list.  Perm may be partly instantiated.

perm([], []).
perm([X|Xs], Ys1) :-
	perm(Xs, Ys),
	insert(Ys, X, Ys1).


insert(L, X, [X|L]).
insert([H|T], X, [H|L]) :-
	insert(T, X, L).



%   permutation(?List, ?Perm)
%   is true when List and Perm are permuations of each other.
%   Unlike perm/2, it will work even when List is not a proper list.
%   It even acts in a marginally sensible way when Perm isn't proper
%   either, but it will still backtrack forever.
%   Be careful: this is quite efficient, but the number of permutations of an
%   N-element list is N!, and even for a 7-element list that is 5040.

permutation(List, Perm) :-
	permutation(List, Perm, Perm).

permutation([], [], []).
permutation([X|Xs], Ys1, [_|Zs]) :-
	permutation(Xs, Ys, Zs),
	insert(Ys, X, Ys1).



%   perm2(A,B, C,D)
%   is true when {A,B} = {C,D}.  It is very useful for writing pattern
%   matchers over commutative operators.  It is used more than perm is.

perm2(X,Y, X,Y).
perm2(X,Y, Y,X).



%   proper_length(+List, ?Length)
%   succeeds when List is a proper list, binding Length to its length.
%   That is, is_list(List) & length(List, Length).

proper_length(List, Length) :-
	proper_length(List, 0, Length).


/*  The original code was
	proper_length(-, _, _) :- !, fail.	% fail for variables too
	proper_length([], N, N).
	proper_length([_|List], N0, N) :-
		N1 is N0+1,
		proper_length(List, N1, N).
    The current version unrolls the loop by a factor of four; I have
    experimented with various degrees of unrolling, and while larger
    degrees do pay off for long lists, they don't do a lot for short
    ones.  This code is about 4.1 times faster than length/2 -- that
    cannot be speeded up because it has to cope with partial lists.
*/
proper_length(*, _, _) :- !, fail.
proper_length([_,_,_,_|L], N0, N) :- !, N1 is N0+4, proper_length(L, N1, N).
proper_length([_,_,_],     N0, N) :- !, N is N0+3.
proper_length([_,_],       N0, N) :- !, N is N0+2.
proper_length([_],         N0, N) :- !, N is N0+1.
proper_length([],          N,  N).



%   remove_dups(+List, ?Pruned)
%   removes duplicated elements from List, which should be a proper list.
%   If List has non-ground elements, Pruned may contain elements which
%   unify; two elements will remain separate iff there is a substitution
%   which makes them different.  E.g. [X,X] -> [X] but [X,Y] -> [X,Y].

remove_dups(List, Pruned) :-
	sort(List, Pruned).



%   reverse(?List, ?Reversed)
%   is true when List and Reversed are lists with the same elements
%   but in opposite orders.  Either List or Reversed should be a
%   proper list: given either argument the other can be found.  If
%   both are incomplete reverse/2 can backtrack forever trying ever
%   longer lists.

reverse(List, Reversed) :-
	reverse(List, Reversed, [], Reversed).

reverse([], [], Reversed, Reversed).
reverse([Head|Tail], [_|Bound], Sofar, Reversed) :-
	reverse(Tail, Bound, [Head|Sofar], Reversed).



%   rev(+List, ?Reversed)
%   is a version of reverse/2 which only works one way around.
%   Its List argument must be a proper list whatever Reversed is.
%   You should use reverse/2 in new programs, though rev/2 is
%   faster when it is safe to use it.

rev(List, Reversed) :-
	rev(List, [], Reversed).

rev([], Reversed, Reversed).
rev([Head|Tail], Sofar, Reversed) :-
	rev(Tail, [Head|Sofar], Reversed).



%   same_length(?List1, ?List2)
%   is true when List1 and List2 are both lists and have the same number
%   of elements.  No relation between the values of their elements is
%   implied.  It may be used to generate either list given the other,
%   or indeed to generate two lists of the same length, in which case
%   the arguments will be bound to lists of length 0, 1, 2, ... 
%   The current versions of reverse/2 and permutation/2 were obtained
%   by mixing this in with unidirectional versions.

same_length([], []).
same_length([_|List1], [_|List2]) :-
	same_length(List1, List2).



%   same_length(?List1, ?List2, ?Length)
%   is true when List1 and List2 are both lists, Length is a non-negative
%   integer, and both List1 and List2 have exactly Length elements.  No
%   relation between the elements of the lists is implied.  If Length
%   is instantiated, or if either List1 or List2 is bound to a proper
%   list, same_length is determinate and terminating.  library(length)
%   has more predicates with this structure.

same_length(List1, List2, Length) :-
	(   integer(Length) ->
	    Length >= 0,
	    'same length'(Length, List1, List2)
	;   nonvar(Length) ->
	    should_be(integer, Length, 3, same_length(List1,List2,Length))
	;   var(List1) ->		% swap List1 and List2 around to
	    'same length'(List2, List1, 0, Length)
	;
	    'same length'(List1, List2, 0, Length)
	).

'same length'(0, List1, List2) :- !,	% delay unification
	List1 = [],			% to block infinite loops
	List2 = [].
'same length'(N, [_|Rest1], [_|Rest2]) :-
	M is N-1,			% N > 0, M >= 0
	'same length'(M, Rest1, Rest2).


'same length'([], [], N, N).
'same length'([_|Rest1], [_|Rest2], I, N) :-
	J is I+1,
	'same length'(Rest1, Rest2, J, N).



%   select(?X, ?Xlist, ?Y, ?Ylist)
%   is true when X is the Kth member of Xlist and Y the Kth element of Ylist
%   for some K, and apart from that Xlist and Ylist are the same.  You can
%   use it to replace X by Y or vice versa.  Either Xlist or Ylist should
%   be a proper list.  This is very like sets:select/3.  Note that the
%   arguments are like the arguments of member/2, twice.

select(X, [X|Tail], Y, [Y|Tail]).
select(X, [Head|Xlist], Y, [Head|Ylist]) :-
	select(X, Xlist, Y, Ylist).



%   selectchk(?X, +Xlist, ?Y, +Ylist)
%   is to select/4 as memberhck/2 is to member/2.  That is, it finds the
%   first K such that X unifies with the Kth element of Xlist and Y with
%   the Kth element of Ylist, and it commits to the bindings thus found.
%   If you have Keys and Values in "parallel" lists, you can use this to
%   find the Value associated with a particular Key (much better methods
%   exist).  Except for argument order, this is identical to correspond/4,
%   but selectchk/4 is a member of a coherent family.  Note that the
%   arguments are like the arguments of memberchk/2, twice.

selectchk(X, [X|Tail], Y, [Y|Tail]) :- !.
selectchk(X, [Head|Xlist], Y, [Head|Ylist]) :-
	selectchk(X, Xlist, Y, Ylist).



%   shorter_list(Short, Long)
%   is true when Short is a list is strictly shorter than Long.  Long
%   doesn't have to be a proper list provided it is long enough.  This
%   can be used to generate lists shorter than Long, lengths 0, 1, 2...
%   will be tried, but backtracking will terminate with a list that is
%   one element shorter than Long.  It cannot be used to generate lists
%   longer than Short, because it doesn't look at all the elements of the
%   longer list.

shorter_list([], [_|_]).
shorter_list([_|Short], [_|Long]) :-
	shorter_list(Short, Long).
	


%   subseq(Sequence, SubSequence, Complement)
%   is true when SubSequence and Complement are both subsequences of the
%   list Sequence (the order of corresponding elements being preserved)
%   and every element of Sequence which is not in SubSequence is in the
%   Complement and vice versa.  That is,
%   length(Sequence) = length(SubSequence)+length(Complement), e.g.
%   subseq([1,2,3,4], [1,3,4], [2]).  This was written to generate subsets
%   and their complements together, but can also be used to interleave two
%   lists in all possible ways.  Note that if S1 is a subset of S2, it will
%   be generated *before S2 as a SubSequence and *after it as a Complement.

subseq([], [], []).
subseq([Head|Tail], Sbsq, [Head|Cmpl]) :-
	subseq(Tail, Sbsq, Cmpl).
subseq([Head|Tail], [Head|Sbsq], Cmpl) :-
	subseq(Tail, Sbsq, Cmpl).



%   subseq0(+Sequence, ?SubSequence)
%   is true when SubSequence is a subsequence of Sequence, but may
%   be Sequence itself.   Thus subseq0([a,b], [a,b]) is true as well
%   as subseq0([a,b], [a]).  Sequence must be a proper list, since
%   there are infinitely many lists with a given SubSequence.
%   ?- setof(X, subseq0([a,b,c],X), Xs).
%   Xs = [[],[a],[a,b],[a,b,c],[a,c],[b],[b,c],[c]] 
%   ?- bagof(X, subseq0([a,b,c,d],X), Xs).
%   Xs = [[a,b,c,d],[b,c,d],[c,d],[d],[],[c],[b,d],[b],[b,c],[a,c,d],
%	  [a,d],[a],[a,c],[a,b,d],[a,b],[a,b,c]] 

subseq0(List, List).
subseq0(List, Rest) :-
	subseq1(List, Rest).


%   subseq1(+Sequence, ?SubSequence)
%   is true when SubSequence is a proper subsequence of Sequence,
%   that is it contains at least one element less.  Sequence must
%   be a proper list, as SubSequence does not determine Sequence.

subseq1([_Head|Tail], Rest) :-
	subseq0(Tail, Rest).
subseq1([ Head|Tail], [Head|Rest]) :-
	subseq1(Tail, Rest).



%   sumlist(+Numbers, ?Total)
%   is true when Numbers is a list of integers, and Total is their sum.
%   Note that in Dec-10 compiled Prolog this will only work as stated;
%   interpreters will almost certainly accept integer expressions.  Also
%   note here as elsewhere in Prolog arithmetic that machine arithmetic
%   wraps round in Quintus Prolog: (2^28 - 1)+1 = -2^28 .

sumlist(Numbers, Total) :-
	sumlist(Numbers, 0, Total).

sumlist([], Total, Total).
sumlist([Head|Tail], Sofar, Total) :-
	Next is Sofar+Head,
	sumlist(Tail, Next, Total).


%   transpose(?X, ?Y)
%   is true when X is a list of the form [[X11,...,X1m],...,[Xn1,...,Xnm]]
%   and Y is its transpose, that is, Y = [[X11,...,Xn1],...,[X1m,...,Xnm]]
%   We insist that both lists should have this rectangular form, so that
%   the predicate can be invertible.  For the same reason, we reject empty
%   arrays with m = 0 or n = 0.

transpose(Xs, Ys) :-
	Xs = [X|_],	same_length(X, Ys), % length(X) = length([Y|Ys]) = M
	Ys = [Y|_],	same_length(Y, Xs), % length(Y) = length([X|Xs]) = N
	transpose(Ys, Xs, Xs).

transpose([], Zs, Xs) :-
	transpose_1(Zs, Xs).
transpose([Y|Ys], Zs, Xs) :-
	transpose_1(Xs, Y, Xs1),
	transpose(Ys, Zs, Xs1).

transpose_1([], []).
transpose_1([_|Zs], [[]|Xs]) :-
	transpose_1(Zs, Xs).

transpose_1([], [], []).
transpose_1([[H|T]|Xs], [H|Hs], [T|Ts]) :-
	transpose_1(Xs, Hs, Ts).

