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

%   Module : ordsets
%   Author : Richard A. O'Keefe
%   Updated: 1/3/90
%   Purpose: Ordered set manipulation utilities

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%   In this module, sets are represented by ordered lists with no
%   duplicates.  Thus {c,r,a,f,t} would be [a,c,f,r,t].  The ordering
%   is defined by the @< family of term comparison predicates, which
%   is the ordering used by sort/2 and setof/3.

%   The benefit of the ordered representation is that the elementary
%   set operations can be done in time proportional to the sum of the
%   argument sizes rather than their product.  You should use the
%   operations defined here in preference to those in library(sets)
%   unless there is a compelling reason why you can't.  Some of the
%   unordered set routines, such as member/2, length/2 and select/3 can
%   be used unchanged on ordered sets; feel free so to use them.

%   There is no ordset_to_list/2, as an ordered set is a list already.

:- module(ordsets, [
	is_ordset/1,		%  Set ->
	list_to_ord_set/2,	%  List -> Set
	ord_add_element/3,	%  Set x Elem -> Set
	ord_del_element/3,	%  Set x Elem -> Set
	ord_disjoint/2,		%  Set x Set ->
	ord_disjoint_union/3,	%  Set x Set -> Set
	ord_intersect/2,	%  Set x Set ->
	ord_intersect/3,	%  Set x Set -> Set
	ord_intersection/3,	%  Set x Set -> Set
	ord_intersection/2,	%  list(Set) -> Set
	ord_seteq/2,		%  Set x Set ->
	ord_setproduct/3,	%  Set x Set -> SetOfPairs
	ord_subset/2,		%  Set x Set ->
	ord_subtract/3,		%  Set x Set -> Set
	ord_symdiff/3,		%  Set x Set -> Set
	ord_union/3,		%  Set x Set -> Set
	ord_union/2,		%  list(Set) -> Set
	ord_union/4		%  Set x Set -> Set x Set
   ]).

sccs_id('"@(#)90/01/03 ordsets.pl    36.1"').



%   is_ordset(List)
%   is true when List is a list of terms [T1,T2,...,Tn] and the
%   terms are strictly increasing: T1 @< T2 @< ... @< Tn.  The
%   output of sort/2 always satisfies this test.  Anything which
%   satisfies this test can be given to the predicates in this
%   file, regardless of where you got it.

is_ordset(-) :- !, fail.	% catch and reject variables.
is_ordset([]).
is_ordset([Head|Tail]) :-
	is_ordset(Tail, Head).

is_ordset(-, _) :- !, fail.	% catch and reject variables.
is_ordset([], _).
is_ordset([Head|Tail], Left) :-
	Head @> Left,
	is_ordset(Tail, Head).



%   list_to_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  The only reason for giving it
%   a name at all is that you may not have realised that sort/2 could be
%   used this way.

list_to_ord_set(List, Set) :-
	sort(List, Set).



%   ord_add_element(+Set1, +Element, ?Set2)
%   is the equivalent of add_element for ordered sets.  It should give
%   exactly the same result as merge(Set1, [Element], Set2), but a bit
%   faster, and certainly more clearly.

ord_add_element([], Element, [Element]).
ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_add_element(Order, Head, Tail, Element, Set).


ord_add_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_add_element(Tail, Element, Set).
ord_add_element(=, Head, Tail, _, [Head|Tail]).
ord_add_element(>, Head, Tail, Element, [Element,Head|Tail]).



%   ord_del_element(+Set1, +Element, ?Set2)
%   is the equivalent of del_element for ordered sets.  Because it uses
%   ordering, it typically builds less structure, but is slower than
%   del_element.  I am beginning to wonder whether a predicate
%	set_plus(SmallSet, Element, LargeSet)
%   would be a better way of doing this, the idea being that
%   LargeSet = SmallSet U {Element} and Element is not in SmallSet.
%   There is already a predicate with this effect called select/3.

ord_del_element([], _, []).
ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element(Order, Element, Head, Tail, Set).

ord_del_element(<, Element, Head, Tail, [Head|Set]) :-
	ord_del_element(Tail, Element, Set).
ord_del_element(=, _, _, Set, Set).
ord_del_element(>, _, Head, Tail, [Head|Tail]).



%   ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  If the
%   arguments are not ordered, I have no idea what happens.

ord_disjoint(Set1, Set2) :-
	\+ ord_intersect(Set1, Set2).



%   ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.
%   Note that the test is == rather than = .
%   ord_intersect/2 has been unfolded in ord_intersect/5 to avoid
%   constructing anything.

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(=, _, _, _, _).
ord_intersect(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).
ord_intersect(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).



%   ord_intersect(+Set1, +Set2, ?Intersection)
%   is an obsolete synonym for ord_intersection/3.

ord_intersect(Set1, Set2, Intersection) :-
	ord_intersection(Set1, Set2, Intersection).



%   ord_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersection([], _, []).
ord_intersection([Head1|Tail1], Set2, Intersection) :-
	ord_intersection_1(Set2, Head1, Tail1, Intersection).

ord_intersection_1([], _, _, []).
ord_intersection_1([Head2|Tail2], Head1, Tail1, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_1(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersection_1(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection_1(<, _, Tail1, Head2, Tail2, Intersection) :-
	ord_intersection_1(Tail1, Head2, Tail2, Intersection).
ord_intersection_1(>, Head1, Tail1, _, Tail2, Intersection) :-
	ord_intersection_1(Tail2, Head1, Tail1, Intersection).



%   ord_intersection(+ListOfSets, ?Intersection)
%   is true when ListOfSets is a nonempty proper list of ordered sets
%   and Intersection is their intersection.  Earlier versions of this
%   package used a "logarithmic" method for this, similar to the code
%   in ord_union/2.  However, experiments showed that intersection is
%   better done using a "linear" method (in the case of union, we are
%   faced with _growing_ intermediate results, but in intersection we
%   have a case of _shrinking_ intermediate results).  We cannot take
%   an empty ListOfSets because its intersection is the universe!

ord_intersection([Set|Sets], Intersection) :-
	ord_intersection_3(Sets, Set, Intersection).

ord_intersection_3([], Intersection, Intersection).
ord_intersection_3([Set|Sets], Intersection0, Intersection) :-
	ord_intersection(Set, Intersection0, Intersection1),
	ord_intersection_3(Sets, Intersection1, Intersection).



%   ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.

ord_seteq(Set1, Set2) :-
	Set1 == Set2.



%   ord_setproduct(+Set1, +Set2, ?Product)
%   is in fact identical to setproduct(Set1, Set2, Product).
%   If Set1 and Set2 are ordered sets, Product will be an ordered
%   set of x1-x2 pairs.  Note that we cannot solve for Set1 and
%   Set2, because there are infinitely many solutions when
%   Product is empty, and may be a large number in other cases.

ord_setproduct([], _, []).
ord_setproduct([H|T], L, Product) :-
	ord_setproduct(L, H, Product, Rest),
	ord_setproduct(T, L, Rest).

ord_setproduct([], _, L, L).
ord_setproduct([H|T], X, [X-H|TX], TL) :-
	ord_setproduct(T, X, TX, TL).



%   ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.

ord_subset([], _).
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).

ord_subset(=, _, Tail1, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).



%   ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2.

ord_subtract([], _, []).
ord_subtract([Head1|Tail1], Set2, Difference) :-
	ord_subtract_1(Set2, Head1, Tail1, Difference).

ord_subtract_1([], Head1, Tail1, [Head1|Tail1]).
ord_subtract_1([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_subtract_2(Tail1, Head2, Tail2, Difference).
ord_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
	ord_subtract_1(Tail2, Head1, Tail1, Difference).
ord_subtract(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_subtract(Tail1, Tail2, Difference).

ord_subtract_2([], _, _, []).
ord_subtract_2([Head1|Tail1], Head2, Tail2, Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).



%   ord_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.

ord_symdiff([], Set2, Set2).
ord_symdiff([Head1|Tail1], Set2, Difference) :-
	ord_symdiff(Set2, Head1, Tail1, Difference).

ord_symdiff([], Head1, Tail1, [Head1|Tail1]).
ord_symdiff([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_symdiff(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_symdiff(Tail1, Head2, Tail2, Difference).
ord_symdiff(>, Head1, Tail1, Head2, Tail2, [Head2|Difference]) :-
	ord_symdiff(Tail2, Head1, Tail1, Difference).
ord_symdiff(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_symdiff(Tail1, Tail2, Difference).



%   ord_disjoint_union(+Set1, +Set2, ?Union)
%   is true when Set1 and Set2 (given to be ordered sets) have no element
%   in common, and Union is their union.  The meaning is the same as
%	ord_disjoint(Set1, Set2),
%	ord_union(Set1, Set2, Union)
%   but it is more efficient.

ord_disjoint_union([], Set2, Set2).
ord_disjoint_union([Head1|Tail1], Set2, Union) :-
	ord_disjoint_union_1(Set2, Head1, Tail1, Union).

ord_disjoint_union_1([], Head1, Tail1, [Head1|Tail1]).
ord_disjoint_union_1([Head2|Tail2], Head1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_disjoint_union_1(Order, Head1, Tail1, Head2, Tail2, Union).

ord_disjoint_union_1(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_disjoint_union_1(Tail1, Head2, Tail2, Union).
ord_disjoint_union_1(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_disjoint_union_1(Tail2, Head1, Tail1, Union).



%   ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.
%   The previous version had some cuts, and was 10-20% slower.

ord_union([], Set2, Set2).
ord_union([Head1|Tail1], Set2, Union) :-
	ord_union_1(Set2, Head1, Tail1, Union).

ord_union_1([], Head1, Tail1, [Head1|Tail1]).
ord_union_1([Head2|Tail2], Head1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_union_1(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union_1(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_union_1(Tail1, Head2, Tail2, Union).
ord_union_1(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_union_1(Tail2, Head1, Tail1, Union).
ord_union_1(=, Head1, Tail1, _,     Tail2, [Head1|Union]) :-
	ord_union(Tail1, Tail2, Union).



%   ord_union(+OldSet, +NewSet, ?Union, ?ReallyNew)
%   is true when Union is NewSet U OldSet and ReallyNew is NewSet \ OldSet.
%   This is useful when you have an iterative problem, and you're adding
%   some possibly new elements (NewSet) to a set (OldSet), and as well as
%   getting the updated set (Union) you would like to know which if any of
%   the "new" elements didn't already occur in the set (ReallyNew).

ord_union([], Set2, Set2, Set2).
ord_union([Head1|Tail1], Set2, Union, New) :-
	ord_union_1(Set2, Head1, Tail1, Union, New).

ord_union_1([], Head1, Tail1, [Head1|Tail1], []).
ord_union_1([Head2|Tail2], Head1, Tail1, Union, New) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union, New).

ord_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union], New) :-
	ord_union_2(Tail1, Head2, Tail2, Union, New).
ord_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union], [Head2|New]) :-
	ord_union_1(Tail2, Head1, Tail1, Union, New).
ord_union(=, Head1, Tail1, _,     Tail2, [Head1|Union], New) :-
	ord_union(Tail1, Tail2, Union, New).

ord_union_2([], Head2, Tail2, [Head2|Tail2], [Head2|Tail2]).
ord_union_2([Head1|Tail1], Head2, Tail2, Union, New) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union, New).



%   ord_union(+ListOfSets, ?Union)
%   is true when ListOfSets is given as a proper list of ordered sets
%   and Union is their union.  Letting K be the length of ListOfSets,
%   and N the sum of the sizes of its elements, the cost is of order
%   N.lg(K).  The auxiliary routine
%   ord_union(N, L, U, R)
%   is true when the union of the first N sets in L is U and
%   R is the remaining elements of L.

ord_union(ListOfSets, Union) :-
	length(ListOfSets, NumberOfSets),
	ord_union_3(NumberOfSets, ListOfSets, Union, []).

ord_union_3(0, R, [], R) :- !.
ord_union_3(1, [U|R], U, R) :- !.
ord_union_3(2, [A,B|R], U, R) :- !,
	ord_union(A, B, U).
ord_union_3(N, R0, U, R) :-
	P is N>>1,	% |first  half of list|
	Q is N- P,	% |second half of list|
	ord_union_3(P, R0, A, R1),
	ord_union_3(Q, R1, B, R),
	ord_union(A, B, U).

/*  You will notice that ord_union/2 and ord_intersection/2 have
    the same structure.  This is no accident.  Suppose we want to
    compute <F>([X1,...,Xn]) = <G>(X1) <H> ... <H> <G>(Xn),
    where <G> is any function, and <H> is associative with left
    identity <E> (it does not need to be commutative or to have
    a right identity) we can perform the calculation in at least
    the following ways:

    <F>_lin(List, Answer) :-
	<F>_lin(List, <E>, Answer).

    <F>_lin([], Answer, Answer).
    <F>_lin([X|Xs], SoFar, Answer) :-
	<G>(X, Xg),
	<H>(SoFar, Xg, Next),
	<F>_lin(Xs, Next, Answer).


    <F>_log(List, Answer) :-
	length(List, Length),
	<F>_log(Length, List, Answer, []

    <F>_log(0, R, <E>, R) :- !.
    <F>_log(1, [X|R], Answer, R) :- !,
	<G>(X Answer).
    <F>_log(2, [X1,X2|R], Answer, R) :- !,
	<G>(X1, G1),
	<G>(X2, G2),
	<H>(G1, G2, Answer).
    <F>_log(N, R0, Answer, R) :-
	P is N>>1,	% |first  half of list|
	Q is N- P,	% |second half of list|
	<F>_log(P, R0, A, R1),
	<F>_log(Q, R1, B, R),
	<H>(A, B, Answer).

    You will note that the third clause of <F>_log/4 is actually
    redundant.  If you partially evaluate the fourth clause with
    N=2 that is the answer you get.  The optimisation is usually
    a good idea.  You will further note that the first clause of
    <F>_log/4 is only used when <F>_log([], X) is called, so it
    could be moved up into that procedure.

    The <F>_lin schema is easy to grasp.  However, the <F>_log
    schema can be considerably more efficient.  Consider sorting.
    Here <E> = [], <G>(X,[X]), and <H>(X,Y,Z) = merge(X,Y,Z).
    Merging a single element into a list of length N costs O(N)
    time, so <sort>_lin would take O(N**2) time to sort a list
    of length N, whereas <sort>_log would take O(N.lgN) time.
    Hence the names.  <F>_log is a real win when the cost of the
    <H> operation is non-trivial and increases with bigger
    operands.  Numerical analysts know that <sum>_log is often
    the best way to sum a list of floating point numbers, that is,
    that it gives a more accurate answer (because there are only
    O(lgN) roundoffs in it, as opposed to O(N) in <sum>_lin), but
    that is a rather marginal case.  sumlist/3 in library(lists)
    is <sum>_lin because it was originally intended for integers.
*/
