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
%
%         bianchi 6/6/96
% aggiunto il predicato memberchk/2 perche deve esserci anche in questo 
% modulo
%




%   Module : sets
%   Authors: Lawrence Byrd + Richard A. O'Keefe
%   Updated: 10/25/90
%   Purpose: Set manipulation utilities
%   SeeAlso: ordsets

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(sets, [
	add_element/3,		%  Elem x Set -> Set
	del_element/3,		%  Elem x Set -> Set
    %dmiles      is_set/1,		%  List ->
	disjoint/2,		%  Set x Set ->
	disjoint_union/3,	%  Set x Set -> Set
	intersect/2,		%  Set x Set ->
	intersect/3,		%  Set x Set -> Set
	intersection/2,		%  list(Set) -> Set
%dmiles	intersection/3,		%  Set x Set -> Set
%dmiles	list_to_set/2,		%  List -> Set
	pairfrom/4,		%  Set -> Elem x Elem x Set
	power_set/2,		%  Set -> Set of Sets
   %dmiles      select/3,		%  Elem <- Set -> Set
   %dmiles      selectchk/3,		%  Elem x Set -> Set
	seteq/2,		%  Set x Set ->
	setproduct/3,		%  Set x Set -> Pairs
%dmiles  	subset/2,		%  Set x Set ->
%dmiles  	subtract/3,		%  Set x Set -> Set
	symdiff/3,		%  Set x Set -> Set
	union/2,		%  list(Set) -> Set
   %dmiles  union/3,		%  Set x Set -> Set
	union/4			%  Set x Set -> Set x Set
   ]).
:- use_module(library(basics)). /*
   %dmiles       member/2,		%  Elem x Set ->
   %dmiles  	memberchk/2,		%  Elem x Set ->
	nonmember/2		%  Elem x Set ->
   */

sccs_id('"@(#)90/10/25 sets.pl	58.1"').

/*  Sets are represented as lists with no repeated elements.
    The ordered representation used in `ordsets' is much more
    efficient, but these routines were designed before sort/2
    entered the language.

    Before the introduction of modules, it used to be the case
    that if you loaded library(sets), you got library(basics) too.
    Now if you want {member,memberchk,nonmember}/2, you have to
    ask for library(basics) yourself.  Eheu.  We could export
    them from sets, but then you'd have trouble if you did load
    library(basics).  Eheu again.

    The predicates in this file are defined only when their arguments
    are of the correct types (as shown in the :- pred declaration) and
    in the right instantiation state.  Typically the last argument will
    be unified with a function result and can be in any state, but all
    the other arguments must be proper lists and "sufficiently"
    instantiated for memberchk/2 to be sound.  Many of the predicates
    do odd things if these requirements are violated:
	disjoint(X, Y) fails
	union([], 2, X) succeeds with X = 2
	add_element(3, X, Y) binds X = Y = [3|_]
    These oddities are not errors:  they are just "undefined".  The
    basic problem is that we have to know when something is NOT in a
    list, and for that to work things have to be ground.

    Using proper_list/1 and must_be_proper_list/3 from library(types),
    we could easily make this module check that arguments have the
    right form, but we currently don't do that.
*/

:- mode
	add_element(+, +, ?),
	del_element(+, +, ?),
	    del_element_1(+, +, ?),
	disjoint(+, +),
	disjoint_union(+, +, ?),
	is_set(+),
	select(?, ?, ?),
	selectchk(+, +, ?),
	pairfrom(?, ?, ?, ?),
	intersect(+, +),
	subset(+, +),
	seteq(+, +),
	list_to_set(+, ?),
	power_set(?, ?),
	    ps(+, ?),
		ps(+, +, +, ?),
	intersect(+, +, ?),		% obsolete
	intersection(+, +, ?),
	intersection(+, ?),
	    intersection1(+, +, ?),
	    memberchk_all(+, +),
	setproduct(?, ?, ?),
	    setproduct(?, ?, ?, -),
	subtract(+, +, ?),
	symdiff(+, +, ?),
	    symdiff(+, +, ?, ?),
	union(+, +, ?),
	union(+, ?),
	    union1(+, ?),
		union2(+, ?),
	union(+, _, ?, ?).

/* pred
	add_element(T, list(T), list(T)),
	del_element(T, list(T), list(T)),
	disjoint(list(T), list(T)),
	disjoint_union(list(T), list(T), list(T)),
	is_set(list(T)),
	select(T, list(T), list(T)),
	selectchk(T, list(T), list(T)),
	pairfrom(list(T), T, T, list(T)),
	intersect(list(T), list(T)),
	subset(list(T), list(T)),
	seteq(list(T), list(T)),
	list_to_set(list(T), list(T)),
	power_set(list(T), list(list(T))),
	    ps(list(T), list(list(T))),
		ps(list(list(T)), T, list(list(T)), list(list(T))),
	intersect(list(T), list(T), list(T)),
	intersection(list(T), list(T), list(T)),
	intersection(list(list(T)), list(T)),
	    intersection1(list(T), list(list(T)), list(T)),
	subtract(list(T), list(T), list(T)),
	symdiff(list(T), list(T), list(T)),
	    symdiff(list(T), list(T), list(T), list(T)),
	setproduct(list(T), list(U), list(pair(T,U))),
	    setproduct(list(U), T, list(pair(T,U))),
	union(list(list(T)), list(T)),
	    union1(list(list(T)), list(T)),
		union2(list(T), list(T)),
	union(list(T), list(T), list(T)),
	union(list(T), list(T), list(T), list(T)).
*/




%   add_element(+Element, +Set1, ?Set2)
%   is true when Set1 and Set2 are sets represented as unordered lists,
%   and Set2 = Set1 U {Element}.  It may only be used to calculate Set2
%   given Element and Set1.  However, if Set1 is a partial list, there
%   is an unpleasant hack using add_element(Element, Set1, Set1) which
%   adds new Elements at the end of Set1.

add_element(Element, Set1, Set2) :-
	memberchk(Element, Set1),
	!,
	Set2 = Set1.
add_element(Element, Set1, [Element|Set1]).



%   del_element(+Element, +Set1, ?Set2)
%   is true when Set1 and Set2 are sets represented as unordered lists,
%   and Set2 = Set1 \ {Element}.  It may only be used to calculate Set2
%   given Element and Set1.  If Set1 does not contain Element, Set2 will
%   be identical to Set1 (the old version made a new copy of Set1).  If
%   Set1 is not an unordered set, but contains more than one copy of
%   Element, only the first will be removed.  If you want to delete all
%   copies of a given element, use delete/3 from library(lists).  For a
%   version which fails if Element is not in Set1, use selectchk/3.

del_element(Element, Set1, Set2) :-
	selectchk(Element, Set1, Result),
	!,
	Set2 = Result.
del_element(_, Set1, Set1).



%   disjoint(+Set1, +Set2)
%   is true when the two given sets have no elements in common.
%   It is the opposite of intersect/2.  If either of the arguments
%   is improper, disjoint/2 will fail.  Note that the success of
%   disjoint/2 does not entail Set1 and Set2 being lists: the goal
%   disjoint(1,2) succeeds.  disjoint/2 is only defined for lists.

disjoint(Set1, Set2) :-
	member(Element, Set1),
	memberchk(Element, Set2),
	!, fail.
disjoint(_, _).



%   is_set(+List)
%   is true when List is a proper list that contains no repeated elements.
%   That, is, List represents a set in the style used by this package.
%   See the description of nonmember/2 for some restrictions.  The way we
%   test for List being proper is rather curious: if it ends with a
%   variable the call to nonmember/2 must fail, which is the reason for
%   the odd clause order and the cut in the first clause.

is_set([Head|Tail]) :- !,
	nonmember(Head, Tail),
	is_set(Tail).
is_set([]).



%   select(?Element, ?Set, ?Residue)
%   is true when Set is a list, Element occurs in Set, and Residue is
%   everything in Set except Element (things stay in the same order).

select(X, [X|R],     R        ).
select(X, [A,X|R],   [A|R]    ).
select(X, [A,B,X|R], [A,B|R]  ).
select(X, [A,B,C|L], [A,B,C|R]) :-
	select(X, L, R).

/*  The original code was
	select(X, [X|R], R).
	select(X, [H|T], [H|R]) :- select(X, T, R).
    This has been unrolled to save 10-20% of the time.
    It would be nice if select/3 were in library(basics), but we're
    stuck with it now.  Ah, hindsight.
*/



%   selectchk(+Element, +Set, ?Residue)
%   is to select/3 what memberchk/2 is to member/2.  That is, it locates
%   the first occurrence of Element in Set, and deletes it, giving Residue.
%   It is steadfast in Residue.

selectchk(X, [X|R],     Residue) :- !, Residue = R.
selectchk(X, [A,X|R],   Residue) :- !, Residue = [A|R].
selectchk(X, [A,B,X|R], Residue) :- !, Residue = [A,B|R].
selectchk(X, [A,B,C|L], [A,B,C|R]) :-
	selectchk(X, L, R).

/*  The original code, had it existed, would have been
	selectchk(X, [X|T], R) :- !, R = T.
	selectchk(X, [H|T], [H|R]) :- selectchk(X, T, R).
    This has been unrolled, in order to make del_element/3 faster.
*/



%   pairfrom(?Set, ?Element1, ?Element2, ?Residue)
%   is true when Set is a list, Element1 occurs in list, Element2
%   occurs in list after Element1, and Residue is everything in Set
%   bar the two Elements.  The point of this thing is to select
%   pairs of elements from a set without selecting the same pair
%   twice in different orders.
%   This can be used to select two elements from a given Set or to
%   insert two elements into a given Residue, but if neither Set
%   nor Residue is proper you're in trouble.  You could diagonalise
%   by doing Set = [_,_|L], same_length(L, Residue), and then
%   calling pairfrom.  We could do that here, but for the two uses
%   that are intended it is not necessary.

pairfrom([Element1|Set], Element1, Element2, Residue) :-
	select(Element2, Set, Residue).
pairfrom([Head|Tail], Element1, Element2, [Head|Rest]) :-
	pairfrom(Tail, Element1, Element2, Rest).



%   intersect(+Set1, +Set2)
%   is true when the two sets have a member in common.  It assumes
%   that both sets are known, and that you don't care which element
%   it is that they share.  If either argument is partial, intersect/2
%   will succeed: this isn't always right.  You should ensure that the
%   arguments are proper lists.

intersect(Set1, Set2) :-
	member(Element, Set1),		%  generates Elements from Set1
	memberchk(Element, Set2),	%  tests them against Set2
	!.				%  if it succeeds once, is enough.



%   subset(+Set1, +Set2)
%   is true when each member of Set1 occurs in Set2.
%   It can only be used to test two given sets; it cannot be used
%   to generate subsets.  There is no predicate for generating
%   subsets as such, but the predicates subseq/3,subseq0/2,subseq1/2
%   in library(lists) may do what you want (they preserve the order
%   of elements within a list).

subset([], _).
subset([Element|Elements], Set) :-
	memberchk(Element, Set),
	subset(Elements, Set).



%   seteq(+Set1, +Set2)
%   is true when each Set is a subset of the other.  There are two
%   ways of doing this.  The fast one is commented out.

seteq(Set1, Set2) :-
	subset(Set1, Set2),
	subset(Set2, Set1).
%	sort(Set1, Standard),
%	sort(Set2, Standard).



%   list_to_set(+List, ?Set)
%   is true when List and Set are lists, and Set has the same elements
%   as List in the same order, except that it contains no duplicates.
%   The two are thus equal considered as sets.  If you really want to
%   convert a list to a set, list_to_ord_set is faster, but this way
%   preserves as much of the original ordering as possible.
%   If List contains several copies of an element X, only the LAST
%   copy of X is retained.  If you want to convert a List to a Set,
%   retaining the FIRST copy of repeated elements, call
%	symdiff([], List, Set)

list_to_set([], []).
list_to_set([Head|Tail], Set) :-
	memberchk(Head, Tail),
	!,
	list_to_set(Tail, Set).
list_to_set([Head|Tail], [Head|Set]) :-
	list_to_set(Tail, Set).



%   power_set(?Set, ?PowerSet)
%   is true when Set is a list and PowerSet is a list of lists which
%   represents the power set of the set that Set represents.  The
%   particular representation of the power set chosen has this defining
%   property: if A subset-of B subset-of Set, then B appears *BEFORE*
%   A in PowerSet.  In particular, the first element of PowerSet must
%   be Set itself, and the last element of PowerSet must be [].  As an
%   example, power_set([a,b], X) binds X=[[a,b],[a],[b],[]].
%   Note that length(PowerSet) = 2**length(Set), so that for Sets with
%   more than about 18 elements, this isn't a very practical operation.

power_set(Set, [Set|Rest]) :-
	ps(Set, [Set|Rest]).


ps([], [[]]).
ps([Head|Tail], ListPow) :-
	ps(Tail, TailPow),
	ps(TailPow, Head, TailPow, ListPow).


ps([], _, ListPow, ListPow).
ps([Subset|Subsets], Element, TailPow, [[Element|Subset]|ListPow]) :-
	ps(Subsets, Element, TailPow, ListPow).



%   intersection(+Set1, +Set2, ?Intersection)
%   is true when all three arguments are lists representing sets,
%   and Intersection contains every element of Set1 which is also
%   an element of Set2, the order of elements in Intersection
%   being the same as in Set1.  That is, Intersection represents
%   the intersection of the sets represented by Set1 and Set2.
%   If Set2 is a partial list, Intersection will be empty, which
%   is not, of course, correct.  If Set1 is a partial list, this
%   predicate will run away on backtracking.  Set1 and Set2 should
%   both be proper lists, but this is not checked.  Duplicates in
%   Set1 may survive in Intersection.  It is worthy of note that
%   if Set1 is an ordset, Intersection is an ordset, despite Set2.

intersection([], _, []).
intersection([Element|Elements], Set, Intersection) :-
	memberchk(Element, Set),
	!,
	Intersection = [Element|Rest],
	intersection(Elements, Set, Rest).
intersection([_|Elements], Set, Intersection) :-
	intersection(Elements, Set, Intersection).



%   intersection(+ListOfSets, ?Intersection)
%   is true when Intersection is the intersection of all the sets in
%   ListOfSets.  The order of elements in Intersection is taken from
%   the first set in ListOfSets.  This has been turned inside out to
%   minimise the storage turnover.

intersection([Set|Sets], Intersection) :-
	intersection1(Set, Sets, Intersection).

intersection1([], _, []).
intersection1([Element|Elements], Sets, Intersection) :-
	memberchk_all(Sets, Element),
	!,
	Intersection = [Element|Rest],
	intersection1(Elements, Sets, Rest).
intersection1([_|Elements], Sets, Intersection) :-
	intersection1(Elements, Sets, Intersection).

memberchk_all([], _).
memberchk_all([Set|Sets], Element) :-
	memberchk(Element, Set),
	memberchk_all(Sets, Element).


%   intersect(+Set1, +Set2, ?Intersection)
%   is an obsolete synonym of intersection/3.

intersect(Set1, Set2, Intersection) :-
	intersection(Set1, Set2, Intersection).



%   subtract(+Set1, +Set2, ?Difference)
%   is like intersect, but this time it is the elements of Set1 which
%   *are* in Set2 that are deleted.  Note that duplicated Elements of
%   Set1 which are not in Set2 are retained in Difference.

subtract([], _, []).
subtract([Element|Elements], Set, Difference) :-
	memberchk(Element, Set),
	!,
	subtract(Elements, Set, Difference).
subtract([Element|Elements], Set, [Element|Difference]) :-
	subtract(Elements, Set, Difference).



%   symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2,
%   that is, if each element of Union occurs in one of Set1 and Set2 but
%   not both.  The construction method is such that the answer will have
%   no duplicates even if the Sets do.

symdiff(Set1, Set2, Difference) :-
	symdiff(Set1, Set2, Difference, Difference1),
	symdiff(Set2, Set1, Difference1, []).

symdiff([], _, Diff, Diff).
symdiff([Elem|Rest], Avoid, Diff0, Diff) :-
	memberchk(Elem, Avoid),
	!,
	symdiff(Rest, Avoid, Diff0, Diff).
symdiff([Elem|Rest], Avoid, [Elem|Diff1], Diff) :-
	symdiff(Rest, [Elem|Avoid], Diff1, Diff).



%   setproduct(Set1, Set2, CartesianProduct)
%   is true when Set1 is a set (list) and Set2 is a set (list) and
%   CartesianProduct is a set of Elt1-Elt2 pairs, with a pair for
%   for each element Elt1 of Set1 and Elt2 of Set2.

setproduct([], _, []).
setproduct([H|T], L, Product) :-
	setproduct(L, H, Product, Rest),
	setproduct(T, L, Rest).

setproduct([], _, L, L).
setproduct([H|T], X, [X-H|TX], TL) :-
	setproduct(T, X, TX, TL).



%   disjoint_union(+Set1, +Set2, ?Union)
%   is true when disjoint(Set1, Set2) and union(Set1, Set2, Union),
%   that is, Set1 and Set2 have no element in command and Union is
%   their union.

disjoint_union([], Union, Union).
disjoint_union([Element|Elements], Set, [Element|Union]) :-
	nonmember(Element, Set),
	disjoint_union(Elements, Set, Union).



%   union(+Set1, +Set2, ?Union)
%   is true when subtract(Set1,Set2,Diff) and append(Diff,Set2,Union),
%   that is, when Union is the elements of Set1 that do not occur in
%   Set2, followed by all the elements of Set2.

union([], Union, Union).
union([Element|Elements], Set, Union) :-
	memberchk(Element, Set),
	!,
	union(Elements, Set, Union).
union([Element|Elements], Set, [Element|Union]) :-
	union(Elements, Set, Union).



%   union(+Set1, +Set2, ?Union, ?Difference)
%   is true when union(Set1, Set2, Union) and subtract(Set1, Set2, Difference).
%   This was added to keep sets.pl and ordsets.pl parallel.

union([], Union, Union, []).
union([Element|Elements], Set, Union, Difference) :-
	memberchk(Element, Set),
	!,
	union(Elements, Set, Union, Difference).
union([Element|Elements], Set, [Element|Union], [Element|Difference]) :-
	union(Elements, Set, Union, Difference).



%   union(+ListOfSets, ?Union)
%   is true when Union is the union of all the sets in ListOfSets.
%   It has been arranged with storage turnover in mind.

union(Sets, Union) :-
	union1(Sets, Answer),
	append(Answer, [], Answer),	% cauterise it
	!,
	Union = Answer.

union1([], _).
union1([Set|Sets], Answer) :-
	union2(Set, Answer),
	union1(Sets, Answer).

union2([], _).
union2([Element|Elements], Answer) :-
	memberchk(Element, Answer),	% add_element hack
	union2(Elements, Answer).


