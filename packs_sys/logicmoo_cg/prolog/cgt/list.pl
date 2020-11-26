/* AUTHOR(S) ************************************************************

Michel Wermelinger
Dept. de Informatica, Univ. Nova de Lisboa, Quinta da Torre
P - 2825 Monte da Caparica, PORTUGAL
Phone: (+351) (1) 295 44 64 ext. 1360  Internet: mw@fct.unl.pt

************************************************************************/

/* GENERALITIES *********************************************************
 
File Name	: LIST.PL
Creation Date	: 90/06/16 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Utility predicates on lists and sets (lists without
		  duplicates; the order of the elements doesn't matter)
 
************************************************************************/

/* HISTORY **************************************************************

1.01	92/03/20  mw	added nth_delete

************************************************************************/

/* CONTENTS *************************************************************
 
conc/3		concatenates two lists 
delete_all/3	deletes from a list all occurrences of a given element 
delete_dup/2	deletes all duplicate occurrences of every list member
delete_eq/2	the same as delete_dup/2 but uses == instead of =
delete_one/3	deletes from a list a single occurrence of a given element
is_list/1       succeeds iff argument is a list
last/2		computes the last element of a list 
length/2	computes the number of elements of a list
list_of/2	succeeds if all elements are equal
member/2	tests for list membership 
nth_delete/3    deletes nth member of a list
nth_member/3	permits the use of a list as an array
reverse/2	reverses the order of the list elements

difference/3	computes the difference of two sets
intersection/3	computes the common members of two sets
subset/2	tests if the members of a list are contained in another one
union/3		computes the union of two sets

apply/2		applies a given predicate to every list element
map/3		applies a given predicate to map a list into another one 

************************************************************************/

/************************************************************************

			L I S T   P R E D I C A T E S

************************************************************************/

/* conc/3 ***************************************************************

Usage		: conc(?List1, ?List2, ?List3)
Argument(s)	: lists
Description	: succeeds iff List3 is the concatenation of List1 and List2
Notes		: at least two of the three arguments must be given 

************************************************************************/

conc([H|T], L, [H|R]) :- 
	conc(T, L, R).
conc([], L, L).

/* delete_all/3 *********************************************************

Usage		: delete_all(+Element, +List, ?NewList)
Argument(s)	: term and lists
Description	: NewList is List but with no occurrences of Element
Notes		: 

************************************************************************/

delete_all(H, [H|T], T2) :-
	delete_all(H, T, T2).
delete_all(X, [H|T], [H|T2]) :-
	delete_all(X, T, T2).
delete_all(_, [], []) :-
	!.

/* delete_dup/2 *********************************************************

Usage		: delete_dup(+List, ?NewList)
Argument(s)	: lists
Description	: NewList has the same members as List but without duplicates
Notes		: 

************************************************************************/

delete_dup([H|T1], [H|T3]) :-
	delete_all(H, T1, T2),
	delete_dup(T2, T3).
delete_dup([], []).

/* delete_eq/2 **********************************************************

Usage		: delete_eq(+List, ?NewList)
Argument(s)	: lists
Description	: the same as delete_dup/2 but two elements are duplicates
		  iff they are equal, not if they just unify 
Notes		: 

************************************************************************/

delete_eq([H|T], L) :-
	member(X, T), X == H, delete_eq(T, L).
delete_eq([H|T], [H|L]) :-
	delete_eq(T, L).
delete_eq([], []) :-
	!.

/* delete_one/2 *********************************************************

Usage		: delete_one(+Element, +List, ?NewList)
Argument(s)	: 		term	list	list
Description	: deletes one single occurrence of Element from List
		  returning NewList 
Notes		: succeeds always, even if Element is not member of List

************************************************************************/

delete_one(H, [H|T], T).
delete_one(H, [X|T], [X|R]) :-
	delete_one(H, T, R).
delete_one(_, [], []).

/* is_list/1 ************************************************************

Usage		: is_list(+List)
Argument(s)	: term
Description	: succeeds iff List is a valid Prolog list
Notes		: 

************************************************************************/

%is_list([]).
%is_list([_|_]).

/* last/2 ***************************************************************

Usage		: last(+List, ?Element)
Argument(s)	: 	list	term
Description	: succeeds iff Element is the last element of List
Notes		: 

************************************************************************/
:- if( \+ current_predicate(last/1)).

last(L, X) :-
	conc(_, [X], L).

:- endif.

/* length/2 *************************************************************

Usage		: length(+List, ?Length)
Argument(s)	: 	  list	integer
Description	: succeeds iff Length is the number of elements of List
Notes		: 

************************************************************************/
:- if( \+ current_predicate(length/2)).

length([_|T], Y) :-
	length(T, X), succ(X, Y).
length([], 0).

:- endif.

/* list_of/2 ************************************************************

Usage		: list_of(?List, ?Element)
Argument(s)	: 	   list	   term
Description	: succeeds iff all members of List unify with Element
Notes		: at least one of the arguments must be instantiated

************************************************************************/

list_of([], _).
list_of([H|T], H) :- list_of(T, H).

/* member/2 *************************************************************

Usage		: member(?Element, +List)
Argument(s)	: 	   term	    list
Description	: succeeds iff Element unifies with a member of List
Notes		: if -Element, generates all list members by backtracking 

************************************************************************/
/*
member(H, [H|_]).
member(H, [_|T]) :-
	member(H, T).

*/

/* nth_member/2 *********************************************************

Usage		: nth_member(?Element, ?List, ?N)
Argument(s)	: 	       term	list  integer
Description	: succeeds iff the N-th member of List is Element
Notes		: at least two of the predicates must be instantiated
		  the first element of List has position 1, not 0

************************************************************************/

nth_member(H, [H|_], 1).
nth_member(H, [_|T], N) :-
	nth_member(H, T, N0), succ(N0, N).

/* nth_delete/3 *********************************************************

Usage		: nth_delete(+List1, +Number, ?List2)
Argument(s)	: 	      list   integer  list
Description	: succeeds iff List2 is List1 without its N-th member
Notes		: 

************************************************************************/

nth_delete([_|T], 1, T).
nth_delete([_|T], N, R) :-
	N0 is N - 1, nth_delete(T, N0, R).

/* reverse/2 ************************************************************

Usage		: reverse(+List, ?ReversedList)
Argument(s)	: lists
Description	: succeeds iff ReversedList and List have the same members
		  but in opposite order
Notes		: 

************************************************************************/

reverse(List, Rev) :-
   	rev(List, [], Rev).

rev([H|T], WorkList, Rev) :-
   	rev(T, [H|WorkList], Rev).
rev([], Rev, Rev).

/************************************************************************

			  S E T   P R E D I C A T E S

************************************************************************/

/* difference/3 *********************************************************

Usage		: difference(+Set1, +Set2, ?Diff)
Argument(s)	: lists
Description	: Diff contains all members of Set1 which aren't in Set2
Notes		: 

************************************************************************/

difference([H|T], L, D) :- 
	member(H, L), difference(T, L, D).
difference([H|T], L, [H|R]) :- 
	difference(T, L, R).
difference([], _, []).

/* intersection/3 *******************************************************

Usage		: intersection(+Set1, +Set2, ?Intersection)
Argument(s)	: lists
Description	: Intersection contains the common members of Set1 and Set2
Notes		: 

************************************************************************/

intersection([H|L1], L2, L3) :-
	member(H, L2), !, 
	intersection(L1, L2, L4), L3 = [H|L4].
intersection([_|L1], L2, L3) :-
	intersection(L1, L2, L3).
intersection([], _, []).

/* subset/2 *************************************************************

Usage		: subset(+SubSet, +Set)
Argument(s)	: lists
Description	: succeeds iff all members of Subset are members of Set
Notes		: 

************************************************************************/

subset([H|T], S) :- 
	member(H, S),
	subset(T, S).
subset([], _).

/* union/3 **************************************************************

Usage		: union(+Set1, +Set2, ?Union)
Argument(s)	: sets (lists)
Description	: Union has all members of Set1 and Set2 but without
		  duplicates 
Notes		: 

************************************************************************/

union([H|X], Y, Z) :- 
	member(H, Y), union(X, Y, Z).
union([H|X], Y, [H|Z]) :- 
	union(X, Y, Z).
union([], L, L).

/************************************************************************

			  M E T A - P R E D I C A T E S

************************************************************************/

/* apply/2 **************************************************************

Usage		: apply(+Predicate, +List)
Argument(s)	: 	  atom	     list
Description	: succeeds iff Predicate can be applied successfully to 
		  all members of List
Notes		: Predicate(+Term, ...) must exist

************************************************************************/

apply(P, [H|T]) :-
	P =.. [F, _|A], G =.. [F, H|A], call(G), apply(P, T).
apply(_, []).
	
/* map/2 ****************************************************************

Usage		: map(+Predicate, +List, ?NewList)
Argument(s)	: atom and lists
Description	: NewList is obtained by applying Predicate to all
		  members of List
Notes		: Predicate(+Term1, ?Term2, ...) must exist

************************************************************************/

map(P, [X|L1], [Y|L2]) :- 
	P =.. [F, _, Arg2|T], copy_term(Arg2, Y), G =.. [F, X, Y|T],
        call(G), map(P, L1, L2).
map(_, [], []).
