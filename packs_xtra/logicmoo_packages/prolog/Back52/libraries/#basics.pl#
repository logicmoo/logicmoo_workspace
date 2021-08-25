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

%   Module : basics
%   Author : by now, nobody knows
%   Updated: 4/30/90
%   Defines: the basic list processing predicates

%   Adapted from shared code written by the same author(s); all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(basics, [
	%append/3,		% append/3 has become a built-in predicate
	member/2,
	memberchk/2,
	nonmember/2
   ]).

:- mode
	%append(?, ?, ?),	% append/3 has become a built-in predicate
	member(?, ?),
	memberchk(+, +),
	nonmember(+, +).

/* pred
	append(list(T), list(T), list(T)),
	member(T, list(T)),
	memberchk(T, list(T)),
	nonmember(T, list(T)).
*/

sccs_id('"@(#)90/04/30 basics.pl	41.2"').


%   append(?Prefix, ?Suffix, ?Combined)
%   is true when all three arguments are lists, and the members of Combined
%   are the members of Prefix followed by the members of Suffix.  It may be
%   used to form Combined from a given Prefix and Suffix, or to take a given
%   Combined apart.  For example, we could define member/2 as
%	member(X, L) :- append(_, [X|_], L).
%
%   With Release 3.0 of Quintus Prolog, append/3 has become a built-in
%   predicate.
%
% append([], L, L).
% append([H|T], L, [H|R]) :-
% 	append(T, L, R).



%   member(?Element, ?Set)
%   is true when Set is a list, and Element occurs in it.  It may be used
%   to test for an element or to enumerate all the elements by backtracking.
%   Indeed, it may be used to generate the Set!

member(X, [X|_]    ).
member(X, [_,X|_]  ).
member(X, [_,_,X|_]).
member(X, [_,_,_|L]) :-
	member(X, L).

/*  The original definition was
	member(X, [X|_]).
	member(X, [_|L]) :- member(X, L).
    We have unrolled that, to save 10-20% of the time.
*/



%   memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs in a known Set.  In return for this limited use, it
%   is more efficient than member/2 when it is applicable.

memberchk(X, [X|_]    ) :- !.
memberchk(X, [_,X|_]  ) :- !.
memberchk(X, [_,_,X|_]) :- !.
memberchk(X, [_,_,_|L]) :-
	memberchk(X, L).

/*  The original definition was
	memberchk(X, [X_|]) :- !.
	memberchk(X, [_|L]) :- memberchk(X, L).
    We have unrolled that, to save 10-20% of the time.
*/



%   nonmember(+Element, +Set)
%   means that Element does not occur in Set.  It does not make sense
%   to instantiate Element in any way, as there are infinitely many
%   terms which do not occur in any given set.  Nor can we generate
%   Set; there are infinitely many sets not containing a given Element.
%   Read it as "the given Element does not occur in the given list Set".
%   This code was suggested by Bruce Hakami; seven versions of this
%   operation were benchmarked and this found to be the fastest.
%   The old code was for DEC-10 Prolog, which did not compile (\+)/1.

nonmember(Element, Set) :-
	\+ member(Element, Set).

/*  The code we would really like to have is
	nonmember(_, []).
	nonmember(X, [E|L]) :- X ~= E, nonmember(X, L).
    which would be sound.
*/


