/* Part of dcgutils
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(snobol, [
		any//1
   ,  notany//1
   ,  arb//0
   ,  arbno//1
   ,  bal//1
	,	span//1
   ,  break//1
   ,  len//1
   ,  rem//0
   ,  ($)//2
   ,  op(400,yfx,$)
   ]).


/** <module> SNOBOL-inspired DCG operators

   NB.
   FAIL is just {fail} or dcg_core:fail
   SUCCEED is {repeat} or dcg_core:repeat.
   FENCE is ! (cut).

   Sequence capture in SNOBOL ($) is also $ here: use Phrase $ List to
   capture the sequence matched by Phrase in the List.

   ABORT cannot be implemented in plain Prolog because there is no
   ancestral cut operator. Instead abort//0 just throws an exception
   which you must arrange to catch yourself.

   POS, RPOS, TAB and RTAB are not context-free rules and can only be
   implemented in paired-state DCG which counts the current position in
   the string.
*/

:- meta_predicate arbno(//,?,?), $(//,?,?,?).

% SNOBOL4ish rules
%
%	Others:
%		maxarb
%		pos rpos
%		tab rtab


%% Phrase $ List is nondet.
%  True when Phrase is true and List is the sequence of
%  terminals matched by it.
$(P,L,S1,S2) :- phrase(P,S1,S2), dlist(L,S1,S2).

% need to be careful with difference lists...
dlist(Cs,L1,L2) :- is_list(Cs), !, append(Cs,L2,L1).
dlist([],L1,L2) :- L1==L2, !.
dlist([C|Cs],L1,L3) :- must_be(nonvar,L1), L1=[C|L2], dlist(Cs,L2,L3).

%% rem// is det.
rem(_,[]).

%% abort// is det.
abort(_,_) :- throw(abort).

%% any(+L:list(_))// is nondet.
%  Matches any element of L.
any(L)    --> [X], {member(X,L)}.

%% notany(+L:list(_))// is nondet.
%  Matches anything not in L.
notany(L) --> [X], {maplist(dif(X),L)}.

%% arb// is nondet.
%  Matches an arbitrary sequence. Proceeds cautiously.
arb       --> []; [_], arb.

%% arbno(+P:phrase)// is nondet.
%  Matches an arbitrary number of P. Proceeds cautiously.
%  Any variables in P are shared across calls.
arbno(P)  --> []; call_dcg(P), arbno(P).

%% span(+L:list(_))// is nondet.
%  Matches the longest possible sequence of symbols from L.
span(L) --> any(L), span_tail(L).
span_tail(_, [], []).
span_tail(L) --> span(L).
span_tail(L, [X|T], [X|T]) :- maplist(dif(X), L).

%% break(+L:list(_))// is nondet.
%  Matches the longest possible sequence of symbols not in L.
break(_, [], []).
break(L) --> notany(L), break(L).
break(L, [X|T], [X|T]) :- member(X, L).

%% len(+N:natural)// is det.
%% len(-N:natural)// is nondet.
%  Matches any N symbols.
len(N, L1, L2) :- length(L, N), append(L, L2, L1).

%% bal(+Delims:list(C))// is nondet.
%  Matches any expression with balanced generalised parentheses.
%  The opening and closing parenthesis must be supplied as a list
%  of terminals [Open,Close].
bal(Delims) --> bal_one(Delims), arbno(bal_one(Delims)).
bal_one(Delims) --> {Delims=[O,C]}, [O], bal(Delims), [C].
bal_one(Delims) --> notany(Delims).
