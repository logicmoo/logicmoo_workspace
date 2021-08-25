/**********************************************************************
 *
 * @(#) dynamicDef.pl 1.16@(#)
 *
 */


% !! Remember: Any changes to the following list should be carefully
%              reflected in     clearEnvironment
%                       and     saveEnvironment.

% The following predicates belong to the translated terminologial 
% axioms.
:- multifile(in/9).
:- dynamic(in/9).
:- multifile(kb_in/10).
:- dynamic(kb_in/10).
:- multifile(eq/9).
:- dynamic(eq/9).
:- multifile(constraint/8).
:- dynamic(constraint/8).
:- multifile(rel/5).
:- dynamic(rel/5).
% The following predicates are used for additional informations about
% the terminology and the world description.
:- multifile(axiom/3).
:- dynamic(axiom/3).
:- multifile(closed/5).
:- dynamic(closed/5).
:- multifile(compiledPredicate/2).
:- dynamic(compiledPredicate/2).
:- multifile(conceptElement/7).
:- dynamic(conceptElement/7).
:- multifile(conceptEqualSets/6).
:- dynamic(conceptEqualSets/6).
:- multifile(conceptHierarchy/3).
:- dynamic(conceptHierarchy/3).
:- multifile(conceptName/4).
:- dynamic(conceptName/4).
:- multifile(conceptSubsets/6).
:- dynamic(conceptSubsets/6).
:- multifile(environment/3).
:- dynamic(environment/3).
:- multifile(given_change/4).
:- dynamic(given_change/4).
:- multifile(given_inflLink/4).
:- dynamic(given_inflLink/4).
:- multifile(modalAxioms/6).
:- dynamic(modalAxioms/6).
:- multifile(roleAttributes/5).
:- dynamic(roleAttributes/5).
:- multifile(roleDefault/4).
:- dynamic(roleDefault/4).
:- multifile(roleDefNr/4).
:- dynamic(roleDefNr/4).
:- multifile(roleDomain/4).
:- dynamic(roleDomain/4).
:- multifile(roleElement/8).
:- dynamic(roleElement/8).
:- multifile(roleEqualSets/6).
:- dynamic(roleEqualSets/6).
:- multifile(roleHierarchy/3).
:- dynamic(roleHierarchy/3).
:- multifile(roleName/4).
:- dynamic(roleName/4).
:- multifile(roleNr/5).
:- dynamic(roleNr/5).
:- multifile(roleRange/4).
:- dynamic(roleRange/4).
:- multifile(roleSubsets/6).
:- dynamic(roleSubsets/6).
:- multifile(sub/4).
:- dynamic(sub/4).
:- multifile(succ/4).
:- dynamic(succ/4).
% The following predicates are used during computations only.
:- multifile(abductiveDerivation/3).
:- dynamic(abductiveDerivation/3).
:- multifile(consistencyDerivation/3).
:- dynamic(consistencyDerivation/3).
:- multifile(hypothesis/1).
:- dynamic(hypothesis/1).
:- multifile(inconsistencyCheck/3).
:- dynamic(inconsistencyCheck/3).
:- multifile(option/2).
:- dynamic(option/2).
:- multifile(nsub/4).
:- dynamic(nsub/4).
:- multifile(nsub3/2).
:- dynamic(nsub3/2).
:- multifile(sub3/2).
:- dynamic(sub3/2).
:- multifile(succ3/2).
:- dynamic(succ3/2).
:- multifile(value/2).
:- dynamic(value/2).
% Predicates which are no longer needed
%:- multifile(falsum/2).
%:- dynamic(falsum/2).
%:- multifile(numb/1).
%:- dynamic(numb/1).
:- op(1200,xfx,<-).
/**********************************************************************
 *
 * @(#) sets.pl 1.1@(#)
 *
 */

%   member(?Element, ?Set)
%   is true when Set is a list, and Element occurs in it.  It may be used
%   to test for an element or to enumerate all the elements by backtracking.
%   Indeed, it may be used to generate the Set!

member(X, [X|_]    ).
member(X, [_,X|_]  ).
member(X, [_,_,X|_]).
member(X, [_,_,_|L]) :-
        member(X, L).

%   reverseList(+List1,-List2
%   reverses the list List1 to get List2

reverseList([],[]) :- !.
reverseList([H|T],L2) :-
	reverseList(T,L1),
	append(L1,[H],L2),
	!.

%   memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs in a known Set.  In return for this limited use, it
%   is more efficient than member/2 when it is applicable.

memberchk(X, L) :- 
	nonvar(X), 
	nonvar(L),
	memberchk1(X,L).

memberchk1(X, [X|_]    ) :- !.
memberchk1(X, [_,X|_]  ) :- !.
memberchk1(X, [_,_,X|_]) :- !.
memberchk1(X, [_,_,_|L]) :-
	memberchk1(X, L).

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
	nonvar(Element),
	nonvar(Set),
	not(member(Element, Set)).

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

%   union(+ListOfSets, ?Union)
%   is true when Union is the union of all sets in ListOfSets.

union([],[]).
union([Set1],Set1).
union([Set1,Set2|Sets],Union) :-
	union(Set1,Set2,Set),
	union([Set|Sets],Union).


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


%   deleteInList(+List, +Kill, ?Residue)
%   is true when List is a list, in which Kill may or may not occur, and
%   Residue is a copy of List with all elements equal to Kill deleted.
%   To extract a single copy of Kill, use select(Kill, List, Residue).
%   If List is not proper, deleteInList/3 will FAIL.  Kill and the elements of
%   List should be sufficiently instantiated for \= to be sound.

deleteInList(-, _, _) :- !, fail.		% reject partial lists
deleteInList([], _, []).
deleteInList([Kill|Tail], Kill, Residue) :- !,
	deleteInList(Tail, Kill, Residue).
deleteInList([Head|Tail], Kill, [Head|Residue]) :-
    %	Head \= Kill,
	deleteInList(Tail, Kill, Residue).


subset([],_S2) :- !.
subset([E1|S1],S2) :-
	not(not(member(E1,S2))),
	subset(S1,S2),
	!.

equalset(S1,S2) :-
	subset(S1,S2),
	subset(S2,S1),
	!.

%----------------------------------------------------------------------	
%   Module : lists
%   Authors: Bob Welham, Lawrence Byrd, and Richard A. O'Keefe
%   Updated: 10/25/90
%   Defines: list processing utilities
%   SeeAlso: library(flatten)

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

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



/**********************************************************************
 *
 * @(#) lib.pl 1.7@(#)
 *
 */

/* POPLOG PROLOG LIBRARIES
 */

/* Library gensym
 * import:  gensym(_,_)
 */

/* QUINTUS PROLOG LIBRARIES
 */

/* Library strings
 * import:  gensym(_,_)
 */


/**********************************************************************
 *
 * COUNTER
 *
 */

/**********************************************************************
 *
 * setCounter(+Counter,+Value)
 * creates a new counter Counter with value Value.
 *
 */

setCounter(Counter,N) :-
        asserta(value(Counter,N)),
        !.

/**********************************************************************
 *
 * addCounter(+Counter,+Value)
 * adds Value to the current value of counter Counter.
 *
 */
 
addCounter(Counter,N) :-
        retract(value(Counter,M)),
        Sum is N + M,
        asserta(value(Counter,Sum)),
        !.

/**********************************************************************
 *
 * getCounter(+Counter,-Value)
 * retrieves the current value Value of counter Counter.
 *
 */

getCounter(Counter,N) :-
        value(Counter,N),
        !.

/**********************************************************************
 *
 * writes(+List)
 * put each character in List.
 *
 */

writes([]) :- !.
writes([H|T]) :- put(H), writes(T).

/***********************************************************************
 *
 * printTime(+G)
 * execute goal G and report the runtime the execution needed.
 * Only available for SICStus Prolog and Quintus Prolog.
 *
 */

printTime(G) :-
	(currentProlog(poplog) ; currentProlog(macprolog)),
	!,
	call(G),
	!.
printTime(G) :-
	!,
	getRuntime(T0),
	printTime(G,T0).

printTime(G,T0) :-
	call(G),
	getRuntime(T1),
	T is T1 - T0,
	format('Total runtime ~3d sec.~n', [T]).
printTime(_,T0) :-
	getRuntime(T1),
	T is T1 - T0,
	format('Total runtime ~3d sec.~n', [T]),
	!,
	fail.

/**********************************************************************
 *
 * simple_term(X) 
 * it contrast to the usage in the Quintus Prolog user manual we
 * call a term `simple` if it is either an atom or a variable.
 * This predicate succeeds iff X is a simple term in this sense.
 *
 */

simple_term(X) :-
	var(X),
	!.
simple_term(X) :-
	atomic(X),
	!.

/**********************************************************************
 *
 * LIBRARY HANDLING
 *
 */

loadLibraries(sicstus) :-
	assertz((gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	name(Prefix,P1),
	name(N,N1),
	append(P1,N1,V1),
	name(V,V1),
	!)),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(runtime,[RT,CT]))),
	assertz((getRuntime(RT) :-
	statistics(runtime,[RT|_]))),
	assertz((append([],L2,L2))),
	assertz((append([A1|L1],L2,[A1|L3]) :-
	append(L1,L2,L3))),
	assertz((not(Goal) :- call(\+ Goal))),
	assertz((once(Goal) :- Goal, !)),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	!.
loadLibraries(eclipse) :-
	assertz((gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	name(Prefix,P1),
	name(N,N1),
	append(P1,N1,V1),
	name(V,V1),
	!)),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(runtime,[RT,CT]))),
	assertz((getRuntime(RT) :-
	statistics(runtime,[RT|_]))),
	assertz((append([],L2,L2))),
	assertz((append([A1|L1],L2,[A1|L3]) :-
	append(L1,L2,L3))),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	!.
loadLibraries(swiprolog) :-
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	assertz((portray(not(F)) :- display(not(F)))),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(cputime,RT1), RT is (ceil(RT1 * 100000)) mod 100000, statistics(atoms,CT))),
	assertz((getRuntime(RT) :-
	statistics(cputime,RT1), RT is ceil(RT1 * 1000))),
	index(kb_in(1,0,0,0,1,1,0,0,0,0)),
	index(eq(1,0,0,1,1,0,0,0,0)),
	index(constraint(1,0,0,1,0,0,0,0)),
	assertz((retractall(Head) :- retract(Head), fail)),
	assertz((retractall(Head) :- retract((Head :- _Body)), fail)),
	assertz((retractall(_))),
	!.
loadLibraries(poplog) :-
	op(600,xfy,':'),
	assertz((gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	name(Prefix,P1),
	name(N,N1),
	append(P1,N1,V1),
	name(V,V1),
	!)),
	assertz((append([],L2,L2))),
	assertz((append([A1|L1],L2,[A1|L3]) :-
	append(L1,L2,L3))),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	assertz((once(Goal) :- Goal, !)),
	assertz((saveMOTEL(F) :- save_program(F))),
	!.
loadLibraries(quintus) :-
	assertz((gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	name(Prefix,P1),
	name(N,N1),
	append(P1,N1,V1),
	name(V,V1),
	!)),
	assertz((getTwoRandomNumbers(RT,CT) :-
	statistics(runtime,[RT,CT]))),
	assertz((getRuntime(RT) :-
	statistics(runtime,[RT|_]))),
	assertz((not(Goal) :- call(\+ Goal))),
	assertz((once(Goal) :- Goal, !)),
	assertz((ask(A1) :- deduce(A1))),
	assertz((ask(A1,A2,A3,A4) :- deduce(A1,A2,A3,A4))),
	assertz((ask(A1,A2) :- deduce(A1,A2))),
	assertz((ask(A1,A2,A3) :- deduce(A1,A2,A3))),
	assertz((map(A1,A2,A3) :- hop_map(A1,A2,A3))),
	assertz((map(A1,A2,A3,A4) :- hop_map(A1,A2,A3,A4))),
	assertz((saveMOTEL(F) :- save_program(F))),
	!.
loadLibraries(macprolog) :-
	op(600,xfy,':'),
	!.

testForMacprolog(others) :-
	current_op(_X,_Y,':'),
	!,
	fail.
testForMacprolog(macprolog) :-
	unknown(_X,fail),
	!.

getLibraries :-
	testForMacprolog(_),
	!,
	asserta(currentProlog(macprolog)),
	version('MOTEL-0.4 Tue Aug 04 15:00:00 MET 1992'),
	loadLibraries(macprolog).
getLibraries :-
	current_op(1190,fx,delay),
	!,
	sicstus,
	asserta(currentProlog(eclipse)),
	set_flag(variable_names,off),
	loadLibraries(eclipse).
getLibraries :-
	current_op(_X,_Y,?),
	style_check(-singleton),
	!,
	asserta(currentProlog(swiprolog)),
	style_check(-discontiguous),
	loadLibraries(swiprolog).
getLibraries :-
	setof((X,Y),prolog_flag(X,Y),L),
	member((single_var,_Z),L),
	!,
	asserta(currentProlog(quintus)),
	version('MOTEL-0.4 Tue Aug 04 15:00:00 MET 1992'),
	prolog_flag(single_var,_,off),
	loadLibraries(quintus).
getLibraries :-
	prolog_flag(_X,_Y),
	!,
	asserta(currentProlog(sicstus)),
	version('MOTEL-0.4 Tue Aug 04 15:00:00 MET 1992'),
	prolog_flag(single_var_warnings,_,off),
	prolog_flag(compiling,_,fastcode),
	prolog_flag(unknown,_,fail),
%	asserta(foreign_file('int.o',[int_init])),
%	asserta(foreign(int_init,int_init)),
%	load_foreign_files(['int.o'],[]),
%	int_init,
	loadLibraries(sicstus).
getLibraries :-
	tell('/tmp/v1'), version, told,
	!,
	asserta(currentProlog(poplog)),
	version('MOTEL-0.4 Tue Aug 04 15:00:00 MET 1992'),
	loadLibraries(poplog).

/**********************************************************************
 *
 * OPTIONS
 *
 */

/***********************************************************************
 *
 * setOption(+Option,+Set)
 * set option Option to value Set.
 *
 */

setOption(Option,Set) :-
	retractall(option(Option,_)),
	asserta(option(Option,Set)),
	!.

/**********************************************************************
 *
 * ifOption(+Option,+Set,+Goal)
 * executes Goal if the current value of Option is Set otherwise
 * the predicate suceeds.
 *
 */

ifOption(Option,Set,Goal) :-
	option(Option,Set),
	call(Goal),
	!.
ifOption(_,_,_) :-
	!.

retractall(Env,Pred/Arity) :-
	constructHead(Env,Pred/Arity,Head),
	retractall(Head), 
	!.

:- getLibraries.




% Copyright (C) 1993 Patrick Brandmeier
%                    Ullrich Hustadt
%                    Renate  Schmidt
%                    Jan     Timm

% This file is part of the MOTEL distribution.

% MOTEL is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 1, or (at your option)
% any later version.

% MOTEL is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
/**********************************************************************
 *
 * @(#) abduction.pl 1.2@(#)
 *
 */

getAbductionHyps(L,[]) :-
	var(L),
	!.
getAbductionHyps([],[]) :-
	!.
getAbductionHyps([in(Env,RN,modal(MS),C,X,A1,A2,A3,A4)|L1],
	[in(Env,RN,modal(MS),C,X,A1,A2,A3,A4)|L2]) :-
	!,
	getAbductionHyps(L1,L2).

doMinimalityCheck(GL1,[in(Env,RN,modal(MS),C,X,_A1,_A2,_A3,_A4)|GL2]) :-
	append(GL1,GL2,GL),
	HYPS = [or(GL),rl([]),fl(H3)],
	constructMLCall(Env,rn(_AX3,_RN3,_S3,_O3),bodyMC(MS),headMC(MS),
	                C,X,HYPS,[],CALLS,_PT35,Goal),
	not(Goal),
	doMinimalityCheck([in(Env,RN,modal(MS),C,X,_A1,_A2,_A3,_A4)|GL1],
	                   GL2),
	!.
doMinimalityCheck(_GL1,[]) :-
	!.
	

doConsistencyCheck(GL1,[in(Env,RN,modal(MS),C,X,_A1,_A2,_A3,_A4)|GL2]) :-
	append(GL1,GL2,GL),
	HYPS = [or(GL),rl([]),fl(H3)],
	normalizeNot(not(C),C1),
	constructMLCall(Env,rn(_AX3,_RN3,_S3,_O3),bodyMC(MS),headMC(MS),
	                C1,X,HYPS,[],CALLS,_PT35,Goal),
	not(Goal),
	doConsistencyCheck([in(Env,RN,modal(MS),C,X,_A1,_A2,_A3,_A4)|GL1],
	                   GL2),
	!.
doConsistencyCheck(_GL1,[]) :-
	!.
	

	
/**********************************************************************
 *
 * @(#) callStack.pl 1.4@(#)
 *
 */

/**********************************************************************
 *
 * THE CALL STACK 
 * is a list of elements of the following form:
 * - true 
 * - in(rn(AX,RN,_,_),modal(MS),C,X,hyp(HYPS))
 * - eq(rn(AX,RN,_,_),modal(MS),X,Y,hyp(HYPS))
 * - constraint(rn(AX,RN,_,_),MS,(card,app(_F:R,X),Rel,N),hyp(HYPS))
 *
 */

/***********************************************************************
 * 
 * cCS(+CallStack,Call)
 * succeeds if the top call on CallStack is not already contained
 * elsewhere in CallStack and Call is not already contained in CallStack.
 * This predicate is used to prevent nontermination.
 *
 */

cCS([],_) :- !.
cCS(IL,A1) :-
%	print('trying '), print(A1), nl,
	noAxiom(A1,IL),
%	IL = [I1|IL1],
	noDouble(IL),
%	printAxiom(A1), nl,
%	print('------------------------------------------------------------'),
%	nl,
	not(clashCS([A1|IL])),
	!.

testEqualAbductiveHypotheses(D1,D2) :-
	currentEnvironment(Env),
	abductiveDerivation(Env,D1,HL1),
	abductiveDerivation(Env,D2,HL2),
	!,
	equalset(HL1,HL2),
	!.
testEqualAbductiveHypotheses(_D1,_D2) :-
	!.

testEqualHypotheses(H1,H2) :-
%	equalset(H1,H2),
	!.

% To prove in(C,X) it is not allowed to use another in-clause generated 
% from the same axiom
sameAxiom(AX,_RN1,MS1,in(C1,X1),HYPS1,D1,
          in(rn(AX,_RN2,_,_),modal(MS2),C2,X2,hyp(HYPS2),ab(D2))) :- 
	not(not(X1 = X2)),
	not(not(C1 = C2)),
	testEqualAbductiveHypotheses(D1,D2),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,in(_C,X1),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),X2,_,hyp(HYPS2))) :- 
	nonvar(X1),
	nonvar(X2),
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,in(_C,X1),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),_,X2,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
% To prove in(C,X) it is not allowed to use a constraint-clause generated 
% from the same axiom
sameAxiom(AX,_RN1,MS1,in(_C,X1),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),MS2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,in(_C,X1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),MS2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,eq(X1,_Y),HYPS1,_D1,
          in(rn(AX,_RN2,_,_),modal(MS2),_C,X2,hyp(HYPS2),ab(_D2))) :- 
	nonvar(X1),
	nonvar(X2),
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,eq(_X,Y1),HYPS1,_D1,
          in(rn(AX,_RN2,_,_),modal(MS2),_C,Y2,hyp(HYPS2),ab(_D2))) :- 
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,eq(X1,Y1),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),X2,Y2,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,eq(X1,app(_Y1)),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),MS2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :-   
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), 
	!.
sameAxiom(AX,_RN1,MS1,eq(_X,app(_F1:R1,Y1)),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),MS2,(card,app(_F2:R2,Y2),_Rel,_N),hyp(HYPS2))) :-
	not(not(R1 = R2)),
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,eq(X1,app(_Y1)),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),MS2,(card,app(_,X2),_Rel,_N),hyp(HYPS2))) :-   
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), 
	!.
sameAxiom(AX,_RN1,MS1,eq(_X,app(_F1:R1,Y1)),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),MS2,(card,app(_F2:R2,Y2),_Rel,_N),hyp(HYPS2))) :-
	not(not(R1 = R2)),
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,eq(_X,Y1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),MS2,(card,app(_,Y2),_Rel,_N),hyp(HYPS2))) :-
	not(not(Y1 = Y2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,c(X1,_,_),HYPS1,_D1,
          in(rn(AX,_RN2,_,_),modal(MS2),_,X2,hyp(HYPS2),ab(_D2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,c(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,c(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,c(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),_,app(_,X2),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,c(X1,R1,Rel1),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),MS2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,c(X1,R1,Rel1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),MS2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,sc(X1,_,_),HYPS1,_D1,
          in(rn(AX,_RN2,_,_),modal(MS2),_,X2,hyp(HYPS2),ab(_D2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,sc(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,sc(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),X2,_,hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,sc(X1,_,_),HYPS1,_D1,
          eq(rn(AX,_RN2,_,_),modal(MS2),_,app(_,X2),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,sc(X1,R1,Rel1),HYPS1,_D1,
          constraint(rn(AX,_RN2,_,_),MS2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(AX,_RN1,MS1,sc(X1,R1,Rel1),HYPS1,_D1,
          solveConstraint(rn(AX,_RN2,_,_),MS2,(card,app(_F:R2,X2),Rel2,_N),hyp(HYPS2))) :- 
	not(not(X1 = X2)),
	not(not(R1 = R2)),
	not(not(Rel1 = Rel2)),
	testEqualHypotheses(HYPS1,HYPS2),
	equalWorlds(MS1,MS2), !.
sameAxiom(_AX,_,_,_,_,_,_) :- !, fail.

equalWorlds(W1,W2) :-
	var(W1),
	var(W2),
	!.
equalWorlds(W1,W2) :-
	var(W1),
	nonvar(W2),
	!,
	fail.
equalWorlds(W1,W2) :-
	var(W2),
	nonvar(W1),
	!,
	fail.
equalWorlds([],[]) :-
	!.
equalWorlds(app(_F:m(_MOp,_A),_W1),[]) :-
	!,
	fail.
equalWorlds([],app(_F:m(_MOp,_A),_W2)) :-
	!,
	fail.
equalWorlds(app(F1:m(MOp,A1),W1),app(F2:m(MOp,A2),W2)) :-
	A1 == A2,
%	var(F1),
%	var(F2),
%       not(not(F1 = F2)),
	!,
	equalWorlds(W1,W2).
equalWorlds(app(F1:m(MOp,A1),_W1),app(F2:m(MOp,A2),_W2)) :-
	A1 == A2,
	nonvar(F1),
	nonvar(F2),
	not(not(F1 = F2)),
	!.
equalWorlds(_W1,_W2) :-
	!,
	fail.
	

noAxiom(true,_) :- !.
noAxiom(_,[]) :- !.
noAxiom(in(rn(AX,RN,_,_),modal(MS),C,X,hyp(HYPS),ab(D)),[C1|CL]) :-
	not(sameAxiom(AX,RN,MS,in(C,X),HYPS,D,C1)),
	noAxiom(in(rn(AX,RN,_,_),modal(MS),C,X,hyp(HYPS),ab(D)),CL).
noAxiom(eq(rn(AX,RN,_,_),modal(MS),X,Y,hyp(HYPS)),[C1|CL]) :-
	not(sameAxiom(AX,RN,MS,eq(X,Y),HYPS,_D,C1)),
	noAxiom(eq(rn(AX,RN,_,_),modal(MS),X,Y,hyp(HYPS)),CL).
noAxiom(constraint(rn(AX,RN,_,_),MS,(card,app(_F:R,X),Rel,N),hyp(HYPS)),[C1|CL]) :-
	not(sameAxiom(AX,RN,MS,c(X,R,Rel),HYPS,_D,C1)),
	noAxiom(constraint(rn(AX,RN,_,_),MS,(card,app(_,X),Rel,N),hyp(HYPS)),CL).
noAxiom(solveConstraint(rn(AX,RN,_,_),MS,(card,app(_F:R,X),Rel,N),hyp(HYPS)),[C1|CL]) :-
	not(sameAxiom(AX,RN,MS,sc(X,R,Rel),HYPS,_D,C1)),
	noAxiom(solveConstraint(rn(AX,RN,_,_),MS,(card,app(_,X),Rel,N),hyp(HYPS)),CL).

noDouble([in(rn(AX,RN,_,_),modal(MS),not(C),X,hyp(_HYPS1),ab(D))|IL]) :-
	!,
	not(member(in(rn(AX1,RN1,_,_),modal(MS),not(C),X,hyp(_HYPS2),ab(D)),IL)),
	not(member(in(rn(AX2,RN2,_,_),modal(MS),C,X,hyps(_HYPS3),ab(noAb)),IL)),
	!,
	noDouble(IL).
noDouble([in(rn(AX,RN,_,_),modal(MS),C,X,hyp(_HYPS1),ab(D))|IL]) :-
	!,
	not(member(in(rn(AX1,RN1,_,_),modal(MS),C,X,hyp(_HYPS2),ab(D)),IL)),
	not(member(in(rn(AX2,RN2,_,_),modal(MS),not(C),X,hyps(_HYPS3),ab(noAb)),IL)),
	!,
	noDouble(IL).
noDouble([eq(rn(AX,RN,_,_),modal(MS),X,Y,hyp(_HYPS1))|IL]) :-
	!,
	not(member(eq(rn(AX1,RN1,_,_),modal(MS),X,Y,hyp(_HYPS2)),IL)),
	!,
	noDouble(IL).
noDouble([constraint(rn(AX,RN,_,_),MS,(card,app(_F:R,X),Rel,N),hyp(_HYPS1))|IL]) :-
	!,
	not(member(constraint(rn(AX1,RN1,_,_),MS,(card,app(_F:R,X),Rel,N),hyp(_HYPS2)),IL)),
	!,
	noDouble(IL).
noDouble([solveConstraint(rn(AX,RN,_,_),MS,(card,app(_F:R,X),Rel,N),hyp(_HYPS1))|IL]) :-
	!,
	not(member(solveConstraint(rn(AX1,RN1,_,_),MS,(card,app(_F:R,X),Rel,N),hyp(_HYPS2)),IL)),
	!,
	noDouble(IL).
noDouble([I1|IL]) :-
	not(member(I1,IL)),
	!,
	noDouble(I1,IL).
noDouble([]) :-
	!.

printAxiom(solveConstraint(MS,(card,app((_FF:R),X),Rel,N),hyp(HYPS))) :-
	print('axiom???'),
	print('   '),
	print(solveConstraint(MS,(app(R,X),Rel,N),hyp(HYPS))),
	!.
printAxiom(eq(rn(AX,RN,_,_),modal(MS),Y,app((_FF:R),X),hyp(HYPS))) :-
	print(rn(AX,RN)),
	print('   '),
	print(eq(MS,Y,app(R,X),hyp(HYPS))),
	!.
printAxiom(in(rn(AX,RN,_,_),modal(_MS),CN,CON,hyp(HYP))) :-
	print(rn(AX,RN)),
	print('   '),
	print(in(CN,CON,hyp(HYP))),
	!.
printAxiom(constraint(rn(AX,RN,_,_),MS,(card,app((_FF:R),X),Rel,N),hyp(HYPS))) :-
	print(rn(AX,RN)),
	print('   '),
	print(constraint(MS,(app(R,X),Rel,N),hyp(HYPS))),
	!.
printAxiom(true) :-
	!.

/**********************************************************************
 *
 * clashCS(+CL)
 * succeeds if CL is a clash, i.e. it obeys one of the following 
 * conditions:
 * - it contains in(bot,X) for some X.
 * - it contains both in(A,X) and in(not(A),X) for some A and some X.
 *
 */

last([],[],_) :-
	!,
	fail.
last([L1],[],L1) :-
	!.
last([L1|LL1],[L1|LL2],Last) :-
	last(LL1,LL2,Last),
	!.

generateClashGoal(CS1,Goal) :-
	last(CS1,CS2,in(rn(AX,RN,S,O),modal(W1),C,X,hyp(HYPS))),
	getCurrentEnvironment(EnvName),
	environment(EnvName,Env,_),
	constructMLHead(Env,rn(_AX1,_RN1,user,_O1),W1,C1,X,CS1,noAb,[],_,Goal),
	!.

		
clashCS(CL) :-
	retract(clashTest(possible)),
	assertz(clashTest(impossible)),
	generateClashGoal(CL,Goal),
	!,
	doClashTest(Goal).
clashCS(_CL) :-
	!,
	fail.
	
doClashTest(InHead1) :-
	call(InHead1),
	InHead1 = in(Env,_,modal(W1),C1,X,hyp(HYP),ab(_),call(_CALL),_),
	atomic(X),
	normalizeNot(not(C1),C2),
	constructMLHead(Env,rn(_AX2,_RN2,_S2,_O2),W1,C2,X,HYP,noAb,[],_,InHead2),
	call(InHead2),
	print('Clash test succeeded for'), nl,
	print(HYP), nl,
	print('and'), nl,
	print(InHead1), nl,
	nl,
	retract(clashTest(impossible)),
	assertz(clashTest(possible)),
	!.
doClashTest(Goal) :-
	% the clash goal has failed, so there is no clash
	print('Clash test succeeded for'), nl,
	print(HYP), nl,
	nl,
	retract(clashTest(impossible)),
	assertz(clashTest(possible)),
	!,
	fail.




% clashCS(CL) :-
% 	clashTest(possible),
% 	member(in(rn(_,_,_,_),modal(_MS),bot,_X,hyp(_HYPS1)),CL),
% 	!.
% clashCS(CL) :-
% 	clashTest(possible),
% 	member(in(rn(_,_,_,_),modal(MS),not(A),X,hyp(_HYPS1)),CL),
% 	member(in(rn(_,_,_,_),modal(MS),A,X,hyp(_HYPS2)),CL),
% 	!.
% clashCS(CL) :-
% 	clashTest(possible),
% 	member(constraint(rn(_,_,_,_),MS,
% 			  (card,app(_F1:R,X),'>=',N1),hyp(_HYPS1)),CL),
% 	member(constraint(rn(_,_,_,_),MS,
% 			  (card,app(_F2:R,X),'=<',N2),hyp(_HYPS2)),CL),
% 	number(N1),
% 	number(N2),
% 	N1 > N2,
% 	!.
% clashCS(CL) :-
% 	member(constraint(rn(_,_,_,_),MS,
% 			  (card,app(_F1:R,X),'=<',N1),hyp(_HYPS1)),CL),
% 	number(N1),
% 	countAllRoleFillersInCS(MS,R,X,CL,N2),
% 	N2 > N1,
% 	!.
% 		
% 		 
% countAllRoleFillersInCS(MS,R,X,CL,N) :-
% 	getAllRoleFillersInCS(MS,R,X,CL,[],RF),
% 	length(RF,N).
% 
% getAllRoleFillersInCS(_MS,_R,_X,[],RF,RF) :-
% 	!.
% getAllRoleFillersInCS(MS,R,X,
%    [eq(rn(_,_,_,_),modal(MS),Y,app(_F:R,X),hyp(_HYPS))|CL],RF1,RF2) :-
% 	nonvar(Y),
% 	nonvar(X),
% 	atomic(Y),
% 	not(member((X,Y),RF1)),
% 	!,
% 	getAllRoleFillersInCS(MS,R,CL,[(X,Y)|RF1],RF2).
% getAllRoleFillersInCS(MS,R,X,[_|CL],RF1,RF2) :-
% 	getAllRoleFillersInCS(MS,R,X,CL,RF1,RF2),
% 	!.

/**********************************************************************
 *
 * @(#) clash.pl 1.2@(#)
 *
 */

clashInHyp(CL) :-
	member(in(_,modal(_MS),bot,_X,hyp(_HYPS1),ab(_)),CL),
	!.
clashInHyp(CL) :-
	member(in(_N2,modal(MS2),A,X,hyp(_HYPS2),ab(_D2)),CL),
	atomic(A),
	member(in(_N1,modal(MS1),not(A),X,hyp(_HYPS1),ab(_D1)),CL),
	not(not(MS1 = MS2)),
	!.
% clashInHyp(CL) :-
% 	member(constraint(rn(_,_,_,_),MS,
% 			  (card,app(_F1:R,X),'>=',N1),hyp(_HYPS1)),CL),
% 	member(constraint(rn(_,_,_,_),MS,
% 			  (card,app(_F2:R,X),'=<',N2),hyp(_HYPS2)),CL),
% 	number(N1),
% 	number(N2),
% 	N1 > N2,
% 	!.
% clashInHyp(CL) :-
% 	member(constraint(rn(_,_,_,_),MS,
% 			  (card,app(_F1:R,X),'=<',N1),hyp(_HYPS1)),CL),
% 	number(N1),
% 	countAllRoleFillersInCS(MS,R,X,CL,N2),
% 	N2 > N1,
% 	!.
% 		
% 		 
% countAllRoleFillersInCS(MS,R,X,CL,N) :-
% 	getAllRoleFillersInCS(MS,R,X,CL,[],RF),
% 	length(RF,N).
% 
% getAllRoleFillersInCS(_MS,_R,_X,[],RF,RF) :-
% 	!.
% getAllRoleFillersInCS(MS,R,X,
%    [eq(rn(_,_,_,_),modal(MS),Y,app(_F:R,X),hyp(_HYPS))|CL],RF1,RF2) :-
% 	nonvar(Y),
% 	nonvar(X),
% 	atomic(Y),
% 	not(member((X,Y),RF1)),
% 	!,
% 	getAllRoleFillersInCS(MS,R,CL,[(X,Y)|RF1],RF2).
% getAllRoleFillersInCS(MS,R,X,[_|CL],RF1,RF2) :-
% 	getAllRoleFillersInCS(MS,R,X,CL,RF1,RF2),
% 	!.
% 
/**********************************************************************
 *
 * @(#) classifier.pl 1.12@(#)
 *
 */

/***********************************************************************
 *
 * subsumes(+Name1,+Name2)
 * Parameter: Name1     concept or role name
 *            Name2     concept or role name
 * true iff Name1 subsumes Name2 in modal context []
 * (so Name1 and Name2 must both be concept names or role names).
 *
 */

subsumes(N1,N2) :-
	getCurrentEnvironment(EnvName),
	subsumes(EnvName,[],N1,N2).

/***********************************************************************
 *
 * subsumes(+MS,+Name1,+Name2)
 * Parameter: MS        modal context
 *            Name1     concept or role name
 *            Name2     concept or role name
 * true iff Name1 subsumes Name2 (so Name1 and Name2 must both be
 * concept names or role names).
 *
 */

subsumes(MS,N1,N2) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	currentEnvironment(Env),
	clause(conceptName(Env,_MS1,_W1,N1),_),
	clause(conceptName(Env,_MS2,_W2,N2),_),
	!,
	subsumes(concepts,Env,MS,N1,N2).
subsumes(MS,N1,N2) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	currentEnvironment(Env),
	clause(roleName(Env,_MS1,_W1,N1),_),
	clause(roleName(Env,_MS2,_W2,N2),_),
	subsumes(roles,Env,MS,N1,N2).

subsumes(EnvName,MS,N1,N2) :-
	environment(EnvName,Env,_),
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	clause(conceptName(Env,_MS1,_W1,N1),_),
	clause(conceptName(Env,_MS2,_W2,N2),_),
	!,
	subsumes(concepts,Env,MS,N1,N2).
subsumes(EnvName,MS,N1,N2) :-
	environment(EnvName,Env,_),
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	currentEnvironment(Env),
	clause(roleName(Env,_MS1,_W1,N1),_),
	clause(roleName(Env,_MS2,_W2,N2),_),
	subsumes(roles,Env,MS,N1,N2).

subsumes(concepts,Env,MS,C,D) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	constructMLHead(Env,_RN1,W1,D,aaa,_HYPS,noAb,_CALLS,abox,InHeadD),
	asserta((InHeadD :- call(G1))),
 	getQuery(Env,W1,C,aaa,Exp,InHeadC),
%	convertToGoal(Env,_RN2,MS,C,aaa,[or([]),rl([]),fl(_DML1)],noAb,[],
%		      _PT2,InHeadC),
	call((call(G1), InHeadC)),
	retract((InHeadD :- _Body)).
subsumes(concepts,Env,MS,_C,D) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,_G1],_),
	constructMLHead(Env,_RN1,W1,D,aaa,_HYPS,noAb,_CALLS,abox,InHeadD),
	retract((InHeadD :- _Body)),
	!,
	fail.
subsumes(roles,Env,MS,R,S) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(skolem,SF),
	constructEqHead(Env,_RN1,W1,bbb,SF,S,aaa,_HYPS,noAb,_CALLS,abox,InHeadS),
	asserta((InHeadS :- call(G1))),
	constructEqHead(Env,_RN2,W1,bbb,_FF,R,aaa,[or([]),rl([]),fl(_DML1)],
			noAb,[],_PT2,InHeadR),
	call((G1, InHeadR)),
	retract((InHeadS :- _Body)).
subsumes(roles,Env,MS,_R,S) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,_G1],_),
	constructEqHead(Env,_RN2,W1,bbb,_FF,S,aaa,_HYPS,noAb,_CALLS,_,InHeadS),
	retract((InHeadS :- _Body)),
	!,
	fail.

/***********************************************************************
 *
 * classified(+MS,+Name)
 * succeeds iff Name is already in the appropriate subsumption hierarchy 
 * in modal context MS.
 *
 */

classified(Env,MS,Concept) :-
	clause(conceptName(Env,_MS1,_W1,Concept),_),
	!,
	conceptHierarchy(Env,MS,Tree),
	search(Concept,Tree).
classified(Env,MS,Role) :-
	clause(roleName(Env,_MS1,_W1,Role),_),
	roleHierarchy(Env,MS,Tree),
	search(Role,Tree).


search(Concept,node(CL,_NL)) :-
	member(Concept,CL),
	!.
search(Concept,node(_CL,NL)) :-
	searchSubtrees(Concept,NL),
	!.

searchSubtrees(_Concept,[]) :-
	!,
	fail.
searchSubtrees(Concept,[N1|_]) :-
	search(Concept,N1),
	!.
searchSubtrees(Concept,[_|NL]) :-
	searchSubtrees(Concept,NL).


search(Concept,node(CL,NL),[node(CL,NL)]) :-
	member(Concept,CL),
	!.
search(Concept,node(_CL,NL),T1) :-
	searchSubtrees(Concept,NL,T1),
	!.

searchSubtrees(_Concept,[],[]) :-
	!.
searchSubtrees(Concept,[N1|NL],T2) :-
	search(Concept,N1,T1),
	searchSubtrees(Concept,NL,TL),
	append(T1,TL,T2),
	!.

/***********************************************************************
 *
 * classify
 * compute the subsumption hierarchy 
 * side effects: 
 * asserts a clause
 *               conceptHierarchy(MS,Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 * This is now done using the new classification algorithm written
 * by Patrick Brandmeier.
 *
 */

classify :-
	newClassify.
classify(Arg1) :-
	newClassify(Arg1).
classify(EnvName,MS) :-
	newClassify(EnvName,MS).

/***********************************************************************
 *
 * classify(+NewConcept)
 * adds concept NewConcept to the subsumption hierarchy in the modal
 * context [].
 * side effects: 
 * asserts a clause
 *               conceptHierarchy([],Tree)
 * or            roleHierachy([],Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 *
 */

classify(EnvName,NewConcept) :-
	environment(EnvName,Env,_),
	atomic(NewConcept),
	clause(conceptName(Env,_MS1,_W2,NewConcept),_), % _MS1 might be [] ?
	classify(concepts,[],NewConcept).
classify(EnvName,NewRole) :-
	environment(EnvName,Env,_),
	atomic(NewRole),
	clause(roleName(Env,_MS1,_W1,NewRole),_), % _MS1 might be [] ?
	classify(roles,[],NewRole).


/***********************************************************************
 *
 * oldClassify
 * compute the subsumption hierarchy in the modal context MS
 * side effects: 
 * asserts a clause
 *               conceptHierarchy(MS,Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 * This is the original classification algorithm written by 
 * Ullrich Hustadt.
 *
 */

oldClassify :-
	getCurrentEnvironment(EnvName),
	oldClassify(EnvName,[]).

oldClassify(EnvName) :-
	environment(EnvName,_Env,_),
	!,
	oldClassify(EnvName,[]).
oldClassify(MS) :-
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	oldClassify(EnvName,MS).

oldClassify(EnvName,MS) :-
	environment(EnvName,Env,_),
	clause(conceptName(Env,MS,_,Concept),_),
	once(classify(concepts,Env,MS,Concept)),
	fail.
oldClassify(EnvName,MS) :-
	environment(EnvName,Env,_),
	clause(roleName(Env,MS,_,Role),_),
	once(classify(roles,Env,MS,Role)),
	fail.
oldClassify(_,_) :-
	!.
	

/***********************************************************************
 *
 * classify(+MS,+NewConcept)
 * adds concept NewConcept to the subsumption hierarchy in the modal
 * context MS.
 * side effects: 
 * asserts a clause
 *               conceptHierarchy(MS,Tree)
 * or            roleHierarchy(MS,Tree)
 * where Tree is a tree representation of the subsumption hierarchy.
 *
 */
 
classify(EnvName,MS,NewConcept) :-
	environment(EnvName,Env,_),
	clause(conceptName(Env,_MS1,_W1,NewConcept),_), % _MS1 might be MS ?
	!,
	classify(concepts,Env,MS,NewConcept).
classify(EnvName,MS,NewRole) :-
	environment(EnvName,Env,_),
	clause(roleName(Env,_MS1,_W1,NewRole),_), % _MS1 might be MS ?
	!,
	classify(roles,Env,MS,NewRole).

classify(concepts,Env,MS,NewConcept) :-
	classified(Env,MS,NewConcept),
	!.
classify(roles,Env,MS,NewRole) :-
	classified(Env,MS,NewRole),
	!.
classify(concepts,Env,MS,NewConcept) :-
	retract(conceptHierarchy(Env,MS,OldTree)),
	classify(concepts,Env,MS,NewConcept,OldTree,NewTree),
	assertz(conceptHierarchy(Env,MS,NewTree)).
classify(roles,Env,MS,NewRole) :-
	retract(roleHierarchy(Env,MS,OldTree)),
	classify(roles,Env,MS,NewRole,OldTree,NewTree),
	assertz(roleHierarchy(Env,MS,NewTree)).

classify(Type,Env,MS,NewConcept,OldTree,NewTree) :-
	testForSubsumption(Type,Env,MS,NewConcept,OldTree,NewTree,_Judgement),
	!.


/***********************************************************************
 *
 * testForSubsumption(+Type,+MS,+NewConcept,+OldTree
 *                    -NewTree,-Judgement)
 * builds a tree representation NewTree of the subsumption hierarchy 
 * Judgement has the following meaning:
 * below  : NewConcept is below  the top concept of OldTree
 *          in this case NewTree is instantiated with the tree which
 *          has NewConcept inserted in OldTree
 * beside : NewConcept is beside the top concept of OldTree
 *          in this case NewTree is instantiated with the tree which
 *          has NewConcept as top concept and all concepts of OldTree
 *          which are subsumed by NewConcept below it
 * above  : NewConcept is above  the top concept of OldTree
 *          in this case NewTree is not instantiated
 * in     : NewConcept is equivalent to the top concept of OldTree
 *          in this case NewTree is instantiated with the tree which
 *          has NewConcept inserted in OldTree
 *
 */

testForSubsumption(Type,Env,MS,NewConcept,node([ClassifiedConcept|CL],AL),NewTree,Judgement) :-
	once(subsume2(Type,Env,MS,NewConcept,ClassifiedConcept)), 
	testForEquivalence(Type,Env,MS,NewConcept,node([ClassifiedConcept|CL],AL),NewTree,Judgement),
	!.
testForSubsumption(Type,Env,MS,NewConcept,node([ClassifiedConcept|CL],AL),NewTree,below) :-
	% to get here the subsumption test in the first clause
        % must have failed
	once(subsume2(Type,Env,MS,ClassifiedConcept,NewConcept)),
	% so only x \in NewConcept        => x \in ClassifiedConcept
	% but not x \in ClassifiedConcept => x \in NewConcept
	tfsList1(Type,Env,MS,NewConcept,[ClassifiedConcept|CL],[],AL,
                below([]),beside([]),above([]),NewTree),
	!.
testForSubsumption(Type,Env,MS,NewConcept,node([ClassifiedConcept|CL],AL),NewTree,beside) :-
	% neither x \in NewConcept        => x \in ClassifiedConcept
	% nor     x \in ClassifiedConcept => x \in NewConcept
	tfsList2(Type,Env,MS,NewConcept,[ClassifiedConcept|CL],[],AL,
                below([]),beside([]),above([]),NewTree),
	!.

tfsList1(_Type,_,_MS,NewConcept,N,_NL1,[],
        below(NL3),beside(NL4),above(NL5),Tree) :-
	buildTree1(NewConcept,N,below(NL3),beside(NL4),above(NL5),Tree),
	!.
tfsList1(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
        below(NL3),beside(NL4),above(NL5),NewTree) :-
	testForSubsumption(Type,Env,MS,NewConcept,Node1,Tree,Judgement),
	continue1(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
	         below(NL3),beside(NL4),above(NL5),Tree,Judgement,NewTree).

buildTree1(NewConcept,N,below([]),beside(NL2),above(NL3),
	node(N,[node([NewConcept],NL3)|NL2])) :- 
	!.
buildTree1(_NewConcept,N,below(NL1),beside(NL2),above(_),
	node(N,NL)) :-
	union(NL1,NL2,NL),
	!.
buildTree1(_NewConcept,_N,_,_,_,_) :-
	!,
	fail.

	
continue1(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
         below(NL3),beside(NL4),above(NL5),Tree,below,NewTree) :-
	% NL4 can be non-empty
	% NL5 should be the empty list !
	tfsList1(Type,Env,MS,NewConcept,N,[Node1|NL1],NL2,
                below([Tree|NL3]),beside(NL4),above(NL5),NewTree),
	!.
continue1(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
         below(NL3),beside(NL4),above(NL5),
         node([NewConcept],[]),beside,NewTree) :-
	tfsList1(Type,Env,MS,NewConcept,N,[Node1|NL1],NL2,
                below(NL3),beside([Node1|NL4]),above(NL5),NewTree),
	!.
continue1(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
         below(NL3),beside(NL4),above(NL5),
         node([NewConcept],[N1|NL]),beside,NewTree) :-
	union(NL5,[N1|NL],NL6),
	tfsList1(Type,Env,MS,NewConcept,N,[Node1|NL1],NL2,
                below(NL3),beside([Node1|NL4]),above(NL6),NewTree),
	!.
continue1(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
         below(NL3),beside(NL4),above(NL5),_Tree,above,NewTree) :-
	tfsList1(Type,Env,MS,NewConcept,N,[Node1|NL1],NL2,
	        below(NL3),beside(NL4),above([Node1|NL5]),NewTree),
	!.
continue1(_Type,_,_MS,_NewConcept,N,NL1,[_Node1|NL2],
         below(_NL3),beside(_NL4),above(_NL5),
         Tree,in,node(N,NL)) :-
        % NL3, NL4 and NL5 can be non-empty
	reverseList(NL1,NL6),
	union(NL6,[Tree|NL2],NL),
	!.

tfsList2(_Type,_,_MS,NewConcept,N,_NL1,[],
        below(NL3),beside(NL4),above(NL5),Tree) :-
	buildTree2(NewConcept,N,below(NL3),beside(NL4),above(NL5),Tree),
	!.
tfsList2(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
        below(NL3),beside(NL4),above(NL5),NewTree) :-
	testForSubsumption(Type,Env,MS,NewConcept,Node1,Tree,Judgement),
	continue2(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
	         below(NL3),beside(NL4),above(NL5),Tree,Judgement,NewTree).

buildTree2(NewConcept,_N,below([]),beside(_NL2),above([]),
	node([NewConcept],[])) :-
	!.
buildTree2(NewConcept,_N,below([]),beside(_NL2),above(NL3),
	node([NewConcept],NL3)) :- 
	!.
buildTree2(_NewConcept,_N,_,_,_,_) :-
	!,
	fail.

	
continue2(_Type,_,_MS,_NewConcept,_N,_NL1,[_Node1|_NL2],
         below(_NL3),beside(_NL4),above(_NL5),_Tree,below,_NewTree) :-
	!,
	fail.
continue2(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
         below(NL3),beside(NL4),above(NL5),_Tree,beside,NewTree) :-
	tfsList2(Type,Env,MS,NewConcept,N,[Node1|NL1],NL2,
                below(NL3),beside([Node1|NL4]),above(NL5),NewTree),
	!.
continue2(Type,Env,MS,NewConcept,N,NL1,[Node1|NL2],
         below(NL3),beside(NL4),above(NL5),_Tree,above,NewTree) :-
	tfsList2(Type,Env,MS,NewConcept,N,[Node1|NL1],NL2,
	        below(NL3),beside(NL4),above([Node1|NL5]),NewTree),
	!.
continue2(_Type,_,_MS,_NewConcept,_N,_NL1,[_Node1|_NL2],
         below(_NL3),beside(_NL4),above(_NL5),
         _Tree,in,node(_N,_NL)) :-
	!,
	fail.

testForEquivalence(Type,Env,MS,NewConcept,node([ClassifiedConcept|CL],AL),
	           node([NewConcept,ClassifiedConcept|CL],AL),in) :-
	once(subsume2(Type,Env,MS,ClassifiedConcept,NewConcept)),
	% so NewConcept = ClassifiedConcept
	!.
testForEquivalence(_Type,_,_MS,_NewConcept,node([_ClassifiedConcept|_CL],_AL),
	           _,above) :-
	% so only x \in ClassifiedConcept => x \in NewConcept
        % but not x \in NewConcept        => x \in ClassifiedConcept
	!.

subsume2(Type,Env,MS,X,Y) :- var(X),!,fail.
subsume2(Type,Env,MS,X,Y) :- var(Y),!,fail.
subsume2(Type,Env,MS,X,top) :- !,fail.
subsume2(Type,Env,MS,bot,X) :- !,fail.
subsume2(Type,Env,MS,X,bot) :- !.
subsume2(Type,Env,MS,top,X) :- !.
subsume2(Type,Env,MS,X,Y) :- 
	sub3(X,Y),
	!.
subsume2(Type,Env,MS,X,Y) :- 
	nsub3(X,Y),
	!,fail. 
subsume2(Type,Env,MS,X,Y) :- 
	X \== Y,
	subsumes(Type,Env,MS,X,Y), 
  	cont4(X,Y),
	!.
subsume2(Type,Env,MS,X,Y) :- 
	X \== Y,
	cont5a(X,Y),
	!,
	fail.
cont4(top,Y).
cont4(X,Y) :- 
	assert1(sub3(X,Y)),
	succ3(Z,X),
	cont4(Z,Y),!.
cont4(X,Y). 
cont5a(bot,X) :- !.
cont5a(X,bot) :- !,fail.
cont5a(X,Y) :-
	assert1(nsub3(X,Y)),
	succ3(Y,Z),
	cont5a(X,Z),!.

assert2(G) :- not(G),assert(G),!.
assert2(G) :-!.

retract2(G) :- retract(G),!.
retract2(G) :- !.

succ2(X,Y) :- succ3(X,Y),!.
succ2(X,bot) :- !.


/***********************************************************************
 *
 * showHierarchy(+Type)
 * Parameter: Type     'concepts' or 'roles'
 * display subsumption hierarchy in the modal context [].
 *
 */

showHierarchy(Type) :-
	getCurrentEnvironment(EnvName),
	showHierarchy(EnvName,[],Type).

/***********************************************************************
 *
 * showHierarchy(+EnvName,+MS,+Type)
 * Parameter: EnvName   environment name
 *            MS        modal context
 *            Type      'concepts' or 'roles'
 * display subsumption hierarchy in the modal context MS.
 *
 */

showHierarchy(EnvName,MS,concepts) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Tree),
	showDag([],Tree).
showHierarchy(EnvName,MS,roles) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Tree),
	showDag([],Tree).

showHierarchy(EnvName,Type) :-
	environment(EnvName,_,_),
	!,
	showHierarchy(EnvName,[],Type).
showHierarchy(MS,Type) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	showHierarchy(EnvName,MS,Type).

/***********************************************************************
 *
 * getHierarchy(+Type,-H)
 * Parameter: Type     'concepts' or 'roles'
 * instantiates H with the internal representation of the subsumption 
 * hierarchy of Type in the current environment and modal context [].
 *
 */

getHierarchy(Type,H) :-
	getCurrentEnvironment(EnvName),
	getHierarchy(EnvName,[],Type,H).

/***********************************************************************
 *
 * getHierarchy(+EnvName,+MS,+Type,-H)
 * Parameter: EnvName   environment name
 *            MS        modal context
 *            Type      'concepts' or 'roles'
 * instantiates H with the internal representation of the subsumption 
 * hierarchy of Type in environment EnvName and modal context MS.
 *
 */

getHierarchy(EnvName,MS,concepts,Tree) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Tree).
getHierarchy(EnvName,MS,roles,Tree) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Tree).

getHierarchy(EnvName,Type,Tree) :-
	environment(EnvName,_,_),
	!,
	getHierarchy(EnvName,[],Type,Tree).
getHierarchy(MS,Type,Tree) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	getHierarchy(EnvName,MS,Type,Tree).

/***********************************************************************
 *
 * showDag(+Depth,+Tree)
 * display subtree of the tree representation of the subsumption 
 * hierarchy which is located at depth D, where D is the lenght of
 * the list Depth of minus signs, in the hierarchy.
 *
 */

showDag(Depth,node(CL,AL)) :-
	writes(Depth),
	writes(" "),
	printClass(CL),
	printArgs([45|Depth],AL).

printClass([C1]) :-
	print(C1),
	nl,
	!.
printClass([C1,C2|CL]) :-
	print(C1),
	writes(" ("),
	printRest([C2|CL]),
	writes(")"),
	nl.
printRest([]) :- !.
printRest([C1]) :-
	print(C1).
printRest([C1,C2|CL]) :-
	print(C1),
	print(", "),
	printRest([C2|CL]).

printArgs(_Depth,[]) :- !.
printArgs(Depth,[N1|NL]) :-
	showDag(Depth,N1),
	printArgs(Depth,NL).






/**********************************************************************
 *
 * @(#) classifier2.pl 1.35@(#)
 *
 */

init_new_daten :- 
	currentEnvironment(Env),
	init_new_daten(Env).

init_new_daten(Env) :-
        init_succ(_),
	init_sub(_),
	init_nsub(_),
	assert(conceptName1(Env,_,top)),
	assert(roleName1(Env,_,top)),
       	assertz(succ(concepts,Env,_,top,bot)),
	assertz(sub(concepts,Env,_,top,_)),
	assertz(nsub(concepts,Env,_,X,X)),	
	assertz(succ(roles,Env,_,top,bot)),
	assertz(sub(roles,Env,_,top,_)),
	assertz(nsub(roles,Env,_,X,X)),
	assertz(sub(roles,Env,_,_,bot)),
	assertz(sub(concepts,Env,_,_,bot)).

init_new_daten1 :-
	currentEnvironment(Env),
	!,
	init_new_daten1(Env).	
init_new_daten1(Env) :-
	conceptName(Env,MS,W1,NewConcept),
	not(name(NewConcept,[99,111,110,99,101,112,116|_])),
	assert1(conceptName1(Env,MS,W1,NewConcept)),
	fail.
init_new_daten1(Env) :-
	roleName(Env,MS,W1,NewRole),
	not(name(NewRole,[114,111,108,101|_])),
	assert1(roleName1(Env,MS,W1,NewRole)),
	fail.
init_new_daten1(Env).

init_succ(MS) :- 
 	currentEnvironment(Env),
        init_succ(Env,MS),
	!.
init_succ(MS).
init_succ(Env,MS) :- 
	retractall(succ(_,Env,MS,_,_)),
	!.
init_sub(MS) :-
	currentEnvironment(Env),
	init_sub(Env,MS).
init_sub(MS).
init_sub(Env,MS) :- 
	retractall(sub(_,Env,MS,_,_)),
	!.

init_nsub(MS) :-
 	currentEnvironment(Env),
	init_nsub(Env,MS).
init_nsub(MS).
init_nsub(Env,MS) :-
	retractall(nsub(_,Env,MS,_,_)),
	!.

/********************************************************************/
% Test-functions 

neu1 :- newClassify,
	show_dag([]),printStat. 
show :- getCurrentEnvironment(EnvName),
	environment(EnvName,Env,_),
	showDefconcept(Env),
	showDefprimconcept(Env),
	showDefrole(Env),
        showDefprimrole(Env).
test1(Concept):- 
	environment(EnvName,Env,_),	
	conceptEqualSets(Env,user,MS,Concept,CT,_),
	find_concept2(concepts,Env,MS,Concept,CT).

test2 :- 
	environment(EnvName,Env,_),	
	conceptEqualSets(Env,user,MS,Concept,CT,_),
	conceptEqualSets(Env,user,MS,Concept1,Concept,_),
	print(Concept),
	print(Concept1),nl,
	fail.

test3(MS,_) :- 
	environment(EnvName,Env,_),	
	conceptEqualSets(Env,user,MS,Concept,CT,_),
	clause(conceptName(Env,_MS1,_W1,Concept),_),
	conceptEqualSets(Env,user,MS,CT,Concept1,_),
%	conceptName(Env,_MS2,_W2,Concept1),
	print(Concept),print(" "),
	print(CT),print(" "),
	print(Concept1),
	nl,
	fail.
test3(MS,_) :- 
	environment(EnvName,Env,_),	
	conceptSubsets(Env,user,MS,Concept,CT,_),
	clause(conceptName(Env,_MS1,_W1,Concept),_),
	conceptSubsets(Env,user,MS,CT,Concept1,_),
%	not(conceptName(Env,_MS2,_W2,Concept)),
	print(Concept),print(" "),
	print(CT),print(" "),
	print(Concept1),
	nl,
	fail.
test3(MS) :- 
	environment(EnvName,Env,_),	
	roleEqualSets(Env,user,MS,Concept,CT,_),
	clause(roleName(Env,_MS1,_W1,Concept),_),
	roleEqualSets(Env,user,MS,CT,Concept1,_),
%	roleName(Env,MS,Concept1),
	print(Concept),print(" "),
	print(CT),print(" "),
	print(Concept1),
	nl,
	fail.

test3(MS) :- 
	environment(EnvName,Env,_),	
	roleSubsets(Env,user,MS,Concept,CT,_),
	clause(roleName(Env,_MS1,_W1,Concept),_),
	roleSubsets(Env,user,MS,CT,Concept1,_),
%	not(roleName(Env,MS,Concept)),
	print(Concept),print(" "),
	print(CT),print(" "),
	print(Concept1),
	nl,
	fail.
	
% just for fun and test

neu1(MS) :- newClassify(MS).

newClassify :-
	getCurrentEnvironment(EnvName),
	newClassify(EnvName,[]).

newClassify(EnvName) :-
	environment(EnvName,_Env,_),
	!,
	newClassify(EnvName,[]).
newClassify(MS) :-
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	newClassify(EnvName,MS).

newClassify(EnvName,MS) :-
	environment(EnvName,Env,_),
	testa(Env,MS).
	
testa(Env,MS) :-
	init_new_daten(Env),
	initStat,
	testb(Env,MS),
	buildOrdering(Env,MS,CTree,RTree),
	retractall(conceptHierarchy(Env,MS,_)),
	retractall(roleHierarchy(Env,MS,_)),
	assert(conceptHierarchy(Env,MS,CTree)),
	assert(roleHierarchy(Env,MS,RTree)),
	ifOption(testOutput,yes,printStat),
%	ifOption(testOutput,yes,show_dag(MS)),
	!.	
testb(Env,MS) :-
        not(find_concept(concepts,Env,MS)),
        not(find_role(roles,Env,MS)).
	
find_concept(concepts,Env,MS) :-
	getConceptName(Env,MS,Concept),
	not(name(Concept,[99,111,110,99,101,112,116|_])),
	ifOption(testOutput,yes,(print(Concept), nl)),
	addCounter(conceptsClassified,1),
	find_concept1(concepts,Env,MS,Concept).
find_role(roles,Env,MS) :-
	getRoleName(Env,MS,Role),
	not(name(Role,[114,111,108,101|_])),
	addCounter(rolesClassified,1),
	find_role1(roles,Env,MS,Role).

find_role1(roles,Env,MS,Role) :-
	roleEqualSets(Env,user,MS,Role,CT,_),
	find_role2(roles,Env,MS,Role,CT),
	!,
 	fail.
find_role1(roles,Env,MS,Role) :-
	roleSubsets(Env,user,MS,Role,CT,_),
	find_prole2(roles,Env,MS,Role,CT),
 	!,
	fail.
/*
find_role1(roles,Env,MS,Role) :-
	roleEqualSets(Env,user,MS,Role,CT,_),
	find_role11(roles,Env,MS,Role,CT),
	!,
	fail.
find_role1(roles,Env,MS,Role) :-
	roleSubsets(Env,user,MS,Role,CT,_),
	find_role11(roles,Env,MS,Role,CT),
	!,
	fail.
find_role11(roles,Env,MS,Role,CT) :-
	roleEqualSets(Env,user,MS,CT,CT1,_),
	find_prole2(roles,Env,MS,Role,CT1),
	find_role11(roles,Env,MS,Role,CT).
find_role11(roles,Env,MS,Role,CT) :-
	roleSubsets(Env,user,MS,CT,CT1,_),
	find_prole2(roles,Env,MS,Role,CT1),
	find_role11(roles,Env,MS,Role,CT).
*/

find_role1(roles,Env,MS,Role) :-
	make_succ2(roles,Env,MS,Role),
	!,
	fail.
find_concept1(concepts,Env,MS,Concept) :-
	conceptEqualSets(Env,user,MS,Concept,CT,_),
	find_concept2(concepts,Env,MS,Concept,CT),
	!,
 	fail.
find_concept1(concepts,Env,MS,Concept) :-
	conceptSubsets(Env,user,MS,Concept,CT,_),
	find_pconcept2(concepts,Env,MS,Concept,CT),
 	!,
	fail.
/*
find_concept1(concepts,Env,MS,Concept) :-
	conceptEqualSets(Env,user,MS,Concept,CT,_),
	find_concept11(concepts,Env,MS,Concept,CT),
	!,
	fail.
find_concept1(concepts,Env,MS,Concept) :-
	conceptSubsets(Env,user,MS,Concept,CT,_),
	find_concept11(concepts,Env,MS,Concept,CT),
	!,
	fail.
find_concept11(concepts,Env,MS,Concept,CT) :-
	conceptEqualSets(Env,user,MS,CT,CT1,_),
	find_pconcept2(concepts,Env,MS,Concept,CT1),
	find_concept11(concepts,Env,MS,Concept,CT).
find_concept11(concepts,Env,MS,Concept,CT) :-
	conceptSubsets(Env,user,MS,CT,CT1,_),
	find_pconcept2(concepts,Env,MS,Concept,CT1),
	find_concept11(concepts,Env,MS,Concept,CT).
*/
find_concept1(concepts,Env,MS,Concept) :-
	make_succ2(concepts,Env,MS,Concept),
	!,
	fail.
/***** Entwicklungsecke....

test fuer den trans.abschluss von roleEqualSets,roleSubsets,concept...
find_role1(roles,Env,MS,Role) :-
	roleEqualSets(Env,user,MS,Role,CT,_),
	find_role11(roles,Env,MS,Role,CT),
	!,
	fail.
find_role11(roles,Env,MS,Role,CT) :-
	find_role2(roles,Env,MS,Role,CT),
	!,
	roleEqualSets(Env,user,MS,CT,CT1,_),
	find_role11(roles,Env,MS,Role,CT1),
	!.
find_role11(roles,Env,MS,Role,CT) :-
	!.

find_role1(roles,Env,MS,Role) :-
	roleSubsets(Env,user,MS,Role,CT,_),
	find_prole11(roles,Env,MS,Role,CT),
 	!,
	fail.
find_prole11(roles,Env,MS,Role,CT) :-
	find_prole2(roles,Env,MS,Role,CT),
	!,
	roleSubsets(Env,user,MS,CT,CT1,_),
	find_prole11(roles,Env,MS,Role,CT1),
	!.
find_prole11(roles,Env,MS,Role,CT) :-
	!.
*/

/*******************************************/

find_concept2(concepts,Env,MS,Concept,CT) :-
	getConceptName(Env,MS,CT),
	(succ(concepts,Env,MS,Topconcept,Concept),
	subsume1(concepts,Env,MS,Topconcept,CT)),
	assert1(nsub(concepts,Env,MS,Concept,CT)),
	assert1(nsub(concepts,Env,MS,CT,Concept)),
	assert_succ(concepts,Env,MS,Topconcept,CT),
	!.	
find_concept2(concepts,Env,MS,Concept,CT) :-
%	getConceptName(Env,_MS1,W1,CT),
	getConceptName(Env,MS,CT),
	(succ(concepts,Env,MS,Topconcept,CT),
	subsume1(concepts,Env,MS,Topconcept,Concept)),
	assert1(nsub(concepts,Env,MS,Concept,CT)),
	assert1(nsub(concepts,Env,MS,CT,Concept)),
	assert_succ(concepts,Env,MS,Topconcept,Concept),
	!.
find_concept2(concepts,Env,MS,Concept,CT) :-
	getConceptName(Env,MS,CT),
	assert1(nsub(concepts,Env,MS,Concept,CT)),
	assert1(nsub(concepts,Env,MS,CT,Concept)),
	assert1(succ(concepts,Env,MS,top,Concept)),
	assert1(succ(concepts,Env,MS,top,CT)),
	!.
find_concept2(concepts,Env,MS,Concept,CT) :-
	CT = and([X|[R]]),
	conceptEqualSets(Env,user,MS,Concept1,R,_),
	assert_succ(concepts,Env,MS,X,Concept),
	assert_succ(concepts,Env,MS,Concept1,Concept),		
	!. 
find_concept2(concepts,Env,MS,Concept,CT) :-
	CT = and(L),
	find_concept21(concepts,Env,MS,Concept,L),
	!.

find_concept2(concepts,Env,MS,Concept,CT) :-
	CT = and([X|[R]]),
	R = some(Role,Concept1),
	getRoleName(Env,MS,Role),
	X == Concept1,
	assert_succ(concepts,Env,MS,X,Concept),	
	!.

find_concept2(concepts,Env,MS,Concept,CT) :-
	CT = some(Role,Concept1),
	find_concept25(Env,MS,Concept,Concept1),
	!. 

find_concept2(concepts,Env,MS,Concept,CT) :-
	CT = or([and(L)]),
	find_concept26(concepts,Env,MS,Concept,L),
	!.
	
find_concept2(concepts,Env,MS,Concept,CT) :-
	CT = or([L|R]),
	L = and(L1),
	find_concept3(Env,MS,Concept,R,L1,Z),
	find_concept31(Env,MS,Concept,Z),	
	!.
find_concept2(concepts,Env,MS,Concept,CT) :-
	CT = or(L),
	find_concept26(concepts,Env,MS,Concept,L),
	!.

find_concept21(concepts,Env,MS,Concept,[]) :-
	!.
find_concept21(concepts,Env,MS,Concept,[X|R]) :-
	getConceptName(Env,MS,X),
	assert_succ(concepts,Env,MS,X,Concept),
	find_concept21(concepts,Env,MS,Concept,R),
	!.
find_concept21(concepts,Env,MS,Concept,[X|R]) :-    
	X = not(R1),
	getConceptName(Env,MS,R1),
	setofOrNil(K,find_concept22(concepts,Env,MS,Concept,R1,K),L),
	find_concept23(concepts,Env,MS,Concept,L),
	find_concept21(concepts,Env,MS,Concept,R),
	!.
find_concept21(concepts,Env,MS,Concept,[X|R]) :-
	find_concept21(concepts,Env,MS,Concept,R),
	!,
	fail.

find_concept22(concepts,Env,MS,Concept,R1,K) :-
	succ(concepts,Env,MS,K,R1),
	not(succ(concepts,Env,MS,K,Concept)).

find_concept23(concepts,Env,MS,Concept,[]) :-
	!.	
find_concept23(concepts,Env,MS,Concept,[L1|R1]) :-
	find_concept24(concepts,Env,MS,Concept,L1),
	find_concept23(concepts,Env,MS,Concept,R1),
	!.
find_concept24(concepts,Env,MS,Concept,L1) :-
	succ(concepts,Env,MS,Top,L1),
	succ(concepts,Env,MS,Top,K),
	subsume1(concepts,Env,MS,K,Concept),
	setofOrNil(Nf,succ(concepts,Env,MS,K,Nf),Lnf),
	make_succ1(concepts,Env,MS,K,Lnf,Concept),
	!. 
		
find_concept25(Env,MS,Concept,Concept1) :-
	succ(concepts,Env,MS,K,Concept1),
	setofOrNil(Nf,succ(concepts,Env,MS,Concept1,Nf),Lnf),
	make_succ1(concepts,Env,MS,K,Lnf,Concept),
	fail.
find_concept25(Env,MS,Concept,Concept1).

find_concept26(concepts,Env,MS,Concept,[C1|R]) :-
	getConceptName(Env,MS,C1),
	subsume1(concepts,Env,MS,C1,Concept),
	assert_succ(concepts,Env,MS,C1,Concept),
	find_concept26(concepts,Env,MS,Concept,R).

find_concept3(Env,MS,Concept,[],Z,Z) :- 
	!.
find_concept3(Env,MS,Concept,[L|R],Z,K) :-
	L = and(L1),
	intersection(Z,L1,Z1),
	find_concept3(Env,MS,Concept,R,Z1,K),
	!.

find_concept31(Env,MS,Concept,[]) :-
	!.
find_concept31(Env,MS,Concept,[L1|R1]) :-
	assert_succ(concepts,Env,MS,L1,Concept),
	find_concept31(Env,MS,Concept,R1),
	!.

% das hier ist die entwicklungsecke....	
/*
% weiss nicht ob das stimmt
find_concept21(concepts,Env,MS,Concept,[X|R]) :-    
	X = all(R1,X1),
	getRoleName(Env,MS,R1),
	getConceptName(Env,MS,X1),
	assert_succ(concepts,Env,MS,X1,Concept),
	find_concept21(concepts,Env,MS,Concept,R),
	!.
*/
% ********************** Primconcepte **************************
% es fehlt noch defprimconcept(_,_,some(_,_..))
%                   "         (_,not(),...)
%                   "         (_,...(),...)
					
find_pconcept2(concepts,Env,MS,PrimConcept,CT) :-
	CT = not(X),
	getConceptName(Env,MS,X),
	cont1a(concepts,Env,MS,[],X,PrimConcept),
%	succ(concepts,Env,MS,Topconcept,X),
	find_pconcept23(Env,MS,X,PrimConcept,Top),
	assert_succ(concepts,Env,MS,Top,PrimConcept),
	assert_succ(concepts,Env,MS,Top,X),
	!.
find_pconcept2(concepts,Env,MS,Primconcept,CT) :-
	CT = and(L),
	find_pconcept24(Env,MS,Primconcept,L),
	!.

find_pconcept2(concepts,Env,MS,PrimConcept,CT) :-
	getConceptName(Env,MS,CT),
	assert1(sub(concepts,Env,MS,CT,PrimConcept)),
	direct_succ(concepts,Env,MS,[],CT,PrimConcept,Z,L1),
        contb(concepts,Env,MS,Z,L1,PrimConcept),
	!.

find_pconcept2(concepts,Env,MS,PrimConcept,CT) :-
	CT = some(X,Y),
	find_pconcept21(Env,MS,PrimConcept,X,Y),
	!.

find_pconcept2(concepts,Env,MS,Primconcept,CT) :-
	CT = and([X|[R]]),
%	getConceptName(Env,MS,W1,X),
	getConceptName(Env,MS,X),
	R = not(Y),
	getConceptName(Env,MS,Y),
	find_pconcept23(Env,MS,X,Y,Top),
	assert_succ(concepts,Env,MS,Top,PrimConcept),	
%	assert1(sub(concepts,Env,MS,PrimConcept)),
	!.



find_pconcept2(concepts,Env,MS,Primconcept,CT) :-
	CT = or([and(L)]),
	find_pconcept26(concepts,Env,MS,Primconcept,L),
	!.
	
find_pconcept2(concepts,Env,MS,Primconcept,CT) :-
	CT = or([L|R]),
	L = and(L1),
	find_pconcept3(Env,MS,Primconcept,R,L1,Z),
	find_pconcept31(Env,MS,Primconcept,Z),	
	!.
find_pconcept2(concepts,Env,MS,Primconcept,CT) :-
	CT = or(L),
	find_pconcept26(concepts,Env,MS,Primconcept,L),
	!.

find_pconcept26(concepts,Env,MS,Primconcept,[C1|R]) :-
%	getConceptName(Env,_MS,W1,C1),
	getConceptName(Env,MS,X),
	subsume1(concepts,Env,MS,C1,Primconcept),
	find_pconcept27(concepts,Env,MS,Primconcept,C1),
  	find_pconcept26(concepts,Env,MS,Primconcept,R).

find_pconcept27(concepts,Env,MS,Primconcept,C1):-
	assert1(sub(concepts,Env,MS,C1,Primconcept)),
	direct_succ(concepts,Env,MS,[],C1,PrimConcept,Z,L1),
        contb(concepts,Env,MS,Z,L1,PrimConcept),
	!.

find_pconcept3(Env,MS,Primconcept,[],Z,Z) :- 
	!.
find_pconcept3(Env,MS,Primconcept,[L|R],Z,K) :-
	L = and(L1),
	intersection(Z,L1,Z1),
	find_pconcept3(Env,MS,Primconcept,R,Z1,K),
	!.

find_pconcept31(Env,MS,Primconcept,[]) :-
	!.
find_pconcept31(Env,MS,Primconcept,[L1|R1]) :-
	find_pconcept27(concepts,Env,MS,Primconcept,L1),
	find_pconcept31(Env,MS,Primconcept,R1),
	!.


find_pconcept21(Env,MS,PrimConcept,X,Y) :-
	Y = or([Y1|[Y2]]),
	conceptEqualSets(Env,user,MS,Concept,some(X,Y1),_),
	conceptEqualSets(Env,user,MS,Concept1,some(X,Y2),_),
	find_pconcept23(Env,MS,Concept1,Concept,Top),
	assert_succ(concepts,Env,MS,Top,PrimConcept),
	!.

find_pconcept23(Env,MS,X,Y,X) :-
	sub(concepts,Env,MS,X,Y),
	!.
find_pconcept23(Env,MS,X,Y,Y) :-
	sub(concepts,Env,MS,Y,X),
	!.
find_pconcept23(Env,MS,X,Y,Top) :-
	sub(concepts,Env,MS,Top,X),sub(concepts,Env,MS,Top,Y).

find_pconcept24(Env,MS,Primconcept,[X|R]) :-
	getConceptName(Env,MS,X),
	assert1(sub(concepts,Env,MS,X,PrimConcept)),
	direct_succ(concepts,Env,MS,[],X,PrimConcept,Z,L1),
        contb(concepts,Env,MS,Z,L1,PrimConcept),
	find_pconcept24(Env,MS,Primconcept,R),
	!.

/*	
find_pconcept2(concepts,Env,MS,Primconcept,CT) :-
	CT = and([X|[R]]),
	getConceptName(Env,_MS1,W1,X),
	getConceptName(Env,_MS2,W2,R),
	direct_succ(concepts,Env,MS,[],X,PrimConcept,Z,L1),
        contb(concepts,Env,MS,Z,L1,PrimConcept),
	!.

find_pconcept2(concepts,Env,MS,PrimConcept,CT) :-
	conceptName1(Env,MS1,CT),
	find_pconcept23(Env,MS,CT,PrimConcept,Top),
	assert_succ(concepts,Env,MS,Top,PrimConcept),
	assert_succ(concepts,Env,MS,Top,CT),
	!.
*/

/*************************************************************************
*                    jetzt mit rollen
*/
find_role2(roles,Env,MS,Role,CT) :-
	getRoleName(Env,MS,CT),
	succ(roles,Env,MS,Toprole,Role),
	assert1(nsub(roles,Env,MS,Role,Ct)),
	assert1(nsub(roles,Env,MS,Ct,Role)),
	assert_succ(roles,Env,MS,Toprole,CT),
	!.	
find_role2(roles,Env,MS,Role,CT) :-
	getRoleName(Env,MS,CT),
	succ(roles,Env,MS,Toprole,CT),
	assert1(nsub(roles,Env,MS,Role,Ct)),
	assert1(nsub(roles,Env,MS,Ct,Role)),
	assert_succ(roles,Env,MS,Toprole,Role),
	!.
find_role2(roles,Env,MS,Role,CT) :-	
	getRoleName(Env,MS,CT),
	assert1(nsub(roles,Env,MS,Role,Ct)),
	assert1(nsub(roles,Env,MS,Ct,Role)),
	assert_succ(roles,Env,MS,top,Role),
	assert_succ(roles,Env,MS,top,CT),
	!.	
find_role2(roles,Env,MS,Role,CT) :-	
	CT = and([X|[R]]),
	roleEqualSets(Env,user,MS,Role1,R,_),
	assert_succ(roles,Env,MS,X,Role),
	assert_succ(roles,Env,MS,Role1,Role),	
	!. 
find_role2(roles,Env,MS,Role,CT) :-	
	CT = and([X|[R]]),
	getRoleName(Env,MS,X),
	getRoleName(Env,MS,R),
	assert_succ(roles,Env,MS,X,Role),
	assert_succ(roles,Env,MS,R,Roles),
	!.
find_role2(roles,Env,MS,Role,CT) :-	
	CT = and([X|[R]]),
	R = some(_,Role1),
	X == Role1,
	assert_succ(Roles,Env,MS,X,Role),	
	!.
find_role2(roles,Env,MS,Role,CT) :-	
	CT = or([X|[R]]),
	find_role3(Env,MS,Role,X,R).
find_role2(roles,Env,MS,Role,CT) :-
	CT = and(L),
	find_role21(roles,Env,MS,Role,L),
	!.
find_role2(roles,Env,MS,Role,CT) :-
	CT = restr(Role1,Concept),
	assert_succ(roles,Env,MS,Role1,Role),
	!.


find_role2(roles,Env,MS,Role,CT) :-
	CT = or([and(L)]),
	find_role26(roles,Env,MS,Role,L),
	!.
	
find_role2(roles,Env,MS,Role,CT) :-
	CT = or([L|R]),
	L = and(L1),
	find_role30(Env,MS,Role,R,L1,Z),
	find_role31(Env,MS,Role,Z),	
	!.
find_role2(roles,Env,MS,Role,CT) :-
	CT = or(L),
	find_role26(roles,Env,MS,Role,L),
	!.

find_role26(roles,Env,MS,Role,[C1|R]) :-
	getRoleName(Env,MS,C1),
	subsume1(roles,Env,MS,C1,Role),
	assert_succ(roles,Env,MS,C1,Role),
	find_role26(roles,Env,MS,Role,R).

find_role30(Env,MS,Role,[],Z,Z) :- 
	!.
find_role30(Env,MS,Role,[L|R],Z,K) :-
	L = and(L1),
	intersection(Z,L1,Z1),
	find_role30(Env,MS,Role,R,Z1,K),
	!.

find_role31(Env,MS,Role,[]) :-
	!.
find_role31(Env,MS,Role,[L1|R1]) :-
	assert_succ(roles,Env,MS,L1,Role),
	find_role31(Env,MS,Role,R1),
	!.


find_role3(Env,MS,Role,X,R) :-
	X = and([X1|[R1]]),
	subsume1(roles,Env,MS,Role,R1),
	find_role2(roles,Env,MS,Role,X).
find_role3(Env,MS,Role,X,R) :-
	R = and([X1|[R1]]),
	subsume1(roles,Env,MS,Role,R1),
	find_role2(roles,Env,MS,Role,X).

find_role21(roles,Env,MS,Role,[]) :-
	!.
find_role21(roles,Env,MS,Role,[X|R]) :-
	getRoleName(Env,MS,X),
	assert_succ(roles,Env,MS,X,Role),
	find_role21(roles,Env,MS,Role,R),
	!.
find_role21(roles,Env,MS,Role,[X|R]) :-
	X = not(R1),
	getRoleName(Env,MS,R1),
	setofOrNil(K,find_role22(roles,Env,MS,Role,R1,K),L),
	find_role23(roles,Env,MS,Role,L),
	!.
find_role22(roles,Env,MS,Role,R1,K) :-
	succ(roles,Env,MS,K,R1),
	not(succ(roles,Env,MS,K,Role)).

find_role23(roles,Env,MS,Role,[]) :-
	!.	
find_role23(roles,Env,MS,Role,[L1|R1]) :-
	find_role24(roles,Env,MS,Role,L1),
	find_role23(roles,Env,MS,Role,R1),
	!.
find_role24(roles,Env,MS,Role,L1) :-
	succ(roles,Env,MS,Top,L1),
	succ(roles,Env,MS,Top,K),
	subsume1(roles,Env,MS,K,Role),
	setofOrNil(Nf,succ(roles,Env,MS,K,Nf),Lnf),
	make_succ1(roles,Env,MS,K,Lnf,Role),
	!.

/******** PrimRollen ************************************************/

find_prole2(roles,Env,MS,PrimRole,CT) :-
	CT = and(L),
	find_prole24(Env,MS,PrimRole,L),
	!.

find_prole2(roles,Env,MS,PrimRole,CT) :-
	CT = not(X),
	getRoleName(Env,MS,X),
	cont1a(roles,Env,MS,X,PrimRole),
	find_prole23(Env,MS,X,PrimRole,Top),
	assert_succ(roles,Env,MS,Top,PrimRole),
	assert_succ(roles,Env,MS,Top,X),
	!.

find_prole2(roles,Env,MS,PrimRole,CT) :-
	convertMS(Env,[[],true],MS,[],[W1,G1],_),
	call(G1),
	getRoleName(Env,MS,CT),
	assert1(sub(roles,Env,MS,CT,PrimRole)),
	direct_succ(roles,Env,MS,[],CT,PrimRole,Z,L1),
              contb(roles,Env,MS,Z,L1,PrimRole),
	!.
/*
find_prole2(roles,Env,MS,PrimRole,CT) :-
	CT = and([X|[R]]),
	getRoleName(Env,MS,X),
	getRoleName(Env,MS,R),	
	find_prole23(Env,MS,X,R,Top),
	assert_succ(roles,Env,MS,Top,PrimRole),
	!.

find_prole2(roles,Env,MS,PrimRole,CT) :-
	getRoleName(Env,MS,CT),
	find_prole23(Env,MS,PrimRole,CT,Top),
	assert_succ(roles,Env,MS,Top,PrimRole),
	assert_succ(roles,Env,MS,Top,CT),
	!.
*/
find_prole2(roles,Env,MS,PrimRole,CT) :-
	CT = some(X,Y),
	find_prole21(Env,MS,PrimRole,X,Y),
	!.
find_prole2(roles,Env,MS,PrimRole,CT) :-
	CT = and([X|[R]]),
	getRoleName(Env,MS,X),
	R = not(Y),
	getRoleName(Env,MS,Y),
	find_prole23(Env,MS,X,Y,Top),
	assert_succ(roles,Env,MS,Top,PrimRole),	
	!.
	
find_prole2(roles,Env,MS,Primrole,CT) :-
	CT = or([L|R]),
	L = and(L1),
	find_prole3(Env,MS,Primrole,R,L1,Z),
	find_prole31(Env,MS,Primrole,Z),	
	!.
find_prole2(roles,Env,MS,Primrole,CT) :-
	CT = or(L),
	find_prole26(roles,Env,MS,Primrole,L),
	!.

find_prole21(Env,MS,PrimRole,X,Y) :-
	Y = or([Y1|[Y2]]),
	roleEqualSets(Env,user,MS,Role,some(X,Y1),_),
	roleEqualSets(Env,user,MS,Role1,some(X,Y2),_),
	find_prole23(Env,MS,Role,Role1,Top),
	assert_succ(roles,Env,MS,Top,PrimRole),
	!.

find_prole23(Env,MS,X,Y,X) :-
	sub(roles,Env,MS,X,Y),
	!.
find_prole23(Env,MS,X,Y,Y) :-
	sub(roles,Env,MS,Y,X),
	!.
find_prole23(Env,MS,X,Y,Top) :-
	sub(roles,Env,MS,Top,X),sub(roles,Env,MS,Top,Y).

find_prole24(Env,MS,PrimRole,[]).
find_prole24(Env,MS,PrimRole,[X|R]) :-
	getRoleName(Env,MS,X),
	assert1(sub(roles,Env,MS,X,PrimRole)),
	direct_succ(roles,Env,MS,[],X,PrimRole,Z,L1),
              contb(roles,Env,MS,Z,L1,PrimRole),
	find_prole24(Env,MS,PrimRole,R),
	!.

find_prole26(roles,Env,MS,Primrole,[C1|R]) :-
	convertMS(Env,[[],true],MS,[],[W1,G1],_),
	call(G1),
	getRoleName(Env,MS,C1),
	subsume1(roles,Env,MS,C1,Primrole),
	find_prole27(roles,Env,MS,Primrole,C1),
  	find_prole26(roles,Env,MS,Primrole,R).

find_prole27(roles,Env,MS,Primrole,C1):-
	assert1(sub(roles,Env,MS,C1,Primrole)),
	direct_succ(roles,Env,MS,[],C1,PrimRole,Z,L1),
              contb(roles,Env,MS,Z,L1,Primrole),
	!.

find_prole3(Env,MS,Primrole,[],Z,Z) :- 
	!.
find_prole3(Env,MS,Primrole,[L|R],Z,K) :-
	L = and(L1),
	intersection(Z,L1,Z1),
	find_prole3(Env,MS,Primrole,R,Z1,K),
	!.

find_prole31(Env,MS,Primrole,[]) :-
	!.
find_prole31(Env,MS,Primrole,[L1|R1]) :-
	find_prole27(roles,Env,MS,Primrole,L1),
	find_prole31(Env,MS,Primrole,R1),
	!.

/****************************************************************/

make_succ(MS) :-           
	currentEnvironment(Env),            
              not(make_succ(concepts,Env,MS)),
	not(make_succ(roles,Env,MS)),!.
make_succ(concepts,Env,MS) :-        
	getConceptName(Env,MS,NewConcept),
	ifOption(testOutput,yes,(print(NewConcept),nl)),
	make_succ2(concepts,Env,MS,NewConcept),
	fail.
make_succ(roles,Env,MS) :-
	getRoleName(Env,MS,NewRole),
	ifOption(testOutput,yes,(print(NewRole),nl)),
	make_succ2(roles,Env,MS,NewRole),
       	fail.
make_succ2(Type,Env,MS,NewConcept) :- 
              NewConcept \== top,!,
              NewConcept \== bot,!,
              direct_succ(Type,Env,MS,[],top,NewConcept,X,L),
              contb(Type,Env,MS,X,L,NewConcept),
              !.

contb(Type,Env,MS,[],L,NewConcept) :- 
        !.

contb(Type,Env,MS,[X|R],L,NewConcept) :-
        setofOrNil(Y,contc(Type,Env,MS,X,Y,L),L1),
	list_to_set(L1,L2),
        make_succ1(Type,Env,MS,X,L2,NewConcept),
        !,
        contb(Type,Env,MS,R,L,NewConcept).
contb(Type,Env,MS,X,L,NewConcept) :-
	list_to_set(L,L1),	
        make_succ1(Type,Env,MS,X,L1,NewConcept),
        !.

contc(Type,Env,MS,X,Y,L) :-
        sub(Type,Env,MS,X,Y),member(Y,L).

direct_succ(Type,Env,MS,_Done,bot,X,_,[]) :- fail.
direct_succ(Type,Env,MS,Done,X,NewConcept,Z,L1) :-
	subsume1(Type,Env,MS,X,NewConcept),
	setofOrNil(Y,(succ1(Type,Env,MS,X,Y), not(member(Y,[X|Done]))),L),
	!,
	check(Type,Env,MS,[X|Done],L,_,X,NewConcept,Z,L1),
	!.
direct_succ(Type,Env,MS,_Done,X,NewConcept,X,[]) :- 
	!.

check(Type,Env,MS,Done,[Y|L],_L1,X,NewConcept,Z,L1) :-
        subsume1(Type,Env,MS,Y,NewConcept),
	!,
        direct_succ(Type,Env,MS,Done,Y,NewConcept,Z1,L10),
	!,        	 				 	 	
	conta(Type,Env,MS,[Y|Done],L,L2,X,NewConcept,Z1,L10,Z,L1),
        !.
check(Type,Env,MS,Done,[Y|L],L2,X,NewConcept,Z,L1) :-
	!,
	check(Type,Env,MS,[Y|Done],L,[Y|L2],X,NewConcept,Z,L1).
check(Type,Env,MS,Done,[],L2,X,NewConcept,X,L1) :-
	check1(Type,Env,MS,Done,L2,NewConcept,L1),
	!.

conta(Type,Env,MS,_Done,[],L2,X,NewConcept,Z1,L10,Z1,L10) :-
        !.
conta(Type,Env,MS,Done,L,L2,X,NewConcept,Z1,L10,Z,L1) :-
        check(Type,Env,MS,Done,L,L2,X,NewConcept,Z2,L11),
 	union1(Z1,Z2,Za),delete1(Za,top,Z),
	union1(L10,L11,L1),
        !.
check1(_,_,_,_,[],_,[]) :- !.
check1(Type,Env,MS,Done,[Y|L],NewConcept,[Y|L1]) :-
	subsume1(Type,Env,MS,NewConcept,Y),
	!,
	check1(Type,Env,MS,[Y|Done],L,NewConcept,L1).
check1(Type,Env,MS,Done,[Y|L],NewConcept,L1) :-
	not(member(Y,Done)),
	setofOrNil(Z,succ1(Type,Env,MS,Y,Z),L2),
	check1(Type,Env,MS,[Y|Done],L2,NewConcept,L3),
	check1(Type,Env,MS,[Y|Done],L,NewConcept,L4),
	union(L3,L4,L5),
	deleteInList(L5,top,L1),
	!.
check1(Type,Env,MS,Done,[Y|L],NewConcept,L1) :-
	check1(Type,Env,MS,[Y|Done],L,NewConcept,L1),
	!.

make_succ1(Type,Env,MS,X,[Y|L],NewConcept) :- 
	not(succ(Type,Env,MS,NewConcept,Y)),
        retract1(succ(Type,Env,MS,X,Y)),
%	assert1(succ(Type,Env,MS,NewConcept,Y)),
        assert_succ(Type,Env,MS,NewConcept,Y),
	!,
	make_succ1(Type,Env,MS,X,L,NewConcept). 
make_succ1(Type,Env,MS,X,[Y|L],NewConcept) :- 
% 	assert1(succ(Type,Env,MS,x,NewConcept)),
        assert_succ(Type,Env,MS,X,NewConcept),
	!,
	make_succ1(Type,Env,MS,X,L,NewConcept).
make_succ1(Type,Env,MS,X,[],NewConcept) :- 
%	assert1(succ(Type,Env,MS,X,NewConcept)),
        assert_succ(Type,Env,MS,X,NewConcept),
	!.


/****************  practical funktions ******************************/	

subsume1(Type,Env,MS,X,Y) :- var(X),!,fail.
subsume1(Type,Env,MS,X,Y) :- var(Y),!,fail.
subsume1(Type,Env,MS,X,top) :- !,fail.
subsume1(Type,Env,MS,bot,X) :- !,fail.
subsume1(Type,Env,MS,X,[]) :- !.
subsume1(Type,Env,MS,X,bot) :- !.
subsume1(Type,Env,MS,top,X) :- !.
subsume1(Type,Env,MS,X,Y) :- 
	sub(Type,Env,MS,X,Y),
	!.
subsume1(Type,Env,MS,X,Y) :- 
	nsub(Type,Env,MS,X,Y),
	!,
	fail. 
subsume1(Type,Env,MS,X,Y) :- 
	X \== Y,
	addCounter(Type,1),
	subsumes(Type,Env,MS,X,Y), 
	cont(Type,Env,MS,[],X,Y),
	!.
subsume1(Type,Env,MS,X,Y) :- 
	X \== Y,
	cont1a(Type,Env,MS,[],X,Y),
	!,
	fail.

cont(Type,Env,MS,_,top,Y).
cont(Type,Env,MS,Done,X,Y) :- 
	assert1(sub(Type,Env,MS,X,Y)),
	succ1(Type,Env,MS,Z,X),
	not(member(Z,Done)),
	cont(Type,Env,MS,[Z|Done],Z,Y),!.
cont(Type,Env,MS,_,X,Y). 
cont1a(Type,Env,MS,_,bot,X) :- 
	!.
cont1a(Type,Env,MS,_,X,bot) :- 
	!,fail.
cont1a(Type,Env,MS,Done,X,Y) :-
       member(X,Done), 
       !.
cont1a(Type,Env,MS,Done,X,Y) :-
	assert1(nsub(Type,Env,MS,X,Y)),
	succ1(Type,Env,MS,X,Z),
	cont1a(Type,Env,MS,[X|Done],Z,Y),
	!.

delete1([X|R],top,Z) :-
	deleteInList([X|R],top,Z),
	!.
delete1(X,top,Z) :-
	!.

union1([],[],[]).
union1([X|R],[Y|R1],Z):-
	union([X|R],[Y|R1],Z),
	!.
union1([X|R],Y,Z) :-
	union([X|R],[Y],Z),
	!.
union1([X],Y,Z) :-
	union([X],[Y],Z),
	!.
union1(X,[Y],Z) :-
	union([X],[Y],Z),
	!.
union1(X,[Y|R],Z) :-
	union([X],[Y|R],Z),
	!.
union1(X,Y,Z) :-
	union([X],[Y],Z),
	!.
assert1(G) :- 
	not(G),
	assert(G),
	!.
assert1(G) :-
	!.
% assert_succ wurde wegen eines Fehlers in direct_succ(bzw conta) veraendert
% duerfte an der Laufzeit nicht sehr viel ausmachen,ging auch einfacher
% als den Fehler zu finden...

assert_succ(Type,Env,MS,X,X) :-
	!.
/*
assert_succ(Type,Env,MS,X,RorC) :-
	assert1(succ(Type,Env,MS,X,RorC)),
	cont(Type,Env,MS,[],X,RorC),
	!.
*/
assert_succ(Type,Env,MS,X,RorC) :-
	cont(Type,Env,MS,[],X,RorC),
	not((sub(Type,Env,MS,X,Y),not(var(Y)),sub(Type,Env,MS,Y,RorC),Y \== RorC)),
	assert1(succ(Type,Env,MS,X,RorC)),
	!.
assert_succ(Type,Env,MS,X,RorC).


retract1(G) :- 
	retract(G),
	!.
retract1(G) :- 
	!.

succ1(Type,Env,MS,X,Y) :- 
	succ(Type,Env,MS,X,Y).
%	!.
succ1(Type,Env,MS,X,bot).
% 	:-  !.

/*****************************************************************************/
/***************** print and statistic - functions ***************************/
newShowHierarchy :-
	show_dag.

show_dag :-
	currentEnvironment(Env),
	show_dag(Env,[]).
show_dag(MS) :-
	currentEnvironment(Env),
	show_dag(Env,MS).
show_dag(Env,MS) :-
	!,
	print('Concepts'),nl,
        not(show_dag(concepts,Env,MS,top,[])),nl,nl,
	print('Roles'),nl,
	not(show_dag(roles,Env,MS,top,[])).
show_dag(Type,Env,MS,bot,_) :- !,fail.
show_dag(Type,Env,MS,Node,L) :-
	writes(L),
	print(Node),nl,
	succ(Type,Env,MS,Node,N),
  	show_dag(Type,Env,MS,N,[45|L]),
	fail.

initStat :-
	!,
	setCounter(subsumptionTests,0),
	setCounter(concepts,0),
	setCounter(roles,0),
	setCounter(conceptsClassified,0),
	setCounter(rolesClassified,0),
	getRuntime(T0),
	setCounter(runtime,T0),
	!.
getStat(CN,CST,RN,RST,T) :-
	!,
	getRuntime(T1),
	getCounter(subsumptionTests,ST),
	getCounter(concepts,CST),
	getCounter(conceptsClassified,CN),
	getCounter(roles,RST),
	getCounter(rolesClassified,RN),
	getCounter(runtime,T0),
	T is T1 - T0,
	!.
printStat :-
	!,
	getStat(CN,CST,RN,RST,T),
	format('Concepts classified:         ~d~n',CN),
	format('Subsumption tests performed: ~d~n',CST),
	format('Roles    classified:         ~d~n',RN),
	format('Subsumption tests performed: ~d~n',RST),
	format('Total runtime:               ~3d sec.~2n',T),
	!.

buildOrdering(Env,MS,CTree,RTree) :- 
	buildOrdering(concepts,Env,MS,top,[],CTree),
	buildOrdering(roles,Env,MS,top,[],RTree),
	!.


buildOrdering(Type,Env,MS,bot,Done,node([bot|EquivClass],[])) :-
	!,
	setofOrNil(Z2,(succ(Type,Env,MS,bot,Z2),succ(Type,Env,MS,Z2,bot)),EquivClass),
	!.
buildOrdering(Type,Env,MS,Concept1,Done,node([Concept1|EquivClass],SubtreeList)) :-
	setofOrNil(Z1,succ(Type,Env,MS,Concept1,Z1),S1),
	setofOrNil(Z2,(succ(Type,Env,MS,Concept1,Z2),succ(Type,Env,MS,Z2,Concept1)),EquivClass),
	successorSet(S1,EquivClass,Succ),
	append(Done,[Concept1|EquivClass],Done1),
	buildOrderingList(Type,Env,MS,Succ,Done1,SubtreeList).

buildOrderingList(_Type,_Env,_MS,[],_Done,[]) :-
	!.
buildOrderingList(Type,Env,MS,[C1|CL],Done,SubtreeList) :-
	member(C1,Done),
	!,
	buildOrderingList(Type,Env,MS,CL,Done,SubtreeList).
buildOrderingList(Type,Env,MS,[C1|CL],Done,[Subtree|SubtreeList]) :-
	buildOrdering(Type,Env,MS,C1,Done,Subtree),
	buildOrderingList(Type,Env,MS,CL,Done,SubtreeList),
	!.

successorSet(S1,EquivClass,S2) :-
	successor_set(S1,EquivClass,S3),
	((S3 \== [], S2 = S3) ; (S2 = [bot])),
	!.

successor_set([],_,[]) :-
	!.
successor_set([C1|CL],EquivClass,S2) :-
	member(C1,EquivClass),
	!,
	successor_set(CL,EquivClass,S2).
successor_set([bot|CL],EquivClass,S2) :-
	!,
	successor_set(CL,EquivClass,S2).
successor_set([C1|CL],EquivClass,[C1|S2]) :-
	successor_set(CL,EquivClass,S2).
/**********************************************************************
 *
 * @(#) compileEnv.pl 1.9@(#)
 *
 */

/**********************************************************************
 *
 * compileEnvironment(FileName)
 * 
 */

compileEnvironment(FileName) :-
	see(FileName),
	read(environment(EnvName,_Env,_Comment)),
	seen,
	compileEnvironment(FileName,EnvName),
	!.
compileEnvironment(FileName) :-
	% Some file handling error has occured
	seen,
	!, 
	fail.

compileEnvironment(FileName,EnvName) :-
	see(FileName),
	read(environment(_EnvName,Env,Comment)),
	(removeEnvironment(EnvName) ; true),
	termExpansion(on,Env,CPList),
	tell('/tmp/compile.tmp'),
	write((:- dynamic(constraint/8))), write('.'), nl,
	write((:- dynamic(numb/1))), write('.'), nl,
%	write((:- dynamic(in/9))), write('.'), nl,
%	write((:- dynamic(kb_in/10))), write('.'), nl,
	write((:- dynamic(falsum/2))), write('.'), nl,
%	write((:- dynamic(conceptName/4))), write('.'), nl,
%	write((:- dynamic(roleName/4))), write('.'), nl,
%	write((:- dynamic(conceptEqualSets/6))), write('.'), nl,
	write((:- dynamic(conceptSubsets/6))), write('.'), nl,
%	write((:- dynamic(eq/9))), write('.'), nl,
	write((:- dynamic(inconsistencyCheck/3))), write('.'), nl,
	write((:- dynamic(roleEqualSets/6))), write('.'), nl,
	write((:- dynamic(roleSubsets/6))), write('.'), nl,
	write((:- dynamic(conceptElement/7))), write('.'), nl,
	write((:- dynamic(roleElement/8))), write('.'), nl,
	write((:- dynamic(closed/5))), write('.'), nl,
	write((:- dynamic(sub/4))), write('.'), nl,
	write((:- dynamic(succ/4))), write('.'), nl,
	write((:- dynamic(nsub/4))), write('.'), nl,
	write((:- dynamic(sub3/2))), write('.'), nl,
	write((:- dynamic(succ3/2))), write('.'), nl,
	write((:- dynamic(nsub3/2))), write('.'), nl,
	write((:- dynamic(abductiveDerivation/3))), write('.'), nl,
	write((:- dynamic(consistencyDerivation/3))), write('.'), nl,
	write((:- dynamic(hypothesis/1))), write('.'), nl,
	write((:- dynamic(roleDomain/4))), write('.'), nl,
	write((:- dynamic(roleRange/4))), write('.'), nl,
	write((:- dynamic(roleDefault/4))), write('.'), nl,
	write((:- dynamic(roleNr/4))), write('.'), nl,
	write((:- dynamic(roleDefNr/4))), write('.'), nl,
	write((:- dynamic(roleAttributes/5))), write('.'), nl,
%	write((:- dynamic(given_inflLink/4))), write('.'), nl,
%	write((:- dynamic(given_change/4))), write('.'), nl,
	write((:- dynamic(value/2))), write('.'), nl,
	write((:- dynamic(option/2))), write('.'), nl,
%	write((:- dynamic(environment/3))), write('.'), nl,
%	write((:- dynamic(conceptHierarchy/3))), write('.'), nl,
%	write((:- dynamic(roleHierarchy/3))), write('.'), nl,
	write((:- dynamic(modalAxiom/6))), write('.'), nl,
%	write((:- dynamic(rel/5))), write('.'), nl,
	write((:- dynamic(compiledPredicate/2))), write('.'), nl,
	writeq((:- asserta(environment(EnvName,Env,Comment)))), write('.'), nl,
	writeq((:- retractall(currentEnvironment(_)))), write('.'), nl,
	writeq((:- asserta(currentEnvironment(Env)))), write('.'), nl,
	writeCompiledPredicateFactsToFile(Env,CPList),
	expand_term((in(Env,Name,modal(MS),CN,CON,hyp(HYP),
                        ab(D),call(CALL),PT) :-
	                   kb_in(Env,pr(5),Name,modal(MS),CN,CON,hyp(HYP),
                                 ab(D),call(CALL),PT)),
		    InClause1),
	writeq(InClause1), write('.'), nl,
	expand_term((in(Env,Name,modal(MS),CN,CON,
                        hyp([or(H1),rl(H2),fl(H3)]),ab(noAb),call(CALL),PT) :-
		           clashInHyp(H2), !, fail),
		    InClause2),
	writeq(InClause2), write('.'), nl,
	expand_term(in(Env,X2,X3,X4,X5,X6,X7,X8,X9), Head3),
	writeq((Head3 :- kb_in(Env,pr(3),X2,X3,X4,X5,X6,X7,X8,X9))),
	write('.'), nl,
	expand_term((in(Env,Name,modal(MS),CN,CON,hyp(HYP),
                        ab(D),call(CALL),PT) :-
		          (CN \== top, CN \== bot, CN \== not(top), 
                           CN \== not(bot),
	                   kb_in(Env,pr(3),Name,modal(MS),CN,CON,hyp(HYP),
                                 ab(D),call(CALL),PT))),
		    InClause4),
	writeq(InClause4), write('.'), nl,
	expand_term((in(Env,Name,modal(MS),CN,CON,hyp(HYP),
                        ab(D),call(CALL),PT) :-
		          (CN \== top,CN \== bot, CN \== not(top), 
                           CN \== not(bot),
			   kb_in(Env,pr(1),Name,modal(MS),CN,CON,hyp(HYP),
				 ab(D),call(CALL),PT))),
		    InClause5),
	writeq(InClause5), write('.'), nl,
	repeat,
	read(Clause),
	treatClause(Clause),
	seen,
	told,
	assertConnectionClauses(Env),
	termExpansion(off,Env),
	compile('/tmp/compile.tmp'),
	!.
compileEnvironment(FileName,EnvName) :-
	% Some file handling error has occured
	seen,
	told,
	!,
	fail.

treatClause('end_of_file') :-
	!.
treatClause((:-dynamic Pred/Arity)) :-
%	write((:-dynamic Pred/Arity)), write('.'), nl,
	!,
	fail.
treatClause((in(_X1,_X2,_X3,_X4,_X5,_X6,_X7,_X8,_X9) :- _Body)) :-
	!,
	fail.
treatClause(X) :-
	expand_term(X,Y),
	writeq(Y), write('.'), nl,
	!,
	fail.

writeCompiledPredicateFactsToFile(Env,[]) :-
	!.
writeCompiledPredicateFactsToFile(Env,[Pred/Arity|List]) :-
	writeq((compiledPredicate(Env,Pred/Arity))),
	write('.'), nl,
	writeCompiledPredicateFactsToFile(Env,List).

assertConnectionClauses(Env) :-
	expand_term(constraint(Env,X2,X3,X4,X5,X6,X7,X8),CompConAtom),
	assertz((constraint(Env,X2,X3,X4,X5,X6,X7,X8) :-
		 CompConAtom)),
	expand_term(eq(Env,X2,X3,X4,X5,X6,X7,X8,X9),CompEqAtom),
	assertz((eq(Env,X2,X3,X4,X5,X6,X7,X8,X9) :-
		 CompEqAtom)),
	expand_term(in(Env,X2,X3,X4,X5,X6,X7,X8,X9),CompInAtom),
	assertz((in(Env,X2,X3,X4,X5,X6,X7,X8,X9) :-
		 CompInAtom)),
%	assertz((kb_in(Env,X2,X3,X4,X5,X6,X7,X8,X9,X10) :-
%		 comp_kb_in(Env,X2,X3,X4,X5,X6,X7,X8,X9,X10))),
	expand_term(rel(Env,X2,X3,X4,X5),CompRelAtom),
	assertz((rel(Env,X2,X3,X4,X5) :-
		 CompRelAtom)),
	!.

termExpansion(on,env(Id),
              [CompCon/8,CompEq/9,CompIn/9,CompKb_in/10,CompRel/6]) :-
	% Generate the names for the compiled in, kb_in, constraint, and rel
	% predicates in environment Id.
	name(Id,IdChars),
	name(in,InChars),
	append(InChars,[95,99,95|IdChars],CompInChars),
	name(CompIn,CompInChars),
	name(constraint,ConChars),
	append(ConChars,[95,99,95|IdChars],CompConChars),
	name(CompCon,CompConChars),
	name(eq,EqChars),
	append(EqChars,[95,99,95|IdChars],CompEqChars),
	name(CompEq,CompEqChars),
	name('kb_in',Kb_inChars),
	append(Kb_inChars,[95,99,95|IdChars],CompKb_inChars),
	name(CompKb_in,CompKb_inChars),
	name('rel',RelChars),
	append(RelChars,[95,99,95|IdChars],CompRelChars),
	name(CompRel,CompRelChars),
	% Abolish any previously asserted clauses for the 
	% compiled predicades
	abolish(CompCon/8),
	abolish(CompEq/9),
	abolish(CompIn/9),
	abolish(CompKb_in/10),
	abolish(CompRel/6),
	% Generate the atoms for these predicates 
	CompConAtom =.. [CompCon|[X4,X1,X2,X3,X5,X6,X7,X8]],
	CompEqAtom =.. [CompEq|[X4-X5,X1,X2,X3,X6,X7,X8,X9]],
	CompInAtom =.. [CompIn|[X4-X5,X1,X2,X3,X6,X7,X8,X9]],
	CompKb_inAtom =.. [CompKb_in|[X5-X6,X1,X2,X3,X4,X7,X8,X9,X10]],
	CompRelAtom =.. [CompRel|[X1,X2,X3,X4,X5,X6]],
	% Assert the term_expansion rules needed to translate the
	% interpreted clauses into compiled clauses.
	abolish(term_expansion/2),
	assertz((term_expansion((Head :- Body),(Head1 :- Body1)) :-
	term_expansion(Head,Head1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L, Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L; Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion(\+Atom,\+Atom1) :-
	term_expansion(Atom,Atom1))),
	assertz((term_expansion(constraint(X1,X2,X3,X4,X5,X6,X7,X8),
				CompConAtom))),
	assertz((term_expansion(eq(X1,X2,X3,X4,X5,X6,X7,X8,X9),
				CompEqAtom))),
	assertz((term_expansion(in(X1,X2,X3,X4,X5,X6,X7,X8,X9),
				CompInAtom))),
	assertz((term_expansion(kb_in(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10),
				CompKb_inAtom))),
	assertz((term_expansion(rel(X1,X2,X3,X4,X5),
				CompRelAtom))),
	assertz((term_expansion(once(Body1),once(Body2)) :-
		term_expansion(Body1,Body2))),
	assertz((term_expansion(call(Body1),call(Body2)) :-
		 term_expansion(Body1,Body2))),
	assertz(term_expansion(X,X)),
	!.
termExpansion(off,_) :-
	abolish(term_expansion/2),
	!.

/**********************************************************************
 *
 * @(#) cnf.pl 1.2@(#)
 *
 */

/***********************************************************************
 *
 * negate(+C1,-C2)
 * C2 is just the term not(C1).
 *
 */

negate(not(C1),C1) :- !.
negate(C1,not(C1)) :- !.

/***********************************************************************
 *
 * invert(+R1,-R2)
 * R2 is just the term inverse(R1).
 *
 */

invert(inverse(R),R) :- !.
invert(R,inverse(R)) :- !.

/***********************************************************************
 *
 * normalizeNot(+C1,-C2)
 * applies the laws
 *      not(and([A,B]))   -> and([not(A),not(B)])
 *      not(or([A,B]))    -> or([not(A),not(B)])
 *      not(not(A))       -> A
 *      not(all(R,C))     -> some(R,not(C))
 *      not(some(R,C))    -> all(R,not(C))
 *      not(atleast(N,R)) -> atmost(N-1,R)
 *      not(atmost(N,R))  -> atleast(N+1,R)
 *      not(b(O,P,C))     -> d(O,P,not(C))
 *      not(d(O,P,C))     -> b(O,P,not(C))
 * to C1 as long as possible to get C2.
 *
 */

normalizeNot(not(and([C1,C2|L1])),or(L3)) :-
	!,
	hop_map(negate,[C1,C2|L1],L2),
	hop_map(normalizeNot,L2,L3).
normalizeNot(not(and([C1])),C2) :-
	negate(C1,C2).
normalizeNot(not(and([])),'bot') :-
	!.
normalizeNot(not(set(L)),not(set(L))) :-
	!.
normalizeNot(not(or([C1,C2|L1])),and(L3)) :-
	!,
	hop_map(negate,[C1,C2|L1],L2),
	hop_map(normalizeNot,L2,L3).
normalizeNot(not(or([C1])),C2) :-
	negate(C1,C2).
normalizeNot(not(or([])),'top') :-
	!.
normalizeNot(not(all(R,C1)),some(R,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(some(R,C1)),all(R,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(atleast(N,R)),atmost(M,R)) :-
	M is N-1.
normalizeNot(not(atmost(N,R)),atleast(M,R)) :-
	M is N+1.
normalizeNot(not(b(O,P,C1)),d(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(d(O,P,C1)),b(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(bc(O,P,C1)),dc(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(dc(O,P,C1)),bc(O,P,C2)) :-
	normalizeNot(not(C1),C2).
normalizeNot(not(b(O,P)),d(O,P)) :-
	!.
normalizeNot(not(d(O,P)),b(O,P)) :-
	!.
normalizeNot(not(bc(O,P)),dc(O,P)) :-
	!.
normalizeNot(not(dc(O,P)),bc(O,P)) :-
	!.
normalizeNot(not(not(C1)),C3) :-
	normalizeNot(C1,C3).
normalizeNot(not(set([])),top) :- !.
normalizeNot(C1,C1).

/***********************************************************************
 *
 * normalizeInverse(+R1,-R2)
 * applies the laws
 *      inverse(and([R,S])) -> and([inverse(R),inverse(S)])
 *      inverse(inverse(R)) -> R
 * to R1 as long as possible to get R2.
 *
 */

normalizeInverse(inverse(and(RL1)),and(RL3)) :-
	hop_map(invert,RL1,RL2),
	hop_map(normalizeInverse,RL2,RL3),
	!.
normalizeInverse(inverse(inverse(R1)),R3) :-
	normalizeInverse(R1,R3).
normalizeInverse(R1,R1).

/***********************************************************************
 *
 * flatten(+C1,-C2)
 * deletes unnecessary occurrences of `and' and `or' in C1 to get C2.
 *
 */

flatten(and(L1),and(L2)) :-
	!,
	hop_map(flatten,L1,L3),
	flattenAnd([],L3,L2).
flatten(or(L1),or(L2)) :-
	!,
	hop_map(flatten,L1,L3),
	flattenOr([],L3,L2).
flatten(set(L1),set(L1)) :-
	!.
flatten(all(R1,C1),all(R2,C2)) :-
	flatten(R1,R2),
	flatten(C1,C2).
flatten(some(R1,C1),some(R2,C2)) :-
	flatten(R1,R2),
	flatten(C1,C2).
flatten(atleast(N,R1),atleast(N,R2)) :-
	flatten(R1,R2).
flatten(atmost(N,R1),atmost(N,R2)) :-
	flatten(R1,R2).
flatten(b(O,P,C1),b(O,P,C2)) :-
	flatten(C1,C2).
flatten(d(O,P,C1),d(O,P,C2)) :-
	flatten(C1,C2).
flatten(bc(O,P,C1),bc(O,P1,C2)) :-
	flatten(P,P1),
	flatten(C1,C2).
flatten(dc(O,P,C1),dc(O,P1,C2)) :-
	flatten(P,P1),
	flatten(C1,C2).
flatten(not(C1),not(C2)) :-
	!,
	flatten(C1,C2).
flatten(inverse(R1),inverse(R2)) :-
	flatten(R1,R2).
flatten(C1,C1).


/***********************************************************************
 *
 * flattenAnd(+L1,+L2,-L3)
 * eliminates occurrences of `and' in L2 to get L2'. L3 is the result
 * of appending L2' to L1.
 *
 */

flattenAnd(L1,[and(L2)|L3],L4) :-
	!,
%	flattenAnd([],L2,L5),
	L5 = L2,
	append(L1,L5,L6),
	flattenAnd(L6,L3,L4).
flattenAnd(L1,[C1|L3],L4) :-
	append(L1,[C1],L6),
	flattenAnd(L6,L3,L4).
flattenAnd(L1,[],L1).

/***********************************************************************
 *
 * flattenOr(+L1,+L2,-L3)
 * eliminates occurrences of `or' in L2 to get L2'. L3 is the result
 * of appending L2' to L1.
 *
 */
 
flattenOr(L1,[or(L2)|L3],L4) :-
	!,
%	flattenOr([],L2,L5),
	L5 = L2,
	append(L1,L5,L6),
	flattenOr(L6,L3,L4).
flattenOr(L1,[C1|L3],L4) :-
	append(L1,[C1],L6),
	flattenOr(L6,L3,L4).
flattenOr(L1,[],L1).

/***********************************************************************
 *
 * distributeAnd(and(+L1),or(+L2),or(-L3))
 * here or(L3) has the form
 *     or([C_1,...,C_n])
 * where C_i is the result of applying de Morgan's laws to
 * and(L1|[A_i]) 
 * where A_i is the ith element of L2.
 *
 */

distributeAnd(and(L1),or([C2|L2]),or([C3|L4])) :-
	% L3 := L1 and C2
	append(L1,[C2],L3),
	% C3 := deMorganAnd(L3)
	deMorgan(and(L3),C3),
	% build other disjuncts
	distributeAnd(and(L1),or(L2),or(L4)).
distributeAnd(and(_L1),or([]),or([])).

/***********************************************************************
 *
 * distributeOr(or(+L1),and(+L2),and(-L3))
 * here and(L3) has the form
 *     and([C_1,...,C_n])
 * where C_i is the result of applying de Morgan's laws to
 * or(L1|[A_i]) 
 * where A_i is the ith element of L2.
 *
 */
	
distributeOr(or(L1),and([C2|L2]),and([C3|L4])) :-
	% L3 := L1 or C2
	append(L1,[C2],L3),
	% C3 := deMorgan(L3)
	deMorgan(or(L3),C3),
	% build other conjuncts
	distributeOr(or(L1),and(L2),and(L4)).
distributeOr(or(_L1),and([]),and([])).
	
/***********************************************************************
 *
 * deMorganAnd(+L1,+L2,-C1)
 * applies de Morgan's law
 *      and([A,or([B,C])]) -> or([and([A,B]),and([A,C])])
 * to and(L1|L2) as long as possible to get C1.
 *
 */

deMorganAnd(L1,[or(L2)|L3],L4) :-
	append(L1,L3,L5),
	distributeAnd(and(L5),or(L2),L4).
deMorganAnd(L1,[C1|L3],L4) :-
	append(L1,[C1],L5),
	deMorganAnd(L5,L3,L4).
deMorganAnd(L1,[],and(L1)).

/***********************************************************************
 *
 * deMorganOr(+L1,+L2,-C1)
 * applies de Morgan's law
 *      or([A,or([B,C])]) -> and([or([A,B]),or([A,C])])
 * to or(L1|L2) as long as possible to get C1.
 *
 */

deMorganOr(L1,[and(L2)|L3],L4) :-
	append(L1,L3,L5),
	distributeOr(or(L5),and(L2),L4).
deMorganOr(L1,[C1|L3],L4) :-
	append(L1,[C1],L5),
	deMorganOr(L5,L3,L4).
deMorganOr(L1,[],or(L1)).

/***********************************************************************
 *
 * deMorgan(+C1,-C2)
 * applies de Morgan's laws to C1
 *      and([A,or([B,C])]) -> or([and([A,B]),and([A,C])])
 *      or([A,and([B,C])]) -> and([or([A,B]),or([A,C])])
 * as long as possible to get C2.
 *
 */

deMorgan(and(L1),C1) :-
	deMorganAnd([],L1,C1).
deMorgan(or(L1),C1) :-
	deMorganOr([],L1,C1).
deMorgan(C1,C1) :-
	!.

/***********************************************************************
 *
 * cnf(+C1,-C2)
 * C2 is the conjunctive normalform of C1.
 *
 */

cnf(C1,C6) :-
	normalizeNot(C1,C2),
	flatten(C2,C3),
	normalizeInverse(C3,C4),
	deMorgan(C4,C5),
	flatten(C5,C6).

/**********************************************************************
 *
 * @(#) conceptFunctions.pl 1.5@(#)
 *
 */

/***********************************************************************
 *
 * memberConcept(+Concept,+Dag)
 * Arguments: Concept     concept name
 *            Dag         subsumption hierarchy
 * checks wether or not Concept occurs in the subsumption hierarchy.
 *
 */

memberConcept(Concept,Dag) :-
	memberElement(Concept,Dag).

memberConceptSubtrees(Concept,List) :-
	memberElementSubtrees(Concept,List).

/***********************************************************************
 *
 * memberDirectSubConcepts(+Concept,+Dag)
 * Arguments: Concept     concept name
 *            Dag         subsumption hierarchy
 * checks wether or not Concept occurs in the direct subconcepts of
 * the top concept of Dag.
 *
 */

memberDirectSubConcepts(Concept,node(_CL,NL)) :-
	!,
	memberDirectSubElements(Concept,NL).

memberDirectSubConcepts(Concept,List) :-
	memberDirectSubElements(Concept,List).

/***********************************************************************
 *
 * getDirectSuperConcepts(+EnvName,+MS,+Concept,-CL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are direct super concepts
 * of Concept.
 *
 */

getDirectSuperConcepts(EnvName,MS,Concept,CL) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	getDirectSuperElements(Concept,CL,Dag).


/***********************************************************************
 *
 * getAllSuperConcepts(+EnvName,+MS,+Concept,-CL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are super concepts of
 * Concept.
 *
 */

getAllSuperConcepts(EnvName,MS,Concept,CL) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	getAllSuperElements(Concept,CL,Dag).

/***********************************************************************
 *
 * getDirectSubConcepts(+EnvName,+MS,+Concept,-CL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are direct super concepts
 * of Concept.
 *
 */

getDirectSubConcepts(EnvName,MS,Concept,CL) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	getDirectSubElements(Concept,CL,Dag).

/***********************************************************************
 *
 * getAllSubConcepts(+EnvName,+MS,+Concept,-CL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Concept     concept name
 *            CL          list of concept names
 * CL is the list of all concept names which are super concepts of 
 * Concept.
 *
 */

getAllSubConcepts(EnvName,MS,Concept,CL) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	getAllSubElements(Concept,CL,Dag).

/***********************************************************************
 *
 * getConcepts(+MS,-CL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            CL          list of concept names
 * CL is the list of all concept names in the subsumption hierarchy.
 *
 */

getConcepts(EnvName,MS,['top'|CL]) :-
	getAllSubConcepts(EnvName,MS,'top',CL).

/***********************************************************************
 *
 * testDirectSuperConcept(+EnvName,+MS,+Concept1,+Concept2,-Concept)
 * Arguments: EnvName        environment identifier
 *            MS             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct superconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct superconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testDirectSuperConcept(EnvName,MS,Concept1,Concept2,Concept) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	testDirectSuperElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * testDirectSubConcept(+EnvName,+MS,+Concept1,+Concept2,-Concept)
 * Arguments: EnvName        environment identifier
 *            MS             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct subconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct subconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testDirectSubConcept(EnvName,MS,Concept1,Concept2,Concept) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	testDirectSubElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * testSuperConcept(+EnvName,+MS,+Concept1,+Concept2,-Concept)
 * Arguments: EnvName        environment identifier
 *            MS             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct superconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct superconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testSuperConcept(EnvName,MS,Concept1,Concept2,Concept) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	testSuperElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * testSubConcept(+EnvName,+MS,+Concept1,+Concept2,-Concept)
 * Arguments: EnvName        environment identifier
 *            MS             modal context
 *            Concept1       concept name
 *            Concept2       concept name
 *            Concept        concept name
 * Concept is Concept1 iff Concept1 is a direct superconcept of Concept2
 * or
 * Concept is Concept2 iff Concept2 is a direct superconcept of Concept1
 * otherwise
 * the predicate fails.
 *
 */

testSubConcept(EnvName,MS,Concept1,Concept2,Concept) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Dag),
	testSubElement(Concept1,Concept2,Concept,Dag).

/***********************************************************************
 *
 * getCommonSuperConcepts(+EnvName,+MS,+CL1,-CL2)
 * Arguments: EnvName  environment identifier
 *            MS       modal context
 *            CL1      list of concept names
 *            CL2      list of concept names
 * CL2 is the list of all concept names subsuming all concepts in CL1.
 *
 */

getCommonSuperConcepts(EnvName,MS,CL1,CL2) :-
	hop_map(getAllSuperConcepts,[EnvName,MS],CL1,CLL1),
	intersection(CLL1,CL2).

/***********************************************************************
 *
 * getCommonSubConcepts(+EnvName,+MS,+CL1,-CL2)
 * Arguments: EnvName  environment identifier
 *            MS       modal context
 *            CL1      list of concept names
 *            CL2      list of concept names
 * CL2 is the list of all concept names which are subsumed by all
 * concepts in CL1.
 *
 */

getCommonSubConcepts(EnvName,MS,CL1,CL2) :-
	hop_map(getAllSubConcepts,[EnvName,MS],CL1,CLL1),
	intersection(CLL1,CL2).

/***********************************************************************
 *
 * getAllObjects(+EnvName,+MS,+O)
 *
 */

getAllObjects(EnvName,MS,O13) :-
	!,
	environment(EnvName,Env,_),
	setofOrNil(X1,[C1,AX1]^(conceptElement(Env,MS,_,user,X1,C1,AX1)),O1),
	setofOrNil(X2,[R2,Y2,AX2]^roleElement(Env,MS,_,user,X2,Y2,R2,AX2),O2),
	setofOrNil(Y3,[R3,X3,AX3]^roleElement(Env,MS,_,user,X3,Y3,R3,AX3),O3),
	union( O1,O2,O12),
	union(O12,O3,O13),
	!.
/**********************************************************************
 *
 * @(#) constraints.pl 1.2@(#)
 *
 */

/**********************************************************************
 *
 * solveConstraint(MS,(card,app((FF:R),X),Rel,N),hyp(HYPS),call(CALLS))
 * if Rel is '>=', 
 *    the predicate succeeds if the cardinality of 
 *    app((FF:R),X) in modal context MS is greater than N.
 *    If N is a variable, it will be instantiated with the greatest
 *    number M such that the cardinality of  app((FF:R),X) in modal 
 *    context MS is provably greater than M.
 * if Rel is '=<', 
 *    the predicate succeeds if the cardinality of 
 *    app((FF:R),X) in modal context MS is smaller than N.
 *    If N is a variable, it will be instantiated with the greatest
 *    number M such that the cardinality of  app((FF:R),X) in modal 
 *    context MS is provably smaller than M.
 *
 */

solveConstraint(Env,MS,(card,app((FF:R),X),Rel,N),(M,S),hyp(HYPS),ab(D),call(CALLS),PTO) :-
%	SolveHead = solveConstraint(MS,(card,app((FF:R),X),Rel,N),hyp(HYPS)),
%	cCS(CALLS,SolveHead),
%	CALLS1 = [SolveHead|CALLS],
	length(CALLS,XXX),
%	format('trying ~d  solve(~w(~w)) ~w ~w~n',[XXX,R,X,Rel,N]),
	collectAllFillers(Env,MS,R,X,HYPS,D,CALLS,S),
	computeNumber(S,Rel,(M,PTAbox)),
	continueSolve(Env,MS,(card,app((FF:R),X),Rel,N),hyp(HYPS),ab(D),call(CALLS),(M,PTAbox),PT),
	PTO = proved(card(R,X,Rel,N),hyp(HYPS),basedOn(PT)).
	

computeNumber([],'=<',(noRestriction,basedOn(noAboxEntries))) :- !.
computeNumber([],'>=',(noRestriction,basedOn(noAboxEntries))) :- !.
computeNumber(S,_Rel,(M,and(PL))) :-
	reduceToSolutionSet(S,EL,PL),
	length(EL,M).

reduceToSolutionSet([],[],[]) :- !.
reduceToSolutionSet([(E1,PT1,_)|L],L2,L3) :-
	member((E1,_PT2,_R2),L),
	!,
	reduceToSolutionSet(L,L2,L3).
reduceToSolutionSet([(E1,PT1,_)|L],[E1|L2],[PT1|L3]) :-
	reduceToSolutionSet(L,L2,L3).

continueSolve(_,_,(card,_,'=<',N),hyp(_),ab(_),call(_),(M,_PTAbox),_) :-
	number(M),
	nonvar(N),
	M >= N,
	!,
	fail.
continueSolve(Env,MS,(card,app((FF:R),X),Rel,N),hyp(HYPS),ab(D),call(CALLS),(M1,PTAbox),PT3) :-
	collectAllConstraints(Env,MS,FF,R,X,Rel,HYPS,D,CALLS,S),
	findNumberRestriction(Rel,(M1,PTAbox),S,(M3,PT3)),
	!,
	comparison(Rel,M3,N).

collectAllFillers(Env,MS,R,X,HYPS,D,CALLS,S) :-
	EqLiteral = eqGenerator(Env,AX,RN,S,O,MS,Y,app((FF:R),X),HYPS,D,CALLS,PT),
	bagof((Y,PT,[Env,MS,R,X,HYPS,D,CALLS]),AX^RN^S^O^FF^EqLiteral,S),
	!.
collectAllFillers(_,_,_,_,_,_,_,[]) :-
	!.


collectAllConstraints(Env,MS,FF,R,X,Rel,HYPS,D,CALLS,S) :-
%	constructConHead(Env,rn(AX,RN,S,O),MS,FF,R,X,Rel,M2,HYPS,D,CALLS,PT,C1),
	C1 = constraint(Env,rn(AX,RN,S,O),MS,(card,app((FF:R),X),Rel,M2),
                       hyp(HYPS),ab(D),call(CALLS),PT),
	bagof((M2,PT,[Env,MS,FF,R,X,Rel,HYPS,D,CALLS]),AX^RN^S^O^FF^C1,S),
	!.
collectAllConstraints(_,_MS,_FF,_R,_X,_Rel,_HYPS,_D,_CALLS,[]) :-
	!.


/**********************************************************************
 * 
 * comparison(+Rel,+M,?N)
 * if N is a variable then N is instantiated with M and the predicate
 * succeeds.
 * if N is a number, the predicates succeeds if then goal Rel(M,N)
 * succeeds.
 *
 */

comparison(_Rel,M3,N) :-
	var(N),
	!,
	N = M3.
comparison(Rel,M3,N) :-
	number(M3), number(N),
	Goal =.. [Rel,M3,N],
	call(Goal).
	
/**********************************************************************
 *
 * findNumberRestristriction(+Rel,+L,-N)
 * if Rel is '=<' then N will be instantiated with the smallest number
 * in the list of numbers L.
 * if Rel is '>=' then N will be instantiated with the greatest number
 * in the list of numbers L.
 *
 */

findNumberRestriction('=<',(noRestriction,PT1),[],(1000000,PT1)) :- !.
findNumberRestriction('>=',(noRestriction,PT1),[],(0,PT1)) :- !.
findNumberRestriction('>=',(N,PT1),[],(N,PT1)) :- !.
findNumberRestriction('=<',(N,PT1),[],(1000000,noConstraintsFound)) :- !.
findNumberRestriction(_,(noRestriction,_),[(N1,PT2,_)],(N1,PT2)) :- !.
findNumberRestriction('=<',(M,_PT1),[(N1,PT2,_)],(N1,PT2)) :-
	N1 =< M,
	!.
findNumberRestriction('=<',(M,PT1),[(_N1,_,_)],(M,PT1)) :-
	!.
findNumberRestriction('>=',(M,PT1),[(N1,_,_)],(M,PT1)) :-
	M >= N1,
	!.
findNumberRestriction('>=',(_M,_),[(N1,PT2,_)],(N1,PT2)) :-
	!.
findNumberRestriction('>=',(K,PT1),[(N1,_,_)|NL],(N2,PT3)) :-
	findNumberRestriction('>=',(K,PT1,_),NL,(N2,PT3)),
	N2 >= N1,
	!.
findNumberRestriction('=<',(K,PT1),[(N1,_,_)|NL],(N2,PT3)) :-
	findNumberRestriction('=<',(K,PT1,_),NL,(N2,PT3)),
	N2 =< N1,
	!.
findNumberRestriction(_,_,[(N1,PT1,_)|_NL],(N1,PT1)) :-
	!.








/**********************************************************************
 *
 * @(#) construct.pl 1.11@(#)
 *
 */
inProofTerm(MS,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	nonvar(AX),
	conceptSubsets(_Env,_user,MS1,C1,C2,AX),
	PT = proved(in(MS1,D,X),usingAxiom(defprimconcept(C1,C2)),basedOn(PT1)),
	!.
inProofTerm(MS,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	nonvar(AX),
	conceptEqualSets(_Env,_user,MS1,C1,C2,AX),
	PT = proved(in(MS1,D,X),usingAxiom(defconcept(C1,C2)),basedOn(PT1)),
	!.
inProofTerm(MS,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	nonvar(AX),
	conceptElement(_Env,MS1,_,user,C1,C2,AX),
	PT = proved(in(MS1,D,X),usingAxiom(assert_ind(C1,C2)),basedOn(PT1)),
	!.
inProofTerm(MS,rn(AX,Rule,_,_),D,X,HYPS,PT1,PT) :-
	!.

inProofTerm(MS,Name,D,X,HYPS,PT1,PT) :-
	PT = proved(in(MS,Name,D,X),basedOn(PT1)),
	!.
inProofTerm(MS,D,X,HYPS,PT1,PT) :-
	PT = proved(in(MS,D,X),basedOn(PT1)),
	!.

eqProofTerm(MS,Y,_FF,R,X,HYPS,PT1,PT) :-
	nonvar(R),
	atomic(R),
	!,
	Rel =.. [R,MS,X,Y],
	PT = proved(Rel,basedOn(PT1)),
	!.
eqProofTerm(MS,Y,_FF,R,X,HYPS,PT1,PT) :-
	Rel = rel(R,MS,X,Y),
	PT = proved(Rel,basedOn(PT1)),
	!.
conProofTerm(MS,R,X,Rel,N,HYPS,PT1,PT) :-
	PT = proved(card(R,MS,X,Rel,N),basedOn(PT1)),
	!.
/***********************************************************************
 *
 * makeTerm(+TermPieces,-Term)
 *
 */

makeTerm(Term,Term) :-
	var(Term),
	!.
makeTerm(Term,Term) :-
	atomic(Term),
	!.
makeTerm([Functor|ArgList],Term) :-
	hop_map(makeTerm,ArgListTerms,ArgList),
	Term =.. [Functor|ArgListTerms].
makeTerm(Term,Term).

%element(X) :-
%	atomic(X),
%	!.
%element(X) :-
%	var(X),
%	!.
element(_) :- !.

relation(R,RN,X1,Y1) :-
%	nonvar(R),
%	R =.. [RN,X1,Y1].
	T =.. [RN,X1,Y1],
	R = T.

eqGenerator(Env,AX,RN,S,O,MS,X,Y,HYPS,D,CALLS,PT) :-
	eq(Env,rn(AX,RN,S,O),modal(MS),X,Y,hyp(HYPS),ab(D),call(CALLS),PT),
	nonvar(X),
	nonvar(Y),
	atomic(X).

gensymbol(Symbol,_L,NewSymbol) :-
	gensym(Symbol,NewSymbol),
	!.


/**********************************************************************
 *
 * ruleName(+AxiomName,+RuleName,+Orientation)
 *
 */

ruleName(AxiomName,RuleName,Origin,Orientation,
	 rn(AxiomName,RuleName,Origin,Orientation)) :- 
	!.

reverseOrientation(lInR,rInL) :- !.
reverseOrientation(rInL,lInR) :- !.


typeOfDefinition(_,_,C,system) :-
	var(C),
	!.
typeOfDefinition(Env,MS,C,user) :-
	getConceptName(Env,MS,C),
	!.
typeOfDefinition(_,_,C,system) :-
	atomic(C),
	name(C,[99,111,110,99,101,112,116|_]),
	!.
typeOfDefinition(Env,MS,R,user) :-
	getRoleName(Env,MS,R),
	!.
typeOfDefinition(_,_,R,system) :-
	atomic(R),
	name(R,[114,111,108,101|_]),
	!.
typeOfDefinition(Env,MS,not(C),Type) :-
	!,
	typeOfDefinition(Env,MS,C,Type).
typeOfDefinition(_,_,normal(C),system) :-
	!.
typeOfDefinition(_,_,not(normal(C)),system) :-
	!.
typeOfDefinition(_,_,_,user) :-
	!.

% someInterpretation([]).
% someInterpretation([I1|IL]) :-
% 	call(I1),
% 	someInterpretation(IL).
% 
% allInterpretation([]) :-
% 	fail.
% allInterpretation([I1|IL]) :-
% 	(call(I1) ; allInterpretation(IL)).
% 
% roleConjunction(X,IL) :-
% 	var(X),
% 	someInterpretation(IL).
% roleConjunction(X,IL) :-
% 	nonvar(X),
% 	name(X,[115,107,111,108,101,109|_]),
% 	allInterpretation(IL).
% roleConjunction(X,IL) :-
% 	nonvar(X),
% 	not(name(X,[115,107,111,108,101,109|_])),
% 	someInterpretation(IL).


/***********************************************************************
 *
 * convertMS(Env,+MS1,+ModalOperator,WVL1,-MS2,WVL2)
 * Arguments: MS1                modal context
 *            ModalOperator      modal operator
 *            WVL1               list of free world variables already
 *                               generated during the conversion
 *            MS2                modal context
 *            WVL2               list of all free world variables 
 *                               generated during the conversion
 * MS2 is the translation of ModalOperator appended to MS1.
 *
 */

genagent(X,_,X) :-
	var(X),
	!.
genagent(all,free,_A) :-
	!.
genagent(all,skolemize,A) :-
	gensym(agent,A),
	!.
genagent(A,_,A) :-
	!.

convertMS(positive,Env,Start,MS,WVL1,End,WVL2) :-
	!,
	convertMS(Env,Start,MS,WVL1,End,WVL2).
convertMS(negative,Env,Start,MS1,WVL1,End,WVL2) :-
	!,
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	convertMS(Env,Start,MS3,WVL1,End,WVL2).
	

convertMS(_Env,_,MS,WVL,[_W1,true],WVL) :-
	var(MS),
	!.
convertMS(_Env,[MS1,Lits1],[],WVL,[MS1,Lits1],WVL) :-
	!.
convertMS(Env,[MS1,Lits1],[d(MOp,A)|L],WVL,[MS3,Lits3],WVL3) :-
	gensym(wp,WP),
	WPTerm = [WP,WVL],
	genagent(A,skolemize,Agent),
	MS2 = app(WPTerm:m(MOp,Agent),MS1),
	convertMS(Env,[MS2,Lits1],L,WVL,[MS3,Lits3],WVL3),
	!.
convertMS(Env,[MS1,Lits1],[b(MOp,A)|L],WVL,[MS3,Lits3],WVL3) :-
	genagent(A,free,Agent),
	Lit = rel(Env,_,m(MOp,Agent),MS1,MS2),
	convertMS(Env,[MS2,(Lit,Lits1)],L,[MS2|WVL],[MS3,Lits3],WVL3),
	!.
convertMS(Env,[MS1,Lits1],[dc(MOp,C)|L],WVL,[MS3,Lits3],WVL3) :-
	gensym(wp,WP),
	WPTerm = [WP,WVL],
	genagent(all,skolemize,Agent),
	MS2 = app(WPTerm:m(MOp,Agent),MS1),
	getQuery(Env,MS1,C,Agent,_Exp,Body),
	convertMS(Env,[MS2,(once(Body),Lits1)],L,WVL,[MS3,Lits3],WVL3),
	!.
convertMS(Env,[MS1,Lits1],[bc(MOp,C)|L],WVL,[MS3,Lits3],WVL3) :-
	genagent(all,free,Agent),
	Lit = rel(Env,_,m(MOp,Agent),MS1,MS2),
	getQuery(Env,MS1,C,Agent,_Exp,Body),
	convertMS(Env,[MS2,((once(Body),Lit),Lits1)],L,[MS2|WVL],[MS3,Lits3],WVL3),
	!.


/***********************************************************************
 *
 * THE STRUCTURE OF THE IN-CLAUSES
 * 
 * 1) THE HEAD
 *    in(Env,RN,modal(W),A1,X,hyp(C1),ab(D),call(H1),Exp)
 *    Env is a internal environment name
 *    RN  is a rule name
 *    W   is a world
 *    A1  is a concept name or the negation of a concept name
 *    X   is a free variable
 *    C1  is a list of clauses --- the hypotheses that can be used
 *    D   is a name identifying a specific abductive derivation
 *    H1  is a list of calls   --- the calls to in that have already
 *                                 been used
 *    Exp is a explanation term
 * 2) THE BODY
 *
 */

/***********************************************************************
 *
 * constructMLHead(+ModalSequence,
 *                 +ConceptName,+Constraint,
 *                 +Hypotheses,+CallStack,-Inhead)
 * 
 */

constructInHead(Env,Name,MS,CN,CON,HYP,D,CALL,PT1,InHead) :-
	inProofTerm(MS,Name,CN,CON,HYP,PT1,PT),
	InHead = in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
	!.

constructKBHead(Env,Priority,Name,MS,CN,CON,HYP,D,CALL,PT1,InHead) :-
	inProofTerm(MS,Name,CN,CON,HYP,PT1,PT),
	InHead = kb_in(Env,Priority,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
	!.

constructMLHead(Env,Name,MS,CN,CON,HYP,D,CALL,PT1,InHead) :-
	inProofTerm(MS,Name,CN,CON,HYP,PT1,PT),
	InHead = kb_in(Env,pr(3),Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
	!.

getEnvironment(kb_in(Env,pr(_),_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),Env).
getEnvironment(in(Env,_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),Env).
getModalSequence(kb_in(_,pr(_),_,modal(MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),MS).
getModalSequence(in(_,_,modal(MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),MS).
getConceptName(kb_in(_,pr(_),_,modal(_MS),CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),CN).
getConceptName(in(_,_,modal(_MS),CN,_CON,hyp(_HYP),ab(_),call(_CALL),_),CN).
getConstraint(kb_in(_,pr(_),_,modal(_MS),_CN,CON,hyp(_HYP),ab(_),call(_CALL),_),CON).
getConstraint(in(_,_,modal(_MS),_CN,CON,hyp(_HYP),ab(_),call(_CALL),_),CON).
getHypotheses(kb_in(_,pr(_),_,modal(_MS),_CN,_CON,hyp(HYP),ab(_),call(_CALL),_),HYP).
getHypotheses(in(_,_,modal(_MS),_CN,_CON,hyp(HYP),ab(_),call(_CALL),_),HYP).
getCallStack(kb_in(_,pr(_),_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(CALL),_),CALL).
getCallStack(in(_,_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(CALL),_),CALL).
getExplanation(kb_in(_,pr(_),_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),E),E).
getExplanation(in(_,_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),E),E).
getInExplanation(kb_in(_,pr(_),_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),
	         proved(I,_)),I).
getInExplanation(in(_,_,modal(_MS),_CN,_CON,hyp(_HYP),ab(_),call(_CALL),
	         proved(I,_)),I).

/**********************************************************************
 *
 * constructEqHead(Env,+MS,+Y,+F,+R,+X,+HYPS,+CALLS,-L)
 *
 */

constructEqHead(Env,Name,MS,Y,F,R,X,HYPS,D,CALLS,PT1,L) :-
	eqProofTerm(MS,Y,F,R,X,HYPS,PT1,PT),
	L = eq(Env,Name,modal(MS),Y,app((F:R),X),hyp(HYPS),ab(D),call(CALLS),PT),
	!.

/**********************************************************************
 *
 * constructEqMark(+MS,+Y,+F,+R,+X,+HYPS,+CALLS,+AN,-L)
 *
 */

constructEqMark(Name,MS,Y,F,R,X,HYPS,_D,_CALLS,L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = eq(Name,modal(MS),Y,app((F:R),X),hyp(H1)),
	!.

/**********************************************************************
 *
 * constructEqCall(Env,+MS,+Y,+F,+R,+X,+HYPS,+CALLS,+AN,-L)
 *
 */

constructEqCall(Env,rn(AX,RN,_Source,Orientation),bodyMC(MS1),headMC(MS2),
	        Y,F,R,X,HYPS,D,CALLS,PT,L) :-
	constructEqMark(rn(AX,RN,_S1,Orientation),MS2,Y,F,R,X,HYPS,D,CALLS,C1),
	L = eq(Env,rn(_AX2,_RN2,_S2,_O2),modal(MS1),Y,app((F:R),X),
               hyp(HYPS),ab(D),call([C1|CALLS]),PT),
	!.

/***********************************************************************
 *
 * constructMLMark(+ModalSequence,+ConceptName,+Constraint,
 *                      +AxiomName,-LoopCheck)
 *
 */

constructMLMark(Name,MS,CN,CON,HYPS,D,LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = in(Name,modal(MS),CN,CON,hyp(H1),ab(D)),
	!.

constructMLMark(kb_in(_,Pr,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(_),_),LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = in(Name,modal(MS),CN,CON,hyp(H1),ab(D)),
	!.
constructMLMark(in(_,Pr,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(_),_),LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = in(Name,modal(MS),CN,CON,hyp(H1),ab(D)),
	!.
constructMLMark(in(_,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(_),_),LoopCheck) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	LoopCheck = in(Name,modal(MS),CN,CON,hyp(H1),ab(D)),
	!.

/***********************************************************************
 *
 * constructMLCall(+Env,rn(+AX1,+RN1,+S1,+O1),bodyMC(MS1),headMC(MS2), 
 *                 +ConceptName,+Variable,
 *                 +Hypotheses,+DerivationName,+CallStack,+Proofterm,
 *                 -InTerm)
 *
 * The information in rn(AX1,RN1,S1,O1)  is used in the following way:
 * AX1, RN1, and O1 is used in the construction of the MLMark
 * which is added to the call stack. If AX1 is `no' then the MLMark is
 * not added to the call stack at all.
 * S1 is used in the construction of InHead. If S1 is a variable, any 
 * rule can be used to prove the call. If S1 is `user' then only user
 * provided rules may be used. If S1 is `system' then only system provided
 * rules may be used.
 *
 */

constructMLCall(Env,rn(AX1,RN1,S1,O1),bodyMC(MS1),headMC(MS2),
                CN,CON,HYPS,D,CALLS,PT1,InHead) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	constructMLMark(rn(AX1,RN1,_S2,_O2),MS2,CN,CON,HYPS,D,Mark),
	convert_loop(AX1,CALLS,Mark,CALLS1),
	getNegatedConcept(CN,C2),
	InHeadH = in(_NameH,modal(MS1),C2,CON,hyp(_H),ab(_D)),
	getSource(S1,Source),
	InHead = in(Env,rn(_AX3,_RN3,Source,_O3),modal(MS1),CN,CON,
                    hyp([or(H1),rl([InHeadH|H2]),fl(H3)]),
                    ab(D),call(CALLS1),PT1),
	!.


getSource(V1,_V2) :-
	var(V1),
	!.
getSource(any,_V2) :-
	!.
getSource(V1,V1) :-
	!.

getNegatedConcept(CN,not(CN)) :-
	var(CN),
	!.
getNegatedConcept(CN,C2) :-
	normalizeNot(not(CN),C2),
	!.

/***********************************************************************
 *
 * constructConHead(Env,+Name,+MS,+F,+R,+X,+Rel,+N,
 *                  +HYPS,+CALLS,-Literal)
 *
 */

constructConHead(Env,Name,MS,F,R,X,Rel,N,HYPS,D,CALLS,PT1,L) :-
	conProofTerm(MS,R,X,Rel,N,HYPS,PT1,PT),
	L = constraint(Env,Name,MS,(card,app((F:R),X),Rel,N),
                       hyp(HYPS),ab(D),call(CALLS),PT),
	!.

/***********************************************************************
 *
 * constructConMark(+MS,+F,+R,+X,+Rel,+N,+HYPS,+CALLS,+AN,-Literal)
 *
 */

constructConMark(Name,MS,F,R,X,Rel,N,HYPS,_D,_CALLS,L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = constraint(Name,MS,(card,app((F:R),X),Rel,N),hyp(H1)),
	!.

constructConMark(constraint(_,Name,MS,(card,A,Rel,N),hyp(HYPS),ab(_D),call(_CALLS),_PT),L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = constraint(Name,MS,(card,A,Rel,N),hyp(H1)),
	!.

/***********************************************************************
 *
 * constructSolveConMark(+MS,+F,+R,+X,+Rel,+N,+HYPS,+CALLS,+AN,-Literal)
 *
 */

constructSolveConMark(Name,MS,F,R,X,Rel,N,HYPS,_D,_CALLS,L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = solveConstraint(Name,MS,(card,app((F:R),X),Rel,N),hyp(H1)),
	!.

constructSolveConMark(constraint(_,Name,MS,(card,A,Rel,N),hyp(HYPS),ab(_D),call(_CALLS),_PT),L) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	L = solveConstraint(Name,MS,(card,A,Rel,N),hyp(H1)),
	!.

/***********************************************************************
 *
 * constructConCall(Env,+MS,+F,+R,+X,+Rel,+N,+HYPS,+CALLS,+AN,-Literal)
 *
 */

constructConCall(Env,bodyMC(MS1),headMC(MS2),F,R,X,Rel,N,HYPS,D,CALLS,AN,PT1,L) :-
	constructConMark(MS2,F,R,X,Rel,N,HYPS,D,CALLS,AN,Mark),
        L = constraint(Env,_Name,MS1,(card,app((F:R),X),Rel,N),
                       hyp(HYPS),ab(D),call([Mark|CALLS]),PT1),
	!.


addDefaultML(I1,L1) :-
	var(L1),
	!,
	L1 = [I1|_L2],
	!.
addDefaultML(I1,[_|L1]) :-
	addDefaultML(I1,L1),
	!.

memberDML(I1,L) :-
	nonvar(L),
	L = [I1|L2],
	!.
memberDML(I1,L) :-
	nonvar(L),
	L = [_|L2],
	memberDML(I1,L2).


/**********************************************************************
 *
 * getAxiom(+Env,+MS,AX)
 * succeeds if AX is an axiom in environment Env and modal context
 * MS.
 *
 */
	
getAxiom(Env,MS,Ax) :-
	axiom(Env,MS,Ax).

/**********************************************************************
 *
 * getConceptName(+Env,+MS,CN)
 * succeeds if CN is a concept name in environment Env and modal context
 * MS.
 *
 */

getConceptName(Env,MS1,CN) :-
	convertMS(negative,Env,[[],true],MS1,[],[W1,G1],_),
	clause(conceptName(Env,_,W1,CN),_),
	once((call(G1),conceptName(Env,_,W1,CN))).

/**********************************************************************
 *
 * getRoleName(+Env,+MS,CN)
 * succeeds if CN is a role name in environment Env and modal context
 * MS.
 *
 */

getRoleName(Env,MS1,CN) :-
	convertMS(negative,Env,[[],true],MS3,[],[W1,G1],_),
	clause(roleName(Env,_,W1,CN),_),
	once((call(G1),roleName(Env,_,W1,CN))).


/**********************************************************************
 *
 * @(#) dag.pl 1.3@(#)
 *
 */

/***********************************************************************
 *
 * memberElement(+Element,+Dag)
 * Parameter: Element     element name
 *            Dag         subsumption hierarchy
 * checks wether or not Element occurs in the subsumption hierarchy.
 *
 */

memberElement(Element,node(CL,_NL)) :-
	member(Element,CL),
	!.
memberElement(Element,node(_CL,NL)) :-
	memberElementSubtrees(Element,NL),
	!.

memberElementSubtrees(_Element,[]) :-
	!,
	fail.
memberElementSubtrees(Element,[N1|_NL]) :-
	memberElement(Element,N1),
	!.
memberElementSubtrees(Element,[_N1|NL]) :-
	memberElementSubtrees(Element,NL).

/***********************************************************************
 *
 * memberDirectSubElements(+Element,+Dag)
 * Parameter: Element     element name
 *            Dag         subsumption hierarchy
 * checks wether or not Element occurs in the direct subelements of
 * the top element of Dag.
 *
 */

memberDirectSubElements(Element,node(_CL,NL)) :-
	!,
	memberDirectSubElements(Element,NL).

memberDirectSubElements(_Element,[]) :-
	!,
	fail.
memberDirectSubElements(Element,[node(CL,_NL1)|_NL]) :-
	member(Element,CL),
	!.
memberDirectSubElements(Element,[_N1|NL]) :-
	memberDirectSubElements(Element,NL).

/***********************************************************************
 *
 * getDirectSuperElements(+Element,-CL,+Dag)
 * Parameter: Dag         subsumption hierarchy
 *            Element     element name
 *            CL          list of element names
 * CL is the list of all element names which are direct super elements
 * of Element.
 *
 */

getDirectSuperElements(Element,CL,node(CL,NL)) :-
	memberDirectSubElements(Element,node(CL,NL)),
	!.
getDirectSuperElements(Element,CL,node(_,NL)) :-
	getDirectSuperElements(Element,CL,NL).

getDirectSuperElements(_Element,[],[]) :-
	!.
getDirectSuperElements(Element,CL,[N1|NL]) :-
	getDirectSuperElements(Element,CL1,N1),
	getDirectSuperElements(Element,CL2,NL),
	union(CL1,CL2,CL).

/***********************************************************************
 *
 * getAllSuperElements(+Element,-CL,+Dag)
 * Parameter: Element     element name
 *            CL          list of element names
 *            Dag         subsumption hierarchy
 * CL is the list of all element names which are super elements of
 * Element.
 *
 */

getAllSuperElements(Element,CL,Dag) :-
	getAllSuperElements(Element,CL,[],Dag).

getAllSuperElements(Element,CL1,CL1,node(CL,_NL)) :-
	member(Element,CL),
	!.
getAllSuperElements(Element,CL3,CL1,node(CL,NL)) :-
	union(CL,CL1,CL2),
	getAllSuperElements(Element,CL3,CL2,NL).

getAllSuperElements(_Element,[],_CL1,[]) :-
	!.
getAllSuperElements(Element,CL2,CL1,[N1|NL]) :-
	getAllSuperElements(Element,CL3,CL1,N1),
	getAllSuperElements(Element,CL4,CL1,NL),
	union(CL3,CL4,CL2).


/***********************************************************************
 *
 * getDirectSubElements(+Element,-CL,+Dag)
 * Parameter: Element     element name
 *            CL          list of element names
 *            Dag         subsumption hierarchy
 * CL is the list of all element names which are direct sub elements
 * of Element.
 *
 */

getDirectSubElements(Element,CL1,node(CL,NL)) :-
	member(Element,CL),
	!,
	getSubElements(CL1,NL).
getDirectSubElements(Element,CL1,node(_CL,NL)) :-
	getDirectSubElements(Element,CL1,NL).

getDirectSubElements(_Element,[],[]) :-
	!.
getDirectSubElements(Element,CL,[N1|NL]) :-
	getDirectSubElements(Element,CL1,N1),
	getDirectSubElements(Element,CL2,NL),
	union(CL1,CL2,CL).

getSubElements([],[]) :-
	!.
getSubElements(CL,[node(CL1,_)|NL]) :-
	getSubElements(CL2,NL),
	union(CL1,CL2,CL).


/***********************************************************************
 *
 * getAllSubElements(+Dag,+Element,-CL,+Dag)
 * Parameter: Element     element name
 *            CL          list of element names
 *            Dag         subsumption hierarchy
 * CL is the list of all element names which are sub elements of 
 * Element
 *
 */

getAllSubElements(Element,CL1,node(CL,NL)) :-
	member(Element,CL),
	!,
	getElements(CL1,NL).
getAllSubElements(Element,CL1,node(_CL,NL)) :-
	getAllSubElements(Element,CL1,NL),
	!.

getAllSubElements(_Element,[],[]) :-
	!.
getAllSubElements(Element,CL,[N1|NL1]) :-
	getAllSubElements(Element,CL2,N1),
	getAllSubElements(Element,CL3,NL1),
	union(CL2,CL3,CL).

/***********************************************************************
 *
 * getElements(-CL,+Dag)
 * Parameter: CL     list of element names
 *            Dag    subsumption hierarchy
 * CL is the list of all element names in the subsumption hierarchy.
 *
 */

getElements(CL,node(CL1,NL)) :-
	getElements(CL2,NL),
	union(CL1,CL2,CL).
getElements([],[]) :-
	!.
getElements(CL,[N1|NL]) :-
	getElements(CL1,N1),
	getElements(CL2,NL),
	union(CL1,CL2,CL).


/***********************************************************************
 *
 * testDirectSuperElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct superelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct superelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testDirectSuperElement(Element1,Element2,Element1,node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberDirectSubElements(Element2,node(CL,NL)).
testDirectSuperElement(Element1,Element2,Element2,node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberDirectSubElements(Element1,node(CL,NL)).

/***********************************************************************
 *
 * testDirectSubElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct subelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct subelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testDirectSubElement(Element1,Element2,Element2,node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberDirectSubElements(Element2,node(CL,NL)).
testDirectSubElement(Element1,Element2,Element1,node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberDirectSubElements(Element1,node(CL,NL)).


/***********************************************************************
 *
 * testSuperElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct superelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct superelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testSuperElement(Element1,Element2,Element1,node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberElementSubtrees(Element2,NL).
testSuperElement(Element1,Element2,Element2,node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberElementSubtrees(Element1,NL).


/***********************************************************************
 *
 * testSubElement(+Element1,+Element2,-Element,+Dag)
 * Parameter: Element1       element name
 *            Element2       element name
 *            Element        element name
 *            Dag            subsumption hierarchy
 * Element is Element1 iff Element1 is a direct superelement of Element2
 * or
 * Element is Element2 iff Element2 is a direct superelement of Element1
 * otherwise
 * the predicate fails
 *
 */

testSubElement(Element1,Element2,Element1,node(CL,NL)) :-
	member(Element2,CL),
	!,
	memberElementSubtrees(Element1,NL).
testSubElement(Element1,Element2,Element2,node(CL,NL)) :-
	member(Element1,CL),
	!,
	memberElementSubtrees(Element2,NL).


/***********************************************************************
 *
 * getCommonSuperElements(+CL1,-CL2,+Dag)
 * Parameter: CL1      list of element names
 *            CL2      list of element names
 *            Dag      subsumption hierarchy
 * CL2 is the list of all element names subsuming all elements in CL1.
 *
 */

getCommonSuperElements(CL1,CL2,Dag) :-
	hop_map(getAllSuperElements,[Dag],CL1,CLL1),
	intersection(CLL1,CL2).

/***********************************************************************
 *
 * getCommonSubElements(+CL1,-CL2,Dag)
 * Parameter: CL1      list of element names
 *            CL2      list of element names
 *            Dag      subsumption hierarchy
 * CL2 is the list of all element names which are subsumed by all
 * elements in CL1.
 *
 */

getCommonSubElements(CL1,CL2,Dag) :-
	hop_map(getAllSubElements,[Dag],CL1,CLL1),
	intersection(CLL1,CL2).








/**********************************************************************
 *
 * @(#) env.pl 1.21@(#)
 *
 */


/**********************************************************************
 *
 * getCurrentEnvironment(EnvName)
 * gets the name of the current environment
 *
 */

getCurrentEnvironment(EnvName) :-
	currentEnvironment(Env),
	environment(EnvName,Env,_),
	!.

/**********************************************************************
 *
 * makeEnvironment(+Name,+Comment)
 * creates new environement with name Name. Comment can be any string
 * Name will become the current environment.
 *
 */

makeEnvironment(Name,Comment) :-
	getTwoRandomNumbers(RT,CT),
	FirstChar is 97 + (CT mod 26),
	Runtime   is (RT mod 10000),
	name(Runtime,RTChars),
	name(EnvIdentifier,[FirstChar|RTChars]),
	asserta(environment(Name,env(EnvIdentifier),Comment)),
	retractall(currentEnvironment(_)),
	asserta(currentEnvironment(env(EnvIdentifier))),
	!.

/**********************************************************************
 *
 * showEnvironment(+Name)
 * 
 */

showEnvironment :- 
	getCurrentEnvironment(Name),
	showEnvironment(Name),
	!.

showEnvironment(EnvName) :-
	environment(EnvName,Name,Comment),
	write('Knowledge base '), 
	write(EnvName), 
	nl,
	write('('),
	write(Comment),
	write(')'),
	nl,
	showModalAxioms(Name),
	showDefprimconcept(Name),
	showDefconcept(Name),
	showDefprimrole(Name),
	showDefrole(Name),
	showDefclosed(Name),
	showAssertConcept(Name),
	showAssertRole(Name),
	showFDW(Name),
	!.

showModalAxioms(Name) :-
	modalAxioms(Name,user,K,C,MOp,A),
	(nonvar(A) ; (A = C)),
	write('        '), write('     modalAxioms('), write(K), write(','),
	write(MOp), write(','), write(A), write(')'), nl,
	fail.
showModalAxioms(_) :-
	!.
showAssertConcept(Name) :-
	clause(conceptElement(Name,MS,_,user,A,C,Ax),_),
	write(Ax), write(':     assert_ind('), write(MS), write(','),
	write(A), write(','), write(C), write(')'), nl,
	fail.
showAssertConcept(_) :-
	!.
showAssertRole(Name) :-
	clause(roleElement(Name,MS,_,user,A,B,R,Ax),_),
	write(Ax), write(':     assert_ind('), write(MS), write(','),
	write(A), write(','), write(B), write(','), write(R), write(')'), nl,
	fail.
showAssertRole(_) :-
	!.
showDefconcept(Name) :-
	conceptEqualSets(Name,user,MS,CN,CT,Ax),
	write(Ax), write(':     defconcept('), write(MS), write(','),
	write(CN), write(','), write(CT), write(')'), nl,
	fail.
showDefconcept(_Name) :-
	!.
showDefprimconcept(Name) :-
	conceptSubsets(Name,user,MS,CN,CT,Ax),
	write(Ax), write(': defprimconcept('), write(MS), write(','),
	write(CN), write(','), write(CT), write(')'), nl,
	fail.
showDefprimconcept(_Name) :-
	!.
showDefrole(Name) :-
	roleEqualSets(Name,user,MS,CN,CT,Ax),
	write(Ax), write(':        defrole('), write(MS), write(','),
	write(CN), write(','), write(CT), write(')'), nl,
	fail.
showDefrole(_Name) :-
	!.
showDefprimrole(Name) :-
	roleSubsets(Name,user,MS,CN,CT,Ax),
	write(Ax), write(':    defprimrole('), write(MS), write(','),
	write(CN), write(','), write(CT), write(')'), nl,
	fail.
showDefprimrole(_Name) :-
	!.
showDefclosed(Name) :-
	closed(Name,MS,X,Y,R),
	write('axiom   '), write(':     defclosed('), write(MS), write(','),
	write(X), write(','), write(Y), write(','), write(R), write(')'),
	nl,
	fail.
showDefclosed(_Name) :-
	!.


/**********************************************************************
 *
 * removeEnvironment(+Name)
 *
 */

removeEnvironment :-
	getCurrentEnvironment(EnvName),
	!,
	removeEnvironment(EnvName).


removeEnvironment(Name) :-
	clearEnvironment(Name),
	retractall(environment(Name,_,_)),
	retract(currentEnvironment(Name)),
	asserta(currentEnvironment(env(e0))),
	!.
removeEnvironment(_Name) :-
	% if we get here, Name was not the current environemt
	!.

/***********************************************************************
 *
 * clearEnvironment(Name)
 *
 */

clearEnvironment :- 
	getCurrentEnvironment(EnvName),
	clearEnvironment(EnvName),
	!.

clearEnvironment(EnvName) :-
	environment(EnvName,Env,_),
	retractCompiledPredicates(Env),
	retractall(Env,in/9),
	retractall(Env,kb_in/10),
	retractall(Env,eq/9),
	retractall(Env,constraint/8),
	retractall(Env,rel/5),
	retractall(Env,closed/5),
	retractall(Env,compiledPredicate/2),
	retractall(Env,conceptElement/7),
	retractall(Env,conceptEqualSets/6),
	retractall(Env,conceptHierarchy/3),
	retractall(Env,conceptName/4),
	retractall(Env,conceptSubsets/6),
	retractall(Env,environment/3),
	retractall(Env,given_change/4),
	retractall(Env,given_inflLink/4),
	retractall(Env,modalAxioms/6),
	retractall(Env,roleAttributes/5),
	retractall(Env,roleDefault/4),
	retractall(Env,roleDefNr/4),
	retractall(Env,roleDomain/4),
	retractall(Env,roleElement/8),
	retractall(Env,roleEqualSets/6),
	retractall(Env,roleHierarchy/3),
	retractall(Env,roleName/4),
	retractall(Env,roleNr/5),
	retractall(Env,roleRange/4),
	retractall(Env,roleSubsets/6),
	retractall(Env,sub/4),
	retractall(Env,succ/4),
	retractall(Env,abductiveDerivation/3),
	retractall(Env,consistencyDerivation/3),
	retractall(Env,hypothesis/1),
	retractall(Env,inconsistencyCheck/3),
	retractall(Env,option/2),
	retractall(Env,nsub/4),
	retractall(Env,nsub3/2),
	retractall(Env,sub3/2),
	retractall(Env,succ3/2),
	!.

/**********************************************************************
 *
 * retractCompiledPredicates(+Env)
 * if the environment Env contains compiled predicates, then for each
 * compiled predicate Pred there is a fact 
 *                    compilePredicate(Env,Pred/Arity).
 * So when the environment is to be removed, we just abolish the 
 * compiled predicates.
 *
 */

retractCompiledPredicates(Env) :-
	compiledPredicate(Env,Pred/Arity),
	abolish(Pred/Arity),
	fail.
retractCompiledPredicates(_) :-
	!.


/**********************************************************************
 *
 * initEnvironment(Name)
 *
 */

initEnvironment :- 
	getCurrentEnvironment(EnvName),
	initEnvironment(EnvName),
	!.

initEnvironment(EnvName) :-
	clearEnvironment(EnvName),
	environment(EnvName,Env,_),
	assert(theory(Env,
	[
        (in([],P,pair(X,Y)) <- equal(X,Z), in([],P,pair(Z,Y))),
	(in([],P,pair(X,Y)) <- equal(Y,Z), in([],P,pair(X,Z))),
	(in([],C,X) <- equal(X,Y), in([],C,Y)),
	(equal(X,Y) <- equal(Y,X)),
	(equal(X,X) <- true),
	(in(MS,top,X) <- true)])),
	assertInRules(Env),
	% Assert equality axioms
	assertEqRule(Env,1),
	% Assert 'top' role
%	assertEqRule(Env,2),
	assertEqRule(Env,3),
	% Proof by hypothesis for roles (Test 14.07.92)
	assertEqRule(Env,4),
	% Assert 'top' concept
	assertInRule(Env,1),
	% Assert 'bot' concept
	assertInRule(Env,2),
	% Proof by hypothesis for concepts
	assertInRule(Env,3),
	% Assert X in some(r,c) => X in atleast(1,r)
%	gensym(axiom,AN11),
%	assertInRule(Env,3,AN11),
	% Assert X in atleast(1,r) => X in some(r,'top')
%	assertInRule(Env,4,AN11),
	% Assert X in atmost(0,r) => X in all(r,c)
%	gensym(axiom,AN10),
%	assertInRule(Env,1,AN10),
	% Assert X in all(r,'bot') => X in atmost(0,r)
%	assertInRule(Env,2,AN10),
	% Assert not('top') law
	% necessary for inconsistent knowledge bases?
	% bad influence on runtime!
%	assertInRule(Env,4),
	% Assert double negation laws
	gensym(axiom,AN6),
%	assertInRule(Env,5,AN6),
%	assertInRule(Env,6,AN6),
	% Concrete domains
	gensym(axiom,AN7),
%	assertInRule(Env,7,AN7),
%	assertInRule(Env,8,AN7),
%	assertInRule(Env,9,AN7),
	%%  Abductive Reasoning
	% Proof by abductive hypothesis
	assertAbductionRule(Env,1),
	% Proof by abduction
	assertAbductionRule(Env,2),
	% Meta Reasoning
%	metaReasoning,
	% Assert concept hierarchy
	assertz(conceptHierarchy(Env,[],node(['top'],[]))),
	assertz(conceptName(Env,[],[],'top')),
	assertz(conceptName(Env,[],[],'bot')),
	% Assert role hierarchy
	assertz(roleHierarchy(Env,[],node(['top'],[]))),
	assertz(roleName(Env,[],[],'top')),
	initFuncdep,
	!.

/**********************************************************************
 *
 * assertInRules(+Env)
 * asserts the clauses for the in predicate which is used to 
 * construct goals in the user interface. In general the in clauses
 * just call corresponding kb_in clauses. The kb_in clauses result
 * from the translation of terminological and assertional axioms.
 *
 * !! Remember: Changes to this clauses have to be reflected in the
 *              definition of the compileEnv predicate.
 *
 */

assertInRules(Env) :-
	assertz((in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
		 ifOption(traceOutput,yes,(length(CALL,Depth), format('trying ~d  in(~w,~w)~n',[Depth,CN,CON]))),
	kb_in(Env,pr(5),Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT),
		 ifOption(traceOutput,yes,(length(CALL,Depth), format('succeeded ~d  in(~w,~w)~n',[Depth,CN,CON]))))),
% There are no kb_in clauses with priority 4 at the moment (07.10.92)
%	assertz((in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%	kb_in(Env,pr(4),Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT))),
	assertz((in(Env,Name,modal(MS),CN,CON,hyp([or(H1),rl(H2),fl(H3)]),ab(noAb),call(CALL),PT) :-
		 clashInHyp(H2), !, fail)),
	assertz((in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
		 (CN \== top, CN \== bot, CN \== not(top), CN \== not(bot),
	kb_in(Env,pr(3),Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT)))),
% There are no kb_in clauses with priority 2 at the moment (07.10.92)
%	assertz((in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%	kb_in(Env,pr(2),Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT))),
	assertz((in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
		 (CN \== top,CN \== bot, CN \== not(top), CN \== not(bot),
	kb_in(Env,pr(1),Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT)))),
% Experimental code (07.10.92 uh)
% It might be useful to have global information about the failure of
% derivations. With the code below such a failure is used to assert to
% hypothesis that the negation of the goal is true.
%	assertz((in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%		 (nonvar(CON), nonvar(CN), 
%		  \+ hypothesis(in(Env,modal(MS),CN,CON,ab(D),PT)),
%		  getNegatedConcept(CN,C1),
%		  assertz(hypothesis(in(Env,modal(MS),C1,CON,ab(D),assume))),
%		  fail))),
% There are no kb_in clauses with priority 0 at the moment (07.10.92)
%	assertz((in(Env,Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT) :-
%	kb_in(Env,pr(0),Name,modal(MS),CN,CON,hyp(HYP),ab(D),call(CALL),PT))),
	!.

assertEqRule(Env,1) :-
	gensym(axiom,AN1),
	gensym(rule,RN1),
	constructEqHead(Env,rn(AN1,RN1,user,lInR),W1,app((F:R),X),F,R,X,HYPS,AB,CALLS,PT3,EqHead1),
	constructMLCall(Env,rn(AX,_RN4,_S4,_O4),bodyMC(W1),headMC(W1),normal(R),X,HYPS,AB,CALLS,PT3,InHead2),
	assertz((EqHead1 :- cCS(CALLS,true),  simple_term(X))),
%       The following would be correct
%	assertz((EqHead1 :- cCS(CALLS,true),  InHead2)),
%       old code (uh 20.08.92)
%	assertz((eq(Env,rn(AN1,RN1,user,lInR),modal(MS),X,X,hyp(HYPS),
%                   ab(_AB),call(CALLS),proved(eq(MS,X,X,hyp(HYPS),
%                   basedOn(true)))) :- 
%                (cCS(CALLS,true)))),
	!.
assertEqRule(Env,2) :-
	Role1 =.. ['top',_X,_Y],
	gensym(axiom,AN3),
	gensym(rule,RN3),
	constructMLHead(Env,rn(AN3,RN3,user,lInR),_MS,_,Role1,
			_HYPS,_D,_CALLS,tbox,InHeadR),
	assertz((InHeadR)),
	!.
assertEqRule(Env,3) :-
	gensym(axiom,AN20),
	gensym(rule,RN20),
	constructEqHead(Env,rn(AN20,RN20,user,lInR),_W1,_Y,_F,top,_X,_HYPS,_D,_CALLS,tbox,EqHead20),
	assertz(EqHead20),
	!.
assertEqRule(Env,4) :-
	gensym(axiom,AN21),
	gensym(rule,RN21),
	HYPS = [or(H1),rl(H2),fl(H3)],
	constructEqHead(Env,rn(AN21,RN21,user,lInR),W1,Y,F,R,X,HYPS,D,_CALLS,tbox,EqHead20),
	constructEqMark(rn(_AN21,_RN21,_,_),W1,Y,F,R,X,_HYPS2,D,_CALLS2,EqMark20),
	assertz((EqHead20 :- append(H1,H2,H), member(EqMark20,H))),
	!.


assertInRule(Env,1) :-
	% For all X: X in top
	% Priority 5 (high priority)
	gensym(axiom,AN2),
	gensym(rule,RN2),
	constructKBHead(Env,pr(5),rn(AN2,RN2,user,lInR),_W1,'top',_X,
			_HYPS,_D,_CALLS,tbox,InHead),
	assertz(InHead),
	!.
assertInRule(Env,2) :-
	% For all X: X in not(bot) 
	% What is actually needed is the equivalence of top and not(bot).
	% So we need
	% For all X: X in top if X in not(bot)
	% is subsumed by assertInRule(Env,1).
	% For all X: X in not(top) if X in bot
	% This rule will not be asserted.
	% For all X: X in bot if X in not(top)
	% is subsumed by assertInRule(Env,4).
	% For all X: X in not(bot) if X in top.
	% is subsumed by assertInRule(Env,2), i.e. the rule we will
	% assert now.
	% Priority 5 (high priority)
	gensym(axiom,AN4),
	gensym(rule,RN4),
	constructKBHead(Env,pr(5),rn(AN4,RN4,user,lInR),_W1,not(bot),X,
	                _HYPS,_D,_CALLS,tbox,InHead1),
	assertz(InHead1),
	!.
assertInRule(Env,3) :-
	% For all X: X in C if (X in C) is a hypothesis
	% Priority 5 (high priority)
	gensym(axiom,AN4),
	gensym(rule,RN4),
	HYPS = [or(H1),rl(H2),fl(H3)],
	constructInHead(Env,rn(_AN5,_RN5,_S5,_O5),MS,C,X,_HYPS,_D1,_CALLS1,_,InHead1),
	constructKBHead(Env,pr(5),rn(AN4,RN4,system,lInR),MS,C,X,
	                HYPS,_D,_CALLS2,usingHyp(InHead1),InHead2),
	assertz((InHead2 :- append(H1,H2,H), member(InHead1,H))),
	constructMLMark(InHead1,Mark1),
	assertz((InHead2 :- (append(H1,H2,H), member(Mark1,H)) ; memberDML(Mark1,H3))),
	!.
assertInRule(Env,4) :-
	% For all X: X in not(top) => X in C 
	% Priority 1 (low priority)
	% necessary for inconsistent knowledge bases ?
	gensym(axiom,AN7),
	gensym(rule,RN7),
	constructKBHead(Env,pr(1),rn(AN7,RN7,system,lInR),MS,_C,X,
	                HYPS,D,CALLS,PT3,InHead3),
	constructMLCall(Env,rn(AN7,_RN7,_S7,_O7),bodyMC(MS),headMC(MS),
                        not('top'),X,HYPS,D,CALLS,PT3,L3),
 	constructMLMark(InHead3,Mark3),
	assertz((InHead3 :- cCS(CALLS,Mark3), L3)),
	!.

assertInRule(Env,1,AN10) :- 
	% Assert x in atmost(0,r) => x in all(r,c)
	% Priority 1 (low priority)
	gensym(rule,Rule),
	ruleName(AN10,Rule,system,lInR,RN),
	convertInConsequence(Env,pr(1),RN,_MS,W,all(R,C),X,
	                     HYPS,AB,CALLS,PT,(EqLiteral,InHead)),
	constructMLMark(InHead,Mark),
	convertInAntecedent(Env,rn(AN10,system,lInR),bodyMC(W),headMC(W),
			    atmost(0,R),X,HYPS,AB,CALLS,PT,Body),
	asserta((InHead :- (nonvar(C), (cCS(CALLS,Mark), once((EqLiteral, Body)))))),
	!.
assertInRule(Env,2,AxiomName) :-
	% Assert x in all(r,'bot') => x in atmost(0,r)
	% Priority 1 (low priority)
	convertInAntecedent(Env,rn(AxiomName,_O,_Orientation),
                            bodyMC(W1),headMC(W1),all(R,'bot'),X,
			    HYPS,AB,CALLS,PT1,Body),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,system,lInR,RN1),
	convertInConsequence(Env,pr(1),RN1,_MS,W1,
			     atmost(0,R),X,HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (nonvar(R),(cCS(CALLS,Mark1), once(Body))))),
	!.
assertInRule(Env,3,AxiomName) :-
	% Assert x in some(R,top) => x in atleast(1,R)
	% Priority 1 (low priority)
	convertInAntecedent(Env,rn(AxiomName,system,lInR),
			    bodyMC(W1),headMC(W1),
			    some(R,'top'),X,HYPS,AB,CALLS,PT1,Body),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,system,lInR,RN1),
	convertInConsequence(Env,pr(1),RN1,_MS,W1,
			     atleast(1,R),X,HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (nonvar(R), cCS(CALLS,Mark1), once(Body)))),
	!.
assertInRule(Env,4,AxiomName) :-
	% Assert x in atleast(1,R) => x in some(R,top)
	% Priority 1 (low priority)
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,system,lInR,RN1),
	convertInConsequence(Env,pr(1),RN1,_MS,W1,some(R,'top'),X,
	                     HYPS,AB,CALLS,PT1,(EqLiteral, InHead1)),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(Env,rn(AxiomName,system,lInR),
	                    bodyMC(W1),headMC(W1),
			    atleast(1,R),X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), once((EqLiteral, Body))))),
	!.
assertInRule(Env,5,AN6) :-
	% For all X: X in C => X in not(not(C))
	% Priority 1 (low priority)
	gensym(rule,RN6),
	constructKBHead(Env,pr(1),rn(AN6,RN6,system,lInR),MS,not(not(C)),X,
	                HYPS,D,CALLS,PT4,Consequence3),
	constructMLCall(Env,rn(AN6,_RN9,_S9,_O9),bodyMC(MS),headMC(MS),C,X,
	                HYPS,D,CALLS,PT4,Antecedent4),
	constructMLMark(Consequence3,AxiomHead3),
	assertz((Consequence3 :- cCS(CALLS,AxiomHead3), Antecedent4)),
	!.
assertInRule(Env,6,AN6) :-
	% For all X: X in not(not(C)) => X in C 
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(Env,pr(1),rn(AN6,RN8,system,lInR),MS,C,X,
	                HYPS,D,CALLS,PT3,Consequence4),
	constructMLCall(Env,rn(AN6,_RN7,_S7,_O7),bodyMC(MS),headMC(MS),
			not(not(C)),X,HYPS,D,CALLS,PT3,Antecedent3),
	constructMLMark(Consequence4,AxiomHead4),
	assertz((Consequence4 :- cCS(CALLS,AxiomHead4), Antecedent3)),
	!.
assertInRule(Env,7,AN7) :-
	% For all X: X in set(S2) and subset(S2,S1) => X in S1
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(Env,pr(1),rn(AN7,RN8,system,lInR),MS,set(S1),X,
	                HYPS,D,CALLS,PT2,Consequence1),
	constructMLCall(Env,rn(AN7,_RN2,_S2,_O2),bodyMC(MS),headMC(MS),
			set(S2),X,HYPS,D,CALLS,PT2,Antecedent2),
	L1 = subset(S2,S1),
	constructMLMark(Consequence1,AxiomHead1),
	assertz((Consequence1 :- cCS(CALLS,AxiomHead1), (Antecedent2, L1))),
	!.
assertInRule(Env,8,AN7) :-
	% For all X: X in set(S2) and X in set(S3) and 
	%            intersection(S2,S3,S1) => X in S1
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(Env,pr(1),rn(AN7,RN8,system,lInR),MS,set(S1),X,
	                HYPS,D,CALLS,and([PT2,PT3]),Consequence1),
	constructMLCall(Env,rn(AN7,_RN2,_S2,_O2),bodyMC(MS),headMC(MS),
			set(S2),X,HYPS,D,CALLS,PT2,Antecedent2),
	constructMLCall(Env,rn(AN7,_RN3,_S3,_O3),bodyMC(MS),headMC(MS),
			set(S3),X,HYPS,D,CALLS,PT3,Antecedent3),
	L1 = intersection([S2,S3],S1),
	constructMLMark(Consequence1,AxiomHead1),
	assertz((Consequence1 :- cCS(CALLS,AxiomHead1), (Antecedent3, (Antecedent2, L1)))),
	!.
assertInRule(Env,9,AN7) :-
	% For all X: X in set(S2) and X in set(S3) and 
	%            intersection(S2,S3,S1) => X in S1
	% Priority 1 (low priority)
	gensym(rule,RN8),
	constructKBHead(Env,pr(1),rn(AN7,RN8,system,lInR),MS,not(set(S1)),X,
	                HYPS,D,CALLS,and([PT2,PT3]),Consequence1),
	constructMLCall(Env,rn(AN7,_RN2,_S2,_O2),bodyMC(MS),headMC(MS),
			set(S2),X,HYPS,D,CALLS,PT2,Antecedent2),
	constructMLCall(Env,rn(AN7,_RN3,_S3,_O3),bodyMC(MS),headMC(MS),
			set(S3),X,HYPS,D,CALLS,PT3,Antecedent3),
	L1 = subtract(S2,S3,S1),
	constructMLMark(Consequence1,AxiomHead1),
	assertz((Consequence1 :- cCS(CALLS,AxiomHead1), (Antecedent3, (Antecedent2, L1)))),
	!.


assertAbductionRule(Env,1) :-
	% Proof by abductive hypothesis
	gensym(axiom,AN1),
	gensym(rule,RN1),
	constructInHead(Env,rn(_AN2,_RN2,_S2,_O2),MS,C,X,
	                _HYPS1,_D,_CALLS1,_,InHead1),
	constructMLHead(Env,rn(AN1,RN1,system,lInR),MS,C,X,_HYPS2,D1,_CALLS2,usingAbHyp(in(MS,C,X)),InHead2),
	assertz((InHead2 :- memberDML(InHead1,D1))),
	!.
assertAbductionRule(Env,2) :-
	% Proof by abduction
	gensym(axiom,AN1),
	gensym(rule,RN1),
	constructInHead(Env,rn(_AN2,_RN2,_S2,_O2),MS,C,X,_HYPS,_D0,_CALLS3,_,InHead2),
	constructMLHead(Env,rn(AN1,RN1,system,lInR),MS,C1,X,
	                HYPS,D1,CALLS,usingAbHyp(in(MS,C,X)),InHead1),
	L1 = normalizeNot(C1,C),
	L2 = not(memberDML(InHead2,D1)),
	L3 = addDefaultML(InHead2,D1),
	assertz((InHead1 :- L1, L2, L3)),
	!.


	

/**********************************************************************
 *
 * switchToEnvironment(Name)
 *
 */

switchToEnvironment(Name) :-
	environment(Name,Env,_),
	retractall(currentEnvironment(_)),
	asserta(currentEnvironment(Env)),
	!.

/**********************************************************************
 *
 * saveEnvironment(FileName)
 * 
 */

saveEnvironment(FileName) :-
	getCurrentEnvironment(EnvName),
	saveEnvironment(EnvName,FileName).

saveEnvironment(EnvName,FileName) :-
	tell(FileName),
	writeEnvironment(EnvName),
	told,
	% to be implemented
	!.

writeEnvironment(EnvName) :-
	environment(EnvName,Env,C),
	writeq(environment(EnvName,Env,C)), write('.'), nl,
%	write(':- dynamic(constraint/8).'), nl,
%	write(':- dynamic(in/9).'), nl,
%	write(':- dynamic(kb_in/10).'), nl,
%	write(':- dynamic(rel/5).'), nl,
%	write(':- dynamic(eq/9).'), nl,
%	write(':- dynamic(conceptHierarchy/3).'), nl,
%	write(':- dynamic(roleHierarchy/3).'), nl,
%	write(':- dynamic(conceptEqualSets/6).'), nl,
%	write(':- dynamic(conceptSubsets/6).'), nl,
%	write(':- dynamic(roleEqualSets/6).'), nl,
%	write(':- dynamic(roleSubsets/6).'), nl,
%	write(':- dynamic(conceptName/4).'), nl,
%	write(':- dynamic(roleName/4).'), nl,
%	write(':- dynamic(falsum/2).'), nl,
%	write(':- dynamic(inconsistencyCheck/3).'), nl,
%	write(':- dynamic(conceptElement/6).'), nl,
%	write(':- dynamic(roleElement/7).'), nl,
%	write(':- dynamic(modalAxioms/6).'), nl,
	writeall(in(Env,_A0,_B0,_C0,_D0,_E0,_F0,_G0,_H0)),
	writeall(kb_in(Env,_A1,_B1,_C1,_D1,_E1,_F1,_G1,_H1,_I1)),
	writeall(eq(Env,_A11,_B11,_C11,_D11,_E11,_F11,_G11,_H11)),
	writeall(constraint(Env,_A12,_B12,_C12,_D12,_E12,_F12,_G12)),
	writeall(rel(Env,_A17,_B17,_C17,_D17)),
	writeall(closed(Env,_A18,_B18,_C18,_D18)),
	writeall(compiledPredicate(Env,_A19)),
	writeall(conceptElement(Env,_A14,_B14,_C14,_D14,_E14,_F14)),
	writeall(conceptEqualSets(Env,_A4,_B4,_C4,_D4,_E4)),
	writeall(conceptHierarchy(Env,_A2,_B2)),
	writeall(conceptName(Env,_A8,_B8,_C8)),
	writeall(conceptSubsets(Env,_A5,_B5,_C5,_D5,_E5)),
	writeall(Env,given_change/4),
	writeall(Env,given_inflLink/4),
	writeall(modalAxioms(Env,_A16,_B16,_C16,_D16,_E16)),
	writeall(Env,roleAttributes/5),
	writeall(Env,roleDefault/4),
	writeall(Env,roleDefNr/4),
	writeall(Env,roleDomain/4),
	writeall(Env,roleElement/8),
	writeall(Env,roleEqualSets/6),
	writeall(Env,roleHierarchy/3),
	writeall(Env,roleName/4),
	writeall(Env,roleNr/5),
	writeall(Env,roleRange/4),
	writeall(Env,roleSubsets/6),
	writeall(succ(_A17,Env,_B17,_C17)),
	writeall(sub(_A18,Env,_B18,_C18)),
	!.

writeall(Head) :-
	clause(Head,Body),
	writeq((Head :- Body)), write('.'), nl, 
	fail.
writeall(_) :- !.

writeall(Env,Pred/Arity) :-
	constructHead(Env,Pred/Arity,Head),
	clause(Head,Body),
	writeq((Head :- Body)), write('.'), nl, 
	fail.
writeall(_,_) :- !.


constructHead(Env,Pred/Arity,Head) :-
	constructArguments(Env,Arity,[],Arguments),
	Head =.. [Pred|Arguments],
	!.
constructArguments(Env,1,Args,[Env|Args]) :-
	!.
constructArguments(Env,N,Args,Arguments) :-
	M is (N - 1),
	constructArguments(Env,M,[X|Args],Arguments),
	!.

/**********************************************************************
 *
 * loadEnvironment(FileName)
 * 
 */

loadEnvironment(FileName) :-
	see(FileName),
	read(environment(EnvName,Env,C)),
	(removeEnvironment(EnvName) ; true),
	asserta(environment(EnvName,Env,C)),
	repeat,
	read(Clause),
	assertClause(Clause),
	seen,
	!.
loadEnvironment(FileName) :-
	seen,
	!,
	fail.
loadEnvironment(FileName,EnvName) :-
	see(FileName),
	read(environment(_EnvName2,Env,C)),
	(removeEnvironment(EnvName) ; true),
	assertz(environment(EnvName,Env,C)),
	repeat,
	read(Clause),
	assertClause(Clause),
	seen,
	!.
loadEnvironment(_FileName,_EnvName) :-
	seen,
	!, 
	fail.

assertClause('end_of_file') :-
	!.
assertClause(Clause) :-
	assertz(Clause),
	fail.

/**********************************************************************
 *
 * copyEnvironment(Name1,Name2)
 * copies environment Name1 to environment Name2.
 *
 */

copyEnvironment(Name2) :-
	getCurrentEnvironment(Name1),
	copyEnvironment(Name1,Name2).

copyEnvironment(Name1,Name2) :-
	environment(Name1,Env1,Comment),
	makeEnvironment(Name2,Comment),
	% !! This environment mustn't be initializes because
	% the clauses asserted usually during initialization
	% will also be copied from environment Name1.
	environment(Name2,Env2,_),
	term_expansion(copy,on,Env1,Env2),
	!,
	copyAll(Env1,Env2,in/9),
	copyAll(Env1,Env2,kb_in/10),
	copyAll(Env1,Env2,eq/9),
	copyAll(Env1,Env2,constraint/8),
	copyAll(Env1,Env2,rel/5),
	copyAll(Env1,Env2,closed/5),
	copyAll(Env1,Env2,compiledPredicate/2),
	copyAll(Env1,Env2,conceptElement/7),
	copyAll(Env1,Env2,conceptEqualSets/6),
	copyAll(Env1,Env2,conceptHierarchy/3),
	copyAll(Env1,Env2,conceptName/4),
	copyAll(Env1,Env2,conceptSubsets/6),
	copyAll(Env1,Env2,environment/3),
	copyAll(Env1,Env2,given_change/4),
	copyAll(Env1,Env2,given_inflLink/4),
	copyAll(Env1,Env2,modalAxioms/6),
	copyAll(Env1,Env2,roleAttributes/5),
	copyAll(Env1,Env2,roleDefault/4),
	copyAll(Env1,Env2,roleDefNr/4),
	copyAll(Env1,Env2,roleDomain/4),
	copyAll(Env1,Env2,roleElement/8),
	copyAll(Env1,Env2,roleEqualSets/6),
	copyAll(Env1,Env2,roleHierarchy/3),
	copyAll(Env1,Env2,roleName/4),
	copyAll(Env1,Env2,roleNr/5),
	copyAll(Env1,Env2,roleRange/4),
	copyAll(Env1,Env2,roleSubsets/6),
%	copyAll(Env1,Env2,sub/4),
%	copyAll(Env1,Env2,succ/4),
%	copyAll(Env1,Env2,option/2),
%	copyAll(Env1,Env2,nsub/4),
	term_expansion(copy,off,Env1,Env2),
	!.

term_expansion(copy,on,Env1,Env2) :-
	abolish(term_expansion/2),
	assertz((term_expansion((Head :- Body),(Head1 :- Body1)) :-
	term_expansion(Head,Head1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L, Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion((L; Body), (L1,Body1)) :-
	term_expansion(L,L1),
	term_expansion(Body,Body1))),
	assertz((term_expansion(\+Atom,\+Atom1) :-
	term_expansion(Atom,Atom1))),
	assertz((term_expansion(once(Body1),once(Body2)) :-
		term_expansion(Body1,Body2))),
	assertz((term_expansion(call(Body1),call(Body2)) :-
		 term_expansion(Body1,Body2))),
	assertTermExpansionClause(in/9,Env1,Env2),
	assertTermExpansionClause(kb_in/10,Env1,Env2),
	assertTermExpansionClause(eq/9,Env1,Env2),
	assertTermExpansionClause(constraint/8,Env1,Env2),
	assertTermExpansionClause(rel/5,Env1,Env2),
	assertTermExpansionClause(closed/5,Env1,Env2),
	assertTermExpansionClause(compiledPredicate/2,Env1,Env2),
	assertTermExpansionClause(conceptElement/7,Env1,Env2),
	assertTermExpansionClause(conceptEqualSets/6,Env1,Env2),
	assertTermExpansionClause(conceptHierarchy/3,Env1,Env2),
	assertTermExpansionClause(conceptName/4,Env1,Env2),
	assertTermExpansionClause(conceptSubsets/6,Env1,Env2),
	assertTermExpansionClause(environment/3,Env1,Env2),
	assertTermExpansionClause(given_change/4,Env1,Env2),
	assertTermExpansionClause(given_inflLink/4,Env1,Env2),
	assertTermExpansionClause(modalAxioms/6,Env1,Env2),
	assertTermExpansionClause(roleAttributes/5,Env1,Env2),
	assertTermExpansionClause(roleDefault/4,Env1,Env2),
	assertTermExpansionClause(roleDefNr/4,Env1,Env2),
	assertTermExpansionClause(roleDomain/4,Env1,Env2),
	assertTermExpansionClause(roleElement/8,Env1,Env2),
	assertTermExpansionClause(roleEqualSets/6,Env1,Env2),
	assertTermExpansionClause(roleHierarchy/3,Env1,Env2),
	assertTermExpansionClause(roleName/4,Env1,Env2),
	assertTermExpansionClause(roleNr/5,Env1,Env2),
	assertTermExpansionClause(roleRange/4,Env1,Env2),
	assertTermExpansionClause(roleSubsets/6,Env1,Env2),
	assertTermExpansionClause(sub/4,Env1,Env2),
	assertTermExpansionClause(succ/4,Env1,Env2),
	assertz((term_expansion(succ(X1,Env1,X3,X4),
				succ(X1,Env2,X3,X4)))),
	assertz((term_expansion(sub(X1,Env1,X3,X4),
				sub(X1,Env2,X3,X4)))),
	assertz(term_expansion(X,X)),
	!.
term_expansion(copy,off,_Env1,_Env2) :-
	abolish(term_expansion/2),
	!.


assertTermExpansionClause(Pred/Arity,Env1,Env2) :-
	constructArguments(Env,Arity,[],[Env1|Arguments]),
	Head1 =.. [Pred|[Env1|Arguments]],
	Head2 =.. [Pred|[Env2|Arguments]],
	assertz((term_expansion(Head1,Head2))),
	!.

expandTerm(A,B) :-
	expand_term(A,B),
	!.

copyall(Env1,_Env2,Pred,Args) :-
	Head1 =.. [Pred,Env1|Args],
	clause(Head1,Body1),
	expandTerm((Head1,Body1),(Head2,Body2)),
	assertz((Head2 :- Body2)),
	fail.
copyall(_,_,_,_) :- !.

copyAll(Env1,_Env2,Pred/Arity) :-
	constructHead(Env1,Pred/Arity,Head1),
	clause(Head1,Body1),
	expandTerm((Head1,Body1),(Head2,Body2)),
	assertz((Head2 :- Body2)),
	fail.
copyAll(_,_,_) :- !.

/**********************************************************************
 *
 * renameEnvironment(Name1,Name2)
 * renames environment Name1 to environment Name2.
 *
 */

renameEnvironment(Name1,Name2) :-
	retract(environment(Name1,Env,C)),
	asserta(environment(Name2,Env,C)),
	% to be implemented
	!.
/**********************************************************************
 *
 * @(#) examples.pl 1.5@(#)
 *
 */

example(1) :-
	makeEnvironment('ex1','von HJO'),
	initEnvironment,
	defconcept(fatherAcademic,and([male,some(child,academic)])),
	defconcept(grandfatherAcademic,and([male,some(child,fatherAcademic)])),
	assert_ind(tom,tim,child),
	assert_ind(tim,mike,child),
	assert_ind(mike,male),
	assert_ind(mike,academic),
	assert_ind(tim,male),
	assert_ind(tom,male).
%%% Example  2:
%%% KRIS-Example
% setof(C,ask(elementOf(mary,C)),L)
% gives L = ['top',grandparent,parent,parent_with_sons_only,
%            parent_with_two_children,person] 
% in Total runtime 12.167 sec. (05.06.92)
example(2) :-
	makeEnvironment('ex2','krisExample'),
	initEnvironment,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defconcept(males,some(sex,male)),
	defconcept(females,some(sex,female)),
	defprimconcept(person,some(sex,or([male,female]))),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,some(sex,female)])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,some(sex,male))])),
	defconcept(parent_with_two_children,and([parent,atleast(2,child)])),
	assert_ind(tom,father),
	assert_ind(tom,peter,child),
	assert_ind(tom,harry,child),
	assert_ind(mary,parent_with_sons_only),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example  3:
% inconsistent([])
% succeeds in Total runtime 0.000 sec. (05.06.92)
example(3) :-
	makeEnvironment('ex3','Inconsistence'),
	initEnvironment,
	defprimconcept(parent_with_one_child,atmost(1,child)),
	assert_ind(mary,parent_with_one_child),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example  4:
% Modal Operators
example(4) :-
	makeEnvironment('ex4','Inconsistence'),
	initEnvironment,
	defconcept([b(believe,a1)],c1,b(want,a2,car)),
	defconcept([b(believe,a1)],c2,b(want,a3,car)),
	defprimconcept([b(believe,a1)],c1,c2),
	assert_ind([b(believe,a1)],audi,c1).
%%% Example  5:
% subsumes([],c1,c2).
% fails    in Total runtime 0.050 sec. (05.06.92)
% subsumes([],c2,c1).
% succeeds in Total runtime 0.050 sec. (05.06.92)
example(5) :-
	makeEnvironment('ex5','Subsumption'),
	initEnvironment,
	defconcept(c1,and([all(r,a),all(and([r,q]),b)])),
	defconcept(c2,all(and([r,q]),and([a,b]))).
%%% Example  6:
% subsumes([],c1,c2).
% fails    in Total runtime 0.033 sec. (05.06.92)
% subsumes([],c2,c1).
% succeeds in Total runtime 0.033 sec. (05.06.92)
example(6) :-
	makeEnvironment('ex6','Subsumption'),
	initEnvironment,
	defrole(r1,and([r,q])),
	defconcept(d0,and([a,b])),
	defconcept(d1,all(r,a)),
	defconcept(d2,all(r1,b)),
	defconcept(c1,and([d1,d2])),
	defconcept(c2,all(r1,d0)).
%%% Example  7:
example(7) :-
	makeEnvironment('ex7','Subsumption'),
	initEnvironment,
	defconcept(c1,atleast(3,r)),
	defconcept(c2,and([all(and([r,p]),a),all(and([r,q]),not(a)),atleast(2,and([r,p])),atleast(2,and([r,q]))])).
%%% Example  8;
% ask(elementOf(tom,heterosexual))
% succeeds in Total runtime 0.033 sec. (05.06.92)
example(8) :-
	makeEnvironment('ex8','Disjunction of complementary concepts'),
	initEnvironment,
	defprimconcept(male),
	defconcept(female,not(male)),
	defconcept(heterosexual,or([male,female])).
%%% Example  9:
% Variation of the KRIS-Example
% ask(elementOf(chris,male))
% succeeds in Total runtime 0.000 sec. (05.06.92)
example(9) :-
	makeEnvironment('ex9','Variation of the KRIS example'),
	initEnvironment,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defprimconcept(person,or([male,female])),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,female])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,male)])),
	defconcept(parent_with_two_children,and([parent,atleast(2,child)])),
	assert_ind(tom,father),
	assert_ind(tom,peter,child),
	assert_ind(tom,harry,child),
	assert_ind(mary,parent_with_sons_only),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example 10:
% ask(elementOf(tom,c2)) 
% succeeds in Total runtime 0.017 sec. (05.06.92)
example(10) :-
	makeEnvironment('ex10','Inverse Role'),
	initEnvironment,
	defrole(r2,inverse(r1)),
	defconcept(c1,all(r1,c2)),
	defconcept(c3,some(r2,c1)),
	assert_ind(tom,c3).
%%% Example 11:
% inconsistent([])
% succeeds in Total runtime 0.034 sec. (05.06.92)
example(11) :-
	makeEnvironment('ex11','Inconsistence'),
	initEnvironment,
	defconcept(c1,and([atleast(2,child),atmost(1,child)])),
	assert_ind(tom,c1).
%%% Example 12:
% subsumes([],c1,c2)
% succeeds in Total runtime 0.050 sec. (05.06.92)
% subsumes([],c2,c1)
% fails    in Total runtime 0.200 sec. (05.06.92)
example(12) :-
	makeEnvironment('ex12','Subsumption'),
	initEnvironment,
	defconcept(c1,and([person,atleast(2,child)])),
	defconcept(c2,and([person,atleast(3,restr(child,lawyer))])).
%%% Example 13:
% subsumes([],c1,c2)
% succeeds in Total runtime 0.117 sec. (05.06.92)
example(13) :-
	makeEnvironment('ex13','Subsumption'),
	initEnvironment,
	defconcept(c1,and([person,atmost(4,restr(child,doctor))])),
	defconcept(c2,and([person,female,atmost(3,child)])).
%%% Example 14:
% subsumes([],c1,c2)
% succeeds ???
% subsumes([],c2,c1)
% succeeds in Total runtime 0.250 sec. (06.06.92)
example(14) :-
	makeEnvironment('ex14','Subsumption'),
	initEnvironment,
	defconcept(c1,atmost(0,restr(r,and([atleast(3,s1),atleast(4,s2)])))),
	defconcept(c2,all(restr(r,atleast(2,s1)),atmost(2,s2))).
%%% Example 15:
% subsumes([],c2,c1)
% succeeds in Total runtime 0.067 sec. (05.06.92)
example(15) :-
	makeEnvironment('ex15','Subsumption'),
	initEnvironment,
	defconcept(c1,and([person,all(friend,doctor),all(restr(friend,doctor),atleast(1,speciality))])),
	defconcept(c2,and([person,all(friend,atleast(1,speciality))])).
%%% Example 16:
% subsumes([],c2,c1)
% succeeds in Total runtime 0.450 sec. (06.06.92)
example(16) :-
	makeEnvironment('ex16','Subsumption'),
	initEnvironment,
	defconcept(c1,and([atleast(1,restr(child,lawyer)),atleast(1,restr(child,doctor))])),
	defconcept(c2,or([atleast(2,child),atleast(1,restr(child,and([lawyer,doctor])))])).
%%% Example 17:
% subsumes([],c2,c1)
% succeeds in Total runtime 0.034 sec. (05.06.92)
example(17) :-
	makeEnvironment('ex17','Subsumption'),
	initEnvironment,
	defconcept(c1,some(and([child,friend]),doctor)),
	defconcept(c2,and([some(child,doctor),some(friend,doctor)])).
%%% Example 18:
% ask(elementOf(mary,c4))
% succeeds in Total runtime 0.117 sec. (05.06.92)
example(18) :-
	makeEnvironment('ex18','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c3,and([atmost(4,child),atleast(2,restr(child,female))])),
	defconcept(c4,atmost(2,restr(child,female))),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,peter,child),
	assert_ind(mary,tom,child),
	assert_ind(mary,c3).
%%% Example 19
% ask(elementOf(amy,female))
% succeeds in Total runtime 0.067 sec. (06.06.92)
example(19) :-
	makeEnvironment('ex19','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c5,and([atmost(2,restr(child,male))])),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,tom,child),
	assert_ind(mary,peter,child),
	assert_ind(mary,amy,child),
	assert_ind(mary,c5).
%%% Example 20
% inconsistent([])
% succeeds in Total runtime 5.167 sec. (05.06.92)
example(20) :-
	makeEnvironment('ex20','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c5,and([atmost(2,restr(child,male)),atmost(1,restr(child,female))])),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,tom,child),
	assert_ind(mary,peter,child),
	assert_ind(mary,amy,child),
	assert_ind(mary,jane,child),
	assert_ind(mary,c5).
%%% Example 21
% ask(elementOf(betty,female))
example(21) :-
	makeEnvironment('ex21','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c1,and([atmost(1,restr(teacher,male)),atmost(1,restr(teacher,female))])),
	defconcept(c2,and([atmost(2,restr(teacher,male)),atmost(1,restr(teacher,female))])),
	assert_ind(tom,c1),
	assert_ind(sue,c1),
	assert_ind(david,c2),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,peter,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher),
	assert_ind(david,betty,teacher),
	assert_ind(david,chris,teacher),
	assert_ind(david,peter,teacher).
%%% Example 22
% ask(elementOf(amy,female))
% should succeeds
% but fails in the current implementation
example(22) :-
	makeEnvironment('ex22','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defrole(maleTeacher,restr(teacher,male)),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 23
% is a variant of example 23 with user provided names for the 
% restricted roles.
% ask(elementOf(amy,female))
% should succeeds
% but fails in the current implementation
example(23) :-
	makeEnvironment('ex23','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defprimrole(maleTeacher,teacher),
	defprimrole(femaleTeacher,teacher),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 24
% ask(elementOf(audi,c3))
% succeeds in Total runtime 1.634 sec. (24.06.92)
example(24) :-
	makeEnvironment('ex24','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,a1),
	defconcept(c1,b(believe,a1,auto)),
	defconcept(c3,b(believe,a1,c1)),
	defconcept([b(believe,a1)],c1,b(believe,a1,auto)),
	defconcept([b(believe,a1)],c3,b(believe,a1,c1)),
	assert_ind(audi,c1).
%%% Example 25
% not(ask(elementOf(audi,c3)))
% succeeds in Total runtime 0.033 sec. (24.06.92)
example(25) :-
	makeEnvironment('ex25','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,a1),
	defconcept([b(believe,a1)],c1,b(believe,a1,auto)),
	defconcept([b(believe,a1)],c3,b(believe,a1,c1)),
	assert_ind(audi,c1).
%%% Example 26
% subsumes([],c2,c1)
% succeeds in Total runtime 0.034 sec. (24.06.92)
% not(subsumes([],c1,c2))
% succeeds in Total runtime 1.333 sec. (24.06.92)
example(26) :-
	makeEnvironment('ex27','Subsumption'),
	initEnvironment,
	defconcept(c1,atmost(0,r)),
	defconcept(c2,all(r,c5)).
%%% Example 27
% subsumes([],c2,c1) 
% succeeds in Total runtime 0.067 sec. (24.06.92)
% not(subsumes([],c1,c2))
% succeeds
example(27) :-
	makeEnvironment('ex28','Subsumption'),
	initEnvironment,
	defconcept(c1,not(some(r,'top'))),
	defconcept(c2,all(r,c5)).
%%% Example 28
% ask(ex28,[b(believe,john)],elementOf(audi,auto),P)
% succeeds
example(28) :-
	makeEnvironment('ex28','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,a1),
	modalAxioms(kd45,believe,all),
	defprimconcept(auto),
	assert_ind([b(believe,all)],audi,auto).
%%% Example 29
% is a variant of example 23 with a more restricted definition of c1
% ask(elementOf(amy,female))
% should succeeds
% but fails in the current implementation
example(29) :-
	makeEnvironment('ex29','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defprimrole(teacher),
	defrole(maleTeacher,restr(teacher,male)),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(2,femaleTeacher)])),
	assert_ind(tom,c1),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
example(30) :-
	makeEnvironment('ex30','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defrole(maleTeacher,restr(teacher,not(female))),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 31
% First test example for defclosed
% ask(elementOf(tom,onlyMaleChildren))
% succeeds
example(31) :-
	makeEnvironment('ex31','defclosed'),
	initEnvironment,
	defconcept(onlyMaleChildren,all(child,male)),
	assert_ind(tom,peter,child),
	assert_ind(tom,chris,child),
	assert_ind(tom,tim,child),
	assert_ind(peter,male),
	assert_ind(chris,male),
	assert_ind(tim,male),
	defclosed(tom,_Y,child).
%%% Example 32
% First test example for abduction
% abduce(elementOf(robin,male),H,E)
% abduce(elementOf(robin,female),H,E)
example(32) :-
	makeEnvironment('ex32','abduction'),
	initEnvironment,
	defconcept(male,not(female)).
%%% Example 33
% Second test example for abduction
% abduce(elementOf(nixon,dove),H,E)
% abduce(elementOf(nixon,hawk),H,E)
% gives unexpected results!!!
example(33) :-
	makeEnvironment('ex33','abduction'),
	initEnvironment,
	defconcept(c1,and([quaker,normalQuaker])),
	defconcept(c2,and([republican,normalRepublican])),
	defprimconcept(c1,dove),
	defprimconcept(c2,hawk),
	assert_ind(nixon,quaker),
	assert_ind(nixon,republican).
%%% Example 34
% The following gives an inconsistent specification of
% the penguin - bird problem. So
% inconsistent(ex34)
% succeeds
example(34) :-
	makeEnvironment('ex34',abduction),
	initEnvironment,
	defprimconcept(penguin,and([bird,not(fly)])),
	defprimconcept(bird,fly),
	assert_ind(tweety,penguin),
	assert_ind(john,bird).
%%% Example 35
% This is a consistent specification of the penguin - bird problem.
% abduce(ex35,[],elementOf(john,fly),H,E).
% succeeds with
% H = [in(env(e1),rn(_7982,_7983,_7984,_7985),modal([]),normalBird,john,
%         hyp(_7989),ab(_7991),call(_7993),
%         proved(in([],normalBird,john),hyp(_7989),basedOn(_8005)))],
% E = proved(in([],fly,john),hyp([]),
%            basedOn(and([proved(in([],bird,john),hyp([]),basedOn(abox)),
%                         proved(in([],normalBird,john),hyp([]),
%     basedOn(usingAbHyp(in(env(e1),rn(_7525,_7526,_7527,_7528),modal([]),
%                           normalBird,john,hyp(_7532),ab(_7534),call(_7536),
%                           proved(in([],normalBird,john),hyp(_7532),
%                           basedOn(_7548))))))])))
% and
% abduce(ex35,[],elementOf(tweety,fly),H,E).
% fails
example(35) :-
	makeEnvironment('ex35',abduction),
	initEnvironment,
	defprimconcept(penguin,and([bird,not(normalBird)])),
	defprimconcept(and([bird,normalBird]),fly),
	assert_ind(tweety,penguin),
	assert_ind(john,bird).
%%% Example 36
% Variant of example 33 giving the expected results:
% abduce(ex36,[],elementOf(nixon,dove),H,E).
% succeeds with
% H = [in(env(e4),rn(_8077,_8078,_8079,_8080),modal([]),
%         normalQuaker,nixon,hyp(_8084),ab(_8086),call(_8088),
%         proved(in([],normalQuaker,nixon),hyp(_8084),basedOn(_8100)))],
% E = proved(in([],dove,nixon),hyp([]),
%        basedOn(and([proved(in([],quaker,nixon),hyp([]),basedOn(abox)),
%                     proved(in([],normalQuaker,nixon),hyp([]),
%           basedOn(usingAbHyp(in(env(e4),rn(_7620,_7621,_7622,_7623),
%                   modal([]),normalQuaker,nixon,hyp(_7627),ab(_7629),
%                   call(_7631),proved(in([],normalQuaker,nixon),
%                   hyp(_7627),basedOn(_7643))))))]))) 
% and
% abduce(ex36,[],elementOf(nixon,hawk),H,E).
% succeeds with
% H = [in(env(e4),rn(_8077,_8078,_8079,_8080),modal([]),
%         normalRepublican,nixon, hyp(_8084),ab(_8086),call(_8088),
%         proved(in([],normalRepublican,nixon),hyp(_8084),basedOn(_8100)))],
% E = proved(in([],dove,nixon),hyp([]),
%        basedOn(and([proved(in([],republican,nixon),hyp([]),basedOn(abox)),
%                     proved(in([],normalRepublican,nixon),hyp([]),
%           basedOn(usingAbHyp(in(env(e4),rn(_7620,_7621,_7622,_7623),
%                   modal([]),normalRepublican,nixon,hyp(_7627),ab(_7629),
%                   call(_7631),proved(in([],normalRepublican,nixon),
%                   hyp(_7627),basedOn(_7643))))))]))) 
example(36) :-
	makeEnvironment('ex36','abduction'),
	initEnvironment,
	defprimconcept(and([quaker,normalQuaker]),dove),
	defprimconcept(and([republican,normalRepublican]),hawk),
	assert_ind(nixon,quaker),
	assert_ind(nixon,republican).
%%% Example 37
example(37) :-
	makeEnvironment('ex37','abduction'),
	initEnvironment,
	defprimconcept(rained_last_night,grass_is_wet),
	defprimconcept(sprinkler_was_on,grass_is_wet),
	defprimconcept(grass_is_wet,shoes_are_wet).
%%% Example 38
% ask(elementOf(ideaste,c2))
% should succeed
example(38) :-
	makeEnvironment('ex38','disjunctive_information'),
	initEnvironment,
	assert_ind(ideaste,oedipus,hasChild),
	assert_ind(oedipus,polyneikes,hasChild),
	assert_ind(ideaste,polyneikes,hasChild),
	assert_ind(polyneikes,thersandros,hasChild),
	assert_ind(oedipus,fatherMurderer),
	assert_ind(thersandros,not(fatherMurderer)),
	defconcept(c1,and([fatherMurderer,some(hasChild,not(fatherMurderer))])),
	defconcept(c2,some(hasChild,c1)).
%%% Example 39
% ask(elementOf(lucky,female))
% succeeds
example(39) :-
	makeEnvironment('ex39','negation_as_failure'),
	initEnvironment,
	defrole(parentOf,inverse(childOf)),
	defconcept(male,not(female)),
	defprimconcept(and([some(parentOf,top),naf(not(female))]),female),
	assert_ind(mary,lucky,childOf).
%%% Example 40
% ask(elementOf(peter,richPerson))
% succeeds.
% After
% assert_ind(peter,poorPerson)
% the query
% ask(elementOf(peter,richPerson))
% fails
example(40) :-
	makeEnvironment('ex40','negation_as_failure'),
	initEnvironment,
	defprimconcept(and([doctor,naf(not(richPerson))]),richPerson),
	defconcept(poorPerson,not(richPerson)),
	assert_ind(peter,doctor).
%%% Example 41
% ask(elementOf(tom,richPerson))
% succeeds.
% After 
% assert_ind(tom,poorPerson)
% the query
% ask(elementOf(tom,richPerson))
% fails
example(41) :-
	makeEnvironment('ex41','negation_as_failure'),
	initEnvironment,
	defrole(doctorParentOf,restr(inverse(childOf),doctor)),
	defrole(childOfDoctor,inverse(r1)),
	defprimconcept(and([some(doctorParentOf,top),naf(not(richPerson))]),richPerson),
	defconcept(poorPerson,not(richPerson)),
	assert_ind(chris,doctor),
	assert_ind(chris,tom,childOf).
%%% Example 42
% ask(elementOf(audi,fourWheels))
% succeeds.
% After
% assert_ind(audi,fiveWheels)
% the query
% ask(elementOf(audi,fourWheels))
% fails
example(42) :-
	makeEnvironment('ex42','negation_as_failure'),
	initEnvironment,
	defconcept(fourWheels,and([atleast(4,wheels),atmost(4,wheels)])),
	defconcept(fiveWheels,and([atleast(5,wheels),atmost(5,wheels)])),
	defprimconcept(and([car,naf(not(fourWheels))]),fourWheels),
	assert_ind(audi,car).
%%% Example 43
example(43) :-
	makeEnvironment('ex43','concrete_domains'),
	initEnvironment,
	defconcept(colors,set([b,y,r])),
	defconcept(blueOrYellow,set([b,y])),
	defconcept(red,set([r])),
	defconcept(blue,set([b])),
	defconcept(yellow,set([y])),
	defconcept(redOrYellow,set([r,y])),
	defconcept(blueOrRed,set([b,r])),
	defconcept(yellowOrBlue,set([y,b])).
%%% Example 44
% subsumes(c2,c1)
% should succeed
example(44) :-
	makeEnvironment('ex44','concrete_domains'),
	initEnvironment,
	defconcept(c1,set([a,b])),
	defconcept(c2,set([a,b,c])).
%%% Example 45
example(45) :-
	makeEnvironment('ex45','concrete_domains'),
	initEnvironment,
	defconcept(c1,set([a,b,c])),
	defconcept(c2,set([a,b])),
	defconcept(nc2,not(c2)).
%%% Example 46
% An insufficient specification of 
% The bmw is either yellow, blue, or red but not yellow. 
% ask(elementOf(bmw,c3))
% fails
example(46) :-
	makeEnvironment('ex46','concrete_domains'),
	initEnvironment,
	defconcept(c1,some(hasCol,set([yellow,blue,red]))),
	defconcept(c2,some(hasCol,not(set([yellow])))),
	defconcept(c3,some(hasCol,set([blue,red]))),
	assert_ind(bmw,c1),
	assert_ind(bmw,c2).
%%% Example 47
% A correct specification of
% The bmw is either yellow, blue, or red but not yellow. 
% ask(elementOf(bmw,c3))
% succeeds
example(47) :-
	makeEnvironment('ex47','concrete_domains'),
	initEnvironment,
	defconcept(c1,and([some(hasCol,set([yellow,blue,red])),all(hasCol,set([yellow,blue,red]))])),
	defconcept(c2,some(hasCol,not(set([yellow])))),
	defconcept(c3,some(hasCol,set([blue,red]))),
	assert_ind(bmw,c1),
	assert_ind(bmw,c2).
example(48) :-
	makeEnvironment('ex48','concrete_concepts'),
	initEnvironment,
	defconcept(oneSpouse,and([atleast(1,spouse),atmost(1,spouse)])),
	assert_ind(m1,oneSpouse),
	defprimconcept(some(inverse(spouse),set([m1])),set([g0,g1,g2])),
	assert_ind(g0,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g0])),set([m1,g1,g2])),
	assert_ind(g1,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g1])),set([m1,g0,g2])),
	assert_ind(g2,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g2])),set([m1,g0,g1])),
	defconcept(zeroSH,and([atleast(0,sh),atmost(0,sh)])),
	defconcept(oneSH,and([atleast(1,sh),atmost(1,sh)])),
	defconcept(twoSH,and([atleast(2,sh),atmost(2,sh)])),
	assert_ind(g0,zeroSH),
	assert_ind(g1,oneSH),
	assert_ind(g2,twoSH),
	defprimconcept(and([some(inverse(sh),set([m1])),set([m1])]),bot),
	defprimconcept(and([some(inverse(sh),set([g0])),set([g0])]),bot),
	defprimconcept(and([some(inverse(sh),set([g1])),set([g1])]),bot),
	defprimconcept(and([some(inverse(sh),set([g2])),set([g2])]),bot),
	defprimconcept(and([some(inverse(spouse),set([m1])),some(inverse(sh),set([m1]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g0])),some(inverse(sh),set([g0]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g1])),some(inverse(sh),set([g1]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g2])),some(inverse(sh),set([g2]))]),bot),
%	defconcept(some(sh,set([m1])),some(inverse(sh),set([m1]))),
%	defconcept(some(sh,set([g0])),some(inverse(sh),set([g0]))),
%	defconcept(some(sh,set([g1])),some(inverse(sh),set([g1]))),
%	defconcept(some(sh,set([g2])),some(inverse(sh),set([g2]))).
	defrole(sh,inverse(sh)),
	defrole(spouse,inverse(spouse)).
%%% Example 49
% ask(elementOf(p,c4))
% should fail
example(49) :-
	makeEnvironment('ex49','defaults'),
	initEnvironment,
	defconcept(c4,and([c5,c6])),
	defprimconcept(and([c0,naf(not(c2))]),c5),
	defprimconcept(and([c0,naf(not(c3))]),c6),
	defconcept(c1,or([not(c2),not(c3)])),
	assert_ind(p,c0),
	assert_ind(p,c1).
example(50) :-
	makeEnvironment('ex50','complete_or'),
	initEnvironment,
	defprimconcept(c1,c0),
	defprimconcept(not(c1),c0).
example(51) :-
	makeEnvironment('ex51','functional_dependencies'),
	initEnvironment,
	def(posInfl(f,d)),
	def(posInfl(h,f)),
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)),
	def(negInfl(b,e)),
	def(negInfl(e,d)),
	def(posInfl(g,e)),
	def(posInfl(a,g)),
	def(increase(a)).
example(52) :-
	makeEnvironment('ex52','functional_dependencies'),
	initEnvironment,
	def(increase(hasCubicCapacity)),
	def(negInfl(withRebate,hasPrice)),
	def(posInfl(hasPrice,hasOverallCost)),
	def(posInfl(hasCubicCapacity,hasListPrice)),
	def(posInfl(hasListPrice,hasPrice)),
	def(posInfl(hasCubicCapacity,hasFuelConsumption)),
	def(posInfl(hasFuelConsumption,hasOverallCost)),
	def(posInfl(hasCubicCapacity,hasMaxSpeed)),
	def(negInfl(hasCatConverter,hasMaxSpeed)),
	def(posInfl(hasCatConverter,hasFuelConsumption)),
	def(posInfl(hasCubicCapacity,hasWeight)),
	def(negInfl(hasWeight,hasMaxSpeed)).
example(53) :-
	makeEnvironment('ex53','functional_dependencies'),
	initEnvironment,
	def(increase(hasCubicCapacity)),
	def(infl(withRebate,hasPrice,-1.0)),
	def(infl(hasPrice,hasOverallCost,1.0)),
	def(infl(hasCubicCapacity,hasListPrice,1.2)),
	def(infl(hasListPrice,hasPrice,1.0)),
	def(infl(hasCubicCapacity,hasFuelConsumption,0.8)),
	def(infl(hasFuelConsumption,hasOverallCost,1.0)),
	def(infl(hasCubicCapacity,hasHorsePower,1.0)),
	def(infl(hasHorsePower,hasFuelConsumption,1.0)),
	def(infl(hasHorsePower,hasMaxSpeed,1.0)),
	def(infl(hasFuelType,hasMaxSpeed,0.8)),
	def(infl(hasCatConverter,hasHorsePower,-0.5)),
	def(infl(hasCubicCapacity,hasWeight,0.5)),
	def(infl(hasWeight,hasHorsePower,-1.0)).
example(54) :-
	makeEnvironment('ex54','functional_dependencies'),
	initEnvironment,
	def(negInfl(a,b)),
	def(posInfl(b,e)),
	def(posInfl(e,d)),
	def(negInfl(g,e)),
	def(negInfl(a,g)).
%
%	Apart from the notation identical to ex54.
%
example(55) :-
	makeEnvironment('ex55','functional_dependencies'),
	initEnvironment,
	def(infl(a,b,1.0)),
	def(infl(b,e,1.0)),
	def(infl(e,d,1.0)),
	def(infl(g,e,1.0)),
	def(infl(a,g,-1.0)).
example(56) :-
	makeEnvironment('ex56','functional_dependencies'),
	initEnvironment,
	def(infl(a,b,1.0)),
	def(infl(b,e,1.0)),
	def(infl(e,d,1.0)),
	def(infl(g,e,1.0)),
	def(infl(a,g,-1.0)),
	def(infl(f,g,0.5)),
	def(infl(f,h,-0.5)),
	def(infl(h,d,0.3)).
example(57) :-
	makeEnvironment('ex57','functional_dependencies'),
	initEnvironment,
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)).
example(58) :- 
	makeEnvironment('ex58','functional_dependencies'),
	initEnvironment,
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)),
	def(infl(e,b,-1.0)),
	def(infl(e,c,0.5)).
example(59) :-
	sb_defenv('mybox','sb.lit'),
	sb_initenv,
	sb_primconcept(person),
	sb_primconcept(woman,[supers([person])]),
	sb_primconcept(man,[supers([person])]),
	sb_disjoint(man,woman),
	sb_primelemrole(child,'domain-range'(parent,person,person)),
	sb_defconcept(parent,[supers([person]),
                              nr(child,1,30,2)]),
	sb_defconcept(mother,[supers([parent,woman])]),
	sb_defconcept(father,[supers([parent,man])]),
	sb_defconcept(granni,[supers([grandparent,mother])]),
	sb_defelem(harry,[isa(parent)]),
	sb_defelem(mary,[isa(mother), 
                         irole(child, 
                               iname('marys-child'),
                               [nr(1,30,2), vr(harry)])]).
example(60) :-
	makeEnvironment('ex60','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,peter),
	defprimconcept([b(believe,peter)],doctor,richPerson),
	assert_ind([b(believe,peter)],tom,doctor).
%%% Example 61
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,nest))
% deduce(elementOf(tweety,not(emu)))
% deduce(elementOf(tweety,not(cuckoo)))
% succeed
example(61) :-
	makeEnvironment('ex61','Defaults and the lottery paradox'),
	initEnvironment,	
	defprimconcept(and([bird,naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(not(nest))]), nest),
	defprimconcept(emu,not(fly)),
	defprimconcept(cuckoo,not(nest)),
	assert_ind(tweety,bird).
%%% Example 62
% deduce(elementOf(tweety,bird))
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,nest))
% consistent([])
% succeed
% deduce(elementOf(tweety,not(emu)))
% deduce(elementOf(tweety,emu))
% deduce(elementOf(tweety,not(cuckoo)))
% deduce(elementOf(tweety,cuckoo))
% deduce(elementOf(tweety,not(bird)))
% fail
example(62) :-
	makeEnvironment('ex62','Defaults and the lottery paradox'),
	initEnvironment,	
	defprimconcept(and([bird,naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(not(nest))]), nest),
	defprimconcept(emu,not(fly)),
	defprimconcept(cuckoo,not(nest)),
	defconcept(bird,or([emu,cuckoo])),
	assert_ind(tweety,bird).
%%% Example 63
% deduce(elementOf(tweety,bird))
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,nest))
% deduce(elementOf(tweety,sparrow))
% deduce(elementOf(tweety,not(emu)))
% deduce(elementOf(tweety,not(cuckoo)))
% consistent([])
% succeed
example(63) :-
	makeEnvironment('ex63','Defaults and the lottery paradox'),
	initEnvironment,	
	defprimconcept(and([bird,naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(not(nest))]), nest),
	defprimconcept(emu,not(fly)),
	defprimconcept(cuckoo,not(nest)),
	defconcept(bird,or([sparrow,emu,cuckoo])),
	assert_ind(tweety,bird).
%%% Example 64
% deduce(elementOf(peter,leftHandUsable))
% deduce(elementOf(peter,rightHandUsable))
% deduce(elementOf(peter,oneHandUsable))
% succeed
% deduce(elementOf(peter,bothHandsUsable))
% deduce(elementOf(peter,not(bothHandsUsable))
% fail
example(64) :-
	makeEnvironment('ex64','Defaults and the lottery paradox'),
	initEnvironment,	
	defprimconcept(naf(leftHandBroken),leftHandUsable),
	defprimconcept(naf(rightHandBroken),rightHandUsable),
	defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
	defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
	defconcept(bothHandsUsable,and([leftHandUsable,rightHandUsable])),
	assert_ind(peter,oneHandBroken).
%%% Example 65
% deduce(elementOf(peter,leftHandUsable))
% can prove leftHandUsable by default because
% cannot prove leftHandBroken because
% can prove oneHandBroken but
% cannot prove not(rightHandBroken) because
% cannot prove rightHandUsable because
% can prove rightHandBroken because
% can prove oneHandBroken and
% can prove not(leftHandBroken) because
% can prove leftHandUsable by default because
% cannot prove leftHandBroken because the loop check prevents
%                                     the application of any axiom
% deduce(elementOf(peter,rightHandUsable))
% deduce(elementOf(peter,not(bothHandsUsable))
% succeed
% deduce(elementOf(peter,bothHandsUsable))
% deduce(elementOf(peter,oneHandUsable))
% cannot prove oneHandUsable becauce
% (cannot prove leftHandUsable because
%  can prove leftHandBroken because
%  oneHandBroken is a fact and
%  (can prove not(rightHandBroken) because
%   can prove rightHandUsable by default because
%   cannot prove rightHandBroken because
%   can prove oneHandBroken but 
%   cannot prove not(leftHandBroken) because
%   cannot prove leftHandUsable because the loop check prevents
%                                       the application of any axiom))
% and it is also not possible possible to prove rightHandUsable
% for similar reasons
% deduce(elementOf(peter,not(oneHandUsable)))
% fail
example(65) :-
	makeEnvironment('ex65','Defaults and the lottery paradox'),
	initEnvironment,	
	defprimconcept(naf(leftHandBroken),leftHandUsable),
	defprimconcept(naf(rightHandBroken),rightHandUsable),
	defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
	defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
	defconcept(bothHandsUsable,and([leftHandUsable,rightHandUsable])),
	defprimconcept(leftHandBroken,not(leftHandUsable)),
	defprimconcept(rightHandBroken,not(rightHandUsable)),
	assert_ind(peter,oneHandBroken).
%%% Example 66
% deduce(elementOf(peter,leftHandUsable))
% deduce(elementOf(peter,rightHandUsable))
% deduce(elementOf(peter,oneHandUsable))
% deduce(elementOf(peter,not(bothHandsUsable))
% succeed
% deduce(elementOf(peter,bothHandsUsable))
% deduce(elementOf(peter,not(oneHandUsable)))
% fail
example(66) :-
	makeEnvironment('ex66','Defaults and the lottery paradox'),
	initEnvironment,	
	defprimconcept(naf(leftHandBroken),leftHandUsable),
	defprimconcept(naf(rightHandBroken),rightHandUsable),
	defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
	defconcept(oneHandUsable,or([naf(not(leftHandUsable)),naf(not(rightHandUsable))])),
	defconcept(bothHandsUsable,and([leftHandUsable,rightHandUsable])),
	defprimconcept(leftHandBroken,not(leftHandUsable)),
	defprimconcept(rightHandBroken,not(rightHandUsable)),
	assert_ind(peter,oneHandBroken).
%%% Example 67
example(67) :-
	makeEnvironment('ex67','Defaults and the lottery paradox'),
        initEnvironment,        
        defprimconcept(naf(leftHandBroken),leftHandUsable),
        defprimconcept(naf(rightHandBroken),rightHandUsable),
        defprimconcept(leftHandBroken,not(leftHandUsable)),
        defprimconcept(rightHandBroken,not(rightHandUsable)),
        defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
        defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
        defconcept(bothHandsBroken,and([leftHandBroken,rightHandBroken])),
        assert_ind(peter,oneHandBroken),
        assert_ind(peter,not(bothHandsBroken)).
example(68) :-
	makeEnvironment('ex68','Defaults and the lottery paradox'),
        initEnvironment,        
        defprimconcept(naf(bot),leftHandUsable),
        defprimconcept(naf(bot),rightHandUsable),
        defprimconcept(leftHandBroken,not(leftHandUsable)),
        defprimconcept(rightHandBroken,not(rightHandUsable)),
        defconcept(oneHandUsable,or([leftHandUsable,rightHandUsable])),
        defconcept(oneHandBroken,or([leftHandBroken,rightHandBroken])),
        defconcept(bothHandsBroken,and([leftHandBroken,rightHandBroken])),
        assert_ind(peter,oneHandBroken),
        assert_ind(peter,not(bothHandsBroken)).
%%% Example 69
% deduce(elementOf(tweety,bird))
% succeeds
% deduce(elementOf(tweety,not(bird)))
% deduce(elementOf(tweety,fly))
% deduce(elementOf(tweety,not(fly)))
% deduce(elementOf(tweety,nest))
% deduce(elementOf(tweety,not(nest)))
% fail
example(69) :-
	makeEnvironment('ex69','Defaults and the lottery paradox'),
	initEnvironment,	
	defprimconcept(and([bird,naf(exception),naf(not(fly))]), fly),
	defprimconcept(and([bird,naf(exception),naf(not(nest))]), nest),
	defprimconcept(emu,exception),
	defprimconcept(cuckoo,exception),
	defconcept(bird,or([emu,cuckoo])),
	assert_ind(tweety,bird).
%%% Example 70
% deduce(elementOf(a,clearTop))
% deduce(elementOf(a,not(clearTop)))
% fail
% deduce(elementOf(b,clearTop))
% deduce(elementOf(b,clearTop))
% succeed
example(70) :-
	makeEnvironment('ex70','Defaults and existential quantification'),
	initEnvironment,
	defconcept(blocked,some(on,top)),
	defprimconcept(and([block,naf(blocked)]),clearTop),
	assert_ind(a,block),
	assert_ind(b,block),
	assert_ind(c,block),
	assert_ind(a,b,on).
example(71) :-
	makeEnvironment('ex71','PRACMA'),
	initEnvironment,
	defprimconcept(sporttyp),
	defprimconcept(envtyp),
	sb_primconcept([b(believe,pk)],carwish,[supers([car])]),
	sb_primelemrole([bc(want,sporttyp)],has_tyre,'domain-range'(carwish,broad_tyre,broad_tyre)),
	sb_primelemrole([bc(believe,sporttyp)],speed,'domain-range'('2cv',low,low)),
	sb_primelemrole([bc(want,envtyp)],has_part,'domain-range'(carwish,cat_conv,cat_conv)),
	sb_primelemrole([bc(believe,envtyp)],speed,'domain-range'('2cv',fast,fast)),
%	modalAxioms(kd45,believe,pk),	
%	modalAxioms(kd45,want,pk),	
	modalAxioms(kd45,believe,concept(sporttyp)),
	modalAxioms(kd45,want,concept(sporttyp)),
	modalAxioms(kd45,believe,concept(envtyp)),
	modalAxioms(kd45,want,concept(envtyp)).
example(72) :-
	makeEnvironment('ex72','DEMO'),
	initEnvironment,
	defprimconcept(sporttyp),
	defprimconcept(umwelttyp),
	modalAxioms([b(believe,pv)],k,want,concept(sporttyp)),
	modalAxioms([b(believe,pv)],k,want,concept(umwelttyp)),
	modalAxioms(kd45,believe,all),
	sb_primconcept([b(believe,all)], vw, [supers([auto])]),
	sb_primconcept([b(believe,all)], opel, [supers([auto])]),
	assert_ind([b(believe,all)],polo,vw),
	assert_ind([b(believe,all)],manta,opel),
	sb_defconcept([b(believe,pv),bc(want,sporttyp)],wunsch_auto,[supers([auto,hatSpoiler])]),
	% Anmerkung:
        % In MOTEL ist es m"oglich, mehrere sich erg"anzende Definitionen 
        % f"ur ein Konzept (hier z.B. vw) zu haben. Damit ein Konzept im
        % Verlauf des Dialogs st"andig zu verfeinern. 
	sb_primconcept([b(believe,pv),bc(believe,sporttyp)],vw,[supers([langsam])]),
	defprimconcept([b(believe,pv),bc(believe,sporttyp)],and([auto,hatKat,naf(not(langsam))]),langsam),
	sb_defconcept([b(believe,pv),bc(want,umwelttyp)],wunsch_auto,[supers([auto,hatKat])]),
	sb_primconcept([b(believe,pv),bc(believe,umwelttyp)],vw,[supers([not(langsam)])]),
	assert_ind([b(believe,pv)],pk,sporttyp),
        % Anmerkung:
	% Bei der folgenden Definition reicht es nicht zu sagen, da\3
        % polo ein Auto ist oder das alle glauben, da\3 polo ein Auto ist,
        % da man durchaus Sachen im want haben kann, die der Realit"at 
        % widersprechen. Deshalb mu\3 pk wollen, da\3 polo ein auto ist.
	assert_ind([b(believe,pv),b(want,pk)],polo,auto).
        % Demo:
        %
        % setof(C,ask([b(believe,pk)],elementOf(polo,C)),L).
        % L = [auto,langsam,top,vw,not(bot)]
        % Zun"achst erbt hier der pk vom b(believe,all), den Glauben, da\3
        % polo ein vw und damit ein auto ist. Vom b(believe,sporttyp) erbt 
        % er, da\3 vw's langsam sind, womit auch der polo langsam ist.
        % 
        % setof(C,ask([b(believe,pk)],elementOf(manta,C)),L)
        % L = [auto,opel,top,not(bot)]
        % Da es sich bei dem manta um einen opel handelt, wird zun"achst
        % nicht angenommen, da\3 der manta langsam ist.
        %
        % assert_ind([b(believe,pv),b(believe,pk)],manta,hatKat)
        % Hiermit haben wir festgelegt, da\3 der pk glaubt, da\3 der manta
        % einen Katalysator hat. Nun erbt der pk vom sporttyp aber auch
        % die Regel, da\3 Autos mit Katalysatoren normalerweise langsam sind.
        % Dies f"uhrt bei der Wiederholung der letzten Anfrage zu folgendem
        % Ergebnis:
        %
        % setof(C,ask([b(believe,pk)],elementOf(manta,C)),L)
        % L = [auto,hatKat,langsam,opel,top,not(bot)]
        %
        % Wir k"onnen neben der Deduktion auf Abduktion verwenden:
        %
        % abduce([b(want,pk)],H,elementOf(polo,wunsch_auto),E).
        % E = proved(in(app(_A:m(want,pk),[]),wunsch_auto,polo),
        %     basedOn(and([proved(in(app(_A:m(want,pk),[]),auto,polo),
        %     basedOn(abox)),
        %     proved(in(app(_A:m(want,pk),[]),hatSpoiler,polo),
        %     basedOn(usingAbHyp(in(app(_A:m(want,pk),[]),hatSpoiler,polo))))]))),
        % H = [in(app(_B:m(want,pk),[]),hatSpoiler,polo)]          
        % D.h. pk will den polo als Wunschauto, wenn der polo einen Spoiler hat.
        %
        % Nun erhalten wir von PRACMA die Anweisungen zu einem 
        % Stereotypwechsel:
        %
        % delete_ind(pk,sporttyp)
        % assert_ind(pk,umwelttyp)
        %
        % Dadurch "andern sich die Anfrageergebnisse wie folgt:
        %
        % setof(C,ask([b(believe,pk)],elementOf(polo,C)),L).
        % L = [auto,top,vw,not(bot),not(langsam)]
        %
        % Der polo geh"ort nun zu den nicht langsamen Autos, da umwelttypen
        % genau dies glauben.
        % 
        % setof(C,ask([b(believe,pk)],elementOf(manta,C)),L).
        % L = [auto,hatKat,opel,top,not(bot)]
        % 
        % Der Manta hat zwar immernoch einen Katalysator, ist aber trotzdem
        % nicht langsam, da umwelttypen nicht glauben, da\3 Katalysatoren ein
        % Auto langsam machen.
        %
        % Wir k"onnen auch in diesem Fall fragen, unter welchen Umst"anden
        % pk den polo f"ur sein Wunschauto halten w"urde:
        %
        % abduce([b(want,pk)],H,elementOf(polo,wunsch_auto),E).
        % E = proved(in(app(_A:m(want,pk),[]),wunsch_auto,polo),
        %     basedOn(and([proved(in(app(_A:m(want,pk),[]),auto,polo),
        %     basedOn(abox)),
        %     proved(in(app(_A:m(want,pk),[]),hatKat,polo),
        %     basedOn(usingAbHyp(in(app(_A:m(want,pk),[]),hatKat,polo))))]))),
        % H = [in(app(_B:m(want,pk),[]),hatKat,polo)]
        %
        % Wie erwartet, soll das Wunschauto von pk nun einen Katalysator haben.
example(73) :-
	assert_ind([b(believe,all)],polo,vw),
	defprimconcept([b(believe,pv),bc(believe,sporttyp)],vw,langsam),
	assert_ind([b(believe,pv)],pk,sporttyp),
	modalAxioms(kd45,believe,pv),
	modalAxioms([b(believe,pv)],kd45,believe,concept(sporttyp)).
example(74) :-
	makeEnvironment('ex74','DEMO'),
	initEnvironment,
	defprimconcept(sporttyp),
	defprimconcept(umwelttyp),
	modalAxioms([b(believe,pv)],k,want,concept(sporttyp)),
	modalAxioms([b(believe,pv)],k,want,concept(umwelttyp)),
	modalAxioms(kd45,believe,all),
	sb_primconcept([b(believe,all)], vw, [supers([auto])]),
	sb_primconcept([b(believe,all)], opel, [supers([auto])]),
	assert_ind([b(believe,all)],polo,vw),
	assert_ind([b(believe,all)],manta,opel),
	defprimconcept([b(believe,pv),bc(want,sporttyp)],and([auto,or([hatSpoiler,hatSchiebedach])]),wunsch_auto),
	defprimconcept([b(believe,pv),bc(believe,sporttyp)],vw,langsam),
	defprimconcept([b(believe,pv),bc(believe,sporttyp)],and([auto,hatKat,naf(not(langsam))]),langsam),
	defconcept([b(believe,pv),bc(want,umwelttyp)],wunsch_auto,and([auto,hatKat])),
	defprimconcept([b(believe,pv),bc(believe,umwelttyp)],vw,not(langsam)),
	assert_ind([b(believe,pv)],pk,sporttyp),
	assert_ind([b(believe,pv),b(want,pk)],polo,auto).
example(75) :-
	makeEnvironment('ex75','DEMO'),
	initEnvironment,
	defprimconcept(racer),
	defprimconcept(creeper),
	modalAxioms([b(believe,ps)],k,want,concept(racer)),
	modalAxioms([b(believe,ps)],k,want,concept(creeper)),
	modalAxioms(kd45,believe,all),
	defprimconcept([b(believe,all)], vw, car),
	defprimconcept([b(believe,all)], bmw, car),
	assert_ind([b(believe,all)],beetle,vw),
	assert_ind([b(believe,all)],'bmw735',bmw),
	defprimconcept([b(believe,ps),bc(want,racer)],and([car,or([has_spoiler,has_sliding_roof])]),dream_car),
	defprimconcept([b(believe,ps),bc(believe,racer)],vw,slow),
	defprimconcept([b(believe,ps),bc(believe,racer)],and([car,has_cat_conv,naf(not(slow))]),slow),
	defconcept([b(believe,ps),bc(want,creeper)],dream_car,and([car,has_cat_conv])),
	defprimconcept([b(believe,ps),bc(believe,creeper)],vw,not(slow)),
	assert_ind([b(believe,ps)],pc,racer),
	assert_ind([b(believe,ps),b(want,pc)],beetle,car).
example(76) :-
	makeEnvironment('ex76','SETHEO'),
	initEnvironment,
	defprimconcept(racer),
	defprimconcept(creeper),
	modalAxioms([b(believe,ps)],k,want,concept(racer)),
	modalAxioms([b(believe,ps)],k,want,concept(creeper)),
	modalAxioms(kd45,believe,all),
	defprimconcept([b(believe,all)], vw, car),
	defprimconcept([b(believe,all)], bmw, car),
	assert_ind([b(believe,all)],beetle,vw),
	assert_ind([b(believe,all)],'bmw735',bmw),
	defprimconcept([b(believe,ps),bc(want,racer)],and([car,or([has_spoiler,has_sliding_roof])]),dream_car),
	defprimconcept([b(believe,ps),bc(believe,racer)],vw,slow),
	defconcept([b(believe,ps),bc(want,creeper)],dream_car,and([car,has_cat_conv])),
	defprimconcept([b(believe,ps),bc(believe,creeper)],vw,not(slow)),
	assert_ind([b(believe,ps)],pc,racer),
	assert_ind([b(believe,ps),b(want,pc)],beetle,car).




/**********************************************************************
 *
 * @(#) fdwAbduce.pl 1.1@(#)
 *
 */

aux_abduce(Env,World,[],change(Y,_),Ws) :-
	bagof(W,Z^changingInfl(Env,World,Z,Y,W),Ws),
	!.

aux_abduce(_,_,[],_,[]).

aux_abduce(Env,World,[change(X,Wx)|Hs],change(Y,_),[Wy|Ws]) :-
	infl(Env,World,X,Y,Wxy),
	not(given_change(Env,World,X,_)),
	weightOf_change(Wx,Wxy,Wy),
	aux_abduce(Env,World,Hs,change(Y,_),Ws).

aux2_abduce(_,_,_,[]).

aux2_abduce(EnvName,MS,change(X,Wx),[Change|Changes]) :-
	nonvar(Change),
	abduce(EnvName,MS,change(X,Wx),Change,[]),
	abduce(EnvName,MS,change(X,Wx),Changes,[]).

/***********************************************************************
 *
 * default_change(+-Change,+-WeightedChange)
 *
 *	Change is of the form 
 *		increase(+-X), noChange(+-X), decrease(+-X).
 *
 *	WeightedChange is of the form
 *		change(+-X,+-W)
 *	with W being the default weight associated with Change.
 */

default_change(increase(X),change(X,W)) :-
	nonvar(W),
	W > 0.0.

default_change(decrease(X),change(X,W)) :-
	nonvar(W),
	W < 0.0.

default_change(noChange(X),change(X,W)) :-
	nonvar(W),
	W = 0.0.

default_change(increase(X),change(X,1.0)).

default_change(decrease(X),change(X,-1.0)).

default_change(noChange(X),change(X,0.0)).

/***********************************************************************
 *
 * default_changes(+-Changes,+-WeightedChanges)
 *
 *	Changes is a list of
 *		increase(+-X), noChange(+-X), decrease(+-X)
 *	predicates.
 *
 *	WeightedChanges is a list of
 *		change(+-X,+-W)
 *	predicates where W being the default weight associated 
 *	with the appropriate Changes predicates.
 */

default_changes([],[]).

default_changes([Change|Changes],[WeightedChange|WeightedChanges]) :-
	default_change(Change,WeightedChange),
	default_changes(Changes,WeightedChanges).










/**********************************************************************
 *
 * @(#) fdwUserInterface.pl 1.1@(#)
 *
 */


/***********************************************************************
 *
 * initFuncdep
 *
 *	asserts default given_ clauses which prevent errors if the user
 *	has not (implicitly) defined any given_ clauses.
 */

initFuncdep :-
	assertz((given_inflLink(_,_,_,_) :- !, fail)),
	assertz((given_change(_,_,_,_) :- !, fail)).
	

/***********************************************************************
 *
 * initialize, initialise
 *
 *	Similar to initialize in
 *	~hustadt/pop/motel/motel-0.0.6/userInterface.pl
 */

% For those of us who prefer the alternative spelling
initialise :-
	initialize.

/***********************************************************************
 *
 * listFDW
 *
 *	lists the internal representation of the information defined by 
 *	the user.
 */

listFDW :-
	listing(given_inflLink),
	listing(given_change).

/***********************************************************************
 *
 * showFDW
 *
 *	displays the user defined information in the knowledge base.
 *	(Similar to showEnvironment.)
 */

showFDW :-
        getCurrentEnvironment(EnvName),
	environment(EnvName,Env,Comment),
        print('Functional Dependencies: '),
        print(EnvName),
        print(' ('),
        print(Comment),
        print(')'),
        nl,
	showFDW(Env).

showFDW(Env) :-
	showInfl(Env),
	showChange(Env).

showInfl(Env) :-
	given_inflLink(Env,World,app(_,W,X),Y),
	print(World),
	print('infl('),
	print(X), print(','), print(Y), print(','), print(W), print(').'),
	nl,
	fail.

showInfl(_).

showChange(Env) :-
	given_change(Env,World,X,W),
	print(World),
	print('change('),
	print(X), print(','), print(W), print(').'),
	nl,
	fail.

showChange(_).

showFD :-
        getCurrentEnvironment(EnvName),
	environment(EnvName,Env,Comment),
        print('Functional Dependencies: '),
        print(EnvName),
        print(' ('),
        print(Comment),
        print(')'),
        nl,
	showFD(Env).

showFD(Env) :-
	showPosInfl(Env),
	showNegInfl(Env),
	showNoInfl(Env),
	showIncrease(Env),
	showDecrease(Env),
	showNoChange(Env).

showPosInfl(Env) :-
	given_inflLink(Env,World,app(_,W,X),Y),
	W > 0.0,
	print(World),
	print('posInfl('),
	print(X), print(','), print(Y), print(').'),
	nl,
	fail.

showPosInfl(_).

showNegInfl(Env) :-
	given_inflLink(Env,World,app(_,W,X),Y),
	W < 0.0,
	print(World),
	print('negInfl('),
	print(X), print(','), print(Y), print(').'),
	nl,
	fail.

showNegInfl(_).

showNoInfl(Env) :-
	given_inflLink(Env,World,app(_,0.0,X),Y),
	print(World),
	print('noInfl('),
	print(X), print(','), print(Y), print(').'),
	nl,
	fail.

showNoInfl(_).

showIncrease(Env) :-
	given_change(Env,World,X,W),
	W > 0.0,
	print(World),
	print('increase('),
	print(X), print(').'),
	nl,
	fail.

showIncrease(_).

showDecrease(Env) :-
	given_change(Env,World,X,W),
	W < 0.0,
	print(World),
	print('decrease('),
	print(X), print(').'),
	nl,
	fail.

showDecrease(_).

showNoChange(Env) :-
	given_change(Env,World,X,0.0),
	print(World),
	print('noChange('),
	print(X), print(').'),
	nl,
	fail.

showNoChange(_).

/***********************************************************************
 *
 * def(+EnvName,+MS,:+Fact)
 *
 *	Fact is one of 
 *		infl(+X,+Y,+W),
 *		posInfl(+X,+Y), negInfl(+X,+Y), noInfl(+X,+Y),
 *		change(+X,+W),
 *		increase(+X), decrease(+X).
 *
 *	This predicate is used to update the knowledge base of
 *	information about the functional dependencies. The definition
 *	of multiple influences between attributes and multiple changes 
 *	on an attribute are prevented. 
 *
 *	Note that, X and Y denote roles/attributes (of cars) and W
 *	denotes the weight of X influencing Y or W denotes the weight
 *	of change of an attribute. 
 *
 *	posInfl is assigned the weight 1.0, negInfl the weight -1.0 and 
 *	noInfl the weight 0.0. The weights for increase, decrease and
 *	noChange are 1.0, -1.0 and 0.0, respectively.
 */

def(EnvName,MS,infl(X,Y,W)) :-
	get_Env_World(EnvName,MS,Env,World),
	atomic(X),
	assertNames(Env,World,X,role),
	atomic(Y),
	assertNames(Env,World,Y,role),
	wellDefined_InflWeight(W),
	not(given_inflLink(Env,World,app(_,_,X),Y)),
	gensym(sk,F),
	asserta(given_inflLink(Env,World,app(F,W,X),Y)).

def(EnvName,MS,change(X,W)) :-
	get_Env_World(EnvName,MS,Env,World),
	atomic(X),
	assertNames(Env,World,X,role),
	wellDefined_ChangeWeight(W),
	not(given_change(Env,World,X,_)),
	asserta(given_change(Env,World,X,W)).

def(EnvName,MS,posInfl(X,Y)) :-
	def(EnvName,MS,infl(X,Y,1.0)).

def(EnvName,MS,negInfl(X,Y)) :-
	def(EnvName,MS,infl(X,Y,-1.0)).

def(EnvName,MS,noInfl(X,Y)) :-
	def(EnvName,MS,infl(X,Y,0.0)).

def(EnvName,MS,increase(X)) :-
	def(EnvName,MS,change(X,1.0)).

def(EnvName,MS,decrease(X)) :-
	def(EnvName,MS,change(X,-1.0)).

def(EnvName,MS,noChange(X)) :-
	def(EnvName,MS,change(X,0.0)).

/***********************************************************************
 *
 * def(:+Fact)
 *
 *	calls def(+EnvName,+MS,:+Fact) with default environment EnvName 
 *	and empty modal sequence.
 */

def(Fact) :-
        getCurrentEnvironment(EnvName),
	def(EnvName,[],Fact).

/***********************************************************************
 *
 * def(+EnvName,:+Fact)
 *
 *	calls def(+EnvName,+MS,:+Fact) with default environment EnvName 
 *	and empty modal sequence.
 */

def(EnvName,Fact) :-
        environment(EnvName,_,_),
	def(EnvName,[],Fact).

/***********************************************************************
 *
 * def(+MS,:+Fact)
 *
 *	calls def(+EnvName,+MS,:+Fact) with default environment EnvName 
 *	and empty modal sequence.
 */

def(MS,Fact) :-
        nonvar(MS),
        (MS = [] ; MS = [_|_]),
        !,
        getCurrentEnvironment(EnvName),
	def(EnvName,[],Fact).

/***********************************************************************
 *
 * undef(+EnvName,+MS,:+-Fact)
 *
 *	retracts all facts matching Fact.
 */

undef(EnvName,MS,infl(X,Y,W)) :-
        environment(EnvName,Env,_),
	retract(given_inflLink(Env,MS,app(_,W,X),Y)),
	fail.
undef(EnvName,MS,change(X,W)) :-
        environment(EnvName,Env,_),
	retract(given_change(EnvName,MS,X,W)),
	fail.
undef(EnvName,MS,posInfl(X,Y)) :-
        environment(EnvName,Env,_),
	retract(given_inflLink(Env,MS,app(_,1.0,X),Y)),
	fail.
undef(EnvName,MS,negInfl(X,Y)) :-
        environment(EnvName,Env,_),
	retract(given_inflLink(Env,MS,app(_,-1.0,X),Y)),
	fail.
undef(EnvName,MS,noInfl(X,Y)) :-
        environment(EnvName,Env,_),
	retract(given_inflLink(Env,MS,app(_,0.0,X),Y)),
	fail.
undef(EnvName,MS,increase(X)) :-
        environment(EnvName,Env,_),
	retract(given_change(Env,MS,X,1.0)),
	fail.
undef(EnvName,MS,decrease(X)) :-
        environment(EnvName,Env,_),
	retract(given_change(Env,MS,X,-1.0)),
	fail.
undef(EnvName,MS,noChange(X)) :-
        environment(EnvName,Env,_),
	retract(given_change(Env,MS,X,0.0)),
	fail.
undef(_,_,_).

/***********************************************************************
 *
 * undef(:+-Fact)
 *
 *	retracts all facts matching Fact in default environment and
 *	default modal context.
 */

undef(Fact) :-
        getCurrentEnvironment(EnvName),
	undef(EnvName,[],Fact).

/***********************************************************************
 *
 * undef(+EnvName,:+-Fact)
 *
 *	retracts all facts matching Fact in default environment and
 *	default modal context.
 */

undef(EnvName,Fact) :-
        environment(EnvName,_,_),
	!,
	undef(EnvName,[],Fact).

/***********************************************************************
 *
 * undef(+MS,:+-Fact)
 *
 *	retracts all facts matching Fact in default environment and
 *	default modal context.
 */

undef(MS,Fact) :-
        (MS = [] ; MS = [_|_]),
        !,
        getCurrentEnvironment(EnvName),
	undef(EnvName,[],Fact).

/***********************************************************************
 *
 * get_Env_World(+EnvName,+MS,-Env,-World),
 *
 */

get_Env_World(EnvName,MS,Env,World) :-
	environment(EnvName,Env,_),
        convertMS(Env,[[],true],MS,[],[World,G1],_),
        call(G1).

/**********************************************************************
 *
 * %A%
 *
 */
%------------------------------------------------------------------------------
% Project:      MOTEL 1.0
% Module:       folToClause.pl
% Purpose:      Translation of first-order logic formulae to clauses
% Last Change:  04.02.93
% Language:     Prolog
% Author:       Ullrich Hustadt
% Address:      Max-Planck-Institut for Computer Science
%               Im Stadtwald
%               6600 Saarbr"ucken
%               Germany
% Email:        Ullrich.Hustadt@mpi-sb.mpg.de
% Copyright:    (C) 1993 Ullrich Hustadt
% Copying:      This software is provided under the GNU General Public License.
% Warranty:     This is a research prototype. There is absolutely no warranty.
%------------------------------------------------------------------------------
%
% Syntax of first-order logic formulae
% ====================================
% Atom    => in(X,ConceptName)
%            eq(PrologTerm,PrologTerm)
%            rel(RoleName,PrologTerm,PrologTerm)
%         
% Formula =>   Atom
%            | and([Formula,Formula]) 
%            | or([Formula,Formula])
%            | not(Formula)
%            | implies(Formula,Formula)
%            | equivalent(Formula,Formula)
%            | forall(PrologVar,Formula)
%            | exists(PrologVar,Formula)
% 
% Syntax of clauses
% =================
% Clause  => cl(Head,Body)
% Head    =>   []
%            | [Atom|Head]
% Body    =>   []
%            | [Atom|Body]
    
%----------------------------------------------------------------------
% translate(+F,-C)
% Parameter: F   First-order formula
%            C   Clause

translate(X,Clauses) :-
	implout(X,X1),
	negin(X1,X2),
	skolem(X2,X3,[]),
	univout(X3,X4),
	conjn(X4,X5),
	clausify(X5,Clauses,[]).


%----------------------------------------------------------------------
% implout(+F1,-F2)
% Arguments: F1   First-order formula
%            F2   First-order formula
% removes implications and equivalences in F1 resulting in F2
%
% Author: Ullrich Hustadt

implout(equivalent(P,Q),or([and([P1,Q1]),and([not(P1),not(Q1)])])) :-
	!,
	implout(P,P1),
	implout(Q,Q1).
implout(implies(P,Q),or([not(P1),Q1])) :-
	!,
	implout(P,P1),
	implout(Q,Q1).
implout(forall(X,P),forall(X,P1)) :-
	!,
	implout(P,P1).
implout(exists(X,P),exists(X,P1)) :-
	!,
	implout(P,P1).
implout(and(L),and(L1)) :-
	!,
	map(implout,L,L1).
implout(or(L),or(L1)) :-
	!,
	map(implout,L,L1).
implout(not(P),not(P1)) :-
	!,
	implout(P,P1).
implout(P,P).

%----------------------------------------------------------------------
% negin(+F1,-F2)
% Parameter: F1   First-order formula
%            F2   First-order formula
% computes the negation normal form of F1 
%
% Author: Ullrich Hustadt

negin(not(P),P1) :-
	!,
	neg(P,P1).
negin(forall(X,P),forall(X,P1)) :-
	!,
	negin(P,P1).
negin(exists(X,P),exists(X,P1)) :-
	!,
	negin(P,P1).
negin(and(L),and(L1)) :-
	!,
	map(negin,L,L1).
negin(or(L),or(L1)) :-
	!,
	map(negin,L,L1).
negin(P,P).

%----------------------------------------------------------------------
% neg(+F1,-F2)
% Parameter: F1   First-order formula
%            F2   First-order formula
% negates formula F1 to get F2
%
% Author: Ullrich Hustadt

neg(not(P),P1) :-
	!,
	negin(P,P1).
neg(forall(X,P),exists(X,P1)) :-
	!,
	neg(P,P1).
neg(exists(X,P),forall(X,P1)) :-
	!,
	neg(P,P1).
neg(and(L),or(L1)) :-
	!,
	map(neg,L,L1).
neg(or(L),and(L1)) :-
	!,
	map(neg,L,L1).
neg(P,not(P)).

%----------------------------------------------------------------------
% skolem(+F1,-F2,*Vars)
% Parameter: F1     First-order formula
%            F2     First-order formula
%            Vars   List of variables
% F2 is the skolemized form of F1.
%
% Author: Ullrich Hustadt

skolem(forall(X,P),forall(X,P1),Vars) :-
	!,
	skolem(P,P1,[X|Vars]).
skolem(exists(X,P),P2,Vars) :-
	!,
	skolem(P,P1,Vars),
	gensym(f,F),
	Sk =.. [F|Vars],
	subst(P1,P2,X,Sk).
skolem(and(L),and(L1),Vars) :-
	!,
	map(skolem,[Vars],L,L1).
skolem(or(L),or(L1),Vars) :-
	!,
	map(skolem,[Vars],L,L1).
skolem(P,P,_).


%----------------------------------------------------------------------
% subst(+F1,-F2,+X,+Sk)
% Parameter: F1     First-order formula
%            F2     First-order formula
%            X      Variable that will be substituted
%            Sk     Skolem term 
% substitutes Sk for X in formula F1.
% 
% Author: Ullrich Hustadt

subst(T1,T2,X,Sk) :-
	(atomic(T1) ; var(T1)),
	T1 == X,
	!,
	T2 = Sk.
subst(T1,T2,X,_Sk) :-
	(atomic(T1) ; var(T1)),
	not(T1 == X),
	!,
	T2 = T1.
subst(forall(Y,P),forall(Y,P),X,_Sk) :-
	X == Y,
	!.
subst(forall(Y,P),forall(Y,P1),X,Sk) :-
	!,
	subst(P,P1,X,Sk).
subst(exists(Y,P),exists(Y,P),X,_Sk) :-
	X == Y,
	!.
subst(exists(Y,P),exists(Y,P1),X,Sk) :-
	!,
	subst(P,P1,X,Sk).
subst(and(L),and(L1),X,Sk) :-
	!,
	map(subst,[X,Sk],L,L1).
subst(or(L),or(L1),X,Sk) :-
	!,
	map(subst,[X,Sk],L,L1).
subst(not(P),not(P1),X,Sk) :-
	!,
	subst(P,P1,X,Sk).
subst(T1,T2,X,Sk) :-
	!,
	T1 =.. [F|Args],
	map(subst,[X,Sk],Args,Args1),
	T2 =.. [F|Args1].

%----------------------------------------------------------------------
% univout(+F1,-F1)
% Parameter: F1   First-order formula
%            F2   First-order formula
% removes quantifiers
%
% Author: Ullrich Hustadt

univout(forall(_X,P),P1) :-
	!,
	univout(P,P1).
univout(and(L),and(L1)) :-
	!,
	map(univout,L,L1).
univout(or(L),or(L1)) :-
	!,
	map(univout,L,L1).
univout(P,P).

%----------------------------------------------------------------------
% conjn(+F1,-F2)
% Parameter: F1   First-order formula
%            F2   First-order formula
% computes the conjunctive normal form of F1
%
% Author: Ullrich Hustadt

conjn(and(L),R) :-
	!,
	map(conjn,L,L1),
	conjn1(and(L1),R).
conjn(or(L),R) :-
	!,
	map(conjn,L,L1),
	conjn1(or(L1),R).
conjn(P,P).

conjn1(or([and([P,Q]),R]),and([P1,Q1])) :-
	!,
	conjn(or([P,R]),P1),
	conjn(or([Q,R]),Q1).
conjn1(or([R,and([P,Q])]),and([P1,Q1])) :-
	!,
	conjn(or([P,R]),P1),
	conjn(or([Q,R]),Q1).
conjn1(P,P).

clausify(and([P,Q]),C1,C2) :-
	!,
	clausify(P,C1,C3),
	clausify(Q,C3,C2).
clausify(P,[cl(A,B)|Cs],Cs) :-
	inclause(P,A,[],B,[]),
	!.
clausify(_,C,C).

inclause(or([P,Q]),A,A1,B,B1) :-
	!,
	inclause(P,A2,A1,B2,B1),
	inclause(Q,A,A2,B,B2).
inclause(not(P),A,A,B1,B) :-
	!,
	not(memq(P,A)),
	putin(P,B,B1).
inclause(P,A1,A,B,B) :-
	not(memq(P,B)),
	putin(P,A,A1).

putin(X,[],[X]) :-
	!.
putin(X,[Y|L],L) :-
	X == Y,
	!.
putin(X,[Y|L],[Y|L1]) :-
	putin(X,L,L1).


	
memq(_X,[]) :-
	!,
	fail.
memq(X,[Y|_L]) :-
	X == Y,
	!.
memq(X,[_|L]) :-
	memq(X,L).
	
	


/**********************************************************************
 *
 * @(#) hop.pl 1.4@(#)
 *
 */

/***********************************************************************
 * 
 * hop_map(+Pred,+InList,-OutList)
 * calls Pred(X,Y) for all X in InList and collects all Y to get
 * OutList.
 *
 */

hop_map(_Pred,[],[]) :- !.
hop_map(Pred,[H1|T1],[H2|T2]) :-
	Clause =.. [Pred,H1,H2],
	call(Clause),
	hop_map(Pred,T1,T2).

/***********************************************************************
 * 
 * hop_map(+Pred,+[A1,...,An],+InList,-OutList)
 * calls Pred(A1,...,An,X,Y) for all X in InList and collects all Y to 
 * get OutList.
 *
 */

hop_map(_Pred,_Args,[],[]) :- !.
hop_map(Pred,Args,[H1|T1],[H2|T2]) :-
	Clause =.. [Pred|[H1,H2|Args]],
	call(Clause),
	hop_map(Pred,Args,T1,T2).

mapGoal(_Goal,_X,[]) :-
	!.
mapGoal(Goal,X,[(Y,_PTY)|L1]) :-
	not(not((X = Y, call(Goal)))),
	mapGoal(Goal,X,L1).
	
/**********************************************************************
 *
 * try(+G)
 * calls G and succeeds even if G fails.
 *
 */

try(G)  :- call(G).
try(_G) :- true.


/**********************************************************************
 *
 * doboth(G1,G2)
 * calls G1 and if G1 succeeds G2 is called.
 *
 */

doboth(G1,G2) :-
	call(G1),
	call(G2).

/**********************************************************************
 *
 * tell(GoalList)
 * calls all the goals given in argument GoalList which is either a
 * list of PROLOG goals or a single PROLOG goal.
 *
 */

callList([]) :-
	!.
callList([G1|GL]) :-
	!,
	call(G1),
	callList(GL).
callList(G1) :-
	call(G1).


/**********************************************************************
 *
 * toList(+ListTerm,-List)
 * converts a LISP-like list ListTerm into a PROLOG-like list List
 *
 */

tolist(nil,[]) :- !.
tolist(cons(A,L1),[A|L2]) :-
	tolist(L1,L2).

/**********************************************************************
 *
 * setofOrNil(A,B,C)
 * succeeds with the same result as setof(A,B,C) if setof(A,B,C) 
 * succeeds. Otherwise C will be instantiated with the empty list.
 *
 */

setofOrNil(A,B,C) :-
	setof(A,B,C),
	!.
setofOrNil(A,B,[]) :-
	!.

/**********************************************************************
 *
 * bagofOrNil(A,B,C)
 * succeeds with the same result as bagof(A,B,C) if bagof(A,B,C) 
 * succeeds. Otherwise C will be instantiated with the empty list.
 *
 */

bagofOrNil(A,B,C) :-
	setof(A,B,C),
	!.
bagofOrNil(A,B,[]) :-
	!.

/**********************************************************************
 *
 * @(#) infl.pl 1.4@(#)
 *
 */

/***********************************************************************
 *
 * getInflDescription(+Env,+World,-DescriptY,+-Y)
 *
 *	Given a role Y this predicate returns its description in terms
 *	of the least attribute on which Y depends as a possible chain 
 *	of influence relationships. DescriptY is an attribute or it 
 *	is of the form app(sk_,W,Z) where W denotes a weight.
 *
 *	This predicate is now superfluous. Its replacement is the one
 *	with three arguments.
 */

getInflDescription(Env,World,app(F,W,DescriptZ),Y) :-
	given_inflLink(Env,World,app(F,W,Z),Y),
	getInflDescription(Env,World,DescriptZ,Z).

getInflDescription(Env,World,X,X) :-
	atom(X),
	!,
	not(given_inflLink(Env,World,_,X)).

getInflDescription(Env,World,X,X) :-
	var(X),
	!.

/***********************************************************************
 *
 * getInflDescription(+Env,+World,-DescriptY,+-X,+-Y)
 *
 *	Given a role Y this predicate returns its description in terms
 *	of the attribute X on which Y depends as a chain of influence 
 *	relationships. DescriptY is an attribute or it is of the form 
 *	app(sk_,W,Z) where W denotes a weight.
 */

getInflDescription(Env,World,app(F,W,DescriptZ),X,Y) :-
	given_inflLink(Env,World,app(F,W,Z),Y),
	getInflDescription(Env,World,DescriptZ,X,Z).

getInflDescription(Env,World,X,X,X) :-
	!.

/***********************************************************************
 *
 * test_inflLink(+-X,+-Y,+-W)
 *
 *	X is either an attribute/role or it is of the form
 *	app(sk_,_,_).
 *
 *	Y is of the form app(+-F,1.0,+Z) or app(+-F,-1.0,+Z).
 *
 *	This predicate tests for influence of weight W between X and Y.
 */

test_inflLink(X,Y,W) :-
	nonvar(X),
	Y = app(F,W,X),
	!,
	nonvar(F).

test_inflLink(X,Y,W) :-
	var(X),
	nonvar(Y),
	Y = app(F,W,X),
	atom(X),
	!.

test_inflLink(X,Y,W) :-
	var(X),
	var(Y),
	!,
	fail.

test_inflLink(X,app(F,W1,Y),W) :-
	test_inflLink(X,Y,W2),
	weightOf_ChainedInfl(W1,W2,W).

/***********************************************************************
 *
 * inflLink(+Env,+World,+-X,+-Y,+-W)
 *
 *	tests if attribute X influences attribute Y with weight W.
 *
 *	Note: the output for uninstantiated X and/or Y depends on the
 *	order of the literals in the body of infl.
 */

inflLink(Env,World,X,Y,W) :-
	wellDefined_attribute(Env,World,X),
	wellDefined_attribute(Env,World,Y),
	(var(W);
	wellDefined_InflWeight(W)),
	getInflDescription(Env,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

inflLink(Env,World,X,Y,W) :-
	var(X),
	wellDefined_attribute(Env,World,Y),
	(var(W);
	wellDefined_InflWeight(W)),
	getInflDescription(Env,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

inflLink(Env,World,X,Y,W) :-
	wellDefined_attribute(Env,World,X),
	var(Y),
	(var(W);
	wellDefined_InflWeight(W)),
	getInflDescription(Env,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

inflLink(Env,World,X,Y,W) :-
	var(X),
	var(Y),
	(var(W);
	wellDefined_InflWeight(W)),
	getInflDescription(Env,World,Z2,X,Y),
	test_inflLink(X,Z2,W).

/***********************************************************************
 *
 * leastInfl(+Env,+World,+-X,+-Y)
 *
 *	succeeds if X is a least attribute influencing Y.
 */

leastInfl(Env,World,X,Y) :-
	getInflDescription(Env,World,_,X,Y),
	not(X = Y),
	not(given_inflLink(Env,World,_,X)).

/***********************************************************************
 *
 * leastInfls(+Env,+World,+-Xs,+Y)
 *
 *	collects the least attributes influencing Y in Xs.
 */

leastInfls(Env,World,Xs,Y) :-
	setof(X,leastInfl(Env,World,X,Y),Xs).

/***********************************************************************
 *
 * greatestInfl(+Env,+World,+-X,+-Y)
 *
 *	succeeds if Y is a greatest attribute influenced by X.
 */

greatestInfl(Env,World,X,Y) :-
	getInflDescription(Env,World,_,X,Y),
	not(X = Y),
	not(given_inflLink(Env,World,app(_,_,Y),_)).

/***********************************************************************
 *
 * greatestInfls(+Env,+World,+X,+-Ys)
 *
 *	collects the greatest attributes influenced by X in Ys.
 */

greatestInfls(Env,World,X,Ys) :-
	setof(Y,greatestInfl(Env,World,X,Y),Ys).

/***********************************************************************
 *
 * infl(+Env,+World,+-X,+-Y,+-W)
 *
 *	computes the cumulative weight W of all the influence links 
 *	between the attributes X and Y.
 */

infl(Env,World,X,Y,W) :-
	bagof(Weight,inflLink(Env,World,X,Y,Weight),Weights),
	weightOf_TotalInfl(Weights,W).

/***********************************************************************
 *
 * maxPosInfl(+Env,+World,+-X,+-Y,+-Wmax)
 *
 *	succeeds if Wmax is the greatest weight with which X influences 
 *	Y positively.
 */

maxPosInfl(Env,World,X,Y,WMax) :-
	var(X),
	bagof(W,Z^posInfl(Env,World,Z,Y,W),Ws),
	motel_max(Ws,WMax,wellDefined_InflWeight),
	posInfl(Env,World,X,Y,WMax).

maxPosInfl(Env,World,X,Y,WMax) :-
	var(Y),
	bagof(W,Z^posInfl(Env,World,X,Z,W),Ws),
	motel_max(Ws,WMax,wellDefined_InflWeight),
	posInfl(Env,World,X,Y,WMax).

maxPosInfl(Env,World,X,Y,WMax) :-
	nonvar(X),
	nonvar(Y),
	posInfl(Env,World,X,Y,WMax).

/***********************************************************************
 *
 * maxNegInfl(+Env,+World,+-X,+-Y,+-WMin)
 *
 *	succeeds if WMin is the greatest weight with which X influences 
 *	Y negatively.
 */

maxNegInfl(Env,World,X,Y,WMin) :-
	var(X),
	bagof(W,Z^negInfl(Env,World,Z,Y,W),Ws),
	motel_min(Ws,WMin,wellDefined_InflWeight),
	negInfl(Env,World,X,Y,WMin).

maxNegInfl(Env,World,X,Y,WMin) :-
	var(Y),
	bagof(W,Z^negInfl(Env,World,X,Z,W),Ws),
	motel_min(Ws,WMin,wellDefined_InflWeight),
	negInfl(Env,World,X,Y,WMin).

maxNegInfl(Env,World,X,Y,WMin) :-
	nonvar(X),
	nonvar(Y),
	negInfl(Env,World,X,Y,WMin).

/***********************************************************************
 *
 * posInfl(+Env,+World,+-X,+-Y)
 *
 *	succeeds if attribute X influences attribute Y positively.
 */

posInfl(Env,World,X,Y) :-
	infl(Env,World,X,Y,W),
	W > 0.0.

/***********************************************************************
 *
 * posInfl(+Env,+World,+-X,+-Y,+-W)
 *
 *	succeeds if attribute X influences attribute Y positively with
 *	weight W.
 */

posInfl(Env,World,X,Y,W) :-
	infl(Env,World,X,Y,W),
	W > 0.0.

/***********************************************************************
 *
 * negInfl(+Env,+World,+-X,+-Y)
 *
 *	succeeds if attribute X influences attribute Y negatively.
 */

negInfl(Env,World,X,Y) :-
	infl(Env,World,X,Y,W),
	W < 0.0.

/***********************************************************************
 *
 * negInfl(+Env,+World,+-X,+-Y,+-W)
 *
 *	succeeds if attribute X influences attribute Y negatively with
 *	weight W.
 */

negInfl(Env,World,X,Y,W) :-
	infl(Env,World,X,Y,W),
	W < 0.0.

/***********************************************************************
 *
 * noInfl(+Env,+World,+-X,+-Y)
 *
 *	succeeds if the cumulative influence between the attributes X and
 *	Y is 0.0.
 */

noInfl(Env,World,X,Y) :-
	infl(Env,World,X,Y,0.0).

/***********************************************************************
 *
 * simultInfl(+Env,+World,+-Xs,+-Y,+-W)
 *
 *	checks if the list Xs is well-defined (that is, is Xs a SET of
 *	independent attributes) and computes the total weight W of the 
 *	attributes in the list Xs simultaneously influencing attribute Y.
 */

simultInfl(Env,World,Xs,Y,W) :-
	nonvar(Xs),
	wellDefined_setOfAttributes(Env,World,Xs),
	aux_simultInfl(Env,World,Xs,Y,Ws),
	weightOf_SimultInfl(Ws,W).

simultInfl(Env,World,Xs,Y,W) :-
	var(Xs),
	!,
	leastInfl(Env,World,Xs,Y),
	aux_simultInfl(Env,World,Xs,Y,Ws),
	wellDefined_setOfAttributes(Env,World,Xs),
	weightOf_SimultInfl(Ws,W).

aux_simultInfl(_,_,[],_,[]).

aux_simultInfl(Env,World,[X|Xs],Y,[W|Ws]) :-
	infl(Env,World,X,Y,W),
	aux_simultInfl(Env,World,Xs,Y,Ws).

aux_simultInfl(Env,World,[X|Xs],Y,Ws) :-
	not(getInflDescription(Env,World,_,X,Y)),
	aux_simultInfl(Env,World,Xs,Y,Ws).

/***********************************************************************
 *
 * simultPosInfl(+Env,+World,+-Xs,+-Y)
 *
 *	succeeds if the simultaneous influence of the attributes in the
 *	list Xs on the attribute Y is positive.
 */

simultPosInfl(Env,World,Xs,Y) :-
	simultInfl(Env,World,Xs,Y,W),
	W > 0.0.

/***********************************************************************
 *
 * simultNegInfl(+Env,+World,+-Xs,+-Y)
 *
 *	succeeds if the simultaneous influence of the attributes in the
 *	list Xs on the attribute Y is positive.
 */

simultNegInfl(Env,World,Xs,Y) :-
	simultInfl(Env,World,Xs,Y,W),
	W < 0.0.

/***********************************************************************
 *
 * simultNoInfl(+Env,+World,+-Xs,+-Y)
 *
 *	succeeds if the simultaneous influence of the attributes in the
 *	list Xs on the attribute Y is positive.
 */

simultNoInfl(Env,World,Xs,Y) :-
	simultInfl(Env,World,Xs,Y,0.0).

/***********************************************************************
 *
 * change(+Env,+World,+-Y,+-Wy)
 *
 *	determines the change in Y.
 */

change(Env,World,Y,Wy) :-
	bagof(W,X^changingInfl(Env,World,X,Y,W),Ws),
	weightOf_SimultChange(Ws,Wy).

change(Env,World,Y,Wy) :-
	given_change(Env,World,Y,Wy).

/***********************************************************************
 *
 * changingInfl(+Env,+World,+-X,+-Y,+-Wy)
 *
 *	succeeds if the influencing attribute X of Y changes. Wy is the
 *	weight of the resulting change in Y.
 */

changingInfl(Env,World,X,Y,Wy) :-
	infl(Env,World,X,Y,Wxy),
	given_change(Env,World,X,Wx),
	weightOf_change(Wx,Wxy,Wy).

/***********************************************************************
 *
 * increase(+Env,+World,+-Y)
 *
 *	succeeds if attribute Y increases.
 */

increase(Env,World,Y) :-
	change(Env,World,Y,W),
	W > 0.0.
%	change(Env,World,Y,1.0).

/***********************************************************************
 *
 * decrease(+Env,+World,+-Y)
 *
 *	succeeds if attribute Y decreases.
 */

decrease(Env,World,Y) :-
	change(Env,World,Y,W),
	W < 0.0.
%	change(Env,World,Y,-1.0).

/***********************************************************************
 *
 * noChange(+Env,+World,+-Y)
 *
 *	succeeds if attribute Y does not change (i.e. there is neither
 *	an increase nor a decrease).
 */

noChange(Env,World,Y) :-
	change(Env,World,Y,0.0).

/***********************************************************************
 *
 * wellDefined_attribute(+EnvName,+World,+X)
 *
 *	Is X an attribute?
 *
 *	Note: At the moment this clause succeeds if X is an atom. We
 *	may want to do more verifying here.
 */

wellDefined_attribute(Env,World,X) :-
	atom(X),
	roleName(Env,World,X),
	!.

/***********************************************************************
 *
 * wellDefined_setOfAttributes(+Env,+World,+Xs)
 *
 *	Succeeds if Xs is a variable or if Xs is a SET of independent
 *	(with respect to the influence relationship) atoms.
 *
 *	Note: In the current implementation the independence is NOT 
 *	verified.
 */

wellDefined_setOfAttributes(Env,World,Xs) :-
	isSet(Xs),
	noInflLinks(Env,World,Xs).

/***********************************************************************
 *
 * isSet(+L)
 *
 *	Succeeds if L is a SET.
 */

isSet([]) :-
	!.
isSet([E|L]) :-
	not(member(E,L)),
	isSet(L).


/***********************************************************************
 *
 * noInflLinks(+Env,+World,+Xs)
 *
 *	Succeeds if there are no links among any of the attributes in Xs.
 *
 *	NOTE: The complexity is quadratic, namely (n-1)^2, 
 *	where n is the length of Xs.
 */

noInflLinks(Env,World,[X|Xs]) :-
	wellDefined_attribute(Env,World,X),
	aux_noInflLinks(Env,World,X,Xs),
	noInflLinks(Env,World,Xs).

noInflLinks(_,_,[]).

aux_noInflLinks(Env,World,X,[Y|Ys]) :-
	not(getInflDescription(Env,World,_,X,Y)),
	not(getInflDescription(Env,World,_,Y,X)),
	aux_noInflLinks(Env,World,X,Ys).

aux_noInflLinks(_,_,_,[]).

/***********************************************************************
 *
 * wellDefined_SimultChanges(+Changes)
 *
 *	Succeeds if Changes is a list of changes of the form
 *	change(X,W) in which no X occurs more than once.
 */

wellDefined_SimultChanges(Changes) :-
	attributes(Changes,Xs),
	isSet(Xs),
	!.

/***********************************************************************
 *
 * attributes(+Changes,+-Xs)
 *
 *	Generates a list Xs of attributes X from the list Changes.
 *	The elements of Changes are of the form change(X,W).
 */

attributes([],[]).

attributes([change(X,_)|Changes],[X|Xs]) :-
	attributes(Changes,Xs).

/***********************************************************************
 *
 * wellDefined_InflWeight(+W)
 *
 *	Is the given weight W of influence well-defined?
 */

wellDefined_InflWeight(W) :-
	float(W),
	!.
wellDefined_InflWeight(W) :-
	integer(W),
	!.
%	W >= -1.0,
%	W =< 1.0.

/***********************************************************************
 *
 * weightOf_ChainedInfl(+-W1,+-W2,+-W)
 *
 *	defines the weight W of chained influences with weights W1 and
 *	W2.
 *	W is given by f(W1,W2) where in this implementation f is
 *	multiplication.
 */

weightOf_ChainedInfl(W1,W2,W) :-
	product(W1,W2,W,wellDefined_ChangeWeight).

/***********************************************************************
 *
 * weightOf_TotalInfl(+Ws,+-W)
 *
 *	computes the the total weight W from the Ws. 
 *	Here, W is the sum of the Ws.
 *	We could have just as well chosen W to be the arithmetic
 *	mean of the Ws.
 *	Which is better remains open for the moment.
 */

weightOf_TotalInfl(Ws,W) :-
	sum(Ws,W,wellDefined_InflWeight).
%	arithm_Mean(Ws,W,wellDefined_InflWeight).

/***********************************************************************
 *
 * weightOf_SimultInfl(+-Ws,+-W)
 *
 *	computes the weight W of a list of simultaneous influences from
 *	different attributes with weights specified in Ws.
 *	Here, W is the sum of the Ws.
 *	We could have just as well chosen W to be the arithmetic
 *	mean of the Ws.
 *	Which is better remains open for the moment.
 */

weightOf_SimultInfl(Ws,W) :-
	sum(Ws,W,wellDefined_InflWeight).
%	arithm_Mean(Ws,W,wellDefined_InflWeight).

/***********************************************************************
 *
 * wellDefined_ChangeWeight(+W)
 *
 *	Is the given weight W of change well-defined?
 */

wellDefined_ChangeWeight(W) :-
	float(W),
	!.
wellDefined_ChangeWeight(W) :-
	integer(W),
	!.
%	W >= -1.0,
%	W =< 1.0.

/***********************************************************************
 *
 * weightOf_change(+-Wx,+-Wxy,-+Wy)
 *
 *	succeeds if Wy = f(Wx,Wxy) for a given f. 
 *	I chose f to be multiplication.
 */

weightOf_change(Wx,Wxy,Wy) :-
	product(Wx,Wxy,Wy,wellDefined_ChangeWeight).

/***********************************************************************
 *
 * weightOf_SimultChange(+Ws,+-W)
 *
 *	computes the weight W of the change resulting from
 *	simultaneous changes with weights Ws.
 *	W is the sum over the Ws.
 */

weightOf_SimultChange(Ws,W) :-
	sum(Ws,W,wellDefined_ChangeWeight).

/***********************************************************************
 *
 * arithm_Mean([+-Value|+Values],+-Mean,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name for
 *	checking whether each of the values is well-defined this clause
 *	computes the arithmetical mean (Mean) over Values.
 *	Provided Mean is given the first value may be a variable.
 */

arithm_Mean([],0.0,IsWellDefName) :-
	!.

arithm_Mean([Value|Values],Mean,IsWellDefName) :-
	var(Value),
	!,
	length([Value|Values],N),
	Sum is Mean * N,
	sum([Value|Values],Sum,IsWellDefName).

arithm_Mean(Values,Mean,IsWellDefName) :-
	sum(Values,Sum,IsWellDefName),
	length(Values,N),
	Mean is Sum / N.

/***********************************************************************
 *
 * sum([+-Value|+Values],+-Sum,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause computes the sum (Sum) of the values.
 *	Provided Sum is given the first value may be a variable.
 */

sum([Value|Values],Sum,IsWellDefName) :-
	var(Value),
	!,
	IsWellDef =.. [IsWellDefName,Sum],
	IsWellDef,
	sum(Values,VSum,IsWellDefName),
	Value is Sum - VSum.

sum([Value|Values],Sum,IsWellDefName) :-
	IsWellDef =.. [IsWellDefName,Value],
	IsWellDef,
	sum(Values,VSum,IsWellDefName),
	Sum is Value + VSum.

sum([],0.0,_).

/***********************************************************************
 *
 * product(+Factor1,+Factor2,+-Product,+IsWellDefName)
 * product(+-Factor1,+Factor2,+Product,+IsWellDefName)
 * product(+Factor1,+-Factor2,+Product,+IsWellDefName)
 *
 *	Given two values (Factor1 and Factor2) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause computes the product (Product) of the 
*	values.
 */

product(Factor1,Factor2,Product,IsWellDefName) :-
	IsWellDef1 =.. [IsWellDefName,Factor1],
	IsWellDef1,
	IsWellDef2 =.. [IsWellDefName,Factor2],
	IsWellDef2,
	Product is Factor1 * Factor2,
	!.

product(Factor1,Factor2,Product,IsWellDefName) :-
	IsWellDef1 =.. [IsWellDefName,Factor2],
	IsWellDef1,
	IsWellDef2 =.. [IsWellDefName,Product],
	IsWellDef2,
	Factor1 is Product / Factor2,
	!.

product(Factor1,Factor2,Product,IsWellDefName) :-
	IsWellDef1 =.. [IsWellDefName,Factor1],
	IsWellDef1,
	IsWellDef2 =.. [IsWellDefName,Product],
	IsWellDef2,
	Factor2 is Product / Factor1,
	!.

/***********************************************************************
 *
 * max([+-Value|+Values],+-Max,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause determines the maximum (Max) of the 
 *	values.
 *
 * max(+Value1,+Value2,+-Max)
 *
 *	returns the bigger value of Value1 and Value2 in Max.
 */

motel_max([Max],Max,_) :-
	!.

motel_max([Value|Values],Max,IsWellDefName) :-
	IsWellDef =.. [IsWellDefName,Value],
	IsWellDef,
	motel_max(Values,VMax,IsWellDefName),
	lub(Value,VMax,Max).

lub(Value1,Value2,Value1) :-
	Value1 > Value2,
	!.

lub(Value1,Value2,Value2).

/***********************************************************************
 *
 * min([+-Value|+Values],+-Min,+IsWellDefName)
 *
 *	Given a list of values (Values) and a predicate name 
 *	(IsWellDefName) for checking whether each of the values is 
 *	well-defined this clause determines the minimum (Min) of the 
 *	values.
 *
 * min(+Value1,+Value2,+-Min)
 *
 *	returns the smaller value of Value1 and Value2 in Min.
 */

motel_min([Min],Min,_) :-
	!.

motel_min([Value|Values],Min,IsWellDefName) :-
	IsWellDef =.. [IsWellDefName,Value],
	IsWellDef,
	motel_min(Values,VMin,IsWellDefName),
	glb(Value,VMin,Min).

glb(Value1,Value2,Value1) :-
	Value1 < Value2,
	!.

glb(Value1,Value2,Value2).

/**********************************************************************
 *
 * %A%
 *
 */

/***********************************************************************
 *
 * defprimconcept(+Environment,+Left)
 * Parameter: ConceptName       concept name
 * defines the concept ConceptName in modal context [].
 *
 */

defprimconcept(Left) :-
	getCurrentEnvironment(EnvName),
	defprimconcept(EnvName,[],Left).


/***********************************************************************
 *
 * defprimconcept(+Environment,+Left)
 * Parameter: ConceptName       concept name
 * defines the concept ConceptName in modal context [].
 *
 */

defprimconcept(EnvName,Left) :-
	environment(EnvName,_,_),
	!,
	defprimconcept(EnvName,[],Left).
defprimconcept(MS,Left) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	defprimconcept(EnvName,MS,Left).
defprimconcept(Left,Right) :-
	getCurrentEnvironment(EnvName),
	defprimconcept(EnvName,[],Left,Right).
	

/***********************************************************************
 *
 * defprimconcept(+Environment,+MS,+Left)
 * Parameter: ModalSequence     modal context
 *            ConceptName       concept name
 * defines the concept ConceptName in modal context ModalSequence.
 *
 */

defprimconcept(EnvName,MS,Left) :-
	environment(EnvName,Env,_),
	(MS = [] ; MS = [_|_]),
	atomic(Left),
	assertz(conceptSubsets(Env,user,MS,Left,'top',noAxiom)),
	assertz(axiom(Env,MS,defprimconcept(MS,Left,'top'))),
	assertNames(Env,MS,Left,concept),
	!.

defprimconcept(MS,Left,Right) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	defprimconcept(EnvName,MS,Left,Right).

/***********************************************************************
 *
 * defprimconcept(+Environment,+Left,+Right)
 * Parameter: ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be a subset of the concept
 * ConceptTerm in modal context [].
 *
 */

defprimconcept(EnvName,Left,Right) :-
	environment(EnvName,_,_),
	defprimconcept(EnvName,[],Left,Right).

/***********************************************************************
 *
 * defprimconcept(+Environment,+ModalSequence,+Left,+Right)
 * Parameter: ModalSequence     modal context
 *            ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be a subset of the concept
 * ConceptTerm in modal context ModalSequence.
 *
 */

defprimconcept(EnvName,MS,L,R) :-
	environment(EnvName,Env,_),
%	nonvar(MS),
	cnf(L,Left),
	cnf(R,Right),
	assertNames(Env,MS,Left,concept),
	assertNames(Env,MS,Right,concept),
	assertz(axiom(Env,MS,defprimconcept(MS,L,R))),
	unfold(Env,[(user,concept,Left,Right)],[(_Origin,_,L1,_,R1)|DL]),
	gensym(axiom,AxiomName1),
	assertz(conceptSubsets(Env,user,MS,Left,Right,AxiomName1)),
	typeOfDefinition(Env,MS,L1,Origin),
	assertConceptLInR(Env,rn(AxiomName1,Origin,lInR),MS,L1,R1),
	defList(Env,MS,DL,_),
	negate(R1,NotRight1),
	cnf(NotRight1,NotRight),
	negate(L1,NotLeft1),
	cnf(NotLeft1,NotLeft),
	notClauseL(Env,MS,NotRight,NotLeft).


notClauseL(Env,MS,Left,Right) :-
	% assertz that Left is included in Right
	unfold(Env,[(user,concept,Left,Right)],[(_O,_,Concept1,C3,Concept2)|DL2]),
	defPositiveList(Env,MS,DL2),
	gensym(axiom,AxiomName2),
	typeOfDefinition(Env,MS,Concept1,O),
	assertz(conceptSubsets(Env,user,MS,Concept1,C3,AxiomName2)),
	assertConceptLInR(Env,rn(AxiomName2,O,lInR),MS,Concept1,Concept2).
/* 
notClauseL(Env,MS,Left,Right) :-
	% assertz that Left is included in Right
	atomic(Left),
	!,
	unfold(Env,[(user,concept,Left,Right)],[(_O,_,Concept1,C3,Concept2)|DL2]),
	defPositiveList(Env,MS,DL2),
	gensym(axiom,AxiomName2),
	typeOfDefinition(Env,MS,Concept1,O),
	assertConceptLInR(Env,rn(AxiomName2,O,lInR),MS,Concept1,Concept2),
	assertz(conceptSubsets(Env,user,MS,Concept1,C3,AxiomName2)).
notClauseL(Env,MS,Left,Right) :-
	atomic(Right),
	!,
	gensym(concept,Concept1),
	unfold(Env,[(system,concept,Concept1,Left)],DL2),
	defPositiveList(Env,MS,DL2),
	gensym(axiom,AxiomName2),
	assertConceptLInR(Env,rn(AxiomName2,system,lInR)MS,Concept1,Right),
	assertz(conceptSubsets(Env,system,MS,Concept1,Concept2,AxiomName2)).
notClauseL(Env,MS,Left,Right) :-
	!,
	gensym(concept,Concept1),
	gensym(concept,Concept2),
	unfold(Env,[(system,concept,Concept1,Left),(system,concept,Concept2,Right)],DL2),
	defPositiveList(Env,MS,DL2),
	gensym(axiom,AxiomName2),
	assertConceptLInR(Env,rn(AxiomName,system,lInR),MS,Concept1,Concept2),
	assertz(conceptSubsets(Env,system,MS,Concept1,Concept2,AxiomName2)).
*/

notClausesLR(Env,MS,Left,Right,DL2) :-
	unfold(Env,[(system,concept,Left,Right)],DL2),
	defPositiveList(Env,MS,DL2).

/***********************************************************************
 *
 * defconcept(+ConceptName,+ConceptTerm)
 * Parameter: ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be equivalent to the concept
 * ConceptTerm in modal context [].
 *
 */ 

defconcept(ConceptName,ConceptTerm) :-
	getCurrentEnvironment(EnvName),
	defconcept(EnvName,[],ConceptName,ConceptTerm).

defconcept(MS,CN,CT) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	defconcept(EnvName,MS,CN,CT),
	!.
defconcept(EnvName,ConceptName,ConceptTerm) :-
	nonvar(EnvName),
	environment(EnvName,_,_),
	!,
	defconcept(EnvName,[],ConceptName,ConceptTerm).

/***********************************************************************
 *
 * defconcept(+ModalSequence,+ConceptName,+ConceptTerm)
 * Parameter: ModalSequence     modal context
 *            ConceptName       concept name
 *            ConceptTerm       concept term
 * defines the concept ConceptName to be equivalent to the concept
 * ConceptTerm in modal context ModalSequence.
 *
 */ 

defconcept(EnvName,MS,CT1,CT2) :-
	nonvar(EnvName),
	environment(EnvName,Env,_),
	cnf(CT1,ConceptTerm1),
	cnf(CT2,ConceptTerm2),
	assertNames(Env,MS,ConceptTerm1,concept),
	assertNames(Env,MS,ConceptTerm2,concept),
	assertz(axiom(Env,MS,defconcept(MS,CT1,CT2))),
	unfold(Env,[(user,concept,ConceptTerm1,ConceptTerm2)],DL),
	defList(Env,MS,DL,_).

defPositiveList(_,_,[]) :- !.
defPositiveList(Env,MS,[(Origin,concept,ConceptName,CTO,ConceptTerm)|DL]) :-
	gensym(axiom,AxiomName),
	assertz(conceptEqualSets(Env,Origin,MS,ConceptName,CTO,AxiomName)),
	assertConceptRInL(Env,rn(AxiomName,Origin,rInL),MS,ConceptName,ConceptTerm),
	assertConceptLInR(Env,rn(AxiomName,Origin,lInR),MS,ConceptName,ConceptTerm),
	defPositiveList(Env,MS,DL).
defPositiveList(Env,MS,[(_Origin,role,RN,_RTO,RT)|RDL]) :-
	gensym(axiom,AxiomName),
	assertRoleLInR(Env,MS,RN,RT,AxiomName),
	assertRoleRInL(Env,MS,RN,RT,AxiomName),
	defPositiveList(Env,MS,RDL).

defList(_,_,[],[]) :- !.
defList(Env,MS,[(Origin,concept,ConceptName,CTO,ConceptTerm)|DL],
        NeededDL3) :-
	gensym(axiom,AxiomName),
	assertz(conceptEqualSets(Env,Origin,MS,ConceptName,CTO,AxiomName)),
	assertConceptRInL(Env,rn(AxiomName,Origin,rInL),MS,ConceptName,ConceptTerm),
	assertConceptLInR(Env,rn(AxiomName,Origin,lInR),MS,ConceptName,ConceptTerm),
	negate(ConceptTerm,NotRight1),
	cnf(NotRight1,NotRight),
	negate(ConceptName,NotLeft1),
	cnf(NotLeft1,NotLeft),
	notClausesLR(Env,MS,NotRight,NotLeft,NeededDL1),
	defList(Env,MS,DL,NeededDL2),
	append(NeededDL1,NeededDL2,NeededDL3).
defList(Env,MS,[(Origin,role,RN,RTO,RT)|RDL],NeededDL) :-
	gensym(axiom,AxiomName),
	assertz(roleEqualSets(Env,Origin,MS,RN,RTO,AxiomName)),
	assertRoleLInR(Env,MS,RN,RT,AxiomName),
	assertRoleRInL(Env,MS,RN,RT,AxiomName),
	defList(Env,MS,RDL,NeededDL).

undefList(_,_,[]) :- !.
undefList(EnvName,MS,[(Origin,concept,ConceptName,CTO,ConceptTerm)|DL]) :-
	undefconcept(EnvName,MS,ConceptName,CTO),
	undefList(Env,MS,DL).
undefList(EnvName,MS,[(Origin,role,RN,RTO,RT)|RDL]) :-
	undefrole(EnvName,MS,RN,RTO),
	undefList(Env,MS,RDL).

/***********************************************************************
 *
 * assert_ind(+ModalSequence,+ABoxElement,+ConceptTerm)
 * Parameter: ModalSequence     modal context
 *            ABoxElement       name of ABox element
 *            ConceptTerm       concept term
 * adds ABoxElement to Concept in modal context ModalSequence.
 *
 */

assert_ind(X,CT) :-
	getCurrentEnvironment(EnvName),
	assert_ind(EnvName,[],X,CT).

assert_ind(EnvName,X,CT) :-
	environment(EnvName,_,_),
	!,
	assert_ind(EnvName,[],X,CT).
assert_ind(MS,X,CT) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	assert_ind(EnvName,MS,X,CT).

assert_ind(X,Y,R) :-
	getCurrentEnvironment(EnvName),
	assert_ind(EnvName,X,Y,R).

assert_ind(EnvName,MS,X,C) :-
	environment(EnvName,Env,_),
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	!,
	atomic(X),
	gensym(axiom,AxiomName),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,user,lInR,RN1),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	asserta((conceptElement(Env,MS,W1,user,X,C,AxiomName) :- call(G1))),
	assertz(axiom(Env,MS,assert_ind(MS,X,C))),
	constructMLHead(Env,RN1,W1,C,X,_HYPS,_D,_CALLS,abox,InHead),
	asserta((InHead :- call(G1))),
	assertNames(Env,MS,C,concept).

/***********************************************************************
 *
 * assert_ind(+ModalSequence,+ABoxElement1,+ABoxElement2,+Role)
 * Parameter: ModalSequence     modal context
 *            ABoxElement1      name of ABox element
 *            ABoxElement2      name of ABox element
 *            Role              role name
 * adds the pair (ABoxElement1,ABoxElement2) to Role in modal context
 * ModalSequence.
 *
 */

assert_ind(EnvName,X,Y,R) :-
	environment(EnvName,_,_),
	!,
	atomic(X),
	atomic(Y),
	!,
	assert_ind(EnvName,[],X,Y,R).
assert_ind(MS,X,Y,R) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	!,
	atomic(X),
	atomic(Y),
	!,
	assert_ind(EnvName,MS,X,Y,R).

assert_ind(EnvName,MS,X,Y,R) :-
	environment(EnvName,Env,_),
	atomic(X),
	atomic(Y),
	atomic(R),
	Role1 =.. [R,X,Y],
	asserta(Role1),
%	Role2 =.. [R,X,Y],
	gensymbol(skolem,[X,Y],SF),
	gensym(axiom,AX),
	gensym(rule,RN),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	constructEqHead(Env,rn(AX,RN,user,lInR),W1,Y,SF,R,X,_,_D,CALLS,abox,EqLiteral),
	asserta((EqLiteral :- (cCS(CALLS,true), call(G1)))),
	assertNames(Env,MS,R,role),
	assertz(axiom(Env,MS,assert_ind(MS,X,Y,R))),
	asserta((roleElement(Env,MS,W1,user,X,Y,R,AX) :- call(G1))).


/***********************************************************************
 *
 * defprimrole(+RN)
 * Parameter: RN        role name
 * defines the role RN in modal context [].
 * 
 */

defprimrole(Role) :-
	currentEnvironment(Env),
	!,
	assertNames(Env,[],Role,role),
	asserta(roleSubsets(Env,user,[],Role,'top',noAxiom)).

defprimrole(EnvName,Role) :-
	environment(EnvName,Env,_),
	!,
	assertNames(Env,[],Role,role),
	asserta(axiom(Env,[],defprimrole([],Role,'top'))),
	asserta(roleSubsets(Env,user,[],Role,'top',noAxiom)).

/***********************************************************************
 *
 * defprimrole(+MS,+RN)
 * Parameter: MS        modal context
 *            RN        role name
 * defines the role RN in modal context MS.
 * 
 */

defprimrole(MS,Role) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	!,
	currentEnvironment(Env),
	assertNames(Env,MS,Role,role),
	asserta(axiom(Env,MS,defprimrole(MS,Role,'top'))),
	asserta(roleSubsets(Env,user,MS,Role,'top',noAxiom)).

defprimrole(R1,R2) :-
	getCurrentEnvironment(EnvName),
	defprimrole(EnvName,[],R1,R2).

/***********************************************************************
 *
 * defprimrole(+RN,+Role)
 * Parameter: RN        role name
 *            Role      role term
 * defines the role RN to be a subset of the role Role in modal
 * context [].
 * 
 */

defprimrole(EnvName,RN,Role) :-
	environment(EnvName,_,_),
	atomic(RN),
	!,
	defprimrole(EnvName,[],RN,Role).
defprimrole(MS,RN,Role) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	atomic(RN),
	!,
	defprimrole(EnvName,MS,RN,Role).

/***********************************************************************
 *
 * defprimrole(+MS,+RN,+Role)
 * Parameter: MS        modal context
 *            RN        role name
 *            Role      role term
 * defines the role RN to be a subset of the role Role in modal
 * context MS.
 *
 */

defprimrole(EnvName,MS,RN,Role) :-
	environment(EnvName,Env,_),
	atomic(RN),
	assertNames(Env,MS,RN,role),
	assertNames(Env,MS,Role,role),
	unfold(Env,[(user,role,RN,Role)],[(user,role,RN,_,RT)|RDL]),
	gensym(axiom,AxiomName),
	asserta(axiom(Env,MS,defprimrole(MS,RN,Role))),
	asserta(roleSubsets(Env,user,MS,RN,Role,AxiomName)),
	assertRoleLInR(Env,MS,RN,RT,AxiomName),
	defList(Env,MS,RDL,_).

/***********************************************************************
 *
 * defrole(+RN,+Role)
 * Parameter: RN        role name
 *            Role      role term
 * defines role RN to be equivalent to the role Role in modal context
 * [].
 *
 */

defrole(RN,Role) :-
	getCurrentEnvironment(EnvName),
	defrole(EnvName,[],RN,Role).

defrole(MS,RN,Role) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	defrole(EnvName,[],RN,Role).
defrole(EnvName,RN,Role) :-
	nonvar(EnvName),
	environment(EnvName,_,_),
	defrole(EnvName,[],RN,Role).

/***********************************************************************
 *
 * defrole(+MS,+RN,+Role)
 * Parameter: MS        modal context
 *            RN        role name
 *            Role      role term
 * defines the role RN to be equivalent to the role Role in modal
 * context MS.
 *
 */

defrole(EnvName,MS,RN,Role) :-
	environment(EnvName,Env,_),
	atomic(RN),
	unfold(Env,[(user,role,RN,Role)],RDL),
	assertNames(Env,MS,RN,role),
	assertNames(Env,MS,Role,role),
	asserta(axiom(Env,MS,defrole(MS,RN,Role))),
	defList(Env,MS,RDL,_).

/**********************************************************************
 *
 * defdisjoint(EnvName,MS,ConceptList)
 * Parameter: EnvName         environment name
 *            MS              modal context
 *            ConceptList     list of concept names
 *
 */

defdisjoint(CL) :-
	getCurrentEnvironment(EnvName),
	defdisjoint(EnvName,[],CL),
	!.

defdisjoint(EnvName,CL) :-
	environment(EnvName,_,_),
	defdisjoint(EnvName,[],CL),
	!.
defdisjoint(MS,CL) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	defdisjoint(EnvName,MS,CL),
	!.

defdisjoint(_EnvName,_MS,[]) :-
	!.
defdisjoint(EnvName,MS,[C1|CL]) :-
	defdisjoint(EnvName,MS,C1,CL),
	defdisjoint(EnvName,MS,CL),
	!.

defdisjoint(_EnvName,_MS,_C1,[]) :-
	!.
defdisjoint(EnvName,MS,C1,[C2|CL]) :-
	defprimconcept(EnvName,MS,C1,not(C2)),
	defdisjoint(EnvName,MS,C1,CL).


/**********************************************************************
 *
 * defclosed(EnvName,MS,X,Y,R)
 *
 */

defclosed(X,Y,R) :-
	getCurrentEnvironment(EnvName),
	defclosed(EnvName,[],X,Y,R),
	!.

defclosed(EnvName,X,Y,R) :-
	environment(EnvName,_,_),
	defclosed(EnvName,[],X,Y,R),
	!.
defclosed(MS,X,Y,R) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	defclosed(EnvName,MS,X,Y,R),
	!.

defclosed(EnvName,MS,X,Y,R) :-
	environment(EnvName,Env,_),
	assertz(closed(Env,MS,X,Y,R)),
	!.

/***********************************************************************
 *
 * assertNames(+Type,+MS,+CT)
 * Arguments: Type   'concept' or 'role'
 *            MS     modal context
 *            T      concept or role term
 * asserts for each concept name CN in T a fact
 *            conceptName(CN)
 * and for each role name RN in T a fact
 *            roleName(RN)
 * These facts are used to distinguish concept and role names introduced 
 * by the user from those introduced by the system.
 *
 */

assertNames(Env,MS,CT,Type) :-
	namesInTerm(CT,CNL1,Type),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	hop_map(assertName,[Env,MS,W1,G1],CNL1,_).

assertName((concept,CN1),alreadyAsserted,Env,MS,W1,G1) :-
% If the concept name is already asserted with identical modal sequence, 
% then we do nothing.
	clause(conceptName(Env,MS,_,CN1),_),
	!.
assertName((role,CN1),alreadyAsserted,Env,MS,W1,G1) :-
% If the role name is already asserted with identical modal sequence, 
% then we do nothing.
	clause(roleName(Env,MS,_,CN1),_),
	!.
assertName((concept,CN1),newAsserted,Env,MS,W1,G1) :-
% Otherwise we assert the concept name
% Remember: The fact that the concept name is not already asserted with
% identical modal sequence does not mean that we are not already able to 
% deduce that the concept name is present in the modal context corresponding
% to the modal sequence.
	assertz((conceptName(Env,MS,W1,CN1) :- G1)),
	!.
assertName((role,CN1),newAsserted,Env,MS,W1,G1) :-
% Otherwise we assert the role name
% Remember: The fact that the role name is not already asserted with
% identical modal sequence does not mean that we are not already able to 
% deduce that the role name is present in the modal context corresponding
% to the modal sequence.
	assertz((roleName(Env,MS,W1,CN1) :- G1)),
	!.
	
/***********************************************************************
 *
 * namesInTerm(+T1,-TL+Type) 
 * Arguments: T1     concept or role term
 *            TL     list of pairs (Type1,Name)
 *            Type   'concept' or 'role'
 * TL is the list of all concept and role names in T1.
 *
 */

namesInTerm(and(CTL),CNL,Type) :-
	hop_map(namesInTerm,[Type],CTL,CNLL),
	union(CNLL,CNL),
	!.
namesInTerm(or(CTL),CNL,Type) :-
	hop_map(namesInTerm,[Type],CTL,CNLL),
	union(CNLL,CNL),
	!.
namesInTerm(some(R,C),L,_) :-
	namesInTerm(R,L1,role),
	namesInTerm(C,L2,concept),
	append(L1,L2,L).
namesInTerm(all(R,C),L,_) :-
	namesInTerm(C,L1,concept),
	namesInTerm(R,L2,role),
	append(L1,L2,L).
namesInTerm(restr(R,C),L,_) :-
	namesInTerm(C,L1,concept),
	namesInTerm(R,L2,role),
	append(L1,L2,L).
namesInTerm(atleast(_N,R),L,_) :-
	namesInTerm(R,L,role).
namesInTerm(atmost(_N,R),L,_) :-
	namesInTerm(R,L,role).
namesInTerm(inverse(R),L,Type) :-
	namesInTerm(R,L,Type).
namesInTerm(not(C),L,Type) :-
	namesInTerm(C,L,Type).
namesInTerm(naf(C),L,Type) :-
	namesInTerm(C,L,Type).
namesInTerm(b(_O,_P,C),L,Type) :-
	namesInTerm(C,L,Type).
namesInTerm(d(_O,_P,C),L,Type) :-
	namesInTerm(C,L,Type).
namesInTerm(bc(_O,D,C),L,Type) :-
	namesInTerm(D,L1,Type),
	namesInTerm(C,L2,Type),
	append(L1,L2,L).
namesInTerm(dc(_O,D,C),L,Type) :-
	namesInTerm(D,L1,Type),
	namesInTerm(C,L2,Type),
	append(L1,L2,L).
namesInTerm(set(_L),[],_Type) :-
	!.
namesInTerm(L,[(Type,L)],Type) :-
	atomic(L),
	!.



/**********************************************************************
 *
 * %A%
 *
 */
%------------------------------------------------------------------------------
% Project:      MOTEL 1.0
% Module:       malcToFOL.pl
% Purpose:      Translation of modal KL-ONE terms to first-order logic 
%               formulae to clauses
% Last Change:  27.03.93
% Language:     Prolog
% Author:       Ullrich Hustadt
% Address:      Max-Planck-Institut for Computer Science
%               Im Stadtwald
%               6600 Saarbr"ucken
%               Germany
% Email:        Ullrich.Hustadt@mpi-sb.mpg.de
% Copyright:    (C) 1993 Ullrich Hustadt
% Copying:      This software is provided under the GNU General Public License.
% Warranty:     This is a research prototype. There is absolutely no warranty.
%------------------------------------------------------------------------------

axiomToClause(MC,VL,T1,Op,T2,C) :-
	axiomToFOL(MC,VL,T1,Op,T2,F),
	forallQuantify(VL,F,F1),
	translate(F1,C1),
	clausesToLOP(C1,C).

forallQuantify([],F,F) :-
	!.
forallQuantify([X|VL],F1,forall(X,F2)) :-
	forallQuantify(VL,F1,F2).

%----------------------------------------------------------------------
% axiomToFOL(+MODALCONTEXT,+VARLIST,+TERM1,+OPERATOR,+TERM2,-FORMULA)
% If OPERATOR is 'equivalent', then FORMULA is the translation of the 
% equivalence of TERM1 and TERM2 in MODALCONTEXT.
% If OPERATOR is 'implies', then FORMULA is the translation of the
% implication of TERM2 by TERM1 in MODALCONTEXT.

axiomToFOL(MC,VL,_,in,C,F) :-
	!,
	malcToFOL(functional,U,VL,C,F1),
	modalContextToFOL(MC,[],U,F1,F).
axiomToFOL(MC,VL,C1,Op,C2,F) :-
	malcToFOL(functional,U,VL,C1,F1),
	malcToFOL(functional,U,VL,C2,F2),
	F3 =.. [Op,F1,F2],
	modalContextToFOL(MC,[],U,F3,F).

modalContextToFOL([],V,V,F,F) :-
	!.
modalContextToFOL([b(O,A)|MC],U1,V,F3,
	          forall(U2,implies(rel(X1,X2,U1,U2),F4))) :-
	convertMS(e1,[U1,true],[b(O,A)],[],[U2,(rel(e1,X1,X2,U1,U2), true)],_),
	modalContextToFOL(MC,U2,V,F3,F4).
modalContextToFOL([d(O,A)|MC],U1,V,F3,F4) :-
	convertMS(e1,[U1,true],[d(O,A)],[],
                  [app(W1 : m(O,A), U1),true],_),
	modalContextToFOL(MC,app(typed(W1,m(O,A)),U1),V,F3,F4).
modalContextToFOL([bc(O,C)|MC],U1,V,F3,
	          forall(A,forall(U2,implies(and([F1,rel(X1,m(O,A),U1,U2)]),F4)))) :-
	convertMS(e1,[U1,true],[bc(O,C)],[],
	          [U2,((once(_G),rel(e1,X1,m(O,A),U1,U2)),true)],_),
	malcToFOL(functional,U1,[A],C,F1),
	modalContextToFOL(MC,U2,V,F3,F4).
modalContextToFOL([dc(O,C)|MC],U1,V,F3,
	          forall(V,and([F1,F4]))) :-
	convertMS(e1,[U1,true],[dc(O,C)],[],
	          [app(W1 : m(O,A), U1),_G],_),
	malcToFOL(functional,U1,[A],C,F1),
	modalContextToFOL(MC,[app(typed(W1,m(O,A)), U1)],V,F3,F4).


    
%----------------------------------------------------------------------
% malcToFOL(functional,+WORLD,+VARLIST,+TERM,-FORMULA)
% translates TERM in WORLD for VARLIST into the first-order logic 
% formula FORMULA.
%
% Author: Ullrich Hustadt

malcToFOL(Trans,U,[X],and([C1]),F1) :-
	malcToFOL(Trans,U,[X],C1,F1).
malcToFOL(Trans,U,[X],and([C1|CL]),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X],C1,F1),
	malcToFOL(Trans,U,[X],and(CL),F2).
malcToFOL(Trans,U,[X],or([C1]),F1) :-
	malcToFOL(Trans,U,[X],C1,F1).
malcToFOL(Trans,U,[X],or([C1|CL]),
	   or([F1,F2])) :-
	malcToFOL(Trans,U,[X],C1,F1),
	malcToFOL(Trans,U,[X],or(CL),F2).
malcToFOL(Trans,U,[X],not(C),not(F)) :-
	malcToFOL(Trans,U,[X],C,F),
	!.
malcToFOL(Trans,U,[X],naf(C),F) :-
	malcToFOL(Trans,U,[X],C,F),
	!.
malcToFOL(Trans,U,[X],all(R,C),
	   forall(Y,implies(F1,F2))) :-
	malcToFOL(Trans,U,[X,Y],R,F1),
	malcToFOL(Trans,U,[Y],C,F2).
malcToFOL(relational,U,[X],some(R,C),
	   exists(Y,and([F1,F2]))) :-
	malcToFOL(relational,U,[X,Y],R,F1),
	malcToFOL(relational,U,[Y],C,F2).
malcToFOL(functional,U,[X],some(R,C),
	   exists(F,F2)) :-
	malcToFOL(functional,U,[app(fun(F,R),X)],C,F2).
malcToFOL(Trans,U,[X],atleast(N,R),F) :-
	nrToFOL(U,[X],atleast(N,R),F).
malcToFOL(Trans,U,[X],atmost(N,R),F) :-
	nrToFOL(U,[X],atmost(N,R),F).
malcToFOL(Trans,U,[X],b(O,A,C2),
           forall(V,implies(rel(O,A,U,V),F))) :-
	malcToFOL(Trans,V,[X],C2,F).    
malcToFOL(Trans,U,[X],d(O,A,C2),
	   exists(V,and([rel(O,A,U,V),F]))) :-
	malcToFOL(Trans,V,[X],C2,F).
malcToFOL(Trans,U,[X],bc(O,C1,C2),
	   forall(A,forall(V,implies(and([F1,rel(O,A,U,V)]),F2)))) :-
        malcToFOL(Trans,U,[A],C1,F1),
	malcToFOL(Trans,V,[X],C2,F2).
malcToFOL(Trans,U,[X],dc(O,C1,C2),
	   forall(A,exists(V,and([and([F1,rel(O,A,U,V)]),F2])))) :-
	malcToFOL(Trans,U,[A],C1,F1),
	malcToFOL(Trans,V,[X],C2,F2).
malcToFOL(Trans,U,[X],A,F) :-
	atomic(A),
	F =.. [in,U,A,X].
malcToFOL(Trans,U,[X,Y],inverse(R),F) :-
	malcToFOL(Trans,U,[Y,X],R,F).
malcToFOL(Trans,U,[X,Y],and([R1]),F) :-
	!,
	malcToFOL(Trans,U,[X,Y],R1,F).
malcToFOL(Trans,U,[X,Y],and([R1|RL]),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X,Y],R1,F1),
	malcToFOL(Trans,U,[X,Y],and(RL),F2).
malcToFOL(Trans,U,[X,Y],restrict(R,C),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X,Y],R,F1),
	malcToFOL(Trans,U,[Y],C,F2).
malcToFOL(Trans,U,[X,Y],restr(R,C),
	   and([F1,F2])) :-
	malcToFOL(Trans,U,[X,Y],R,F1),
	malcToFOL(Trans,U,[Y],C,F2).
malcToFOL(relational,U,[X,Y],P,F) :-
	atomic(P),
	F =.. [in,U,P,pair(X,Y)].
malcToFOL(functional,U,[X,Y],P,equal(Y,app(fun(F,P),X))) :-
	atomic(P),
	atomic(X),
	gensym(f,F),
	!.
malcToFOL(functional,U,[X,Y],P,equal(Y,app(fun(F,P),X))) :-
	atomic(P),
	var(X),
	!.
	
	

%----------------------------------------------------------------------
% nrToFOL([+VAR],+NUMBERRESTRICTION,-FORMULA)
% translates NUMBERRESTRICTION into FORMULA for variable VAR.
%
% Author: Ullrich Hustadt

nrToFOL(U,[X],atmost(0,R),forall(Y,not(F1))) :-
	!,
	malcToFOL(functional,U,[X,Y],R,F1).
nrToFOL(U,[X],atmost(M,R),F) :-
	% M >= 1
	N is M+1,
	nVars(N,VarList),
	relConjunction(U,X,VarList,R,F1),
	eqDisjunction(VarList,F2),
	quantify(forall,VarList,implies(F1,F2),F).
nrToFOL(_U,[_X],atleast(0,_R),true) :-
	!.
nrToFOL(U,[X],atleast(1,R),exists(Y,F1)) :-
	!,
	malcToFOL(functional,U,[X,Y],R,F1).
nrToFOL(U,[X],atleast(N,R),F) :-
	nVars(N,VarList),
	relConjunction(U,X,VarList,R,F1),
	neqConjunction(VarList,F2),
	quantify(exists,VarList,and([F1,F2]),F).

%----------------------------------------------------------------------
% quantify(+QUANTIFIER,+VARLIST,+MATRIX,-FORMULA)
% FORMULA is a formula with a quantifier prefix consisting only of
% quantifiers equal to QUANTIFIER and containing all variables in 
% VARLIST and the matrix of FORMULA is MATRIX.
%
% Author: Ullrich Hustadt

quantify(forall,[Y1],F,forall(Y1,F)) :-
	!.
quantify(forall,[Y1|YL],F,forall(Y1,F3)) :-
	quantify(forall,YL,F,F3).
quantify(exists,[Y1],F,exists(Y1,F)) :-
	!.
quantify(exists,[Y1|YL],F,exists(Y1,F3)) :-
	quantify(exists,YL,F,F3).

%----------------------------------------------------------------------
% eqDisjunction(+VARLIST,-FORMULA)
% FORMULA is a disjunction containing equalities for any pair
% of variables that can be build using variables in VARLIST. VARLIST
% must include at least 2 variables.
%
% Author: Ullrich Hustadt

eqDisjunction([Y1,Y2],F1) :-
	!,
	eqDisjunction(Y1,[Y2],F1),
	!.
eqDisjunction([Y1|YL],or([F1,F2])) :-
	eqDisjunction(Y1,YL,F1),
	eqDisjunction(YL,F2).

eqDisjunction(Y1,[Y2],equal(Y1,Y2)) :-
	!.
eqDisjunction(Y1,[Y2|YL],or([equal(Y1,Y2),F2])) :-
	eqDisjunction(Y1,YL,F2).

%----------------------------------------------------------------------
% neqConjunction(+VARLIST,-FORMULA)
% FORMULA is a conjunction containing inequalities for any pair
% of variables that can be build using variables in VARLIST. VARLIST
% must include at least 2 variables.
%
% Author: Ullrich Hustadt

neqConjunction([Y1,Y2],F1) :-
	!,
	neqConjunction(Y1,[Y2],F1),
	!.
neqConjunction([Y1|YL],and([F1,F2])) :-
	neqConjunction(Y1,YL,F1),
	neqConjunction(YL,F2).

neqConjunction(Y1,[Y2],not(equal(Y1,Y2))) :-
	!.
neqConjunction(Y1,[Y2|YL],and([not(equal(Y1,Y2)),F2])) :-
	neqConjunction(Y1,YL,F2).

%----------------------------------------------------------------------
% relConjunction(+VAR,+VARLIST,+ROLETERM,-FORMULA)
% FORMULA is a conjunction containing the translation of ROLETERM for
% any pair that can be build taking VAR and an element of VARLIST.
%
% Author: Ullrich Hustadt

relConjunction(_U,_X,[],_,true) :-
	!.
relConjunction(U,X,[Y1],R,F) :-
	!,
	malcToFOL(functional,U,[X,Y1],R,F).
relConjunction(U,X,[Y1|YL],R,and([F1,F2])) :-
	malcToFOL(functional,U,[X,Y1],R,F1),
	relConjunction(U,X,YL,R,F2).

%----------------------------------------------------------------------
% nVars(+N,-VARLIST)
% VARLIST is a list of N fresh Prolog variables.
%
% Author: Ullrich Hustadt

nVars(0,[]) :-
	!.
nVars(N,[_Y1|VL]) :-
	N >= 1,
	!,
	M is N-1,
	nVars(M,VL).
nVars(_,[]).

%----------------------------------------------------------------------


printNHProlog([(false <- T1)|CL]) :-
	!,
	write(<-),
	print(T1),
	write(' .'),
	nl,
	printNHProlog(CL).
printNHProlog([(H1 <- true)|CL]) :-
	!,
	print(H1),
	write(' '),
	write(<-),
	write(' .'),
	nl,
	printNHProlog(CL).
printNHProlog([C1|CL]) :-
	print(C1),
	write(.),
	nl,
	printNHProlog(CL).
printNHProlog([]).

%----------------------------------------------------------------------
% clausesToNHProlog(+CLAUSES,-NHCLAUSES)
% translates CLAUSES which are given in abstract syntax to NHCLAUSES
% which are in near-Horn Prolog syntax.
%
% Author: Ullrich Hustadt

clausesToNHProlog([cl(HL,TL)|CL],[C2|CL2]) :-
	literalsToNHProlog(HL,HL1),
	literalsToNHProlog(TL,TL1),
	implicationToNHProlog(HL1,TL1,C2),
	clausesToNHProlog(CL,CL2).
clausesToNHProlog([],[]).

literalsToNHProlog([H1,H2|HL],(H1,HL2)) :-
	literalsToNHProlog([H2|HL],HL2).
literalsToNHProlog([H1],H1) :-
	!.
literalsToNHProlog([],true) :-
	!.

implicationToNHProlog(HL1,[],HL1) :-
	!.
implicationToNHProlog(HL1,TL1,(HL1 :- TL1)) :-
	!.

%----------------------------------------------------------------------
% clausesToLOP(+CLAUSES,-NHCLAUSES)
% translates CLAUSES which are given in abstract syntax to NHCLAUSES
% which are in LOP syntax.
%
% Author: Ullrich Hustadt


clausesToLOP([cl(HL0,TL0)|CL],[C2|CL2]) :-
	clauseToSequent(cl(HL0,TL0),HL,TL),
	literalsToLOP(succedent,HL,HL1),
	literalsToLOP(antecedent,TL,TL1),
	implicationToLOP(HL1,TL1,C2),
	clausesToLOP(CL,CL2).
clausesToLOP([],[]).

clauseToSequent(cl([],TL),HL1,[]) :-
	!,
	map(negateLiterals,TL,HL1).
clauseToSequent(cl(HL,TL),HL,TL) :-
	!.

negateLiterals(~L,L) :-
	!.
negateLiterals(L,~L) :-
	!.

literalsToLOP(antecedent,[H1,H2|HL],(H1,HL2)) :-
	literalsToLOP(antecedent,[H2|HL],HL2).
literalsToLOP(succedent,[H1,H2|HL],(H1;HL2)) :-
	literalsToLOP(succedent,[H2|HL],HL2).
literalsToLOP(_,[H1],H1) :-
	!.
literalsToLOP(_,[],true) :-
	!.

implicationToLOP(HL1,true,(HL1 <- true)) :-
	!.
implicationToLOP(HL1,TL1,(HL1 <- TL1)) :-
	!.


%----------------------------------------------------------------------
% envToFOL(+CLAUSES,-NHCLAUSES)
% translates CLAUSES which are given in abstract syntax to NHCLAUSES
% which are in LOP syntax.
%
% Author: Ullrich Hustadt

envToFOL(Name,CL) :-
	translateModalAxioms(Name,CL1),
	translateAxioms(Name,CL2),
	append(CL1,CL2,CL),
	!.

translateModalAxiom([],[]) :-
	!.
translateModalAxiom([[MS,KClass,MOp,concept(C)]|L1],CL) :-
	!,
	malcToFOL(functional,U,[A],C,F1),
	translateClass(KClass,MOp,C,[A],F2),
	modalContextToFOL(MS,[],U,forall(A,implies(F1,F2)),F3),
	translate(F3,C1),
	clausesToLOP(C1,CL1),
	translateModalAxiom(L1,CL2),
	append(CL1,CL2,CL).
translateModalAxiom([[MS,KClass,MOp,all]|L1],CL) :-
	!,
	translateClass(KClass,MOp,all,[A],F2),
	modalContextToFOL(MC,[],U,forall(A,F2),F3),
	translate(F3,C1),
	clausesToLOP(C1,CL1),
	translateModalAxiom(L1,CL2),
	append(CL1,CL2,CL).
translateModalAxiom([[MS,KClass,MOp,A]|L1],CL) :-
	!,
	translateClass(KClass,MOp,C,[A],F2),
	modalContextToFOL(MC,[],U,forall(A,F2),F3),
	translate(F3,C1),
	clausesToLOP(C1,CL1),
	translateModalAxiom(L1,CL2),
	append(CL1,CL2,CL).

translateClass(kd45,MOp,C,[A],
	rel(C,m(MOp,A),B,app(typed(F,m(MOp,A)),D))) :-
	!.
translateClass(k,   MOp,C,[A],
        rel(C,m(MOp,A),B,app(typed(F,m(MOp,A)),B))) :-
	!.
translateClass(kd5, MOp,C,[A],
        and([rel(C,m(MOp,A),app(typed(F1,m(MOp,A)),U),app(typed(F2,m(MOp,A)),V)),
             rel(C,m(MOp,A),U,app(typed(F2,m(MOp,A)),U))])) :-
	!.


translateModalAxioms(Name,CL) :-
	setofOrNil([MS,KClass,MOp,Concept],
                   [X1,B1]^clause(modalAxioms(Name,MS,user,KClass,Concept,MOp,X1),B1),
		   L1),
	translateModalAxiom(L1,CL).

translateAxiom([],[]) :-
	!.
translateAxiom([[A1,A2,A3,A4,A5]|L1],CL) :-
	axiomToClause(A1,A2,A3,A4,A5,CL1),
	translateAxiom(L1,CL2),
	append(CL1,CL2,CL).

translateAxioms(Name,CL) :-
	theory(Name,CL0),
	setofOrNil([MS,[A],_,in,C],
	           [X1,Ax,B1]^clause(conceptElement(Name,MS,X1,user,A,C,Ax),B1),
		   L1),
	translateAxiom(L1,CL1),
	setofOrNil([MS,[A,B],_,in,R],
	           [X1,Ax,B1]^clause(roleElement(Name,MS,X1,user,A,B,R,Ax),B1),
		   L2),
	translateAxiom(L2,CL2),
	setofOrNil([MS,[X],CT1,equivalent,CT2],
	           [Ax,B1]^clause(conceptEqualSets(Name,user,MS,CT1,CT2,Ax),B1),
		   L3),
	translateAxiom(L3,CL3),
	setofOrNil([MS,[X],CT1,implies,CT2],
	           [Ax,B1]^clause(conceptSubsets(Name,user,MS,CT1,CT2,Ax),B1),
		   L4),
	translateAxiom(L4,CL4),
	setofOrNil([MS,[X,Y],RN,equivalent,RT],
	           [Ax,B1]^clause(roleEqualSets(Name,user,MS,RN,RT,Ax),B1),
		   L5),
	translateAxiom(L5,CL5),
	setofOrNil([MS,[X,Y],RN,implies,RT],
	           [Ax,B1]^clause(roleSubSets(Name,user,MS,RN,RT,Ax),B1),
		   L6),
	translateAxiom(L6,CL6),
%	closed(Name,MS,X,Y,R),
	append(CL1,CL2,CL12),
	append(CL12,CL3,CL13),
	append(CL13,CL4,CL14),
	append(CL14,CL5,CL15),
	append(CL15,CL6,CL16),
	append(CL0,CL16,CL),
	!.


/**********************************************************************
 *
 * @(#) modal.pl 1.6@(#)
 *
 */

modalAxioms(KName,MOp,A) :-
	getCurrentEnvironment(EnvName),
	modalAxioms(EnvName,[],KName,MOp,A).

/**********************************************************************
 *
 * genclass(+Agent,-Class) 
 * classifies Agent to distinguish axioms for a modal operator behaving
 * equally for all agents from axioms for a mutual modal operator.
 *
 */

genclass(_,_,A,A,every,true) :-
	var(A),
	!.
genclass(_,_,all,_,all,true) :-
	!.
genclass(Env,[W1,G1],concept(C),Agent,C,(Body,G1)) :-
	!,
	getQuery(Env,W1,C,Agent,_Exp,Body),
	!.
genclass(_,_,A,A,some,true) :-
	!.

/**********************************************************************
 * 
 * assertMA(+Class,+Head,+WorldGoal,+Goal)
 * asserts the appropriate clause for the given Class. If Class is all,
 * i.e. we are dealing with a mutual modal operator, no world checks 
 * have to be done. Otherwise WorldGoal has to be added to the body of
 * the clause.
 *
 */

assertMA(A1,rel(Env,every,m(MOp,A1),X,Y), WG, G) :-
	var(A1),
	asserta((rel(Env,every,m(MOp,A1),X,Y) :- (WG, G))),
	!.
assertMA(all,rel(Env,all,m(MOp,A),X,Y), _WG, G) :-
	asserta((rel(Env,all,m(MOp,A),X,Y) :- G)),
	!.
assertMA(A,rel(Env,some,m(MOp,A),X,Y), WG, G) :-
	asserta((rel(Env,some,m(MOp,A),X,Y) :- (WG, G))),
	!.
assertMA(concept(_),rel(Env,D,m(MOp,A),X,Y), WG, G) :-
	asserta((rel(Env,D,m(MOp,A),X,Y) :- (WG, G))),
	!.


/**********************************************************************
 *
 * modalAxioms(+EnvName,+KripkeClass,+MOp,+Agent)
 * asserts the modal axioms for the modal operator MOp and agent Agent
 * in environment EnvName for KripkeClass.
 *
 */
	
modalAxioms(MS,KName,MOp,A1) :-
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	modalAxioms(EnvName,MS,KName,MOp,A1).
modalAxioms(EnvName,KName,MOp,A1) :-
	environment(EnvName,_,_),
	modalAxioms(EnvName,[],KName,MOp,A1).

modalAxioms(EnvName,MS,k,MOp,A1) :-
	environment(EnvName,Env,_),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	genclass(Env,[W1,G1],A1,A,C,Goal),
	retractall(rel(Env,C,m(MOp,A),_,_)),
	retractall(modalAxioms(Env,MS,user,_,A1,MOp,A)),
	assertMA(A1,
                 rel(Env,C,m(MOp,A),U,app(_FF:m(MOp,A),U)), 
		 (not(not(world(Env,m(MOp,A),U,V)))), 
		 (normal(Env,U), Goal)),
	asserta(modalAxioms(Env,MS,user,k,A1,MOp,A)),
	!.
modalAxioms(EnvName,MS,kd45,MOp,A1) :-
	environment(EnvName,Env,_),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	genclass(Env,[W1,G1],A1,A,C,Goal),
	retractall(rel(Env,C,m(MOp,A),_,_)),
	retractall(modalAxioms(Env,MS,user,_,A1,MOp,A)),
	assertMA(A1,
	         rel(Env,C,m(MOp,A),U,app(_FF:m(MOp,A),V)), 
		 (not(not(world(Env,m(MOp,A),U,V)))), 
		 (normal(Env,U), Goal)),
%	assertMA(A1,
%                 rel(Env,C,m(MOp,A),U,app(_FF:m(MOp,A),U)), 
%		 (not(not(world(Env,m(MOp,A),U,V)))), 
%		 (normal(Env,U), Goal)),
	asserta(modalAxioms(Env,MS,user,kd45,A1,MOp,A)),
	!.
modalAxioms(EnvName,MS,kd4e,MOp,A) :-
	modalAxioms(EnvName,kd45,MOp,A).
modalAxioms(EnvName,MS,kd5,MOp,A1) :-
	environment(EnvName,Env,_),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	genclass(Env,[W1,G1],A1,A,C,Goal),
	retractall(rel(Env,C,m(MOp,A),_,_)),
	retractall(modalAxioms(Env,MS,user,_,A1,MOp,A)),
	assertMA(A1,
	         rel(Env,C,m(MOp,A),app(_F1:m(MOp,A),U),app(_F2:m(MOp,A),V)), 
		 ((world(Env,m(MOp,A),U,V), not(U == []))), 
		 Goal),
	assertMA(A1,
	         rel(Env,C,m(MOp,A),U,app(_F2:m(MOp,A),U)), 
		 true, 
		 Goal),
	asserta(modalAxioms(Env,MS,user,kd5,A1,MOp,A)),
	!.
modalAxioms(EnvName,MS,kd4,MOp,A1) :-
	environment(EnvName,Env,_),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	genclass(Env,[W1,G1],A1,A,C,Goal),
	retractall(rel(Env,C,m(MOp,A),_,_)),
	retractall(modalAxioms(Env,MS,user,_,A1,MOp,A)),
	assertMA(A1,rel(Env,C,m(MOp,A),U,app(_F1:m(MOp,A),U)), Goal),
	assertMA(A1,rel(Env,C,m(MOp,A),U,app(_F1:m(MOp,A),V)), (world(Env,m(MOp,A),U,V), (rel(Env,_,m(MOp,A),U,V), Goal))),
	asserta(modalAxioms(Env,MS,user,k4,A1,MOp,A)),
	!.
modalAxioms(EnvName,MS,kt,MOp,A1) :-
	environment(EnvName,Env,_),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	genclass(Env,[W1,G1],A1,A,C,Goal),
	retractall(rel(Env,C,m(MOp,A),_,_)),
	retractall(modalAxioms(Env,MS,user,_,A1,MOp,A)),
	assertMA(A1,rel(Env,C,m(MOp,A),U,app(_F1:m(MOp,A),U)), Goal),
	assertMA(A1,rel(Env,C,m(MOp,A),U,U), Goal),
	asserta(modalAxioms(Env,MS,user,kt,A1,MOp,A)),
	!.

/**********************************************************************
 *
 * normal(+EnvName,+World)
 * succeeds if World is normal, i.e. has a successor.
 *
 */

normal(_,_).

/**********************************************************************
 *
 * world(+EnvName,m(+MOp,+Agent),+WorldSequence) 
 * checks wether or not WorldSequence is a sequence of worlds for
 * modal operator MOp and agent Agent.
 *
 */

world(_Env,m(_MOp,_A),U,U).
world(Env,m(MOp,A),U,app(_FF:m(MOp,A),V)) :-
	world(Env,m(MOp,A),U,V).

/**********************************************************************
 *
 * @(#) roleFunctions.pl 1.2@(#)
 *
 */

/***********************************************************************
 *
 * getDirectFatherRoles(+EnvName,+MS,+Role,-RL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are direct father roles
 * of Role.
 *
 */

getDirectFatherRoles(EnvName,MS,Role,RL) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Dag),
	getDirectSuperElements(Role,RL,Dag).

/***********************************************************************
 *
 * getAllFatherRoles(+EnvName,+MS,+Role,-RL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are father roles of
 * Role
 *
 */

getAllFatherRoles(EnvName,MS,Role,RL) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Dag),
	getAllSuperElements(Role,RL,[],Dag).

/***********************************************************************
 *
 * getDirectSonRoles(+EnvName,+MS,+Role,-RL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are direct father roles
 * of Role
 *
 */

getDirectSonRoles(EnvName,MS,Role,RL) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Dag),
	getDirectSubElements(Role,RL,Dag).

/***********************************************************************
 *
 * getAllSonRoles(+EnvName,+MS,+Role,-RL)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role        role name
 *            RL          list of role names
 * RL is the list of all role names which are father roles of 
 * Role
 *
 */

getAllSonRoles(EnvName,MS,Role,RL) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Dag),
	getAllSubElements(Role,RL,Dag).

/***********************************************************************
 *
 * getRoles(+EnvName,+MS,-RL)
 * Arguments: EnvName     environment identifier
 *            MS     modal context
 *            RL     list of role names
 * RL is the list of all role names in the subsumption hierarchy.
 *
 */

getRoles(EnvName,MS,['top'|RL]) :-
	getAllSonRoles(EnvName,MS,'top',RL).

/***********************************************************************
 *
 * testDirectFatherRole(+EnvName,+MS,+Role1,+Role2,-Role)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct father role of Role2
 * or
 * Role is Role2 iff Role2 is a direct father role of Role1
 * otherwise
 * the predicate fails
 *
 */

testDirectFatherRole(EnvName,MS,Role1,Role2,Role) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Dag),
	testDirectSuperElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * testDirectSonRole(+EnvName,+MS,+Role1,+Role2,-Role)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct subrole of Role2
 * or
 * Role is Role2 iff Role2 is a direct subrole of Role1
 * otherwise
 * the predicate fails
 *
 */

testDirectSonRole(EnvName,MS,Role1,Role2,Role) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Dag),
	testDirectSuperElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * testFatherRole(+EnvName,+MS,+Role1,+Role2,-Role)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct father role of Role2
 * or
 * Role is Role2 iff Role2 is a direct father role of Role1
 * otherwise
 * the predicate fails
 *
 */

testFatherRole(EnvName,MS,Role1,Role2,Role) :-
	environment(EnvName,Env,_),
	roleHierarchy(Env,MS,Dag),
	testSuperElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * testSonRole(+EnvName,+MS,+Role1,+Role2,-Role)
 * Arguments: EnvName     environment identifier
 *            MS          modal context
 *            Role1       role name
 *            Role2       role name
 *            Role        role name
 * Role is Role1 iff Role1 is a direct father role of Role2
 * or
 * Role is Role2 iff Role2 is a direct father role of Role1
 * otherwise
 * the predicate fails
 *
 */

testSonRole(Env,MS,Role1,Role2,Role) :-
	roleHierarchy(Env,MS,Dag),
	testSubElement(Role1,Role2,Role,Dag).

/***********************************************************************
 *
 * getCommonFatherRoles(+EnvName,+MS,RL1,RL2)
 * Arguments: EnvName  environment identifier
 *            MS       modal context
 *            RL1      list of role names
 *            RL2      list of role names
 * RL2 is the list of all role names subsuming all roles in RL1.
 *
 */

getCommonFatherRoles(EnvName,MS,RL1,RL2) :-
	hop_map(getAllFatherRoles,[EnvName,MS],RL1,RLL1),
	intersection(RLL1,RL2).

/***********************************************************************
 *
 * getCommonSonRoles(+EnvName,+MS,RL1,RL2)
 * Arguments: EnvName  environment identifier
 *            MS       modal context
 *            RL1      list of role names
 *            RL2      list of role names
 * RL2 is the list of all role names which are subsumed by all
 * roles in RL1.
 *
 */

getCommonSonRoles(EnvName,MS,RL1,RL2) :-
	hop_map(getAllSonRoles,[EnvName,MS],RL1,RLL1),
	intersection(RLL1,RL2).

/**********************************************************************
 *
 * @(#) revision.pl 1.27@(#)
 *
 */

/**********************************************************************
 *
 * undefconcept(+MS,+CN)
 *
 */


undefconcept(CN) :-
	getCurrentEnvironment(EnvName),
	undefconcept(EnvName,[],CN).

undefconcept(EnvName,CN) :-
	environment(EnvName,_,_),
	!,
	undefconcept(EnvName,[],CN).
undefconcept(MS,CN) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	undefconcept(EnvName,MS,CN).
undefconcept(CN,CT) :-
	getCurrentEnvironment(EnvName),
	undefconcept(EnvName,[],CN,CT).

undefconcept(EnvName,MS,CN) :-
	environment(EnvName,Env,_),
	(MS = [] ; MS = [_|_]),
	!,
	undefConcept(Env,MS,CN).
undefconcept(MS,CN,CT) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	undefconcept(EnvName,MS,CN,CT).
undefconcept(EnvName,CN,CT) :-
	undefconcept(EnvName,[],CN,CT).

undefconcept(EnvName,MS,CN,CT) :-
	environment(EnvName,Env,_),
	
	conceptEqualSets(Env,_user,MS,CN,CT,AX),
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
%	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
%	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
 	retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retractall_all(query(Env,MS,CN,_CT,_PT,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(EnvName,MS,CN,CT),
	retract(conceptEqualSets(Env,_user,MS,CN,CT,AX)),
	!.

undefConcept(Env,MS,CN) :-
	conceptEqualSets(Env,user,_,CN,_,Ax),
	
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
%	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_,_)),
%	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_,_)),
 	retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
   	retract_all(query(Env,MS,CN,_CT,_PT,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(EnvName,MS,CN,CT),
	retractall(conceptEqualSets(Env,user,MS,CN,_CT,Ax)),
	fail,
	!.
undefConcept(_Env,_MS,_CN) :-
	!.

retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)) :-
	clause(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_)),_),
	member(rn(AX,_,_,_),[Name]),	
	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
	fail.
retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)).

retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)) :-
	clause(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_)),_),
	member(rn(AX,_,_,_),[Name]),	
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
	fail.
retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)) :-
	retractall(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(in(_Name1,rn(AX,_,_,_),_,_,_,_,_,_,_)).

retract_all(query(Env,MS,CN,_CT,_PT,_PT1)) :-
	query(Env,MS,CN1,CT,PT,PT1),
	collect(PT,Liste),
	member(CN,Liste),
	retractall(query(Env,MS,CN1,CT,PT,PT1)),
	fail.
retract_all(query(Env,MS,CN,_CT,_PT,_PT1)).

/**********************************************************************
 *
 * undefrole(+MS,+CN)
 *
 */
undefrole(RN) :-
	getCurrentEnvironment(EnvName),
	undefrole(EnvName,[],RN).

undefrole(EnvName,RN) :-
	environment(EnvName,_,_),
	!,
	undefrole(EnvName,[],RN).
undefrole(MS,RN) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	undefrole(EnvName,MS,RN).
undefrole(RN,RT) :-
	getCurrentEnvironment(EnvName),
	undefrole(EnvName,[],RN,RT).

undefrole(EnvName,MS,RN) :-
	environment(EnvName,Env,_),
	(MS = [] ; MS = [_|_]),
	!,
	undefRole(Env,MS,RN).
undefrole(MS,RN,RT) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	undefrole(EnvName,MS,RN,RT).
undefrole(EnvName,RN,RT) :-
	undefrole(EnvName,[],RN,RT).
undefrole(EnvName,MS,RN,RT) :-
	environment(EnvName,Env,_),

	roleEqualSets(Env,_user,MS,RN,RT,AX),
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_)),

%	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
%	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
 	retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(query(Env,MS,CN,_CT,_PT,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(EnvName,MS,RN,RT),
	retract(roleEqualSets(Env,_user,MS,RN,RT,AX)),
	!.
undefRole(Env,MS,RN) :-
	roleEqualSets(Env,user,MS,RN,_,Ax),

	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_,_)),
%	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_,_)),
% 	retract_all(query(Env,MS,RN,_RT,_PT,_)),
 	retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(EnvName,MS,RN,_),
	retractall(roleEqualSets(Env,user,MS,RN,_RT,Ax)),
	fail,
	!.
undefRole(_Env,_MS,_RN) :-
	!.

/**********************************************************************
 *
 * undefprimconcept(+MS,+CN)
 *
 */

undefprimconcept(CN) :-
	getCurrentEnvironment(EnvName),
	undefprimconcept(EnvName,CN,_).
undefprimconcept(CN,CT) :-
	getCurrentEnvironment(EnvName),
	!,
	undefprimconcept(EnvName,[],CN,CT).

undefprimconcept(EnvName,CN,CT) :-
	environment(EnvName,_,_),
	!,
	undefprimconcept(EnvName,[],CN,CT).
undefprimconcept(MS,CN,CT) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	undefprimconcept(EnvName,MS,CN,CT).

undefprimconcept(EnvName,MS,CN,CT) :-	
	environment(EnvName,Env,_),

	conceptSubsets(Env,_user,MS,CN,CT,AX),
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
%	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
%	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
 	retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(query(Env,MS,CN,_CT,_PT,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,lInR),_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,lInR),_,_,_,_,_,_)),
	change_classifier(EnvName,MS,CN,CT),
	retract(conceptSubsets(Env,_user,MS,CN,CT,AX)),
	!.
/**********************************************************************
 *
 * undefprimrole(+MS,+CN)
 *
 */
undefprimrole(RN) :-
	getCurrentEnvironment(EnvName),
	undefprimrole(EnvName,[],RN).

undefprimrole(EnvName,RN) :-
	environment(EnvName,_,_),
	!,
	undefprimrole(EnvName,[],RN).
undefprimrole(MS,RN) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	undefprimrole(EnvName,MS,RN).
undefprimrole(RN,RT) :-
	getCurrentEnvironment(EnvName),
	undefprimrole(EnvName,[],RN,RT).

undefprimrole(EnvName,MS,RN) :-
	environment(EnvName,Env,_),
	(MS = [] ; MS = [_|_]),
	!,
	undefprimRole(Env,MS,RN).
undefprimrole(MS,RN,RT) :-
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	undefprimrole(EnvName,MS,RN,RT).
undefprimrole(EnvName,RN,RT) :-
	undefprimrole(EnvName,[],RN,RT).
undefprimrole(EnvName,MS,RN,RT) :-
	environment(EnvName,Env,_),

	roleSubsets(Env,_user,MS,RN,RT,AX),
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
%	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
%	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
 	retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(query(Env,MS,RN,_RT,_PT,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,lInR),_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,lInR),_,_,_,_,_,_)),
	change_classifier(EnvName,MS,RN,RT),
	retract(roleSubsets(Env,_user,MS,RN,RT,AX)),
	!.
undefprimRole(Env,MS,RN) :-
	roleSubsets(Env,user,MS,RN,_,Ax),

	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_,_)),
	retractall(kb_in(Env,rn(AX,_,_,_),_,_,_,_,_,_,_,_)),
 	retract_all(query(Env,MS,RN,_RT,_PT,_)),
 	retract_all(kb_in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
 	retract_all(in(Env,_Name1,rn(AX,_,_,_),_,_,_,_,_,_,proved(in([],Name,_,_),_))),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	retractall(eq(Env,rn(AX,_,_,_),_,_,_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_)),
	retractall(constraint(Env,rn(AX,_,_,_),_,_,_,_,_,_)),
	change_classifier(EnvName,MS,RN,_),
	retractall(roleSubsets(Env,user,MS,RN,_RT,Ax)),
	fail,
	!.

/** ist in arbeit    */


delete_ind(X,C) :-
	completeParameter([(X,C)],EnvName,MS,_,_),
	delete_ind(EnvName,MS,X,C).
delete_ind(EnvName,X,C) :-
	environment(EnvName,_,_),
	!,
	delete_ind(EnvName,[],X,C).
delete_ind(MS,X,C) :-
	(MS = []; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	delete_ind(EnvName,MS,X,C).
delete_ind(X,Y,R) :-
	getCurrentEnvironment(EnvName),
	delete_ind(EnvName,[],X,Y,R).
delete_ind(EnvName,MS,X,C) :-
	environment(EnvName,Env,_),
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	!,
	atomic(X),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	constructMLHead(Env,rn(_,_,user,lInR),W1,C,X,_HYPS,_D,_CALLS,abox,InHead),
	not(not((retract((InHead :- call(user:G1))) ;
 	 retract((InHead :- call(G1)))))),
	not(not((retract((conceptElement(Env,_,W1,_,X,C,_) :- call(user:G1))) ;
	 retract((conceptElement(Env,_,W1,_,X,C,_) :- call(G1)))))),
	 retractall((InHead :- call(user:G1))),
	 retractall((InHead :- call(G1))),
	 retractall((conceptElement(Env,_,W1,_,X,C,_) :- call(user:G1))),
	 retractall((conceptElement(Env,_,W1,_,X,C,_) :- call(G1))).
delete_ind(P1,X,Y,R) :-
	completeParameter([(X,Y,R)],EnvName,MS,_,_),
	delete_ind(EnvName,MS,X,Y,R).
delete_ind(EnvName,MS,X,Y,R) :-
	environment(EnvName,Env,_),
	atomic(X),
	atomic(Y),
	atomic(R),
	Role1 =.. [R,X,Y],
	retract(Role1),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	constructEqHead(Env,rn(_,_,user,lInR),W1,Y,_FF,R,X,_,_D,CALLS,abox,EqLiteral),
	not(not((retract((EqLiteral :- (cCS(CALLS,true), call(user:G1))));
	 retract((EqLiteral :- (cCS(CALLS,true), call(G1))))))),
	not(not((retract((roleElement(Env,_,W1,X,Y,R,_) :- call(user:G1))) ;
	 retract((roleElement(Env,_,W1,X,Y,R,_) :- call(G1)))))),
	retractall((EqLiteral :- (cCS(CALLS,true), call(user:G1)))),
	retractall((EqLiteral :- (cCS(CALLS,true), call(G1)))),
	retractall((roleElement(Env,_,W1,X,Y,R,_) :- call(user:G1))),
	retractall((roleElement(Env,_,W1,X,Y,R,_) :- call(G1))).

	
/***
*    change_classifier veraendert die Sub.Hierarchie nach undefconcept      
***/
change_classifier(CN,CT) :-
	change_classifier([],CN,CT),
	!.
change_classifier(MS,CN,CT) :-
	getCurrentEnvironment(EnvName),
	environment(EnvName,Env,_),
	change_classifier(EnvName,MS,CN,CT),
	!.

change_classifier(EnvName,MS,CN,CT) :-
	environment(EnvName,Env,_),
	getConceptName(Env,[],CN),
	getAllSubConcepts(EnvName,MS,CN,CL1),
	find_definition(CN,CL2),
 	union1(CL1,CL2,CL),print(CL),
	change_classifier1(Env,MS,CN,CT,CL),
	delete_hierarchy(concepts,Env,MS,CN),
	change_classifier2(Env,MS,CT,CL),
	!.
change_classifier(EnvName,MS,RN,CT) :-
	environment(EnvName,Env,_),
	getRoleName(Env,[],RN),
	getAllSonRoles(EnvName,MS,RN,CL1),
	find_definition(RN,CL2),
 	union1(CL1,CL2,CL),print(CL),
	change_classifier1(Env,MS,RN,CT,CL),
	delete_hierarchy(roles,Env,MS,RN),
	change_classifier2(Env,MS,CT,CL),
	!.
change_classifier(EnvName,MS,CN,CT).

change_classifier1(Env,MS,CR,CT,[]) :-
	!.
change_classifier1(Env,MS,CR,CT,[H|T]) :-
	getConceptName(Env,MS,H),
	(H \== top,H \== bot),
	delete_hierarchy(concepts,Env,MS,H),
	change_classifier1(Env,MS,CR,CT,T),
	!.
change_classifier1(Env,MS,CR,CT,[H|T]) :-
	getRoleName(Env,MS,H),
	(H \== top,H \== bot),
	delete_hierarchy(roles,Env,MS,H),
	change_classifier1(Env,MS,CR,CT,T),
	!.
change_classifier1(Env,MS,CR,CT,[H|T]) :-
	change_classifier1(Env,MS,CR,CT,T),
	!.

change_classifier2(Env,MS,CT,[]) :-
	!.
change_classifier2(Env,MS,CT,[H|T]) :-
	getConceptName(Env,MS,H),
	not(make_succ2(concepts,Env,MS,H)),
	change_classifier2(Env,MS,CT,T),
	!.
change_classifier2(Env,MS,CT,[H|T]) :-
	getRoleName(Env,MS,H),
	not(make_succ2(roles,Env,MS,H)),
	change_classifier2(Env,MS,CT,T),
	!.
change_classifier2(Env,MS,CT,[H|T]) :-
	change_classifier2(Env,MS,CT,T),
	!.
delete_hierarchy(Type,Env,MS,CR) :-
        clause(succ(Type,Env,MS,CR,SC),_),
	clause(succ(Type,Env,MS,PC,CR),_),
	subsume1(Type,Env,MS,PC,SC),
	assert_succ(Type,Env,MS,PC,SC),
	fail.
delete_hierarchy(Type,Env,MS,CR) :-
	retractall(succ(Type,Env,MS,CR,_)),
	retractall(succ(Type,Env,MS,_,CR)),
	retractall(sub(Type,Env,MS,CR,_)),
	retractall(sub(Type,Env,MS,_,CR)),
	retractall(nsub(Type,Env,MS,CR,_)),
	retractall(nsub(Type,Env,MS,_,CR)),
	!.
	
/*****************************************************************************
*      find_definition   sucht alle concepte die irgendwie mit dem 	     *
* 			  zu loeschenden concept verbunden sind.             *
*****************************************************************************/

find_definition(Concept,Liste) :-
	find_definition([],Concept,Liste).
find_definition(MS,Concept,Liste) :-
	getCurrentEnvironment(EnvName),
	environment(EnvName,Env,_),
	find_definition(Env,MS,Concept,Liste),
	!.
find_definition(Env,MS,Concept,Liste) :-
	getConceptName(Env,MS,Concept),
	find_conceptEqualSets(Env,_user,MS,Concept,CT1,_),
	collect(CT1,List1),
	find_conceptSubsets(Env,_user,MS,Concept,CT2,_),
	collect(CT2,List2),
	union1(List1,List2,List3),
	find_Def_concept(MS,Concept,List_of_Concepts),
	union1(List3,List_of_Concepts,Liste),
	!.
find_definition(Env,MS,Role,Liste) :-
	getRoleName(Env,MS,Role),
	find_roleEqualSets(Env,_user,MS,Role,CT1,_),
	collect(CT1,List1),
	find_roleSubsets(Env,_user,MS,Role,CT2,_),
	collect(CT2,List2),
	union1(List1,List2,List3),
	find_Def_role(MS,Role,List_of_Roles),
	union1(List3,List_of_Roles,Liste),
	!.

find_conceptEqualSets(Env,_user,MS,Concept,CT1,_) :-
	conceptEqualSets(Env,_user,MS,Concept,CT1,_),
	!.
find_conceptEqualSets(Env,_user,MS,Concept,[],_).
find_conceptSubsets(Env,_user,MS,Concept,CT2,_) :-
	conceptSubsets(Env,_user,MS,Concept,CT2,_),
	!.
find_conceptSubsets(Env,_user,MS,Concept,[],_).

find_roleEqualSets(Env,_user,MS,Role,CT1,_) :-
	roleEqualSets(Env,_user,MS,Role,CT1,_),
	!.
find_roleEqualSets(Env,_user,MS,Role,[],_).
find_roleSubsets(Env,_user,MS,Role,CT2,_) :-
	roleSubsets(Env,_user,MS,Role,CT2,_),
	!.
find_roleSubsets(Env,_user,MS,Role,[],_).


find_Def_concept(MS,Concept,List_of_Concepts) :-
	getCurrentEnvironment(EnvName),
	environment(EnvName,Env,_),
	find_Def_concept(Env,MS,Concept,List_of_Concepts).
% Removed redundant code 
% 15.02.93 uh
% find_Def_concept(MS,Concept,List_of_Concepts) :-
%	getCurrentEnvironment(EnvName),
%	environment(EnvName,Env,_),
%	find_Def_concept(Env,MS,Concept,List_of_Concepts).
find_Def_concept(Env,MS,Concept,List_of_Concepts) :-
	setofOrNil(CN,find_Def_concept1(Env,MS,Concept,CN),List_of_Concepts),
	!.

find_Def_concept1(Env,MS,Concept,CN) :-
	conceptEqualSets(Env,_user,MS,CN,CT,AX),
	atom(CN),
	not(name(CN,[99,111,110,99,101,112,116|_])),	
	collect(CT,Liste),
	member(Concept,Liste).
find_Def_concept1(Env,MS,Concept,CN) :-
	conceptSubsets(Env,_user,MS,CN,CT,AX),
	atom(CN),
	not(name(CN,[99,111,110,99,101,112,116|_])),
	collect(CT,Liste),
	member(Concept,Liste).
find_Def_role(MS,Role,List_of_Roles) :-
	getCurrentEnvironment(EnvName),
	environment(EnvName,Env,_),
	find_Def_role(Env,MS,Role,List_of_Roles).
find_Def_role(Env,MS,Role,List_of_Roles) :-
	setofOrNil(CN,find_Def_role1(Env,MS,Role,CN),List_of_Roles),
	!.
find_Def_role1(Env,MS,Role,CN) :-
	roleEqualSets(Env,_user,MS,CN,CT,AX),
	atom(CN),
	not(name(CN,[99,111,110,99,101,112,116|_])),	
	collect(CT,Liste),
	member(Role,Liste).
find_Def_role1(Env,MS,Role,CN) :-
	roleSubsets(Env,_user,MS,CN,CT,AX),
	atom(CN),
	not(name(CN,[99,111,110,99,101,112,116|_])),
	collect(CT,Liste),
	member(Role,Liste).
/****************************************************************************/
/*       collect      sammelt aus einer bel.verschachtelten kette von      
*/
/*                    ausdruecken alle Concept'e oder Role's .             
*/
/***************************************************************************/	

collect(CT,Liste) :-
	collect1(CT,L),
	collect2(L,Liste),
	!.
collect2([],[]).
collect2([H|T],[H|L]) :-
	currentEnvironment(Env),
	nonvar(H),
	(clause(conceptName(Env,_,_,H),_);clause(roleName(Env,_,_,H),_)),
	collect2(T,L),
	!.
collect2([H|T],L) :-
	collect2(T,L),
	!.
collect1([],L) :-
	!.
collect1([X|R],L) :-
	(atom(X);number(X)),
	collect1(R,L1),
	union1(X,L1,L),
	!.
collect1([[X1|R1]|R],L) :-
	(atom(X1);number(X1)),
	collect1(R1,L1),
	collect1(R,L2),
	union1(L1,L2,L3),
	union1(L3,X1,L),
	!.
collect1([[X1|R1]|R],L) :-
	X1 =.. X2,
	collect1(X2,L2),
	collect1(R1,L1),
	collect1(R,L3),
	union1(L1,L2,L4),
	union1(L4,L3,L),
	!.
collect1([X|R],L3) :-
	X =.. X1,
	collect1(X1,L1),
	collect1(R,L2),
	union1(L1,L2,L3),
	!.
collect1(X,[X|L]) :-
	(atom(X);number(X)),
	currentEnvironment(Env),
	(clause(conceptName(Env,_,_,X),_);clause(roleName(Env,_,_,X),_)),
	!.

collect1(X,[L|L1]) :-
	X =.. X1,
	collect1(X1,L1),!.
/**********************************************************************
 *
 * @(#) sb2.pl 1.20@(#)

 *
 */

/*-------------------------------------------------------------------------------
 * Der Konstruktor: sb_defenv(EName,Comment)
 * generiert ein neues Environment und bindet dies ans uebergebene Symbol EName,
 * EName wird also zu current environment.
 * Comment kann irgendein string sein.
 *-----------------------------------------*/

  
  sb_defenv(EName,Comment):- makeEnvironment(EName,Comment).


/*-------------------------------------------------------------------------------
 * sb_initenv
 * loescht den Inhalt eines environment, und initialisiert den in sb_defenv schon
 * definierten EName als Tbox-Environment.
 *----------------------------------------*/

 sb_initenv:- 
	initEnvironment.
 sb_initenv(EnvName) :- 
	initEnvironment(EnvName).


/*-------------------------------------------------------------------------------
 * Der 4-stellige Hilfskonstruktor make__primconcept mit folgenden Parametern:
 * (EnvName,MS,CName1,supers(Liste_von_Konzepte))  
 * definiert ein Konzept CName1 als Subkonzept von den in der Liste auftretenden 
 * Superkonzepten in environment EnvName und modal context MS.
 *-------------------------------------------*/


make_primconcept(EnvName,MS,CName1,supers([])).
make_primconcept(EnvName,MS,CName1,supers([X|T])):-
		  defprimconcept(EnvName,MS,CName1,X),
		  make_primconcept(EnvName,MS,CName1,supers(T)).

/*------------------------------------------------------------------------------
 * make_primconcept(EnvName,MS,CName1,restr-inh(RName1,restricts(RName2,
 *                                                     range(CName2,CNameDef))))
 * 
 * definiert ein Konzept CName1, an dem eine Rolle RName2  auf den Rollenfueller
 * CName2 (range) restrigniert wird.
 * Und zwar in evironment=EnvName und modal context MS. 
 * Der neuen Rolle wird das Symbol RName1 zugewiesen,
 * die Domain-Menge dieser neuen Rolle (CNameDom) ist gerade eine Teilmenge von 
 * CName1.
 *----------------------------------------*/
make_primconcept(EnvName,MS,CName1,
                'restr-inh'(RName1,restricts(RName2,range(CName2,CNameDef)))):-
	make_primconcept(EnvName,MS,CName1,
	                 restrict_inh(RName1, restricts(RName2 ,
				 		        range(CName2,CNameDef)))).
make_primconcept(EnvName,MS,CName1,
                'restrict-inh'(RTerm1,restricts(RTerm2,range(CName2,CNameDef)))):-
       make_primconcept(EnvName,MS,CName1,
                        restrict_inh(RTerm1, restricts(RTerm2,
						       range(CName2,CNameDef)))).

make_primconcept(EnvName,MS,CName1,restrict_inh(RTerm1, restricts(RTerm2 ,
						     range(CName2,CNameDef)))):-
	expand_role(EnvName,MS,RTerm1,RName1,_,_,_),
	expand_role(EnvName,MS,RTerm2,RName2,_,_,_),
	environment(EnvName,Env,_),
	defrole(EnvName,MS,RName1 , restr(RName2 , CName2)),
	gensym(concept,CNameDom),
	defconcept(EnvName,MS,CNameDom ,some(RName2 ,top)),
        defprimconcept(EnvName,MS,CNameDom ,CName1),
%	defprimconcept(EnvName,MS,and([some(inverse(RName1),top),
%				       naf(not(CNameDef))]),CNameDef),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleDomain(Env,W1,RName1,CNameDom) :- G1)),
	assertz((roleRange(Env,W1,RName1,CName2) :- G1)),
	assertz((roleDefault(Env,W1,RName1,CNameDef) :- G1)),
	assertz((roleTripel(Env,W1,RName1,CNameDom,CName2,CNameDef))).
   
/*----------------------------------------------------------------------------
 * make_primconcept(EnvName,MS,CName1, nr(RName1,MinNr,MaxNr,DefNr))
 * definiert ein Konzept CName1, an dem eine "number restriction" stattfindet:
 * die Rolle RName1 wird bzgl. ihrer Kardinalitaet restrigniert auf
 * (MinNr,MaxNr,DefNr),
 * und zwar in  environment=EnvName und modal context=MS,
 * die Dom-Menge der Rolle RName1 ist dann ein CNameDom als Teilmenge von CName1.
 *-----------------------------------------*/


make_primconcept(EnvName,MS,CName1 , nr(RTerm1, MinNr,MaxNr,DefNr)):-
	environment(EnvName,Env,_),
	expand_role(EnvName,MS,RTerm1,RName1,CNameDomT,CName2T,CNameDefT),
	gensym(concept,CNameDom),
	defconcept(EnvName,MS,CNameDom, and([atleast(MinNr,RName1),atmost(MaxNr,RName1)])),
	defconcept(EnvName,MS,CNameDom, some(RName1,top)), 
	defprimconcept(EnvName,MS,CNameDom , CName1),
%	gensym(concept,CNameDef),
%	defconcept(EnvName,MS,CNameDef, and([atleast(DefNr,RName1),atmost(DefNr,RName1)])),
%	defprimconcept(EnvName,MS,and([some(inverse(RName1)),naf(not(CNameDef))]),CNameDef),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleNr(Env,W1,RName1,MinNr,MaxNr) :- G1)),
	assertz((roleDefNr(Env,W1,RName1,DefNr) :- G1)),
	assertz((roleAll(Env,W1,Rname1,CNameDomT,CName2T,CNameDefT,MinNr,MaxNr,DefNr))).



/*------------------------------------------------------------------------------
 *sb_primconcept(CName)
 * definiert ein generelles Konzept CName in modal context [].
 *---------------------------------------*/


 sb_primconcept(CName):- 
	getCurrentEnvironment(EnvName),
	sb_primconcept(EnvName,[],CName).


/*-------------------------------------------------------------------------------
 * Der 2-stellige Konstruktor sb_primconcept mit jeweils folgenden Parametern:
 * (EnvName,CName1), oder (MS,CName1) definiert ein generelles Konzept CName1
 * in invironment EnvName und modal context [], bzw. in current Environment und
 * modal context MS.
 *----------------------------------------*/
  
sb_primconcept(EnvName,Left) :-
	environment(EnvName,_,_),
	!,
	sb_primconcept(EnvName,[],Left),
	!.


sb_primconcept(MS,Left) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_primconcept(EnvName,MS,Left),
	!.



/*-----------------------------------------------------------------------------
 * Der 2-stellige Konstruktor sb_primconcept(CName1,SpecListe) hat als Argumente ein
 * CName1 und eine Liste von bis jetzt eingef"uhrten Pr"adikaten wie:
 * [supers(Liste von concepts) , restr-inh(...) , nr(...)], es werden dann jeweils die
 * entsprechenden Hilfskonstruktoren mit dem einzelnen Listenelement als aktueller Parameter 
 * aktiviert bzw. aufgerufen und zwar in current environment und [] als modal context.
 *-----------------------------------------*/

sb_primconcept(CName1,[]) :-
	!.
sb_primconcept(CName1,[X|T]):-
	getCurrentEnvironment(EnvName),
	sb_primconcept(EnvName,[],CName1,[X|T]),
	!.



/*-----------------------------------------------------------------------------
 * Der 3-stellige Konstruktor sb_primconcept(A,CName1,SpecListe) funktioniert analog
 * nur in A= Environment und [] als modal context, bzw. in current environment und
 * A=MS als modal context.
 *-----------------------------------------*/

sb_primconcept(A,CName1,[]) :-
	!.
sb_primconcept(A,CName1,[X|T]):-
	environment(A,_,_),
	!,
	sb_primconcept(A,[],CName1,[X|T]),
	!.
sb_primconcept(A,CName1,[X|T]):-
	(A = [] ; A = [_|_]),
	getCurrentEnvironment(EnvName),
	!,
	sb_primconcept(EnvName,A,CName1,[X|T]).

/*-------------------------------------------------------------------------------
 * Der 3-stellige Konstruktor von  sb_primconcept mit folgenden Parametern:
 * (Environment,MS,CName1)  
 * definiert ein Konzept CName1 in environment EnvName und modal context MS. 
 *-------------------------------------------*/

sb_primconcept(EnvName,MS,CName):-
	   environment(EnvName,Env,_),
	   (MS = [] ; MS = [_|_]),
	   defprimconcept(EnvName,MS,CName),
	   !.

/*-----------------------------------------------------------------------------
 * Der 4-stellige Konstruktor sb_primconcept(EnvName,MS,CName1,SpecListe) wird aktiviert 
 * mit expliziter Angabe von Environment=EnvName und modal context =MS.
 *-----------------------------------------*/

sb_primconcept(EnvName,MS,CName1,[]) :-
	!.
sb_primconcept(EnvName,MS,CName1,[X|T]):-
	make_primconcept(EnvName,MS,CName1,X),
        sb_primconcept(EnvName,MS,CName1,T),
	!.

/*------------------------------------------------------------------------------
 * make_defconcept(EnvName,MS,CName1,restr-inh(RName1,restricts(RName2,
 *                                          range(CName2,CNameDef))),CNameDom)
 * 
 * definiert ein Konzept CName1, an dem eine Rolle RName2  auf den Rollenfueller
 * CName2 (range) restrigniert wird.
 * Und zwar in evironment=EnvName und modal context MS. 
 * Der neuen Rolle wird das Symbol RName1 zugewiesen,
 * die Domain-Menge dieser neuen Rolle (CNameDom) ist gerade eine Teilmenge von 
 * CName1.
 *----------------------------------------*/

make_defconcept(EnvName,MS,CName1,restrict_inh(RName1, restricts(RName2 ,
				     range(CName2,CNameDef))),CNameDom):-
	make_defconcept(EnvName,MS,CName1,'restr-inh'(RName1, restricts(RName2 ,
                        range(CName2,CNameDef))),CNameDom).
make_defconcept(EnvName,MS,CName1,'restrict-inh'(RName1, restricts(RName2 ,
				     range(CName2,CNameDef))),CNameDom):-
	make_defconcept(EnvName,MS,CName1,'restr-inh'(RName1, restricts(RName2 ,
                        range(CName2,CNameDef))),CNameDom).

make_defconcept(EnvName,MS,CName1,'restr-inh'(RName1, restricts(RName2 ,
				     range(CName2,CNameDef))),CNameDom):-
	environment(EnvName,Env,_),
	defrole(EnvName,MS,RName1 , restr(RName2 , CName2)),
	gensym(concept,CNameDom),
	defconcept(EnvName,MS,CNameDom ,some(RName2 ,top)),
%	defprimconcept(EnvName,MS,and([some(inverse(RName1),top),
%				       naf(not(CNameDef))]),CNameDef),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleDomain(Env,MS,RName1,CNameDom) :- G1)),
	assertz((roleRange(Env,MS,RName1,CName2) :- G1)),
	assertz((roleDefault(Env,MS,RName1,CNameDef) :- G1)),
	assertz((roleTripel(Env,MS,RName1,CNameDom,CName2,CNameDef))).
    
/*----------------------------------------------------------------------------
 * make_defconcept(EnvName,MS,CName1, nr(RName1,MinNr,MaxNr,DefNr),CNameDom)
 * definiert ein Konzept CName1, an dem eine "number restriction" stattfindet:
 * die Rolle RName1 wird bzgl. ihrer Kardinalitaet restrigniert auf
 * (MinNr,MaxNr,DefNr),
 * und zwar in  environment=EnvName und modal context=MS.
 *-----------------------------------------*/


make_defconcept(EnvName,MS,CName1 , nr(RTerm, MinNr,MaxNr,DefNr),CNameDom):-
	environment(EnvName,Env,_),
	expand_role(EnvName,MS,RTerm,RName1,CNameDomT,CNameT,CNameDefT),
	gensym(concept,CNameDom),
	defconcept(EnvName,MS,CNameDom, and([atleast(MinNr,RName1),atmost(MaxNr,RName1)])),
%	defconcept(EnvName,MS,CNameDom, some(RName1,top)), 
%	gensym(concept,CNameDef),
%	defconcept(EnvName,MS,CNameDef, and([atleast(DefNr,RName1),atmost(DefNr,RName1)])),
%	defprimconcept(EnvName,MS,and([some(inverse(RName1)),naf(not(CNameDef))]),CNameDef),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleNr(Env,W1,RName1,MinNr,MaxNr) :- G1)),
	assertz((roleDefNr(Env,W1,RName1,DefNr) :- G1)),
	assertz((roleAll(Env,W1,RName1,CNameDomT,CNameT,CNameDefT,MinNr,MaxNr,DefNr) :- G1)).

make_defconcept(EnvName,MS,CName1 , necres(RTerm, nec),CNameDom):-
	!,
	environment(EnvName,Env,_),
	expand_role(EnvName,MS,RTerm,RName1,_,_,_),
	gensym(concept,CNameDom),
	defconcept(EnvName,MS,CNameDom,atleast(1,RName1)),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleDomain(Env,W1,RName1,CNameDom) :- G1)),
	!.
make_defconcept(EnvName,MS,CName1 , necres(RTerm, _),CNameDom):-
	!,
	environment(EnvName,Env,_),
	expand_role(EnvName,MS,RTerm,RName1,_,_,_),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleDomain(Env,W1,RName1,CNameDom) :- G1)),
	!.


expand_role(_,_,RTerm,RTerm,_,_,_) :-
	atomic(RTerm),
	!.
expand_role(EnvName,MS1,RTerm,RName1,CName1,CName2,CNameDef) :-
	RTerm = sb_primelemrole(EnvName,MS2,RName1, 'domain-range'(CName1,CName2,CNameDef)),
	!,
	append(MS1,MS2,MS3),
	sb_primelemrole(EnvName,MS3,RName1, 'domain-range'(CName1,CName2,CNameDef)).
expand_role(EnvName,MS1,RTerm,RName1,CName1,CName2,CNameDef) :-
	RTerm = sb_primelemrole(MS2,RName1, 'domain-range'(CName1,CName2,CNameDef)),
	!,
	append(MS1,MS2,MS3),
	sb_primelemrole(MS3,RName1, 'domain-range'(CName1,CName2,CNameDef)).
expand_role(EnvName,MS1,RTerm,RName1,CName1,CName2,CNameDef) :-
	RTerm = sb_primelemrole(RName1, 'domain-range'(CName1,CName2,CNameDef)),
	!,
	sb_primelemrole(MS1,RName1, 'domain-range'(CName1,CName2,CNameDef)).
expand_role(_,_,RTerm,RTerm,_,_,_).


/*-----------------------------------------------------------------------------
 * elem(I,Liste,H) nimmt das I-te Element einer Liste und sieht so aus:
 *-----------------------------------------*/


elem(1,[H|T],H).
elem(I,[H|T],X):- Hilf is I-1,
		  elem(Hilf,T,X).


/*-----------------------------------------------------------------------------
 * Der 2-stellige Konstruktor sb_defconcept(CName1,SpecListe) hat als Argumente ein
 * CName1 und eine Liste von bis jetzt eingef"uhrten Pr"adikaten wie:
 * [supers(Liste von concepts) , restr-inh(...) , nr(...)], es werden dann jeweils die
 * entsprechenden Hilfskonstruktoren mit dem einzelnen Listenelement als aktueller Parameter 
 * aktiviert bzw. aufgerufen und zwar in current environment und [] als modal context.
 *-----------------------------------------*/

sb_defconcept(C1,SpecList):- 
	getCurrentEnvironment(EnvName),
	sb_defconcept(EnvName,[],C1,SpecList),
	!.

/*-----------------------------------------------------------------------------
 * Der 3-stellige Konstruktor sb_defconcept(A,CName1,SpecListe) funktioniert analog
 * nur in A= Environment und [] als modal context, bzw. in current environment und
 * A=MS als modal context.
 *-----------------------------------------*/


sb_defconcept(A,C1,SpecList) :- 
	environment(A,_,_),
	!,
	sb_defconcept(A,[],C1,SpecList),
	!.
sb_defconcept(A,C1,SpecList) :-
	(A = [] ; A = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_defconcept(EnvName,A,C1,SpecList),
	!.

/*-----------------------------------------------------------------------------
 * Der 4-stellige Konstruktor sb_defconcept(EnvName,MS,CName1,SpecListe) wird aktiviert 
 * mit expliziter Angabe von Environment=EnvName und modal context =MS.
 *-----------------------------------------*/


sb_defconcept(EnvName,MS,C1,SpecList) :-
	sb_defconcept(EnvName,MS,C1,SpecList,[]).

sb_defconcept(EnvName,MS,C1,[],[]) :-
	!.
sb_defconcept(EnvName,MS,C1,[],CList) :-
	!,
	defconcept(EnvName,MS,C1,and(CList)),
	!.
sb_defconcept(EnvName,MS,C1,[supers(L)|SpecList],CList1) :-
	!,
	append(L,CList1,CList2),
	sb_defconcept(EnvName,MS,C1,SpecList,CList2).
sb_defconcept(EnvName,MS,C1,[Spec1|SpecList],CList1) :-
	make_defconcept(EnvName,MS,C1,Spec1,Concept),
	sb_defconcept(EnvName,MS,C1,SpecList,[Concept|CList1]).
sb_defconcept(EnvName,MS,C1,supers(L),[]) :-
	!,
	defconcept(EnvName,MS,C1,and(L)).

/*---------------------------------------------------------------------------
 * sb_primelemrole(RName1,domain-range(CName1,CName2,CNameDef))
 * definiert eine neue generelle Rolle RName1 mit CName1 als domain, CName2 
 * als range und CNameDef als "default value restriction" in modal context [].
 *------------------------------------------*/


sb_primelemrole(RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	getCurrentEnvironment(EnvName),
	sb_primelemrole(EnvName,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).

/*---------------------------------------------------------------------------
 * sb_primelemrole(X,RName1,domain-range(CName1,CName2,CNameDef))
 * definiert eine neue generelle Rolle RName1 mit CName1 als domain, CName2 
 * als range und CNameDef als "default value restriction" in modal context []
 * und X=environment bzw. in modal context X=MS und current environment.
 *------------------------------------------*/


sb_primelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	environment(X,_,_),
	sb_primelemrole(X,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).
sb_primelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	(X = [] ; X = [_|_]),
	getCurrentEnvironment(EnvName),
	sb_primelemrole(EnvName,X,RName1, 'domain-range'(CName1,CName2,CNameDef)).

/*---------------------------------------------------------------------------
 * sb_primelemrole(EnvName,MS,RName1,domain-range(CName1,CName2,CNameDef))
 * definiert eine neue generelle Rolle RName1 mit CName1 als domain, CName2 
 * als range und CNameDef als "default value restriction" in modal context MS 
 * und environment EnvName.
 *------------------------------------------*/

sb_primelemrole(EnvName,MS,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	environment(EnvName,Env,_),
	defprimconcept(EnvName,MS,CName1,some(RName1,top)),
	defprimconcept(EnvName,MS,some(inverse(RName1),top),CName2),
%	defprimconcept(ENvName,MS,and([CName2,naf(not(CNameDef))]),CNameDef),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleDomain(Env,W1,RName1,CName1) :- G1)),
	assertz((roleRange(Env,W1,RName1,CName2) :- G1)),
	assertz((roleDefault(Env,W1,RName1,CNameDef) :- G1)),
	assertz((roleTripel(Env,W1,RName1,CName1,CName2,CNameDef) :- G1)),
	!.

/*----------------------------------------------------------------------------
 * sb_defelemrole(RName1,restricts(RName2,range(CName1,CNameDef)))
 * restrigniert die Rolle RName2 bzgl. des Rollenfuellers, auf CName1,
 * in modal context [] und current environment.
 * (CNameDef gibt den Default-Rollenfueller an).
 *------------------------------------------*/

sb_defelemrole(RName1, restricts(RName2, range(CName1,CNameDef))):-
	getCurrentEnvironment(EnvName),
	sb_defelemrole(EnvName,[],RName1,restricts(RName2,range(CName1,CNameDef))),
	!.

/*----------------------------------------------------------------------------
 * sb_defelemrole(X,RName1,restricts(RName2,range(CName1,CNameDef)))
 * restrigniert die Rolle RName2 bzgl. des Rollenfuellers, auf CName1,
 * in modal context [] und X=environment bzw. in modal context X=MS und 
 * current environment.
 *------------------------------------------*/


sb_defelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
	environment(X,_,_),
	!,
	sb_defelemrole(X,[],RName1, restricts(RName2, range(CName1,CNameDef))).
sb_defelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
	(X = [] ; X = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_defelemrole(EnvName,X,RName1, restricts(RName2, range(CName1,CNameDef))).

/*----------------------------------------------------------------------------
 * sb_defelemrole(EnvName,MS,RName1,restricts(RName2,range(CName1,CNameDef)))
 * restrigniert die Rolle RName2 bzgl. des Rollenfuellers, auf CName1,
 * in modal context MS und  environment EnvName. 
 *------------------------------------------*/


sb_defelemrole(EnvName,MS,RName1, restricts(RName2, range(CName1,CNameDef))):-
	environment(EnvName,Env,_),
	defrole(EnvName,MS,RName1,restr(RName2,CName1)),
%	defprimconcept(EnvName,MS,and([some(inverse(RName1),top),
%                                      naf(not(CNameDef))]),CNameDef),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleRange(Env,MS,RName1,CName1) :- G1)),
	assertz((roleDefault(Env,MS,RName1,CNameDef) :- G1)),
	!.



/*----------------------------------------------------------------------------
 * sb_disjoint(CName1,CName2)
 * definiert generelle Konzepte CName1,CName2 als disjunkt.
 *-------------------------------------------*/

		     
sb_disjoint(CName1,CName2):- 
%	defprimconcept(CName1,not(CName2)),
	defprimconcept(CName2,not(CName1)),
	!.


sb_disjoint(X,CName1,CName2):- 
%	defprimconcept(X,CName1,not(CName2)),
	defprimconcept(X,CName2,not(CName1)),
	!.


sb_disjoint(EnvName,MS,CName1,CName2):- 
%	defprimconcept(EnvName,MS,CName1,not(CName2)),
	defprimconcept(EnvName,MS,CName2,not(CName1)),
	!.



/*----------------------------------------------------------------------------
 * sb_expand(CName1,CName2)
 * erlaubt die Erweiterung der Definition eines bereits existierenden Konzeptes,
 * der Konstruktor fuegt zu einem bestehenden Konzept ein weiteres "Vaterkonzept
 * hinzu, die Moeglichkeit der Erweiterung ist also analog zur Definition der 
 * Konzepte.
 *------------------------------------------*/

 /*? sb_expand(CName1,CName2):- defprimconcept(CName1,CName2)??*/






make_irole(EnvName,MS,ICName1,irole(RName,iname(IRName),
                                    nr(MinNr,MaxNr,DefNr))):-
	environment(EnvName,Env,_),
%       defprimrole(EnvName,MS,IRName,restr(RName,
%	                                    and([atleast(MinNr,RName),
%			  		    atmost(MaxNr,RName),
%					    some(inverse(RName),top)]))),
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	assertz((roleNr(Env,W1,IRName,MinNr,MaxNr) :- G1)),
	assertz((roleDefNr(Env,W1,IRName,DefNr) :- G1)).
                           


make_irole(EnvName,MS,ICName1,irole(RName,iname(IRName),vr(ICName2))) :-
        environment(EnvName,Env,_),
%	gensym(concept,CName2),
%       defprimrole(EnvName,MS,IRName, restr(RName,
%                 			     CName2)),
	getRoleDomain(Env,MS,RName,CDom),
	getRoleRange(Env,MS,RName,CRang),
	!,
	consistCheck(EnvName,MS,ICName1,CDom),
	!,
	consistCheck(EnvName,MS,ICName2,CRang),
	!,
%	consistCheck(EnvName,MS,ICName2,CName2),
	assert_ind(EnvName,MS,ICName1,ICName2,IRName).

constructRestriction(RName,[],[top]) :-
	!.
constructRestriction(RName,[nr(MinNr,MaxNr,DefNr)|L1],
                     [atleast(MinNr,top), atmost(MaxNr,top) | L2]) :-
	constructRestriction(RName,L1,L2),
	!.
constructRestriction(RName,[vr(ICName2)|L1],[ICName2|L2]) :-
	constructRestriction(RName,L1,L2),
	!.
     
 /*-------------------------------------------------------------------------------
  * make_defelem(ICName,isa(CName))
  * individualisiert ein Konzept CName mit dem Instanz-Namen ICName vom Typ string,
  * d.h. es wird das Abox-Element ICName zu Konzept hinzugefuegt und zwar in modal
  * context [] bzw MS.
  *-----------------------------------------*/


consistCheck(Env,MS,IC,Concept) :- 
% vor dem Test die Normalform von not(Concept) mittels
% normalizeNot(not(Concept),NotConcept)
% bilden und dann
% sb_ask(Env,MS,(isa(IC,NotConcept)))
% aufrufen
			 normalizeNot(not(Concept),NotConcept),
			 sb_ask(Env,MS,(isa(IC,NotConcept))),
			 nl,
			 write('--- impossible --- '),
			 !,
			 fail.

			    



consistCheck(Env,MS,IC,Concept) :-
			          !.



make_defelem(EnvName,MS,ICName,isa(CName)):-
	  consistCheck(EnvName,MS,ICName,CName),
	  assert_ind(EnvName,MS,ICName,CName),
	  !.





/*--------------------------------------------------------------------------------
 * make_defelem(EnvName,MS,ICName1,irole(RName,iname(IRName),[nr(MinNr,MaxNr,DefNr),vr(ICName2)]))
 * erzeugt eine Instanz ICName1 fuer ein Konzept, an dem auch die Rolle RName
 * individualisiert wird mit dem Instanznamen IRName,
 * der Kardinalitaet der indiv. Rolle nr(MinNr,MaxNr,DefNr),
 * dem Rollenfueller der indiv. Rolle vr(ICName2),
 * Und zwar in modal context MS und environment=EnvName.
 *------------------------------------------*/
 



make_defelem(EnvName,MS,ICName1,irole(RName,iname(IRName),SpecList)) :-
	constructRestriction(IRName,SpecList,RestrList),
	defprimrole(EnvName,MS,IRName,restr(RName,and(RestrList))),
        make_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),SpecList)).

make_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),[])).
make_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),[X|T])) :-
	make_irole(EnvName,MS,ICName1,irole(RName,iname(IRName),X)),
	make_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),T)).


/*--------------------------------------------------------------------------------
 * sb_defelem(ICName1,ISpecListe)
 * ISpecListe=[isa(...),irole(iname(...)nr(...)vr(...))]
 * erzeugt eine Instanz ICName1 fuer ein Konzept, an dem auch die Rolle RName
 * individualisiert werden kann mit dem Instanznamen IRName,
 * der Kardinalitaet der indiv. Rolle nr(MinNr,MaxNr,DefNr),
	* dem Rollenfueller der indiv. Rolle vr(ICName2),
 * Und zwar in modal context [] und current environment.
 * Domain dieser Rolle ist eine Teilmenge von indiv. Konzept ICName1.
 *------------------------------------------*/



sb_defelem(ICName1,SpecListe) :-
	getCurrentEnvironment(EnvName),
	sb_defelem(EnvName,[],ICName1,SpecListe).



/*-----------------------------------------------------------------------------
 *Wie oben, nur in A=environment und modal context [], bzw. in current environment
 *und modal context A=MS.
 *-----------------------------------------*/


sb_defelem(A,ICName1,[]).

sb_defelem(A,ICName1,SpecList) :-
	environment(A,_,_),
	!,
	sb_defelem(A,[],ICName1,SpecList).
sb_defelem(A,ICName1,SpecList) :-
	(A = [] ; A = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_defelem(EnvName,A,ICName1,SpecList).


/*-----------------------------------------------------------------------------
 *Wie oben, nur explizit in environment EnvName und modal context MS.
 *-----------------------------------------*/


sb_defelem(EnvName,MS,ICName1,[]).
 
sb_defelem(EnvName,MS,ICName1,[X|T]):-
	make_defelem(EnvName,MS,ICName1,X),
	sb_defelem(EnvName,MS,ICName1,T). 


/************************ sb_ask-Selektoren fuer die TBox **********************/
 

sb_ask(Q) :-
	getCurrentEnvironment(EnvName),
	sb_ask(EnvName,[],Q).

sb_ask(E,Q) :-
	environment(E,_,_),
	!,
	sb_ask(E,[],Q).
sb_ask(M,Q) :-
	(M = [] ; M = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_ask(EnvName,M,Q).

/*----------------------------------------------------------------------
 *
 * sb_ask(supers(CName1,CName2)))
 *
 * ueberprueft, ob CName1 ein direktes Subkonzept von CName2 ist.
 *
 *
 *-----------------------------------------------------------------------*/


 sb_ask(EnvName,MS,(supers(CName1,CName2))) :-
	getDirectSuperConcepts(EnvName,MS,CName1,CL),
	member(CName2,CL).

/*---------------------------------------------------------------------------
 * sb_ask(EnvName,MS,(supers*(CName1,CName2)))
 *
 * ueberprueft auch transitive supers-Beziehungen (Subsumptionsbeziehungen) zwischen
 * Subkonzept CName1 und Superkonzept CName2.
 *-----------------------------------------*/

 sb_ask(EnvName,MS,(supers*(CName1,CName2))) :-
	getAllSuperConcepts(EnvName,MS,CName1,CL),
	member(CName2,CL).


/*------------------------------------------------------------------------------
 * sb_ask(EnvName,MS,(role(RName,CNameDom,CNameRan)))
 *
 * dieser Selektor dient zum Zugriff bzw. zur Anfrage nach Rollenbeziehungen
 * zwischen zwei Konzepten, oder aber auch nach Domain und Range einer Rolle.
 *--------------------------------------------*/

 sb_ask(EnvName,MS,(role(RName,Subconcept,CNameRan))) :-
	  environment(EnvName,Env,_),
	sb_ask(EnvName,MS,(supers*(Subconcept,Superconcept))),
	getRoleDomain(Env,MS,RName,Superconcept),
	getRoleRange(Env,MS,RName,CNameRan).

 sb_ask(EnvName,MS,(role(RName,CNameDom,CNameRan))) :-
	  environment(EnvName,Env,_),
	  !,
	getRoleDomain(Env,MS,RName,CNameDom),
	getRoleRange(Env,MS,RName,CNameRan).



/*-----------------------------------------------------------------------------
 * sb_ask(EnvName,MS,(roleDef(RName,CNameDef)))
 *
 * dient zur Anfrage bzgl. DefaultRange einer Rolle.
 *--------------------------------------------*/


 sb_ask(EnvName,MS,(roleDef(RName,CNameDef))) :-   
	  environment(EnvName,Env,_),
	  !,
	getRoleDefault(Env,MS,RName,CNameDef).

/*----------------------------------------------------------------------------
 * sb_ask(EnvName,MS,(roleNr(RName,MinNr,MaxNr)))
 *
 * der Selektor dient zum Zugriff auf die Number Restriction einer Rolle RName
 * am einem Konzept CName.
 *-------------------------------------------*/


 sb_ask(EnvName,MS,(roleNr(RName,MinNr,MaxNr))) :-  
	  environment(EnvName,Env,_),
	  !,
	getRoleNr(Env,MS,RName,MinNr,MaxNr).
				       
                                       
              

/*----------------------------------------------------------------------------
 * sb_ask(EnvName,MS,(roleDefNr(RName,DefNr)))
 *
 * der Selektor dient zum Zugriff auf die Default- Number Restriction einer Rolle
 * RName am einem Konzept CName.
 *-------------------------------------------*/


 sb_ask(EnvName,MS,(roleDefNr(RName,DefNr))) :-  
	  environment(EnvName,Env,_),
	  !,
	getRoleDefNr(Env,MS,RName,DefNr).



/*----------------------------------------------------------------------------
 ************************* sb_ask-Selektoren fuer die ABox ***********************
 *
 * sb_ask(EnvName,MS,(isa(ICName,CName)))
 *
 * ermoeglicht den Zugriff Initial.-Beziehung einer Instanz ICName zum entsprech-
 * ende generellen Konzept CName.
 *------------------------------------------*/


 sb_ask(EnvName,MS,(isa(ICName,CName))) :- 
	ask(EnvName,MS,elementOf(ICName,CName),_).


sb_ask(EnvName,MS,(attributes(CN,Attribute,Value))) :-
	attribute(concept,EnvName,MS,CN,[Attribute,Value]).
sb_ask(EnvName,MS,(attributes(CN,RN,Attribute,Value))) :-
	attribute(role,EnvName,MS,[CN,RN],[Attribute,Value]).

sb_ask(EnvName,MS,(subrole(Subrole,Superrole))) :-
	getDirectFatherRoles(EnvName,MS,Subrole,RL),
	member(Superrole,RL).

sb_ask(EnvName,MS,(irole(R,X,Y))) :-
	environment(EnvName,Env,_),
	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
	EqLiteral = eq(Env,rn(_AX,_RN,_S,_O),modal(W1),Y,CON,A1,X,Pc,call(_CALLS),ab(noAb),Exp,Gd,Od,L1,L2,Anc1,Anc2,Y,_PT),
%	EqLiteral = eq(Env,rn(_AX,_RN,_S,_O),modal(W1),Y,app((FF:R),X),hyp(_HYPS),ab(noAb),call(_CALLS),_PT),
	call(G1),
	call(EqLiteral).


%----------------------------------------------------------------------
% sb_ask(EnvName,MS,(allRoles(+CName,-Info)))
% Arguments: Cname ConceptName
%	     Info is a list consisting of lists with elements:
%               Rnames role-name
%	        Cnames domain 
% 	        Min    Minimalnr. der role
%              	Max    Maximalnr. der role
%           	Defnr  Defaultnr. der role  
%
% liefert alle rolen,domains,Minnr,Maxnr,Defnr die von einem konzept ausgehen
% bsp : sb_ask(env,ms,allRoles(golf,X))    X = ((has_part golf windshield 1 1 1)(consumes golf gasoline 1 1 1))
% Author: Patrick Brandmeier


sb_ask(EnvName,MS,(allRoles(CName,Info_list))) :-
	setofOrNil(Info,[RName,CNameRan,Info]^(sb_ask(EnvName,MS,(role(RName,CName,CNameRan))),
	 	 sb_ask(EnvName,MS,(allRoles(RName,CName,CNameRan,Info)))),Info_list).
	
sb_ask(EnvName,MS,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan,Min,Max,Defnr]))) :-
	sb_ask(EnvName,MS,(roleNr(RName,Min,Max))),
	sb_ask(EnvName,MS,(roleDefNr(RName,Defnr))),
	!.
sb_ask(EnvName,MS,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan]))) :-
	!.


%----------------------------------------------------------------------
% sb_fact(EnvName,MS,(all_roles(+CName,-Info)))
% Arguments: Cname ConceptName
%	     Info is a list consisting of lists with elements:
%               Rnames role-name
%	        Cnames domain 
% 	        Min    Minimalnr. der role
%              	Max    Maximalnr. der role
%           	Defnr  Defaultnr. der role  
%
% liefert alle rolen,domains,Minnr,Maxnr,Defnr die von einem konzept ausgehen
% bsp : sb_fact(env,ms,allRoles(golf,X))    X = ((has_part golf windshield 1 1 1)(consumes golf gasoline 1 1 1))
% Author: Patrick Brandmeier


sb_fact(EnvName,MS,(allRoles(CName,Info_list))) :-
	setofOrNil(Info,[RName,CNameRan]^(sb_fact(EnvName,MS,(role(RName,CName,CNameRan))),
	 	 sb_fact(EnvName,MS,(allRoles(RName,CName,CNameRan,Info)))),Info_list).
	
sb_fact(EnvName,MS,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan,Min,Max,Defnr]))) :-
	sb_fact(EnvName,MS,(roleNr(RName,Min,Max))),
	sb_fact(EnvName,MS,(roleDefNr(RName,Defnr))),
	!.
sb_fact(EnvName,MS,(allRoles(RName,CName,CNameRan,[RName,CName,CNameRan]))) :-
	!.


%----------------------------------------------------------------------
% sb_ask(EnvName,MS,(direct_super_role(+-RName,+-CNameDom,+-CNameRan,+-SRName,+-SCNameDom,+-SCNameRan))) :-
% Arguments: Rname RoleName
%	     CNameDom Domain
%            CNameRan Range 
%	     SRName Super-Role-Name
% 	     SRNameDom Super-Role_domain
%            SRNameRan Super-Role-range
%
% ist erfuellt wenn (SRName SCNameDom SCNameRan) die direkte super-rolle von (RName CNameDom CNameRan) ist
%
% Author: Patrick Brandmeier

sb_ask(EnvName,MS,(direct_super_role(RName,CNameDom,CNameRan,RName,SCNameDom,CNameRan))) :-
	var(CNameDom),
	getDirectSubConcepts(EnvName,MS,SCNameDom,CL),
	member(CNameDom,CL),
	!.

sb_ask(EnvName,MS,(direct_super_role(RName,CNameDom,CNameRan,RName,SCNameDom,CNameRan))) :-
	sb_ask(EnvName,MS,(supers(CNameDom,SCNameDom))),
	!.
%	sb_ask(EnvName,MS,(role(SRName,SCNameDom,SCNameRan))),	
%	!.


%----------------------------------------------------------------------
% sb_ask(EnvName,MS,(risa(+-IRName,+-ICNameDom,+-ICNameRan,+-GRName,+-GCNameDom,+-GCNameRan))) :-
% Arguments: IRname    individ.-RoleName
%	     ICNameDom individ.-Domain
%            ICNameRan individ.-Range 
%	     GRName    genereller Super-Role-Name
% 	     GRNameDom genereller Super-Role_domain
%            GRNameRan genereller Super-Role-range
% ist erfuellt wenn (GRName GCNameDom GCNameRan) die generelle - rolle von (IRName ICNameDom ICNameRan) ist
%  
% Author: Patrick Brandmeier

sb_ask(EnvName,MS,(risa(IRName,ICNameDom,ICNameRan,GRName,GCNameDom,GCNameRan))) :-
	environment(EnvName,Env,_),
	getConceptElement(Env,MS,ICNameDom,GCNameDom),
	sb_ask(EnvName,MS,role(GRName,GCNameDom,GCNameRan)).

sb_ask(EnvName,MS,(risa(IRName,ICNameDom,ICNameRan,IRName,GCNameDom,ICNameRan))) :-
	environment(EnvName,Env,_),
	getConceptElement(Env,MS,ICNameDom,GCNameDom),
	!.

%----------------------------------------------------------------------
% sb_ask(EnvName,MS,(rall(+-RName,+-CNameDom,+-CNameRan,CNameDef,+-MinNr,+-MaxNr,+-DefNr))) :-
% Arguments: Rname    RoleName
%	     CNameDom RoleDomain
%            CNameRan RoleRange 
%	     CNameDef ConceptNameDef.
% 	     MinNR    Minim.Nr
%            MaxNr    Maxi. Nr
% 
% ist erfuellt wenn 
%


sb_ask(EnvName,MS1,(rall(RName,CNameDom,CNameRan,CNameDef,MinNr,MaxNr,DefNr))) :-
	environment(EnvName,Env,_),
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	convertMS(negative,Env,[[],true],MS3,[],[W1,G1],_),
	call(G1),
	roleAll(Env,W1,RName,CNameDom,CNameRan,CNameDef,MinNr,MaxNr,DefNr),
	!.


/*-----------------------------------------------------------------------------
 */

sb_attributes(CN,AList) :-
	getCurrentEnvironment(EnvName),
	sb_assert_attributes(concept,EnvName,[],CN,AList).
sb_attributes(A1,CN,AList) :-
	environment(A1,_,_),
	!,
	sb_assert_attributes(concept,A1,[],CN,AList).
sb_attributes(A1,CN,AList) :-
	(A1 = [] ; A1 = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_assert_attributes(concept,EnvName,A1,CN,AList).
sb_attributes(CN,R,AList) :-
	getCurrentEnvironment(EnvName),
	sb_assert_attributes(role,EnvName,[],[CN,R],AList).
sb_attributes(A1,A2,CN,AList) :-
	environment(A1,_,_),
	(A2 = [] ; A2 = [_|_]),
	!,
	sb_assert_attributes(concept,A1,A2,CN,AList).
sb_attributes(EnvName,CN,RN,AList) :-
	sb_assert_Attributes(role,EnvName,[],[CN,RN],AList).
sb_attributes(EnvName,MS,CN,RN,AList) :-	
	sb_assert_attributes(role,EnvName,MS,[CN,RN],AList).

sb_assert_attributes(Type,EnvName,MS,Spec,[]) :-
	!.
sb_assert_attributes(Type,EnvName,MS,Spec,[Pair|AList]) :-
	assertz(attribute(Type,EnvName,MS,Spec,Pair)),
	sb_assert_attributes(Type,EnvName,MS,Spec,AList).

/*------------------------------------------------------------------------------
 * Die 2- bis 4-stellige Funktion sb_unprimconcept([Env],[MS],CN,SpecListe) erwartet 
 * als letztes Argument  eine Liste von Konstruktoren: supers,restr-inh,nr, die irgendwann
 * mit sb_primconcept bzgl. CN definiert wurde.
 * Die Fkt. sb_unprimconcept eliminiert die entsprechenden Subsumption-Beziehungen wieder 
 * und l"oscht ebenfalls Eintr"age der Rollen wie: roleDomain,roleNr,etc..
 *-----------------------------*/

 

 unmake_primconcept(EnvName,MS,CN,supers([])) :- !.

 unmake_primconcept(EnvName,MS,CN,supers([X|T])) :-
			    environment(EnvName,Env,_),
			    conceptSubsets(Env,_user,MS,CN,X,AX),
			    undefprimconcept(EnvName,MS,CN,X),
			    unmake_primconcept(EnvName,MS,CN,supers(T)).

unmake_primconcept(EnvName,MS,CName1,
		   'restr-inh'(R1,restricts(R2,range(CName2,CNameDef)))):-
          unmake_primconcept(EnvName,MS,CName1,
			    restrict_inh(R1, restricts(R2 ,
							   range(CName2,CNameDef)))).



unmake_primconcept(EnvName,MS,CName1,
		   'restrict-inh'(R1,restricts(R2,range(CName2,CNameDef)))) :-
        unmake_primconcept(EnvName,MS,CName1,
			 restrict_inh(R1, restricts(R2 ,
							range(CName2,CNameDef)))).
 


unmake_primconcept(EnvName,MS,CName1,
	           restrict_inh(R1,restricts(R2,range(CName2,CNameDef)))) :-
	environment(EnvName,Env,_),
	!,
	getRoleDomain(Env,MS,R1,CNameDom),
	!,
	getRoleRange(Env,MS,R1,CName2),
	undefprimconcept(EnvName,MS,CNameDom,CName1),
	retract((roleDomain(Env,MS,R1,CNameDom) :- _)),
	retract((roleRange(Env,MS,R1,CName2) :- _)),
	retract((roleDefault(Env,MS,R1,CNameDef) :- _)),
	retract((roleTripel(Env,MS,R1,CNameDom,CName2,CNameDef))),
        !.



unmake_primconcept(EnvName,MS,CName1 , nr(R1, MinNr,MaxNr,DefNr)) :-
	environment(EnvName,Env,_),
	!,
	getRoleNr(Env,MS,R1,MinNr,MaxNr),
	!,
	getRoleDefNr(Env,MS,R1,DefNr),
	conceptEqualSets(Env,_user,MS,CNameDom,
			and([atleast(MinNr,R1),atmost(MaxNr,R1)]),AX),
        undefconcept(EnvName,MS,CNameDom,and([atleast(MinNr,R1),
	 				      atmost(MaxNr,R1)])),
        undefconcept(EnvName,MS,CNameDom,some(R1,top)),
        undefprimconcept(EnvName,MS,CNameDom,CName1),
        retract((roleNr(Env,MS,R1,MinNr,MaxNr) :- _)),
	retract((roleDefNr(Env,MS,R1,DefNr) :- _)),
	!.



/*------------------------------------------------------------------------
 * sb_unprimconcept(EnvName,MS,CName1,SpecList)
 *
 *------------------------------------------*/


sb_unprimconcept(CName1,[]) :- !.

sb_unprimconcept(CName1,[X|T]) :-
			    getCurrentEnvironment(EnvName),
			    sb_unprimconcept(EnvName,[],CName1,[X|T]),
			    !.



sb_unprimconcept(A,CName1,[X|T]) :-
			     environment(A,_,_),
			     !,
			     sb_unprimconcept(A,[],CName1,[X|T]),
			     !.


sb_unprimconcept(A,CName1,[X|T]) :-
			     (A=[] ; A=[_,_]),
			     getCurrentEnvironment(EnvName),
			     !,
			     sb_unprimconcept(EnvName,A,CName1,[X|T]).



sb_unprimconcept(EnvName,MS,CName1,[]) :- !.

sb_unprimconcept(EnvName,MS,CName1,[X|T]) :-
			     unmake_primconcept(EnvName,MS,CName1,X),
			     sb_unprimconcept(EnvName,MS,CName1,T),
			     !.




/*------------------------------------------------------------------------------
 * Die 4-stellige Fkt. unmake_defconcept(Env,MS,CN,supers(List_of_concepts)) erwartet
 * als letztes Argument  eine Liste (Menge) von Konzepten. Eine Obermenge dieser Menge wurde
 * irgendwann in AND-Verkn"upfung mit CN "aquivalent gemacht und zwar in der Form:
 *
 *    sb_defconcept(CN, [supers([C1,C2,...,Cn]),..]) ----->   defconcept(CN,and([C1,C2,..,Cn])).    
 *
 *-------------------------------------------------*/



unmake_defconcept(EnvName,MS,CName1,restrict_inh(RName1, restricts(RName2 ,
				    range(CName2,CNameDef))),CNameDom):-
        unmake_defconcept(EnvName,MS,CName1,'restr-inh'(RName1, restricts(RName2 ,
			  range(CName2,CNameDef))),CNameDom).


unmake_defconcept(EnvName,MS,CName1,'restrict-inh'(RName1, restricts(RName2 ,
					range(CName2,CNameDef))),CNameDom):-
        unmake_defconcept(EnvName,MS,CName1,'restr-inh'(RName1, restricts(RName2 ,
			  range(CName2,CNameDef))),CNameDom).


unmake_defconcept(EnvName,MS,CName1,'restr-inh'(RName1, restricts(RName2 ,
				  range(CName2,CNameDef))),CNameDom):-

        environment(EnvName,Env,_),
	!,
	roleEqualSets(Env,_user,MS,RName1,restr(RName2,CName2)),
	undefrole(EnvName,MS,RName1,restr(RName2,CName2)),
	!,
	getRoleDomain(Env,MS,RName1,CNameDom),
	!,
	conceptEqualSets(Env,_usr,MS,CNameDom,some(RName2,top)),
	undefconcept(CNameDom,some(RName2,top)),
        retract((roleDomain(Env,MS,RName1,CNameDom) :- _)),
	retract((roleRange(Env,MS,RName1,CName2) :- _)),
	retract((roleDefault(Env,MS,RName1,CNameDef) :- _)),
	retract((roleTripel(Env,MS,RName1,CNameDom,CName2,CNameDef))),
	retract((roleAll(Env,MS,RName1,CNameDom,CName2,CNameDef,_,_,_))),
	!.


unmake_defconcept(EnvName,MS,CName1, nr(RTerm,MinNr,MaxNr,DefNr),CNameDom) :-
	environment(EnvName,Env,_),
        unexpand_role(EnvName,MS,RTerm,R1),
	!,
	conceptEqualSets(Env,_usr,MS,CNameDom,and([atleast(MinNr,R1),
					           atmost(MaxNr,R1)]),AX),
        undefconcept(EnvName,MS,CNameDom,and([atleast(MinNr,R1),
						   atmost(MaxNr,R1)])),
	!,
	conceptEqualSets(Env,_usr,MS,CNameDom,some(R1,top)),
        undefconcept(EnvName,MS,CNameDom,some(R1,top)),       
	retract((roleNr(Env,MS,R1,MinNr,MaxNr) :- _)),
	retract((roleDefNr(Env,MS,R1,DefNr) :- _)),
	!.
        						   
        
	    
unmake_defconcept(EnvName,MS,CName1, necres(RTerm, nec),CNameDom):-
	!,
	environment(EnvName,Env,_),
	unexpand_role(EnvName,MS,RTerm,R1),
	conceptEqualSets(Env,_usr,MS,CNameDom,atleast(1,R1),AX),
	undefconcept(EnvName,MS,CNameDom,atleast(1,R1)),
	retract((roleDomain(Env,MS,R1,CNameDom) :- _)),
	!.



unmake_defconcept(EnvName,MS,CName1 , necres(RTerm, _),CNameDom):-
        !,
	environment(EnvName,Env,_),
	unexpand_role(EnvName,MS,RTerm,R1),
	retract((roleDomain(Env,MS,RName1,CNameDom) :- _)),
	!.


/*---------------------------------------------------------------------
 * unexpand_role(EnvName,MS,RTerm,R1)
 *
 *-------------------------------------*/


unexpand_role(_,_,RTerm,RTerm) :-
	 atomic(RTerm),
	 !.

unexpand_role(EnvName,MS,RTerm,R1) :-
         RTerm = sb_unprimelemrole(EnvName,MS,R1, 'domain-range'(C1,C2,CNameDef)),
         !,
	 call(RTerm).
	   


unexpand_role(EnvName,MS,RTerm,R1) :-
         RTerm = sb_unprimelemrole(MS,R1, 'domain-range'(C1,C2,CNameDef)),
         !,
	 call(RTerm).


unexpand_role(EnvName,MS,RTerm,R1) :-
         RTerm = sb_unprimelemrole(R1, 'domain-range'(C1,C2,CNameDef)),
         !,
	 call(RTerm).



unexpand_role(_,_,RTerm,RTerm).



/**********************************************************************
 *
 * sb_undefconcept(EnvName,MS,CName1,SpecList)
 *
 *
 */

sb_undefconcept(CName1,[]):- !.

sb_undefconcept(CName1,SpecList) :-
         getCurrentEnvironment(EnvName),
	 sb_undefconcept(EnvName,[],CName1,SpecList),
	 !.



sb_undefconcept(A,CName1,SpecList) :-
         environment(A,_,_),
       	 !,
	 sb_undefconcept(A,[],CName1,SpecList),
	 !.


sb_undefconcept(A,CName1,SpecList) :-
         (A=[] ; A=[_,_]),
         getCurrentEnvironment(EnvName),
         !,
         sb_undefconcept(EnvName,A,CName1,SpecList).


sb_undefconcept(EnvName,MS,CName1,SpecList) :-
           sb_undefconcept(EnvName,MS,CName1,SpecList,[]).


sb_undefconcept(EnvName,MS,CName1,[],[]) :- !.

sb_undefconcept(EnvName,MS,CName1,[],HList) :-
	   environment(EnvName,Env,_),
	   !,
	   conceptEqualSets(Env,_user,MS,CName1,and(HList),AX),
	   undefconcept(EnvName,MS,CName1,and(HList)),
	   !.


sb_undefconcept(EnvName,MS,CName1,[supers(L)|SpecList],CL1) :-
	   !,
	   append(L,CL1,CL2),
	   sb_undefconcept(EnvName,MS,CName1,SpecList,CL2).

sb_undefconcept(EnvName,MS,CName1,[Spec1|SpecList],CL1) :-
           unmake_defconcept(EnvName,MS,CName1,Spec1,Concept),
           sb_undefconcept(EnvName,MS,CName1,SpecList,[Concept|CL1]).



sb_undefconcept(EnvName,MS,CName1,supers(L),[]) :-
           environment(EnvName,Env,_),
	   !,
	   conceptEqualSets(Env,_user,MS,CName1,and(L),AX),
	   undefconcept(EnvName,MS,CName1,and(L)).



/**********************************************************************
 *
 * sb_unprimelemrole(EnvName,MS,RName1, 'domain-range'(CName1,CName2,CNameDef)) *
 *
 */


sb_unprimelemrole(RName1, 'domain-range'(CName1,CName2,CNameDef)):-
           getCurrentEnvironment(EnvName),
	   sb_unprimelemrole(EnvName,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).



sb_unprimelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	   getCurrentEnvironment(X),
	   sb_unprimelemrole(X,[],RName1, 'domain-range'(CName1,CName2,CNameDef)).


sb_unprimelemrole(X,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	   (X = [] ; X = [_|_]),
	   getCurrentEnvironment(EnvName),
	   sb_unprimelemrole(EnvName,X,RName1, 'domain-range'(CName1,CName2,CNameDef)).




sb_unprimelemrole(EnvName,MS,RName1, 'domain-range'(CName1,CName2,CNameDef)):-
	   environment(EnvName,Env,_),
	   !,
	   conceptSubsets(Env,_usr,MS,CName1,some(RName1,top)),
	   undefprimconcept(EnvName,MS,CName1,some(RName1,top)),
	   !,
	   conceptSubsets(Env,_usr,MS,some(inverse(RName1),top)),
	   undefprimconcept(EnvName,MS,some(inverse(RName1),top),CName2),
	   retract((roleDomain(Env,MS,RName1,CName1) :- _)),
	   retract((roleRange(Env,MS,RName1,CName2) :- _)),
	   retract((roleDefault(Env,MS,RName1,CNameDef) :- _)),
	   retract((roleTripel(Env,MS,RName1,CName1,CName2,CNameDef))),
	   retract((roleAll(Env,MS,RName1,CName1,CName2,CNameDef,_,_,_))),
	   !.


/**********************************************************************
 *
 * sb_undefelemrole(RName1,restricts(RName2,range(CName1,CNameDef)))
 * 
 */


sb_undefelemrole(RName1, restricts(RName2, range(CName1,CNameDef))):-
        getCurrentEnvironment(EnvName),
	sb_undefelemrole(EnvName,[],RName1, restricts(RName2, range(CName1,CNameDef))),
	!.


sb_undefelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
	getCurrentEnvironment(X),
	!,
	sb_undefelemrole(X,[],RName1, restricts(RName2, range(CName1,CNameDef))),
        !.


sb_undefelemrole(X,RName1, restricts(RName2, range(CName1,CNameDef))):-
        (X = [] ; X = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_undefelemrole(EnvName,X,RName1, restricts(RName2, range(CName1,CNameDef))),
	!.


sb_undefelemrole(EnvName,MS,RName1, restricts(RName2, range(CName1,CNameDef))) :-
        environment(EnvName,Env,_),
	!,
	roleEqualSets(Env,_user,MS,RName1,restr(RName2,CName1)),
	undefrole(EnvName,MS,RName1,restr(RName2,CName1)),
	!,
	roleRange(Env,MS,RName1,CName1),
	!,
	roleDefault(Env,MS,RName1,CNameDef),
	retract((roleRange(Env,MS,RName1,CName1) :- _)),
	retract((roleDefault(Env,MS,RName1,CNameDef) :- _)),
	!.

/*----------------------------------------------*/


unmake_irole(EnvName,MS,ICName1,irole(RName,iname(IRName),
				      nr(MinNr,MaxNr,DefNr))):-
        environment(EnvName,Env,_),
	!,
	roleSubsets(Env,_user,MS,IRName,restr(RName,and([atleast(MinNr,RName),
				            	    atmost(MaxNr,RName),
						    some(inverse(RName),top)]))),

	undefprimrole(EnvName,MS,IRName,restr(RName,
					      and([atleast(MinNr,RName),
					      atmost(MaxNr,RName),
					      some(inverse(RName),top)]))),
        !,
	getRoleNr(Env,MS,IRName,MinNr,MaxNr),
	!,
	getRoleDefNr(Env,MS,IRName,DefNr),
	retract((roleNr(Env,MS,IRName,MinNr,MaxNr) :- _)),
	retract((roleDefNr(Env,MS,IRName,DefNr) :- _)),
	!.

unmake_irole(EnvName,MS,ICName1,irole(RName,iname(IRName),vr(ICName2))) :-
	environment(EnvName,Env,_),
	!,
	roleSubsets(Env,_user,MS,IRName,restr(RName,CName2)),
	undefprimrole(EnvName,MS,IRName, restr(RName,CName2)),
        delete_ind(EnvName,MS,ICName1,ICName2,IRName).

unmake_defelem(EnvName,MS,ICName,isa(CName)):-
	delete_ind(EnvName,MS,ICName,CName),
	!.

unmake_defelem(EnvName,MS,ICName1,irole(RName,iname(IRName),SpecList)) :-
	constructRestriction(IRName,SpecList,RestrList),
	!,
	roleSubsets(Env,_user,MS,IRName,restr(RName,and(RestrList))),
	undefprimrole(EnvName,MS,IRName,restr(RName,and(RestrList))),
	unmake_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),SpecList)).


unmake_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),[])) :-
	!.
unmake_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),[X|T])) :-
	unmake_irole(EnvName,MS,ICName1,irole(RName,iname(IRName),X)),
	unmake_defelem_list(EnvName,MS,ICName1,irole(RName,iname(IRName),T)).



/**********************************************************************
 *
 * sb_undefelem(EnvName,MS,ICName1,[X|T])
 *
 */

sb_undefelem(ICName1,SpecListe) :-
	getCurrentEnvironment(EnvName),
	sb_undefelem(EnvName,[],ICName1,SpecListe).



sb_undefelem(A,ICName1,SpecList) :-
        environment(A,_,_),
	!,
	sb_undefelem(A,[],ICName1,SpecList).

sb_undefelem(A,ICName1,SpecList) :-
	(A = [] ; A = [_|_]),
	!,
	getCurrentEnvironment(EnvName),
	sb_undefelem(EnvName,A,ICName1,SpecList).


sb_undefelem(EnvName,MS,ICName1,[]) :-
	!.

sb_undefelem(EnvName,MS,ICName1,[X|T]):-
	unmake_defelem(EnvName,MS,ICName1,X),
        sb_undefelem(EnvName,MS,ICName1,T).


/**********************************************************************
 *
 * sb_fact(EnvName,MS,elementOf(X,C),P)
 *
 */

sb_fact(P1) :-
	completeParameter([P1],EnvName,MS,Query,Proof),
	sb_fact(EnvName,MS,Query,Proof).
sb_fact(P1,P2) :-
	completeParameter([P1,P2],EnvName,MS,Query,Proof),
	sb_fact(EnvName,MS,Query,Proof).
sb_fact(P1,P2,P3) :-
	completeParameter([P1,P2,P3],EnvName,MS,Query,Proof),
	sb_fact(EnvName,MS,Query,Proof).

sb_fact(EnvName,MS,isa(X,C),Exp) :-
	retractall(hypothesis(_)),
 	environment(EnvName,Env,_),
 	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
 	getNegatedConcept(C,C1),
 	getFactQuery(Env,W1,C,X,Exp,Goal),
 	call((call(G1), Goal)),
 	atomic(X),
	allowedAnswerConcept(Env,C).
sb_fact(EnvName,MS,(role(RName,CNameDom,CNameRan)),proved(fact,basedOn(tbox))) :-
	environment(EnvName,Env,_),
	!,
	getRoleDomain(Env,MS,RName,CNameDom),
	getRoleRange(Env,MS,RName,CNameRan).
sb_fact(EnvName,MS,(attributes(CN,Attribute,Value)),proved(fact,basedOn(tbox))) :-
	attribute(concept,EnvName,MS,CN,[Attribute,Value]).
sb_fact(EnvName,MS,(attributes(CN,RN,Attribute,Value)),proved(fact,basedOn(tbox))) :-
	attribute(role,EnvName,MS,[CN,RN],[Attribute,Value]).
sb_fact(EnvName,MS,irole(R,X,Y),Exp) :-
	retractall(hypothesis(_)),
	environment(EnvName,Env,_),
	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
	getFactQuery(Env,W1,Y,R,X,Exp,Goal),
	call(G1), 
	call(Goal).
	
getFactQuery(Env,W1,C0,X,Exp,Goal) :-
	constructMLHead(Env,_RN1,W1,C0,CON,A1,X,Pc,_CALLS,_D,Exp,Gd,Od,L1,L2,Anc1,Anc2,Y,PT,InHead),
%	constructMLHead(Env,_RN1,W1,C0,X,_HYPS,_D,_CALLS,PT,InHead),
	getExplanation(InHead,Exp),
	Goal = (clause(InHead,Body), ((PT == abox), call(Body))),
	!.
getFactQuery(Env,W1,Y,R,X,PT,Goal) :-
	constructEqHead(Env,_RN1,W1,Y,CON,A1,X,Pc,_CALLS,_D,Exp,Gd,Od,L1,L2,Anc1,Anc2,Y,PT,EqHead),
%	constructEqHead(Env,_RN1,W1,Y,_F,R,X,_HYPS,_D,_CALLS,PT,EqHead),
	Goal = (clause(EqHead,Body), ((PT == abox), call(Body))),
	!.

getRoleDomain(Env,MS1,RName,CDom) :-
	convertMS(negative,Env,[[],true],MS1,[],[W1,G1],_),
	clause(roleDomain(Env,W1,RName,CDom),_),
	once((call(G1),roleDomain(Env,W1,RName,CDom))).

getRoleRange(Env,MS1,RName,CRange) :-
	convertMS(negative,Env,[[],true],MS1,[],[W1,G1],_),
	clause(roleRange(Env,W1,RName,CRange),_),
	once((call(G1),roleRange(Env,W1,RName,CRange))).

getRoleDefault(Env,MS1,RName,CNameDef) :-
	convertMS(negative,Env,[[],true],MS1,[],[W1,G1],_),
	clause(roleDefault(Env,W1,RName,CNameDef),_),
	once((call(G1),roleDefault(Env,W1,RName,CNameDef))).

getRoleNr(Env,MS1,RName,MinNr,MaxNr) :- 
	convertMS(negative,Env,[[],true],MS1,[],[W1,G1],_),
	clause(roleNr(Env,W1,RName,MinNr,MaxNr),_),
	once((call(G1),roleNr(Env,W1,RName,MinNr,MaxNr))).

getRoleDefNr(Env,MS1,R1,DefNr) :-
	convertMS(negative,Env,[[],true],MS1,[],[W1,G1],_),
	clause(roleDefNr(Env,W1,R1,DefNr),_),
	once((call(G1),roleDefNr(Env,W1,R1,DefNr))).

getRoleTripel(Env,MS1,RName1,CNameDomT,CNameT,CNameDefT) :-
	convertMS(Env,[[],true],MS1,[],[W1,G1],_),
	clause(roleTripel(Env,W1,RName1,CNameDomT,CNameT,CNameDefT),_),
	once((call(G1),roleTripel(Env,W1,RName1,CNameDomT,CNameT,CNameDefT))).

getConceptElement(Env,MS1,X,C) :-
	convertMS(Env,[[],true],MS1,[],[W1,G1],_),
	clause(conceptElement(Env,_W1,_,X,C,_),_),
	once((call(G1),conceptElement(Env,W1,_,X,C,_))).
/**********************************************************************
 *
 * @(#) tellConcept.pl 1.9@(#)
 *
 */

/***********************************************************************
 *
 * assertConceptRInL(+ModalSequence,+CT1,+CT2,+AxiomName)
 * asserts membership clauses for the inclusion of CT2 into CT1 in 
 * modal context ModalSequence.
 *
 */

assertConceptRInL(Env,Name,MS,CT1,CT2) :-
	assertConceptLInR(Env,Name,MS,CT2,CT1).

/***********************************************************************
 *
 * assertConceptLInR(+ModalSequence,+ConceptName,+Constraint,+AxiomName)
 * asserts membership clauses for the inclusion of ConceptName into
 * Constraint in modal context ModalSequence.
 *
 */

assertConceptLInR(_Env,rn(_,_,_),_MS,_CN,or([])) :- 
	!.
assertConceptLInR(Env,rn(AxiomName,Source,Orientation),MS,CN,or([CT1|CTs])) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	hop_map(negate,[CT1|CTs],NCTs),
	convertInAntecedentList(Env,rn(AxiomName,_AnySource1,Orientation),
                                bodyMC(W1),headMC(W1),NCTs,X,
				HYPS,AB,CALLS,PTL,INCTs),
	convertInAntecedent(Env,rn(AxiomName,_AnySource2,Orientation),
                            bodyMC(W1),headMC(W1),
			    CN,X,HYPS,AB,CALLS,PT1,Body),
	assertOrConceptLInR(Env,rn(AxiomName,Source,Orientation),
                            MS,PT1,Body,W1,G1,X,HYPS,AB,CALLS,
                            or([CT1|CTs]),[],[],PTL,INCTs).
assertConceptLInR(_Env,rn(_,_,_),_MS,_CN,and([])) :-
	!.
assertConceptLInR(Env,rn(AxiomName,O2,Orientation2),MS,CN1,and([CN2|CTs])) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation2),
                            bodyMC(W1),headMC(W1),
			    CN1,X,HYPS,AB,CALLS,PT1,Body),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O2,Orientation2,RN2),
	convertInConsequence(Env,pr(3),RN2,MS,W1,CN2,X,HYPS,AB,CALLS,PT1,InHead2),
	constructMLMark(InHead2,Mark2),
%	asserta((InHead2 :- (cCS(CALLS,Mark2), (call(G1), once(Body))))),
	asserta((InHead2 :- (cCS(CALLS,Mark2), (call(G1), Body)))),
	assertConceptLInR(Env,rn(AxiomName,O2,Orientation2),MS,CN1,and(CTs)),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,set(Set1)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,set(Set1),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), once(Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,not(set(Set1))) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,not(set(Set1)),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), once(Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,not(D)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,not(D),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), once(Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,naf(D)) :-
	% in the consequence not and naf have the same meaning
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,not(D),X,HYPS,AB,CALLS,PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
			    bodyMC(W1),headMC(W1),
	                    CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), once(Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,all(R,D)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,all(R,D),X,HYPS,AB,CALLS,PT1,
	                     (EqLiteral, InHead1)),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (EqLiteral, Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,some(R,D)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,some(R,D),X,
	                     HYPS,AB,CALLS,PT1,(EqLiteral, InHead1)),
	constructMLMark(InHead1,Mark1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (EqLiteral, Body))))),
	gensym(rule,RuleName2),
	ruleName(AxiomName,RuleName2,system,Orientation,RN2),
	convertInConsequence(Env,pr(3),RN2,MS,W1,normal(R),X,
			     HYPS,AB,CALLS,PT2,InHead2),
	constructMLMark(InHead2,Mark2),
	asserta((InHead2 :- cCS(CALLS,Mark2), (call(G1), Body))),
	!.
assertConceptLInR(Env,rn(AxiomName,_S,Orientation),MS,CN,atleast(N,R)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	typeOfDefinition(Env,MS,R,S1),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,S1,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,atleast(N,R),X,
			     HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), once(Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,atmost(N,R)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W1),CN,X,HYPS,AB,CALLS,PT1,Body),
	typeOfDefinition(Env,MS,R,O1),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O1,Orientation,RN1),
	convertInConsequence(Env,pr(3),RN1,MS,W1,atmost(N,R),X,
			     HYPS,AB,CALLS,PT1,InHead1),
	constructConMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), once((call(G1), Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,b(MOp,P1,D)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	genagent(P1,free,P),
	C1 = rel(Env,_,m(MOp,P),W1,W2),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body),
	constructMLHead(Env,RN1,W2,D,X,HYPS,AB,CALLS,and([C1,PT1]),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (C1, Body))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,bc(MOp,C,D)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RuleName),
	ruleName(AxiomName,RuleName,O,Orientation,RN1),
	genagent(P1,free,P),
	C1 = rel(Env,_,m(MOp,P),W1,W2),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
	                    bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource2,Orientation),
			    bodyMC(W1),headMC(W2),C,P,HYPS,AB,CALLS,PT2,Body2),
	constructMLHead(Env,RN1,W2,D,X,HYPS,AB,CALLS,and([C1,PT1,PT2]),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (C1, (Body1, Body2)))))),
	!.
assertConceptLInR(Env,rn(AxiomName,_S1,Orientation),MS,CN,d(MOp,P1,D)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],WVL),
	gensym(rule,RuleName),
	typeOfDefinition(Env,MS,D,S2),
	ruleName(AxiomName,RuleName,S2,Orientation,RN1),
	gensym(wp,WP),
	WPTerm =.. [WP,WVL],
	genagent(P1,skolemize,P),
	W2 = app(WPTerm:m(MOp,P),W1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body),
	constructMLHead(Env,RN1,W2,D,X,HYPS,AB,CALLS,
			PT1,InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), Body)))),
	!.
assertConceptLInR(Env,rn(AxiomName,_S1,Orientation),MS,CN,dc(MOp,C,D)) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],WVL),
	gensym(rule,RuleName),
	typeOfDefinition(Env,MS,D,S2),
	ruleName(AxiomName,RuleName,S2,Orientation,RN1),
	gensym(wp,WP),
	WPTerm =.. [WP,WVL],
	genagent(P1,skolemize,P),
	W2 = app(WPTerm:m(MOp,P),W1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource1,Orientation),
                            bodyMC(W1),headMC(W2),CN,X,HYPS,AB,CALLS,PT1,Body1),
	convertInAntecedent(Env,rn(AxiomName,_AnySource2,Orientation),
                            bodyMC(W1),headMC(W2),C,P,HYPS,AB,CALLS,PT2,Body2),
	constructMLHead(Env,RN1,W2,D,X,HYPS,AB,CALLS,
			and([PT1,PT2]),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), (call(G1), (Body1, Body2))))),
	!.
assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,ConceptTerm) :-
	assertConceptLInR(Env,rn(AxiomName,O,Orientation),MS,CN,and([ConceptTerm])).

/***********************************************************************
 * 
 * assertOrConceptLInR(Env,MS,InHead,or([CT1|CTs]),[INCT|CTs]),AxiomName).
 *
 */

assertOrConceptLInR(_Env,rn(_,_,_),_MS,_PT1,_InHead,_W1,_G1,_X,_HYPS,_,_CALLS,
		    or([]),_FPTL,_First,_PTL,_INCTs) :-
	!.
assertOrConceptLInR(Env,rn(AxiomName,O,Orientation),
                    MS,PT2,InHead2,W1,G1,X,HYPS,AB,CALLS,or([CT1|CTs]),
	            FPTL,First,[PT1|PTL1],[INCT1|INCTs]) :-
	append([InHead2|First],INCTs,L1),
	append([PT2|FPTL],PTL1,PTL),
%	hop_map(getInExplanation,L1,PTL),
	constructBody(L1,Body),
	gensym(rule,RuleName),
	typeOfDefinition(Env,MS,CT1,S1),
	ruleName(AxiomName,RuleName,S1,Orientation,RN1),
	constructMLHead(Env,RN1,W1,CT1,X,HYPS,AB,CALLS,and(PTL),InHead1),
	constructMLMark(InHead1,Mark1),
	asserta((InHead1 :- (cCS(CALLS,Mark1), once((call(G1), Body))))),
	append(First,[INCT1],L2),
	append(FPTL,[PT1],FPTL2),
	!,
	assertOrConceptLInR(Env,rn(AxiomName,O,Orientation),
                            MS,PT2,InHead2,W1,G1,X,HYPS,AB,CALLS,or(CTs),
                            FPTL2,L2,PTL1,INCTs).

constructBody([],true) :-
	!.
constructBody([I1],I1) :-
	!.
constructBody([I1|IL],(I1,B1)) :-
	constructBody(IL,B1).


/***********************************************************************
 *
 * convertAndList(+ModalSequence,+ConceptTermList,
 *                   -X,-CallStack,-InTermStructure,+AxiomName)
 *
 */

convertAndList(_,_,_,_,[],_X,_HYPS,_,_CALLS,true,[]).
convertAndList(Env,Name,MC1,MC2,[CTerm],X,HYPS,AB,CALLS,InCTerm,[PT1]) :-
	convertInAntecedent(Env,Name,MC1,MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm).
convertAndList(Env,Name,MC1,MC2,[CTerm|CTermList],X,HYPS,AB,CALLS,
	       (InCTerm,InCTermList),[PT1|PTL]) :-
	convertInAntecedent(Env,Name,MC1,MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
	convertAndList(Env,Name,MC1,MC2,CTermList,X,
		       HYPS,AB,CALLS,InCTermList,PTL).

/***********************************************************************
 *
 * convertOrList(+ModalSequence,+ConceptTermList,
 *                   -X,-CallStack,-InTermStructure,+AxiomName)
 *
 */

convertOrList(_,_,_,[],_X,_HYPS,_AB,_CALLS,_AX,false,[]).
convertOrList(Env,Name,MC1,MC2,[CTerm],X,HYPS,AB,CALLS,InCTerm,[PT1]) :-
	convertInAntecedent(Env,Name,MC1,MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
	!.
convertOrList(Env,Name,bodyMC(W1),MC2,[CTerm|CTermList],X,
	      HYPS,AB,CALLS,
              (InCTerm ; (InCTermList)),[PT1|PTL]) :-
	HYPS = [or(H1),rl(H2),fl(H3)],
	Name = rn(AX,_S,_O),
	convertInAntecedent(Env,Name,bodyMC(W1),MC2,
                            CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
	convertOrList(Env,Name,bodyMC(W1),MC2,CTermList,X,
		      HYPS,AB,CALLS,InCTermList,PTL).
% convertOrList(Env,Name,bodyMC(W1),MC2,[CTerm|CTermList],X,
% 	      HYPS,AB,CALLS,
%               (InCTerm ; (NewH1 = [HypTerm|H1], InCTermList)),[PT1|PTL]) :-
% 	HYPS = [or(H1),rl(H2),fl(H3)],
% 	Name = rn(AX,_S,_O),
% 	convertInAntecedent(Env,Name,bodyMC(W1),MC2,
%                             CTerm,X,HYPS,AB,CALLS,PT1,InCTerm),
% 	negate(CTerm,CTermN),
% 	constructMLHead(Env,rn(AX,_RN1,_S1,_O1),W1,CTermN,X,
% 			HYPS,AB,_CALLS,_,HypMLTerm),
% 	constructMLMark(HypMLTerm,HypTerm),
% 	convertOrList(Env,Name,bodyMC(W1),MC2,CTermList,X,
%		      [or(NewH1),rl(H2),fl(H3)],AB,CALLS,InCTermList,PTL).

/***********************************************************************
 *
 * convertInAntecedent(Env,+ModalSequence,+ConceptTerm,-Variable,
 *              -Hypotheses,-CallStack,+AxiomName,-InLiteral)
 *
 */

convertInAntecedent(Env,rn(AX,S1,_O),MC1,MC2,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	(var(D) ; atomic(D)),
	!,
	constructMLCall(Env,rn(AX,_RN1,S1,_O1),MC1,MC2,
	                D,X,HYPS,AB,CALLS,PT1,InHead).
convertInAntecedent(Env,rn(AX,Source,_O),MC1,MC2,set(S1),X,HYPS,AB,CALLS,PT1,Body) :-
	constructMLCall(Env,rn(AX,_RN1,Source,_O1),MC1,MC2,
	                set(S1),X,HYPS,AB,CALLS,PT1,InHead1),
	Body = ((nonvar(S1), (nongeneric(X), member(X,S1))) ; InHead1),
	!.
convertInAntecedent(Env,rn(AX,Source,_O),MC1,MC2,not(set(S1)),X,HYPS,AB,CALLS,PT1,Body) :-
	constructMLCall(Env,rn(AX,_RN1,Source,_O1),MC1,MC2,
	                not(set(S1)),X,HYPS,AB,CALLS,PT1,InHead1),
	Body = ((nonvar(S1), (atomic(X), (nongeneric(X), not(member(X,S1)))) ; InHead1)),
	!.
convertInAntecedent(Env,Name,MC1,MC2,and(L),X,HYPS,AB,CALLS,and(PTL),Body) :-
	convertAndList(Env,Name,MC1,MC2,L,X,HYPS,AB,CALLS,Body,PTL),
	!.
convertInAntecedent(Env,Name,MC1,MC2,or(L),X,HYPS,AB,CALLS,or(PTL),Body) :-
	convertOrList(Env,Name,MC1,MC2,L,X,HYPS,AB,CALLS,Body,PTL),
	!.
convertInAntecedent(Env,rn(AX,S1,_O1),MC1,MC2,
	            not(D),X,HYPS,AB,CALLS,PT1,Body) :-
	constructMLCall(Env,rn(AX,_RN,S1,_O2),MC1,MC2,
	                not(D),X,HYPS,AB,CALLS,PT1,InHead),
	Body = InHead,
	!.
convertInAntecedent(Env,rn(AX,S1,_O1),bodyMC(MS1),MC2,
	            naf(D),X,HYPS,AB,CALLS,PT1,Body) :-
	% in the antecedent `x in naf(D) is provable' means 
	% `x in D is not provable'
	atomic(D),
	!,
	HYPS = [or(H1),rl(H2),fl(H3)],
	NewHYPS = [or(H1),rl([]),fl(H3)],
	convertInAntecedent(Env,rn(AX,S1,_O1),bodyMC(MS1),MC2,D,X,NewHYPS,
			    AB,CALLS,PT2,BodyD),
	PT1 = byDefault(in(MS1,not(D),X),hyp(NewHYPS),basedOn([])),
	constructMLHead(Env,rn(AX,_RN3,_S3,_O3),MS1,not(D),X,
			HYPS,AB,_CALLS,_,DefaultMLTerm),
	constructMLMark(DefaultMLTerm,DefaultTerm),
	L1 = addDefaultML(DefaultTerm,H3),
%	L1 = asserta(hypothesis(in(Env,modal(MS1),not(D),X,hyp(HYPS),ab(AB),PT1))),
	constructMLMark(BodyD,MarkD),
	Body = (member(MarkD,HYPS) ; (nongeneric(X), (not(BodyD), nongeneric(X), L1))),
	!.
convertInAntecedent(Env,rn(AX,S1,_O1),bodyMC(MS1),MC2,
	            naf(D),X,HYPS,AB,CALLS,PT1,Body) :-
	% in the antecedent `x in naf(D) is provable' means 
	% `x in D is not provable'
	HYPS = [or(H1),rl(H2),fl(H3)],
	NewHYPS = [or(H1),rl([]),fl(H3)],
	convertInAntecedent(Env,rn(AX,S1,_O1),bodyMC(MS1),MC2,D,X,NewHYPS,
			    AB,CALLS,PT2,BodyD),
	constructMLMark(BodyD,MarkD),
	normalizeNot(not(D),D1),
	PT1 = byDefault(in(MS1,D1,X),hyp(NewHYPS),basedOn([])),
	constructMLHead(Env,rn(AX,_RN3,_S3,_O3),MS1,D1,X,
			HYPS,AB,_CALLS,_,DefaultMLTerm),
	constructMLMark(DefaultMLTerm,DefaultTerm),
	L1 = addDefaultML(DefaultTerm,H3),
%	L1 = asserta(hypothesis(in(Env,modal(MS1),D1,X,hyp(HYPS),ab(AB),PT1))),
	Body = (nongeneric(X), (not(BodyD), nongeneric(X), L1)),
	!.
convertInAntecedent(Env,rn(AX,S1,_O1),MC1,MC2,
	            all(R,D),X,HYPS,AB,CALLS,or([and([PT2,PT1]),PT3]),
                    ((EqLiteral, Body); (InHead2; (C1, (C2, C3))))) :-
	% create a new skolem constant 
	gensymbol(skolem,[X,Y],SF),
	% construct equational literal
	constructEqCall(Env,rn(AX,_RN1,_S2,_O2),MC1,MC2,Y,SF,R,X,HYPS,AB,CALLS,PT2,EqLiteral),
	convertInAntecedent(Env,rn(AX,S1,_O3),MC1,MC2,D,Y,HYPS,AB,CALLS,PT1,Body),
	constructMLCall(Env,rn(AX,_RN4,_S4,_O4),MC1,MC2,not(normal(R)),X,HYPS,AB,CALLS,PT3,InHead2),
	MC1 = bodyMC(W1),
	C1 = closed(Env,MS,X,_,R),
	C2 = collectAllFillers(Env,W1,R,X,HYPS,D,CALLS,S),
	C3 = mapGoal(Body,Y,S),
	!.
convertInAntecedent(Env,rn(AX,S1,_O1),MC1,MC2,
	            some(R,D),X,HYPS,AB,CALLS,and([PT2,PT1]),(EqLiteral, once((Body, ground(Y,true))))) :-
	/* construct equational literal */
	constructEqCall(Env,rn(AX,_RN1,S1,_O2),MC1,MC2,Y,_FF,R,X,
			HYPS,AB,CALLS,PT2,EqLiteral),
	convertInAntecedent(Env,rn(AX,_S2,_O3),MC1,MC2,D,Y,
			    HYPS,AB,CALLS,PT1,Body),
	!.
convertInAntecedent(Env,rn(AX,_S,_O1),bodyMC(MS1),headMC(MS2),
	            atleast(N,R),X,HYPS,AB,CALLS,PT1,Body) :-
	% construct equational literal
	constructSolveConMark(rn(AX,_RN1,_S1,_O2),MS2,_FF1,R,X,'>=',N,
			 HYPS,AB,CALLS,Mark),
	Body = solveConstraint(Env,MS1,(card,app((_FF2:R),X),'>=',N),_,
			       hyp(HYPS),ab(AB),call([Mark|CALLS]),PT1),
	!.
convertInAntecedent(Env,rn(AX,_S,_O),bodyMC(MS1),headMC(MS2),
	            atmost(N,R),X,HYPS,AB,CALLS,PT1,Body) :-
	/* construct right term */
        constructSolveConMark(rn(AX,_RN1,_S1,_O1),MS2,_FF1,R,X,'=<',N,
			 HYPS,AB,CALLS,Mark),
	Body = solveConstraint(Env,MS1,(card,app((_FF2:R),X),'=<',N),_,
			       hyp(HYPS),ab(AB),call([Mark|CALLS]),PT1),
	!.
convertInAntecedent(Env,rn(AX,S1,_O),bodyMC(MSnew),headMC(MSold),
	            b(MOp,P1,D),X,HYPS,AB,CALLS,PT1,InHead) :-
	/* construct right term */
	gensym(wp,WP),
	genagent(P1,skolemize,P),
	MS1 = app(WP:m(MOp,P),MSnew),
        constructMLCall(Env,rn(AX,_RN1,S1,_O1),bodyMC(MS1),headMC(MSold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInAntecedent(Env,rn(AX,S1,_O),bodyMC(MSnew),headMC(MSold),
	            bc(MOp,C,D),X,HYPS,AB,CALLS,and([PT1,PT2]),(InHead, Body)) :-
	/* construct right term */
	gensym(wp,WP),
	genagent(P1,skolemize,P),
	MS1 = app(WP:m(MOp,P),MSnew),
        constructMLCall(Env,rn(AX,_RN1,S1,_O1),bodyMC(MS1),headMC(MSold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	convertInAntecedent(Env,rn(AX,_RN2,_S2),bodyMC(MSold),headMC(MSold),
			    C,P,HYPS,AB,CALLS,PT2,Body),
	!.
convertInAntecedent(Env,rn(AX,S1,_O),bodyMC(MSnew),headMC(MSold),
	            d(MOp,P1,D),X,HYPS,AB,CALLS,PT1,InHead) :-
	/* construct right term */
        genagent(P1,free,P),
        MS1 = app(_FF:m(MOp,P),MSnew),
        constructMLCall(Env,rn(AX,_RN1,S1,_O1),bodyMC(MS1),headMC(MSold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInAntecedent(Env,rn(AX,S1,_O),bodyMC(MSnew),headMC(MSold),
	            dc(MOp,C,D),X,HYPS,AB,CALLS,and([PT1,PT2]),(InHead, Body)) :-
	/* construct right term */
        genagent(P1,free,P),
        MS1 = app(_FF:m(MOp,P),MSnew),
        constructMLCall(Env,rn(AX,_RN1,S1,_O1),bodyMC(MS1),headMC(MSold),
	                D,X,HYPS,AB,CALLS,PT1,InHead),
	convertInAntecedent(Env,rn(AX,_RN2,_S2),bodyMC(MSold),headMC(MSold),
			    C,P,HYPS,AB,CALLS,PT2,Body),
	!.
convertInAntecedent(Env,rn(AX,S,_O),MC1,MC2,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	CON = X,
	constructMLCall(Env,rn(AX,_RN1,S,_O1),MC1,MC2,
	                D,CON,HYPS,AB,CALLS,PT1,InHead),
	!.

convertInAntecedentList(_Env,_,_,_,[],_X,_HYPS,_AB,_CALLS,[],[]) :-
	!.
convertInAntecedentList(Env,Name,MC1,MC2,[NCT],X,HYPS,AB,CALLS,[PT1],[INCT]) :-
	convertInAntecedent(Env,Name,MC1,MC2,NCT,X,HYPS,AB,CALLS,PT1,INCT),
	!.
convertInAntecedentList(Env,Name,MC1,MC2,[NCT|NCTs],X,
                        HYPS,AB,CALLS,[PT1|PTL],[INCT|INCTs]) :-
	convertInAntecedent(Env,Name,MC1,MC2,NCT,X,HYPS,AB,CALLS,PT1,INCT),
	convertInAntecedentList(Env,Name,MC1,MC2,NCTs,X,
				HYPS,AB,CALLS,PTL,INCTs).


/***********************************************************************
 *
 * convertInConsequence(Env,+ModalSequence,+ConceptTerm,-X,
 *               -Hypotheses, -CallStack, +AxiomName,
 *               -InLiteral)
 *
 */

convertInConsequence(Env,Pr,rn(AX,RN,_S,O),MS,W1,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	(var(D) ; atomic(D)),
	!,
	typeOfDefinition(Env,MS,D,S2),
	constructKBHead(Env,Pr,rn(AX,RN,S2,O),W1,D,X,HYPS,AB,CALLS,PT1,InHead).
convertInConsequence(Env,Pr,rn(AX,RN,_S,O),MS,W1,some(R,D),X,
                     HYPS,AB,CALLS,PT1,(EqLiteral, InHead)) :-
	% construct equational literal
	gensymbol(skolem,[X,Y],SF),
	constructEqCall(Env,rn(AX,_RN2,_S2,_O2),bodyMC(W1),headMC(W1),
	                Y,SF,R,X,HYPS,AB,CALLS,PT2,EqLiteral),
	typeOfDefinition(Env,MS,D,S2),
	convertInConsequence(Env,Pr,rn(AX,RN,S2,O),MS,W1,D,Y,HYPS,AB,CALLS,
                             and([PT1,PT2]),InHead),
	!.
convertInConsequence(Env,Pr,rn(AX,RN,_S,O),MS,W1,all(R,D),X,
                     HYPS,AB,CALLS,PT1,((EqCall, ground(Y,true)), InHead)) :-
	% construct equation literal
	constructEqCall(Env,rn(AX,_RN2,_S2,_O2),bodyMC(W1),headMC(W1),
	                Y,_FF,R,X,HYPS,AB,CALLS,PT2,EqCall),
	typeOfDefinition(Env,MS,D,S2),
	convertInConsequence(Env,Pr,rn(AX,RN,S2,O),MS,W1,D,Y,HYPS,AB,CALLS,
                             and([PT1,PT2]),InHead),
	!.
convertInConsequence(Env,_Pr,Name,_MS,W1,atleast(N,R),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	/* construct role term */
        constructConHead(Env,Name,W1,_FF,R,X,'>=',N,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(Env,_Pr,Name,_MS,W1,atmost(N,R),X,HYPS,AB,CALLS,PT1,InHead) :-
	/* construct role term */
        constructConHead(Env,Name,W1,_FF,R,X,'=<',N,HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(Env,Pr,rn(AX,RN,_S,O),MS,W1,not(D),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	typeOfDefinition(Env,MS,D,S2),
	constructKBHead(Env,Pr,rn(AX,RN,S2,O),W1,not(D),X,
			HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(Env,Pr,rn(AX,RN,_S,O),MS,W1,naf(D),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	% in the consequence not and naf have the same meaning
	typeOfDefinition(Env,MS,D,S2),
	constructKBHead(Env,Pr,rn(AX,RN,S2,O),W1,not(D),X,
			HYPS,AB,CALLS,PT1,InHead),
	!.
convertInConsequence(Env,Pr,rn(AX,RN,_S,O),MS,W1,set(Set1),X,
                     HYPS,AB,CALLS,PT1,InHead) :-
	typeOfDefinition(Env,MS,D,S2),
	constructKBHead(Env,Pr,rn(AX,RN,S2,O),W1,set(Set1),X,
			HYPS,AB,CALLS,PT1,InHead),
	!.
%convertInConsequence(Env,rn(AX,RN,_S,O),MS,W1,b(MOp,P,D),X,
%                      HYPS,AB,CALLS,PT1,InHead) :-
%	gensym(wp,WP),
%	MS1 = app(WP:m(MOp,P),W1),
%	typeOfDefinition(Env,MS,D,S2),
%	constructMLHead(Env,rn(AX,RN,S2,O),MS1,D,X,HYPS,AB,CALLS,PT1,InHead),
%	!.
%convertInConsequence(Env,rn(AX,RN,_S,O),MS,W1,d(MOp,P,D),X,HYPS,CALLS,PT1,InHead) :-
%	MS1 = app(WP:m(MOp,P),W1),
%	typeOfDefinition(Env,MS,D,S2),
%	constructMLHead(Env,rn(AX,RN,S2,O),MS1,D,X,HYPS,AB,CALLS,PT1,InHead),
%	!.
convertInConsequence(Env,Pr,rn(AX,RN,_S,O),MS,W1,D,X,HYPS,AB,CALLS,PT1,InHead) :-
	/* add loop check to control list */
        CON = X,
	typeOfDefinition(Env,MS,D,S2),
	constructKBHead(Env,Pr,rn(AX,RN,S2,O),W1,D,CON,HYPS,AB,CALLS,PT1,InHead).

convertInConsequenceList(_Env,_Pr,_Name,_MS,[],_X,_HYPS,_AB,_CALLS,no,[]) :-
	!.
convertInConsequenceList(Env,Pr,Name,MS,[NCT],X,HYPS,AB,CALLS,[INCT]) :-
	convertInConsequence(Env,Pr,Name,MS,NCT,X,HYPS,AB,CALLS,INCT),
	!.
convertInConsequenceList(Env,Pr,Name,MS,[NCT|NCTs],X,
                         HYPS,AB,CALLS,[INCT|INCTs]) :-
	convertInConsequence(Env,Pr,Name,MS,NCT,X,HYPS,AB,CALLS,INCT),
	convertInConsequenceList(Env,Pr,Name,MS,NCTs,X,HYPS,AB,CALLS,INCTs).

/***********************************************************************
 *
 * convert_loop(LoopTerm,+CALLS,+Constraint,-CALLS)
 *
 */

convert_loop(no,CALLS,_,CALLS).
convert_loop(_,CALLS,CON,[CON|CALLS]).

convertToGoal(Env,RN,MS1,CN,X,HYPS,AB,CALLS,PT,G) :-
	convertMS(negative,Env,[[],true],MS1,[],[W1,G1],_),
	getQuery(Env,W1,CN,X,PT,G),
%	G = call((in(Env,RN,modal(W1),CN,X,hyp(HYPS),ab(AB),call(CALLS),PT), G1)),
	!.

	
nongeneric(X) :-
	var(X),
	!.
nongeneric(aaa) :-
	!,
	fail.
nongeneric(_) :-
	!.


ground(T,Result) :-
	var(T),
	!,
	Result = false.
ground(T,Result) :-
	atomic(T),
	!,
	Result = true.
ground(T,Result) :-
	T =.. [F|Args],
	map(ground,Args,Results),
	member(false,Results),
	!,
	Result = false.
ground(T,true) :-
	!.
/**********************************************************************
 *
 * @(#) tellRole.pl 1.5@(#)
 *
 */

/***********************************************************************
 *
 * assertRoleLInR(Env,+MS,+RN,+RT,+AN)
 *
 */

assertRoleLInR(Env,MS,R1,inverse(R2),AN) :-
	!,
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN),
	ruleName(AN,RN,user,lInR,Name),
	constructEqHead(Env,Name,W1,X,inverse(F),R2,app((F:R1),X),HYPS,AB,CALLS,true,EqLiteral1), 
%	asserta((EqLiteral1 :- G1)),
	constructEqHead(Env,Name,W1,X,F,R2,app((inverse(F):R1),X),HYPS,AB,CALLS,true,EqLiteral2), 
%	asserta((EqLiteral2 :- G1)),
	gensym(rule,RN3),
	constructEqHead(Env,rn(AN,RN3,user,rInL),W1,X,inverse(F),inverse(R2),
			Y,HYPS,AB,CALLS,PT1,EqLiteral3),
	constructEqCall(Env,rn(AN,RN3,_S3,_O3),bodyMC(W1),headMC(W1),X,F,R1,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral4),
%	asserta((EqLiteral3 :- cCS(CALLS,true), (call(G1), EqLiteral4))),
	gensym(rule,RN4),
	constructEqHead(Env,rn(AN,RN4,user,rInL),W1,Y,inverse(F1),R2,X,HYPS,AB,CALLS,
			PT2,EqLiteral5),
	constructEqCall(Env,rn(AN,RN4,_S3,_O3),bodyMC(W1),headMC(W1),
			X,F1,R1,Y,HYPS,AB,CALLS,PT2,EqLiteral6),
	asserta((EqLiteral5 :- cCS(CALLS,true), (call(G1), EqLiteral6))),
	true.
assertRoleLInR(Env,MS,R1,and(RL),AN) :-
	!,
	assertAndConstraintLInR(Env,MS,R1,and(RL),AN),
        assertAndRoleLInR(Env,MS,R1,and(RL),AN).
assertRoleLInR(Env,MS,R1,restr(R2,C),AN) :-
	!,
	assertRoleLInRRestr1(Env,MS,R1,restr(R2,C),AN),
	assertRoleLInRRestr3(Env,MS,R1,restr(R2,C),AN),
	getComplementRole(Env,MS,R1,restr(R2,C),R3,restr(R2,CNF)),
	assertRoleLInRRestr4(Env,MS,R1,restr(R2,C),R3,restr(R2,CNF),AN).
assertRoleLInR(Env,MS,R1,R2,AN) :-
	!,
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF1),
	gensym(rule,RN1),
	ruleName(AN,RN1,user,lInR,Name1),
	constructEqHead(Env,Name1,W1,Y,SF1,R2,X,HYPS,AB,CALLS,PT1,EqLiteral2),
	constructEqMark(rn(AN,RN1,_S2,_O2),W1,Y,SF1,R2,X,HYPS,AB,CALLS,EqMark2),
	constructEqCall(Env,rn(AN,RN1,_S3,_O3),bodyMC(W1),headMC(W1),Y,_FF,R1,X,HYPS,AB,CALLS,PT1,EqLiteral1),
	asserta((EqLiteral2 :- (cCS(CALLS,EqMark2), (call(G1), EqLiteral1)))),
	gensymbol(skolem,[X,Y],SF2),
	gensym(rule,RN2),
	constructConHead(Env,rn(AN,RN2,user,lInR),W1,SF2,R2,X,'>=',N,
                         HYPS,AB,CALLS,PT1,C2),
	constructConMark(C2,Mark2),
	constructSolveConMark(rn(AN,RN2,_S4,_O4),W1,_FF3,R1,X,'>=',N,HYPS,AB,CALLS,Mark1),
	C1 = solveConstraint(Env,W1,(card,app((_FF:R1),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT1),
	asserta((C2 :- (cCS(CALLS,Mark2), (call(G1), C1)))),
	gensym(rule,RN5),
	gensym(skolem,SF3),
	constructConHead(Env,rn(AN,RN5,user,lInR),W1,SF3,R1,X,'=<',N,
                         HYPS,AB,CALLS,PT1,C4),
	constructConMark(C4,Mark4),
	constructSolveConMark(rn(AN,RN5,_S6,_O6),W1,_FF4,R2,X,'=<',N,HYPS,AB,CALLS,Mark5),
	C5 = solveConstraint(Env,MS,(card,app((_FF2:R2),X),'=<',N),_,hyp(HYPS),ab(AB),call([Mark5|CALLS]),PT1),
	asserta((C4 :- (cCS(CALLS,Mark4), (call(G1), C5)))).
	
/**********************************************************************
 *
 * assertRoleLInRRestr1(+MS,+R1,restr(+R2,+C),+AN)
 * handles the case R1 is included in restr(R2,C).
 * asserts the constraints and membership clauses describing the 
 * relationship of R1 and R2.
 *
 */

assertRoleLInRRestr1(Env,MS,R1,restr(R2,C),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN1),
	typeOfDefinition(Env,MS,C,S1),
	constructMLHead(Env,rn(AN,RN1,S1,lInR),W1,C,Y,HYPS,AB,CALLS,PT1,InHead),
	constructMLMark(InHead,InMark),
	constructEqCall(Env,rn(AN,RN1,_S2,_O2),bodyMC(W1),headMC(W1),
	                Y,F,R1,X,HYPS,AB,[InMark|CALLS],PT1,EqLiteral11),
	asserta((InHead :- (cCS(CALLS,InMark), (call(G1), (EqLiteral11, ground(Y,true)))))),
	gensym(skolem,SF),
	gensym(rule,RN2),
	typeOfDefinition(Env,MS,C,S2),
	constructEqHead(Env,rn(AN,RN2,S2,lInR),W1,Y,SF,R2,X,
                        HYPS,AB,CALLS,PT2,EqLiteral2),
	constructEqMark(rn(AN,RN2,_S3,_O3),W1,Y,SF,R2,X,HYPS,AB,CALLS,EqMark2),
	constructEqCall(Env,rn(AN,RN2,_S4,_O4),bodyMC(W1),headMC(W1),
                        Y,F,R1,X,HYPS,AB,[EqMark2|CALLS],PT2,EqLiteral21),
	asserta((EqLiteral2 :- (cCS(CALLS,EqMark2), (call(G1), EqLiteral21)))),
	!.


assertRoleLInRRestr3(Env,MS,R1,restr(R2,C),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN1),
	typeOfDefinition(Env,MS,C,S1),
	constructConHead(Env,rn(AN,RN1,S1,lInR),W1,G,R2,X,'>=',N,
	                 HYPS,AB,CALLS,and([PT2,PT4]),C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN,_RN2,_S2,_O2),
                         W1,_FF1,R1,X,'>=',N,HYPS,AB,CALLS,Mark2),
	C2 = solveConstraint(Env,W1,(card,app((F:R1),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	asserta((C1 :- (cCS(CALLS,Mark1), (call(G1), C2)))),
	gensym(rule,RN3),
	constructConHead(Env,rn(AN,RN3,S1,lInR),W1,G,R1,X,'=<',N,
                         HYPS,AB,CALLS,PT4,C3),
	constructConMark(C3,Mark3),
	constructSolveConMark(rn(AN,RN3,_S4,_O4),
                         W1,_FF3,R2,X,'=<',N,HYPS,AB,CALLS,Mark4),
	C4 = solveConstraint(Env,W1,(card,app((F:R2),X),'=<',N),_,hyp(HYPS),ab(AB),call([Mark4|CALLS]),PT4),
	asserta((C3 :- (cCS(CALLS,Mark3), (call(G1), C4)))).


/**********************************************************************
 *
 * assertRoleLInRRestr2(Env,+MS,+R1,restr(+R2,+C1),
 *                          +R3,restr(+R2,+C2),+AN)
 * handles the case R1 is included in restr(R2,C).
 * asserts the constraints describing the relationship between 
 * R1 = restr(R2,C1), R3 = restr(R2,not(C1)) and R2.
 *
 */

assertRoleLInRRestr4(Env,MS,R1,restr(R2,_C),R3,restr(R2,_CNF),AN1) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(Env,rn(AN1,RN,user,lInR),W1,_FF,R1,X,'=<',N1,
	                 HYPS,AB,CALLS,and([PT2,PT3]),C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN1,RN,_S2,_O2),W1,FF1,R2,X,'=<',N2,HYPS,AB,CALLS,Mark2),
	C2 = solveConstraint(Env,W1,(card,app((FF1:R2),X),'=<',N2),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	constructSolveConMark(rn(AN1,RN,_S3,_O3),W1,FF2,R3,X,'>=',N3,HYPS,AB,CALLS,Mark3),
	C3 = solveConstraint(Env,W1,(card,app((FF2:R3),X),'>=',N3),_,hyp(HYPS),ab(AB),call([Mark3|CALLS]),PT3),
	asserta((C1 :- (cCS(CALLS,Mark1), (call(G1), (C2, (C3, (M is N2 - N3, comparison('=<',M,N1)))))))),
	!.




/***********************************************************************
 *
 * assertAndRoleLInR(+MS,+Lit,+X,+Y,+RT,+CALLS,+AN)
 *
 */

assertAndRoleLInR(_,_MS,_,and([]),_AN) :-
	!.
assertAndRoleLInR(Env,MS,R1,and([R2|RL]),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(skolem,SF),
	gensym(rule,RN),
	constructEqHead(Env,rn(AN,RN,user,lInR),
                        W1,Y,SF,R2,X,HYPS,AB,CALLS,PT1,EqLiteral2),
	constructEqMark(rn(AN,RN,_S1,_O1),W1,Y,SF,R2,X,HYPS,AB,CALLS,EqMark2),
	constructEqCall(Env,rn(AN,RN,_S2,_O2),bodyMC(W1),headMC(W1),Y,_F,R1,X,
	                HYPS,AB,[EqMark2|CALLS],PT1,EqLiteral1),
	asserta((EqLiteral2 :- (cCS(CALLS,EqMark2), (call(G1), EqLiteral1)))),
	assertAndRoleLInR(Env,MS,R1,and(RL),AN).

/***********************************************************************
 *
 * assertAndConstraintLInR(+MS,+RN,+RT,+AN)
 *
 */

assertAndConstraintLInR(_,_MS,_,and([]),_AN) :-
	!.
assertAndConstraintLInR(Env,MS,R1,and([R2|RL]),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN),
	ruleName(AN,RN,user,lInR,Name),
	constructConHead(Env,Name,W1,_FF,R1,X,Rel,N,HYPS,AB,CALLS,PT2,C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF2,R2,X,Rel,N,HYPS,AB,CALLS,Mark2),	
	gensymbol(skolem,[X],SF),
	C2 = solveConstraint(Env,W1,(card,app((SF:R2),X),Rel,N),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	asserta((C1 :- cCS(CALLS,Mark1), (call(G1), C2))),
	assertAndConstraintLInR(Env,MS,R1,and(RL),AN).

/***********************************************************************
 *
 * assertAndConstraintRInL(+MS,+RN,+RT,+AN)
 *
 */

assertAndConstraintRInL(_,_MS,_,and([]),_AN) :-
	!.
assertAndConstraintRInL(Env,MS,R1,and([R2|RL]),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(Env,rn(AN,RN,user,rInL),W1,_FF,R2,X,'>=',N,
	                 HYPS,AB,CALLS,PT2,C1),
	constructConMark(C1,Mark1),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF1,R1,X,'>=',N,HYPS,AB,CALLS,Mark2),
	gensymbol(skolem,[X],SF),
	C2 = solveConstraint(Env,W1,(card,app((SF:R1),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	asserta((C1 :- cCS(CALLS,Mark1), (call(G1), C2))),
	assertAndConstraintRInL(Env,MS,R1,and(RL),AN).


/***********************************************************************
 *
 * assertRoleRInL(Env,+MS,+RN,+RT,+AN)
 *
 */

assertRoleRInL(Env,MS,R1,inverse(R2),_AN) :-
	!,
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN1),
	constructEqHead(Env,rn(AN,RN1,user,rInL),W1,X,F,R1,
                        app((inverse(F):R2),X),HYPS,AB,CALLS,
			true,EqLiteral1),
%	asserta((EqLiteral1 :- call(G1))),
	gensym(rule,RN2),
	constructEqHead(Env,rn(AN,RN2,user,rInL),
                        W1,X,inverse(F),R1,app((F:R2),X),HYPS,AB,CALLS,
			true,EqLiteral2),
%	asserta((EqLiteral2 :- call(G1))),
	gensym(rule,RN3),
	constructEqHead(Env,rn(AN,RN3,user,rInL),W1,Y,inverse(F),inverse(R2),
			X,HYPS,AB,CALLS,PT1,EqLiteral3),
	constructEqCall(Env,rn(AN,RN3,_S3,_O3),bodyMC(W1),headMC(W1),X,F,R1,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral4),
%	asserta((EqLiteral3 :- cCS(CALLS,true), (call(G1), EqLiteral4))),
	gensym(rule,RN4),
	constructEqHead(Env,rn(AN,RN4,user,rInL),W1,Y,inverse(F1),R1,X,HYPS,AB,CALLS,
			PT2,EqLiteral5),
	constructEqCall(Env,rn(AN,RN4,_S3,_O3),bodyMC(W1),headMC(W1),
			X,F1,R2,Y,HYPS,AB,CALLS,PT2,EqLiteral6),
	asserta((EqLiteral5 :- cCS(CALLS,true), (call(G1), EqLiteral6))).
assertRoleRInL(Env,MS,R1,restr(R2,C), AN) :-
	!,
	assertRoleRInLRestr1(Env,MS,R1,restr(R2,C),AN),
	getComplementRole(Env,MS,R1,restr(R2,C),R3,restr(R2,CNF)),
	assertRoleRInLRestr2(Env,MS,R1,restr(R2,C),R3,restr(R2,CNF),AN),
	assertRoleRInLRestr3(Env,MS,R1,restr(R2,C),AN),
	assertRoleRInLRestr4(Env,MS,R1,restr(R2,C),R3,restr(R2,CNF),AN).
assertRoleRInL(Env,MS,R1,and(RL),AN) :-
	!,
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF),
	gensym(rule,RN1),
	roleBody(Env,W1,and(RL),X,Y,HYPS,AB,CALLS,AN,Body,PTL),
	constructEqHead(Env,rn(AN,RN1,user,rInL),
                        W1,Y,SF,R1,X,HYPS,AB,CALLS,and([PTL]),EqLiteral1),
	constructEqMark(rn(AN,RN1,_S2,_O2),W1,Y,SF,R1,X,HYPS,AB,CALLS,EqMark1),
	asserta((EqLiteral1 :- (cCS(CALLS,EqMark1), (call(G1), Body)))),
	assertAndConstraintRInL(Env,MS,R1,and(RL),AN).
assertRoleRInL(Env,MS,R1,R2,AN) :-
	!,
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF),
	gensym(rule,RN1),
	constructEqHead(Env,rn(AN,RN1,user,rInL),W1,X,SF,R1,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral1),
	constructEqMark(rn(AN,RN1,_S2,_O2),W1,X,SF,R1,Y,HYPS,AB,CALLS,EqMark1),
	constructEqCall(Env,rn(AN,RN1,_S3,_O3),bodyMC(W1),headMC(W1),X,_F,R2,Y,
	                HYPS,AB,CALLS,PT1,EqLiteral2),
	asserta((EqLiteral1 :- (cCS(CALLS,EqMark1), (call(G1), EqLiteral2)))),
	gensym(rule,RN2),
	constructConHead(Env,rn(AN,RN2,user,rInL),W1,_FF5,R1,X,'>=',N,
	                 HYPS,AB,CALLS,PT1,C2),
	constructConMark(C2,Mark2),
	constructSolveConMark(rn(AN,RN2,_S4,_O4),W1,_FF3,R2,X,'>=',N,HYPS,AB,CALLS,Mark1),
	C1 = solveConstraint(Env,W1,(card,app((_FF:R2),X),'>=',N),_,hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT1),
	asserta((C2 :- (cCS(CALLS,Mark2), (call(G1), C1)))),
	gensym(rule,RN5),
	gensym(skolem,SF3),
	constructConHead(Env,rn(AN,RN5,user,rInL),W1,SF3,R2,X,'=<',N,
	                 HYPS,AB,CALLS,PT5,C4),
	constructConMark(C4,Mark4),
	constructSolveConMark(rn(AN,RN5,_S6,_O6),W1,_FF4,R1,X,'=<',N,HYPS,AB,CALLS,Mark5),
	C5 = solveConstraint(Env,W1,(card,app((_FF2:R1),X),'=<',N),_,hyp(HYPS),ab(AB),call([Mark5|CALLS]),PT5),
	asserta((C4 :- (cCS(CALLS,Mark4), (call(G1), C5)))).

/**********************************************************************
 *
 * getComplementRole(+MS,restr(+R2,C),-R3,restr(+R2,-CNF))
 * CNF is the normalform of not(C).
 * If there is already a role name R for restr(R2,CNF) then R3
 * will be instantiated with R.
 * If there is no role name for restr(R2,CNF) then a role name R
 * is generated, clauses for R will be provided, and R3 will be
 * instantiated with R.
 *
 */
 
getComplementRole(Env,MS,_R1,restr(R2,C),R3,restr(R2,CNF)) :-
	negate(C,CN),
	cnf(CN,CNF),
	roleEqualSets(Env,system,MS,R3,restr(R2,CNF),_AX),
	!.
getComplementRole(Env,MS,_R1,restr(R2,C),R3,restr(R2,CNF)) :-
	gensym(role,R3),
	negate(C,CN),
	cnf(CN,CNF),
	gensym(axiom,AN),
	asserta(roleEqualSets(Env,system,MS,R3,restr(R2,CNF),AN)),
	assertRoleLInRRestr1(Env,MS,R3,restr(R2,CNF),AN),
	assertRoleLInRRestr3(Env,MS,R3,restr(R2,CNF),AN),
	assertRoleRInLRestr1(Env,MS,R3,restr(R2,CNF),AN),
%	assertRoleRInLRestr2(Env,MS,R1,restr(R2,CNF),R3,restr(R2,C),AN),
	assertRoleRInLRestr3(Env,MS,R3,restr(R2,CNF),AN),
	!.
	

/**********************************************************************
 *
 * assertRoleRInLRestr1(+MS,+R1,restr(+R2,C),+AN)
 * handles the case restr(R2,C) is included in R1.
 * asserts the constraints and membership clauses describing the 
 * relationship of R1 and R2.
 *
 */

assertRoleRInLRestr1(Env,MS,R1,restr(R2,C),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensymbol(skolem,[X,Y],SF),
	gensym(rule,RN),
	constructEqHead(Env,rn(AN,RN,user,rInL),
                        W1,Y,SF,R1,X,HYPS,AB,CALLS,and([PTEq2,PTIn]),EqLiteral1),
	constructEqMark(rn(AN,RN,_S1,_O1),
                        W1,Y,SF,R1,X,HYPS,AB,CALLS,EqMark1),
	constructEqCall(Env,rn(AN,RN,_S2,_O2),
                        bodyMC(W1),headMC(W1),Y,_FF,R2,X,HYPS,AB,CALLS,PTEq2,EqLiteral2),
	constructMLCall(Env,rn(AN,RN,_S3,_O3),
                        bodyMC(W1),headMC(W1),C,Y,HYPS,AB,CALLS,PTIn,InHead),
	asserta((EqLiteral1 :- (cCS(CALLS,EqMark1), (call(G1), (EqLiteral2, (ground(Y,true), once(InHead))))))).


assertRoleRInLRestr2(Env,MS,R1,restr(R2,_C),R3,restr(R2,_CNF),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(Env,rn(AN,RN,user,rInL),W1,_FF,R1,X,'>=',N1,
	                 HYPS,AB,CALLS,and([PT2,PT3]),D1),
	constructConMark(D1,Mark1),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF2,R2,X,'>=',_N2,HYPS,AB,CALLS,Mark2),
	D2 = solveConstraint(Env,W1,(card,app((_FF3:R2),X),'>=',N2),_,hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT2),
	constructSolveConMark(rn(AN,RN,_S3,_O3),W1,_FF4,R3,X,'=<',_N3,HYPS,AB,CALLS,Mark3),
	D3 = solveConstraint(Env,W1,(card,app((_FF5:R3),X),'=<',N3),_,hyp(HYPS),ab(AB),call([Mark3|CALLS]),PT3),
	asserta((D1 :- (cCS(CALLS,Mark1), (call(G1), (D2, (D3, (M is N2 - N3, comparison('>=',M,N1)))))))),
	!.
	
	
assertRoleRInLRestr3(Env,MS,R1,restr(R2,C),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	negate(C,CN),
	cnf(CN,CNF),
	gensym(rule,RN),
	typeOfDefinition(Env,W1,CNF,S),
	constructMLHead(Env,rn(AN,RN,S,user),
                        W1,CNF,Y,HYPS,AB,CALLS,and([PT1,PT2]),InHead),
	constructMLMark(InHead,Mark),
%	constructConMark(rn(AN,RN,_S2,_O2),W1,_FF2,R2,X,'>=',_N2,
%	                 HYPS,CALLS,Mark2),
%	D2 = solveConstraint(Env,MS,(card,app((_FF4:R2),X),'>=',N2),(M2,L2),
%	                     hyp(HYPS),ab(AB),call([Mark2|CALLS]),PT3),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF1,R1,X,'=<',_N1,
	                 HYPS,AB,CALLS,Mark1),
	D1 = solveConstraint(Env,W1,(card,app((_FF3:R1),X),'=<',N1),(M1,L1),
	                     hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT2),
	constructEqCall(Env,rn(AN,RN,_S3,_O3),bodyMC(W1),headMC(W1),
                        Y,_FF,R2,X,HYPS,AB,CALLS,PT1,EqLiteral2),
% Removed this rule for test purposes
% uh 03.10.92
%	asserta((InHead :- (cCS(CALLS,Mark), (call(G1), (EqLiteral2, (ground(Y,true), (D1, (not(member((Y,_),L1)), N1 = M1)))))))),
	!.

assertRoleRInLRestr4(Env,MS,R1,restr(R2,_C),R3,restr(R2,_CNF),AN) :-
	convertMS(positive,Env,[[],true],MS,[],[W1,G1],_),
	gensym(rule,RN),
	constructConHead(Env,rn(AN,RN,user,rInL),W1,_FF2,R2,X,Rel,N2,
	                 HYPS,AB,CALLS,and([PT1,PT3]),D2),
	constructConMark(D2,Mark2),
	constructSolveConMark(rn(AN,RN,_S1,_O1),W1,_FF1,R1,X,Rel,_N1,
	                 HYPS,AB,CALLS,Mark1),
	D1 = solveConstraint(Env,W1,(card,app((_FF4:R1),X),Rel,N1),_,
	                     hyp(HYPS),ab(AB),call([Mark1|CALLS]),PT1),
	constructSolveConMark(rn(AN,RN,_S3,_O3),W1,_FF3,R3,X,Rel,_N3,
	                 HYPS,AB,CALLS,Mark3),
	D3 = solveConstraint(Env,W1,(card,app((_FF5:R3),X),Rel,N3),_,
	                     hyp(HYPS),ab(AB),call([Mark3|CALLS]),PT3),
	asserta((D2 :- (cCS(CALLS,Mark2), (call(G1), (D1, (D3, (M is N1 + N3, comparison(Rel,M,N2)))))))),
	!.
	

/***********************************************************************
 *
 * roleBody(+MS,+RT,+X,+Y,+CALLS,+AN,-Body)
 *
 */

roleBody(_,_MS,and([]),_X,_Y,_,_,_,_,true,[]) :- 
	!.
roleBody(Env,MS,and([R1]),X,Y,HYPS,AB,CALLS,AN,Body,[PT1]) :-
	constructEqCall(Env,rn(AN,_RN,_S1,_O1),bodyMC(MS),headMC(MS),Y,_FF,R1,X,HYPS,AB,CALLS,PT1,Body),
	!.
roleBody(Env,MS,and([R1|RL]),X,Y,HYPS,AB,CALLS,AN,(EqLiteral, Body),[PT1|PTL]) :-
	constructEqCall(Env,rn(AN,_RN1,_S1,_O1),
                        bodyMC(MS),headMC(MS),Y,_FF,R1,X,HYPS,AB,CALLS,PT1,EqLiteral),
	roleBody(Env,MS,and(RL),X,Y,HYPS,AB,CALLS,AN,Body,PTL).










/**********************************************************************
 *
 * @(#) testMotel.pl 1.6@(#)
 *
 */

testMotel :-
	testAllMotelExamples(1),
	!.

testMotel(N) :-
	testAllMotelExamples(N),
	!.

testAllMotelExamples(64) :-
	print('Test complete'), nl, nl,
	!.
testAllMotelExamples(N) :-
	initialize,
	print('Example '), print(N), nl, example(N),
	once(testMotelExample(N)),
	M is N + 1,
	testAllMotelExamples(M).

testMotelExample(1) :-	
	print('No goal for this example'), nl.
testMotelExample(2) :-
	printTime(setof(C,E^deduce(ex2,[],elementOf(mary,C),E),L1)), print(L1), nl,
	printTime(setof(D,F^deduce(ex2,[],elementOf(tom,D),F),L2)), print(L2), nl.
testMotelExample(3) :-
	tryGoal(inconsistent(ex3)).
testMotelExample(4) :-
	print('No goal for this example'), nl.
testMotelExample(5) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(6) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(7) :-
	print('No goal for this example'), nl.
testMotelExample(8) :-
	tryGoal(deduce(elementOf(tom,heterosexual))).
testMotelExample(9) :-
	tryGoal(deduce(elementOf(chris,male))).
testMotelExample(10) :-
	tryGoal(deduce(elementOf(tom,c2))).
testMotelExample(11) :-
	tryGoal(inconsistent(ex11)).
testMotelExample(12) :-
	tryGoal(subsumes([],c1,c2)),
	tryGoal(not(subsumes([],c2,c1))).
testMotelExample(13) :-
	tryGoal(subsumes([],c1,c2)).
testMotelExample(14) :-
%	initialize, print('Example 14'), nl, example(14),
%	tryGoal(subsumes([],c2,c1)),
	!.
testMotelExample(15) :-
	tryGoal(subsumes([],c2,c1)).
testMotelExample(16) :-
	tryGoal(subsumes([],c2,c1)).
testMotelExample(17) :-
	tryGoal(subsumes([],c2,c1)).
testMotelExample(18) :-
	tryGoal(deduce(elementOf(mary,c4))).
testMotelExample(19) :-
	tryGoal(deduce(elementOf(amy,female))).
testMotelExample(20) :-
	tryGoal(inconsistent(ex20)).
testMotelExample(21) :-
	print('No goal for this example'), nl,
% 	deduce(elementOf(betty,female)),
	!.
testMotelExample(22) :-
% 	deduce(elementOf(amy,female)),
	print('No goal for this example'), nl.
testMotelExample(23) :-
% 	deduce(elementOf(amy,female))
	print('No goal for this example'), nl.
testMotelExample(24) :-
	tryGoal(deduce(elementOf(audi,c3))).
testMotelExample(25) :-
	tryGoal(not(deduce(elementOf(audi,c3)))).
testMotelExample(26) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(27) :-
	tryGoal(not(subsumes([],c1,c2))),
	tryGoal(subsumes([],c2,c1)).
testMotelExample(28) :-
	tryGoal(deduce(ex29,[b(believe,john)],elementOf(audi,auto),_P)).
testMotelExample(29) :-
	print('No goal for this example'), nl.
testMotelExample(30) :-
	print('No goal for this example'), nl.
testMotelExample(31) :-
	tryGoal(deduce(elementOf(tom,onlyMaleChildren))).
testMotelExample(32) :-
	tryGoal(abduce(_H1,elementOf(robin,male),_E1)),
	tryGoal(abduce(_H2,elementOf(robin,female),_E2)).
testMotelExample(33) :-
	tryGoal(abduce(_H3,elementOf(nixon,dove),_E3)),
	tryGoal(abduce(_H4,elementOf(nixon,hawk),_E4)).
testMotelExample(34) :-
	tryGoal(inconsistent(ex34)).
testMotelExample(35) :-
	tryGoal(abduce(ex35,[],_H5,elementOf(john,fly),_E5)),
	tryGoal(not(abduce(ex35,[],_H8,elementOf(tweety,fly),_E8))).
testMotelExample(36) :-
	tryGoal(abduce(ex36,[],_H6,elementOf(nixon,dove),_E6)),
	tryGoal(abduce(ex36,[],_H7,elementOf(nixon,hawk),_E7)).
testMotelExample(37) :-
	print('No goal for this example'), nl.
testMotelExample(38) :-
	tryGoal(deduce(elementOf(ideaste,c2))).
testMotelExample(39) :-
	tryGoal(deduce(elementOf(lucky,female))),
	tryGoal(assert_ind(lucky,male)),
	tryGoal(not(deduce(elementOf(lucky,female)))),
	tryGoal(consistent([])).
testMotelExample(40) :-
	tryGoal(deduce(elementOf(peter,richPerson))),
	tryGoal(assert_ind(peter,poorPerson)),
	tryGoal(not(deduce(elementOf(peter,richPerson)))),
	tryGoal(consistent([])),
	tryGoal(not(subsumes(richPerson,doctor))).
testMotelExample(41) :-
	tryGoal(deduce(elementOf(tom,richPerson))),
	tryGoal(assert_ind(tom,poorPerson)),
	tryGoal(not(deduce(elementOf(tom,richPerson)))),
	tryGoal(consistent([])).
testMotelExample(42) :-
	tryGoal(deduce(elementOf(audi,fourWheels))),
	tryGoal(assert_ind(audi,fiveWheels)),
	tryGoal(not(deduce(elementOf(audi,fourWheels)))),
	tryGoal(consistent([])).
testMotelExample(43) :-
	tryGoal(deduce(elementOf(r,red))),
	tryGoal(deduce(elementOf(r,redOrYellow))),
	tryGoal(deduce(elementOf(r,colors))).
testMotelExample(44) :-
	tryGoal(subsumes(c2,c12)).
testMotelExample(45) :-
	print('No goal for this example'), nl.
testMotelExample(46) :-
	print('No goal for this example'), nl.
testMotelExample(47) :-
	tryGoal(deduce(elementOf(bmw,c3))).
testMotelExample(48) :-
	print('No goal for this example'), nl.
testMotelExample(49) :-
	tryGoal(not(deduce(elementOf(p,c4)))).
testMotelExample(50) :-
	tryGoal(deduce(elementOf(peter,c0))).

testMotelExample(51) :-
	tryGoal(deduce(posInfl(a,d))),
	tryGoal(deduce(posInfl(b,d))),
	tryGoal(bagof(Y1,deduce(posInfl(a,Y1)),Y1s)),
	tryGoal(verifySolution(Y1s,[b,c,d,g])),
	tryGoal(deduce(infl(a,d,1.0))),
	tryGoal(bagof((X1,W1),deduce(infl(X1,e,W1)),X1W1Pairs)),
	tryGoal(verifySolution(X1W1Pairs,[(a,0.0),(b,-1.0),(g,1.0)])),
	tryGoal(deduce(simultInfl([a,h],d,2.0))),
	tryGoal(deduce(change(d,1.0))),
	tryGoal(bagof(X2,deduce(increase(X2)),X2s)),
	tryGoal(verifySolution(X2s,[b,c,d,g,a])).

testMotelExample(52) :-
	tryGoal(deduce(negInfl(withRebate,hasOverallCost))),
	tryGoal(deduce(posInfl(hasListPrice,hasOverallCost))),
	tryGoal(deduce(posInfl(hasCubicCapacity,hasPrice))),
	tryGoal(deduce(posInfl(hasCubicCapacity,hasOverallCost))),
	tryGoal(deduce(posInfl(hasCatConverter,hasOverallCost))),
	tryGoal(deduce(simultInfl([hasCubicCapacity,hasCatConverter],hasOverallCost,3.0))),
	tryGoal(deduce(simultInfl([hasCubicCapacity,hasCatConverter],hasMaxSpeed,-1.0))),
	tryGoal(deduce(leastInfl(hasCubicCapacity,hasMaxSpeed))),
	tryGoal(deduce(leastInfls([hasCatConverter,hasCubicCapacity],hasMaxSpeed))),
	tryGoal(deduce(maxPosInfl(hasCubicCapacity,hasOverallCost,2.0))),
	tryGoal(bagof((X1,W1),deduce(maxNegInfl(X1,hasMaxSpeed,W1)),X1W1Pairs)),
	tryGoal(verifySolution(X1W1Pairs,[(hasCatConverter,-1.0),(hasWeight,-1.0)])),
	tryGoal(bagof(X2,deduce(increase(X2)),X2s)),
	tryGoal(verifySolution(X2s,[hasFuelConsumption,hasListPrice,hasOverallCost,hasPrice,hasWeight,hasCubicCapacity])),
	tryGoal(bagof((X3,W3),(deduce(leastInfl(X3,hasMaxSpeed)),abduce(change(X3,W3),change(hasMaxSpeed,1.0))),X3W3s)),
	tryGoal(verifySolution(X3W3s,[(hasCatConverter,-1.0)])).
testMotelExample(53) :-
	print('No goal for this example'), nl.
testMotelExample(54) :-
	print('No goal for this example'), nl.
testMotelExample(55) :-
	print('No goal for this example'), nl.
testMotelExample(56) :-
	print('No goal for this example'), nl.
testMotelExample(57) :-
	print('No goal for this example'), nl.
testMotelExample(58) :-
	print('No goal for this example'), nl.
testMotelExample(59) :-
	tryGoal(sb_ask(isa(harry,parent))),
	tryGoal(sb_ask(isa(harry,person))),
	printTime(setof((X,Y),sb_ask(role(child,X,Y)),L1)), print(L1), nl,
	printTime(setof(X,sb_ask(roleDef(child,X)),L2)), print(L2), nl,
	printTime(setof((X,Y),sb_ask(roleNr('marys-child',X,Y)),L3)), print(L3), nl,
	printTime(setof(X,sb_ask(roleDefNr('marys-child',X)),L4)), print(L4), nl.
testMotelExample(60) :-
	tryGoal(deduce(ex60,[b(believe,peter)],elementOf(tom,richPerson),E)),
	tryGoal(assert_ind([b(believe,peter)],tom,not(richPerson))),
	tryGoal(inconsistent([b(believe,peter)])).
testMotelExample(61) :-
	tryGoal(deduce(elementOf(tweety,fly))),
	tryGoal(deduce(elementOf(tweety,nest))),
	tryGoal(deduce(elementOf(tweety,not(emu)))),
	tryGoal(deduce(elementOf(tweety,not(cuckoo)))),
	tryGoal(consistent([])).
testMotelExample(62) :-
	tryGoal(deduce(elementOf(tweety,fly))),
	tryGoal(deduce(elementOf(tweety,nest))),
	tryGoal(not(deduce(elementOf(tweety,not(emu))))),
	tryGoal(not(deduce(elementOf(tweety,not(cuckoo))))),
	tryGoal(not(deduce(elementOf(tweety,emu)))),
	tryGoal(not(deduce(elementOf(tweety,cuckoo)))),
	tryGoal(consistent([])).
testMotelExample(63) :-
	tryGoal(deduce(elementOf(tweety,fly))),
	tryGoal(deduce(elementOf(tweety,nest))),
	tryGoal(deduce(elementOf(tweety,not(emu)))),
	tryGoal(deduce(elementOf(tweety,not(cuckoo)))),
	tryGoal(deduce(elementOf(tweety,sparrow))),
	tryGoal(consistent([])).


tryGoal(G) :-
	call(G),
	!,
	print('Goal '), print(G), print(' succeeded'), nl.
tryGoal(G) :-
	print('Goal '), print(G), print(' failed'), nl.

/***********************************************************************
 *
 * verifySolution(+TestSol,+ExpectedSol)
 *
 *	prints an error message if TestSol and ExpectedSol do not
 *	match.
 */

verifySolution(TestSol,ExpectedSol) :-
	nonvar(ExpectedSol),
	nonvar(TestSol),
	!,
	TestSol = ExpectedSol.
verifySolution(TestSol,ExpectedSol) :-
	print('Solutions differ: test solution is '),
	print(TestSol),
	print(', while expected solution is '),
	print(ExpectedSol).

/**********************************************************************
 *
 * @(#) unfold.pl 1.3@(#)
 *
 */

/***********************************************************************
 *
 * unfold(Env,+Type,+CT,+List1,-CN,-List2)
 * Parameter: Type      'concept' or 'role'
 *            CT        concept term
 *            List1     List of tuples (Origin,T,T1,T2)
 *                      where Origin is either 'user' or 'system'
 *                            T      is either 'concept' or 'role'
 *                            T1     is a concept term or role term
 *                            T2     is a concept term or role term
 *            CN        concept name
 *            List2     List of triples (Origin,CN,CT)
 * unfolds concept terms or role terms so that in List2 for all tuples
 * (O,T,CN,all(R1,C1)), (O,T,CN,and([C1,...,Cn])), 
 * (O,T,RN,and([R1,...,Rn])), ...
 * CN and the Ci are concept names and RN and the Ri are role names.
 *
 * 130892   UH   (c)
 *
 */


unfold(_Env,O,_Side,_Type,CT,DL1,O,CT,DL1) :-
	atomicConcept(CT),
	!.
unfold(_Env,_O,_Side,concept,not(CT),DL1,system,not(CT),DL1) :-
	atomicConcept(CT),
	!.
unfold(Env,_O,_Side,concept,CT,DL1,user,C,DL1) :-
	conceptEqualSets(Env,system,_,C,CT,_),
	clause(conceptName(Env,_,_C),_),
	!.
unfold(Env,_O,_Side,concept,CT,DL1,system,C,DL1) :-
	conceptEqualSets(Env,system,_,C,CT,_),
	!.
unfold(Env,_O,left,concept,CT,DL1,system,CT1,DL2) :-
	gensym(concept,C),
	unfold(Env,[(system,concept,C,CT)],[(_,concept,C,CT,CT1)|DL]),
	append(DL1,DL,DL2).
unfold(Env,_O,right,concept,CT,DL1,system,C,DL2) :-
	gensym(concept,C),
	unfold(Env,[(system,concept,C,CT)],[(system,concept,C,CT,CT1)|DL]),
	append(DL1,[(system,concept,C,CT,CT1)|DL],DL2).
unfold(Env,_O,_Side,role,RT,DL1,system,R,DL2) :-
	gensym(role,R),
	unfold(Env,[(system,role,R,RT)],[(system,role,R,RT,RT1)|DL]),
	append(DL1,[(system,role,R,RT,RT1)|DL],DL2).

/***********************************************************************
 *
 * unfold(Env,+List1,-List2)
 * Parameter: List1     List of tuples (Origin,Type,T1,T2)
 *                      where Origin is either 'user' or 'system'
 *                            T      is either 'concept' or 'role'
 *                            T1     is a concept or role term
 *                            T2     is a concept or role term
 *            List2     List of tuples (Origin,Type,CN,CT)
 * unfolds concept terms or role terms so that in List2 for all tuples
 * (O,T,CN,all(R1,C1)), (O,T,CN,and([C1,...,Cn])), 
 * (O,T,RN,and([R1,...,Rn])), ...
 * CN and the Ci are concept names and RN and the Ri are role names.
 *
 * 130892   UH   (c)
 *
 */

unfold(Env,[(Origin,concept,A,all(R,B))|L1],[(NewOrigin,concept,A1,all(R,B),all(R1,B1))|DL3]) :-
	unfold(Env,L1,L2),
	unfold(Env,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(Env,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
	unfold(Env,Origin,right,concept,B,DL2,_NewOriginB1,B1,DL3),
        !.
unfold(Env,[(Origin,concept,A,some(R,B))|L1],[(NewOrigin,concept,A1,some(R,B),some(R1,B1))|DL3]) :-
	unfold(Env,L1,L2),
	unfold(Env,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(Env,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
	unfold(Env,Origin,right,concept,B,DL2,_NewOriginB1,B1,DL3),
        !.
unfold(Env,[(Origin,concept,A,atmost(N,R))|L1],[(NewOrigin,concept,A1,atmost(N,R),atmost(N,R1))|DL2]) :-
	integer(N),
	unfold(Env,L1,L2),
	unfold(Env,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(Env,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
        !.
unfold(Env,[(Origin,concept,A,atleast(N,R))|L1],[(NewOrigin,concept,A1,atleast(N,R),atleast(N,R1))|DL2]) :-
	integer(N),
	unfold(Env,L1,L2),
	unfold(Env,Origin,left,concept,A,L2,NewOrigin,A1,DL1),
	unfold(Env,Origin,right,role,R,DL1,_NewOriginR1,R1,DL2),
        !.
unfold(Env,[(Origin,Type,A,and(L1))|L2],[(NewOrigin,Type,A1,and(L1),and(L3))|L5]) :-
	unfold(Env,L2,DL1),
	unfold(Env,Origin,left,Type,A,DL1,NewOrigin,A1,L4),
	unfoldList(Env,Type,L1,L3,CL3),
	append(L4,CL3,L5),
	!.
unfold(Env,[(Origin,Type,A,set(L1))|L2],[(NewOrigin,Type,A1,set(L1),C)|L4]) :-
	unfold(Env,L2,DL1),
	unfoldSetToConcept(set(L1),C),
	unfold(Env,Origin,left,Type,A,DL1,NewOrigin,A1,L4),
	!.
unfold(Env,[(Origin,concept,A,or(L1))|L2],[(NewOrigin,concept,A1,or(L1),or(L3))|L5]) :-
	unfold(Env,L2,DL1),
	unfold(Env,Origin,left,concept,A,DL1,NewOrigin,A1,L4),
	unfoldList(Env,concept,L1,L3,CL3),
	append(L4,CL3,L5),
	!.
unfold(Env,[(Origin,concept,A,not(B))|L2],[(NewOrigin,concept,A1,not(B),not(B1))|L3]) :-
	unfold(Env,L2,L4),
	unfold(Env,Origin,left,concept,A,L4,NewOrigin,A1,L5),
	unfold(Env,Origin,right,concept,B,L5,_NewOriginB,B1,L3),
	!.
unfold(Env,[(Origin,concept,A,naf(B))|L2],[(NewOrigin,concept,A1,naf(B),naf(B1))|L3]) :-
	unfold(Env,L2,L4),
	unfold(Env,Origin,left,concept,A,L4,NewOrigin,A1,L5),
	unfold(Env,Origin,right,concept,B,L5,_NewOriginB,B1,L3),
	!.
unfold(Env,[(Origin,concept,A,b(O,P,B))|L2],[(NewOrigin,concept,A1,b(O,P,B),b(O,P,B1))|L3]) :-
	unfold(Env,L2,L4),
	unfold(Env,Origin,left,concept,A,L4,NewOrigin,A1,DL1),
	unfold(Env,Origin,right,concept,B,DL1,_NewOriginB1,B1,L3),
	!.
unfold(Env,[(Origin,concept,A,bc(O,C,B))|L2],[(NewOrigin,concept,A1,bc(O,P,B),bc(O,C1,B1))|L5]) :-
	unfold(Env,L2,L4),
	unfold(Env,Origin,left,concept,A,L4,NewOrigin,A1,DL1),
	unfold(Env,Origin,right,concept,C,DL1,_NewOriginB1,C1,L3),
	unfold(Env,Origin,right,concept,B,DL1,_NewOriginB1,B1,L4),
	append(L3,L4,L5),
	!.
unfold(Env,[(Origin,concept,A,d(O,P,B))|L2],[(NewOrigin,concept,A1,d(O,P,B1),d(O,P,B1))|L3]) :-
	unfold(Env,L2,L4),
	unfold(Env,Origin,left,concept,A,L4,NewOrigin,A1,DL1),
	unfold(Env,Origin,right,concept,B,DL1,_NewOriginB1,B1,L3),
	!.
unfold(Env,[(Origin,role,RN,restr(R,C))|L1],[(NewOrigin,role,RN1,restr(R,C),restr(R1,C1))|L2]) :-
	unfold(Env,L1,L3),
	unfold(Env,Origin,left,role,RN,L3,NewOrigin,RN1,L4),
	unfold(Env,Origin,right,role,R,L4,_NewOriginR1,R1,L5),
	unfold(Env,Origin,right,concept,C,L5,_NewOriginC1,C1,L2),
        !.
unfold(Env,[(Origin,role,RN,inverse(R))|L1],[(NewOrigin,role,RN1,inverse(R),inverse(R1))|L5]) :-
	unfold(Env,L1,L3),
	unfold(Env,Origin,left,role,RN,L3,NewOrigin,RN1,L4),
	unfold(Env,Origin,right,role,R,L4,_NewOriginR1,R1,L5),
        !.
unfold(_Env,[(Origin,Type,A,B)],[(Origin,Type,A,B,B)]) :-
	atomicConcept(B),
	!.
unfold(_Env,[],[]) :- !.
	

unfoldList(_Env,_,[],[],[]) :- !.
unfoldList(Env,Type,[CT1|CTL1],[CT1|CTL2],DL1) :-
	atomicConcept(CT1),
	unfoldList(Env,Type,CTL1,CTL2,DL1),
	!.
unfoldList(Env,Type,[CT1|CTL1],[CN|CNL2],DL3) :-
	conceptEqualSets(Env,system,_,CN,CT1,_),
	!,
	unfoldList(Env,Type,CTL1,CNL2,DL3).
unfoldList(Env,Type,[CT1|CTL1],[CN|CNL2],DL1) :-
	gensym(Type,CN),
	unfold(Env,[(system,Type,CN,CT1)],DL),
	unfoldList(Env,Type,CTL1,CNL2,DL3),
	append(DL,DL3,DL1).


/**********************************************************************
 *
 * atomicConcept(+CT) 
 * succeeds if the concept term CT can be regarded as a atomic concept
 * for our translation. In the current implementation variables, 
 * identifiers, singleton sets, and their negation are regarded as 
 * atomic.
 *
 * 130892   UH   (c)
 * 140892   UH   Added clauses for sets and negation of variables
 * 140892   UH   Documented
 *
 */

atomicConcept(CT) :-
	var(CT),
	!.
atomicConcept(CT) :-
	atomic(CT),
	!.
atomicConcept(not(CT)) :-
	var(CT),
	!.
atomicConcept(not(CT)) :-
	atomic(CT),
	!.
atomicConcept(set([E1])) :-
	!.
%atomicConcept(not(set([E1]))) :-
%	!.
	
/**********************************************************************
 *
 * unfoldElementToSet(+Set,-CT)
 * for a given set Set the concept term CT consisting of a disjunktion 
 * of all singleton set contained in Set is computed.
 *
 * 130892   UH   (c)  
 * 140892   UH   Documented
 *
 */

unfoldElementToSet(E1,set([E1])).

unfoldSetToConcept(set([]),bot) :-
	!.
unfoldSetToConcept(set([E1]),set([E1])) :-
	!.
unfoldSetToConcept(set([E1|L1]),or(L2)) :-
	hop_map(unfoldElementToSet,[E1|L1],L2),
	!.


/**********************************************************************
 *
 * %A%
 *
 */

/***********************************************************************
 *
 * initialize
 * cleans TBox, ABox, hierarchies, ...
 *
 */

initialize :-
	retractCompiledPredicates(_),
	retractall(_,in/9),
	retractall(_,kb_in/10),
	retractall(_,eq/9),
	retractall(_,constraint/8),
	retractall(_,rel/5),
	retractall(_,closed/5),
	retractall(_,compiledPredicate/2),
	retractall(_,conceptElement/7),
	retractall(_,conceptEqualSets/6),
	retractall(_,conceptHierarchy/3),
	retractall(_,conceptName/4),
	retractall(_,conceptSubsets/6),
	retractall(_,environment/3),
	retractall(_,given_change/4),
	retractall(_,given_inflLink/4),
	retractall(_,modalAxioms/6),
	retractall(_,roleAttributes/5),
	retractall(_,roleDefault/4),
	retractall(_,roleDefNr/4),
	retractall(_,roleDomain/4),
	retractall(_,roleElement/8),
	retractall(_,roleEqualSets/6),
	retractall(_,roleHierarchy/3),
	retractall(_,roleName/4),
	retractall(_,roleNr/4),
	retractall(_,roleRange/4),
	retractall(_,roleSubsets/6),
	retractall(_,sub/4),
	retractall(_,succ/4),
	retractall(_,abductiveDerivation/3),
	retractall(_,consistencyDerivation/3),
	retractall(_,hypothesis/1),
	retractall(_,inconsistencyCheck/3),
	retractall(_,option/2),
	retractall(_,nsub/4),
	retractall(_,nsub3/2),
	retractall(_,sub3/2),
	retractall(_,succ3/2),
	retractall(_,value/2),
	retractall(_,query/6),
	asserta(environment(initial,env(e0),'Initial Environment')),
	asserta(currentEnvironment(env(e0))),
	initEnvironment(initial),
	!.

retractRoles(Env) :-
 	clause(roleName(Env,_MS,_,RN),_),
 	Head =.. [RN,_,_],
 	retractall(Head),
	fail.
retractRoles(_).


/**********************************************************************
 *
 * loadKB(+FileName)
 * 
 */

loadKB(FileName) :-
	see(FileName),
	repeat,
	read(Goal),
	doFileGoal(Goal),
	!.
loadKB(_) :-
	seen,
	!,
	fail.

loadKB(FileName,EnvName) :-
	var(EnvName),
	loadKB(FileName),
	% The file FileName should contain a call to makeEnvironment
	% Due to the definition of makeEnvironment the new environment
	% should be described by the first environment/3 fact in the
	% database.
	environment(EnvName,_,_),
	!.


doFileGoal('end_of_file') :-
	seen,
	!.
doFileGoal(Goal) :-
	once(call(Goal)),
	fail.

/**********************************************************************
 *
 * getKB(+Name,-Set)
 * Set contains all terminological and assertional axioms in 
 * knowledge base Name.
 *
 */

getKB(Set) :- 
	getCurrentEnvironment(Name),
	getKB(Name,Set),
	!.

getKB(EnvName,Set08) :-
	environment(EnvName,Name,_Comment),
	bagofOrNil(Clause1,
                   [K1,C1,MOp1,A1]^(modalAxioms(Name,user,K1,C1,MOp1,A1), 
                   Clause1 = modalAxioms(K1,MOp1,A1)),Set1),
	bagofOrNil(Clause2,
                   [MS2,W1,G1,A2,C2,Ax2]^(clause(conceptElement(Name,MS2,W1,user,A2,C2,Ax2),G1),
                   Clause2 = assert_ind(MS2,A2,C2)),Set2),
	bagofOrNil(Clause3,
                   [MS3,W1,G1,A3,B3,R3,Ax3]^(clause(roleElement(Name,MS3,W1,user,A3,B3,R3,Ax3)),
	           Clause3 = assert_ind(MS3,A3,B3,R3)),Set3),
	bagofOrNil(Clause4,
                   [MS4,CN4,CT4,Ax4]^(conceptEqualSets(Name,user,MS4,CN4,CT4,Ax4),
                   Clause4 = defconcept(MS4,CN4,CT4)),Set4),
	bagofOrNil(Clause5,
                   [MS5,CN5,CT5,Ax5]^(conceptSubsets(Name,user,MS5,CN5,CT5,Ax5),
                   Clause5 = defprimconcept(MS5,CN5,CT5)),Set5),
	bagofOrNil(Clause6,
		   [MS6,CN6,CT6,Ax6]^(roleEqualSets(Name,user,MS6,CN6,CT6,Ax6),
                   Clause6 = defrole(MS6,CN6,CT6)),Set6),
	bagofOrNil(Clause7,
                   [MS7,CN7,CT7,Ax7]^(roleSubsets(Name,user,MS7,CN7,CT7,Ax7),
		   Clause7 = defprimrole(MS7,CN7,CT7)),Set7),
	bagofOrNil(Clause8,
		   [MS8,X8,Y8,R8]^(closed(Name,MS8,X8,Y8,R8),
		   Clause8 = defclosed(MS8,X8,Y8,R8)),Set8),
	append(   [],Set1,Set01),
	append(Set01,Set2,Set02),
	append(Set02,Set3,Set03),
	append(Set03,Set4,Set04),
	append(Set04,Set5,Set05),
	append(Set05,Set6,Set06),
	append(Set06,Set7,Set07),
	append(Set07,Set8,Set08),
	!.

/**********************************************************************
 *
 * saveKB(+EnvName,+FileName)
 *
 */

saveKB(FileName) :-
	getCurrentEnvironment(EnvName),
	!,
	saveKB(EnvName,FileName).

saveKB(EnvName,FileName) :-
	environment(EnvName,Env,C),
	tell(FileName),
	writeq(makeEnvironment(EnvName,C)), write('.'), nl,
	writeq(initEnvironment(EnvName)), write('.'), nl,
	transformAndWrite(conceptEqualSets(Env,user,MS1,C11,C12,_E4),
	                  defconcept(EnvName,MS1,C11,C12)),
	transformAndWrite(conceptSubsets(Env,user,MS2,C21,C22,_E5),
                          defprimconcept(EnvName,MS2,C21,C22)),
	transformAndWrite(roleEqualSets(Env,user,MS3,R31,R32,_E6),
	                  defrole(EnvName,MS3,R31,R32)),
	transformAndWrite(roleSubsets(Env,user,MS4,R41,R42,_E7),
                          defprimrole(EnvName,MS4,R41,R42)),
	transformAndWrite(conceptElement(Env,MS5,_W1,user,A51,C52,_E14),
                          assert_ind(EnvName,MS5,A51,C52)),
	transformAndWrite(roleElement(Env,MS6,_W1,user,A61,A62,R61,_F15),
	                  assert_ind(EnvName,MS6,A61,A62,R61)),
	transformAndWrite(modalAxioms(Env,user,A71,_A72,A73,A74),
                          modalAxioms(Env,A71,A73,A74)),
	transformAndWrite(roleAttributes(Env,A71,B71,C71),
			  (environment(EnvName,NewEnv,_), assert(roleAttributes(NewEnv,A71,B71,C71)))),
	transformAndWrite(roleDefault(Env,A81,B81,C81),
			  (environment(EnvName,NewEnv,_), assert(roleDefault(NewEnv,A81,B81,C81)))),
	transformAndWrite(roleDefNr(Env,A91,B91,C91),
			  (environment(EnvName,NewEnv,_), assert(roleDefNr(NewEnv,A91,B91,C91)))),
	transformAndWrite(roleDomain(Env,A82,B82,C82),
			  (environment(EnvName,NewEnv,_), assert(roleDomain(NewEnv,A82,B82,C82)))),
	transformAndWrite(roleNr(Env,A83,B83,C83),
			  (environment(EnvName,NewEnv,_), assert(roleNr(NewEnv,A83,B83,C83)))),
	transformAndWrite(roleRange(Env,A84,B84,C84),
			  (environment(EnvName,NewEnv,_), assert(roleRange(NewEnv,A84,B84,C84)))),
        told,
        !.
saveKB(_,_) :-
	told,
	!,
	fail.
			  
transformAndWrite(G1,G2) :-
	clause(G1,_Body),
	writeq(G2), write('.'), nl,
	fail.
transformAndWrite(_,_) :-
	!.


/***********************************************************************
 *
 * deduce(EnvName,MS,Query,Proof)
 *
 */

deduce(P1) :-
	completeParameter([P1],EnvName,MS,Query,Proof),
	deduce(EnvName,MS,Query,Proof).
deduce(P1,P2) :-
	completeParameter([P1,P2],EnvName,MS,Query,Proof),
	deduce(EnvName,MS,Query,Proof).
deduce(P1,P2,P3) :-
	completeParameter([P1,P2,P3],EnvName,MS,Query,Proof),
	deduce(EnvName,MS,Query,Proof).

deduce(EnvName,MS,elementOf(X,C),Exp) :-
	option(useSetheo,yes),
	!,
	deduceSetheo(EnvName,MS,elementOf(X,C),Exp).
deduce(EnvName,MS,elementOf(X,C),Exp) :-
	deduceMOTEL(EnvName,MS,elementOf(X,C),Exp).

deduceMOTEL(EnvName,MS,elementOf(X,C),Exp) :-
	retractall(hypothesis(_)),
 	environment(EnvName,Env,_),
 	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
	clause(query(Env,W1,C,X,Exp,Goal),_).
deduceMOTEL(EnvName,MS,elementOf(X,C),Exp) :-
	retractall(hypothesis(_)),
 	environment(EnvName,Env,_),
 	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
 	getNegatedConcept(C,C1),
	constructMLMark(rn(_AN5,_RN5,_S5,_O5),W1,C1,X,_HYPS,_D1,InHead1),
 	getQuery(Env,W1,C,X,Exp,Goal),
	performQuery(X,G1,Goal),
	allowedAnswerConcept(Env,C),
%	getExplanation(InHead,Exp),
% 	anlegen einer clausel die in undefconcept wieder geloescht wird...
 	setQuery(Env,W1,C,X,Exp,Goal).
deduceMOTEL(EnvName,MS,roleFiller(X,R,L,N),Exp) :-
	retractall(hypothesis(_)),
	environment(EnvName,Env,_),
	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
	call(G1),
	solveConstraint(Env,W1,(card,app((_FF:R),X),'>=',_N1),(_N,L),
	                hyp([]),ab(noAb),call([]),Exp),
	nonvar(X),
	length(L,N).

deduceSetheo(EnvName,MS,elementOf(X,C),Exp) :-
 	environment(EnvName,Env,_),
 	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
	getQuery(Env,MS,C,X,GL),
	write('Trying '), print(X), write(' in '), print(C), write('.'),nl,
	tell('/tmp/aaa.lop'),
	envToFOL(Env,CL),
	printNHProlog(CL),
	printNHProlog(GL),
	told,
	shell('/HG/local/provers/setheo/bin.sun4/inwasm -cons -nosgreord /tmp/aaa',S1),
	S1 = 0,
	shell('/HG/local/provers/setheo/bin.sun4/wasm /tmp/aaa',S2),
	S2 = 0, 
	shell('/HG/local/provers/setheo/bin.sun4/sam -dr -cons /tmp/aaa',S3),
	S3 = 0.


setQuery(Env,W1,C,X,Exp,Goal) :-
	assert(query(Env,W1,C,X,Exp,Goal)),
	!.
setQuery(Env,W1,C,X,Exp,Goal) :-
	!.

splitGoal([(in(A1,B1,C1) <- true)],[(false <- in(A1,B1,C1))]) :-
	!.
splitGoal([(~ in(A1,B1,C1) <- true)],[(false <- ~ in(A1,B1,C1))]) :-
	!.
splitGoal([B|C],[B|D]) :-
	splitGoal(C, D).

getQuery(Env,MS1,C0,X,C3) :-
	var(C0),
	var(X),
	clause(conceptName(Env,_,_,C0),_),
        conceptElement(Env,_,user,X,_,_),
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	axiomToFOL(MS3,[X],_,in,C0,F),
	translate(F,C1),
	clausesToLOP(C1,C2),
	splitGoal(C2,C3).
getQuery(Env,MS1,C0,X,C3) :-
	var(C0),
	var(X),
	clause(conceptName(Env,_,_,C0),_),
        conceptElement(Env,_,user,X,_,_),
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	axiomToFOL(MS3,[X],_,in,not(C0),F),
	translate(F,C1),
	clausesToLOP(C1,C2),
	splitGoal(C2,C3).
getQuery(Env,MS1,C0,X,C3) :-
	var(C0),
	nonvar(X),
	setof(F1,[A1,A2,A3]^clause(conceptName(Env,A1,A2,F1),A3),L1),
	member(C0,L1),
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	axiomToFOL(MS3,[X],_,in,C0,F),
	translate(F,C1),
	clausesToLOP(C1,C2),
	splitGoal(C2,C3).
getQuery(Env,MS1,C0,X,C3) :-
	var(C0),
	nonvar(X),
	setof(F1,[A1,A2,A3]^clause(conceptName(Env,A1,A2,F1),A3),L1),
	member(C0,L1),
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	axiomToFOL(MS3,[X],_,in,not(C0),F),
	translate(F,C1),
	clausesToLOP(C1,C2),
	splitGoal(C2,C3).
getQuery(Env,MS1,C0,X,C3) :-
	nonvar(C0),
	var(X),
        conceptElement(Env,_,user,X,_,_),
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	axiomToFOL(MS3,[X],_,in,C0,F),
	translate(F,C1),
	clausesToLOP(C1,C2),
	splitGoal(C2,C3).
getQuery(Env,MS1,C0,X,C3) :-
	nonvar(C0),
	nonvar(X),
	hop_map(negate,MS1,MS2),
	hop_map(normalizeNot,MS2,MS3),
	axiomToFOL(MS3,[X],_,in,C0,F),
	translate(F,C1),
	clausesToLOP(C1,C2),
	splitGoal(C2,C3).
getQuery(Env,W1,C0,X,Exp,Goal) :-
	var(C0),
	clause(conceptName(Env,_,_,C0),_),
	constructMLCall(Env,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal).

performQuery(X,G1,Goal) :-
	nonvar(X),
	!,
 	once((call((call(G1), Goal)), atomic(X))).
performQuery(X,G1,Goal) :-
	!,
 	call((call(G1), Goal)), 
	atomic(X).

/***********************************************************************
 *
 * deduce(+EnvName,+MS,:+-Info,-E)
 *
 *	If instantiated, Info is one of 
 *		infl(+-X,+-Y,+-W),
 *		posInfl(+-X,+-Y), negInfl(+-X,+-Y), noInfl(+-X,+-Y),
 *		simultInfl(+-Xs,+-Y,+-W), 
 *		simultPosInfl(+-Xs,+-Y), simultNegInfl(+-Xs,+-Y), 
 *		simultNoInfl(+-Xs,+-Y), 
 *		change(+-X,+-W),
 *		increase(+-X), decrease(+-X), noChange(+-X).
 *
 *	Succeeds if Info can be inferred by deduction.
 */

deduce(EnvName,MS,infl(X,Y,W),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	infl(Env,World,X,Y,W).
deduce(EnvName,MS,simultInfl(X,Y,W),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	simultInfl(Env,World,X,Y,W).
deduce(EnvName,MS,leastInfl(X,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	leastInfl(Env,World,X,Y).
deduce(EnvName,MS,leastInfls(Xs,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	leastInfls(Env,World,Xs,Y).
deduce(EnvName,MS,greatestInfl(X,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	greatestInfl(Env,World,X,Y).
deduce(EnvName,MS,greatestInfls(Xs,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	greatestInfls(Env,World,Xs,Y).
deduce(EnvName,MS,maxPosInfl(X,Y,W),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	maxPosInfl(Env,World,X,Y,W).
deduce(EnvName,MS,maxNegInfl(X,Y,W),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	maxNegInfl(Env,World,X,Y,W).
deduce(EnvName,MS,change(Y,W),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	change(Env,World,Y,W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deduce(EnvName,MS,posInfl(X,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	posInfl(Env,World,X,Y).
deduce(EnvName,MS,negInfl(X,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	negInfl(Env,World,X,Y).
deduce(EnvName,MS,simultPosInfl(Xs,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	simultPosInfl(Env,World,Xs,Y).
deduce(EnvName,MS,simultNegInfl(Xs,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	simultNegInfl(Env,World,Xs,Y).
deduce(EnvName,MS,simultNoInfl(Xs,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	simultNoInfl(Env,World,Xs,Y).
deduce(EnvName,MS,noInfl(X,Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	noInfl(Env,World,X,Y).
deduce(EnvName,MS,increase(X),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	increase(Env,World,X).
deduce(EnvName,MS,decrease(X),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	decrease(Env,World,X).
deduce(EnvName,MS,noChange(Y),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	noChange(Env,World,Y).




getQuery(Env,W1,C0,X,Exp,Goal) :-
	var(C0),
	clause(conceptName(Env,_,_,C0),_),
	constructMLCall(Env,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal).
getQuery(Env,W1,C0,X,Exp,Goal) :-
	var(C0),
	clause(conceptName(Env,_,_,C1),_),
	C0 = not(C1),
	constructMLCall(Env,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal).
%getQuery(Env,W1,C0,X,Exp,Goal) :-
% 	var(C0),
%	!,
%	constructMLCall(Env,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
%			C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal),
%	!.
getQuery(Env,W1,C0,X,Exp,Goal) :-
	nonvar(C0),
	convertInAntecedent(Env,rn(no,user,_O1),bodyMC(W1),headMC(_),
			    C0,X,[or([]),rl([]),fl(_DML1)],noAb,[],Exp,Goal),
	!.

/***********************************************************************
 *
 * abduce(+-Hypothesis,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypothesis Hypothesis.
 */
abduce(Hyps,elementOf(X,Y)) :-
	!,
	getCurrentEnvironment(EnvName),
	abduce(EnvName,[],Hyps,elementOf(X,Y),_).
abduce(Hypothesis,Consequent) :-
        getCurrentEnvironment(EnvName),
	abduce(EnvName,[],Hypothesis,Consequent,[]).

/***********************************************************************
 *
 * abduce(+EnvName,+-Hypothesis,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypothesis Hypothesis.
 */

abduce(EnvName,Hypothesis,elementOf(X,C)) :-
	nonvar(EnvName),
	environment(EnvName,_,_),
	!,
	abduce(EnvName,[],elementOf(X,C),_Exp).
abduce(MS,Hypothesis,elementOf(X,C)) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
        getCurrentEnvironment(EnvName),
	!,
	abduce(EnvName,MS,Hypothesis,elementOf(X,C),_Exp).
abduce(Hypothesis,elementOf(X,C),Exp) :-
	getCurrentEnvironment(EnvName),
	!,
	abduce(EnvName,[],Hypothesis,elementOf(X,C),Exp).
abduce(EnvName,Hypothesis,Consequent) :-
        environment(EnvName,_,_),
	!,
	abduce(EnvName,[],Hypothesis,Consequent,[]).
abduce(MS,Hypothesis,Consequent) :-
	nonvar(MS),
        (MS = [] ; MS = [_|_]),
        getCurrentEnvironment(EnvName),
	!,
	abduce(EnvName,MS,Hypothesis,Consequent,[]).

abduce(EnvName,Hyps,elementOf(X,Y),Exp) :-
	nonvar(EnvName),
	environment(EnvName,_,_),
	!,
	abduce(EnvName,[],Hyps,elementOf(X,Y),Exp).
abduce(MS,Hyps,elementOf(X,Y),Exp) :-
	nonvar(MS),
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	!,
	abduce(EnvName,MS,Hyps,elementOf(X,Y),Exp).
abduce(EnvName,MS,Hyps,elementOf(X,Y)) :-
	!,
	abduce(EnvName,MS,Hyps,elementOf(X,Y),_Exp).

abduce(EnvName,MS,Hyps,elementOf(X,C),Exp) :-
	environment(EnvName,Env,_),
	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
	constructMLCall(Env,rn(no,_RN1,user,_O1),bodyMC(W1),headMC(_),
			C,X,[or([]),rl([]),fl(_DML1)],D,[],_Exp1,InHead),
	call((call(G1), InHead)),
	getConstraint(InHead,X),
	atomic(X),
	allowedAnswerConcept(Env,C),
	getAbductionHyps(D,GL),
	once((doConsistencyCheck([],GL),doMinimalityCheck([],GL))),
	getExplanation(InHead,Exp),
	getAbductionHypotheses(D,Hyps).

getAbductionHypotheses(L,[]) :-
	var(L),
	!.
getAbductionHypotheses([],[]) :-
	!.
getAbductionHypotheses([in(Env,RN,modal(MS),C,X,_,_,_,_)|L1],
	                [assert_ind(MS1,X,C)|L2]) :- 
	!,
	translateModalContext(MS,MS1),
	getAbductionHypotheses(L1,L2).
getAbductionHypotheses([C1|L1],
	                [C1|L2]) :- 
	!,
	getAbductionHypotheses(L1,L2).


translateModalContext([],[]) :-
	!.
translateModalContext(app([WP,_]:m(M,A),W),MS) :-
	var(WP),
	!,
	translateAgent(A,A1),
	translateModalContext(W,MS2),
	append(MS2,[d(M,A1)],MS).
translateModalContext(app([WP,_]:m(M,A),W),MS) :-
	!,
	translateAgent(A,A1),
	translateModalContext(W,MS2),
	append(MS2,[b(M,A1)],MS).

translateAgent(A,all) :-
	var(A),
	!.
translateAgent(A,A) :-
	!.

/***********************************************************************
 *
 * abduce(+EnvName,+MS,+-change(+-X,+-Wx),+-change(+-Y,+-Wy),[]).
 *
 *	Succeeds if, under the hypothesis of change(+-X,+-Wx), 
 *	change(+-Y,+-Wy) follows.
 */

abduce(EnvName,MS,change(X,Wx),change(Y,Wy),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	wellDefined_ChangeWeight(Wx),
	infl(Env,World,X,Y,Wxy),
	not(given_change(Env,World,X,_)),
	bagof(W,Z^changingInfl(Env,World,Z,Y,W),Ws),
	weightOf_change(Wx,Wxy,Wy1),
	weightOf_SimultChange([Wy1|Ws],Wy).

abduce(EnvName,MS,change(X,Wx),change(Y,Wy),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	var(Wx),
	wellDefined_ChangeWeight(Wy),
	infl(Env,World,X,Y,Wxy),
	not(given_change(Env,World,X,_)),
	bagof(W,Z^changingInfl(Env,World,Z,Y,W),Ws),
	weightOf_SimultChange([Wy1|Ws],Wy),
	weightOf_change(Wx,Wxy,Wy1).

abduce(EnvName,MS,change(X,Wx),change(Y,Wy),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	infl(Env,World,X,Y,Wxy),
	not(given_change(Env,World,_,_)),
	weightOf_change(Wx,Wxy,Wy).

abduce(EnvName,MS,change(X,Wx),change(X,Wx),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	wellDefined_attribute(Env,World,X),
	not(given_change(Env,World,X,_)),
	wellDefined_ChangeWeight(Wx).

/***********************************************************************
 *
 * abduce(+EnvName,+MS,+-Hypothesis,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypothesis Hypothesis.
 *
 *	Hypothesis and Consequent are of the form:
 *		increase(+-X), decrease(+-X), noChange(+-X). 
 */

abduce(EnvName,MS,Hypothesis,Consequent,[]) :-
	var(Hypothesis),
	nonvar(Consequent),
	once(default_change(Consequent,WeightedConsequent)),
	abduce(EnvName,MS,change(X,Wx),WeightedConsequent,[]),
	once(default_change(Hypothesis,change(X,Wx),[])).

abduce(EnvName,MS,Hypothesis,Consequent,[]) :-
	nonvar(Hypothesis),
%	var(Consequent),
	once(default_change(Hypothesis,WeightedHypothesis)),
	abduce(EnvName,MS,WeightedHypothesis,change(Y,Wy),[]),
	once(default_change(Consequent,change(Y,Wy))).

/***********************************************************************
 *
 * abduce(+EnvName,+MS,+Changes,+-change(+-Y,+-W)).
 *
 *	Succeeds if change(+-Y,+-W) follows under the hypotheses of 
 *	Changes.
 *	Changes is a list of changes of the form change(+-X,+W).
 */

abduce(EnvName,MS,Hs,change(Y,W),[]) :-
	get_Env_World(EnvName,MS,Env,World),
	nonvar(Hs),
	aux_abduce(Env,World,Hs,change(Y,W),Ws),
	wellDefined_SimultChanges(Hs),
	weightOf_SimultChange(Ws,W).

/***********************************************************************
 *
 * abduce(+EnvName,+MS,+-Hypotheses,+-Consequent).
 *
 *	Succeeds if Consequent follows under the hypotheses Hypotheses.
 *
 *	Hypotheses  is a list of 
 *		increase(+-X), decrease(+-X), noChange(+-X). 
 *	predicates and Consequent is one of these.
 */

abduce(EnvName,MS,Hypotheses,Consequent,[]) :-
	nonvar(Hypotheses),
	nonvar(Consequent),
	once(default_changes(Hypotheses,WeightedHypotheses)),
	once(default_change(Consequent,WeightedConsequent)),
	abduce(EnvName,MS,WeightedHypotheses,WeightedConsequent,[]).

abduce(EnvName,MS,Hypotheses,Consequent,[]) :-
	nonvar(Hypotheses),
%	var(Consequent),
	once(default_changes(Hypotheses,WeightedHypotheses)),
	abduce(EnvName,MS,WeightedHypotheses,change(Y,Wy),[]),
	once(default_change(Consequent,change(Y,Wy))).

/***********************************************************************
 *
 * abduce(+EnvName,+MS,+-Change,+-Changes).
 *
 *	Succeeds if Changes are hold under the hypothesis that Change
 *	holds.
 *
 *	Changes (respectively Change) is a list of changes 
 *	(respectively a change) of the form change(+-X,+-W).
 */

abduce(EnvName,MS,Change,Changes,[]) :-
	nonvar(Changes),
	aux2_abduce(EnvName,MS,Change,Changes).

/***********************************************************************
 *
 * abduce(EnvName,MS,+-Hypothesis,+-Consequents).
 *
 *	Succeeds if Consequents follow under the hypothesis Hypothesis.
 *
 *	Hypothesis is of the form:
 *		increase(+-X), decrease(+-X), noChange(+-X). 
 *	Consequents is a list of these.
 */

abduce(EnvName,MS,Hypothesis,Consequents,[]) :-
	nonvar(Hypothesis),
	nonvar(Consequents),
	once(default_change(Hypothesis,WeightedHypothesis)),
	once(default_changes(Consequents,WeightedConsequents)),
	abduce(EnvName,MS,WeightedHypothesis,WeightedConsequents,[]).

abduce(EnvName,MS,Hypothesis,Consequents,[]) :-
	var(Hypothesis),
	nonvar(Consequents),
	once(default_changes(Consequents,WeightedConsequents)),
	abduce(EnvName,MS,change(X,Wx),WeightedConsequents,[]),
	once(default_change(Hypothesis,change(X,Wx))).





/***********************************************************************
 *
 * allowedAnswerConcept(+C)
 * true iff C is a concept introduced by the user. No concept names
 * introduces by the system or concept terms are allowed as answer.
 *
 */

allowedAnswerConcept(Env,C) :-
	atomic(C),
	!,
	clause(conceptName(Env,_,_,C),_),
	!.
% allowedAnswerConcept(Env,C) :-
%	atomic(C),
%	conceptEqualSets(Env,user,_,C,_,_),
%	!.
% allowedAnswerConcept(Env,C) :-
%	atomic(C),
%	conceptEqualSets(Env,user,_,_,C,_),
%	!.
%allowedAnswerConcept(Env,C) :-
%	atomic(C),
%	conceptSubsets(Env,user,_,C,_,_),
%	!.
%allowedAnswerConcept(Env,C) :-
%	atomic(C),
%	conceptSubsets(Env,user,_,_,C,_).
allowedAnswerConcept(Env,C) :-
	nonvar(C),
	C = not(D),
	!,
	nonvar(D),
	not(D = not(E)),
	!,
	allowedAnswerConcept(Env,D).
allowedAnswerConcept(_,normal(_)) :-
	!,
	fail.
allowedAnswerConcept(_,not(normat(_))) :-
	!,
	fail.
allowedAnswerConcept(Env,C) :-
	not(atomic(C)),
	!.

/***********************************************************************
 *
 * inconsistent(+EnvName)
 *
 */


inconsistent :-
	getCurrentEnvironment(EnvName),
	inconsistent(EnvName,[]).

inconsistent(EnvName) :-
	nonvar(EnvName),
	environment(EnvName,_,_),
	inconsistent(EnvName,[]).
inconsistent(MS) :-
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	inconsistent(EnvName,MS).

inconsistent(EnvName,MS) :-
	environment(EnvName,Env,_),
	convertMS(negative,Env,[[],true],MS,[],[W1,G1],_),
	constructMLHead(Env,rn(_AX1,_RN1,user,_O1),W1,C,X,[or([]),rl([]),fl(_DML1)],noAb,[],_,InHead1),
	call((call(G1), InHead1)),
	getConstraint(InHead1,X),
	atomic(X),
	constructMLHead(Env,rn(_AX2,_RN2,_S2,_O2),W1,not(C),X,[or([]),rl([]),fl(_DML1)],noAb,[],_,InHead2),
	call((call(G1), InHead2)).

/***********************************************************************
 *
 * consistent(+EnvName)
 *
 */

consistent :-
	not(inconsistent).

consistent(EnvName) :-
	nonvar(EnvName),
	environment(EnvName,_,_),
	not(inconsistent(EnvName,[])).
consistent(MS) :-
	(MS = [] ; MS = [_|_]),
	getCurrentEnvironment(EnvName),
	not(inconsistent(EnvName,MS)).

consistent(EnvName,MS) :-
	not(inconsistent(EnvName,MS)).

/***********************************************************************
 *
 *
 */

metaReasoning :-
	constructMLHead(Env,rn(ti,ti,system,lInR),W1,C,X,
			_HYPS,_D,_CALLS,inconsistency,InHead1),
	constructMLHead(Env,rn(ti,ti,system,lInR),W1,not(C),X,
			_HYPS,_D,_CALLS,inconsistency,InHead2),
	Lit11 = not(inconsistencyCheck(_,_,_)),
	Lit13 = asserta(InHead2),
	Lit14 = asserta(inconsistencyCheck(MS,C,X)),
	Lit15 = tryInconsistency(MS,C,X,InHead2),
	assertz((InHead1 :- atomic(C), atomic(X), Lit11, Lit13, Lit14, Lit15)),
	Lit23 = asserta(InHead1),
	Lit24 = asserta(inconsistencyCheck(MS,C,X)),
	Lit25 = tryInconsistency(MS,C,X,InHead1),
	assertz((InHead2 :- atomic(C), atomic(X), Lit11, Lit23, Lit24, Lit25)).


tryInconsistency(MS,C,X,InHead) :-
	inconsistent(MS),
	!,
	retract(inconsistencyCheck(MS,C,X)),
	retract(InHead).
tryInconsistency(MS,C,X,InHead) :-
	!,
	retract(inconsistencyCheck(MS,C,X)),
	retract(InHead),
	!,
	fail.

/**********************************************************************
 *
 *
 *
 */

realize(EnvName,MS,X,CL) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Tree1),
	conceptElement(Env,MS,_,user,X,C1,_),
	search(C1,Tree1,TreeList),
	realizeArgs(EnvName,MS,X,TreeList,CL).

realizeDag(EnvName,MS,X,node(CL,AL),CL1) :-
	realizeArgs(EnvName,MS,X,AL,CL0),
	realizeNode(EnvName,MS,X,CL,CL0,CL1).

realizeArgs(_,_,_,[],[]) :-
	!.
realizeArgs(EnvName,MS,X,[C|AL],CL3) :-
	realizeDag(EnvName,MS,X,C,CL1),
	realizeArgs(EnvName,MS,X,AL,CL2),
	append(CL1,CL2,CL3).

realizeNode(EnvName,MS,X,_CL,[C0|CL0],[C0|CL0]) :-
	!.
realizeNode(EnvName,MS,X,[C|CL],[],CL1) :-
	deduce(EnvName,MS,elementOf(X,C),_),
	!,
	CL1 = [C|CL].
realizeNode(_,_,_,_,_,[]) :-
	!.

sbAsk(EnvName,MS,X,CL) :-
	environment(EnvName,Env,_),
	conceptHierarchy(Env,MS,Tree1),
	askDag(EnvName,MS,X,Tree1,CL).

askDag(EnvName,MS,X,node(CL,AL),CL1) :-
	askArgs(EnvName,MS,X,AL,CL0),
	askNode(EnvName,MS,X,CL,CL0,CL1).

askArgs(_,_,_,[],[]) :-
	!.
askArgs(EnvName,MS,X,[C|AL],CL3) :-
	askDag(EnvName,MS,X,C,CL1),
	askArgs(EnvName,MS,X,AL,CL2),
	append(CL1,CL2,CL3).

askNode(_EnvName,_MS,_esX,CL,[C0|CL0],CL1) :-
	!,
	append([C0|CL0],CL,CL1).
askNode(EnvName,MS,X,[C|CL],[],CL1) :-
	deduce(EnvName,MS,elementOf(X,C),_),
	!,
	CL1 = [C|CL].
askNode(_,_,_,_,_,[]) :-
	!.

/**********************************************************************
 *
 * completeParameter(+ParameterList,-EnvName,-MS,-Query,-Proof)
 * takes a list of parameters ParameterList and instantiates the
 * variables EnvName,MS,Query,Proof correctly.
 *
 */

completeParameter([Query],EnvName,[],Query,_Proof) :-
	getCurrentEnvironment(EnvName),
	!.
completeParameter([P1,P2],P1,_MS,P2,_Proof) :-
	var(P1),
	!.
completeParameter([P1,P2],P1,_MS,P2,_Proof) :-
	nonvar(P1),
	atomic(P1),
	P1 \== [],
	!.
completeParameter([P1,P2],EnvName,P1,P2,_Proof) :-
	nonvar(P1),
	(P1 = [] ; P1 = [_|_]),
	!,
	getCurrentEnvironment(EnvName).
completeParameter([P1,P2],EnvName,[],P1,P2) :-
	nonvar(P1),
	!,
	getCurrentEnvironment(EnvName).
completeParameter([P1,P2,P3],P1,P2,P3,_Proof) :-
	var(P1),
	var(P2),
	!.
completeParameter([P1,P2,P3],P1,P2,P3,_Proof) :-
	nonvar(P1),
	atomic(P1),
	P1 \== [],
	var(P2),
	!.
completeParameter([P1,P2,P3],EnvName,P1,P2,P3) :-
	nonvar(P1),
	(P1 = [] ; P1 = [_|_]),
	!,
	getCurrentEnvironment(EnvName).
completeParameter([P1,P2,P3],P1,P2,P3,_Proof) :-
	nonvar(P2),
	(P2 = [] ; P2 = [_|_]),
	!.
completeParameter([P1,P2,P3],P1,[],P2,P3) :-
	nonvar(P1),
	atomic(P1),
	P1 \== [],
	!.
completeParameter([P1,P2,P3],P1,[],P2,P3) :-
	!.
completeParameter([P1,P2,P3,P4],P1,P2,P3,P4) :-
	!.

/**********************************************************************
 *
 * @(#) initCall.pl 1.4@(#)
 *
 */

:- nl, nl.
:- write('Welcome to MOTEL (Version 0.8 July 1993)').
:- nl.
:- write('Copyright (c) 1993, Patrick Brandmeier, Ullrich Hustadt').
:- nl.
:- write('                    Renate Schmidt, Jan Timm. All rights preserved.').
:- nl, nl.
:- write('MOTEL is distributed in the hope that it will be useful, but').
:- nl.
:- write('WITHOUT ANY WARRANTY;  without even the implied warranty of,').
:- nl.
:- write('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.').
:- nl, nl.
:- initialize.
