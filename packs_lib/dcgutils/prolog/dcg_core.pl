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

:- module(dcg_core, [
		nop/2

   % state handling
	,	trans//2
	,	set//1, get//1, set_with//1
	,	with//2, iso//1

   % control
	,	once//1
	,	repeat//0
   ,  fail//0
   ,  (\+)//1
   ,  forall//2
   ,  freeze//2

   % combinators
	,	(>>)//2
   ,  (//)//2
	,	maybe//1
	,	opt//1
	,	if//3, 	if//2
	,	do_then_call//3
	,	do_then_call//4
	,	do_then_call//5
	,	lift//1
	,	lift//2
	,	lift//3

   % repetition
	,	until//2
	,	exhaust//1
	,	rep//2
	,	rep_with_sep//3
   ,  rep_nocopy//2
	,	iterate//3
	,	parmap//2, parmap//3, parmap//4, parmap//5, parmap//6
	,	seqmap//2, seqmap//3, seqmap//4, seqmap//5, seqmap//6
	,	seqmap_n//3, seqmap_n//4, seqmap_n//5
	,	seqmap_with_sep//3
	,	seqmap_with_sep//4
	,	seqmap_with_sep//5
	,	seqmap_ints//3
	,	seqmap_args//4
	,	seqmap_args//5
	,	seqmap_args//6

   % aggregation
	,	findall//3
	,	setof//3

   % sequencing primitives
	,	out//1
   ,  list//1
   % ,  cons//1
]).

/** <module> DCG utilities
 
This module contains predicates for working with definite clause
grammars and the related stateful programming style where state 
arguments are automatically threaded through sequences
of calls. Some useful DCG procedures are also included.

When a predicate is declared with type =|foo(...)// is Det|=,
any requirements on the type of the DCG state are hidden, i.e. the
types of the two extra arguments are hidden. In these cases,
the documentation below will sometimes state that the predicate
'runs in the =|S|= DCG'.

---+++ Types used in this module

We use the following to denote types of terms that can
be interpreted as DCG phrases with or without further
arguments.  
	* phrase(S)
	  If P is a term of type =|phrase(S)|=, then P is a valid DCG phrase
	  when the DCG state is of type =|S|=, i.e. =|phrase(P,S1,S2)|= is
	  valid Prolog goal when S1 and S2 are of type =|S|=. N.B. the type
	  =|phrase(S)|= is almost but not quite equivalent to the binary
	  predicate type =|pred(S,S)|=. All such predicates are valid phrases,
	  but phrases involving braces (e.g. {Goal}), commas, semicolons,
	  and if-then constructs (->) are not equivalent to predicates
	  with two extra arguments.
	* phrase(A,S)
	  If P is of type =|phrase(A,S)|= and X has type A, then =|call(P,X)|= 
	  is a valid DCG phrase when the DCG is of type S. This type _|is|_
	  equivalent to =|pred(A,S,S)|= because the only way to call it
	  is with call//1 inside a DCG or call/3 outside it.
	* phrase(A,B,S)
	  If P is of type =|phrase(A,B)|= and =|X|= and =|Y|= are of types
	  =|A|= and =|B|= respectively, then =|call(P,X,Y)|=
	  is a valid DCG phrase. And so on. You get the idea.
	
*/

:- meta_predicate 
		if(0,//,//,?,?) 
	,	if(0,//,?,?) 
	,	maybe(//,?,?)
	,	opt(//,?,?)
	,	once(//,?,?)
	,	repeat(?,?)
	,	\+(//,?,?)
   ,  forall(//,//,?,?)
   ,  freeze(-,//,?,?)
	,	>>(//,//,?,?)
   ,  //(//,//,?,?)

	,	rep(?,//,?,?)
	,	rep_with_sep(//,?,//,?,?)
	,	rep_nocopy(+,//,?,?)
	,	exhaust(//,?,?)
	,	with(?,//,?,?)
	,	iso(//,?,?)
	,	set_with(1,?,?)
	,	iterate(4,?,?,?,?)
   ,  findall(?,//,-,?,?)
   ,  setof(?,//,-,?,?)
	,	parmap(3,?,?,?)
	,	parmap(4,?,?,?,?)
	,	parmap(5,?,?,?,?,?)
	,	parmap(6,?,?,?,?,?,?)
	,	parmap(7,?,?,?,?,?,?,?)
	,	seqmap(3,?,?,?)
	,	seqmap(4,?,?,?,?)
	,	seqmap(5,?,?,?,?,?)
	,	seqmap(6,?,?,?,?,?,?)
	,	seqmap(7,?,?,?,?,?,?,?)
	,	seqmap_n(+,3,?,?,?)
	,	seqmap_n(+,4,?,?,?,?)
	,	seqmap_n(+,5,?,?,?,?,?)
	,	seqmap_ints(3,+,+,?,?)
	,	seqmap_with_sep(//,3,?,?,?)
	,	seqmap_with_sep(//,4,?,?,?,?)
	,	seqmap_with_sep(//,5,?,?,?,?,?)
	,	seqmap_args(3,+,+,?,?,?)
	,	seqmap_args(4,+,+,?,?,?,?)
	,	seqmap_args(5,+,+,?,?,?,?,?)
	,	do_then_call(//,3,?,?,?)
	,	do_then_call(//,4,?,?,?,?)
	,	do_then_call(//,5,?,?,?,?,?)
	,	until(0,//,?,?)
   ,  lift(0,?,?)
   ,  lift(1,?,?,?)
   ,  lift(2,?,?,?,?)
	.


%% nop// is det.
%
%  Do nothing. (More neutral than []).
nop(X,X).

%% trans( ?Old:S, ?New:S, ?S1:int, ?S2:S) is det.
%
%  Unifies Old and New with the states S1 and S2 respectively.
trans(X,Y,X,Y).

%% set(S:A, S1:_, S2:A) is det.
%  Set state to S. 
set(S,_,S).

%% get(S:A, S1:A, S2:A) is det.
%  Get state to S.
get(S,S,S).

%% set_with(+G:pred(A), S1:_, S2:A) is det.
%
%  Set current state using a given callable goal G, which should accept one argument.
%  should be of type pred( -S:A), ie it should set S to the new desired
%  state, which is installed in the DCG state.
set_with(G,_,S) :- call(G,S).

%% with(S:A, P:phrase(A), S1:B, S2:B) is nondet.
%
%  Run phrase P starting from state S and discarding
%  the final state, meanwhile preserving the state
%  of the current system, i.e. guarantees S1=S2.
with(S,G) --> {call_dcg(G,S,_)}.

%% iso(P:phrase(A), S1:A, S2:A) is nondet.
%
%  Run phrase P starting with current state but discarding
%  its final state and preserving the current state, so
%  that S1=S2.
iso(G,S,S) :- call_dcg(G,S,_).
% iso(G)    --> get(S), {call_dcg(G,S,_)}.

%% once(G:phrase(_))// is semidet.
%  Call DCG phrase G succeeding at most once.
once(G,A,B) :- once(call_dcg(G,A,B)).

%% repeat// is nondet.
%  Create an infinite number of choice points.
repeat(A,A) :- repeat.

%% fail// is nondet.
%  Fails immediately.
fail(_,_) :- fail.

%% \+(G:phrase(_)) is semidet.
%  Succeeds if G fails..
\+(G,A,B) :- \+call_dcg(G,A,B).

%% forall(P:phrase(A), Q:phrase(A)) is semidet.
%  True if Q can succeed after all possible successes of P.
%  Leaves state unchanged.
forall(P,Q) --> \+ (call_dcg(P), \+call_dcg(Q)).

%% freeze(@V:var,+G:phrase(A),?S1:A,?S2:A) is nondet.
%  Suspends the application of DCG goal G to S1 and S2 until variable V is instantiated.
freeze(Var,Goal,S1,S2) :- freeze(Var,call_dcg(Goal,S1,S2)).

%% >>(G1:phrase(S), G2:phrase(S))// is nondet.
% Sequential conjuction of phrases G1 and G2, equivalent to (G1,G2),
% but sometimes more convenient in terms of operator priorities.
A >> B --> call_dcg(A), call_dcg(B).

%% //(+P1:phrase(A), +P2:phrase(A), ?S1:A, ?S2:A) is nondet.
%
%  Parallel goal operator - succeeds if both phrases succeeds with the
%  same start and end states. P1 is called first. 
%  Note, this can be used to capture the list
%  of terminals matched by another phrase by using =|Phrase // list(Terms)|=.
%  phrase P, eg.
%  ==
%  ?- phrase(paren(arb)//list(C),"(hello)world",_)
%  C = "(hello)".
%  true
%  ==
//(P1,P2,S1,S2) :- call_dcg(P1,S1,S2), call_dcg(P2,S1,S2).


%% maybe(P:phrase(_))// is det.
%  Try P, if it fails, then do nothing. If it succeeds,
%  cut choicepoints and continue.
maybe(P)  --> call_dcg(P) -> []; [].

%% opt(P:phrase(_))// is nondet.
%  P or nothing. Like maybe but does not cut if P succeeds.
opt(P)  --> call_dcg(P); [].

%% if(G:pred,P,Q)// is det.
%% if(G:pred,P)// is det.
%
%  If Prolog goal =|call(G)|= succeeds, do P, otherwise, do Q.
%  if(G,P) is equivalent to if(G,P,nop), i.e. does nothing
%  if P fails.
if(A,B,C) --> {call(A)} -> call_dcg(B); call_dcg(C). % used to have nonvar(A)
if(A,B)   --> {call(A)} -> call_dcg(B); [].


% do_then_call( +S:phrase, +P:phrase(A), X:A)// is nondet.
% do_then_call( +S:phrase, +P:phrase(A,B), X:A, Y:B)// is nondet.
% do_then_call( +S:phrase, +P:phrase(A,B,C), X:A, Y:B, Z:C)// is nondet.
%
%  Call phrase S, then call phrase P with arguments A, B, C etc.
do_then_call(S,P,A) --> call_dcg(S), call(P,A).
do_then_call(S,P,A,B) --> call_dcg(S), call(P,A,B).
do_then_call(S,P,A,B,C) --> call_dcg(S), call(P,A,B,C).


lift(P) --> { call(P) }.
lift(P,X) --> { call(P,X) }.
lift(P,X,Y) --> { call(P,X,Y) }.


%% exhaust( P:phrase(_))// is det.
%
%  Run phrase sequentially as many times as possible until it fails.
%  Any choice points left by G are cut.
exhaust(G) --> call_dcg(G) -> exhaust(G); [].


%% until( +Q:pred, +P:phrase(_))// is det.
%
%	Repeatedly call phrase P and test ordinary Prolog goal
%	Q until Q fails. P and Q are copied together before each
%	iteration, so variables can be shared between them, but
%	are not shared between iterations.
until( Pred, Op) -->
	{copy_term(Pred/Op,Pred1/Op1)},
	call(Op1),
	(	{call(Pred1)} 
	-> {Pred/Op=Pred1/Op1}
	;	until(Pred, Op)
	).

%% iterate( +P:phrase(A,A,S), +X:A, -Y:A)// is nondet.
%
%  Sequentially call P zero or more times, passing in X on
%  the first call and threading the result through subsequent calls,
%  (as well as threading the DCG state in the normal way)
%  ending in Y.

iterate(_,A,A) --> [].
iterate(F,A1,A3) --> call(F,A1,A2), iterate(F,A2,A3).


%% rep( +N:natural, +P:phrase(_))// is nondet.
%% rep( -N:natural, +P:phrase(_))// is nondet.
%
%  Equivalent to N sequential copies of phrase P.
%  Free variables in P are *not* shared between copies.
%  If N is unbound on entry, rep//2 is _cautious_: it tries
%  gradually increasing N from 0 on backtracking.

rep(N,G,S1,S2) :- 
	(	var(N) 
	->	rep_var(N,G,S1,S2)
	;	rep_nonvar(N,G,S1,S2)
	).

%% rep_with_sep( +Q:phrase(A), +N:natural, +P:phrase(A))// is nondet.
%% rep_with_sep( +Q:phrase(A), -N:natural, +P:phrase(A))// is nondet.
%
%  As rep//2, but repeats are interspersed with Q. N must be 1 or greater.

rep_with_sep(Q,N,G) --> 
   {copy_term(G,G1)},
   (  {var(N)} 
   -> call_dcg(G1), rep_var(M,Q>>G), {succ(M,N)}
   ;  call_dcg(G1), {succ(M,N)}, rep_nonvar(M,Q>>G)
   ).

rep_var(0,_,S,S).
rep_var(N,G,S1,S3) :- 
	copy_term(G,G1), call_dcg(G1,S1,S2), 
	rep_var(M,G,S2,S3), succ(M,N).

rep_nonvar(0,_,S,S) :- !.
rep_nonvar(N,G,S1,S3) :- 
	copy_term(G,G1), call_dcg(G1,S1,S2), 
	succ(M,N), rep_nonvar(M,G,S2,S3).


%% rep_nocopy( +N:natural, +P:phrase(_))// is nondet.
%
%	Like rep//2 but does not copy P before calling, so
%	any variables in P are shared between all calls.
%	Also, N cannot be a variable in this implementation.
rep_nocopy(0,_) --> !.
rep_nocopy(N,P) --> call(P), {succ(M,N)}, rep_nocopy(M,P).


%% seqmap( +P:phrase(A,S), X:list(A))// is nondet.
%% seqmap( +P:phrase(A,B,S), X:list(A), Y:list(B))// is nondet.
%% seqmap( +P:phrase(A,B,C,S), X:list(A), Y:list(B), Z:list(C))// is nondet.
%% seqmap( +P:phrase(A,B,C,D,S), X:list(A), Y:list(B), Z:list(C), W:list(D))// is nondet.
%% seqmap( +P:phrase(A,B,C,D,E,S), X:list(A), Y:list(B), Z:list(C), W:list(D), V:list(E))// is nondet.
%
%  seqmap//N is like maplist/N except that P is an incomplete _phrase_
%  rather an ordinary goal, which is applied to the elements of the supplied
%  lists _|in order|_, while threading the DCG state correctly through all
%  the calls.
%
%  seqmap//N is very powerful - it is like =foldl= and =mapaccum= in functional 
%  languages, but with the added flexibility of bidirectional Prolog variables.
%  
%  @see maplist/2.

seqmap(_,[])             --> [].
seqmap(P,[A|AX])         --> call(P,A), seqmap(P,AX).
seqmap(_,[],[])          --> [].
seqmap(P,[A|AX],[B|BX])  --> call(P,A,B), seqmap(P,AX,BX).
seqmap(_,[],[],[])          --> [].
seqmap(P,[A|AX],[B|BX],[C|CX])  --> call(P,A,B,C), seqmap(P,AX,BX,CX).
seqmap(_,[],[],[],[])          --> [].
seqmap(P,[A|AX],[B|BX],[C|CX],[D|DX])  --> call(P,A,B,C,D), seqmap(P,AX,BX,CX,DX).
seqmap(_,[],[],[],[],[])          --> [].
seqmap(P,[A|AX],[B|BX],[C|CX],[D|DX],[E|EX])  --> call(P,A,B,C,D,E), seqmap(P,AX,BX,CX,DX,EX).

true(_,_).
parmap(_,[])             --> true.
parmap(P,[A|AX])         --> call(P,A) // parmap(P,AX).
parmap(_,[],[])          --> true.
parmap(P,[A|AX],[B|BX])  --> call(P,A,B) // parmap(P,AX,BX).
parmap(_,[],[],[])          --> true.
parmap(P,[A|AX],[B|BX],[C|CX])  --> call(P,A,B,C) // parmap(P,AX,BX,CX).
parmap(_,[],[],[],[])          --> true.
parmap(P,[A|AX],[B|BX],[C|CX],[D|DX])  --> call(P,A,B,C,D) // parmap(P,AX,BX,CX,DX).
parmap(_,[],[],[],[],[])          --> true.
parmap(P,[A|AX],[B|BX],[C|CX],[D|DX],[E|EX])  --> call(P,A,B,C,D,E) // parmap(P,AX,BX,CX,DX,EX).

%% seqmap_n( +N:natural, +P:phrase(A), X:list(A))// is nondet.
%% seqmap_n( +N:natural, +P:phrase(A,B), X:list(A), Y:list(B))// is nondet.
%% seqmap_n( +N:natural, +P:phrase(A,B,C), X:list(A), Y:list(B), Z:list(C))// is nondet.
%
%  seqmap_n//.. is like seqmap/N except that the lists of arguments are of length N.

seqmap_n(0,_,[])             --> [].
seqmap_n(N,P,[A|AX])         --> {succ(M,N)}, call(P,A), seqmap_n(M,P,AX).
seqmap_n(0,_,[],[])          --> [].
seqmap_n(N,P,[A|AX],[B|BX])  --> {succ(M,N)}, call(P,A,B), seqmap_n(M,P,AX,BX).
seqmap_n(0,_,[],[],[])          --> [].
seqmap_n(N,P,[A|AX],[B|BX],[C|CX])  --> {succ(M,N)}, call(P,A,B,C), seqmap_n(M,P,AX,BX,CX).



%% seqmap_with_sep(+S:phrase, +P:phrase(A), X:list(A))// is nondet.
%% seqmap_with_sep(+S:phrase, +P:phrase(A,B), X:list(A), Y:list(B))// is nondet.
%% seqmap_with_sep(+S:phrase, +P:phrase(A,B,C), X:list(A), Y:list(B), Z:list(C))// is nondet.
%
%  As seqmap//2.. but inserting the separator phrase S between each call to P.
%  NB: *Fails* for empty lists.
%
%  @see seqmap//2
seqmap_with_sep(S,P,[A|AX]) --> call(P,A), seqmap(do_then_call(S,P),AX).
seqmap_with_sep(S,P,[A|AX],[B|BX]) --> call(P,A,B), seqmap(do_then_call(S,P),AX,BX).
seqmap_with_sep(S,P,[A|AX],[B|BX],[C|CX]) --> call(P,A,B,C), seqmap(do_then_call(S,P),AX,BX,CX).


%% seqmap_ints( +P:phrase(integer), +I:integer, +J:integer)// is nondet.
%
%  Equivalent to seqmap(P) applied to the list of integers from I to J inclusive.
%
%  @see seqmap//2.
seqmap_ints(P,L,N) --> 
	(	{L>N} -> []
	;	{M is L+1}, call(P,L), seqmap_ints(P,M,N)
	).


%% seqmap_args( +P:phrase(integer), +I:integer, +J:integer, X:term)// is nondet.
%% seqmap_args( +P:phrase(integer), +I:integer, +J:integer, X:term, Y:term)// is nondet.
%% seqmap_args( +P:phrase(integer), +I:integer, +J:integer, X:term, Y:term, Z:term)// is nondet.
%
%  Like seqmap//N, but applied to the arguments of term X, Y and Z, from the I th to the
%  J th inclusive.
%
%  @see seqmap//2.

seqmap_args(P,L,N,A) --> 
	(	{L>N} -> []
	;	{succ(L,M), arg(L,A,AA)},
		call(P,AA), seqmap_args(P,M,N,A)
	).

seqmap_args(P,L,N,A,B) --> 
	(	{L>N} -> []
	;	{succ(L,M), arg(L,A,AA), arg(L,B,BB)},
		call(P,AA,BB), seqmap_args(P,M,N,A,B)
	).

seqmap_args(P,L,N,A,B,C) --> 
	(	{L>N} -> []
	;	{succ(L,M), arg(L,A,AA), arg(L,B,BB), arg(L,C,CC)},
		call(P,AA,BB,CC), seqmap_args(P,M,N,A,B,C)
	).



%% setof( Template:X, Phrase:phrase(S), Results:list(X), S1:S, S2:S) is nondet.
setof(X,Q,XS,S1,S2) :- setof(X,call_dcg(Q,S1,S2),XS).

%% findall( Template:X, Phrase:phrase(S), Results:list(X), S1:S, S2:S) is nondet.
findall(X,Q,XS,S1,S2) :- findall(X,call_dcg(Q,S1,S2),XS).


%%% ------------------------------------------------------------------
%%% These are for sequence building DCGs.
%%% ------------------------------------------------------------------


%% out(?X)// is det.
%
%  Equivalent to [X]. prepends X to the difference list represented by
%  the DCG state variables.
out(L,[L|L0],L0).

%% list(?L)// is nondet.
%
%  Matches or outputs a sequence of nonterminals.
list(L,S1,S2) :- append(L,S2,S1).

%% cons(?X)// is det.
cons(H,T,[H|T]) :- throw(deprecated(cons/3)).

