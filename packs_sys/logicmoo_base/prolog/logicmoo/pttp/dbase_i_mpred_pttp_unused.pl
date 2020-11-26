:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_pttp_unused,[]).
:- endif.

%%% Adam Farquhar 9/1/95
%%%
%%% An interpreted model elimination theorem prover.
%%%
%%% I use iterative deepening with (1) a filter so that goals found on
%%% previous iterations are not returned, (2) a test so that if no
%%% paths are pruned due to the depth cutoff, no further deepening
%%% will occur, and (3) a method to determine a minimum depth bound
%%% increment that might result in a new answer being found.
%%%
%%% Unification does NOT include an occurs check.  This is because I
%%% want to work with a logic that allows for infinite terms.
%%%

%%% USAGE
/***
clear
  - clear out database and reset any flags.
show 
   - show the internal clauses.
pttp_prove(Goal) 
pttp_prove(Goal,InitialBound)
pttp_prove(Goal,InitialBound,Max)
  - try to pttp_prove the full FOL Goal, which may be of the form
    Term^Goal, which will cause the variables in Term to be bound.
    pttp_prove constructs a clause query(Term) :- Goal and then tries to
    pttp_prove query(Term).
prove_goal(Goal)
prove_goal(Goal,InitialBound)
prove_goal(Goal,InitialBound,Max)
  - Use iterative deepening to pttp_prove Goal within Max starting with a
    bound of InitialBound. Goal needs to be an atomic formula.
prove_goal_bounded(Goal,Ancestors,InitialBound,-Remaining)
prove_goal_bounded(+Goal,Ancestors,+Bound,-Remaining)
  - Try to pttp_prove Goal within Bound.
rule(Head,Body,Cost)
  - Clauses are represented with the predicate rule/3.  Calls to
    system predicates may be done by asserting rule/3.  E.g., if you
    want to allow for append/3 to be accessed, all you need to do is
    to assert: 
       rule(append(X,Y,Z),[],1) :- append(X,Y,Z).
    where [] is the empty body and 1 is the cost of execution.

SYNTAX
  all(X,some(Y,loves(X,Y))).
  a <=> b.
  a <= b and c or d.
  b and c or d => a.
  ~(a and b) <=> ~a or ~b.

***/

%%% BUGS
%%% 
%%% 9/12/95 No known bugs.
%%%

%%% CHANGES
%%% 9/12/95
%%% * Eliminated builtin.  The same effect can be achieved by using
%%% rule/3. Improved id;  Added max, tally/3.  Simplified and rewrote
%%% contrapositives.  Eliminated unused code.  Tried using ord... for
%%% ancestor list, but removed it because it slowed things down on the
%%% current examples.  On hpp-ss10d-3, e5(100) takes 1 sec,
%%% chang_lee_example2 takes 8 sec.
%%%

%%% TODO
%%%
%%% - Figure out how to write apply, holds, inverse, function.
%%%
%%% - The clausal form converter seems to reverse the order of
%%% predicates occasionaly.  This can be trouble with builtins like
%%% is/2, which require certain vars to be bound.
%%%
%%% - pttp_prove needs a better interface for collecting bindings.
%%% E.g. if we query some(X,p(X)), then the X gets renamed when
%%% converting to clausal form, so we don't see the bindings.
%%%
%%% - Contrapositives should be careful with builtins.  We should
%%% probably not include them.  We also need to be careful with
%%% ordering in clause formation (see bugs).
%%%
%%% - See what the effect of indexing the ancestor list would be.  We
%%% used to have two lists (as did Stickel) -- one for postive and one
%%% for negative ancestors.  The main reason for this was that Stickel
%%% used the functor not_xxx rather than ~(xxx) to represent negation.
%%% This made run-time computation of the negation more expensive.
%%% + I tried using ord_add_element and ord_identical_member for the
%%% ancestor list.  This slowed things down on the example problems,
%%% none of which have very deep ancestor lists (11 for chang/lee2).
%%% Perhaps a good binary tree would be worth the effort.
%%%
%%% - Should the rules be passed as an argument rather than indexed in
%%% the database?  If so, be careful about variable bindings!  This
%%% would probably be faster for simple examples.  For larger
%%% problems, it would depend on the quality of the indexing
%%% mechanism.  Since we currently lose the principle functor index
%%% (they are all 'rule') it might still be a win.
%%%
%%% - Collect Proof
%%%

%%% ISSUES
%%%
%%% - Should I use member or identical_member when checking for an
%%% ancestor?  Should the test backtrack to look at more ancestors? (I
%%% don't think so).  Should I use subsumes_chk?
%%%

%% Moved this to my .sicstusrc file.
%% assertz_new(C) :- C -> true ; assertz(C).
%% :- assertz_new(library_directory('/a/axf/theorem-provers/me')).

%% :- ensure_loaded(library(clause)).
:- use_module(clause).
:- use_module(library(basics)).
:- use_module(library(lists)).
:- was_dynamic rule/3.

%%% Model Elimination Theorem Prover
%%%

clear :- abolish(rule/3),
	assert_list(
	[(rule(apply(X,Y),[],1) :- apply(X,Y)),
	(rule(X is Y,[],1) :- X is Y)
    ]).

show :- listing(rule/3).

axioms([]).
axioms([H|T]) :-
	axiom(H),
	axioms(T).

axiom(Fol) :-
	fol_rules(Fol,Clauses),
	assert_list(Clauses).

%%% pttp_prove takes an arbitrary FOL statement and tries to pttp_prove it.
%%% The Sentence may be a FOL sentence in which case only success/failure
%%% is returned, or it can be a term Vars^Sentence, which will cause
%%% Vars to have the bindings from the successful proofs.

pttp_prove(Sentence) :- pttp_prove(Sentence,5).
pttp_prove(Sentence,InitialBound) :- pttp_prove(Sentence,InitialBound,100).

pttp_prove(Sentence0,InitialBound,Max) :-
	(Sentence0 = Vars^Sentence
    ->
	Query = query(Vars)
    ;
	Query = query, Sentence = Sentence0),
	retract_query,
	axiom(Sentence => Query),
	prove_goal(Query,InitialBound,Max).

retract_query :-
	member(Q,[query,~query,query(_),~query(_)]),
	retractall(rule(Q,_,_)),
	clause(rule(A,B,C),true),
	member(Q,B),
	retract(rule(A,B,C)),
	fail.
retract_query.


prove_goal(Goal) :- prove_goal(Goal,5,100).
prove_goal(Goal,InitialBound) :- prove_goal(Goal,InitialBound,100).

prove_goal(Goal,InitialBound,Max) :-
	id(prove_goal_bounded(Goal,[]),InitialBound,Max).

prove_goal_bounded(Goal,Anc,_B,_B) :-
	%% (optional) If there is an identical ancestor, then we are
	%% in a branch that is doomed to fail, so we can prune it.
	identical_member(Goal, Anc),
	!,
	fail.
prove_goal_bounded(Goal,Anc,B0,B) :-
	%% The model elimination reduction operation.  The goal is
	%% solved if we find a negated ancestor.
	negate(Goal,Negated),
	identical_member(Negated, Anc),
	tally(1,B0,B).
prove_goal_bounded(Goal,Anc,B0,B) :-
	rule(Goal,Body,MinProofCost),
	tally(MinProofCost,B0,B1),
	prove_body(Body,[Goal|Anc],B1,B).

prove_body([],_,B,B).
prove_body([Goal|Goals],Anc,B0,B) :-
	prove_goal_bounded(Goal,Anc,B0,B1),
	prove_body(Goals,Anc,B1,B).

%%% Convert to Clausal Form and add Contrapositives
%%%
%%% clausal_form(Fol,Clauses) translates between FOL and Clausal form.
%%% Each Clause is a term clause(PosAtoms,NegAtoms).
%%% We use a "rule" form in which everything looks like a horn clause.

fol_rules(Fol,Rules) :-
	clausal_form(Fol,Clauses),
	clausal_to_rules(Clauses,Rules).
	
clausal_to_rules([],[]) :- !.
clausal_to_rules([Clause|Clauses],Rules) :-
	contrapositives(Clause,Rules0),
	clausal_to_rules(Clauses,Rules1),
	append(Rules0,Rules1,Rules).
	
contrapositives(clause(Head,Body),C) :-
	negate_list(Head,Not_Head),
	append(Not_Head,Body,Atoms),
	contras(Atoms,Atoms,C).

contras([],_,[]).
contras([H|T],Body,[Head_Rule|Tail_Rules]) :-
	negate(H,Not_H),
	delete(Body,H,ThisBody),
	length(Body,Cost),
	Head_Rule = rule(Not_H,ThisBody,Cost),
	contras(T,Body,Tail_Rules).

%%% ITERATIVE DEEPENING
%%%
%% The idea for this is From O'Keefe pg66.  The iterative deepening
%% driver (1) only returns a particular solution/tree one time, and
%% (2) increments the depth bound the minimum amount believed
%% necessary to find a new solution (using the estimated cost provided
%% to tally).  The tally/3 predicate should be called by the Goal
%% where appropriate.

id(Goal,Bound,Max) :- id(Goal,0,Bound,Max).

id(Goal,PrevBound,Bound,Max) :-
	Bound =< Max,
	flag(depth_cutoff,0),
   % the * is not a typo
	apply*(Goal,[Bound,Remaining]),
	Remaining < Bound - PrevBound.
id(Goal,_,Bound,Max) :-
	flag(depth_cutoff,Increment),
	Increment > 0,
	Bound1 is Bound + Increment,
	Bound1 =< Max,
	%%format('~Nid: Increasing bound to: ~p.~n',[Bound1]),
	id(Goal,Bound,Bound1,Max).

tally(N,B0,B) :-
	(B0 >= N				% or > ?
    ->
	B is B0 - 1
    ;
	D is N - B0 + 1,
	flag(depth_cutoff,C),
	(C = 0 ; C > D),
	flag(depth_cutoff,D),
	!,
	fail
    ).

%%% General Utilities

timed(Goal) :-
	statistics(runtime,[T0,_]),
	call(Goal),
	statistics(runtime,[T1,_]),
	Secs is (T1 - T0) / 1000.0,
	format('~nExecution took ~p seconds.~n',[Secs]).

% the * is not a typo
apply*(Goal,MoreArgs) :-
	Goal =.. List0,
	append(List0,MoreArgs,List),
	NewGoal =.. List,
	NewGoal.

apply(Rel,Args) :-
	Goal =.. [Rel|Args],
	Goal.

assert_list([]).
assert_list([H|T]) :- assertz(H),assert_list(T).

%ord_identical_member([H|T],X) :-
%	compare(Order,X,H),
%	ord_identical_member(Order,H,T,X).
%ord_identical_member(<,_H,_T,_X) :- !, fail.
%ord_identical_member(=,_,_,_).
%ord_identical_member(>,_H,T,X) :-
%	ord_identical_member(T,X).	

identical_member(X,[H|T]) :-
	X == H
    ->
	true
    ;
	identical_member(X,T).

%%% Global flags.  There is probably a library/builtin for this.

flag(Flag,Val) :-
	var(Val)
    ->
	global(Flag,Val)
    ;
	(retractall(global(Flag,_)),
	assert(global(Flag,Val))).

%%% Logic Utilities

negate_list([],[]).
negate_list([H|T],[Not_H|Not_T]) :-
	negate(H,Not_H),
	negate_list(T,Not_T).

negate(~(G),G) :- !.
negate(G,~(G)).


%%% Examples

e :- e1,e2,e3,e4,e5,e6,e7,e8,e9,
	chang_lee_example1, chang_lee_example2.

e1 :-
	clear,
	axiom((a or b) and ~(a)),
	prove_goal(b),
	\+ prove_goal(a),
	pttp_prove(a or b).

e2 :- clear,
	axioms(
	[rev([],[]),
	(rev([X|Xs],Zs) <= rev(Xs,Ys) and append(Ys,[X],Zs)),
	append([],Xs,Xs),
	(append([X|Xs],Ys,[X|Zs]) <= append(Xs,Ys,Zs))
    ]),
	pttp_prove(rev([a,b,c],[c,b,a])),
	\+ pttp_prove(rev([a,b,c],[c,b,a,z])),
	e2(30,497).

%nrev([],[]).
%nrev([X|Xs],Zs) :- nrev(Xs,Ys),appn(Ys,[X],Zs).
%appn([],Xs,Xs).
%appn([X|Xs],Ys,[X|Zs]) :- appn(Xs,Ys,Zs).

%% Naive reverse.  The recursion gets quite deep, which means that
%% checking the ancestor list will be  expensive.  On such
%% problems, it would almost certainly pay out to use an indexed
%% ancestor list.  Remember that nrev(30)
%% takes 496 inferences, add one for query, and we get 497.  Starting
%% with a bound of 497 results in an answer in .27 sec (about 10*
%% slower than interpreted prolog).  Each Iterative deepening step
%% increases the bound by only 2 or 3.  This means that a bound of 496
%% takes .52 sec (.25+.27) and 494 takes .76sec.  It is the last
%% portion of the tree that is really expensive, so even a bound of
%% 400 takes 23.7sec.  The moral: set the initial bound right the
%% first time!

e2(N,B) :-
	length(X,N),
	numbervars(X,0,_),
	timed(prove_goal(rev(X,Y),B,100000)),
	reverse(X,Y).

%% the assert's are done directly because the clausal form reorders goals.
e3 :-
	clear,
	assertz(rule(len([],0),[],0)),
	assertz(rule(len([_A|B],N),[len(B,N1),N is N1 + 1],2)),
	pttp_prove(X^len([a,b],X)),
	X == 2.

e4 :- 
	clear,
	axioms(
	[q(X) => p(X),
	q(2),
	q(1)]),
	bagof(X,pttp_prove(X^p(X)),L),
	(L = [1,2] ; L = [2,1]).

e5_1(0).
e5_1(N) :- N > 0, axiom(p(N)),N1 is N-1, e5_1(N1).

%% This is much faster than I expected.  e5(100) takes about 1
%% second (ss10-3) including converting to clausal form and asserting
%% the rules.

%% Almost all of the time here is in the bagof or findall.  If we just
%% fail, then it is almost immediate for N=100.  For QP,findall is

e5 :- e5(5).
e5(N) :-
    clear, e5_1(N),
    axiom(q(X) <= p(X)),
    axiom(r(X,Y) <= p(X) and q(Y)),
    %%bagof(r(X,Y),pttp_prove([X,Y]^r(X,Y)),L),
    bagof(r(X,Y),prove_goal(r(X,Y)),L),
    length(L,Len),
    Len =:= N*N.

e6 :- clear,
	axioms(
	[p(X) <= apply(format,['APPLY: ~p~n',[X]]) and q(X),
	q(1),
	q(2)]),
	bagof(X,pttp_prove(X^p(X)),L),
	format('Should have done APPLY on ~p.~n',[L]).

e7 :- clear,
	axiom(a or b or c),
	axiom(~c),
	pttp_prove(a or b),
	\+ pttp_prove(a or c).

e8 :- clear,
	axiom(a or b or c),
	pttp_prove(a or b or c),
	\+ pttp_prove(a or b),
	\+ pttp_prove(b or c),
	\+ pttp_prove(a or c).

e9 :- clear,
	pttp_prove(~(a and b) <=> ~a or ~b).

e10 :- clear,
	axiom(p(X) or q(X) <= r(X)),
	axiom(r(a) and r(b)),
	pttp_prove(X^(p(X) or q(X))),
	\+ pttp_prove(p(a)),
	\+ pttp_prove(p(b)),
	\+ pttp_prove(q(a)),
	\+ pttp_prove(q(b)),
	\+ pttp_prove(p(a) or q(b)),
	pttp_prove(p(a) or q(a)),
	pttp_prove(p(b) or q(b)).
	
chang_lee_example1 :-
	nl,
	write(chang_lee_example1),
	clear,
	axioms([
		p(g(X,Y),X,Y),
		p(X,h(X,Y),Y),
		(p(U,Z,W) <= p(X,Y,U) and p(Y,Z,V) and p(X,V,W)),
		(p(X,V,W) <= p(X,Y,U) and p(Y,Z,V) and p(U,Z,W)),
		(query(X) <= p(k(X),X,k(X)))
	    ]),
	fail.				% clear stack used in compilation
chang_lee_example1 :-
	%% there are several solutions, at lest one of which results
	%% in a circular term.  We used to get the non-circular one
	%% first, but now the order is changed and we get the circular
	%% one.
	prove_goal(query(_)).
	
chang_lee_example2 :-
	nl,
	write(chang_lee_example2),nl,
	clear,
	axioms(
	[p(e,X,X),
	p(X,e,X),
	p(X,X,e),
	p(a,b,c),
	(p(U,Z,W) <= p(X,Y,U) and p(Y,Z,V) and p(X,V,W)),
	(p(X,V,W) <= p(X,Y,U) and p(Y,Z,V) and p(U,Z,W)),
	(query <= p(b,a,c))
	]),
	fail.
chang_lee_example2 :-
	prove_goal(query).
	
END_OF_FILE
if test 12934 -ne `wc -c <'atp.pl'`; then
    echo shar: \"'atp.pl'\" unpacked with wrong size!
fi
# end of 'atp.pl'
fi
if test -f 'clause.pl' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'clause.pl'\"
else
echo shar: Extracting \"'clause.pl'\" \(18663 characters\)
sed "s/^X//" >'clause.pl' <<'END_OF_FILE'
%   File   : CLAUSE
%   Author : R.A.O'Keefe
%   Updated: 10 March 1984
%   Purpose: Convert a formula in FOPC to clausal form.
%   Needs  : ord_union/3 from UTIL:ORDSET.PL.


:- module(clause,
	[clausal_form/2,
	clausal_form_of_negation/2,
	units_separated/4,
	all/2,
	some/2,
	(=>)/2,
	(<=)/2,
	(<=>)/2,
	%if/3,
	and/2,
	or/2,
	(~)/1
    ]).
	

:- use_module(library(ordsets)).

/*----------------------------------------------------------------------

    This module has three entry points:
	clausal_form(Formula, Clauses)
	clausal_form_of_negation(Formula, Clauses)
	units_separated(Clauses, PosUnits, NegUnits, NonUnits)

    The Formula is an <expr>, where
	<expr> ::= all(<variable>, <expr>)
		|  some(<variable>, <expr>)
		|  <expr> => <expr>
		|  <expr> <=> <expr>
		|  if(<expr>,<expr>,<expr>)
		|  <expr> and <expr>
		|  <expr> or <expr>
		|  ~ <expr>
		|  <atom>

	<atom> ::= <predicate>(<term>,...,<term>)

	<term> ::= <variable>
		|  <constant>
		|  <functor>(<term>,...,<term>)

    The Clauses are a sentence, where
	<sentence> ::= []			(true)
		|  <clause> . <sentence>	(and)

	<clause> ::= clause(<atoms>, <atoms>)
	<atoms> ::= [] | <atom> . <atoms>

    Note that this representation of a clause is not quite the
    usual one.  clause([a,b,c], [d,e,f]) represents
	a v b v c <- d & e & f
    or, if you don't like "Kowalski form",
	a v b v c v ~d v ~e v ~f

    The reason for the two entry points is that the formula may
    contain free variables, these are to be understood as being
    universally quantified, and the negation of the universal
    closure of a formula is not at all the same thing as the
    universal closure of the negation!

    units_separated takes a list of clauses such as the other two predicates
    might produce, and separates them into a list of positive unit clauses
    (represented just by <atom>s), a list of negative unit clauses (also
    represented by their single <atom>s), and a list of non-unit clauses.
    Some theorem provers might find this separation advantageous, but it is
    not buillt into clausal_form becauses some provers would not benefit.

----------------------------------------------------------------------*/

:- public
	clausal_form/2,
	clausal_form_of_negation/2,
	units_seaparated/4.

:- mode
	clausal_form(+, -),
	clausal_form_of_negation(+, -),
	    pass_one(+, -),
		pass_one(+, +, -),
		pass_one(+, -, +, +, -),
		term_one(+, +, +, -),
		    term_one(+, +, +, +, -),
	    pass_two(+, -),
		pass_two_pos(+, -),
		pass_two_pos(+, -, +, +),
		    term_two(+, -, +),
			term_var(+, +, -),
			term_two(+, +, +, +),
		pass_two_neg(+, -, +, +),
		    sent_and(+, +, -),
		    sent_or(+, +, -),
	units_separated(+, -, -, -),
	contains(+, ?),
	literally_contains(+, +),
	does_not_literally_contain(+, ?).


:- op(700, xfx, [contains,literally_contains,does_not_literally_contain]).
:- op(910,  fy, ~).
:- op(920, xfy, and).
:- op(930, xfy, or).
:- op(940, xfx, [=>, <=>, <=]).


units_separated([], [], [], []).
units_separated([clause([],[Neg])|Clauses], PosL, [Neg|NegL], NonL) :- !,
	units_separated(Clauses, PosL, NegL, NonL).
units_separated([clause([Pos],[])|Clauses], [Pos|PosL], NegL, NonL) :- !,
	units_separated(Clauses, PosL, NegL, NonL).
units_separated([Clause|Clauses], PosL, NegL, [Clause|NonL]) :-
	units_separated(Clauses, PosL, NegL, NonL).


clausal_form(Formula, Clauses) :-
	pass_one(Formula, ClosedAndImplicationFree),
	pass_two(ClosedAndImplicationFree, Clauses).


clausal_form_of_negation(Formula, Clauses) :-
	pass_one(Formula, ClosedAndImplicationFree),
	pass_two(~ClosedAndImplicationFree, Clauses).


/*----------------------------------------------------------------------

    The first pass over the formula does two things.
    1a. It locates the free variables of the formula.
    2.  It applies the rules
	    A => B	--> B v ~A
	    A <=> B	--> (B v ~A) /\ (A v ~B)
	    if(A,B,C)	--> (B v ~A) /\ (A v C)
	to eliminate implications.  Even in a non-clausal
	theorem prover this can be a good idea, eliminating
	<=> and if is essential if each subformula is to
	have a definite parity, and that in turn is vital
	if we are going to replace existential quantifiers
	by Skolem functions.
    1b. It adds explicit quantifiers for the free variables.
    The predicate which does all this is pass_one/5:
	pass_one(+Formula,		% The original formula
		 -Translation,		% its implication-free equivalent
		 +Bound,		% The binding environment
		 +Free0,		% The variables known to be free
		 -Free)			% Free0 union Formula's free variables
    The binding environment just tells us which variables occur in quantifiers
    dominating this subformula, it doesn't matter yet whether they're
    universal or existential.

    The translated formula is still an <expr>, although there are practical
    advantages to be gained by adopting a slightly different representation,
    but the neatness of being able to say that
	pass_one(F, G) --> pass_one(G, G)
    outweighs them.

----------------------------------------------------------------------*/

pass_one(Formula, ClosedAndImplicationFree) :-
	pass_one(Formula, ImplicationFree, [], [], FreeVariables),
	pass_one(FreeVariables, ImplicationFree, ClosedAndImplicationFree).


pass_one([], Formula, Formula).
pass_one([Var|Vars], Formula, all(Var,Closure)) :-
	pass_one(Vars, Formula, Closure).


pass_one(all(Var,B), all(Var,D), Bound, Free0, Free) :- !,
	pass_one(B, D, [Var|Bound], Free0, Free).
pass_one(some(Var,B), some(Var,D), Bound, Free0, Free) :- !,
	pass_one(B, D, [Var|Bound], Free0, Free).
pass_one(A and B, C and D, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A or B, C or D, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A => B, D or ~C, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A <= B, C, Bound, Free0, Free) :- !,
	pass_one(B => A, C, Bound, Free0, Free).
pass_one(A <=> B, (D or ~C) and (C or ~D), Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(if(T,A,B), (C or ~U) and (D or U), Bound, Free0, Free) :- !,
	pass_one(T, U, Bound, Free0, Free1),
	pass_one(A, C, Bound, Free1, Free2),
	pass_one(B, D, Bound, Free2, Free).
pass_one(~A, ~C, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free).
pass_one(Atom, Atom, Bound, Free0, Free) :-
	%   An Atom is "anything else".  If Atoms were explicitly flagged,
	%   say by being written as +Atom, we wouldn't need those wretched
	%   cuts all over the place.  The same is true of pass_two.
	term_one(Atom, Bound, Free0, Free).


%   term_one/4 scans a term which occurs in a context where some
%   variables are Bound by quantifiers and some free variables (Free0)
%   have already been discovered.  Free is returned as the union of the
%   free variables in this term with Free0.  Note that though we call
%   does_not_literally_contain twice, it is doing two different things.
%   The first call determines that the variable is free.  The second
%   call is part of adding an element to a set, which could perhaps have
%   been a binary tree or some other data structure.

term_one(Term, Bound, Free0, Free) :-
	nonvar(Term),
	functor(Term, _, Arity),
	!,
	term_one(Arity, Term, Bound, Free0, Free).
term_one(Var, Bound, Free0, [Var|Free0]) :-
	Bound does_not_literally_contain Var,
	Free0 does_not_literally_contain Var,
	!.
term_one(_, _, Free0, Free0).

term_one(0, _, _, Free0, Free0) :- !.
term_one(N, Term, Bound, Free0, Free) :-
	arg(N, Term, Arg),
	term_one(Arg, Bound, Free0, Free1),
	M is N-1, !,
	term_one(M, Term, Bound, Free1, Free).


/*----------------------------------------------------------------------

    pass_two does the following in one grand sweep:
    1.  The original formula might have used the same variable in any
	number of quantifiers.  In the output, each quantifier gets a
	different variable.
    2.  But existentally quantified variables are replaced by new Skolem
	functions, not by new variables.  As a result, we can simply drop
	all the quantifiers, every remaining variable is universally
	quantified.
    3.  The rules
	~ all(V, F)	--> some(V, ~F)
	~ some(V, F)	--> all(V, ~F)
	~ (A and B)	--> ~A or ~B
	~ (A or B)	--> ~A and ~B
	~ ~ A		--> A
	are applied to move negations down in front of atoms.
    4.  The rules
	A or A		--> A
	A or ~A		--> true
	A or true	--> true
	A or false	--> A
	(A or B) or C	--> A or (B or C)
	(A and B) or C	--> (A or C) and (B or C)
	A or (B and C)	--> (A or B) and (A or C)
	A and true	--> A
	A and false	--> false
	(A and B) and C	--> A and (B and C)
	are applied to the clauses which we build as we work our
	way back up the tree.  The rules
	A and A		--> A
	A and ~A	--> false
	A and (~A or B)	--> A and B
	are NOT applied.  This is best done, if at all, after all the
	clauses have been generated.  The last two rules are special
	cases of resolution, so it is doubtful whether it is worth
	doing them at all.

    The main predicate is pass_two_pos/4:
	pass_two_pos(+Formula,		% The formula to translate
		     -Translation,	% its translation
		     +Univ,		% universal quantifiers in scope
		     +Rename)		% how to rename variables
    Rename is var | var(Old,New,Rename), where Old is a source variable,
    and New is either a new variable (for universal quantifiers) or a
    Skolem function applied to the preceding new variables (for existential
    quantifiers).  Univ is those New elements of the Rename argument which
    are variables.  pass_two_neg produces the translation of its Formula's
    *negation*, this saves building the negation and then handling it.

----------------------------------------------------------------------*/

pass_two(ClosedAndImplicationFree, ClausalForm) :-
	pass_two_pos(ClosedAndImplicationFree, PreClausalForm, [], var),
	pass_two_pos(PreClausalForm, ClausalForm).


%   pass_two_pos/2 does two things.  First, if there was only one clause,
%   pass_two_pos/4 wouldn't have wrapped it up in a list.  This we do here.
%   Second, if one of the clauses is "false", we return that as the only
%   clause.  This would be the place to apply A & A --> A.

pass_two_pos(clause(P,N), [clause(P,N)]) :- !.
pass_two_pos(Sentence, [clause([],[])]) :-
	Sentence contains clause([],[]),
	!.
pass_two_pos(Sentence, Sentence).


pass_two_pos(all(Var,B), Translation, Univ, Rename) :- !,
	pass_two_pos(B, Translation, [New|Univ], var(Var,New,Rename)).
pass_two_pos(some(Var,B), Translation, Univ, Rename) :- !,
	gensym('f-', SkolemFunction),
	SkolemTerm =.. [SkolemFunction|Univ],
	pass_two_pos(B, Translation, Univ, var(Var,SkolemTerm,Rename)).
pass_two_pos(A and B, Translation, Univ, Rename) :- !,
	pass_two_pos(A, C, Univ, Rename),
	pass_two_pos(B, D, Univ, Rename),
	sent_and(C, D, Translation).
pass_two_pos(A or B, Translation, Univ, Rename) :- !,
	pass_two_pos(A, C, Univ, Rename),
	pass_two_pos(B, D, Univ, Rename),
	sent_or(C, D, Translation).
pass_two_pos(~A, Translation, Univ, Rename) :- !,
	pass_two_neg(A, Translation, Univ, Rename).
pass_two_pos(true, [], _, _) :- !.
pass_two_pos(false, clause([],[]), _, _) :- !.
pass_two_pos(Atom, clause([Renamed],[]), _, Rename) :-
	%   An Atom is "anything else", hence the cuts above.
	term_two(Atom, Renamed, Rename).


pass_two_neg(all(Var,B), Translation, Univ, Rename) :- !,
	gensym('g-', SkolemFunction),
	SkolemTerm =.. [SkolemFunction|Univ],
	pass_two_neg(B, Translation, Univ, var(Var,SkolemTerm,Rename)).
pass_two_neg(some(Var,B), Translation, Univ, Rename) :- !,
	pass_two_neg(B, Translation, [New|Univ], var(Var,New,Rename)).
pass_two_neg(A and B, Translation, Univ, Rename) :- !,
	pass_two_neg(A, C, Univ, Rename),
	pass_two_neg(B, D, Univ, Rename),
	sent_or(C, D, Translation).
pass_two_neg(A or B, Translation, Univ, Rename) :- !,
	pass_two_neg(A, C, Univ, Rename),
	pass_two_neg(B, D, Univ, Rename),
	sent_and(C, D, Translation).
pass_two_neg(~A, Translation, Univ, Rename) :- !,
	pass_two_pos(A, Translation, Univ, Rename).
pass_two_neg(true, clause([],[]), _, _) :- !.
pass_two_neg(false, [], _, _) :- !.
pass_two_neg(Atom, clause([],[Renamed]), _, Rename) :-
	%   An Atom is "anything else", hence the cuts above.
	term_two(Atom, Renamed, Rename).



term_two(OldTerm, NewTerm, Rename) :-
	nonvar(OldTerm),
	functor(OldTerm, FunctionSymbol, Arity),
	functor(NewTerm, FunctionSymbol, Arity),
	!,
	term_two(Arity, OldTerm, NewTerm, Rename).
term_two(OldVar, NewTerm, Rename) :-
	term_var(Rename, OldVar, NewTerm).


term_var(var(Old,New,_), Var, New) :-
	Old == Var,
	!.
term_var(var(_,_,Rest), Var, New) :-
	term_var(Rest, Var, New).


term_two(0, _, _, _) :- !.
term_two(N, OldTerm, NewTerm, Rename) :-
	arg(N, OldTerm, OldArg),
	term_two(OldArg, NewArg, Rename),
	arg(N, NewTerm, NewArg),
	M is N-1, !,
	term_two(M, OldTerm, NewTerm, Rename).


/*----------------------------------------------------------------------

	sent_and(S1, S2, "S1 and S2")
	sent_or(S1, S2, "S1 or S2")
    perform the indicated logical operations on clauses or sets of
    clauses (sentences), using a fair bit of propositional reasoning
    (hence our use of "literally" to avoid binding variables) to try
    to keep the results simple.  There are several rules concerning
    conjunction which are *not* applied, but even checking for
	A and A --> A
    would require us to recognise alphabetic variants of A rather
    than literal identity.  So far the naivety abount conjunction
    has not proved to be a practical problem.

----------------------------------------------------------------------*/

sent_or(clause(P1,_), clause(_,N2), []) :-
	P1 contains Atom,
	N2 literally_contains Atom,
	!.
sent_or(clause(_,N1), clause(P2,_), []) :-
	N1 contains Atom,
	P2 literally_contains Atom,
	!.
sent_or(clause(P1,N1), clause(P2,N2), clause(P3,N3)) :- !,
	ord_union(P1, P2, P3),
	ord_union(N1, N2, N3).
sent_or([], _, []) :- !.
sent_or(_, [], []) :- !.
sent_or([Clause|Clauses], Sentence, Answer) :- !,
	sent_or(Sentence, Clause, X),
	sent_or(Clauses, Sentence, Y),
	sent_and(X, Y, Answer).
sent_or(Sentence, [Clause|Clauses], Answer) :- !,
	sent_or(Sentence, Clause, X),
	sent_or(Clauses, Sentence, Y),
	sent_and(X, Y, Answer).


sent_and([], Sentence, Sentence) :- !.
sent_and(Sentence, [], Sentence) :- !.
sent_and([H1|T1], [H2|T2], [H1,H2|T3]) :- !,
	sent_and(T1, T2, T3).
sent_and([H1|T1], Clause, [Clause,H1|T1]) :- !.
sent_and(Clause, [H2|T2], [Clause,H2|T2]) :- !.
sent_and(Clause1, Clause2, [Clause1,Clause2]).


[Head|_] contains Head.
[_|Tail] contains Something :-
	Tail contains Something.


[Head|_] literally_contains Something :-
	Head == Something,
	!.
[_|Tail] literally_contains Something :-
	Tail literally_contains Something.


[] does_not_literally_contain Anything.
[Head|Tail] does_not_literally_contain Something :-
	Head \== Something,
	Tail does_not_literally_contain Something.


/*----------------------------------------------------------------------
    Debugging kit.
	portray_sentence(ListOfClauses)
	    displays a list of clauses, one per line.
	portray_a_clause(Clause)
	    displays a single clause in "Kowalski notation"
	t(Formula)
	    translates a formula and prints the result.
*/
:- public
	t1/0,t9/0,t/1.


portray_sentence([Clause]) :- !,
	portray_a_clause(Clause),
	nl.
portray_sentence([Clause|Clauses]) :-
	portray_a_clause(Clause),
	write(' AND'), nl,
	portray_sentence(Clauses).
portray_sentence([]) :-
	write('TRUE'), nl.


portray_a_clause(clause(PosAtoms, NegAtoms)) :-
	numbervars(PosAtoms, 0, N),
	numbervars(NegAtoms, N, _),
	portray_a_clause(PosAtoms, ' v '),
	write(' <- '),
	portray_a_clause(NegAtoms, ' & '),
	fail.
portray_a_clause(_).


portray_a_clause([Atom], _) :- !,
	print(Atom).
portray_a_clause([Atom|Atoms], Separator) :-
	print(Atom), write(Separator),
	portray_a_clause(Atoms, Separator).
portray_a_clause([], _) :-
	write([]).


t(X) :-
	clausal_form(X, Y),
	portray_sentence(Y).

t1 :- t((a=>b) and (b=>c) and (c=>d) and (d=>a) => (a<=>d)).

t2 :- t(continuous(F,X) <=> all(Epsilon, Epsilon > 0 =>
	    some(Delta, Delta > 0 and all(Y,
		abs(Y-X) < Delta => abs(val(F,Y)-val(F,X)) < Epsilon
	)))).

t3 :- clausal_form_of_negation(
	( subset(S1,S2) <=> all(X, member(X,S1) => member(X,S2) )) =>
	( subset(T1,T2) and subset(T2,T3) => subset(T1,T3) )	,Y),
	portray_sentence(Y).

t4 :- t(subset(T1,T2) and subset(T2,T3) => subset(T1,T3)).


t5 :- t((a=>b) and (b=>c)).

t6 :- t(~(a and b)).

t7 :- t((a and b) or c).

t8 :- t((a and b) or (a and ~b) or (~a and b) or (~a and ~b)).

t9 :- t(
	(true(P) <=> t(w0,P)) and
	(t(W1,P1 and P2) <=> t(W1,P1) and t(W1,P2)) and
	(t(W1,P1 or P2) <=> t(W1,P1) or t(W1,P2)) and
	(t(W1,P1 => P2) <=> (t(W1,P1) => t(W1,P2))) and
	(t(W1,P1 <=> P2) <=> (t(W1,P1) <=> t(W1,P2))) and
	(t(W1,~P1) <=> ~t(W1,P1)) and
	(t(W1,know(A1,P1)) <=> all(W2,k(A1,W1,W2)=>t(W2,P1))) and
	k(A1,W1,W1) and
	(k(A1,W1,W2) => (k(A1,W2,W3) => k(A1,W1,W3))) and
	(k(A1,W1,W2) => (k(A1,W1,W3) => k(A1,W2,W3))) and
	(t(W1,know(A,P)) <=> all(W2,k(A,W1,W2) => t(W2,P)))
	).

/*----------------------------------------------------------------------*/


%   File   : GENSYM.PL
%   Author : Lawrence Byrd?
%   Updated: 21 February 1984
%   Purpose: create new atoms
%   Needs  : append/3.

:- use_module(library(lists)).

:- public
	cgensym/2,
	concat/3,
	gensym/2.

:- mode
	cgensym(+, ?),
	concat(+, +, ?),
	gensym(+, ?).

%   gensym(Prefix, V)
%   binds V to a new atom whose name begins with Prefix and ends with a
%   number.  E.g. gensym(a,X), gensym(a,Y), gensym(a,Z) might bind
%   X to a1, Y to a2, Z to a3.  It only succeeds once per call, to get
%   another binding for V you have to call it again.

gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   retract(flag(gensym(Prefix), M))
	|   M = 0
	),
	N is M+1,
	asserta(flag(gensym(Prefix), N)),
	concat(Prefix, N, V),
	!.


%   cgensym(Prefix, V)
%   binds V to a new atom unless it is already bound.  Thus
%   cgensym(a, fred) would succeed, but cgensym(a, X) would bind
%   X to a new atom, maybe a4.  "c" standard for "conditional".

cgensym(Prefix, V) :-
	nonvar(V), !,
	atomic(V),
	atomic(Prefix).
cgensym(Prefix, V) :-
	gensym(Prefix, V).


%   concat(Name1, Name2, Name3)
%   is like append on atoms.  That is, it appends the name of Name1 and
%   the name of Name2, and binds Name3 to the atom named by the result.
%   Unlike append, it will only work one way round.  Examples:
%   concat(a, b, ab), concat(10, 23, 1023), concat(gs, 46, gs46).
%   concat(04, 05, 405)*??*

concat(N1, N2, N3) :-
	name(N1, Ls1),
	name(N2, Ls2),
	append(Ls1, Ls2, Ls3),
	name(N3, Ls3).




END_OF_FILE
if test 18663 -ne `wc -c <'clause.pl'`; then
    echo shar: \"'clause.pl'\" unpacked with wrong size!
fi
# end of 'clause.pl'
fi
echo shar: End of shell archive.
exit 0


























end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.








askable_t(A, B, C, F, E, H, G, I, J) :-
 D=not_askable_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_askable_t(A, B, C, F, E, H, G, I, J, D)
 ).
askable_t(A, B, C, F, E, H, G, I, J) :-
 D=not_askable_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).
not_askable_t(A, B, C, F, E, H, G, I, J) :-
 D=askable_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).
not_askable_t(A, B, C, F, E, H, G, I, J) :-
 D=askable_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_not_askable_t(A, B, C, F, E, H, G, I, J, D)
 ).



end_of_file.


:- kb_shared was_pttp_functor/3.
:- was_dynamic was_pttp_functor/3.

was_pttp_functor(internal, int_unknown_t, 9).
was_pttp_functor(internal, int_unknown_t, 8).
was_pttp_functor(internal, int_unknown_t, 7).
was_pttp_functor(internal, int_unknown_t, 13).
was_pttp_functor(internal, int_unknown_t, 12).
was_pttp_functor(internal, int_unknown_t, 11).
was_pttp_functor(internal, int_unknown_t, 10).
was_pttp_functor(internal, int_proven_not_t, 9).
was_pttp_functor(internal, int_proven_not_t, 8).
was_pttp_functor(internal, int_proven_not_t, 7).
was_pttp_functor(internal, int_proven_not_t, 13).
was_pttp_functor(internal, int_proven_not_t, 12).
was_pttp_functor(internal, int_proven_not_t, 11).
was_pttp_functor(internal, int_proven_not_t, 10).
was_pttp_functor(internal, int_proven_t, 9).
was_pttp_functor(internal, int_proven_t, 8).
was_pttp_functor(internal, int_proven_t, 7).
was_pttp_functor(internal, int_proven_t, 13).
was_pttp_functor(internal, int_proven_t, 12).
was_pttp_functor(internal, int_proven_t, 11).
was_pttp_functor(internal, int_proven_t, 10).
was_pttp_functor(internal, int_pred_t, 9).
was_pttp_functor(internal, int_pred_t, 8).
was_pttp_functor(internal, int_pred_t, 7).
was_pttp_functor(internal, int_pred_t, 13).
was_pttp_functor(internal, int_pred_t, 12).
was_pttp_functor(internal, int_pred_t, 11).
was_pttp_functor(internal, int_pred_t, 10).
was_pttp_functor(internal, int_pred_isa_t, 9).
was_pttp_functor(internal, int_pred_isa_t, 8).
was_pttp_functor(internal, int_pred_isa_t, 7).
was_pttp_functor(internal, int_pred_isa_t, 13).
was_pttp_functor(internal, int_pred_isa_t, 12).
was_pttp_functor(internal, int_pred_isa_t, 11).
was_pttp_functor(internal, int_pred_isa_t, 10).
was_pttp_functor(internal, int_possible_t, 9).
was_pttp_functor(internal, int_possible_t, 8).
was_pttp_functor(internal, int_possible_t, 7).
was_pttp_functor(internal, int_possible_t, 13).
was_pttp_functor(internal, int_possible_t, 12).
was_pttp_functor(internal, int_possible_t, 11).
was_pttp_functor(internal, int_possible_t, 10).
was_pttp_functor(internal, int_not_proven_not_t, 9).
was_pttp_functor(internal, int_not_proven_not_t, 8).
was_pttp_functor(internal, int_not_proven_not_t, 7).
was_pttp_functor(internal, int_not_proven_not_t, 13).
was_pttp_functor(internal, int_not_proven_not_t, 12).
was_pttp_functor(internal, int_not_proven_not_t, 11).
was_pttp_functor(internal, int_not_proven_not_t, 10).
was_pttp_functor(internal, int_not_pred_t, 9).
was_pttp_functor(internal, int_not_pred_t, 8).
was_pttp_functor(internal, int_not_pred_t, 7).
was_pttp_functor(internal, int_not_pred_t, 13).
was_pttp_functor(internal, int_not_pred_t, 12).
was_pttp_functor(internal, int_not_pred_t, 11).
was_pttp_functor(internal, int_not_pred_t, 10).
was_pttp_functor(internal, int_not_pred_isa_t, 9).
was_pttp_functor(internal, int_not_pred_isa_t, 8).
was_pttp_functor(internal, int_not_pred_isa_t, 7).
was_pttp_functor(internal, int_not_pred_isa_t, 13).
was_pttp_functor(internal, int_not_pred_isa_t, 12).
was_pttp_functor(internal, int_not_pred_isa_t, 11).
was_pttp_functor(internal, int_not_pred_isa_t, 10).
was_pttp_functor(internal, int_not_possible_t, 9).
was_pttp_functor(internal, int_not_possible_t, 8).
was_pttp_functor(internal, int_not_possible_t, 7).
was_pttp_functor(internal, int_not_possible_t, 13).
was_pttp_functor(internal, int_not_possible_t, 12).
was_pttp_functor(internal, int_not_possible_t, 11).
was_pttp_functor(internal, int_not_possible_t, 10).
was_pttp_functor(internal, int_not_mudIsa, 9).
was_pttp_functor(internal, int_not_mudIsa, 8).
was_pttp_functor(internal, int_not_mudIsa, 7).
was_pttp_functor(internal, int_not_mudIsa, 13).
was_pttp_functor(internal, int_not_mudIsa, 12).
was_pttp_functor(internal, int_not_mudIsa, 11).
was_pttp_functor(internal, int_not_mudIsa, 10).
was_pttp_functor(internal, int_not_either_t, 9).
was_pttp_functor(internal, int_not_either_t, 8).
was_pttp_functor(internal, int_not_either_t, 7).
was_pttp_functor(internal, int_not_either_t, 13).
was_pttp_functor(internal, int_not_either_t, 12).
was_pttp_functor(internal, int_not_either_t, 11).
was_pttp_functor(internal, int_not_either_t, 10).
was_pttp_functor(internal, int_not_both_t, 9).
was_pttp_functor(internal, int_not_both_t, 8).
was_pttp_functor(internal, int_not_both_t, 7).
was_pttp_functor(internal, int_not_both_t, 13).
was_pttp_functor(internal, int_not_both_t, 12).
was_pttp_functor(internal, int_not_both_t, 11).
was_pttp_functor(internal, int_not_both_t, 10).
was_pttp_functor(internal, int_not_assuming_t, 9).
was_pttp_functor(internal, int_not_assuming_t, 8).
was_pttp_functor(internal, int_not_assuming_t, 7).
was_pttp_functor(internal, int_not_assuming_t, 13).
was_pttp_functor(internal, int_not_assuming_t, 12).
was_pttp_functor(internal, int_not_assuming_t, 11).
was_pttp_functor(internal, int_not_assuming_t, 10).
was_pttp_functor(internal, int_not_assumed_t, 9).
was_pttp_functor(internal, int_not_assumed_t, 8).
was_pttp_functor(internal, int_not_assumed_t, 7).
was_pttp_functor(internal, int_not_assumed_t, 13).
was_pttp_functor(internal, int_not_assumed_t, 12).
was_pttp_functor(internal, int_not_assumed_t, 11).
was_pttp_functor(internal, int_not_assumed_t, 10).
was_pttp_functor(internal, int_not_asserted_t, 9).
was_pttp_functor(internal, int_not_asserted_t, 8).
was_pttp_functor(internal, int_not_asserted_t, 7).
was_pttp_functor(internal, int_not_asserted_t, 13).
was_pttp_functor(internal, int_not_asserted_t, 12).
was_pttp_functor(internal, int_not_asserted_t, 11).
was_pttp_functor(internal, int_not_asserted_t, 10).
was_pttp_functor(internal, int_not_possible_t, 9).
was_pttp_functor(internal, int_not_possible_t, 8).
was_pttp_functor(internal, int_not_possible_t, 7).
was_pttp_functor(internal, int_not_possible_t, 13).
was_pttp_functor(internal, int_not_possible_t, 12).
was_pttp_functor(internal, int_not_possible_t, 11).
was_pttp_functor(internal, int_not_possible_t, 10).
was_pttp_functor(internal, int_not_askable_t, 9).
was_pttp_functor(internal, int_not_askable_t, 8).
was_pttp_functor(internal, int_not_askable_t, 7).
was_pttp_functor(internal, int_not_askable_t, 13).
was_pttp_functor(internal, int_not_askable_t, 12).
was_pttp_functor(internal, int_not_askable_t, 11).
was_pttp_functor(internal, int_not_askable_t, 10).
was_pttp_functor(internal, int_not_answerable_t, 9).
was_pttp_functor(internal, int_not_answerable_t, 8).
was_pttp_functor(internal, int_not_answerable_t, 7).
was_pttp_functor(internal, int_not_answerable_t, 13).
was_pttp_functor(internal, int_not_answerable_t, 12).
was_pttp_functor(internal, int_not_answerable_t, 11).
was_pttp_functor(internal, int_not_answerable_t, 10).
was_pttp_functor(internal, int_mudIsa, 9).
was_pttp_functor(internal, int_mudIsa, 8).
was_pttp_functor(internal, int_mudIsa, 7).
was_pttp_functor(internal, int_mudIsa, 13).
was_pttp_functor(internal, int_mudIsa, 12).
was_pttp_functor(internal, int_mudIsa, 11).
was_pttp_functor(internal, int_mudIsa, 10).
was_pttp_functor(internal, int_fallacy_t, 9).
was_pttp_functor(internal, int_fallacy_t, 8).
was_pttp_functor(internal, int_fallacy_t, 7).
was_pttp_functor(internal, int_fallacy_t, 13).
was_pttp_functor(internal, int_fallacy_t, 12).
was_pttp_functor(internal, int_fallacy_t, 11).
was_pttp_functor(internal, int_fallacy_t, 10).
was_pttp_functor(internal, int_either_t, 9).
was_pttp_functor(internal, int_either_t, 8).
was_pttp_functor(internal, int_either_t, 7).
was_pttp_functor(internal, int_either_t, 13).
was_pttp_functor(internal, int_either_t, 12).
was_pttp_functor(internal, int_either_t, 11).
was_pttp_functor(internal, int_either_t, 10).
was_pttp_functor(internal, int_both_t, 9).
was_pttp_functor(internal, int_both_t, 8).
was_pttp_functor(internal, int_both_t, 7).
was_pttp_functor(internal, int_both_t, 13).
was_pttp_functor(internal, int_both_t, 12).
was_pttp_functor(internal, int_both_t, 11).
was_pttp_functor(internal, int_both_t, 10).
was_pttp_functor(internal, int_assuming_t, 9).
was_pttp_functor(internal, int_assuming_t, 8).
was_pttp_functor(internal, int_assuming_t, 7).
was_pttp_functor(internal, int_assuming_t, 13).
was_pttp_functor(internal, int_assuming_t, 12).
was_pttp_functor(internal, int_assuming_t, 11).
was_pttp_functor(internal, int_assuming_t, 10).
was_pttp_functor(internal, int_asserted_t, 9).
was_pttp_functor(internal, int_asserted_t, 8).
was_pttp_functor(internal, int_asserted_t, 7).
was_pttp_functor(internal, int_asserted_t, 13).
was_pttp_functor(internal, int_asserted_t, 12).
was_pttp_functor(internal, int_asserted_t, 11).
was_pttp_functor(internal, int_asserted_t, 10).
was_pttp_functor(internal, int_possible_t, 9).
was_pttp_functor(internal, int_possible_t, 8).
was_pttp_functor(internal, int_possible_t, 7).
was_pttp_functor(internal, int_possible_t, 13).
was_pttp_functor(internal, int_possible_t, 12).
was_pttp_functor(internal, int_possible_t, 11).
was_pttp_functor(internal, int_possible_t, 10).
was_pttp_functor(internal, int_askable_t, 9).
was_pttp_functor(internal, int_askable_t, 8).
was_pttp_functor(internal, int_askable_t, 7).
was_pttp_functor(internal, int_askable_t, 13).
was_pttp_functor(internal, int_askable_t, 12).
was_pttp_functor(internal, int_askable_t, 11).
was_pttp_functor(internal, int_askable_t, 10).
was_pttp_functor(internal, int_answerable_t, 9).
was_pttp_functor(internal, int_answerable_t, 8).
was_pttp_functor(internal, int_answerable_t, 7).
was_pttp_functor(internal, int_answerable_t, 13).
was_pttp_functor(internal, int_answerable_t, 12).
was_pttp_functor(internal, int_answerable_t, 11).
was_pttp_functor(internal, int_answerable_t, 10).
was_pttp_functor(external, poss_t, 9).
was_pttp_functor(external, poss_t, 8).
was_pttp_functor(external, poss_t, 7).
was_pttp_functor(external, poss_t, 13).
was_pttp_functor(external, poss_t, 12).
was_pttp_functor(external, poss_t, 11).
was_pttp_functor(external, poss_t, 10).
was_pttp_functor(external, not_pred_t, 9).
was_pttp_functor(external, not_pred_t, 8).
was_pttp_functor(external, not_pred_t, 7).
was_pttp_functor(external, not_pred_t, 13).
was_pttp_functor(external, not_pred_t, 12).
was_pttp_functor(external, not_pred_t, 11).
was_pttp_functor(external, not_pred_t, 10).
was_pttp_functor(external, not_pred_isa_t, 9).
was_pttp_functor(external, not_pred_isa_t, 8).
was_pttp_functor(external, not_pred_isa_t, 7).
was_pttp_functor(external, not_pred_isa_t, 13).
was_pttp_functor(external, not_pred_isa_t, 12).
was_pttp_functor(external, not_pred_isa_t, 11).
was_pttp_functor(external, not_pred_isa_t, 10).
was_pttp_functor(external, not_possible_t, 9).
was_pttp_functor(external, not_possible_t, 8).
was_pttp_functor(external, not_possible_t, 7).
was_pttp_functor(external, not_possible_t, 13).
was_pttp_functor(external, not_possible_t, 12).
was_pttp_functor(external, not_possible_t, 11).
was_pttp_functor(external, not_possible_t, 10).
was_pttp_functor(external, not_mudIsa, 9).
was_pttp_functor(external, not_mudIsa, 8).
was_pttp_functor(external, not_mudIsa, 7).
was_pttp_functor(external, not_mudIsa, 13).
was_pttp_functor(external, not_mudIsa, 12).
was_pttp_functor(external, not_mudIsa, 11).
was_pttp_functor(external, not_mudIsa, 10).
was_pttp_functor(external, not_either_t, 9).
was_pttp_functor(external, not_either_t, 8).
was_pttp_functor(external, not_either_t, 7).
was_pttp_functor(external, not_either_t, 13).
was_pttp_functor(external, not_either_t, 12).
was_pttp_functor(external, not_either_t, 11).
was_pttp_functor(external, not_either_t, 10).
was_pttp_functor(external, not_both_t, 9).
was_pttp_functor(external, not_both_t, 8).
was_pttp_functor(external, not_both_t, 7).
was_pttp_functor(external, not_both_t, 13).
was_pttp_functor(external, not_both_t, 12).
was_pttp_functor(external, not_both_t, 11).
was_pttp_functor(external, not_both_t, 10).
was_pttp_functor(external, not_assuming_t, 9).
was_pttp_functor(external, not_assuming_t, 8).
was_pttp_functor(external, not_assuming_t, 7).
was_pttp_functor(external, not_assuming_t, 13).
was_pttp_functor(external, not_assuming_t, 12).
was_pttp_functor(external, not_assuming_t, 11).
was_pttp_functor(external, not_assuming_t, 10).
was_pttp_functor(external, not_assumed_t, 9).
was_pttp_functor(external, not_assumed_t, 8).
was_pttp_functor(external, not_assumed_t, 7).
was_pttp_functor(external, not_assumed_t, 13).
was_pttp_functor(external, not_assumed_t, 12).
was_pttp_functor(external, not_assumed_t, 11).
was_pttp_functor(external, not_assumed_t, 10).
was_pttp_functor(external, not_asserted_t, 9).
was_pttp_functor(external, not_asserted_t, 8).
was_pttp_functor(external, not_asserted_t, 7).
was_pttp_functor(external, not_asserted_t, 13).
was_pttp_functor(external, not_asserted_t, 12).
was_pttp_functor(external, not_asserted_t, 11).
was_pttp_functor(external, not_asserted_t, 10).
was_pttp_functor(external, not_possible_t, 9).
was_pttp_functor(external, not_possible_t, 8).
was_pttp_functor(external, not_possible_t, 7).
was_pttp_functor(external, not_possible_t, 13).
was_pttp_functor(external, not_possible_t, 12).
was_pttp_functor(external, not_possible_t, 11).
was_pttp_functor(external, not_possible_t, 10).
was_pttp_functor(external, not_askable_t, 9).
was_pttp_functor(external, not_askable_t, 8).
was_pttp_functor(external, not_askable_t, 7).
was_pttp_functor(external, not_askable_t, 13).
was_pttp_functor(external, not_askable_t, 12).
was_pttp_functor(external, not_askable_t, 11).
was_pttp_functor(external, not_askable_t, 10).
was_pttp_functor(external, not_answerable_t, 9).
was_pttp_functor(external, not_answerable_t, 8).
was_pttp_functor(external, not_answerable_t, 7).
was_pttp_functor(external, not_answerable_t, 13).
was_pttp_functor(external, not_answerable_t, 12).
was_pttp_functor(external, not_answerable_t, 11).
was_pttp_functor(external, not_answerable_t, 10).
was_pttp_functor(external, askable_t, 9).
was_pttp_functor(external, askable_t, 8).
was_pttp_functor(external, askable_t, 7).
was_pttp_functor(external, askable_t, 13).
was_pttp_functor(external, askable_t, 12).
was_pttp_functor(external, askable_t, 11).
was_pttp_functor(external, askable_t, 10).
was_pttp_functor(external, answerable_t, 9).
was_pttp_functor(external, answerable_t, 8).
was_pttp_functor(external, answerable_t, 7).
was_pttp_functor(external, answerable_t, 13).
was_pttp_functor(external, answerable_t, 12).
was_pttp_functor(external, answerable_t, 11).
was_pttp_functor(external, answerable_t, 10).


:- was_dynamic int_unknown_t/13.
:- kb_shared int_unknown_t/13.


:- was_dynamic answerable_t/13.
:- kb_shared answerable_t/13.


:- was_dynamic int_unknown_t/12.
:- kb_shared int_unknown_t/12.


:- was_dynamic answerable_t/12.
:- kb_shared answerable_t/12.


:- was_dynamic int_unknown_t/11.
:- kb_shared int_unknown_t/11.


:- was_dynamic answerable_t/11.
:- kb_shared answerable_t/11.


:- was_dynamic int_unknown_t/10.
:- kb_shared int_unknown_t/10.

int_unknown_t(E, F, G, C, H, A, Q, D, S, B) :-
 test_and_decrement_search_cost(A, 3, L), K=[B|C], D=[I, [unknown_t(E, F, G), B, C, H]|J], M=[I|J], call_proof(askable_t(E, F, G, K, H, L, N, M, O), askable_t(E, F, G)), call_proof(not_assumed_t(E, F, G, K, H, N, P, O, R), not_assumed_t(E, F, G)), call_proof(poss_t(E, F, G, K, H, P, Q, R, S), poss_t(E, F, G)).
int_unknown_t(E, F, G, C, H, A, O, D, Q, B) :-
 test_and_decrement_search_cost(A, 2, L), K=[B|C], D=[I, [unknown_t(E, F, G), B, C, H]|J], M=[I|J], call_proof(not_assumed_t(E, F, G, K, H, L, N, M, P), not_assumed_t(E, F, G)), call_proof(poss_t(E, F, G, K, H, N, O, P, Q), poss_t(E, F, G)).

:- was_dynamic answerable_t/10.
:- kb_shared answerable_t/10.


:- was_dynamic int_unknown_t/9.
:- kb_shared int_unknown_t/9.


:- was_dynamic answerable_t/9.
:- kb_shared answerable_t/9.

answerable_t(A, B, C, F, E, H, G, I, J) :-
 D=not_answerable_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).

:- was_dynamic int_unknown_t/8.
:- kb_shared int_unknown_t/8.


:- was_dynamic answerable_t/8.
:- kb_shared answerable_t/8.


:- was_dynamic int_unknown_t/7.
:- kb_shared int_unknown_t/7.


:- was_dynamic answerable_t/7.
:- kb_shared answerable_t/7.


:- was_dynamic int_not_possible_t/13.
:- kb_shared int_not_possible_t/13.


:- was_dynamic int_possible_t/13.
:- kb_shared int_possible_t/13.


:- was_dynamic not_possible_t/13.
:- kb_shared not_possible_t/13.


:- was_dynamic int_not_possible_t/12.
:- kb_shared int_not_possible_t/12.


:- was_dynamic int_possible_t/12.
:- kb_shared int_possible_t/12.


:- was_dynamic not_possible_t/12.
:- kb_shared not_possible_t/12.


:- was_dynamic int_not_possible_t/11.
:- kb_shared int_not_possible_t/11.


:- was_dynamic int_possible_t/11.
:- kb_shared int_possible_t/11.


:- was_dynamic not_possible_t/11.
:- kb_shared not_possible_t/11.


:- was_dynamic int_not_possible_t/10.
:- kb_shared int_not_possible_t/10.

int_not_possible_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-not_possible_t(E, F, G), B, H, C]|J], N=[I|J], ( call_proof(not_true_t(E, F, G, H, K, L, M, N, O), not_true_t(E, F, G))
 ; call_proof(not_askable_t(E, F, G, H, K, L, M, N, O), not_askable_t(E, F, G))
 ).
int_not_possible_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-not_possible_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(not_true_t(E, F, G, H, K, L, M, N, O), not_true_t(E, F, G)).

:- was_dynamic int_possible_t/10.
:- kb_shared int_possible_t/10.

int_possible_t(E, F, G, C, H, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [possible_t(E, F, G), B, C, H]|J], N=[I|J], call_proof(true_t(E, F, G, K, H, L, M, N, O), true_t(E, F, G)).

:- was_dynamic not_possible_t/10.
:- kb_shared not_possible_t/10.


:- was_dynamic int_not_possible_t/9.
:- kb_shared int_not_possible_t/9.


:- was_dynamic int_possible_t/9.
:- kb_shared int_possible_t/9.


:- was_dynamic not_possible_t/9.
:- kb_shared not_possible_t/9.

not_possible_t(A, B, C, F, E, H, G, I, J) :-
 D=possible_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_not_possible_t(A, B, C, F, E, H, G, I, J, D)
 ).
not_possible_t(A, B, C, F, E, H, G, I, J) :-
 D=possible_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).

:- was_dynamic int_not_possible_t/8.
:- kb_shared int_not_possible_t/8.


:- was_dynamic int_possible_t/8.
:- kb_shared int_possible_t/8.


:- was_dynamic not_possible_t/8.
:- kb_shared not_possible_t/8.


:- was_dynamic int_not_possible_t/7.
:- kb_shared int_not_possible_t/7.


:- was_dynamic int_possible_t/7.
:- kb_shared int_possible_t/7.


:- was_dynamic not_possible_t/7.
:- kb_shared not_possible_t/7.


:- was_dynamic int_not_assuming_t/13.
:- kb_shared int_not_assuming_t/13.


:- was_dynamic int_assuming_t/13.
:- kb_shared int_assuming_t/13.


:- was_dynamic not_assuming_t/13.
:- kb_shared not_assuming_t/13.


:- was_dynamic int_not_assuming_t/12.
:- kb_shared int_not_assuming_t/12.


:- was_dynamic int_assuming_t/12.
:- kb_shared int_assuming_t/12.


:- was_dynamic not_assuming_t/12.
:- kb_shared not_assuming_t/12.


:- was_dynamic int_not_assuming_t/11.
:- kb_shared int_not_assuming_t/11.


:- was_dynamic int_assuming_t/11.
:- kb_shared int_assuming_t/11.


:- was_dynamic not_assuming_t/11.
:- kb_shared not_assuming_t/11.


:- was_dynamic int_not_assuming_t/10.
:- kb_shared int_not_assuming_t/10.


:- was_dynamic int_assuming_t/10.
:- kb_shared int_assuming_t/10.


:- was_dynamic not_assuming_t/10.
:- kb_shared not_assuming_t/10.


:- was_dynamic int_not_assuming_t/9.
:- kb_shared int_not_assuming_t/9.


:- was_dynamic int_assuming_t/9.
:- kb_shared int_assuming_t/9.


:- was_dynamic not_assuming_t/9.
:- kb_shared not_assuming_t/9.


:- was_dynamic int_not_assuming_t/8.
:- kb_shared int_not_assuming_t/8.


:- was_dynamic int_assuming_t/8.
:- kb_shared int_assuming_t/8.


:- was_dynamic not_assuming_t/8.
:- kb_shared not_assuming_t/8.


:- was_dynamic int_not_assuming_t/7.
:- kb_shared int_not_assuming_t/7.


:- was_dynamic int_assuming_t/7.
:- kb_shared int_assuming_t/7.


:- was_dynamic not_assuming_t/7.
:- kb_shared not_assuming_t/7.


:- was_dynamic int_not_answerable_t/13.
:- kb_shared int_not_answerable_t/13.


:- was_dynamic int_answerable_t/13.
:- kb_shared int_answerable_t/13.


:- was_dynamic not_answerable_t/13.
:- kb_shared not_answerable_t/13.


:- was_dynamic int_not_answerable_t/12.
:- kb_shared int_not_answerable_t/12.


:- was_dynamic int_answerable_t/12.
:- kb_shared int_answerable_t/12.


:- was_dynamic not_answerable_t/12.
:- kb_shared not_answerable_t/12.


:- was_dynamic int_not_answerable_t/11.
:- kb_shared int_not_answerable_t/11.


:- was_dynamic int_answerable_t/11.
:- kb_shared int_answerable_t/11.


:- was_dynamic not_answerable_t/11.
:- kb_shared not_answerable_t/11.


:- was_dynamic int_not_answerable_t/10.
:- kb_shared int_not_answerable_t/10.

int_not_answerable_t(E, F, G, H, C, A, O, D, Q, B) :-
 test_and_decrement_search_cost(A, 2, L), K=[B|C], D=[I, [-not_answerable_t(E, F, G), B, H, C]|J], M=[I|J], call_proof(not_assumed_t(E, F, G, H, K, L, N, M, P), not_assumed_t(E, F, G)), call_proof(poss_t(E, F, G, H, K, N, O, P, Q), poss_t(E, F, G)).

:- was_dynamic int_answerable_t/10.
:- kb_shared int_answerable_t/10.


:- was_dynamic not_answerable_t/10.
:- kb_shared not_answerable_t/10.


:- was_dynamic int_not_answerable_t/9.
:- kb_shared int_not_answerable_t/9.


:- was_dynamic int_answerable_t/9.
:- kb_shared int_answerable_t/9.


:- was_dynamic not_answerable_t/9.
:- kb_shared not_answerable_t/9.

not_answerable_t(A, B, C, F, E, H, G, I, J) :-
 D=not_unknown_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).
not_answerable_t(A, B, C, F, E, H, G, I, J) :-
 D=not_unknown_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_not_answerable_t(A, B, C, F, E, H, G, I, J, D)
 ).

:- was_dynamic int_not_answerable_t/8.
:- kb_shared int_not_answerable_t/8.


:- was_dynamic int_answerable_t/8.
:- kb_shared int_answerable_t/8.


:- was_dynamic not_answerable_t/8.
:- kb_shared not_answerable_t/8.


:- was_dynamic int_not_answerable_t/7.
:- kb_shared int_not_answerable_t/7.


:- was_dynamic int_answerable_t/7.
:- kb_shared int_answerable_t/7.


:- was_dynamic not_answerable_t/7.
:- kb_shared not_answerable_t/7.


:- was_dynamic int_fallacy_t/13.
:- kb_shared int_fallacy_t/13.


:- was_dynamic askable_t/13.
:- kb_shared askable_t/13.


:- was_dynamic int_fallacy_t/12.
:- kb_shared int_fallacy_t/12.


:- was_dynamic askable_t/12.
:- kb_shared askable_t/12.


:- was_dynamic int_fallacy_t/11.
:- kb_shared int_fallacy_t/11.


:- was_dynamic askable_t/11.
:- kb_shared askable_t/11.


:- was_dynamic int_fallacy_t/10.
:- kb_shared int_fallacy_t/10.

int_fallacy_t(E, F, G, C, H, A, O, D, Q, B) :-
 test_and_decrement_search_cost(A, 2, L), K=[B|C], D=[I, [fallacy_t(E, F, G), B, C, H]|J], M=[I|J], call_proof(true_t(E, F, G, K, H, L, N, M, P), true_t(E, F, G)), call_proof(not_true_t(E, F, G, K, H, N, O, P, Q), not_true_t(E, F, G)).

:- was_dynamic askable_t/10.
:- kb_shared askable_t/10.


:- was_dynamic int_fallacy_t/9.
:- kb_shared int_fallacy_t/9.


:- was_dynamic askable_t/9.
:- kb_shared askable_t/9.



:- was_dynamic int_fallacy_t/8.
:- kb_shared int_fallacy_t/8.


:- was_dynamic askable_t/8.
:- kb_shared askable_t/8.


:- was_dynamic int_fallacy_t/7.
:- kb_shared int_fallacy_t/7.


:- was_dynamic askable_t/7.
:- kb_shared askable_t/7.


:- was_dynamic int_not_askable_t/13.
:- kb_shared int_not_askable_t/13.


:- was_dynamic int_askable_t/13.
:- kb_shared int_askable_t/13.


:- was_dynamic not_askable_t/13.
:- kb_shared not_askable_t/13.


:- was_dynamic int_not_askable_t/12.
:- kb_shared int_not_askable_t/12.


:- was_dynamic int_askable_t/12.
:- kb_shared int_askable_t/12.


:- was_dynamic not_askable_t/12.
:- kb_shared not_askable_t/12.


:- was_dynamic int_not_askable_t/11.
:- kb_shared int_not_askable_t/11.


:- was_dynamic int_askable_t/11.
:- kb_shared int_askable_t/11.


:- was_dynamic not_askable_t/11.
:- kb_shared not_askable_t/11.


:- was_dynamic int_not_askable_t/10.
:- kb_shared int_not_askable_t/10.

int_not_askable_t(E, F, G, H, C, A, Q, D, S, B) :-
 test_and_decrement_search_cost(A, 3, L), K=[B|C], D=[I, [-not_askable_t(E, F, G), B, H, C]|J], M=[I|J], call_proof(not_assumed_t(E, F, G, H, K, L, N, M, O), not_assumed_t(E, F, G)), call_proof(not_unknown_t(E, F, G, H, K, N, P, O, R), not_unknown_t(E, F, G)), call_proof(poss_t(E, F, G, H, K, P, Q, R, S), poss_t(E, F, G)).

:- was_dynamic int_askable_t/10.
:- kb_shared int_askable_t/10.

int_askable_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-askable_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(assumed_t(E, F, G, H, K, L, M, N, O), assumed_t(E, F, G)).
int_askable_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-askable_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(possible_t(E, F, G, H, K, L, M, N, O), possible_t(E, F, G)).

:- was_dynamic not_askable_t/10.
:- kb_shared not_askable_t/10.


:- was_dynamic int_not_askable_t/9.
:- kb_shared int_not_askable_t/9.


:- was_dynamic int_askable_t/9.
:- kb_shared int_askable_t/9.


:- was_dynamic not_askable_t/9.
:- kb_shared not_askable_t/9.

not_askable_t(A, B, C, F, E, H, G, I, J) :-
 D=askable_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).
not_askable_t(A, B, C, F, E, H, G, I, J) :-
 D=askable_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_not_askable_t(A, B, C, F, E, H, G, I, J, D)
 ).

:- was_dynamic int_not_askable_t/8.
:- kb_shared int_not_askable_t/8.


:- was_dynamic int_askable_t/8.
:- kb_shared int_askable_t/8.


:- was_dynamic not_askable_t/8.
:- kb_shared not_askable_t/8.


:- was_dynamic int_not_askable_t/7.
:- kb_shared int_not_askable_t/7.


:- was_dynamic int_askable_t/7.
:- kb_shared int_askable_t/7.


:- was_dynamic not_askable_t/7.
:- kb_shared not_askable_t/7.


:- was_dynamic int_not_asserted_t/13.
:- kb_shared int_not_asserted_t/13.


:- was_dynamic int_asserted_t/13.
:- kb_shared int_asserted_t/13.


:- was_dynamic not_asserted_t/13.
:- kb_shared not_asserted_t/13.


:- was_dynamic int_not_asserted_t/12.
:- kb_shared int_not_asserted_t/12.


:- was_dynamic int_asserted_t/12.
:- kb_shared int_asserted_t/12.


:- was_dynamic not_asserted_t/12.
:- kb_shared not_asserted_t/12.


:- was_dynamic int_not_asserted_t/11.
:- kb_shared int_not_asserted_t/11.


:- was_dynamic int_asserted_t/11.
:- kb_shared int_asserted_t/11.


:- was_dynamic not_asserted_t/11.
:- kb_shared not_asserted_t/11.


:- was_dynamic int_not_asserted_t/10.
:- kb_shared int_not_asserted_t/10.

int_not_asserted_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-not_asserted_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(not_assumed_t(E, F, G, H, K, L, M, N, O), not_assumed_t(E, F, G)).

:- was_dynamic int_asserted_t/10.
:- kb_shared int_asserted_t/10.


:- was_dynamic not_asserted_t/10.
:- kb_shared not_asserted_t/10.


:- was_dynamic int_not_asserted_t/9.
:- kb_shared int_not_asserted_t/9.


:- was_dynamic int_asserted_t/9.
:- kb_shared int_asserted_t/9.


:- was_dynamic not_asserted_t/9.
:- kb_shared not_asserted_t/9.

not_asserted_t(A, B, C, F, E, H, G, I, J) :-
 D=asserted_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_not_asserted_t(A, B, C, F, E, H, G, I, J, D)
 ).

:- was_dynamic int_not_asserted_t/8.
:- kb_shared int_not_asserted_t/8.


:- was_dynamic int_asserted_t/8.
:- kb_shared int_asserted_t/8.


:- was_dynamic not_asserted_t/8.
:- kb_shared not_asserted_t/8.


:- was_dynamic int_not_asserted_t/7.
:- kb_shared int_not_asserted_t/7.


:- was_dynamic int_asserted_t/7.
:- kb_shared int_asserted_t/7.


:- was_dynamic not_asserted_t/7.
:- kb_shared not_asserted_t/7.


:- was_dynamic int_not_possible_t/13.
:- kb_shared int_not_possible_t/13.


:- was_dynamic int_possible_t/13.
:- kb_shared int_possible_t/13.


:- was_dynamic not_possible_t/13.
:- kb_shared not_possible_t/13.


:- was_dynamic int_not_possible_t/12.
:- kb_shared int_not_possible_t/12.


:- was_dynamic int_possible_t/12.
:- kb_shared int_possible_t/12.


:- was_dynamic not_possible_t/12.
:- kb_shared not_possible_t/12.


:- was_dynamic int_not_possible_t/11.
:- kb_shared int_not_possible_t/11.


:- was_dynamic int_possible_t/11.
:- kb_shared int_possible_t/11.


:- was_dynamic not_possible_t/11.
:- kb_shared not_possible_t/11.


:- was_dynamic int_not_possible_t/10.
:- kb_shared int_not_possible_t/10.


:- was_dynamic int_possible_t/10.
:- kb_shared int_possible_t/10.


:- was_dynamic not_possible_t/10.
:- kb_shared not_possible_t/10.


:- was_dynamic int_not_possible_t/9.
:- kb_shared int_not_possible_t/9.


:- was_dynamic int_possible_t/9.
:- kb_shared int_possible_t/9.


:- was_dynamic not_possible_t/9.
:- kb_shared not_possible_t/9.


:- was_dynamic int_not_possible_t/8.
:- kb_shared int_not_possible_t/8.


:- was_dynamic int_possible_t/8.
:- kb_shared int_possible_t/8.


:- was_dynamic not_possible_t/8.
:- kb_shared not_possible_t/8.


:- was_dynamic int_not_possible_t/7.
:- kb_shared int_not_possible_t/7.


:- was_dynamic int_possible_t/7.
:- kb_shared int_possible_t/7.


:- was_dynamic not_possible_t/7.
:- kb_shared not_possible_t/7.


:- was_dynamic int_not_assumed_t/13.
:- kb_shared int_not_assumed_t/13.


:- was_dynamic int_proven_t/13.
:- kb_shared int_proven_t/13.


:- was_dynamic not_assumed_t/13.
:- kb_shared not_assumed_t/13.


:- was_dynamic int_not_assumed_t/12.
:- kb_shared int_not_assumed_t/12.


:- was_dynamic int_proven_t/12.
:- kb_shared int_proven_t/12.


:- was_dynamic not_assumed_t/12.
:- kb_shared not_assumed_t/12.


:- was_dynamic int_not_assumed_t/11.
:- kb_shared int_not_assumed_t/11.


:- was_dynamic int_proven_t/11.
:- kb_shared int_proven_t/11.


:- was_dynamic not_assumed_t/11.
:- kb_shared not_assumed_t/11.


:- was_dynamic int_not_assumed_t/10.
:- kb_shared int_not_assumed_t/10.

int_not_assumed_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-not_assumed_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(not_proven_t(E, F, G, H, K, L, M, N, O), not_proven_t(E, F, G)).
int_not_assumed_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-not_assumed_t(E, F, G), B, H, C]|J], N=[I|J], ( call_proof(not_true_t(E, F, G, H, K, L, M, N, O), not_true_t(E, F, G))
 ; call_proof(not_possible_t(E, F, G, H, K, L, M, N, O), not_possible_t(E, F, G))
 ; call_proof(not_answerable_t(E, F, G, H, K, L, M, N, O), not_answerable_t(E, F, G))
 ).
int_not_assumed_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-not_assumed_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(not_true_t(E, F, G, H, K, L, M, N, O), not_true_t(E, F, G)).

:- was_dynamic int_proven_t/10.
:- kb_shared int_proven_t/10.

int_proven_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-true_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(asserted_t(E, F, G, H, K, L, M, N, O), asserted_t(E, F, G)).
int_proven_t(E, F, G, H, C, A, Q, D, S, B) :-
 test_and_decrement_search_cost(A, 3, L), K=[B|C], D=[I, [-true_t(E, F, G), B, H, C]|J], M=[I|J], call_proof(askable_t(E, F, G, H, K, L, N, M, O), askable_t(E, F, G)), call_proof(not_unknown_t(E, F, G, H, K, N, P, O, R), not_unknown_t(E, F, G)), call_proof(poss_t(E, F, G, H, K, P, Q, R, S), poss_t(E, F, G)).
int_proven_t(E, F, G, H, C, A, O, D, Q, B) :-
 test_and_decrement_search_cost(A, 2, L), K=[B|C], D=[I, [-true_t(E, F, G), B, H, C]|J], M=[I|J], call_proof(not_unknown_t(E, F, G, H, K, L, N, M, P), not_unknown_t(E, F, G)), call_proof(poss_t(E, F, G, H, K, N, O, P, Q), poss_t(E, F, G)).
int_proven_t(E, F, G, H, C, A, O, D, Q, B) :-
 test_and_decrement_search_cost(A, 2, L), K=[B|C], D=[I, [-true_t(E, F, G), B, H, C]|J], M=[I|J], call_proof(not_unknown_t(E, F, G, H, K, L, N, M, P), not_unknown_t(E, F, G)), call_proof(poss_t(E, F, G, H, K, N, O, P, Q), poss_t(E, F, G)).

:- was_dynamic not_assumed_t/10.
:- kb_shared not_assumed_t/10.


:- was_dynamic int_not_assumed_t/9.
:- kb_shared int_not_assumed_t/9.


:- was_dynamic int_proven_t/9.
:- kb_shared int_proven_t/9.


:- was_dynamic not_assumed_t/9.
:- kb_shared not_assumed_t/9.

not_assumed_t(A, B, C, F, E, H, G, I, J) :-
 D=true_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).
not_assumed_t(A, B, C, F, E, H, G, I, J) :-
 D=true_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_not_assumed_t(A, B, C, F, E, H, G, I, J, D)
 ).

:- was_dynamic int_not_assumed_t/8.
:- kb_shared int_not_assumed_t/8.


:- was_dynamic int_proven_t/8.
:- kb_shared int_proven_t/8.


:- was_dynamic not_assumed_t/8.
:- kb_shared not_assumed_t/8.


:- was_dynamic int_not_assumed_t/7.
:- kb_shared int_not_assumed_t/7.


:- was_dynamic int_proven_t/7.
:- kb_shared int_proven_t/7.


:- was_dynamic not_assumed_t/7.
:- kb_shared not_assumed_t/7.


:- was_dynamic int_not_proven_not_t/13.
:- kb_shared int_not_proven_not_t/13.


:- was_dynamic int_proven_not_t/13.
:- kb_shared int_proven_not_t/13.


:- was_dynamic poss_t/13.
:- kb_shared poss_t/13.


:- was_dynamic int_not_proven_not_t/12.
:- kb_shared int_not_proven_not_t/12.


:- was_dynamic int_proven_not_t/12.
:- kb_shared int_proven_not_t/12.


:- was_dynamic poss_t/12.
:- kb_shared poss_t/12.


:- was_dynamic int_not_proven_not_t/11.
:- kb_shared int_not_proven_not_t/11.


:- was_dynamic int_proven_not_t/11.
:- kb_shared int_proven_not_t/11.


:- was_dynamic poss_t/11.
:- kb_shared poss_t/11.


:- was_dynamic int_not_proven_not_t/10.
:- kb_shared int_not_proven_not_t/10.

int_not_proven_not_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-poss_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(assumed_t(E, F, G, H, K, L, M, N, O), assumed_t(E, F, G)).
int_not_proven_not_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-poss_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(possible_t(E, F, G, H, K, L, M, N, O), possible_t(E, F, G)).
int_not_proven_not_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-poss_t(E, F, G), B, H, C]|J], N=[I|J], call_proof(true_t(E, F, G, H, K, L, M, N, O), true_t(E, F, G)).
int_not_proven_not_t(E, F, G, H, C, A, M, D, O, B) :-
 test_and_decrement_search_cost(A, 1, L), K=[B|C], D=[I, [-poss_t(E, F, G), B, H, C]|J], N=[I|J], ( call_proof(true_t(E, F, G, H, K, L, M, N, O), true_t(E, F, G))
 ; call_proof(possible_t(E, F, G, H, K, L, M, N, O), possible_t(E, F, G))
 ; call_proof(not_answerable_t(E, F, G, H, K, L, M, N, O), not_answerable_t(E, F, G))
 ).

:- was_dynamic int_proven_not_t/10.
:- kb_shared int_proven_not_t/10.

int_proven_not_t(E, F, G, C, H, A, Q, D, S, B) :-
 test_and_decrement_search_cost(A, 3, L), K=[B|C], D=[I, [not_true_t(E, F, G), B, C, H]|J], M=[I|J], call_proof(askable_t(E, F, G, K, H, L, N, M, O), askable_t(E, F, G)), call_proof(not_assumed_t(E, F, G, K, H, N, P, O, R), not_assumed_t(E, F, G)), call_proof(not_unknown_t(E, F, G, K, H, P, Q, R, S), not_unknown_t(E, F, G)).
int_proven_not_t(E, F, G, C, H, A, O, D, Q, B) :-
 test_and_decrement_search_cost(A, 2, L), K=[B|C], D=[I, [not_true_t(E, F, G), B, C, H]|J], M=[I|J], call_proof(not_unknown_t(E, F, G, K, H, L, N, M, P), not_unknown_t(E, F, G)), call_proof(not_assumed_t(E, F, G, K, H, N, O, P, Q), not_assumed_t(E, F, G)).
int_proven_not_t(E, F, G, C, H, A, O, D, Q, B) :-
 test_and_decrement_search_cost(A, 2, L), K=[B|C], D=[I, [not_true_t(E, F, G), B, C, H]|J], M=[I|J], call_proof(not_assumed_t(E, F, G, K, H, L, N, M, P), not_assumed_t(E, F, G)), call_proof(not_unknown_t(E, F, G, K, H, N, O, P, Q), not_unknown_t(E, F, G)).

:- was_dynamic poss_t/10.
:- kb_shared poss_t/10.


:- was_dynamic int_not_proven_not_t/9.
:- kb_shared int_not_proven_not_t/9.


:- was_dynamic int_proven_not_t/9.
:- kb_shared int_proven_not_t/9.


:- was_dynamic poss_t/9.
:- kb_shared poss_t/9.

poss_t(A, B, C, F, E, H, G, I, J) :-
 D=not_true_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; int_not_proven_not_t(A, B, C, F, E, H, G, I, J, D)
 ).
poss_t(A, B, C, F, E, H, G, I, J) :-
 D=not_true_t(A, B, C), ( identical_member_special_loop_check(D, E)
 -> fail
 ; ( identical_member_cheaper(D, F), !
 ; unifiable_member_cheaper(D, F)
 ), G=H, I=[K, [redn, D, F, E]|L], J=[K|L]
 ; fail
 ).

:- was_dynamic int_not_proven_not_t/8.
:- kb_shared int_not_proven_not_t/8.


:- was_dynamic int_proven_not_t/8.
:- kb_shared int_proven_not_t/8.


:- was_dynamic poss_t/8.
:- kb_shared poss_t/8.


:- was_dynamic int_not_proven_not_t/7.
:- kb_shared int_not_proven_not_t/7.


:- was_dynamic int_proven_not_t/7.
:- kb_shared int_proven_not_t/7.


:- was_dynamic poss_t/7.
:- kb_shared poss_t/7.


:- was_dynamic int_not_mudIsa/13.
:- kb_shared int_not_mudIsa/13.


:- was_dynamic int_mudIsa/13.
:- kb_shared int_mudIsa/13.


:- was_dynamic not_mudIsa/13.
:- kb_shared not_mudIsa/13.


:- was_dynamic int_not_mudIsa/12.
:- kb_shared int_not_mudIsa/12.


:- was_dynamic int_mudIsa/12.
:- kb_shared int_mudIsa/12.


:- was_dynamic not_mudIsa/12.
:- kb_shared not_mudIsa/12.


:- was_dynamic int_not_mudIsa/11.
:- kb_shared int_not_mudIsa/11.


:- was_dynamic int_mudIsa/11.
:- kb_shared int_mudIsa/11.


:- was_dynamic not_mudIsa/11.
:- kb_shared not_mudIsa/11.


:- was_dynamic int_not_mudIsa/10.
:- kb_shared int_not_mudIsa/10.


:- was_dynamic int_mudIsa/10.
:- kb_shared int_mudIsa/10.


:- was_dynamic not_mudIsa/10.
:- kb_shared not_mudIsa/10.


:- was_dynamic int_not_mudIsa/9.
:- kb_shared int_not_mudIsa/9.


:- was_dynamic int_mudIsa/9.
:- kb_shared int_mudIsa/9.


:- was_dynamic not_mudIsa/9.
:- kb_shared not_mudIsa/9.


:- was_dynamic int_not_mudIsa/8.
:- kb_shared int_not_mudIsa/8.


:- was_dynamic int_mudIsa/8.
:- kb_shared int_mudIsa/8.


:- was_dynamic not_mudIsa/8.
:- kb_shared not_mudIsa/8.


:- was_dynamic int_not_mudIsa/7.
:- kb_shared int_not_mudIsa/7.


:- was_dynamic int_mudIsa/7.
:- kb_shared int_mudIsa/7.


:- was_dynamic not_mudIsa/7.
:- kb_shared not_mudIsa/7.


:- was_dynamic int_not_pred_isa_t/13.
:- kb_shared int_not_pred_isa_t/13.


:- was_dynamic int_pred_isa_t/13.
:- kb_shared int_pred_isa_t/13.


:- was_dynamic not_pred_isa_t/13.
:- kb_shared not_pred_isa_t/13.


:- was_dynamic int_not_pred_isa_t/12.
:- kb_shared int_not_pred_isa_t/12.


:- was_dynamic int_pred_isa_t/12.
:- kb_shared int_pred_isa_t/12.


:- was_dynamic not_pred_isa_t/12.
:- kb_shared not_pred_isa_t/12.


:- was_dynamic int_not_pred_isa_t/11.
:- kb_shared int_not_pred_isa_t/11.


:- was_dynamic int_pred_isa_t/11.
:- kb_shared int_pred_isa_t/11.


:- was_dynamic not_pred_isa_t/11.
:- kb_shared not_pred_isa_t/11.


:- was_dynamic int_not_pred_isa_t/10.
:- kb_shared int_not_pred_isa_t/10.


:- was_dynamic int_pred_isa_t/10.
:- kb_shared int_pred_isa_t/10.


:- was_dynamic not_pred_isa_t/10.
:- kb_shared not_pred_isa_t/10.


:- was_dynamic int_not_pred_isa_t/9.
:- kb_shared int_not_pred_isa_t/9.


:- was_dynamic int_pred_isa_t/9.
:- kb_shared int_pred_isa_t/9.


:- was_dynamic not_pred_isa_t/9.
:- kb_shared not_pred_isa_t/9.


:- was_dynamic int_not_pred_isa_t/8.
:- kb_shared int_not_pred_isa_t/8.


:- was_dynamic int_pred_isa_t/8.
:- kb_shared int_pred_isa_t/8.


:- was_dynamic not_pred_isa_t/8.
:- kb_shared not_pred_isa_t/8.


:- was_dynamic int_not_pred_isa_t/7.
:- kb_shared int_not_pred_isa_t/7.


:- was_dynamic int_pred_isa_t/7.
:- kb_shared int_pred_isa_t/7.


:- was_dynamic not_pred_isa_t/7.
:- kb_shared not_pred_isa_t/7.


:- was_dynamic int_not_pred_t/13.
:- kb_shared int_not_pred_t/13.


:- was_dynamic int_pred_t/13.
:- kb_shared int_pred_t/13.


:- was_dynamic not_pred_t/13.
:- kb_shared not_pred_t/13.


:- was_dynamic int_not_pred_t/12.
:- kb_shared int_not_pred_t/12.


:- was_dynamic int_pred_t/12.
:- kb_shared int_pred_t/12.


:- was_dynamic not_pred_t/12.
:- kb_shared not_pred_t/12.


:- was_dynamic int_not_pred_t/11.
:- kb_shared int_not_pred_t/11.


:- was_dynamic int_pred_t/11.
:- kb_shared int_pred_t/11.


:- was_dynamic not_pred_t/11.
:- kb_shared not_pred_t/11.


:- was_dynamic int_not_pred_t/10.
:- kb_shared int_not_pred_t/10.


:- was_dynamic int_pred_t/10.
:- kb_shared int_pred_t/10.


:- was_dynamic not_pred_t/10.
:- kb_shared not_pred_t/10.


:- was_dynamic int_not_pred_t/9.
:- kb_shared int_not_pred_t/9.


:- was_dynamic int_pred_t/9.
:- kb_shared int_pred_t/9.


:- was_dynamic not_pred_t/9.
:- kb_shared not_pred_t/9.


:- was_dynamic int_not_pred_t/8.
:- kb_shared int_not_pred_t/8.


:- was_dynamic int_pred_t/8.
:- kb_shared int_pred_t/8.


:- was_dynamic not_pred_t/8.
:- kb_shared not_pred_t/8.


:- was_dynamic int_not_pred_t/7.
:- kb_shared int_not_pred_t/7.


:- was_dynamic int_pred_t/7.
:- kb_shared int_pred_t/7.


:- was_dynamic not_pred_t/7.
:- kb_shared not_pred_t/7.


:- was_dynamic int_not_either_t/13.
:- kb_shared int_not_either_t/13.


:- was_dynamic int_either_t/13.
:- kb_shared int_either_t/13.


:- was_dynamic not_either_t/13.
:- kb_shared not_either_t/13.


:- was_dynamic int_not_either_t/12.
:- kb_shared int_not_either_t/12.


:- was_dynamic int_either_t/12.
:- kb_shared int_either_t/12.


:- was_dynamic not_either_t/12.
:- kb_shared not_either_t/12.


:- was_dynamic int_not_either_t/11.
:- kb_shared int_not_either_t/11.


:- was_dynamic int_either_t/11.
:- kb_shared int_either_t/11.


:- was_dynamic not_either_t/11.
:- kb_shared not_either_t/11.


:- was_dynamic int_not_either_t/10.
:- kb_shared int_not_either_t/10.


:- was_dynamic int_either_t/10.
:- kb_shared int_either_t/10.


:- was_dynamic not_either_t/10.
:- kb_shared not_either_t/10.


:- was_dynamic int_not_either_t/9.
:- kb_shared int_not_either_t/9.


:- was_dynamic int_either_t/9.
:- kb_shared int_either_t/9.


:- was_dynamic not_either_t/9.
:- kb_shared not_either_t/9.


:- was_dynamic int_not_either_t/8.
:- kb_shared int_not_either_t/8.


:- was_dynamic int_either_t/8.
:- kb_shared int_either_t/8.


:- was_dynamic not_either_t/8.
:- kb_shared not_either_t/8.


:- was_dynamic int_not_either_t/7.
:- kb_shared int_not_either_t/7.


:- was_dynamic int_either_t/7.
:- kb_shared int_either_t/7.


:- was_dynamic not_either_t/7.
:- kb_shared not_either_t/7.


:- was_dynamic int_not_both_t/13.
:- kb_shared int_not_both_t/13.


:- was_dynamic int_both_t/13.
:- kb_shared int_both_t/13.


:- was_dynamic not_both_t/13.
:- kb_shared not_both_t/13.


:- was_dynamic int_not_both_t/12.
:- kb_shared int_not_both_t/12.


:- was_dynamic int_both_t/12.
:- kb_shared int_both_t/12.


:- was_dynamic not_both_t/12.
:- kb_shared not_both_t/12.


:- was_dynamic int_not_both_t/11.
:- kb_shared int_not_both_t/11.


:- was_dynamic int_both_t/11.
:- kb_shared int_both_t/11.


:- was_dynamic not_both_t/11.
:- kb_shared not_both_t/11.


:- was_dynamic int_not_both_t/10.
:- kb_shared int_not_both_t/10.


:- was_dynamic int_both_t/10.
:- kb_shared int_both_t/10.


:- was_dynamic not_both_t/10.
:- kb_shared not_both_t/10.


:- was_dynamic int_not_both_t/9.
:- kb_shared int_not_both_t/9.

int_not_both_t(true_t(B, D, F), not_true_t(C, E, G), K, I, A, P, J, R, H) :-
 test_and_decrement_search_cost(A, 1, O), unify(B, C), unify(D, E), unify(F, G), N=[H|I], J=[L, [-not_both_t(true_t(B, D, F), not_true_t(B, D, F)), H, K, I]|M], Q=[L|M], call_proof(askable_t(B, D, F, K, N, O, P, Q, R), askable_t(B, D, F)).

:- was_dynamic int_both_t/9.
:- kb_shared int_both_t/9.


:- was_dynamic not_both_t/9.
:- kb_shared not_both_t/9.


:- was_dynamic int_not_both_t/8.
:- kb_shared int_not_both_t/8.


:- was_dynamic int_both_t/8.
:- kb_shared int_both_t/8.


:- was_dynamic not_both_t/8.
:- kb_shared not_both_t/8.

not_both_t(A, B, E, D, G, F, H, I) :-
 C= (A, B), ( identical_member_special_loop_check(C, D)
 -> fail
 ; ( identical_member_cheaper(C, E), !
 ; unifiable_member_cheaper(C, E)
 ), F=G, H=[J, [redn, C, E, D]|K], I=[J|K]
 ; int_not_both_t(A, B, E, D, G, F, H, I, C)
 ).

:- was_dynamic int_not_both_t/7.
:- kb_shared int_not_both_t/7.


:- was_dynamic int_both_t/7.
:- kb_shared int_both_t/7.


:- was_dynamic not_both_t/7.
:- kb_shared not_both_t/7.
