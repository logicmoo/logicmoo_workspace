%:- module(logicmoo_cf,[ ]).

:- use_module(library(logicmoo_common)).

%   Author : R.A.O'Keefe
%   Updated: 10 March 1984
%   Purpose: Convert a formula in FOPC to clausal form.
%   Needs  : ord_union/3 from UTIL:ORDSET.PL.

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
	clausal_form_of_negation/2.
	

/*
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
*/

:- op(700, xfx, [contains,literally_contains,does_not_literally_contain]).
:- op(910,  fy, ~).
:- op(920, xfy, and).
:- op(930, xfy, or).
:- op(940, xfx, [=>, <=>]).


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


[] does_not_literally_contain _Anything.
[Head|Tail] does_not_literally_contain Something :-
	Head \== Something,
	Tail does_not_literally_contain Something.


/*----------------------------------------------------------------------
    Debugging kit.
	portray_sentence(ListOfClauses)
	    displays a list of clauses, one per line.
	portray_clause(Clause)
	    displays a single clause in "Kowalski notation"
	t(Formula)
	    translates a formula and prints the result.
*/
:- public
	t1/0,t9/0,t/1.


portray_sentence([Clause]) :- !,
	portray_clause(Clause),
	nl.
portray_sentence([Clause|Clauses]) :-
	portray_clause(Clause),
	write(' AND'), nl,
	portray_sentence(Clauses).
portray_sentence([]) :-
	write('TRUE'), nl.


portray_clause(clause(PosAtoms, NegAtoms)) :-
	numbervars(PosAtoms, 0, N),
	numbervars(NegAtoms, N, _),
	portray_clause(PosAtoms, ' v '),
	write(' <- '),
	portray_clause(NegAtoms, ' & '),
	fail.
portray_clause(_).


portray_clause([Atom], _) :- !,
	print(Atom).
portray_clause([Atom|Atoms], Separator) :-
	print(Atom), write(Separator),
	portray_clause(Atoms, Separator).
portray_clause([], _) :-
	write([]).


t(X) :-
	clausal_form(X, Y),
  wdmsg(Y).
	%portray_sentence(Y).

t0 :- t(some(X, livesAt(X, green_house) and drinks(X, coffee))).

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

:- fixup_exports.

:- dynamic user:prolog_load_file/2.
:- multifile user:prolog_load_file/2.
%:- use_module(library(logicmoo_common)).
%user:prolog_load_file(Spec, Options):- maybe_load_clif_file(Spec, Options),!.


%:- baseKB:ensure_loaded(baseKB:library('logicmoo/common_logic/common_logic_clif.pfc')).

%:- kif_compile.




   
