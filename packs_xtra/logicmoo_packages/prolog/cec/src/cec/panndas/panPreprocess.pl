/*
 *	file:		panPreprocess.pl
 *	version:	1.0
 *	date:		November 3, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains ..
 *
 *	history:
 *	891103	uwe	Added this comment
 *	900117	ulrich	Modified pan_insertMissingDecls
 *	900119	uwe	Modified pan_makeNumerals to yield "succ(succ(...(0))"
 *	900205	uwe	Modified pan_transformCondEqs:
 *			  "and" sometimes becomes "b_and",
 *			  "Expr" sometimes becomes "Expr = true"
 *			Modified pan_collectDeclarations, such that module
 *			  expressions do no lead to declarations of "+"
 *	900222	uwe	Added pan_preprocessPanndacTermOutput
 *			Modified pan_transformEquation:
 *			  "not(Expr)" sometimes becomes "Expr = false"
 *	900307	uwe	Removed pan_insertMissingDecls (no longer needed
 *			  due to new standard decls file)
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pan_preprocessPanndacOutput(TermList, NewTermList)

pan_preprocessPanndacOutput(TermList, NewTermList) :-
  pan_mapTransformCondEqs(TermList, TL1),
  pan_mapMakeNumerals(TL1, NewTermList).


% pan_preprocessPanndacTermOutput(Term, NewTerm)

pan_preprocessPanndacTermOutput(Term, NewTerm) :-
  pan_transformExpression(Term, T1),
  pan_makeNumerals(T1, NewTerm).


% pan_transformCondEqs(Term, NewTerm)

pan_transformCondEqs(op(Declaration), op(Declaration)) :-
  !.
pan_transformCondEqs('isSort<'(S1,S2), S1 < S2) :-
  !.
pan_transformCondEqs(module(Mod), module(Mod)) :-
  !.
pan_transformCondEqs(using(module(Mod),Used), using(module(Mod),Used)) :-
  !.
pan_transformCondEqs('->'(Cond,Conseq), '=>'(NewCond,NewConseq)) :-
  !,
  pan_transformCondition(Cond, NewCond),
  pan_transformEquation(Conseq, NewConseq).
pan_transformCondEqs(Term, NewTerm) :-
  pan_transformEquation(Term, NewTerm).

pan_mapTransformCondEqs([], []).
pan_mapTransformCondEqs([Arg|ArgRest], [NewArg|NewArgRest]) :-
  pan_transformCondEqs(Arg, NewArg),
  pan_mapTransformCondEqs(ArgRest, NewArgRest).

pan_transformCondition('and'(Cond1,Cond2), 'and'(NewCond1,NewCond2)) :-
  !,
  pan_transformCondition(Cond1, NewCond1),
  pan_transformCondition(Cond2, NewCond2).
pan_transformCondition(Term, NewTerm) :-
  pan_transformEquation(Term, NewTerm).

pan_transformEquation(T1 = T2, NewT1 = NewT2) :-
  !,
  pan_transformExpression(T1, NewT1),
  pan_transformExpression(T2, NewT2).
pan_transformEquation(not(T1), NewT1 = false) :-
  !,
  pan_transformExpression(T1, NewT1).
pan_transformEquation(T1, NewT1 = true) :-
  pan_transformExpression(T1, NewT1).

pan_transformExpression((and), (b_and)) :-
  !.
pan_transformExpression(Term, Term) :-
  atomic(Term),
  !.
pan_transformExpression(Term, NewTerm) :-
  Term =.. [Functor|ArgList],
  ( Functor = (and) ->
      NewFunctor = (b_and)
    ; NewFunctor = Functor
  ),
  pan_mapTransformExpression(ArgList, NewArgList),
  NewTerm =.. [NewFunctor|NewArgList].

pan_mapTransformExpression([], []).
pan_mapTransformExpression([Arg|ArgRest], [NewArg|NewArgRest]) :-
  pan_transformExpression(Arg, NewArg),
  pan_mapTransformExpression(ArgRest, NewArgRest).


% pan_makeNumerals(Term, NewTerm)
% transforms atoms like '12' into succ(succ(succ(....succ(0)...))).

pan_makeNumerals(Atom, UnaryNotation) :-
  atom_chars(Atom, L),
  number_chars(Number, L),
  integer(Number),
  Number >= 0,
  pan_naturalToUnaryNotation(Number, UnaryNotation),
  !.
pan_makeNumerals(Atom, Atom) :-
  atomic(Atom),
  !.
pan_makeNumerals(Term, NewTerm) :-
  Term =.. [Functor|ArgList],
  pan_mapMakeNumerals(ArgList, NewArgList),
  NewTerm =.. [Functor|NewArgList].

pan_mapMakeNumerals([], []).
pan_mapMakeNumerals([Arg|ArgRest], [NewArg|NewArgRest]) :-
  pan_makeNumerals(Arg, NewArg),
  pan_mapMakeNumerals(ArgRest, NewArgRest).

pan_naturalToUnaryNotation(0, 0) :-
  !.
pan_naturalToUnaryNotation(N, succ(U)) :-
  M is N - 1,
  pan_naturalToUnaryNotation(M, U).



% % pan_insertMissingDecls(Specification, NewSpecification)
% 
% pan_insertMissingDecls(Specification, NewSpecification) :-
%   pan_mapCollectDeclarations(Specification, [], Declarations, _),
%   append(Specification, Declarations, NewSpecification).
% /* change the order of Specification and Declarations, so that
%    module expressions remain in the first position 
%    17.01.90 ulrich
% */
% 
% pan_collectDeclarations(module(_), Decls, Decls, _) :-
%   !.
% pan_collectDeclarations(using(module(_),_), Decls, Decls, _) :-
%   !.
% pan_collectDeclarations(Term, Decls, NewDecls, Mode) :-
%   atomic(Term),
%   !,
%   pan_collectDecl(Term/0, Decls, NewDecls, Mode).
% pan_collectDeclarations(T1 = T2, Decls, NewDecls, _) :-
%   pan_collectDeclarations(T1, Decls, D1, eq),
%   pan_collectDeclarations(T2, D1, NewDecls, eq).
% pan_collectDeclarations('=>'(T1,T2), Decls, NewDecls, _) :-
%   pan_collectDeclarations(T1, Decls, D1, eq),
%   pan_collectDeclarations(T2, D1, NewDecls, eq).
% pan_collectDeclarations(op(D), Decls, NewDecls, _) :-
%   pan_collectDeclarations(D, Decls, NewDecls, dcl).
% pan_collectDeclarations(Term, Decls, NewDecls, Mode) :-
%   Term =.. [Functor|Args],
%   functor(Term, _, Arity),
%   pan_collectDecl(Functor/Arity, Decls, D1, Mode),
%   pan_mapCollectDeclarations(Args, D1, NewDecls, Mode).
% 
% pan_mapCollectDeclarations([], Decls, Decls, _).
% pan_mapCollectDeclarations([Arg|ArgRest], Decls, NewDecls, Mode) :-
%   pan_collectDeclarations(Arg, Decls, D1, Mode),
%   pan_mapCollectDeclarations(ArgRest, D1, NewDecls, Mode).
% 
% pan_collectDecl(integer/0, Decls, NewDecls, dcl) :-
%   !,
%   pan_insert(Decls, (natural < integer), NewDecls).
% pan_collectDecl(natural/0, Decls, NewDecls, dcl) :-
%   !,
%   pan_insert(Decls, (natural < integer), NewDecls).
% pan_collectDecl((+)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((+),(integer * integer -> integer))), D2),
%   pan_insert(D2, op(:((+),(natural * natural -> natural))), NewDecls).
% pan_collectDecl((+)/1, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((+),(integer -> integer))), D2),
%   pan_insert(D2, op(:((+),(natural -> natural))), NewDecls).
% pan_collectDecl((-)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((-),(integer * integer -> integer))), NewDecls).
% pan_collectDecl((-)/1, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((-),(integer -> integer))), NewDecls).
% pan_collectDecl((*)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((*),(integer * integer -> integer))), D2),
%   pan_insert(D2, op(:((*),(natural * natural -> natural))), NewDecls).
% pan_collectDecl((/)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((/),(integer * integer -> integer))), D2),
%   pan_insert(D2, op(:((/),(natural * natural -> natural))), NewDecls).
% pan_collectDecl((mod)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((mod),(integer * integer -> integer))), D2),
%   pan_insert(D2, op(:((mod),(natural * natural -> natural))), NewDecls).
% pan_collectDecl((rem)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, :(op(rem, 300, xfx),(integer * integer -> integer)), D2),
%   pan_insert(D2, :(op(rem, 300, xfx),(natural * natural -> natural)), NewDecls).
% pan_collectDecl((**)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, :(op(**, 200, xfx),(integer * natural -> integer)), D2),
%   pan_insert(D2, :(op(**, 200, xfx),(natural * natural -> natural)), NewDecls).
% pan_collectDecl((abs)/1, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, :(op(abs, 200, fx),(integer -> natural)), NewDecls).
% /*
% pan_collectDecl((and)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, op(:((and),(bool * bool -> bool))), NewDecls).
% */
% /* cec is not allowed to declare an operator and
%    17.01.90 ulrich
% */
% pan_collectDecl((or)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, :(op(or, 950, xfx),(bool * bool -> bool)), NewDecls).
% pan_collectDecl((xor)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, :(op(xor, 950, xfx),(bool * bool -> bool)), NewDecls).
% pan_collectDecl((not)/1, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, op(:((not),(bool -> bool))), NewDecls).
% pan_collectDecl((<)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((<),(integer * integer -> bool))), NewDecls).
% pan_collectDecl((<=)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, :(op(<=, 700, xfx),(integer * integer -> bool)), NewDecls).
% pan_collectDecl((>)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((>),(integer * integer -> bool))), NewDecls).
% pan_collectDecl((>=)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, op(:((>=),(integer * integer -> bool))), NewDecls).
% pan_collectDecl((/=)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, (natural < integer), D1),
%   pan_insert(D1, :(op(/=, 700, xfx),(integer * integer -> bool)), NewDecls).
% pan_collectDecl((<->)/2, Decls, NewDecls, eq) :-
%   !,
%   pan_insert(Decls, :(op(<->, 1050, xfx),(bool * bool -> bool)), NewDecls).
% % pan_collectDecl(Number/0, Decls, NewDecls, eq) :-
% %   integer(Number),
% %   Number >= 0,
% %   !,
% %   pan_insert(Decls, (natural < integer), D1),
% %   pan_insert(D1, op(:(Number, natural)), NewDecls).
% % pan_collectDecl(Number/0, Decls, NewDecls, eq) :-
% %   integer(Number),
% %   !,
% %   pan_insert(Decls, op(:(Number, integer)), NewDecls).
% pan_collectDecl(_, Decls, Decls, _).
