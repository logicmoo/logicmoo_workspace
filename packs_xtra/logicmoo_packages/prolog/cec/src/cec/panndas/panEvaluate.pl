/*
 *	file:		panEvaluate.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the predicate called for transfering parts of 
 *	specifications from CEC to PAnndA-S.
 *
 *	history:
 *	891010	js	Added this comment
 *	891128	uh	Added description
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pan_evaluate(PanndasExpresssion, Result, ShouldReturnResult)

% "eval" is no longer a command of the PAnndA-C editor,
% thus "pan_evaluate" can only be called via "get_object" and there should be
% no incorrect calls to this predicate

% ----- variables -----
pan_evaluate(Var, _, _) :-
  var(Var),
  !, write('system error in panndac-cec-connection!'),
  nl,
  write('(pan_evaluate called with a variable)'),
  nl,
  fail.

% ----- other expressions -----
pan_evaluate(Command, Result, ShouldReturnResult) :-
  Command =.. [Functor|Args],
  name(Functor, String),
  pan_toLower(String, LcString),
  name(LcFunctor, LcString),
  LcCommand =.. [LcFunctor|Args],
  !, pan_lcEvaluate(LcCommand, Result, ShouldReturnResult).

% ----- catchall -----
pan_evaluate(OtherExpression, _, _) :-
  write('system error in panndac-cec-connection!'),
  nl,
  write('(pan_evaluate called with '),
  write(OtherExpression),
  write(')'),
  nl,
  fail.


% pan_lcEvaluate(PanndasExpresssion, Result, ShouldReturnResult)

% ----- standard cec commands -----
% pan_lcEvaluate(applyrule(Term, Rule), X, yes) :-
%   !, applyRule(Term, Rule, X).
% pan_lcEvaluate(eval(Term), X, yes) :-
%   !, eval(Term, X).
% pan_lcEvaluate(norm(Term), X, yes) :-
%   !, norm(Term, X).
% pan_lcEvaluate(prove(Term1,Term2), _, no) :-
%   !, prove(Term1,Term2).
pan_lcEvaluate(getequation(N), X, yes) :-
  !, pan_getEquation(N, X).
pan_lcEvaluate(getrule(N), X, yes) :-
  !, pan_getRule(N, X).
pan_lcEvaluate(getnonopeqn(N), X, yes) :-
  !, pan_getNonopEqn(N, X).
% ----- catchall -----
pan_lcEvaluate(OtherExpression, _, _) :-
  write('system error in panndac-cec-connection!'),
  nl,
  write('(pan_evaluate called with '),
  write(OtherExpression),
  write(')'),
  nl,
  fail.

