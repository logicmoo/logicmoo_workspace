/*
 *	file:		panGetEquation.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the implementation of the transferation of
 *	parts of CEC-specifications to PAnndA-S.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

pan_getEquation(N, Equation) :-
  '$equation'(N, ([], [Eqn])),
  !, pan_getVarsAndSorts(Eqn, [], _, VSL),
  ( VSL = [] ->
      Equation = Eqn
    ; pan_listToSequence(VSL, (';'), DomainList),
      Equation = pan_forall(DomainList,Eqn)
  ).
pan_getEquation(N, Equation) :-
  '$equation'(N, (CondList, [Conseq])),
  !, pan_getVarsAndSorts((CondList, Conseq), [], _, VSL),
  ( VSL = [] ->
      Equation = '->'(CL,Conseq)
    ; pan_listToSequence(VSL, (';'), DomainList),
      Equation = pan_forall(DomainList,'->'(CL,Conseq))
  ),
  pan_listToSequence(CondList, ('and'), CL).
pan_getEquation(N, _) :-
  !, write('no equation #'),
  write(N),
  write(' available.'),
  nl,
  fail.
  

pan_getRule(N, Rule) :-
  '$rule'(N, ([], [Eqn])),
  !, pan_getVarsAndSorts(Eqn, [], _, VSL),
  ( VSL = [] ->
      Rule = Eqn
    ; pan_listToSequence(VSL, (';'), DomainList),
      Rule = pan_forall(DomainList,Eqn)
  ).
pan_getRule(N, Rule) :-
  '$rule'(N, (CondList, [Conseq])),
  !, pan_getVarsAndSorts((CondList, Conseq), [], _, VSL),
  ( VSL = [] ->
      Rule = '->'(CL,Conseq)
    ; pan_listToSequence(VSL, (';'), DomainList),
      Rule = pan_forall(DomainList,'->'(CL,Conseq))
  ),
  pan_listToSequence(CondList, ('and'), CL).
pan_getRule(N, _) :-
  !, write('no rule #'),
  write(N),
  write(' available.'),
  nl,
  fail.
  

pan_getNonopEqn(N, NonopEqn) :-
  '$nonoperational equation'(N, ([], [Eqn])),
  !, pan_getVarsAndSorts(Eqn, [], _, VSL),
  ( VSL = [] ->
      NonopEqn = Eqn
    ; pan_listToSequence(VSL, (';'), DomainList),
      NonopEqn = pan_forall(DomainList,Eqn)
  ).
pan_getNonopEqn(N, NonopEqn) :-
  '$nonoperational equation'(N, (CondList, [Conseq])),
  !, pan_getVarsAndSorts((CondList, Conseq), [], _, VSL),
  ( VSL = [] ->
      NonopEqn = '->'(CL,Conseq)
    ; pan_listToSequence(VSL, (';'), DomainList),
      NonopEqn = pan_forall(DomainList,'->'(CL,Conseq))
  ),
  pan_listToSequence(CondList, ('and'), CL).
pan_getNonopEqn(N, _) :-
  !, write('no nonoperational equation #'),
  write(N),
  write(' available.'),
  nl,
  fail.
  

% pan_listToSequence(List, Functor, Sequence)

pan_listToSequence([E|R], F, Res) :-
  pan_listToSeq(R, E, F, Res).

pan_listToSeq([], T, _, T) :-
  !.  
pan_listToSeq([E|R], T, F, Res) :-
  T1 =.. [F, T, E],
  pan_listToSeq(R, T1, F, Res).
