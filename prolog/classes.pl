:- module(classes,[]).

%% utility
M.find(C) := Pos :-
  Pos=M.findClass(C),
  !.

M.find(C) := Pos :-
  Pos=M.findEqClasses(C),
  !.

%M.find(C) := np :- !.

M.findClass(C) := Pos :-
  C=M.Pos,!.

M.findEqClasses(C) := Pos :-
  CL=M.Pos,
  is_list(CL),
  memberchk(C,CL),!.

M.findOne(LC) := Pos :-
  member(C,LC),
  Pos=M.find(C),!.

%M.findOne(_) := np :- !.
