% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% pro_patr.pl [Chapter  7] Prolog PATR interpreter
% - originally written by Bob Carpenter (except where noted)

% Simple test grammars can be found in the files eng_syn.ptr,
% eng_lex.ptr and french.ptr which demonstrate the format for rules.

% The operator precedences assume DEC10 Prolog:
?- library(dec10).
%
?- reconsult('dagunify.pl').
?- op(600,xfx,===).
?- op(500,xfx,--->).
?- op(400,xfx,ule).
?- op(600,xfx,ord).

X === Y  :-
  denotes(X,Z),
  denotes(Y,Z).

denotes(Var,Var) :- var(Var), !.
denotes(Atom,Atom) :- atomic(Atom), !.
denotes(Dag:Path,Value) :-
  pathval(Dag,Path,Value,_).

% Left-corner recognizer due Pereira and Shieber, taken from
% their book _Prolog and Natural Language Analysis_ p. 180.
leaf(Dag) --> [Word], {Dag ord Word}.
leaf(Dag) --> {_ ule Dag ---> []}.

recognize(Dag1) -->
  leaf(Dag0),
  left_corner(Dag0,Dag1).

left_corner(Dag1,Dag2) --> [], {unify(Dag1,Dag2)}.
left_corner(Dag1, Dag2) -->
  {_ ule Dag0 ---> [Dag1|Dags]},
  recognize_rest(Dags),
  left_corner(Dag0,Dag2).

recognize_rest([]) --> [].
recognize_rest([Dag|Dags]) -->
  recognize(Dag),
  recognize_rest(Dags).

% The 'test' predicate takes a list of words, and will print out
% the 'extensional' part of the dag that it gets recognized as (i.e.
% the part without path equivalences).

test(String) :-
  recognize(Category,String,[]),
  pp_dag(Category), nl.

% pretty printing routine to print out legible dags without
% specifying path equations.

pp_dag(Dag) :-
  nl, pp_dag(Dag,0,yes).

pp_dag(VarTail,Column,yes) :-
  var(VarTail),!.
pp_dag(VarTail,Column,no) :-
  var(VarTail), write(VarTail), !.
pp_dag(_,Column,yes) :-
  nl, tab(Column), fail.
pp_dag([F : Dag1|RestDag],Column,_) :-
  !, write(F), write(' : '),
  atom_length(F,N), NewColumn is Column + N + 3,
  pp_dag(Dag1,NewColumn,no),
  pp_dag(RestDag,Column,yes).
pp_dag(Atom,_,_)  :-
  !, write(Atom).

atom_length(Atom,N) :-
  name(Atom,List),
  length(List,N).
