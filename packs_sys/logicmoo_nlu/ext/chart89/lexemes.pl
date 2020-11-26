% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% lexemes.pl [Chapter  7] Implementing lexical macros and WFCs
%
?- reconsult('pro_patr.pl').
?- op(600,xfx,exeme).

denotes(R + S, R + S) :- !.

macro(syn_iV,L) :-
  L : syn : cat  === v,
  L : syn : arg0 : cat  === np,
  L : syn : arg0 : case === nom.

macro(syn_tV,L) :-
  macro(syn_iV,L),
  L : syn : arg1 : cat  === np,
  L : syn : arg1 : case === acc.

macro(mor_regV,L) :-
  L : mor : root === R,
  L : mor : form1 === R + '',
  L : mor : form2 === R + '',
  L : mor : form3 === R + s,
  L : mor : form4 === R + ed,
  L : mor : form5 === R + ed,
  L : mor : form6 === R + ed,
  L : mor : form7 === R + ing.

L exeme love :-
  L : mor : root === love,
  macro(syn_tV,L),
  macro(mor_regV,L),
  L : sem === love2a.

W ord Form :-
  L : mor : form3 === Form,
  L exeme _,
  W : sem === L : sem,
  W : mor === Form,
  W : syn === L : syn,
  W : syn : arg0 : per === 3,
  W : syn : arg0 : num === sing,
  W : syn : tense === pres.
