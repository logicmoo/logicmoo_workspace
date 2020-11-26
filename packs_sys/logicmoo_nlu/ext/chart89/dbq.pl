% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% dbq.pl [Chapter  9] Evaluator for DBQ formulae
%
?- reconsult('airlines.pl').
%
% DBQ evaluator
%
?- reconsult('airlines.pl').
%
and(Prop1,Prop2) :- Prop1, Prop2.

or(Prop1,Prop2) :-  Prop1; Prop2.

exists(_,Restriction,Body) :-
         Restriction,Body, !.

all(_,Restriction,Body) :-
 not((Restriction,not(Body))).

printout(Variable) :-
        write(Variable), nl.
