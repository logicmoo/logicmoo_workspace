% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% families.pl [Chapter  9] Inference rules about family relationships
%
% assumes a rather conservative view of family life
?-library(dec10).
?-op(400,xfx,if).
?-op(300,xfy,and).
% example rules
aunt(X,Y) if married(X,Z) and uncle(Z,Y).
aunt(X,Y) if nephew(Y,X) and female(X).
aunt(X,Y) if niece(Y,X) and female(X).
aunt(X,Y) if sibling(X,Z) and parent(Z,Y) and female(X).
brother(X,Y) if sibling(X,Y) and male(X).
brother_in_law(X,Z) if brother(X,Y) and married(Y,Z).
brother_in_law(X,Z) if husband(X,Y) and sibling(Y,Z).
child(X,Y) if parent(Y,X).
child(X,Z) if child(X,Y) and married(Y,Z).
child(X,Y) if daughter(X,Y).
child(X,Y) if son(X,Y).
cousin(X,Y) if cousin(Y,X).
cousin(X,Y) if parent(Z,X) and aunt(Z,Y).
cousin(X,Y) if parent(Z,X) and nephew(Y,Z).
cousin(X,Y) if parent(Z,X) and niece(Y,Z).
cousin(X,Y) if parent(Z,X) and uncle(Z,Y).
daughter(X,Y) if female(X) and child(X,Y).
daughter_in_law(X,Z) if married(X,Y) and son(Y,Z).
father(X,Y) if parent(X,Y) and male(X).
father_in_law(X,Z) if father(X,Y) and married(Y,Z).
female(X) if aunt(X,Y).
female(X) if daughter(X,Y).
female(X) if daughter_in_law(X,Y).
female(X) if grandmother(X,Y).
female(X) if married(X,Y) and male(Y).
female(X) if mother(X,Y).
female(X) if mother_in_law(X,Y).
female(X) if niece(X,Y).
female(X) if sister(X,Y).
female(X) if sister_in_law(X,Y).
female(X) if wife(X,Y).
grandfather(X,Y) if grandparent(X,Y) and male(X).
grandmother(X,Y) if grandparent(X,Y) and female(X).
grandparent(X,Z) if parent(X,Y) and parent(Y,Z).
husband(X,Y) if wife(Y,X).
husband(X,Y) if male(X) and married(X,Y).
male(X) if brother(X,Y).
male(X) if brother_in_law(X,Y).
male(X) if father(X,Y).
male(X) if father_in_law(X,Y).
male(X) if grandfather(X,Y).
male(X) if husband(X,Y).
male(X) if married(X,Y) and female(Y).
male(X) if nephew(X,Y).
male(X) if son(X,Y).
male(X) if son_in_law(X,Y).
male(X) if uncle(X,Y).
married(X,Y) if married(Y,X).
married(X,Y) if husband(X,Y).
married(X,Y) if wife(X,Y).
mother(X,Y) if parent(X,Y) and female(X).
mother_in_law(X,Z) if mother(X,Y) and married(Y,Z).
nephew(X,Y) if aunt(Y,X) and male(X).
nephew(X,Y) if uncle(Y,X) and male(X).
niece(X,Y) if aunt(Y,X) and female(X).
niece(X,Y) if uncle(Y,X) and female(X).
parent(X,Y) if child(Y,X).
parent(X,Z) if married(X,Y) and parent(Y,Z).
parent(X,Y) if father(X,Y).
parent(X,Y) if mother(X,Y).
parent(X,Y) if parent(X,Z) and sibling(Y,Z).
sibling(X,Y) if brother(X,Y).
sibling(X,Y) if parent(Z,X) and parent(Z,Y) and distinct(X,Y).
sibling(X,Y) if sibling(Y,X).
sibling(X,Y) if sister(X,Y).
sister(X,Y) if sibling(X,Y) and female(X).
sister_in_law(X,Z) if sister(X,Y) and married(Y,Z).
sister_in_law(X,Z) if wife(X,Y) and sibling(Y,Z).
son(X,Y) if male(X) and child(X,Y).
son_in_law(X,Z) if married(X,Y) and daughter(Y,Z).
uncle(X,Y) if married(X,Z) and aunt(Z,Y).
uncle(X,Y) if nephew(Y,X) and male(X).
uncle(X,Y) if niece(Y,X) and male(X).
uncle(X,Y) if sibling(X,Z) and parent(Z,Y) and male(X).
wife(X,Y) if husband(Y,X).
wife(X,Y) if female(X) and married(X,Y).
