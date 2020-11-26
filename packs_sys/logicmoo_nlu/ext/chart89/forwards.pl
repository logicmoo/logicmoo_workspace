% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% forwards.pl [Chapter  9] Simple forwards inference program
%
?-library(dec10).
?-op(400,xfx,if).
?-op(300,xfy,and).
?-reconsult('families.pl').
%
% new facts are added by 'add', not 'assert'

% Rules are stored in clauses for the predicate 'rule' because we want
% to be able to backtrack over all rules, likewise facts in 'fact'.

% the top-level program

go :- read(Fact), add(Fact,'user input'), go.

add(Fact,_) :-
   fact(Fact), !.
add(Fact,'user input') :-
   assert(fact(Fact)),
   find_new_consequences(Fact).
add(Theorem,Premises) :-
   write('   |- '), write(Theorem),
   write(', from '),write(Premises),write('.'),nl,
   assert(fact(Theorem)),
   find_new_consequences(Theorem).

find_new_consequences(Fact) :-
   Theorem if Premises,
   select(Fact,Premises,Remainder),
   all_facts(Remainder),
   add(Theorem,Premises),
   fail.
find_new_consequences(Fact).

% utilities.

% select an element from an and-list and return the rest

select(Fact,Fact,true).
select(Fact,Fact and Facts,Facts).
select(Fact1,Fact2 and Facts1,Fact2 and Facts2) :-
                 select(Fact1,Facts1,Facts2).

all_facts(Fact and Facts) :-
	fact(Fact),
	all_facts(Facts).
all_facts(Fact) :-
	fact(Fact).

% some universal truths

fact(true).
fact(equals(X,X)).
fact(distinct(X,Y)) :- X \= Y.
