% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% plan_gen.pl [Chapter 10] Generation of plans involving illocutionary acts
%
% A reconstruction of Cohen's planning program - it handles all
% 4 of his examples.  The program starts planning a sequence of
% actions to achieve a goal, but stops as soon as it finds an
% initial action that it can successfully execute.  It then returns
% that action, together with all the higher-level actions to which
% it contributes.  This is a limited form of planning, but it
% suffices to give sensible answers for the Cohen examples.
% This program generates plans - it is not a plan recognition program
%
?- reconsult('library.pl').
%
?- op(23,xfy,believes).
?- op(23,xfy,wants).
?- op(23,xfy,can_do).
?- op(200,xfx,if).

listlen(List,Length) :- lenacc(List,0,Length).
lenacc([],Length,Length).
lenacc([Head|Tail],A,Length) :- A1 is A + 1, lenacc(Tail,A1,Length).
%
% member(X,Y) - if X is a member of Y, C&M 3rd ed. p50-51
%
member(X,[X|_]).
member(X,[_|Y]) :-
  member(X,Y).
%
% test problems: initial states and goals
%
initial(1,[                        % fig 2
   channel(sue,alan),
   at(alan,inside),
   at(sue,inside)
   ]).
initial(2,X) :-                    % fig 4
   write('do test 1 with request operator disabled'), nl,
   abort.
initial(3,[                        % fig 5
   channel(sue,ann),
   channel(ann,sue),
   knows_ref(ann,combination)
   ]).
initial(4,[                        % fig 6 done as fig 7
   channel(sue,alan),
   channel(alan,sue),
   channel(alan,tom),
   channel(tom,alan),
   knows_ref(tom,combination)
   ]).
%
goal(1,sue,at(alan,outside)).
goal(2,sue,at(alan,outside)).
goal(3,sue,knows_ref(sue,combination)).
goal(4,sue,knows_ref(sue,combination)).
%
%
% how to run the test problems
%
test(Id) :-
      listlen(Actions,Length),
      write([trying,plan,length,Length]), nl,
      planner(Id,Actions).

test(Id,N) :-
	listlen(Actions,N), !,
      planner(Id,Actions).
%
planner(Id,Actions) :-
      initial(Id,World),
      goal(Id,Agent,Goal),
      plan(Agent,Goal,World,Actions,[],Done),
      write_acts(Actions), nl,
      write(alternatives), read(no).

% operator(Pattern,Can_do,Want,Effect).
operator(request(Speaker,Addressee,Action),
   [Addressee can_do Action,channel(Speaker,Addressee)],
   [Speaker wants request(Speaker,Addressee,Action)],
   [Addressee believes Speaker wants Action]).

operator(cause_to_want(Agent1,Agent2,Action),
   [Agent2 can_do Action,
    Agent2 believes Agent1 wants Action],
   [],
   [Agent2 wants Action]).

operator(move(Agent,Source,Destination),
   [at(Agent,Source)],
   [Agent wants move(Agent,Source,Destination)],
   [at(Agent,Destination)]).

operator(inform(Speaker,Addressee,Proposition),
   [Proposition,channel(Speaker,Addressee)],
   [Speaker wants inform(Speaker,Addressee,Proposition)],
   [Addressee believes Speaker believes Proposition]).

operator(inform_ref(Speaker,Addressee,Predicate),
   [knows_ref(Speaker,Predicate), channel(Speaker,Addressee)],
   [Speaker wants inform_ref(Speaker,Addressee,Predicate)],
   [knows_told_ref(Addressee,Speaker,Predicate)]).

operator(convince_ref(Speaker,Addressee,Predicate),
   [knows_told_ref(Addressee,Speaker,Predicate)],
   [],
   [knows_ref(Addressee,Predicate)]).

operator(convince(Speaker,Addressee,Proposition),
   [Addressee believes Speaker believes Proposition],
   [],
   [Addressee believes Proposition]).

universal_knowledge(channel(_,_)).
universal_knowledge(at(_,_)).
universal_knowledge(_ can_do _).
universal_knowledge(knows_ref(_,_)).

db_goal(A,Goal,Goal) :- universal_knowledge(Goal), !.
db_goal(A,Goal,A believes Goal).

% Plan to achieve a single goal

plan(Agent,_ believes Goal,World,Actions1,Actions2,Done) :-
   universal_knowledge(Goal), !,
   plan(Agent,Goal,World,Actions1,Actions2,Done).
plan(Agent,Goal,World,Actions,Actions,Done) :-
   groundstate(Goal,State),
   db_goal(Agent,Goal,DBG),
   member(DBG,World),
   ((State=ground,!);true).
plan(Agent1,(Agent2 can_do Action),World,Actions1,Actions2,Done) :- !,
   operator(Action,Can_do,_,_),
   allplan(Agent1,Can_do,World,Actions1,Actions2,Done).
plan(Agent,Agent wants Want,World,Actions,Actions,Done).
plan(Agent,Agent believes Goal,World,Actions1,Actions2,Done) :-
   plan(Agent,Goal,World,Actions1,Actions2,Done).
plan(Agent,Goal,World,[Action|Actions0],Actions2,Done) :-
   operator(Action,Can_do,Want,Effects),
   member(Goal,Effects),
   write([trying,operator,Action]), nl,
   allplan(Agent,Can_do,World,Actions0,Actions1,Done),
   allplan(Agent,Want,World,Actions1,Actions2,Done),
   Done=yes.
%
% Plan to achieve a set of goals -- stop as soon as one action
% has been performed (as soon as Done becomes instantiated):

allplan(Agent,[],World,Actions,Actions,Done).
allplan(Agent,[Goal|Goals],World,Actions0,Actions2,Done) :-
   var(Done), !,
   plan(Agent,Goal,World,Actions0,Actions1,Done),
   allplan(Agent,Goals,World,Actions1,Actions2,Done).
allplan(Agent,Goals,World,Actions,Actions,Done).

groundstate(Goal,nonground) :- not(ground(Goal)), !.
groundstate(Goal,ground).
%
ground(Goal) :- numbervars(Goal,1,1).
%
%
write_acts([]) :- !,
	write('** plan is:'), nl.
write_acts([Action|Actions]) :-
	write_acts(Actions),
	write(Action), nl.
