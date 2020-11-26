%% :- consult('sample.pl').

%%%% DOMAIN

domain(domain(A,B,C,D,E,F),A).

requirements(domain(A,B,C,D,E,F),Requirement) :-
	member(Requirement,B).

types(domain(A,B,C,D,E,F),Type) :-
	member(Type,C).

predicates(domain(A,B,C,D,E,F),Predicate) :-
	member(Predicate,D).

functions(domain(A,B,C,D,E,F),Function) :-
	member(Function,E).

%% derived(domain(A,B,C,D,E,F),Function) :-
%% 	member(Function,E).

actions(domain(A,B,C,D,E,F),Action) :-
	member(Action,F).

%%%% PROBLEM

problem(problem(Problem,B,C,D,E,F),Problem).

problemDomain(problem(A,B,C,D,E,F),B).

objects(problem(A,B,C,D,E,F),Object) :-
	member(Object,C).

init(problem(A,B,C,D,E,F),Assertion) :-
	member(Assertion,D).

goal(problem(A,B,C,D,E,F),Goals) :-
	member(Goals,E).

pddlGoals(problem(A,B,C,D,E,F),E).

%%%% HELPERS

%% listActions(Actions) :-
%% 	findall(domain(A,B,C,D,E,F),domain(A,B,C,D,E,F),Domains),
%% 	member(Domain,Domains),
%% 	findall(Action,actions(Domain,Action),Actions).

%% display :-
%% 	listActions(Actions),
%% 	member(Action,Actions),
%% 	see(Action),
%% 	fail.
%% display.
