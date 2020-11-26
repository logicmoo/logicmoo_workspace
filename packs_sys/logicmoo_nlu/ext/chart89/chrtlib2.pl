% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% chrtlib2.pl [Chapter  6] Library predicates for argument chart parsers
%
%-------------------predicates for chart-as-argument parsers-----------
%
start_agenda([],V0,[]).
start_agenda([Word|Words],V0,Agenda) :-
	V1 is V0+1,
	findall(edge(V0,V1,Category,[]),
		word(Category,Word),
		Agenda1),
    start_agenda(Words,V1,Agenda2),
    append(Agenda1,Agenda2,Agenda).
%
% findall - find all items satisfying goal, C&M 3rd ed. p156-157
%
/*
findall(Item,Goal,Items) :-
	asserta(found(mark)),
	call(Goal),
	asserta(found(Item)),
	fail.
findall(Item,Goal,Items) :-
	collect_found([],List),
	!,
	Items = List.
*/
%
collect_found(Items,List) :-
	getnext(Item),
	!,
	collect_found([Item|Items],List).
collect_found(List,List).
%
getnext(Item) :-
	retract(found(Item)),
	!,
	Item \== mark.
%
%
extend_edges([],Chart,Chart).
extend_edges([Edge|Agenda1],Chart1,Chart2) :-
   member(Edge,Chart1), !,
   extend_edges(Agenda1,Chart1,Chart2).
extend_edges([Edge|Agenda1], Chart1, Chart3) :-
%   dbgwrite(Edge),
    Chart2 = [Edge|Chart1],
	new_edges(Edge,Chart2,Edges),
%	add_edges(Edges,Agenda1,Agenda2),      % depth-first processing
	add_edges(Agenda1,Edges,Agenda2),      % breadth-first processing
	extend_edges(Agenda2,Chart2,Chart3).
%
add_edge(Edge,Edges,Edges) :-
	member(Edge,Edges), !.
add_edge(Edge,Edges,[Edge|Edges]).
%
add_edges([],Edges,Edges).
add_edges([Edge|Edges],Edges1,Edges3) :-
	add_edge(Edge,Edges1,Edges2),
	add_edges(Edges,Edges2,Edges3).
%
found_parse(Chart) :-
	initial(Symbol),
	member(edge(0,M,Symbol,[]),Chart),
	N is M + 1,
	not(member(edge(_,N,_,_),Chart)),
        writeq(Chart).
%
% member(X,Y) - if X is a member of Y, C&M 3rd ed. p50-51
%
/*
member(X,[X|_]).
member(X,[_|Y]) :-
	member(X,Y).
*/

%
:- include('fxamples.pl').
:- include('flexicon.pl').
:- include('fsgrules.pl').
%
/*
atomic :-
	reconsult('examples.pl'),
	reconsult('lexicon.pl'),
	consult('psgrules.pl').

:-features.
*/
