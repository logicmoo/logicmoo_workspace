% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% tdchart2.pl [Chapter  6] Top down chart parser
% based on one due to Tom Khabaza, May 1987.
%
% It can be set to process agenda items depth first or breadth first
% by flipping two arguments in extend_edges.
%
% Use 'features' to load up feature-based example files, and 'atomic' to
% load up the standard atomic CF-PSG example files.
%
?- reconsult('library.pl').
?- reconsult('chrtlib2.pl').
%
start_active(Category,Vertex,Edges) :-
	findall(edge(Vertex,Vertex,Category,Categories),
		rule(Category,Categories),
		Edges).
%
new_edges(edge(V1,V2,Category1,[]),Chart,Edges) :-
	findall(edge(V0,V2,Category2,Categories),
		(Category1 = Category3,		% do they unify?
		 member(edge(V0,V1,Category2,[Category3|Categories]),Chart)),
		Edges).
%
new_edges(edge(V1,V2,Category1,[Category2|Categories]),Chart,Edges) :-
	start_active(Category2,V2, Edges1),
	findall(edge(V1,V3,Category1,Categories),
		(Category2 = Category3,		% do they unify?
		 member(edge(V2,V3,Category3,[]),Chart)),
		Edges2),
	append(Edges1,Edges2,Edges).
%
test(String) :-
	initial(Symbol),
	start_agenda(String,0,Chart1),
	start_active(Symbol,0,Agenda),
	extend_edges(Agenda,Chart1,Chart2),
	nl,
	write(String), !,
	nl,
	found_parse(Chart2).
%
