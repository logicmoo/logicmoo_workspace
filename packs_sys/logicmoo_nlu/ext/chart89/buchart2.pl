% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% buchart2.pl [Chapter  6] Bottom-up chart parser - with term unification
% to allow use of feature structures it can be set to process agenda items
% depth first or breadth first by flipping two arguments in extend_edges call.
%
% Use 'features' to load up feature-based example files, and 'atomic' to
% load up the standard atomic CF-PSG example files.
%
:- include('library.pl').
:- include('chrtlib2.pl').
%
new_edges(edge(V1,V2,Category1,[]),Chart,Edges) :-
	findall(edge(V1,V1,Category2,[Category4|Categories1]),
		(Category1 = Category4,		% do they unify?
		 rule(Category2,[Category4|Categories1])),
		Edges1),
	findall(edge(V0,V2,Category3,Categories2),
		(Category1 = Category5,		% do they unify?
		 member(edge(V0,V1,Category3,[Category5|Categories2]),Chart)),
		Edges2),
	add_edges(Edges1,Edges2,Edges).
new_edges(edge(V1,V2,Category1,[Category2|Categories]),Chart,Edges) :-
	findall(edge(V1,V3,Category1,Categories),
		(Category2 = Category3,		% do they unify?
		 member(edge(V2,V3,Category3,[]),Chart)),
		Edges).
%
test(String) :-
	start_agenda(String,0,Agenda),
    extend_edges(Agenda,[],Chart),
    found_parse(Chart).
