% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% buchart1.pl [Chapter  6] A bottom-up chart parser
%
?- reconsult('examples.pl').
?- reconsult('lexicon.pl').
?- consult('psgrules.pl').
?- reconsult('library.pl').
?- reconsult('chrtlib1.pl').
%
parse(V0,Vn,String) :-
	start_chart(V0,Vn,String).	% defined in chrtlib1.pl
%
add_edge(V0,V1,Category,Categories,Parse) :-
	edge(V0,V1,Category,Categories,Parse),!.
%
add_edge(V1,V2,Category1,[],Parse) :-
	assert_edge(V1,V2,Category1,[],Parse),
	foreach(rule(Category2,[Category1|Categories]),
		add_edge(V1,V1,Category2,[Category1|Categories],[Category2])),
	foreach(edge(V0,V1,Category2,[Category1|Categories],Parses),
		add_edge(V0,V2,Category2,Categories,[Parse|Parses])).
add_edge(V0,V1,Category1,[Category2|Categories],Parses) :-
	assert_edge(V0,V1,Category1,[Category2|Categories],Parses),
	foreach(edge(V1,V2,Category2,[],Parse),
		add_edge(V0,V2,Category1,Categories,[Parse|Parses])).
