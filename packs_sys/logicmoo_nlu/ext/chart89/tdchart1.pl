% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% tdchart1.pl [Chapter  6] A top-down chart parser
%
?- reconsult('examples.pl').
?- reconsult('lexicon.pl').
?- consult('psgrules.pl').
?- reconsult('library.pl').
?- reconsult('chrtlib1.pl').
%
parse(V0,Vn,String) :-
       start_chart(V0,Vn,String),
       initial(Symbol),
       start_active(V0,Symbol).
%
% Add active edges of type Category at vertex V0 by looking up
% the rules which expand Category in the grammar
%
start_active(V0,Category) :-
        foreach(rule(Category,Categories),
        	add_edge(V0,V0,Category,Categories,[Category])).
%
add_edge(V1,V2,Category,Categories,Parse) :-
       edge(V1,V2,Category,Categories,Parse),!.
add_edge(V1,V2,Category1,[],Parse) :-
       assert_edge(V1,V2,Category1,[],Parse),
        foreach(edge(V0,V1,Category2,[Category1|Categories],Parses),
        	add_edge(V0,V2,Category2,Categories,[Parse|Parses])).
add_edge(V1,V2,Category1,[Category2|Categories],Parses) :-
       assert_edge(V1,V2,Category1,[Category2|Categories],Parses),
        foreach(edge(V2,V3,Category2,[],Parse),
        	add_edge(V1,V3,Category1,Categories,[Parse|Parses])),
        start_active(V2,Category2).
%
