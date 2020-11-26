% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% chrtlib1.pl [Chapter  6] Library predicates for database chart parsers
%
%------------------predicates for chart-in-database parsers--------------
%
% start_chart
% uses add_edge (defined by particular chart parser) to insert inactive
% edges for the words (and their respective categories) into the chart
%
start_chart(V0,V0,[]).
start_chart(V0,Vn,[Word|Words]) :-
	V1 is V0+1,
	foreach(word(Category,Word),
		add_edge(V0,V1,Category,[],[Word,Category])),
	start_chart(V1,Vn,Words).
% test
% allows use of test sentences (in examples.pl) with chart parsers
%
test(String) :-
	V0 is 1,
	initial(Symbol),
	parse(V0,Vn,String),
	foreach(edge(V0,Vn,Symbol,[],Parse),
		mwrite(Parse)),
	retractall(edge(_,_,_,_,_)).
%
% foreach - for each X do Y
%
foreach(X,Y) :-
	X,
	do(Y),
	fail.
foreach(X,Y) :-
	true.
do(Y) :- Y,!.
%
% mwrite prints out the mirror image of a tree encoded as a list
%
mwrite(Tree) :-
	mirror(Tree,Image),
	write(Image),
	nl.
%
% mirror - produces the mirror image of a tree encoded as a list
%
mirror([],[]) :- !.
mirror(Atom,Atom) :-
	atomic(Atom).
mirror([X1|X2],Image) :-
	mirror(X1,Y2),
	mirror(X2,Y1),
	append(Y1,[Y2],Image).
%
% assert_edge
% asserta(edge(...)), but gives option of displaying nature of edge created
%
assert_edge(V1,V2,Category1,[],Parse1) :-
        asserta(edge(V1,V2,Category1,[],Parse1)).
%	dbgwrite(inactive(V1,V2,Category1)).
assert_edge(V1,V2,Category1,[Category2|Categories],Parse1) :-
        asserta(edge(V1,V2,Category1,[Category2|Categories],Parse1)).
%	dbgwrite(active(V1,V2,Category1,[Category2|Categories])).
%
