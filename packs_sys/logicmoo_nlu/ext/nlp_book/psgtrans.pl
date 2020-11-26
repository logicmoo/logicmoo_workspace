% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% psgtrans.pl [Chapter  4] Translate simple phrase structure grammar rules
%
?- reconsult('library.pl').
?- op(255,xfx,--->).
%
translate((LHS_in ---> RHS_in), (LHS_out :- RHS_out)) :-
	LHS_out =.. [LHS_in,S0,Sn],
	add_vars(RHS_in,S0,Sn,RHS_out).
%
add_vars((RHS_in1,RHS_in2),S0,Sn,RHS_out) :- !,
	add_vars(RHS_in1,S0,S1,RHS_out1),
	add_vars(RHS_in2,S1,Sn,RHS_out2),
	combine(RHS_out1,RHS_out2,RHS_out).
add_vars(RHS_in,S0,Sn,true) :-
	islist(RHS_in), !,
	append(RHS_in,Sn,S0).
add_vars(RHS_in,S0,Sn,RHS_out) :-
	atom(RHS_in),
	RHS_out =.. [RHS_in,S0,Sn].
%
combine(true,RHS_out2,RHS_out2) :- !.
combine(RHS_out1,true,RHS_out1) :- !.
combine(RHS_out1,RHS_out2,(RHS_out1,RHS_out2)).
%
