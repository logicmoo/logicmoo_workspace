% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% dcgtrans.pl [Chapter  4] Full DCG translator originally from C&M
%
% It would be attractive to make all operator declarations dependent on
% current_op values, but, unfortunately, the latter predicate is not in C&M.
%
?- reconsult('library.pl').
?- op(255,xfx,--->).
?- op(255,fy,'{').
?- op(255,yf,'}').
%
% translate a grammar rule
%
translate((LHS_in ---> RHS_in), (LHS_out :- RHS_out)) :-
	dcg_lhs(LHS_in,S0,Sn,LHS_out),
	dcg_rhs(RHS_in,S0,Sn,RHS_1),
	flatten2(RHS_1,RHS_out).
%
dcg_lhs((LHS_in1,LHS_in2),S0,Sn,LHS_out) :- !,
	nonvar(LHS_in1),
	islist(LHS_in2),
	tag(LHS_in1,S0,S1,LHS_out),
	append(LHS_in2,Sn,S1).
dcg_lhs(LHS_in,S0,Sn,LHS_out) :-
	nonvar(LHS_in),
	tag(LHS_in,S0,Sn,LHS_out).
%
dcg_rhs(V,S0,Sn,phrase(V,S0,Sn)) :-
   var(V), !.
dcg_rhs((RHS_in1,RHS_in2),S0,Sn,RHS_out) :- !,
	dcg_rhs(RHS_in1,S0,S1,RHS_out1),
	dcg_rhs(RHS_in2,S1,Sn,RHS_out2),
	dcg_and(RHS_out1,RHS_out2,RHS_out).
dcg_rhs((RHS_in1;RHS_in2),S0,Sn,(RHS_out1;RHS_out2)) :- !,
	dcg_or(RHS_in1,S0,Sn,RHS_out1),
	dcg_or(RHS_in2,S0,Sn,RHS_out2).
dcg_rhs({RHS_in},S0,S0,RHS_in) :- !.
dcg_rhs(!,S0,S0,!) :- !.
dcg_rhs(RHS_in,S0,Sn,true) :-
	islist(RHS_in), !,
	append(RHS_in,Sn,S0).
dcg_rhs(RHS_in,S0,Sn,RHS_out) :-
	tag(RHS_in,S0,Sn,RHS_out).
%
% Auxiliary predicates
%
dcg_or(In,S0,Sn,Out) :-
	dcg_rhs(In,S1,Sn,Out1),
	( var(S1),
	  S1 \== Sn, !,		% S1 not unifiable with Sn
	  S0=S1,
	  Out=Out1;
	  Out=(S0=S1,Out1) ).
%
dcg_and(true,In,In) :- !.
dcg_and(In,true,In) :- !.
dcg_and(In1,In2,(In1,In2)).
%
tag(In,S0,Sn,Out) :-
	In=..[Predicate|Arguments],
	append(Arguments,[S0,Sn],New_arguments),
	Out=..[Predicate|New_arguments].
%
flatten2(In,In) :- var(In), !.
flatten2((In1,In2),Out1) :- !,
	flatten1(In1,Out1,Out2),
	flatten2(In2,Out2).
flatten2(In,In).
%
flatten1(In1,(In1,In2),In2) :-
	var(In1), !.
flatten1((In1,In2),Out1,In3) :- !,
	flatten1(In1,Out1,Out2),
	flatten1(In2,Out2,In3).
flatten1(In1,(In1,In2),In2).
%
%
% phrase - standardly used with DCGs (may already be system predicate)
%
% phrase(Category,String,Left) :-
%	Category =.. List,
%	append(List,[String,Left],New),
%	Goal =.. New,
%	call(Goal).
%
%phrase(Cat,String) :- phrase(Cat,String,[]).
