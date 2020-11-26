pfc_add_bc_rule(Head,Body) :-
  compile_bc(Head,Body,Head1,Body1),
  assert_unique_rule(Head1,Body1).


compile_bc(Head0,Body0,Head1,Body1) :-
 compile_bc_goal(Head0,Head1,[(Head0 => Body0)|S]),
 compile_bc_body(Body0,Body1,[],S).

% compile_bc_goal(+G0,-G1,-S)

% compile_bc_goal({G0},G1,S) :- ...

compile_bc_goal(G0,G1,S) :-
  % creates a copy of term G0 with one extra
  % argument (S).  E.G.:   G0=P(A1,A2,...,An), G1=P(A1,A2,...,An,S)
  functor(G0,P,N),
  N1 is N+1,
  functor(G1,P,N1),
  pfcCopyArgs(N,G0,G1),
  arg(N1,G1,S),
  pfc_add_bc_linc(G0,G1).


compile_bc_body((A0,B0),(A1,B1),Sl,S) :-
  % compile a conjunction.
  !,
  compile_bc_goal(A0,A1,S1),
  compile_bc_body(B0,B1,[S1|Sl],S).

compile_bc_body(B0,(B1,SupportCode),Sl,S) :-
  % the final goal in the body compiles into the extended goal
  % and the call to append to concatenate the supports.
  compile_bc_goal(B0,B1,S1),
  pfc_bc_support_code(S,[S1|Sl],SupportCode).

pfc_bc_support_code(S,[Singleton],S=Singleton) :- !.

pfc_bc_support_code(SVar,SList,appendList(SList,SVar)).



% This predicate adds a Prolog rule to determine the fact from prolog
% with support = "prolog".  Example: 
% pfc_add_bc_linc(foo(A),foo(A,_123) adds foo(A,prolog) :- foo(A).

% ... pfcFact(G,S)...


pfc_add_bc_linc(Goal,ExtendedGoal) :-
  functor(ExtendedGoal,P,N),
  functor(ExtendedGoalCopy,P,N), 
  N1 is N-1,
  pfcCopyArgs(N1,ExtendedGoal,ExtendedGoalCopy),
  arg(N,ExtendedGoalCopy,prolog),
  assert_unique_rule(ExtendedGoalCopy,Goal).

appendList([],[]).
appendList([X],X).
appendList([H|T],L) :-
  appendList(T,Tl),
  append(H,Tl,L).

assert_unique_rule(Head,Body) :-
  clause(Head,Body)
    -> true
     | assert((Head :- Body)).


%   pfcCopyArgs(+N, +Goal0, +Goal)
%   copies the first N arguments of Goal0 to Goal.

pfcCopyArgs(N, Goal0, Goal) :-
	(   N =:= 0 -> true
	;   arg(N, Goal0, Arg),
	    arg(N, Goal,  Arg),
	    M is N-1,
	    pfcCopyArgs(M, Goal0, Goal)
	).



% :- dynamic foo/1, foo/2.
% :- dynamic bar/1, bar/2.

test :-
 abolish([foo/1,foo/2]),
 abolish([bar/1,bar/2]),
 abolish([qux/1,qux/2]),
 pfc_add_bc_rule(foo(X), (bar(X),qux(X))),
 pfc_add_bc_rule(bar(Y), bar(f(Y))),
 listing([foo,bar,qux]).
