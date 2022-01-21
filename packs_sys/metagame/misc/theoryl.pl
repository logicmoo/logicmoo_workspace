%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%
% This file is thanks to William Cohen, who may reserve the
% copyright.
%============================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% theoryl.pl -- routines to manipulate the domain theory
%%
%% external routines:
%%    theory_clause(?Head,?Body,?Id)   :- like Quintus clause/3
%%    theory_assert(+Clause,-Id)       :- like Quintus assert/2
%%
%% syntax: domain theory is a set of labeled clauses of the form "tag::A:-B"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% syntax for theory clauses

%:- current_op(P,T,(:-)),op(P,xfy,(::)).
:- op(1200,xfy,(::)).

theory_clause(G) :- theory_clause(G,true,_Id).

theory_clause(G,H) :- theory_clause(G,H,_Id).

%% theory_clause(A,B,Id) :- clause A:-B with identifier Id is in the theory

theory_clause(G,H,Id) :-
	var(Id),
	!,
	theory_clause0(G,H,Id).
theory_clause(G,H,ith_converse(I,Id)) :-
	!,
	theory_clause0(H,Gs,Id),
	ith_and_member(I,G,Gs).
theory_clause(G,H,Id) :- 
	!,
	theory_clause0(G,H,Id).

theory_clause0(A,B,Id) :-
	(Id::C),
	( ((A:-B)=C) -> true
        ; otherwise -> (A:-B)=(C:-true) ).

ith_and_member(0,A,(A,_)) :- !.
ith_and_member(0,A,A) :- !.
ith_and_member(I,A,(_,B)) :- I>0, I1 is I-1, ith_and_member(I1,A,B).

system_predicate(Goal) :-
	functor(Goal,F,N),
	functor(PredSpec,F,N),
	\+(theory_clause(PredSpec,_,_)).



%% theory_assert(+Clause,?Id) :- create a new Horn clause 
%%    of the form (G:-H) and asign it id Idnew

:- dynamic next_clause_id/1.
next_clause_id(0).

% Default to assertz.
%theory_assert(Clause) :- theory_assert_az(Clause,_,z).

theory_assert(Clause) :- theory_assert(Clause,_).


%% theory_assert[az](+Clause,?Id) :- create a new Horn clause 
%%    of the form (G:-H) and asign it the id Idnew

theory_assert(Clause,new(N)) :- 
	clause_parts(Clause,G,H),
	retract(next_clause_id(N)),
	N1 is N+1,
	assert(next_clause_id(N1)),
	assert((new(N)::G:-H)).


%% theory_assert_az(+Clause,?Id,AZ) :- create a new Horn clause 
%%    of the form (G:-H) and asign it the id Idnew
%% If AZ = a, asserta, if z, assertz.	

theory_assert_az(Clause,new(N),AZ) :- 
	clause_parts(Clause,G,H),
	retract(next_clause_id(N)),
	N1 is N+1,
	assert(next_clause_id(N1)),
	assertaz(AZ,(new(N)::G:-H)).

assertaz(a,C) :- asserta(C).
assertaz(z,C) :- assertz(C).


clause_parts((G:-H),G,H) :- !.
clause_parts(G,G,true) :- \+functor(G,(:-),2).

%% new_id(+Id) :- test if Id is recently asserted

new_id(new(_)).


%% theory_retract(?(G:-H)) :- retract a Horn clause

theory_retract(Clause,Id) :- 
	clause_parts(Clause,G,H),
	retract((Id::G:-H)).

theory_retract(Clause) :- theory_retract(Clause,_Id).

theory_clear :- retractall((_Id::_Clause)).


theory_listing :- whenever(theory_clause(C),portray_clause(C)).
