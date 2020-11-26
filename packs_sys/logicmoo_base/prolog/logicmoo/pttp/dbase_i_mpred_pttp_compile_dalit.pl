:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_pttp_compile,[]).
:- endif.

:- abolish(pttp_prove,6).
:- abolish(search_cost,3).
:- abolish(search,6).
:- abolish(make_wrapper,3).
:- abolish(add_features,2).
:- abolish(add_args,13).


%%% ****h* PTTP/PTTP-dalit
%%% COPYRIGHT
%%%   Copyright (c) 1988-2003 Mark E. Stickel, SRI International, Menlo Park, CA 94025  USA
%%% 
%%%   Permission is hereby granted, free of charge, to any person obtaining a
%%%   copy of this software and associated documentation files (the "Software"),
%%%   to deal in the Software without restriction, including without limitation
%%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%%   and/or sell copies of the Software, and to permit persons to whom the
%%%   Software is furnished to do so, subject to the following conditions:
%%% 
%%%   The above copyright notice and this permission notice shall be included
%%%   in all copies or substantial portions of the Software.
%%% 
%%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%%   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%%   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%%   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%%   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%%   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%%   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% DESCRIPTION
%%%  
%%%      A Prolog Technology Theorem Prover
%%%  
%%%               Mark E. Stickel
%%%  
%%%   This file contains changes to PTTP to use
%%%   depth-first iterative deepening dalit_search with bound on
%%%   D_Alit (maximum number of A-literals on a branch)
%%%   instead of
%%%   D_Inf (total number of subgoals).
%%%
%%%   To use, load pttp and then load this file
%%%   to replace changed definitions.
%%% SOURCE

:- abolish(pttp_prove,6).
pttp_prove(Goal,Max,Min,Inc,ProofIn,ProofOut):-dalit_prove(Goal,Max,Min,Inc,ProofIn,ProofOut).

dalit_prove(Goal,Max,Min,Inc,ProofIn,ProofOut) :-
	expand_input_proof(ProofIn,PrfEnd),
	PrevInc is Min + 1,
	dalit_add_args(INFO,Goal,_,_,[],_,_,[],[],DepthIn,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),
	!,
	timed_call(dalit_search(Goal1,Max,Min,Inc,PrevInc,DepthIn),'Proof'),
	contract_output_proof(ProofOut1,ProofOut),
	write_proof(ProofOut1),
	nl.

dalit_prove(Goal,Max,Min,Inc,ProofIn) :-
	dalit_prove(Goal,Max,Min,Inc,ProofIn,_).

dalit_prove(Goal,Max,Min,Inc) :-
	dalit_prove(Goal,Max,Min,Inc,[],_).

dalit_prove(Goal,Max,Min) :-
	dalit_prove(Goal,Max,Min,1,[],_).

dalit_prove(Goal,Max) :-
	dalit_prove(Goal,Max,0,1,[],_).

dalit_prove(Goal) :-
	dalit_prove(Goal,10000,0,1,[],_).

:- abolish(search_cost,3).
search_cost(Body,HeadArgs,N):-dalit_search_cost(Body,HeadArgs,N).
dalit_search_cost(Body,HeadArgs,N) :-
	Body = dalit_search_cost(M) ->
		N = M;
	Body = (A , B) ->
		(A = dalit_search_cost(M) ->	% if first conjunct is dalit_search_cost(M),
			N = M;		% dalit_search cost of conjunction is M
		%true ->
			dalit_search_cost(A,HeadArgs,N1),
			dalit_search_cost(B,HeadArgs,N2),
			max(N1,N2,N));
	Body = (A ; B) ->
		dalit_search_cost(A,HeadArgs,N1),
		dalit_search_cost(B,HeadArgs,N2),
		min(N1,N2,N);
	pttp_builtin(Body) ->
		N = 0;
	%true ->
		N = 1.

:- abolish(search,6).
search(Goal,Max,Min,Inc,PrevInc,DepthIn):-dalit_search(Goal,Max,Min,Inc,PrevInc,DepthIn).

dalit_search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn) :-
	Min > Max,
	!,
	fail.
dalit_search(Goal,_Max,Min,_Inc,PrevInc,DepthIn) :-
        write_search_progress(Min),
	DepthIn = Min,
	call(Goal),
	true.			% should fail if solution found previously
dalit_search(Goal,Max,Min,Inc,_PrevInc,DepthIn) :-
	Min1 is Min + Inc,
	dalit_search(Goal,Max,Min1,Inc,Inc,DepthIn).

:- abolish(make_wrapper,3).
make_wrapper(Body,HeadArgs,N):-dalit_make_wrapper(Body,HeadArgs,N).

dalit_make_wrapper(_DefinedPreds,[query,0],true) :-
	!.
dalit_make_wrapper(DefinedPreds,[P,N],Result) :-
	functor(Goal,P,N),
	Goal =.. [P|Args],
	ExtraArgs = [PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut],
	list_append(Args,ExtraArgs,Args1),
	Head =.. [P|Args1],
	internal_functor(P,IntP),
	list_length_pttp(ExtraArgs,NExtraArgs),
	NN is N + NExtraArgs + 1,
	(identical_member_special([IntP,NN],DefinedPreds) ->
	        list_append(ExtraArgs,[GoalAtom],ExtraArgs2),
		list_append(Args,ExtraArgs2,Args2),
		IntHead =.. [IntP|Args2];
	%true ->
		IntHead = fail),
	(is_negative_functor(P) ->
		negated_literal(Goal,PosGoal),
		Red = redn,  % atom in proof is negation of actual literal
		C1Ancestors = NegAncestors,
		C2Ancestors = PosAncestors;
	%true ->
		PosGoal = Goal,
		Red = red,
		C1Ancestors = PosAncestors,
		C2Ancestors = NegAncestors),
	(N = 0 ->	% special case for propositional calculus
		V1 = (identical_member_special(GoalAtom,C2Ancestors) , !);
	%true ->
		V1 = ((identical_member_special(GoalAtom,C2Ancestors) , !);
		       unifiable_member(GoalAtom,C2Ancestors))),
	V2 = (
		 ProofIn = [Prf,[Red,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
		 ProofOut = [Prf|PrfEnd]),
	conjoin_pttp(V1,V2,Reduce),
	Result = (Head :- GoalAtom = PosGoal,
		  	  (identical_member_special_loop_check(GoalAtom,C1Ancestors) ->
			   	fail;
			  %true ->
			   	(Reduce;
				 IntHead))).

query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut) :-
	int_query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut,query).

:- abolish(add_features,2).
add_features(A,B):-dalit_add_features(A,B).

dalit_add_features((Head :- Body),(Head1 :- Body1)) :-
	(functor(Head,query,_) ->
		Head2 = Head,
		dalit_add_args(INFO,Body,yes,query,[],
		         PosAncestors,NegAncestors,
			 PosAncestors,NegAncestors,
		         DepthIn,
			 ProofIn,ProofOut,
			 Body1,_);
	%true ->
		linearize(Head,Head2,[],_,true,Matches),
		(is_negative_literal(Head) ->
			PosGoal = no;
		%true ->
			PosGoal = yes),
		Head =.. [_|HeadArgs],
		dalit_add_args(INFO,Body,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,
			 ProofIn,ProofOut,
			 Body2,New),
		(is_ftVar(New) ->
			PushAnc = true;
		PosGoal = yes ->
			NewNegAncestors = NegAncestors,
			PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
		%true ->
			NewPosAncestors = PosAncestors,
			PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),
		dalit_search_cost(Body,HeadArgs,Cost),
		test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp),
		conjoin_pttp(PushAnc,Body2,Body4),
		conjoin_pttp(Matches,Body4,Body5),
		conjoin_pttp(TestExp,Body5,Body1)),
    	Head2 =.. [P|L],
	internal_functor(P,IntP),
	list_append(L,[PosAncestors,NegAncestors,
		       DepthIn,
		       ProofIn,ProofOut,
		       GoalAtom],
		    L1),
	Head1 =.. [IntP|L1].

:- abolish(add_args,13).

add_args(INFO,Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,
	 ProofIn,ProofOut,
	 Body1,New):- 
  dalit_add_args(INFO,Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,
	 ProofIn,ProofOut,
	 Body1,New).

dalit_add_args(INFO,Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,
	 ProofIn,ProofOut,
	 Body1,New) :-
	Body = (A , B) ->
		dalit_add_args(INFO,A,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthIn,
			 ProofIn,Proof1,
		         A1,New),
		dalit_add_args(INFO,B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
			 DepthIn,
			 Proof1,ProofOut,
                         B1,New),
		conjoin_pttp(A1,B1,Body1);
	Body = (A ; B) ->
		throw(unimplemented);
	functor(Body,dalit_search_cost,_) ->
		ProofOut = ProofIn,
		Body1 = true;
	Body = infer_by(N) ->
		(PosGoal = yes -> 
			N1 = N;
		%true ->  % atom in proof is negation of actual literal
			isNegOf(N1,N)),
		Body1 = (ProofIn = [Prf,[N1,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
			 ProofOut = [Prf|PrfEnd]);
	Body = dalit_search_cost(N) ->
		ProofOut = ProofIn,
		Body1 = true;
	pttp_builtin(Body) ->
		ProofOut = ProofIn,
		Body1 = Body;
	%true ->
		Body =.. L,
		list_append(L,
                       [NewPosAncestors,NewNegAncestors,
			DepthIn,
			ProofIn,ProofOut],
		       L1),
		Body1 =.. L1,
		New = yes.
%%% ***

:- fixup_exports.
