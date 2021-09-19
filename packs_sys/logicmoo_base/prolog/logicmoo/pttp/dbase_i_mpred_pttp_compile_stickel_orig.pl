:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_pttp_compile,[]).
:- endif.

:- nop('$set_source_module'( baseKB)).

:- abolish(pttp_prove,6).
:- abolish(search_cost,3).
:- abolish(search,6).
:- abolish(make_wrapper,3).
:- abolish(add_features,2).
:- abolish(add_args,13).

pttp_prove(Goal,Max,Min,Inc,ProofIn,ProofOut,ShowProof) :-
	expand_input_proof(ProofIn,PrfEnd),
	PrevInc is Min + 1,
	add_args(Goal,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),!,
        search(Goal1,Max,Min,Inc,PrevInc,DepthIn,DepthOut),
        (ShowProof == no ; ( contract_output_proof(ProofOut1,ProofOut),write_proof(ProofOut1),nl)).

%%% ***
%%% ****f* PTTP/pttp_prove
%%% DESCRIPTION
%%%   pttp_prove(Goal) can be used to pttp_prove Goal using code compiled by PTTP.
%%%   It uses depth-first iterative-deepening search and prints the proof.
%%%
%%%   Depth-first iterative-deepening search can be controlled
%%%   by extra paramaters of the pttp_prove predicate:
%%%      pttp_prove(Goal,Max,Min,Inc,ProofIn)
%%%   Max is the maximum depth to search (defaults to a big number),
%%%   Min is the minimum depth to search (defaults to 0),
%%%   Inc is the amount to increment the bound each time (defaults to 1).
%%%   ProofIn specifies initial steps of proof to retrace (defaults to []).
%%%
%%%   A query can be compiled along with the axioms by including the
%%%   clause 'query :- ...'.  The query can then be proved by 'pttp_prove(query)'.
%%% SOURCE

pttp_prove(Goal,Max,Min,Inc,ProofIn,ProofOut) :- prove_inc(Goal,Max,Min,Inc,ProofIn,ProofOut).

prove_inc(Goal,Max,Min,Inc,ProofIn,ProofOut) :-
	expand_input_proof(ProofIn,PrfEnd),
	PrevInc is Min + 1,
	add_args(_INFO,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),
	!,
	timed_call(search(Goal1,Max,Min,Inc,PrevInc,DepthIn,DepthOut),'Proof'),
	contract_output_proof(ProofOut1,ProofOut),
	must(write_proof(ProofOut1)),
	nl.

pttp_prove(Goal,Max,Min,Inc,ProofIn) :-
	pttp_prove(Goal,Max,Min,Inc,ProofIn,_).

pttp_prove(Goal,Max,Min,Inc) :-
	pttp_prove(Goal,Max,Min,Inc,[],_).

pttp_prove(Goal,Max,Min) :-
	pttp_prove(Goal,Max,Min,1,[],_).

pttp_prove(Goal,Max) :-
	pttp_prove(Goal,Max,2,3).

%pttp_prove(Goal) :- pttp_prove(Goal,15).
pttp_prove(Goal) :-  pttp_prove(Goal,1000000,0,1,[],_).


%%% ****if* PTTP/test_and_decrement_search_cost_expr
%%% SOURCE

test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,Expr) :-
	Cost == 0 ->
		Depth1 = DepthIn,
		Expr = true;
	%true ->
		Expr = test_and_decrement_search_cost(DepthIn,Cost,Depth1).

test_and_decrement_search_cost(DepthIn,Cost,Depth1):- DepthIn >= Cost , Depth1 is DepthIn - Cost.

%%% ****if* PTTP/add_features
%%% SOURCE

add_features(In,Out):- is_ftVar(In),!,dtrace,Out=In.
add_features(true,true):- !.
add_features((Head :- Body),NewHeadBody):- 
   must_det_l((
    add_features0((Head :- Body),OUT),
    OUT = (Head1 :- Body1),
    new_head_body(Head, Body,Head1 ,Body1,NewHeadBody))).

%new_head_body(Head, Body,Head1 ,Body1,(Head1 :- Body1)):-!.
% new_head_body(Head, Body,Head1 ,Body1,(Head1 :- head_body_was(Head, Body), Body1)):- arg_checks()
new_head_body(Head, infer_by(_ProofID),Head1 ,Body1,(Head1 :- Body1)):- ground(Head),!.
new_head_body(Head, _Body,Head1 ,Body1,(Head1 :- Body1)):- is_query_lit(Head),!.
new_head_body(_Head, _Body,Head1 ,Body1,(Head1 :- Body1)):- 
   true. 
   %   dmsg(pp((head_features(Head1) :-head_body_was(Head, Body), Body1))).

:- meta_predicate(call_proof(0,*)).
call_proof(Call,_):-catch(call(Call),E,(wdmsg(error(E:Call)),fail)).

add_features0((Head :- Body),OUT):-pttp_builtin(Head),!, add_features_hb(Head , Body , _Head1 , Body1),!,OUT=(Head :- Body1).
add_features0(B,A):- add_features1(B,A).

add_features1((Head :- Body),(Head1 :- Body1)) :- (ground(Head);is_query_lit(Head)),!, add_features_hb_normal(Head , Body , Head1 , Body1).

add_features1((Head :- Body),(Head1 :- Body1)) :- add_features_hb(Head , Body , Head1 , Body1).

add_features_hb(Head , Body ,Head1 , Body1) :-     
       linearize(Head,Head2,[],_,true,Matches),
       (is_negative_literal(Head) ->
               PosGoal = no;
       %true ->
               PosGoal = yes),
       Head =.. [HF|HeadArgs],
      
       add_args(Head,Body,
          PosGoal,GoalAtom,HeadArgs,
          PosAncestors,NegAncestors,
          NewPosAncestors,NewNegAncestors,
          Depth1,DepthOut,
          ProofIn,ProofOut,
                Body2,New),

       (is_ftVar(New) ->
               PushAnc = true;

       PosGoal = yes ->
               NewNegAncestors = NegAncestors,
               PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
       %true -> ( PosGoal == false)
               NewPosAncestors = PosAncestors,
               PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),

   search_cost(Body,HeadArgs,Cost),       
   test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp0),

   argument_type_checking(HF,HeadArgs,TypeCheck),
   conjoin_pttp(TestExp0,TypeCheck,TestExp),
       conjoin_pttp(PushAnc,Body2,Body4),
       conjoin_pttp(Matches,Body4,Body5),
       conjoin_pttp(pretest_call(TestExp),Body5,Body1),
     
     add_head_args(Head2,
          PosGoal,GoalAtom,HeadArgs,
          PosAncestors,NegAncestors,
          NewPosAncestors,NewNegAncestors,
          DepthIn,DepthOut,
          ProofIn,ProofOut,Head1,New).

add_features_hb_normal(Head , Body ,Head1 , Body1) :-
       ( is_query_lit(Head) ->
		Head2 = Head,
		add_args(Head2,Body,yes,query,[],
		         PosAncestors,NegAncestors,
			 PosAncestors,NegAncestors,
		         DepthIn,DepthOut,
			 ProofIn,ProofOut,
			 Body1,_);
	%true ->
		linearize(Head,Head2,[],_,true,Matches),
		(is_negative_literal(Head) ->
			PosGoal = no;
		%true ->
			PosGoal = yes),
		Head =.. [HF|HeadArgs],
            
		add_args(Head,Body,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,DepthOut,
			 ProofIn,ProofOut,
			 Body2,New),    

		(is_ftVar(New) ->
			PushAnc = true;

		PosGoal = yes ->
			NewNegAncestors = NegAncestors,
			PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
		%true -> ( PosGoal == false)
			NewPosAncestors = PosAncestors,
			PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),


              search_cost(Body,HeadArgs,Cost),       
              test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp0),
              argument_type_checking(HF,HeadArgs,TypeCheck),
              conjoin_pttp(TestExp0,TypeCheck,TestExp),
		conjoin_pttp(PushAnc,Body2,Body4),
		conjoin_pttp(Matches,Body4,Body5),
		conjoin_pttp(pretest_call(TestExp),Body5,Body1)                
                ),
     
    	Head2 =.. [P|L],
	internal_functor(P,IntP),
	list_append(L,
                    [PosAncestors,NegAncestors,
		       DepthIn,DepthOut,
		       ProofIn,ProofOut,
		       GoalAtom],
		    L1),
	Head1 =.. [IntP|L1].



add_head_args(HeadIn,
          _PosGoal,GoalAtom,_HeadArgs,
          PosAncestors,NegAncestors,
          _NewPosAncestors,_NewNegAncestors,
          DepthIn,DepthOut,
          ProofIn,ProofOut,
          Head1,_New):-
     correct_pttp_head(true_t,HeadIn,Head),
        Head =.. [P|L],
	internal_functor(P,IntP),
	list_append(L, [PosAncestors,NegAncestors, DepthIn,DepthOut, ProofIn,ProofOut, GoalAtom], L1),
	Head1 =.. [IntP|L1].

%%% ***
%%% ****if* PTTP/add_args
%%% SOURCE

:- was_export(add_args/15).

add_args(INFO,(A , B),PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
		add_args(INFO,A,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthIn,Depth1,
			 ProofIn,Proof1,
		         A1,New),
		add_args(INFO,B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,DepthOut,
			 Proof1,ProofOut,
                         B1,New),
		conjoin_pttp(A1,B1,Body1),!.

add_args(INFO,(A ; B),PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
               add_args(INFO,A,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthA,DepthOut,
			 ProofIn,ProofOut,
			 A2,New),
		add_args(INFO,B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthB,DepthOut,
			 ProofIn,ProofOut,
			 B2,New),
   % dtrace,
                search_cost(A,HeadArgs,CostA),
		search_cost(B,HeadArgs,CostB),
		(CostA < CostB ->
			DepthA = DepthIn,
			Cost is CostB - CostA,
			test_and_decrement_search_cost_expr(DepthIn,Cost,DepthB,TestExp),
			A1 = A2,
			conjoin_pttp(pretest_call(TestExp),B2,B1);
		CostA > CostB ->
			DepthB = DepthIn,
			Cost is CostA - CostB,
			test_and_decrement_search_cost_expr(DepthIn,Cost,DepthA,TestExp),
			B1 = B2,
			conjoin_pttp(pretest_call(TestExp),A2,A1);
		%true ->
		        DepthA = DepthIn,
			DepthB = DepthIn,
			A1 = A2,
			B1 = B2),
		disjoin(A1,B1,Body1).

add_args(_INFO,Search_cost,_PosGoal,_GoalAtom,_HeadArgs,_PosAncestors,_NegAncestors,_NewPosAncestors,_NewNegAncestors,Depth,Depth,Proof,Proof,true,_New):- 
  functor(Search_cost,search_cost,_),!.

add_args(INFO,infer_by(_N),PosGoal,GoalAtom,_HeadArgs,
         PosAncestors,NegAncestors,
	 _NewPosAncestors,_NewNegAncestors,
	 Depth,Depth,
	 ProofIn,ProofOut,
	 Body1,_New) :-
    (PosGoal = yes -> 
			N1 = INFO;
		%true ->  % atom in proof is negation of actual literal
			isNegOf(N1,INFO)),
    Body1 = (ProofIn = [Prf,[N1,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
			 ProofOut = [Prf|PrfEnd]).

add_args(_INFO,Body,_PosGoal,_GoalAtom,_HeadArgs,_PosAncestors,_NegAncestors,_NewPosAncestors,_NewNegAncestors,Depth,Depth,Proof,Proof,Body,_New):-
   pttp_builtin(Body),!.

% normal lit
add_args(_INFO,BodyIn,
         _PosGoal,_GoalAtom,_HeadArgs,
         _PosAncestors,_NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
   correct_pttp_head(asserted_t,BodyIn,Body), 
    Body =.. L,
    list_append(L, [NewPosAncestors,NewNegAncestors,DepthIn,DepthOut,ProofIn,ProofOut], L1),
    Body11 =.. L1,
    Body1 = call_proof(Body11,Body),
    New = yes.





%%% ***
%%% ****if* PTTP/search_cost
%%% DESCRIPTION
%%%   Search cost is ordinarily computed by counting literals in the body.
%%%   It can be given explicitly instead by including a number, as in
%%%     p :- search_cost(3).     (ordinarily, cost would be 0)
%%%     p :- search_cost(1),q,r. (ordinarily, cost would be 2)
%%%     p :- search_cost(0),s.   (ordinarily, cost would be 1)
%%% SOURCE

search_cost(Body,HeadArgs,N) :-
	Body = search_cost(M) ->
		N = M;
	Body = (A , B) ->
		(A = search_cost(M) ->	% if first conjunct is search_cost(M),
			N = M;		% search cost of conjunction is M
		%true ->
			search_cost(A,HeadArgs,N1),
			search_cost(B,HeadArgs,N2),
			N is N1 + N2);
	Body = (A ; B) ->
		search_cost(A,HeadArgs,N1),
		search_cost(B,HeadArgs,N2),
		min(N1,N2,N);
	pttp_builtin(Body) ->
		N = 0;
	%true ->
		N = 1.
%%% ***
%%% ****if* PTTP/search
%%% DESCRIPTION
%%%   Search driver predicate.
%%%   Note that depth-bounded execution of Goal is enabled by
%%%   the fact that the DepthIn and DepthOut arguments of
%%%   search are also the DepthIn and DepthOut arguments of Goal.
%%% SOURCE
:- meta_predicate search(0,?,?,?,?,?,?).
search(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut):-search0(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

:- meta_predicate search0(0,?,?,?,?,?,?).
search0(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut):- % dtrace,
        search1(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

search1(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
	Min > Max,
	!,
	fail.

:- meta_predicate search1(0,*,*,*,*,*,*).
search1(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        write_search_progress(Min),
	DepthIn = Min,
	catchv(call(Goal),E,(wdmsg(E=Goal),dtrace)),
	DepthOut < PrevInc.	% fail if solution found previously
search1(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
	Min1 is Min + Inc,
	search(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).
%%% ***

%%% ****if* PTTP/make_wrapper
%%% SOURCE

make_wrapper(_DefinedPreds,[query,0],true) :-
	!.
make_wrapper(DefinedPreds,[P,N],Result):- must_det(make_wrapper0(DefinedPreds,[P,N],Result)).

make_wrapper0(DefinedPreds,[P,N],Result) :-
	functor(Goal,P,N),
	Goal =.. [P|Args],
	ExtraArgs = [PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut],
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
		V1 = (identical_member_cheaper(GoalAtom,C2Ancestors) , !);
	%true ->
		V1 = ((identical_member_cheaper(GoalAtom,C2Ancestors) , !);
		       unifiable_member_cheaper(GoalAtom,C2Ancestors))),
	V2 = (DepthOut = DepthIn,
		 ProofIn = [Prf,[Red,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
		 ProofOut = [Prf|PrfEnd]),
	conjoin_pttp(V1,V2,Reduce),
	Result = (Head :- GoalAtom = PosGoal,
		  	  (identical_member_special_loop_check(GoalAtom,C1Ancestors) ->
			   	fail;
			  %true ->
			   	(Reduce;
				 IntHead))).
%%% ***
%%% ****if* PTTP/query
%%% DESCRIPTION
%%%   query uses the following definition instead of the more complex one
%%%   that would be created by make_wrapper
%%% SOURCE

% 3 = suc(suc(1))
% 3/67 = div(suc(suc(1)),67)


query(PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut) :- 
  get_int_query(Int_query),
	call(Int_query,PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut,query).

:- fixup_exports.
