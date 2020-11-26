/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Planner Compiler
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(planner_cm,
	  [ query_clause/8,
	    compile_rules/2,
	    describe_i_structure/1 ]).

:- use_module('swilib/fromonto').
:- use_module('swilib/term_support').
:- use_module('swilib/pretty').
:- use_module('swilib/err').
:- use_module(library(occurs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Defines
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_cs_module(planner_cs_simple).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Auxiliary Stuff
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_val([_-V|KVs], [V|KVs1]) :-
	map_val(KVs, KVs1).
map_val([], []).

list_to_andseq([X,Y|Z], (X,YZ1)) :- 
	!,
	list_to_andseq([Y|Z], YZ1).
list_to_andseq([X], X).
list_to_andseq([], true).

random_integer(N) :-
	prolog_flag(max_integer, I),
	N is random(I).

plan_option(O, Os) :- memberchk(O, Os).    

gen_lit_id(Id) :-
	gensym(l, Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Data Structures
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule_action(      rule(X, _, _, _), X).
rule_post(        rule(_, X, _, _), X).
rule_pre(         rule(_, _, X, _), X).
rule_constraints( rule(_, _, _, X), X).

is_rule(          rule(_, _, Pre, _)) :-
	Pre \= [].

fact_post(        rule(_, Xs, _, _), X) :-
	( Xs = [_, _|_] ->
	  err( 'Fact with multiple post parts: ~q.', [Xs] )
          % maybe these could be used nondeterministically ?
        ; [X] = Xs
        ).
fact_post(        fact(X, _), X).
fact_constraints( fact(_, X), X).
fact_constraints( rule(_, _, _, X), X).
fact_action( fact(_, _), '$empty').
fact_action( rule(X, _, _, _), X).

is_fact(   fact(_, _)).
is_fact(   rule(_, _, [], _)).

declaration_object( declare(_, X), X).

is_declaration(fluent, declare(fluent, _)).

make_args(X) :- functor(X, args, 13).

args_depth_in(X, Y)      :- arg(1, X, Y).
args_infs_in(X, Y)       :- arg(2, X, Y).
args_infs_out(X, Y)      :- arg(3, X, Y).
args_pool_in(X, Y)       :- arg(4, X, Y).
args_pool_out(X, Y)      :- arg(5, X, Y).
args_ancestors(X, Y)     :- arg(6, X, Y).
args_plan(X, Y)          :- arg(7, X, Y).
args_fluent_heads(X, Y)  :- arg(8, X, Y).
args_constraints(X, Y)   :- arg(9, X, Y).
args_depth_out(X, Y)     :- arg(10, X, Y).
args_ancestors_out(X, Y) :- arg(11, X, Y).
args_cs_in(X, Y)         :- arg(12, X, Y).
args_cs_out(X, Y)        :- arg(13, X, Y).

scp_head(   scp(X, _, _, _, _), X).
scp_body(   scp(_, X, _, _, _), X).
scp_before( scp(_, _, X, _, _), X).
scp_after(  scp(_, _, _, X, _), X).
scp_info(   scp(_, _, _, _, X), X).

make_scp(scp(_, _, _, _, _)).

default_scp(SCP) :-
	scp_body(SCP, Body),
	scp_before(SCP, Before),
	scp_after(SCP, After),
	scp_info(SCP, Info),
	( var(Body) -> Body = [] ; true ),
	( var(Before) -> Before = [] ; true ),
	( var(After) -> After = [] ; true ),
	( var(Info) -> Info = extra(defaulted) ; true ).

scp_to_clause(SCP, C) :-
	scp_head(SCP, H),
	scp_before(SCP, Before),
	scp_body(SCP, Body),
	scp_after(SCP, After),
	append(Body, After, B1),
	append(Before, B1, B2),
	list_to_andseq(B2, B),
	( B = true ->
	  C = H
        ; C = (H :- B)
        ).

map_scp_to_clause([X|Xs], [X1|Xs1]) :-
    scp_to_clause(X, X1),
    map_scp_to_clause(Xs, Xs1).
map_scp_to_clause([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Construction and Compile-Time Access of I-Structures
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% describe_i_structure(+Options)
%%%% 
%%%% Print the layout of the extra information attached to compiled
%%%% predicates in symbolic form. For debugging purposes.
%%%% 
describe_i_structure(Os) :-
	access_i(Os, I, [depth(d1,d2), infs(j1,j2), pool(s1, s2),
	                 plan(p), ancestors(a), goals(g), cs(c1, c2),
			 depends(z)]),
	writeq(I),
	nl.

getarg(Keyterm, Arglist) :-
	memberchk(Keyterm, Arglist),
	!.
getarg(_, _).

access_i(Os, I, Arglist) :-
	getarg(depth(D1, D2), Arglist),
	getarg(infs(J1,J2), Arglist),
	getarg(pool(S1,S2), Arglist),
	getarg(plan(P), Arglist),
	getarg(cs(C1, C2), Arglist),
	I0 = [S1, S2],
	( plan_option(bd, Os) ->
	  ( plan_option(bdxo, Os) ->
	    I1 = [D1,D2|I0]
	  ; I1 = [D1|I0]
          )
        ; I1 = I0
        ),
        ( plan_option(bj, Os) ->
	  I2 = [J1,J2|I1]
        ; I2 = I1
        ),
        ( plan_option(p(_), Os) ->
	  I3 = [P|I2]
        ; I3 = I2
        ),
	( plan_option(cs(_), Os) ->
	  I4 = [C1,C2|I3]
	; I4 = I3
	),
	IN = I4,
	I =.. [i|IN].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Compiler Core
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% compile_rules(+Options, +Rules)
%%%% 
%%%% Compiles a list of planning rules according to the specified options into
%%%% Prolog code.
%%%%
%%%% See special sections on syntax of Rules and recognized Options.
%%%% 
compile_rules(Os, FRs) :-
	( plan_option(out(Out), Os) ->
	  true
        ; Out = '/tmp/out_planner.pl'
	),
	compile_frs(FRs, Os, SCPs),
	( plan_option(cs_module(CSModule), Os) ->
          true
        ; default_cs_module(CSModule)
        ),
	( var(Out) ->
	  map_scp_to_clause(SCPs, Out1),
	  Out = Out1
	  % Out = [(:- use_module(CSModule))|Out1]
        ; msg('Planner compilation: Writing output to file: ~w.', [Out]),
	  ( plan_option(module(Module), Os) -> true ; Module = out_planner ),
	  onto_file(
	   ( pp_clause((:- module(Module, []))),
	     nl,
	     % pp_clause((:- use_module(CSModule))),
	     % nl,
	     member(SCP, SCPs),
	     % write_info_comment(SCP),  
	     scp_to_clause(SCP, C),
	     pp_clause(C),
	     nl,
	     fail
	   ; true
           ),
	   Out)
        ).

write_info_comment(SCP) :-
	scp_info(SCP, Info),
	\+ \+ ( numbervars(Info, 0, _),
		format(user_error, '%% ~q~n~n', [Info])),
	flush_output(user_error).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_frs(FRs, Os, SCPs) :-
        msg('Planner compilation: init'),
	frs_fluent_heads(FRs, FluentHeads),
	heads_predicates(FluentHeads, FluentPreds),
	msg('Planner compilation: rules'),
	findall( RuleSCP,
	         ( member(Rule, FRs),
		   is_rule(Rule),
		   rule_scp(Rule, Os, FluentHeads, RuleSCP)
	         ),
		 RuleSCPs
	       ),
	msg('Planner compilation: facts'),
	findall( FactSCP,
	         ( member(Fact, FRs),
		   is_fact(Fact),
		   fact_scp(Fact, Os, FluentHeads, FactSCP)
	         ),
		 FactSCPs
	       ),
	msg('Planner compilation: pool'),
        findall( PoolsolveSCP,
	         ( member(Pred/Arity, FluentPreds),
		   poolsolve_scp(Pred, Arity, Os, PoolsolveSCP)
	         ),
		 PoolsolveSCPs
	       ),
	AlemSCPs = [],
	make_extra_scps(Os, FactSCPs, FluentHeads, ExtraSCPs),
	append(FactSCPs, RuleSCPs, SCPs1),
	append(PoolsolveSCPs, SCPs1, SCPs3),
	append(AlemSCPs, SCPs3, SCPs4),
	append(ExtraSCPs, SCPs4, SCPs5),

	SCPsN = SCPs5,
	msg('Planner compilation: sorting'),
	sort_scps(SCPsN, Os, FluentHeads, SCPs),
	msg('Planner compilation: done').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

frs_fluent_heads(FRs, Heads) :-
	findall(Head,
	        ( member(Rule, FRs),
		  is_rule(Rule),
		  rule_post(Rule, Post),
		  member(Head, Post)
	        ; member(Rule, FRs),
		  is_declaration(fluent, Rule),
		  declaration_object(Rule, Head)
	        ),
		Heads1
	       ),
	remove_subsumed_elements(Heads1, Heads).

heads_predicates(Heads, Preds) :-
	findall(P/F, ( member(H, Heads), functor(H, P, F) ), Preds1),
	sort(Preds1, Preds).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


scp_literal(Term, Os, Term1, I) :-
        proper_scp_literal(Term, Os, Term1, I).

proper_scp_literal(Term, _Os, Term1, I) :-
	Term =.. [Pred|Args],
	name(Pred, Name),
	append([0'c, 0'_], Name, Name1),
	name(Pred1, Name1),
	append(Args, [I], Args1),
	Term1 =.. [Pred1|Args1].


rule_scp(Rule, Os, FluentHeads, SCP) :-
	rule_pre(Rule, Pre),
	rule_post(Rule, Post),
	rule_action(Rule, Action),
	rule_constraints(Rule, Constraints1),
	rule_auto_constraints(Rule, Constraints1, Constraints),

	select(Head1, Post, ToPool),

	make_scp(SCP),
	scp_head(SCP, Head),
	scp_body(SCP, Body),
	scp_before(SCP, Before),
	scp_after(SCP, After),

	get_type_constraints(Constraints, Os, TypeCs),
	term_variables(Head1, Vs),
	get_neq_constraints(Constraints, Os, Vs, NeqCs),
	get_safe_constraints(Constraints,
			     Os, [], BVs, false, Constraints3, SafeCs),
	merge_cs_call(Os, Constraints, K1, K2, RedCs),

	append(SafeCs, RedCs, CsL1),
	append(NeqCs, CsL1, CsL2),
	append(TypeCs, CsL2, CsL),

        ( plan_option(cxc, Os),
	  member(Subgoal, Pre),
	  Head1 == Subgoal,
	  has_fluent_pred(Head1, FluentHeads) % may non-fluents occur here?
          ->
	  fail
        ; true
	),

	( has_fluent_pred(Head1, FluentHeads) ->
          scp_literal(Head1, Os, Head2, I)
	; proper_scp_literal(Head1, Os, Head2, I) % can/may this happen?
        ),

	access_i(Os, I, [depth(D1,D2), infs(J1,JN), pool(S1,S2), plan(P1),
			 ancestors(A1), depends(Z), cs(K1,KN)]),

	scp_info(SCP, rule_contrapositive([pre-Pre, post-Post,
	                                   head-Head1, action-Action])),
	append(CsL, Body1, Body),

	( plan_option(pxo, Os),
	  ToPool \= [] ->
	  P2 = action(Action, Ps, Label)
	; P2 = action(Action, Ps)
	),

	P1 = P2,

	sort_subgoals(Pre, Os, Head1, Post, FluentHeads, Pre1),

	make_args(BeforeArgs),
	args_fluent_heads(BeforeArgs,  FluentHeads),
	args_constraints(BeforeArgs,   Constraints3),
	args_depth_in(BeforeArgs,      D1),
	args_depth_out(BeforeArgs,     DE),
	args_infs_in(BeforeArgs,       J1),
	args_infs_out(BeforeArgs,      JE),
	args_ancestors(BeforeArgs,     A1),
	args_ancestors_out(BeforeArgs, A2),
	args_pool_in(BeforeArgs,       S1),
	
	compute_scp_before(Os, Head1, BeforeArgs, Pre1, Before2),
	
	add_oc_equations(Head2, Before2, Head, Before),
	
	make_args(BodyArgs),
	args_fluent_heads(BodyArgs, FluentHeads),
	args_constraints(BodyArgs,  Constraints3),
	args_depth_in(BodyArgs,     DE),
	args_infs_in(BodyArgs,      JE),
	args_infs_out(BodyArgs,     JN),
	args_ancestors(BodyArgs,    A2),
	args_pool_in(BodyArgs,      S1),
	args_pool_out(BodyArgs,     SN),
	args_cs_in(BodyArgs,        K2),
	args_cs_out(BodyArgs,       KN),
	
	compute_scp_body(Os, Pre1, BodyArgs, Vs, BVs, Ps, Ds, Zs, Body1),
	
	Z =.. [z|Zs],

	compute_scp_after(Os, ToPool, Label,
			  Z, Ds, D1, DE, D2, SN, S2, After).



compute_scp_body(Os, [L|Ls], Args, Vs, BVs, [P|Ps], [D|Ds], [Z|Zs], Ls1) :-
	args_fluent_heads(Args, FluentHeads),
	args_constraints(Args,  Cs),
	args_depth_in(Args,     D1),
	args_infs_in(Args,      J1),
	args_infs_out(Args,     JN),
	args_ancestors(Args,    A),
	args_pool_in(Args,      S1),
	args_pool_out(Args,     SN),
	args_cs_in(Args,        K1),
	args_cs_out(Args,       KN),
	proper_scp_literal(L, Os, L1, I),
	access_i(Os, I, [depth(D1, D), infs(J1, J2), pool(S1, S2),
			 plan(P), ancestors(A), depends(Z), cs(K1, K2)]),

	term_variables(L-Vs, Vs1),
	term_variables(L-BVs, BVs1),
	get_neq_constraints(Cs, Os, Vs1, NeqCs),
	( Ls = [] -> Finalp = true ; Finalp = false ),
	get_safe_constraints(Cs, Os, BVs1, BVs2, Finalp, Cs1, SafeCs),
	reduce_cs_within_body_call(Os, K2, K3, RedCs),

	append(RedCs, Ls2, Ls3),
	append(SafeCs, Ls3, Ls4),
	append([L1|NeqCs], Ls4, Ls1),

	make_args(Args1),
	args_fluent_heads(Args1, FluentHeads),
	args_constraints(Args1,  Cs1),
	args_depth_in(Args1,     D1),
	args_infs_in(Args1,      J2),
	args_infs_out(Args1,     JN),
	args_ancestors(Args1,    A),
	args_pool_in(Args1,      S2),
	args_pool_out(Args1,     SN),
	args_cs_in(Args1,        K3),
	args_cs_out(Args1,       KN),
	compute_scp_body(Os, Ls, Args1, Vs1, BVs2, Ps, Ds, Zs, Ls2).
compute_scp_body(_, [], Args, _, _, [], [], [], []) :-
	args_infs_in(Args,      JN),
	args_infs_out(Args,     JN),
	args_pool_in(Args,      SN),
	args_pool_out(Args,     SN),
	args_cs_in(Args,        KN),
	args_cs_out(Args,       KN).


compute_scp_before(Os, _Head, Args, Pre, Before) :-
	args_fluent_heads(Args,  FluentHeads),
	args_depth_in(Args,      D1),
	args_depth_out(Args,     D2),
	args_infs_in(Args,       J1),
	args_infs_out(Args,      J2),
	args_ancestors(Args,     A1),
	args_ancestors_out(Args, A2),
        Bef1 = [],
	A2 = A1,
	( plan_option(bdxs, Os) ->
	  pre_fluents(Pre, FluentHeads, Pre1),
	  length(Pre1, Size),
	  DVal = Size
        ; DVal = 1
        ),

	( plan_option(bjxs, Os) ->
	  pre_fluents(Pre, FluentHeads, Pre1),
	  length(Pre1, Size),
	  JVal = Size
        ; JVal = 1
        ),

	( plan_option(bd, Os) ->
	  DVal1 = DVal,
	  decr_clause(DVal1, D1, D2, DecD)
        ; DecD = []
        ),

	( plan_option(bj, Os) ->
	  decr_clause(JVal, J1, J2, DecJ)
        ; DecJ = []
        ),

	append(DecJ, Bef1, Bef2),
	append(DecD, Bef2, Bef3),

	Before = Bef3.

decr_clause(Val, V1, V2, [V1 >= Val, V2 is V1 - Val]).

compute_scp_after(Os, ToPool, L, Z, Ds, D1, DE, D2, S1, S2, After) :-
	add_to_pool_clause(Os, ToPool, L, Z, D2, S1, S2, After1),
	( plan_option(bdxo, Os) ->
	  After = [maximum(Ds, D3), D2 is D3 + (D1 - DE) | After1]
        ; After = After1
        ).

pre_fluents(Pre, FluentHeads, FluentPre) :-
	findall( L,
	         ( member(L, Pre), is_fluent(L, FluentHeads) ),
		 FluentPre
	       ).

is_fluent(L, FluentHeads) :-
	member(L1, FluentHeads),
	\+ \+ unify_with_occurs_check(L, L1),
	!.

has_fluent_pred(L, FluentHeads) :-
	functor(L, F, A),
	member(L1, FluentHeads),
	functor(L1, F, A),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fact_scp( Fact, Os, FluentHeads, SCP ) :-

	fact_post(Fact, Head0),
	( has_fluent_pred(Head0, FluentHeads) ->
	  scp_literal(Head0, Os, Head, I)
        ; proper_scp_literal(Head0, Os, Head, I)
        ),
	access_i(Os, I, [pool(S,S),
	                 depth(D,D),
	                 infs(J,J),
			 plan(P),
		         depends(z),
			 cs(K1,K2)]),
	make_scp(SCP),

	fact_action(Fact, Action),
	( Action == '$empty' -> P1 = empty ; P1 = fact(Action) ),

	fact_constraints(Fact, Constraints),

	P = P1,
	
	get_type_constraints(Constraints, Os, TypeCs),
	term_variables(Head0, Vs),
	get_neq_constraints(Constraints, Os, Vs, NeqCs),
	get_safe_constraints(Constraints, Os, [], _, true, _, SafeCs),
	merge_cs_call(Os, Constraints, K1, K2, RedCs),

	append(SafeCs, RedCs, CsL1),
	append(NeqCs, CsL1, CsL2),
	append(TypeCs, CsL2, CsL),
	
	add_oc_equations(Head, CsL, Head1, Body),

	scp_body(SCP, Body),

	scp_head(SCP, Head1),

	( Action == '$empty' ->
	  scp_info(SCP, fact(Head0))
	; scp_info(SCP, fact_with_action(Head0))
	),

	default_scp(SCP).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Query Clause
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Compile a Prolog query clause from the given parameters.
%%%% 
query_clause(Goal, Start, Constraints, Os, FluentHeads,
	     Depth, Infs, Clause) :-
	( plan_option(p(Plan), Os) -> Plan = goal(Ps) ; true ),
	( plan_option(pool(ResultPool), Os) -> true ; true ),
	( plan_option(infs(ResultInfs), Os) -> true ; true ),
	( plan_option(cs(ResultK), Os) ->
	  ( plan_option(cs_module(CSModule), Os) ->
	    true
          ; default_cs_module(CSModule)
          ),
	  ( memberchk(cs(KInput), Constraints) ->
	    compile_cs(CSModule, KInput, K1)
	  ; CSModule:make_true_cs(K1) 
	  )
	; true
	),

        make_pool(Os, Start, S1), % ... maybe scp_after stuff here too

	make_ancestors(A),

	make_args(QueryArgs),
	args_fluent_heads(QueryArgs, FluentHeads),
	args_constraints(QueryArgs,  Constraints),
	args_depth_in(QueryArgs,     Depth),
	args_infs_in(QueryArgs,      Infs),
	args_infs_out(QueryArgs,     ResultInfs),
	args_ancestors(QueryArgs,    A),
	args_pool_in(QueryArgs,      S1),
	args_pool_out(QueryArgs,     ResultPool),
	args_cs_in(QueryArgs,        K1),
	args_cs_out(QueryArgs,       ResultK),
	
	compute_scp_body(Os, Goal, QueryArgs, [], [], Ps, _Ds, _Zs, QueryL),
	list_to_andseq(QueryL, Clause).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Extra stuff
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_extra_scps( Os, _FactSCPs, FluentHeads, SCPs) :-
	make_scp(OsSCP),
	scp_head(OsSCP, dec_plan_options(Os)),
	default_scp(OsSCP),
	make_scp(FhSCP),
	scp_head(FhSCP, dec_fluent_heads(FluentHeads)),
	default_scp(FhSCP),
	LSCPs = [],
	poolfact_access_scps(Os, PfSCPs),
	append(PfSCPs, LSCPs, SCPs1),
	SCPs = [OsSCP,FhSCP|SCPs1].
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Subgoal Sorting
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_subgoals(Pre, Os, Head, Post, FluentHeads, Pre1) :-
	Info = 	[os-Os, head-Head, post-Post, fluent_heads-FluentHeads],
	map_subgoal_val(Pre, Info, Pre2),
	keysort(Pre2, Pre3),
	map_val(Pre3, Pre1).

subgoal_val(L, Info, V-L) :-
	memberchk(os-Os, Info),
	( plan_option(sn, Os) ->
	  V = 0                         %% assume keysort is stable  
	; plan_option(sr, Os) ->
	  random_integer(V)
        ; ( true
	  ; plan_option(s1, Os)
          ) ->
	  memberchk(head-Head, Info),
	  term_variables(Head, HeadVars),
	  term_variables(L, Vars),
	  abs_difference(Vars, HeadVars, NonHeadVars),
	  length(NonHeadVars, LNonHeadVars),
	  length(Vars, LFreeVars),
	  term_size(L, LSize),
	  VarsVal is (LNonHeadVars + 1)/(LFreeVars + 1),
	  SizeVal is LFreeVars/LSize,
	  FreeVarsVal = LFreeVars,
	  memberchk(fluent_heads-FluentHeads, Info),
	  ( has_fluent_pred(L, FluentHeads) ->
	    FluentVal = 1
	  ; FluentVal = 0
          ),
	  ( Head == L ->
	    HeadLoopVal = 1
	  ; HeadLoopVal = 0
          ),
	  copy_term(Head, Head1),
	  ( subsumeschk(L, Head1) ->
	    HeadLoopSubsVal = 1
	  ; HeadLoopSubsVal = 0
          ),
	  memberchk(post-Post, Info),
	  ( member(L1, Post), L == L1 ->
	    LoopVal = 1
	  ; LoopVal = 0
          ),
	  LoopVals=s(HeadLoopVal, HeadLoopSubsVal, LoopVal),  
	    
          V = s(LoopVals, VarsVal, SizeVal, FluentVal, FreeVarsVal)
        ).

term_size(T, 1) :-
	var(T),
	!.
term_size(T, 1) :-
	atomic(T),
	!.
term_size(T, N) :-
	functor(T, _, A),
	map_term_size(A, T, N).
	
map_term_size(0, _, 1) :-
	!.
map_term_size(A, T, N) :-
	A > 0,
	A1 is A - 1,
	map_term_size(A1, T, N1),
	arg(A, T, T1),
	term_size(T1, N2),
	N is N1 + N2.

abs_difference([X|Xs], Ys, Zs) :-
	( absmember(X, Ys) ->
	  Zs = Zs1
        ; Zs = [X|Zs1]
        ),
	abs_difference(Xs, Ys, Zs1).
abs_difference([], _, []).

abs_union([X|Xs], Ys, Zs) :-
	( absmember(X, Ys) ->
	  Zs = Zs1
	; Zs = [X|Zs1]
	),
	abs_union(Xs, Ys, Zs1).
abs_union([], Xs, Xs).

map_subgoal_val([X|Xs], Y1, [X1|Xs1]) :-
    subgoal_val(X, Y1, X1),
    map_subgoal_val(Xs, Y1, Xs1).
map_subgoal_val([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Safe Constraints
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_safe_constraints(Cs, Os, Vs, Vs1, Finalp, Cs2, Cs1) :-
	gec(Cs, Os, Finalp, Vs, Vs1, Cs2, Cs1),
	( Finalp = true,  member(C, Cs2),
	  ( C = safe(_) ; C = safe(_, _) ) ->
	  err('Unsafe safe constraint: ~q.', [C])
	; true
	).

%% 
%% Disabled for Security Reasons
%% 
%% gec(Cs, Os, Finalp, Vs, Vs1, Cs1, [E|Cs2]) :-
%% 	( 
%% 	select( safe(E), Cs, Cs3 ), T=k
%% 	; select( safe(T, E), Cs, Cs3 )
%% 	),
%% 	term_variables(E, VsE),
%% 	term_variables(T, VsT),
%% 	\+ ( member(V, VsE),
%% 	     \+ absmember(V, VsT),
%% 	     \+ absmember(V, Vs) ),
%% 	!,
%% 	abs_union(VsT, Vs, Vs4),
%% 	gec(Cs3, Os, Finalp, Vs4, Vs1, Cs1, Cs2).
%% gec(Cs, _, _, Vs, Vs, Cs, []).
%%

gec(Cs, _, _, Vs, Vs, Cs, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Clause Local Neq Constraints
%%%% 
%%%% Just local to clauses, as in the `92 Setheo implementation.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_neq_constraints(Cs, _Os, Vs, Cs1) :-
	gns(Cs, Vs, Cs1).

gns([C|Cs], Vs, Cs1) :-
	( C = neq(A,B),
	  term_variables(A-B, Vs1),
	  \+ ( member(V1, Vs1),
	       \+ absmember(V1, Vs)
	     ) ->
	  Cs1 = [A \== B | Cs2],
	  gns(Cs, Vs, Cs2)
        ;
	 gns(Cs, Vs, Cs1)
        ).	    
gns([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Type Constraints. [*** TODO: REWORK]
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type_constraints(Cs, _Os, []) :-
	gtc_fun(Cs).

gtc_fun([C|Cs]) :-
	( C = type(Type, Var) ->
	  make_type_wrapper(Type, Var, _),
	  gtc_fun(Cs)
        ; gtc_fun(Cs)
        ).
gtc_fun([]).

% *** consistency checks: type functor not as normal functor...
% unique type for each symbol ...

make_type_wrapper(Type, Var, Arg) :-
	( atom(Type) ->
	  Var =.. [Type,_]
        ; % consp(Type) ->
	  mtw(Type, Var, Arg)
        ).

mtw([T|Ts], V, Arg) :-
	V =.. [T,V2],
	mtw(Ts, V2, Arg).
mtw([], Arg, Arg).

gtc_pred([C|Cs], Cs1) :-
	( functor(C, type, _) ->
	  make_type_literal(C, C1),
	  Cs1 = [C1 | Cs2] ,
	  gtc_pred(Cs, Cs2)
        ; gtc_pred(Cs, Cs1)
        ).
gtc_pred([], []).

make_type_literal(L, L1) :-
	L =.. [type,Type|Vs],
	name(type, NPrefix),
	name(Type, NType),
	append(NPrefix, [0'_|NType], NFunctor),
	name(Functor, NFunctor),
	L1 =.. [Functor|Vs].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Sort SCPs.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_scps(SCPs, Os, FluentHeads, SCPs1) :-
	prune_scps(SCPs, SCPs2),
	GlobalInfo = [os-Os, fluent_heads-FluentHeads],
	map_scp_sort_key(SCPs2, GlobalInfo, SCPs3),
	keysort(SCPs3, SCPs4),
	map_val(SCPs4, SCPs1).

prune_scps(SCPs, SCPs1) :-
	%% quickwritten to remove duplicate delay clauses
	findall(SCP, member(SCP, SCPs), SCPs2), % to stdize apart the elements
	remove_subsumed_elements(SCPs2, SCPs1).

map_scp_sort_key([X|Xs], Y1, [X1|Xs1]) :-
    scp_sort_key(X, Y1, X1),
    map_scp_sort_key(Xs, Y1, Xs1).
map_scp_sort_key([], _, []).

scp_sort_key(SCP, GlobalInfo, K-SCP) :-
	scp_head(SCP, Head),
	scp_info(SCP, Info),
	functor(Head, Functor, Arity),
	scp_sort_info_val(Info, GlobalInfo, InfoVal),
	%% *** other criteria like term depth etc 
	K = s(Functor, Arity, InfoVal, Head).

scp_sort_info_val( alem(_), _, 60).
scp_sort_info_val( fact(_), _, 80).
scp_sort_info_val( fact_with_action(_), _, 95).
scp_sort_info_val( pool(_), _, 90).
% scp_sort_info_val( pool(X), _, p-0).
scp_sort_info_val( rule_contrapositive(Args), GlobalInfo, m-N) :-
	memberchk(os-Os, GlobalInfo),
	( plan_option(or, Os) ->
	  random_integer(N)
        ; memberchk(pre-Pre, Args),
	  memberchk(post-Post, Args),
	  memberchk(head-Head, Args),
	  memberchk(action-Action, Args),
	  memberchk(fluent_heads-FluentHeads, GlobalInfo),
	  pre_fluents(Pre, FluentHeads, Pre1),
	  length(Pre1, NPre),
	  length(Pre, NPre2),
	  length(Post, LPost),
	  NPost is - LPost,
	  ( member(L1, Pre), L1 == Head ->
	    CycleVal = 1, CycleVal1 = 0
	  ; CycleVal = 0, CycleVal1 = 1
          ),
	  ( plan_option(o2, Os) ->
	    N = s(NPost, NPre, NPre2, Action, Head)
	  ; plan_option(o3, Os) ->
	    N = s(LPost, NPre, NPre2, Head, Action)
	  ; ( plan_option(o1, Os) ; true )->
	    % *** N = s(NPost, NPre, NPre2, Head, Action)
	    N = s(CycleVal, NPost, NPre, NPre2, Head, Action)
          )
        ).
scp_sort_info_val( logic_fact(_), _, 0). % ???
scp_sort_info_val( extra(_), _, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Poolsolve Clauses
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

poolsolve_scp(Pred, Arity, Os, SCP) :-
	make_scp(SCP),
	scp_head(SCP, Head),
	scp_before(SCP, Body),
	scp_info(SCP, pool(Pred/Arity)),
	functor(Head0, Pred, Arity),
	scp_literal(Head0, Os, Head, I),
	access_i(Os, I, [pool(S1,S2),
	                 depth(D1,D2),
	                 infs(J,J),
			 plan(P),
			 depends(Z),
			 cs(K1,K2)]),
        ( plan_option(pxo, Os) ->
	  P1 = ref(Label)
        ; P1 = empty
        ),
	( plan_option(pxc, Os) ->
	  P = plan(P1, [])
        ; plan_option(pxf, Os) ->
	  P = plan(P1, [fact(Head0)])
        ; P = P1
        ),
	( plan_option(cxp, Os) ->
	  Body1 = (!)
        ; Body1 = []
        ),
	reduce_cs_call(Os, K1, K2, RedCs),
	append(RedCs, Body1, Body2),
	
	pool_select_clause(Os, Head0, Label, Z, S1, S2, D1, D2, PoolSelect),
	append(PoolSelect, Body2, Body),
	default_scp(SCP).


%%%% 
%%%% Pool Representation
%%%% 

make_pool(Os, Facts, Pool) :-
	% ... Clause is [], no runtime calls needed
	InitLabel = start,
	InitDepends = z,
	add_to_pool_clause(Os, Facts, InitLabel, InitDepends, 0, [], Pool, []).

add_to_pool_clause(Os, Facts, Label, Z, D2, S1, S2, Clause) :-
	map_make_poolfact(Facts, Os, Label, Z, D2, Facts1),
	append(Facts1, S1, S2),
	Clause = [].

pool_select_clause(Os, Fact, Label, Z, S1, S2, D1, D2, Clause) :-
	make_poolfact(Fact1, Os, Label, Z, D, PoolFact1),
	( plan_option(bdxo, Os) ->
	  ( plan_option(bdxoc, Os) ->  
            %% **** NOT YET DEPTH OPTIMAL, HAS TO INCREASE Depth for
	    %% WHOLE CLAUSE, (BDXO here NOT NEEDED...)
	    Test = [ unify_with_occurs_check(Fact, Fact1), 2*D1 >= D ]
	  ; Test = [ unify_with_occurs_check(Fact, Fact1), D2 = D ]
          )
        ; Test = [ unify_with_occurs_check(Fact, Fact1) ]
        ),
	Clause = [ select(PoolFact1, S1, S2) | Test ].

%% runtime accessors - perhaps they can be avoided ...

poolfact_access_scps(Os, SCPs) :-
	access_poolfact(Os, PoolFact, Fact, Label, Z, D),
	make_scp(FactLiteral),
	scp_head(FactLiteral, dec_poolfact_literal(PoolFact, Fact)),
	default_scp(FactLiteral),
	make_scp(FactDepends),
	scp_head(FactDepends, dec_poolfact_depends(PoolFact, Z)),
	default_scp(FactDepends),
	make_scp(FactLabel),
	scp_head(FactLabel, dec_poolfact_label(PoolFact, Label)),
	default_scp(FactLabel),
	make_scp(FactDepth),
	scp_head(FactDepth, dec_poolfact_depth(PoolFact, D)),
	default_scp(FactDepth),
	SCPs = [FactLiteral, FactDepends, FactLabel, FactDepth].

make_poolfact(Fact, Os, Label, Z, D, PoolFact) :-
	access_poolfact(Os, PoolFact, Fact, Label, Z, D).

access_poolfact(Os, PoolFact, Fact, Label, _Z, D) :-
	Args0 = [],
	( plan_option(pxo, Os) ->
	  Args1 = [Label]
        ; Args1 = Args0
        ),
	( plan_option(bdxo, Os) ->
	  Args2 = [D|Args1]
        ; Args2 = Args1
        ),
	Args = Args2,
	( Args = [] -> 
	  PoolFact = Fact
        ; PoolFact =.. [pf,Fact|Args]
        ).

map_make_poolfact([X|Xs], Y1, Y2, Y3, Y4, [X1|Xs1]) :-
    make_poolfact(X, Y1, Y2, Y3, Y4, X1),
    map_make_poolfact(Xs, Y1, Y2, Y3, Y4, Xs1).
map_make_poolfact([], _, _, _, _, []).


%%%% 
%%%% Ancestors Representation.
%%%% 

make_ancestors([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Occurs Check Equations for Repeated Variables in the Head
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_oc_equations(Head, Body, Head1, Body1) :-
	Head =.. Head2,
	append(Head3, [I], Head2),
	oc_equations(Head3, Body, Head4, Body1),
	append(Head4, [I], Head5),
	Head1 =.. Head5.

oc_equations(Head, Body, Head1, Body1) :- 
	oce(Head, [], Body, Head1, _, Body1).

oce(X, Vs, Es, X, Vs, Es) :-
	atomic(X),
	!.
oce(X, Vs, Es, X1, Vs, [unify_with_occurs_check(X, X1)|Es]) :-
	var(X),
	absmember(X, Vs),
	!. 
oce(X, Vs, Es, X, [X|Vs], Es) :-
	var(X),
	!.
oce(X, Vs, Es, X1, Vs1, Es1) :- 
	X =.. [F|As],
	oces(As, Vs, Es, As1, Vs1, Es1),
	X1 =.. [F|As1].
oces([], Vs, Es, [], Vs, Es).
oces([A|As], Vs, Es, [A1|As1], Vs1, Es1) :- 
	oce(A, Vs, Es, A1, Vs2, Es2),
	oces(As, Vs2, Es2, As1, Vs1, Es1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Automatically Inferred NEQ Constraints
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule_auto_constraints(Rule, Constraints, Constraints1) :-
	rule_taut_nequations(Rule, TautNEQs),
	append(TautNEQs, Constraints, Constraints2),
	sort(Constraints2, Constraints1).

% TODO: - add subsumption nequations
%         also constraints have to be considered for subsumption!
%       - merge the nequations with the user supplied nequations

rule_taut_nequations(Rule, NEQs) :-
	%% bindings under which POST is sub-multiset of PRE
	rule_post(Rule, Post),
	rule_pre(Rule, Pre),
	length(Post, LPost),
	length(Pre, LPre),
	LPost =< LPre,
	!,
	term_variables(Post-Pre, Vars),
	findall( Vars,
	         rule_taut_bindings(Post, Pre, Vars),
		 Varlists
	       ),
	map_simplify_binding(Vars, Varlists, Bindings),
	sort(Bindings, Bindings1),
	map_convert_binding(Bindings1, NEQs).
rule_taut_nequations(_Rule, []).	

rule_taut_bindings([L|Ls], Pre, Vars) :-
	select(L1, Pre, Pre1),
	unify_with_occurs_check(L, L1),
	rule_taut_bindings(Ls, Pre1, Vars).
rule_taut_bindings([], _, _).

map_convert_binding([X|Xs], [X1|Xs1]) :-
    convert_binding(X, X1),
    map_convert_binding(Xs, Xs1).
map_convert_binding([], []).

convert_binding(binding(L, R), neq(L1, R1)) :-
	termlist_to_equation_side(L, L1),
	termlist_to_equation_side(R, R1).

termlist_to_equation_side([], nil).
termlist_to_equation_side([Term], Term).
termlist_to_equation_side([T1,T2|Terms], Side) :-
	Side =.. [s,T1,T2|Terms].


%%%%
%%%% slightly adapted from cm.pl
%%%% 
%%%% map simplify binding instantiates L (i.e. the original variables),
%%%% which seems correct (i.e. "imperceptable") since:
%%%% 
%%%% - only the 1st occurence of a var in L is instantiated to a var in R
%%%% 
%%%% - each R can be assumed to be standardized apart form the original L
%%%%   as well as from all other Rs.
%%%%   

map_simplify_binding(L, [R|Rs], [B|Bs]) :-
	simplify_binding(binding(L,R), B),
	map_simplify_binding(L, Rs, Bs).
map_simplify_binding(_, [], []).

simplify_binding(binding(L,R), binding(L1,R1)) :-
	simplify_binding(L, R, [], [], L1, R1).

simplify_binding([], [], _, _, [], []).
simplify_binding([L|Ls], [R|Rs], Vars, PrevRs, Ls1, Rs1) :-
        %%
        %% (1st occurence of a var is propagated by unification into
        %% the right side)
        %%
	( var(R)                     ->  %% right side is a variable
          ( ( contains_var(R, Rs)
            ; contains_var(R, PrevRs)
            )                        ->  %%   contained in another right side
	    ( absmember(R, Vars)     ->  %%     already treated
	      Vars1 = Vars,
              Ls1 = [L|Ls2],             %%       take the item     
	      Rs1 = [R|Rs2]
            ;                            %%     not yet treated
	      L = R,                     %%       unify it with left side
	      Vars1 = [R|Vars],          %%       mark as treated
              Ls1 = Ls2,                 %%       drop the item
	      Rs1 = Rs2	       
            )
	  ;                              %%   not contained in another right s.
	      Vars1 = Vars,
              Ls1 = Ls2,                 %%     drop the item
	      Rs1 = Rs2	       
	  )
        ;                                %% right side is not a variable
	      Vars1 = Vars,
              Ls1 = [L|Ls2],             %%     take the item     
	      Rs1 = [R|Rs2]
        ),	  
	simplify_binding(Ls, Rs, Vars1, [R|PrevRs], Ls2, Rs2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CS - Proper Constraints
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_cs_call(Os, Cs, Cs1, [CSModule:reduce_cs(Cs, Cs1)]) :-
	plan_option(cs(_), Os),
	( plan_option(cs_module(CSModule), Os) ->
	  true
        ; default_cs_module(CSModule)
        ),
	!.
reduce_cs_call(_, _, _, []).

reduce_cs_within_body_call(_, Cs, Cs, []).

merge_cs_call(Os, Constraints, K1, K2, Call) :-
	plan_option(cs(_), Os),
	!,
	( plan_option(cs_module(CSModule), Os) ->
	  true
        ; default_cs_module(CSModule)
        ),
	( memberchk(cs(Cs), Constraints) ->
	  CSModule:compile_merge_cs(Cs, K1, K2, Call)
	; Call = [CSModule:reduce_cs(K1, K2)]
	).
merge_cs_call(_, _, _, _, []).

compile_cs(CSModule, KInput, K1) :-
	CSModule:compile_merge_cs(KInput, K0, K1, MergeCall),
	CSModule:make_true_cs(K0),
	map_call(MergeCall).

map_call([X|Xs]) :-
	call(X),
	map_call(Xs).
map_call([]).

	      
