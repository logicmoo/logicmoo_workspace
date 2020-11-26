/* <module> mpred_stubs.
% Provides a prolog dabase in these predicates...
%
%  Manages the hybrid_tPredStubImpl/1 in various files
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_stubs_file_module,
          [ 
agenda_rescan_mpred_prop/0,
assert_mpred_t/1,
call_for_literal/3,
call_for_literal_db/3,
call_for_literal_db0/3,
call_for_literal_db2/3,
call_for_literal_ideep_ilc/1,
call_mpred_body/2,
call_mpred_body_ilc/2,
call_provided_mpred_storage_op/3,
call_rule_db/3,
call_wdmsg/2,
call_wdmsg/4,
constrain_args/1,
constrain_args/2,
create_stub_body/2,
create_stub_body/3,
cwdl/2,
erase_mpred_storage_op/1,
ensure_universal_stub_plus_mt_why/2,
first_mpred_prop/1,
get_cc/2,
hybrid_tPredStubImpl/1,
is_call_op/1,
is_mpred_change_op/1,
is_mpred_op/1,
is_non_call_op/1,
is_proc_only/1,
is_same_clauses/2,
is_same_clauses/3,
is_tCol/1,
last_arg_ground/1,
last_arg_ground/3,
make_builtin/1,
mpred_prop_ordered/2,
mpred_t_call_op/2,
mpred_t_mpred_storage_clauses_facts/3,
mpred_t_storage_op/2,
mud_call_store_op/2,
mustIsa/2,
must_op/2,
must_same_clauses/2,
no_rescans/0,
out_of_mpred_t/1,
provide_clauses_list/2,
really_add_mpred_storage_op/1,
registerCycPredMtWhy/1,
registerCycPredMtWhy_3/3,
registerCycPredMtWhy_3/4,
renumbervarZ/2,
tf_result/2,
test_call_cut/0,
wff_check_failed/3,
wff_check_mpred_t_throw/1,
mpred_stubs_file/0
          ]).

%:- include('mpred_header.pi').

:- endif.

% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx


:- meta_predicate

% mpred_stubs
call_mpred_body(*,0),
% mpred_stubs
call_mpred_body_ilc(*,0),
% mpred_stubs
cwdl(0,+),
% mpred_stubs
must_op(*,0),
% mpred_stubs
tf_result(0, -),
registerCycPredMtWhy(0),
call_provided_mpred_storage_op(*,0,*).

:- module_transparent
agenda_rescan_mpred_prop/0,
assert_mpred_t/1,
call_for_literal/3,
call_for_literal_db/3,
call_for_literal_db0/3,
call_for_literal_db2/3,
call_for_literal_ideep_ilc/1,
call_mpred_body/2,
call_mpred_body_ilc/2,
call_provided_mpred_storage_op/3,
call_rule_db/3,
call_wdmsg/2,
call_wdmsg/4,
constrain_args/1,
constrain_args/2,
create_stub_body/2,
create_stub_body/3,
cwdl/2,
erase_mpred_storage_op/1,
first_mpred_prop/1,
get_cc/2,
hybrid_tPredStubImpl/1,
is_call_op/1,
is_mpred_change_op/1,
is_mpred_op/1,
is_non_call_op/1,
is_proc_only/1,
is_same_clauses/2,
is_same_clauses/3,
is_tCol/1,
last_arg_ground/1,
last_arg_ground/3,
make_builtin/1,
mpred_prop_ordered/2,
mpred_t_call_op/2,
baseKB:mpred_provide_storage_op/2,
mpred_t_mpred_storage_clauses_facts/3,
mpred_t_storage_op/2,
mud_call_store_op/2,
mustIsa/2,
must_op/2,
must_same_clauses/2,
no_rescans/0,
out_of_mpred_t/1,
provide_clauses_list/2,
really_add_mpred_storage_op/1,
registerCycPredMtWhy/1,
registerCycPredMtWhy_3/3,
registerCycPredMtWhy_3/4,
renumbervarZ/2,
test_call_cut/0,
wff_check_failed/3,
wff_check_mpred_t_throw/1.


:- set_how_virtualize_file(bodies).

%= 	 	 

%% hybrid_tPredStubImpl( ?VALUE1) is semidet.
%
% Hybrid True Structure Predicate Stub Implimentation.
%
hybrid_tPredStubImpl(prologHybrid).
hybrid_tPredStubImpl(prologPTTP).
hybrid_tPredStubImpl(prologDynamic).
hybrid_tPredStubImpl(prologBuiltin).
hybrid_tPredStubImpl(prologKIF).
hybrid_tPredStubImpl(prologEquality).



%= 	 	 

%% make_builtin( :TermF) is semidet.
%
% Make Builtin.
%
make_builtin(P):- 
 get_arity(P,F,A),
  show_failure(why,(atom(F),integer(A))),
  functor(B,F,A),
  (predicate_property(B,built_in) -> true ;
  (locally(set_prolog_flag(access_level,system),lock_predicate(F/A)),
  check_context_module,
    ain(mpred_prop(F,A,prologBuiltin)),ain(arity(F,A)))).


/*
1 = single value

t = type transitve becoming more specific
T = type transitve becoming more general
tt = type transitve becoming more specific
TT = type transitve becoming more general

i = instance (non col)
p3 = predciate arity
r3 = function arity
a = agent
v = value
o = object
l = literal
f = formula of formulas
 



genls =  ct_cT
relationAllInstance = p2_ct_v
relationMostInstance = p2_ct_sv
relationMostExists = p2_ct_ct
isa X BinaryPredicate = isa_p2_c
isa BinaryPredicate Role = isa_p2_pc
isa 4 number =  isa_v_ft
implies P Q =  





*/
% ================================================
% db_redir_op_if_needed/4
% ================================================
/* TODO RE-INCORPERATE
db_redir_op_if_needed(Op,C0,Prop,_ARGS):- glean_pred_props_maybe(C0),fail.

% predProxyDatabase/1
db_redir_op_if_needed(Op,C0,Prop,_ARGS):- get_mpred_prop(Prop,predProxyDatabase(Other)),must(nonvar(Other)),!,call(Other,Op,C0).

% predProxyAssert/1
db_redir_op_if_needed(change(assert,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyAssert(How)),must(nonvar(How)),!, once(ignore((call(How,C0), run_database_hooks(change(assert,A),C0)))).

% predProxyRetract/1
db_redir_op_if_needed(change(retract,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyRetract(How)),must(nonvar(How)),!, once(ignore((call(How,C0), run_database_hooks(change( retract,A),C0)))).

% predProxyQuery/1
db_redir_op_if_needed(query(Must,HLDS),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyQuery(How)),must(nonvar(How)),!, mpred_op(query(Must,HLDS),call(How,C0)).

% plain props
db_redir_op_if_needed(Op,_C0,Prop,ARGS):- database_modify_units(Op,Unit).

*/

% ================================================================================
% INSTALL STORAGE STUBS
% ================================================================================


% :- meta_predicate baseKB:decl_database_hook(?,0).
%OLD baseKB:decl_database_hook(change(assert,_),local_q_mpred_isa(F,A,StubType)):- maybe_storage_stub(F,StubType).

%OLD baseKB:decl_database_hook(change(assert,_),isa(F,StubType)):- maybe_storage_stub(F,StubType).

%OLD baseKB:decl_database_hook(change(assert,_),arity_no_bc(F,StubType)):-  hybrid_tPredStubImpl(StubType),local_q_mpred_isa(F,A,StubType),must(ensure_universal_stub(F/A)).



%= 	 	 

%% create_stub_body( ?Head, ?Stub) is semidet.
%
% Create Stub Body.
%
create_stub_body(Head,Stub):-create_stub_body(call(conjecture),Head,Stub).
% return fail to cut and fail

%= 	 	 

%% create_stub_body( ?VALUE1, ?Head, ?Stub) is semidet.
%
% Create Stub Body.
%
create_stub_body(call(conjecture),Head,Stub):- Stub = (call_provided_mpred_storage_op(call(conjecture),Head,Then),
   ((Then=(!,Whatnot))->
     (!,Whatnot);
      Then)).


%= 	 	 

%% erase_mpred_storage_op( ?Head) is semidet.
%
% Erase Managed Predicate Storage Oper..
%
erase_mpred_storage_op(Head):-
   create_stub_body(_,Head,Stub),
   doall(retract((Head:- Stub))),
   foreach(clause(Head,Stub,Ref),erase_safe(clause(Head,Stub,Ref),Ref)).


%= 	 	 

%% really_add_mpred_storage_op( ?Head) is semidet.
%
% Really Add Managed Predicate Storage Oper..
%
really_add_mpred_storage_op(Head):-
   create_stub_body(call(conjecture),Head,Stub),
   asserta_if_new((Head:- Stub)).



%= 	 	 

%% renumbervarZ( ?H, ?GGG) is semidet.
%
% Renumbervar Z.
%
renumbervarZ((H:-B),GGG):-is_src_true(B),!,copy_term(H,GG),unnumbervars(GG,GGG),numbervars(GGG,0,_).
renumbervarZ(H,GGG):-copy_term(H,GG),unnumbervars(GG,GGG),numbervars(GGG,0,_).

%= 	 	 

%% is_same_clauses( ?Head, ?NEWHBLISTN, ?HBLISTN) is semidet.
%
% If Is A Same Clauses.
%
is_same_clauses(Head,NEWHBLISTN,HBLISTN):-
   must_maplist(renumbervarZ,HBLISTN,HBLIST),
   must_maplist(renumbervarZ,NEWHBLISTN,NEWHBLIST),
   sort(NEWHBLIST,NEWHBLISTS),
   sort(HBLIST,HBLISTS),
   length(NEWHBLIST,LN),
   length(HBLIST,LO),
   list_difference_eq(HBLIST,NEWHBLIST,MISSING),length(MISSING,LM),
   list_difference_eq(NEWHBLIST,HBLIST,EXTRA),length(EXTRA,LE),
   (NEWHBLIST=@=HBLIST -> true ;((NEWHBLISTS=@=HBLISTS;LM=0) -> wdmsg(trace_or_throw(must_same_clauses(Head,[extra(LE)|EXTRA],[missing(LM)|MISSING]))) ;  
     ((wdmsg((trace_or_throw(must_same_clauses(Head,[LO|HBLIST],[LN|NEWHBLIST],[extra(LE)|EXTRA],[missing(LM)|MISSING])))),!,fail)))).


%= 	 	 

%% must_same_clauses( ?Head, ?HBLISTN) is semidet.
%
% Must Be Successfull Same Clauses.
%
must_same_clauses(Head,HBLISTN):-
   provide_clauses_list(Head,NEWHBLISTN),
   must(is_same_clauses(Head,NEWHBLISTN,HBLISTN)).


%= 	 	 

%% is_same_clauses( ?Head, ?HBLISTN) is semidet.
%
% If Is A Same Clauses.
%
is_same_clauses(Head,HBLISTN):-
   provide_clauses_list(Head,NEWHBLISTN),
   must(is_same_clauses(Head,NEWHBLISTN,HBLISTN)).

/*

clause_u  checks using  TODO  clause_u(C).
call(conjecture)
call(once)  % TODO
change(assert,a) asserts first if =@= is not first
change(assert,z) asserts last if no =@=
change( retract,one)  using =
change( retract,all)  using =

good_for_hybrid(H,F):- not(local_q_mpred_isa(F,A,_ANY_)),predicate_property(H,number_of_clauses(0)),predicate_property(H,dynamic).
ensure_exists(Head):-get_pifunctor3(Head,PHead,F),get_functor(Head,F,A),(predicate_property(PHead,dynamic)->true;(predicate_property(PHead,_)->dmsg(warn(static_pred,F/A));dynamic(F/A))).

*/


% -- CODEBLOCK

%= 	 	 

%% is_tCol( ?V) is semidet.
%
% If Is A True Structure Col.
%
is_tCol(V):-is_ftVar(V),!,fail.
is_tCol(tCol).
is_tCol(F):- local_q_mpred_isa(F,1,tCol);a(tCol,F);a(F,_).


local_q_mpred_isa(F,A,C):- call_u(mpred_prop(F,A,C)).
%= 	 	 

%% is_proc_only( ?V) is semidet.
%
% If Is A Proc.
%
is_proc_only(V):-is_ftVar(V),!,fail.
is_proc_only(F):- functor(P,F,1),predicate_property(P,_), \+ tCol(F).

% -- CODEBLOCK

%= 	 	 

%% is_call_op( ?Var) is semidet.
%
% If Is A Call Oper..
%
is_call_op(Var):-var(Var),!,trace_or_throw(var_is_call_op(Var)).
is_call_op(call(_)):-!.
is_call_op(query(_,_)):-!.
is_call_op(call).


%= 	 	 

%% is_non_call_op( ?Op) is semidet.
%
% If Is A Not Call Oper..
%
is_non_call_op(Op):-is_mpred_op(Op),not(is_call_op(Op)).

% -- CODEBLOCK

%= 	 	 

%% is_mpred_change_op( :TermARG1) is semidet.
%
% If Is A Managed Predicate Change Oper..
%
is_mpred_change_op(change(_,_)).

% -- CODEBLOCK

%= 	 	 

%% is_mpred_op( :TermOp) is semidet.
%
% If Is A Managed Predicate Oper..
%
is_mpred_op(Op):-is_mpred_change_op(Op).
is_mpred_op(call(_)).
is_mpred_op(query(_,_)).
is_mpred_op(clauses(_)).

% -- CODEBLOCK
:- was_export(last_arg_ground/1).

%= 	 	 

%% last_arg_ground( ?HEAD) is semidet.
%
% Last Argument Ground.
%
last_arg_ground(HEAD):-compound(HEAD),functor(HEAD,F,A),last_arg_ground(F, A, HEAD),!.

%= 	 	 

%% last_arg_ground( ?VALUE1, ?A, ?VALUE3) is semidet.
%
% Last Argument Ground.
%
last_arg_ground(mud_test,_,_).
last_arg_ground(_,A,_):-A>2,!.
last_arg_ground(_,A,HEAD):-arg(A,HEAD,Arg),!,ground(Arg).



%= 	 	 

%% call_provided_mpred_storage_op( ?UPARAM1, :GoalH, ?UPARAM3) is semidet.
%
% Call Provided Managed Predicate Storage Oper..
%
call_provided_mpred_storage_op(call(_),H,true):-was_isa(H,I,C),!,isa_asserted(I,C).
call_provided_mpred_storage_op(Op,H,true):-!,no_repeats(loop_check(may_storage_op(Op,H),clause_u(H))).



%= 	 	 

%% test_call_cut is semidet.
%
% Test Call Cut.
%
test_call_cut:- X=!,dmsg(testing_call_cut),X.
test_call_cut:- throw(test_failed(testing_call_cut)).
% :-doall(test_call_cut).

%change(X,B):-mpred_op(X,B),!.
%change(X,B,C):-mpred_op(change(X,B),C),!.
%retract(X,Y):-mpred_op(change(retract,X),Y),!.



%= 	 	 

%% must_op( ?Op, :GoalH) is semidet.
%
% Must Be Successfull Oper..
%
must_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_op0(Op,  H )).
must_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,must_op(Op, HH).
must_op(change(assert,_),Call):-!,must(Call),!.
must_op(change( retract,_),Call):-!,must(Call),!.
must_op(clauses(_),Call):-!,loop_check(on_x_debug(Call)).
must_op(call(_),Call):-!,loop_check(on_x_debug(Call)).
must_op(_,Call):-!,loop_check(on_x_debug(Call)).


%= 	 	 

%% call_wdmsg( ?P, ?DB) is semidet.
%
% Call Wdmsg.
%
call_wdmsg(P,DB):- t_l:noDBaseMODs(_),!,wdmsg(error(noDBaseMODs(P,DB))).
call_wdmsg(P,DB):- get_functor(DB,F,A), call_wdmsg(P,DB,F,A).


%= 	 	 

%% call_wdmsg( ?P, ?DB, ?VALUE3, ?A) is semidet.
%
% Call Wdmsg.
%
call_wdmsg(P,DB,t,_A):-!, append_term(P,DB,CALL),dmsg((CALL)),call_u(CALL).
call_wdmsg(P,MP,F,A):- local_q_mpred_isa(F,A,prologHybrid),must(A>1),into_functor_form(t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,call_u(CALL).
call_wdmsg(P,MP,F,A):-  
  (\+ local_q_mpred_isa(F,A,prologDynamic)), 
  (\+ local_q_mpred_isa(F,A,prologBuiltin)), 
  /* functor(FA,F,A), kb_shared(FA), */ into_functor_form(t,MP,DB),!, 
  append_term(P,DB,CALL),dmsg(info(CALL)),!,call_u(CALL).
call_wdmsg(P,DB,F,A):- append_term(P,DB,CALL),dmsg(info(CALL)),must(local_q_mpred_isa(F,A,prologDynamic);local_q_mpred_isa(F,A,prologBuiltin)),!,call_u(CALL).
%call_wdmsg(P,DB,S,_):-  dtrace((append_term(P,DB,CALL),dmsg((CALL)),call_u(CALL))).


% ================================================================================
% INSTALL MISSING STUBS
% ================================================================================

%:-agenda_rescan_mpred_prop.

% pass 2

%= 	 	 

%% no_rescans is semidet.
%
% No Rescans.
%
no_rescans.

:- was_export(agenda_rescan_mpred_prop/0).


%= 	 	 

%% agenda_rescan_mpred_prop is semidet.
%
% Agenda Rescan Managed Predicate Props.
%
agenda_rescan_mpred_prop:- loop_check(rescan_mpred_prop_ilc,true).

%= 
:- use_module(prolog_statistics:library(statistics)).
% :- reconsult(library(statistics)).
%% rescan_mpred_prop_ilc is semidet.
%
% Rescan Managed Predicate Props Inside Of Loop Checking.
%
rescan_mpred_prop_ilc:-no_rescans,!.
rescan_mpred_prop_ilc:-rescan_duplicated_facts(user,local_q_mpred_isa(_,_)),fail.
rescan_mpred_prop_ilc:- prolog_statistics:time(forall(find_and_call(mpred_prop_ordered(Pred,Prop)),ain(local_q_mpred_isa(Pred,Prop)))),fail.
rescan_mpred_prop_ilc.


%= 	 	 

%% first_mpred_prop( :TermARG1) is semidet.
%
% First Managed Predicate Props.
%
first_mpred_prop(meta_argtypes(_)).


%= 	 	 

%% mpred_prop_ordered( ?VALUE1, ?VALUE2) is semidet.
%
% Managed Predicate Prop Ordered.
%
mpred_prop_ordered(Pred,Prop):-first_mpred_prop(Prop),local_q_mpred_isa(Pred,_,Prop),\+ (local_q_mpred_isa(Pred,_,prologDynamic)).
mpred_prop_ordered(Pred,Prop):-local_q_mpred_isa(Pred,_,Prop),not(first_mpred_prop(Prop)),not(local_q_mpred_isa(Pred,_,prologDynamic)).


% ================================================================================
% GRABOUT STORAGE STUB CLAUSES
% ================================================================================

%= 	 	 

%% provide_clauses_list( ?Head, ?HBLISTO) is semidet.
%
% Provide Clauses List.
%
provide_clauses_list(Head,HBLISTO):- get_pifunctor(Head,PHead),  
  findall((PHead :- B),
   no_repeats([PHead:B],((call_no_cuts(baseKB:mpred_provide_storage_clauses(PHead,B,Proof)),is_source_proof(Proof)))),
   HBLIST),
   create_stub_body(PHead,Stub),
   delete(HBLIST,Stub,HBLISTO),!.



%= 	 	 

%% get_cc( ?PI, ?NC) is semidet.
%
% Get Cc.
%
get_cc(PI,NC):-provide_clauses_list(PI,HBLISTO),length(HBLISTO,NC).


% ==============================
% SETUP HYBRID HOOK
% ==============================

%= 	 	 

%% mpred_provide_setup( ?Op, ?HeadIn, ?StubType, ?OUT) is semidet.
%
% Hook To [baseKB:mpred_provide_setup/4] For Module Mpred_stubs.
% Managed Predicate Provide Setup.
%
baseKB:mpred_provide_setup(Op,HeadIn,StubType,OUT):- StubType \== prologDynamic,
 sanity(var(OUT)),
 must((StubType = prologHybrid)),
 OUT=defined(baseKB:mpred_provide_setup(Op,HeadIn,StubType)).

%% tf_result( :Goal, +TF) is semidet.
%
% True/false Result.
%
tf_result(Call,TF):-(Call->TF=true;TF=fail).

%= 	 	 

%% assert_mpred_t( :TermDB) is semidet.
%
% Assert Managed Predicate True Stucture.
%
assert_mpred_t((G:-B)):-is_src_true(B),!,must(assert_mpred_t(G)).
assert_mpred_t(DB):-once(fully_expand(change(assert,ain),DB,MP)),DB\=@=MP,!,must(assert_mpred_t(MP)).
assert_mpred_t((G1,G2)):-!,assert_mpred_t(G1),assert_mpred_t(G2).
assert_mpred_t(G):-add_from_file(G).

%:- was_export(portray_hb/2).
%portray_hb(H,B):- B==true, !, portray_one_line(H).
%portray_hb(H,B):- portray_one_line((H:-B)).


%= 	 	 

%% hook_mpred_listing( ?What) is semidet.
%
% Hook To [baseKB:hook_mpred_listing/1] For Module Mpred_stubs.
% Hook Managed Predicate Listing.
%
baseKB:hook_mpred_listing(Match):- fail,
 (( 
  dif:dif(Proof,prologRef(_)),
  no_repeats([H,B],((baseKB:mpred_provide_storage_clauses(H,B,Proof)),
                Proof\=prologRef(_))),term_matches_hb(Match,H,B),portray_hb(Proof:H,B))),fail.

      

%= 	 	 

%% clause_u( ?H, ?B, ?What) is semidet.
%
% Hook To [isa_lmconf:mpred_provide_storage_clauses/3] For Module Mpred_stubs.
% Managed Predicate Provide Storage Clauses.
%
:- multifile(baseKB:mpred_provide_storage_clauses/3).
baseKB:mpred_provide_storage_clauses(H,B,Proof):-mpred_t_mpred_storage_clauses_facts(H,B,Proof).


%= 	 	 

%% mpred_t_mpred_storage_clauses_facts( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Managed Predicate True Structure Managed Predicate Storage Clauses Facts.
%
mpred_t_mpred_storage_clauses_facts(H,true,t(H)):-is_list(H),!,length(H,A),A>2,loop_check(call_u(t(H))).
mpred_t_mpred_storage_clauses_facts(H,true,t(H)):-compound(H),!,current_predicate(into_plist_arities/4),functor(H,_,A),A>1,loop_check(call_u(t(H))).
% mpred_t_mpred_storage_clauses_facts(H,B,W):-mpred_t_mpred_storage_clauses_rules(H,B,W),H\=isa(_,_).

% TODO USE PFC FOR FOREWARD RULES
% TODO USE PTTP FOR BACKARDS RULES
% mpred_t_mpred_storage_clauses_rules(H,B,ruleForward(B,H)):-ruleForward(B,H).
% mpred_t_mpred_storage_clauses_rules(H,B,ruleBackward(H,B)):-ruleBackward(H,B).
% mpred_t_mpred_storage_clauses_rules(H,B,'<=>'):-'<=>'(HH,B),each_subterm(HH,SubTerm),compound(SubTerm),SubTerm = H.
% mpred_t_mpred_storage_clauses_rules(H,B,'<=>'):-'<=>'(B,HH),each_subterm(HH,SubTerm),compound(SubTerm),SubTerm = H.



%= 	 	 

%% mpred_provide_storage_op( :TermOp, ?G) is semidet.
%
% Hook To [isa_lmconf:mpred_provide_storage_op/2] For Module Mpred_stubs.
% Managed Predicate Provide Storage Oper..
%
baseKB:mpred_provide_storage_op(Op,HB):-
  must(baseKB:is_mpred_op(Op)),
  (quietly(baseKB:remodulize(Op,HB,HeadBody)),get_functor(HeadBody,F),
    once(F==t; baseKB:a(prologHybrid,F)),   
    locally_tl(already_in_file_term_expansion,mpred_t_storage_op(Op,HeadBody))).

% ====================================================
% mpred_t_storage_op/2
% ====================================================
% HOOK Simplification

%= 	 	 

%% mpred_t_storage_op( ?Op, :TermH) is semidet.
%
% Managed Predicate True Structure Storage Oper..
%
mpred_t_storage_op(Op,(Head:-Body)):- is_src_true(Body),!,mpred_t_storage_op(Op,Head).


mpred_t_storage_op(Op,H):- baseKB:pfcManageHybrids,!,baseKB:mpred_provide_storage_op(Op,H).

mpred_t_storage_op(Op,(:-(Body))):-!,loop_check(mpred_op(Op,(:-(Body))),true),!.

% HOOK for ISA alt-forms
mpred_t_storage_op(_,isa(_,_)):- !,fail. % <- keeps u out of isa hybrids hairs
mpred_t_storage_op(Op,X):- was_isa(X,I,C),!,mpred_op(Op,isa(I,C)).

% HOOK MOST ALL CALLS
mpred_t_storage_op(Op,HeadBodyI):- quietly(((expand_term(HeadBodyI,HeadBodyM)),HeadBodyI\=@=HeadBodyM)),!,mpred_t_storage_op(Op,HeadBodyM).
mpred_t_storage_op(Op,X):- not(is_non_call_op(Op)),!,mpred_t_call_op(Op,X).

% RULE HOOK (for prolog special wrapper body stubs)
mpred_t_storage_op(Op,(Head:-Body)):-
 reduce_mpred_op(Op,Op2),
  special_wrapper_body(Body,direct_to_prolog),!,
  dmsg(direct_to_prolog_special_wrapper_body(Op2,Head,Body)),
   (mud_call_store_op(Op2,(Head:-Body))).  

% OLD RULE HOOK (but we are using it in parallel)
mpred_t_storage_op(Op,(Head:-Body)):- \+ if_defined(use_kif(Head,Body)),
  dmsg(saved_clause_in_hybridRule(Op,Head,Body)),!,
      (mud_call_store_op(Op,ruleBackward(Head,Body))).  

% PTTP RULE HOOK   
mpred_t_storage_op(Op,(Head:-Body)):- 
   if_defined(use_kif(Head,Body)),
   reduce_mpred_op(Op,Op2), 
   CALL0 = (call(Op2,ruleBackward(Head,Body))), % remember outside of KIF just in case
   must(((CALL0,if_defined(mpred_t_tell_kif(Op2,(Head:-Body)))))),!.

% KIF RULE HOOK   
mpred_t_storage_op(Op,RULE):- if_defined(is_kif_clause(RULE)),!,
  reduce_mpred_op(Op,Op2),
  if_defined(mpred_t_tell_kif(Op2,RULE)),!.

% REOP HOOK mpred_t_storage_op(Op1,HeadBody):- reduce_mpred_op(Op1,Op2), Op1\==Op2, mpred_t_storage_op(Op2,HeadBody).
% FACT:-true HOOK   

% FACT DB HOOK
mpred_t_storage_op(Op,HeadBody):-
    into_functor_form(t,HeadBody,DB),
     % wff_check_mpred_t_throw(DB),
     must((mud_call_store_op(Op,DB),sanity(show_call(why,DB)))),!.


%= 	 	 

%% mud_call_store_op( ?Op, :TermOPRAND) is semidet.
%
% Application Call Storage Oper..
%
mud_call_store_op(Op,(H:-B)):- is_src_true(B),!,mud_call_store_op(Op,H).
mud_call_store_op(Op,t('$si$':'$was_imported_kb_content$', _, OPRAND)):-!,loop_check(mpred_op(Op,OPRAND),true).
mud_call_store_op(Op,OPRAND):- show_success(why,wff_check_failed(Op,OPRAND,_WHY)),!.
mud_call_store_op(Op,OPRAND):- reduce_mpred_op(Op,Op2),show_call(why,call(Op2,OPRAND)).


%= 	 	 

%% wff_check_failed( ?VALUE1, ?DB, ?WHY) is semidet.
%
% Well-formed Formula Check Failed.
%
wff_check_failed(_,DB,WHY):- DB =  t('$si$':'$was_imported_kb_content$', WHY, _Assert).

%= 	 	 

%% wff_check_mpred_t_throw( ?DB) is semidet.
%
% Well-formed Formula Check Managed Predicate True Structure Throw.
%
wff_check_mpred_t_throw(DB):- wff_check_failed(_,DB,WHY),trace_or_throw(crazy_mpred_t_was_imported_kb_content(WHY,DB)).
wff_check_mpred_t_throw(_).

% ====================================================
% mpred_t_call_op/2
% ====================================================
% ISA CALL

%= 	 	 

%% mpred_t_call_op( ?Op, ?X) is semidet.
%
% Managed Predicate True Structure Call Oper..
%
mpred_t_call_op(_,isa(_,_)):- !,fail.
mpred_t_call_op(Op,X):- was_isa(X,I,C),!,mpred_op(Op,isa(I,C)).

% FACT CALL HOOK
mpred_t_call_op(_,FACT):- get_functor(FACT, F,A), !,
     lc_tcall(call_for_literal(F,A,FACT)),!.



% ====================================================
% call_for_literal/3
% ====================================================


%% call_for_literal( ?VALUE1, ?VALUE2, ?HEAD) is semidet.
%
% Call For Literal.
%
:- meta_predicate call_for_literal(?,1,*).
call_for_literal(_,_,HEAD):- if_defined(use_kif(HEAD,true)),!,call_u(kif_ask(HEAD)).
call_for_literal(_,_,HEAD):- call_u(use_ideep_swi),!, call_for_literal_ideep_ilc(HEAD),!,loop_check_term(cwdl(CALL,7),HEAD,(CALL)).
call_for_literal(F,A,HEAD):- call_for_literal_db(F,A,HEAD).


%= 	 	 

%% call_for_literal_db( ?F, ?A, ?HEAD) is semidet.
%
% Call For Literal Database.
%
:- meta_predicate call_for_literal_db(?,1,*).
call_for_literal_db(F,A,HEAD):- P=F, HEAD=..[P|ARGS],
   ((lmcache:after_mpred_load)->kb_shared(F/A);true),
   constrain_args(P,ARGS),call_for_literal_db0(F,A,HEAD),constrain_args(P,ARGS).



%= 	 	 

%% cwdl( :GoalCALL, +DEEP7) is semidet.
%
% Cwdl.
%
cwdl(CALL,DEEP7):- call_with_depth_limit(CALL,DEEP7,Result),
   ( Result == depth_limit_exceeded -> (!,fail) ; true).


%= 	 	 

%% call_for_literal_ideep_ilc( ?HEAD) is semidet.
%
% Call For Literal Ideep Inside Of Loop Checking.
%
call_for_literal_ideep_ilc(HEAD):- get_functor(HEAD,F,A),call_for_literal_db(F,A,HEAD).


%= 	 	 

%% call_for_literal_db0( ?F, ?A, ?HEAD) is semidet.
%
% Call For Literal Database Primary Helper.
%
call_for_literal_db0(F,A,HEAD):-no_repeats(HEAD,call_for_literal_db2(F,A,HEAD)).

:- style_check(-singleton).

%= 	 	 

%% call_for_literal_db2( ?VALUE1, ?VALUE2, ?HEAD) is semidet.
%
% Call For Literal Database Extended Helper.
%
call_for_literal_db2(_,_,HEAD):- clause_u(HEAD).
call_for_literal_db2(F,_,   _):- (a(completelyAssertedCollection,F);a(completeExtentAsserted,F)),!,fail.
call_for_literal_db2(F,A,HEAD):- loop_check(call_rule_db(F,A,HEAD)).
call_for_literal_db2(F,A,HEAD):- \+ call_u(use_kif(HEAD,true)),HEAD=..[P1,A1,A2],dif(P2,P1),loop_check_term(clause_u(genlPreds(P2,P1)),gp(P1),fail),
   call_u(t(P2,A1,A2)).



%= 	 	 

%% out_of_mpred_t( ?VALUE1) is semidet.
%
% Out Of Managed Predicate True Stucture.
%
out_of_mpred_t(HEAD):-clause_safe(HEAD,true)*->true;show_success(why,call_u(fact_always_true(HEAD))).



%= 	 	 

%% call_rule_db( ?F, ?A, ?HEAD) is semidet.
%
% Call Rule Database.
%
call_rule_db(F,_A,_HEAD):- a(completelyAssertedCollection,F),!,fail.
call_rule_db(_F,_A,HEAD):- if_defined(use_kif(HEAD,_)),!,call_u(kif_ask(HEAD)).
call_rule_db(_F,_A,HEAD):- ruleBackward(HEAD,BODY,_Why),call_mpred_body(HEAD,BODY).

:- style_check(+singleton).
:- style_check(-singleton).


%= 	 	 

%% call_mpred_body( ?HEAD, :GoalBODY) is semidet.
%
% Call Managed Predicate Body.
%
call_mpred_body(_,true):-!.
call_mpred_body(HEAD,and(A,B)):- !,call_mpred_body(HEAD,A),!,call_mpred_body(HEAD,B).
call_mpred_body(HEAD,(A,B)):- !,call_mpred_body(HEAD,A),call_mpred_body(HEAD,B).
call_mpred_body(HEAD,BODY):- no_repeats(loop_check_term(call_mpred_body_ilc(HEAD,BODY),body_of(HEAD),(nop(dmsg(failure(call_mpred_body_ilc(HEAD,BODY)))),!,fail))).


%= 	 	 

%% call_mpred_body_ilc( ?HEAD, :GoalBODY) is semidet.
%
% Call Managed Predicate Body Inside Of Loop Checking.
%
call_mpred_body_ilc(_HEAD,BODY):- on_x_debug(BODY).



%= 	 	 

%% mustIsa( ?VALUE1, ?VALUE2) is semidet.
%
% Must Be Successfull  (isa/2).
%
mustIsa(I,C):-nonvar(I),!,call_u(isa(I,C)),!.
mustIsa(I,C):-when(nonvar(I),call_u(isa(I,C))).


%= 	 	 

%% constrain_args( ?HEAD) is semidet.
%
% Constrain Arguments.
%
constrain_args(_):-!.
constrain_args(HEAD):-HEAD=..[P|ARGS],constrain_args(P,ARGS).


%= 	 	 

%% constrain_args( ?P, ?AR) is semidet.
%
% Constrain Arguments.
%
constrain_args(_,_):-!.
constrain_args(_P,[AR,GS]):-!,dif(AR,GS).
constrain_args(_,[_P,AR,GS]):-!,dif(AR,GS).
constrain_args(A,B):- if_defined(constrain_args_pttp(A,B),fail).


% call_body_req(HEAD):- functor(HEAD,F,A),HEAD_T=..[F|ARGS],HEAD_T=..[t,F|ARGS],hook_body_req(HEAD,HEAD_T).

/*

% ========================================================================================
% BODY StubType
% ========================================================================================

body_req_isa(I,C):-isa_backchaing(I,C).

body_call_cyckb(HEAD_T):- if_defined(t_l:el_holds_DISABLED_KB), HEAD_T =.. [t|PLIST], baseKB:use_cyc_database,!, no_repeats(if_defined(kbp_t(PLIST))).

% =====================================
% = body_req
% =====================================
body_req(HEAD,HEAD_T):- (hook_body_req(HEAD,HEAD_T)).

:- was_export(body_req_normal/4).
%hook_body_req(HEAD,HEAD_T):- local_q_mpred_isa(F,A,prologPTTP),!,dmsg(warn(hook_body_req(HEAD,HEAD_T))),fail.
%hook_body_req(HEAD,HEAD_T):- local_q_mpred_isa(F,A,prologDynamic),!,dmsg(warn(hook_body_req(HEAD,HEAD_T))),fail.
hook_body_req(_,_,isa(I,C),_):- !, body_req_isa(I,C).
hook_body_req(_,_,_,t(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_,t(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_ ,HEAD_T):- t_l:useOnlyExternalDBs,!, body_call_cyckb(HEAD_T).
% loop checking is not usefull (why the cut was added)
hook_body_req(HEAD,HEAD_T):-  no_repeats(body_req_normal(HEAD,HEAD_T)).


:- was_export(body_req_normal/4).
body_req_normal(HEAD,HEAD_T):- not(ground(HEAD)),!,no_repeats(HEAD_T,body_req_1(HEAD,HEAD_T)).
body_req_normal(HEAD,HEAD_T):- body_req_1(HEAD,HEAD_T),!. 

:- was_export(body_req_1/4).
body_req_1(HEAD,HEAD_T):- get_functor(HEAD,F), local_q_mpred_isa(F,A,lc_tcall),!, lc_tcall(body_req_2(HEAD,HEAD_T)).
body_req_1(HEAD,HEAD_T):- body_req_2(HEAD,HEAD_T).

body_req_2(HEAD,  _):-   get_functor(HEAD,F), local_q_mpred_isa(F,A,external(Module)),!,call(Module:HEAD).
body_req_2(HEAD,HEAD_T):- body_req_with_rules(HEAD,HEAD_T).

body_req_with_rules(HEAD,HEAD_T):-body_req_no_rules(HEAD,HEAD_T).
body_req_with_rules(HEAD,HEAD_T):-body_req_only_rules(HEAD,HEAD_T).

body_req_no_rules(HEAD, _):-     clause(HEAD,  true).
body_req_no_rules(_,_,_  , HEAD_T):- clause(HEAD_T,true).
body_req_no_rules(F,_,_,HEAD_T):- body_req_plus_cyc(F,_,_,HEAD_T).

body_req_only_rules(HEAD, _):-  ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).
body_req_only_rules(_,_,_,t(F,Obj,LValue)):-  choose_val(F,Obj,LValue).

body_req_plus_cyc(F,_,_,HEAD_T):-  local_q_mpred_isa(F,A,cycPlus2(_)),t_l:useOnlyExternalDBs,!,locally(baseKB:use_cyc_database,body_call_cyckb(HEAD_T)).

*/

/*
foo_b(b1).
foo_b(b2):-!.
foo_b(b3):-!.

:- must_det((findall(R,call_no_cuts(foo_b(R)),List),length(List,3))).
*/


%OLD baseKB:decl_database_hook(AR,C):-smart_decl_database(AR,C).

/*
smart_decl_database(AR,svo(S,V,O)):- !,dbase2pred2svo(DBASE,PRED,svo(S,V,O)),!,smart_db_op(AR,DBASE,PRED,svo(S,V,O)).
smart_decl_database(AR,DBASE):- functor_catch(DBASE,t,_),!,dbase2pred2svo(DBASE,PRED,SVO),!,smart_db_op(AR,DBASE,PRED,SVO).
smart_decl_database(AR,PRED):- dbase2pred2svo(DBASE,PRED,SVO),!,smart_db_op(AR,DBASE,PRED,SVO).

smart_db_op(change( retract,AR),A,B,C):- retract_ar_fact(AR,A), retract_ar_fact(AR,B),  retract_ar_fact(AR,C).

retract_ar_fact(all,What):- predicate_property(What,dynamic), !, doall((retract_ar_fact(one,What),fail)).
retract_ar_fact(all,What):- not(predicate_property(What,_)),!.
retract_ar_fact(all,What):- copy_term(What,WO),ignore(once(WO)),must_det(What=@=WO).

retract_ar_fact(one,What):- predicate_property(What,dynamic),!, clause(What,true),retract(What:-true).
retract_ar_fact(one,What):- predicate_property(What,_),!, clause_safe(What,true),!.
retract_ar_fact(one,What):- dmsg(mssing(retract_ar_fact(one,What))).


make_functorskel(_,_,_):-!. % currently ununused
make_functorskel(F,_,_):- fskel(F,_,_,_,_,_,_),!.
make_functorskel(F,N,_):- arity_no_bc(F,N),make_functorskel(F,N,SKEL),asserta(SKEL),!.
make_functorskel(F,N,_):- ignore(arity_no_bc(F,A)),dmsg(todo(trace_or_throw(illegal_make_functorskel(F,N,A)))).

dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-compound(PRED),functor(PRED,F,N),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-compound(DBASE),!,arg(1,DBASE,F),must_det(arity_no_bc(F,N)),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-nonvar(F),must(arity_no_bc(F,N)),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.
*/

:- was_export(registerCycPredMtWhy/1).



%= 	 	 

%% registerCycPredMtWhy_3( ?CM, ?M, ?PI, :TermF) is semidet.
%
% Register Cyc Predicate User Microtheory Generation Of Proof Helper Number 3..
%
registerCycPredMtWhy_3(_CM,M,PI,F/A2):-
  registerCycPredMtWhy_3(M,PI,F/A2).


%= 	 	 

%% registerCycPredMtWhy_3( ?M, ?PI, :TermF) is semidet.
%
% Register Cyc Predicate User Microtheory Generation Of Proof Helper Number 3..
%
registerCycPredMtWhy_3(M,_PI,F/A2):- 
  ignore((A2==3,assertz_if_new(is_never_type(F)))),
  A is A2 - 2, % decl_mpred_mfa(M,F,A),
  ain(mpred_prop(F,A,cycPlus2)).



%= 	 	 

%% registerCycPredMtWhy( :GoalP) is semidet.
%
% Register Cyc Predicate User Microtheory Generation Of Proof.
%
registerCycPredMtWhy(P):-!,baseKB:with_pi(P,baseKB:registerCycPredMtWhy_3).



%= 	 	 

%% ensure_universal_stub_plus( ?F, ?AMinus2) is semidet.
%
% Ensure Universal Stub Plus Presently Unused.
%
ensure_universal_stub_plus(F,AMinus2):-
  kb_shared(F/AMinus2).
% ensure_universal_stub_plus(F,AMinus2):- /*decl_mpred(F,arity(F,AMinus2)), */ decl_mpred_mfa(user,F,AMinus2).
   

%= 	 	 

%% ensure_universal_stub_plus_mt_why( ?F, ?A2) is semidet.
%
% Ensure Universal Stub Plus User Microtheory Generation Of Proof.
%
ensure_universal_stub_plus_mt_why(F,A2):- once(( AMinus2 is A2 -2, ensure_universal_stub_plus(F,AMinus2))),fail.

%ensure_universal_stub_plus_mt_why(F,A2):- cannot_override(F,A2,Why),!,dmsg(cannot_override_plus_2(F,A2,Why)).

ensure_universal_stub_plus_mt_why(F,A2):- 
   export(F/A2),
   functor(HEAD,F,A2),
   HEAD=..[F|ARGS],
   append(ARGSMinus2,[_,_],ARGS),
   HEADMinus2=..[F|ARGSMinus2],
   AMinus2 is A2 -2,
   assert_if_new((HEAD:-HEADMinus2)),!,
  % compile_predicates([HEAD]),
   defaultAssertMt(M),
   kb_shared(M:F/AMinus2).


:- fixup_exports.

mpred_stubs_file.
