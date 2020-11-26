
:- module(common_logic_kb_hooks,
 [kbp_t/1,with_el_holds_disabled/1,noGenlPreds/1,cyckb_t/3,link_to_holds2/2,
  assert_kif/1, assert_kif_dolce/1,
   assert_next_queue/1,
  assert_to_db_list/2,
  big_kb_ASSERTION/2,
  convert_easy_strings/0,
  convert_easy_strings2/0,
  cyckb_t/1,cyckb_t/2,cyckb_t/3,cyckb_t/4,cyckb_t/5,cyckb_t/6,cyckb_t/7,cyckb_t/8,
  cyckb_t_call/1,
  cyckb_t_implies/2,
  cyckb_t_via_genlPreds/1,
  cyckb_t_via_implies/1,
  drain_assert_next_buffer/0,
  el_holds_DISABLED_KB/0,
  get_assertions/2,
  get_b_dnf/2,
  get_dnf_props/6,
  get_props/4,
  cycAssert/1,cycAssert/2,
  get_varsp/2,
  hide_empty_strings/0,
  hide_term_rewrites/0,

  kb_f/1,
  kb_mt/2,
  kb_t/1,
  kb_t/3,
  kbp_t/1,
  kbp_t_list/1,
  kbp_t_list/2,
  kbp_t_list/3,
  kbp_t_list_0/3,
  kbp_t_list_1/3,
  kbp_t_list_prehook/2,
  kbp_to_mpred_0/0,
  kbp_to_mpred_nomore/0,
  kbp_to_mpred_t/0,
  link_to_holds/2,
  link_to_holds/3,
  link_to_holds2/2,
  link_to_holds2/3,
  link_to_holds_DYNAMIC/2,
  link_to_holds_list/2,
  
  move_implied/0,
  move_kb_assertions_matching/4,
noGenlPreds/1,
  nv1000/1,
  proof_from_clause/3,
  prove_calllist/3,
  tiny_kb_ASSERTION/2,
  with_el_holds_disabled/1,
  with_el_holds_enabled/1,
  with_kb_assertions_matching/3,
  write_assertions/0
  ]).

:- include(library('logicmoo/common_logic/common_header.pi')).

%:- baseKB:ensure_loaded(library(pfc)).

% :- dynamic_multifile kbp_t_list_prehook/2.

:-
  op(1150,fx,(was_export)),
  op(1150,fx,(dynamic_multifile)).



:- dynamic_multifile el_assertions:el_holds/4.
:- dynamic_multifile el_assertions:el_holds/5.
:- dynamic_multifile el_assertions:el_holds/6.
:- dynamic_multifile el_assertions:el_holds/7.
:- dynamic_multifile el_assertions:el_holds/8.
:- dynamic_multifile el_assertions:el_holds/9.
:- dynamic_multifile el_assertions:el_holds/10.
:- dynamic_multifile el_assertions:el_holds/11.
:- dynamic_multifile el_assertions:el_holds/12.
:- dynamic_multifile el_assertions:el_holds/13.
:- dynamic_multifile el_assertions:el_holds/14.

:- meta_predicate with_kb_assertions_matching(?,?,0).

:- dynamic_multifile(el_assertions:el_holds_pred_impl/1).
%:- dynamic_multifile is_cyckb_t_pred/2.
:- dynamic_multifile el_assertions:el_holds_pred_impl/1.
%:- dynamic_multifile el_assertions:is_cyckb_t_pred/2.
:- dynamic cyckb_t/3.

:- was_export(kbp_t/1). 

:- set_how_virtualize_file(bodies).


%% assert_kif( ?String) is det.
%
% Assert Knowledge Interchange Format.
%
assert_kif(D):- ain(clif(D)).

%% assert_kif_dolce( ?String) is det.
%
% Assert Knowledge Interchange Format Dolce.
%
assert_kif_dolce(String):-input_to_forms(String,Forms,_Vars),dmsg(warn(assert_kif_dolce(Forms))),!,assert_kif(Forms).

cycAssert(O):-assert_kif(O).
cycAssert(O,Mt):-assert_kif(Mt:O).

%= 	 	 

%% kbp_t( ?VALUE1) is semidet.
%
% Knowledge Base P- True Stucture.
%
kbp_t(_):- \+ lmcache:loaded_external_kbs(_),!,fail.
% kbp_t(PLIST):- ground(PLIST),!,no_repeats(call_no_cuts(kbp_t_list_prehook(PLIST,PLISTO))),kbp_t_list(PLISTO).
% kbp_t(PLIST):- kbp_t_list_prehook(PLIST,PLISTO),kbp_t_list(PLISTO).
% TODO RE-ENABLE 
% kbp_t(PLIST):- kbp_t_list(PLIST). % append(PLIST,[_MT,_PROOF],PLISTO), apply(el_assertions:el_holds,PLISTO).  % el_assertions:el_holds has 2 extra args our callers shouldnt be forced to use.. but this is a big slowdown


:- was_export(kb_f/1).

%= 	 	 

%% kb_f( ?X) is semidet.
%
% Knowledge Base False.
%
kb_f(X):- assertion_f(X).



%= 	 	 

%% get_b_dnf( ?DNFA, ?DNFAO) is semidet.
%
% Get Backtackable Disjunctive Normal Form.
%
get_b_dnf([DNFA],DNFA):-!.
get_b_dnf(DNFA,DNFAO):- length(DNFA,L),atom_concat(and,L,Pred),!,DNFAO=..[Pred|DNFA].


%= 	 	 

%% get_dnf_props( ?TRUTH, ?VALUE2, ?DNFC, ?VARS, ?MT, :TermDNFCO) is semidet.
%
% Get Disjunctive Normal Form Props.
%
get_dnf_props(TRUTH,[],DNFC,VARS,MT,[dnf(c,DNFCO)|PROPS]):-!,get_props(TRUTH,VARS,MT,PROPS),get_b_dnf(DNFC,DNFCO).
get_dnf_props(TRUTH,DNFA,[],VARS,MT,[dnf(a,DNFAO)|PROPS]):-!,get_props(TRUTH,VARS,MT,PROPS),get_b_dnf(DNFA,DNFAO).
get_dnf_props(TRUTH,DNFA,DNFC,VARS,MT,[dnf(ca,(DNFCO<DNFAO))|PROPS]):-!,get_props(TRUTH,VARS,MT,PROPS),get_b_dnf(DNFC,DNFCO),get_b_dnf(DNFA,DNFAO).


%= 	 	 

%% get_props( ?TRUTH, ?VARS, ?VALUE3, :TermVARSP) is semidet.
%
% Get Props.
%
get_props(TRUTH,VARS,isMissing,VARSP):-!,get_props(TRUTH,VARS,notmissing,[_|VARSP]),!.
get_props(':TRUE-DEF',VARS,MT,[amt(MT),str(':DEFAULT'),truth(':TRUE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':FALSE-DEF',VARS,MT,[amt(MT),str(':DEFAULT'),truth(':FALSE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':TRUE-MON',VARS,MT,[amt(MT),str(':MONOTONIC'),truth(':TRUE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':FALSE-MON',VARS,MT,[amt(MT),str(':MONOTONIC'),truth(':FALSE')|VARSP]):-get_varsp(VARS,VARSP),!.

%= 	 	 

%% get_varsp( ?VARS, ?VARS) is semidet.
%
% Get Varsp.
%
get_varsp([],[]):-!.
get_varsp(VARS,[vars(VARS)]):-!.


%= 	 	 

%% tiny_kb_ASSERTION( ?PLIST, ?PROPS) is semidet.
%
% Tiny Knowledge Base Assertion.
%
tiny_kb_ASSERTION(_PLIST,_PROPS):-!,fail.
%MAYBE LATER tiny_kb_ASSERTION(PLIST,PROPS):- 'TINYKB-ASSERTION'(TRUTH,[DNFA,DNFC],MT,VARS,PLIST),get_dnf_props(TRUTH,DNFA,DNFC,VARS,MT,PROPS).
%MAYBE LATER tiny_kb_ASSERTION(PLIST,PROPS):- 'TINYKB-ASSERTION'(TRUTH,[DNFA,DNFC],MT,VARS,_HL,PLIST),get_dnf_props(TRUTH,DNFA,DNFC,VARS,MT,PROPS).

%big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, DNF, MT, VARS, A1437, DIR),dnf_to_pnf(DNF,PLIST),get_props(TRUTH,VARS,MT,PROPS).
%big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, _DNF, MT, VARS, A1437, DIR,_,PLIST),get_props(TRUTH,VARS,MT,PROPS).
%big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, _DNF, MT, VARS, A1437, DIR,PLIST),get_props(TRUTH,VARS,MT,PROPS).

%= 	 	 

%% big_kb_ASSERTION( ?VALUE1, ?VALUE2) is semidet.
%
% Big Knowledge Base Assertion.
%
big_kb_ASSERTION(_,_):-fail.

:- was_export(get_assertions/2).
% get_assertions(PLIST,PROPS):-big_kb_ASSERTION(PLISTIn,PROPS),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).

%= 	 	 

%% get_assertions( ?PLIST, ?PROPS) is semidet.
%
% Get Assertions.
%
get_assertions(PLIST,PROPS):-current_predicate(tiny_kb_ASSERTION/2),!,tiny_kb_ASSERTION(PLISTIn,PROPS),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).
get_assertions(PLIST,PROPS):-between(2,19,X),length(PLISTIn,X),kbp_t_list(PLISTIn,PROPS,_),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).



%= 	 	 

%% nv1000( ?S) is semidet.
%
% Nv Secondary Helper Primary Helper Primary Helper Primary Helper.
%
nv1000(S):-numbervars(S,100,_,[singletons(true),attvar(bind)]).


% length(SENT,N),N>1,append(SENT,[MT,Props],PLIST),apply(el_assertions:el_holds,PLIST),member(Var,SENT),var(Var).
% length(SENT,N),N>1,kbp_t_list(SENT,Proof),member(Var,SENT),var(Var).

:- was_export((kb_t/1)).

%= 	 	 

%% kb_t( ?Call) is semidet.
%
% Knowledge Base True Stucture.
%
kb_t(Call):- into_plist(Call,PLIST),[AH|LIST]=PLIST,!, kb_t(AH,LIST,PLIST).



%= 	 	 

%% kb_t( ?AH, ?VALUE2, ?PLIST) is semidet.
%
% Knowledge Base True Stucture.
%
kb_t(AH,_,PLIST):-var(AH),!,kbp_t(PLIST).
kb_t(t,PLIST,_):- !,kbp_t(PLIST).  % t is our versuion of '$holds' or call/N
kb_t(genls,PLIST,_):- !,kbp_t([genls|PLIST]). % rewrite hack for SUMO callers
kb_t(AH,PLIST,_):- is_holds_true(AH),!,kb_t(PLIST). % is_holds_true/1 is temp disabled for speed
kb_t(AH,PLIST,_):- is_holds_false(AH),!,kb_f(PLIST). % is_holds_false(not).
kb_t(_,_,PLIST):- kbp_t(PLIST).


:- was_export(link_to_holds2/2).

%= 	 	 

%% link_to_holds2( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds Extended Helper.
%
link_to_holds2(Pred,Target:TPred):- !,link_to_holds2(Pred,Target,TPred).
link_to_holds2(Pred,TargetPred):- !,link_to_holds2(Pred,user,TargetPred).


%= 	 	 

%% link_to_holds2( ?Pred, ?M, ?TargetPred) is semidet.
%
% Link Converted To Holds Extended Helper.
%
link_to_holds2(Pred,M,TargetPred):- 
  forall(between(2,12,X),((length(PLIST,X),append(PLIST,[_MT],PLISTMT),append(PLISTMT,[_PROOF],PLISTMTPROOF), 
   export(Pred/X),  
 % X2 is X + 2, nop(export(TargetPred/X2)),  
  A=..[Pred|PLIST],
  B=..[TargetPred|PLISTMTPROOF],  
   assertz_if_new((A :- (M:B) ))))).

:- was_export(link_to_holds/2).

%= 	 	 

%% link_to_holds( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds.
%
link_to_holds(Pred,Target:TPred):- !,link_to_holds(Pred,Target,TPred).
link_to_holds(Pred,TargetPred):- !,link_to_holds(Pred,user,TargetPred).

%= 	 	 

%% link_to_holds( ?Pred, ?M, ?TargetPred) is semidet.
%
% Link Converted To Holds.
%
link_to_holds(Pred,M,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
   export(Pred/X),  
   nop(export(TargetPred/X)),  
  A=..[Pred|PLIST],
  B=..[TargetPred|PLIST],  
   assertz_if_new((A:- M:B)))).

:- was_export(link_to_holds_DYNAMIC/2).

%= 	 	 

%% link_to_holds_DYNAMIC( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds Dynamic.
%
link_to_holds_DYNAMIC(Pred,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
   export(Pred/X),  
   export(TargetPred/X),  
  A=..[Pred|PLIST],
  B=..[TargetPred|PLIST],  
   assertz_if_new((A:-B)))).
:- was_export(link_to_holds_list/2).

%= 	 	 

%% link_to_holds_list( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds List.
%
link_to_holds_list(Pred,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
   export(Pred/X),  
   export(TargetPred/1),  
  A=..[Pred|PLIST],
  B=..[TargetPred,PLIST],  
   assertz_if_new((A:-B)))).


/*
cyckb_t(P,A1,A2,A3,A4,A5,A6,A7):- t([P,A1,A2,A3,A4,A5,A6,A7]).
cyckb_t(P,A1,A2,A3,A4,A5,A6):- t([P,A1,A2,A3,A4,A5,A6]).
cyckb_t(P,A1,A2,A3,A4,A5):- t([P,A1,A2,A3,A4,A5]).
cyckb_t(P,A1,A2,A3,A4):- t([P,A1,A2,A3,A4]).
cyckb_t(P,A1,A2,A3):- t([P,A1,A2,A3]).
cyckb_t(P,A1,A2):- t([P,A1,A2]).
cyckb_t(P,A1):- t([P,A1]).
*/

:- dynamic_multifile(el_holds_DISABLED_KB/0).
:- was_export(el_holds_DISABLED_KB/0).
:- asserta(el_holds_DISABLED_KB).


%= 	 	 

%% with_el_holds_enabled( :Goal) is semidet.
%
% Using El Holds Enabled.
%
:- meta_predicate(with_el_holds_enabled(0)).
with_el_holds_enabled(Goal):-locally_hide(el_holds_DISABLED_KB,Goal).

%= 	 	 

%% with_el_holds_disabled( :Goal) is semidet.
%
% Using El Holds Disabled.
%
:- meta_predicate(with_el_holds_disabled(0)).
with_el_holds_disabled(Goal):-locally(el_holds_DISABLED_KB,Goal).

%:- link_to_holds_DYNAMIC(cyckb_t,el_holds_DISABLED_KB).
:- link_to_holds2(cyckb_t,el_assertions:el_holds).

:- was_export(cyckb_t/1).

%= 	 	 

%% cyckb_t( ?Compound) is semidet.
%
% Cyckb True Stucture.
%
cyckb_t([P|LIST]):-!, \+ (el_holds_DISABLED_KB), apply(cyckb_t,[P|LIST]).
cyckb_t(Compound):- \+ (el_holds_DISABLED_KB), Compound=..[F,A|List] , apply(cyckb_t,[F,A|List]).

:- was_export(noGenlPreds/1).

%= 	 	 

%% noGenlPreds( ?X) is semidet.
%
% No Genl Predicates.
%
noGenlPreds(coGenlPreds).
noGenlPreds(isa).
noGenlPreds(genls).
noGenlPreds(X):-not(atom(X)),!.
noGenlPreds(_).

:- link_to_holds_list(cyckb_t,cyckb_t_via_genlPreds).

%= 	 	 

%% cyckb_t_via_genlPreds( :TermGP) is semidet.
%
% Cyckb True Structure Via Genl Predicates.
%
cyckb_t_via_genlPreds([GP|_]):- noGenlPreds(GP),!,fail.
cyckb_t_via_genlPreds([GP,A,B]):- loop_check(cyckb_t(genlInverse,P,GP)), P\=GP, loop_check(cyckb_t([P,B,A])).
cyckb_t_via_genlPreds([GP|LIST]):- loop_check(cyckb_t(genlPreds,P,GP)), P\=GP, loop_check(cyckb_t([P|LIST])).


:- link_to_holds_list(cyckb_t,cyckb_t_via_implies).

%= 	 	 

%% cyckb_t_via_implies( ?CONSEQ) is semidet.
%
% Cyckb True Structure Via Implies.
%
cyckb_t_via_implies(CONSEQ):- fail, loop_check(cyckb_t_implies(ANTE,CONSEQ)), loop_check(cyckb_t_call(ANTE)).


%= 	 	 

%% cyckb_t_call( ?ANTE) is semidet.
%
% Cyckb True Structure Call.
%
cyckb_t_call(ANTE):- nop(cyckb_t_call(ANTE)),!,fail.

%= 	 	 

%% cyckb_t_implies( ?ANTE, ?CONSEQ) is semidet.
%
% Cyckb True Structure Implies.
%
cyckb_t_implies(ANTE,CONSEQ):- nop(cyckb_t_implies(ANTE,CONSEQ)),!,fail.

:- thread_local t_l:useDbase_t/0.


%= 	 	 

%% kbp_t_list_prehook( ?PLIST, ?PLIST) is semidet.
%
% Knowledge Base P- True Structure List Prehook.
%
%:- kb_shared(kbp_t_list_prehook/2).
kbp_t_list_prehook(PLIST,PLIST).

:- was_export(kbp_t_list/1). 

%= 	 	 

%% kbp_t_list( ?PLIST) is semidet.
%
% Knowledge Base P- True Structure List.
%

kbp_t_list(PLIST):- t_l:useDbase_t, call_u(t(PLIST)).
kbp_t_list(PLIST):- apply(cyckb_t,PLIST).


:- was_export(kbp_t_list/2). 
% kbp_t_list(PLIST,t(PLIST)):- t_l:useDbase_t,  t(PLIST).

%= 	 	 

%% kbp_t_list( ?PLIST, ?Proof) is semidet.
%
% Knowledge Base P- True Structure List.
%
kbp_t_list(PLIST,Proof):- kbp_t_list(PLIST,_A,Proof).

% 
%  current_predicate(F/A),functor(P,F,A),predicate_property(P,number_of_clauses(N)),dif(B,true), clause(P, B, Ref),B\=(!,_), B=true.

:- was_export(kbp_t_list/3). 
kbp_t_list(PLIST,Props):- tiny_kb_ASSERTION(PLIST,Props).

%= 	 	 

%% kbp_t_list( ?PLIST, ?Props, ?Proof) is semidet.
%
% Knowledge Base P- True Structure List.
%
kbp_t_list(PLIST,[amt(t)],Proof):- t_l:useDbase_t,  CallList = [t|PLIST],Call=..CallList,/*Call,*/ clause(Call,true,Ref),clause(Head, Body, Ref),proof_from_clause(Head, Body, Proof).
kbp_t_list(PLIST,Props,Proof):- is_list(PLIST),!,kbp_t_list_1(PLIST,Props,Proof).
kbp_t_list(PLIST,Props,Proof):- kbp_t_list_0(PLIST,Props,Proof).


%= 	 	 

%% kbp_t_list_0( ?PLIST, ?Props, ?Proof) is semidet.
%
% Knowledge Base P- True Structure list  Primary Helper.
%
kbp_t_list_0(PLIST,Props,Proof):- between(3,2,N), length(PLIST,N),kbp_t_list_1(PLIST,Props,Proof).
kbp_t_list_0(PLIST,Props,Proof):- between(4,12,N), length(PLIST,N),kbp_t_list_1(PLIST,Props,Proof).


%= 	 	 

%% kbp_t_list_1( ?PLIST, :TermMT, ?Proof) is semidet.
%
% Knowledge Base P- True Structure list  Secondary Helper.
%
kbp_t_list_1(PLIST,[amt(MT)|PropsV], Proof):- append(PLIST,[MT,PropsV],CallList),!,prove_calllist(el_assertions:el_holds,CallList,Proof).
% kbp_t_list_1(PLIST,[cyckb_t], Proof):- CallList = [cyckb_t|PLIST],prove_calllist(cyckb_t,CallList,Proof).


%= 	 	 

%% prove_calllist( ?Functor, ?CallList, ?Proof) is semidet.
%
% Prove Calllist.
%
prove_calllist(Functor,CallList,Proof):- Call =.. [Functor|CallList], clause(Call, true,Ref),clause(PHead, PBody, Ref),proof_from_clause(PHead, PBody, Proof).
prove_calllist(Functor,CallList,Proof):- dif(Body,true), Head =.. [Functor|CallList],clause(Head, Body, Ref),must_det(not(Body=true)),Body,clause(PHead, PBody, Ref),proof_from_clause(PHead, PBody, Proof).

:- was_export(kb_mt/2).

%= 	 	 

%% kb_mt( ?C, ?MT) is semidet.
%
% Knowledge Base User Microtheory.
%
kb_mt(C,MT):- into_plist(C,PLIST),!,  append([el_assertions:el_holds|PLIST],[MT,_PropsV],CallList),Call=..CallList,Call.
kb_mt(C,t):- t_l:useDbase_t, call_u(t(C)).




%= 	 	 

%% proof_from_clause( ?Head, ?VALUE2, ?Head) is semidet.
%
% Proof Converted From Clause.
%
proof_from_clause(Head, true, Head):-!.
proof_from_clause(Head, Body, ((Head:- Body))).

:- dynamic assert_next_queue/1.
:- export(assert_next_queue/1).

:- was_export(move_kb_assertions_matching/4).

%= 	 	 

%% move_kb_assertions_matching( ?PLIST, ?Match, ?Replace, ?Where) is semidet.
%
% Move Knowledge Base Assertions Matching.
%
move_kb_assertions_matching(PLIST,Match,Replace,Where):- 
% dmsg(move_kb_assertions_matching(PLIST,Match,Replace,to(Where))),
  doall((kbp_t_list(PLIST,Call),
   forall(retract(Call),
   (subst(PLIST:Call,Match,Replace,NewPLIST:NewCall),
   assert_to_db_list(Where,[rewrite,NewPLIST,NewCall]))))).



%= 	 	 

%% assert_to_db_list( ?HOLDS, ?PLIST) is semidet.
%
% Assert Converted To Database List.
%
assert_to_db_list(HOLDS,PLIST):- safe_univ(Call,[HOLDS|PLIST]), assert(assert_next_queue(Call)).



%= 	 	 

%% with_kb_assertions_matching( ?PLIST, ?Proof, :Goal) is semidet.
%
% Using Knowledge Base Assertions Matching.
%
with_kb_assertions_matching(PLIST,Proof,Call):- doall((kbp_t_list(PLIST, Proof),Call)).
 
:- was_export(kbp_to_mpred_t/0).

%= 	 	 

%% kbp_to_mpred_t is semidet.
%
% Knowledge Base P- Converted To Managed Predicate True Stucture.
%
kbp_to_mpred_t:- must_det(locally_tl(useOnlyExternalDBs,kbp_to_mpred_0)).


%= 	 	 

%% kbp_to_mpred_0 is semidet.
%
% Knowledge Base P- Converted To Managed Predicate  Primary Helper.
%
kbp_to_mpred_0:-!.
% kbp_to_mpred_0:- once(time_call(move_implied)),fail.
kbp_to_mpred_0:- once(time_call(hide_term_rewrites)),fail.
kbp_to_mpred_0:- once(time_call(hide_empty_strings)),fail.
% kbp_to_mpred_0:- once(time_call(convert_easy_strings)),fail.
% kbp_to_mpred_0:- once(time_call(convert_easy_strings2)),fail.
kbp_to_mpred_0:- time_call(drain_assert_next_buffer),!.


%= 	 	 

%% kbp_to_mpred_nomore is semidet.
%
% Knowledge Base P- Converted To Managed Predicate No More.
%
kbp_to_mpred_nomore:- forall((into_plist(_Call,PLIST),kbp_t(PLIST)),assert_to_db_list(_F,PLIST)),
 retractall(baseKB:use_cyc_database),tell('a.txt'),listing(t),listing('ASSERTION'),told,dmsg(done_mpred_t).


:- was_export(move_implied/0).

%= 	 	 

%% move_implied is semidet.
%
% Move Implied.
%
move_implied:-doall((between(2,6,Len),length(PLIST,Len), 
   Call=..[assertion_holds,implied|PLIST],
   retract(hl_holds:Call),
   append(ALIST,[Last],PLIST),NewCall=..[assertion_holds,impliedBy,ALIST,Last],
   assert(assert_next_queue(hl_holds:NewCall)))),
   drain_assert_next_buffer.

:- was_export(hide_term_rewrites/0).

%= 	 	 

%% hide_term_rewrites is semidet.
%
% Hide Term Rewrites.
%
hide_term_rewrites :- locally_tl(useOnlyExternalDBs,
 % remove badjuju from the KB (that is unbould slots in the middle of GAFs)
 % hl_holds:retractall(assertion_holds(isa, badjuju, 'Thing')),
 % hl_holds:retractall(el_assertions:el_holds(genls, badjuju, 'AerosolStuff',_,_)), 
 % hl_holds:retractall(assertion_holds(genls, badjuju, 'BiologicalAgentStuff')), 
 % the next few lines will cover the top
 doall((between(2,6,Len),length(PLIST,Len),
 forall(member(vvvar,PLIST),move_kb_assertions_matching(PLIST,vvvar,_,term_rewrites_kb))))),
 drain_assert_next_buffer.

:- was_export(hide_empty_strings/0).

%= 	 	 

%% hide_empty_strings is semidet.
%
% Hide Empty Strings.
%
hide_empty_strings :- locally_tl(useOnlyExternalDBs,
 % remove more badjuju from the KB (that is unbould slots in the middle of GAFs)
 % the next few lines will cover the top
 doall((between(2,6,Len),length(PLIST,Len),
 forall(member('',PLIST),move_kb_assertions_matching(PLIST,'','',term_rewrites_kb))))),
 drain_assert_next_buffer.


:- was_export(convert_easy_strings/0).

%= 	 	 

%% convert_easy_strings is semidet.
%
% Convert Easy Strings.
%
convert_easy_strings:-
 doall((between(2,6,Len),length(PLIST,Len),
 forall(member(string(_),PLIST),
  time_call(with_kb_assertions_matching(PLIST,Proof,must_det(print_sentence(Proof))))))),drain_assert_next_buffer.


%= 	 	 

%% convert_easy_strings2 is semidet.
%
% Convert Easy Strings Extended Helper.
%
convert_easy_strings2:-
 doall((between(2,6,Len),length(PLIST,Len),
 forall(member([_|_],PLIST),
  time_call(with_kb_assertions_matching(PLIST,Proof,must_det(print_sentence(Proof))))))),drain_assert_next_buffer.


%= 	 	 

%% drain_assert_next_buffer is semidet.
%
% Drain Assert Next Buffer.
%
drain_assert_next_buffer:- predicate_property(assert_next_queue(_),number_of_clauses(CL)),dmsg(drain_assert_next_buffer(CL)),
 time_call(doall((retract(assert_next_queue(Call)),asserta_if_new(Call)))).


%= 	 	 

%% write_assertions is semidet.
%
% Write Assertions.
%
write_assertions:-
 tell(holds_all),
 listing(assertion_holds_mworld0),
 listing(assertion_holds),
 listing(term_rewrites_kb),
 told.

:- fixup_exports.

