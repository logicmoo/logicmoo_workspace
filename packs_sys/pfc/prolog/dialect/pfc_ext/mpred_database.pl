/*  

% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_kb_ops.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- if(current_prolog_flag(xref,true)).
:- module(mpred_kb_ops,[]).
:- include('mpred_header.pi').
:- endif.

%:- user:use_module(library(clpfd),['#='/2]).

%% get_arity( :TermTerm, ?F, ?A) is semidet.
%
% Get Arity.
%
get_arity(Term,F,A):- atom(Term),F=Term,!,ensure_arity(F,A).
get_arity(F/A,F,A):-!,atom(F),ensure_arity(F,A),!,(A>0).
get_arity('//'(F , A),F,A2):- must(integer(A)),!, atom(F), is(A2 , A+2), ensure_arity(F,A2),!,(A2>0).  
get_arity('//'(F , A),F,A2):- 
  use_module(library(clpfd),['#='/2]),!, 
  atom(F), clpfd:call('#='(A2 , A+2)), 
  ensure_arity(F,A2),!,(A2>0). 
get_arity(M:FA,F,A):-atom(M),!,get_arity(FA,F,A).
get_arity(FA,F,A):- get_functor(FA,F,A),must(A>0).

% arity_no_bc(F,A):- call_u(arity(F,A)).
arity_no_bc(F,A):- clause_b(arity(F,A)).
arity_no_bc(F,A):- clause_b(support_hilog(F,A)).
arity_no_bc(F,A):- clause_b(functorDeclares(F)),!,A=1.
arity_no_bc(completeExtentAsserted,1).
arity_no_bc(home,2).
arity_no_bc(record,2).
arity_no_bc(F,A):- suggest_m(M),clause_b(mpred_prop(M,F,AA,_)),nonvar(AA),A=AA.
%arity_no_bc(F,A):- current_predicate(F/A)
% arity_no_bc(F,A):- current_predicate(_:F/A),\+(current_predicate(_:F/AA),AA\=A). =

%% ensure_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Ensure Arity.
%
ensure_arity(F,A):- 
 one_must(
   arity_no_bc(F,A),
   one_must(
    (current_predicate(F/A),(A>0),assert_arity(F,A)),
    (ground(F:A),(A>0),assert_arity(F,A)))),
  !.


%=

%% assert_arity( ?F, :PRED2A) is semidet.
%
% Assert Arity.
%

assert_arity(F,A):- sanity(\+ ((bad_arity(F,A), trace_or_throw_ex(assert_arity(F,A))))), arity_no_bc(F,A),!.
assert_arity(F,A):- arity_no_bc(F,AA), A\=AA,dmsg_pretty(assert_additional_arity(F,AA->A)),!,ain_fast(arity(F,A)).
assert_arity(F,A):- ain_fast(arity(F,A)),!.

bad_arity(F,_):- \+ atom(F).
bad_arity(_,A):- \+ integer(A).
bad_arity('[|]',_).
bad_arity(typeProps,0).
bad_arity(argIsa,2).
bad_arity(isEach,_).
bad_arity(_,0).
bad_arity(prologDynamic,2).
bad_arity(F,A):- \+ good_pred_relation_name(F,A).


%=

%% good_pred_relation_name( ?F, ?A) is semidet.
%
% Good Predicate Relation Name.
%
good_pred_relation_name(F,A):- \+ bad_pred_relation_name0(F,A).


%=

%% bad_pred_relation_name0( ?V, ?VALUE2) is semidet.
%
% Bad Predicate Relation Name Primary Helper.
%
bad_pred_relation_name0(V,_):- \+ atom(V),!.
bad_pred_relation_name0('[]',_).
bad_pred_relation_name0('',_).
bad_pred_relation_name0('!',_).
bad_pred_relation_name0('{}',_).
bad_pred_relation_name0(',',_).
bad_pred_relation_name0('[|]',_).

%=

%% bad_pred_relation_name1( ?X, ?Y) is semidet.
%
% Bad Predicate Relation Name Secondary Helper.
%
bad_pred_relation_name1(X,Y):-bad_pred_relation_name0(X,Y).
bad_pred_relation_name1(F,A):-must_det((atom_codes(F,[C|_]),to_upper(C,U))),!, U == C, A>1.
bad_pred_relation_name1(F,A):-arity_no_bc(F,AO), A \= AO.

% :-after_boot(writeq("Seen Mpred_props at start!\n")),!.

%=

%% functor_check_univ( ?G1, ?F, ?List) is semidet.
%
% Functor Check Univ.
%
functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,user]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.


%:- endif.
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_bugger.pl')).
%:- ensure_loaded(pfc_lib).
%:- use_module(mpred_type_isa).
%:- use_module(library(util_varnames)).

/*
:- module_transparent retract_mu/1,
         assert_mu/4,
         asserta_mu/2,
         assertz_mu/2,
         assert_u/1,
         asserta_u/1,
         assertz_u/1,
         attempt_side_effect/1.
*/
:- module_transparent(attvar_op/2).


:- meta_predicate 
      pred_head(1,*),
      attempt_side_effect(+),
      call_s(*),
      oncely(*),
      naf(*),
      call_s2(*),
      mpred_update_literal(*,*,0,*),
      mpred_retry(*),
%      mpred_op(?, ?),
      mpred_facts_only(*),
      map_unless(1,:,*,*),      
      pfc_is_callable(*),     
%      deducedSimply(*),
      cnstrn0(*,+),
      cnstrn(*),
      cnstrn(+,*),
      attvar_op(*,*),
      % clause_u(+,+,-),
      % call_u(+),
      assertz_mu(*),      
      assertz_mu(*,+),
      if_missing1(*),
      assert_mu(+),
      assert_mu(+,+,+,+),
      ain_minfo_2(1,*),
      ain_minfo(1,*),                                    
%      whenAnd(0,0),
      % mpred_call_0(*),
      mpred_bc_only(*),
      mpred_bc_only0(*),
      mpred_prove_neg(*),
      call_u_req(*),
      pfcBC_NoFacts(*).

 :- meta_predicate mpred_get_support_one(0,*).
 :- meta_predicate mpred_get_support_precanonical_plus_more(0,*).
 % :- meta_predicate '__aux_maplist/2_cnstrn0+1'(*,0).
 :- meta_predicate repropagate_0(*).
 :- meta_predicate trigger_supporters_list(0,*).
 :- meta_predicate repropagate_meta_wrapper(*).
 :- meta_predicate repropagate_0(*).


% oncely later will throw an error if there where choice points left over by call
:- meta_predicate(oncely(*)).
:- was_export(oncely/1).


%% oncely( :Goal) is semidet.
%
% Oncely.
%
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,call_u(Call).
oncely(Call):-once(Call).
% ================================================
% mpred_op/2
% ================================================

/*
query(t, call_u, G):- call_u(G).
query(_, _, Op, G):- dtrace(call_u(call(Op,G))).
once(A,B,C,D):-trace_or_throw_ex(once(A,B,C,D)).
*/




% ================================================
% pfc_is_callable/call_u/naf
% ================================================

%:- was_dynamic(naf/1).
:- meta_predicate(naf(*)).
:- was_export(naf/1).



%% naf( :Goal) is semidet.
%
% Negation-By-Faliure.
%
naf(Goal):- (\+ call_u(Goal)).

:- meta_predicate(pfc_is_callable(*)).
:- was_export(pfc_is_callable/1).



%% pfc_is_callable( :GoalC) is semidet.
%
% If Is A Callable.
%
pfc_is_callable(C):-current_predicate(_,C),!.


:- style_check(+singleton).

% TODO READD
%:- foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttRelationType,
%     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,functorIsMacro,prologListValued,prologSingleValued),P),.. )


% TODO ISSUE https://github.com/logicmoo/PrologMUD/issues/7

%% check_context_module is semidet.
%
% Check Context Module. (throws if it turns out wrong)
%

check_context_module:- !.
% check_context_module:- is_release,!.
check_context_module:- 
  sanity((source_context_module(M1),clause_b(mtHybrid(M1)))),
  sanity((defaultAssertMt(M2),clause_b(mtHybrid(M2)))).

%% check_real_context_module is semidet.
%
% Check Real Context Module (throws if it turns out wrong)
%
check_real_context_module:- is_release,!.
check_real_context_module:-!.
check_real_context_module:- sanity((context_module(M1),defaultAssertMt(M2),must(M1==M2))).


% ======================= mpred_file('pfcsyntax').	% operator declarations.

:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').




%% mreq( +G) is semidet.
%
% Mreq.
%
mreq(G):- if_defined(call_u(G),fail).

% ======================= mpred_file('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                            bug in the recorded database.
%   Purpose: core Pfc predicates.

/*

LogicMOO is mixing Mark Stickel's PTTP (prolog techn theorem prover) to create horn clauses that 
 PFC forwards and helps maintain in visible states )  in prolog knowledge baseable.. We use '$spft'/4 to track deductions
Research~wise LogicMOO has a main purpose is to prove that grounded negations (of contrapostives) are of first class in importance in helping
with Wff checking/TMS 
Also alows an inference engine constrain search.. PFC became important since it helps memoize and close off (terminate) transitive closures

*/


%% is_side_effect_disabled is semidet.
%
% If Is A Side Effect Disabled.
%
is_side_effect_disabled:- t_l:no_attempt_side_effects,!.
is_side_effect_disabled:- t_l:side_effect_ok,!,fail.
is_side_effect_disabled:- t_l:noDBaseMODs(_),!.



%% f_to_mfa( +EF, ?R, ?F, ?A) is semidet.
%
% Functor Converted To Module-functor-arity.
%
f_to_mfa(EF,R,F,A):-w_get_fa(EF,F,A),
              (((current_predicate(F/A),functor(P,F,A),predicate_property(_M:P,imported_from(R)))*->true;
              current_predicate(F/A),functor(P,F,A),source_file(R:P,_SF))),
              current_predicate(R:F/A).


%% w_get_fa( +PI, ?F, ?A) is semidet.
%
% W Get Functor-arity.
%
w_get_fa(PI,_F,_A):-is_ftVar(PI),!.
w_get_fa(F/A,F,A):- !.
w_get_fa(PI,PI,_A):- atomic(PI),!.
w_get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
w_get_fa(Mask,F,A):-get_functor(Mask,F,A).



:- multifile(baseKB:mpred_hook_rescan_files/0).
:- dynamic(baseKB:mpred_hook_rescan_files/0).
:- use_module(library(logicmoo_common)).
%:- was_dynamic(use_presently/0).
% used to annotate a predciate to indicate PFC support


%% is_mpred_action( :TermP) is semidet.
%
% If Is A Managed Predicate Action.
%
is_mpred_action('$VAR'(_)):-!,fail.
is_mpred_action(remove_if_unsupported(_,_)).
is_mpred_action(P):-is_static_predicate(P).

%% mpred_is_builtin( +P) is semidet.
%
% PFC If Is A Builtin.
%
mpred_is_builtin(P):- predicate_property(P,built_in), \+ predicate_property(P,dynamic).
mpred_is_builtin(P):- callable(P),functor(P,F,_),clause_b(prologBuiltin(F)).
mpred_is_builtin(F):- current_predicate(F/A),A>0,functor(P,F,A),mpred_is_builtin(P).

/* UNUSED TODAY

:- use_module(library(mavis)).
:- use_module(library(type_check)).
:- use_module(library(typedef)).
*/



:- thread_local((t_l:use_side_effect_buffer/0 , t_l:verify_side_effect_buffer/0)).

%% record_se is semidet.
%
% Record Se.
%
record_se:- (t_l:use_side_effect_buffer ; t_l:verify_side_effect_buffer).



%% add_side_effect( +Op, ?Data) is semidet.
%
% Add Side Effect.
%
add_side_effect(_,_):- ( \+  record_se ),!.
add_side_effect(Op,Data0):- current_why(Why),serialize_attvars(Data0,Data),assert(t_l:side_effect_buffer(Op,Data,Why)).


%% attvar_op( +:PRED1, ?Data) is semidet.
%
% Attribute Variable Oper..
%


listing_s(P):-call_s(xlisting(P)).

assert_s(H):- assertz_s(H).
retractall_s(H):- forall(clause_s(H,_,R),erase(R)).
clause_s(H,B):- clause_s(H,B,_).

retract_s(H):- lookup_s(H,R),erase(R).

lookup_s(H):- lookup_s(H,_). 

lookup_s(M:(H:-B),R):- !,clause_s(M:H,B,R).
lookup_s((H:-B),R):-  !,clause_s(H,B,R).
lookup_s(H,R):- clause_s(H,true,R).

lookq_s(X):-lookq_s(X,_Ref).

lookq_s(M:(H:-B),R):- !,clauseq_s(M:H,B,R).
lookq_s((H:-B),R):- !, clauseq_s(H,B,R).
lookq_s(H,R):- clauseq_s(H,true,R).

asserta_s(H):- fix_mp(clause(assert,asserta_s),H,M,H0),asserta_i(M:H0).
assertz_s(H):- fix_mp(clause(assert,assertz_s),H,M,H0),assertz_i(M:H0).
clause_s(H,B,R):- fix_mp(clause(clause,clause_s),H,M,H0),clause_u(M:H0,B,R).
clauseq_s(H,B,R):- fix_mp(clause(clause,clauseq_s),H,M,H0),clause_u(M:H0,B,R),clause(M:HC,BC,R),H0=@=HC,BC=@=B.

call_s(G0):-
  strip_module(G0,_,G),functor(G,F,A),
  (memberchk(F/A,[(',')/2])->
  mpred_METACALL(call_s,G);
  call_s2(G0)).

call_s2(G0):-
  strip_module(G0,WM,G),
  defaultAssertMt(U),  
  must(current_predicate(_,U:G)->(CALL=U:G);(current_predicate(_,WM:G0)->CALL=WM:G0; fail)),
  call(call,(
 '$set_source_module'(S,U),'$module'(M,U),
  setup_call_cleanup( % _each
    ('$set_source_module'(U),'$set_typein_module'(U)),
       call(CALL),
     ('$set_source_module'(S),'$set_typein_module'(M))))).


:- module_transparent(attvar_op/2).

% % attvar_op(Op,Data):- deserialize_attvars(Data,Data0), attvar_op(Op,Data0).
attvar_op(Op,MData):-
 must_det_l((
   strip_module(Op,_,OpA), sanity( \+ atom(OpA)),
   fix_mp(clause(assert,OpA),MData,M,Data),
   add_side_effect(OpA,M:Data),
   quietly(current_prolog_flag(assert_attvars,true)->deserialize_attvars(Data,Data0);Data=Data0))),!,
   attempt_side_effect_mpa(M,OpA,Data0).


:- thread_local(t_l:no_attempt_side_effects/0).

%% attempt_side_effect( +PSE) is semidet.
%
% Physical Side Effect.
%
attempt_side_effect(PSE):- to_physical_mpa(PSE,M,P,A),!,attempt_side_effect_mpa(M,P,A).

to_physical_mpa(PSE,M,P,A):- strip_module(PSE,M,PA),to_physical_pa(PA,P,A).
to_physical_pa(PA,P,A):-PA=..[P,A],!. to_physical_pa(PA,call,PA).


:- meta_predicate(db_op_call(*,1,?)).
db_op_call(_What,How,Data):- call(How,Data).

% attempt_side_effect_mpa(M,OpA,Data):- record_se,!,add_side_effect(OpA,M:Data).
attempt_side_effect_mpa(M,db_op_call(_,retract_u0),Data0):- \+ lookup_u(M:Data0),!,fail.
attempt_side_effect_mpa(M,OpA,Data0):- \+ record_se, is_side_effect_disabled,!,mpred_warn('no_attempt_side_effects ~p',attempt_side_effect_mpa(M,OpA,Data0)).
% @TODO BROKEN phys ical_side_effect_call(M,assertz_i,Data0):- must((compile_aux_clauses(M:Data0))),!.
attempt_side_effect_mpa(M,OpA,Data0):- show_failure(M:call(M:OpA,M:Data0)).


/*

  b_setval(th_asserts,[]),
  call_u(G),
  b_getval(th_asserts,List).

attempt_side_effect_mpa(C) :- 
   b_getval(th_asserts,List),
   b_setval(th_asserts,[C|List]),!.



*/
%% erase_w_attvars( +Data0, ?Ref) is semidet.
%
% Erase W Attribute Variables.
%
erase_w_attvars(Data0,Ref):- attempt_side_effect(erase(Ref)),add_side_effect(erase,Data0).


%% mpred_nochaining( +Goal) is semidet.
%
% PFC No Chaining.
%
mpred_nochaining(Goal):- locally_tl(no_attempt_side_effects,call(Goal)).


%% with_chaining( +Goal) is semidet.
%
% PFC No Chaining.
%
with_chaining(Goal):- locally(- t_l:no_attempt_side_effects,call(Goal)).

% TODO ISSUE https://github.com/logicmoo/PrologMUD/issues/7


%% match_source_ref1( :TermARG1) is semidet.
%
% Match Source Ref Secondary Helper.
%
match_source_ref1(ax):-!.
match_source_ref1(mfl4(_VarNameZ,_,_,_)).

%% make_uu_remove( :TermU) is semidet.
%
% Make Uu Remove.
%
make_uu_remove((_,ax)).


%% has_functor( :TermC) is semidet.
%
% Has Functor.
%

% -- % has_functor(_):-!,fail.
has_functor(F/A):-!,is_ftNameArity(F,A),!.
has_functor(C):- (\+ is_ftCompound(C)),!,fail.
has_functor(C):- is_ftCompound(C),\+is_list(C).


%% mpred_each_literal( +P, ?E) is semidet.
%
% PFC Each Literal.
%
mpred_each_literal(P,E):-is_ftNonvar(P),P=(P1,P2),!,(mpred_each_literal(P1,E);mpred_each_literal(P2,E)).
mpred_each_literal(P,P). %:-conjuncts_to_list(P,List),member(E,List).



%% retract_eq_quitely( +H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely.
%
retract_eq_quitely(H):- call_u(retract_eq_quitely_f(H)).

%% retract_eq_quitely_f( +H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely False.
%
retract_eq_quitely_f((H:-B)):- !,clause_asserted_i(H,B,Ref),erase(Ref).
retract_eq_quitely_f(pfclog(H)):- retract_eq_quitely_f(H),fail.
retract_eq_quitely_f((H)):- clause_asserted_i(H,true,Ref),erase(Ref).


%% assert_eq_quitely( +H) is semidet.
%
% Assert Using (==/2) (or =@=/2) ) Quitely.
%
assert_eq_quitely(H):- attvar_op(db_op_call(assert,assert_if_new),H).


%% mpred_is_tautology( +Var) is semidet.
%
% PFC If Is A Tautology.
%
% :- module_transparent( (mpred_is_tautology)/1).
mpred_is_tautology(V):- (is_ftVar(V) -> true;(copy_term_nat(V,VC),numbervars(VC),mpred_is_taut(VC))),!.



%% mpred_is_taut( :TermA) is semidet.
%
% PFC If Is A Taut.
%
mpred_is_taut(A):-var(A),!.
mpred_is_taut(A:-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<==>B):-!,(mpred_is_taut(A==>B);mpred_is_taut(B==>A)).
mpred_is_taut(A==>B):- A==B,!.
mpred_is_taut((B,_)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut((_,B)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(A,_)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(_,A)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.


% baseKB:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_storage_op(Op,Hook)).


%% is_retract_first( +VALUE1) is semidet.
%
% If Is A Retract First.
%
is_retract_first(one).
is_retract_first(a).


%% pfc_provide_storage_op( +Op, ?I1) is semidet.
%
% Prolog Forward Chaining Provide Storage Oper..
%
pfc_provide_storage_op(Op,(I1,I2)):-!,pfc_provide_storage_op(Op,I1),pfc_provide_storage_op(Op,I2).
pfc_provide_storage_op(Op,(nesc(P))):-!,pfc_provide_storage_op(Op,P).
%pfc_provide_storage_op(change(assert,_AorZ),Fact):- loop_check_nr(ainPreTermExpansion(Fact)).
% pfcRem1 to just get the first
pfc_provide_storage_op(change(retract,OneOrA),FactOrRule):- is_retract_first(OneOrA),!,
            loop_check_nr(mpred_withdraw(FactOrRule)),
  ignore((ground(FactOrRule),mpred_remove(FactOrRule))).
% mpred_remove should be forcefull enough
pfc_provide_storage_op(change(retract,all),FactOrRule):- loop_check_nr(mpred_remove(FactOrRule)),!.
% pfc_provide_storage_op(clause_u,FactOrRule):- is_ftNonvar(FactOrRule),!,loop_check_nr(clause_u(FactOrRule)).


% pfcDatabaseGoal(G):-is_ftCompound(G),get_functor(G,F,A),pfcDatabaseTerm(F/A).




%% mpred_pbody( +H, ?B, ?R, ?BIn, ?WHY) is semidet.
%
% PFC Pbody.
%
%mpred_pbody(H,B,_R,fail,deduced(backchains)):- get_bc_clause(H,_H,B),!.
%mpred_pbody(H,infoF(INFO),R,B,Why):-!,mpred_pbody_f(H,INFO,R,B,Why).
%mpred_pbody(H,B,R,BIn,WHY):- is_src_true(B),!,BIn=B,get_why(H,H,R,WHY).
%mpred_pbody(H,B,R,B,asserted(R,(H:-B))).


%% get_why( +VALUE1, ?CL, ?R, :TermR) is semidet.
%
% Get Generation Of Proof.
%
get_why(_,CL,R,asserted(R,CL:-U)):- get_mz(MZ), clause_u('$spft'(MZ,CL, U, ax),true),!.
get_why(H,CL,R,deduced(R,WHY)):- (mpred_get_support(H,WH)*->WHY=(H=WH);(mpred_get_support(CL,WH),WHY=(CL=WH))).



%% mpred_pbody_f( +H, ?CL, ?R, ?B, ?WHY) is semidet.
%
% PFC Pbody False.
%
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<-B),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<==>B),sub_term_eq(H,HH),get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B<==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,fail,infoF(CL)):- trace_or_throw_ex(mpred_pbody_f(H,CL,R)).


%% sub_term_eq( +H, ?HH) is semidet.
%
% Sub Term Using (==/2) (or =@=/2) ).
%
sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.


%% sub_term_v( +H, ?HH) is semidet.
%
% Sub Term V.
%
sub_term_v(H,HH):-H=@=HH,!.
sub_term_v(H,HH):-each_subterm(HH,ST),ST=@=H,!.

%% all_different_head_vals(+Clause) is det.
%
% Enforces All Different Head Vals.
%
all_different_head_vals(HB):- (\+ compound(HB) ; ground(HB)),!.
all_different_head_vals(HB):- 
  mpred_rule_hb(HB,H,B),
  term_slots(H,Slots),  
  (Slots==[]->
     all_different_head_vals(B);
    (lock_vars(Slots),all_different_head_vals_2(H,Slots),unlock_vars(Slots))),!.
  

all_different_head_vals_2(_H,[]):-!.
all_different_head_vals_2(H,[A,R|EST]):-get_assertion_head_arg(_,H,E1),E1 ==A,dif(A,E2),get_assertion_head_arg(_,H,E2),\+ contains_var(A,E2),all_different_vals(dif_matrix,[A,E2,R|EST]),!.
all_different_head_vals_2(_H,[A,B|C]):-all_different_vals(dif_matrix,[A,B|C]),!.
all_different_head_vals_2(HB,_):- \+ compound(HB),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1 ==A, H=..[_|ARGS], all_different_vals(dif_matrix,ARGS),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1 ==A,  get_assertion_head_arg(_,H,E2), A\==E2, \+ contains_var(A,E2), dif(A,E2),!.
all_different_head_vals_2(H,[A]):-get_assertion_head_arg(_,H,E1),E1\==A, compound(E1), contains_var(A,E1), all_different_head_vals_2(E1,[A]),!.
all_different_head_vals_2(_,_).
   	 

%% mpred_rule_hb( +Outcome, ?OutcomeO, ?AnteO) is semidet.
%
% Calculate PFC Rule Head+body.
%
mpred_rule_hb(Outcome,OutcomeO,Body):- nonvar(OutcomeO),!,mpred_rule_hb(Outcome,OutcomeN,Body),must(OutcomeO=OutcomeN).
mpred_rule_hb(Outcome,OutcomeO,BodyO):- nonvar(BodyO),!,mpred_rule_hb(Outcome,OutcomeO,BodyN),must(BodyN=BodyO).
mpred_rule_hb(Outcome,OutcomeO,AnteO):- 
  quietly((mpred_rule_hb_0(Outcome,OutcomeO,Ante),
  mpred_rule_hb_0(Ante,AnteO,_))).
% :-mpred_trace_nochilds(mpred_rule_hb/3).


%% mpred_rule_hb_0( +Rule, -Head, -Body) is nondet.
%
% Calculate PFC rule Head+Body  Primary Helper.
%


mpred_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0(Outcome,OutcomeO,true):- \+compound(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):- mpred_rule_hb(Outcome1,Outcome1O,Ante1),mpred_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
mpred_rule_hb_0((Ante1==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1=>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1*->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0((Outcome/Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(rhs([Outcome]),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(rhs([OutcomeH|OutcomeT]),OutcomeO,Ante2):- !, mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0({Outcome},OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<-Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 & Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 , Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<==>Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1<==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(_::::Outcome,OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb_0(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0('$bt'(Outcome,Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0('$pt'(_MZ,Ante1,Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(pk(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0('$nt'(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0('$spft'(_MZ,Outcome,Ante1a,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(que(Outcome,_),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome:-Ante),Outcome,Ante):-(nonvar(Outcome)-> ! ; true).
mpred_rule_hb_0(Outcome,Outcome,true).


%% ain_minfo( +G) is semidet.
%
% Assert If New Metainformation.
%
:- module_transparent(ain_minfo/1).
ain_minfo(G):-ain_minfo(assertz_if_new,G).

%% ain_minfo( :PRED1How, ?H) is semidet.
%
% Assert If New Metainformation.
%
:- module_transparent(ain_minfo/2).
ain_minfo(How,(H:-True)):-is_src_true(True),must(is_ftNonvar(H)),!,ain_minfo(How,H).
ain_minfo(How,(H<-B)):- !,ain_minfo(How,(H:-infoF(H<-B))),!,get_bc_clause(H,Post),ain_minfo(How,Post),ain_minfo_2(How,(B:-infoF(H<-B))).
ain_minfo(How,(B==>H)):- !,ain_minfo(How,(H:-infoF(B==>H))),!,ain_minfo_2(How,(B:-infoF(B==>H))).
ain_minfo(How,(B<==>H)):- !,ain_minfo(How,(H:-infoF(B<==>H))),!,ain_minfo(How,(B:-infoF(B<==>H))),!.
ain_minfo(How,((A,B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,((A;B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,(-(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(~(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(A:-INFOC)):- is_ftNonvar(INFOC), get_bc_clause(A,AA,INFOCC),A=AA,INFOC==INFOCC,!,attvar_op(How,(A:-INFOC)),!.
ain_minfo(How,'$bt'(H,_)):-!,get_bc_clause(H,Post),attvar_op(How,Post).
ain_minfo(How,'$nt'(H,Test,Body)):-!,attvar_op(How,(H:-fail,'$nt'(H,Test,Body))).
ain_minfo(How,'$pt'(MZ,H,Body)):-!,attvar_op(How,(H:-fail,'$pt'(MZ,H,Body))).
ain_minfo(How,(A0:-INFOC0)):- mpred_is_info(INFOC0), copy_term_and_varnames((A0:-INFOC0),(A:-INFOC)),!,must((mpred_rewrap_h(A,AA),imploded_copyvars((AA:-INFOC),ALLINFO), attvar_op(How,(ALLINFO)))),!.
%ain_minfo(How,G):-mpred_trace_msg(skipped_add_meta_facts(How,G)).
ain_minfo(_,_).

:- was_export(ain_minfo_2/2).

%% ain_minfo_2( :PRED1How, ?G) is semidet.
%
% Assert If New Metainformation  Extended Helper.
%
:- module_transparent(ain_minfo_2/2).
ain_minfo_2(How,G):-ain_minfo(How,G).


%% mpred_is_info( :TermC) is semidet.
%
% PFC If Is A Info.
%
mpred_is_info((CWC,Info)):- (atom(CWC),is_a_info(CWC));mpred_is_info(Info).
mpred_is_info(mpred_bc_only(C)):-is_ftNonvar(C),!.
mpred_is_info(infoF(C)):-is_ftNonvar(C),!.
mpred_is_info(inherit_above(_,_)).


is_a_info(fail).
is_a_info(CWC):- is_pfc_chained(CWC).

is_pfc_chained(cwc).
is_pfc_chained(awc).
is_pfc_chained(zwc).
is_pfc_chained(fwc).
is_pfc_chained(bwc).
is_pfc_chained(wac).



:- module_transparent(is_ain_clause/2).
is_ain_clause( _, Var):- var(Var),!, fail.
is_ain_clause( M,(:- Body)):- !, is_ain_body(M,Body),!.
is_ain_clause( M,(P:- Body)):- !,(is_ain_head(M,P);is_ain_body(M,Body)),!.
is_ain_clause( M,(P)):- !, is_ain_head(M, P).

:- module_transparent(is_ain_head/2).
is_ain_head(_, P):- var(P),!.
is_ain_head(_,(_,_)):- !.
is_ain_head(_,(_;_)):- !.
is_ain_head(_,not(_)):- !.
is_ain_head(_,\+(_)):- !.
is_ain_head(M, P):- is_ain_body(M, P),!.
is_ain_head(_,==>(_)):- !.
is_ain_head(_,==>(_,_)):- !.
is_ain_head(_,<==>(_,_)):- !.
is_ain_head(_,<==(_)):- !.
is_ain_head(_,<==(_,_)):- !.
is_ain_head(_,'::::'(_,_)):- !.
is_ain_head(baseKB,_).
is_ain_head(_,=>(_)):- !.
is_ain_head(_,=>(_,_)):- !.
is_ain_head(_,_):- get_how_virtualize_file(Lang),!,Lang=heads.

:- module_transparent(is_ain_body/2).
is_ain_body(_, P):- var(P),!,fail.
is_ain_body(M, (P,_)):- !, nonvar(P), is_ain_body(M, P).
is_ain_body(_, CWC):- atom(CWC),  is_pfc_chained(CWC).
is_ain_body(M, P):- functor(P,F,A), \+ \+ mpred_prop(M,F,A,_), !,
  \+ (mpred_prop(M,F,A,Prop), is_pfc_prolog_only_prop(Prop)).
is_ain_body(M, MP):- strip_module(MP,M2,P), M2\==M, !,is_ain_body(M2,P).

is_pfc_prolog_only_prop(prologOnly).
is_pfc_prolog_only_prop(prologBuiltin).


%cwc(Call):- callable(Call),Call.

%:- was_dynamic(not_not/1).

%% mpred_rewrap_h( +A, ?A) is semidet.
%
% PFC Rewrap Head.
%
mpred_rewrap_h(A,A):- is_ftNonvar(A),\+ is_static_predicate(A).
mpred_rewrap_h(A,F):- functor(A,F,_),\+ is_static_predicate(F),!.
%mpred_rewrap_h(A,not_not(A)):-!.


%% cwc is det.
%
% Cwc.
%
cwc:-true.

%% fwc is det.
%
% Fwc.
%
fwc:-true.

%% bwc is semidet.
%
% Bwc.
%
bwc:-true.

%% wac is semidet.
%
% Wac.
%
wac:-true.

awc:-true.
zwc:-true.


%% is_fc_body( +P) is semidet.
%
% If Is A Forward Chaining Body.
%
is_fc_body(P):- has_body_atom(fwc,P).

%% is_bc_body( +P) is semidet.
%
% If Is A Backchaining Body.
%
is_bc_body(P):- has_body_atom(bwc,P).

%% is_action_body( +P) is semidet.
%
% If Is A Action Body.
%
is_action_body(P):- has_body_atom(wac,P).



%% has_body_atom( +WAC, ?P) is semidet.
%
% Has Body Atom.
%
has_body_atom(WAC,P):- call(
   WAC==P -> true ; (is_ftCompound(P),get_assertion_head_arg(1,P,E),has_body_atom(WAC,E))),!.

/*
has_body_atom(WAC,P,Rest):- call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(1,P,E),E==WAC,get_assertion_head_arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(2,P,E),E==WAC,get_assertion_head_arg(1,P,Rest),!.
*/

:- module_transparent( (get_assertion_head_arg)/3).
get_assertion_head_arg(N,P,E):-get_assertion_head_unnegated(P,PP),!,arg(N,PP,E).

same_functors(Head1,Head2):-must_det(get_unnegated_functor(Head1,F1,A1)),must_det(get_unnegated_functor(Head2,F2,A2)),!,F1=F2,A1=A2.


%% mpred_update_literal( +P, ?N, ?Q, ?R) is semidet.
%
% PFC Update Literal.
%
mpred_update_literal(P,N,Q,R):-
    get_assertion_head_arg(N,P,UPDATE),call(replace_arg(P,N,Q_SLOT,Q)),
    must(call_u(Q)),update_value(Q_SLOT,UPDATE,NEW), 
    replace_arg(Q,N,NEW,R).


% '$spft'(MZ,5,5,5).

%% update_single_valued_arg(+Module, +P, ?N) is semidet. 
%
% Update Single Valued Argument.
%
:- module_transparent( (update_single_valued_arg)/3).

update_single_valued_arg(M,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).
update_single_valued_arg(_,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).

update_single_valued_arg(world,P,N):- !, update_single_valued_arg(baseKB,P,N).
update_single_valued_arg(M,P,N):- break, \+ clause_b(mtHybrid(M)), trace, clause_b(mtHybrid(M2)),!,
   update_single_valued_arg(M2,P,N).

update_single_valued_arg(M,P,N):- 
  get_assertion_head_arg(N,P,UPDATE),
  is_relative(UPDATE),!,
  dtrace,
  break,
  replace_arg(P,N,OLD,Q),
  must_det_l((clause_u(Q),update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R))),!,
  update_single_valued_arg(M,R,N).


update_single_valued_arg(M,P,N):- 
 call_u((must_det_l((

  call_u(mtHybrid(M)),
  mpred_type_args \= M,
  mpred_kb_ops \= M,
  get_assertion_head_arg(N,P,UPDATE),
  replace_arg(P,N,Q_SLOT,Q),
  var(Q_SLOT),
  same_functors(P,Q),
  % current_why(U),
  must_det_l((
     % rtrace(attvar_op(assert_if_new,M:'$spft'(MZ,P,U,ax))),
     % (call_u(P)->true;(assertz_mu(P))),
     assertz_mu(M,P),
     doall((
          lookup_u(M:Q,E),
          UPDATE \== Q_SLOT,
          erase(E),
          mpred_unfwc1(M:Q))))))))).

% ======================= 
% utils
% ======================= 

%% map_literals( +P, ?G) is semidet.
%
% Map Literals.
%
map_literals(P,G):-map_literals(P,G,[]).


%% map_literals( +VALUE1, :TermH, ?VALUE3) is semidet.
%
% Map Literals.
%
map_literals(_,H,_):-is_ftVar(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- mpred_literal(H),must(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.



%% map_unless( :PRED1Test, ?Pred, ?H, ?S) is semidet.
%
% Map Unless.
%
map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.


:- meta_predicate(map_first_arg(*,+)).
%% map_first_arg( +Pred, ?List) is semidet.
%
% PFC Maptree.
%
map_first_arg(CMPred,List):- strip_module(CMPred,CM,Pred), map_first_arg(CM,Pred,List,[]).

:- meta_predicate(map_first_arg(+,*,+,+)).
%% map_first_arg( +Pred, :TermH, ?S) is semidet.
%
% PFC Maptree.
%
map_first_arg(CM,Pred,H,S):-is_ftVar(H),!,CM:apply(Pred,[H|S]).
map_first_arg(_,_,[],_) :- !.
map_first_arg(CM,Pred,(H,T),S):-!, map_first_arg(CM,Pred,H,S), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,(H;T),S):-!, map_first_arg(CM,Pred,H,S) ; map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,[H|T],S):-!, CM:apply(Pred,[H|S]), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,H,S):- CM:apply(Pred,[H|S]). 

%:- fixup_exports.

% % :- ensure_loaded(logicmoo(util/rec_lambda)).

%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});-mpred_isa(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))). 


%% pfcVerifyMissing( +GC, ?GO, ?GO) is semidet.
%
% Prolog Forward Chaining Verify Missing.
%
pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example mpred_freeLastArg(mpred_isa(I,C),~(mpred_isa(I,C))):-is_ftNonvar(C),!.
%example mpred_freeLastArg(mpred_isa(I,C),(mpred_isa(I,F),C\=F)):-!.

%% mpred_freeLastArg( +G, ?GG) is semidet.
%
% PFC Free Last Argument.
%
mpred_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
mpred_freeLastArg(_G,false).


%% mpred_current_op_support( +VALUE1) is semidet.
%
% PFC Current Oper. Support.
%
mpred_current_op_support((p,p)):-!.


%% pfcVersion( +VALUE1) is semidet.
%
% Prolog Forward Chaining Version.
%
pfcVersion(6.6).


% % :- '$set_source_module'(mpred_kb_ops).

%% correctify_support( +S, ?S) is semidet.
%
% Correctify Support.
%
correctify_support(U,(U,ax)):-var(U),!.
correctify_support((U,U),(U,ax)):-!.
correctify_support((S,T),(S,T)):-!.
correctify_support((U,_UU),(U,ax)):-!.
correctify_support([U],S):-correctify_support(U,S).
correctify_support(U,(U,ax)).


%% clause_asserted_local( :TermABOX) is semidet.
%
% Clause Asserted Local. 
%
clause_asserted_local(MCL):-
  strip_mz(MCL,MZ,CL),
  must(CL='$spft'(MZ,P,Fact,Trigger )),!,
  clause_u('$spft'(MZ,P,Fact,Trigger),true,Ref),
  clause_u('$spft'(MZ,UP,UFact,UTrigger),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).



%% is_already_supported( +P, ?S, ?UU) is semidet.
%
% If Is A Already Supported.
%
is_already_supported(P,(S,T),(S,T)):- clause_asserted_local('$spft'(_MZ,P,S,T)),!.
is_already_supported(P,_S,UU):- clause_asserted_local('$spft'(_MZ,P,US,UT)),must(get_source_uu(UU)),UU=(US,UT).

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term_and_varnames(P,PC),sp ftY(PC,_,_),P=@=PC,!.


if_missing1(Q):- mpred_literal_nv(Q), call_u( \+ ~ Q), if_missing_mask(Q,R,Test),!, lookup_u(R), Test.


%% if_missing_mask( +Q, ?R, ?Test) is semidet.
%
% If Missing Mask.
%

if_missing_mask(M:Q,M:R,M:Test):- nonvar(Q),!,if_missing_mask(Q,R,Test).
if_missing_mask(Q,~Q,\+Q):- \+ is_ftCompound(Q),!.

%if_missing_mask(ISA, ~ ISA, \+ ISA):- functor(ISA,F,1),(F==tSwim;call_u(functorDeclares(F))),!.
if_missing_mask(HB,RO,TestO):- once(mpred_rule_hb(HB,H,B)),B\==true,HB\==H,!,
     if_missing_mask(H,R,TestO),subst(HB,H,R,RO).

if_missing_mask(ISA, ISA, \+ ISA):- functor(ISA, _F,1),!.% (F==tSwim;call_u(functorDeclares(F))),!.

if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_n_mask(Q,N,R,Test),!.

if_missing_mask(ISA, ~ ISA, \+ ISA).

%% if_missing_n_mask( +Q, ?N, ?R, ?Test) is semidet.
%
% If Missing Mask.
%
if_missing_n_mask(Q,N,R,Test):-
  get_assertion_head_arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),get_assertion_head_arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):- 
 must((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) -> 
    (get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/


%% which_missing_argnum( +VALUE1, ?VALUE2) is semidet.
%
% Which Missing Argnum.
%
which_missing_argnum(Q,N):- compound(Q),\+ compound_name_arity(Q,_,0),
 must((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (call_u(singleValuedInArg(F,N)) -> true; which_missing_argnum(Q,F,A,N)).

which_missing_argnum(_,_,1,_):-!,fail.
which_missing_argnum(Q,_F,A,N):- between(A,1,N),get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was).

mpred_run_pause:- asserta(t_l:mpred_run_paused).
mpred_run_resume:- retractall(t_l:mpred_run_paused).

without_running(G):- (t_l:mpred_run_paused->G;locally_tl(mpred_run_pause,G)).

%mpred_remove_file_support(_File):- !.
mpred_remove_file_support(File):- 
 get_mz(MZ),
  forall((must((filematch(File,File0),atom(File),freeze(Match,contains_var(File0,Match))))),
      forall(lookup_u('$spft'(MZ, W, Match, AX),Ref),
         (wdmsg(removing('$spft'(MZ, W, Match, AX))), erase(Ref),remove_if_unsupported(W)))).



%% mpred_get_support_precanonical( +F, ?Sup) is semidet.
%
% PFC Get Support Precanonical.
%
mpred_get_support_precanonical(F,Sup):-fully_expand(mpred_get_support_precanonical,F,P),mpred_get_support(P,Sup).

%% spft_precanonical( +F, ?SF, ?ST) is semidet.
%
% Spft Precanonical.
%

spft_precanonical(F,SF,ST):- fully_expand(spft_precanonical,F,P),!,mpred_get_support(P,(SF,ST)).
                                                      

%% trigger_supporters_list( +U, :TermARG2) is semidet.
%
% Trigger Supports Functor (list Version).
%
trigger_supporters_list(U,[]) :- match_source_ref1(U),!.
trigger_supporters_list(U,[]) :- atom(U),!.

trigger_supporters_list(Trigger,[Fact|MoreFacts]) :-
  mpred_get_support_precanonical_plus_more(Trigger,(Fact,AnotherTrigger)),
  must(trigger_supporters_list(AnotherTrigger,MoreFacts)).

mpred_retry(G):- fail; quietly(G).


%% { ?G} is semidet.
%
% an escape construct for bypassing the FOL''s salient body goals. 
% 
%
:- meta_predicate('{}'(*)).
:- module_transparent( ({})/1).
'{}'(G):- call_u(G).
:- sexport(({})/1).

%% neg_in_code( +G) is semidet.
%
% Negated In Code.
%
:- meta_predicate neg_in_code(*).
:- export(neg_in_code/1).
neg_in_code(G):- nr_lc_ex((neg_in_code0(G))).
                                                   
% :- kb_shared(baseKB:proven_neg/1).

:- meta_predicate neg_in_code0(*).
:- export(neg_in_code0/1).
/*
neg_in_code0(G):- cwc, nr_lc_ex(proven_neg(G)).
neg_in_code0(G):- cwc, var(G),!,nr_lc_ex(lookup_u(~ G)).
neg_in_code0(call_u(G)):- !,neg_in_code0(G).
neg_in_code0(~(G)):- nonvar(G),!,  \+ nr_lc_ex(~G) ,!.
neg_in_code0(G):-  is_ftNonvar(G), a(prologSingleValued,G),
      must((if_missing_mask(G,R,Test),nonvar(R),nonvar(Test))),call_u(R),!,call_u(Test).
neg_in_code0(G):- cwc, clause(~G,Call)*-> call_u(Call).
*/
neg_in_code0(G):- nr_lc_ex(neg_may_naf(G)), \+ nr_lc_ex(G),!.
% neg_in_code0(_:G):-!,baseKB:neg_in_code0(G).


:- meta_predicate neg_may_naf(*).
:- module_transparent(neg_may_naf/1).
:- export(neg_may_naf/1).

%% neg_may_naf( :GoalP) is semidet.
%
% Negated May Negation-by-faliure.
%
neg_may_naf(P):- mpred_non_neg_literal(P),get_functor(P,F),clause_u(prologNegByFailure(F),true),!.
neg_may_naf(P):- is_ftCompound(P),is_never_pfc(P).
                                          

%% call_u_req( +G) is semidet.
%
% Req.
%
call_u_req(G):- nr_lc_ex(call_u(G)).


%% mpred_call_only_facts(:Fact) is nondet.
%% mpred_call_only_facts(+Why,:Fact) is nondet.
%
% PFC Call Only Facts.
%
% is true iff Fact is a fact available for forward chaining.
%
% Note that this has the side effect [maybe] of catching unsupported facts and
% assigning them support from God. (g,ax)
%
mpred_call_only_facts(_Why,Clause):- mpred_call_only_facts(Clause).
mpred_call_only_facts(Clause) :-  
   strip_module(Clause,_,H), 
   on_x_debug(nr_lc_ex(locally_tl(infAssertedOnly(H),call_u(H)))). 



% TODO: test removal
%mpred_call_0(prologHybrid(H)):-get_functor(H,F),!,isa_asserted(F,prologHybrid).



%% call_with_bc_triggers( +MP) is semidet.
%
% Call Using Backchaining Triggers.
%
call_with_bc_triggers(MP) :- strip_module(MP,_,P), functor(P,F,A), \+ t_l:infBackChainPrevented(F/A), 
  lookup_u('$bt'(P,Trigger)),
  no_repeats(mpred_get_support('$bt'(P,Trigger),S)),
  once(no_side_effects(P)),
  locally_tl(infBackChainPrevented(F/A),mpred_eval_lhs(Trigger,S)).


:- thread_local t_l:infBackChainPrevented/1.

%% mpred_call_with_no_triggers( +Clause) is semidet.
%
% PFC Call Using No Triggers.
%
mpred_call_with_no_triggers(Clause) :-  strip_module(Clause,_,F),
  % = this (is_ftVar(F)) is probably not advisable due to extreme inefficiency.
  (is_ftVar(F)    ->  mpred_facts_and_universe(F) ;
     mpred_call_with_no_triggers_bound(F)).


%% mpred_call_with_no_triggers_bound( +F) is semidet.
%
% PFC Call Using No Triggers Bound.
%
mpred_call_with_no_triggers_bound(F):- mpred_call_with_no_triggers_uncaugth(F).

%% mpred_call_with_no_triggers_uncaugth( +Clause) is semidet.
%
% PFC Call Using No Triggers Uncaugth.
%
mpred_call_with_no_triggers_uncaugth(Clause) :-  strip_module(Clause,_,F),
  show_failure(mpred_call_with_no_triggers_bound,no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;call_u(F)).
  %= we check for system predicates as well.
  %has_cl(F) -> (clause_u(F,Condition),(Condition==true->true;call_u(Condition)));
  %call_u(F).


%% mpred_bc_only( +M) is semidet.
%
% PFC Backchaining Only.
%

%mpred_bc_only(G):- !,defaultAssertMt(W), nr_lc_ex(mpred_BC_w_cache(W,G)).
%mpred_bc_only(M:G):- !, nr_lc_ex(with_umt(M,mpred_bc_only0(G))).
mpred_bc_only(G):- nr_lc_ex((mpred_bc_only0(G))).

%% mpred_bc_only0( +G) is semidet.
%
% PFC Backchaining Only Primary Helper.
%
mpred_bc_only0(G):- mpred_unnegate(G,Pos),!, show_call(why,\+ mpred_bc_only(Pos)).
% mpred_bc_only0(G):- pfcBC_NoFacts(G).
mpred_bc_only0(G):- mpred_BC_w_cache(G,G).

% mpred_bc_only0(G):- mpred_call_only_facts(G).


%% mpred_bc_and_with_pfc( +M) is semidet.
%
% PFC Backchaining + FACTS + Inheritance.
%
mpred_bc_and_with_pfc(G):- mpred_bc_and_with_pfc_0(G).

mpred_bc_and_with_pfc_0(G):- loop_check(mpred_call_only_facts(G)). % was missing
mpred_bc_and_with_pfc_0(G):- mpred_bc_only0(G).
mpred_bc_and_with_pfc_0(G):- strip_module(G,M,P),inherit_above(M,P).




% % :- '$set_source_module'(mpred_kb_ops).

%%
%= pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from mpred_bc_only/1
%%

%% pfcBC_NoFacts( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts.
%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (mpred_slow_search,pfcBC_Cache(F)).


%% mpred_slow_search is semidet.
%
% PFC Slow Search.
%
mpred_slow_search.


/*
%% ruleBackward( +R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition):- call_u(( ruleBackward0(R,Condition),functor(Condition,F,_),
  \+ consequent_arg(_,v(call_u_no_bc,call,call_u),F))).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_src_true(Condition);mpred_is_info(Condition)).

%% ruleBackward0( +F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition):- call_u((  '<-'(F,Condition),\+ (is_src_true(Condition);mpred_is_info(Condition)) )).

%{X}:-dmsg_pretty(legacy({X})),call_u(X).
*/


%% pfcBC_NoFacts_TRY( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts Try.
%


pfcBC_NoFacts_TRY(F) :- no_repeats(ruleBackward(F,Condition,Support)),
  % neck(F),
  copy_term((Condition,Support),(CCondition,SupportC)),
  no_repeats(F,call_u(Condition)),  
  maybe_support_bt(F,CCondition,SupportC).

maybe_support_bt(P,_,_):-mpred_ignored_bt(P),!.
maybe_support_bt(F,Condition,Support):-  
  doall((no_repeats(Why,call_u('$bt'(F,'$pt'(_MZ,A,Why)))) *-> mpred_add_support_fast(F,(A,Why)))),
  doall((no_repeats(Why,call_u('$bt'(F,Why))) *-> mpred_add_support_fast(F,('$bt'(F,Why),Support)))),
  ignore((maybeSupport(F,(Condition,Support)))).

:- meta_predicate mpred_why_all(*).
mpred_why_all(Call):- !,
      call_u(Call),
      doall((
        lookup_u(Call),
        ignore(show_failure(mpred_why(Call))),
        dmsg_pretty(result=Call),nl)),
   forall(Call,ignore(show_failure(mpred_why(Call)))).

mpred_why_all(Call):-
      doall((
        call_u(Call),
        ignore(show_failure(mpred_why(Call))),
        dmsg_pretty(result=Call),nl)).



%% pfcBC_Cache( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class Cache.
%
pfcBC_Cache(F) :- mpred_call_only_facts(pfcBC_Cache,F),
   ignore((ground(F),( (\+call_u(F)), maybeSupport(F,(g,ax))))).



%% maybeSupport( +P, ?VALUE2) is semidet.
%
% Maybe Support.
%
maybeSupport(P,_):-mpred_ignored_bt(P),!.
maybeSupport(P,S):- fail, ( \+ ground(P)-> true;
  (predicate_property(P,dynamic)->mpred_post(P,S);true)).

maybeSupport(P,S):- 
   mpred_add_support_fast(P,S),
   maybeMaybeAdd(P,S).
 
maybeMaybeAdd(P,_):- \+ predicate_property(P,dynamic),!.
maybeMaybeAdd(P,_):- \+ \+ clause_u(P,true),!.
maybeMaybeAdd(P,S):- 
 locally_tl(assert_dir(a),
    assert_u_confirmed_was_missing(P)),
   mpred_trace_op(add,P,S),
   mpred_enqueue(P,S).


%% mpred_ignored_bt( :TermC) is semidet.
%
% PFC Ignored.
%
mpred_ignored_bt(argIsa(F, A, argIsaFn(F, A))).
mpred_ignored_bt(genls(A,A)).
mpred_ignored_bt(isa(tCol,tCol)).
%mpred_ignored_bt(isa(W,tCol)):-mreq(baseKB:hasInstance_dyn(tCol,W)).
mpred_ignored_bt(isa(W,_)):-is_ftCompound(W),call_u(isa(W,meta_argtypes)).
mpred_ignored_bt(C):-clause_safe(C,true). 
mpred_ignored_bt(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
mpred_ignored_bt(isa(_,argIsaFn(_, _))).



%% has_cl( +H) is semidet.
%
% Has Clause.
%
has_cl(H):-predicate_property(H,number_of_clauses(_)).

% an action is undoable if there exists a method for undoing it.


%% mpred_negation_w_neg( +P, ?NF) is semidet.
%
% PFC Negation W Negated.
%
mpred_negation_w_neg(~(P),P):-is_ftNonvar(P),!.
mpred_negation_w_neg(P,NF):-mpred_nf1_negation(P,NF).


%% hook_one_minute_timer_tick is semidet.
%
% Hook To [baseKB:hook_one_minute_timer_tick/0] For Module Mpred_pfc.
% Hook One Minute Timer Tick.
%
baseKB:hook_one_minute_timer_tick:-mpred_cleanup.


%% mpred_cleanup is semidet.
%
% PFC Cleanup.
%
mpred_cleanup:- forall((no_repeats(F-A,(call_u(mpred_prop(M,F,A,pfcRHS)),A>1))),mpred_cleanup(M,F,A)).


%% mpred_cleanup(M, +F, ?A) is semidet.
%
% PFC Cleanup.
%
mpred_cleanup(M,F,A):-functor(P,F,A),predicate_property(P,dynamic)->mpred_cleanup_0(M,P);true.


%% mpred_cleanup_0(M, +P) is semidet.
%
% PFC cleanup  Primary Helper.
%
mpred_cleanup_0(M,P):- findall(P-B-Ref,M:clause(P,B,Ref),L),
  M:forall(member(P-B-Ref,L),erase_w_attvars(clause(P,B,Ref),Ref)),forall(member(P-B-Ref,L),M:attvar_op(db_op_call(assertz,assertz_if_new),((P:-B)))).

% :-debug.
%isInstFn(A):-!,trace_or_throw_ex(isInstFn(A)).

%= mpred_unnegate(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

/*
%% mpred_unnegate( +P, ?P) is semidet.
%
% PFC Negation.
%
mpred_unnegate((-P),P).
% mpred_unnegate((~P),P).
mpred_unnegate((\+(P)),P).
*/
/*

%% mpred_negated_literal( +P) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P):-is_reprop(P),!,fail.
mpred_negated_literal(P):-mpred_negated_literal(P,_).

%% mpred_negated_literal( +P, ?Q) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P,Q) :- is_ftNonvar(P),
  mpred_unnegate(P,Q),
  mpred_literal(Q).

*/

%% mpred_is_assertable( +X) is semidet.
%
% PFC If Is A Assertable.
%
mpred_is_assertable(X):- mpred_literal_nv(X),\+ functor(X,{},_).

%% mpred_literal_nv( +X) is semidet.
%
% PFC Literal Nv.
%
mpred_literal_nv(X):-is_ftNonvar(X),mpred_literal(X).

/*
%% mpred_literal( +X) is semidet.
%
% PFC Literal.
%
mpred_literal(X) :- is_reprop(X),!,fail.
mpred_literal(X) :- cyclic_term(X),!,fail.
mpred_literal(X) :- atom(X),!.
mpred_literal(X) :- mpred_negated_literal(X),!.
mpred_literal(X) :- mpred_positive_literal(X),!.
mpred_literal(X) :- is_ftVar(X),!.

*/

%% is_reprop( +X) is semidet.
%
% If Is A Reprop.
%
is_reprop(X):- compound(X),is_reprop_0(X).

%% is_reprop_0( +X) is semidet.
%
% If Is A reprop  Primary Helper.
%
is_reprop_0(~(X)):-!,is_reprop(X).
is_reprop_0(X):-get_functor(X,repropagate,_).


%% mpred_non_neg_literal( +X) is semidet.
%
% PFC Not Negated Literal.
%
mpred_non_neg_literal(X):- is_reprop(X),!,fail.
mpred_non_neg_literal(X):- atom(X),!.
mpred_non_neg_literal(X):- sanity(stack_check),
    mpred_positive_literal(X), X \= ~(_), X \= mpred_prop(_,_,_,_), X \= conflict(_).

% ======================= mpred_file('pfcsupport').	% support maintenance


%% is_relative( :TermV) is semidet.
%
% If Is A Relative.
%
is_relative(V):- (\+is_ftCompound(V)),!,fail.
is_relative(update(_)).
is_relative(replace(_)).
is_relative(rel(_)).
is_relative(+(X)):- \+ is_ftVar(X).
is_relative(-(X)):- \+ is_ftVar(X).
is_relative(*(X)):- \+ is_ftVar(X).

/*
% TODO not called yet
%= mpred_get_trigger_key(+Trigger,-Key)
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

mpred_get_trigger_key('$pt'(MZ,Key,_),Key).
mpred_get_trigger_key(pk(Key,_,_),Key).
mpred_get_trigger_key('$nt'(Key,_,_),Key).
mpred_get_trigger_key(Key,Key).
*/

/*

the FOL i get from SUMO, CycL, UMBEL and many *non* RDF ontologies out there.. i convert to Datalog..  evidently my conversion process is unique as it preserves semantics most by the book conversions gave up on. 


% TODO not called yet
%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger baseable clause that stores the trigger.
%=
mpred_trigger_key(X,X) :- is_ftVar(X), !.
mpred_trigger_key(chart(word(W),_L),W) :- !.
mpred_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
mpred_trigger_key(chart(Concept,_L),Concept) :- !.
mpred_trigger_key(X,X).
*/
% ======================= mpred_file('pfcdb').	% predicates to manipulate database.

%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%	restore, reset, etc).






%% clause_or_call( +H, ?B) is semidet.
%
% Clause Or Call.
%
clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,call_u(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,on_x_log_throw(call_u(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause_u(H,B)*->!;fail) ; clause_u(H,B)).
clause_or_call(H,true):- call_u(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).


% as opposed to simply using clause(H,true).

%% should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

%% should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.



%% no_side_effects( +P) is semidet.
%
% No Side Effects.
%
no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).


:- was_dynamic(functorIsMacro/1).


%% compute_resolve( +NewerP, ?OlderQ, ?SU, ?SU, ?OlderQ) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,SU,SU,(mpred_blast(OlderQ),mpred_ain(NewerP,S),mpred_withdraw(conflict(NewerP)))):-
  must(correctify_support(SU,S)),
  wdmsg_pfc(compute_resolve(newer(NewerP-S)>older(OlderQ-S))).
compute_resolve(NewerP,OlderQ,S1,[U],Resolve):-compute_resolve(OlderQ,NewerP,[U2],S1,Resolve),match_source_ref1(U),match_source_ref1(U2),!.
compute_resolve(NewerP,OlderQ,SU,S2,(mpred_blast(OlderQ),mpred_ain(NewerP,S1),mpred_withdraw(conflict(NewerP)))):-
  must(correctify_support(SU,S1)),
  wdmsg_pfc(compute_resolve((NewerP-S1)>(OlderQ-S2))).



%% compute_resolve( +NewerP, ?OlderQ, ?Resolve) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,Resolve):-
   supporters_list_how(NewerP,S1),
   supporters_list_how(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).



%% is_resolved( +C) is semidet.
%
% If Is A Resolved.
%
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,C),\+mpred_call_only_facts(Why,~(C)).
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,~(C)),\+mpred_call_only_facts(Why,C).

:- must(nop(_)).


%% mpred_prove_neg( +G) is semidet.
%
% PFC Prove Negated.
%
mpred_prove_neg(G):-  (dtrace), \+ mpred_bc_only(G), \+ mpred_fact(G).


%% pred_head( :PRED1Type, ?P) is semidet.
%
% Predicate Head.
%
pred_head(Type,P):- no_repeats(P,(call(Type,P),\+ nonfact_metawrapper(P),is_ftCompound(P))).


%% pred_head_all( +P) is semidet.
%
% Predicate Head All.
%
pred_head_all(P):- pred_head(pred_all,P).


%% nonfact_metawrapper( :TermP) is semidet.
%
% Nonfact Metawrapper.
%
nonfact_metawrapper(~(_)).
nonfact_metawrapper('$pt'(_,_,_)).
nonfact_metawrapper('$bt'(_,_)).
nonfact_metawrapper('$nt'(_,_,_)).
nonfact_metawrapper('$spft'(_,_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(M:P):-atom(M),!,nonfact_metawrapper(P).
nonfact_metawrapper(P):- get_functor(P,F,_), 
   (a(prologSideEffects,F);a(rtNotForUnboundPredicates,F)).
nonfact_metawrapper(P):-rewritten_metawrapper(P).


%% rewritten_metawrapper( +C) is semidet.
%
% Rewritten Metawrapper.
%
rewritten_metawrapper(_):-!,fail.
%rewritten_metawrapper(isa(_,_)).
rewritten_metawrapper(C):-is_ftCompound(C),functor(C,t,_).


%% meta_wrapper_rule( :TermARG1) is semidet.
%
% Meta Wrapper Rule.
%
meta_wrapper_rule((_<-_)).
meta_wrapper_rule((_<==>_)).
meta_wrapper_rule((_==>_)).
meta_wrapper_rule((_:-_)).



%% pred_all( +P) is semidet.
%
% Predicate All.
%
pred_all(P):-pred_u0(P).
pred_all(P):-pred_t0(P).
pred_all(P):-pred_r0(P).


%% pred_u0( +P) is semidet.
%
% Predicate For User Code Primary Helper.
%
pred_u0(P):-pred_u1(P),has_db_clauses(P).
pred_u0(P):-pred_u2(P).

%% pred_u1( +VALUE1) is semidet.
%
% Predicate For User Code Secondary Helper.
%
pred_u1(P):-a(pfcControlled,F),arity_no_bc(F,A),functor(P,F,A).
pred_u1(P):-a(prologHybrid,F),arity_no_bc(F,A),functor(P,F,A).
pred_u1(P):-a(prologDynamic,F),arity_no_bc(F,A),functor(P,F,A).

%% pred_u2( +P) is semidet.
%
% Predicate For User Code Extended Helper.
%
pred_u2(P):- compound(P),functor(P,F,A),sanity(no_repeats(arity_no_bc(F,A))),!,has_db_clauses(P).
pred_u2(P):- no_repeats(arity_no_bc(F,A)),functor(P,F,A),has_db_clauses(P).



%% has_db_clauses( +PI) is semidet.
%
% Has Database Clauses.
%
has_db_clauses(PI):-modulize_head(PI,P),
   predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause_u(P,true).



%% pred_t0(+ ?P) is semidet.
%
% Predicate True Stucture Primary Helper.
%
pred_t0(P):-mreq('==>'(P)).
pred_t0(P):-mreq('$pt'(_,P,_)).
pred_t0(P):-mreq('$bt'(P,_)).
pred_t0(P):-mreq('$nt'(P,_,_)).
pred_t0(P):-mreq('$spft'(_,P,_,_)).

%pred_r0(-(P)):- call_u(-(P)).
%pred_r0(~(P)):- mreq(~(P)).


%% pred_r0( :TermP) is semidet.
%
% Predicate R Primary Helper.
%
pred_r0(P==>Q):- mreq(P==>Q).
pred_r0(P<==>Q):- mreq(P<==>Q).
pred_r0(P<-Q):- mreq(P<-Q).


%% cnstrn( +X) is semidet.
%
% Cnstrn.
%
:- module_transparent(cnstrn/1).
cnstrn(X):-term_variables(X,Vs),maplist(cnstrn0(X),Vs),!.

%% cnstrn( +V, ?X) is semidet.
%
% Cnstrn.
%
:- module_transparent(cnstrn/2).
cnstrn(V,X):-cnstrn0(X,V).

%% cnstrn0( +X, ?V) is semidet.
%
% Cnstrn Primary Helper.
%
:- module_transparent(cnstrn0/2).
cnstrn0(X,V):-when(is_ftNonvar(V),X).


%% rescan_pfc is semidet.
%
% Rescan Prolog Forward Chaining.
%
rescan_pfc:-forall(clause(baseKB:mpred_hook_rescan_files,Body),show_entry(rescan_pfc,Body)).


%% mpred_facts_and_universe( +P) is semidet.
%
% PFC Facts And Universe.
%
mpred_facts_and_universe(P):- (is_ftVar(P)->pred_head_all(P);true),call_u(P). % (meta_wrapper_rule(P)->call_u(P) ; call_u(P)).


%% repropagate( :TermP) is semidet.
%
% Repropagate.
%                                   
repropagate(_):-  notrace((check_context_module,fail)).
repropagate(P):-  quietly(repropagate_0(P)).

%repropagate(P):-  check_real_context_module,fail.

repropagate_0(P):-  notrace(is_ftVar(P)),!.
repropagate_0(USER:P):- USER==user,!,repropagate_0(P).
repropagate_0(==>P):- !,repropagate_0(P).
repropagate_0(P):-  meta_wrapper_rule(P),!,call_u(repropagate_meta_wrapper(P)).
repropagate_0(F/A):- is_ftNameArity(F,A),!,functor(P,F,A),!,repropagate_0(P).
repropagate_0(F/A):- atom(F),is_ftVar(A),!,repropagate_atom(F).
repropagate_0(F):- atom(F),!,repropagate_atom(F).
repropagate_0(P0):- p0_to_mp(P0,P),
     \+ predicate_property(P,_),catch('$find_predicate'(P0,PP),_,fail),PP\=[],!,
     forall(member(M:F/A,PP),must((functor(Q,F,A),repropagate_0(M:Q)))).
repropagate_0(P):-  notrace((\+ predicate_property(_:P,_),dmsg_pretty(undefined_repropagate(P)))),dumpST,dtrace,!,fail.
repropagate_0(P):- repropagate_meta_wrapper(P).

predicates_from_atom(Mt,F,P):- var(Mt),!,
  ((guess_pos_assert_to(Mt),Mt:current_predicate(F,M:P0)) *-> ((Mt==M)->P=P0;P=M:P0) ; 
    ('$find_predicate'(F,PP),member(Mt:F/A,PP),functor(P0,F,A),P=Mt:P0)).

predicates_from_atom(Mt,F,P):- 
    (Mt:current_predicate(F,M:P0)*->true;
      (catch('$find_predicate'(F,PP),_,fail),PP\=[],member(Mt:F/A,PP),accessable_mt(Mt,M),functor(P0,F,A))),
      ((Mt==M)->P=P0;P=M:P0).

accessable_mt(Mt,M):- M=Mt.
accessable_mt(Mt,M):- clause_b(genlMt(Mt,M)).
%accessable_mt(_Mt,baseKB).
                             
                                                                
repropagate_atom(F):- 
     guess_pos_assert_to(ToMt),
     forall(predicates_from_atom(ToMt,F,P),repropagate_0(P)).
     %ToMt:catch('$find_predicate'(F,PP),_,fail),PP\=[],!,
     %forall(member(M:F/A,PP),must((functor(Q,F,A),repropagate_0(M:Q)))).

p0_to_mp(MP,SM:P0):- 
  strip_module(MP,M0,P0),
  ((M0==query;M0==pfc_lib;is_code_module(M0))-> (get_query_from(SM),sanity(pfc_lib\==SM));SM=M0).

:- export(repropagate_meta_wrapper/1).
:- module_transparent(repropagate_meta_wrapper/1).

:- thread_local(t_l:is_repropagating/1).
%% repropagate_meta_wrapper( +P) is semidet.
%
% Repropagate Meta Wrapper Rule.
%
repropagate_meta_wrapper(P):-
 call_u(doall((no_repeats((mpred_facts_and_universe(P))),
    locally_tl(is_repropagating(P),ignore((once(show_failure(fwd_ok(P))),show_call(mpred_fwc(P)))))))).


predicate_to_goal(P,Goal):-atom(P),get_arity(P,F,A),functor(Goal,F,A).
predicate_to_goal(PF/A,Goal):-atom(PF),get_arity(PF,F,A),functor(Goal,F,A).
predicate_to_goal(G,G):-compound(G),!.


%% fwd_ok( :TermP) is semidet.
%
% Forward Repropigated Ok.
%
fwd_ok(_):-!.
fwd_ok(P):-ground(P),!.
fwd_ok(if_missing1(_,_)).
fwd_ok(idForTest(_,_)).
fwd_ok(clif(_)).
fwd_ok(pfclog(_)).
fwd_ok(X):-compound(X),get_assertion_head_arg(_,X,E),compound(E),functor(E,(:-),_),!.
% fwd_ok(P):-must(ground(P)),!.


%% mpred_facts_only( +P) is semidet.
%
% PFC Facts Only.
%
mpred_facts_only(P):- (is_ftVar(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),no_repeats(P).



:- thread_local(t_l:user_abox/1).

% :- ((prolog_load_context(file,F),  prolog_load_context(source,F))-> throw(prolog_load_context(source,F)) ; true). :- include('mpred_header.pi').
:- style_check(+singleton).

% TODO READD
%:- foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttRelationType,
%     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,functorIsMacro,prologListValued,prologSingleValued),P),)

%% get

% =================================================
% ==============  UTILS BEGIN        ==============
% =================================================

%% ruleBackward(+R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition,Support):- ruleBackward0(R,Condition,Support),
  functor(Condition,F,_),\+ arg(_,v(call_u,mpred_bc_only,inherit_above),F).

%% ruleBackward0(+F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition,Support):- call_u('<-'(FF,Condition)),copy_term('<-'(FF,Condition),Support),FF=F.
%ruleBackward0(F,Condition,(F :- Condition)):- clause_u(F,Condition),\+ (is_src_true(Condition);mpred_is_info(Condition)).


% ======================= 
% user''s program''s database
% ======================= 


%% assert_mu(+X) is semidet.
%
% Assert For User Code.
%

assert_mu(MH):-  fix_mp(clause(assert,assert_u), MH,M,H),get_unnegated_functor(H,F,A),assert_mu(M,H,F,A).
asserta_mu(MH):- fix_mp(clause(assert,asserta_u),MH,M,H),asserta_mu(M,H).
assertz_mu(MH):- fix_mp(clause(assert,assertz_u),MH,M,H),assertz_mu(M,H).


% :- kb_shared(baseKB:singleValuedInArg/2).
:- thread_local(t_l:assert_dir/1).

%% assert_mu(+Module, +Pred, ?Functor, ?Arity) is semidet.
%
% Assert For User Code.
%
assert_mu(M,M2:Pred,F,A):- M == M2,!, assert_mu(M,Pred,F,A).
% maYBE assert_mu(M,(M2:Pred :- B),F,A):- M == M2,!, assert_mu(M,(Pred :- B),F,A).
assert_mu(M,_:Pred,F,A):- dtrace,sanity(\+ is_ftVar(Pred)),!, assert_mu(M,Pred,F,A).
assert_mu(M,(Pred:- (AWC,More)),_,_):- AWC == awc,!,asserta_mu(M,(Pred:- (AWC,More))).
assert_mu(M,(Pred:- (ZWC,More)),_,_):- ZWC == zwc,!,assertz_mu(M,(Pred:- (ZWC,More))).
%assert_mu(M,Pred,F,_):- clause_b(singleValuedInArg(F,SV)),!,must(update_single_valued_arg(M,Pred,SV)),!.
%assert_mu(M,Pred,F,A):- a(prologSingleValued,F),!,must(update_single_valued_arg(M,Pred,A)),!.
assert_mu(M,Pred,F,_):- a(prologOrdered,F),!,assertz_mu(M,Pred).
assert_mu(M,Pred,_,_):- t_l:assert_dir(Where),!, (Where = a -> asserta_mu(M,Pred); assertz_mu(M,Pred)).
%assert_mu(M,Pred,_,1):- !, assertz_mu(M,Pred),!.
assert_mu(M,Pred,_,_):- assertz_mu(M,Pred).


:-thread_local(t_l:side_effect_ok/0).


%% assertz_mu(+M, ?X) is semidet.
%
% Assertz Module Unit.
%
%assertz_mu(abox,X):-!,defaultAssertMt(M),!,assertz_mu(M,X).
%assertz_mu(M,X):- check_never_assert(M:X), clause_asserted_u(M:X),!.
% assertz_mu(M,X):- correct_module(M,X,T),T\==M,!,assertz_mu(T,X).
% assertz_mu(_,X):- must(defaultAssertMt(M)),!,must((expire_tabled_list(M:X),show_call(attvar_op(db_op_call(assertz,assertz_i),M:X)))).


assertz_mu(_,X):- check_never_assert(X),fail.
%assertz_mu(M,M2:Pred,F,A):- M == M2,!, assertz_mu(M,Pred,F,A).
%assertz_mu(M,'$spft'(MZ,P,mfl4(VarNameZ,KB,F,L),T)):-M\==KB,!,assertz_mu(KB,'$spft'(MZ,P,mfl4(VarNameZ,KB,F,L),T)).
assertz_mu(M,X):- strip_module(X,_,P), %sanity(check_never_assert(M:P)), 
    must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(assertz,assertz_i),M:P)))).
   %(clause_asserted_u(M:P)-> true; must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(assertz,assertz_i),M:P))))).

%% asserta_mu(+M, ?X) is semidet.
%
% Asserta Module Unit.
%
%asserta_mu(abox,X):-!,defaultAssertMt(M),!,asserta_mu(M,X).
% asserta_mu(M,X):- correct_module(M,X,T),T\==M,!,asserta_mu(T,X).
% asserta_mu(_,X):- must(defaultAssertMt(M)),!,must((expire_tabled_list(M:X),show_failure(attvar_op(db_op_call(assertz,assertz_i),M:X)))).

asserta_mu(_,X):- check_never_assert(X),fail.
asserta_mu(M,X):- strip_module(X,_,P),!, %sanity(check_never_assert(M:P)), 
    must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(asserta,asserta_i),M:P)))).
   %(clause_asserted_u(M:P)-> true; must((expire_tabled_list(M:P),show_failure(attvar_op(db_op_call(asserta,asserta_i),M:P))))).


%% retract_mu( :TermX) is semidet.
%
% Retract For User Code.
%
% retract_mu(que(X,Y)):-!,show_failure(why,retract_eq_quitely_f(que(X,Y))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu(H0):- throw_depricated, strip_module(H0,_,H),defaultAssertMt(M),show_if_debug(attvar_op(db_op_call(retract,retract_i),M:H)),!,must((expire_tabled_list(H))).
retract_mu(X):- check_never_retract(X),fail.
retract_mu(~(X)):-!,show_success(why,retract_eq_quitely_f(~(X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu((X)):-!,show_success(why,retract_eq_quitely_f((X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu(M:(H:-B)):- atom(M),!, clause_u(H,B,R),erase(R).
retract_mu((H:-B)):-!, clause_u(H,B,R),erase(R).
%retract_mu(~(X)):-must(is_ftNonvar(X)),!,retract_eq_quitely_f(~(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
%retract_mu(hs(X)):-!,retract_eq_quitely_f(hs(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).





:- retractall(t_l:mpred_debug_local).
:- thread_local(t_l:in_rescan_mpred_hook/0).

%:- module_transparent(mpred_call_0/1).

 :- meta_predicate update_single_valued_arg(+,+,*).
 :- meta_predicate assert_mu(*,+,*,*).
 :- meta_predicate mpred_facts_and_universe(*).
 :- meta_predicate {*}.
 :- meta_predicate neg_in_code0(*).
 :- meta_predicate repropagate_meta_wrapper(*).
 :- meta_predicate mpred_get_support_via_sentence(*,*).

:- dynamic(infoF/1).



mpred_kb_ops_file.

%:- fixup_exports.




