/* * module 
% ===================================================================
% File 'mud_builtin.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt)) == holds(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt)) == padd(height,Obj,ObjHt,...) == moo(QueryForm)
% kretract[all](Obj,height(ObjHt)) == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- multifile baseKB:agent_action_queue/3.
:- dynamic baseKB:agent_action_queue/3.

:- thread_local(t_l:disable_px/0).
:- retractall(t_l:disable_px).

:- must(\+ t_l:disable_px).


/*
:- dynamic   lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.
:- dynamic lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.

*/
%:- '$set_source_module'(baseKB).
:- set_prolog_flag(runtime_speed, 0).
:- set_prolog_flag(runtime_safety, 2).
:- set_prolog_flag(runtime_debug, 2).
:- set_prolog_flag(unsafe_speedups, false).

:- set_prolog_flag(expect_pfc_file,always).


listing_break(G):- cwc, listing(G),break.

:- dynamic(agent_call_command/2).
:- import(agent_call_command/2).

% :- mpred_core:import(baseKB:ttExpressionType/1).


:- assert_until_eof(infSupertypeName).
:- call_on_eof(dmsg(infSupertypeName)).

:- include(prologmud(mud_header)).


% :- gripe_time(60,user:ensure_loaded(library(logicmoo_user))).

% :- set_defaultAssertMt(baseKB).
:- file_begin(pfc).


:- file_begin(code).

:- kb_global(baseKB:(  irc_event_hooks/3)).
:- kb_global(baseKB:(  deliver_event_hooks/2)).
:- kb_global(baseKB:   irc_user_plays/3).

:- kb_global(baseKB:   mudDescription/2).
:- kb_global(baseKB:   term_specifier_text/2).
:- kb_global(baseKB:   type_action_info/3).
:- kb_global(baseKB:   update_charge/2).
:- kb_global(baseKB:   update_stats/2).
:- kb_global(baseKB:   use_usable/4).
:- kb_global(baseKB:   text_actverb/2).
:- kb_global(baseKB:   vtActionTemplate/1).
% :- kb_global(baseKB:   mud_test/0).
:- kb_global(baseKB:  mud_test/1).
:- kb_global(baseKB:  mud_test/2).
:- kb_global(baseKB:  mud_test_local/0).
:- kb_global(baseKB:  mud_test_local/1).
:- kb_global(baseKB:  mud_test_local/2).
:- kb_global(baseKB:   world_agent_plan/3).
:- kb_global(baseKB:   action_info/2).
:- kb_global(baseKB:   action_rules/4).
:- kb_global(baseKB:   action_verb_useable/5).
:- kb_global(baseKB:   agent_command/2).
:- kb_global(baseKB:   agent_command_fallback/2).
:- kb_global(baseKB:   agent_text_command/4).
:- kb_global(baseKB:   check_permanence/4).

:- file_begin(pfc).


:-op(0,fx,  ('disabled')).
:-op(0,fx,  ('enabled')).
:-op(0,fy,  ('disabled')).
:-op(0,fy,  ('enabled')).
% :- '@'(ensure_loaded(library(bugger)),user).

:- multifile(user_db:grant_openid_server/2).
:- dynamic(user_db:grant_openid_server/2).

:- kb_shared('$was_imported_kb_content$'/2).
:- discontiguous('$was_imported_kb_content$'/2).
:- kb_shared(  disabled/1).
:- discontiguous(  disabled/1).
:- kb_shared(  enabled/1).
:- discontiguous(  enabled/1).
:- kb_shared   was_enabled/1.
:- discontiguous(  was_enabled/1).
:- kb_shared   listing_mpred_hook/1.


:- kb_shared   genls/2.
:- kb_shared(  isa/2).

:- style_check((-(singleton))).

/*
:-op(1190,fx,  (disabled)).
:-op(1190,fx,  (enabled)).
:-op(1190,fy,  (disabled)).
:-op(1190,fy,  (enabled)).
:-op(1120,fx,  (export)).
*/


:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

% these do not get defined!?
% :- kb_shared user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

% :- kb_shared(mpred_online:semweb_startup/0).
% :- break.
:- kb_shared(  tChannel/1).


:- kb_shared pfcManageHybrids/0.
:- kb_shared   defnSufficient/2.
% :- kb_shared   lmcache:loaded_external_kbs/1.
%:- kb_shared t_l:infMustArgIsa/0.
:- thread_local t_l:repl_to_string/2.
:- thread_local t_l:repl_writer/2.
:- thread_local(t_l:into_form_code/0).
% :- thread_local t_l:current_local_why/2.
% :- break.
:- kb_shared   loading_module_h/1.
:- kb_shared   registered_module_type/2.
:- kb_shared   must_compile_special_clause_file/1.

% HOOKS
:- kb_shared   decl_coerce/3.
:- kb_shared   listen_to_ops/2.
:- kb_shared   deduce_facts/2.
:- kb_shared   default_type_props/3.
:- kb_shared   fact_always_true/1.
:- kb_shared   fact_is_false/2.
:- kb_shared   fact_maybe_deduced/1.
:- kb_shared   never_assert_u/2.
:- kb_shared   impl_coerce_hook/3.

:- kb_shared   create_random_fact/1.
% :- kb_shared   local_term_anglify/2.
% :- kb_shared   term_anglify_last/2.
% :- kb_shared   term_anglify_np/3.
% :- kb_shared   term_anglify_np_last/3.

% :- kb_shared   hooked_random_instance/3.

:- kb_shared   now_unused/1.
:- kb_shared   provide_mpred_read_attributes/3.
:- kb_shared   provide_mpred_setup/4.
:- kb_shared   provide_mpred_clauses/3.
:- kb_shared   provide_mpred_op/2.
:- kb_shared   provide_mpred_write_attributes/2.

% DYN HOOKS
% :- kb_shared   is_never_type/1.

% DYN FOR CODE
:- dynamic lmcache:after_mpred_load/0.
:- thread_local use_cyc_database/0.
:- kb_shared use_cyc_database/0.

:- kb_shared   fact_is_false/2.
% :- kb_shared(kbp_t_list_prehook/2).


% DYN KB
:- kb_shared   only_if_pttp/0.
:- kb_shared   use_kif/2.
% :- kb_shared   is_mpred_prop/2.
%:- kb_shared   hasInstance_dyn/2.
%:- kb_shared   arity/2.
/*
%:- kb_shared   mpred_prop/3.
:- kb_shared   '<=>'/2.
% :- kb_shared   ruleForward/2.
:- kb_shared   ruleRewrite/2.
% :- kb_shared   ruleBackward/2.

% :-must((  mpred_prop(t,_,prologHybrid))).

*/

:- kb_global(baseKB:   term_specifier_text/2).
:- kb_global(baseKB:   update_charge/2).
:- kb_global(baseKB:   update_stats/2).
:- kb_global(baseKB:   use_usable/4).
:- kb_global(baseKB:   text_actverb/2).
:- kb_global(baseKB:   vtActionTemplate/1).
:- kb_global(baseKB:  mud_test/0).
:- kb_global(baseKB:  mud_test/1).
:- kb_global(baseKB:  mud_test/2).
:- kb_global(baseKB:  mud_test_local/0).
:- kb_global(baseKB:  mud_test_local/1).
:- kb_global(baseKB:  mud_test_local/2).
:- kb_global(baseKB:   world_agent_plan/3).
:- kb_global(baseKB:   action_info/2).
:- kb_global(baseKB:   action_rules/4).
:- kb_global(baseKB:   action_verb_useable/5).
:- kb_global(baseKB:   agent_command/2).
:- kb_global(baseKB:   agent_text_command/4).
:- kb_global(baseKB:   check_permanence/4).


% predicateConventionMt(agent_call_command,baseKB).

% :- ensure_loaded('logicmoo/pfc/autoexec.pfc').

% isa(iPerson99,tPerson).

:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- dynamic(mudTermAnglify/2).
:- discontiguous(mudTermAnglify/2).

tWorld(iWorld7).

/*
:- rtrace.
:- trace.
*/
ttExpressionType(ftProlog).

% 

% ~tSet(ftChangeQuantity).

%:-  rtrace((trace,ain(ttExpressionType(ftChangeQuantity)))).
%:- break.

% :- mpred_post1(ttExpressionType(ftChangeQuantity)).

ttExpressionType(ftChangeQuantity).
:- assert((ftChangeQuantity(X):- compound(X),arg(X,1,Q),quotedIsa(Q,ftNumber))).


% ==> neg(arity(mudAreaConnected,1)).

%ruleRewrite(isa(isInstFn(Sub),Super),genls(Sub,Super)):-ground(Sub:Super),!.

:- dynamic(tItem/1).
:- dynamic(ttAgentType/1).



:- dynamic(disjointWith/2).
/*
disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):- disjointWithT(A,B).
disjointWith(A,B):- disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.
*/

disjointWith(Sub, Super) ==> disjointWith( Super, Sub).
disjointWith(tObj,tRegion).
disjointWith(ttSpatialType,ttAbstractType).


rtBinaryPredicate(arity).

%rtBinaryPredicate(Pred) ==> arity(Pred,2),tPred(Pred).
%arity(Pred,2),tPred(Pred) ==> rtBinaryPredicate(Pred).

% () <==> rtBinaryPredicate(Pred).
prologHybrid(relationMostInstance(rtBinaryPredicate,tCol,vtValue)).
%relationMostInstance(BP,_,_)==>(rtBinaryPredicate(BP),rtBinaryPredicate(BP)).
prologHybrid(relationAllInstance(rtBinaryPredicate,tCol,vtValue)).
relationAllInstance(BP,_,_)==>rtBinaryPredicate(BP).


% (isa(Inst,Type), tCol(Inst)) ==> isa(Type,ttTypeType).
% (isa(TypeType,ttTypeType) , isa(Inst,TypeType), genls(SubInst,Inst)) ==> isa(SubInst,TypeType).

(sometimesSlow, ttExpressionType(FT),{compound(FT)})==>meta_argtypes(FT).

tCol(vtDirection).

 % mdefault(genls(tPartofObj,tItem)).

tCol(tCol).

:- must(ain(tCol(tAgent))).


% defined more correctly below dividesBetween(S,C1,C2) ==> (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).
% dividesBetween(S,C1,C2) ==> (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).
% disjointWith(tItem,tPathway).
%:-export(repl_to_string(tAgent,ftTerm)).
%:-export(repl_writer(tAgent,ftTerm)).
%:-export(repl_writer/2).
%prologHybrid(typeProps(tCol,ftVoprop)).
dividesBetween(tHominid,tMale,tFemale).
dividesBetween(tAgent,tHumanControlled,tNpcAgent).
dividesBetween(tObj,tItem,tAgent).
dividesBetween(tItem,tMassfull,tMassless).
disjointdividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tTemporalThing,tObj,tRegion).
formatted_resultIsa(ftDiceFn(ftInt,ftInt,ftInt),ftInt).
% disjointWith(P1,P2) ==> ((neg(isa(C,P1))) <==> isa(C,P2)).

% isa(Col1, ttObjectType) ==> neg(isa(Col1, ttExpressionType)).




rtArgsVerbatum(functorIsMacro).
% tCol(ArgsIsa):-mpred_is_trigger(ArgsIsa).
% tCol(ArgsIsa):-ttRelationType(ArgsIsa).
% TODO decide if OK
%(mpred_prop(_,_,meta_argtypes(ArgTypes)),{is_declarations(ArgTypes)}) ==> meta_argtypes(ArgTypes).

tSet(COL)==>tCol(COL).
ttExpressionType(COL)==>tCol(COL).


tCol(functorIsMacro).
tCol(tCol).
tCol(tFunction).
tCol(tPred).
tCol(tRelation).
tCol(ttExpressionType).
tCol(ttSpatialType).
tCol(vtActionTemplate).
tSet(tCol).
tSet(tContainer).
tSet(tFunction).
tSet(tPred).
tSet(tRegion).
tSet(tRelation).
ttTypeType(ttExpressionType).
ttTypeType(ttSpatialType).

ftSpec(vtActionTemplate).

isa(tRegion,ttSpatialType).
isa(tRelation,ttAbstractType).

% genlPreds(genls,equals).
% genls(A, B):- tCol(A),{A=B}.

% must(Goal):- Goal. % (quietly((visible(+all),visible(+unify),visible(+exception),leash(-all),leash(+exception))),(trace,Goal),leash(+all)).

% :- gutracer.


tCol(tFly).

prologHybrid(localityOfObject(tObj,tSpatialThing)).


persistInMudIsa(tItem).
persistInMudIsa(tAgent).
persistInMudIsa(tRegion).

sometimesHack(genls).

~tPathway(apathFn(iOfficeRoom7, vNorth)).

tPathway(PATH):- cwc, PATH=@=apathFn(iOfficeRoom7, vNorth),trace_or_throw(error(tPathway(PATH))).


(sometimesSlow, tCol(Col), {isa_from_morphology(Col,Type)}) ==> isa(Col,Type).

(sometimesSlow, tInstance(Inst), {isa_from_morphology(Inst,Type)}) ==> isa(Inst,Type).

% HOW TO MAKE THIS FAST? isa(Inst,Type) <= {isa_from_morphology(Inst,Type)}.

%((disjointWith(P1,P2) , genls(C1,P1), {dif:dif(C1,P1)}) ==>    disjointWith(C1,P2)).
% (disjointWith(C1,P2) <- (genls(C1,P1), {dif:dif(C1,P1)}, disjointWith(P1,P2))).

% :- rtrace.
completelyAssertedCollection(Complete)==> {must(\+ ttExpressionType(Complete))}.


==> tSourceCode(iSourceCode7,comment("PrologMUD Server code")).
==> tSourceData(iSourceData8,comment("PrologMUD WorldState Data")).


%isLoadedType(tSourceCode) ==> (tPred(Toy),arity(Toy,A)/ ( \+ current_predicate(Toy/A)) ==> dynamic(Toy/A)).
%isLoadedType(tSourceCode) ==> (functorDeclares(Toy),prologArity(Toy,A)/( \+ current_predicate(Toy/A)) ==> dynamic(Toy/A)).


% functorDeclares(Toy),tFunction(Toy),arity(Toy,A),{A2 is A + 1}==>prologArity(Toy,A2).

tCol(completelyAssertedCollection).
rtArgsVerbatum(completeIsaAsserted).
tSpatialThing(I)==>completeIsaAsserted(I).
genls(completelyAssertedCollection,tCol).
completelyAssertedCollection(tItem).
completelyAssertedCollection(tRegion).
completelyAssertedCollection(tObj).
% :-must.
completelyAssertedCollection(tAgent).
completelyAssertedCollection(tCarryAble).
completelyAssertedCollection(vtVerb).
% :-rnotrace.


ttTypeType(completeIsaAssertedType).

completeIsaAssertedType(Col) ==> (isa(I,Col) ==> completeIsaAsserted(I)).
completeIsaAssertedType(tAgent).
completeIsaAssertedType(tCarryAble).
completeIsaAssertedType(tObj).

tCol(ttTypeByAction).
:-must(ain(tCol(ttTypeByAction))).
:- nortrace.



genls(ttTypeByAction,completelyAssertedCollection).

% dividesBetween(tItem,tPathway).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tObj,tItem,tAgent).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tAgent,tHumanControlled,tNpcAgent).

((sometimesBuggy,genls(A,B)/ground(genls(A,B)))==>{call((call((trace,(must(ain(tCol(A))),must(ain(tCol(B))))))))}).

((sometimesBuggy,dividesBetween(S,C1,C2),{ground(S:C1:C2)}) ==> ((disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)))).

% slow... ttObjectType(Col1) ==> ~ttExpressionType(Col1).

neg(isa(I,Super)) :- {ground(isa(I,Super))}, (isa(I,Sub), disjointWith(Sub, Super)).
% disjointWith(P1,P2) ==> {\+(isa(P1,rtAvoidForwardChain)),\+(isa(P2,rtAvoidForwardChain))},(neg(isa(C,P1)) <==> isa(C,P2)).


tCol(ttSpatialType).


% ===================================================================
% MUD TMS - Type checker system / Never Assert / Retraction checks
% ===================================================================
/*
never_assert_u(mudAtLoc(iArea1025, _),isa(iArea1025,tRegion)).
never_assert_u(localityOfObject(iArea1025, iOfficeRoom7),isa(iArea1025,tRegion)).
never_assert_u(localityOfObject(R,_),isa(R,tRegion)):- isa(R,tRegion).
never_assert_u(mudFacing(R,_),isa(R,tRegion)):- isa(R,tRegion).
never_assert_u(mudAtLoc(R,_),isa(R,tRegion)):- isa(R,tRegion).

%deduce_facts_forward(localityOfObject(_,Region),isa(Region,tSpatialThing)).
deduce_facts_forward(localityOfObject(Obj,_),isa(Obj,tObj)).
fix_argIsa(F,N,vtDirection(Val),vtDirection):-ain(mpred_prop(F,_,relationMostInstance(N,Val))),!.

*/

typeCheckDecl(vtActionTemplate(ArgTypes),is_declarations(ArgTypes)).

% Representations
vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes).

((meta_argtypes(ArgTypes)/get_functor(ArgTypes,F)),vtVerb(F))==>vtActionTemplate(ArgTypes).



argIsa(aDirectionsFn,2,ftListFn(vtDirection)).
argIsa(apathFn,1,tRegion).
argIsa(apathFn,2,vtDirection).
argIsa(localityOfObject,1,tObj).
argIsa(localityOfObject,2,tSpatialThing).
argIsa(mudColor,1,tObj).
argIsa(mudColor,2,vtColor).
argIsa(mudFacing,1,tObj).
argIsa(mudFacing,2,vtDirection).
argIsa(mudMemory,2,ftTerm).

tCol(vtVerb).



tCol(tChannel).
tSet(tItem).
tCol(vtVerb).

% prologIsFlag(tAgent(ftID),[tSet]).
% prologDynamic(createableSubclassType/2).
% alt_forms1(none_AR,localityOfObject(P,R),mudAtLoc(P,L)):-ground(localityOfObject(P,R)),call_u(mudAtLoc(P,L)),nonvar(L),once(locationToRegion(L,R)).
% alt_forms1(none_AR,mudAtLoc(P,L),localityOfObject(P,R)):-ground(mudAtLoc(P,L)),once(locationToRegion(L,R)),nonvar(R).
% argsIsa(mudFacing,ftTerm).
% we need a way to call this: maxCapacity
% we need a way to call this: typeMaxCapacity
%:- compile_predicates([isa/2]).
%prologHybrid(repl_to_string(tAgent,term),[prologSingleValued,relationMostInstance(tAgent,default_repl_obj_to_string)]).
% prologHybrid(repl_writer(tAgent,term),[prologSingleValued,relationMostInstance(tAgent,default_repl_writer)]).
%:- forall(ttRelationType(F),dynamic(F/1)).
%:- foreach(retract(isa(I,C)),assert_hasInstance(C,I)).
%isa(AT,ttAgentType):- genls(AT,tAgentGeneric).
%genls(AT,tAgentGeneric):- isa(AT,ttAgentType).
%subFormat(ftTextType,ftText).
%prologIsFlag(tItem(ftID)).
%prologIsFlag(tRegion(ftID),[tSet]).
%prologIsFlag(tRegion(ftID),tCol).
prologIsFlag(tThinking(tAgent)).

==> prologHybrid(isEach(mudLastCmdSuccess/3,mudLastCommand/2,mudNamed/2, mudSpd/2,mudStr/2,typeGrid/3)).

singleValuedHybrid(F)==>(singleValued(F),prologHybrid(F)).


:- dynamic(mudDescription/2).
:- dynamic((tItem/1, tRegion/1, instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, mudToHitArmorClass0/2, tThinking/1, tDeleted/1, mudWeight/2, mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2, mudDescription/2, mudFacing/2, mudCmdFailure/2, mudSpd/2, typeGrid/3, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, wearsClothing/2)).
:- dynamic((mudArmorLevel/2, mudLevelOf/2, mudToHitArmorClass0/2, mudBareHandDamage/2,
   chargeCapacity/2, mudEnergy/2, tCol/1, tAgent/1, tItem/1, tRegion/1, instVerbOverride/3,
   mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, tThinking/1, mudWeight/2,
   mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2,
   mudDescription/2, mudFacing/2, failure/2, gridValue/4, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, mudWearing/2)).



==> prologMultiValued(mudDescription(ftTerm,ftString),[prologOrdered,prologHybrid]).
prologMultiValued(mudDescription(ftTerm,ftText), [predProxyAssert(add_description),predProxyRetract(remove_description),predProxyQuery(query_description)],prologHybrid).
==> prologMultiValued(mudDescription(ftTerm,ftText),[predProxyAssert(add_description),prologHybrid]).
==> prologMultiValued(mudKeyword(ftTerm,ftString),prologHybrid).
prologMultiValued(mudMemory(tAgent,ftTerm),prologHybrid).
prologMultiValued(mudNamed(ftTerm,ftTerm),prologHybrid).
prologMultiValued(mudPossess(tObj,tObj),prologHybrid).
prologMultiValued(nameString(ftTerm,ftString),prologHybrid).
prologMultiValued(pathDirLeadsTo(tRegion,vtDirection,tRegion),prologHybrid).
prologMultiValued(pathName(tRegion,vtDirection,ftString),prologHybrid).
prologMultiValued(genls(tCol,tCol),prologHybrid).
==> prologSingleValued(typeGrid(tCol,ftInt,ftListFn(ftString)),prologHybrid).
prologMultiValued(typeGrid(tCol,ftInt,ftListFn(ftString)),prologHybrid).
prologMultiValued(verbAsWell(ftTerm,ftAction,ftAction),prologHybrid).

prologNegByFailure(mudNeedsLook(tObj,ftBoolean),prologHybrid).
prologNegByFailure(tAgent(ftID),prologHybrid).
prologNegByFailure(tCol(ftID),prologHybrid).
prologNegByFailure(tCol(ftID),prologHybrid).
prologNegByFailure(tItem(ftID),prologHybrid).
prologNegByFailure(tRegion(ftID),prologHybrid).
prologNegByFailure(tThinking(tAgent),prologHybrid).
pathName(Region,Dir,Text)==>mudDescription(apathFn(Region,Dir),Text).


==> prologSingleValued(chargeCapacity(tChargeAble,ftInt),prologHybrid).
prologSingleValued(location_center(tRegion,xyzFn(tRegion,ftInt,ftInt,ftInt)),prologHybrid).
==> prologSingleValued(mudAgentTurnnum(tAgent,ftInt),[relationMostInstance(tAgent,0)],prologHybrid).
% :- listing( prologSingleValued ).


singleValuedHybrid(mudArmor(tObj,ftInt)).
singleValuedHybrid(mudArmorLevel(tWearAble,ftInt)).
:- mpred_notrace_exec.

singleValuedHybrid(mudAtLoc(tObj,xyzFn(tRegion,ftInt,ftInt,ftInt))).
singleValuedHybrid(mudAttack(tObj,ftInt)).
singleValuedHybrid(mudBareHandDamage(tAgent,ftInt)).
% singleValuedHybrid(mudBareHandDamage(tAgent,ftDiceFn)).


% prologSingleValued(mudEnergy(tChargeAble,ftInt(90)),prologHybrid).
singleValuedHybrid(mudEnergy(tChargeAble,ftInt)).
==> prologSingleValued(mudEnergy(tObj,ftInt),[relationMostInstance(tAgent,90),relationMostInstance(tChargeAble,130)],prologHybrid).
==> prologSingleValued(mudHygiene(tObj,ftInt),[relationMostInstance(tObj,90)],prologHybrid).

:- mpred_notrace_exec.
:- ain(==>((prologSingleValued(mudFacing(tObj,vtDirection),[relationMostInstance(tObj,vNorth)],prologHybrid)))).


singleValuedHybrid(mudPermanence(tItem,vtVerb,vtPerminance)).
singleValuedHybrid(mudHealth(tObj,ftInt)).
singleValuedHybrid(mudHeight(tObj,ftInt)).
singleValuedHybrid(mudHeight(tSpatialThing,ftInt)).
singleValuedHybrid(mudID(tObj,ftID)).
singleValuedHybrid(mudLevelOf(tCarryAble,ftInt)).
singleValuedHybrid(mudWeight(tObj,ftInt)).

singleValuedHybrid(mudMaxHitPoints(tAgent,ftInt),[prologHybrid],prologHybrid).
singleValuedHybrid(mudLastCommand(tAgent,ftAction)).
==> prologSingleValued(mudNonHunger(tAgent,ftInt),[relationMostInstance(tAgent,90)],prologHybrid).
==> prologSingleValued(mudMoveDist(tAgent,ftInt),[relationMostInstance(tAgent,1)]).
==> prologSingleValued(mudNeedsLook(tAgent,ftBoolean),relationMostInstance(tAgent,vFalse),prologHybrid).
singleValuedHybrid(mudScore(tAgent,ftInt)).
singleValuedHybrid(mudSpd(tAgent,ftInt)).
singleValuedHybrid(mudStm(tAgent,ftInt)).
singleValuedHybrid(mudStr(tAgent,ftInt)).
singleValuedHybrid(mudToHitArmorClass0(tAgent,ftInt)).
% prologSingleValued(spawn_rate(isPropFn(genls(tObj)),ftInt)).

==> prologSingleValued(spawn_rate(tCol,ftInt)).
==> prologSingleValued(stat_total(tAgent,ftInt)).

resultIsa(apathFn,tPathway).
% '<==>'(isa(Whom,tNpcAgent),whenAnd(isa(Whom,tAgent),naf(isa(Whom,tHumanControlled)))).
'<==>'(mudDescription(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).
'<==>'(nameString(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).


rtDeduceArgTypes(pathDirLeadsTo).
rtDeduceArgTypes(F)/current_predicate(F,P),argIsa(F,N,T)/arg(N,P,E),{call(P)}==>isa(E,T).

vtValue(Val)/(atom(Val),i_name_lc(Val,KW))==>mudKeyword(Val,KW).

ttPredAndValueType(Str)/
  (i_name('mud',Str,Pred),
  i_name('vt',Str,VT)) ==> 
    (rtRolePredicate(Pred),
     ttValueType(VT),
      mudKeyword(VT,Str),mudKeyword(Pred,Str),
      argIsa(Pred,2,VT),
      argIsa(Pred,1,tTemporalThing)).

%:- mpred_trace_exec.
ttPredAndValueType("size").
ttPredAndValueType("texture").
ttPredAndValueType("color").
:- mpred_notrace_exec.
ttPredAndValueType("shape").
ttPredAndValueType("material").


%relationMostInstance(arg1Isa,rtRolePredicate,tTemporalThing).
%relationMostInstance(arg2QuotedIsa,rtRolePredicate,ftTerm).

% mudKeyword(W,R) <= {atom(W),i_name_lc(W,R)}.

ttValueType(vtSize).
ttValueType(vtTexture).
ttValueType(vtColor).

ttValueType(VT)==>tInferInstanceFromArgType(VT).

prologHybrid(typeHasGlyph(tCol,ftString)).
prologHybrid(mudMaxHitPoints(tAgent,ftInt)).
prologHybrid(mudStowing(tAgent,tItem)).
% :- break.

prologHybrid(text_actverb(ftText,vtVerb)).
:- sanity(((argIsa(text_actverb,1,C);argQuotedIsa(text_actverb,1,C)))).

:-dynamic((latitude/2, mudMoveDist/2, longitude/2)).
prologHybrid(typeHasGlyph,2).
prologHybrid(mudActAffect/3).
prologHybrid(mudAtLoc,2).
prologHybrid(mudColor/2).
prologHybrid(mudHealth,2).
prologHybrid(mudMaterial/2).
prologHybrid(mudNeedsLook,2).
:- kb_global(baseKB:mudNeedsLook/2).
prologHybrid(mudNeedsLook/2,[completeExtentAsserted]).
prologHybrid(mudShape/2).
prologHybrid(mudSize/2).
prologHybrid(mudTexture/2).
prologHybrid(pathDirLeadsTo/3).
prologDynamic(mudMoveDist/2).
:- dynamic(mudMoveDist/2).
meta_argtypes(mudMoveDist(tAgent,ftInt)).
% ==> prologSingleValued(mudMoveDist,[predicateConventionMt(abox),query(call),relationMostInstance(tAgent,1)]).
prologDynamic(stat_total/2).

:- dynamic(vtBasicDir/1).

tCol(vtBasicDir).
:- must(ain(tCol(vtBasicDirPlusUpDown))).
% :- break.
tCol(vtDirection).
tCol(vtVerb).
:- dynamic stat_total/2.
:- dynamic(spawn_rate/2).
tCol(mobMonster).
%prologDynamic(action_info(vtActionTemplate,ftText)).
prologDynamic(agent_command(tAgent,ftAction)).
:- ain(prologSideEffects(agent_command(tAgent,ftAction))).
%prologBuiltin(member(ftTerm,ftTerm)).
prologDynamic(mud_test(ftTerm,ftCallable)).
prologDynamic(use_action_templates(ftTerm)).


prologHybrid(typeHasGlyph(tCol,ftString)).
prologHybrid(mudColor(tSpatialThing,vtColor)).
prologHybrid(mudKnowing(tAgent,ftAssertion)).
==> prologHybrid(mudLabelTypeProps(ftString,tCol,ftVoprop)).
prologHybrid(mudListPrice(tItem,ftNumber)).
:-dynamic(mudOpaqueness/2).
prologHybrid(mudOpaqueness(ftTerm,ftPercent)).
prologHybrid(mudPossess(tAgent,tObj)).
prologHybrid(mudShape(tSpatialThing,vtShape)).
prologHybrid(mudSize(tSpatialThing,vtSize)).
prologHybrid(mudTextSame(ftText,ftText)).
prologHybrid(mudTexture(tSpatialThing,vtTexture)).
meta_argtypes(aDirectionsFn(ftTerm,ftListFn(ftTerm))).

==>prologListValued(mudGetPrecepts(tAgent,ftListFn(tSpatialThing))). % [predicateConventionMt(abox)].

prologListValued(mudNearBody(tAgent,ftListFn(tSpatialThing)),[]).
prologListValued(mudNearReach(tAgent,ftListFn(tSpatialThing))). % [predicateConventionMt(abox)].
prologMultiValued(action_rules(tAgent,vtVerb,ftListFn(ftVar),ftVoprop)).
prologMultiValued(mudLastCmdSuccess(tAgent,ftAction,ftBoolean)).
prologMultiValued(descriptionHere(ftTerm,ftString)).
prologMultiValued(descriptionHere(ftTerm,ftString),prologOrdered).
prologMultiValued(determinerString(ftTerm,ftString)).
prologMultiValued(typeHasGlyph(ftTerm,ftString)).
prologMultiValued(gridValue(tRegion,ftInt,ftInt,tObj)).
prologMultiValued(instVerbOverride(ftTerm,ftAction,ftAction)).
:- mpred_notrace_exec.
prologMultiValued(isa(ftTerm,tCol)).
prologMultiValued(mudActAffect(ftTerm,ftTerm,ftTerm)).
prologMultiValued(mudActAffect(tItem,vtVerb,ftTerm(ftVoprop))).
prologMultiValued(mudCmdFailure(tAgent,ftAction)).


==> tPred(isEach(tAgent/1, mudEnergy/2,mudHealth/2, mudAtLoc/2, failure/2, typeGrid/3, gridValue/4, isa/2, tItem/1, mudMemory/2, pathName/3, mudPossess/2, tRegion/1, mudScore/2, mudStm/2, mudFacing/2, localityOfObject/2, tThinking/1, mudWearing/2, mudFacing/2, mudHeight/2, act_term/2, nameString/2, mudDescription/2, pathDirLeadsTo/3, mudAgentTurnnum/2)).
prologHybrid(mudToHitArmorClass0 / 2).
prologHybrid(mudAtLoc/2).
prologBuiltin((agent_command/2)).
==> prologHybrid(isEach(argIsa/3, formatted_resultIsa/2, typeHasGlyph/2, inRegion/2, 
  mudContains/2, isa/2, mudLabelTypeProps/3, mudMemory/2, mudPossess/2, mudStowing/2, 
  genls/2, mudToHitArmorClass0/2, pddlSomethingIsa/2, resultIsa/2,  
  completeExtentAsserted/1, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, 
  ttExpressionType/1, typeProps/2)).

==> prologHybrid(isEach(tItem/1, tRegion/1, instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, mudToHitArmorClass0/2, tThinking/1, tDeleted/1, mudWeight/2, mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2, mudDescription/2, mudFacing/2, mudCmdFailure/2, mudSpd/2, typeGrid/3, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, wearsClothing/2)).
==> prologHybrid(isEach( mudArmorLevel/2, mudLevelOf/2, mudToHitArmorClass0/2, mudBareHandDamage/2, chargeCapacity/2, mudEnergy/2, tCol/1, tAgent/1, tItem/1, tRegion/1, instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, tThinking/1, mudWeight/2, mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2, mudDescription/2, mudFacing/2, failure/2, gridValue/4, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, mudWearing/2)).

% :-must(fully_expand(clause(asert,test),prologHybrid(typeHasGlyph,2),(arity(typeHasGlyph, 2), prologHybrid(typeHasGlyph), tPred(typeHasGlyph)))).

arity(typeHasGlyph,2).
arity(mudTermAnglify,2).
arity(mudMaxHitPoints,2).


prologHybrid(instVerbOverride(ftTerm,ftAction,ftAction)).
%isa(localityOfObject,prologHybrid). 
%isa(mudActAffect, prologMultiValued).
%isa(mudMaxHitPoints,prologHybrid).
isa(vtDirection,ttValueType).

prologMultiValued(agent_text_command(tAgent,ftText,tAgent,ftAction)).

formatted_resultIsa(apathFn(tRegion,vtDirection),tPathway).

prologBuiltin(is_vtActionTemplate/1).

is_vtActionTemplate(C):-nonvar(C),get_functor(C,F),!,atom_concat(act,_,F).

defnSufficient(ftAction,is_vtActionTemplate).
defnSufficient(ftAction,vtVerb).
defnSufficient(ftTerm,vtValue).

:- use_module(library(logicmoo_plarkc)).

:- install_constant_renamer_until_eof.
:- set_prolog_flag(do_renames_sumo,never).

genls('FemaleAnimal',tAgent).
genls('MaleAnimal',tAgent).
==>genls(isEach('PortableObject','ProtectiveAttire','SomethingToWear'),tCarryAble).
==>genls(isEach('ProtectiveAttire','SomethingToWear'),tWearAble).
==>genls(isEach(tRegion,tAgent),tChannel).

genls(tAgent,tObj).
genls(tAgent,tSpatialThing).
genls(tItem,tObj).
genls(tItem,tSpatialThing).
genls(tObj,tSpatialThing).
genls(tPred,tRelation).
genls(tRegion,tSpatialThing).
genls(ttObjectType,tCol).
genls(ttSpatialType,tCol).
genls(tFunction,tRelation).
tPred(meta_argtypes).
meta_argtypes(aDirectionsFn(ftTerm,ftListFn(ftTerm))).
meta_argtypes(apathFn(tRegion,vtDirection)).
meta_argtypes(xyzFn(tRegion,ftInt,ftInt,ftInt)).

% :- set_prolog_flag(assert_attvars,true).

genls(ttTypeByAction,tCol).

% :-locally(set_prolog_flag(assert_attvars,true),ain(((ttTypeByAction(X) ==> tCol(X))))).

% (isa(Inst,Type),isa(Type,ttTypeByAction)) ==> isa(Inst,tHasAction).

ttTypeByAction(C),isa(I,C),{\+ is_in_world(I)} ==> \+ isa(I,C).

/*
% Produces actEat(String):- current_agent(Agent),agent_call_command(Agent,actEat(String)).
((vtActionTemplate(Compound)/(compound(Compound), 
    \+ current_predicate(_,Compound),
    arg(1,Compound,TC),
    functor(Compound,F,A),
    functor(Skel,F,A),
    arg(1,Compound,String)))
  ==> 
   ( (Skel 
       :- 
       current_agent(Agent),agent_call_command(Agent,Skel)),
    {nop((asserta_if_new((agent_call_command(Agent,Skel) 
       :- agent_coerce_for(mudPossess,TC,Agent,String,Obj),!,
          agent_call_command(Agent,Skel)))))})).
*/


genls(tAgent,tObj).
genls(tAgent,tSpatialThing).
genls(tItem,tObj).
genls(tItem,tSpatialThing).
genls(tObj,tSpatialThing).
genls(tPred,tRelation).
genls(tRegion,tSpatialThing).
genls(tFunction,tRelation).

genls(tCarryAble,tItem).
genls(tChargeAble,tItem).
genls(tContolDevice,tChargeAble).

genls(tDoor,tFurniture).
 % mdefault(genls(tDoor,tItem)).
genls(tDrinkAble,tItem).
genls(tEatAble,tItem).
genls(tFurniture,tObj).
genls(tFurniture,tPartofObj).
genls(tHumanControlled,tAgent).
genls(mobMonster,tAgentGeneric).
genls(tNpcAgent,tAgent).
genls(tPathway,tDoor).
genls(tUseAble,tItem).
genls(tWearAble,tItem).
genls(vtBasicDir,vtBasicDirPlusUpDown).
genls(vtBasicDirPlusUpDown,vtDirection).
genls(vtDirection,vtValue).

:- kb_shared(vtPosture/1).

tCol(vtPosture).
genls(vtPosture,vtVerb).


action_to_able(actSearch,tSearchAble).
action_to_able(actOperate,tOperateAble).
action_to_able(actObserve,tObserveAble).
(action_to_able(ACT,ABLE)==> ((argIsa(ACT,1,Type),isa(Type,ttObjectType))==> genls(Type,ABLE))).
  

genlInverse(mudContains,mudInsideOf).


arity(mudInsideOf,2).

vtBasicDir(vEast).
vtBasicDir(vNorth).
vtBasicDir(vSouth).
vtBasicDir(vWest).
vtBasicDirPlusUpDown(vDown).
vtBasicDirPlusUpDown(vUp).
%localityOfObject(Above,HasSurface):- mudLocOnSurface(Above,HasSurface).
%localityOfObject(Clothes,Agent):- mudSubPart(Agent,Clothes).
%localityOfObject(Inner,Container):- mudInsideOf(Inner,Container).
%localityOfObject(Inner,Outer):- only_if_pttp, localityOfObject(Inner,Container),localityOfObject(Container,Outer).
nameString(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).
meta_argtypes(mudMaterial(tSpatialThing,vtMaterial)).
meta_argtypes(mudTexture(tSpatialThing,vtTexture)).
meta_argtypes(mudWearing(tAgent,tWearAble)).
meta_argtypes(pathName(tRegion,vtDirection,ftString)).
meta_argtypes(resultIsa(tFunction,tCol)).
meta_argtypes(wasSuccess(tAgent,vtActionTemplate,ftBoolean)).
meta_argtypes(type_action_info(tCol,vtActionTemplate,ftText)).
%NEXT TODO predTypeMax(mudEnergy,tObj,130).
%NEXT TODO predTypeMax(mudHealth,tObj,500).

tCol(ttAgentType).

prologHybrid(pathDirLeadsTo(tRegion,vtDirection,tRegion)).




==> prologHybrid(mudAreaConnected(tRegion,tRegion),rtSymmetricBinaryPredicate).
rtArgsVerbatum(mudAreaConnected).

rtSymmetricBinaryPredicate(mudAreaConnected).

% :- sanity((listing(mudAreaConnected/2),!)),!.

ttAgentType(mobMonster).
% instTypeProps(apathFn(Region,_Dir),tPathway,[localityOfObject(Region)]).


==> ftSpec(vtActionTemplate).

disjointWith(tObj,tRegion).
disjointWith(tRegion,tObj).


ttTemporalType(tAgent).
ttTemporalType(tItem).
ttTemporalType(tObj).
ttTemporalType(tRegion).

tCol(tChannel).
tChannel(A):- tAgent(A).
tChannel(A):- tRegion(A).
tChannel(iGossupChannel).
%ttTypeFacet(tChannel).
:-ain_expanded(isa(tObj,ttTemporalType)).
:-ain_expanded(isa(tRegion,ttTemporalType)).

% cycAssert(A,B):- trace_or_throw(cycAssert(A,B)).
%:- install_constant_renamer_until_eof.
%:- set_prolog_flag_until_eof(do_renames,term_expansion).

% genls('SetOrCollection',tCol).
genls(ttSetOrCollection, tCol).
% genls('Collection',tCol).

meta_argtypes(verb_affordance(vtVerb,tTemporalThing,rtStatPred,ftChangeQuantity,ftChangeQuantity)).

prologHybrid(dividesBetween(tCol,tCol,tCol)).

% defined more correctly below dividesBetween(S,C1,C2) ==> (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

dividesBetween(tAgentGeneric,tAgent,tNonCorporialAgent).
dividesBetween(tAgent,tMale,tFemale).
dividesBetween(tAgent,tNpcAgent,tHumanControlled).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgent).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tTemporalThing,tObj,tRegion).
formatted_resultIsa(ftDiceFn(ftInt,ftInt,ftInt),ftInt).

isa(iDungeonMaster,tNonCorporialAgent).

genls(tNonCorporialAgent,tMassless).

isa(tRegion,ttTemporalType).

completelyAssertedCollection(tNonCorporialAgent).
completelyAssertedCollection(tAgentGeneric).
completelyAssertedCollection(tItem).
completelyAssertedCollection(tRegion).
completelyAssertedCollection(tObj).
completelyAssertedCollection(tAgent).
completelyAssertedCollection(tCarryAble).
completelyAssertedCollection(vtVerb).
genls(ttTypeByAction,completelyAssertedCollection).

arity(mudAreaConnected,2).


% ensure_some_pathBetween(R1,R2):- cwc,must((ain(pathDirLeadsTo(R1,aRelatedFn(vtDirection,R1,R2),R2)),ain(pathDirLeadsTo(R2,aRelatedFn(vtDirection,R2,R1),R1)))),!.

% mudAreaConnected(R1,R2)/ground(mudAreaConnected(R1,R2)) ==> isa(R1,tRegion),isa(R2,tRegion),mudAreaConnected(R2,R1).

(mudAreaConnected(R1,R2)/
 (ground(mudAreaConnected(R1,R2)),
   \+ pathDirLeadsTo(R1,_,R2),
  {sys_random_path_dir(Dir)},reverse_dir(Dir,Rev),\+ pathDirLeadsTo(R1,Dir,_NotR2), 
   \+ pathDirLeadsTo(R2,Rev,_NotR1))) ==>
  pathDirLeadsTo(R1,Dir,R2).
  

% pathDirLeadsTo(R1,Dir,_)==>tPathway(apathFnPLD(R1,Dir)).

% Is PathFn a total reifable function or a  partual unreifiable ?
rtPartialFunction(apathFn).
rtReifiableFunction(apathFn).

pathDirLeadsTo(R1,Dir,R2)/reverse_dir(Dir,Rev) ==> pathDirLeadsTo(R2,Rev,R1).
pathDirLeadsTo(R1,_,R2) ==> mudAreaConnected(R1,R2).

singleValuedInArg(pathDirLeadsTo,2).


% ==================================================
% Classes of things
% ==================================================


genls(tAgent,tObj).
genls(tItem,tObj).
genls(tClothing, tWashAble).
genls(tClothing, tWearAble).
genls(tFood,tEatAble).
genls(tFood, tItem).
genls(tClothing, tItem).

genls( tCarryAble, tItem).
genls( tCarryAble, tDropAble).

/*

tCol(genlsInheritable).
:-dynamic(genlsInheritable/1).

genlsInheritable(tCol).
genlsInheritable(ttRelationType).
:-must(ain((genls(ttTypeType,genlsInheritable)))).

:- dynamic(nearestIsa/2).
(genls(C,SC)/ground(genls(C,SC)),nearestIsa(SC,W),\+ genlsInheritable(W) )==>isa(C,W).

*/

% throw(sane_transitivity (genls( tCarryAble, tThrowAble))).
% genls( tCarryAble, tCarryAble).

genls(tPortableDevice,tCarryAble).

prologIsFlag(spatialInRegion/1).

:-do_gc.

genls(tClothing, tFoldAble).
genls(tClothing, tWearAble).

genls(tLiquidContainer, tDrinkAble).
genls(tLiquidContainer, tCarryAble).


genls(tFoldAble, tCarryAble).
% genls(tThrowAble, tCarryAble).
genls(tPortableDevice,tCarryAble).
genls(tPortableDevice,tPhysicalDevice).
genls(tPhysicalDevice,tUseAble).
genls(tWearAble, tCarryAble).
genls(tFood,tCarryAble).
genls(tCarryAble,tObj).
genls(tPartofObj,tNotTakAble).
genls(tBodyPart,tPartofObj).
genls(tSpatialThing,tLookAble).
genls(tFurnature,tOntoAble).
genls(tFurnature,tItem).

genls(tPartofFurnature,tPartofObj).






%(isa(I,Sub), disjointWith(Sub, Super)) ==> neg(isa(I,Super)).


 % mdefault(genls(tPartofObj,tItem)).


(sometimesBuggy,dividesBetween(S,C1,C2)/ground(v(S,C1,C2))) ==> (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

% disjointWith(P1,P2) ==> (not(isa(C,P1)) <==> isa(C,P2)).


isa(tRegion,ttSpatialType).
isa(tRelation,ttAbstractType).
==>typeProps(tTorso,[mudColor(isLikeFn(mudColor,tSkin)),mudShape(vUnique)]).
==>typeProps(tSkin,[mudColor(vUnique),mudShape(vUnique)]).

%Empty Location
% You *have* to use 0 as the id of the empty location. (no way!)
mudLabelTypeProps("--",tRegion,[]).

%NEXT TODO predTypeMax(mudEnergy,tAgent,120).

typeProps(tAgent,[predInstMax(mudHealth,500)]).
%genls('Indoors-IsolatedFromOutside',tRegion).
genls(tIndoorsIsolatedFromOutside, tRegion).
% genls('SpaceInAHOC',tRegion).
genls(tPlaceLikeSpaceInAHOC,tRegion).

:- if( \+ current_prolog_flag(address_bits, 32)).
%:- before_boot(set_prolog_stack_gb(16)).
:- endif.


% TOO SLOW 
typeProps(tAgent,[mudMoveDist(1)]).
% isRandom(vtBasicDir)
typeProps(tAgent,[predInstMax(mudHealth,500), predInstMax(mudEnergy,200), mudHealth(90), 
  % mudEnergy(90),  
  mudFacing(vNorth), mudAgentTurnnum(0), mudScore(1)]).
% typeProps(tAgent,mudLastCommand(actStand)).
% typeProps(tAgent,mudNeedsLook(vFalse)).

typeProps(tFood,[mudHeight(0)]).


% TOO STICKY ==>
typeProps(tItem,mudEnergy(140)).

% I am developing a Conflict learning system that works for full FOL .. it works to produces conflict duce_tru horn clauses(HC) heads (as well as normally what is derived from HCs) of cource if i tried to ground the Conflists it produce exponeticals so what i do is enure all conflicts can be found by backchaining at a depth of three 
typeProps(C,Ps)==> (isa(I,C)==>props(I,Ps)).

% typeProps(tAgent,[mudMemory(aDirectionsFn([vNorth,vSouth,vEast,vWest,vNE,vNW,vSE,vSW,vUp,vDown]))]).

%typeProps(tItem,mudListPrice(0)).
typeProps(tObj,mudOpaqueness(100)).
typeProps(tRegion,mudOpaqueness(1)).
% CRAZY typeProps(tSpatialThing,mudHeight(1)).

% :-end_module_type(dynamic).


pass3==> (mudLabelTypeProps(Lbl,Type,Props)/ground(typeHasGlyph(Type,Lbl))==> (typeHasGlyph(Type,Lbl) , typeProps(Type,Props))).

ps3:- cwc, with_mpred_trace_exec(ain(pass3)).

% Vacuum World example objects........
mudLabelTypeProps("wl",tWall,[mudHeight(3),mudWeight(4)]).

%TOO SLOW isa(I,SC)<=isa(I,C),genls(C,SC).

wearsClothing(A,I)==>tAgent(A),tClothing(I).


% The verb to relate (he relates, they relate, he related, it is related, he is relating) implies the universal relation.
rtBinaryPred(mudUniversallyRelated).

synonomousExternalContext(conceptuallyRelated,iInform7System,mudUniversallyRelated).

% The verb to incorporate (he incorporates, they incorporate, he incorporated, it is incorporated, he is incorporating) implies the incorporation relation. The verb to be part of implies the reversed incorporation relation.
tSpatialTanability(mudIncorporationRelation).
% The verb to provide (he provides, they provide, he provided, it is provided, he is providing) implies the provision relation.
tSpatialTanability(mudProducesRelation).  % CreationEvent
tSpatialTanability(mudSupportedBy).  % mudSupportedBy is not cycSupportedBy
tSpatialTanability(mudEnclosedBy). % contains-Underspecified 

meta_argtypes(inPermeates(tSolidTangibleThinh, tLiquidTangibleThing)).

tSpatialTanability(P)==>argsIsa(P,tTangible).
/*
wearsClothing(A,I) ==> (isa(I,IType),
   wornOnTypeType(IType,BType),
    hasBodyPart(A,BPart),
    isa(BPart,BType),
      wornOn(I,BPart)).
wornOn(I,BPart),hasBodypart(A,BPart) ==> wearsClothing(A,I).
wornOn(I,BPart),isa(BPart,BType),isa(I,IType) ==> wornOnTypeType(IType,BType).
*/



genls(tBread, tFood).

==>
 typeProps(tCrackers,
  [mudColor(vTan),tBread,
   mudShape(isEach(vCircular,vFlat)),
   mudSize(vSmall),
   mudTexture(isEach(vDry,vCoarse))]).

nonvar_must_be(V,G):- (var(V);G),!.

% TODO SPEED THIS UP 
% mudKeyword(I,Str)<= {(nonvar(I);nonvar(Str)), nonvar_must_be(I,\+tCol(I)), nonvar_must_be(Str,string(Str))}, isa(I,Type),mudKeyword(Type,Str).

pfc_slow((mudKeyword(Type,Str),tCol(Type),isa(I,Type)/(atom(I),ftID(I)) ==> mudKeyword(I,Str))).


action_info(C,_)==>vtActionTemplate(C).

completelyAssertedCollection(cachedPredicate).

rtArgsVerbatum(cachedPredicate).

ttRelationType(cachedPredicate).
cachedPredicate(P)/predicate_to_goal(P,Goal)==>{forall(call_u(Goal),ain(Goal))}.

cachedPredicate(vtActionTemplate(_)).


/*

% from inform7
prologHybrid(mudRelating(ftID,ftID)).
prologHybrid(mudProviding(ftID,ftID)).
prologHybrid(mudSupportsSpatially(ftID,ftID)).
prologHybrid(mudIncorporates(ftID,ftID)).
prologHybrid(mudEncloses(ftID,ftID)).
prologHybrid(mudContainment(ftID,ftID)).

An object has a text called printed name.
An object has a text called printed plural name.
An object has a text called an indefinite article.
An object can be plural-named or singular-named. An object is usually singular-named.
An object can be proper-named or improper-named. An object is usually improper-named.

A room can be privately-named or publically-named. A room is usually publically-named.
A room can be lighted or dark. A room is usually lighted.
A room can be visited or unvisited. A room is usually unvisited.
A room has a text called description.

Y [can] be C1 or C2.  
Y is [usually] C2.

A thing can be lit or unlit. A thing is usually unlit.
A thing can be edible or inedible. A thing is usually inedible.
A thing can be fixed in place or portable. A thing is usually portable.
A thing can be scenery.
A thing can be wearable.
A thing can be pushable between rooms.

The north is a direction.
The northeast is a direction.
The northwest is a direction.
The south is a direction.
The southeast is a direction.
The southwest is a direction.
The east is a direction.
A/sr1 - SR1 - Physical World Model S30 14
The west is a direction.
The up is a direction.
The down is a direction.
The inside is a direction.
The outside is a direction.
The north has opposite south. Understand "n" as north.
The northeast has opposite southwest. Understand "ne" as northeast.
The northwest has opposite southeast. Understand "nw" as northwest.
The south has opposite north. Understand "s" as south.
The southeast has opposite northwest. Understand "se" as southeast.
The southwest has opposite northeast. Understand "sw" as southwest.
The east has opposite west. Understand "e" as east.
The west has opposite east. Understand "w" as west.
Up has opposite down. Understand "u" as up.
Down has opposite up. Understand "d" as down.
Inside has opposite outside. Understand "in" as inside.
Outside has opposite inside. Understand "out" as outside.
The inside object translates into I6 as "in_obj".
The outside object translates into I6 as "out_obj".
The verb to be above implies the mapping up relation.
The verb to be mapped above implies the mapping up relation.
The verb to be below implies the mapping down relation.
The verb to be mapped below implies the mapping down relatio

A door has an object called other side.
The other side property translates into I6 as "door_to".
Leading-through relates one room (called the other side) to various doors.
The verb to be through implies the leading-through relation.
S33. Containers and supporters. The carrying capacity property is the exception to the remarks above
about the qualitative nature of the world model: here for the first and only time we have a value which can
be meaningfully compared.
Section SR1/6 - Containers
The specification of container is "Represents something into which portable
things can be put, such as a teachest or a handbag. Something with a really
large immobile interior, such as the Albert Hall, had better be a room
instead."
A container can be enterable.
A container can be opaque or transparent. A container is usually opaque.
A container has a number called carrying capacity.
The carrying capacity of a container is usually 100.
Include (- has container, -) when defining a container

The specification of supporter is "Represents a surface on which things can be
placed, such as a table."
A supporter can be enterable.
A supporter has a number called carrying capacity.
The carrying capacity of a supporter is usually 100.
A supporter is usually fixed in place.
Include (-
has transparent supporter
-) when defining a supporter

A door can be open or closed. A door is usually closed.
A door can be openable or unopenable. A door is usually openable.
A container can be open or closed. A container is usually open.
A container can be openable or unopenable. A container is usually unopenable.

Before rules is a rulebook. [20]
Instead rules is a rulebook. [21]
Check rules is a rulebook. [22]
Carry out rules is a rulebook. [23]
After rules is a rulebook. [24]
Report rules is a rulebook. [25]

Action-processing rules is a rulebook. [10]
The action-processing rulebook has a person called the actor.
Setting action variables is a rulebook. [11]
The specific action-processing rules is a rulebook. [12]
The specific action-processing rulebook has a truth state called action in world.
The specific action-processing rulebook has a truth state called action keeping silent.
The specific action-processing rulebook has a rulebook called specific check rulebook.
The specific action-processing rulebook has a rulebook called specific carry out rulebook.
The specific action-processing rulebook has a rulebook called specific report rulebook.
The specific action-processing rulebook has a truth state called within the player''s sight.
The player''s action awareness rules is a rulebook. [13]
S16. The rules on accessibility and visibility, which control whether an action is physically possible, have
named outcomes as a taste of syntactic sugar.
Accessibility rules is a rulebook. [14]
Reaching inside rules is an object-based rulebook. [15]
Reaching inside rules have outcomes allow access (success) and deny access (failure).
Reaching outside rules is an object-based rulebook. [16]
Reaching outside rules have outcomes allow access (success) and deny access (failure).
Visibility rules is a rulebook. [17]
Visibility rules have outcomes there is sufficient light (failure) and there is
insufficient light (success).
S17. Two rulebooks govern the processing of asking other people to carry out actions:
Persuasion rules is a rulebook. [18]
Persuasion rules have outcomes persuasion succeeds (success) and persuasion fails (failure).
Unsuccessful attempt by is a rulebook. [19]

*/


:- dynamic(baseKB:mudNeedsLook/2).
:- export(baseKB:mudNeedsLook/2).
:- pfc_lib:import(baseKB:mudNeedsLook/2).


prologBuiltin(onEachLoad/0).
rtArgsVerbatum(onEachLoad).
rtArgsVerbatum(must).

ttRelationType(rtStatPred).

prologHybrid(normalAgentGoal(rtStatPred,ftTerm)).

((rtRolePredicate(Pred)==>
  ((relationMostInstance(Pred,Type,Value))==>
    ((isa(X,Type))==>mdefault(t(Pred,X,Value)))))).


:- ain((relationMostInstance(Pred,Type,Value))==>
 ((isa(X,Type))==>mdefault(t(Pred,X,Value)))).


(rtStatPred(Pred)/must(atom(Pred))==>(
    arity(Pred,2),
    rtRolePredicate(Pred),
    singleValuedInArg(Pred,2),
    rtBinaryPredicate(Pred))).

(((normalAgentGoal(Pred,N)/must(atom(Pred)) ==>
 ({kb_shared(Pred/2),
   AT=..[Pred,tAgent,ftPercent]},
   rtStatPred(Pred),
   meta_argtypes(AT),
   relationMostInstance(Pred,tAgent,N))))).

normalAgentGoal(mudEnergy,140).
normalAgentGoal(mudHealth,90).
normalAgentGoal(mudHygiene,90).

normalAgentGoal(mudNonHunger,90).
normalAgentGoal(mudBladderEmpty,90).
normalAgentGoal(mudSecureRoom,90).
normalAgentGoal(mudFun,90).
normalAgentGoal(mudNonLonelinessSocial,90).
normalAgentGoal(mudSadToHappy,90).
normalAgentGoal(mudComfort,90).

% TO SLOW typeProps(tAgent,[mudStr(2),mudHeight(2),mudStm(2),mudSpd(2)]).

%normalAgentGoal(Pred,Val)==>  (tAgent(A)==>agentGoals(A,Pred,((t(Pred,A,V),V>=Val)))).
%agentGoals(A,About,State)/State ==> \+ agentTODO(A,actImprove(About)).

prologHybrid(on_command_show(tAgent,vtActionTemplate,ftTerm)).
prologHybrid(agentTODO(tAgent,vtActionTemplate)).
prologHybrid(agentGOAL(tAgent,ftAssertable)).

normalAgentGoal(Pred,Val) ==>  ( t(Pred,A,V)/(V<Val) ==> agentTODO(A,actImprove(Pred))).
normalAgentGoal(Pred,Val) ==>  ( t(Pred,A,V)/(V<Val) ==> agentGOAL(A,t(Pred,A,Val))).

:-ain((normalAgentGoal(Pred,Val) ==>  ( t(Pred,A,V)/(V<Val) ==> agentGOAL(A,t(Pred,A,Val))))).
% :-mpred_fwd((normalAgentGoal(Pred,Val) ==>  ( t(Pred,A,V)/(V<Val) ==> agentGOAL(A,t(Pred,A,Val))))).
normalAgentGoal(Pred,Val)==>  (tAgent(A)==>mdefault(t(Pred,A,Val))).


genls(tRoom,tRegion).

tAgent(iExplorer7).

mud_listing(M):- cwc, xlisting((M,-completely_expanded,-'$spft',-'$nt',-'$pt',- (==>))).

% :- mud_listing(iExplorer7).

==> onQueueEmpty(must(mudFun(iExplorer7,_W))).

vtActionTemplate(actImprove(rtStatPred)).

%:- listing(vtActionTemplate/1).

% check to make sure the canonicalizer left the compound..
==> onQueueEmpty(sanity(clause(baseKB:vtActionTemplate(actImprove(rtStatPred)),true))).
% instead of replacing with..
==> onQueueEmpty(sanity( \+ clause(baseKB:vtActionTemplate(actImprove),true))).


 
/*

% :- set_prolog_flag(dialect_pfc,cwc).
:- notrace(kif_to_boxlog(((parent('$VAR'('G'),'$VAR'('P')) & parent('$VAR'('P'),'$VAR'('C'))) => grandparent('$VAR'('G'),'$VAR'('C'))),O)),dmsg(O).

 the CycL language extends Prolog''s first order logic capabilities with some higher order logics.  
 It also extrends prolog to show proofs.. one issue is the CycL language never signed up for cuts or other execution  orders.    
 PrologMUD extends the CycL language to allow preset program flow (unless a predicate is declared to not honor order of execution 
  (this is usually best!)).  PrologMUD  implements a new design of the cyc canonicalizer..   

 usually in Cyc the rules "(implies (and Axy Byz) (and Cwxyz Dwx))" are converted to DNF (Correct me if I am wrong.. 
 since i have heard it uses ConjuntiveNormalForm as well) ... the DNF generates Clausal forms..  The forms choosen 



?-  kif_to_boxlog(((parent('$VAR'('G'),'$VAR'('P')) & parent('$VAR'('P'),'$VAR'('C'))) => grandparent('$VAR'('G'),'$VAR'('C'))),O),dmsg(O).

O = [ (-parent(G, P):- -grandparent(G, C), parent(P, C)), 
      (-parent(P, C):- -grandparent(G, C), parent(G, P)), 
      (grandparent(G, C):-parent(G, P), parent(P, C))].


?- kif_to_boxlog( (grandparent('$VAR'('G'),'$VAR'('C')) => exists('$VAR'('P'), (parent('$VAR'('G'),'$VAR'('P')) & parent('$VAR'('P'),'$VAR'('C'))))),O).

    (-grandparent(G, C):- mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)), (-parent(G, P) ; -parent(P, C))),   % You have proven G is not the grandparent of C when you have proven tha G has no children or that C has no parents
    (-mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)):- grandparent(G, C), (-parent(G, P) ; -parent(P, C))), 
    (parent(G, P):-grandparent(G, C), mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G))), % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P
    (parent(P, C):-grandparent(G, C), mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G))),
    (mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)):- grandparent(G, C),  \+ (parent(G, P) , parent(P, C)))]  % We failed to find a true P


O = [ 
      (-grandparent(G, P):- -parent(G, _P) ; -parent(_P, P)),    
      parent(G, P):- grandparent(G, C), parent(P,C),   % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P
      parent(P, C):- grandparent(G, C), parent(G,P))].   % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P

*/

/*

(((meta_argtypes(Types)/
 (functor(Types,F,A), A >1, functor(Matcher,F,A),arity(F,A)))
  ==> 
    ((Matcher ==> {between(1,A,N),arg(N,Matcher,I),arg(N,Types,T),ground(I:T)},\+ttExpressionType(T),isa(I,T),{dmsg(isa(I,T))})))).

((argQuotedIsa(Pred, _, 'CycLSentence') ==> 'SententialOperator'(Pred))).

*/

 % :- set_prolog_flag(dialect_pfc,cwc).


% :- time(must(ain_expanded(prologSingleValued(mudFacing666(tObj,vtDirection),[relationMostInstance(tObj,vNorth)],prologHybrid)))).

% :- profile((ain_expanded(prologSingleValued(mudFacing666(tObj,vtDirection),[relationMostInstance(tObj,vNorth)],prologHybrid)))).
