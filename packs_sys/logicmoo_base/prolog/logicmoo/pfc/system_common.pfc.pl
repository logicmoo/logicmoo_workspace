%:- module(system_common,[]).
%:- set_module(class(development)).
:- '$set_source_module'(baseKB).
%:- use_module(library(pfc)).
:- expects_dialect(pfc).

/** <module> system_common
% =============================================
% File 'system_common.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% pain(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~ P =         test nesc false                        assert(~P),retract(P), disable(P)
%
%   ~ ~(P) =         test possible (via not impossible)      retract( ~(P)), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract( ~(P))
%   ~ \+(P) =        same as P                               same as P
%     \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles
*/

% :- require('system_base.pfc').

% :- autoload.
:- mpred_unload_file.
:- style_check(-discontiguous).

:- kb_shared(startup_option/2).

:- meta_predicate(without_depth_limit(0)).
without_depth_limit(G):- call_with_depth_limit(G,72057594037927935,Result),sanity(Result\==depth_limit_exceeded).

/*
without_depth_limit(G):-
   ('$depth_limit'(72057594037927935,Was,_), 
    (Was == -1 -> call(G);  % Not inside cwdl
    (Was > 72000000000000000 -> call(G);  % We left Depth limit slightly messed
      call_cleanup(G,'$depth_limit'(Was,_,_))))).
*/


%:- use_module(library(rtrace)).
:- mpred_unload_file.
:- begin_pfc.
% :- '$set_source_module'(baseKB).
% :- prolog_load_context(module,Mod),sanity(Mod==baseKB),writeq(prolog_load_context(module,Mod)),nl.

:- ensure_abox(baseKB).




completelyAssertedCollection(tInferInstanceFromArgType).
completelyAssertedCollection(ttNotTemporalType).



%% prologNegByFailure( ?VALUE1) is semidet.
%
% Prolog Negated By Failure.
%
prologNegByFailure(prologNegByFailure).
prologNegByFailure(quotedIsa/3).
~prologNegByFailure(isa/2).

% :- listing( ~ / 1).


/*
==>(someTimesBuggy2ndOrder,
((someTimesBuggy2ndOrder,genlPreds(C1,C2),arity(C1,2)) ==>
  {P1 =.. [C1,X,Y],
    P2 =.. [C2,X,Y]},
  clif(P1 => P2))).

someTimesBuggy2ndOrder ==>
((genlPreds(C1,C2),arity(C1,3)) ==>
  {P1 =.. [C1,X,Y,Z],
    P2 =.. [C2,X,Y,Z]},
  clif(P1 => P2)).

*/
% tSet(C)/(atom(C),TCI=..[C,I]) ==> (arity(C,1),mpred_univ(C,I,TCI), ... )


% :- prolog.
% tPred


==> completelyAssertedCollection(isEach(tCol,tPred,pfcControlled)).

ttRelationType(C)==>completelyAssertedCollection(C).

% ~genls(meta_argtypes,ftSpec).

:- dynamic(baseKB:mudWielding/2).

prologMultiValued(P)==> \+ prologSingleValued(P).
prologSingleValued(P)==> \+ prologMultiValued(P).

~(ttExpressionType(prologEquality)).
ttRelationType(prologEquality).
prologEquality(mudEquals/2).
prologEquality(('=')/2).
prologEquality(('==')/2).

arity(',',2).

:- nortrace.


~(isa((','), prologEquality)).

==>tSet(isEach(tCol,tPred,pfcControlled)).

rtArgsVerbatum(meta_argtypes).
rtArgsVerbatum(functorIsMacro).
rtArgsVerbatum(functorDeclares).

pfcControlled(genlPreds).
pfcControlled(isa).
pfcControlled(argIsa).


%tCol(X) ==> isa(X,tCol).
%tCol(X) ==> ruleRewrite(t(X,I),isa(I,X)).

==>( specialized_conflict_tests ==> (( ~tCol(T),tCol(T)) ==> conflict(~tCol(T)))).

%typeProps(tCoffeeCup,[mudColor(vBlack),mudColor(isEach(vBrown,vBlack)),mudSize(vSmall),mudShape(vCuplike),mudMaterial(vGlass),mudTexture(vSmooth)]).
%props(iCoffeeCup7,[mudColor(vBlack),mudColor(isEach(vBrown,vBlack)),mudSize(vSmall),mudShape(vCuplike),mudMaterial(vGlass),mudTexture(vSmooth)]).

:- sanity(get_lang(pfc)).

% tCol(C)/(\+ never_isa_syntax(C))==>{decl_as_isa(C)}.

%underkill - Though it is making bad things happen 
ttExpressionType(C)==> \+ completelyAssertedCollection(C).

   % :- mpred_trace_exec.
  ==> prologHybrid(isLoaded(tMicrotheory),pfcControlled).
  tSet(isLoaded).
  :- mpred_notrace_exec.

==> prologHybrid(isLoadedType(ttModuleType),pfcControlled).


:- kb_shared((onSpawn)/1).
 	 	 

%% onSpawn( :TermA) is semidet.
%
% Whenever Spawn.
%

:- kb_shared((onSpawn)/1).

rtArgsVerbatum(onSpawn).

onSpawn(When==>State)/nonvar(State) ==> ( When ==> onSpawn(State)).
onSpawn(State)/mpred_literal(State) ==> {addSpawn(State)}.


%:-ain(((ttModuleType(ModType),isa(Thing,ModType),isLoaded(Thing), \+ ttExpressionType(ModType) ==> isLoadedType(ModType)))).
%==>(((onSpawn(Idea)==> ((isLoadedType(tSourceData),isRuntime) ==> {ain_expanded(Idea,())})))).

onAlways(Idea)==>Idea.

onStart(Idea)/definitional(Idea) ==> onAlways(Idea).
((onStart(Idea)/ ( \+ definitional(Idea))) ==> 
  (isRuntime ==> {get_startup_uu(UU),ain_expanded(Idea,UU)})).

==>pfcControlled(prologArity(tRelation,ftInt)).
==>pfcControlled(isa(ftTerm,tCol)).

tSet(tSet).
tSet(tCol).
tFixedArityRelation(tSet).
tFixedArityRelation(tCol).
ttRelationType(prologHybrid).

:- check_clause_counts.

%:- rtrace,trace.
%:- notrace, nortrace.



prologHybrid(mudToCyc(ftTerm,ftTerm)).

:- sanity(arity(mudToCyc,2)).

% col_as_isa(X)==>tFixedArityRelation(X),arity(X,1).
col_as_unary(X)==>tFixedArityRelation(X),arity(X,1).

:- kb_shared(ttExpressionType/1).

tSet(ttExpressionType).
tSet(completelyAssertedCollection).

ttExpressionType(C) ==> ( \+ completelyAssertedCollection(C), \+ tSet(C), ~ tSet(C), tCol(C)).


:- sanity(get_lang(pfc)).
%WEIRD ~(tCol(C))/completelyAssertedCollection(C)==> \+ completelyAssertedCollection(C).
% EASIER
% ~tCol(C) ==> ~completelyAssertedCollection(C).
% (tCol(C),\+ ttExpressionType(C)) ==> tSet(C).
% ((tCol(P), \+ ttExpressionType(P)) <==> tSet(P)).

/*

@TODO BASE DOES THIS

tSet(C) ==>
({((
  % 
  % dynamic(C/1),
  % wdmsg(c_tSet(C)),  
  %call((shouldnt_be_set(C) -> (show_failure(mpred_why(tSet(C))),
  %   show_failure(mpred_why(tCol(C))),break) ; true)),
  must(atom(C)),
  %\+ ttExpressionType(C),
  ( \+ is_static_predicate(C/1)),
  functor(Head,C,1),
  call(BHead=baseKB:Head),
  ( \+(predicate_property(BHead,_))-> kb_shared(C/1); true),
  baseKB:export(baseKB:C/1),
  %mpred_type_isa:import(baseKB:C/1),
  nop(predicate_property(BHead,dynamic)->true;show_pred_info(BHead))))},
  functorDeclares(C),
  pfcControlled(C),
  \+ ttExpressionType(C),
  tCol(C),
  arity(C,1)).
*/


/*
tSet(C)==>
 ( {atom(C), functor(Head,C,1), call(BHead=baseKB:Head),
   ( \+(predicate_property(BHead,_))-> kb_shared(C/1); true),
    nop(predicate_property(BHead,dynamic)->true;show_pred_info(BHead))},
   functorDeclares(C),
   pfcControlled(C),
   arity(C,1)).
*/


/*
tSet(C)/(atom(C),TCI=..[C,I]) ==> (arity(C,1),
 % mpred_univ(C,I,TCI),
 {call_u((decl_type(C), 
  ignore((
   \+ is_static_predicate(C/1),
   kb_shared(C/1),
   \+ completelyAssertedCollection(C),
   call_u(ain((
   ((TCI :- 
    ((cwc, call_u((
      predicate_property(TCI,number_of_rules(1)),
    lazy(( \+ call_u(~(TCI)))),
    isa_backchaing(I,C))))))))))))))}).


*/
/*
ttExpressionType(P) ==> 
 {get_functor(P,F), functor(Head,F,1), call(BHead=baseKB:Head),
  call((\+ predicate_property(BHead,defined) -> kb_shared(F/1); true)),
  call((predicate_property(BHead,dynamic)->(ain(Head==>{ignore(retract(Head))}));show_pred_info(BHead)))},
  ~functorIsMacro(F),
  ~functorDeclares(F),
  ~tSet(F),
  notAssertibleCollection(F),
  tCol(F),
  completelyDecidableCollection(F),
  arity(F,1).
*/

% % :- mpred_trace_exec.

tSet(tKnownID).
% :- xlisting(tKnownID).
%?- isa(tKnownID,W).
%:- break.

:- mpred_notrace_exec.

% (tInferInstanceFromArgType(Col),tCol(Col)/i_name('',Col,ColName),tPred(Prop)/i_name('',Prop,PropName),{ColName=PropName}==> tInferInstanceFromArgType(Prop)).

% (tInferInstanceFromArgType(Prop),tPred(Prop),arity(Prop,N)/(N>1) ==> ({i_name('vt',Prop,FinalType)},tCol(FinalType),tInferInstanceFromArgType(FinalType),argIsa(Prop,N,FinalType))).

prologSideEffects(write/1).
prologSideEffects(resolveConflict/1).



/*
((hybrid_support(F,A)/(is_ftNameArity(F,A), \+ prologDynamic(F),\+ is_static_predicate(F/A))) ==>
  ({    
    functor(G,F,A),
     (var(M)->must(defaultAssertMt(M));true),
     (var(M)->ignore(( current_predicate(F,M:G), \+ predicate_property(M:G,imported_from(_))));true),
     (var(M)->predicate_property(M:G,exported);true),
     % must(rebuild_pred_into(G,G,ain,[+dynamic,+multifile,+discontiguous])),         
     % (predicate_property(M:G,dynamic)->true;must(convert_to_dynamic(M,F,A))),
     kb_shared(M:F/A),
     show_failure(hybrid_support, \+ is_static_predicate(F/A))}),
     prologHybrid(F),
    arity(F,A)).
*/


arity(functorIsMacro,1).

functorIsMacro(functorIsMacro).
ttRelationType(X)==>functorDeclares(X).
% tCol(X)==>functorDeclares(X).
% functorDeclares(X)==>tCol(X).
% functorIsMacro(X)==>functorDeclares(X).
functorIsMacro(pddlSomethingIsa/2).
tPred(pddlSomethingIsa(ftTerm,ftListFn(tCol))).

/*
prologBuiltin(A) :- cwc,head_to_functor_name(A, B),prologBuiltin(B).
prologBuiltin(P) :- cwc,is_ftCompound(P),!,get_functor(P,F,A),functor(C,F,A),(predicate_property(C,built_in)). % predicate_property(P,static)).
ttRelationType(PT)==> {atom(PT),H=..[PT,I]}, (H:-cwc,head_to_functor_name(I,F),call_u(call(PT,F))).
*/


tCol(iExplorer4)==>{trace_or_throw(never_tCol(iExplorer4))}.

==> isa(pddlSomethingIsa/2, prologHybrid).

arity(argIsa,3).


% prologHybrid(F/A)/(atom(F),number(A)) ==> arity(F,A),{must(dynamic_safe(F/A))}.

%:-mpred_trace_exec.

% Functions
hardCodedExpansion ==> ((tFunction(ArgTypes)/is_declarations(ArgTypes)) ==> meta_argtypes(ArgTypes),{get_functor(ArgTypes,F)}, tFunction(F)).
% FormatTypes
hardCodedExpansion ==> (ttExpressionType(ArgTypes)/is_declarations(ArgTypes) ==> meta_argtypes(ArgTypes)).

argIsa(completeExtentAsserted,1,tPred).

((meta_argtypes(ArgTypes)/sanity(is_ftCompound(ArgTypes))) ==> 
   ({get_functor(ArgTypes,F,A),A>1},arity(F,A),{arg(N,ArgTypes,Type)},argIsa(F,N,Type))).


meta_argtypes(argIsa(tRelation,ftInt,tCol)).

:- mpred_run.
:- mpred_notrace_exec.

/* 

@ TODO BASE DOES THIS
tSet(rtNotForUnboundPredicates).
ttRelationType(rtNotForUnboundPredicates).

functorIsMacro(tCol).


tCol(tCol).
tCol(tSet).

rtArgsVerbatum(meta_argtypes).
rtArgsVerbatum(functorIsMacro).
rtArgsVerbatum(functorDeclares).
tCol(prologMultiValued).
tCol(prologSingleValued).
tCol(tFunction).
tCol(tInferInstanceFromArgType).
tCol(tPred).
tCol(tRelation).
tCol(ttSpatialType).
ttTypeType(ttTypeType).
ttTypeType(C)==>tSet(C).
ttTypeType(ttAgentType).
*/

:- sanity(tSet(ttAgentType)).

tCol(tWorld).
completelyAssertedCollection(tInferInstanceFromArgType).
completelyAssertedCollection(ttNotTemporalType).


/* 

@ TODO BASE DOES THIS

completelyAssertedCollection(prologSingleValued).
completelyAssertedCollection(tCol).
completelyAssertedCollection(ttExpressionType).
completelyAssertedCollection(ttValueType).
completelyAssertedCollection(ttTemporalType).
completelyAssertedCollection(tRelation).
completelyAssertedCollection(tPred).

% completelyAssertedCollection(C)==>completeExtentAsserted(C).

% :- throw(this_file).disjointWith(Sub, Super) ==> disjointWith( Super, Sub).

ttRelationType(rtSymmetricBinaryPredicate).



tSet(rtNotForUnboundPredicates).

*/

completelyAssertedCollection(completeExtentAsserted).
completeExtentAsserted(completelyAssertedCollection).
completelyAssertedCollection(completelyAssertedCollection).
completeExtentAsserted(functorDeclares).
completeExtentAsserted(completeExtentAsserted).
arity(completeExtentAsserted,1).

% tSet(completeExtentAsserted).
argIsa(completeExtentAsserted,1,tPred).
meta_argtypes(genlPreds(tPred,tPred)).
:- must_det(argIsa(genlPreds,2,_)).
completeExtentAsserted(defnSufficient).


:- kb_shared(ttNotTemporalType/1).
ttNotTemporalType(ftInt).
%ttNotTemporalType(ftTerm).
ttNotTemporalType(tCol).
ttNotTemporalType(ttExpressionType).
ttNotTemporalType(ttValueType).

==>ttNotTemporalType(tCol).
ttNotTemporalType(T)==>tCol(T).
==>ttTemporalType(tTemporalThing).
ttTemporalType(T)==>tCol(T).

arity(argQuoted,1).







(ttExpressionType(FT),{is_ftCompound(FT)})==>meta_argtypes(FT).

tSet(vtDirection).

:- sanity(get_lang(pfc)).

disjointWith(tPred,tFunction).

disjointWith(ttTemporalType,ttAbstractType).


prologSideEffects(P)==>rtNotForUnboundPredicates(P).

isa(tRelation,ttAbstractType).




:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).

:- reconsult(pack(logicmoo_base/t/examples/pfc/'neg_sanity.pfc')).


:- endif. % load_time_sanity


%P/(is_ftNonvar(P),get_functor(P,F)),afterAdding(F,Do)/is_ftNonvar(Do)==>{call(Do,P)}.
%~P/(is_ftNonvar(P),get_functor(P,F)),afterRemoving(F,Do)/is_ftNonvar(Do)==>{call(Do,P)}.




%:-rtrace.
% (tCol(Inst), {isa_from_morphology(Inst,Type)}) ==> (isa(Inst,Type)).

% HOW TO MAKE THIS FAST?  isa(Inst,Type) <= {isa_from_morphology(Inst,Type)}.

%((disjointWith(P1,P2) , genls(C1,P1), {dif:dif(C1,P1)}) ==>    disjointWith(C1,P2)).
% (disjointWith(C1,P2) <= (genls(C1,P1), {dif:dif(C1,P1)}, disjointWith(P1,P2))).

tSet(completelyAssertedCollection).
rtArgsVerbatum(completeIsaAsserted).
% BAD genls(completeIsaAsserted,tTemporalThing).
%assumed genls(A,B)==>(arity(A,1),arity(B,1)).
%assumed genls(completelyAssertedCollection,tCol).
/*
completelyAssertedCollection(completelyAssertedCollection).
completelyAssertedCollection(tPred).
completelyAssertedCollection(tRelation).
completelyAssertedCollection(ttExpressionType).
completelyAssertedCollection(tCol).
completelyAssertedCollection(functorIsMacro).
% completelyAssertedCollection(functorDeclares).
completelyAssertedCollection(ttRelationType).
completelyAssertedCollection(completelyAssertedCollection).
*/
% dividesBetween(S,C1,C2) ==> (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

% disjointWith(P1,P2) ==> ((~(isa(C,P1))) <==> isa(C,P2)).

% isa(Col1, ttObjectType) ==> ~(isa(Col1, ttExpressionType)).


% tCol(ArgsIsa):-ttRelationType(ArgsIsa).
% TODO decide if OK
%tCol(F):-t(functorDeclares,F).
tSet(ttExpressionType).



%assumed genls(ttSpatialType,ttTemporalType).
%assumed genls(tSpatialThing,tTemporalThing).




% remove conflicts early 
% (~(P)/mpred_non_neg_literal(P) ==> ( {mpred_remove(P)}, (\+P ))).

==>tCol(rtAvoidForwardChain, comment("rtAvoidForwardChain means that backchain is required for subclasses 
     to gain membership TODO: Give example "
     )).

tCol('tThing').
arity('tThing',1).
% genls(ttExpressionType,rtAvoidForwardChain).
isa('tThing',rtAvoidForwardChain).


%isa('CycLTerm',rtAvoidForwardChain).
prologHybrid(quotedIsa(ftTerm,ttExpressionType)).

:- kb_shared(quotedIsa/2).

isa_or_type(X,Y):- cwc, quotedIsa(X,Y).
isa_or_type(X,Y):- cwc, isa(X,Y).

/*
  ftSpec
  tCol
 ttFormatType | tCol
*/
:- kb_shared(mainClass/2).

mainClass(I,C)==>isa(I,C).

not_isa(I,C):- cwc, mainClass(I,MC),disjointWith(MC,DC),genls(C,DC).

% isa(I,C):- cwc, is_ftNonvar(C),ttExpressionType(C),!,quotedIsa(I,C).
%isa(I,C):- cwc, tCol(C),(ttExpressionType(C)*->quotedIsa(I,C);loop_check(isa_backchaing(I,C))).
%isa(I,C):- cwc, tSet(C),(ttExpressionType(C)*->quotedIsa(I,C)).
% isa(I,C):- cwc, when(?=(I,C),\+ clause_b(isa(I,C))), (loop_check(visit_pred(I,C))*->true;loop_check(no_repeats(isa_backchaing(I,C)))).
%isa(I,C):- cwc, loop_check(visit_pred(I,C)).
%isa(I,C):- cwc, loop_check(visit_isa(I,C)).

isa([tIndividual(tSack)],C):-C==ftListFn(ftAskable),!.
isa(iExplorer2,C):- C==rtArgsVerbatum,!,fail.
:- asserta((isa(I,C):- ground(I:C),not_isa(I,C),!,fail)).
% isa(I,C):- cwc, no_repeats(loop_check(isa_backchaing(I,C))).
% isa(_,C):- nonvar(C),\+ tSet(C),!,fail.

quotedIsa(_,C):- nonvar(C), tSet(C),!,fail.
quotedIsa(I,C):- cwc, loop_check(term_is_ft(I,C)).

dif_in_arg(P,N,Q):- cwc, ground(P),P=..[F|ARGS],arg(N,P,B),Q=..[F|ARGS],nb_setarg(N,Q,A),dif(A,B).

tSet(ttSpatialType).
tSet(tSpatialThing).
completelyAssertedCollection(tCol).



:- kb_shared(isa/2).

% ttRelationType(Prop)==>tCol(Prop).

:- forall(between(1,12,N),kb_shared(proven_holds_t/N)).
:- forall(between(1,12,N),kb_shared(proven_not_holds_t/N)).
:- forall(between(1,12,N),kb_shared(proven_poss_t/N)).
:- forall(between(1,12,N),kb_shared(proven_not_poss_t/N)).

%:-baseKB:agenda_slow_op_enqueue(ain(((arity(Pred,2),argIsa(Pred,1,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tCol(Col)), \+prologSideEffects(Pred), t(Pred,Arg,_)/is_ftNonvar(Arg)) ==> t(Col,Arg)))).
%:-baseKB:agenda_slow_op_enqueue(ain(((arity(Pred,2),argIsa(Pred,2,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tCol(Col)), \+prologSideEffects(Pred), t(Pred,_,Arg)/is_ftNonvar(Arg)) ==> t(Col,Arg)))).
%:-add_slow(((arity(Pred,2),argIsa(Pred,2,Col)/(is_ftNonvar(Pred),Col\=ftTerm,tCol(Col)),t(Pred,_,Arg)/is_ftNonvar(Arg)) ==> t(Col,Arg))).

%(((P/(has_functor(P),get_functor(P,F,A),A\=2,\+prologSideEffects(F),mpred_literal(P)) ==> {baseKB:agenda_slow_op_enqueue(deduceEachArgType(P))}))).

% :-rtrace.
     

%:- ensure_loaded('system_domains.pfc').


/*
:- ain(((vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) ==> vtActionTemplate(ArgTypes)))).
:- ain(((baseKB:action_info(ArgTypes,_)/is_declarations(ArgTypes) ==> vtActionTemplate(ArgTypes)))).
:- ain(((functorIsMacro(Compound)/compound_functor(Compound,F)) ==> functorDeclares(F))).
hardCodedExpansion ==> ((ttExpressionType(FT)/is_declarations(FT))==>meta_argtypes(FT)).


*/

disjointWith(tCol,tIndividual).
% :- noguitracer.
%:- rtrace.
/*
codeTooSlow ==>
((  arity(F,A)/(atom(F),\+ is_sentence_functor(F),number(A),A>1,A<10,functor(P,F,A),\+ rtLogicalConnective(F)), 
  \+ meta_argtypes_guessed(P),   
   (argIsa(F,A,NOTFT)/NOTFT\==ftTerm),
   (argIsa(F,1,NOTFT2)/NOTFT2\==ftTerm),
 {generateArgVars(P, argIsa(F), '='(_))}
==> meta_argtypes_guessed(P))).
*/
meta_argtypes_guessed(P)==>meta_argtypes(P).
   
:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).


% :- if_startup_script(locally_tl(pfcExpansion,ensure_loaded(mpred_i_mpred_mpred_testing))).

% :-asserta(baseKB:isa_pred_now_locked).


% :-loadTinyAssertions1.

%:-prolog_repl.
%:-showTinyAssertions.
%:-prolog_repl.
%:-loadTinyAssertions2.


:- endif.

:- meta_predicate(~(0)).
:- kb_shared(~(0)).

% pddlObjects(Type,EList)==>  isa(isEach(EList),Type).
% pddlSorts(Type,EList)==> genls(isEach(EList),Type).


:- kb_shared(argIsa/3).


/*
prologBuiltin(col_arity/2).
col_arity(Spec,A):-arity(Spec,A),!.
col_arity(Spec,A):-atom(Spec),!,A=1.
col_arity(Spec,A):-compound(Spec),functor(Spec,_,A).
isa(Spec,tCol)/col_arity(Spec,A) ==> arity(Spec,A).
*/

% :-ain((mpred_isa(I,C)==>{ain((isa(I,tPred),mpred_isa(I,C),props(I,[C])))})).
% :-ain((t(C,I)==>{ /* retract(hasInstance_dyn(C,I)), */ ain((isa(I,C))) , ain((props(I,C)))})).


% :-include('mpred_header.pi').
tSet(tPred).

:- sanity(assert_argIsa(tPred,1,tPred)).
:- sanity(ain(argIsa(tPred,1,tPred))).


/*
% reflexive equality
equal(A,B) ==> equal(B,A).
equal(A,B),{ \+ (A=B}),equal(B,C),{ \+ (A=C)} ==> equal(A,C).

notequal(A,B) ==> notequal(B,A).
equal(A,C),notequal(A,B) ==> notequal(C,B).
*/

:- dynamic(either/2).
% is this how to define constraints?
% either(P,Q) ==> (~(P) ==> Q), (~(Q) ==> P).
(either(P,Q) ==> ((~(P) <==> Q), (~(Q) <==> P))).
% ((P,Q ==> false) ==> (P ==> ~(Q)), (Q ==> ~(P))).


:-  /**/ export(member/2).
:-  /**/ export(arg/3).
%:-  /**/ export(call_u/1).
% prologDynamic(cycAssert/2).
:-  /**/ export(integer/1).
% :-  /**/ export(makeConstant/1).
% :-  /**/ export(naf/1).
:-  /**/ export(number/1).
:-  /**/ export(string/1).
:-  /**/ export(var/1).



tSet(completeExtentAsserted).
tSet(ttExpressionType).

rtArgsVerbatum(functorIsMacro).
rtArgsVerbatum(functorDeclares).

%((prologHybrid(C),{get_functor(C,F,A),C\=F}) ==> arity(F,A)).
==>prologHybrid(typeProps/2).

==>typeProps(Type,List)/(member(isa(Super),List),nonvar(Super))==>genls(Type,Super).

arity(typeProps,2).

% :- decl_mpred_pfc ~/1.


:- ignore(show_failure(why,arity(typeProps,2))).
:- sanity(call_u(arity(typeProps,2))).

% ==> (==>(argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologHybrid,prologPTTP,prologDynamic,functorIsMacro,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).
% :- ain_expanded(==>(isa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologPTTP,prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,functorIsMacro,prologListValued,prologSingleValued),functorDeclares))).
% ==>(genls(isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,prologDynamic,prologBuiltin,prologKIF,functorIsMacro,prologListValued,prologSingleValued),tPred)).
:- assert_hasInstance(tCol,tCol).
:- expects_dialect(pfc).
:- file_begin(pfc).

 
% FIXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxx
% FIXX 
==> prologHybrid(isEach( tCol/1, disjointWith/2, genls/2,genlPreds/2, meta_argtypes/1)).
% FIXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxx

% FIXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxx
% FIXX 
==> prologHybrid(isEach( ttNotTemporalType/1,ttTemporalType/1 )).
% TODO FIX 
% :- decl_mpred(tDeleted(ftID),[prologIsFlag]).
prologIsFlag(tDeleted(ftID)).
% FIXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxx

==>prologHybrid(genlInverse/2).
==>prologHybrid(genlPreds/2).
==>prologHybrid(argIsa/3).
==>prologHybrid(predProxyAssert,2).
==>prologHybrid(predProxyQuery, 2).
==>prologHybrid(predProxyRetract, 2).

%assumed prologHybrid(disjointWith/2).
==>prologHybrid(instTypeProps/3).
==>prologHybrid(predTypeMax/3).
==>prologHybrid(resultIsa/2).
%assumed prologHybrid(isa/2).
==>prologDynamic(arg/3).
~tSet(meta_argtypes).
tSet(tInferInstanceFromArgType).
% tCol(tPathway).
%assumed genls(tFunction,tRelation).

tSet(ttValueType).

ttExpressionType(ftString).
ttExpressionType(ftVar).


ttExpressionType(ftCallable).
ttExpressionType(ftPercent).


:- dynamic(vtColor/1).
% :- rtrace, visible(-all),visible(+exception).
isa(vRed,vtColor).


completelyAssertedCollection(vtValue).


isa(vtColor,ttValueType).

isa(X,ttValueType) ==> genls(X,vtValue).
isa(X,ttValueType) ==> completelyAssertedCollection(X).

isa(vtValue,ttValueType).


% :- sanity((vtColor(vRed))).


%argIsa(Prop,N,Type) :- cwc,number(N),loop_check(argIsa_known(Prop,N,Type)),must(ground(argIsa(Prop,N,Type))).
%argIsa(Prop,N,Type) <- {cwc,number(N),argIsa_known(Prop,N,Type),must(ground(argIsa(Prop,N,Type)))}.

ttExpressionType(Type) ==> (argIsa(Prop,N,Type),{number(N)} ==> argQuotedIsa(Prop,N,Type)).

:- discontiguous(prologSingleValued/1).


:- do_gc.

:- kb_shared(mudLabelTypeProps/3).
%:- listing(ttRelationType).
% :- rtrace.
% :- forall((ttRelationType(F),functorDeclares(F)),ain(genls(F,tPred))).
:- nortrace.
% :-  /**/ export(mtForPred/2).

/*
:- rtrace.
:- debug,dtrace,(kb_shared((argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, 
    isa/2,  genls/2, pddlSomethingIsa/2, ttSpatialType/1, 
    resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, 
    ttExpressionType/1, typeProps/2))).

:- prolog. 
*/
/*
:- kb_shared((argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, 
    isa/2,  genls/2, pddlSomethingIsa/2, 
    resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, 
    ttExpressionType/1, typeProps/2)).
*/
/* FIX
==>
*/
prologHybrid(isEach(argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, isa/2, 
   genls/2, pddlSomethingIsa/2, resultIsa/2, subFormat/2, tCol/1, tRegion/1, 
   completelyAssertedCollection/1, ttExpressionType/1, typeProps/2)).


:- ain(isa(ttExpressionType,ttAbstractType)).
:- discontiguous(subFormat/2).
:- kb_shared(tChannel/1).

% ain((I/(mpred_literal(I),fully_expand(_,I,O),I \=@=O )==> ({format('~q~n',[fully_expand(I->O)])},O))).

/* subFormat(ftDeplictsFn(tCol),ftSpec). */
/* subFormat(ftDeplictsFn(meta_argtypes),ftSpec). */
subFormat(ftVoprop,ftSpec).

%==> tFunction(opQuote(isEach(ftRest(ftTerm)))).
==> tFunction(isRandom(tSet)).
==> tFunction(isAnd(ftListFn(ftSpec))).
==> tFunction(isMost(ftListFn(ftSpec))).
==> tFunction(isOneOf(ftListFn(ftSpec))).
==> tFunction(isNot(ftSpec)).
==> tFunction(isOptional(ftSpec,ftTerm)).
==> tFunction(isOptionalStr(ftString)).
==> tFunction(exactStr(ftString)).

resultIsa(ftDeplictsFn,ftSpec).

==> prologHybrid(quotedDefnIff/2).


isa(argIsa,prologHybrid).
isa(determinerString/2, prologMultiValued).
isa(quotedDefnIff, completeExtentAsserted).
isa(ftInt,ttExpressionType).
isa(ftNumber,ttExpressionType).
isa(ftString,ttExpressionType).
isa(isInstFn,tFunction).
isa(isKappaFn,tFunction).
isa(prologMultiValued, tCol).
arity(ftListFn,1).
arity(isLikeFn,2).
arity(ftDeplictsFn,1).

arity(tFunction,1).
==> tFunction(ftDiceFn(ftInt,ftInt,ftInt)).
==> tFunction(ftListFn(tCol)).
==> tFunction(ftDeplictsFn).

completelyAssertedCollection(rtAvoidForwardChain).
completelyAssertedCollection('SententialOperator').


tSet(rtAvoidForwardChain).
tSet('SententialOperator').
%TODO rtAvoidForwardChain('$VAR'('FUNC')).

cycBetween(A,B,N):-
  (number(A) -> 
     ((number(B);number(N)),system_between(A,B,N));
     ((number(B),number(N))->system_between(A,B,N))).



:- multifile(equals/2).
:- dynamic(equals/2).
:- export(equals/2).
arity(equals,2).
equals(X,Y):-equals_call(X,Y).
:- must((arity_no_bc(equals,_))).

arity(termOfUnit,2).
arity(trueSentence,1).
arity(evaluate,2).
arity(different,2).
:- forall(system_between(1,11,A),kb_shared(holds/A)).


==>rtAvoidForwardChain(isEach(equals,different,evaluate,trueSentence,'TINYKB-ASSERTION',termOfUnit)).

/*
:- must((arity_no_bc(holds,_))).
arity('FunctionToArg',2). 
arity('TINYKB-ASSERTION',5).
arity('TINYKB-ASSERTION',6).
arity(holds,2).
==>rtAvoidForwardChain(isEach('FunctionToArg',hold)).
*/

genls('rtSententialRelation','rtSententialOperator').
genls('rtSententialOperator',rtAvoidForwardChain).
genls('rtVariableArityRelation',rtAvoidForwardChain).
genls('rtCommutativeRelation',rtAvoidForwardChain).
genls('tFunction',rtAvoidForwardChain).
genls('rtEvaluatableRelation',rtAvoidForwardChain).

tSet('rtCommutativeRelation').
tSet('rtEvaluatableRelation').
tSet('rtSententialRelation').
tSet('rtVariableArityRelation').


rtArgsVerbatum(completeIsaAsserted).
%completelyAssertedCollection(Ext):- fwc, arg(_,vv(tCol,vtDirection,ttExpressionType,tRegion,ftString, genlPreds),Ext).
completeExtentAsserted(formatted_resultIsa).
completeExtentAsserted(quotedDefnIff).
completelyAssertedCollection(completelyAssertedCollection).

ttExpressionType(ftVar).
ttExpressionType(ftVoprop).


ttStringType('CharacterString').
ttStringType('SubLString').
ttStringType('ControlCharacterFreeString').
ttStringType('SubLListOfStrings').
% ttStringType(['ListOfTypeFn', X]):-atom(X),ttStringType(X).


% resultIsa(F,C)==>(ftSpec(C),'tFunction'(F)).
% ( meta_argtypes(FT)/dif(FT,COL), genls(FT, COL),tCol(COL),{\+ (isa(COL,ttExpressionType))}) ==> formatted_resultIsa(FT,COL).

%:- mpred_trace.
%:- pfcWatch.
%:- mpred_warn.
% next_test :- sleep(1),pfcReset.


% :- kb_shared((disjointWith/2,genls/2)).

prologHybrid(argIsa(tRelation,ftInt,tCol)).
prologHybrid(formatted_resultIsa(ttExpressionType,tCol)).

:- sanity(argIsa(genlPreds,2,_)).

tCol(vtVerb).
:- sanity(tCol(vtVerb)).
:- sanity(isa(vtVerb,tCol)).
:- sanity(t(tCol,vtVerb)).



prologHybrid(quotedDefnIff(ttExpressionType,ftTerm)).
prologHybrid(defnNecessary(ttExpressionType,ftTerm)).
prologHybrid(quotedDefnIff(ttExpressionType,ftTerm)).


tFuncton(isLikeFn(tPred,tCol)).
tRelation('==>'(ftAskable,ftAssertable)).
prologHybrid(subFormat(ttExpressionType,ttExpressionType)).
prologMultiValued(comment(ftTerm,ftString)).
prologMultiValued(genlInverse(tPred,tPred)).
prologMultiValued(genlPreds(tPred,tPred)).
prologMultiValued(predProxyAssert(prologMultiValued,ftTerm)).
prologMultiValued(predProxyQuery(prologMultiValued,ftTerm)).

genlInverse(P1,P2) ==> ( t(P1,A,B) ==> t(P2,B,A)).

/*
(((P/(has_functor(P),get_functor(P,F,A),A>1,ground(P), \+ (arg(_,P,E),(number(E);is_list(E))), mpred_literal_nonvar(P), \+ prologSideEffects(F)) 
  ==> {wdmsg(P),deduceEachArgType(P)}))).
*/

:- if(true).
==> prologHybrid(instTypeProps(ftID,tCol,ftRest(ftVoprop))).
==> functorIsMacro(macroSomethingDescription(ftTerm,ftListFn(ftString))).
==> functorIsMacro(pddlObjects(tCol,ftListFn(ftID))).
==> functorIsMacro(pddlDescription(ftID,ftListFn(ftString))).
==> functorIsMacro(pddlPredicates(ftListFn(ftVoprop))).
==> functorIsMacro(pddlSorts(tCol,ftListFn(tCol))).
==> functorIsMacro(pddlTypes(ftListFn(tCol))).
:- endif.


% prologMultiValued('<==>'(ftTerm,ftTerm)).
prologMultiValued('<-'(ftAssertable,ftAskable)).
prologMultiValued('==>'(ftAskable,ftAssertable)).
prologNegByFailure(predArgMulti(prologMultiValued,ftInt)).
prologNegByFailure(tDeleted(ftID)).

%= 	 	 

%% prologSingleValued( ?ARG1, ?ARG2) is semidet.
%
% Prolog Single Valued.
%
prologSingleValued(predInstMax(ftID,prologSingleValued,ftInt),prologHybrid).
prologSingleValued(predTypeMax(prologSingleValued,tCol,ftInt),prologHybrid).
resultIsa(txtFormatFn,ftText).
%'<==>'(prologMultiValued(CallSig,[predProxyAssert(aina),predProxyRetract(del),predProxyQuery(call)]),prologDynamic(CallSig)).
%'<==>'(prologMultiValued(CallSig,[predProxyAssert(pttp_tell),predProxyRetract(pttp_retract),predProxyQuery(pttp_ask)]),prologPTTP(CallSig)).
subFormat(ftAtom,ftTerm).
subFormat(ftCallable,ftProlog).
resultIsa(ftDiceFn,ftInt).
% subFormat(ftID,ftTerm).
subFormat(ftInt,ftNumber).
subFormat(ftInteger,ftNumber). 
subFormat(ftNumber,ftPercent).
subFormat(ftPercent,ftNumber).
subFormat(ftString,ftTerm). %  "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
subFormat(ftString,ftText). %  "hello world" 
subFormat(ftTerm,ftProlog).
subFormat(ftText,ftTerm). % s("hello","world").
subFormat(ftVar,ftProlog).
==> subFormat(ftVoprop,ftRest(ftVoprop)).
subFormat(ftVoprop,ftTerm).

%ttExpressionType(C) ==> {cls,wdmsg(ttExpressionTypeB1(C)),dumpST,wdmsg(ttExpressionTypeC1(C)),break}.
%ttExpressionType(C) ==> {wdmsg(ttExpressionType3(C))}.
subFormat(_,COL)/(atom(COL))==>ttExpressionType(COL).
%:- break.
subFormat(COL,_)/(atom(COL))==>ttExpressionType(COL).
%:- break.
% tCol(W)==>{quietly(guess_supertypes(W))}.

%:- cls.
%:- mpred_trace_exec.
%:- break.


:- sanity(ttRelationType(prologMultiValued)).


tSet(tNewlyCreated).

:- dynamic(tNewlyCreated/1).
tNewlyCreated(W)==>{guess_types(W)}.


:-kb_shared(rtUnaryPredicate/1).
:-kb_shared(ttSpatialType/1).

ttUnverifiableType(ftID).
% ttUnverifiableType(ftListFn(ftTerm)).
% ttUnverifiableType(ftListFn).
% ttUnverifiableType(ftDiceFn(ftInt,ftInt,ftInt)).

ttUnverifiableType(ftDice).
ttUnverifiableType(ftString).
ttUnverifiableType(ftTerm).
ttUnverifiableType(ftText).
ttUnverifiableType(ftVoprop).

ttUnverifiableType(tCol).
ttUnverifiableType(tFunction).
ttUnverifiableType(tPred).
ttUnverifiableType(ttExpressionType).
ttUnverifiableType(vtDirection).


%ttRelationType(ArgsIsa)==>tPred(ArgsIsa).
%TODO isa(_,ArgsIsa)==>tCol(ArgsIsa).

%:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),set_prolog_flag(debug, true).


/*
disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):- disjointWithT(A,B).
disjointWith(A,B):- disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.
disjointWith(Sub, Super) ==> disjointWith( Super, Sub).
*/


disjointWith(ttTemporalType,ttAbstractType).

prologHybrid(dividesBetween(tCol,tCol,tCol)).

quotedDefnIff(X,_)==>ttExpressionType(X).


quotedDefnIff(ftInt,integer).
quotedDefnIff(ftFloat,float).
quotedDefnIff(ftAtom,atom).
quotedDefnIff(ftString,is_ftString2).
% ftString(X):- cwc, is_ftString2(X).
quotedDefnIff(ftSimpleString,string).
quotedDefnIff(ftCallable,pfc_is_callable).
quotedDefnIff(ftCompound,is_ftCompound).
quotedDefnIff(ftGround,ground).
quotedDefnIff(ftID,is_id).
quotedDefnIff(ftTerm,is_ftNonvar).
quotedDefnIff(ftVar,is_ftVar).
quotedDefnIff(ftNonvar,is_ftNonvar).
quotedDefnIff(ftNumber,number).
quotedDefnIff(ftList,is_list).
% quotedDefnIff(ftRest,is_rest).
quotedDefnIff(ftBoolean,is_boolean).
quotedDefnIff(ftText,is_ftText).

==> ((
 (quotedDefnIff(ftRest(Type),is_rest_of(Type)):- cwc, is_ftNonvar(Type)),
 (quotedDefnIff(ftListFn(Type),is_list_of(Type)):- cwc, is_ftNonvar(Type)),
 (quotedDefnIff(ftCodeIs(SomeCode),SomeCode):- cwc, is_ftNonvar(SomeCode)))).
% :- listing(quotedDefnIff).

:- kb_global(baseKB:ftText/1).

((ttExpressionType(FT)/(append_term(FT,Arg,Head),
  (predicate_property(Head,undefined);predicate_property(Head,number_of_clauses(0)))
    )) ==> 
    ({OO = (Head:- !, term_is_ft(Arg,FT))},OO)).

% tCol(Type),(rtBinaryPredicate(Pred)/(functor(G,Pred,2),G=..[Pred,isInstFn(Type),Value])), G ==> relationMostInstance(Pred,Type,Value).


%((genlPreds(Col1,Col2),(arity(Col1,1);arity(Col2,1)))==>genls(Col1,Col2)).
%((genls(Col1,Col2),(tPred(Col1);tPred(Col2)))==>genlPreds(Col1,Col2)).

tSet(rtBinaryPredicate).
ttRelationType(rtBinaryPredicate).


% (arity(Pred,2),tPred(Pred)) <==> isa(Pred,rtBinaryPredicate).

ttRelationType('rtUnaryPredicate').

isa(arity,rtBinaryPredicate).



specialFunctor('\\+').
specialFunctor('/').


:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).
/*
:- sanity((expand_props(_,==>props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),ain(mdefault(O)))).

:- sanity((fully_expand(_,props(iCrackers666,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]),O),mpred_why(mdefault(O)))).
*/
:- endif.

arity(Pred,2),tPred(Pred) <==> rtBinaryPredicate(Pred).

% if arity is ever greater than 1 it can never become 1
% arity(F,A)/(number(A),A>1) ==> ~(arity(F,1)).

completelyAssertedCollection(rtBinaryPredicate).


% TODO ADD THIS 
%(tCol(Super),completelyAssertedCollection(Super),genls(Sub, Super), isa(I,Sub), {ground(I:Sub:Super),\==(Sub, Super)}) ==> isa(I,Super).

 % :- mpred_trace_exec.

/*

(implies 
    (and 
      (isa ?PRED ReflexiveBinaryPredicate) 
      (arg1Isa ?PRED ?CONSTRAINT1) 
      (isa ?OBJ1 ?CONSTRAINT1) 
      (equals ?OBJ1 ?OBJ2)) 
    (?PRED ?OBJ1 ?OBJ2))

*/
% ((genlPreds(equals,P),argIsa(P,1,Col)) ==>  (t(P,A,B):- (nonvar(A),A==B,isa(A,Col)))).
% genlPreds(equals,genls).
:- mpred_notrace_exec.
rtReflexiveBinaryPredicate(TB)==>genlPreds(equals,TB).

% (isa(TypeType,ttTypeType) , isa(Inst,TypeType), genls(SubInst,Inst)) ==> isa(SubInst,TypeType).

ttExpressionType(ftAction).
{type_prefix(_Prefix,Type),atom(Type),atom_concat(ft,_,Type)}==>ttExpressionType(Type).
{type_suffix(_Suffix,Type),atom(Type),atom_concat(ft,_,Type)}==>ttExpressionType(Type).


{type_prefix(_Prefix,Type)}==>tCol(Type).
{type_suffix(_Suffix,Type)}==>tCol(Type).

((tCol(C)/( \+ ttExpressionType(C))) ==> tSet(C)).



tSet(tPred).
prologHybrid(isa/2).

%mpred_online:semweb_startup:- with_no_term_expansions(if_file_exists(use_module(logicmoo(dbase/mpred_i_rdf_store)))).

% :- with_no_term_expansions(if_file_exists(use_module(logicmoo(mobs/planner/mpred_i_hyhtn)))).
tSet(prologIsFlag).
tSet(prologDynamic).
prologHybrid(formatted_resultIsa/2).

:- sanity(argIsa(genlPreds,2,_)).
:- sanity(tCol(vtVerb)).
:- sanity(t(tCol,vtVerb)).
:- sanity(isa(vtVerb,tCol)).


ttAgentType(mobPhilosopher).

:- sanity(genls(mobPhilosopher,tAgent)).


%:- mpred_trace_all.
isa(iPlato7,mobPhilosopher).

:-must(isa(iPlato7,mobPhilosopher)).

:- if((current_prolog_flag(runtime_debug,D),D>2)).
:- mpred_test(\+ isa(iPlato7,ftAtom)).

%:- mpred_test(\+ quotedIsa(iPlato7,mobPhilosopher)).
%:- sanity(mpred_test(~quotedIsa(iPlato7,mobPhilosopher))).
:- sanity(mpred_test(quotedIsa(iPlato7,ftAtom))).
:- mpred_notrace_all.
:- endif.

tCol(ttAbstractType).



% :- listing(disjointWith/2).

genlsFwd(tItem,'tArtifact').
genlsFwd(tRegion,'tPlace').

:- set_prolog_flag(do_renames,restore).

meta_argtypes(knows(tAgent,ftAskable)).
meta_argtypes(beliefs(tAgent,ftAskable)).
meta_argtypes(loves(tAgent,tTemporalThing)).

