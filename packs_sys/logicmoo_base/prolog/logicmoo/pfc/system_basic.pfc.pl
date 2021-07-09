%:- module(system_basic,[]).
%:- set_module(class(development)).
:- '$set_source_module'(baseKB).
%:- use_module(library(pfc)).
:- expects_dialect(pfc).

/** <module> system_basic
% =============================================
% File 'system_basic.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
*/

%:- cls.
% :- xlisting( (==>) /2 ).

:- set_prolog_flag_until_eof(runtime_debug, 1). % 2 = important but dont sacrifice other features for it

:- if((current_prolog_flag(runtime_debug,D),D>1)).
:- '$def_modules'([clause_expansion/2],O),dmsg(O),nl.
:- make:list_undefined([]).
:- endif.

:- style_check(-discontiguous).
%:- set_prolog_flag_until_eof(runtime_speed,0). % 0 = dont care
:- set_prolog_flag_until_eof(runtime_speed, 1). % 1 = default
:- set_prolog_flag_until_eof(runtime_debug, 1). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag_until_eof(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag_until_eof(unsafe_speedups, false).


:- kb_shared(quotedIsa/2).
:- kb_shared(tCol/1).

:- sanity(is_pfc_file).

arity(is_never_type,1).
arity(F,1):- cwc, tCol(F). % current_predicate(F/1)).  % is_ftNameArity(F,1), , (col_as_unary(F);ttTypeType(F)), \+((call((dif:dif(Z,1))), arity(F,Z))).
~(tCol('$VAR')).

==> ttRelationType(tFunction).
==> ttRelationType(tPred).

%:- listing(ttRelationType/1).

%:- mpred_trace_exec.

completelyAssertedCollection(tRelation).
completelyAssertedCollection(tPred).
%:- listing(completelyAssertedCollection/1).
%:- break.

:- (ain((completelyAssertedCollection(tFunction)))).
:- (ain((ttRelationType(X)/sanity(atom(X))==>(arity(X,1),pfcControlled(X))))).

:- mpred_notrace_exec.

ttTypeType(C)==>completelyAssertedCollection(C).
completelyAssertedCollection(ttTypeType).
% (isa(Inst,Type), tCol(Inst)) ==> isa(Type,ttTypeType).

% tCols are either syntactic or existential
completelyAssertedCollection(ttExpressionType).  % syntactic
completelyAssertedCollection(tSet). % existential

completelyAssertedCollection(C)==>tSet(C).

%ttExpressionType(T)==>completelyDecidableCollection(T).

% relations are predsor functions
completelyAssertedCollection(tRelation).
completelyAssertedCollection(tPred).
completelyAssertedCollection(tFunction).

completelyAssertedCollection(functorIsMacro).

completelyAssertedCollection(tPred).
~completelyAssertedCollection(meta_argtypes).
completelyAssertedCollection(tTemporalThing).
completelyAssertedCollection(tInferInstanceFromArgType).
completelyAssertedCollection(ttNotTemporalType).
completelyAssertedCollection(ttSpatialType).
completelyAssertedCollection(ttTemporalType).
completelyAssertedCollection(ttTypeType).
completelyAssertedCollection(ttUnverifiableType).

tSet(rtNotForUnboundPredicates).
tSet(tPred).
tSet(tRelation).
tSet(prologBuiltin).
tSet(tFunction).
tSet(ttTemporalType).
tSet(functorIsMacro).

tPred(P) :- cwc, tRelation(P), \+ tFunction(P).

(((tRelation(P), \+ tFunction(P)) ==> tPred(P))).


:- kb_shared(disjointWith/2).

rtSymmetricBinaryPredicate(disjointWith).

% @TODO decide how to best impl the next line

% propagate and query swapped args - @TODO find a way to enforce as last pred
rtSymmetricBinaryPredicate(F) ==> {fxy_args_swapped(F,X,Y,P1,P2),nop(was_singleton(X,Y))}, 
                                                                % ( P1 ==>{loop_check(mpred_fwc1( P2),true)}),
                                                                % (~P1 ==>{loop_check(mpred_fwc1(~P2),true)}),
                                                                  ( P1/ (X @< Y) ==>{mpred_fwc1( P2)}),
                                                                  (~P1/ (X @< Y) ==>{mpred_fwc1(~P2)}),
                                                                  (~P1:- (cwc, loop_check(~P2))),
                                                                  ( P1:- (cwc, loop_check( P2))).


%:- meta_predicate(mp_test_agr(?,+,-,*,^,:,0,1,5,9)).
%mp_test_agr(_,_,_,_,_,_,_,_,_,_).
%:- mpred_test(predicate_property(mp_test_agr(_,_,_,_,_,_,_,_,_,_),meta_predicate(_))).
% becomes         mp_test_agr(+,+,-,?,^,:,0,1,0,0)


%((prop_mpred(pfcWatches,F,A)/is_ftNameArity(F,A),prologHybrid(F)))==>prop_mpred(pfcVisible,F,A).


%:- set_prolog_flag_until_eof(gc,true).



%% completelyAssertedCollection( ?VALUE1) is semidet.
%
% Completely Asserted Collection.
%

completeExtentAsserted(functorIsMacro).
completelyAssertedCollection(completeExtentAsserted).
:- on_f_rtrace(ain(mpred_database_term(F,_,_)==>completeExtentAsserted(F))).
prologNegByFailure(prologNegByFailure).

completelyAssertedCollection(functorIsMacro).  % Items read from a file might be a special Macro Head
completelyAssertedCollection(ttRelationType).  % Or they might be a predciate declarer
% completelyAssertedCollection(functorDeclares).  % or they might declare other things
% completelyAssertedCollection(functorIsMacro).  % or they might declare other things


:- dynamic(baseKB:ttTypeType/1).
:- kb_shared(ttTypeType/1).
tSet(ttTypeType).
tSet(ttExpressionType).

% :- mpred_trace_exec.
ttTypeType(F)==>tSet(F).
ttTypeType(ttTypeType).

~tCol(functorDeclares).              
tSet(F)==>functorDeclares(F).

:- kb_shared(ttExpressionType/1). % hard coded like: compound/1
ttExpressionType(ftCallable).
ttExpressionType(ftString).
ttExpressionType(ftAtom).
ttExpressionType(ftProlog).

% :- rtrace((ain_expanded(tCol(tCol)))).

:- kb_shared(meta_argtypes/1).
:- kb_shared(type_checking/0).
:- kb_shared(mudToCyc/2).

rtArgsVerbatum(ftSpec).
rtArgsVerbatum(vtActionTemplate).
meta_argtypes(support_hilog(tRelation,ftInt)).

((codeTooSlow,((tPred(F),
 arity(F,A)/
  (is_ftNameArity(F,A),A>1, 
      \+ prologBuiltin(F), 
      % sanity(mpred_must(\+ arity(F,1))),
      sanity(mpred_must(\+ tCol(F)))))) )
   ==> (~(tCol(F)),support_hilog(F,A))).

/*
((codeTooSlow,(support_hilog(F,A)
  /(is_ftNameArity(F,A),
    \+ is_static_predicate(F/A), \+ prologDynamic(F)))) ==>
   (prop_mpred(_,F,A,pfcVisible), 
    {% functor(Head,F,A) ,Head=..[F|TTs], TT=..[t,F|TTs],
    %  (CL = (Head :- cwc, call(second_order(TT,CuttedCall)), ((CuttedCall=(C1,!,C2)) -> (C1,!,C2);CuttedCall)))
    CL = arity(F,A)
    },
   (CL))).
*/

:- kb_shared(t/2).
((t(T,I):- cwc, I==T,completelyAssertedCollection==I,!)).
((t(T,I):- cwc, I==T,completeExtentAsserted==I,!)).
((t(T,I):- ((cwc, I==T,ttExpressionType==I,!,fail)))).


% ===================================================================
% Type checker system / Never Assert / Retraction checks
% ===================================================================
compilerDirective(disjoint_type_checking,comment("Typecheck semantics")).

:- ain(((tRelation(P), \+ tFunction(P)) ==> tPred(P))).


type_checking ==> (( typeCheckDecl(Each,Must), Each, {\+ Must}) ==> failed_typeCheckDecl(Each,Must)).

failed_typeCheckDecl(Each,Must)==>{trace_or_throw(failed_typeCheckDecl(Each,Must))}.
prologDynamic(is_never_type/1).

never_assert_u(vtVerb(BAD),vtVerbError):- BAD=='[|]'.
arity(meta_argtypes,1).
rtArgsVerbatum(meta_argtypes).
never_assert_u(meta_argtypes(tSet(ftAssertable)),badRules).



:- asserta(elmt:elmt_is_a_module).
:- forall(between(4,9,N),kb_global(elmt:exactlyAssertedELMT/N)).
:- kb_shared(genls/2).

:- kb_shared(tAtemporalNecessarilyEssentialCollectionType/1).
:- kb_shared(completelyAssertedCollection/1).

:- kb_shared(tCol/1).
:- kb_shared(ttTypeFacet/1).

:- begin_pfc.


% ((prologHybrid(F),arity(F,A))==>{kb_shared(F/A)}).

%arity(F,A)/prologHybrid(F)==>{kb_shared(F/A)}.
%prologHybrid(F)/arity(F,A)==>{kb_shared(F/A)}.


% ======================================================================================= %
% Types/Sets/Collections
% ======================================================================================= %

% We assume we know our own classification system 
completelyAssertedCollection(completelyAssertedCollection).

% all completely asserted collections are finite sets
completelyAssertedCollection(A)==>tSet(A).



% tSets are part of the KR expressions language and are types of collections
(tSet(A) ==> (tCol(A), \+ ttExpressionType(A))).

% all indiviuals combined make up a set containing individuals
tSet(tIndividual).

% Types/Sets/Collections are not themselves individuals and are usable always as arity 1
% tCol(A),{sanity(atom(A))} ==> ~tIndividual(A),{decl_type(A), kb_shared(A/1)}.

~tIndividual(A):- is_ftNonvar(A), loop_check(tCol(A)).
tCol(A) ==> {decl_type(A), kb_shared(A/1)}.


% KR expressions exists outside of the logic and are types of collections
ttExpressionType(A)==> ( tCol(A), \+ tSet(A) ).


% ======================================================================================= %
% ttTypeFacet - Every type (tCol) has at least two facets below
% ======================================================================================= %
completelyAssertedCollection(ttTypeFacet).


ttTypeFacet(T)==>tSet(T).


% "Type describes aspects of types":::: 
ttTypeFacet(ttTypeType).

% "Type describes aspects of individuals (non-types)"::::
ttTypeFacet(ttIndividualType).

% Type describes a quoted expression in KR (has no finite instances)
ttTypeFacet(ttExpressionType). 

% Type describes finite instance members in KR 
ttTypeFacet(tSet).             

% New members of this type should not be deduced merely by position in a formula
ttTypeFacet(ttUnverifiableType).  

% This type exists even in impossible worlds
ttTypeFacet(tAtemporalNecessarilyEssentialCollectionType).  

% This type''s finite instance members are all known 
ttTypeFacet(completelyAssertedCollection).  

% ======================================================================================= %
% ttTypeType - Type types are disjoint from each other (facets are not)
% ======================================================================================= %
completelyAssertedCollection(ttTypeType).  % from ttTypeFacet(completelyAssertedCollection). 

% Facets for types are also type types
ttTypeType(ttTypeFacet). 


% "Facet based" type instances are known to be known
genls(ttTypeFacet,completelyAssertedCollection). 

% "if something is a type facet, then *that something* is known as set with finite members"
typeGenls(ttTypeFacet,tCol). 

% All type-types are enumerated eventually
ttTypeType(RT)==>completelyAssertedCollection(RT). 



typeType(ttActionType/1). 
typeType(ttAgentType/1). 

:- kb_shared(argQuotedIsa/3).

:- kb_shared(typeGenls/2).
:- kb_shared(typeProps/2).



% NOTE:  KEEP PREDS AND COLS Separate completelyAssertedCollection(RT)==>completeExtentAsserted(RT).

% ======================================================================================= %
% Sub-instance caching
% ======================================================================================= %
==>(typeGenls(TT,ST) ==>
  (ttTypeType(TT) , tSet(ST) , (isa(Inst,TT) ==> genls(Inst,ST)))).


tooSlow ==> (((typeGenls(SUBCOLTYPE ,SUBCOL),genls(SUBCOLTYPE,COLTYPE),typeGenls(COLTYPE ,COL)) ==>
   genls(SUBCOL,COL))).

genls(C,P) ==> (tCol(C), tCol(P)).
isa(_,C) ==> tCol(C).

tooSlow ==> ((genls(C,P)/(C\=P, \+ ttExpressionType(C) , \+ ttExpressionType(P) , \+ rtAvoidForwardChain(P) )) ==> genlsFwd(C,P)).

% (genls(C,P)/(C\=P), completelyAssertedCollection(P))  ==> genlsFwd(C,P).

tooSlow ==>  ((genlsFwd(C,P)/(C\=P) ==> ((isa(I,C) ==> isa(I,P))))).

%(\+ tooSlow) ==>  ((genls(C,P)/sanity(C\=P) ==> ((isa(I,C) ==> isa(I,P))))).
==>
(\+ tooSlow) ==>  ((genls(C,P)/(C\=P) ==> ((isa(I,C) ==> isa(I,P))))).


tooSlow ==> 
(((genls(C1,C2), ( \+ genlsFwd(C1,C2)))==>
 ({get_functor(C1,F1),get_functor(C2,F2), F2\==F1, 
    P1 =.. [F1,X], P2 =.. [F2,X], 
   asserta_if_new(baseKB:((P2:-loop_check(P1))))}))).

% genls(ttRelationType,completelyAssertedCollection).

% ======================================================================================= %
% Instances of ttTypeType
% ======================================================================================= %
ttTypeType(TT)==>tSet(TT).

% tSet(RT)==>functorDeclares(RT).
% tCol(P)==>{sanity(atom(P))},functorIsMacro(P).

% ~ ttRelationType(col_as_unary).
%ttTypeType(ttExpressionType).
%ttTypeType(ttTypeType).

:-discontiguous(completeExtentAsserted/1).
ttTypeType(ttActionType,comment("Types of actions such PostureChangingAction")).
ttTypeType(ttAgentType,comment("Types of agents such tHuman")).
ttTypeType(ttEventType,comment("Events such StartRaining")).

:- mpred_notrace_exec.

ttTypeType(ttExpressionType).
ttTypeType(ttItemType).
ttTypeType(ttMicrotheoryType).
ttTypeType(ttRegionType).
ttTypeType(ttRelationType).
ttTypeType(ttSituationType).
ttTypeType(ttSpatialType).
ttTypeType(ttTemporalType).
ttTypeType(ttTopicType).
ttTypeType(ttValueType).
ttTypeType(ttIndividualType).


% ttUnverifiableType(ftDice).
% ttUnverifiableType(ftDiceFn(ftInt,ftInt,ftInt)).
% ttUnverifiableType(ftListFn(ftTerm)).
%ttUnverifiableType(ftListFn).
:- dynamic(tItem/1).
:- dynamic(ttItemType/1).
genls(tSpatialThing,tTemporalThing).
genls(ttItemType,ttObjectType).
genls(ttObjectType,ttSpatialType).
genls(ttRegionType,ttSpatialType).

genls(ttTemporalType,ttIndividualType).
genls(tTemporalThing,tIndividual).

ttUnverifiableType(vtDirection).

typeGenls(ttRelationType,tRelation).
typeGenls(ttExpressionTypeType,ttExpressionType).
typeGenls(ttIndividualType,tIndividual).
% "if something is a type facet, then *that something* is known as set with finite members"
typeGenls(ttTypeFacet,tCol).
typeGenls(ttValueType,vtValue).

typeGenls(ttSpatialType,tSpatialThing).
typeGenls(ttAgentType,tAgent).
typeGenls(ttObjectType,tObj).
typeGenls(ttRegionType,tRegion).
typeGenls(ttItemType,tItem).
tSet(tItem).

ttTypeType(TT)==>(isa(C,TT)==>tCol(C)).




disjointWith(C,D)==> tCol(C),tCol(D).

:- if(false). % true,false
:- listing(disjointWith/2).
:- listing( (~) /1).
:- mpred_notrace_exec.
:- endif.

% disjointWith(ttRegionType,ttAgentType).
% disjointWith(ttRelationType,ttTypeType).

((typeGenls(COLTYPE1,COL1),disjointWith(COL1,COL2),
  typeGenls(COLTYPE2,COL2)/dif(COLTYPE1,COLTYPE2)) ==> ((disjointWith(COLTYPE1,COLTYPE2)))).

((typeGenls(COLTYPE1,COL1),disjointWith(COLTYPE1,COLTYPE2)/(ttTypeType(COLTYPE2)),
  typeGenls(COLTYPE2,COL2)/dif(COL1,COL2)) ==> (disjointWith(COL1,COL2))).

rtArgsVerbatum(disjointPartition).
arity(disjointPartition,1).


% disjointWith(P1,P2) ==> ((~isa(C,P2):- loop_check(isa(C,P1))), (~isa(C,P1):- loop_check(isa(C,P2)))).
disjointWith(P1,P2) ==> (expand((~isa(C,P2):- is_ftNonvar(C),loop_check(isa(C,P1))))).

disjointWith(ttRelationType,ttTypeType).

% :- ain((disjointWith(P1,P2) , genls(C1,P1)) ==>  disjointWith(C1,P2)).
disjointWith(C1,P2):- cwc, C1\=P2,disjointWith(P1,P2),C1\=P1,genls(C1,P1),!.


% :- ain(disjointWith(P1,P2) ==> {writeln(disjointWith(P1,P2))}).

disjointPartition(
 [ttIndividualType, 
  ttTypeType, 
  ttValueType]).

(disjointPartition(List), {member(L,List),dif(L,R),member(R,List)})==> disjointWith(L,R).

disjoint_type_checking ==> (disjointWith(C1,C2) ==> (isa(Inst,C1)/isa(Inst,C2) ==> warningsAbout(isa(Inst,disjointWith(C1,C2)),type_checking))).

% ==> disjoint_type_checking.

disjointPartition(
 [ttActionType,
  ttAgentType, 
  ttEventType, 
  ttExpressionType, 
  ttItemType, 
  ttMicrotheoryType, 
  ttRegionType, 
  ttRelationType, 
  ttSituationType, 
  ttTopicType, 
  % ttTypeFacet,
  ttValueType]).

:- if(false). % true,false
:- listing(disjointWith/2).
:- listing( ( ~ )/1).
:- mpred_notrace_exec.
:- endif.                    

ttAgentType(tHuman).

:- if(false).
isa(mobAgent6,tHuman).
:- xlisting(mobAgent6).
:- endif.
/*
disjointPartition([
 ttActionType, ttEventType, ttExpressionType, ttIndividualType, ttMicrotheoryType, 
 ttAgentType,ttItemType, ttRegionType, 
  ttRelationType, ttSituationType, ttSpatialType, ttTemporalType, ttTopicType, %   ttTypeType, ttValueType]).
*/



isa(iExplorer2,C):- cwc, C==rtArgsVerbatum,!,fail.
% isa(I,C):- isa_complete(I,C).
isa(I,C):- cwc, isa_complete(I,C), \+ isa(C,ttExpressionType).

never_assert_u(genls(tPinkBook,tAgent)).

%  % :- mpred_trace_exec.
:- mpred_notrace_exec.



%col_as_unary(Col)==>tCol(Col).
%:- nortrace.
%:-break.

functorIsMacro(props).
functorIsMacro(tiProps).

%% mudEquals( ?X, ?Y) is semidet.
%
% Application Equals.
%

:- multifile(mudEquals/2).
:- kb_shared(mudEquals/2).
:- export(mudEquals/2).
mudEquals(X,Y):-equals_call(X,Y).




% ((prologHybrid(C),{must(callable(C)),get_functor(C,F,A),C\=F}) ==> arity(F,A)).




% :- assert_if_new((isa(I,T):- cwc, visit_isa(I,T))).

% :- mpred_notrace_exec.


:- do_gc.

%:- set_fileAssertMt(baseKB).

:- kb_shared(agent_call_command/2).
:- export(agent_call_command/2).
:- system:import(agent_call_command/2).


:- kb_global(baseKB:decided_not_was_isa/2).



% :-  abolish(yall:'/' / 2).

% :- expand_file_search_path(pack(logicmoo_nlu/ext/pldata),X),exists_directory(X),!,assert_if_new(user:file_search_path(pldata,X)).

%^ :- ensure_loaded(logicmoo(logicmoo_plarkc)).




%:- rtrace.
%:- kb_shared(mpred_prop/4).
%:- kb_global(baseKB:mpred_prop/4).
%:- nortrace.



tAtemporalNecessarilyEssentialCollectionType(ANECT)==>
       decontextualizedCollection(ANECT).


:- kb_shared(marker_supported/2).
:- kb_shared(pass2/0).
:- kb_shared(sometimesSlow/0).
:- kb_shared(sometimesBuggy/0).
:- kb_shared(redundantMaybe/0).

%interArgIsaSome(isa(tRelation,ttRelationType)).
%interArgIsaSome(isa(tAgent,ttAgentType)).
%interArgIsa1_2(isa,tAgent,ttAgentType).

% NEVER (P/mpred_non_neg_literal(P) ==> { remove_negative_version(P) } ).

%:- kb_shared(mpred_mark_C/1).
:- kb_shared(tCol/1).

:- kb_shared(subFormat/2).

:- kb_shared(genlsFwd/2).


% prologHybrid(arity/2).

:- begin_pfc.
:- sanity(get_lang(pfc)).
:- set_file_lang(pfc).
% :- mpred_ops.

:- mpred_notrace_exec.


/*
Unneeded yet

ttTypeType(C)/( is_never_type(C) ; decided_not_was_isa(C,W)) ==> (conflict((ttTypeType(C)/( decided_not_was_isa(C,W);is_never_type(C))))).
*/

/*
tCol(tCol).
tCol(tPred).
% :- sanity(listing(tCol/1)).
*/

%ttExpressionType(ftList(ftInt)).

%:- sanity((fix_mp(clause(assert,sanity),arity(apathFn,2),M,O),M:O=arity(apathFn,2))).


arity(tCol,1).
arity(xyzFn,4).
arity(isKappaFn,2).
arity(isInstFn,1).
arity(ftListFn,1).
arity(argsIsa, 2).
arity(argIsa, 3).
arity(apathFn,2).
arity('<=>',2).

%  % :- mpred_trace_exec.
tCol(F)==>arity(F,1).
:- mpred_notrace_exec.

/*
?- fully_expand((==> (ftSpec(ftListFn(_72012)):- cwc,callable(_72012),ftSpec(_72012))),O).

?- fully_expand_head(clause(asserta,once),(==> (ftSpec(ftListFn(_72012)):- cwc,callable(_72012),ftSpec(_72012))),O).
*/
tCol(ftListFn(Atom)):- cwc, nonvar(Atom),tCol(Atom).
ftSpec(ftListFn(Atom)):- cwc, nonvar(Atom),ftSpec(Atom).
ttExpressionType(ftListFn(Atom)):- cwc, nonvar(Atom),!,ttExpressionType((Atom)).
tSet(ftListFn(Atom)):- cwc, nonvar(Atom),!,tSet(Atom).

% :- mpred_trace_exec.
ttExpressionType(ftAssertable).
ttExpressionType(ftAskable).




% NAUTs
tSet(tUnreifiableFunction,
genls(tFunction),
comment("
A specialization of Function-Denotational instances of which are such that their values 
are not reified in the Cyc system. More precisely, an instance of UnreifiableFunction 
is such that closed \"NA[R|U]Ts\" (see CycLNonAtomicTerm) 
built from its standard CycL name are _not_ instances of #$HLReifiedDenotationalTerm. 
Constrast with ReifiableFunction. Usually it is more efficient to make functions reifiable; 
but it is not desirable to reify every non-atomic term, such as those built from (names of) 
instances of MathematicalFunctionOnScalars. For example, it would be cumbersome to
 reify every term of the form (Inch N) that happened to appear in a CycL assertion."
)).

% NARTs
tSet(tReifiableFunction,comment("A specialization of Function-Denotational. Each instance of ReifiableFunction is denoted by a 
CycL constant that can stand in the 0th (or \"arg0\") position in a CycLReifiableNonAtomicTerm (q.v.). For example, GovernmentFn is a 
reifiable function, so the term `(GovernmentFn France)' is a reifiable non-atomic term (or \"NAT\"). And since this particular 
term actually _is_ reified in the Cyc Knowledge Base, it is, more specifically, a CycLNonAtomicReifiedTerm 
(or \"NART\"). The NART `(GovernmentFn France)' is treated more or less the same as if it were a CycL constant 
(named, say, `GovernmentOfFrance'). Similary, the constant for GovernmentFn can be applied to the constant (or other reified or 
reifiable term) for _any_ instance of GeopoliticalEntity to form a reifiable NAT that denotes that region's government; and should 
 this NAT appear in a sentence that is asserted to the KB, it will thereby become a NART. Note, however, that not all NATs are such that it 
is desireable that they should become reified (i.e. become NARTs) if they appear in assertions; for more on this see UnreifiableFunction."
),
genls(tFunction)).


tSet(vtLinguisticObject).
vtLinguisticObject(vtVerb).

tReifiableFunction(aVerbFn).
conceptuallyRelated("go",actMove).
arity(aVerbFn,1).
resultIsa(aVerbFn(ftString),vtVerb).

:- kb_shared(genls/2).


:- kb_shared( ( =@=> ) /2 ).
:- kb_shared( ( macroExpandExact ) /3 ).

:- op(1185,yfx, ( =@=> )).
tiProps(C,I)=@=>isa(I,C).
tiProps(C,I,P1)=@=>props(I,[C,P1]).
tiProps(C,I,P1,P2)=@=>props(I,[C,P1,P2]).
tiProps(C,I,P1,P2,P3)=@=>props(I,[C,P1,P2,P3]).
tiProps(C,I,P1,P2,P3,P4)=@=>props(I,[C,P1,P2,P3,P4]).

'=@=>'((I,{PreReq}),O) ==> macroExpandExact(I,PreReq,O).
('=@=>'(I,O) / (I\=(_,_))) ==> macroExpandExact(I,true,O).

% '=@=>'(I,O) ==> ('==>'(I,O)).

macroExpandExact(P,PreReq,Q) ==>
(  P, { PreReq,mpred_why(P,Why) } ==> {ignore(retract(P)),mpred_ain(Q,Why)}).


isRegisteredCycPred(apply,maplist,3).

:- kb_shared(isRegisteredCycPred/3).

/*
:- ((rtrace, dtrace)).

(({fail,current_module(Mt),
   predicate_property(Mt:P,defined), 
 \+ predicate_property(Mt:P,imported_from(_)),
 functor(P,F,A)})
  ==>isRegisteredCycPred(Mt,F,A)).
*/

/* prolog_listing:listing */
% :- printAll(isRegisteredCycPred/3).

% ~(tCol({})).

%:- unload_file(library(yall)).



% Unneeded yet
% pass2



/*

doRemoveMe ==> ~ removeMe(_,_).

removeMe(1,2).
removeMe(1,3).

doRemoveMe.



doRedelMe ==>  {redelMe(A,B)}, \+ redelMe(A,B).

redelMe(1,2).
redelMe(1,3).

doRedelMe.

 % :- listing(removeMe/2).
 % :- listing(redelMe/2).

:- dbreak.
*/

%  % :- set_prolog_flag_until_eof(dialect_pfc,cwc).
%  % :- mpred_trace_exec.

% isa(I,C)==>{dmsg(isa(I,C))}.



%:- if( \+ flag_call(runtime_speed==true)).
%(((CI,{was_isa(CI,I,C)},\+ ~isa(I,C)) ==> actn(mpred_post_exactly(isa(I,C))))).
%:- endif.

% :- abolish(system:arity,2).
% :- system:import(arity/2).


tSet(tFoo).    
isa(iBar,tFoo).


/*
:- locally(set_prolog_flag_until_eof(expect_pfc_file,always),autoload([verbose(true)]))).
*/
% :- xlisting(tFoo).

% :- (rtrace(isa(iBar,tFoo))-> true; (break, rtrace(isa(iBar,tFoo)))).

:- isa(iBar,tFoo).

:- mpred_notrace_exec.

:- scan_missed_source.

(vtValue(Val)/(atom(Val),i_name_lc(Val,KW)))==>mudKeyword(Val,KW).

ttPredAndValueType(Str)/
  (i_name('mud',Str,Pred),
  i_name('vt',Str,VT)) ==> 
    (rtRolePredicate(Pred),
     ttValueType(VT),
      mudKeyword(VT,Str),mudKeyword(Pred,Str),
      argIsa(Pred,2,VT),
      argIsa(Pred,1,tTemporalThing)).

% :- mpred_trace_exec.
ttPredAndValueType("size").
ttPredAndValueType("texture").
ttPredAndValueType("color").
:- mpred_notrace_exec.
ttPredAndValueType("shape").
ttPredAndValueType("material").

