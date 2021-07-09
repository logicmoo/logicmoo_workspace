/* 
% ===================================================================
% File 'logicmoo_i_cyc_rewriting.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which make us more like Cyc
% special module hooks into the logicmoo engine allow
% syntax to be recocogized via our CycL/KIF handlers 
%
% Dec 13, 2035
% Douglas Miles

*/
:- module(logicmoo_i_cyc_rewriting,

          [            atom_concatM/3,
            atom_concatR/3,
            % BAD?  baseKB:cycPrepending/2,
            % cyc_to_clif_notify/2,
            rename_atom/2,
            cyc_to_pdkb/2,
            cyc_to_mpred_idiom1/2,
            cyc_to_mpred_idiom_unused/2,
            cyc_to_mpred_sent_idiom_2/3,
            % delay_rule_eval/3,
            
            % BAD?  baseKB:cyc_to_plarkc/2,
            expT/1,
            %lmcache:isCycAvailable_known/0,
            %lmcache:isCycUnavailable_known/1,
            isF/1,
            isFT/1,
            %isPT/1,
            
            isRT/1,
            isV/1,
            isVT/1,
%   baseKB:mpred_to_cyc/2,
           notFormatType/1,
            label_args/3,
            list_to_ops/3,

          


            make_kw_functor/3,
            make_kw_functor/4,
            mpred_postpend_type/2,
            mpred_prepend_type/2
            ]).

:- set_module(class(library)).
:- use_module(library(file_scope)).
:- use_module(library(wam_cl/sreader)).

:- absolute_file_name(library('../ext/pldata/'),Dir,[file_type(directory)]),
   asserta_new(user:file_search_path(pldata,Dir)).

:- set_prolog_flag_until_eof(subclause_expansion,false).


:- module_transparent(show_missing_renames/0).
:- export(show_missing_renames/0).
% show_missing_renames:- !.
show_missing_renames:- retractall(baseKB:rn_new(I,I)),dmsg(call(listing(baseKB:rn_new/2))).
  
:- module_transparent(install_constant_renamer_until_eof/0).
:- export(install_constant_renamer_until_eof/0).

:- create_prolog_flag(do_renames_sumo,maybe,[keep(true)]).

install_constant_renamer_until_eof:-  
  call_on_eof(show_missing_renames),  
  call_on_eof(set_prolog_flag(do_renames_sumo,maybe)),
  set_prolog_flag_until_eof(do_renames,term_expansion).


:- dynamic(baseKB:cycPrepending/2).
% baseKB:cycPrepending(ft,'Atom').
baseKB:cycPrepending(ft,'AtomicAssertion').
baseKB:cycPrepending(ft,'AtomicSentence').
baseKB:cycPrepending(ft,'AtomicTerm').
baseKB:cycPrepending(ft,'Character').
baseKB:cycPrepending(ft,'Constant').
baseKB:cycPrepending(ft,'Expression').
baseKB:cycPrepending(ft,'ExpressionAskable').
baseKB:cycPrepending(ft,'ExpressionAssertible').
% BAD?  baseKB:cycPrepending(ft,'GenericRelationFormula').
baseKB:cycPrepending(ft,'InferenceDataStructure').
baseKB:cycPrepending(ft,'NonNegativeScalarInterval').
baseKB:cycPrepending(ft,'InferenceSupportedTerm').
baseKB:cycPrepending(ft,'KBDatastructure').
baseKB:cycPrepending(ft,'Keyword').
baseKB:cycPrepending(ft,'List').
baseKB:cycPrepending(ft,'NonAtomicReifiedTerm').
baseKB:cycPrepending(ft,'NonAtomicTerm').
baseKB:cycPrepending(ft,'NonAtomicTerm-Askable').
baseKB:cycPrepending(ft,'NonAtomicTerm-Assertible').
baseKB:cycPrepending(ft,'NonNegativeInteger').
baseKB:cycPrepending(ft,'NonVariableNonKeywordSymbol').
baseKB:cycPrepending(ft,'NonVariableSymbol').
baseKB:cycPrepending(ft,'PositiveInteger').
baseKB:cycPrepending(ft,'PropositionalSentence').
baseKB:cycPrepending(ft,'RealNumber').
baseKB:cycPrepending(ft,'ReifiableDenotationalTerm').
baseKB:cycPrepending(ft,'ReifiableNonAtomicTerm').
baseKB:cycPrepending(ft,'ReifiedDenotationalTerm').
baseKB:cycPrepending(ft,'RepresentedAtomicTerm').
baseKB:cycPrepending(ft,'Sentence').
baseKB:cycPrepending(ft,'String').
baseKB:cycPrepending(ft,'Symbol').

/*
baseKB:cycPrepending(ft,'NonAtomicTerm-ClosedFunctor').
baseKB:cycPrepending(ft,'OpenDenotationalTerm').
baseKB:cycPrepending(ft,'OpenExpression').
baseKB:cycPrepending(ft,'OpenFormula').
baseKB:cycPrepending(ft,'OpenNonAtomicTerm').
baseKB:cycPrepending(ft,'OpenSentence').
baseKB:cycPrepending(ft,'ClosedAtomicSentence').
% BAD?  baseKB:cycPrepending(ft,'ClosedAtomicTerm').
baseKB:cycPrepending(ft,'ClosedDenotationalTerm').
baseKB:cycPrepending(ft,'ClosedExpression').
baseKB:cycPrepending(ft,'ClosedFormula').
baseKB:cycPrepending(ft,'ClosedNonAtomicTerm').
baseKB:cycPrepending(ft,'ClosedSentence').
*/

/*
baseKB:cycPrepending(ft,'RepresentedTerm').
baseKB:cycPrepending(ft,'RuleAssertion').
baseKB:cycPrepending(ft,'ScalarIntegralValue').
baseKB:cycPrepending(ft,'SentenceAskable').
baseKB:cycPrepending(ft,'SentenceAssertible').
baseKB:cycPrepending(ft,'SupportDatastructure').
baseKB:cycPrepending(ft,'TheTerm').
baseKB:cycPrepending(ft,'AssertedAssertion').
baseKB:cycPrepending(ft,'Assertion').
baseKB:cycPrepending(ft,'DeducedAssertion').
baseKB:cycPrepending(ft,'DenotationalTerm').
baseKB:cycPrepending(ft,'DenotationalTerm-Assertible').
baseKB:cycPrepending(ft,'DocumentationConstant').
baseKB:cycPrepending(ft,'GAFAssertion').
baseKB:cycPrepending(ft,'HLPrototypicalTerm').
baseKB:cycPrepending(ft,'IndeterminateTerm').
baseKB:cycPrepending(ft,'IndexedTerm').
baseKB:cycPrepending(ft,'TruthValueSentence').
*/

baseKB:cycPrepending(v,'SingleEntry').
baseKB:cycPrepending(v,'SetTheFormat').

baseKB:cycPrepending(tt,'TransformationModuleSupportedCollection').
baseKB:cycPrepending(tt,'RemovalModuleSupportedCollection-Generic').

baseKB:cycPrepending(rt,'UnaryPredicate').
baseKB:cycPrepending(rt,'RemovalModuleSupportedPredicate-Generic').
baseKB:cycPrepending(rt,'RemovalModuleSupportedPredicate-Specific').
baseKB:cycPrepending(rt,'InferenceSupportedPredicate').
baseKB:cycPrepending(rt,'SentenceClosedPredicate').
baseKB:cycPrepending(rt,'TransformationModuleSupportedPredicate').
baseKB:cycPrepending(rt,'ArgGenlQuantityBinaryPredicate').
baseKB:cycPrepending(rt,'BinaryFunction').
baseKB:cycPrepending(rt,'UnreifiableFunction').
baseKB:cycPrepending(rt,'UnaryFunction').
baseKB:cycPrepending(rt,'LogicalConnective').
baseKB:cycPrepending(rt,'ELRelationOneWay').
baseKB:cycPrepending(t,'Function').




baseKB:cycPrepending(v,'Forward-AssertionDirection').
baseKB:cycPrepending(v,'Code-AssertionDirection').
baseKB:cycPrepending(v,'Backward-AssertionDirection').
baseKB:cycPrepending(vt,'AssertionDirection').

baseKB:cycPrepending(tt,'InferenceSupportedCollection').
% BAD?  baseKB:cycPrepending(rt,A):-atom(A),atom_contains(A,'Predicate').

prepender(Pre,C):- builtin_rn_or_rn(C,P),atom_concat(Pre,C,P).



% documentation(C,'EnglishLanguage',S)=@=>comment(C,S).







:- multifile(baseKB:rn_new/2).
:- dynamic(baseKB:rn_new/2).
:- export(baseKB:rn_new/2).


builtin_rn_or_rn_new(C,P):-builtin_rn(C,P).
builtin_rn_or_rn_new(C,P):-baseKB:rnc(C,P).
builtin_rn_or_rn_new(C,P):-baseKB:rn_new(C,P).

builtin_rn_or_rn(P,PP):-builtin_rn_or_rn_new(P,PP),!.
builtin_rn_or_rn(P,PP):-builtin_rn_or_rn_new(_,P),!,P=PP.

% BAD? builtin_rn('BaseKB', baseKB).
builtin_rn('between', cycBetween).
% builtin_rn('forall', cycforAll).

% BAD?  builtin_rn('equals', mudEquals).
% BAD?  builtin_rn('termOfUnit',skolem ).

% BAD? builtin_rn('ScalarInterval', 'tScalarInterval').
% BAD? builtin_rn('SetOrCollection',ftSpec).
% BAD? builtin_rn(Was, baseKB):-mtUndressedMt(Was).
% BAD?  builtin_rn('or', 'v').
% BAD?  builtin_rn('and', '&').

builtin_rn('PhysicalPartOfObject',tPartTypePhysicalPartOfObject).
builtin_rn("PhysicalPartOfObject",tPartTypePhysicalPartOfObject).
builtin_rn('SpecifiedPartTypeCollection',ttSpecifiedPartTypeCollection).
builtin_rn("SpecifiedPartTypeCollection",ttSpecifiedPartTypeCollection).
builtin_rn('SiblingDisjointCollectionType',ttTypeType).
builtin_rn('forAll', 'all').
builtin_rn('thereExists', 'exists').
builtin_rn('Relation', tRelation).
builtin_rn('thereExistsAtLeast', 'atleast').
builtin_rn('thereExistsAtMost', 'atmost').
builtin_rn('CycLClosedAtomicTerm', 'ftAtomicTerm').
builtin_rn('UnitOfMeasure', 'ttUnitOfMeasure').
builtin_rn('CharacterString', ftString).
builtin_rn('Collection',tCol).
builtin_rn('CollectionType',ttTypeType).
builtin_rn('ObjectType',ttValueType).
builtin_rn('AspatialThing',vtValue).
builtin_rn('RelationshipType',ttRelationType).
builtin_rn('Predicate',tPred).
builtin_rn('SubLExpressionType',ttExpressionType).

builtin_rn('holds', 't').
builtin_rn('dot_holds', 't').

builtin_rn('SubLListOfStrings', ftListFn(ftString)).

builtin_rn('ArgGenlQuantityTernaryPredicate',rtArgGenlQuantityTernaryPredicate).
builtin_rn('ELRelation',rtELRelation).
builtin_rn('InterArgFormatPredicate',rtInterArgFormatPredicate).
builtin_rn('ArgTypePredicate',rtArgTypePredicate).
builtin_rn('ArgIsaPredicate',rtArgIsaPredicate).
builtin_rn('SententialRelation',rtSententialRelation).
builtin_rn('ThePrototypicalBinaryPredicate',rtThePrototypicalBinaryPredicate).
builtin_rn('ThePrototypicalTransitiveBinaryPredicate',rtThePrototypicalTransitiveBinaryPredicate).
builtin_rn('TruthFunction',rtTruthFunction).
builtin_rn('ArgTypeBinaryPredicate',rtArgTypeBinaryPredicate).
builtin_rn('ArgGenlTernaryPredicate',rtArgGenlTernaryPredicate).
builtin_rn('ArgIsaTernaryPredicate',rtArgIsaTernaryPredicate).
builtin_rn('ArgGenlBinaryPredicate',rtArgGenlBinaryPredicate).
builtin_rn('ArgIsaBinaryPredicate',rtArgIsaBinaryPredicate).
builtin_rn('ArgTypeTernaryPredicate',rtArgTypeTernaryPredicate).
builtin_rn('InterArgIsaPredicate',rtInterArgIsaPredicate).
builtin_rn('AntiTransitiveBinaryPredicate',rtAntiTransitiveBinaryPredicate).
builtin_rn('BookkeepingPredicate',rtBookkeepingPredicate).
builtin_rn('DistributingMetaKnowledgePredicate',rtDistributingMetaKnowledgePredicate).
builtin_rn('DocumentationPredicate',rtDocumentationPredicate).
builtin_rn('AntiSymmetricBinaryPredicate',rtAntiSymmetricBinaryPredicate).
builtin_rn('EvaluatableFunction',rtEvaluatableFunction).
builtin_rn('EvaluatableRelation',rtEvaluatableRelation).
builtin_rn('ArgConstraintPredicate',rtArgConstraintPredicate).
builtin_rn('ScopingRelation',rtScopingRelation).
builtin_rn('MicrotheoryDesignatingRelation',rtMicrotheoryDesignatingRelation).
builtin_rn('TransitiveBinaryPredicate',rtTransitiveBinaryPredicate).
builtin_rn('ArgQuotedIsaBinaryPredicate',rtArgQuotedIsaBinaryPredicate).
builtin_rn('ArgQuotedIsaPredicate',rtArgQuotedIsaPredicate).
builtin_rn('ArgQuotedIsaTernaryPredicate',rtArgQuotedIsaTernaryPredicate).
builtin_rn('ArgSometimesIsaPredicate',rtArgSometimesIsaPredicate).
builtin_rn('AssociativeRelation',rtAssociativeRelation).
builtin_rn('CollectionDenotingFunction',rtCollectionDenotingFunction).
builtin_rn('EvaluatablePredicate',rtEvaluatablePredicate).
builtin_rn('ExceptionPredicate',rtExceptionPredicate).
builtin_rn('IndeterminateTermDenotingFunction',rtIndeterminateTermDenotingFunction).
builtin_rn('QuaternaryRelation',rtQuaternaryRelation).
builtin_rn('QuintaryRelation',rtQuintaryRelation).
builtin_rn('TernaryRelation',rtTernaryRelation).
builtin_rn('AsymmetricBinaryPredicate',rtAsymmetricBinaryPredicate).
builtin_rn('SymmetricBinaryPredicate',rtSymmetricBinaryPredicate).
builtin_rn('BinaryRelation',rtBinaryRelation).
builtin_rn('CommutativeRelation',rtCommutativeRelation).
builtin_rn('UnaryRelation',rtUnaryRelation).
builtin_rn('PartiallyCommutativeRelation',rtPartiallyCommutativeRelation).
builtin_rn('IrreflexiveBinaryPredicate',rtIrreflexiveBinaryPredicate).
builtin_rn('ReflexiveBinaryPredicate',rtReflexiveBinaryPredicate).
builtin_rn('QuaternaryPredicate',rtQuaternaryPredicate).
builtin_rn('QuintaryPredicate',rtQuintaryPredicate).
builtin_rn('TernaryPredicate',rtTernaryPredicate).
builtin_rn('BinaryPredicate',rtBinaryPredicate).
builtin_rn('CycLReformulationRulePredicate',rtReformulationRulePredicate).
builtin_rn('DefaultMonotonicPredicate',rtDefaultMonotonicPredicate).
builtin_rn('InferenceRelatedBookkeepingPredicate',rtInferenceRelatedBookkeepingPredicate).
builtin_rn('FixedArityRelation',rtFixedArityRelation).
builtin_rn('VariableArityRelation',rtVariableArityRelation).
builtin_rn('SkolemFunction',rtSkolemFunction).
builtin_rn('VariableAritySkolemFunction',rtVariableAritySkolemFunction).
builtin_rn('FixedAritySkolemFunction',rtFixedAritySkolemFunction).
builtin_rn('ReifiableFunction',rtReifiableFunction).
builtin_rn('InferenceSupportedCollection',ttInferenceSupportedCollection).
builtin_rn('AssertionDirection',vtAssertionDirection).
builtin_rn('Backward-AssertionDirection',vBackwardAssertionDirection).
builtin_rn('Code-AssertionDirection',vCodeAssertionDirection).
builtin_rn('Forward-AssertionDirection',vForwardAssertionDirection).
builtin_rn('UnaryFunction',rtUnaryFunction).
builtin_rn('UnreifiableFunction',rtUnreifiableFunction).
builtin_rn('BinaryFunction',rtBinaryFunction).
builtin_rn('ArgGenlQuantityBinaryPredicate',rtArgGenlQuantityBinaryPredicate).
builtin_rn('TransformationModuleSupportedPredicate',rtTransformationModuleSupportedPredicate).
builtin_rn('SentenceClosedPredicate',rtSentenceClosedPredicate).
builtin_rn('InferenceSupportedPredicate',rtInferenceSupportedPredicate).
builtin_rn('RemovalModuleSupportedPredicate-Specific',rtRemovalModuleSupportedPredicateSpecific).
builtin_rn('RemovalModuleSupportedPredicate-Generic',rtRemovalModuleSupportedPredicateGeneric).
builtin_rn('UnaryPredicate',rtUnaryPredicate).
builtin_rn('RemovalModuleSupportedCollection-Generic',ttRemovalModuleSupportedCollectionGeneric).
builtin_rn('TransformationModuleSupportedCollection',ttTransformationModuleSupportedCollection).
builtin_rn('SetTheFormat',vSetTheFormat).
builtin_rn('SingleEntry',vSingleEntry).
builtin_rn('Symbol',ftSymbol).
builtin_rn('String',ftString).
builtin_rn('Sentence',ftSentence).
builtin_rn('RepresentedAtomicTerm',ftRepresentedAtomicTerm).
builtin_rn('ReifiedDenotationalTerm',ftReifiedDenotationalTerm).
builtin_rn('ReifiableNonAtomicTerm',ftReifiableNonAtomicTerm).
builtin_rn('ReifiableDenotationalTerm',ftReifiableDenotationalTerm).
builtin_rn('RealNumber',ftRealNumber).
builtin_rn('PropositionalSentence',ftPropositionalSentence).
builtin_rn('PositiveInteger',ftPositiveInteger).
builtin_rn('NonVariableSymbol',ftNonVariableSymbol).
builtin_rn('NonVariableNonKeywordSymbol',ftNonVariableNonKeywordSymbol).
builtin_rn('NonNegativeInteger',ftNonNegativeInteger).
builtin_rn('NonAtomicTerm-Assertible',ftNonAtomicTermAssertible).
builtin_rn('NonAtomicTerm-Askable',ftNonAtomicTermAskable).
builtin_rn('NonAtomicTerm',ftNonAtomicTerm).
builtin_rn('NonAtomicReifiedTerm',ftNonAtomicReifiedTerm).
builtin_rn('List',ftList).
builtin_rn('Keyword',ftKeyword).
builtin_rn('KBDatastructure',ftKBDatastructure).
builtin_rn('InferenceSupportedTerm',ftInferenceSupportedTerm).
builtin_rn('NonNegativeScalarInterval',ftNonNegativeScalarInterval).
builtin_rn('InferenceDataStructure',ftInferenceDataStructure).
builtin_rn('ExpressionAssertible',ftExpressionAssertible).
builtin_rn('ExpressionAskable',ftExpressionAskable).
builtin_rn('Expression',ftExpression).
builtin_rn('Constant',ftConstant).
builtin_rn('Character',ftCharacter).
builtin_rn('AtomicTerm',ftAtomicTerm).
builtin_rn('AtomicSentence',ftAtomicSentence).
builtin_rn('AtomicAssertion',ftAtomicAssertion).
builtin_rn('Atom',ftAtom).
builtin_rn('Integer',ftInt).
builtin_rn('CycLFormulaicSentence',ftSentence).
builtin_rn('FormulaicSentence',ftSentence).
builtin_rn('SubLFormulaicSentence',ftSentence).
builtin_rn(icSentenceSentence,ftSentence).
builtin_rn('CycLVariable',ftVar).
builtin_rn('Variable',ftVar).
builtin_rn('CycLExpressionType',ttExpressionType).
builtin_rn('ExpressionType',ttExpressionType).
builtin_rn('Agent-Generic',tAgent).
builtin_rn('Agent',tAgent).
builtin_rn('Collection',tCol).
builtin_rn('Function-Denotational',tFunction).
builtin_rn('HumanCyclist',tHumanCyclist).
builtin_rn('KnowledgeBase',tKnowledgeBase).
builtin_rn('Microtheory',tMicrotheory).
builtin_rn('Predicate',tPred).
builtin_rn('CycProblemStore',tProblemStore).
builtin_rn('RuleTemplate',tRuleTemplate).
builtin_rn('Thing',tThing).
builtin_rn('Individual',tIndividual).
builtin_rn('DayOfWeekType',vtDayOfWeekType).
builtin_rn('MonthOfYearType',vtMonthOfYearType).
builtin_rn('Format',vtFormat).
builtin_rn('CycInferenceProblemLinkStatus',vtInferenceProblemLinkStatus).
builtin_rn('CycHLTruthValue',vtHLTruthValue).
builtin_rn('CycProvabilityStatus',vtProvabilityStatus).
builtin_rn('TruthValue',vtTruthValue).
builtin_rn('CanonicalizerDirective',vtCanonicalizerDirective).
builtin_rn('CollectionSubsetFn',tColOfCollectionSubsetFn).
builtin_rn('True',vTrue).
builtin_rn('False',vFalse).
builtin_rn('Guest',vGuest).
builtin_rn('CycAdministrator',vAdministrator).
builtin_rn('IntervalEntry',vIntervalEntry).
builtin_rn('SingleEntry',vSingleEntry).
builtin_rn('CycLClosedAtomicTerm',ftAtomicTerm).
builtin_rn('SetTheFormat',vSetTheFormat).
builtin_rn('AssertedFalseDefault',vAssertedFalseDefault).
builtin_rn('AssertedFalseMonotonic',vAssertedFalseMonotonic).
builtin_rn('AssertedTrueDefault',vAssertedTrueDefault).
builtin_rn('AssertedTrueMonotonic',vAssertedTrueMonotonic).
builtin_rn('MonotonicallyFalse',vMonotonicallyFalse).
builtin_rn('MonotonicallyTrue',vMonotonicallyTrue).
builtin_rn('DefaultFalse',vDefaultFalse).
builtin_rn('DefaultTrue',vDefaultTrue).
builtin_rn('Good-ProblemProvabilityStatus',vGoodProblemProvabilityStatus).
builtin_rn('Neutral-ProblemProvabilityStatus',vNeutralProblemProvabilityStatus).
builtin_rn('NoGood-ProblemProvabilityStatus',vNoGoodProblemProvabilityStatus).
builtin_rn('Unknown-HLTruthValue',vUnknownHLTruthValue).
builtin_rn('ExistentialQuantifier-Bounded',vExistentialQuantifierBounded).
builtin_rn('AllowGenericArgVariables',vAllowGenericArgVariables).
builtin_rn('AllowKeywordVariables',vAllowKeywordVariables).
builtin_rn('RelaxArgTypeConstraintsForVariables',vRelaxArgTypeConstraintsForVariables).
builtin_rn('LeaveSomeTermsAtEL',vLeaveSomeTermsAtEL).
builtin_rn('LeaveSomeTermsAtELAndAllowKeywordVariables',vLeaveSomeTermsAtELAndAllowKeywordVariables).
builtin_rn('LeaveVariablesAtEL',vLeaveVariablesAtEL).
builtin_rn('DontReOrderCommutativeTerms',vDontReOrderCommutativeTerms).
builtin_rn('ReformulationBackwardDirection',vReformulationBackwardDirection).
builtin_rn('ReformulationForwardDirection',vReformulationForwardDirection).
builtin_rn('ReformulationNeitherDirection',vReformulationNeitherDirection).
builtin_rn('CycLSentence-ClosedPredicate',ftSentenceAssertible).
builtin_rn('CycLNonAtomicTerm-ClosedFunctor',ftNonAtomicTerm).
builtin_rn('April',vApril).
builtin_rn('August',vAugust).
builtin_rn('December',vDecember).
builtin_rn('February',vFebruary).
builtin_rn('January',vJanuary).
builtin_rn('July',vJuly).
builtin_rn('June',vJune).
builtin_rn('March',vMarch).
builtin_rn('May',vMay).
builtin_rn('November',vNovember).
builtin_rn('October',vOctober).
builtin_rn('September',vSeptember).
builtin_rn('Sunday',vSunday).
builtin_rn('Monday',vMonday).
builtin_rn('Tuesday',vTuesday).
builtin_rn('Wednesday',vWednesday).
builtin_rn('Thursday',vThursday).
builtin_rn('Friday',vFriday).
builtin_rn('Saturday',vSaturday).
builtin_rn('ArgGenlQuantityTernaryPredicate',rtArgGenlQuantityTernaryPredicate).


builtin_rn('Likelihood-QuantityType', ttLikelihoodQuantityType).
builtin_rn('Standing-QuantityType', ttStandingQuantityType).
builtin_rn('CardinalityQuantType', ttCardinalityQuantType).
builtin_rn('TaskSchedulerRegularTimeIntervalExpression', ftTaskSchedulerRegularTimeIntervalExpression).
builtin_rn('ThePrototypicalGaugeStatusType', ttThePrototypicalGaugeStatusType).
builtin_rn('TransitiveNPFrameType', ttTransitiveNPFrameType).
builtin_rn('TransitiveGerundPPFrameType', ttTransitiveGerundPPFrameType).
builtin_rn('TransitiveFrameType', ttTransitiveFrameType).
builtin_rn('ClausalFrameType', ttClausalFrameType).
builtin_rn('TransitiveParticleNPFrameType', ttTransitiveParticleNPFrameType).
builtin_rn('ParticleFrameType', ttParticleFrameType).
builtin_rn('TripleComplementFrameType', ttTripleComplementFrameType).
builtin_rn('TransitiveCopulaFrameType', ttTransitiveCopulaFrameType).
builtin_rn('TransitiveAdverbFrameType', ttTransitiveAdverbFrameType).
builtin_rn('GerundPhraseFrameType', ttGerundPhraseFrameType).
builtin_rn('PassiveVoiceOnlyFrameType', ttPassiveVoiceOnlyFrameType).
builtin_rn('ActiveVoiceOnlyFrameType', ttActiveVoiceOnlyFrameType).
builtin_rn('DitransitiveInfinitivePhraseFrameType', ttDitransitiveInfinitivePhraseFrameType).
builtin_rn('DitransitiveNP-GenericFrameType', ttDitransitiveNPGenericFrameType).
builtin_rn('DitransitiveParticleNP-PPFrameType', ttDitransitiveParticleNPPPFrameType).
builtin_rn('DitransitiveFrameType', ttDitransitiveFrameType).
builtin_rn('PleonasticFrameType', ttPleonasticFrameType).
builtin_rn('TransitiveThatClauseFrameType', ttTransitiveThatClauseFrameType).
builtin_rn('TransitiveInfinitivePhraseFrameType', ttTransitiveInfinitivePhraseFrameType).
builtin_rn('PleonasticInfinitivePhraseFrameType', ttPleonasticInfinitivePhraseFrameType).
builtin_rn('WHClauseFrameType', ttWHClauseFrameType).
builtin_rn('DitransitivePPFrameType', ttDitransitivePPFrameType).
builtin_rn('DitransitivePP-NPFrameType', ttDitransitivePPNPFrameType).
builtin_rn('DitransitiveNP-PPGerundObjectFrameType', ttDitransitiveNPPPGerundObjectFrameType).
builtin_rn('TransitivePPFrameType', ttTransitivePPFrameType).
builtin_rn('IntransitiveParticleFrameType', ttIntransitiveParticleFrameType).
builtin_rn('IntransitiveFrameType', ttIntransitiveFrameType).
builtin_rn('ConditionExpression', ftConditionExpression).
builtin_rn('ComputerCodeExpression', ftComputerCodeExpression).
builtin_rn('AppositiveExpression', ftAppositiveExpression).
builtin_rn('DefiniteExpression', ftDefiniteExpression).
builtin_rn('IndefiniteExpression', ftIndefiniteExpression).
builtin_rn('FirstPersonExpression', ftFirstPersonExpression).
builtin_rn('SecondPersonExpression', ftSecondPersonExpression).
builtin_rn('ThirdPersonExpression', ftThirdPersonExpression).
builtin_rn('MasculineExpression', ftMasculineExpression).
builtin_rn('FeminineExpression', ftFeminineExpression).
builtin_rn('NeuterExpression', ftNeuterExpression).
builtin_rn('SingularExpression', ftSingularExpression).
builtin_rn('TaskSchedulerTimePatternExpression', ftTaskSchedulerTimePatternExpression).
builtin_rn('TaskSchedulerDatePatternExpression', ftTaskSchedulerDatePatternExpression).
builtin_rn('TaskSchedulerTemplateExpression', ftTaskSchedulerTemplateExpression).
builtin_rn('TelephoneType', ttTelephoneType).
builtin_rn('SyntacticNodeMeaningExpression', ftSyntacticNodeMeaningExpression).
builtin_rn('CollectivePropertyModifierType', ttCollectivePropertyModifierType).
builtin_rn('PairwiseModifierType', ttPairwiseModifierType).
builtin_rn('SpecifiedValueModifierType', ttSpecifiedValueModifierType).
builtin_rn('ComparativeModifierType', ttComparativeModifierType).
builtin_rn('NumericModifierType', ttNumericModifierType).
builtin_rn('InterfaceModifierType', ttInterfaceModifierType).
builtin_rn('WordNetVerbFrameType', ttWordNetVerbFrameType).
builtin_rn('CycLOpenExpression', ftCycLOpenExpression).
builtin_rn('TypicalityReferenceSetPropertyType-AttackType', ttTypicalityReferenceSetPropertyTypeAttackType).
builtin_rn('CycLFullyGroundExpression', ftCycLFullyGroundExpression).
builtin_rn('CycLClosedExpression', ftCycLClosedExpression).
builtin_rn('SExpression', ftSExpression).
builtin_rn('ForwardReifiableCycLFunctor', rtForwardReifiableCycLFunctor).
builtin_rn('ReifiableCycLFunctor', rtReifiableCycLFunctor).
builtin_rn('UnreifiableCycLFunctor', rtUnreifiableCycLFunctor).
builtin_rn('CycLFunctor', rtCycLFunctor).
builtin_rn('CycLOperator', rtCycLOperator).
builtin_rn('CSQLQuantifier', rtCSQLQuantifier).
builtin_rn('CSQLLogicalOperator', rtCSQLLogicalOperator).
builtin_rn('CSQLOperator', rtCSQLOperator).
builtin_rn('CycLSetExpression', ftCycLSetExpression).
builtin_rn('RegularExpression', ftRegularExpression).
builtin_rn('CycLExpression', ftCycLExpression).
builtin_rn('SubLSExpression', ftSubLSExpression).
builtin_rn('PegStatusType', ttPegStatusType).
builtin_rn('BitStringDatatypeType', ttBitStringDatatypeType).
builtin_rn('FixedSizeComputerDatatypeType', ttFixedSizeComputerDatatypeType).
builtin_rn('UnlimitedSizeComputerDatatypeType', ttUnlimitedSizeComputerDatatypeType).
builtin_rn('VariableLimitedSizeDatatypeType', ttVariableLimitedSizeDatatypeType).
builtin_rn('ComputerDatatypeType', ttComputerDatatypeType).
builtin_rn('ObliqueSubjectAlternationType', ttObliqueSubjectAlternationType).
builtin_rn('LexicalWordType', ttLexicalWordType).
builtin_rn('NonTransitiveAlternationType', ttNonTransitiveAlternationType).
builtin_rn('NLPhraseType', ttNLPhraseType).
builtin_rn('SententialConstituentType', ttSententialConstituentType).
builtin_rn('MetaphorTypeByBasisType', ttMetaphorTypeByBasisType).
builtin_rn('FormalSemanticStructureType', ttFormalSemanticStructureType).
builtin_rn('ObjectReferenceDatatypeType', ttObjectReferenceDatatypeType).
builtin_rn('DefinedDatatypeType', ttDefinedDatatypeType).
builtin_rn('CompositeDatatypeType', ttCompositeDatatypeType).
builtin_rn('TransitivityAlternationType', ttTransitivityAlternationType).
builtin_rn('LinguisticObjectType', ttLinguisticObjectType).
builtin_rn('ThePrototypicalPegStatusType', ttThePrototypicalPegStatusType).
builtin_rn('DateDataType', ttDateDataType).
builtin_rn('ThePrototypicalNLPhraseType', ttThePrototypicalNLPhraseType).
builtin_rn('ThePrototypicalSententialConstituentType', ttThePrototypicalSententialConstituentType).
builtin_rn('ThePrototypicalMetaphorTypeByBasisType', ttThePrototypicalMetaphorTypeByBasisType).
builtin_rn('ProgramExpression', ftProgramExpression).
builtin_rn('StringIndexingSlot', rtStringIndexingSlot).
builtin_rn('WhenClauseFrameType', ttWhenClauseFrameType).
builtin_rn('VariedOrderCollection', ttVariedOrderCollection).
builtin_rn('AtemporalNecessarilyEssentialCollectionType', ttAtemporalNecessarilyEssentialCollectionType).
builtin_rn('SiblingDisjointSetOrCollectionType', ttSiblingDisjointSetOrCollectionType).
builtin_rn('SocialStatusCollectionType', ttSocialStatusCollectionType).
builtin_rn('TotallyOrderedCollection', ttTotallyOrderedCollection).
builtin_rn('NonAbducibleCollection', ttNonAbducibleCollection).
builtin_rn('IntensionalAbstractionTargetType', ttIntensionalAbstractionTargetType).
builtin_rn('PragmaticallyDecontextualizedCollection', ttPragmaticallyDecontextualizedCollection).
builtin_rn('ClarifyingCollectionType', ttClarifyingCollectionType).
builtin_rn('KEClarifyingCollectionType', ttKEClarifyingCollectionType).
builtin_rn('FactGatheringTypicallyUnspecifiedSetOrCollection', ttFactGatheringTypicallyUnspecifiedSetOrCollection).
builtin_rn('NonEmptyCollection', ttNonEmptyCollection).
builtin_rn('EmptyCollection', ttEmptyCollection).
builtin_rn('FixedOrderCollection', ttFixedOrderCollection).
builtin_rn('GrammaticallyEncodedDomainRestrictionType', ttGrammaticallyEncodedDomainRestrictionType).
builtin_rn('MarketDataProductClassificationType-ContentType', ttMarketDataProductClassificationTypeContentType).
builtin_rn('MarketDataProductClassificationType-AssetType', ttMarketDataProductClassificationTypeAssetType).
builtin_rn('MarketDataProductClassificationType', ttMarketDataProductClassificationType).
builtin_rn('ColorPerceptionCategoryType', ttColorPerceptionCategoryType).
builtin_rn('ThePrototypicalSocialQuantityType', ttThePrototypicalSocialQuantityType).
builtin_rn('ConstraintLanguageExpression', ftConstraintLanguageExpression).
builtin_rn('ExtensionalRepresentationPredicate', rtExtensionalRepresentationPredicate).
builtin_rn('RelationPredicate', rtRelationPredicate).
builtin_rn('FunctionOrFunctionalPredicate', rtFunctionOrFunctionalPredicate).
builtin_rn('CycSetExpression', ftCycSetExpression).
builtin_rn('SymmetricalPartType', ttSymmetricalPartType).
builtin_rn('RFProductType', ttRFProductType).
builtin_rn('RFObjectType', ttRFObjectType).
builtin_rn('RFExpenseType', ttRFExpenseType).
builtin_rn('RFVehicleType', ttRFVehicleType).
builtin_rn('RFIncomeType', ttRFIncomeType).
builtin_rn('RFPhysiologicalConditionType', ttRFPhysiologicalConditionType).
builtin_rn('Tie-ClothingType', ttTieClothingType).
builtin_rn('CompositionPredicate', rtCompositionPredicate).
builtin_rn('PersonByActivityType', ttPersonByActivityType).
builtin_rn('OccupationType', ttOccupationType).
builtin_rn('PositionType', ttPositionType).
builtin_rn('UniquePartType', ttUniquePartType).
builtin_rn('MeasurableAttributeType', ttMeasurableAttributeType).
builtin_rn('InterExistingObjectSlot', rtInterExistingObjectSlot).
builtin_rn('MutuallyDisjointIntervalCollection', ttMutuallyDisjointIntervalCollection).
builtin_rn('FoodGroupType', ttFoodGroupType).
builtin_rn('PrimitiveTemporalRelation', rtPrimitiveTemporalRelation).
builtin_rn('ComplexTemporalRelation', rtComplexTemporalRelation).
builtin_rn('TemporalPartSlot', rtTemporalPartSlot).
builtin_rn('SubEventSlot', rtSubEventSlot).
builtin_rn('EthnicGroupType', ttEthnicGroupType).
builtin_rn('PureCompoundType', ttPureCompoundType).
builtin_rn('ChemicalCompoundType', ttChemicalCompoundType).
builtin_rn('DirectionExpression', ftDirectionExpression).
builtin_rn('eyeColor-Old', eyeColorOld).
builtin_rn('CollectionPredicate', rtCollectionPredicate).
builtin_rn('TypePredicate', rtTypePredicate).
builtin_rn('colorSchemeOf-Old', colorSchemeOfOld).
builtin_rn('SpatiallyIntrinsicSlot', rtSpatiallyIntrinsicSlot).
builtin_rn('IntendedFunction', rtIntendedFunction).
builtin_rn('MainFunction', rtMainFunction).
builtin_rn('MentalAttributeDescriptionPredicate', rtMentalAttributeDescriptionPredicate).
builtin_rn('courseOfStudyLevel-Coll', courseOfStudyLevelColl).
builtin_rn('AttributeType', ttAttributeType).
builtin_rn('PhysicalFeatureDescribingPredicate', rtPhysicalFeatureDescribingPredicate).
builtin_rn('PhysicalAttributeDescriptionSlot', rtPhysicalAttributeDescriptionSlot).
builtin_rn('CotemporalObjectsSlot', rtCotemporalObjectsSlot).
builtin_rn('MarketCategoryType', ttMarketCategoryType).
builtin_rn('PrimitiveScalarIntervalType', ttPrimitiveScalarIntervalType).
builtin_rn('PrimitiveQuantityType', ttPrimitiveQuantityType).
builtin_rn('DerivedQuantityType', ttDerivedQuantityType).
builtin_rn('QuantitySlot', rtQuantitySlot).
builtin_rn('IntervalBasedQuantitySlot', rtIntervalBasedQuantitySlot).
builtin_rn('WorkAccountType', ttWorkAccountType).
builtin_rn('OrganismType', ttOrganismType).
builtin_rn('UnitOfMeasureTaxonomicType', ttUnitOfMeasureTaxonomicType).
builtin_rn('EnumeratedType', ttEnumeratedType).
builtin_rn('MutuallyDisjointAttributeType', ttMutuallyDisjointAttributeType).
builtin_rn('PathSystemType', ttPathSystemType).
builtin_rn('RelationExpression', ftRelationExpression).
builtin_rn('PersonTypeByLifeStageType', ttPersonTypeByLifeStageType).
builtin_rn('CycArgumentByJustificationType', ttCycArgumentByJustificationType).
builtin_rn('MentalSlot', rtMentalSlot).
builtin_rn('testStatus-Manual', testStatusManual).
builtin_rn('LinearOrderQuantityType', ttLinearOrderQuantityType).
builtin_rn('InterPersonalRelationSlot', rtInterPersonalRelationSlot).
builtin_rn('TaxType', ttTaxType).
builtin_rn('InternetNewsgroupType', ttInternetNewsgroupType).
builtin_rn('MagazineSeriesType', ttMagazineSeriesType).
builtin_rn('DramaticPerformanceType', ttDramaticPerformanceType).
builtin_rn('Play-DramaticType', ttPlayDramaticType).
builtin_rn('OperaType', ttOperaType).
builtin_rn('SpecifiedPlay-MusicalType', ttSpecifiedPlayMusicalType).
builtin_rn('PhaseIIBattlespaceSource-PITType', ttPhaseIIBattlespaceSourcePITType).
builtin_rn('MilitaryFieldManual-PITType', ttMilitaryFieldManualPITType).
builtin_rn('PublishedEdition-PITType', ttPublishedEditionPITType).
builtin_rn('sourceOfTerm-PIT', sourceOfTermPIT).
builtin_rn('SpecifiedElectronicsProductType', ttSpecifiedElectronicsProductType).
builtin_rn('SpecifiedComputerRoleplayingGameType', ttSpecifiedComputerRoleplayingGameType).
builtin_rn('SpecifiedComputerActionGameType', ttSpecifiedComputerActionGameType).
builtin_rn('SpecifiedComputerArcadeGameType', ttSpecifiedComputerArcadeGameType).
builtin_rn('SpecifiedComputerBoardGameType', ttSpecifiedComputerBoardGameType).
builtin_rn('SpecifiedComputerSportsGameType', ttSpecifiedComputerSportsGameType).
builtin_rn('SpecifiedComputerEducationalGameType', ttSpecifiedComputerEducationalGameType).
builtin_rn('SpecifiedComputerSimulationGameType', ttSpecifiedComputerSimulationGameType).
builtin_rn('SpecifiedComputerStrategyGameType', ttSpecifiedComputerStrategyGameType).
builtin_rn('formalityOfWS-New', formalityOfWSNew).
builtin_rn('politenessOfWS-New', politenessOfWSNew).
builtin_rn('InsuranceType', ttInsuranceType).
builtin_rn('SocialAttributeType', ttSocialAttributeType).
builtin_rn('UnorderedAttributeType', ttUnorderedAttributeType).
builtin_rn('HumanTypeByHairColorType', ttHumanTypeByHairColorType).
builtin_rn('ExceptionRelation', rtExceptionRelation).
builtin_rn('SpecifiedComputerPuzzleGameType', ttSpecifiedComputerPuzzleGameType).
builtin_rn('SpecifiedComputerAdventureGameType', ttSpecifiedComputerAdventureGameType).
builtin_rn('PowerChess98-SpecifiedComputerGameType', ttPowerChess98SpecifiedComputerGameType).
builtin_rn('TheDShow-SpecifiedComputerGameType', ttTheDShowSpecifiedComputerGameType).
builtin_rn('CalvinAndHobbes-SpecifiedComicStripSeriesType', ttCalvinAndHobbesSpecifiedComicStripSeriesType).
builtin_rn('CarTalk-SpecifiedRadioSeriesType', ttCarTalkSpecifiedRadioSeriesType).
builtin_rn('SyndromeType', ttSyndromeType).
builtin_rn('TheDrLauraShow-SpecifiedRadioSeriesType', ttTheDrLauraShowSpecifiedRadioSeriesType).
builtin_rn('adjSemTrans-New', adjSemTransNew).
builtin_rn('adjSemTrans-Restricted-New', adjSemTransRestrictedNew).
builtin_rn('SupportGroupFn-FeelingAttributeType', ttSupportGroupFnFeelingAttributeType).
builtin_rn('Album-RecordingType', ttAlbumRecordingType).
builtin_rn('Single-RecordingType', ttSingleRecordingType).
builtin_rn('SpecifiedMusicTrackType', ttSpecifiedMusicTrackType).
builtin_rn('AttributeCoveringType', ttAttributeCoveringType).
builtin_rn('AttributePartitionType', ttAttributePartitionType).
builtin_rn('KBDescriptorPredicate', rtKBDescriptorPredicate).
builtin_rn('CycSystemDescriptorPredicate', rtCycSystemDescriptorPredicate).
builtin_rn('HumanTypeByPhysiologicalConditionType', ttHumanTypeByPhysiologicalConditionType).
builtin_rn('BusinessByActivityType', ttBusinessByActivityType).
builtin_rn('SpecifiedAcademicTestType', ttSpecifiedAcademicTestType).
builtin_rn('SpecifiedPsychologicalTestType', ttSpecifiedPsychologicalTestType).
builtin_rn('PersonByFacetedActivityType', ttPersonByFacetedActivityType).
builtin_rn('AlternativeMedicineOccupationType', ttAlternativeMedicineOccupationType).
builtin_rn('HumanSubcultureType', ttHumanSubcultureType).
builtin_rn('EIAttack-Espionage-PurposeType', ttEIAttackEspionagePurposeType).
builtin_rn('SubLExpression', ftSubLExpression).
builtin_rn('InformationLeakByOrgType', ttInformationLeakByOrgType).
builtin_rn('PartialDenotationalFunction', rtPartialDenotationalFunction).
builtin_rn('TotalDenotationalFunction', rtTotalDenotationalFunction).
builtin_rn('SpecifiedFoodType', ttSpecifiedFoodType).
builtin_rn('EIAttack-Agent-PurposeType', ttEIAttackAgentPurposeType).
builtin_rn('EIAttack-Military-PurposeType', ttEIAttackMilitaryPurposeType).
builtin_rn('EIAttack-Corporate-PurposeType', ttEIAttackCorporatePurposeType).
builtin_rn('EIAttack-Personal-PurposeType', ttEIAttackPersonalPurposeType).
builtin_rn('HobbyFn-ScriptType', ttHobbyFnScriptType).
builtin_rn('favoriteColorOfPerson-Old', favoriteColorOfPersonOld).
builtin_rn('nounSemTrans-new', nounSemTransNew).
builtin_rn('agentiveNounSemTrans-new', agentiveNounSemTransNew).
builtin_rn('massNounSemTrans-new', massNounSemTransNew).
builtin_rn('BinaryQuantityOrValuePredicate', rtBinaryQuantityOrValuePredicate).
builtin_rn('PortableAudioEquipmentByComponentType', ttPortableAudioEquipmentByComponentType).
builtin_rn('CumulativeType', ttCumulativeType).
builtin_rn('AntiDavidsonianPredicate', rtAntiDavidsonianPredicate).
builtin_rn('multiWordSemTrans-new', multiWordSemTransNew).
builtin_rn('AttireType', ttAttireType).
builtin_rn('AncienEgypt-LouvreCollection', ttAncienEgyptLouvreCollection).
builtin_rn('reproducesAsexually-MicroorganismMob', reproducesAsexuallyMicroorganismMob).
builtin_rn('ArgGenlQuanityBinaryPredicate', rtArgGenlQuanityBinaryPredicate).
builtin_rn('ArgGenlQuanityPredicate', rtArgGenlQuanityPredicate).
builtin_rn('granuleOfStuff-Generic', granuleOfStuffGeneric).
builtin_rn('compoundSemTrans-new', compoundSemTransNew).
builtin_rn('ScriptRelation', rtScriptRelation).
builtin_rn('BodyPartTypeForClothingType', ttBodyPartTypeForClothingType).
builtin_rn('CycSystemRelation', rtCycSystemRelation).
builtin_rn('EucaryoticRNAType', ttEucaryoticRNAType).
builtin_rn('ConservativeGeneralizedQuantifier', rtConservativeGeneralizedQuantifier).
builtin_rn('ExtensionalGeneralizedQuantifier', rtExtensionalGeneralizedQuantifier).
builtin_rn('EucaryoticGeneExpression', ftEucaryoticGeneExpression).
builtin_rn('ProcaryoticGeneExpression', ftProcaryoticGeneExpression).
builtin_rn('IdentifyingAnObjectAsType', ttIdentifyingAnObjectAsType).
builtin_rn('OrganizationByActivityType', ttOrganizationByActivityType).
builtin_rn('primaryFunction-AgnosticIntentional', primaryFunctionAgnosticIntentional).
builtin_rn('soleFunction-AgnosticIntentional', soleFunctionAgnosticIntentional).
builtin_rn('most-GenQuantRelnFromInstance', mostGenQuantRelnFromInstance).
builtin_rn('both-GenQuant', bothGenQuant).
builtin_rn('many-GenQuant', manyGenQuant).
builtin_rn('psRuleTemplateBindings-New', psRuleTemplateBindingsNew).
builtin_rn('PossibleDefinitionalPredicate', rtPossibleDefinitionalPredicate).
builtin_rn('BPVI-MilitaryUnitType', ttBPVIMilitaryUnitType).
builtin_rn('TransitiveViaPredicate', rtTransitiveViaPredicate).
builtin_rn('BPVI-UnitSpecialtyClassificationType', ttBPVIUnitSpecialtyClassificationType).
builtin_rn('BPVI-EquipmentClassificationType', ttBPVIEquipmentClassificationType).
builtin_rn('transportsTo-NEW', transportsToNEW).
builtin_rn('transports-NEW', transportsNEW).
builtin_rn('typesChemicalBondBetweenCapable-New', typesChemicalBondBetweenCapableNew).
builtin_rn('CodonType', ttCodonType).
builtin_rn('KillingTypeThroughEventType', ttKillingTypeThroughEventType).
builtin_rn('VariableOrderCollection', ttVariableOrderCollection).
builtin_rn('ActionQualifyingFunction', rtActionQualifyingFunction).
builtin_rn('no-GenQuant', noGenQuant).
builtin_rn('SpecDenotingFunction', rtSpecDenotingFunction).
builtin_rn('SceneDenotingFunction', rtSceneDenotingFunction).
builtin_rn('SoftwareExpression', ftSoftwareExpression).
builtin_rn('PrimitiveUnitType', ttPrimitiveUnitType).
builtin_rn('DynamicSizeDataType', ttDynamicSizeDataType).
builtin_rn('Double-TheDataType', ttDoubleTheDataType).
builtin_rn('Void-TheDataType', ttVoidTheDataType).
builtin_rn('USCensusRace&OriginType', 'ttUSCensusRace&OriginType').
builtin_rn('WebSearchQuaternaryRuleMacroPredicate', rtWebSearchQuaternaryRuleMacroPredicate).
builtin_rn('WebSearchTernaryRuleMacroPredicate', rtWebSearchTernaryRuleMacroPredicate).
builtin_rn('NonPredicateTruthFunction', rtNonPredicateTruthFunction).
builtin_rn('echelonOfUnit-Coll', echelonOfUnitColl).
builtin_rn('RFMonthType', ttRFMonthType).
builtin_rn('governmentType-Coll', governmentTypeColl).
builtin_rn('jobPositionOpeningStatus-Coll', jobPositionOpeningStatusColl).
builtin_rn('InfectionSiteType', ttInfectionSiteType).
builtin_rn('ClothingStyleType', ttClothingStyleType).
builtin_rn('InterAgentRelationCollectionType', ttInterAgentRelationCollectionType).
builtin_rn('GeoEntityByNationalBehaviorRelationType', ttGeoEntityByNationalBehaviorRelationType).
builtin_rn('DocumentStructureType', ttDocumentStructureType).
builtin_rn('IndividualByFlawType', ttIndividualByFlawType).
builtin_rn('Military-ActionType', ttMilitaryActionType).
builtin_rn('classificationOfInformation-Coll', classificationOfInformationColl).
builtin_rn('designationOfInformation-Coll', designationOfInformationColl).
builtin_rn('designationOfThing-Coll', designationOfThingColl).
builtin_rn('conversationStatus-Coll', conversationStatusColl).
builtin_rn('positionStatusOfArea-Coll', positionStatusOfAreaColl).
builtin_rn('movieAdvisoryRating-Coll', movieAdvisoryRatingColl).
builtin_rn('featureOfPathArtifact-Coll', featureOfPathArtifactColl).
builtin_rn('nucleicAcidSequenceConnectivity-Coll', nucleicAcidSequenceConnectivityColl).
builtin_rn('illuminationLevel-Coll', illuminationLevelColl).
builtin_rn('transparencyOfObject-Coll', transparencyOfObjectColl).
builtin_rn('resistanceToBioDeterioration-Coll', resistanceToBioDeteriorationColl).
builtin_rn('impactAbsorptionOfObject-Coll', impactAbsorptionOfObjectColl).
builtin_rn('colorHasHue-Coll', colorHasHueColl).
builtin_rn('colorHasBrightness-Coll', colorHasBrightnessColl).
builtin_rn('colorHasChroma-Coll', colorHasChromaColl).
builtin_rn('hasOrbitalPathClass-Coll', hasOrbitalPathClassColl).
builtin_rn('complexityOfConfiguration-Coll', complexityOfConfigurationColl).
builtin_rn('uniformColorOfObject-Coll', uniformColorOfObjectColl).
builtin_rn('mainColorOfObject-Coll', mainColorOfObjectColl).
builtin_rn('significantColorOfObject-Coll', significantColorOfObjectColl).
builtin_rn('rightsGranted-Coll', rightsGrantedColl).
builtin_rn('accountStatus-Coll', accountStatusColl).
builtin_rn('proposalStatus-Coll', proposalStatusColl).
builtin_rn('locationState-Coll', locationStateColl).
builtin_rn('activityCriticality-Coll', activityCriticalityColl).
builtin_rn('financialState-Coll', financialStateColl).
builtin_rn('levelOfEncryption-Coll', levelOfEncryptionColl).
builtin_rn('levelOfSecurity-Coll', levelOfSecurityColl).
builtin_rn('hasNumberingSchema-Coll', hasNumberingSchemaColl).
builtin_rn('architecturalStyle-Coll', architecturalStyleColl).
builtin_rn('artisticStyle-Coll', artisticStyleColl).
builtin_rn('clothingStyle-Coll', clothingStyleColl).
builtin_rn('hasStyle-Coll', hasStyleColl).
builtin_rn('mtAnthropacity-Coll', mtAnthropacityColl).
builtin_rn('organismsExistentialState-Coll', organismsExistentialStateColl).
builtin_rn('agentsExistentialState-Coll', agentsExistentialStateColl).
builtin_rn('constructionExistentialState-Coll', constructionExistentialStateColl).
builtin_rn('datingStatus-Coll', datingStatusColl).
builtin_rn('maritalStatus-Coll', maritalStatusColl).
builtin_rn('skinColor-Coll', skinColorColl).
builtin_rn('eventHasStyle-Coll', eventHasStyleColl).
builtin_rn('processorArchitectureOfComputer-Coll', processorArchitectureOfComputerColl).
builtin_rn('tasteOfObject-Coll', tasteOfObjectColl).
builtin_rn('objectEmitsOdor-Coll', objectEmitsOdorColl).
builtin_rn('placeHasOdor-Coll', placeHasOdorColl).
builtin_rn('hasSecurityClearance-Coll', hasSecurityClearanceColl).
builtin_rn('classificationOfReport-Coll', classificationOfReportColl).
builtin_rn('reliabilityOfReportSources-Coll', reliabilityOfReportSourcesColl).
builtin_rn('loanTermOptions-Coll', loanTermOptionsColl).
builtin_rn('loanDedication-Coll', loanDedicationColl).
builtin_rn('loanInterestOptions-Coll', loanInterestOptionsColl).
builtin_rn('loanPrincipal-Coll', loanPrincipalColl).
builtin_rn('styleOfMusicPerformer-Coll', styleOfMusicPerformerColl).
builtin_rn('styleOfArtist-Coll', styleOfArtistColl).
builtin_rn('localOrganizationOpennessState-Coll', localOrganizationOpennessStateColl).
builtin_rn('hasZodiacSign-Coll', hasZodiacSignColl).
builtin_rn('portalState-Coll', portalStateColl).
builtin_rn('softwareFeatures-Coll', softwareFeaturesColl).
builtin_rn('stateOfHealth-Coll', stateOfHealthColl).
builtin_rn('stateOfDevice-Coll', stateOfDeviceColl).
builtin_rn('lockState-Coll', lockStateColl).
builtin_rn('bodyHairLevel-Coll', bodyHairLevelColl).
builtin_rn('postureOfAnimal-Coll', postureOfAnimalColl).
builtin_rn('executePrivilegeOfProgramCopy-Coll', executePrivilegeOfProgramCopyColl).
builtin_rn('activityState-Coll', activityStateColl).
builtin_rn('movieGenres-Coll', movieGenresColl).
builtin_rn('cwGenre-Coll', cwGenreColl).
builtin_rn('listHasFormat-Coll', listHasFormatColl).
builtin_rn('listHasOrder-Coll', listHasOrderColl).
builtin_rn('classificationOfOrder-Coll', classificationOfOrderColl).
builtin_rn('workload-Coll', workloadColl).
builtin_rn('rank-Military-Coll', rankMilitaryColl).
builtin_rn('troopStrengthOfUnit-Coll', troopStrengthOfUnitColl).
builtin_rn('hasComplementaryBasePairingProperty-Coll', hasComplementaryBasePairingPropertyColl).
builtin_rn('verdict-Coll', verdictColl).
builtin_rn('COAEvaluationPredicate', rtCOAEvaluationPredicate).
builtin_rn('IterativeScriptedEventType', ttIterativeScriptedEventType).
builtin_rn('MDMPInteractionDimension', rtMDMPInteractionDimension).
builtin_rn('COAAnalysisEvaluationType', ttCOAAnalysisEvaluationType).
builtin_rn('AccountSystemType', ttAccountSystemType).
builtin_rn('FileSystemType', ttFileSystemType).
builtin_rn('objectHasColor-Coll', objectHasColorColl).
builtin_rn('ScriptRoleConstraintPredicate', rtScriptRoleConstraintPredicate).
builtin_rn('AbductionRelation', rtAbductionRelation).
builtin_rn('NonEntityCollection', ttNonEntityCollection).
builtin_rn('typeHasStyle-Coll', typeHasStyleColl).
builtin_rn('hairColor-Coll', hairColorColl).
builtin_rn('KFD-KBContentTestSpecificationType', ttKFDKBContentTestSpecificationType).
builtin_rn('followingValueOnScale-Coll', followingValueOnScaleColl).
builtin_rn('SKSIContentTestSpecificationType', ttSKSIContentTestSpecificationType).
builtin_rn('sizeOfBedding-Coll', sizeOfBeddingColl).
builtin_rn('sizeOfBed-Coll', sizeOfBedColl).
builtin_rn('schooling-Coll', schoolingColl).
builtin_rn('educationLevel-Coll', educationLevelColl).
builtin_rn('humanMeasurements-Coll', humanMeasurementsColl).
builtin_rn('clothingSize-Coll', clothingSizeColl).
builtin_rn('ploidy-Organism-Coll', ploidyOrganismColl).
builtin_rn('clothingTypeFormality-Coll', clothingTypeFormalityColl).
builtin_rn('clothingFormality-Coll', clothingFormalityColl).
builtin_rn('colorOfType-Coll', colorOfTypeColl).
builtin_rn('stateOfAilment-Coll', stateOfAilmentColl).
builtin_rn('NuclearSubstance-BBNEntityType', ttNuclearSubstanceBBNEntityType).
builtin_rn('PotentialChemicalTreat-BBNEntityType', ttPotentialChemicalTreatBBNEntityType).
builtin_rn('PotentialBiologicalTreat-BBNEntityType', ttPotentialBiologicalTreatBBNEntityType).
builtin_rn('HazardousMaterialType', ttHazardousMaterialType).
builtin_rn('userRightsRelation-Coll', userRightsRelationColl).
builtin_rn('IntendedOrientation-DeviceType', ttIntendedOrientationDeviceType).
builtin_rn('EntityNonInitialIntermittentCollection', ttEntityNonInitialIntermittentCollection).
builtin_rn('IntermittantCollection', ttIntermittantCollection).
builtin_rn('softwareDesignedForArchitecture-Coll', softwareDesignedForArchitectureColl).
builtin_rn('ProductTypeByFunction', rtProductTypeByFunction).
builtin_rn('zodiacSignDateRange-Coll', zodiacSignDateRangeColl).
builtin_rn('succeedingValueOnScale-Coll', succeedingValueOnScaleColl).
builtin_rn('followingOrEqualValueOnScale-Coll', followingOrEqualValueOnScaleColl).
builtin_rn('TroopStrengthRatioPercentType', ttTroopStrengthRatioPercentType).
builtin_rn('hungerLevelOf-Coll', hungerLevelOfColl).
builtin_rn('InterArgExactIsaPredicate', rtInterArgExactIsaPredicate).
builtin_rn('SomethingToWearByGenericType', ttSomethingToWearByGenericType).
builtin_rn('ScriptProcessingEventPredicate', rtScriptProcessingEventPredicate).
builtin_rn('flawAccordingToAgent-Coll', flawAccordingToAgentColl).
builtin_rn('WinningType', ttWinningType).
builtin_rn('DiscourseStructurePredicate', rtDiscourseStructurePredicate).
builtin_rn('HandPositionType', ttHandPositionType).
builtin_rn('UnchangedActorsType', ttUnchangedActorsType).
builtin_rn('participantStatus-Coll', participantStatusColl).
builtin_rn('IndividualMilitaryUnit-TemplateType', ttIndividualMilitaryUnitTemplateType).
builtin_rn('ScriptFunction', rtScriptFunction).
builtin_rn('ScriptRelation-Scenes-Type', ttScriptRelationScenesType).
builtin_rn('ScriptRelation-Predicate', rtScriptRelationPredicate).
builtin_rn('MilitaryEquipment-TemplateType', ttMilitaryEquipmentTemplateType).
builtin_rn('TypesOfMilitaryEquipment-TopicType', ttTypesOfMilitaryEquipmentTopicType).
builtin_rn('MilitaryWeaponTypes-TopicType', ttMilitaryWeaponTypesTopicType).
builtin_rn('TypesOfMilitaryPlatform-TopicType', ttTypesOfMilitaryPlatformTopicType).
builtin_rn('RKF-SME-ProjectileLancherType', ttRKFSMEProjectileLancherType).
builtin_rn('MilitaryWeaponTypes-TemplateType', ttMilitaryWeaponTypesTemplateType).
builtin_rn('GoalPredicate', rtGoalPredicate).
builtin_rn('MaxFordingDepthOfVehicle-RKFTemplateType', ttMaxFordingDepthOfVehicleRKFTemplateType).
builtin_rn('MilitaryDefinedTerrainType', ttMilitaryDefinedTerrainType).
builtin_rn('IndividualTerrain-TopicType', ttIndividualTerrainTopicType).
builtin_rn('MilitaryUnitType', ttMilitaryUnitType).
builtin_rn('RenderingPropertyPredicate', rtRenderingPropertyPredicate).
builtin_rn('DoubleLine-BorderType', ttDoubleLineBorderType).
builtin_rn('SingleLine-BorderType', ttSingleLineBorderType).
builtin_rn('PotentialTargetForScriptedAttackType', ttPotentialTargetForScriptedAttackType).
builtin_rn('IterationTerminationScriptPredicate', rtIterationTerminationScriptPredicate).
builtin_rn('SKSIKBIntegrityTestSpecificationType', ttSKSIKBIntegrityTestSpecificationType).
builtin_rn('SKSITestSpecificationType', ttSKSITestSpecificationType).
builtin_rn('SharedElementPredicate', rtSharedElementPredicate).
builtin_rn('TemporalExistenceEntailingPredicate', rtTemporalExistenceEntailingPredicate).
builtin_rn('OnceTrueTrueTillDeathOfAnArgPredicate', rtOnceTrueTrueTillDeathOfAnArgPredicate).
builtin_rn('ExperimentalSitTypePredicate', rtExperimentalSitTypePredicate).
builtin_rn('ConversionToTemporallyQualified-TopicType', ttConversionToTemporallyQualifiedTopicType).
builtin_rn('FacetPredicate', rtFacetPredicate).
builtin_rn('wordIndexesCycTerm-IR', wordIndexesCycTermIR).
builtin_rn('CoExistenceRelation', rtCoExistenceRelation).
builtin_rn('ActorInMovies-AcademyAwards-TemplateType', ttActorInMoviesAcademyAwardsTemplateType).
builtin_rn('SoftwareTestSpecificationTestOrType', ttSoftwareTestSpecificationTestOrType).
builtin_rn('LoanAgreementTypeByFundingSourceType', ttLoanAgreementTypeByFundingSourceType).
builtin_rn('SituationTypeMatchingPredicate', rtSituationTypeMatchingPredicate).
builtin_rn('SKSISupportedMathFunction', rtSKSISupportedMathFunction).
builtin_rn('WhClauseFrameType', ttWhClauseFrameType).
builtin_rn('MajorCreditCardType', ttMajorCreditCardType).
builtin_rn('suspects-Prop', suspectsProp).
builtin_rn('UnaryEvalautionPredicate', rtUnaryEvalautionPredicate).
builtin_rn('PictorialGenreType', ttPictorialGenreType).
builtin_rn('CartographicalFeatureType', ttCartographicalFeatureType).
builtin_rn('CST-WhichMonthsHaveThirtyDays-durationOfType', ttCSTWhichMonthsHaveThirtyDaysDurationOfType).
builtin_rn('SOEGraphablePredicate', rtSOEGraphablePredicate).
builtin_rn('AgentDataSentencePredicate', rtAgentDataSentencePredicate).
builtin_rn('EventLocationAndDatePredicate', rtEventLocationAndDatePredicate).
builtin_rn('RCCRelation', rtRCCRelation).
builtin_rn('contractee-New', contracteeNew).
builtin_rn('intervalOfTypeLiesBetween-Exclusive-New', intervalOfTypeLiesBetweenExclusiveNew).
builtin_rn('ShapeType', ttShapeType).
builtin_rn('defaultTinyIconTermImagePathnameForType-TillDeathOfAnArg-AnySolidColorOtherThanBlackVariety', defaultTinyIconTermImagePathnameForTypeTillDeathOfAnArgAnySolidColorOtherThanBlackVariety).
builtin_rn('FeelingType', ttFeelingType).
builtin_rn('ShapeDescribingPredicate', rtShapeDescribingPredicate).
builtin_rn('OutdoorLocationByWeatherType', ttOutdoorLocationByWeatherType).
builtin_rn('OutdoorLocationByIlluminationType', ttOutdoorLocationByIlluminationType).
builtin_rn('OutdoorLocationByAmbientConditionType', ttOutdoorLocationByAmbientConditionType).
builtin_rn('ClimaticTerrainType', ttClimaticTerrainType).
builtin_rn('TemporallyQualifiableRelation', rtTemporallyQualifiableRelation).
builtin_rn('TimeDependentRelation', rtTimeDependentRelation).
builtin_rn('ShapeTypeFunction', rtShapeTypeFunction).
builtin_rn('OwSemanticDataType', ttOwSemanticDataType).
builtin_rn('Edited-TEPProvenanceType', ttEditedTEPProvenanceType).
builtin_rn('Unedited-TEPProvenanceType', ttUneditedTEPProvenanceType).
builtin_rn('ILPTest-BodyPred-In-UnboundForType', ttILPTestBodyPredInUnboundForType).
builtin_rn('ILPTest-HeadPred-In-UnboundForType', ttILPTestHeadPredInUnboundForType).
builtin_rn('ClayBasedFines-SoilType', ttClayBasedFinesSoilType).
builtin_rn('VehicleType', ttVehicleType).
builtin_rn('EventCasualtyPredicate', rtEventCasualtyPredicate).
builtin_rn('CasualtyDesignatingPredicate', rtCasualtyDesignatingPredicate).
builtin_rn('GeographicalRegionPredicate', rtGeographicalRegionPredicate).
builtin_rn('SystemEntityPropertyFunction', rtSystemEntityPropertyFunction).
builtin_rn('Endangered-OrganismClassificationType', ttEndangeredOrganismClassificationType).
builtin_rn('probabilityOfSet-Generic', probabilityOfSetGeneric).
builtin_rn('probability-Numeric', probabilityNumeric).
builtin_rn('ArtisticStyleType', ttArtisticStyleType).
builtin_rn('ArchitecturalStyleType', ttArchitecturalStyleType).
builtin_rn('WordNetSpeechPartType', ttWordNetSpeechPartType).
builtin_rn('UnderwaterTerrainFeatureType', ttUnderwaterTerrainFeatureType).
builtin_rn('kineticEnergyOfObject-Translational', kineticEnergyOfObjectTranslational).
builtin_rn('evincesGAF-Generic', evincesGAFGeneric).
builtin_rn('8ByteRealDataType', tt8ByteRealDataType).
builtin_rn('4ByteRealDataType', tt4ByteRealDataType).
builtin_rn('RealNumberDataType', ttRealNumberDataType).
builtin_rn('ComputerDataType', ttComputerDataType).
builtin_rn('BitStringDataType', ttBitStringDataType).
builtin_rn('FixedSizeComputerDataType', ttFixedSizeComputerDataType).
builtin_rn('CompositeDataType', ttCompositeDataType).
builtin_rn('ObjectReferenceDataType', ttObjectReferenceDataType).
builtin_rn('DefinedDataType', ttDefinedDataType).
builtin_rn('YahooFinanceStockQuotePagePredicate', rtYahooFinanceStockQuotePagePredicate).
builtin_rn('PhysicalOrderingPredicate', rtPhysicalOrderingPredicate).
builtin_rn('instanceDiffersFromPrototypicalInstance-Additions', instanceDiffersFromPrototypicalInstanceAdditions).
builtin_rn('instanceDiffersFromPrototypicalInstance-Lacunae', instanceDiffersFromPrototypicalInstanceLacunae).
builtin_rn('instanceDiffersFromPrototypicalInstance-Conflicts', instanceDiffersFromPrototypicalInstanceConflicts).
builtin_rn('HighestInValueModifierType', ttHighestInValueModifierType).
builtin_rn('OtherSurgicalIncisionType', ttOtherSurgicalIncisionType).
builtin_rn('startsNoEarlierThanStartingOf-2', startsNoEarlierThanStartingOf_2).
builtin_rn('NearlyFunctionalSlot', rtNearlyFunctionalSlot).
builtin_rn('CCFPreopPatientStatusType', ttCCFPreopPatientStatusType).
builtin_rn('Paced-HeartRhythmType', ttPacedHeartRhythmType).
builtin_rn('OrganSystemType', ttOrganSystemType).
builtin_rn('GeograhpicalRegionTypeByAdministratorType', ttGeograhpicalRegionTypeByAdministratorType).
builtin_rn('SystemSlot', rtSystemSlot).
builtin_rn('aortaAneurysmSize-1', aortaAneurysmSize_1).
builtin_rn('DisjointMedicalProcedureType', ttDisjointMedicalProcedureType).
builtin_rn('DrugModeOfActionType', ttDrugModeOfActionType).
builtin_rn('ProteinMoleculeType', ttProteinMoleculeType).
builtin_rn('StockType', ttStockType).
builtin_rn('operationType-7', operationType_7).
builtin_rn('hasIndicationForICUAdmission-8', hasIndicationForICUAdmission_8).
builtin_rn('CCFCardiacValveType', ttCCFCardiacValveType).
builtin_rn('priorHistoryOfConditionOnPartTypeWRTEvent-NoLoop', priorHistoryOfConditionOnPartTypeWRTEventNoLoop).
builtin_rn('ReportGeneratorReportTypeType', ttReportGeneratorReportTypeType).
builtin_rn('ETQ-GeneToMoleculeType', ttETQGeneToMoleculeType).
builtin_rn('ChromosomeTypeByCellStageType', ttChromosomeTypeByCellStageType).
builtin_rn('cmuReadTheWeb-MappingPredicate', cmuReadTheWebMappingPredicate).
builtin_rn('ProcessingAlottmentType', ttProcessingAlottmentType).
builtin_rn('causes-SubSitTypeSubSitType-NotNecUnique', causesSubSitTypeSubSitTypeNotNecUnique).
builtin_rn('JCIDSCurrentKBStateComputingPredicate', rtJCIDSCurrentKBStateComputingPredicate).
builtin_rn('ProteinTypeByChemicalCompoundType', ttProteinTypeByChemicalCompoundType).
builtin_rn('HasExpertiseInSimilarSoftwareWRTType', ttHasExpertiseInSimilarSoftwareWRTType).
builtin_rn('MemCpyFunction', rtMemCpyFunction).
builtin_rn('ReadFileFunction', rtReadFileFunction).
builtin_rn('WMemCpyFunction', rtWMemCpyFunction).
builtin_rn('StrNCpyFunction', rtStrNCpyFunction).
builtin_rn('StrCpyFunction', rtStrCpyFunction).
builtin_rn('MemMoveFunction', rtMemMoveFunction).
builtin_rn('MemCCopyFunction', rtMemCCopyFunction).
builtin_rn('BCopyFunction', rtBCopyFunction).
builtin_rn('VsnprintFFunction', rtVsnprintFFunction).
builtin_rn('VsprintFFunction', rtVsprintFFunction).
builtin_rn('SnprintFFunction', rtSnprintFFunction).
builtin_rn('SprintFFunction', rtSprintFFunction).
builtin_rn('PyrimidineType', ttPyrimidineType).
builtin_rn('PurineType', ttPurineType).
builtin_rn('TheLocaltimeFunction', rtTheLocaltimeFunction).
builtin_rn('TheLocaltime_rFunction', rtTheLocaltime_rFunction).
builtin_rn('TheGmtimeFunction', rtTheGmtimeFunction).
builtin_rn('TheGmtime_rFunction', rtTheGmtime_rFunction).
builtin_rn('TheRandFunction', rtTheRandFunction).
builtin_rn('TheRand_rFunction', rtTheRand_rFunction).
builtin_rn('TheGetLastErrorFunction', rtTheGetLastErrorFunction).
builtin_rn('TheLoadLibraryFunction', rtTheLoadLibraryFunction).
builtin_rn('TheSigactionFunction', rtTheSigactionFunction).
builtin_rn('TheCloseHandleFunction', rtTheCloseHandleFunction).
builtin_rn('TheExitProcessFunction', rtTheExitProcessFunction).
builtin_rn('TheCopyFileFunction', rtTheCopyFileFunction).
builtin_rn('codingFunctionArity-4', codingFunctionArity_4).
builtin_rn('OntologyAnnotationPredicate', rtOntologyAnnotationPredicate).
builtin_rn('ThePrintfFunction', rtThePrintfFunction).
builtin_rn('TheWaitForSingleObjectExFunction', rtTheWaitForSingleObjectExFunction).
builtin_rn('couldImpact-WeakEvidence', couldImpactWeakEvidence).
builtin_rn('TheIsspaceFunction', rtTheIsspaceFunction).
builtin_rn('CPTemplate-OppositePhasesWRTStructureType', ttCPTemplateOppositePhasesWRTStructureType).
builtin_rn('The_malloc_dbgFunction', rtThe_malloc_dbgFunction).
builtin_rn('The_aligned_mallocFunction', rtThe_aligned_mallocFunction).
builtin_rn('The_realloc_dbgFunction', rtThe_realloc_dbgFunction).
builtin_rn('HardwareComponentSpecificationForHardwareGroupType', ttHardwareComponentSpecificationForHardwareGroupType).
builtin_rn('TheHeapAllocFunction', rtTheHeapAllocFunction).
builtin_rn('TheHeapFreeFunction', rtTheHeapFreeFunction).
builtin_rn('TheGlobalAllocFunction', rtTheGlobalAllocFunction).
builtin_rn('TheLocalAllocFunction', rtTheLocalAllocFunction).
builtin_rn('TheLocalFreeFunction', rtTheLocalFreeFunction).
builtin_rn('TheGlobalFreeFunction', rtTheGlobalFreeFunction).
builtin_rn('TheFreeFunction', rtTheFreeFunction).
builtin_rn('TheNewFunction', rtTheNewFunction).
builtin_rn('TheDeleteFunction', rtTheDeleteFunction).
builtin_rn('TheAcceptFunction', rtTheAcceptFunction).
builtin_rn('TheBindFunction', rtTheBindFunction).
builtin_rn('TheClosesocketFunction', rtTheClosesocketFunction).
builtin_rn('TheCreateProcessAFunction', rtTheCreateProcessAFunction).
builtin_rn('TheCreateThreadFunction', rtTheCreateThreadFunction).
builtin_rn('TheEncodePointerFunction', rtTheEncodePointerFunction).
builtin_rn('TheDecodePointerFunction', rtTheDecodePointerFunction).
builtin_rn('TheExitFunction', rtTheExitFunction).
builtin_rn('TheFprintfFunction', rtTheFprintfFunction).
builtin_rn('TheGetCurrentProcessIdFunction', rtTheGetCurrentProcessIdFunction).
builtin_rn('TheHeapCreateFunction', rtTheHeapCreateFunction).
builtin_rn('TheGetsFunction', rtTheGetsFunction).
builtin_rn('TheFgetsFunction', rtTheFgetsFunction).
builtin_rn('TheGets_sFunction', rtTheGets_sFunction).
builtin_rn('TheGetws_sFunction', rtTheGetws_sFunction).
builtin_rn('TheGetwsFunction', rtTheGetwsFunction).
builtin_rn('TheFgetwsFunction', rtTheFgetwsFunction).
builtin_rn('TheBsearchFunction', rtTheBsearchFunction).
builtin_rn('TheBsearch_sFunction', rtTheBsearch_sFunction).
builtin_rn('TheGetenvFunction', rtTheGetenvFunction).
builtin_rn('TheWgetenvFunction', rtTheWgetenvFunction).
builtin_rn('CodeAnalysisTQ-AttemptingToReturnAMemoryResourceWithAnIncompatibleFunction', rtCodeAnalysisTQAttemptingToReturnAMemoryResourceWithAnIncompatibleFunction).
builtin_rn('The_wcsdecFunction', rtThe_wcsdecFunction).
builtin_rn('The_tcsdecFunction', rtThe_tcsdecFunction).
builtin_rn('The_strincFunction', rtThe_strincFunction).
builtin_rn('The_wcsincFunction', rtThe_wcsincFunction).
builtin_rn('The_mbsspnpFunction', rtThe_mbsspnpFunction).
builtin_rn('The_wcsspnpFunction', rtTh).
builtin_rn('The_strtimeFunction', rtThe_strtimeFunction).
builtin_rn('The_wstrtimeFunction', rtThe_wstrtimeFunction).
builtin_rn('The_ctime64Function', rtThe_ctime64Function).
builtin_rn('TheWctimeFunction', rtTheWctimeFunction).
builtin_rn('The_wctime64Function', rtThe_wctime64Function).
builtin_rn('The_gmtime32Function', rtThe_gmtime32Function).
builtin_rn('The_gmtime64Function', rtThe_gmtime64Function).
builtin_rn('TheLocaltime32Function', rtTheLocaltime32Function).
builtin_rn('The_localtime64Function', rtThe_localtime64Function).
builtin_rn('TheMysql_autocommitFunction', rtTheMysql_autocommitFunction).
builtin_rn('TheMysql_change_userFunction', rtTheMysql_change_userFunction).
builtin_rn('TheMysql_closeFunction', rtTheMysql_closeFunction).
builtin_rn('TheMysql_connectFunction', rtTheMysql_connectFunction).
builtin_rn('TheMysql_commitFunction', rtTheMysql_commitFunction).
builtin_rn('TheMysql_create_dbFunction', rtTheMysql_create_dbFunction).
builtin_rn('TheMysql_drop_dbFunction', rtTheMysql_drop_dbFunction).
builtin_rn('TheMysql_escape_stringFunction', rtTheMysql_escape_stringFunction).
builtin_rn('TheMysql_fetch_fieldFunction', rtTheMysql_fetch_fieldFunction).
builtin_rn('TheMysql_store_resultFunction', rtTheMysql_store_resultFunction).
builtin_rn('TheMysql_fetch_field_directFunction', rtTheMysql_fetch_field_directFunction).
builtin_rn('TheMysql_use_resultFunction', rtTheMysql_use_resultFunction).
builtin_rn('TheMysql_free_resultFunction', rtTheMysql_free_resultFunction).
builtin_rn('TheMysql_list_dbsFunction', rtTheMysql_list_dbsFunction).
builtin_rn('TheMysql_list_fieldsFunction', rtTheMysql_list_fieldsFunction).
builtin_rn('TheMysql_list_processesFunction', rtTheMysql_list_processesFunction).
builtin_rn('TheMysql_list_tablesFunction', rtTheMysql_list_tablesFunction).
builtin_rn('TheMysql_optionsFunction', rtTheMysql_optionsFunction).
builtin_rn('TheMysql_pingFunction', rtTheMysql_pingFunction).
builtin_rn('DeterminingTheValueOfASlot', rtDeterminingTheValueOfASlot).
builtin_rn('TheMysql_infoFunction', rtTheMysql_infoFunction).
builtin_rn('TheMysql_fetch_fieldsFunction', rtTheMysql_fetch_fieldsFunction).
builtin_rn('TheMysql_fetch_lengthsFunction', rtTheMysql_fetch_lengthsFunction).
builtin_rn('TheMysql_fetch_rowFunction', rtTheMysql_fetch_rowFunction).
builtin_rn('The_allocaFunction', rtThe_allocaFunction).
builtin_rn('MRSharedTradeAgreementStatusType', ttMRSharedTradeAgreementStatusType).
builtin_rn('BoardTrickTypeByBoardType', ttBoardTrickTypeByBoardType).
builtin_rn('ExpansionCardTypeByFunction', rtExpansionCardTypeByFunction).
builtin_rn('TheCurl_global_initFunction', rtTheCurl_global_initFunction).
builtin_rn('TheUriParseUriAFunction', rtTheUriParseUriAFunction).
builtin_rn('TheCurl_easy_initFunction', rtTheCurl_easy_initFunction).
builtin_rn('TheCurl_easy_performFunction', rtTheCurl_easy_performFunction).
builtin_rn('TheLog4c_initFunction', rtTheLog4c_initFunction).
builtin_rn('TheLog_4c_finiFunction', rtTheLog_4c_finiFunction).
builtin_rn('TheLog4c_category_getFunction', rtTheLog4c_category_getFunction).
builtin_rn('TheLog4c_category_logFunction', rtTheLog4c_category_logFunction).
builtin_rn('TheUriToStringCharsRequiredAFunction', rtTheUriToStringCharsRequiredAFunction).
builtin_rn('TheUriToStringAFunction', rtTheUriToStringAFunction).
builtin_rn('ThePointerDereferenceFunction', rtThePointerDereferenceFunction).
builtin_rn('GraphQuery-inheritRolePlayersOfType-SitTypeToSubSitType', ttGraphQueryInheritRolePlayersOfTypeSitTypeToSubSitType).
builtin_rn('WavePropagationTypeByEmitterOrEmitterType', ttWavePropagationTypeByEmitterOrEmitterType).
builtin_rn('VisualIBTTransitionType', ttVisualIBTTransitionType).
builtin_rn('ComputeFarmMachineType', ttComputeFarmMachineType).
builtin_rn('CombinationTherapeuticClassType', ttCombinationTherapeuticClassType).
builtin_rn('SpaceTypeHasObjTypeAtLocationType', ttSpaceTypeHasObjTypeAtLocationType).
builtin_rn('ConstantPolynomialFunction', rtConstantPolynomialFunction).
builtin_rn('ReversingReactionType', ttReversingReactionType).
builtin_rn('patientChronicallyUsesNarcoticWithReleaseProfileBetweenDates-Old', patientChronicallyUsesNarcoticWithReleaseProfileBetweenDatesOld).
builtin_rn('CatalyticInhibitionEventByType', ttCatalyticInhibitionEventByType).
builtin_rn('LevelOfWithRespectToRolePredicate', rtLevelOfWithRespectToRolePredicate).
builtin_rn('ChloroplastPigmentType', ttChloroplastPigmentType).
builtin_rn('GCATProductProvisioningEventType', ttGCATProductProvisioningEventType).
builtin_rn('CompositeActivityType', ttCompositeActivityType).
builtin_rn('SeptentaryFunction', rtSeptentaryFunction).
builtin_rn('FluidFnHydrocarbonMixtureTypeByCompositionType', ttFluidFnHydrocarbonMixtureTypeByCompositionType).
builtin_rn('AcquaintancePredicate', rtAcquaintancePredicate).
builtin_rn('NuancedAcquaintancePredicate', rtNuancedAcquaintancePredicate).
builtin_rn('VGeneratingFunction', rtVGeneratingFunction).
builtin_rn('TermParaphraseFn-PhysicalLocationType', ttTermParaphraseFnPhysicalLocationType).
builtin_rn('TermParaphraseFn-NP-QuaType', ttTermParaphraseFnNPQuaType).
builtin_rn('parsedFromString-9', parsedFromString_9).
builtin_rn('CyclishGlossary-Collection', ttCyclishGlossaryCollection).
builtin_rn('BELLAInteractiveAgentPerformableEventType', ttBELLAInteractiveAgentPerformableEventType).
builtin_rn('GAndARelatedAssumptionType', ttGAndARelatedAssumptionType).
builtin_rn('EqualityOperator', rtEqualityOperator).
builtin_rn('BusinessValuationProjectionPredicate', rtBusinessValuationProjectionPredicate).
builtin_rn('RevenueProjectionStrategyType', ttRevenueProjectionStrategyType).
builtin_rn('RewritingAMathematicalExpressionAsAnEquivalentExpression', ftRewritingAMathematicalExpressionAsAnEquivalentExpression).
builtin_rn('DistributingAnExpressionOutOfAExpression', ftDistributingAnExpressionOutOfAExpression).
builtin_rn('NegationExpression', ftNegationExpression).
builtin_rn('ReplacingSubexpressionWithEquivalentExpression', ftReplacingSubexpressionWithEquivalentExpression).
builtin_rn('BooleanEvidentialPredicate', rtBooleanEvidentialPredicate).
builtin_rn('wellTestWaterCut-ImplicitUnits', wellTestWaterCutImplicitUnits).
builtin_rn('FaultTreeAnalysisEventType', ttFaultTreeAnalysisEventType).
builtin_rn('COPY_OF_ComputingService-SubServiceType', ttCOPY_OF_ComputingServiceSubServiceType).
builtin_rn('BasicWellEventHypothesisType', ttBasicWellEventHypothesisType).
builtin_rn('SingleOperationMathTransformationType', ttSingleOperationMathTransformationType).
builtin_rn('EvaluatingAMathematicalExpression', ftEvaluatingAMathematicalExpression).
builtin_rn('expectedTargetValueOfWellParameterAfterRestartOfHydrocarbonWellWithDate-RGQ-3', expectedTargetValueOfWellParameterAfterRestartOfHydrocarbonWellWithDateRGQ_3).
builtin_rn('GaugeStatusType', ttGaugeStatusType).
builtin_rn('BridgeAlarmEvidentialPredicate', rtBridgeAlarmEvidentialPredicate).
builtin_rn('Device-SingleUser',tObjectDeviceSingleUser).
builtin_rn('LightingDevice',tObjectLightingDevice).
builtin_rn('Handgun',tObjectHandgun).
builtin_rn('SpaceInAHOC',tPlaceLikeSpaceInAHOC).
builtin_rn('Indoors-IsolatedFromOutside',tIndoorsIsolatedFromOutside).
builtin_rn('SomethingToWear',tObjectSomethingToWear).
builtin_rn('ProtectiveAttire',tObjectProtectiveAttire).
builtin_rn('PortableObject',tPortableObject).
builtin_rn('MaleAnimal',mobMaleAnimal).
builtin_rn('FemaleAnimal',mobFemaleAnimal).
builtin_rn('Topic',uitype_Topic).
builtin_rn('TimeParameter',timeOfTimeParameter).
builtin_rn('Situation',stateSituation).
builtin_rn('Place',tPlace).
builtin_rn('PersonTypeByActivity',mobPersonTypeByActivity).
builtin_rn('LinguisticObject',xtLinguisticObject).
builtin_rn('Group',tGroupedGroup).
builtin_rn('Goal',goalGoal).
builtin_rn('FormulaTemplate',uitype_FormulaTemplate).
builtin_rn('Capability',capCapability).
builtin_rn('Artifact',tObjectArtifact).
builtin_rn('AbsoluteValueFn',vAbsoluteValueFn).
builtin_rn('Average',vAverageFn).
builtin_rn('BookkeepingMt',iBookkeepingMt).
builtin_rn('Closed-InferenceProblemLinkStatus',tClosedInferenceProblemLinkStatus).
builtin_rn('CurrentWorldDataCollectorMt-NonHomocentric',iCurrentWorldDataCollectorMtNonHomocentric).
builtin_rn('CycArgumentDatastructure',tCycArgumentDatastructure).
builtin_rn('CycDeductionDatastructure',tCycDeductionDatastructure).
builtin_rn('CycHLSupportDatastructure',tCycHLSupportDatastructure).
builtin_rn('CycInference',tCycInference).
builtin_rn('CycInferenceAnswer',tCycInferenceAnswer).
builtin_rn('CycInferenceAnswerJustification',tCycInferenceAnswerJustification).
builtin_rn('CycInferenceBindingsDataStructure',tCycInferenceBindingsDataStructure).
builtin_rn('CycInferenceDataStructure',ftInferenceDataStructure).
builtin_rn('CycInferenceFn',iCycInferenceFn).
builtin_rn('CycKBDatastructure',ftKBDatastructure).
builtin_rn('CycLAssertedAssertion',ftAssertedAssertion).
builtin_rn('CycLAssertion',ftAssertion).
builtin_rn('CycLAtomicAssertion',ftAtomicAssertion).
builtin_rn('CycLAtomicSentence',ftAtomicSentence).
builtin_rn('CycLAtomicTerm',ftAtomicTerm).
builtin_rn('CycLClosedAtomicSentence',ftClosedAtomicSentence).
builtin_rn('CycLClosedDenotationalTerm',ftClosedDenotationalTerm).
builtin_rn('CycLClosedFormula',ftClosedFormula).
builtin_rn('CycLClosedNonAtomicTerm',ftClosedNonAtomicTerm).
builtin_rn('CycLClosedSentence',ftClosedSentence).
builtin_rn('CycLConstant',ftConstant).
builtin_rn('CycLDeducedAssertion',ftDeducedAssertion).
builtin_rn('CycLDenotationalTerm',ftDenotationalTerm).
builtin_rn('CycLDenotationalTerm-Assertible',ftDenotationalTermAssertible).
builtin_rn('CycLExpression-Askable',ftExpressionAskable).
builtin_rn('CycLExpression-Assertible',ftExpressionAssertible).
builtin_rn('CycLFormula',ftSentence).
builtin_rn('CycLGAFAssertion',ftGAFAssertion).
builtin_rn('CycLGenericRelationFormula',ftGenericRelationFormula).
builtin_rn('CycLIndexedTerm',ftIndexedTerm).
builtin_rn('CyclistDefinitionalMt',iAuthorDefinitionalMt).
builtin_rn('CycLNonAtomicReifiedTerm',ftNonAtomicReifiedTerm).
builtin_rn('CycLNonAtomicTerm',ftNonAtomicTerm).
builtin_rn('CycLNonAtomicTerm-Askable',ftNonAtomicTermAskable).
builtin_rn('CycLNonAtomicTerm-Assertible',ftNonAtomicTermAssertible).
builtin_rn('CycLOpenDenotationalTerm',ftDenotationalTerm).
builtin_rn('CycLOpenFormula',ftSentence).
builtin_rn('CycLOpenNonAtomicTerm',ftNonAtomicTerm).
builtin_rn('CycLOpenSentence',ftSentence).
builtin_rn('CycLPropositionalSentence',ftPropositionalSentence).
builtin_rn('CycLReifiableDenotationalTerm',ftReifiableDenotationalTerm).
builtin_rn('CycLReifiableNonAtomicTerm',ftReifiableNonAtomicTerm).
builtin_rn('CycLReifiedDenotationalTerm',ftReifiedDenotationalTerm).
builtin_rn('CycLRepresentedAtomicTerm',ftRepresentedAtomicTerm).
builtin_rn('CycLRepresentedTerm',ftRepresentedTerm).
builtin_rn('CycLRuleAssertion',ftRuleAssertion).
builtin_rn('CycLSentence',ftSentence).
builtin_rn('CycLSentence-Askable',ftSentenceAskable).
builtin_rn('CycLSentence-Assertible',ftSentenceAssertible).
builtin_rn('CycLTruthValueSentence',ftTruthValueSentence).
builtin_rn('CycProblem',tCycProblem).
builtin_rn('CycProblemLink',tCycProblemLink).
builtin_rn('CycProblemLink-AnswerLink',tCycProblemLinkAnswerLink).
builtin_rn('CycProblemLink-Conjunctive',tCycProblemLinkConjunctive).
builtin_rn('CycProblemLink-Content',tCycProblemLinkContent).
builtin_rn('CycProblemLink-Disjunctive',tCycProblemLinkDisjunctive).
builtin_rn('CycProblemLink-Join',tCycProblemLinkJoin).
builtin_rn('CycProblemLink-JoinOrdered',tCycProblemLinkJoinOrdered).
builtin_rn('CycProblemLink-Logical',tCycProblemLinkLogical).
builtin_rn('CycProblemLink-Removal',tCycProblemLinkRemoval).
builtin_rn('CycProblemLink-Restriction',tCycProblemLinkRestriction).
builtin_rn('CycProblemLink-Split',tCycProblemLinkSplit).
builtin_rn('CycProblemLink-Structural',tCycProblemLinkStructural).
builtin_rn('CycProblemLink-Transformation',tCycProblemLinkTransformation).
builtin_rn('CycProblemLink-Union',tCycProblemLinkUnion).
builtin_rn('CycProof',tCycProof).
builtin_rn('CycSupportDatastructure',tCycSupportDatastructure).
builtin_rn('CycTactic',tCycTactic).
builtin_rn('CycTransformationProof',tCycTransformationProof).
builtin_rn('DateDecodeStringFn',iTimeOf_DateDecodeStringFn).
builtin_rn('DateEncodeStringFn',xDateEncodeStringFn).
builtin_rn('DifferenceFn',vDifferenceFn).
builtin_rn('DirectedMultigraph',tDirectedMultigraph).
builtin_rn('DocumentationConstant',ftDocumentationConstant).
builtin_rn('ELRelation-Reversible',iELRelationReversible).
builtin_rn('EnglishParaphraseMt',iEnglishParaphraseMt).
builtin_rn('equalStrings-CaseInsensitive',equalStringsCaseinsensitive).
builtin_rn('EscapeQuote',xEscapeQuoteFn).
builtin_rn('EverythingPSC',iEverythingPSC).
builtin_rn('ExpFn',vExpFn).
builtin_rn('FixedAritySkolemFuncN',cycFixedAritySkolemFuncN).
builtin_rn('FOL-FunctionFn',iFOLFunctionFn).
builtin_rn('FOL-PredicateFn',iFOLPredicateFn).
builtin_rn('FormulaArgListFn',vFormulaArgListFn).
builtin_rn('FormulaArgSetFn',tSetOfFormulaArgSetFn).
builtin_rn('FormulaArityFn',vFormulaArityFn).
builtin_rn('CycLAssertionDirection',vtAssertionDirection).
builtin_rn('FunctionToArg',iFunctionToArgFn).
builtin_rn('genls-GenlDenotesSpecInstances',genlsGenldenotesspecinstances).
builtin_rn('genls-SpecDenotesGenlInstances',genlsSpecdenotesgenlinstances).
builtin_rn('HLAssertedArgumentKeywordDatastructure',tHLAssertedArgumentKeywordDatastructure).
builtin_rn('HLExternalIDString',xtHLExternalIDString).
builtin_rn('HLPrototypicalTerm',ftHLPrototypicalTerm).
builtin_rn('IndeterminateTerm',ftIndeterminateTerm).
builtin_rn('InferencePSC',iInferencePSC).
builtin_rn('IntervalMaxFn',vIntervalMaxFn).
builtin_rn('IntervalMinFn',vIntervalMinFn).
builtin_rn('Kappa',iKappaFn).
builtin_rn('LogFn',vLogFn).
builtin_rn('Maximum',vMaximumFn).
builtin_rn('MaxRangeFn',vMaxRangeFn).
builtin_rn('MeaningInSystemFn',iMeaningInSystemFn).
builtin_rn('Minimum',vMinimumFn).
builtin_rn('MinRangeFn',vMinRangeFn).
builtin_rn('ModuloFn',vModuloFn).
builtin_rn('MtSpace',mtMtSpaceFn).
builtin_rn('MtTimeDimFn',mtMtTimeDimFn).
builtin_rn('MtTimeWithGranularityDimFn',mtMtTimeWithGranularityDimFn).
builtin_rn('MtUnionFn',mtMtUnionFn).
builtin_rn('Multigraph',tMultigraph).
builtin_rn('CollectionRuleTemplateFn',iCollectionRuleTemplateFn).
builtin_rn('Open-InferenceProblemLinkStatus',tOpenInferenceProblemLinkStatus).
builtin_rn('Percent',vPercentFn).
builtin_rn('PerFn',cycPerFn).
builtin_rn('PlusAll',vPlusAllFn).
builtin_rn('PlusFn',vPlusFn).
builtin_rn('ProblemSolvingCntxt',mtProblemSolvingCntxt).
builtin_rn('QuantityConversionFn',vQuantityConversionFn).
builtin_rn('QuasiQuote',xQuasiQuoteFn).
builtin_rn('QueryMt',iQueryMt).
builtin_rn('Quote',xQuoteFn).
builtin_rn('QuotientFn',vQuotientFn).
builtin_rn('ReformulatorHighlyRelevantFORT',tReformulatorHighlyRelevantFORT).
builtin_rn('RoundClosestFn',vRoundClosestFn).
builtin_rn('RoundDownFn',vRoundDownFn).
builtin_rn('RoundUpFn',vRoundUpFn).
builtin_rn('ScalarIntegralValue',vtScalarIntegralValue).
builtin_rn('ScalarInterval',vtScalarInterval).
builtin_rn('ScalarPointValue',vtScalarPointValue).
builtin_rn('SkolemFuncN',cycSkolemFuncN).
builtin_rn('SkolemFuncNFn',fSkolemFuncNFn).
builtin_rn('SkolemFunctionFn',iSkolemFunctionFn).
builtin_rn('SubLAtom',ftAtom).
builtin_rn('SubLAtomicTerm',ftAtomicTerm).
builtin_rn('SubLCharacter',ftCharacter).
builtin_rn('SubLInteger',ftInt).
builtin_rn('SubLKeyword',ftKeyword).
builtin_rn('SubLList',ftList).
builtin_rn('SubLNonNegativeInteger',ftNonNegativeInteger).
builtin_rn('SubLNonVariableNonKeywordSymbol',ftNonVariableNonKeywordSymbol).
builtin_rn('SubLNonVariableSymbol',ftNonVariableSymbol).
builtin_rn('SubLPositiveInteger',ftPositiveInteger).
builtin_rn('SubLRealNumber',ftRealNumber).
builtin_rn('SubLString',ftString).
builtin_rn('SubLSymbol',ftSymbol).
builtin_rn('substring-CaseInsensitive',substringCaseinsensitive).
builtin_rn('CycLTerm',ftExpression).
builtin_rn('TemporaryEnglishParaphraseMt',xTemporaryEnglishParaphraseMt).
builtin_rn('TheCollectionOf',tColOfTheCollectionOfFn).
builtin_rn('TheEmptyList',vTheEmptyList).
builtin_rn('Set-Mathematical',tSetMathematical).
builtin_rn('TheEmptySet',iTheEmptySet).
builtin_rn('TheList',vTheListFn).
builtin_rn('TheSet',tSetOfTheSetFn).
builtin_rn('TheSetOf',tSetOfTheSetOfFn).
builtin_rn('TheTerm',ftTheTerm).
builtin_rn('Cyclist',mobCyclist).
builtin_rn('TheUser',iTheUser).
builtin_rn('TimesFn',vTimesFn).
builtin_rn('TLAssertionFn',xTLAssertionFn).
builtin_rn('TLReifiedNatFn',xTLReifiedNatFn).
builtin_rn('TLVariableFn',xTLVariableFn).
builtin_rn('PredicateTypeByArity',ttPredicateTypeByArity).
builtin_rn('UncanonicalizerAssertionFn',xUncanonicalizerAssertionFn).
builtin_rn('UnitProductFn',cycUnitProductFn).
builtin_rn('Unity',vUnityFn).
builtin_rn('UniversalVocabularyImplementationMt',iUniversalVocabularyImplementationMt).
builtin_rn('BroadMicrotheory',mtBroadMicrotheory).
builtin_rn('VariableAritySkolemFuncN',cycVariableAritySkolemFuncN).
builtin_rn('WFFSupportedTerm',tWFFSupportedTerm).
builtin_rn('CoreCycLMt',iCoreCycLMt).
builtin_rn('CoreCycLImplementationMt',iCoreCycLImplementationMt).
builtin_rn('BaseKB',iBaseKB).
builtin_rn('LogicalTruthImplementationMt',iLogicalTruthImplementationMt).
builtin_rn('ReformulatorIrrelevantFORT',tReformulatorIrrelevantFORT).
builtin_rn('LogicalTruthMt',iLogicalTruthMt).
builtin_rn('ELRelation-OneWay',iELRelationOneWay).
builtin_rn('UniversalVocabularyMt',iUniversalVocabularyMt).



%load_mpred_name_stream(_Name):- do_gc,repeat,read_one_term(Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.
%load_mpred_name_stream(_Name,Stream):- do_gc,repeat,read_one_term(Stream,Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.


tinykb_assertion_recipe(C,P):- tinykb_assertion_recipe_w(C,C1),unnumbervars(C1,P),!.
tinykb_assertion_recipe_w(C,P):- must((cycl_to_mpred(C,C0),fully_expand_always(C0,P))).


notFormatType(tThing).
notFormatType(tIndividual).
notFormatType(rtInferenceSupportedFunction).

% :- forall(notFormatType(NFT),ain(tSet(NFT))).

tinyKB1_if_loaded(G):- if_defined(tinyKB1(G),fail).

expT('SubLExpressionType').
expT('SubLExpression').
expT('CycLExpression').
expT('ttExpressionType').


isF(X):- atom_concat(_,'Fn',X).
isF(X):- tinyKB1_if_loaded(resultIsa(X,_)).
isF(X):- tinyKB1_if_loaded(resultQuotedIsa(X,_)).
isF(X):- tinyKB1_if_loaded(resultGenl(X,_)).
isF(X):- tinyKB1_if_loaded(isa(X,C)),atom_contains(C,'Function').

isFT(X):- atom_concatR(_,'Expression',X).
isFT(X):- tinyKB1_if_loaded(resultIsa(X,FT)),expT(FT),!.
isFT(X):- tinyKB1_if_loaded(resultGenl(X,FT)),expT(FT),!.
isFT(X):- tinyKB1_if_loaded(resultQuotedIsa(X,_)),!.
isFT(X):- expT(FT),tinyKB1_if_loaded(isa(X,FT)),!.
isFT(X):- expT(FT),tinyKB1_if_loaded(genls(X,FT)),!.

isV(X):- tinyKB1_if_loaded(isa(X,VT)),isVT(VT).
isVT(X):- tinyKB1_if_loaded(genls(X,'Individual')).
isVT(X):- atom_concat('UnitOf',_, X).



isRT(X):- atom_contains(X,'Functor').
isRT(X):- atom_concatR(L,'Fn',X),!,isRT0(L).
isRT(X):- isRT0(X).
isRT(X):- tinyKB1_if_loaded(genls(X,'tPred')).
isRT(X):- tinyKB1_if_loaded(genls(X,'tRelation')).
isRT(X):- tinyKB1_if_loaded(genls(X,'tFunction')).

isRT0(X):- atom_concatR(_,'Pred',X).
isRT0(X):- atom_concatR(_,'Predicate',X).
isRT0(X):- atom_concatR(_,'Operator',X).
isRT0(X):- atom_concatR(_,'Slot',X).
isRT0(X):- atom_concatR(_,'Dimension',X).
isRT0(X):- atom_concatR(_,'Function',X).
isRT0(X):- atom_concatR(_,'Relation',X).
isRT0(X):- atom_concatR(_,'Quantifier',X).



mpred_prepend_type(X,_):- \+ atom(X),!,fail.
mpred_prepend_type(X,PP):- baseKB:cycPrepending(PP,X),!.
%mpred_prepend_type(X,PrePend):- mpred_prepend_type_via(X,PrePend).
mpred_prepend_type(X,_):- starts_lower(X),!,fail.



% mpred_prepend_type(X,t):- tinyKB1_if_loaded(genls(X,'tMicrotheory')),!.
mpred_prepend_type(X,rt):- isRT(X),!.
mpred_prepend_type(X,rt):- isRT(X),!.
mpred_prepend_type(X,ft):- isFT(X), \+ isF(X).


mpred_prepend_type(X,tt):- atom_concatR(_,'Collection',X).
mpred_prepend_type(X,tt):- atom_concatR(_,'Type',X).


%mpred_prepend_type(X,v):- isV(X), \+ isF(X).
% ..                                    ""
%mpred_prepend_type(X,v):- isVT(X),!.
%mpred_prepend_type(X,tt):- tinyKB1_if_loaded(genls(X,'tCol')),!.
%mpred_prepend_type(X,tt):- tinyKB1_if_loaded(isa(X,'AtemporalNecessarilyEssentialCollectionType')),!.
%mpred_prepend_type(X,t):- tinyKB1_if_loaded(isa(X,'tCol')),!.
%mpred_prepend_type(X,t):- tinyKB1_if_loaded(isa(_,X)),!.
%mpred_prepend_type(X,v):- name(X,[C|_]),char_type(C,upper),!.

% sumo

mpred_prepend_type(X,time):- atom_contains(X,'Time').
mpred_prepend_type(X,time):- atom_contains(X,'Date').
mpred_prepend_type(X,xt):- atom_concatR(_,'Language',X).
mpred_prepend_type(X,xt):- atom_contains(X,'Prepos').
mpred_prepend_type(X,xt):- atom_contains(X,'Noun').
mpred_prepend_type(X,xt):- atom_contains(X,'Phrase').
mpred_prepend_type(X,tt):- atom_concatR(_,'Class',X).
mpred_prepend_type(X,rt):- atom_concatR(_,'Role',X).

mpred_prepend_type(X,ft):- atom_concatR(_,'Formula',X).
mpred_prepend_type(X,ft):- atom_concatR(_,'Number',X).
mpred_prepend_type(X,ft):- atom_concatR(_,'Integer',X).
mpred_prepend_type(X,ft):- atom_concatR(_,'List',X).

mpred_prepend_type(X,tt):- atom_concatR(_,'UnitOfMeasure',X).

mpred_prepend_type(X,vt):- atom_concat('UnitOf',_,X).
% mpred_prepend_type(X,vt):- isVT(X),!.



mpred_prepend_type(X,vt):- atom_concatR(_,'Quantity',X).
mpred_prepend_type(X,vt):- atom_concatR(_,'Measure',X).
mpred_prepend_type(X,vt):- atom_concatR(_,'Degree',X).
mpred_prepend_type(X,vt):- atom_concatR(_,'Attribute',X).

mpred_prepend_type(X,mt):- atom_concatR(_,'Context',X).
mpred_prepend_type(X,mob):- atom_concatR(_,'Agent',X).
mpred_prepend_type(X,event):- atom_concatR(_,'Event',X).

mpred_prepend_type(X,act):- atom_concatR(_,'Action',X).
mpred_prepend_type(X,act):- atom_concatR(_,'Process',X).
mpred_prepend_type(X,act):- atom_concatR(_,'ing',X).
mpred_prepend_type(X,act):- atom_concatR(_,'tion',X).
mpred_prepend_type(X,u):- atom_concatR(_,'Fn',X).



mpred_postpend_type(X,_):- starts_lower(X),!,fail.
mpred_postpend_type(C,'Fn'):-isF(C).

% mpred_prepend_type_via(C,Pre):-rename(P,C),dehyphenize_const(C,H),atom_concat(Pre,H,P).


prepend_constant(PT,C,_,P):- transitive_lc(cyc_to_mpred_idiom1,C,PM),dehyphenize_const(PM,PMH),!, atom_concat(PT,PMH,P).

:- was_export(cyc_to_mpred_create/2).
%cyc_to_mpred_create(different,dif).

starts_lower(X):-name(X,[S|_]),char_type(S,lower).
starts_upper(X):-name(X,[S|_]),char_type(S,upper).

never_idiom((:-)).
never_idiom((,)).
never_idiom(Atom):-atom_length(Atom,Len),Len<3.
never_idiom(A):- upcase_atom(A,U),downcase_atom(A,U).
never_idiom(Atom):- atom_chars(Atom,[S|Codes]),last(Codes,E),never_idiom(S,Codes,E).

never_idiom( S,_Codes,_E):- char_type(S,punct),!,S\=='?'.
never_idiom(_S,_Codes, E):- char_type(E,punct),!.
never_idiom(_S, Codes,_E):- member(M,Codes),char_type(M,period),!.




cyc_to_mpred_create(X,_):- \+ atom(X),!,fail.
cyc_to_mpred_create(equiv,(<=>)).
cyc_to_mpred_create(implies,(=>)). 
% cyc_to_mpred_create(KW,SYMBOL):-name(KW,[58,LETTER|REST]),char_type(LETTER,alpha),!,name(SYMBOL,[LETTER|REST]).
cyc_to_mpred_create(KW,KW):-name(KW,[58,LETTER|_]),char_type(LETTER,alpha),char_type(LETTER,upper).
cyc_to_mpred_create(KW,'$VAR'(VAR)):-name(KW,[63,LETTER|REST]),char_type(LETTER,alpha),!,name(SYMBOL,[LETTER|REST]),fix_var_name(SYMBOL,VAR),!.
cyc_to_mpred_create(X,X):- never_idiom(X),!.
cyc_to_mpred_create(X,Y):- starts_lower(X),!,dehyphenize_const(X,Y),!.
% cyc_to_mpred_create(not,(~)).
% cyc_to_mpred_create(X,Y):- starts_lower(X), rename(X,Y),!.
cyc_to_mpred_create(X,Y):- starts_lower(X),!,dehyphenize_const(X,Y).
cyc_to_mpred_create(C,PM):- 
  cyc_to_mpred_idiom_did(C,PM),
  C\==PM,
  azzert_rename(C,PM).
cyc_to_mpred_create(C,PM):- transitive_lc(cyc_to_mpred_idiom1,C,PM),!.
cyc_to_mpred_create(C,P):- atom_concat(it,C,P).


/*
% cyc_to_mpred_create(C,P):- atom(C), once(cyc_to_mpred_idiom1(C,I)), C\==I, loop_check(cyc_to_mpred_create(I,P)),!.
% cyc_to_mpred_create(C,P):- atom(C), transitive_lc(cyc_to_mpred_idiom1,C,I),cyc_to_mpred_create(I,P).
% BAD?  cyc_to_mpred_create(C,P):- rename(P,C).

% TODO USING? baseKB:mpred_to_cyc(P,C):- loop_check(cyc_to_mpred_create(C,P)),!.
%cyc_to_mpred_create(C,P):-baseKB:mpred_to_cyc(P,C),!.
*/

cyc_to_mpred_idiom_did(C,PM):- atom(C),  transitive_lc(cyc_to_mpred_idiom1,C,M),!,
 m_to_pm(M,C,PM),!.

m_to_pm(M,C,P):- mpred_prepend_type(C,PT), (atom_concat(PT,_,M)-> P=M; prepend_constant(PT,C,M,P)).
% m_to_pm(M,C,P):- mpred_postpend_type(C,PT), (atom_concat(_,PT,M)-> P=M; atom_concat(M,PT,P)).


cyc_to_mpred_idiom1('CycLTerm','CycLExpression').
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLSentence-',Type,C),!,atom_concat('Sentence',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Expression-',Type,C),!,atom_concat('Expression',Type,P).
% cyc_to_mpred_idiom1(C,P):-nonvar(C),baseKB:mpred_to_cyc(P,C),!.

cyc_to_mpred_idiom1(_C,_P):-!,fail. % its runtime .. correct?

% TODO remove these next two simplifcations one day
/*
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLOpen',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLClosed',P,C).
% cyc_to_mpred_idiom1(C,P):-atom_concatM('Open',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Closed',P,C).

cyc_to_mpred_idiom1(C,P):-atom_concatM('HL',P,C).

*/

cyc_to_mpred_idiom1(C,P):-atom_concatM('SubL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycSystem',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Lisp',P,C).

% cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyclist',Type,C),!,atom_concat('Author',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyc',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('FormulaicSenten',Type,C),!,atom_concat('Senten',Type,P).
%cyc_to_mpred_idiom1(C,P):-atom_concatM('SExpressi',Type,C),!,atom_concat('Expressi',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Assertible'),!,atom_concat(Type,'Assertible',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Askable'),!,atom_concat(Type,'Askable',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'FormulaicSentence'),!,atom_concat(Type,'Sentence',P).
cyc_to_mpred_idiom1(B,A):-starts_lower(B),dehyphenize_const(B,A).

cyc_to_mpred_idiom_unused([Conj|MORE],Out):-fail, not(is_ftVar(Conj)),!,cyc_to_mpred_sent_idiom_2(Conj,Pred,_),
  locally(thocal:outer_pred_expansion(Conj,MORE),
    ( must_maplist(cyc_to_pdkb,MORE,MOREL), 
       locally(thocal:outer_pred_expansion(Pred,MOREL),       
         list_to_ops(Pred,MOREL,Out)))),!.


atom_concatM(L,M,R):-atom(L),nonvar(R),atom_concat(L,M,R),atom_length(M,N),!,N > 2,starts_upper(M).
atom_concatR(L,M,R):-atom(R),nonvar(L),atom_concat(L,M,R),atom_length(M,N),!,N > 2.
atom_concatR(L,M,R):-atom(R),nonvar(M),atom_concat(L,M,R),atom_length(R,N),!,N > 2.


starts_hungarian(V,P):-atom_concat(V,Rest,P),name(Rest,[A|_]),char_type(A,upper).

make_kw_functor(F,A,CYCL):-make_kw_functor(F,A,CYCL,':ARG'),!.
make_kw_functor(F,A,CYCL,PREFIX):-make_functor_h(CYCL,F,A),CYCL=..[F|ARGS],label_args(PREFIX,1,ARGS).

label_args(_PREFIX,_,[]).
label_args(PREFIX,N,[ARG|ARGS]):-atom_concat(PREFIX,N,TOARG),ignore(TOARG=ARG),!,N2 is N+1,label_args(PREFIX,N2,ARGS).

:- thread_local thocal:outer_pred_expansion/2.

cyc_to_clif_notify(B,A):- cyc_to_pdkb(B,A) -> B\=@=A, nop(dmsg(B==A)).
%cyc_to_clif_entry(I,O):-fail,cyc_to_pdkb(I,M),!,must((compound_name_arity(I,FI,_),compound_name_arity(M,MF,_),FI==MF)),O=M.

re_convert_string(H,H).

really_convert_to_cycString(A,B):- logicmoo_util_strings:convert_to_cycString(A,B).

% a7166_4139461
convert_string(A,A):- \+ atom_contains(A,' '),!.
convert_string(A,B):- really_convert_to_cycString(A,B),!.


:-export(cyc_to_pdkb/2).
cyc_to_pdkb(V,V):-is_ftVar(V),!.
cyc_to_pdkb([],[]):-!.
cyc_to_pdkb([H],HH):- string(H),re_convert_string(H,HH),!.
cyc_to_pdkb(H,HH):- string(H),re_convert_string(H,HH),!.
cyc_to_pdkb(I,O):- \+ (compound(I)),do_renames(I,O),!.
cyc_to_pdkb('uSubLQuoteFn'(V),V):-atom(V),!.
% cyc_to_pdkb(isa(I,C),O):-atom(C),M=..[C,I],!,cyc_to_pdkb(M,O).
cyc_to_pdkb(I,O):- clause_b(ruleRewrite(I,M)),I\=@=M,!,cyc_to_pdkb(M,O).


cyc_to_pdkb([H|T],[HH|TT]):-!,cyc_to_pdkb(H,HH),cyc_to_pdkb(T,TT),!.
cyc_to_pdkb(I,O):-stack_check,do_renames(I,O),!.
cyc_to_pdkb(HOLDS,HOLDSOUT):-HOLDS=..[F|HOLDSL],
  locally(thocal:outer_pred_expansion(F,HOLDSL),must_maplist( cyc_to_pdkb,[F|HOLDSL],[C|HOLDSOUTL])),!,
  ((is_list([C|HOLDSOUTL]), atom(C))-> must(HOLDSOUT=..[C|HOLDSOUTL]) ; HOLDSOUT=[C|HOLDSOUTL]),!.

:-export(do_renames/2).

fix_var_name(A,A):-var(A),!.
fix_var_name('QUOTE','SUBLISP:QUOTE').
fix_var_name(A,B):- atom_concat(':',_,A),!, atomic_list_concat(AB,'-',A),atomic_list_concat(AB,'_',B).
fix_var_name(A,B):- atom_concat('?',QB,A),!,atom_concat('_',QB,B).
fix_var_name(A,B):- atomic_list_concat(AB,'-',A),atomic_list_concat(AB,'_',B).

% rename_atom(A,B):- atom_contains(A,'~'),!,convert_to_cycString(A,B),nb_setval('$has_quote',t),!.
rename_atom(A,B):- current_prolog_flag(do_renames,never),!,A=B.
rename_atom(A,B):- builtin_rn_or_rn(A,B),!.
rename_atom(A,B):- upcase_atom(A,B),A==B,!.

rename_atom(A,B):- is_file_atom(A),!,A=B.

%rename_atom(A,B):- current_prolog_flag(logicmoo_break_atoms,true),atom_contains(A,' '),!,convert_to_cycString(A,B),nb_setval('$has_quote',t),!.
rename_atom(A,B):-  must(cyc_to_mpred_create(A,B)),A\==B,azzert_rename(A,B),!.
rename_atom(A,B):- starts_upper(A),(\+ current_prolog_flag(do_renames_sumo,true)-> A=B ;(atom_concat('tSumo',A,B),azzert_rename(A,B))),!.
rename_atom(A,B):- downcase_atom(A,B),A==B,!.
rename_atom(A,A):- azzert_rename(A,A),!.


is_file_atom(A):- atom_contains(A,' ').
is_file_atom(A):- atom_concat(_,'-',A).
is_file_atom(A):- atom_concat('-',_,A).
is_file_atom(A):- atom_concat('_',_,A).
is_file_atom(A):- atom_concat('?',_,A).
is_file_atom(A):- atom_concat(':',_,A).

cyc_to_mpred_sent_idiom_2(and,(','),trueSentence).

list_to_ops(_,V,V):-is_ftVar(V),!.
list_to_ops(Pred,[],Out):-cyc_to_mpred_sent_idiom_2(_,Pred,Out),!.
list_to_ops(Pred,In,Out):-not(is_list(In)),!,cyc_to_pdkb(In,Mid),cyc_to_mpred_sent_idiom_2(_,Pred,ArityOne),Out=..[ArityOne,Mid].
list_to_ops(_,[In],Out):-!,cyc_to_pdkb(In,Out).
list_to_ops(Pred,[H,T],Body):-!,
    cyc_to_pdkb(H,HH),
    cyc_to_pdkb(T,TT),
    (is_list(TT)-> Body=..[Pred,HH|TT]; Body=..[Pred,HH,TT]).

list_to_ops(Pred,[H|T],Body):-!,
    list_to_ops(Pred,H,HH),
    list_to_ops(Pred,T,TT),
    (is_list(TT)-> Body=..[Pred,HH|TT]; Body=..[Pred,HH,TT]).


dont_touch_this(mfl4).
dont_touch_this(rnc).
dont_touch_this(rnc_new).

:-thread_initialization(nb_setval('$has_kw',[])).
:-thread_initialization(nb_setval('$has_var',[])).

do_renames(A,B):- current_prolog_flag(do_renames,never),!,A=B.
do_renames(A,B):- var(A),!,A=B,!,nb_setval('$has_var',t),!.
%do_renames('$VAR'(A),B):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
do_renames(A,B):- number(A),!,A=B.
do_renames(A,B):- atom(A),!,must(rename_atom(A,B)),!.
do_renames(A,B):- string(A),!,re_convert_string(A,B).
do_renames(A,B):- \+ compound(A),!,A=B.
do_renames(A,B):- compound_name_arity(A,P,0),!,do_renames(P,BP),compound_name_arguments(B,BP,0),!.
do_renames(A,B):- functor(A,F,_),dont_touch_this(F),!,A=B.
do_renames(uN(P,ARGS),B):- \+ is_list(ARGS) -> (uN(P,ARGS)= B) ; (do_renames([P|ARGS],List),cnas(B,uT,List)).
do_renames(uU('SubLQuoteFn',A),uSubLQuoteFn(A)):-var(A),!,nb_setval('$has_var',t),!.
do_renames(uU('SubLQuoteFn','$VAR'(A)),uSubLQuoteFn(A)):-!,nb_setval('$has_quote',t),!,nb_setval('$has_var',t),!.
do_renames('$KW'(A),'$VAR'(B)):- catch((fix_var_name(A,B),!,nb_setval('$has_kw',t)),E,(dtrace(dmsg(E)))),!.
do_renames('$VAR'(A),'$VAR'(B)):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
do_renames([A|String],[A|StringO]):- A == txt,!, (ground(String) -> convert_to_sel_string(fail,a,=,String,StringO) ; String=StringO).
do_renames(X,String):- X=..[s|SS],!,(ground(SS)->convert_to_s_string(SS,String);String=X),!. 
do_renames([A|Rest],[B|List]):- do_renames(A,B),!,do_renames(Rest,List).
do_renames(A,B):- 
  compound_name_arguments(A,P,ARGS),
   must_maplist(do_renames,[P|ARGS],[T|L]),
   do_ren_pass2(T,L,[BB|LL]),!,
   cnas(B,BB,LL).


compute_argIsa(ARG1ISA,NN,ARGISA):-
 % atom(ARG1ISA),
  atom_concat('arg',REST,ARG1ISA),
  member(E,['Genl','Isa','SometimesIsa','Format','QuotedIsa']),atom_concat(N,E,REST),
  atom_number(N,NN),
  atom_concat('arg',E,ARGISA),!.

do_ren_pass2('NART',[P|ARGS],[P|ARGS]):-atom(P).
do_ren_pass2(nartR,[P|ARGS],[P|ARGS]):-atom(P).
do_ren_pass2(nartR,ARGS,[nartR|ARGS]).
do_ren_pass2(uU,ARGS,[u|ARGS]).
do_ren_pass2(t,[P|IC],[t,P|IC]):- \+ atom(P).
% do_ren_pass2(t,[isa,I,C],[C,I]):-atom(C).
do_ren_pass2(t,[isa,I,C],[isa,I,C]).
do_ren_pass2(t,[ARG1ISA,P,C],[ARGISA,P,NN,C]):- compute_argIsa(ARG1ISA,NN,ARGISA).
do_ren_pass2(t,[P|IC],[P|IC]):- intrinsicPred(P).
do_ren_pass2(ARG1ISA,[P,C],[ARGISA,P,NN,C]):- atom(ARG1ISA), compute_argIsa(ARG1ISA,NN,ARGISA),!.
do_ren_pass2(P,IC,[P|IC]):- intrinsicPred(P).
%do_ren_pass2(t,[P|IC],[P|IC]).
do_ren_pass2(P,ARGS,[P|ARGS]).

is_sent_CONNECTIVE(CONNECTIVE):- intrinsicPred(CONNECTIVE).

intrinsicPred(ist).
intrinsicPred(and).
intrinsicPred(~).
intrinsicPred(not).
intrinsicPred(=).
intrinsicPred(or).
intrinsicPred(&).
intrinsicPred(v).
intrinsicPred(t).
intrinsicPred(=>).
intrinsicPred(<=>).
intrinsicPred(implies).
intrinsicPred(equiv).
intrinsicPred(forAll).
intrinsicPred('[|]').
intrinsicPred((;)).
intrinsicPred(termOfUnit).
intrinsicPred(genlMt).
intrinsicPred(genls).
intrinsicPred(argSometimesIsa).
intrinsicPred(argQuotedIsa).
intrinsicPred(argIsa).
intrinsicPred(argGenl).
intrinsicPred(argFormat).
intrinsicPred(format).
intrinsicPred(sformat).
% intrinsicPred(F):-exact_args_f(F).

intrinsicPred(A):-atom(A),atom_concat('thereE',_,A).


:- meta_predicate freeze_pvars(*,0).
freeze_pvars( _ ,Goal):-!,call(Goal). 
freeze_pvars([ ],Goal):-!,call(Goal).
freeze_pvars([V],Goal):-!,freeze(V,Goal).
freeze_pvars([V|Vs],Goal):-freeze(V,freeze_pvars(Vs,Goal)).

:- export(make_functor_h/3).
make_functor_h(CycL,F,A):- length(Args,A),CycL=..[F|Args].


saveRenames:-
   retractall(baseKB:rn_new(N,N)),
    absolute_file_name(pldata('plkb7166/kb7166_pt7_constant_renames_NEW.pl'),O),
         tell(O),
         listing(baseKB:rn_new/2),
         told.

makeRenames:- dmsg("no need to makeRenames!?"),!.
makeRenames:-  
     forall(makeRenames0,true),!.
makeRenames0:- makeCycRenames.

% makeRenames0:- exists_file('./rn2.pl'),must(ensure_loaded('./rn2.pl')),!.


makeCycRenames:- dmsg("no need to makeCycRenames!?"),!.
makeCycRenames:- forall(makeCycRenames_real,true).

makeCycRenames_real:- call_cleanup(makeCycRenames1, (nl,told)).

makeCycRenames1:-!.
makeCycRenames1:- 
  tell('e2c/renames.lisp'),
   writeln('

(define safely-rename-or-merge (Before After)
  (clet ((b (find-constant Before)) (a (find-constant After)))
     (pwhen b
       (punless a
         (ret (cyc-rename b After)))
   ;; purposely doesnt do anything
   (ret (quote (cyc-merge a b))))))
 '
           ), 
    forall(builtin_rn_or_rn_new(C,P),format('(safely-rename-or-merge "~w" "~w")~n',[C,P])),
    told.

add_rename(_KB,M:(:-Goal)):- !, M:call(Goal).
add_rename(KB,(:-Goal)):- !, KB:call(Goal).
add_rename(KB,MRNCP):- strip_module(MRNCP,_,RNCP),asserta(KB:RNCP).

load_renames(File):- \+ exists_source(File), !, dmsg(warning(missing_file(File))).
load_renames(File):- load_with_asserter(File,_,add_rename(baseKB),[]).
% load_renames(File):- catch(((if_file_exists(baseKB:qcompile(File)))),E,dmsg(E)),!.
% load_renames(File):- catch(quietly(nodebugx(if_file_exists(baseKB:ensure_loaded(FILE)))),E,dmsg(E)).
% load_renames(File):- load_with_asserter(pldata(sumo_renames),_AFile,assert_at_line_count(baseKB,Pos),[stream_postion(Pos)]).   

:- multifile(baseKB:rnc/2).
:- dynamic(baseKB:rnc/2).
:- multifile(baseKB:rn_new/2).
:- dynamic(baseKB:rn_new/2).

:- load_renames(pldata('plkb7166/kb7166_pt7_constant_renames.pldata')).
:- load_renames(pldata('plkb7166/kb7166_pt7_constant_renames_NEW.pldata')).
:- load_renames(pldata('sumo_renames.pldata')).

:- forall((baseKB:rnc(N,Y),(\+atom(N);\+atom(Y))),throw(retract(baseKB:rnc(N,Y)))).
:- forall((baseKB:rn_new(N,Y),(\+atom(N);\+atom(Y))),throw(retract(baseKB:rn_new(N,Y)))).

azzert_rename(C,P):- C=P,!.
azzert_rename(C,P):- builtin_rn(C,P),!.
azzert_rename(C,P):- baseKB:rnc(C,P),!.
azzert_rename(C,P):- baseKB:rn_new(C,P),!.
azzert_rename(C,P):- asserta(baseKB:rn_new(C,P)),dmsg_rename(C,P).

dmsg_rename(C,P):- C\=P,dmsg((azzert_rename(C,P))).
dmsg_rename(C,_):- downcase_atom(C,C),!.
dmsg_rename(C,_):- starts_lower(C),!.
dmsg_rename(C,P):- dmsg(warn(azzert_rename(C,P))).

re_symbolize(N,V):- catch(atom_concat(':',N,V),_,fail),!.
re_symbolize(N,V):- ignore(V='?'(N)).


%:- set_prolog_flag(runtime_safety,0).
%:- set_prolog_flag(runtime_debug,1).
%:- set_prolog_flag(runtime_speed,3).

:- nb_linkval('$ra5_often',1).


/*
:- export(do_vname/1).
?- module(test_abcd).
?- mpred_ain([aa(x), b(x,z), cc(t), ((aa(A),b(A,C),cc(C)) ==> dd(A,C)), aa(y), b(y,x), cc(y), aa(y), b(y,t), cc(z), b(z,y), cc(x)]),listing(dd/2).


do_vname(Wff):-
  do_vname(Wff,PO),
   b_getval('$variable_names',V2s),
   wt(current_output,PO,V2s),
   b_getval('$ra5_often',Often),   
   once(when_file_output((((nb_current('$has_var',t);nb_current('$has_quote',t);(flag('$ett',X,X+1),0 is X rem Often))-> 
   wt(user_output,PO,V2s) ; true)))),!.

:- export(do_vname/0).  
do_vname:- do_vname("forward('WordSenseDisambiguationMt',t('isLicensedBy',TERM2,TERMO ),'implies',t('mutuallyLicensing',TERM1,TERM2 ) ,a7166_3090092, monotonic_a_b_w).").
do_vname:- do_vname(forward('WordSenseDisambiguationMt',t('isLicensedBy',TERM2,TERM1 ),'implies',t('mutuallyLicensing',TERM1,TERM2 ) ,a7166_3090092, monotonic_a_b_w)).
do_vname:- do_vname(forward('MultiMediaAnalysisMt',XXX,'tooGeneralForEventExpansion','Homeotherm' ,a7166_5554792, default_d)).
do_vname:- do_vname(forward(iWordSenseDisambiguationMt,t(isLicensedBy,_80274,_80276),implies,t(mutuallyLicensing,_80276,_80274),a7166_3090092,monotonic_a_b_w)).




do_vname(S,PO):-string(S),
   rt(string(S),Wff,Vs),!,
   nb_linkval('$variable_names',Vs),
   do_vname(Wff,PO),!.

do_vname(Wff,PO):- b_getval('$variable_names',Vs),
   b_setval('$has_var',[]),b_setval('$has_quote',[]),
   
  on_x_debug((baseKB:do_renames(Wff,P)->true;throw(do_renames(Wff,P)))),!,

  (nb_current('$has_var',[])-> (PO = P,V2s=Vs) ; must( reread_vars(P-Vs,re_symbolize,PO-V2s))),
   nb_linkval('$variable_names',V2s),!,
 
  once((V2s==[]->true;(compound_name_arity(PO,_,A),arg(A,PO,ID),
    (must_maplist(arg(1),V2s,Names),
     wt(current_output,assertionVars(ID,Names),[]))))),!.
  
*/

:- fixup_exports.
:- gripe_time(7.0, makeRenames).

%:- gripe_time(60,load_files(pldata('kb_7166_assertions.pl'), [if(not_loaded),redefine_module(false),qcompile(auto)])).

gaf_rename(NCMPD,NEW):- \+ compound(NCMPD),!,do_renames(NCMPD,NEW).
gaf_rename([P|ARGS],NEW):- !,
   must_maplist(do_renames,[P|ARGS],[T|L]),!,
   do_ren_pass2(T,L,NEW),!.
gaf_rename(CMPD,NEW):- CMPD=..[P|ARGS],
   gaf_rename([P|ARGS],[M|MID]),!,
   cnas(NEW,M,MID).

%monotonic_fact(nartR('ResultStandsInRelationFn','genlMt'),'ContentModelForPegFn','DiscourseModellingFromLinkagesMt','UniversalVocabularyMt',a7166_1938598 , [ monotonic,forward,fact,not_first_order,asserted,relevant,higher_order,dependants,asserted_when = 20050602 ] ).7
%default_fact(nartR('ResultStandsInRelationFn','genlMt'),'ContentMtOfLinkageFn','LinkagesSpindleHeadMt','UniversalVocabularyMt',a7166_1938599 , [ default,forward,fact,not_first_order,asserted,relevant,higher_order,dependants,asserted_when = 20050602 ] ).
% monotonic_fact('isa',nartR('ResultStandsInRelationFn','disjointWith'),'MetaFunction','UniversalVocabularyMt',a7166_1938600 , [ monotonic,forward,fact,deduced,relevant,computed_skolem,dependants ] ).az

% and(t(nearestGenls,PRO,xtPronoun),t(speechPartPreds,xtPronoun,PRED)),t(speechPartPreds,PRO,PRED),a7166_445054)

/*
forward_default_rule(implies,
    and(
        isa(GEO,tGroupedGeopoliticalEntity),
        t(considersAsEnemy,GEO,ENEMY),
        isa(ENEMY,tGroupedGeopoliticalEntity),
        t(genls,WEAPON_TYPE,tObjectWeaponOfMassDestruction),
        t(increasesCausally,ACQUISITION,
           exists(WEAPON,and(t(possesses,GEO,WEAPON),isa(WEAPON, WEAPON_TYPE))),likelihood)),

        t(increasesCausally,ACQUISITION,
           t(setBehaviorCapable,GEO,u(tSetOfTheSetOfFn,ATTACK,
                   and(isa(ATTACK,actMilitaryAttack),
                       t(destructivePotentialOf,ATTACK,vHighAmountFn(vtDestructivePotential)),
                       t(maleficiary,ATTACK,ENEMY))),performedBy),likelihood),a7166_437341).


% monotonic_rule(implies,t(disjointWith,COL1,COL2),t(collectionOverlapFraction,COL1,COL2,0),a7166_303312).

% assertion_anteceedant(disjointWith,COL1,COL2,a7166_303312,{'COL1':COL1,'COL2':COL2}).
% assertion_consequent(collectionOverlapFraction,COL1,COL2,0,a7166_303312,{'COL1':COL1,'COL2':COL2}).

default_rule(not,
and(t(actionTypeExpressesFeelingType,ACTION_TYPE,FEELING_TYPE_1),
    t(actionTypeExpressesFeelingType,ACTION_TYPE,FEELING_TYPE_2),
    t(contrastedFeelings,FEELING_TYPE_1,FEELING_TYPE_2)),a7166_303675).


%forward_default_rule(implies,and(isa(QUEEN,iClassificationOf_QueenHeadOfState),t(nameString,QUEEN,NAME),t(evaluate,LONGNAME,u(xSubLStringConcatenationFn,["Queen",NAME]))),or(t(substring,"Queen",NAME),t(nameString,QUEEN,LONGNAME)),a7166_437982).

*/


kb_transposer_pt2(A,B,C):-kb_transposer(A,B,C),!.

kb_transposer(I-Vs,Info,OUT):- !,must(kb_transposer(I,Vs,Info,OUT)),!.
kb_transposer(I,Info,OUT):- must(kb_transposer(I,[],Info,OUT)),!.

kb_transposer( '$translate_file_steam',_,_Info,[]).
kb_transposer( end_of_file,_,_Info,end_of_file).
kb_transposer( :-(G),Vs,_Info,[ ( :-(G)) - Vs]).

kb_transposer(I,Vs,_Info,[A1-VSO,A2,A3|A4S]):- compound(I),
 functor(I,IF,IA),
 arg(_,v(default_fact,monotonic_fact,
   code_default_fact,code_monotonic_fact,
   backward_default_fact,backward_monotonic_fact,
   default_rule,monotonic_rule,
   code_default_rule,code_monotonic_rule,
   forward_default_rule,forward_monotonic_rule),IF),
 arg(IA,I,Props),
 is_list(Props),!,

 must_det_l(( 
 I=..[P|List],
 nb_setval('$has_kw',[]),
 append(ARGS,[MTI,ID,Props],List), 
 do_renames(MTI,MT),!,
 must_maplist(do_renames,ARGS,[T|L]),
 do_ren_pass2(T,L,MO),
 append(MO,[ID],NEWARGS),
 cnas(A0, assertion_content,NEWARGS),
 (nb_current('$has_kw',t)-> reread_vars(A0-Vs,re_symbolize,A1-VSO); (A0-Vs=A1-VSO)),
 functor(A1,_,A),
 A2 = assertion_mt(ID,MT),
 A3 = assertion_wrapper(ID,P,A),
 must_maplist(make_assert_prop(ID),Props,A4S))).

kb_transposer(A,B,C,D):- compound(A), kb_transposer_pt2(A,B,C,D),!.

kb_transposer( G,Vs,_Info,[ G - Vs]).

kb_transposer_pt2(assertion_documentation(_,_),_,_,[]).
kb_transposer_pt2(I,VS,_Info,[I-VS]):- functor(I,F,A),atom_concat(assertion_,_,F),ensure_dynamic_decl(F,A),!.
kb_transposer_pt2(I,[],_Info,[O]):- functor(I,F,A),!, ensure_dynamic_decl(assertion_content,A),I=..[F|List],O=..[assertion_content|List].
kb_transposer_pt2(I,VS,_Info,[O-VS,assertion_vars(ID,VS)]):- functor(I,F,A),ensure_dynamic_decl(assertion_content,A),
   I=..[F|List],O=..[assertion_content|List],last(List,ID).



:- dynamic(is_known_pred/2).
ensure_dynamic_decl(F,A):-is_known_pred(F,A),!.
ensure_dynamic_decl(F,A):- asserta(is_known_pred(F,A)),wdmsg(is_known_pred(F/A)).
   
make_assert_prop(ID,TYPE,A4S):-atom(TYPE),!,atom_concat(assertion_,TYPE,ATYPE),A4S=..[ATYPE,ID].
make_assert_prop(ID,TYPE=VALUE,A4S):-atom(TYPE),atom_concat(assertion_,TYPE,ATYPE),gaf_rename(VALUE,VALUEO),A4S=..[ATYPE,ID,VALUEO].
make_assert_prop(ID,TYPE:VALUE,A4S):-atom(TYPE),atom_concat(assertion_,TYPE,ATYPE),gaf_rename(VALUE,VALUEO),A4S=..[ATYPE,ID,VALUEO].

%kb_7166_ensure_translated_1:- ensure_translated_with(pldata('kb7166_assertions.pl'),kb_transposer,_).
%kb_7166_ensure_translated_2:- ensure_translated_with(pldata('kb_7166_assertions-trans.pl'),kb_transposer_pt2,_).
kb_7166_assertions:- ensure_loaded_with(pldata('kb_7166_assertions.pl'),kb_transposer).
% :- (baseKB:load_files([pldata('kb_7166_assertions-trans.pl')],[if(not_loaded),redefine_module(false),qcompile(auto)])).
% :- kb_7166_ensure_translated_2.


:- multifile(system:term_expansion/2).

system:term_expansion(I,Pos,O,Pos2):- compound(I), 
   current_prolog_flag(do_renames,term_expansion),
   (b_getval('$term', Term),(Term==I;Term==[])),
   (nb_current('$term_exp_skip', Was)->(Was\==I;Term==[]);true),
   must(do_renames(I,O)) -> 
   I\==O -> 
   b_setval('$term', O),
   b_setval('$term_exp_skip', Term),
   nop(dmsg(do_renames(I)-->O)),
   Pos=Pos2.
/*
system_clause_expansion(I, O):- compound(I), 
   current_prolog_flag(do_renames,term_expansion),
   % b_getval('$term', Term),Term==I, 
   must(do_renames(I,O))->I\==O -> 
   % b_setval('$term', O),
     nop(dmsg(do_renames(I)-->O)).
*/

:- fixup_exports.


% inform_new(X,Y):- builtin_rn_or_rn_new(X,New),(New==Y-> true; ( writeq(builtin_rn(X,New)),writeln('.'))).


