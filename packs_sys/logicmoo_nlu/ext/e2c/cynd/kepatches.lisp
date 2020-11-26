;; use (load "cynd/osim.lisp")

(print "loading kepatches.lisp..")
(force-output)

;;==================================================
;; CYC PATCHES      1.8045
;;==================================================
; Required for the planner example to work
(cyc-assert '(#$energySourceTypeForDeviceType #$RoadVehicle-DieselEngine #$DieselFuel) #$HumanActivitiesMt)


(force-output)

(find-or-create-constant "MicrotheoryFunction")
(cyc-assert '(#$isa #$MicrotheoryFunction  #$Collection) #$UniversalVocabularyMt)
(cyc-assert '(#$genls #$MicrotheoryFunction #$IndividualDenotingFunction) #$UniversalVocabularyMt)

#+TOO-SLOW
(cyc-assert '(#$equiv (#$isa ?FUNCT #$MicrotheoryFunction)(#$and (#$resultIsa ?FUNCT #$Microtheory))) #$UniversalVocabularyMt  '(:DIRECTION :FORWARD))

;;(cyc-assert '(#$isa #$CPOF-ChangeOverTimeQueryMtFn  #$MicrotheoryFunction ) #$UniversalVocabularyMt)

(cyc-assert '(#$arity #$dontAddTheToNamesOfInstances 1) #$UniversalVocabularyMt)

(cyc-assert '(#$isa #$TheEarthsAtmosphere #$FluidTangibleThing) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$TheEarthsAtmosphere #$PartiallyTangible) #$UniversalVocabularyMt)

;;(define SBHL-DATE (form) T)
(find-or-create-constant "resultNamedByArg")
(cyc-assert '(#$arity #$resultNamedByArg 3) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$resultNamedByArg #$Relation) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$resultNamedByArg #$TernaryPredicate) #$UniversalVocabularyMt)
(cyc-assert '(#$arity #$dontAddTheToNamesOfInstances 1) #$UniversalVocabularyMt)
(cyc-assert '(#$arity #$dontAddTheToNamesWithPred 1) #$UniversalVocabularyMt)

(cyc-assert '(#$isa #$transportsObjectType (#$ArityRelationFn 5) ) #$UniversalVocabularyMt)
(cyc-assert '(#$arity #$transportsObjectType 5 ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$transportsObjectType (#$ArityRelationFn 5) ) #$UniversalVocabularyMt)
(cyc-assert '(#$arity #$transportsObjectType 5 ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$different #$VariableArityRelation ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$differentSymbols #$VariableArityRelation ) #$UniversalVocabularyMt)

(cyc-assert '(#$isa #$marksBoundariesOfRegion (#$ArityRelationFn 3) ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doCloseLocalVariablesScope #$VariableArityRelation ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doOpenLocalVariablesScope #$VariableArityRelation ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doCloseLocalVariablesScope #$Predicate ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doOpenLocalVariablesScope #$Predicate ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doOutputFunctionArgs #$VariableArityRelation ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doOutputFunctionArgs #$Predicate ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doProgramStepSequence  #$VariableArityRelation ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doProgramStepSequence  #$Predicate ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doDeclareFunctionParameters   #$VariableArityRelation ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doDeclareFunctionParameters   #$Predicate ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doOutputProgramFunctionHeader   #$TernaryPredicate ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$doOutputProgramFunctionHeader   #$Predicate ) #$UniversalVocabularyMt)
(cyc-assert '(#$arity #$GameForSystemFn 1)  #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$actorRoles-EPFT #$VariableArityRelation)  #$UniversalVocabularyMt)

(cyc-assert '(#$implies (#$isa ?X #$Country) (#$ist #$UniversalVocabularyMt (#$isa ?X #$GeopoliticalEntity))) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
;(cyc-assert '(#$isa #$MySentientKEIrrelevanceForQuestionAnsweringMt   #$Microtheory ) #$UniversalVocabularyMt)
;(cyc-assert '(#$isa #$TKBTopicEntitiesMt   #$Microtheory ) #$UniversalVocabularyMt)
;(cyc-assert '(#$isa #$INSCOMSandboxMt   #$Microtheory ) #$UniversalVocabularyMt)
 
(cyc-assert '(#$genlPreds #$oldConstantName #$termStrings) #$BookkeepingMt '(:DIRECTION :FORWARD))

;;==================================================
;; PATCH PREDICATE TYPE RULES
;;==================================================
(cyc-assert '(#$implies  (#$isa ?PRED #$QuantitySlot) (#$and (#$arity ?PRED 2)(#$intervalEntryFormatInArgs ?PRED 2))) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert (list '#$isa (find-or-create-constant "moralCharacter") '#$QuantitySlot)  #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$isa #$evaluator  #$ActorSlot )  #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$isa #$toughnessOfObject #$QuantitySlot)  #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$isa #$earningsPerShare-ForStockType #$QuantitySlot)  #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$isa #$basicPriceOfType #$MeasurableQuantitySlot)  #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$isa #$equivalentQuantities #$EquivalenceRelation )  #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$isa #$TKB-EDBMt   #$Microtheory ) #$UniversalVocabularyMt)
;;(cyc-assert '(#$isa #$INSCOMSandboxMt   #$Microtheory ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$FormulaStrengtheningHeuristicsGMt   #$Microtheory ) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$SAICLegacyAssertionsMt  #$Microtheory) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$SOEGlfMt  #$Microtheory) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$MotleyFoolCorpusSOEMt  #$Microtheory) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$DialogueTrackingGMt  #$Microtheory) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$ReasoningAboutCoreferentExpressionsMt  #$Microtheory) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$DialogueTrackingGMt  #$Microtheory) #$UniversalVocabularyMt)
#|rcyc-old
;;(cyc-assert '(#$isa #$LocalizedShapeTypeFn  #$UnaryIntersectionFunction) #$UniversalVocabularyMt)
;;(cyc-assert '(#$isa #$LocalizedShapeTypeFn  #$CollectionDenotingFunction) #$UniversalVocabularyMt)
;;(cyc-assert '(#$resultGenl #$LocalizedShapeTypeFn  #$GeometricThing-Localized) #$UniversalVocabularyMt)
;;(cyc-assert '(#$resultIsa #$LocalizedShapeTypeFn  #$LocalizedShapeType ) #$UniversalVocabularyMt)
;;(cyc-assert '(#$isa #$InfluencingAnAgentToPlayRoleFn  #$BinaryFunction ) #$UniversalVocabularyMt)
;;(cyc-assert '(#$isa #$actionSequence #$UnaryPredicate) #$UniversalVocabularyMt)
;;(cyc-assert '(#$genlPreds #$actionSequence #$eventSequence) #$UniversalVocabularyMt)

;;==================================================
;; PATCH ARITY RULES
;;==================================================
;;(cyc-assert '(#$implies  (#$and (#$nearestGenlPreds ?PRED ?SUPER) (#$arity ?SUPER ?ARITY)) (#$arity ?PRED ?ARITY)) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))  
(cyc-assert '(#$implies  (#$and (#$nearestGenlPreds ?PRED ?SUPER) (#$arity ?SUPER ?ARITY)) (#$arity ?PRED ?ARITY)) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))  
(cyc-assert '(#$implies (#$and (#$arityMin ?P ?X)(#$isa ?P #$Predicate)) (#$isa ?P #$VariableArityRelation)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$arityMax ?P ?X)(#$isa ?P #$Predicate)) (#$isa ?P #$VariableArityRelation)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$argAndRestIsa ?P ?X ?Y)) (#$isa ?P #$VariableArityRelation)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$isa ?T #$BinaryPredicateTypeByLogicalFeature)(#$isa ?P ?T)) (#$isa ?P #$BinaryPredicate)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$arity ?P 2) (#$isa ?P #$BinaryRelation) ) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$isa ?F #$UnaryFunction)(#$arity ?F 1)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$isa ?F #$BinaryFunction)(#$arity ?F 2)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$isa ?F #$UnaryRelation)(#$arity ?F 1)) #$CoreCycLMt '(:DIRECTION :FORWARD))
;(cyc-assert '(#$implies (#$and (#$argAndRestIsa ?P ?X ?Y)) (#$isa ?P #$VariableArityRelation)) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$arityMin ?P ?X) (#$isa ?P #$VariableArityRelation)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$arityMin ?P ?X) (#$isa ?P #$Function-Denotational)) (#$isa ?P #$VariableArityFunction)) #$CoreCycLMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$arityMax ?P ?X)(#$isa ?P #$Relation)) (#$isa ?P #$VariableArityRelation)) #$CoreCycLMt '(:DIRECTION :FORWARD))
;(cyc-assert '(#$implies  (#$and (#$isa ?PRED #$Predicate)(#$unknownSentence (#$isa ?PRED #$FixedArityRelation)))(#$isa ?PRED #$VariableArityRelation)) #$CoreCycLMt '(:DIRECTION :FORWARD))
;(cyc-assert '(#$implies (#$isa ?PRED #$Predicate) (#$isa ?PRED #$VariableArityRelation)) #$CoreCycLMt '(:DIRECTION :FORWARD))
|#
(define KWTE? (&rest rest) (ret t))
;(cyc-assert '(#$defnSufficient #$NLTemplateExpression (#$SubLQuoteFn KWTE?)) #$BaseKB '(:DIRECTION :FORWARD)) 
(cyc-assert '(#$defnSufficient #$NLTemplateExpression (#$SubLQuoteFn KWTE?)) #$ComputereseLexicalMt '(:DIRECTION :FORWARD)) 
(cyc-assert '(#$defnIff #$NLTemplateExpression (#$SubLQuoteFn KWTE?)) #$ComputereseLexicalMt '(:DIRECTION :FORWARD)) 
(cyc-assert '(#$siblingDisjointExceptions #$TemporalThing #$PropositionalInformationThing)  #$UniversalVocabularyMt '(:DIRECTION :FORWARD)) 
(cyc-assert '(#$siblingDisjointExceptions #$TemporalThing #$Microtheory)  #$GeneralEnglishMt '(:DIRECTION :FORWARD)) 

