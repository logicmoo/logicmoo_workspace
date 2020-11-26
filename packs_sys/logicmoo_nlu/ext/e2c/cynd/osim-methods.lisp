;;;; %sim:possibleCycTerms
;;(sim-assert-now `(#$comment ,(sim-map "Action" "Name") "Maps (simActionToName ?PRED ?TEXT) like (#$synonymousExternalConcept ?PRED #$SimVocabularyMt  ?ParamValueObject)") *VocabularyMt*)
                          
;;==================================================
;; INTERFACE MAPPING SETUP (dooMtouches)
;;==================================================
#|
(defsim-pred "touches" *CurrentStateMt* 2)
(sim-assert-now `(#$isa ,(sim-pred "touches")  #$SymmetricBinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "touches")  #$CotemporalPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "touches")  #$TimeDependentRelation) *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "touches")  2) *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "touches")  " (dooMtouches ?OBJ1 ?OBJ2) is true if the two objects are near enough to interact physically.") *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "touches")  #$touches)  *VocabularyMt*)
;;;;(sim-assert-now `(#$isa ,(sim-pred "touches")  #$SpatialPredicate) *VocabularyMt*)
;;;;(sim-assert-now `(#$arg1Isa ,(sim-pred "touches")  #$SpatialThing-Localized) *VocabularyMt*)
;;;;(sim-assert-now `(#$arg2Isa ,(sim-pred "touches")  #$SpatialThing-Localized) *VocabularyMt*)
;;==================================================
;; INTERFACE MAPPING SETUP (simNear)
;;==================================================
(sim-assert-now `(#$isa ,(sim-pred "near")  #$BinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "near")  2) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "near")  #$CotemporalPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "near")  #$TimeDependentRelation) *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "near")  #$near)  *VocabularyMt*)
(defsim-pred "near" *CurrentStateMt* 2)
(sim-assert-now `(#$isa ,(sim-pred "near")  #$SpatialPredicate) *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "near")  " (simNear ?OBJ1 ?OBJ2) is true if the two objects are near enough to interact physically.") *VocabularyMt*)
;;;;(sim-assert-now `(#$arg1Isa ,(sim-pred "near")  #$SpatialThing-Localized) *VocabularyMt*)
;;;;(sim-assert-now `(#$arg2Isa ,(sim-pred "near")  #$SpatialThing-Localized) *VocabularyMt*)
#|
;;==================================================
;; INTERFACE MAPPING SETUP (simClasses)
;;==================================================
(defsim-pred "classes" *StaticStateMt* 2)      
(sim-assert-now `(#$isa ,(sim-pred "classes")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "classes")  #$TaxonomicSlotForAnyThing) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "classes")  #$DefaultMonotonicPredicate) *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "classes")  " (simClasses ?OBJ1 ?OBJ2) is true for the OBJ2 is Instance to OBJ1.") *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "classes")  2) *VocabularyMt*)
(sim-assert-now `(#$arg1Isa ,(sim-pred "classes")  #$BPVItem) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "classes")  #$Thing) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "classes")  #$FirstOrderCollectionPredicate) *VocabularyMt*)
(sim-assert-now '(#$completeExtentDecidable #$simClasses) *CurrentStateMt* '(:DIRECTION :FORWARD))
|#
;;==================================================
;; INTERFACE MAPPING SETUP (simInRegion)
;;==================================================
(defsim-pred "inRegion" *CurrentStateMt* 2)
(sim-assert-now `(#$isa ,(sim-pred "inRegion")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "inRegion")  #$CoexistingObjectsPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "inRegion")  #$SpatialPredicate) *VocabularyMt*)
(sim-assert-now `(#$arg1Isa ,(sim-pred "inRegion")  #$SpatialThing-Localized) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "inRegion")  #$Place-NonAgent) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "inRegion")  #$GeographicalSpaceRegion) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "inRegion")  #$Point) *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "inRegion")  2) *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "inRegion")  " (simInRegion ?OBJ1 ?LOC) is true for the nearest LOC entity to OBJ1.") *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "inRegion")  #$localityOfObject)  *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "inRegion")  #$objectFoundInLocation)  *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "inRegion")  ,(sim-pred "worldOrigin")) *VocabularyMt*)

;;==================================================
;; INTERFACE MAPPING SETUP (simCanSee)
;;==================================================
(defsim-pred "canSee" *CurrentStateMt* 2)
(sim-assert-now `(#$isa ,(sim-pred "canSee")  #$BinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "canSee")  #$PerceivingSlot) *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "canSee")  #$sees)  *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "canSee")  " (simCanSee ?OBJ1 ?OBJ2) is true for the OBJ2 visable to OBJ1.") *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "canSee")  2) *VocabularyMt*) 
(sim-assert-now `(#$arg1Isa ,(sim-pred "canSee")  #$PerceptualAgent) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "canSee")  #$SpatialThing-Localized) *VocabularyMt*)
                                               
;;==================================================
;; INTERFACE MAPPING SETUP (simWorldOrigin)
;;==================================================
(defsim-pred "worldOrigin" *CurrentStateMt* 2)            
(sim-assert-now `(#$isa ,(sim-pred "worldOrigin")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "worldOrigin")  #$CotemporalPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "worldOrigin")  #$TimeDependentRelation) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "worldOrigin")  #$SpatialPredicate) *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "worldOrigin")  #$locatedAtPoint-Spatial)  *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "worldOrigin")  " (simSpacePoint ?OBJ1 ?POINT) is true for the POINT is the location of OBJ1.") *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "worldOrigin")  2) *VocabularyMt*)
(sim-assert-now `(#$arg1Isa ,(sim-pred "worldOrigin")  #$SpatialThing-Localized) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "worldOrigin")  #$Point) *VocabularyMt*)
                                    
;;==================================================
;; INTERFACE MAPPING SETUP (simDirectionFacing)
;;==================================================
(defsim-pred "angles" *CurrentStateMt* 2)            
(sim-assert-now `(#$isa ,(sim-pred "angles")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "angles")  #$TimeDependentRelation) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "angles")  #$SpatialPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "angles")  #$CotemporalPredicate) *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "angles")  #$orientation)  *VocabularyMt*)	   ;;direction-Pointing
;;;;(sim-assert-now `(#$genlPreds ,(sim-pred "angles")  #$direction-Pointing)  *VocabularyMt*)	   ;;
(sim-assert-now `(#$comment ,(sim-pred "angles")  " (simDirectionFacing ?OBJ1 ?ANGLE2D) is true for the POINT is the location of OBJ1.") *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "angles")  2) *VocabularyMt*)
(sim-assert-now `(#$arg1Isa ,(sim-pred "angles")  #$SpatialThing-Localized) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "angles")  #$UnitVectorInterval) *VocabularyMt*)
                                    
;;==================================================
;; INTERFACE MAPPING SETUP (simAttached)
;;==================================================
(defsim-pred "attached" *CurrentStateMt* 3)            
(sim-assert-now `(#$isa ,(sim-pred "attached")  #$BinaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "attached")  #$ConnectionPredicate) *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "attached")  #$transports)  *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "attached")  #$controls)  *VocabularyMt*)
(sim-assert-now `(#$genlPreds ,(sim-pred "attached")  #$connectedTo)  *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "attached")  " (simAttached ?OBJ1 ?OBJ2) is true for the OBJ2 is attached to OBJ1.") *VocabularyMt*)
(sim-assert-now `(#$arity ,(sim-pred "attached")  2) *VocabularyMt*)
(sim-assert-now `(#$arg1Isa ,(sim-pred "attached")  #$SpatialThing-Localized) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "attached")  #$SpatialThing-Localized) *VocabularyMt*)
;;==================================================
;; INTERFACE MAPPING SETUP (simPlan)
;;==================================================
(defsim-pred "plan" *CurrentStateMt* 3)            
(sim-assert-now `(#$isa ,(sim-pred "plan")  #$TernaryPredicate) *VocabularyMt*)
(sim-assert-now `(#$backchainEncouraged ,(sim-pred "plan") ) *VocabularyMt*)
(sim-assert-now `(#$backchainRequired ,(sim-pred "plan") )  *CurrentStateMt* )
(sim-assert-now `(#$conceptuallyRelated ,(sim-pred "plan")  ,(sim-pred "eval") )   *VocabularyMt*)
(sim-assert-now `(#$arg1Isa ,(sim-pred "plan")  #$PlanningDomainMicrotheory) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "plan")  #$Thing) *VocabularyMt*)
(sim-assert-now `(#$arg3Isa ,(sim-pred "plan")  #$Thing) *VocabularyMt*)
;;;;(sim-assert-now `(#$implies (,(sim-pred "eval")  (#$TheList "evalPlanner" ?Mt ?Goal) ?Plan) (,(sim-pred "plan")  ?Mt ?Goal ?Plan))  *CurrentStateMt* )

;;==================================================
;; INTERFACE MAPPING SETUP (simAction)
;;==================================================
(defsim-pred "tasks" *CurrentStateMt* 3)            
(sim-assert-now `(#$isa ,(sim-pred "tasks")  #$Predicate) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "tasks")  #$VariableArityRelation) *VocabularyMt*)
(sim-assert-now `(#$isa ,(sim-pred "tasks")  #$ActionPredicate) *VocabularyMt*)
(sim-assert-now `(#$comment ,(sim-pred "tasks")  "(simAction ?ACTION ?AGENT . EVENTARGS) relates ACTION to the list EVENTARGS of the relevant instances of #$RoleSlots that are inputs to the activity.") *VocabularyMt*)
(sim-assert-now `(#$conceptuallyRelated ,(sim-pred "tasks")  ,(sim-pred "eval") )   *VocabularyMt*)
(sim-assert-now `(#$arg1Genl ,(sim-pred "tasks")  #$Event) *VocabularyMt*)
(sim-assert-now `(#$arg1Genl ,(sim-pred "tasks")  #$IntangibleIndividual) *VocabularyMt*)
(sim-assert-now `(#$arg2Isa ,(sim-pred "tasks")  #$Agent-Generic) *VocabularyMt*)
(sim-assert-now `(#$argAndRestIsa ,(sim-pred "tasks")  3 #$Thing) *VocabularyMt*)
(sim-assert-now `(#$arityMin ,(sim-pred "tasks")  2) *VocabularyMt*)
(sim-assert-now `(#$arityMax ,(sim-pred "tasks")  7) *VocabularyMt*)
(sim-assert-now `(#$backchainEncouraged ,(sim-pred "tasks") ) *VocabularyMt*)
(sim-assert-now `(#$backchainRequired ,(sim-pred "tasks") )  *CurrentStateMt* )
(sim-assert-now `(#$implies (,(sim-pred "eval")  (#$TheList ,(sim-pred "tasks") ?X) ?Y) (,(sim-pred "tasks")  ?X ?Y))  *CurrentStateMt* )
(sim-assert-now `(#$implies (,(sim-pred "eval")  (#$TheList ,(sim-pred "tasks") ?X ?A) ?Y) (,(sim-pred "tasks")  ?X ?A ?Y))  *CurrentStateMt* )
(sim-assert-now `(#$implies (,(sim-pred "eval")  (#$TheList ,(sim-pred "tasks") ?X ?A ?B) ?Y) (,(sim-pred "tasks")  ?X ?A ?B ?Y))  *CurrentStateMt* )
(sim-assert-now `(#$implies (,(sim-pred "eval")  (#$TheList ,(sim-pred "tasks") ?X ?A ?B ?C) ?Y) (,(sim-pred "tasks")  ?X ?A ?B ?C ?Y))  *CurrentStateMt* )
|# 

