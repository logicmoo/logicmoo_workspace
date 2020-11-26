;;;; %doom:possibleCycTerms
(doom-assert-now `(#$comment ,(doom-map "Method" "Name") "Maps (doomMethodToName ?PRED ?TEXT) like (#$synonymousExternalConcept ?PRED #$DoomVocabularyMt  ?ParamValueObject)") *VocabularyMt*)
                          
;;==================================================
;; INTERFACE MAPPING SETUP (dooMtouches)
;;==================================================
#|
(defdoom-pred "touches" *CurrentStateMt* 2)
(doom-assert-now `(#$isa ,(doom-pred "touches")  #$SymmetricBinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "touches")  #$CotemporalPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "touches")  #$TimeDependentRelation) *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "touches")  2) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "touches")  " (dooMtouches ?OBJ1 ?OBJ2) is true if the two objects are near enough to interact physically.") *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "touches")  #$touches)  *VocabularyMt*)
;;;;(doom-assert-now `(#$isa ,(doom-pred "touches")  #$SpatialPredicate) *VocabularyMt*)
;;;;(doom-assert-now `(#$arg1Isa ,(doom-pred "touches")  #$SpatialThing-Localized) *VocabularyMt*)
;;;;(doom-assert-now `(#$arg2Isa ,(doom-pred "touches")  #$SpatialThing-Localized) *VocabularyMt*)
;;==================================================
;; INTERFACE MAPPING SETUP (doomNear)
;;==================================================
(doom-assert-now `(#$isa ,(doom-pred "near")  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "near")  2) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "near")  #$CotemporalPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "near")  #$TimeDependentRelation) *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "near")  #$near)  *VocabularyMt*)
(defdoom-pred "near" *CurrentStateMt* 2)
(doom-assert-now `(#$isa ,(doom-pred "near")  #$SpatialPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "near")  " (doomNear ?OBJ1 ?OBJ2) is true if the two objects are near enough to interact physically.") *VocabularyMt*)
;;;;(doom-assert-now `(#$arg1Isa ,(doom-pred "near")  #$SpatialThing-Localized) *VocabularyMt*)
;;;;(doom-assert-now `(#$arg2Isa ,(doom-pred "near")  #$SpatialThing-Localized) *VocabularyMt*)
#|
;;==================================================
;; INTERFACE MAPPING SETUP (doomClasses)
;;==================================================
(defdoom-pred "classes" *StaticStateMt* 2)      
(doom-assert-now `(#$isa ,(doom-pred "classes")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "classes")  #$TaxonomicSlotForAnyThing) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "classes")  #$DefaultMonotonicPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "classes")  " (doomClasses ?OBJ1 ?OBJ2) is true for the OBJ2 is Instance to OBJ1.") *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "classes")  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa ,(doom-pred "classes")  #$BPVItem) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "classes")  #$Thing) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "classes")  #$FirstOrderCollectionPredicate) *VocabularyMt*)
(doom-assert-now '(#$completeExtentDecidable #$doomClasses) *CurrentStateMt* '(:DIRECTION :FORWARD))
|#
;;==================================================
;; INTERFACE MAPPING SETUP (doomInRegion)
;;==================================================
(defdoom-pred "inRegion" *CurrentStateMt* 2)
(doom-assert-now `(#$isa ,(doom-pred "inRegion")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "inRegion")  #$CoexistingObjectsPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "inRegion")  #$SpatialPredicate) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa ,(doom-pred "inRegion")  #$SpatialThing-Localized) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "inRegion")  #$Place-NonAgent) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "inRegion")  #$GeographicalSpaceRegion) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "inRegion")  #$Point) *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "inRegion")  2) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "inRegion")  " (doomInRegion ?OBJ1 ?LOC) is true for the nearest LOC entity to OBJ1.") *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "inRegion")  #$localityOfObject)  *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "inRegion")  #$objectFoundInLocation)  *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "inRegion")  ,(doom-pred "worldOrigin")) *VocabularyMt*)

;;==================================================
;; INTERFACE MAPPING SETUP (doomCanSee)
;;==================================================
(defdoom-pred "canSee" *CurrentStateMt* 2)
(doom-assert-now `(#$isa ,(doom-pred "canSee")  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "canSee")  #$PerceivingSlot) *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "canSee")  #$sees)  *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "canSee")  " (doomCanSee ?OBJ1 ?OBJ2) is true for the OBJ2 visable to OBJ1.") *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "canSee")  2) *VocabularyMt*) 
(doom-assert-now `(#$arg1Isa ,(doom-pred "canSee")  #$PerceptualAgent) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "canSee")  #$SpatialThing-Localized) *VocabularyMt*)
                                               
;;==================================================
;; INTERFACE MAPPING SETUP (doomWorldOrigin)
;;==================================================
(defdoom-pred "worldOrigin" *CurrentStateMt* 2)            
(doom-assert-now `(#$isa ,(doom-pred "worldOrigin")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "worldOrigin")  #$CotemporalPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "worldOrigin")  #$TimeDependentRelation) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "worldOrigin")  #$SpatialPredicate) *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "worldOrigin")  #$locatedAtPoint-Spatial)  *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "worldOrigin")  " (doomSpacePoint ?OBJ1 ?POINT) is true for the POINT is the location of OBJ1.") *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "worldOrigin")  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa ,(doom-pred "worldOrigin")  #$SpatialThing-Localized) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "worldOrigin")  #$Point) *VocabularyMt*)
                                    
;;==================================================
;; INTERFACE MAPPING SETUP (doomDirectionFacing)
;;==================================================
(defdoom-pred "angles" *CurrentStateMt* 2)            
(doom-assert-now `(#$isa ,(doom-pred "angles")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "angles")  #$TimeDependentRelation) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "angles")  #$SpatialPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "angles")  #$CotemporalPredicate) *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "angles")  #$orientation)  *VocabularyMt*)	   ;;direction-Pointing
;;;;(doom-assert-now `(#$genlPreds ,(doom-pred "angles")  #$direction-Pointing)  *VocabularyMt*)	   ;;
(doom-assert-now `(#$comment ,(doom-pred "angles")  " (doomDirectionFacing ?OBJ1 ?ANGLE2D) is true for the POINT is the location of OBJ1.") *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "angles")  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa ,(doom-pred "angles")  #$SpatialThing-Localized) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "angles")  #$UnitVectorInterval) *VocabularyMt*)
                                    
;;==================================================
;; INTERFACE MAPPING SETUP (doomAttached)
;;==================================================
(defdoom-pred "attached" *CurrentStateMt* 3)            
(doom-assert-now `(#$isa ,(doom-pred "attached")  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "attached")  #$ConnectionPredicate) *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "attached")  #$transports)  *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "attached")  #$controls)  *VocabularyMt*)
(doom-assert-now `(#$genlPreds ,(doom-pred "attached")  #$connectedTo)  *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "attached")  " (doomAttached ?OBJ1 ?OBJ2) is true for the OBJ2 is attached to OBJ1.") *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "attached")  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa ,(doom-pred "attached")  #$SpatialThing-Localized) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "attached")  #$SpatialThing-Localized) *VocabularyMt*)
;;==================================================
;; INTERFACE MAPPING SETUP (doomPlan)
;;==================================================
(defdoom-pred "plan" *CurrentStateMt* 3)            
(doom-assert-now `(#$isa ,(doom-pred "plan")  #$TernaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$backchainEncouraged ,(doom-pred "plan") ) *VocabularyMt*)
(doom-assert-now `(#$backchainRequired ,(doom-pred "plan") )  *CurrentStateMt* )
(doom-assert-now `(#$conceptuallyRelated ,(doom-pred "plan")  ,(doom-pred "eval") )   *VocabularyMt*)
(doom-assert-now `(#$arg1Isa ,(doom-pred "plan")  #$PlanningDomainMicrotheory) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "plan")  #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg3Isa ,(doom-pred "plan")  #$Thing) *VocabularyMt*)
;;;;(doom-assert-now `(#$implies (,(doom-pred "eval")  (#$TheList "evalPlanner" ?Mt ?Goal) ?Plan) (,(doom-pred "plan")  ?Mt ?Goal ?Plan))  *CurrentStateMt* )

;;==================================================
;; INTERFACE MAPPING SETUP (doomAction)
;;==================================================
(defdoom-pred "tasks" *CurrentStateMt* 3)            
(doom-assert-now `(#$isa ,(doom-pred "tasks")  #$Predicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "tasks")  #$VariableArityRelation) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "tasks")  #$ActionPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "tasks")  "(doomAction ?ACTION ?AGENT . EVENTARGS) relates ACTION to the list EVENTARGS of the relevant instances of #$RoleSlots that are inputs to the activity.") *VocabularyMt*)
(doom-assert-now `(#$conceptuallyRelated ,(doom-pred "tasks")  ,(doom-pred "eval") )   *VocabularyMt*)
(doom-assert-now `(#$arg1Genl ,(doom-pred "tasks")  #$Event) *VocabularyMt*)
(doom-assert-now `(#$arg1Genl ,(doom-pred "tasks")  #$IntangibleIndividual) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "tasks")  #$Agent-Generic) *VocabularyMt*)
(doom-assert-now `(#$argAndRestIsa ,(doom-pred "tasks")  3 #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arityMin ,(doom-pred "tasks")  2) *VocabularyMt*)
(doom-assert-now `(#$arityMax ,(doom-pred "tasks")  7) *VocabularyMt*)
(doom-assert-now `(#$backchainEncouraged ,(doom-pred "tasks") ) *VocabularyMt*)
(doom-assert-now `(#$backchainRequired ,(doom-pred "tasks") )  *CurrentStateMt* )
(doom-assert-now `(#$implies (,(doom-pred "eval")  (#$TheList ,(doom-pred "tasks") ?X) ?Y) (,(doom-pred "tasks")  ?X ?Y))  *CurrentStateMt* )
(doom-assert-now `(#$implies (,(doom-pred "eval")  (#$TheList ,(doom-pred "tasks") ?X ?A) ?Y) (,(doom-pred "tasks")  ?X ?A ?Y))  *CurrentStateMt* )
(doom-assert-now `(#$implies (,(doom-pred "eval")  (#$TheList ,(doom-pred "tasks") ?X ?A ?B) ?Y) (,(doom-pred "tasks")  ?X ?A ?B ?Y))  *CurrentStateMt* )
(doom-assert-now `(#$implies (,(doom-pred "eval")  (#$TheList ,(doom-pred "tasks") ?X ?A ?B ?C) ?Y) (,(doom-pred "tasks")  ?X ?A ?B ?C ?Y))  *CurrentStateMt* )
|# 

