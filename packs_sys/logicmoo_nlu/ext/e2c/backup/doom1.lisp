

;;(cyc-unassert
;;  '(#$implies (#$and (#$osim:gameType ?Obj ?String) (#$osim:gameCollection ?Col ?String)) (#$ist #$osim:SituationMt (#$isa ?Obj ?Col)))
;;   #$osim:DataMt)
;;

(cyc-assert '(#$implies (#$and (#$isa ?OBJ #$Agent-Generic)(#$termStrings ?OBJ ?STRING)(#$osim:gameCollection ?COL ?STRING)) (#$ist #$osim:SituationMt (#$isa ?OBJ ?COL))) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$isa ?OBJ #$Portal)(#$termStrings ?OBJ ?STRING)(#$osim:gameCollection ?COL ?STRING)) (#$ist #$osim:SituationMt (#$isa ?OBJ ?COL))) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$isa ?OBJ #$GeographicalRegion)(#$termStrings ?OBJ ?STRING)(#$osim:gameCollection ?COL ?STRING)) (#$ist #$osim:SituationMt (#$isa ?OBJ ?COL))) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$isa ?OBJ #$osim:Instance)(#$termStrings ?OBJ ?STRING)(#$osim:gameCollection ?COL ?STRING)) (#$ist #$osim:SituationMt (#$isa ?OBJ ?COL))) #$UniversalVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$osim:gameType ?Obj ?String) (#$osim:gameCollection ?Col ?String)) (#$ist #$osim:SituationMt (#$isa ?Obj ?Col))) #$osim:DataMt '(:DIRECTION :FORWARD))
;;(FI-REASSERT '(#$ist #$BaseKB (#$implies (#$and (#$osim:gameCollection (#$TLVariableFn 0 "?COL1") (#$TLVariableFn 1 "?STRING")) (#$osim:gameType (#$TLVariableFn 2 "?OBJ") (#$TLVariableFn 1 "?STRING"))) (#$ist #$osim:SituationMt (#$isa (#$TLVariableFn 2 "?OBJ") (#$TLVariableFn 0 "?COL1"))))) '#$BaseKB ':DEFAULT ':FORWARD)
;;(cyc-assert '(#$genlMt #$osim:PlanningMt #$OpenCycExampleTransportationPlanningDomainDataMt) #$BaseKB)
;;(cyc-assert '(#$genlMt #$osim:PlanningMt #$OpenCycExampleTransportationPlanningDomainMt) #$BaseKB)


;==================================================
; INSTANCE DATA
;==================================================
(create-constant "osim:location_center")
(create-constant "osim:room_start")
(create-constant "osim:room_mail")
(create-constant "osim:room_storage")
(create-constant "osim:room_o103")
(create-constant "osim:room_lab2")
(create-constant "osim:room_o111")
(cyc-assert '(#$isa #$osim:location_center #$SpatialThing-Localized) #$osim:SituationMt)
(cyc-assert '(#$osim:gameRegion #$osim:location_center (#$osim:Point3Fn 0 0 0)) #$osim:SituationMt)

(cyc-assert '(#$isa #$osim:room_start #$GeographicalSpaceRegion) #$osim:SituationMt)
(cyc-assert '(#$isa #$osim:room_mail #$GeographicalSpaceRegion) #$osim:SituationMt)
(cyc-assert '(#$isa #$osim:room_storage #$GeographicalSpaceRegion) #$osim:SituationMt)
(cyc-assert '(#$isa #$osim:room_o103 #$GeographicalSpaceRegion) #$osim:SituationMt)
(cyc-assert '(#$isa #$osim:room_lab2 #$GeographicalSpaceRegion) #$osim:SituationMt)
(cyc-assert '(#$isa #$osim:room_o111 #$GeographicalSpaceRegion) #$osim:SituationMt)


(cyc-assert '(#$osim:gameRegion #$osim:room_start (#$osim:Point3Fn 520 0 0)) #$osim:SituationMt)
(cyc-assert '(#$osim:gameRegion #$osim:room_mail (#$osim:Point3Fn 352 -40 0)) #$osim:SituationMt)
(cyc-assert '(#$osim:gameRegion #$osim:room_storage (#$osim:Point3Fn -656 1192 0)) #$osim:SituationMt)
(cyc-assert '(#$osim:gameRegion #$osim:room_o103 (#$osim:Point3Fn 520 -744 0)) #$osim:SituationMt)
(cyc-assert '(#$osim:gameRegion #$osim:room_lab2 (#$osim:Point3Fn 520 1192 0)) #$osim:SituationMt)
(cyc-assert '(#$osim:gameRegion #$osim:room_o111 (#$osim:Point3Fn 520 -1512 0)) #$osim:SituationMt)




;; initCyc('DataMt',forward(implies(and(gameCollection(Col1,String),gameType(Obj,String)),ist('SituationMt',isa(Obj,Col1))))).

;;(cyc-unassert '(#$implies (#$and (#$osim:gameCollection ?Col1 ?String) (#$osim:gameType ?Obj ?String)) (#$ist #$osim:SituationMt (#$isa ?Obj ?Col1)))#$osim:DataMt)
   

;;(cyc-assert '(#$implies (#$osim:gameInRegion ?X ?R) (#$and (#$objectFoundInLocation ?X ?R)(#$isa ?R #GeographicalSpaceRegion))) #$osim:SituationMt '(:DIRECTION :FORWARD))

;;(cyc-assert '(#$implies (#$osim:gameType ?X ?Y) (#$ist #$osim:VocabularyMt (#$isa ?X #$osim:Instance))) #$osim:DataMt '(:DIRECTION :FORWARD))



(cyc-assert '(#$genlMt #$osim:PlanningMt #$TransportationPlanningMt) #$BaseKB)
(cyc-assert '(#$genlMt #$osim:PlanningMt #$UnitedStatesGeographyMt) #$BaseKB)
(cyc-assert '(#$genlMt #$osim:PlanningMt #$ModernMilitaryVehiclesMt) #$BaseKB)
(cyc-assert '(#$genlMt #$osim:DataMt #$OpenCycExampleTransportationPlanningDomainMt) #$BaseKB)
(cyc-assert '(#$genlMt #$osim:DataMt #$GenericTouristMt) #$BaseKB)
(cyc-assert '(#$genlMt #$osim:DataMt #$ExampleAssertionsToolboxMt) #$BaseKB)


