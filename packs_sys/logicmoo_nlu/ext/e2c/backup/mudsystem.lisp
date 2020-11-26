
(create-constant "OSim-IdEntity")
(cyc-assert '(#$isa #$OSim-IdEntity #$Collection) #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$OSim-IdEntity #$AtemporalNecessarilyEssentialCollectionType) #$UniversalVocabularyMt)
(cyc-assert '(#$genls #$OSim-IdEntity #$SpatialThing-Localized) #$UniversalVocabularyMt)
(cyc-assert '(#$genls #$OSim-IdEntity #$Individual) #$UniversalVocabularyMt)
(cyc-assert '(#$genls #$OSim-IdEntity #$SomethingExisting) #$UniversalVocabularyMt)
(cyc-assert '(#$comment #$OSim-IdEntity "#$Individual inside the game engine that is already spawned in the world.") #$UniversalVocabularyMt)



(find-or-create-constant "BPVLocation")
(cyc-assert '(#$isa #$BPVLocation #$Collection)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVLocation #$OSim-IdEntity)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVLocation #$Place-NonAgent)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVLocation #$GeographicalRegion)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVLocation #$InanimateObject)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))

(cyc-assert '(comment #$BPVLocation "This is used to mark locations in the game therefore a specialization of #$OSim-IdEntity #$Place-NonAgent and #$GeographicalRegion.")  '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(comment #$BPVLocation "Every #$BPVLocation may have any of 18+ #$BoundsOfDirectionFn but implicitly must have 6 based on moving allong the 3 axis at a cone-shaped forty-five degree tollerance (*-Generally). at least specified")  '#$OSimVocabularyMt '(:DIRECTION :FORWARD))

(find-or-create-constant "BPVAgent")
(find-or-create-constant "BPVAgentType")
(cyc-assert '(#$genls #$BPVAgent #$OSim-IdEntity)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVAgent #$IndividualAgent)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVAgent #$IntelligentAgent)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))


(find-or-create-constant "BPVItem")
(find-or-create-constant "BPVItemType")
(cyc-assert '(#$genls #$BPVItem #$OSim-IdEntity)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$isa #$BPVItemType #$CollectionType) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$typeGenls #$BPVItemType #$BPVItem)'#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVItem #$InanimateObject)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVItem #$Artifact)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$BPVItem #$Artifact-NonAgentive)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))


;; DISJOINTS OF TYPES
(cyc-assert '(#$disjointWith #$BPVAgent #$BPVItem)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$disjointWith #$BPVLocation #$BPVItem)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$disjointWith #$BPVAgent #$BPVLocation)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$disjointWith #$BPVItem #$Person)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$disjointWith #$BPVLocation #$Person)   '#$OSimVocabularyMt '(:DIRECTION :FORWARD))


(create-constant "osim:descriptionStrings")
(cyc-assert '(#$isa #$osim:descriptionStrings #$BinaryPredicate)  '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(create-constant "osim:descriptionTerms")
(cyc-assert '(#$isa #$osim:descriptionTerms #$BinaryPredicate)  '#$OSimVocabularyMt '(:DIRECTION :FORWARD))

%osim:descriptionTerms

(cyc-assert '(#$arg2Isa #$BoundsOfDirectionFn #$TerrestrialDirection) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$resultIsa #$BoundsOfDirectionFn #$BoundaryMarker) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))


(cyc-assert '(#$implies (#$and (#$isa ?X #$OSim-IdEntity) (#$isa ?X ?Type) (#$relationAllInstance ?Pred ?Type ?Instance)) (#$ist #$OSimDataMappingMt (?Pred ?X ?Instance))) '#$OSimDataMappingMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$locatedAtPoint-Spatial #$Area1000 (#$Point3Fn 0 0 0)) '#$OSimDataMappingMt '(:DIRECTION :FORWARD))
    

(cyc-assert '(#$formsBorderPart (#$BoundsOfDirectionFn ?P1 ?Dir) (#$BorderFn ?P1)) '#$OSimDataMappingMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$isa ?P1 #$BPVLocation) (#$and (#$isa  (#$BorderFn ?P1) #$Border)(#$formsBorderPart (#$BoundsOfDirectionFn ?P1 ?Dir) (#$BorderFn ?P1)))) '#$OSimDataMappingMt '(:DIRECTION :FORWARD))


(define room-leads (loc dir)
  (ret (cdr (car (car  (cyc-query (list #$pathConnects (list #$BoundsOfDirectionFn loc (nsew-term dir) ) loc '?X) #$OSimDataMappingMt))))))

(define nsew-term (loc) 
  (ret 
   (clet ((loc (STRING-DOWNCASE loc)))
   (pcond 
      ((string-equal loc "n") #$North-Generally)
      ((string-equal loc "s") #$South-Generally)
      ((string-equal loc "e") #$East-Generally)
      ((string-equal loc "w") #$West-Generally)
      ((string-equal loc "u") #$Up-Generally)
      ((string-equal loc "d") #$Down-Generally)
      (T (throw 'fail))))))
      


(find-or-create-constant "osim:spawnPredicate")
(cyc-assert '(#$isa #$osim:spawnPredicate #$TernaryPredicate) #$UniversalVocabularyMt)
(cyc-assert '(#$comment #$osim:spawnPredicate "(#$osim:spawnPredicate ?Arg1 ?Arg2 ?P) will return #$isa or #$genls or some other mostly legal reation between the two arguments") #$UniversalVocabularyMt)
(cyc-assert '(#$isa #$osim:spawnPredicate #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(cyc-assert '(#$arity #$osim:spawnPredicate 3) #$UniversalVocabularyMt)
(cyc-assert '(#$arg1Isa #$osim:spawnPredicate #$Thing) #$UniversalVocabularyMt)
(cyc-assert '(#$arg2Genl #$osim:spawnPredicate #$ThreeDimensionalThing) #$UniversalVocabularyMt)
(cyc-assert '(#$arg2Genl #$osim:spawnPredicate #$SomethingExisting) #$UniversalVocabularyMt)
(cyc-assert '(#$arg3Isa #$osim:spawnPredicate #$BinaryPredicate) #$UniversalVocabularyMt)
(cyc-assert '(#$conceptuallyRelated #$osim:spawnPredicate #$OSimViolatesSomeConstraint) #$UniversalVocabularyMt)
(inference-removal-module :removal-spawnPredicate-bound-bound-unbound
 '(:sense :pos 
	:predicate #$osim:spawnPredicate 
	:required-pattern (#$osim:spawnPredicate :fully-bound :fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template  (#$osim:spawnPredicate (:bind value-1) (:bind value-2) :anything) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call spawnPredicate :input)
	:output-construct-pattern  (#$osim:spawnPredicate (:value value-1) (:value value-2) :input)
	:documentation "(#$osim:spawnPredicate <fully-bound> <not-fully-bound>)"
	:example "(#$osim:spawnPredicate -1 ?WHAT)"))
(register-solely-specific-removal-module-predicate #$osim:spawnPredicate)
(define spawnPredicate (vs)
 (clet ((v1 (car vs)) (v2 (nth 1 vs)))
  (cand  (cq (list #$isa v2 #$Collection)) (null (cq (list #$OSimViolatesSomeConstraint v2)))
   (ret (list (fif (cq (list #$isa v1 #$Collection)) #$genls #$isa))))))
       
;;HumanScaleObject     
	
(cyc-assert '(#$implies 
  (#$and 
    (#$isa ?I #$OSim-IdEntity)
    (#$webSearchableStrings ?I ?S) 
    (#$osim:representsContextually ?S ?C)
    (#$genls ?C #$SomethingExisting)     
    (#$genls ?C #$SpatialThing)     
    (#$genls ?C #$ThreeDimensionalThing)     
    (#$osim:spawnPredicate ?I ?C ?P)) 
  (#$ist #$OSimDataMappingMt (?P ?I ?C))) '#$OSimDataMappingMt '(:DIRECTION :FORWARD))

  
;;      

(find-or-create-constant "osim:UnprovableSentence")
(cyc-assert '(#$isa #$osim:UnprovableSentence #$Collection) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$defnSufficient #$osim:UnprovableSentence (#$SubLQuoteFn UnprovableSentence)) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(define UnprovableSentence (v) (null (cq v )))

(find-or-create-constant "osim:ProvableSentence")
(cyc-assert '(#$isa #$osim:ProvableSentence #$Collection) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$defnSufficient #$osim:ProvableSentence (#$SubLQuoteFn ProvableSentence)) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(define ProvableSentence (v) (cand (cq v )))


(cyc-assert '(#$implies (#$isa (#$OSimViolatesSomeConstraint ?C) #$osim:UnprovableSentence) (#$osim:exceedsSomeConstraint ?C))  '#$OSimDataMappingDataMt)
       

(cyc-assert '(#$implies (#$and (#$isa ?X #$BPVAgent) (#$isa ?X ?Type) (#$relationAllInstance ?Pred ?Type ?Instance)) (#$ist #$OSimDataMappingMt (?Pred ?X ?Instance))) '#$OSimDataMappingMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$implies (#$and (#$isa ?X #$BPVAgent) (#$isa ?X ?Type) (#$relationAllInstance ?Pred ?Type ?Instance)) (#$ist #$OSimDataMappingMt (?Pred ?X ?Instance))) '#$OSimDataMappingMt '(:DIRECTION :FORWARD))


(cyc-assert '(#$disjointWith #$osim:ProvableSentence #$osim:UnprovableSentence) '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$osim:ProvableSentence #$CycLSentence-Askable)  '#$OSimVocabularyMt '(:DIRECTION :FORWARD))
(cyc-assert '(#$genls #$osim:UnprovableSentence #$CycLSentence-Askable)  '#$OSimVocabularyMt '(:DIRECTION :FORWARD))

      
(define nsew-from (loc) (ret (STRING-DOWNCASE (string (char (constant-name loc) 0)))))
    
(define nsew-path (loc dir) (ret (list #$BoundsOfDirectionFn  loc (nsew-term dir))))

; (cyc-query '(#$pathConnects (#$BoundsOfDirectionFn #$Area1000 #$North-Generally) ?P1 ?P2) #$OSimDataMappingMt)
(define room-walls (loc) 
  (ret (clet ((str ""))
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "n") loc '?X) #$OSimDataMappingMt)
        T (csetq str (cconcatenate str "n")))
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "s") loc '?X) #$OSimDataMappingMt)
        T (csetq str (cconcatenate str "s")))
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "e") loc '?X) #$OSimDataMappingMt)
        T (csetq str (cconcatenate str "e")))
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "w") loc '?X) #$OSimDataMappingMt)
        T (csetq str (cconcatenate str "w")))	
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "u") loc '?X) #$OSimDataMappingMt)
        T (csetq str (cconcatenate str "u")))	
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "d") loc '?X) #$OSimDataMappingMt)
       T (csetq str (cconcatenate str "d")))	
    str)))
    
(define room-exits (loc) 
  (ret (clet ((str ""))
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "n") loc '?X) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "n")) T)
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "s") loc '?X) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "s")) T)
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "e") loc '?X) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "e")) T)
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "w") loc '?X) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "w")) T)	
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "u") loc '?X) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "u")) T)	
     (pif  (cyc-query  (list #$pathConnects (nsew-path loc "d") loc '?X) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "d")) T)	
    str)))

(define room-exit-type (loc etype) 
  (ret (clet ((str ""))
     (pif  (cyc-query (print (list #$isa (nsew-path loc "n") etype)) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "n")) T)
     (pif (cyc-query (list #$isa (list #$BoundsOfDirectionFn loc #$South-Generally) etype) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "s")) T)
     (pif (cyc-query (list #$isa (list #$BoundsOfDirectionFn loc #$East-Generally) etype) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "e")) T)
     (pif (cyc-query (list #$isa (list #$BoundsOfDirectionFn loc #$West-Generally) etype) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "w")) T)	
     (pif (cyc-query (list #$isa (list #$BoundsOfDirectionFn loc #$Up-Generally) etype) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "u")) T)	
     (pif (cyc-query (list #$isa (list #$BoundsOfDirectionFn loc #$Down-Generally) etype) #$OSimDataMappingMt)
        (csetq str (cconcatenate str "d")) T)	
    str)))


 ;PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
 ;7224 root      3 -13 1955m 433m 103m R 99.8 21.4  23:45.48 latest-cyc.bin
 
 ;PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
  ;7224 root      3 -13 1955m 634m 104m R 99.5 31.3  56:26.76 latest-cyc.bin

(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH1 ?D1 ?D2) (#$pathBetween ?PATH2 ?D2 ?D1))(#$equals ?PATH1 ?PATH2))  #$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
    
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Bedroom)) (#$isa ?PATH #$Doorway))  #$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$OfficeSpace-Personal)) (#$isa ?PATH #$Doorway))  #$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$isa ?PATH #$OpenPortal))  #$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$not (#$isa ?PATH #$Doorway)))  #$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$not (#$isa ?PATH #$ClosedPortal)))  #$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2)(#$isa ?PATH #$Portal)) (#$bordersOn  ?D1 ?D2))#$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )

;;(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH ?P1 ?P2) (#$isa ?P2 #$Hallway)) (#$isa ?PATH #$Doorway))  #$OSimDataMappingMt '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )

;(room-exits #$Area1000)
;;(#$isa  #$GameCollection)

;(define exits-open (loc) (ret (is-of-exit-type loc #$OpenPortal)))
;(define exits-closed (loc) (ret (is-of-exit-type loc #$ClosedPortal)))		'



;(ofl #$ArtifactCol1009-1009-10-Medical-Tricorder681)
(define ofl1 (q &optional (hmt #$EverythingPSC)) (ret (car (cyc-query q hmt '(:backchain T  :time 5 :number 1)))))
(define ofl (term) 
  (clet ((result NIL))
   (csetq result (cdr (car (ofl1 (list #$wornOn term '?W)))))
   (cand result (ret (list #$wornOn result)))
   (csetq result (cdr (car (ofl1 (list #$possesses '?W term)))))
   (cand result (ret (list #$possesses result)))
   (csetq result (cdr (car (ofl1 (list #$locatedAtPoint-Spatial term '?W)))))
   (cand result (ret result))
   (csetq result (cdr (car (ofl1 (list #$and (list #$objectFoundInLocation term '?W)(list #$isa '?W #$OSim-IdEntity))))))
   (cand result (ret (list #$objectFoundInLocation result)))
   (ret result)))


(cyc-assert '(#$equiv 
    (#$isa (#$BoundsOfDirectionFn ?P1 ?D1) #$Wall-GenericBarrier) 
    (#$cavityHasWall ?P1 (#$BoundsOfDirectionFn ?P1 ?D1) ) ) #$OSimDataMappingMt '(:DIRECTION :FORWARD))
    
(cyc-assert '(#$implies 
    (#$isa (#$BoundsOfDirectionFn ?P1 #$East-Generally) #$Wall-GenericBarrier) 
    (#$osimClasses (#$BoundsOfDirectionFn ?P1 #$East-Generally) "mud_eclosed" )) #$OSimDataMappingMt)
(cyc-assert '(#$implies 
    (#$isa (#$BoundsOfDirectionFn ?P1 #$West-Generally) #$Wall-GenericBarrier) 
    (#$osimClasses (#$BoundsOfDirectionFn ?P1 #$West-Generally) "mud_wclosed" )) #$OSimDataMappingMt)
(cyc-assert '(#$implies 
    (#$isa (#$BoundsOfDirectionFn ?P1 #$North-Generally) #$Wall-GenericBarrier) 
    (#$osimClasses (#$BoundsOfDirectionFn ?P1 #$North-Generally) "mud_nclosed" )) #$OSimDataMappingMt)
(cyc-assert '(#$implies 
    (#$isa (#$BoundsOfDirectionFn ?P1 #$South-Generally) #$Wall-GenericBarrier) 
    (#$osimClasses (#$BoundsOfDirectionFn ?P1 #$South-Generally) "mud_sclosed" )) #$OSimDataMappingMt)
(cyc-assert '(#$implies 
    (#$isa (#$BoundsOfDirectionFn ?P1 #$Up-Generally) #$Wall-GenericBarrier) 
    (#$osimClasses (#$BoundsOfDirectionFn ?P1 #$Up-Generally) "mud_uclosed" )) #$OSimDataMappingMt)
(cyc-assert '(#$implies 
    (#$isa (#$BoundsOfDirectionFn ?P1 #$Down-Generally) #$Wall-GenericBarrier) 
;;    (#$and (#$isa (#$BoundsOfDirectionFn ?P1 #$Down-Generally) #$Wall-GenericBarrier) 
    (#$osimClasses (#$BoundsOfDirectionFn ?P1 #$Down-Generally) "mud_dclosed" )) #$OSimDataMappingMt)







