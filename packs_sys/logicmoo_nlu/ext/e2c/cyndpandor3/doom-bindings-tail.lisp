(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "admin_impdoors")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "admin_imp")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "admin_doorframe_pinkyattack")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "admin_bfgcase")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "aas_sabaoth")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "aas_mancubus")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "aas_guardian")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "aas_cyberdemon")) *MappingMt*)
(doom-assert-now '(#$coextensionalSetOrCollections #$Thing (#$DoomClassFn "aas")) *MappingMt*)


;;(templateTopicGenls DoomFoundInLocations-TopicType DoomTypeCreatable)
(find-or-create-constant "DoomWorldSpawn-TopicType")
(cyc-assert '(#$isa #$DoomWorldSpawn-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$titleForFormulaTemplateType-String #$DoomWorldSpawn-TopicType "Topic template for constraining worldspawning") *DEFINITIONMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$DoomWorldSpawn-TopicType) *VOCABMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$templateTopicGenls #$DoomWorldSpawn-TopicType #$Doom-IdEntity) *VOCABMT* '(:DIRECTION :FORWARD))

(find-or-create-constant "DoomFoundInLocations-TopicType")
(cyc-assert '(#$isa #$DoomFoundInLocations-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$isa #$DoomFoundInLocations-TopicType #$Collection) *DEFINITIONMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$coextensionalSetOrCollections #$DoomFoundInLocations-TopicType #$DoomWorldSpawn-TopicType) *DEFINITIONMT* '(:DIRECTION :FORWARD))
(cyc-assert '(#$titleForFormulaTemplateType-String #$DoomFoundInLocations-TopicType "Topic template for constraining objects in rooms") *DEFINITIONMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$DoomFoundInLocations-TopicType) *VOCABMT* '(:DIRECTION :FORWARD))
;;(cyc-assert '(#$templateTopicGenls #$DoomFoundInLocations-TopicType #$DoomTypeCreatable) *VOCABMT* '(:DIRECTION :FORWARD))


(doom-assert-now '(#$disjointWith #$Doom-ProvableSentence #$Doom-UnprovableSentence) *VOCABMT* '(:DIRECTION :FORWARD))

;;;;  
;;(doom-assert-now '(#$implies (#$and (#$isa ?I ?TYPE)(#$coextensionalSetOrCollections ?TYPE #$Doom-IdEntity)(#$termStrings ?I ?S) (#$doom:denotesContextually ?S (#$SpawnTopicTypeFn ?TYPE) ?C)) (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))

	
'(doom-assert-now '(#$implies 
 (#$and 
 (#$isa ?I #$Doom-IdEntity)
 (#$webSearchableStrings ?I ?S) 
 (#$doom:denotesContextually ?S ?C)
 (#$coextensionalSetOrCollections ?C #$SomethingExisting)  
 (#$coextensionalSetOrCollections ?C #$SpatialThing)  
 (#$coextensionalSetOrCollections ?C #$ThreeDimensionalThing)  
 (#$doomSpawnClass ?I ?C ?P)) 
 (#$ist *MAPPINGMT* (?P ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))


(doom-assert-now '(#$termStrings #$Brig "brig") *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$termStrings #$Bedroom "quarters") *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$termStrings #$MachinePistol "phaser") *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$termStrings #$AirplaneCockpit "bridge") *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$termStrings #$PropositionalConceptualWork "world") *MAPPINGMT*)
(doom-assert-now '(#$termStrings #$Bottle "bottle") *MAPPINGMT*)
(doom-assert-now '(#$termStrings #$MarinePersonnel "ensign") *MAPPINGMT*)
(doom-assert-now '(#$termStrings #$Leader "Commander") *MAPPINGMT*)
(doom-assert-now '(#$termStrings #$SignTheDisplay "sign") *MAPPINGMT*)
(doom-assert-now '(#$termStrings #$Elevator "Turbolift") *MAPPINGMT*)
(doom-assert-now '(#$termStrings #$Bottle "bottle") *MAPPINGMT*)
(doom-assert-now '(#$termStrings #$LightingDevice "light") *MAPPINGMT*)

(find-or-create-constant "SpawnTopicTypeFn")
(doom-assert-now '(#$isa #$SpawnTopicTypeFn #$UnaryFunction) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$isa #$SpawnTopicTypeFn #$ReifiableFunction) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$skolemizeForward #$SpawnTopicTypeFn) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$resultIsa #$SpawnTopicTypeFn #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg1Genl #$SpawnTopicTypeFn #$Doom-IdEntity) *MAPPINGMT* '(:DIRECTION :FORWARD))

(doom-assert-now '(#$isa (#$SpawnTopicTypeFn #$BPVAgent) #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$isa (#$SpawnTopicTypeFn #$BPVArtifact) #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$isa (#$SpawnTopicTypeFn #$BPVLocation) #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))

(doom-assert-now '(#$coextensionalSetOrCollections (#$SpawnTopicTypeFn #$BPVAgent) #$DoomWorldSpawn-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$coextensionalSetOrCollections (#$SpawnTopicTypeFn #$BPVArtifact) #$DoomWorldSpawn-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$coextensionalSetOrCollections (#$SpawnTopicTypeFn #$BPVLocation) #$DoomWorldSpawn-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$implies (#$isa (#$SpawnTopicTypeFn ?COL) #$FormulaTemplateTopicType)(#$templateTopicGenls (#$SpawnTopicTypeFn ?COL) ?COL)) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;;;;;(doom-assert-now '(#$implies (#$and (#$isa ?I ?TYPE)(#$coextensionalSetOrCollections ?TYPE #$Doom-IdEntity)(#$termStrings ?I ?S) (#$doom:denotesContextually ?S (#$SpawnTopicTypeFn ?TYPE) ?C)) (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))
  

;;;;(doom-assert-now '(#$implies (#$isa (#$doomViolatesSomeConstraint ?C) #$Doom-UnprovableSentence) (#$doomExceedsSomeConstraint ?C)) *MAPPINGMT*)
  

;;;;(doom-assert-now '(#$locatedAtPoint-Spatial #$Area1000 (#$Point3Fn 0 0 0)) *MAPPINGMT* '(:DIRECTION :FORWARD))



(find-or-create-constant "doom:parsesContextually")
(doom-assert-now '(#$isa #$doom:parsesContextually #$Predicate) *UVMT*)
(doom-assert-now '(#$comment #$doom:parsesContextually "HL Predicate (#$parsesContextually ?String ?Ctx ?Result) uses the (parse-a-sentence-completely ?String #$AllEnglishLexicalMicrotheoryPSC) to produce ?Result that is choosen best in ?Ctx.") *UVMT*)
(doom-assert-now '(#$isa #$doom:parsesContextually #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(doom-assert-now '(#$arity #$doom:parsesContextually 3) *UVMT*)
(doom-assert-now '(#$arg1Isa #$doom:parsesContextually #$TextString) *UVMT*)
(doom-assert-now '(#$arg2Isa #$doom:parsesContextually #$Thing) *UVMT*)
(doom-assert-now '(#$arg3Isa #$doom:parsesContextually #$Thing) *UVMT*)
(inference-removal-module :removal-parsesContextually-bound-bound-unbound
 '(:sense :pos 
	:predicate #$doom:parsesContextually 
	:required-pattern (#$doom:parsesContextually :fully-bound :fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$doom:parsesContextually (:bind value-1) (:bind value-2) :anything) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call parsesContextually :input)
	:output-construct-pattern (#$doom:parsesContextually (:value value-1) (:value value-2) :input)))
(register-solely-specific-removal-module-predicate #$doom:parsesContextually)
(define parsesContextually (vs) 
 (clet ((str (car vs))(mt (nth 1 vs))(res nil))
 (cand (stringp str) 
 (progn
 (cand 
  (cq1 (list #$isa mt #$Microtheory))
  (progn 
	 (csetq res (parse-a-question-completely str mt))
	 (cand res (ret res))
	 (csetq res (parse-a-sentence-completely str mt))
	 (cand res (ret res))))
 (csetq res (parse-a-question-completely str #$AllEnglishLexicalMicrotheoryPSC))
 (cand res (ret res))
 (ret (parse-a-sentence-completely str #$AllEnglishLexicalMicrotheoryPSC))))))

(define parsesContextually (vs) 
 (clet ((str (car vs))(mt (nth 1 vs))(res nil))
 (cand (stringp str) 
 (csetq res (parse-a-question-completely str #$AllEnglishTemplateMt))
 (cand res (ret res))
 (csetq res (parse-a-sentence-completely str #$AllEnglishTemplateMt))
 (cand res (ret res))
 (csetq res (parse-a-question-completely str #$AllEnglishLexicalMicrotheoryPSC))
 (cand res (ret res))
 (ret (parse-a-sentence-completely str #$AllEnglishLexicalMicrotheoryPSC)))))

(define parsesContextually (vs) 
 (clet ((str (car vs))(mt (nth 1 vs))(res nil))
 (cand (stringp str) 
 (csetq res (parse-a-question-completely str #$AllEnglishLexicalMicrotheoryPSC))
 (cand res (ret res))
 (ret (parse-a-sentence-completely str #$AllEnglishLexicalMicrotheoryPSC)))))
 
;;(doom-assert-now '(#$implies (#$and(#$definiteDescriptions ?AREA ?TEXT)(#$doom:parsesContextually ?TEXT #$DoomWorldSpawn-TopicType ?CYCL))(#$futureAssertion ?AREA ?TEXT ?CYCL))*MAPPINGMT* '(:DIRECTION :FORWARD))


 
;;;;(parsesContextually1 "What is the population of the Turkey?")
;;;;(parsesContextually1 "How many people live in the USA?")

(define parsesContextually1 (str) (parsesContextually (list str '#$DoomWorldSpawn-TopicType)))

Three large chairs in the northern part of the room, in front of the railing, face the screen

Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship

You can't see a bed in this room, but you figure it's because Data doesn't sleep 
 )))


(cyc-query '(#$doom:parsesContextually "I love you" #$DoomWorldSpawn-TopicType ?X) *UVMT*) 

;;(define bestdenoted (l ctx) (cand l (clet ((best (car l))) (progn (cdolist (x (cdr l)) (csetq best (better1 best x ctx))) (ret (list (cdr best)))))))


;;doom:denotesContextually
(find-or-create-constant "doom:denotesContextually")
(doom-assert-now '(#$isa #$doom:denotesContextually #$Predicate) *UVMT*)
(doom-assert-now '(#$comment #$doom:denotesContextually "HL Predicate (#$denotesContextually ?String ?Ctx ?Result) uses the (denotation-mapper ?String) to produce ?Result that is choosen best in ?Ctx.") *UVMT*)
(doom-assert-now '(#$isa #$doom:denotesContextually #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(doom-assert-now '(#$arity #$doom:denotesContextually 3) *UVMT*)
(doom-assert-now '(#$arg1Isa #$doom:denotesContextually #$TextString) *UVMT*)
(doom-assert-now '(#$arg2Isa #$doom:denotesContextually #$Thing) *UVMT*)
(doom-assert-now '(#$arg3Isa #$doom:denotesContextually #$Thing) *UVMT*)
;;;;(doom-assert-now '(#$genlPreds #$meetsSpecification #$doom:denotesContextually) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$termRankingPreference) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$Criterion) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$superiorTypeWRTCriterion) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$superiorWRTCriterion) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$ratingTypeCriterion) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$meetsSpecification) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$doomExceedsSomeConstraint) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doom:denotesContextually #$doomViolatesSomeConstraint) *UVMT*)


;;;;(doom-assert-now (#$isa #$DoomWorldSpawn-TopicType #$FormulaTemplateTopicType
(inference-removal-module :removal-denotesContextually-bound-bound-unbound
 '(:sense :pos 
	:predicate #$doom:denotesContextually 
	:required-pattern (#$doom:denotesContextually :fully-bound :fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$doom:denotesContextually (:bind value-1) (:bind value-2) :anything) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call denoter :input)
	:output-construct-pattern (#$doom:denotesContextually (:value value-1) (:value value-2) :input)))
(register-solely-specific-removal-module-predicate #$doom:denotesContextually)
(define denoter (vs) (cand (stringp (car vs))(constantp (nth 1 vs)) (ret (bestdenoted (remove-violaters (denotation-mapper (car vs) nil :diligent) (nth 1 vs)) (nth 1 vs)))))
(define bestdenoted (l ctx) (cand l (clet ((best (car l))) (progn (cdolist (x (cdr l)) (csetq best (better1 best x ctx))) (ret (list (cdr best)))))))


(find-or-create-constant "doom:denotationMapper")
(doom-assert-now '(#$isa #$doom:denotationMapper #$Predicate) *UVMT*)
(doom-assert-now '(#$comment #$doom:denotationMapper "HL Predicate (#$denotationMapper ?String ?Result ?Lex) uses the (denotation-mapper ?String) to produce ?Result that is choosen best in ?Ctx.") *UVMT*)
(doom-assert-now '(#$isa #$doom:denotationMapper #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(doom-assert-now '(#$arity #$doom:denotationMapper 3) *UVMT*)
(doom-assert-now '(#$arg1Isa #$doom:denotationMapper #$TextString) *UVMT*)
(doom-assert-now '(#$arg2Isa #$doom:denotationMapper #$Thing) *UVMT*)
(doom-assert-now '(#$arg3Isa #$doom:denotationMapper #$Thing) *UVMT*)
(doom-assert-now '(#$isa #$DoomWorldSpawn-TopicType #$FormulaTemplateTopicType) *UVMT*)
(inference-removal-module :removal-denotationMapper-bound-unbound-unbound
 '(:sense :pos 
	:predicate #$doom:denotationMapper 
	:required-pattern (#$doom:denotationMapper :fully-bound :not-fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$doom:denotationMapper (:bind value-1) :anything :anything) ((:value value-1)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call dentoterMapper :input)))
(register-solely-specific-removal-module-predicate #$doom:denotationMapper)
(define dentoterMapper (calv) (ret (cand (stringp (car calv))
  (clet ((str (car calv))(lst (denotation-mapper str nil))(results nil))
  (cdolist (x lst) (csetq results (cons (list #$doom:denotationMapper str (car x) (cdr x)) results)))
  (ret results)))))


;;(cyc-query '(#$doom:denotationMapper "good" ?C ?Y) '#$EverythingPSC)

(define remove-violaters (lst ctx)
 (ret (cand (consp lst)
 (clet ((results nil))
 (cdolist (x lst) 
 (cand (consp x) (null (cq (list #$doomViolatesSomeConstraint (cdr x) ctx))) (csetq results (cons x results))))
 (ret results)))))
          
;;;;(cyc-query '(#$and (#$isa ?I #$Doom-IdEntity) (#$nameString ?I ?S) (#$doom:denotesContextually ?S #$DoomWorldSpawn-TopicType ?C)) '#$EverythingPSC)
  
(define cq1 (q &optional (hmt #$EverythingPSC)) (ret (car (cyc-query q hmt '(:backchain T :time 15 :number 1)))))
(define cq (q &optional (hmt #$EverythingPSC)) (ret (cyc-query q hmt '(:backchain T :time 30))))
(define cq (q &optional (hmt #$EverythingPSC)) (clet ((result (cyc-query q hmt '(:backchain T)))) (print (list q hmt result)) (ret result)))
(define better1 (x y ctx) (clet ((best (better2 y x ctx))) (print (list (cdr x) (cdr y ) best )) (ret best)))

(print "loading termStrings.")

 
;;;; (print (list x y ))(force-output)
;;;; Assume violators are removed first with #'remove-violaters
(define better1 (x y ctx)
 (clet ((xx (cdr x)) (yy (cdr y )))
 (pcond 
 ((eq xx yy) (ret x))
 ((cq (list #$doomExceedsSomeConstraint xx ctx)) (ret x ))
 ((cq (list #$doomExceedsSomeConstraint yy ctx)) (ret y ))
 ((cq (list #$termRankingPreference ctx xx yy)) (ret xx ))
 ((cq (list #$termRankingPreference ctx yy xx)) (ret yy ))
 ((cq (list #$superiorWRTCriterion xx yy ctx)) (ret xx ))
 ((cq (list #$superiorWRTCriterion yy xx ctx)) (ret yy ))
 ((> (length (car x)) (length (car y ))) (ret x ) )
 ((< (length (car x)) (length (car y ))) (ret y ) )
 ((consp yy) (ret x ))
 ((consp xx) (ret y ))
 ( (> (length (all-term-assertions xx)) (length (all-term-assertions yy ))) (ret y ))
 ( (< (length (all-term-assertions xx)) (length (all-term-assertions yy ))) (ret x ))
 (T (ret x )))))
      

(define denoter1 (x) (ret (denoter (list x '#$DoomWorldSpawn-TopicType))))
(define denoter2 (str) (ret (remove-violaters (denotation-mapper str nil :diligent) '#$DoomWorldSpawn-TopicType ) ))

(cq '(#$doomViolatesSomeConstraint #$Area1031 #$DoomWorldSpawn-TopicType))
(load "/rcyc/cycdoom/startrek.lisp")
(load "/rcyc/cycdoom/mudtool.lisp")
(denoter1 "Quarters")
(denoter1 "room")
(denoter1 "bridge")
(denoter1 "trombone")
(denoter1 "cargo bay")
(denoter1 "captain")
 
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$Doom-IdEntity)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$Doom-IdEntity)) (ret x ))
 ((cq (list #$isa (cdr x ) #$ChromaticColor)) (ret x ))
 ((cq (list #$isa (cdr y ) #$ChromaticColor)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr x ) #$Device-SingleUser)) (ret x ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$Device-SingleUser)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr x ) #$Facility-Generic)) (ret x ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$Facility-Generic)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$ScalarInterval)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$ScalarInterval)) (ret x ))
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$Ocean)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$Ocean)) (ret x ))
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$IntangibleIndividual)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$IntangibleIndividual)) (ret x )) 
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$Intangible)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$Intangible)) (ret x )) 
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$AminoAcid)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$AminoAcid)) (ret x ))   
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$MolecularComponent)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$MolecularComponent)) (ret x )) 
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$AspatialInformationStore)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$AspatialInformationStore)) (ret x )) 
 ((cq (list #$coextensionalSetOrCollections (cdr x) #$Event)) (ret y ))
 ((cq (list #$coextensionalSetOrCollections (cdr y ) #$Event)) (ret x ))
 ((cq (list #$isa (cdr x ) #$Individual )) (ret y ))
 ((cq (list #$isa (cdr y ) #$Individual )) (ret x ))
 ((> (length (car x)) (length (car y ))) (ret x) )
 ((null (cq (list #$isa (cdr x ) #$Collection ))) (ret y ))
 ((null (cq (list #$isa (cdr y ) #$Collection ))) (ret x ))


(doom-assert-now '
 (#$equals 
 (#$definiteDescriptions ?PLACE "Two large windows offer a great view of space") 
 (#$thereExistExactly 2 ?INST 
 (#$and 
  (#$isa ?INST #$VisualInformationBearingThing) 
  (#$isa ?INST #$WindowPortal) 
  (#$objectFoundInLocation ?INST ?PLACE) 
  (#$visuallyDepicts ?INST #$OuterSpace))))	*MAPPINGMT*)
  

denotation-mapper

(#$implies 
 (#$and 
 (#$isa ?I #$Doom-IdEntity) 
 (#$or (#$termStrings ?I ?S) 
 (#$webSearchableStrings ?I ?S) 
 (#$nameString ?I ?S)) 
 (#$doom:denotesContextually ?S ?C) 
 (#$doomSpawnClass ?I ?C ?P)) 
 (#$ist *MAPPINGMT* 
 (?P ?I ?C)))
 
;;;; (#$or (#$isa ?I #$BPVLocation)(#$isa ?I #$BPVAgent)(#$isa ?I #$BPVArtifact))
 ;;;;(#$or (#$nameString ?I ?S) (#$webSearchableStrings ?I ?S))

(doom-assert-now '(#$implies 
(#$and 
 (#$isa ?I #$Doom-IdEntity)
 (#$termStrings ?I ?S)
 (#$doom:denotesContextually ?S #$DoomWorldSpawn-TopicType ?C))
 (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT*)

(doom-assert-now '(#$implies 
(#$and 
 (#$isa ?I #$BPVAgent)
 (#$termStrings ?I ?S) 
 (#$doom:denotesContextually ?S #$DoomWorldSpawn-TopicType ?C))
 (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))

(doom-assert-now '(#$isa #$TheAtmosphereQuaSinglePieceOfStuff #$FluidTangibleThing) *MAPPINGMT*)
(doom-assert-now '(#$isa #$TheAtmosphereQuaSinglePieceOfStuff #$PartiallyTangible) *MAPPINGMT*)
(doom-assert-now '(#$isa #$TheAtmosphereQuaSinglePieceOfStuff #$SpatialThing-Localized) *MAPPINGMT*)


(doom-assert-now '(#$implies 
(#$and 
;;;; (#$or (#$isa ?I #$BPVLocation)(#$isa ?I #$BPVAgent)(#$isa ?I #$BPVArtifact))
 (#$isa ?I #$Doom-IdEntity)
 (#$termStrings ?I ?S) 
 (#$doom:denotesContextually ?S #$DoomWorldSpawn-TopicType ?C))
 (#$ist *MAPPINGMT* (#$isa ?I ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))

 
 


(find-or-create-constant "DoomTypeCreatable")
(doom-assert-now '(#$isa #$DoomTypeCreatable #$CollectionType) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$coextensionalSetOrCollections #$DoomTypeCreatable) *MAPPINGMT*)
(doom-assert-now '(#$implies (#$coextensionalSetOrCollections ?COL ?HOW) (#$isa ?COL #$DoomTypeCreatable)) *MAPPINGMT*)
(doom-assert-now '(#$comment #$DoomTypeCreatable "If something is an instance of #$DoomTypeCreatable then it may be spawned)") *UVMT*)

(find-or-create-constant "DoomTypePossible")
(doom-assert-now '(#$isa #$DoomTypePossible #$CollectionType) *UVMT*)
(doom-assert-now '(#$coextensionalSetOrCollections #$DoomTypeCreatable #$DoomTypePossible ) *UVMT*)
(doom-assert-now '(#$isa #$GeneralLivingAreaFurnitureMC #$DoomTypePossible ) *MAPPINGMT*)
(doom-assert-now '(#$isa #$Decoration #$DoomTypePossible ) *MAPPINGMT*)
(doom-assert-now '(#$isa #$Person-SupportingFurniture #$DoomTypePossible ) *MAPPINGMT*)



(doom-assert-now '(#$comment #$DoomTypePossible "If something is an instance of #$DoomType then it's specs may be spawned if they are not #$DoomTypeTooGeneral") *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$DoomTypePossible #$DoomTypeCreatable) *MAPPINGMT*)

(exceptWhen (isa ?ARG1 DoomTypeExceptions)
 (implies 
   (isa ?ARG1 DoomTypePossibles) 
   (isa ?ARG1 DoomTypeCreatable)))
	 
;;since i work on an expert system that designs new dooms i needed a better target infrastructure. so i am converting the doom3 doom to a SDK that lets you write dooms that are more RPG-like. 
whenever i get frustrated i look for a new engine/system ;;P.. even though this is a potental govt contract work, I dont get paid and i have to use free things
looks pretty neat i almost want to switch. dooms graphics is not interesting.. mainly its tools that enable devopers to write dooms that i care about. droid area system
well my expert system is a webserver app.. so web doesnt bother me
[13:58] <dmiles> oh so only one person plays?
[13:58] <@Uhfgood> essentially you go in build your bot, upload a script, and enter an arena
[13:59] <@Uhfgood> when others enter, then a fight can take place
[13:59] <dmiles> ah.. i should look at their programming language
[13:59] <dmiles> did you have to write a yacc?
[13:59] <@Uhfgood> many people play... in fact it can't exactly be played alone, although you can run a fight simulation, which pits your bot against 3 simulated bots
;;yacc=yet another c commpiler 
one problem is i am doing research that probly wont make me or them money.. if i get government grants sure i dont mind giving them %
	 

(isa 

DoomTypeExceptions
 
 (CollectionDifferenceFn DoomTypePossibles DoomTypeExceptions) 
(implies (isa ?ARG1 DoomTypeExceptions)(not (isa ?ARG1 DoomTypeCreatable)))

(implies (isa ?ARG1 DoomTypePossibles)(or (isa ?ARG1 DoomTypeExceptions) (isa ?ARG1 DoomTypeCreatable)))

(implies (isa ?ARG1 DoomTypeCreatable) (isa ?ARG1 DoomTypePossibles))
(implies (isa ?ARG1 DoomTypeExceptions) (not (isa ?ARG1 DoomTypePossibles)))

(implies (genls ?X Intangible)(isa ?X DoomTypeExceptions)) 

(implies (genls ?X SolidTangibleThing)(isa ?X DoomTypePossibles)) 

(implies (and (isa ?ARG1 DoomTypePossibles) (not (isa ?ARG1 DoomTypeExceptions))) (isa ?ARG1 DoomTypeCreatable))

(exceptWhen (isa ?ARG1 DoomTypeExceptions)(typeGenlss DoomTypePossibles DoomTypeCreatable))


(implies (isa ?ARG1 (CollectionDifferenceFn DoomTypePossibles DoomTypeExceptions)) (isa ?ARG1 DoomTypeCreatable))

(and 
(isa SymmetricAnatomicalPartType DoomTypeExceptions)
(isa UniqueAnatomicalPartType DoomTypeExceptions)
(isa AnimalBodyPart DoomTypeExceptions)
(isa Currency-US DoomTypeExceptions)
(isa Chairman DoomTypeExceptions)
(isa Star DoomTypeExceptions)
(isa FamousHuman DoomTypeExceptions))
(isa Buttocks DoomTypeExceptions)
(isa Well DoomTypeExceptions)
(isa Healthy DoomTypeExceptions)
(isa EllipticalRegion DoomTypeExceptions)
(genls Doom-IdEntity DoomTypeExceptions)
(implies (genls ?X Doom-IdEntity) (ist DoomMappingMt (isa ?X DoomTypeExceptions)))
(isa DoorInABuilding DoomTypeExceptions)
(isa DoorwayCovering DoomTypeExceptions)
(isa TallPhysicalBuild DoomTypeExceptions)
(isa Block DoomTypeExceptions)
(isa CoverOfIBO DoomTypeExceptions)
(isa Cover-Protector DoomTypeExceptions)
(isa FreeSheet DoomTypeExceptions)


(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (doom:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist DoomMappingMt (doom:possibleCycTerms ?INST ?COL)))
  

(doom-assert-now '(#$typeGenls #$NonessentialAminoAcidType #$DoomTypeExceptions) *MAPPINGMT* '(:DIRECTION :FORWARD))
(isa Terrorist DoomTypeExceptions)


(doom-assert-now '(#$isa #$DoomTypeCreatable #$FacetingCollectionType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$isa #$DoomTypeExceptions #$FacetingCollectionType) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;FacetingCollectionType
  
(implies   
 (and 
;;;; (genls ?COL DoomTypePossibles)
 (isa ?GOOD DoomTypeCreatable) 
 (isa ?BAD DoomTypeExceptions)
 (genls ?COL ?GOOD) 
 (unknownSentence (genls ?COL ?BAD)))
(meetsSomeConstraintOfTopicType ?COL #$DoomFoundInLocations-TopicType))  


(implies 
 (and 
  (doom:possibleCycTerms ?INST ?COL)
  (meetsSomeConstraintOfTopicType ?COL #$DoomFoundInLocations-TopicType))
 (ist DoomMappingMt (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))

(implies 
 (and 
  (doom:possibleCycTerms ?INST ?COL)
  (meetsSomeConstraintOfTopicType ?COL #$DoomWorldSpawn-TopicType))
 (ist DoomMappingMt (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
	 

Mt : DoomMappingMt
Direction : Forward
(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (genls ?COL Person-SupportingFurniture) 
   (doom:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist DoomMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (genls ?COL Decoration) 
   (doom:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist DoomMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
(implies 
  (and 
   (definiteDescriptions ?AREA ?TEXT) 
   (doom:parsesContextually ?TEXT DoomWorldSpawn-TopicType ?CYCL)) 
  (futureAssertion ?AREA ?TEXT ?CYCL))
  

GeneralLivingAreaFurnitureMC Person-SupportingFurniture Decoration


(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (genls ?COL GeneralLivingAreaFurnitureMC) 
   (doom:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist DoomMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
	 
(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (doom:relationCandidateExistsAll objectFoundInLocation ?COL BPVLocation) 
   (doom:denotationMapper ?TEXT ?STRING ?COL)) 
  (ist DoomMappingMt 
   (relationExistsInstance objectFoundInLocation ?COL ?INST)))

(find-or-create-constant "doom:relationCandidateExistsAll")
(doom-assert-now '(#$isa #$doom:relationCandidateExistsAll #$TernaryPredicate) *MappingMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg1Isa #$doom:relationCandidateExistsAll #$Predicate) *MappingMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg2Isa #$doom:relationCandidateExistsAll #$Collection) *MappingMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg3Isa #$doom:relationCandidateExistsAll #$Collection) *MappingMt* '(:DIRECTION :FORWARD))



DoomTypeCreatable

;;(templateTopicGenls DoomFoundInLocations-TopicType DoomTypeCreatable)
(find-or-create-constant "DoomWorldSpawn-TopicType")
(doom-assert-now '(#$isa #$DoomWorldSpawn-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$titleForFormulaTemplateType-String #$DoomWorldSpawn-TopicType "Topic template for constraining worldspawning") *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$DoomWorldSpawn-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$templateTopicGenls #$DoomWorldSpawn-TopicType #$Doom-IdEntity) *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "DoomFoundInLocations-TopicType")
(doom-assert-now '(#$isa #$DoomFoundInLocations-TopicType #$FormulaTemplateTopicType) *UVMT* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$isa #$DoomFoundInLocations-TopicType #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$coextensionalSetOrCollections #$DoomFoundInLocations-TopicType #$DoomWorldSpawn-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$titleForFormulaTemplateType-String #$DoomFoundInLocations-TopicType "Topic template for constraining objects in rooms") *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$formulaTemplateTypeHasTopicType #$IndividualTerrainArtifactTemplate #$DoomFoundInLocations-TopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$templateTopicGenls #$DoomFoundInLocations-TopicType #$DoomTypeCreatable) *MAPPINGMT* '(:DIRECTION :FORWARD))

 


(implies 
  (and 
   (isa ?INST BPVLocation) 
   (definiteDescriptions ?INST ?TEXT) 
   (doom:denotationMapper ?TEXT ?STRING ?COL)
   (meetsSomeConstraintOfTopicType ?COL #$DoomFoundInLocations-TopicType))
  (ist DoomMappingMt 
   (relationExistsInstance spatialThingTypeFoundInLocation ?COL ?INST)))
	 



(implies (genls ?X BPVAgent) (ist DoomMappingMt (isa ?X DoomTypeExceptions)))
)
;;;; EllipticalRegion 

;;(isa Channel-BodyOfWaterLink DoomTypeExceptions)
;; spatialThingTypeFoundInLocation Decoration 

AllEnglishLexicalMicrotheoryPSC
AllEnglishTemplateMt
(define pasc (x) (parse-a-question-completely x #$AllEnglishLexicalMicrotheoryPSC))
(pasc "who do you love?")




(find-or-create-constant "DoomTypeTooGeneral")
(doom-assert-now '(#$isa #$DoomTypeTooGeneral #$CollectionType) *UVMT*)
(doom-assert-now '(#$disjointWith #$DoomTypeTooGeneral #$DoomTypeCreatable) *MAPPINGMT*)
;;;;(doom-assert-now '(#$implies (#$coextensionalSetOrCollections ?COL ?HOW) (#$isa ?COL #$DoomTypeCreatable)) *MAPPINGMT*)
(doom-assert-now '(#$comment #$DoomTypeTooGeneral "If something is an instance of #$DoomTypeTooGeneral then it may not be spawned)") *UVMT*)

(doom-assert-now '(#$coextensionalSetOrCollections (#$CollectionDifferenceFn #$DoomType #$DoomTypeTooGeneral) #$DoomTypeCreatable) *UVMT*)

(doom-assert-now '(#$typeGenls #$DoomTypeCreatable (#$CollectionDifferenceFn #$DoomType #$DoomTypeTooGeneral) ) *UVMT*)
(doom-assert-now '(#$collectionConventionMt #$DoomTypeCreatable *MAPPINGMT*) *UVMT*)
(doom-assert-now '(#$collectionConventionMt #$DoomTypeTooGeneral *MAPPINGMT*) *UVMT*)
(doom-assert-now '(#$collectionConventionMt #$DoomType *MAPPINGMT*) *UVMT*)


(find-or-create-constant "doomViolatesSomeConstraintIsa")
(doom-assert-now '(#$isa #$doomViolatesSomeConstraintIsa #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$doomViolatesSomeConstraintIsa "The all instances of :ARG1 violates the constraint in :ARG2") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "doomViolatesSomeGenlsConstraint")
(doom-assert-now '(#$isa #$doomViolatesSomeConstraintGenl #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$doomViolatesSomeConstraintGenl "The all specs of :ARG1 violates the constraint in :ARG2") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "doomViolatesSomeConstraint")
(doom-assert-now '(#$isa #$doomViolatesSomeConstraint #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$doomViolatesSomeConstraint "The :ARG1 violates the constraint in :ARG2") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "doomExceedsSomeConstraintIsa")
(doom-assert-now '(#$isa #$doomExceedsSomeConstraintIsa #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$doomExceedsSomeConstraintIsa "When something is an instance of :ARG1 exceeds the constraint in :ARG2 and is a prefered example") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "doomExceedsSomeGenlsConstraint")
(doom-assert-now '(#$isa #$doomExceedsSomeConstraintGenl #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$doomExceedsSomeConstraintGenl "When something is an instance of :ARG1 exceeds the constraint in :ARG2 and is a prefered example") *MAPPINGMT* '(:DIRECTION :FORWARD))
(find-or-create-constant "doomExceedsSomeConstraint")
(doom-assert-now '(#$isa #$doomExceedsSomeConstraint #$BinaryPredicate) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$doomExceedsSomeConstraint "The :ARG1 exceeds the constraint in :ARG2 and is a prefered example") *MAPPINGMT* '(:DIRECTION :FORWARD))


(doom-assert-now '(#$arg1Isa #$doomViolatesSomeConstraintIsa #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg1Isa #$doomViolatesSomeConstraintGenl #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg1Isa #$doomViolatesSomeConstraint #$Thing) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg1Isa #$doomExceedsSomeConstraintIsa #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg1Isa #$doomExceedsSomeConstraintGenl #$Collection) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg1Isa #$doomExceedsSomeConstraint #$Thing) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg2Isa #$doomViolatesSomeConstraintIsa #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg2Isa #$doomViolatesSomeConstraintGenl #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg2Isa #$doomViolatesSomeConstraint #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg2Isa #$doomExceedsSomeConstraintIsa #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg2Isa #$doomExceedsSomeConstraintGenl #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$arg2Isa #$doomExceedsSomeConstraint #$FormulaTemplateTopicType) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$completeExtentDecidable #$doomViolatesSomeConstraint) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$completeExtentDecidable #$doomExceedsSomeConstraint) *MAPPINGMT* '(:DIRECTION :FORWARD))


(doom-assert-now '(#$negationPreds #$doomExceedsSomeConstraint #$doomViolatesSomeConstraint) *MAPPINGMT* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$implies (#$doomExceedsSomeConstraint ?P #$DoomWorldSpawn-TopicType)(#$meetsSomeConstraintOfTopicType ?P ?TT)) *MAPPINGMT*)
(doom-assert-now '(#$implies (#$and (#$doomExceedsSomeConstraintIsa ?W ?TT)(#$isa ?P ?W)) (#$doomExceedsSomeConstraint ?P ?TT)) *MAPPINGMT*)

;;(doom-assert-now '(#$implies (#$coextensionalSetOrCollections ?P ?W) (#$doomExceedsSomeConstraint ?P #$DoomWorldSpawn-TopicType)) '*MappingMt*)

(doom-assert-now '(#$implies (#$not (#$doomViolatesSomeConstraint ?P ?TT))(#$meetsSomeConstraintOfTopicType ?P ?TT)) *MAPPINGMT*)
(doom-assert-now '(#$implies (#$doomExceedsSomeConstraint ?P #$DoomWorldSpawn-TopicType) (#$meetsSomeConstraintOfTopicType ?P ?TT)) *MAPPINGMT*)
(doom-assert-now '(#$implies (#$and (#$doomExceedsSomeConstraintGenl ?W ?TT)(#$coextensionalSetOrCollections ?P ?W)) (#$doomExceedsSomeConstraint ?P ?TT)) *MAPPINGMT*)
;;(doom-assert-now '(#$implies (#$doomViolatesSomeConstraintIsa ?COL ?ANYWHERE) (#$ist *MAPPINGMT* (#$typeGenls ?COL #$DoomTypeExceptions))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$implies (#$doomViolatesSomeConstraintGenl ?COL ?ANYWHERE) (#$ist *MAPPINGMT* (#$coextensionalSetOrCollections ?COL #$DoomTypeExceptions))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$implies (#$doomViolatesSomeConstraint ?COL ?ANYWHERE) (#$ist *MAPPINGMT* (#$isa ?COL #$DoomTypeExceptions ))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$implies (#$and (#$coextensionalSetOrCollections ?C ?U)(#$doomViolatesSomeConstraint ?U)) (#$doomViolatesSomeConstraint ?C)) *MAPPINGMT*)
;;(doom-assert-now '(#$implies (#$and (#$doomViolatesSomeConstraintIsa ?Super ?TT)(#$isa ?Sub ?Super))(#$doomViolatesSomeConstraint ?Sub ?TT)) *MAPPINGMT*)
;;(doom-assert-now '(#$implies (#$and (#$doomViolatesSomeConstraintGenl ?Super ?TT)(#$coextensionalSetOrCollections ?Sub ?Super))(#$doomViolatesSomeConstraint ?Sub ?TT)) *MAPPINGMT*)
;;(doom-assert-now '(#$implies (#$and (#$doomViolatesSomeConstraint ?U)(#$coextensionalSetOrCollections ?C ?U)) (#$not (#$doomExceedsSomeConstraint ?C))) *MAPPINGMT* '(:DIRECTION :FORWARD))
;;(cyc-unassert '(#$implies (#$doomViolatesSomeConstraintIsa ?COL ?ANYWHERE) (#$typeGenls #$DoomTypeExceptions ?COL)) *MAPPINGMT*)
;;(cyc-unassert '(#$implies (#$doomViolatesSomeConstraintIsa ?COL ?ANYWHERE) (#$coextensionalSetOrCollections #$DoomTypeExceptions ?COL)) *MAPPINGMT*)
;;(cyc-unassert '(#$implies (#$doomViolatesSomeConstraint ?COL ?ANYWHERE) (#$isa ?COL #$DoomTypeExceptions )) *MAPPINGMT* )

(find-or-create-constant "doomSpawnClass")
(doom-assert-now '(#$isa #$doomSpawnClass #$TernaryPredicate) *UVMT*)
(doom-assert-now '(#$comment #$doomSpawnClass "(#$doomSpawnClass ?Arg1 ?Arg2 ?P) will return #$isa or #$coextensionalSetOrCollections or some other mostly legal reation between the two arguments") *UVMT*)
(doom-assert-now '(#$isa #$doomSpawnClass #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
(doom-assert-now '(#$arity #$doomSpawnClass 3) *UVMT*)
(doom-assert-now '(#$arg1Isa #$doomSpawnClass #$Thing) *UVMT*)
(doom-assert-now '(#$arg2Genl #$doomSpawnClass #$ThreeDimensionalThing) *UVMT*)
(doom-assert-now '(#$arg2Genl #$doomSpawnClass #$SomethingExisting) *UVMT*)
(doom-assert-now '(#$arg3Isa #$doomSpawnClass #$BinaryPredicate) *UVMT*)
(doom-assert-now '(#$conceptuallyRelated #$doomSpawnClass #$doomViolatesSomeConstraint) *UVMT*)
(inference-removal-module :removal-spawnPredicate-bound-bound-unbound
 '(:sense :pos 
	:predicate #$doomSpawnClass 
	:required-pattern (#$doomSpawnClass :fully-bound :fully-bound :not-fully-bound) 
	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$doomSpawnClass (:bind value-1) (:bind value-2) :anything) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call spawnPredicate :input)
	:output-construct-pattern (#$doomSpawnClass (:value value-1) (:value value-2) :input)))
(register-solely-specific-removal-module-predicate #$doomSpawnClass)
(define spawnPredicate (vs)
 (clet ((v1 (car vs)) (v2 (nth 1 vs)))
 (cand (cq (list #$isa v2 #$Collection)) (null (cq (list #$doomViolatesSomeConstraint v2)))
 (ret (list (fif (cq (list #$isa v1 #$Collection)) #$coextensionalSetOrCollections #$isa))))))


(doom-assert-now '(#$ksTermString #$MarinePersonnel "ensign") *MappingMt*)
(doom-assert-now '(#$ksTermString #$Leader "Commander") *MappingMt*)
(doom-assert-now '(#$ksTermString #$SignTheDisplay "sign") *MappingMt*)
(doom-assert-now '(#$ksTermString #$Elevator "Turbolift") *MappingMt*)
(doom-assert-now '(#$ksTermString #$Bottle "bottle") *MappingMt*)


(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$VehiclePart #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$Relation #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$Microtheory #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$Individual #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$DurativeEventType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$Doom-IdEntity #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$DisjointCollectionType #$DoomWorldSpawn-TopicType) '*MappingMt*) 
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$DerivedMeasurableQuantityType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$CollectionType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$BPVItemType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$BPVAgentType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$AnimalActivity #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$Action #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeIsaConstraint #$AccomplishmentType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$VolkswagenTransporterCar #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Specification #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$SocialOccurrence #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Sick #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$ShoeUppers #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Pipeline #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Organization #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Nucleotide #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Movement-TranslationProcess #$DoomWorldSpawn-TopicType) '*MappingMt*) 
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Microtheory #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Intangible #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Hair-Stuff #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Group #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$GatlingGun-GAU4 #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Event-Organized #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Event #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Doom-IdEntity #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Criterion #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Bay #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$BPVItemType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$BPVAgentType #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$AnimalActivity #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$AminoAcid #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Action #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeGenlsConstraint #$Abnormal-Unusual #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint (#$MeatFn #$EdibleFish) #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint (#$GroupFn #$Fish) #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$TrunkOfCar #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Stimulant #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$SpottedPattern #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$QuarterCoin-US #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$OneDollarBill-US #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Ocean #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Issue #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$InformationTransferEvent #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Gold #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Flag #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Fisherman #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$EthnicGroupOfBlacks #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Diligent #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Criterion #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Communicating #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Command #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$BootingAComputer #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Black-HairColor #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$Amphetamine #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$AfricanAmericanPerson #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomViolatesSomeConstraint #$AcademicQuarter #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomExceedsSomeIsaConstraint #$ChromaticColor #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomExceedsSomeGenlsConstraint #$PartOfBuilding #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomExceedsSomeConstraint #$AirplaneCockpit #$DoomWorldSpawn-TopicType) '*MappingMt*)
(doom-assert-now '(#$doomExceedsSomeConstraint #$AirplaneCargoBay #$DoomWorldSpawn-TopicType) '*MappingMt*)
;;(doom-assert-now '(#$doomExceedsSomeConstraint #$DrinkingGlass) '*MappingMt* '(:DIRECTION :FORWARD))
|#
