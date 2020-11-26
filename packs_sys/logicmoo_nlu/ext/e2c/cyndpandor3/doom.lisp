;;;; use (load "cynd/doom.lisp")
(in-package "CYC")
(print "loading doom.lisp...1 of 5.")
;;;;(load "cynd/cycdoom/kepatches.lisp")
(force-output)
;;;;==================================================
;;;; MICROTHEORY SETUP
;;;;==================================================
(defparameter *UVMt* (find-constant "UniversalVocabularyMt"))
(defparameter *BASEKB* (find-constant "BaseKB"))
(defparameter *VocabularyMt* (find-or-create-constant "DoomVocabularyMt"))

(csetq *REQUIRE-CASE-INSENSITIVE-NAME-UNIQUENESS* NIL)
(csetq *THE-CYCLIST* #$CycAdministrator)
(csetq *KE-PURPOSE* #$GeneralCycKE)
(csetq *PPH-TERSE-MODE?* NIL 
           *PPH-USE-BULLETED-LISTS?* T 
           *PPH-REPLACE-BULLETED-LIST-TAGS?* NIL 
           *PARAPHRASE-PRECISION* NIL 
           *PPH-TERSE-MT-SCOPE?* T 
           *PPH-MAXIMIZE-LINKS?* T 
           *PPH-LINK-ARG0?* T 
           *PPH-CONSOLIDATE-OUTPUT-LIST-FOR-NART?* NIL 
           *PPH-USE-SMART-VARIABLE-REPLACEMENT?* T 
           *PPH-DEMERIT-CUTOFF*
             (POSITIVE-INFINITY  )
           *PPH-USE-TITLE-CAPITALIZATION?* NIL 
           *PPH-USE-INDEXICAL-DATES?* NIL 
           *PPH-ADDRESSEE* :UNKNOWN 
           *PPH-SPEAKER* :UNKNOWN 
           *PPH-QUANTIFY-VARS?* T ) 

(ke-assert-now `(#$isa ,*VocabularyMt*  #$Microtheory) *UVMt*)

(define d3 () (load "cynd/doom.lisp"))

(define transd3 () 
    (clet ((ts-file (TRANSLATE-FILE "CynD" "cynd/doom.lisp"))
        (fout (OPEN-TEXT "cynd/doom.trans" :output)))
        (SHOW-TRANS-SUBL-FILE ts-file fout)(close FOUT)
        (C-BACKEND-OUTPUT-FILE-AND-HEADER-FILE ts-file "cynd/doom.c")
        (ret ts-file)))


(define doom-assert-now (sent &rest flags)
     (csetq flags (flatten (cons flags *VocabularyMt*)))
     (clet  ((mt (car (member-if #'MT? flags)))(cmd `(ke-assert-now ,(list 'quote sent) ,mt
        ,(fif (member :default flags) :default :monotonic) ,(fif (member :BACKWARD flags) :backward :forward))))
      (punless (eval  cmd)
        (format t "~&~s~& ; ERROR: ~s~&" cmd (HL-EXPLANATION-OF-WHY-NOT-WFF sent Mt))
        (force-output))))

;;;; Collections
;;(doom-assert-now `(#$isa ,(find-or-create-constant "DoomConstant") #$Collection) *VocabularyMt*)
;;(doom-assert-now `(#$collectionConventionMt #$DoomConstant ,*VocabularyMt*) *BASEKB*)
;;(doom-assert-now `(#$isa ,(find-or-create-constant "DoomCollection") #$CollectionType) *VocabularyMt*)
;;(doom-assert-now `(#$isa #$DoomCollection #$DoomConstant) *VocabularyMt*)
;;(doom-assert-now `(#$genls #$DoomCollection #$DoomConstant) *VocabularyMt*)


;;;; SubL
(define doom-var (doomname) 
    (ret (intern (cconcatenate "*" (string-upcase doomname) "*") :CYC)))

;;;; Globals
(defparameter *first-made* ())
(define doom-cyc (name) 
  (clet ((const (multiple-value-list  (find-or-create-constant name))))
   (csetq *first-made* (cdr  const))
   ;;(doom-assert-now `(#$isa ,const #$DoomConstant) *VocabularyMt*)
   (csetq *first-made* (car  const))
    (ret (car const))))
                          
(define doom-col (doomname) 
    (clet ((const  (doom-cyc (cconcatenate "Doom"  (string-proper doomname)))))
;;;     (doom-assert-now `(#$isa ,const #$FirstOrderCollection) *VocabularyMt*)
    (pwhen *first-made*
     (doom-assert-now `(#$isa ,const #$Collection) *VocabularyMt*)
     (doom-assert-now `(#$collectionConventionMt ,const ,*VocabularyMt*) *BASEKB*))
    ;; (clet ((type  (doom-cyc (cconcatenate "Doom"  (string-proper doomname) "Type"))))(doom-assert-now `(#$isa ,type #$CollectionType) *VocabularyMt*)(doom-assert-now `(#$collectionConventionMt ,type ,*VocabularyMt*) *BASEKB*)(doom-assert-now `(#$typeGenls ,type  ,const) *VocabularyMt*)))
  (ret const)))

(doom-assert-now `(#$isa ,(doom-col "Microtheory")  #$MicrotheoryType) *BASEKB*)
(doom-assert-now `(#$genls ,(doom-col "Microtheory") #$MicrotheorySpindle)*BASEKB*)
;;(doom-assert-now `(#$isa ,(doom-col "Microtheory")  #$AtemporalNecessarilyEssentialCollectionType) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-col "Microtheory")   "#$MicrotheoryType that requires a #$genlMt-Vocabulary to #$DoomVocabularyMt ") *VocabularyMt*)
;;(doom-assert-now `(#$implies (#$isa ?Mt ,(doom-col "Microtheory")) (#$genlMt-Vocabulary ?Mt ,*VocabularyMt*)) *BASEKB* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$isa ,*VocabularyMt*  #$VocabularyMicrotheory) *VocabularyMt*)
(define doom-Mt (doomname) 
  (clet ((const (doom-cyc (cconcatenate "Doom" (string-proper doomname) "Mt"))))
    (doom-assert-now `(#$isa ,const #$Microtheory) *BASEKB*)
     (doom-assert-now `(#$genlMt-Vocabulary ,const ,*VocabularyMt*) *VocabularyMt*)
    (doom-assert-now `(#$isa ,const #$DoomMicrotheory) *VocabularyMt*)
    (set (doom-var (cconcatenate doomname "Mt")) const)
  (ret const)))
(doom-assert-now `(#$isa ,(doom-Mt "Mapping") #$SKSISourceDescriptionMicrotheory) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-Mt "Creation")  #$TheoryMicrotheory) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-Mt "StaticState")  ,(doom-col "StateMicrotheory") ) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-Mt "Planning")  ,(doom-col "AgentMicrotheory") ) *VocabularyMt*)
(doom-assert-now `(#$comment ,*VocabularyMt*  "#$VocabularyMicrotheory that indicates the asserions required for canonicialization of #$DoomMicrotheory  microtheories.") *VocabularyMt*)
(doom-assert-now `(#$comment ,*MappingMt*  "#$MappingMicrotheory that indicates the asserions required for DoomFacet doom string.") *VocabularyMt*)
(doom-assert-now `(#$comment ,*CreationMt*  "#$DoomMicrotheory that has forwarding rules needed to deduce (build outward) obvious parts of the world.") *VocabularyMt*)
(doom-assert-now `(#$comment ,*StaticStateMt*  "#$DoomStateMicrotheory Head of all so that shared static doom properties states can reside.") *VocabularyMt*)
(doom-assert-now `(#$comment ,*PlanningMt*  "#$DoomAgentMicrotheory that is visible to all Doom3 agents so that shared doom actions can reside.") *VocabularyMt*)
(doom-assert-now `(#$genlMt ,*VocabularyMt*  ,*UVMt*) *BASEKB*)
(doom-assert-now `(#$genlMt ,*CreationMt* ,*MappingMt*) *BASEKB*)
(doom-assert-now `(#$genlMt ,*MappingMt*  #$KnowledgeSourceManagementMt) *BASEKB*)
(doom-assert-now `(#$genlMt ,*StaticStateMt*  #$SituationPredicateRepresentationMt) *BASEKB*)
(doom-assert-now `(#$genlMt ,*StaticStateMt*  #$BPV-DefinitionalMt) *BASEKB*)
(doom-assert-now `(#$genlMt ,*PlanningMt* #$ScriptsTestingMt ) *BASEKB*)
(doom-assert-now `(#$genlMt ,*PlanningMt*  ,*StaticStateMt*) *BASEKB*)
(doom-assert-now `(#$genlMt-Vocabulary ,*PlanningMt* #$PlanningVocabularyMt ) *BASEKB*)

(doom-assert-now `(#$genls ,(doom-col "AgentMicrotheory") #$MicrotheorySpindle)*VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "AgentMicrotheory") ,(doom-col "Microtheory") ) *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "AgentMicrotheory")  #$CollectorMicrotheory ) *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "AgentMicrotheory")  #$PlanningDomainMicrotheory ) *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "AgentMicrotheory")  #$PlanSupplementMicrotheory ) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-col "AgentMicrotheory")   "#$MicrotheoryType that requires a #$genlMt to #$DoomPlanningMt") *VocabularyMt*)
(doom-assert-now `(#$implies (#$isa ?Mt ,(doom-col "AgentMicrotheory")) (#$mtSpindle ?Mt  ,*PlanningMt* ?Mt)) *BASEKB* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$comment ,(doom-cyc "AgentMicrotheory") "This #$MicrotheorySpindle represents the existing divergence in standard ways of 
viewing the Doom world, especially according to Doom world view by agent.  The mtSpindleMember #$Microtheory s each represent one such viewing system.")*VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-Mt "CurrentAgent")  ,(doom-col "AgentMicrotheory")  ) *VocabularyMt*)
(doom-assert-now `(#$comment ,*CurrentAgentMt*  "#$DoomAgentMicrotheory sample used for testing.") *VocabularyMt*)

(doom-assert-now `(#$genls ,(doom-col "StateMicrotheory") #$MicrotheorySpindle)*VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "StateMicrotheory") ,(doom-col "Microtheory") ) *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "StateMicrotheory")  #$CollectorMicrotheory ) *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "StateMicrotheory")  #$DataMicrotheory ) *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "StateMicrotheory")  #$TemporalMicrotheory ) *VocabularyMt*)

(doom-assert-now `(#$genls ,(doom-col "StateMicrotheory")  #$HypotheticalContext ) *VocabularyMt*)
;;(doom-assert-now `(#$genls ,(doom-col "StateMicrotheory")  #$DynamicInfoSource ) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-col "StateMicrotheory")   "A collection of #$DoomMicrotheory of dynamic world situations in a time scope.
Each instance of DoomStateMicrotheory that is uniquely linked to a state generated by Doom3 System.
Each instance is a #$HypotheticalContext that holds the assertions that will be distributed by the hypothesis manager 
to form the content of the world in which the system is thinking about. Thus an instance may contain a pair of mutually incompatible 
assertions in situations where two incompatible hypotheses will be constructed.") *VocabularyMt*)

(doom-assert-now `(#$implies (#$isa ?Mt ,(doom-col "StateMicrotheory")) (#$mtSpindle ?Mt  ,*StaticStateMt* ?Mt)) *BASEKB* '(:DIRECTION :FORWARD))

(doom-assert-now `(#$isa ,(doom-Mt "CurrentState")  ,(doom-col "StateMicrotheory")  ) *VocabularyMt*)
(doom-assert-now `(#$comment ,*CurrentStateMt* "Dynamic #$DoomStateMicrotheory for instance level data in computerese mainly.") *VocabularyMt*)
(doom-assert-now `(#$genlMt ,(doom-Mt "CurrentState")   ,(doom-Mt "Creation")  ) *VocabularyMt*)

;;;;(doom-assert-now `(#$genlMt  (#$MtSpace #$CurrentWorldDataCollectorMt-NonHomocentric (#$MtTimeDimFn #$Now)) ,*VocabularyMt*) *BASEKB*)
;;;;(doom-assert-now `(#$genlMt ,(doom-Mt "Mapping")  #$CurrentWorldDataCollectorMt-NonHomocentric) *BASEKB*)
;;;;(doom-assert-now `(#$genlMt #$CurrentWorldDataCollectorMt-NonHomocentric ,*VocabularyMt*) *BASEKB*)

;;;;==================================================
;;;; COLLECTIONS SETUP
;;;;==================================================
(define doom-trans (name doomname) 
    (clet ((const (doom-cyc (cconcatenate "doom" (string-proper name) "To" (string-proper doomname)))))
    (pwhen *first-made* 
      (doom-assert-now `(#$isa ,const #$BinaryPredicate ) *VocabularyMt*)      
      (doom-assert-now `(#$predicateConventionMt ,const  ,*MappingMt*) *BASEKB*)
      (doom-assert-now `(#$conceptuallyRelated  ,const #$synonymousExternalConcept ) *VocabularyMt*)      
      ;;;;(doom-assert-now `(#$genlPreds ,const #$ksTermString ) *VocabularyMt*)
      ;;;;(doom-assert-now `(#$genlPreds ,const #$programStrings ) *VocabularyMt*)
      )
      (ret const)))

;;;; Relations
(doom-assert-now `(#$genls ,(doom-col "Property") #$Relation) *MappingMt*)
(define doom-pred (doomname) 
    (clet ((const  (doom-cyc (cconcatenate "doom" (string-proper doomname)))))
    (pwhen *first-made* 
      (doom-assert-now `(#$isa ,const #$Predicate ) *UVMt*)
      ;;(doom-assert-now `(#$isa ,const #$DoomProperty ) *VocabularyMt*)
     ;;(doom-assert-now `(,(doom-trans "Property" "Name") ,const ,doomname) *MappingMt*))
     )
  (ret const)))
;;;; Collections

(define doom-ItemType (doomname) 
    (clet ((const  (doom-cyc (cconcatenate "Doom-" (string-proper doomname)))))
    (pwhen *first-made* 
        (doom-assert-now `(#$isa ,const #$Collection ) *VocabularyMt*)
        (doom-assert-now `(#$genls ,const #$CollectionType ) *VocabularyMt*)
       ;;(doom-assert-now `(#$defaultDefiningMtForBinaryPredicateOnType ,const ,*CurrentStateMt*) *BASEKB*)
       (doom-assert-now `(#$collectionConventionMt ,const ,*VocabularyMt*) *BASEKB*)
       ;;(doom-assert-now `(#$isa ,const #$ExistenceDependentCollection) *VocabularyMt*)
       (doom-assert-now `(#$genls ,const #$BPVItemType) *VocabularyMt*)
       (doom-assert-now `(,(doom-trans "ItemType" "Name") ,const ,doomname) *MappingMt*))
  (ret const)))
;;;; Collections

(define doom-class (doomname) 
    (clet ((const  (doom-cyc (cconcatenate "Doom_"   (string-proper doomname)))))
    (pwhen *first-made* 
       (doom-assert-now `(#$isa ,const #$Collection) *VocabularyMt*)
       (doom-assert-now `(#$collectionConventionMt ,const ,*StaticStateMt*) *BASEKB*)
       ;;(doom-assert-now `(#$genls ,const #$ExistenceDependentCollection ) *VocabularyMt*)
       ;;(doom-assert-now `(#$genls ,const #$FacetInstanceCollection) *VocabularyMt*)
       (doom-assert-now `(#$isa ,const #$BPVItemType) *VocabularyMt*)
       (doom-assert-now `(,(doom-trans "Class" "Name") ,const ,doomname) *MappingMt*))
  (ret const)))

;;;; Transient                                 
(define doom-t (doomname) 
    (clet ((const  (doom-cyc (cconcatenate "DOOM_" (string-proper doomname)))))
    (pwhen *first-made*      
      (doom-assert-now `(#$definingMt ,const ,*StaticStateMt*) *VocabularyMt*)
      (doom-assert-now `(#$isa ,const #$Individual) *VocabularyMt*)
      (doom-assert-now `(,(doom-trans "Entity" "Name") ,const ,doomname) *MappingMt*)
      (doom-assert-now `(#$termDependsOn ,const ,(doom-trans "Entity" "Name")) *CurrentStateMt*))
  (ret const)))
;;;; Transient
(define doom-func (doomname) 
    (clet ((const  (doom-cyc (cconcatenate "Doom" (string-proper doomname) "Fn"))))
    ;;(pwhen *first-made* 
        ;;(doom-assert-now `(,(doom-trans "Method" "Name") ,const ,doomname) *MappingMt*)
        (doom-assert-now `(#$isa ,const  #$Function-Denotational ) *UVMt*)
  (ret const)))
                                     
;;==================================================
;; Collections BPVItem/BPVAgent/BPVLocation/BPVArtifact
;;==================================================
;;(doom-assert-now '(#$implies (#$and (#$isa ?INST #$BPVItem)(#$unknownSentence (#$doomClasses ?INST ?ANYTEXT))(#$isa ?INST ?SPEC)(#$genls ?SPEC ?COL) (#$doomClassToDoomFacet ?COL  (#$DoomFacetFn ?ParamEvalObject "classname"))) (#$ist *MappingMt* (#$doomClasses ?INST ?ParamEvalObject)))*MappingMt*)
(doom-assert-now '(#$genls #$BPVAgentType #$ExistingObjectType)   *UVMt* '(:DIRECTION :FORWARD))
#|
(doom-assert-now '(#$typeGenls #$BPVAgentType #$BPVAgent)*UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$typeGenls #$BPVAgentType #$BPVAgent)*UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVAgentType #$FirstOrderCollection)   *UVMt* '(:DIRECTION :FORWARD))
(find-or-create-constant "BPVAgent")
(find-or-create-constant "BPVAgentType")
(doom-assert-now `(#$isa #$BPVAgent #$Collection)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVAgent #$BPVItem)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVAgentType #$FirstOrderCollection)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$isa #$BPVAgentType #$CollectionType)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$isa #$BPVAgent #$ExistingObjectType)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVAgent #$IndividualAgent)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVAgent #$IntelligentAgent)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVAgent #$Agent-Generic)   *UVMt* '(:DIRECTION :FORWARD))
;;(#$mtAgentType #$BPVAgentType
(find-or-create-constant "BPVArtifact")
(find-or-create-constant "BPVArtifactType")
(doom-assert-now `(#$isa #$BPVArtifact #$Collection)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVArtifact #$BPVItem)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$isa #$BPVArtifactType #$CollectionType)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$isa #$BPVArtifactType #$FacetingCollectionType)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$typeGenls #$BPVArtifactType #$BPVArtifact)*UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$isa #$BPVArtifact #$ExistingObjectType)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVArtifact #$Artifact-NonAgentive)   *UVMt* '(:DIRECTION :FORWARD))
|#
(doom-assert-now '(#$genls #$BPVArtifact #$InanimateObject)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVArtifact #$Artifact)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVArtifactType #$ExistingObjectType)   *UVMt* '(:DIRECTION :FORWARD))
(find-or-create-constant "BPVLocation")
(find-or-create-constant "BPVLocationType")
(doom-assert-now `(#$isa #$BPVLocation #$Collection)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$isa #$BPVLocationType #$CollectionType)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$typeGenls #$BPVLocationType #$BPVLocation)*UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$facets-Generic #$BPVLocation  #$BPVLocationType)*UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVLocationType #$FacetingCollectionType)   *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$genls #$BPVLocation #$BPVItem)   *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$genls #$BPVLocation #$SpaceRegion-Empirical)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVLocation #$Place-NonAgent)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$BPVLocation  #$PartiallyTangible)   *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$BPVLocation "This is used to mark locations in the doom therefore a specialization of #$BPVItem
 #$Place-NonAgent and #$SpaceRegion-Empirical.")  *VocabularyMt* '(:DIRECTION :FORWARD))

;;(doom-assert-now '(#$implies (#$isa ?X #$BPVLocationType)(#$genls ?X #$DoomLocation))  *VocabularyMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$implies (#$isa ?X #$BPVArtifactType)(#$genls ?X #$DoomArtifact))  *VocabularyMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$implies (#$isa ?X #$BPVAgentType)(#$genls ?X #$DoomAgent))  *VocabularyMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$implies (#$and (#$no #$Animal ?COL) (#$genls ?COL #$SolidTangibleThing)) (#$doom:relationCandidateExistsAll #$objectFoundInLocation ?COL #$BPVLocation)) *MappingMt* '(:DIRECTION :FORWARD))
;;;; DISJOINTS OF TYPES
;;(doom-assert-now '(#$disjointWith #$BPVAgent #$BPVArtifact)   *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$disjointWith #$BPVLocation #$BPVArtifact)   *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$disjointWith #$BPVAgent #$BPVLocation)   *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$comment #$BPVItem "#$Individual inside the doom engine that is already spawned in the world.") *UVMt*)

;;(doom-assert-now '(#$isa #$BPVItem #$ExistingObjectType)   *UVMt* '(:DIRECTION :FORWARD))
;;==================================================
;; Function: Angle2Fn
;;==================================================
(find-or-create-constant "Angle2Fn")
(doom-assert-now `(#$isa ,(doom-cyc "Angle2Fn")  #$BinaryFunction) *UVMt*)
(doom-assert-now `(#$isa ,(doom-cyc "Angle2Fn")  #$UnreifiableFunction) *UVMt*)
(doom-assert-now `(#$arg1Isa ,(doom-cyc "Angle2Fn")  #$AngularDistance) *UVMt*)
(doom-assert-now `(#$arg2Isa ,(doom-cyc "Angle2Fn")  #$AngularDistance) *UVMt*)
(doom-assert-now `(#$resultIsa ,(doom-cyc "Angle2Fn")  #$OrientationVector) *UVMt*)
(doom-assert-now `(#$resultIsa ,(doom-cyc "Angle2Fn")  #$UnitVectorInterval) *UVMt*)
(doom-assert-now `(#$comment ,(doom-cyc "Angle2Fn")  "A #$OrientationVector (two-axis) that can translate to a 3-D ray when applied to a point.") *UVMt*)
                                         
;;==================================================
;; Function: Point3Fn
;;==================================================
(find-or-create-constant "Point3Fn")
;;(fi-kill #$Point3Fn)
(doom-assert-now '(#$isa #$Point3Fn #$TernaryFunction) *UVMt*)
(doom-assert-now `(#$isa #$Point3Fn #$UnreifiableFunction) *UVMt*)
(doom-assert-now `(#$quotedIsa #$Point3Fn #$ForwardReifiableCycLFunctor)  *CreationMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$conceptuallyRelated #$Point3Fn #$Point4Fn)  *UVMt*)
(doom-assert-now `(#$comment #$Point3Fn "A #$Point in a Cartesian coordinate system. (See also #$Point4Fn)") *UVMt*)
;;;;(doom-assert-now `(#$argsIsa #$Point3Fn #$RealNumber) *UVMt*)
;;;;(doom-assert-now `(#$argsIsa #$Point3Fn #$Distance) *UVMt*)
(doom-assert-now `(#$resultIsa #$Point3Fn #$Point) *UVMt*)
;;(doom-assert-now `(#$resultIsa #$Point3Fn #$GeographicalPlace-0D) *UVMt*)
;;(doom-assert-now `(#$resultIsa #$Point3Fn #$SpatialThing-NonSituational) *UVMt*)
;;(doom-assert-now `(#$resultIsa #$Point3Fn #$PositionVector) *UVMt*)
;;(doom-assert-now `(#$resultIsa #$Point3Fn #$Individual)  *UVMt* '(:DIRECTION :FORWARD))
;;;;(doom-assert-now `(#$resultIsa #$Point3Fn #$GeographicalPoint-Intangible-FixedLocation) *UVMt*)

;;==================================================
;; Function: BoundsOfDirectionFn
;;==================================================
(find-or-create-constant "BoundsOfDirectionFn")
;; (fi-kill #$BoundsOfDirectionFn)
(doom-assert-now `(#$isa #$BoundsOfDirectionFn #$BinaryFunction)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$isa #$BoundsOfDirectionFn #$IndividualDenotingFunction)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$isa #$BoundsOfDirectionFn #$ReifiableFunction)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$quotedIsa #$BoundsOfDirectionFn #$ForwardReifiableCycLFunctor)   *CreationMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$isa #$BoundsOfDirectionFn #$TotalFunction)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$comment #$BoundsOfDirectionFn "This is a NART (BoundsOfDirectionFn ?R1 ?D) to produce a instance of a #$Portal (#$Path-Generic) that creates a one-way portal in dirrection ?D from  ?R1 to some location")  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$BoundsOfDirectionFn "Every #$BPVLocation may have any of 18+ #$BoundsOfDirectionFn but implicitly must have 6 based on moving allong the 3 axis at a cone-shaped forty-five degree tollerance (*-Directly). at least specified")  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$arg1Isa #$BoundsOfDirectionFn #$SpatialThing-NonSituational)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$arg2Isa #$BoundsOfDirectionFn #$UnitVector-Precise)  *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$resultIsa #$BoundsOfDirectionFn #$Border)  *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now `(#$resultIsa #$BoundsOfDirectionFn #$Path-Simple)  *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$resultIsa #$BoundsOfDirectionFn #$BoundaryMarker) *VocabularyMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now `(#$resultIsa #$BoundsOfDirectionFn #$Individual)  *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now `(#$resultIsa #$BoundsOfDirectionFn #$SpatialThing-NonSituational) *UVMt*)


;;==================================================
;; Collections DoomItem/DoomAgent/DoomLocation/DoomArtifact
;;==================================================
(doom-assert-now `(#$genls ,(doom-col "Agent") #$BPVAgent)  *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "Location") #$BPVLocation)  *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "Artifact") #$BPVArtifact)  *VocabularyMt*)
(doom-assert-now `(#$coExtensional ,(doom-col "Item") #$BPVItem)  *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "Agent") ,(doom-col "Item"))  *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "Location") ,(doom-col "Item"))  *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "Artifact") #$Artifact)  *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-col "Class") #$FacetInstanceCollection)  *VocabularyMt*)
(doom-assert-now `(#$defaultDefiningMtForInstances ,(doom-col "Item") ,*CurrentStateMt*)  *VocabularyMt*)
(doom-assert-now `(#$defaultDefiningMtForInstances ,(doom-col "Location") ,*StaticStateMt*)  *VocabularyMt*)
(doom-assert-now `(#$defaultDefiningMtForInstances ,(doom-col "Agent") ,*StaticStateMt*)  *VocabularyMt*)
(doom-assert-now `(#$defaultDefiningMtForInstances ,(doom-col "Artifact") ,*StaticStateMt*)  *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-col "Agent")  "An Instanced #$BPVAgent") *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-col "Location") "An Instanced #$BPVLocation") *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-col "Artifact") "An Instanced #$BPVArtifact") *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-col "Item") "An Instanced #$BPVItem") *VocabularyMt*)

(define defdoom-function (name visibleMt)
    (clet ((const (doom-func name))
        (SubLName (intern (cconcatenate "CYC-DOOM-EVAL-" (STRING-UPCASE name)) :CYC)))
    (DEFINE-EVALUATION-DEFN `,SubLName (&rest body) (ret (doom-eval (list :eval `,const body))))
    ;;;;(doom-assert-now `(#$isa ,const #$TermMacroFunction) *VocabularyMt*) ;;;;(expansion DistanceBetweenZipCodesFn (SubstituteFormulaArgFn 1 (StringToRealNumberFn (GetFromSimpleHTTPSource "127.0.0.1" 8080 "getDistance" "fromZip" :ARG1 "toZip" :ARG2)) (Mile ?X))).
    (doom-assert-now `(#$isa ,const #$EvaluatableFunction) *VocabularyMt*)  ;;Function-Denotational
    (doom-assert-now `(#$evaluationDefn ,const (#$SubLQuoteFn ,SubLName)) visibleMt)
    (doom-assert-now `(#$comment ,const  ,(format nil "This was generated by defdoom-function and evals ~s with ~s" const SubLName)))
    (ret const)))
(define req-string (sym)
   (pcond
      ((null sym) (ret "NIL"))
      ((stringp sym) (ret sym)) 
      ((symbolp sym) (ret (cconcatenate "-" (symbol-name sym))))
      ((packagep sym) (ret (package-name sym)))
      ((consp sym))(ret (mapcar #'req-string sym))
      (t (ret (write-to-string sym)))))
(define to-req-string (list) 
    (ret (apply #'cconcatenate "" (mapcar #'req-string  (flatten (list list))))))

 ;;;;  (doom-assert-now `(#$afterAdding ,(doom-trans "Property" "Name") (#$SubLQuoteFn DOOM-AFTER-ADDING-PropertyTOSTRING)) *CurrentStateMt*)
  ;;;; (doom-assert-now `(#$afterRemoving ,(doom-trans "Property" "Name") (#$SubLQuoteFn DOOM-AFTER-REMOVING-PropertyTOSTRING)) *CurrentStateMt*)
  ;;;; (DEFINE-AFTER-ADDING  DOOM-AFTER-ADDING-PropertyTOSTRING (str asrt) (ret (doom-eval (list :add str (assertion-el-formula asrt) (assertion-el-Mt asrt)))))
  ;;;; (DEFINE-AFTER-REMOVING  DOOM-AFTER-REMOVING-PropertyTOSTRING (str asrt) (ret (doom-eval (list :rem str (assertion-el-formula asrt) (assertion-el-Mt asrt)))))
 ;;;;   (fi-reassert '(#$doomPropertyToName #$doomPropertyToName PropertyToName) *MappingMt* :FORWARD)

(print "loading doom.lisp...2 of 5.")
(force-output)
;;(define DOOM-CLASSTODoomFacet-EVAL (&rest r))
;;(define DOOM-ITEMTODoomFacet-EVAL (mode truth  &rest r)(ret (print (list r '(2 3)) )))
;;(define DOOM-ITEMTODoomFacet-EVAL (mode truth  &rest r)(ret (print `((#$doomItemToDoomFacet #$isa #$isa)(#$doomItemToDoomFacet #$genls #$isa)))))

(defdoom-function "Eval"  *VocabularyMt*)
(doom-assert-now '(#$isa #$DoomEvalFn #$UnaryFunction) *UVMt*)
;;(doom-assert-now '(#$isa #$DoomEvalFn #$PartialFunction) *UVMt*)												  
;;(doom-assert-now '(#$isa #$DoomEvalFn #$TermMacroFunction) *UVMt*)		 
;;(doom-assert-now '(#$isa #$DoomEvalFn #$IndeterminateTermDenotingFunction) *UVMt*)  
;;(doom-assert-now '(#$comment #$DoomEvalFn "Is ") *UVMt*)  

;;==================================================
;; Collection DoomItem
;;==================================================
(find-or-create-constant "DoomItemFn")
(find-or-create-constant "doomItemToName")
(doom-assert-now '(#$isa #$DoomItemFn #$InvertibleFunction) *VocabularyMt*)
(doom-assert-now `(#$isa #$doomItemToName #$FunctionalPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa #$doomItemToName #$BinaryPredicate) *VocabularyMt*)
;;(doom-assert-now `(#$genlFunctions #$DoomItemFn #$doomItemToNamedFn)  *VocabularyMt*)
(doom-assert-now `(#$genls ,(doom-col "Item") #$BPVItem) *VocabularyMt*)
(doom-assert-now `(#$isa #$DoomItemFn #$ReifiableFunction) *VocabularyMt*)
(doom-assert-now `(#$isa #$DoomItemFn #$TermMacroFunction) *VocabularyMt*)
(doom-assert-now '(#$isa #$DoomItemFn #$UnaryFunction) *VocabularyMt*)
(doom-assert-now `(#$resultIsa #$DoomItemFn #$DoomItem) *VocabularyMt*)

(doom-assert-now '(#$isa #$doomItemToName #$RelationalNounSlot) #$GeneralLexiconMt ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$arg1Isa #$doomItemToName #$DoomItem) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$arg2Isa #$doomItemToName #$CharacterString) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$comment #$doomItemToName "(#$doomItemToName ITEM STRING) identifies STRING as the name of the doom ITEM.") *VocabularyMt*)
(doom-assert-now '(#$genTemplate #$doomItemToName (#$ConcatenatePhrasesFn (#$BestNLPhraseOfStringFn "the doom itemname for") (#$TermParaphraseFn-NP :ARG1) (#$PhraseFormFn #$presentTense-Generic (#$HeadWordOfPhraseFn (#$BestVerbFormForSubjectFn #$Be-TheWord (#$NthPhraseFn 2)))) (#$TermParaphraseFn-NP (#$StringMentionFn :ARG2)))) #$EnglishParaphraseMt ':DEFAULT ':FORWARD)
(doom-assert-now '(#$multiWordStringDenotesArgInReln (#$TheList "doom" "item") #$Name-TheWord #$CountNoun #$doomItemToName 2) #$EnglishMt ':DEFAULT ':FORWARD)
(doom-assert-now `(#$expansion #$DoomItemFn (#$InstanceWithRelationToFn #$DoomItem #$doomItemToName :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now `(#$expansion #$DoomItemFn (#$InstanceNamedFn #$DoomItem :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now `(#$functionCorrespondingPredicate-Canonical #$DoomItemFn #$doomItemToName 1) *MappingMt* ':DEFAULT ':FORWARD)
;;(doom-assert-now '(#$doomFacetToPredValue (#$DoomFacetFn "name" ?STRING) #$nameString (#$DoomClassFn ?STRING)) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$DoomItemFn #$CharacterString) *VocabularyMt*)
(doom-assert-now `(#$isa #$DoomItemFn #$PartialFunction) *VocabularyMt*)
(doom-assert-now `(#$quotedIsa #$DoomItemFn #$ForwardReifiableCycLFunctor)  *CreationMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$comment #$DoomItemFn 
"(#$DoomItemFn ?STRING) denotes the instance of #$DoomItem named STRING. 
See also #$doomItemToName (which is the #$functionCorrespondingPredicate for #$DoomItemFn).<br>
(#$equals (#$DoomItemFn ?STRING)(#$InstanceWithRelationToFn #$DoomItem (#$DoomPropertyFn \"name\") ?STRING)
(#$equals (#$DoomItemFn ?STRING)(#$InstancNamedFn #$DoomItem ?STRING))<br>
(#$doomInstance ?ITEM (#$DoomFacetFn \"name\" ?STRING)<br>
(#$isa (#$DoomItemFn ?STRING) (#$DoomFacetFn \"name\" ?STRING))<br>
(#$equals (#$DoomItemFn ?STRING)(#$InstancedNameFn #$DoomItem ?STRING))") *VocabularyMt*)
(doom-assert-now '(#$equiv (#$doomItemToName (#$DoomItemFn ?NAME) ?NAME) (#$isa (#$DoomItemFn ?NAME) #$DoomItem)) *MappingMt*)
(doom-assert-now '(#$implies (#$isa ?W #$DoomItem)(#$doomItemToName ?W (#$ConstantNamedFn ?W))) *MappingMt*)

;;==================================================
;; Collection DoomClass
;;==================================================
(find-or-create-constant "DoomClassFn")
(doom-assert-now `(#$isa #$DoomClassFn #$TermMacroFunction) *VocabularyMt*)
(doom-assert-now '(#$isa #$DoomClassFn #$CollectionDenotingFunction) *VocabularyMt*)
(doom-assert-now '(#$isa #$DoomClassFn #$UnaryFunction) *VocabularyMt*)
(doom-assert-now `(#$genlFunctions #$DoomClassFn #$CollectionNamedFn)  *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-col "Class") #$FacetingCollectionType) *MappingMt*)
(doom-assert-now `(#$genls ,(doom-col "Class") #$BPVItemType) *VocabularyMt*)
(doom-assert-now `(#$resultIsa #$DoomClassFn #$DoomClass) *VocabularyMt*)
(doom-assert-now `(#$resultGenl #$DoomClassFn #$BPVItem) *VocabularyMt*)

(doom-assert-now `(#$isa ,(doom-trans "Class" "Name") #$StrictlyFunctionalSlot) *UVMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$isa #$doomClassToName #$RelationalNounSlot) #$GeneralLexiconMt ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$arg1Isa #$doomClassToName #$DoomClass) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$arg2Isa #$doomClassToName #$CharacterString) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$comment #$doomClassToName "(#$doomClassToName CLASS STRING) identifies STRING as the name of the doom CLASS.") *VocabularyMt*)
(doom-assert-now '(#$genTemplate #$doomClassToName (#$ConcatenatePhrasesFn (#$BestNLPhraseOfStringFn "the doom classname for") (#$TermParaphraseFn-NP :ARG1) (#$PhraseFormFn #$presentTense-Generic (#$HeadWordOfPhraseFn (#$BestVerbFormForSubjectFn #$Be-TheWord (#$NthPhraseFn 2)))) (#$TermParaphraseFn-NP (#$StringMentionFn :ARG2)))) #$EnglishParaphraseMt ':DEFAULT ':FORWARD)
(doom-assert-now '(#$strictlyFunctionalInArgs #$doomClassToName 1) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$multiWordStringDenotesArgInReln (#$TheList "doom" "class") #$Name-TheWord #$CountNoun #$doomClassToName 2) #$EnglishMt ':DEFAULT ':FORWARD)
(doom-assert-now `(#$expansion #$DoomClassFn (#$InstanceWithRelationToFn #$DoomClass #$doomClassToName :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now `(#$functionCorrespondingPredicate-Canonical #$DoomClassFn #$doomClassToName 1) *MappingMt* ':DEFAULT ':FORWARD)
(doom-assert-now '(#$doomClassToName (#$DoomClassFn ?NAME) ?NAME) *MappingMt*)
;;(doom-assert-now '(#$doomFacetToPredValue (#$DoomFacetFn "classname" ?STRING) #$isa (#$DoomClassFn ?STRING)) *MappingMt*)
(doom-assert-now `(#$arg1Isa #$DoomClassFn #$CharacterString) *VocabularyMt*)
(doom-assert-now `(#$isa #$DoomClassFn #$ReifiableFunction) *VocabularyMt*)
(doom-assert-now `(#$isa #$DoomClassFn #$PartialFunction) *VocabularyMt*)
(doom-assert-now `(#$quotedIsa #$DoomClassFn #$ForwardReifiableCycLFunctor)  *CreationMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$comment #$DoomClassFn "(#$DoomClassFn ?STRING) denotes the same instance of #$DoomClass named STRING. 
See also #$doomClassToName which is the #$functionCorrespondingPredicate for #$DoomClassFn. 
Also (#$doomClassToFacet ?CLASS (#$DoomFacetFn \"classname\" ?STRING)") *VocabularyMt*)
(doom-assert-now '(#$equiv (#$doomClassToName (#$DoomClassFn ?NAME) ?NAME) (#$isa (#$DoomClassFn ?NAME) #$DoomClass)) *MappingMt*)

;;==================================================
;; Collection DoomProperty
;;==================================================
(find-or-create-constant "DoomPropertyFn")
(doom-assert-now `(#$isa #$DoomPropertyFn #$TermMacroFunction) *VocabularyMt*)
(doom-assert-now '(#$isa #$DoomPropertyFn #$PredicateDenotingFunction) *VocabularyMt*)
(doom-assert-now '(#$isa #$DoomPropertyFn #$UnaryFunction) *VocabularyMt*)
;;(doom-assert-now `(#$genlFunctions #$DoomPropertyFn #$PredicateNamedFn)  *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-col "Property") #$FacetingCollectionType) *MappingMt*)
(doom-assert-now `(#$genls ,(doom-col "Property") #$FunctionalSlot) *VocabularyMt*)
(doom-assert-now `(#$resultIsa #$DoomPropertyFn #$DoomProperty) *VocabularyMt*)

(doom-assert-now `(#$isa ,(doom-trans "Property" "Name") #$StrictlyFunctionalSlot) *UVMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$isa #$doomPropertyToName #$RelationalNounSlot) #$GeneralLexiconMt ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$arg1Isa #$doomPropertyToName #$DoomProperty) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$arg2Isa #$doomPropertyToName #$CharacterString) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$comment #$doomPropertyToName "(#$doomPropertyToName PROPERTY STRING) identifies STRING as the name of the doom PROPERTY.") *VocabularyMt*)
(doom-assert-now '(#$genTemplate #$doomPropertyToName (#$ConcatenatePhrasesFn (#$BestNLPhraseOfStringFn "the doom propertyname for") (#$TermParaphraseFn-NP :ARG1) (#$PhraseFormFn #$presentTense-Generic (#$HeadWordOfPhraseFn (#$BestVerbFormForSubjectFn #$Be-TheWord (#$NthPhraseFn 2)))) (#$TermParaphraseFn-NP (#$StringMentionFn :ARG2)))) #$EnglishParaphraseMt ':DEFAULT ':FORWARD)
(doom-assert-now '(#$strictlyFunctionalInArgs #$doomPropertyToName 1) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now '(#$multiWordStringDenotesArgInReln (#$TheList "doom" "property") #$Name-TheWord #$CountNoun #$doomPropertyToName 2) #$EnglishMt ':DEFAULT ':FORWARD)
(doom-assert-now `(#$expansion #$DoomPropertyFn (#$PredicateNamedFn #$DoomProperty :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
(doom-assert-now `(#$functionCorrespondingPredicate-Canonical #$DoomPropertyFn #$doomPropertyToName 1) *MappingMt* ':DEFAULT ':FORWARD)
(doom-assert-now '(#$doomPropertyToName (#$DoomPropertyFn ?NAME) ?NAME) *MappingMt*)
(doom-assert-now `(#$arg1Isa #$DoomPropertyFn #$CharacterString) *VocabularyMt*)
(doom-assert-now `(#$isa #$DoomPropertyFn #$ReifiableFunction) *VocabularyMt*)
(doom-assert-now `(#$isa #$DoomPropertyFn #$PartialFunction) *VocabularyMt*)
(doom-assert-now `(#$quotedIsa #$DoomPropertyFn #$ForwardReifiableCycLFunctor)  *CreationMt* '(:DIRECTION :FORWARD))
(doom-assert-now `(#$comment #$DoomPropertyFn "(#$DoomPropertyFn ?STRING) denotes the same instance of #$DoomProperty named STRING. 
See also #$doomPropertyToName which is the #$functionCorrespondingPredicate for #$DoomPropertyFn. 
Also (#$doomPropertyToFacet ?PROPERTY (#$DoomFacetFn \"propertyname\" ?STRING)") *VocabularyMt*)
(doom-assert-now '(#$equiv (#$doomPropertyToName (#$DoomPropertyFn ?NAME) ?NAME) (#$isa (#$DoomPropertyFn ?NAME) #$DoomProperty)) *MappingMt*)

;;(doom-assert-now '(#$doomFacetToPredValue (#$DoomFacetFn ?NAME ?STRING) (#$DoomPropertyFn ?NAME) (#$DoomFacetFn ?NAME ?STRING)) *MappingMt*)
;;(doom-assert-now '(#$genTemplate #$doomPropertyToName (#$ConcatenatePhrasesFn (#$BestNLPhraseOfStringFn "the doom Propertyname for") (#$TermParaphraseFn-NP :ARG1) (#$PhraseFormFn #$presentTense-Generic (#$HeadWordOfPhraseFn (#$BestVerbFormForSubjectFn #$Be-TheWord (#$NthPhraseFn 2)))) (#$TermParaphraseFn-NP (#$StringMentionFn :ARG2)))) #$EnglishParaphraseMt ':DEFAULT ':FORWARD)
;;(doom-assert-now '(#$strictlyFunctionalInArgs #$doomPropertyToName 1) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(doom-assert-now '(#$multiWordStringDenotesArgInReln (#$TheList "doom" "Property") #$Name-TheWord #$CountNoun #$doomPropertyToName 2) #$EnglishMt ':DEFAULT ':FORWARD)
;;(doom-assert-now `(#$expansion #$DoomPropertyFn (#$InstanceWithRelationToFn #$DoomProperty #$doomPropertyToName :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(doom-assert-now `(#$functionCorrespondingPredicate-Canonical #$DoomPropertyFn #$doomPropertyToName 1) *MappingMt* ':DEFAULT ':FORWARD)
;;(doom-assert-now '(#$doomFacetToPredValue (#$DoomFacetFn "Propertyname" ?STRING) (#$DoomPropertyFn ?STRING)) *MappingMt*)
;;(doom-assert-now `(#$isa #$DoomPropertyFn #$PartialFunction) *VocabularyMt*)
;;(doom-assert-now '(#$equiv (#$doomPropertyToName (#$DoomPropertyFn ?NAME . ?ARGS) ?NAME) (#$isa (#$DoomPropertyFn ?NAME . ?ARGS) #$DoomProperty)) *MappingMt*)
;;(doom-assert-now '(#$doomPropertyToFacet (#$DoomPropertyFn ?NAME)(#$DoomFacetFn "Propertyname" ?NAME)) *MappingMt*)
;;(doom-assert-now '(#$implies (#$doomPropertyToFacet ?Property (#$DoomFacetFn "Propertyname" ?NAME)) (#$and (#$isa (#$DoomPropertyFn ?NAME) #$DoomProperty)(#$doomPropertyToName (#$DoomPropertyFn ?NAME) ?NAME)(#$genlsPred ?Property (#$DoomPropertyFn ?NAME)))) *MappingMt*)

;;==================================================
;; Function: DoomFacetFn
;;==================================================
(doom-assert-now `(#$isa ,(doom-trans "Facet" "Thing") #$StrictlyFunctionalSlot) *UVMt* ':MONOTONIC ':FORWARD)
(find-or-create-constant "DoomFacetFn")
;;(fi-kill #$Point3Fn)
(doom-assert-now '(#$isa #$DoomFacetFn #$BinaryFunction) *UVMt*)
(doom-assert-now '(#$isa #$DoomFacetFn #$ReifiableFunction) *UVMt*)												  
(doom-assert-now '(#$isa #$DoomFacetFn #$PartialFunction) *UVMt*)												  
(doom-assert-now '(#$isa #$DoomFacetFn #$TermMacroFunction) *UVMt*)		 
(doom-assert-now '(#$isa #$DoomFacetFn #$IndeterminateTermDenotingFunction) *UVMt*)  
(doom-assert-now `(#$quotedIsa #$DoomFacetFn #$ForwardReifiableCycLFunctor)  *CreationMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$isa #$DoomFacetFn #$SubTopicFunction) *UVMt*)					  
;;(doom-assert-now '(#$isa #$DoomFacetFn #$CollectionDenotingFunction) *VocabularyMt*)
;;(doom-assert-now `(#$conceptuallyRelated #$DoomFacetFn #$JavaDoomFacetFn)  *VocabularyMt*)
;;(doom-assert-now `(#$genlFunctions #$DoomFacetFn #$CollectionThingdFn)  *VocabularyMt*)
(doom-assert-now `(#$comment #$DoomFacetFn "(#$DoomFacetFn ?STRING ?VALUE) denotes the instance of #$FirstOrderCollection containing instances of #$DoomItem with property ?STRING set to ?VALUE. 
See also #$doomFacetToPredValue which is the #$functionCorrespondingPredicate for #$DoomFacetFn.
(#$SubcollectionOfWithRelationToFn #$DoomItem (#$DoomPropertyFn :ARG1) (#$DoomEvalFn :ARG2))).
Used in (#$doom<Item/Class>ToFacet ?THING (#$DoomFacetFn ?STRING ?VALUE))") *VocabularyMt*)
(doom-assert-now '(#$arg1Isa #$DoomFacetFn #$CharacterString) *UVMt*)
(doom-assert-now '(#$arg2Isa #$DoomFacetFn #$Thing) *UVMt*)
(doom-assert-now '(#$resultIsa #$DoomFacetFn #$FirstOrderCollection) *UVMt*)
(doom-assert-now `(#$expansion #$DoomFacetFn (#$SubcollectionOfWithRelationToFn #$DoomItem (#$DoomPropertyFn :ARG1) (#$DoomEvalFn :ARG2))) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(doom-assert-now `(#$functionCorrespondingPredicate-Canonical #$DoomFacetFn #$doomFacetToPredValue 1) *MappingMt* ':DEFAULT ':FORWARD)
;;(doom-assert-now '(#$equals (#$DoomFacetFn ?THING)(#$DoomFacetFn "Facetthing" ?THING)) *MappingMt*)
 
;;(#$implies (#$isa ?ITEM ?CLASS) (#$isa (#$InstanceWithRelationToFn ?CLASS ?REL ?VALUE)(#$SubcollectionOfWithRelationToFn ?CLASS ?REL ?VALUE)))
;;(doom-assert-now `(#$isa (#$InstanceWithRelationToFn ?CLASS ?REL ?VALUE)(#$SubcollectionOfWithRelationToFn ?CLASS ?REL ?VALUE))  *VocabularyMt*)
;;(doom-assert-now `(#$isa (#$InstanceWithRelationFromFn ?CLASS ?REL ?VALUE)(#$SubcollectionOfWithRelationFromFn ?CLASS ?REL ?VALUE))  *VocabularyMt*)

;;(#$DoomFacetFn ?PROP EVAL) == (#$SubcollectionOfWithRelationToFn #$DoomItem (#$DoomPropertyFn ?PROP) ?VALUE)
;;(#$DoomItemFn EVAL) == (#$InstanceWithRelationToFn #$DoomItem (#$DoomPropertyFn ?PROP) ?VALUE)

                                                  
(print "loading doom.lisp...3 of 5.")
(force-output)

(define defdoom-pred (doomname Mt &optional (neg-reqs :anything) (pos-reqs :anything))
    ;;(fi-kill (doom-pred doomname))
    (clet ((const (doom-pred doomname))(preqs pos-reqs)(nreqs neg-reqs)(name (STRING-UPCASE doomname)))
        (doom-assert-now `(#$isa ,const  #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
        (doom-assert-now `(#$predicateConventionMt ,const  ,Mt) *BASEKB*)
        
        (pwhen (numberp preqs) 
            (csetq pos-reqs :anything)
            (cdotimes (n preqs) (csetq pos-reqs (cons :fully-bound pos-reqs))))
        (pwhen (numberp nreqs) 
            (csetq neg-reqs :anything)
            (cdotimes (n nreqs) (csetq neg-reqs (cons :fully-bound neg-reqs))))
     (clet (
        (RemovalPosProc (intern (cconcatenate "REMOVAL-" name "-POS" (to-req-string pos-reqs)) :KEYWORD))
        (RemovalBUProc (intern (cconcatenate "REMOVAL-" name "-POS-BOUND-FULLY-BOUND-ANYTHING") :KEYWORD))
        (RemovalNegProc (intern (cconcatenate "REMOVAL-" name "-NEG" (to-req-string neg-reqs)) :KEYWORD))
        (WrapperProc (intern (cconcatenate "DOOM-" name "-EVAL") :CYC))
        (AfterAdding (intern (cconcatenate "DOOM-AFTER-ADDING-" name) :CYC))
        (AfterRemoving (intern (cconcatenate "DOOM-AFTER-REMOVING-" name) :CYC)))
        (csetq pos-reqs (cons const pos-reqs))
        (csetq neg-reqs (cons const neg-reqs))         

    (inference-removal-module `,RemovalPosProc
     `(:sense :pos :predicate ,const  :cost-expression 0 :completeness :complete 
        :required-pattern :ANYTHING  ;; :required-pattern ,pos-reqs
        :output-generate-pattern (:call ,WrapperProc :pos :input)
        :example ,(write-to-string pos-reqs)  
        :documentation ,(cconcatenate "This was generated by defdoom-pred and works on modes " (write-to-string pos-reqs) ".")))
    (inference-removal-module `,RemovalNegProc
     `(:sense :neg :predicate ,const :cost-expression 0 :completeness :complete 
        :required-pattern :ANYTHING  ;; :required-pattern ,neg-reqs
        :output-generate-pattern (:call ,WrapperProc :neg :input)
        :example ,(cconcatenate "(#$not " (write-to-string neg-reqs) ")") 
        :documentation ,(cconcatenate "This was generated by defdoom-pred and works on modes (#$not " (write-to-string neg-reqs) ").")))

   (eval `(define ,WrapperProc (truth &rest args) (ret (doom-eval truth args))))

   (eval `(DEFINE-AFTER-ADDING ,AfterAdding (str asrt) (ret (doom-eval (list :add str (assertion-el-formula asrt) (assertion-el-Mt asrt))))))
   (eval `(DEFINE-AFTER-REMOVING ,AfterRemoving (str asrt) (ret (doom-eval (list :rem str (assertion-el-formula asrt) (assertion-el-Mt asrt))))))

   (doom-assert-now `(#$afterAdding ,const (#$SubLQuoteFn ,AfterAdding)) Mt)
   (doom-assert-now `(#$afterRemoving ,const (#$SubLQuoteFn ,AfterRemoving)) Mt)
   (doom-assert-now `(#$comment ,const  ,(cconcatenate "Generated by defdoom-pred for modes of " (write-to-string pos-reqs) " and (#$not " (write-to-string neg-reqs) ").")) *VocabularyMt*)
   (register-solely-specific-removal-module-predicate const)
   (ret const))))
        
                         
;;==================================================
;; PREDICATE #$doomEval
;;==================================================
(defdoom-pred "eval" *CurrentStateMt* 2)
(doom-assert-now `(#$isa #$doomEval #$FunctionalPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa #$doomEval  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomEval  "(#$doomEval ?Object ?FacetEval)") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomEval  2) *VocabularyMt*)
(doom-assert-now `(#$arg1QuotedIsa #$doomEval  #$SubLListOrAtom) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomEval  #$Thing) *VocabularyMt*)
(doom-assert-now `(#$functionCorrespondingPredicate #$DoomEvalFn #$doomEval 2) *VocabularyMt*)

(doom-assert-now `(#$doomEval ?X (#$DoomEvalFn ?X)) *VocabularyMt*)

(doom-assert-now `(#$genlFunctions #$DoomEvalFn #$IdentityFn) *VocabularyMt*)

;;==================================================
;; PREDICATE #$doomPropertyValue
;;==================================================
(defdoom-pred "propertyValue" *CurrentStateMt* 2 1)
(doom-assert-now `(#$isa #$doomPropertyValue #$TernaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomPropertyValue  "(#$doomPropertyValue ?NAME ?ARG ?VALUE)") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomPropertyValue  3) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomPropertyValue #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomPropertyValue #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg3Isa #$doomPropertyValue #$Thing) *VocabularyMt*)
;;==================================================
;; PREDICATE #$doomPropertyNext
;;==================================================
(defdoom-pred "propertyNext" *CurrentStateMt* 2 1)
(doom-assert-now `(#$isa #$doomPropertyNext #$TernaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomPropertyNext  "(#$doomPropertyNext ?NAME ?ARG ?NEXT)") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomPropertyNext  3) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomPropertyNext #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomPropertyNext #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg3Isa #$doomPropertyNext #$Thing) *VocabularyMt*)
;;==================================================
;; PREDICATE #$doomClassPropertyValue
;;==================================================
(defdoom-pred "classPropertyValue" *StaticStateMt* 2 1)
(doom-assert-now `(#$isa #$doomClassPropertyValue #$TernaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomClassPropertyValue  "(#$doomPropertyValue ?NAME ?ARG ?VALUE)") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomClassPropertyValue  3) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomClassPropertyValue #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomClassPropertyValue #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg3Isa #$doomClassPropertyValue #$Thing) *VocabularyMt*)

;;==================================================
;; PREDICATE #$doomInstance
;;==================================================
(doom-pred "Instance")
(doom-assert-now `(#$isa #$doomInstance  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomInstance  "(#$doomInstance ?Object ?FacetEval)") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomInstance  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomInstance  #$DoomItem) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomInstance  #$FirstOrderCollection) *VocabularyMt*)
;;'(doom-assert-now `(#$equiv (#$and  (#$doomItemToName ?ITEM ?NAME)(#$doomPropertyValue ?NAME ?PROP ?VALUE))(#$doomInstance (#$DoomItemFn ?NAME) (#$DoomFacetFn ?PROP ?VALUE)))  *VocabularyMt*)
;;'(doom-assert-now `(#$equiv (#$doomPropertyValue ?NAME ?PROP ?VALUE)(#$doomInstance (#$DoomItemFn ?NAME) (#$DoomFacetFn ?PROP ?VALUE)))  *VocabularyMt*);;(doom-assert-now `(#$completeExtentDecidable #$doomInstance *CurrentStateMt* '(:DIRECTION :FORWARD))

;;==================================================
;; INTERFACE MAPPING SETUP (classToFacet)
;;==================================================
(doom-pred "classToFacet")
(doom-assert-now `(#$isa #$doomClassToFacet  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomClassToFacet  "SpawnArgs Not Using Inheritance (#$doomClassToFacet ?Collection ?FacetEval)") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomClassToFacet  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomClassToFacet  #$DoomClass) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomClassToFacet  #$FirstOrderCollection) *VocabularyMt*)
(doom-assert-now `(#$transitiveViaArg #$doomClassToFacet #$genls 1) *MappingMt*)
(doom-assert-now `(#$genlPreds #$doomClassToFacet #$genls) *VocabularyMt*)
;;(doom-assert-now '(#$implies (#$doomClassToFacet ?CLASS (#$DoomFacetFn "classname" ?NAME)) 
;;    (#$and (#$isa (#$DoomClassFn ?NAME) #$DoomClass)(#$doomClassToName (#$DoomClassFn ?NAME) ?NAME)
;;          (#$genls ?CLASS (#$DoomClassFn ?NAME)))) *MappingMt*)
;;(doom-assert-now `(#$completeExtentDecidable #$doomClassToFacet *MappingMt* '(:DIRECTION :FORWARD))
#|
;;==================================================
;; INTERFACE MAPPING SETUP (classToFacetImplied)
;;==================================================
(doom-pred "classToFacetImplied" *MappingMt*)
(doom-assert-now `(#$isa #$doomClassToFacetImplied  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomClassToFacetImplied  "SpawnArgs Using Inheritance (#$doomClassToFacetImplied ?DoomClass ?FacetImpliedEval)") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomClassToFacetImplied  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomClassToFacetImplied  #$DoomClass) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomClassToFacetImplied  #$FirstOrderCollection) *VocabularyMt*)
(doom-assert-now `(#$transitiveViaArg #$doomClassToFacetImplied #$genls 1) *MappingMt*)
(doom-assert-now '(#$doomClassToFacetImplied (#$DoomClassFn ?NAME)(#$DoomFacetFn "classname" ?NAME)) *MappingMt*)
;;==================================================
;; INTERFACE MAPPING SETUP (collectionToFacet)
;;==================================================
(doom-pred "collectionToFacet" *CurrentStateMt* 2)
(doom-assert-now `(#$isa #$doomCollectionToFacet  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomCollectionToFacet "When a normal cyc collection is put into doom (#$doomCollectionToFacet ?Instance ?FacetEval) after adding will ensure that the ?FacetEval is reflected as true in the doom.") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomCollectionToFacet  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomCollectionToFacet  #$Collection) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomCollectionToFacet  #$FirstOrderCollection) *VocabularyMt*)
(doom-assert-now `(#$transitiveViaArg #$doomCollectionToFacet #$genls 1) *MappingMt*)
(doom-assert-now `(#$implies (#$doomCollectionToFacet ?COL (#$DoomFacetFn "classname" ?NAME))
    (#$coExtensional  (#$DoomClassFn ?NAME) ?COL))  *MappingMt*)
;;(doom-assert-now `(#$completeExtentDecidable #$doomCollectionToFacetImplied *MappingMt* '(:DIRECTION :FORWARD))
;;==================================================
;; INTERFACE MAPPING SETUP (collectionToFacetImplied)
;;==================================================
(doom-pred "collectionToFacetImplied" *MappingMt*)
(doom-assert-now `(#$isa #$doomCollectionToFacetImplied  #$BinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment #$doomCollectionToFacetImplied "When a normal cyc collection is put into doom (#$doomCollectionToFacetImplied ?Instance ?FacetEval) after adding will ensure that the ?FacetEval is reflected as true in the doom.") *VocabularyMt*)
(doom-assert-now `(#$arity #$doomCollectionToFacetImplied  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa #$doomCollectionToFacetImplied  #$Collection) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa #$doomCollectionToFacetImplied  #$FirstOrderCollection) *VocabularyMt*)
(doom-assert-now `(#$transitiveViaArg #$doomCollectionToFacetImplied #$genls 1) *MappingMt*)
(doom-assert-now `(#$implies (#$doomCollectionToFacetImplied ?COL (#$DoomFacetFn "classname" ?NAME))
    (#$genls  (#$DoomClassFn ?NAME) ?COL))  *MappingMt*)

|#
;;(doom-assert-now `(#$genlPreds #$doomCollectionToFacet #$genls) *VocabularyMt*)

;;(csetq *VocabularyMt*  *UVMt*)
(print "loading doom.lisp...4 of 5.")
(force-output)

#|
;;;;(clet (res) (ccatch :odd-result (cdolist (x `(4 2 1 0) (pif (oddp x) (throw :odd-result x)    (print x))))(pwhen res (print (cconcatenate (str res) " was odd!")))))
;;;;(define-evaluation-defn CYC-doom-eval-FN (el-list) (doom-eval el-list))
(define DOOM-EVAL-REMOVAL-EVAL (&rest all))
(define removal-andAlso-pos-ub (vvs) (clet ((retstuff  (qunify (car vvs)))) (fif retstuff (fif (qunify (cadr vvs)) retstuff NIL) NIL)))
(define removal-andAlso-pos-bu (vvs) (print (fif (vquery (car vvs)) (qunify (cadr vvs))  NIL)))
(define removal-andAlso-pos-bb (vvs) (cand (vquery (car vvs)) (vquery (cadr vvs))))
(define vquery (q1) (fif (eql q1 #$True) `(NIL) (clet ((rq (CYC-QUERY q1 #$EverythingPSC))) (fif rq rq (gquery q1)))))
(define bunify (q1 bl1) (mapcar #'SUBLIS bl1 (make-list (length bl1) q1)))
(define qunify (q1) (bunify q1 (vquery q1)))
(define removal-andAlso-pos-uu (vvs) (andquerylist (bunify vvs (vquery (car vvs)))))
(define removal-andAlso-pos-uu (vvs) (bunify vvs (vquery (car vvs))))
(define removal-andAlso-pos-uu (vvs) (bunify vvs (vquery (cons '#$and vvs))))
(define gquery (q1)  (CYC-QUERY q1 #$InferencePSC))
(define andquerylist (qlist) (mapcar #'andquery qlist))    
(define andquery (qlist) (bunify qlist (vquery (cons '#$and qlist))))
|#
;;;;;;(doom-assert-now `(#$genlMt ,*PlanningMt*  #$EverythingPSC) '*BASEKB*  '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC))

;;;;(#$isa (#$BoundsOfDirectionFn #$Area1010 #$East-Directly) #$Wall-GenericBarrier)
;;;;(,(doom-trans "Collection" "Classes")  (#$BoundsOfDirectionFn #$Area1010 #$East-Directly) #$Wall-GenericBarrier)
;;;;(knownSentence isa       (BoundsOfDirectionFn Area1000 North-Directly) OpenPortal) ?TRUTH)

;;;;(doom-assert-now `(#$implies (#$isa ?X ,(doom-ItemType "Item") ) (#$isa ?X #$SpatialThing-Localized))  *PlanningMt*  '(:DIRECTION :FORWARD))
;;(doom-assert-now `(#$isa ,(doom-t "cyc_bot_1") ,(doom-ItemType "idAI") )  *VocabularyMt*)
;;(doom-assert-now `(#$isa ,(doom-t "player1") ,(doom-ItemType "idPlayer") )  *VocabularyMt*)
;;(doom-assert-now `(#$isa ,(doom-t "cyc_bot_1") #$Agent-Generic)  *VocabularyMt*)
;;(doom-assert-now `(#$isa ,(doom-t "player1") #$Agent-Generic)  *VocabularyMt*)
(force-output)
#|
;;;;(doom-assert-now `(#$implies (#$and (#$isa ?OBJ #$Agent-Generic)(#$termStrings ?OBJ ?STRING)(,(doom-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(doom-assert-now `(#$implies (#$and (#$isa ?OBJ #$Agent-Generic)(#$termStrings ?OBJ ?STRING)(,(doom-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(doom-assert-now `(#$implies (#$and (#$isa ?OBJ #$Portal)(#$termStrings ?OBJ ?STRING)(,(doom-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(doom-assert-now `(#$implies (#$and (#$isa ?OBJ #$GeographicalRegion)(#$termStrings ?OBJ ?STRING)(,(doom-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(doom-assert-now `(#$implies (#$and (#$isa ?OBJ ,(doom-ItemType "Item") )(#$termStrings ?OBJ ?STRING)(,(doom-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
|#
#|(doom-assert-now '(#$implies (#$doomPropertyToFacet ?OBJ (#$DoomFacetFn "name" ?NAME)) 
   (#$and (#$isa (#$DoomPropertyFn ?NAME) #$DoomProperty)
          (#$doomPropertyToName (#$DoomPropertyFn ?NAME) ?NAME)
          (#$genlPreds ?OBJ (#$DoomPropertyFn ?NAME)))) *MappingMt*)|#
                
(define doom-subl (prename args body)
  (clet ((name prename)
          (namestr (symbol-name prename))
         (evalstr `(sl::define ,prename ,args ,@body)))
           (csetq name (join-strings  (mapcar #'string-proper (string-tokenize (string-DOWNCASE namestr) '(#\space #\-))) ""))
           (punless (string-equal (substring name 0 4) "doom") (csetq name (cconcatenate "Doom" (string-proper name))))
           (csetq name (find-or-create-constant (cconcatenate (string-DOWNCASE (substring name 0 1)) (substring name 1))))
           (doom-assert-now `(#$isa ,name #$FunctionalPredicate) *VocabularyMt*)
           (doom-assert-now `(#$programCode (#$OperatorFn ,namestr) (#$SubLEntityFn #$ComputerCode (#$SubLQuoteFn ,evalstr))) *VocabularyMt*)           
           (doom-assert-now `(#$programFunctionOperator ,name (#$OperatorFn ,namestr)) *VocabularyMt*)
           (eval evalstr)
           (ret name)))

(defmacro define-subl (prename args &rest body)
  (clet ((name (doom-subl prename args body))
         (sublsrc `(function (lambda ,args (ret (progn ,@body)))))
         (minargs  (ARGNAMES-FROM-ARGLIST args))
         (applyargs ())(rest ())
         (sublfn `(#$SubLQuoteFn ,prename))
         (applyfn2 `(function (lambda ,args (ret (progn ,@body)))))
         (applyfn `(function ,prename))
         (minarity (pcond ((position '&optional args))((position '&rest args))((position '&body args))(t ())))
         (commentstr ""))
           (pwhen (numberp minarity) 
                (csetq rest '?ARGS)
                (csetq minargs (SUBSEQ minargs 0 minarity)))
           (csetq minargs (mapcar #'make-el-var minargs))
           (csetq applyargs (append minargs (list `',rest)))
           (csetq variables (flatten (append minargs (list rest))))
           (csetq predargs (append minargs `((#$EvaluateSubLFn (#$ExpandSubLFn (,@variables) (apply ,applyfn2 ,@applyargs)))) rest))
           (csetq commentstr (format nil "subl function ~s -> ~s" `(apply ,applyfn ,@applyargs) (cons name (append minargs (list (make-el-var "EVALUATESUBLFN")) rest))))
           (format t "~&applyargs ~s " commentstr)(force-output)
           (doom-assert-now `(#$isa ,name #$Predicate) *VocabularyMt*)           
           (fif (numberp minarity)     
                  (progn 
                    (doom-assert-now `(#$isa ,name #$VariableArityPredicate) *VocabularyMt*)
                    (doom-assert-now `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (doom-assert-now `(#$arityMin ,name ,(1+ (length minargs))) *VocabularyMt*))
                  (progn 
                    (doom-assert-now `(#$isa ,name #$Predicate) *VocabularyMt*)           
                    (doom-assert-now `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (doom-assert-now `(#$arity ,name ,(length predargs)) *VocabularyMt*)))
           (doom-assert-now `(#$argsQuotedIsa ,name #$SubLSExpression) *VocabularyMt*)
           (doom-assert-now `(,name ,@predargs) *VocabularyMt*)
           (doom-assert-now `(#$comment ,name ,commentstr) *VocabularyMt*)))


(defmacro define-subl-p (prename args &rest body)
  (clet ((name (doom-subl prename args body))
         (minargs  (ARGNAMES-FROM-ARGLIST args))
         (applyfn `(function ,prename))
         (applyfn2 `(function (lambda ,args (ret (progn ,@body)))))
         (applyargs ())(rest ())
         (sublfn `(#$SubLQuoteFn ,prename))
         (minarity (pcond ((position '&optional args))((position '&rest args))((position '&body args))(t ())))
         (commentstr ""))
           (pwhen (numberp minarity) 
                (csetq rest '?ARGS)
                (csetq minargs (SUBSEQ minargs 0 minarity)))
           (csetq minargs (mapcar #'make-el-var minargs))
           (csetq applyargs (append minargs (list `',rest)))
           (csetq variables (flatten (append minargs (list rest))))
           (csetq predargs (append minargs rest))
           (csetq commentstr (format nil "subl predidate ~s -> ~s" `(apply ,applyfn ,@applyargs) (cons name predargs)))
           (format t "~&applyargs ~s " commentstr)(force-output) 
           (fif (numberp minarity)     
                  (progn 
                    (doom-assert-now `(#$isa ,name #$VariableArityPredicate) *VocabularyMt*)
                    (doom-assert-now `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (doom-assert-now `(#$arityMin ,name ,(1+ (length minargs))) *VocabularyMt*))
                  (progn 
                    (doom-assert-now `(#$isa ,name #$Predicate) *VocabularyMt*)           
                    (doom-assert-now `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (doom-assert-now `(#$arity ,name ,(length predargs)) *VocabularyMt*)))
           (doom-assert-now `(#$implies 
                   (#$trueSubL (#$ExpandSubLFn (,@variables) (apply ,applyfn2 ,@applyargs)))
                   (,name ,@predargs)) *VocabularyMt*)
           (doom-assert-now `(#$argsQuotedIsa ,name #$SubLSExpression) *VocabularyMt*)
           (doom-assert-now `(#$comment ,name ,commentstr) *VocabularyMt*)))


(define dfn (str) 
  (clet ((retval str))
    (pcond
       ((stringp str)
            (csetq str  (string-trim *WHITESPACE-CHARS* str))
            (pwhen (char= #\( (char str 0))
                 (ret (dfn (read-from-string str))))
            (pwhen (char= #\# (char str 0))
                 (pwhen (char= #\$ (char str 1))
                    (ret (find-or-create-constant (substring str 2))))                   
                 (ret (dfn (read-from-string str))))
            (csetq retval (count #\space str))
            (pwhen (= retval 3)
                 (ret (read-from-string (cconcatenate "(#$Point3Fn" + str ")"))))
            (pwhen (> retval 0)
                 (ret (read-from-string (cconcatenate "(" + str ")"))))
            (csetq retval (find-constant str))
            (pwhen retval (ret retval))
            (ret `(#$DoomItemFn ,str)))
        ((cor (fort-p str)(nart-p str) (el-variable-p str)(constant-p str)(hl-variable-p str)) (ret  str))
        ((cnot (consp str)) (ret str))
        ((member (car str) '(dfn quote)) (ret (dfn (second str)))) 
        ((function? (car str)) (ret str))
        ;;((cor (numberp str)(null str)(constant-p str))(ret str))
        ((cor (fort-p str) (el-variable-p str)(constant-p str)(hl-variable-p str)) (ret  str))
        (t (ret (cons (dfn (car str))(dfn (cdr str))))))))


(defmacro wec (vn exec) (ret `(progn (csetq ,vn ,exec) (length ,vn))))
(defmacro wqc (vn tmpl qry &optional (hmt *SMT*)) (ret `(wec ,vn (ask-template ',tmpl ',(dfn qry) ,hmt))))
(defvar *SMT* #$DoomCurrentStateMt)
                
(force-output)
(find-or-create-constant "Doom-ProvableSentence")
(doom-assert-now '(#$isa #$Doom-ProvableSentence #$Collection) *VocabularyMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$defnSufficient #$Doom-ProvableSentence (#$SubLQuoteFn ProvableSentence)) *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$Doom-ProvableSentence #$CycLSentence-Askable)  *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$genls #$Doom-ProvableSentence #$SubLSExpression)  *VocabularyMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$Doom-ProvableSentence "All sentences that can be proved")  *VocabularyMt* '(:DIRECTION :FORWARD))
(define ProvableSentence (v) (ret (cand (cq v ))))
(define cq (q &optional (hMt #$EverythingPSC))
  (pwhen (consp q)
     (clet ((cmd 
        `(cyc-query ,q ,hMt '(:backchain T)))
     (result (eval cmd)))
     (print (list cmd result))
     (ret result))))

(find-or-create-constant "Doom-UnprovableSentence")
(doom-assert-now '(#$isa #$Doom-UnprovableSentence #$Collection) *VocabularyMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$defnSufficient #$Doom-UnprovableSentence (#$SubLQuoteFn UnprovableSentence)) *UVMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$genls #$Doom-UnprovableSentence #$CycLSentence-Askable)  *UVMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$genls #$Doom-UnprovableSentence #$SubLSExpression)  *VocabularyMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$comment #$Doom-UnprovableSentence "All sentences that can fail to be proved")  *VocabularyMt* '(:DIRECTION :FORWARD))
(doom-assert-now '(#$disjointWith #$Doom-UnprovableSentence #$Doom-ProvableSentence) *UVMt* '(:DIRECTION :FORWARD))
(define UnprovableSentence (v) (ret (null (cq1 v ))))
(define cq1 (q &optional (hMt #$EverythingPSC))
  (pwhen (consp q)
     (clet ((cmd 
        `(cyc-query ,q ,hMt '(:backchain T :time 15 :number 1)))
     (result (eval cmd)))
     (print (list cmd result))
     (ret result))))


;;(define doom-eval (&rest all) (format t "~&~s~&" (cons 'doom-eval all))(ret `((#$doomPropertyValue 1 2 4)(#$doomPropertyValue 1 2 5))))

(defparameter *WorldVocabularyMt* *VocabularyMt*) ;; CreationMt
(defparameter *WorldStaticStateMt* *StaticStateMt*)
(defparameter *WorldCurrentStateMt* *CurrentStateMt*)

(print `(load "cynd/startrek.lisp"))

(print `(load "cynd/doom-bindings.lisp"))
(print `(load "cynd/startrek.lisp"))
(print `(load "cynd/doom-creation.lisp"))
(print `(load "cynd/doom-sksi.lisp"))
(print `(load "cynd/doom-methods.lisp"))
(force-output)


(defmacro doom-warn (&rest code)  (ret 
    `(with-error-handler #'(lambda () 
            (format t "~&ERROR ~s ~s ~&" *ERROR-MESSAGE* ',code)(force-output)
                        (throw :UNEVALUATABLE T) ) (sl::progn ,@code))))

(doom-assert-now '(#$equals #$SpiralStaircase (#$DoomClassFn "mud_twisty")) *MappingMt*)
(defparameter *doom-host* "10.10.10.193")
(defparameter *doom-port* 3691)
(define doom-eval (&rest outval) 
 (THROW-UNEVALUATABLE-ON-ERROR 
  (format t "~&SENDING ~s~&" (cons 'doom-eval outval))(force-output)  
  (ret (clet (retval (stream (doom-warn (OPEN-TCP-STREAM *DOOM-HOST* *DOOM-PORT*))))
      (prin1 outval stream)
      (terpri stream)(force-output stream)
      (csetq retval (doom-warn (read-line stream nil :EOF nil)))
      (format t "RECEIVE ~s" retval)(terpri)(force-output)
      ;;(csetq retval (doom-warn (read stream nil :EOF nil)))
     ;; (doom-warn (fif (equal 200 retval) (csetq retval (read stream nil :EOF nil))))
      (csetq retval (doom-warn (read-from-string retval :EOF nil)))
      (format t "~&AS ~s~&" retval)
      (close stream) retval))))
;;(read-line *STANDARD-INPUT* nil :EOF nil)

;;(doom-assert-now `(#$genlMt (#$MtSpace #$CurrentWorldDataCollectorMt-NonHomocentric (#$MtTimeDimFn #$Now)) ,*CurrentStateMt*)  *UVMt* '(:DIRECTION :FORWARD))
