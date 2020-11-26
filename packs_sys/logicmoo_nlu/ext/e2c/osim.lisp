#|
[root@titan ~]# df -h
Filesystem            Size  Used Avail Use% Mounted on
/dev/mapper/VolGroup00-LogVol00
                      428G  415G   13G  98% /
tmpfs                 3.9G  272K  3.9G   1% /dev/shm
/dev/sda1             190M   45M  136M  25% /boot
/dev/mapper/VolGroup01-LogVol00
                      330G  314G   16G  96% /mnt/oldsys
//enki/c$             932G  327G  606G  36% /mnt/enki
[root@titan ~]# cd /javaCyc

(load "cynd/osim.lisp")
|#

(csetq *SMT* #$EverythingPSC)
(define foc (string) (ret (find-or-create-constant string)))

;;;; use (load "cynd/osim.lisp")
(in-package "CYC")
(define FORCE-PRINT (string) 
 (print string) (force-output))

(force-print "loading sim.lisp...1 of 5.")
;;;;(load "cynd/kepatches.lisp")

;;;;==================================================
;;;; MICROTHEORY SETUP
;;;;==================================================
(defparameter *UVMt* (foc "UniversalVocabularyMt"))
(defparameter *BASEKB* (foc "BaseKB"))
(defparameter *VocabularyMt* (find-or-create-constant "SimVocabularyMt"))
(ke-assert-now `(#$isa ,*VocabularyMt*  #$Microtheory) *UVMt*)


;;; Possible values: T, NIL.  If NIL, require authentication before allowing
;;; modifications to the knowledge base.  If T, any user is allowed to
;;; modify the knowledge base.
(csetq *ALLOW-GUEST-TO-EDIT?* T)

;;; Possible values: The name of a constant representing a Cyclist.  This is the
;;; default Cyclist initially logged into the system.
(csetq *DEFAULT-CYCLIST-NAME* "CycAdministrator")

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

(define d3 () (load "cynd/osim.lisp"))

(define transd3 () 
    (clet ((ts-file (TRANSLATE-FILE "CynD" "cynd/osim.lisp"))
        (fout (OPEN-TEXT "cynd/osim.trans" :output)))
        (SHOW-TRANS-SUBL-FILE ts-file fout)(close FOUT)
        (C-BACKEND-OUTPUT-FILE-AND-HEADER-FILE ts-file "cynd/osim.c")
        (ret ts-file)))

(define transd3 () 
    (clet ((ts-file (TRANSLATE-FILE "CynD" "init/javatest.lisp"))
        (fout (OPEN-TEXT "javatest.trans" :output)))
        (SHOW-TRANS-SUBL-FILE ts-file fout)(close FOUT)
        (C-BACKEND-OUTPUT-FILE-AND-HEADER-FILE ts-file "javatest.c")
        (ret ts-file)))

(define Trans-JavaTest () 
    (clet ((ts-file (TRANSLATE-FILE "JavaTest" "init/javatest.lisp"))
        (fout (OPEN-TEXT "javatest.trans" :output)))
        (SHOW-TRANS-SUBL-FILE ts-file fout)(close FOUT)
        (JAVA-BACKEND-OUTPUT-FILE ts-file "javatest.java")
        (ret ts-file)))


(define sim-retract (sent &rest mts)
 (clet (askable template)
   (csetq mts (member-if #'mt? (flatten (cons mts (list *SMT*)))))
   (csetq sent (fif (equal (car sent) #$ist) sent (list #$ist '?sim-retract-Mt sent)))
   (csetq askable (fif (equal (car sent) #$ist) sent (list #$ist '?sim-retract-Mt sent)))
   (force-print `(ask-template ',sent ',askable ,*SMT*))
   (csetq mts (ask-template sent askable *SMT*))
   (print (length mts))
   (cdolist (sent mts) (ke-unassert-now (third sent)(second sent)))
   (ret mts)))

(define sim-assert (sent &rest flags)
     (csetq flags (flatten (cons flags *VocabularyMt*)))
     (clet  ((mt (car (member-if #'MT? flags)))
     (cmd `(ke-assert-now ,(list 'quote sent) ,mt ,(fif (member :default flags) :default :monotonic) ,(fif (member :BACKWARD flags) :backward :forward))))
      (punless (eval  cmd)
        (format t "~&~s~& ; ERROR: ~s~&" cmd (HL-EXPLANATION-OF-WHY-NOT-WFF sent Mt))
        (force-output))))


(print "start loading MicrotheoryFunction.")(force-output)

'(ke-assert-now `(#$implies (#$and (#$isa ?MT ?MTTYPE)(#$isa ?MTTYPE #$MicrotheoryType))(#$isa ?MT #$Microtheory)) #$BaseKB  :monotonic :FORWARD)
(ke-assert-now `(#$isa ,(foc "MicrotheoryFunction") #$Collection) #$BaseKB)
(ke-assert-now `(#$genls ,(foc "MicrotheoryFunction") #$IndividualDenotingFunction) #$BaseKB)
#+Too-Slow?
(ke-assert-now `(#$implies (#$genls ?MTTYPE #$Microtheory)(#$isa ?MTTYPE #$MicrotheoryType)) #$BaseKB  :default  :forward)
#+Too-Slow?
(ke-assert-now `(#$implies (#$and (#$resultIsa ?FUNCT ?MTTYPE)(#$isa ?MTTYPE #$MicrotheoryType))(#$isa ?FUNCT #$MicrotheoryFunction))#$BaseKB :default  :forward)
(ke-assert-now `(#$implies (#$isa ?FUNCT #$MicrotheoryFunction) (#$resultIsa ?FUNCT #$Microtheory)) #$BaseKB :default  :forward)

(print "done loading MicrotheoryFunction.")(force-output)


(sim-assert `(#$isa ,(find-or-create-constant "SimResearchProject") #$Cyc-BasedProject))

(sim-assert `(#$assertionDirectionSpecificationMtForProject (#$ProjectManagementMicrotheoryFn #$SimResearchProject) #$SimResearchProject))


;;;; Collections
;;(sim-assert `(#$isa ,(find-or-create-constant "SimConstant") #$Collection) *VocabularyMt*)
;;(sim-assert `(#$collectionConventionMt #$SimConstant ,*VocabularyMt*) *BASEKB*)
;;(sim-assert `(#$isa ,(find-or-create-constant "SimCollection") #$CollectionType) *VocabularyMt*)
;;(sim-assert `(#$isa #$SimCollection #$SimConstant) *VocabularyMt*)
;;(sim-assert `(#$genls #$SimCollection #$SimConstant) *VocabularyMt*)
;; seems alot of this very basic things could/should be used by now

;;;; SubL
(define sim-var (simname) 
    (ret (intern (cconcatenate "*" (string-upcase simname) "*") :CYC)))

;;;; Globals
(defparameter *first-made* ())
(define sim-cyc (name) 
  (clet ((const (multiple-value-list  (find-or-create-constant name))))
   (csetq *first-made* (cdr  const))
   ;;(sim-assert `(#$isa ,const #$SimConstant) *VocabularyMt*)
   (csetq *first-made* (car  const))
    (ret (car const))))
                          
(define sim-col (simname) 
    (clet ((super (find-constant simname))(supertype (find-constant (cconcatenate  simname "Type")))(const  (sim-cyc (cconcatenate "Sim"  (string-proper simname)))))
;;;     (sim-assert `(#$isa ,const #$FirstOrderCollection) *VocabularyMt*)
    (pwhen *first-made*
     (sim-assert `(#$isa ,const #$Collection) *VocabularyMt*)
     (sim-assert `(#$collectionConventionMt ,const ,*VocabularyMt*) *BASEKB*))
    ;;(fif super (sim-assert `(#$genls ,const ,super) *VocabularyMt*))
    ;;(fif supertype (sim-assert `(#$isa ,const ,supertype) *VocabularyMt*))
    ;; (clet ((type  (sim-cyc (cconcatenate "Sim"  (string-proper simname) "Type"))))(sim-assert `(#$isa ,type #$CollectionType) *VocabularyMt*)(sim-assert `(#$collectionConventionMt ,type ,*VocabularyMt*) *BASEKB*)(sim-assert `(#$typeGenls ,type  ,const) *VocabularyMt*)))
  (ret const)))


(sim-assert `(#$isa ,(sim-col "Microtheory")  #$MicrotheoryType) *BASEKB*)
;;(sim-assert `(#$genls ,(sim-col "Microtheory") #$MicrotheorySpindle)*BASEKB*)
;;(sim-assert `(#$isa ,(sim-col "Microtheory")  #$AtemporalNecessarilyEssentialCollectionType) *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "Microtheory")   "#$MicrotheoryType that requires a #$genlMt to #$SimVocabularyMt ") *VocabularyMt*)
(sim-assert `(#$implies (#$isa ?MT ,(sim-col "Microtheory")) (#$genlMt ?MT ,*VocabularyMt*)) *VocabularyMt*)
(sim-assert `(#$isa ,*VocabularyMt*  #$VocabularyMicrotheory) *VocabularyMt*)

(define sim-Mt (simname) 
  (clet ((supertype (find-constant (cconcatenate  simname "Microtheory")))
         (const (sim-cyc (cconcatenate "Sim" (string-proper simname) "Mt"))))
    (sim-assert `(#$isa ,const #$Microtheory) *BASEKB*)
     (sim-assert `(#$genlMt ,const ,*VocabularyMt*) *VocabularyMt*)
    (sim-assert `(#$isa ,const #$SimMicrotheory) *VocabularyMt*)
    (fif supertype (sim-assert `(#$isa ,const ,supertype) *VocabularyMt*))
    (set (sim-var (cconcatenate simname "Mt")) const)
  (ret const)))

(sim-assert `(#$isa (#$ProjectManagementMicrotheoryFn #$SimResearchProject) #$SimMicrotheory))
(sim-assert `(#$isa ,(sim-Mt "Mapping") #$SKSISourceDescriptionMicrotheory) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-Mt "Creation")  #$TheoryMicrotheory) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-Mt "StaticState")  ,(sim-col "StateMicrotheory") ) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-Mt "InitialState")  ,(sim-col "StateMicrotheory") ) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-Mt "CurrentState")  ,(sim-col "StateMicrotheory") ) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-Mt "Planning")  ,(sim-col "AgentMicrotheory") ) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-col "TemplateParsingMicrotheory") #$LinguisticObjectType) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "TemplateParsingMicrotheory") #$TemplateParsingMicrotheory))
(sim-assert `(#$genls ,(sim-col "TemplateParsingMicrotheory") #$EnglishLexicalDataMicrotheory))
(sim-assert `(#$genls ,(sim-col "TemplateParsingMicrotheory") #$ProjectMicrotheory ))
(sim-assert `(#$isa ,(sim-Mt "TemplateParsing")  ,(sim-col "TemplateParsingMicrotheory") ) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-Mt "TemplateData")  ,(sim-col "TemplateParsingMicrotheory") ) *VocabularyMt*)
(sim-assert `(#$implies (#$isa ?Mt ,(sim-col "StateMicrotheory")) (#$genlMt-Vocabulary ?Mt ,*VocabularyMt*)) *BASEKB* '(:DIRECTION :FORWARD))

(sim-assert `(#$comment ,*VocabularyMt*  "#$VocabularyMicrotheory that indicates the asserions required for canonicialization of #$SimMicrotheory  microtheories.") *VocabularyMt*)
(sim-assert `(#$comment ,*MappingMt*  "#$MappingMicrotheory that indicates the asserions required for SimFacet sim string.") *VocabularyMt*)
(sim-assert `(#$comment ,*TemplateParsingMt*  "#$TemplateParsingMicrotheory that indicates the asserions required for SimFacet sim string.") *VocabularyMt*)
(sim-assert `(#$comment ,*CreationMt*  "#$SimMicrotheory that has forwarding rules needed to deduce (build outward) obvious parts of the world.") *VocabularyMt*)
(sim-assert `(#$comment ,*StaticStateMt*  "#$SimStateMicrotheory Head of all so that shared static sim properties states can reside.") *VocabularyMt*)
(sim-assert `(#$comment ,*InitialStateMt*  "#$SimStateMicrotheory that is the initial T0 State.") *VocabularyMt*)
(sim-assert `(#$comment ,*PlanningMt*  "#$SimAgentMicrotheory that is visible to all OpenSim agents so that shared sim actions can reside.") *VocabularyMt*)
(sim-assert `(#$comment ,*CurrentStateMt* "a #$notAssertibleMt #$SimStateMicrotheory for instance level data that genlMts to current #$SimStateMicrotheory(s).") *VocabularyMt*)
(sim-assert `(#$genlMt ,*VocabularyMt*  ,*UVMt*) *BASEKB*)
(sim-assert `(#$genlMt ,*CreationMt* ,*MappingMt*) *BASEKB*)
(sim-assert `(#$genlMt ,*MappingMt*  #$KnowledgeSourceManagementMt) *BASEKB*)
(sim-assert `(#$isa ,*TemplateParsingMt*  #$TemplateParsingMicrotheory) *BASEKB*)
(sim-assert `(#$isa ,*TemplateParsingMt*  #$EnglishLexicalMicrotheory) *BASEKB*)

(sim-assert `(#$genlMt ,*StaticStateMt*  #$SituationPredicateRepresentationMt) *BASEKB*)
(sim-assert `(#$genlMt ,*StaticStateMt*  #$BPV-DefinitionalMt) *BASEKB*)
(sim-assert `(#$genlMt ,*StaticStateMt*   ,*CurrentStateMt*) *BASEKB*)
(sim-assert `(#$genlMt ,*CurrentStateMt*   ,*StaticStateMt*) *BASEKB*)
(sim-assert `(#$genlMt ,*CurrentStateMt*   ,*CreationMt*) *BASEKB*)
(sim-assert `(#$genlMt ,*PlanningMt* #$ScriptsTestingMt ) *BASEKB*)
(sim-assert `(#$genlMt ,*PlanningMt*  ,*StaticStateMt*) *BASEKB*)
(sim-assert `(#$genlMt ,*PlanningMt*  ,*CurrentStateMt*) *BASEKB*)
(sim-assert `(#$genlMt-Vocabulary ,*PlanningMt* #$PlanningVocabularyMt ) *BASEKB*)

(sim-assert `(#$isa ,(sim-col "AgentMicrotheory") #$LinguisticObjectType) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "AgentMicrotheory") #$MicrotheorySpindle)*VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "AgentMicrotheory") ,(sim-col "Microtheory") ) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "AgentMicrotheory")  #$CollectorMicrotheory ) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "AgentMicrotheory")  #$PlanningDomainMicrotheory ) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "AgentMicrotheory")  #$PlanSupplementMicrotheory ) *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "AgentMicrotheory")   "#$MicrotheoryType that requires a #$genlMt to #$SimPlanningMt") *VocabularyMt*)
(sim-assert `(#$implies (#$isa ?Mt ,(sim-col "AgentMicrotheory")) (#$mtSpindle ?Mt  ,*PlanningMt* ?Mt)) *BASEKB* '(:DIRECTION :FORWARD))

(sim-retract '(#$ist ?MT (#$genls #$TemporaryLexicalMicrotheory #$CycLTerm)))


(defvar *dt* *TemplateParsingMt*)
(print "loading sim.lisp...1.4 of 5.")
(force-output)

;;(dt-loop *VocabularyMt*)
(define dt-loop (mt)
;;    (sim-assert `(#$genlMt #$DataForNLMt ,mt))
;;    (sim-assert `(#$genlMt #$GeneralLexiconMt ,mt))
;;    (sim-assert `(#$genlMt #$RKFParsingMt ,mt))
   #|(loop-mts *SANDBOX* dt)
    (loop-mts dt *mt*)
    (loop-mts dt mt)
    (loop-mts dt *RKF-MT*)
    (loop-mts dt *VocabularyMt*)
    (loop-mts *VocabularyMt*  dt)
    (loop-mts dt *lmt*)
    (loop-mts dt #$RKFParsingMt)
    (loop-mts dt *SANDBOX*)
    (loop-mts dt *RTP-SYNTACTIC-MT*)|#
    (sim-assert `(#$genlMt #$SimVocabularyMt ,mt))
    (sim-assert `(#$implies (#$isa ?MT #$CreationTemplateMicrotheory) (#$genlMt ,mt ?MT)))
    (sim-assert `(#$implies (#$isa ?Mt #$TemplateParsingMicrotheory) (#$genlMt ,mt ?Mt)) *BASEKB* '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$isa ?Mt #$TemplateParsingMicrotheory) (#$genlMt ?Mt ,mt)) *BASEKB* '(:DIRECTION :FORWARD))
        
    ;;(sim-assert `(#$implies (#$isa ?Mt #$EnglishLexicalDataMicrotheory) (#$genlMt  ?Mt ,*TemplateDataMt*)) *BASEKB* '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$isa ?Mt #$EnglishLexicalMicrotheory) (#$genlMt  ?Mt ,mt)) *BASEKB* '(:DIRECTION :FORWARD))
    
    ;;(sim-assert `(#$implies (#$isa ?Mt #$LexicalMicrotheory) (#$genlMt  ?Mt ,mt)) *BASEKB* '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$isa ?Mt #$ParaphraseDataMicrotheory) (#$genlMt ,mt ?Mt)) *BASEKB* '(:DIRECTION :FORWARD))
    )

    (sim-assert `(#$genlMt #$SimVocabularyMt ,*dt*))
    (sim-assert `(#$implies (#$isa ?MT #$CreationTemplateMicrotheory) (#$genlMt ,*dt* ?MT)))
    (sim-assert `(#$implies (#$isa ?Mt #$TemplateParsingMicrotheory) (#$genlMt ,*dt* ?Mt)) *BASEKB* '(:DIRECTION :FORWARD))
    ;;(sim-assert `(#$implies (#$isa ?Mt #$TemplateParsingMicrotheory) (#$genlMt ?Mt ,*dt*)) *BASEKB* '(:DIRECTION :FORWARD))
        
    ;;(sim-assert `(#$implies (#$isa ?Mt #$EnglishLexicalDataMicrotheory) (#$genlMt  ?Mt ,*TemplateDataMt*)) *BASEKB* '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$isa ?Mt #$EnglishLexicalMicrotheory) (#$genlMt  ?Mt ,*dt*)) *BASEKB* '(:DIRECTION :FORWARD))
    
    ;;(sim-assert `(#$implies (#$isa ?Mt #$LexicalMicrotheory) (#$genlMt  ?Mt ,*dt*)) *BASEKB* '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$isa ?Mt #$ParaphraseDataMicrotheory) (#$genlMt ,*dt* ?Mt)) *BASEKB* '(:DIRECTION :FORWARD))


(sim-retract `(#$genlMt #$SimVocabularyMt ,*dt*))
;;(sim-assert `(#$genlMt #$CycAgencyTheoryMt #$SimVocabularyMt))
(sim-assert `(#$genlMt #$SimMappingMt #$SimVocabularyMt))


(sim-assert `(#$comment ,(sim-cyc "AgentMicrotheory") "This #$MicrotheorySpindle represents the existing divergence in standard ways of 
viewing the Sim world, especially according to Sim world view by agent.  The mtSpindleMember #$Microtheory s each represent one such viewing system.")*VocabularyMt*)
(sim-assert `(#$isa ,(sim-Mt "CurrentAgent")  ,(sim-col "AgentMicrotheory")  ) *VocabularyMt*)
(sim-assert `(#$comment ,*CurrentAgentMt*  "#$SimAgentMicrotheory sample used for testing.") *VocabularyMt*)

(sim-assert `(#$genls ,(sim-col "StateMicrotheory") #$MicrotheorySpindle)*VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "StateMicrotheory") ,(sim-col "Microtheory") ) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "StateMicrotheory")  #$ScenarioCollectorMicrotheory ) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "StateMicrotheory")  #$DataMicrotheory ) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "StateMicrotheory")  #$TemporalMicrotheory ) *VocabularyMt*)

(sim-assert `(#$genls ,(sim-col "StateMicrotheory")  #$HypotheticalContext ) *VocabularyMt*)
;;(sim-assert `(#$genls ,(sim-col "StateMicrotheory")  #$DynamicInfoSource ) *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "StateMicrotheory")   "A collection of #$SimMicrotheory of dynamic world situations in a time scope.
Each instance of SimStateMicrotheory that is uniquely linked to a state generated by OpenSim System.
Each instance is a #$HypotheticalContext that holds the assertions that will be distributed by the hypothesis manager 
to form the content of the world in which the system is thinking about. Thus an instance may contain a pair of mutually incompatible 
assertions in situations where two incompatible hypotheses will be constructed.") *VocabularyMt*)

(sim-assert `(#$implies (#$isa ?Mt ,(sim-col "StateMicrotheory")) (#$mtSpindle ?Mt  ,*StaticStateMt* ,*CurrentStateMt*)) *BASEKB* '(:DIRECTION :FORWARD))

(sim-assert `(#$genls ,(sim-col "CurrentStateMicrotheory") ,(sim-col "StateMicrotheory")))

(sim-assert `(#$isa ,*InitialStateMt* ,(sim-col "CurrentStateMicrotheory")))

(sim-assert `(#$initialSituation ,*CurrentStateMt*  ,*InitialStateMt*) *BASEKB*)
(sim-assert `(#$genlMt ,*CurrentStateMt*  ,(sim-Mt "Creation")  ) *VocabularyMt*)

;;;;(sim-assert `(#$genlMt  (#$MtSpace #$CurrentWorldDataCollectorMt-NonHomocentric (#$MtTimeDimFn #$Now)) ,*VocabularyMt*) *BASEKB*)
;;;;(sim-assert `(#$genlMt ,(sim-Mt "Mapping")  #$CurrentWorldDataCollectorMt-NonHomocentric) *BASEKB*)
;;;;(sim-assert `(#$genlMt #$CurrentWorldDataCollectorMt-NonHomocentric ,*VocabularyMt*) *BASEKB*)

;;;;==================================================
;;;; COLLECTIONS SETUP
;;;;==================================================
(define sim-trans (name simname) 
    (clet ((const (sim-cyc (cconcatenate "sim" (string-proper name) "To" (string-proper simname)))))
    (pwhen *first-made* 
      (sim-assert `(#$isa ,const #$BinaryPredicate ) *VocabularyMt*)      
      (sim-assert `(#$predicateConventionMt ,const  ,*MappingMt*) *BASEKB*)
      (sim-assert `(#$conceptuallyRelated  ,const #$synonymousExternalConcept ) *VocabularyMt*)      
      ;;;;(sim-assert `(#$genlPreds ,const #$ksTermString ) *VocabularyMt*)
      ;;;;(sim-assert `(#$genlPreds ,const #$programStrings ) *VocabularyMt*)
      )
      (ret const)))

;;;; Relations
(sim-assert `(#$genls ,(sim-col "Property") #$Relation) *MappingMt*)
(define sim-pred (simname) 
    (clet ((const  (sim-cyc (cconcatenate "sim" (string-proper simname)))))
    (pwhen *first-made* 
      (sim-assert `(#$isa ,const #$Predicate ) *UVMt*)
      ;;(sim-assert `(#$isa ,const #$SimProperty ) *VocabularyMt*)
     ;;(sim-assert `(#$simRelation ,const ,simname) *MappingMt*))
     )
  (ret const)))
;;;; Collections

(define sim-ItemType (simname) 
    (clet ((const  (sim-cyc (cconcatenate "Sim-" (string-proper simname)))))
    (pwhen *first-made* 
        (sim-assert `(#$isa ,const #$Collection ) *VocabularyMt*)
        (sim-assert `(#$genls ,const #$CollectionType ) *VocabularyMt*)
       ;;(sim-assert `(#$defaultDefiningMtForBinaryPredicateOnType ,const ,*CurrentStateMt*) *BASEKB*)
       (sim-assert `(#$collectionConventionMt ,const ,*VocabularyMt*) *BASEKB*)
       ;;(sim-assert `(#$isa ,const #$ExistenceDependentCollection) *VocabularyMt*)
       (sim-assert `(#$genls ,const #$BPVItemType) *VocabularyMt*)
       (sim-assert `(,(sim-trans "ItemType" "Name") ,const ,simname) *MappingMt*))
  (ret const)))
;;;; Collections

(define sim-class (simname) 
    (clet ((const  (sim-cyc (cconcatenate "Sim_"   (string-proper simname)))))
    (pwhen *first-made* 
       (sim-assert `(#$isa ,const #$Collection) *VocabularyMt*)
       (sim-assert `(#$collectionConventionMt ,const ,*StaticStateMt*) *BASEKB*)
       ;;(sim-assert `(#$genls ,const #$ExistenceDependentCollection ) *VocabularyMt*)
       ;;(sim-assert `(#$genls ,const #$FacetInstanceCollection) *VocabularyMt*)
       (sim-assert `(#$isa ,const #$BPVItemType) *VocabularyMt*)
       ;;(sim-assert `(#$simGenls ,const ,simname) *MappingMt*))
       )
  (ret const)))

;;;; Transient                                 
(define sim-t (simname) 
    (clet ((const  (sim-cyc (cconcatenate "Sim_" (string-proper simname)))))
    (pwhen *first-made*      
      (sim-assert `(#$definingMt ,const ,*StaticStateMt*) *VocabularyMt*)
      (sim-assert `(#$isa ,const #$Individual) *VocabularyMt*)
      (sim-assert `(,(sim-trans "Entity" "Name") ,const ,simname) *MappingMt*)
      (sim-assert `(#$termDependsOn ,const ,(sim-trans "Entity" "Name")) *CurrentStateMt*))
  (ret const)))
;;;; Transient
(define sim-func (simname) 
    (clet ((const  (sim-cyc (cconcatenate "Sim" (string-proper simname) "Fn"))))
    ;;(pwhen *first-made* 
        ;;(sim-assert `(,(sim-trans "Method" "Name") ,const ,simname) *MappingMt*)
        (sim-assert `(#$isa ,const  #$Function-Denotational ) *UVMt*)
  (ret const)))
                                     
;;==================================================
;; Collections BPVItem/BPVAgent/BPVLocation/BPVArtifact
;;==================================================
;;(sim-assert '(#$implies (#$and (#$isa ?INST #$BPVItem)(#$unknownSentence (#$simClasses ?INST ?ANYTEXT))(#$isa ?INST ?SPEC)(#$genls ?SPEC ?COL) (#$simClassToSimFacet ?COL  (#$SimFacetFn ?ParamEvalObject "classname"))) (#$ist *MappingMt* (#$simClasses ?INST ?ParamEvalObject)))*MappingMt*)
(sim-assert '(#$genls #$BPVAgentType #$ExistingObjectType) *UVMt* '(:DIRECTION :FORWARD))
#|
(sim-assert '(#$typeGenls #$BPVAgentType #$BPVAgent)*UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$typeGenls #$BPVAgentType #$BPVAgent)*UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVAgentType #$FirstOrderCollection) *UVMt* '(:DIRECTION :FORWARD))
(find-or-create-constant "BPVAgent")
(find-or-create-constant "BPVAgentType")
(sim-assert `(#$isa #$BPVAgent #$Collection) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVAgent #$BPVItem) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVAgentType #$FirstOrderCollection) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$isa #$BPVAgentType #$CollectionType) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$isa #$BPVAgent #$ExistingObjectType) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVAgent #$IndividualAgent) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVAgent #$IntelligentAgent) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVAgent #$Agent-Generic) *UVMt* '(:DIRECTION :FORWARD))
;;(#$mtAgentType #$BPVAgentType
(find-or-create-constant "BPVArtifact")
(find-or-create-constant "BPVArtifactType")
(sim-assert `(#$isa #$BPVArtifact #$Collection) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVArtifact #$BPVItem) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$isa #$BPVArtifactType #$CollectionType) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$isa #$BPVArtifactType #$FacetingCollectionType) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$typeGenls #$BPVArtifactType #$BPVArtifact)*UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$isa #$BPVArtifact #$ExistingObjectType) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVArtifact #$Artifact-NonAgentive) *UVMt* '(:DIRECTION :FORWARD))
|#
(sim-assert '(#$genls #$BPVArtifact #$InanimateObject) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVArtifact #$Artifact) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVArtifactType #$ExistingObjectType) *UVMt* '(:DIRECTION :FORWARD))
(find-or-create-constant "BPVLocation")
(find-or-create-constant "BPVLocationType")
(sim-assert `(#$isa #$BPVLocation #$Collection) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$isa #$BPVLocationType #$CollectionType) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$typeGenls #$BPVLocationType #$BPVLocation)*UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$facets-Generic #$BPVLocation  #$BPVLocationType)*UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVLocationType #$FacetingCollectionType) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$genls #$BPVLocation #$BPVItem) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$genls #$BPVLocation #$SpaceRegion-Empirical) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVLocation #$Place-NonAgent) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$BPVLocation  #$PartiallyTangible) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$BPVLocation "This is used to mark locations in the sim therefore a specialization of #$BPVItem
 #$Place-NonAgent and #$SpaceRegion-Empirical.") *VocabularyMt* '(:DIRECTION :FORWARD))

(print "loading sim.lisp...1.6 of 5.")
(force-output)

;;(sim-assert '(#$implies (#$isa ?X #$BPVLocationType)(#$genls ?X #$SimLocation)) *VocabularyMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$implies (#$isa ?X #$BPVArtifactType)(#$genls ?X #$SimArtifact)) *VocabularyMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$implies (#$isa ?X #$BPVAgentType)(#$genls ?X #$SimAvatar)) *VocabularyMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$implies (#$and (#$no #$Animal ?COL) (#$genls ?COL #$SolidTangibleThing)) (#$sim:relationCandidateExistsAll #$objectFoundInLocation ?COL #$BPVLocation)) *MappingMt* '(:DIRECTION :FORWARD))
;;;; DISJOINTS OF TYPES
;;(sim-assert '(#$disjointWith #$BPVAgent #$BPVArtifact) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$disjointWith #$BPVLocation #$BPVArtifact) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$disjointWith #$BPVAgent #$BPVLocation) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$comment #$BPVItem "#$Individual inside the sim engine that is already rezed in the world.") *UVMt*)

;;(sim-assert '(#$isa #$BPVItem #$ExistingObjectType) *UVMt* '(:DIRECTION :FORWARD))
;;==================================================
;; Function: Angle2Fn
;;==================================================
(find-or-create-constant "Angle2Fn")
(sim-assert `(#$isa ,(sim-cyc "Angle2Fn")  #$BinaryFunction) *UVMt*)
(sim-assert `(#$isa ,(sim-cyc "Angle2Fn")  #$UnreifiableFunction) *UVMt*)
(sim-assert `(#$arg1Isa ,(sim-cyc "Angle2Fn")  #$AngularDistance) *UVMt*)
(sim-assert `(#$arg2Isa ,(sim-cyc "Angle2Fn")  #$AngularDistance) *UVMt*)
(sim-assert `(#$resultIsa ,(sim-cyc "Angle2Fn")  #$OrientationVector) *UVMt*)
(sim-assert `(#$resultIsa ,(sim-cyc "Angle2Fn")  #$UnitVectorInterval) *UVMt*)
(sim-assert `(#$comment ,(sim-cyc "Angle2Fn")  "A #$OrientationVector (two-axis) that can translate to a 3-D ray when applied to a point.") *UVMt*)
                                         
;;==================================================
;; Function: Point3Fn
;;==================================================
(find-or-create-constant "Point3Fn")
;;(fi-kill #$Point3Fn)
(sim-assert '(#$isa #$Point3Fn #$TernaryFunction) *UVMt*)
(sim-assert `(#$isa #$Point3Fn #$UnreifiableFunction) *UVMt*)
(sim-assert `(#$quotedIsa #$Point3Fn #$ForwardReifiableCycLFunctor) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$conceptuallyRelated #$Point3Fn #$Point4Fn) *UVMt*)
(sim-assert `(#$comment #$Point3Fn "A #$Point in a Cartesian coordinate system. (See also #$Point4Fn)") *UVMt*)
;;;;(sim-assert `(#$argsIsa #$Point3Fn #$RealNumber) *UVMt*)
;;;;(sim-assert `(#$argsIsa #$Point3Fn #$Distance) *UVMt*)
(sim-assert `(#$resultIsa #$Point3Fn #$Point) *UVMt*)
;;(sim-assert `(#$resultIsa #$Point3Fn #$GeographicalPlace-0D) *UVMt*)
;;(sim-assert `(#$resultIsa #$Point3Fn #$SpatialThing-NonSituational) *UVMt*)
;;(sim-assert `(#$resultIsa #$Point3Fn #$PositionVector) *UVMt*)
;;(sim-assert `(#$resultIsa #$Point3Fn #$Individual) *UVMt* '(:DIRECTION :FORWARD))
;;;;(sim-assert `(#$resultIsa #$Point3Fn #$GeographicalPoint-Intangible-FixedLocation) *UVMt*)

;;==================================================
;; Function: BoundsOfDirectionFn
;;==================================================
(find-or-create-constant "BoundsOfDirectionFn")
;; (fi-kill #$BoundsOfDirectionFn)
(sim-assert `(#$isa #$BoundsOfDirectionFn #$BinaryFunction) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$isa #$BoundsOfDirectionFn #$IndividualDenotingFunction) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$isa #$BoundsOfDirectionFn #$ReifiableFunction) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$quotedIsa #$BoundsOfDirectionFn #$ForwardReifiableCycLFunctor) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$isa #$BoundsOfDirectionFn #$TotalFunction) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$comment #$BoundsOfDirectionFn "This is a NART (BoundsOfDirectionFn ?R1 ?D) to produce a instance of a #$Portal (#$Path-Generic) that creates a one-way portal in dirrection ?D from  ?R1 to some location") *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$BoundsOfDirectionFn "Every #$BPVLocation may have any of 18+ #$BoundsOfDirectionFn but implicitly must have 6 based on moving allong the 3 axis at a cone-shaped forty-five degree tollerance (*-Directly). at least specified") *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$arg1Isa #$BoundsOfDirectionFn #$SpatialThing-NonSituational) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$arg2Isa #$BoundsOfDirectionFn #$UnitVector-Precise) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$resultIsa #$BoundsOfDirectionFn #$Border) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert `(#$resultIsa #$BoundsOfDirectionFn #$Path-Simple) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$resultIsa #$BoundsOfDirectionFn #$BoundaryMarker) *VocabularyMt* '(:DIRECTION :FORWARD))
;;(sim-assert `(#$resultIsa #$BoundsOfDirectionFn #$Individual) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert `(#$resultIsa #$BoundsOfDirectionFn #$SpatialThing-NonSituational) *UVMt*)


;;==================================================
;; Collections SimItem/SimAgent/SimLocation/SimArtifact
;;==================================================
(sim-assert `(#$genls ,(sim-col "Avatar") #$BPVAgent) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "Location") #$BPVLocation) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "Artifact") #$BPVArtifact) *VocabularyMt*)
(sim-assert `(#$coExtensional ,(sim-col "Item") #$BPVItem) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "Avatar") ,(sim-col "Item")) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "Location") ,(sim-col "Item")) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "Artifact") #$Artifact) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-col "Class") #$FacetInstanceCollection) *VocabularyMt*)
(sim-assert `(#$defaultDefiningMtForInstances ,(sim-col "Item") ,*CurrentStateMt*) *VocabularyMt*)
(sim-assert `(#$defaultDefiningMtForInstances ,(sim-col "Location") ,*StaticStateMt*) *VocabularyMt*)
(sim-assert `(#$defaultDefiningMtForInstances ,(sim-col "Avatar") ,*StaticStateMt*) *VocabularyMt*)
(sim-assert `(#$defaultDefiningMtForInstances ,(sim-col "Artifact") ,*StaticStateMt*) *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "Avatar")  "An Instanced #$BPVAgent") *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "Location") "An Instanced #$BPVLocation") *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "Artifact") "An Instanced #$BPVArtifact") *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "Item") "An Instanced #$BPVItem") *VocabularyMt*)

(define defsim-function (name visibleMt)
    (clet ((const (sim-func name))
        (SubLName (intern (cconcatenate "CYC-Sim-EVAL-" (STRING-UPCASE name)) :CYC)))
    (DEFINE-EVALUATION-DEFN `,SubLName (&rest body) (ret (sim-eval (list :eval `,const body))))
    ;;;;(sim-assert `(#$isa ,const #$TermMacroFunction) *VocabularyMt*) ;;;;(expansion DistanceBetweenZipCodesFn (SubstituteFormulaArgFn 1 (StringToRealNumberFn (GetFromSimpleHTTPSource "127.0.0.1" 8080 "getDistance" "fromZip" :ARG1 "toZip" :ARG2)) (Mile ?X))).
    (sim-assert `(#$isa ,const #$EvaluatableFunction) *VocabularyMt*)  ;;Function-Denotational
    (sim-assert `(#$evaluationDefn ,const (#$SubLQuoteFn ,SubLName)) visibleMt)
    (sim-assert `(#$comment ,const  ,(format nil "This was generated by defsim-function and evals ~s with ~s" const SubLName)))
  ;;  (define-defsim-function-stub  SubLName)
    (ret const)))

;;(defmacro define-defsim-function-stub (code const)
;;;;;;(ret `(define ,code (&rest body) (ret (cons ,const body)))))

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

 ;;;;  (sim-assert `(#$afterAdding #$simRelation (#$SubLQuoteFn Sim-AFTER-ADDING-PropertyTOSTRING)) ,*CurrentStateMt*)
  ;;;; (sim-assert `(#$afterRemoving #$simRelation (#$SubLQuoteFn Sim-AFTER-REMOVING-PropertyTOSTRING)) ,*CurrentStateMt*)
  ;;;; (DEFINE-AFTER-ADDING  Sim-AFTER-ADDING-PropertyTOSTRING (str asrt) (ret (sim-eval (list :add str (assertion-el-formula asrt) (assertion-el-Mt asrt)))))
  ;;;; (DEFINE-AFTER-REMOVING  Sim-AFTER-REMOVING-PropertyTOSTRING (str asrt) (ret (sim-eval (list :rem str (assertion-el-formula asrt) (assertion-el-Mt asrt)))))
 ;;;;   (fi-reassert '(#$simRelation #$simRelation PropertyToName) *MappingMt* :FORWARD)

(print "loading sim.lisp...2 of 5.")
(force-output)
;;(define Sim-CLASSTOSimFacet-EVAL (&rest r))
;;(define Sim-ITEMTOSimFacet-EVAL (mode truth  &rest r)(ret (print (list r '(2 3)) )))
;;(define Sim-ITEMTOSimFacet-EVAL (mode truth  &rest r)(ret (print `((#$simItemToSimFacet #$isa #$isa)(#$simItemToSimFacet #$genls #$isa)))))

(defsim-function "Eval" *VocabularyMt*)
(sim-assert '(#$isa #$SimEvalFn #$UnaryFunction) *UVMt*)
;;(sim-assert '(#$isa #$SimEvalFn #$PartialFunction) *UVMt*)												  
;;(sim-assert '(#$isa #$SimEvalFn #$TermMacroFunction) *UVMt*)		 
;;(sim-assert '(#$isa #$SimEvalFn #$IndeterminateTermDenotingFunction) *UVMt*)  
;;(sim-assert '(#$comment #$SimEvalFn "Is ") *UVMt*)  

;;==================================================
;; Collection SimItem
;;==================================================
(find-or-create-constant "SimItemFn")
(find-or-create-constant "simItemToName")
(sim-assert '(#$isa #$SimItemFn #$InvertibleFunction) *VocabularyMt*)
;;(sim-assert `(#$isa #$simItemToName #$FunctionalPredicate) *VocabularyMt*)
(sim-assert `(#$isa #$simItemToName #$BinaryPredicate) *VocabularyMt*)
;;(sim-assert `(#$genlFunctions #$SimItemFn #$simItemToNamedFn) *VocabularyMt*)
(sim-assert `(#$genls ,(sim-col "Item") #$BPVItem) *VocabularyMt*)
(sim-assert `(#$isa #$SimItemFn #$ReifiableFunction) *VocabularyMt*)
(sim-assert `(#$isa #$SimItemFn #$TermMacroFunction) *VocabularyMt*)
(sim-assert '(#$isa #$SimItemFn #$UnaryFunction) *VocabularyMt*)
(sim-assert `(#$resultIsa #$SimItemFn #$SimItem) *VocabularyMt*)
(sim-assert `(#$genlPreds #$simItemToName #$termStrings) *VocabularyMt*)

(sim-assert '(#$isa #$simItemToName #$RelationalNounSlot) #$GeneralLexiconMt ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg1Isa #$simItemToName #$BPVItem) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg2Isa #$simItemToName #$CharacterString) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$comment #$simItemToName "(#$simItemToName ITEM STRING) identifies STRING as the name of the sim ITEM.") *VocabularyMt*)
(sim-assert '(#$genTemplate #$simItemToName (#$ConcatenatePhrasesFn (#$BestNLPhraseOfStringFn "the sim itemname for") (#$TermParaphraseFn-NP :ARG1) (#$PhraseFormFn #$presentTense-Generic (#$HeadWordOfPhraseFn (#$BestVerbFormForSubjectFn #$Be-TheWord (#$NthPhraseFn 2)))) (#$TermParaphraseFn-NP (#$StringMentionFn :ARG2)))) #$EnglishParaphraseMt ':DEFAULT ':FORWARD)
;;(sim-assert '(#$multiWordStringDenotesArgInReln (#$TheList "sim" "item") #$Name-TheWord #$CountNoun #$simItemToName 2) #$EnglishMt ':DEFAULT ':FORWARD)
(sim-assert `(#$expansion #$SimItemFn (#$InstanceWithRelationToFn #$SimItem #$simItemToName :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert `(#$expansion #$SimItemFn (#$InstanceNamedFn #$SimItem :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(sim-assert `(#$functionCorrespondingPredicate-Canonical #$SimItemFn #$simItemToName 1) *MappingMt* ':DEFAULT ':FORWARD)
;;(sim-assert '(#$simFacetToPredValue (#$SimFacetFn "name" ?STRING) #$nameString (#$SimClassFn ?STRING)) *VocabularyMt*)
(sim-assert `(#$arg1Isa #$SimItemFn #$CharacterString) *VocabularyMt*)
(sim-assert `(#$isa #$SimItemFn #$PartialFunction) *VocabularyMt*)
(sim-assert `(#$quotedIsa #$SimItemFn #$ForwardReifiableCycLFunctor) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$comment #$SimItemFn 
"(#$SimItemFn ?STRING) denotes the instance of #$SimItem named STRING. 
See also #$simItemToName (which is the #$functionCorrespondingPredicate for #$SimItemFn).<br>
(#$equals (#$SimItemFn ?STRING)(#$InstanceWithRelationToFn #$SimItem (#$SimPropertyFn \"name\") ?STRING)
(#$equals (#$SimItemFn ?STRING)(#$InstancNamedFn #$SimItem ?STRING))<br>
(#$simIsa ?ITEM (#$SimFacetFn \"name\" ?STRING)<br>
(#$isa (#$SimItemFn ?STRING) (#$SimFacetFn \"name\" ?STRING))<br>
(#$equals (#$SimItemFn ?STRING)(#$InstancedNameFn #$SimItem ?STRING))") *VocabularyMt*)
(sim-assert '(#$equiv (#$simItemToName (#$SimItemFn ?NAME) ?NAME) (#$isa (#$SimItemFn ?NAME) #$SimItem)) *MappingMt*)
(sim-assert '(#$implies (#$isa ?W #$SimItem)(#$simItemToName ?W (#$ConstantNamedFn ?W))) *MappingMt*)

;;==================================================
;; Collection SimClass
;;==================================================
(find-or-create-constant "simIsa")
(sim-assert `(#$isa #$simIsa #$BinaryPredicate) *UVMt* ':MONOTONIC ':FORWARD)
;;(sim-assert '(#$isa #$simIsa #$RelationalNounSlot) #$GeneralLexiconMt ':MONOTONIC ':FORWARD)
;;(sim-assert '(#$arg1Isa #$simIsa #$Individual) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg1Isa #$simIsa #$PartiallyTangible) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg2Isa #$simIsa #$Collection) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$comment #$simIsa "(#$simIsa ?INSTANCE ?FACETS) identifies ?INSTANCE as inheriting ?FACETS.") *VocabularyMt*)
(sim-assert '(#$genlPreds #$simIsa #$isa) *VocabularyMt* ':MONOTONIC ':FORWARD)

;;(sim-assert '(#$implies (#$and (#$isa ?INSTANCE ?COL)(#$simGenls ?COL ?CLASSES))(#$ist ,*StaticStateMt* (#$simIsa ?INSTANCE ?CLASSES)))*CreationMt*)

;;==================================================
;; Collection SimProperty
;;==================================================
(find-or-create-constant "SimPropertyFn")
(find-or-create-constant "simRelation")
(sim-assert `(#$isa #$SimPropertyFn #$TermMacroFunction) *VocabularyMt*)
(sim-assert '(#$isa #$SimPropertyFn #$PredicateDenotingFunction) *VocabularyMt*)
(sim-assert '(#$isa #$SimPropertyFn #$UnaryFunction) *VocabularyMt*)
;;(sim-assert `(#$genlFunctions #$SimPropertyFn #$PredicateNamedFn) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-col "Property") #$PredicateType) *MappingMt*)
(sim-assert `(#$genls ,(sim-col "Property") #$SituationPredicate) *VocabularyMt*)
(sim-assert `(#$comment ,(sim-col "Property") "A #$SituationPredicate that my be of any arity situations (dynamic or static occurrences, e.g. \"walking\", or \"containing something\") or in the case where there is an agentive force acting intentionally, #$SimAction") *VocabularyMt*)

(sim-assert `(#$resultIsa #$SimPropertyFn #$SimProperty) *VocabularyMt*)

(sim-assert `(#$isa #$simRelation #$BinaryPredicate) *UVMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$isa #$simRelation #$RelationalNounSlot) #$GeneralLexiconMt ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg1Isa #$simRelation #$Relation) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg2Isa #$simRelation #$CharacterString) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$comment #$simRelation "(#$simRelation RELATION STRING) identifies STRING as the name of the sim RELATION.") *VocabularyMt*)
(sim-assert '(#$genTemplate #$simRelation (#$ConcatenatePhrasesFn (#$BestNLPhraseOfStringFn "the sim propertyname for") (#$TermParaphraseFn-NP :ARG1) (#$PhraseFormFn #$presentTense-Generic (#$HeadWordOfPhraseFn (#$BestVerbFormForSubjectFn #$Be-TheWord (#$NthPhraseFn 2)))) (#$TermParaphraseFn-NP (#$StringMentionFn :ARG2)))) #$EnglishParaphraseMt ':DEFAULT ':FORWARD)
;;(sim-assert '(#$strictlyFunctionalInArgs #$simRelation 1) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(sim-assert '(#$multiWordStringDenotesArgInReln (#$TheList "sim" "property") #$Name-TheWord #$CountNoun #$simRelation 2) #$EnglishMt ':DEFAULT ':FORWARD)
(sim-assert `(#$expansion #$SimPropertyFn (#$PredicateNamedFn #$SimProperty :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(sim-assert `(#$functionCorrespondingPredicate-Canonical #$SimPropertyFn #$simRelation 1) *MappingMt* ':DEFAULT ':FORWARD)
(sim-assert '(#$simRelation (#$SimPropertyFn ?NAME) ?NAME) *MappingMt*)
(sim-assert `(#$arg1Isa #$SimPropertyFn #$CharacterString) *VocabularyMt*)
(sim-assert `(#$isa #$SimPropertyFn #$ReifiableFunction) *VocabularyMt*)
(sim-assert `(#$isa #$SimPropertyFn #$PartialFunction) *VocabularyMt*)
(sim-assert `(#$quotedIsa #$SimPropertyFn #$ForwardReifiableCycLFunctor) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$comment #$SimPropertyFn "(#$SimPropertyFn ?STRING) denotes the same instance of #$SimProperty named STRING. 
See also #$simRelation which is the #$functionCorrespondingPredicate for #$SimPropertyFn. 
Also (#$simActionToFacetNext ?RELATION (#$SimFacetFn \"propertyname\" ?STRING)") *VocabularyMt*)
(sim-assert '(#$implies (#$simRelation (#$SimPropertyFn ?NAME) ?NAME) (#$isa (#$SimPropertyFn ?NAME) #$SimProperty)) *MappingMt*)


;;==================================================
;; Collection SimAction
;;==================================================
(find-or-create-constant "SimActionFn")
(sim-assert `(#$isa #$SimActionFn #$TermMacroFunction) *VocabularyMt*)
(sim-assert '(#$isa #$SimActionFn #$PredicateDenotingFunction) *VocabularyMt*)
(sim-assert '(#$isa #$SimActionFn #$UnaryFunction) *VocabularyMt*)
;;(sim-assert `(#$genlFunctions #$SimActionFn #$PredicateNamedFn) *VocabularyMt*)
(sim-assert `(#$isa ,(sim-col "Action") #$PredicateType) *MappingMt*)
(sim-assert `(#$genls ,(sim-col "Action") #$ActionPredicate) *VocabularyMt*)
(sim-assert `(#$resultIsa #$SimActionFn #$SimAction) *VocabularyMt*)
(sim-assert `(#$expansion #$SimActionFn (#$PredicateNamedFn #$SimAction :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(sim-assert `(#$functionCorrespondingPredicate-Canonical #$SimActionFn #$simRelation 1) *MappingMt* ':DEFAULT ':FORWARD)
(sim-assert '(#$simRelation (#$SimActionFn ?NAME) ?NAME) *MappingMt*)
(sim-assert `(#$arg1Isa #$SimActionFn #$CharacterString) *VocabularyMt*)
(sim-assert `(#$isa #$SimActionFn #$ReifiableFunction) *VocabularyMt*)
(sim-assert `(#$isa #$SimActionFn #$PartialFunction) *VocabularyMt*)
(sim-assert `(#$quotedIsa #$SimActionFn #$ForwardReifiableCycLFunctor) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$comment #$SimActionFn "(#$SimActionFn ?STRING) denotes the same instance of #$SimAction named STRING. 
See also #$simRelation which is the #$functionCorrespondingPredicate for #$SimActionFn. 
Also (#$simActionToFacet ?Action (#$SimFacetFn \"Actionname\" ?STRING)") *VocabularyMt*)
(sim-assert '(#$implies (#$simRelation (#$SimActionFn ?NAME) ?NAME) (#$isa (#$SimActionFn ?NAME) #$SimAction)) *MappingMt*)
            
;;==================================================
;; Function: SimFacetFn
;;==================================================
(sim-assert `(#$isa ,(sim-trans "Facet" "Thing") #$RelationalNounSlot) *UVMt* ':MONOTONIC ':FORWARD)
(find-or-create-constant "SimFacetFn")
;;(fi-kill #$Point3Fn)
(sim-assert '(#$isa #$SimFacetFn #$BinaryFunction) *UVMt*)
(sim-assert '(#$isa #$SimFacetFn #$CollectionDenotingFunction) *UVMt*)
;;(sim-assert '(#$isa #$SimFacetFn #$ReifiableFunction) *UVMt*)												  
;;(sim-assert '(#$isa #$SimFacetFn #$PartialFunction) *UVMt*)												  
;;(sim-assert '(#$isa #$SimFacetFn #$TermMacroFunction) *UVMt*)		 
;;(sim-assert '(#$isa #$SimFacetFn #$IndeterminateTermDenotingFunction) *UVMt*)  
(sim-assert `(#$quotedIsa #$SimFacetFn #$ForwardReifiableCycLFunctor) *VocabularyMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$isa #$SimFacetFn #$SubTopicFunction) *UVMt*)					  
;;(sim-assert `(#$conceptuallyRelated #$SimFacetFn #$JavaSimFacetFn) *VocabularyMt*)
;;(sim-assert `(#$genlFunctions #$SimFacetFn #$CollectionNamedFn) *VocabularyMt*)
(sim-assert `(#$comment #$SimFacetFn "(#$SimFacetFn ?STRING ?VALUE) denotes the instance of #$FirstOrderCollection containing instances of #$SimItem with property ?STRING set to ?VALUE. 
See also #$simFacetToPredValue which is the #$functionCorrespondingPredicate for #$SimFacetFn.
(#$SubcollectionOfWithRelationToFn #$SimItem (#$SimPropertyFn :ARG1) (#$SimEvalFn :ARG2))).
Used in (#$sim<Item/Class>ToFacet ?THING (#$SimFacetFn ?STRING ?VALUE))") *VocabularyMt*)
(sim-assert '(#$arg1Isa #$SimFacetFn #$CharacterString) *UVMt*)
(sim-assert '(#$arg2Isa #$SimFacetFn #$Thing) *UVMt*)
(sim-assert '(#$resultIsa #$SimFacetFn #$FirstOrderCollection) *UVMt*)
;;(sim-assert `(#$expansion #$SimFacetFn (#$SubcollectionOfWithRelationToFn #$SimItem (#$SimPropertyFn :ARG1) (#$SimEvalFn :ARG2))) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert `(#$implies (#$isa (#$SimPropertyFn ?PROP) #$BinaryPredicate) (#$implies (#$different ?VALUE1 ?VALUE2) (#$disjointWith (#$SimFacetFn ?PROP ?VALUE1)(#$SimFacetFn ?PROP ?VALUE2)))) *MappingMt* :BACKWARD)

(sim-assert `(#$implies (#$isa (#$SimFacetFn ?PROP ??VALUE) #$Collection) (#$isa (#$SimPropertyFn ?PROP) #$BinaryPredicate)) *MappingMt*)


;;==================================================
;; Collection SimClass
;;==================================================
;;(sim-assert '(#$isa (#$InstanceNamedFn (#$JavaClassFn "java.io.PrintStream") "out") #$Thing)  *UVMt* ':MONOTONIC ':FORWARD)
(find-or-create-constant "simGenls")
(sim-assert `(#$isa #$simGenls #$BinaryPredicate) *UVMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg1Isa #$simGenls #$Collection) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$arg2Isa #$simGenls #$Collection) *UVMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$genlPreds #$simGenls #$genls) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert '(#$comment #$simGenls "(#$simGenls ?COL ?FACET) identifies ?COL as having the sim ?FACET.") *VocabularyMt*)
(sim-assert '(#$genTemplate #$simGenls (#$ConcatenatePhrasesFn (#$BestNLPhraseOfStringFn "the sim classname for") (#$TermParaphraseFn-NP :ARG1) (#$PhraseFormFn #$presentTense-Generic (#$HeadWordOfPhraseFn (#$BestVerbFormForSubjectFn #$Be-TheWord (#$NthPhraseFn 2)))) (#$TermParaphraseFn-NP (#$StringMentionFn :ARG2)))) #$EnglishParaphraseMt ':DEFAULT ':FORWARD)
;;(sim-assert '(#$isa #$simGenls #$RelationalNounSlot) #$GeneralLexiconMt ':MONOTONIC ':FORWARD)
;;(sim-assert '(#$transitiveViaArgInverse #$simGenls #$genls 1) *BASEKB* ':MONOTONIC ':FORWARD)
;;(sim-assert '(#$implies (#$and (#$simGenls ?COL ?TYPE)(#$genls ?SUB ?COL))(#$simGenls ?SUB ?TYPE)) *BASEKB* ':MONOTONIC ':BACKWARD)
;;(sim-assert '(#$strictlyFunctionalInArgs #$simGenls 1) *VocabularyMt* ':MONOTONIC ':FORWARD)
;;(sim-assert '(#$multiWordStringDenotesArgInReln (#$TheList "sim" "class") #$Name-TheWord #$CountNoun #$simGenls 2) #$EnglishMt ':DEFAULT ':FORWARD)
;;(sim-assert `(#$functionCorrespondingPredicate-Canonical #$SimClassFn #$simGenls 1) *MappingMt* ':DEFAULT ':FORWARD)
;;(sim-assert '(#$implies (#$and (#$simGenls ?COL ?TYPE)(#$genls ?COL ?SUPER))(#$simGenls ?SUPER ?TYPE)) *BASEKB* ':MONOTONIC ':BACKWARD)
;;(sim-assert '(#$simFacetToPredValue (#$SimClassFn ?STRING) #$isa (#$SimClassFn ?STRING)) *MappingMt*)

(sim-assert `(#$isa ,(sim-col "Class") #$FacetingCollectionType) *MappingMt*)
(sim-assert `(#$genls ,(sim-col "Class") #$BPVItemType) *VocabularyMt*)
(find-or-create-constant "SimClassFn")
(sim-assert `(#$isa #$SimClassFn #$TermMacroFunction) *VocabularyMt*)
(sim-assert '(#$isa #$SimClassFn #$CollectionDenotingFunction) *VocabularyMt*)
(sim-assert '(#$isa #$SimClassFn #$UnaryFunction) *VocabularyMt*)
(sim-assert `(#$genlFunctions #$SimClassFn #$CollectionNamedFn) *VocabularyMt*)
(sim-assert `(#$resultIsa #$SimClassFn #$SimClass) *VocabularyMt*)
(sim-assert `(#$resultGenl #$SimClassFn #$BPVItem) *VocabularyMt*)
;;(sim-assert `(#$expansion #$SimClassFn (#$InstanceWithRelationToFn #$SimClass #$simGenls :ARG1)) *VocabularyMt* ':MONOTONIC ':FORWARD)
(sim-assert `(#$arg1Isa #$SimClassFn #$CharacterString) *VocabularyMt*)
(sim-assert `(#$isa #$SimClassFn #$ReifiableFunction) *VocabularyMt*)
(sim-assert `(#$isa #$SimClassFn #$PartialFunction) *VocabularyMt*)
(sim-assert `(#$quotedIsa #$SimClassFn #$ForwardReifiableCycLFunctor) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert `(#$comment #$SimClassFn "(#$SimClassFn ?STRING) denotes the same instance of #$SimClass named STRING. 
See also #$simGenls which is the #$functionCorrespondingPredicate for #$SimClassFn. 
Also (#$simGenls ?CLASS (#$SimFacetFn \"classname\" ?STRING)") *VocabularyMt*)
;;(sim-assert '(#$equiv (#$simGenls (#$SimClassFn ?NAME) ?NAME) (#$isa (#$SimClassFn ?NAME) #$SimClass)) *MappingMt*)
;;(sim-assert '(#$implies (#$and (#$isa ?INSTANCE ?COL)(#$simGenls ?COL ?CLASSES))(#$simIsa ?INSTANCE ?CLASSES))*BASEKB* :BACKWARD)
;;(sim-assert '(#$implies (#$and (#$isa ?INSTANCE ?COL)(#$simGenls ?COL ?CLASSES))(#$isa ?INSTANCE (#$SimClassFn ?CLASSES)))*BASEKB*)
;;(sim-assert '(#$implies (#$and (#$genls ?SUB ?COL)(#$simGenls ?COL ?CLASSES))(#$genls ?SUB (#$SimClassFn ?CLASSES))))
;;(sim-assert '(#$implies (#$simGenls ?COL ?CLASSES)(#$genls ?COL (#$SimClassFn ?CLASSES))))
;;(sim-assert '(#$equiv (#$simGenls ?COL ?CLASSES)(#$genls ?COL (#$SimClassFn ?CLASSES))))
;;(sim-assert '(#$implies (#$isa ?INSTANCE (#$SimClassFn ?CLASSES)) (#$simIsa ?INSTANCE ?CLASSES))*VocabularyMt*)
(sim-assert '(#$simGenls (#$SimFacetFn  "classname" ?NAME) (#$SimClassFn ?NAME)) *MappingMt*)


;;(sim-assert `(#$functionCorrespondingPredicate-Canonical #$SimFacetFn #$simFacetToPredValue 1) *MappingMt* ':DEFAULT ':FORWARD)
;;(sim-assert '(#$equals (#$SimFacetFn ?THING)(#$SimFacetFn "Facetthing" ?THING)) *MappingMt*)
 
;;(#$implies (#$isa ?ITEM ?CLASS) (#$isa (#$InstanceWithRelationToFn ?CLASS ?REL ?VALUE)(#$SubcollectionOfWithRelationToFn ?CLASS ?REL ?VALUE)))
;;(sim-assert `(#$isa (#$InstanceWithRelationToFn ?CLASS ?REL ?VALUE)(#$SubcollectionOfWithRelationToFn ?CLASS ?REL ?VALUE)) *VocabularyMt*)
;;(sim-assert `(#$isa (#$InstanceWithRelationFromFn ?CLASS ?REL ?VALUE)(#$SubcollectionOfWithRelationFromFn ?CLASS ?REL ?VALUE)) *VocabularyMt*)

;;(#$SimFacetFn ?PROP EVAL) == (#$SubcollectionOfWithRelationToFn #$SimItem (#$SimPropertyFn ?PROP) ?VALUE)
;;(#$SimItemFn EVAL) == (#$InstanceWithRelationToFn #$SimItem (#$SimPropertyFn ?PROP) ?VALUE)

                                                  
(print "loading sim.lisp...3 of 5.")
(force-output)

(define defsim-pred (simname Mt &optional (neg-reqs :anything) (pos-reqs :anything))
    ;;(fi-kill (sim-pred simname))
    (clet ((const (sim-pred simname))(preqs pos-reqs)(nreqs neg-reqs)(name (STRING-UPCASE simname)))
        (sim-assert `(#$isa ,const  #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
        (sim-assert `(#$predicateConventionMt ,const  ,Mt) *BASEKB*)
        
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
        (WrapperProc (intern (cconcatenate "Sim-" name "-EVAL") :CYC))
        (AfterAdding (intern (cconcatenate "Sim-AFTER-ADDING-" name) :CYC))
        (AfterRemoving (intern (cconcatenate "Sim-AFTER-REMOVING-" name) :CYC)))
        (csetq pos-reqs (cons const pos-reqs))
        (csetq neg-reqs (cons const neg-reqs))         

    (inference-removal-module `,RemovalPosProc
     `(:sense :pos :predicate ,const  :cost-expression 0 :completeness :complete 
        :required-pattern :ANYTHING  ;; :required-pattern ,pos-reqs
        :output-generate-pattern (:call ,WrapperProc :pos :input)
        :example ,(write-to-string pos-reqs)  
        :documentation ,(cconcatenate "This was generated by defsim-pred and works on modes " (write-to-string pos-reqs) ".")))
    (inference-removal-module `,RemovalNegProc
     `(:sense :neg :predicate ,const :cost-expression 0 :completeness :complete 
        :required-pattern :ANYTHING  ;; :required-pattern ,neg-reqs
        :output-generate-pattern (:call ,WrapperProc :neg :input)
        :example ,(cconcatenate "(#$not " (write-to-string neg-reqs) ")") 
        :documentation ,(cconcatenate "This was generated by defsim-pred and works on modes (#$not " (write-to-string neg-reqs) ").")))

   (eval `(define ,WrapperProc (truth &rest args) (ret (sim-eval truth args))))

   (eval `(DEFINE-AFTER-ADDING ,AfterAdding (str asrt) (ret (sim-eval (list :add str (assertion-el-formula asrt) (assertion-el-Mt asrt))))))
   (eval `(DEFINE-AFTER-REMOVING ,AfterRemoving (str asrt) (ret (sim-eval (list :rem str (assertion-el-formula asrt) (assertion-el-Mt asrt))))))

   (sim-retract `(#$notAssertibleMt ,Mt) *UVMt*)
   (sim-assert `(#$afterAdding ,const (#$SubLQuoteFn ,AfterAdding)) Mt)
   (sim-assert `(#$afterRemoving ,const (#$SubLQuoteFn ,AfterRemoving)) Mt)
   (sim-retract `(#$notAssertibleMt ,Mt) *UVMt*)
   (sim-assert `(#$comment ,const  ,(cconcatenate "Generated by defsim-pred for modes of " (write-to-string pos-reqs) " and (#$not " (write-to-string neg-reqs) ").")) *VocabularyMt*)
   (register-solely-specific-removal-module-predicate const)
   (ret const))))
        
                         
;;==================================================
;; PREDICATE #$simEval
;;==================================================
(defsim-pred "eval" `,*CurrentStateMt* 2)
(sim-assert `(#$isa #$simEval #$FunctionalPredicate) *VocabularyMt*)
(sim-assert `(#$isa #$simEval  #$BinaryPredicate) *VocabularyMt*)
(sim-assert `(#$comment #$simEval  "(#$simEval ?Object ?FacetEval)") *VocabularyMt*)
(sim-assert `(#$arity #$simEval  2) *VocabularyMt*)
(sim-assert `(#$arg1QuotedIsa #$simEval  #$SubLListOrAtom) *VocabularyMt*)
(sim-assert `(#$arg2Isa #$simEval  #$Thing) *VocabularyMt*)
(sim-assert `(#$functionCorrespondingPredicate #$SimEvalFn #$simEval 2) *VocabularyMt*)

(sim-assert `(#$simEval ?X (#$SimEvalFn ?X)) *VocabularyMt*)

(sim-assert `(#$genlFunctions #$SimEvalFn #$IdentityFn) *VocabularyMt*)

;;==================================================
;; PREDICATE #$simPropertyValue
;;==================================================
(defsim-pred "propertyValue" `,*CurrentStateMt* 2 1)
(sim-assert `(#$isa #$simPropertyValue #$TernaryPredicate) *VocabularyMt*)
(sim-assert `(#$comment #$simPropertyValue  "(#$simPropertyValue ?NAME ?PROP ?VALUE)") *VocabularyMt*)
(sim-assert `(#$arity #$simPropertyValue  3) *VocabularyMt*)
(sim-assert `(#$arg1Isa #$simPropertyValue #$Thing) *VocabularyMt*)
(sim-assert `(#$arg2Isa #$simPropertyValue #$Thing) *VocabularyMt*)
(sim-assert `(#$arg3Isa #$simPropertyValue #$Thing) *VocabularyMt*)
;;==================================================
;; PREDICATE #$simPropertyNext
;;==================================================
(defsim-pred "propertyNext" `,*CurrentStateMt* 2 1)
(sim-assert `(#$isa #$simPropertyNext #$TernaryPredicate) *VocabularyMt*)
(sim-assert `(#$comment #$simPropertyNext  "(#$simPropertyNext ?NAME ?PROP ?NEXT)") *VocabularyMt*)
(sim-assert `(#$arity #$simPropertyNext  3) *VocabularyMt*)
(sim-assert `(#$arg1Isa #$simPropertyNext #$Thing) *VocabularyMt*)
(sim-assert `(#$arg2Isa #$simPropertyNext #$Thing) *VocabularyMt*)
(sim-assert `(#$arg3Isa #$simPropertyNext #$Thing) *VocabularyMt*)

;;==================================================
;; PREDICATE #$simClassPropertyValue
;;==================================================
(defsim-pred "classPropertyValue" *StaticStateMt* 2 1)
(sim-assert `(#$isa #$simClassPropertyValue #$TernaryPredicate) *VocabularyMt*)
(sim-assert `(#$comment #$simClassPropertyValue  "(#$simPropertyValue ?NAME ?PROP ?VALUE) Retrives the default for a SimClass or Collection") *VocabularyMt*)
(sim-assert `(#$arity #$simClassPropertyValue  3) *VocabularyMt*)
(sim-assert `(#$arg1Isa #$simClassPropertyValue #$Thing) *VocabularyMt*)
(sim-assert `(#$arg2Isa #$simClassPropertyValue #$Thing) *VocabularyMt*)
(sim-assert `(#$arg3Isa #$simClassPropertyValue #$Thing) *VocabularyMt*)
(sim-assert `(#$equiv (#$simClassPropertyValue ?NAME ?PROP ?VALUE) (#$simGenls (#$SimClassFn ?NAME) (#$SimFacetFn ?PROP ?VALUE))) *MappingMt* :BACKWARD)
;;==================================================
;; PREDICATE #$simIsa
;;==================================================
;;(sim-pred "Instance")
;;(sim-assert `(#$isa #$simIsa  #$BinaryPredicate) *VocabularyMt*)
;;(sim-assert `(#$comment #$simIsa  "(#$simIsa ?Object ?FacetEval)") *VocabularyMt*)
;;(sim-assert `(#$arity #$simIsa  2) *VocabularyMt*)
;;(sim-assert `(#$arg1Isa #$simIsa  #$SimItem) *VocabularyMt*)
;;(sim-assert `(#$arg2Isa #$simIsa  #$FirstOrderCollection) *VocabularyMt*)
;;'(sim-assert `(#$equiv (#$and  (#$simItemToName ?ITEM ?NAME)(#$simPropertyValue ?NAME ?PROP ?VALUE))(#$simIsa (#$SimItemFn ?NAME) (#$SimFacetFn ?PROP ?VALUE))) *VocabularyMt*)
;;'(sim-assert `(#$equiv (#$simPropertyValue ?NAME ?PROP ?VALUE)(#$simIsa (#$SimItemFn ?NAME) (#$SimFacetFn ?PROP ?VALUE))) *VocabularyMt*);;(sim-assert `(#$completeExtentDecidable #$simIsa ,*CurrentStateMt* '(:DIRECTION :FORWARD))

;;==================================================
;; INTERFACE MAPPING SETUP (Collection)
;;==================================================
;;(sim-pred "collection")
;;(sim-assert `(#$isa #$simGenls  #$BinaryPredicate) *VocabularyMt*)
;;(sim-assert `(#$comment #$simGenls  "RezArgs Not Using Inheritance (#$simGenls ?Collection ?FacetEval)") *VocabularyMt*)
;;(sim-assert `(#$arity #$simGenls  2) *VocabularyMt*)
;;(sim-assert `(#$arg1Isa #$simGenls  #$SimClass) *VocabularyMt*)
;;(sim-assert `(#$arg2Isa #$simGenls  #$FirstOrderCollection) *VocabularyMt*)
;;(sim-assert `(#$transitiveViaArg #$simGenls #$genls 1) *MappingMt*)
;;(sim-assert `(#$genlPreds #$simGenls #$genls) *VocabularyMt*)
;;(sim-assert '(#$implies (#$simGenls ?CLASS (#$SimClassFn ?NAME)) 
;;    (#$and (#$isa (#$SimClassFn ?NAME) #$SimClass)(#$simGenls (#$SimClassFn ?NAME) ?NAME)
;;          (#$genls ?CLASS (#$SimClassFn ?NAME)))) *MappingMt*)
;;(sim-assert `(#$completeExtentDecidable #$simGenls *MappingMt* '(:DIRECTION :FORWARD))
#|
;;==================================================
;; INTERFACE MAPPING SETUP (CollectionImplied)
;;==================================================
(sim-pred "CollectionImplied" *MappingMt*)
(sim-assert `(#$isa #$simGenlsImplied  #$BinaryPredicate) *VocabularyMt*)
(sim-assert `(#$comment #$simGenlsImplied  "RezArgs Using Inheritance (#$simGenlsImplied ?SimClass ?FacetImpliedEval)") *VocabularyMt*)
(sim-assert `(#$arity #$simGenlsImplied  2) *VocabularyMt*)
(sim-assert `(#$arg1Isa #$simGenlsImplied  #$SimClass) *VocabularyMt*)
(sim-assert `(#$arg2Isa #$simGenlsImplied  #$FirstOrderCollection) *VocabularyMt*)
(sim-assert `(#$transitiveViaArg #$simGenlsImplied #$genls 1) *MappingMt*)
(sim-assert '(#$simGenlsImplied (#$SimClassFn ?NAME)(#$SimClassFn ?NAME)) *MappingMt*)
;;==================================================
;; INTERFACE MAPPING SETUP (collectionToFacet)
;;==================================================
(sim-pred "collectionToFacet" ,*CurrentStateMt* 2)
(sim-assert `(#$isa #$simGenlsToFacet  #$BinaryPredicate) *VocabularyMt*)
(sim-assert `(#$comment #$simGenlsToFacet "When a normal cyc collection is put into sim (#$simGenlsToFacet ?Instance ?FacetEval) after adding will ensure that the ?FacetEval is reflected as true in the sim.") *VocabularyMt*)
(sim-assert `(#$arity #$simGenlsToFacet  2) *VocabularyMt*)
(sim-assert `(#$arg1Isa #$simGenlsToFacet  #$Collection) *VocabularyMt*)
(sim-assert `(#$arg2Isa #$simGenlsToFacet  #$FirstOrderCollection) *VocabularyMt*)
(sim-assert `(#$transitiveViaArg #$simGenlsToFacet #$genls 1) *MappingMt*)
(sim-assert `(#$implies (#$simGenlsToFacet ?COL (#$SimClassFn ?NAME))
    (#$coExtensional  (#$SimClassFn ?NAME) ?COL)) *MappingMt*)
;;(sim-assert `(#$completeExtentDecidable #$simGenlsToFacetImplied *MappingMt* '(:DIRECTION :FORWARD))
;;==================================================
;; INTERFACE MAPPING SETUP (collectionToFacetImplied)
;;==================================================
(sim-pred "collectionToFacetImplied" *MappingMt*)
(sim-assert `(#$isa #$simGenlsToFacetImplied  #$BinaryPredicate) *VocabularyMt*)
(sim-assert `(#$comment #$simGenlsToFacetImplied "When a normal cyc collection is put into sim (#$simGenlsToFacetImplied ?Instance ?FacetEval) after adding will ensure that the ?FacetEval is reflected as true in the sim.") *VocabularyMt*)
(sim-assert `(#$arity #$simGenlsToFacetImplied  2) *VocabularyMt*)
(sim-assert `(#$arg1Isa #$simGenlsToFacetImplied  #$Collection) *VocabularyMt*)
(sim-assert `(#$arg2Isa #$simGenlsToFacetImplied  #$FirstOrderCollection) *VocabularyMt*)
(sim-assert `(#$transitiveViaArg #$simGenlsToFacetImplied #$genls 1) *MappingMt*)
(sim-assert `(#$implies (#$simGenlsToFacetImplied ?COL (#$SimClassFn ?NAME))
    (#$genls  (#$SimClassFn ?NAME) ?COL)) *MappingMt*)

|#
;;(sim-assert `(#$genlPreds #$simGenlsToFacet #$genls) *VocabularyMt*)

;;(csetq *VocabularyMt* *UVMt*)
(print "loading sim.lisp...4 of 5.")
(force-output)

#|
;;;;(clet (res) (ccatch :odd-result (cdolist (x `(4 2 1 0) (pif (oddp x) (throw :odd-result x)    (print x))))(pwhen res (print (cconcatenate (str res) " was odd!")))))
;;;;(define-evaluation-defn CYC-sim-eval-FN (el-list) (sim-eval el-list))
(define Sim-EVAL-REMOVAL-EVAL (&rest all))
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
;;;;;;(sim-assert `(#$genlMt ,*PlanningMt*  #$EverythingPSC) '*BASEKB*  '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC))

;;;;(#$isa (#$BoundsOfDirectionFn #$Area1010 #$East-Directly) #$Wall-GenericBarrier)
;;;;(,(sim-trans "Collection" "Classes")  (#$BoundsOfDirectionFn #$Area1010 #$East-Directly) #$Wall-GenericBarrier)
;;;;(knownSentence isa       (BoundsOfDirectionFn Area1000 North-Directly) OpenPortal) ?TRUTH)

;;;;(sim-assert `(#$implies (#$isa ?X ,(sim-ItemType "Item") ) (#$isa ?X #$SpatialThing-Localized)) *PlanningMt*  '(:DIRECTION :FORWARD))
;;(sim-assert `(#$isa ,(sim-t "cyc_bot_1") ,(sim-ItemType "idAI") ) *VocabularyMt*)
;;(sim-assert `(#$isa ,(sim-t "player1") ,(sim-ItemType "idPlayer") ) *VocabularyMt*)
;;(sim-assert `(#$isa ,(sim-t "cyc_bot_1") #$Agent-Generic) *VocabularyMt*)
;;(sim-assert `(#$isa ,(sim-t "player1") #$Agent-Generic) *VocabularyMt*)
(force-output)
#|
;;;;(sim-assert `(#$implies (#$and (#$isa ?OBJ #$Agent-Generic)(#$termStrings ?OBJ ?STRING)(,(sim-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(sim-assert `(#$implies (#$and (#$isa ?OBJ #$Agent-Generic)(#$termStrings ?OBJ ?STRING)(,(sim-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(sim-assert `(#$implies (#$and (#$isa ?OBJ #$Portal)(#$termStrings ?OBJ ?STRING)(,(sim-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(sim-assert `(#$implies (#$and (#$isa ?OBJ #$GeographicalRegion)(#$termStrings ?OBJ ?STRING)(,(sim-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
;;;;(sim-assert `(#$implies (#$and (#$isa ?OBJ ,(sim-ItemType "Item") )(#$termStrings ?OBJ ?STRING)(,(sim-trans "Collection" "ItemType")  ?COL ?STRING)) (#$ist ,*PlanningMt*  (#$isa ?OBJ ?COL))) *VocabularyMt* '(:DIRECTION :FORWARD))
|#
#|(sim-assert '(#$implies (#$simActionToFacetNext ?OBJ (#$SimFacetFn "name" ?NAME)) 
   (#$and (#$isa (#$SimPropertyFn ?NAME) #$SimProperty)
          (#$simRelation (#$SimPropertyFn ?NAME) ?NAME)
          (#$genlPreds ?OBJ (#$SimPropertyFn ?NAME)))) *MappingMt*)|#
                
(define sim-subl (prename args body)
  (clet ((name prename)
          (namestr (symbol-name prename))
         (evalstr `(sl::define ,prename ,args ,@body)))
           (csetq name (join-strings  (mapcar #'string-proper (string-tokenize (string-DOWNCASE namestr) '(#\space #\-))) ""))
           (punless (string-equal (substring name 0 4) "sim") (csetq name (cconcatenate "Sim" (string-proper name))))
           (csetq name (find-or-create-constant (cconcatenate (string-DOWNCASE (substring name 0 1)) (substring name 1))))
           (sim-assert `(#$isa ,name #$FunctionalPredicate) *VocabularyMt*)
           (sim-assert `(#$programCode (#$OperatorFn ,namestr) (#$SubLEntityFn #$ComputerCode (#$SubLQuoteFn ,evalstr))) *VocabularyMt*)           
           (sim-assert `(#$programFunctionOperator ,name (#$OperatorFn ,namestr)) *VocabularyMt*)
           (eval evalstr)
           (ret name)))

(defmacro define-subl (prename args &rest body)
  (clet ((name (sim-subl prename args body))
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
           (sim-assert `(#$isa ,name #$Predicate) *VocabularyMt*)           
           (fif (numberp minarity)     
                  (progn 
                    (sim-assert `(#$isa ,name #$VariableArityPredicate) *VocabularyMt*)
                    (sim-assert `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (sim-assert `(#$arityMin ,name ,(1+ (length minargs))) *VocabularyMt*))
                  (progn 
                    (sim-assert `(#$isa ,name #$Predicate) *VocabularyMt*)           
                    (sim-assert `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (sim-assert `(#$arity ,name ,(length predargs)) *VocabularyMt*)))
           (sim-assert `(#$argsQuotedIsa ,name #$SubLSExpression) *VocabularyMt*)
           (sim-assert `(,name ,@predargs) *VocabularyMt*)
           (sim-assert `(#$comment ,name ,commentstr) *VocabularyMt*)))


(defmacro define-subl-p (prename args &rest body)
  (clet ((name (sim-subl prename args body))
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
                    (sim-assert `(#$isa ,name #$VariableArityPredicate) *VocabularyMt*)
                    (sim-assert `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (sim-assert `(#$arityMin ,name ,(1+ (length minargs))) *VocabularyMt*))
                  (progn 
                    (sim-assert `(#$isa ,name #$Predicate) *VocabularyMt*)           
                    (sim-assert `(#$functionalInArgs ,name ,(1+ (length minargs))) *VocabularyMt*)
                    (sim-assert `(#$arity ,name ,(length predargs)) *VocabularyMt*)))
           (sim-assert `(#$implies 
                   (#$trueSubL (#$ExpandSubLFn (,@variables) (apply ,applyfn2 ,@applyargs)))
                   (,name ,@predargs)) *VocabularyMt*)
           (sim-assert `(#$argsQuotedIsa ,name #$SubLSExpression) *VocabularyMt*)
           (sim-assert `(#$comment ,name ,commentstr) *VocabularyMt*)))


(defmacro wec (vn exec) (ret `(progn (csetq ,vn ,exec) (length ,vn))))
(defmacro wqc (vn tmpl qry &optional (hmt *SMT*)) (ret `(wec ,vn (ask-template ',tmpl ',(dfn qry) ,hmt))))

                
(force-output)
(find-or-create-constant "Sim-ProvableSentence")
(sim-assert '(#$isa #$Sim-ProvableSentence #$Collection) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$defnSufficient #$Sim-ProvableSentence (#$SubLQuoteFn ProvableSentence)) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$Sim-ProvableSentence #$CycLSentence-Askable) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$genls #$Sim-ProvableSentence #$SubLSExpression) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$Sim-ProvableSentence "All sentences that can be proved") *VocabularyMt* '(:DIRECTION :FORWARD))
(define ProvableSentence (v) (ret (cand (cq v ))))
(define cq (q &optional (hMt #$EverythingPSC))
  (pwhen (consp q)
     (clet ((cmd 
        `(cyc-query ,q ,hMt '(:backchain T)))
     (result (eval cmd)))
     (print (list cmd result))
     (ret result))))

(find-or-create-constant "Sim-UnprovableSentence")
(sim-assert '(#$isa #$Sim-UnprovableSentence #$Collection) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$defnSufficient #$Sim-UnprovableSentence (#$SubLQuoteFn UnprovableSentence)) *UVMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$genls #$Sim-UnprovableSentence #$CycLSentence-Askable) *UVMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$genls #$Sim-UnprovableSentence #$SubLSExpression) *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$comment #$Sim-UnprovableSentence "All sentences that can fail to be proved") *VocabularyMt* '(:DIRECTION :FORWARD))
(sim-assert '(#$disjointWith #$Sim-UnprovableSentence #$Sim-ProvableSentence) *UVMt* '(:DIRECTION :FORWARD))
(define UnprovableSentence (v) (ret (null (cq1 v ))))
(define cq1 (q &optional (hMt #$EverythingPSC))
  (pwhen (consp q)
     (clet ((cmd 
        `(cyc-query ,q ,hMt '(:backchain T :time 15 :number 1)))
     (result (eval cmd)))
     (print (list cmd result))
     (ret result))))


;;(define sim-eval (&rest all) (format t "~&~s~&" (cons 'sim-eval all))(ret `((#$simPropertyValue 1 2 4)(#$simPropertyValue 1 2 5))))

(defparameter *WorldVocabularyMt* *VocabularyMt*) ;; CreationMt
(defparameter *WorldStaticStateMt* *StaticStateMt*)
(defparameter *WorldCurrentStateMt* *InitialStateMt*)

(print `(load "cynd/startrek.lisp"))

(print `(load "cynd/osim-bindings.lisp"))
(print `(load "cynd/startrek.lisp"))
(print `(load "cynd/osim-creation.lisp"))
(print `(load "cynd/osim-sksi.lisp"))
(print `(load "cynd/osim-methods.lisp"))

(force-output)


(defparameter *Sim-ERROR* nil)

(defmacro sim-warn (&rest code)  (ret 
    `(with-error-handler #'(lambda () 
         (punless *Sim-ERROR* (csetq *Sim-ERROR* *ERROR-MESSAGE*))
         (terpri)(format t "ERROR: ~s ~s" *ERROR-MESSAGE* ',code)(terpri)(force-output)
                        #|(throw :UNEVALUATABLE T)|# ) (sl::progn ,@code))))

(defmacro sim-error-handler (code) (ret
   `#'(lambda () 
         (punless *Sim-ERROR* (csetq *Sim-ERROR* *ERROR-MESSAGE*))
         (terpri)(format t "ERROR: ~s ~s" *ERROR-MESSAGE* ',code)(terpri)(force-output))))
       
(define sim-debugable (somecode) 
    (ret `(with-error-handler 
       (sim-error-handler ,somecode)
        ,somecode)))

(defmacro sim-warn (&rest somecode) 
        `(sl::progn ,@(mapcar #'sim-debugable somecode)))

(define term-el (outval) (ret 
    (pcond 
       ((nart-p outval) (nart-el-formula outval))
       ((assertion-p outval) (assertion-el-ist-formula outval))
       ((consp outval) (cons (term-el (car outval))(term-el (cdr outval))))
       (t outval))))


(sim-assert '(#$equals #$SpiralStaircase (#$SimClassFn "mud_twisty")) *MappingMt*)
(defparameter *sim-host* "10.10.10.193")
(defparameter *sim-port* 3650)
(defmacro game-eval (outval) (ret `(sim-eval :game ,outval)))

(define sim-eval (&rest outvalIn) 
 ;;(THROW-UNEVALUATABLE-ON-ERROR 
  (ret (clet (retval stream (outval (term-el outvalIn)))
    (terpri)(format t "~s" (cons 'Sim-EVAL (mapcar #'(lambda (x) (ret (list 'QUOTE x))) outval)))
    (terpri)(force-output)  
    (csetq *Sim-ERROR* nil)
    (sim-warn (csetq stream (OPEN-TCP-STREAM *Sim-HOST* *Sim-PORT*)))        
    (pwhen (cand (null *Sim-ERROR*) (streamp stream) (output-stream-p stream)(input-stream-p stream))
      (prin1 outval stream)
      (terpri stream)(force-output stream)
      (csetq retval (sim-warn (read-line stream nil :EOF nil)))
      (format t "RECEIVE: ")(princ retval)(terpri)(force-output)
      ;;(csetq retval (sim-warn (read stream nil :EOF nil)))
      ;;(sim-warn (fif (equal 200 retval) (csetq retval (read stream nil :EOF nil))))
      (csetq retval (sim-warn (read-from-string retval :EOF nil)))
      (format t "AS: ~s" retval)(terpri)(force-output)
      (close stream) retval))))
      ;;)
;;(read-line *STANDARD-INPUT* nil :EOF nil)


;;(sim-assert `(#$genlMt (#$MtSpace #$CurrentWorldDataCollectorMt-NonHomocentric (#$MtTimeDimFn #$Now)) ,*CurrentStateMt*) *UVMt* '(:DIRECTION :FORWARD))

(load "cynd/osim-agent-planner.lisp")

(sim-assert '(#$simRelation (#$NonDavidsonianPredFn #$DroppingAnObject (#$TheList #$doneBy #$objectActedOn)) "entity_dropItem") )
(sim-assert '(#$simRelation (#$NonDavidsonianPredFn #$DroppingAnObject-Purposeful (#$TheList #$doneBy #$objectActedOn)) "entity_dropItem") )




(sim-assert '(#$simRelation #$objectFoundInLocation "objectFoundInLocation") )
(sim-assert '(#$simRelation #$sees "entity_canSee") )
(sim-assert '(#$simRelation #$touches-Externally "entity_touches") )
(sim-assert '(#$simRelation #$touches "entity_touches") )
(sim-assert '(#$simRelation #$locatedAtPoint-Spatial "locatedAtSpatial") )
(sim-assert '(#$simRelation #$near "near") )
(sim-assert '(#$simRelation #$traverses "doTraversal") )

(defparameter *simtimestep* 0)

(define sim-goto-next-state (&optional (mt *CurrentStateMt*)(nextMt (find-or-create-constant (cconcatenate  "SimState" (write-to-string (cinc *simtimestep*)) "Mt"))))
    (sim-assert `(#$isa ,nextMt ,(sim-col "StateMicrotheory")  ) *VocabularyMt*)
    (sim-assert `(#$initialSituation ,nextMt ,mt) *BASEKB*)     
    (sim-retract `(#$genlMt ,*CurrentStateMt* ,*CurrentStateMt*) *VocabularyMt*)
    (sim-retract `(#$genlMt ,*CurrentStateMt* ,mt) *VocabularyMt*)
    (sim-retract `(#$genlMt ,*CurrentStateMt* ,*InitialStateMt*) *VocabularyMt*)
    (sim-assert `(#$genlMt ,*CurrentStateMt* ,nextMt) *VocabularyMt*)
    (csetq *InitialStateMt* mt)
    (csetq *CurrentStateMt* nextMt))


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
            (pwhen (= retval 2)
                 (ret (read-from-string (cconcatenate "(#$Point3Fn " str " )"))))
            (pwhen (> retval 0)
                 (ret (read-from-string (cconcatenate "(" str ")"))))
            (csetq retval (find-constant str))
            (pwhen retval (ret retval))
            (ret `(#$SimItemFn ,str)))
        ((cor (fort-p str)(nart-p str) (el-variable-p str)(constant-p str)(hl-variable-p str)) (ret  str))
        ((cnot (consp str)) (ret str))
        ((member (car str) '(dfn quote)) (ret (dfn (second str)))) 
        ((function? (car str)) (ret str))
        ;;((cor (numberp str)(null str)(constant-p str))(ret str))
        ((cor (fort-p str) (el-variable-p str)(constant-p str)(hl-variable-p str)) (ret  str))
        (t (ret (cons (dfn (car str))(dfn (cdr str))))))))
                       


(print `(progn (load "cynd/startrek.lisp")(load "cynd/osim-bindings.lisp")
  (load "cynd/startrek.lisp") (load "cynd/osim-creation.lisp")(load "cynd/osim-sksi.lisp")(load "cynd/osim-methods.lisp")))

(sim-assert `(#$implies (#$isa ?Mt #$SimCurrentStateMicrotheory) (#$genlMt  ,*CurrentStateMt* ?Mt)))

(force-output)

'(sim-assert `(#$notAssertibleMt ,*CurrentStateMt*))


