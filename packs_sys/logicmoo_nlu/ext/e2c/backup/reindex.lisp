
;;(defmacro dquery (varname query) (cand (defvar varname (CYC-QUERY query #$EverythingPSC)) T))
;;(dquery	 *qv9* '(#$isa ?W #$Dog))
;;*qv1*

(define qmake (q1) (CYC-QUERY q1 #$EverythingPSC) )
(define qjoin (q1 q2 ) (ljoin (qmake q1)  (qmake q2)))
(define ljoin (l1 l2) (cdolist (e1 l1) (ejoin e1 l2)))
(define ejoin (e1 l2) (cdolist (e2 l2) (intersection e1 e2 )))

;;(qmake '(#$isa ?X #$Scientist))
;;(qmake '(#$isa ?X #$osim:Instance))
;;(qjoin '(#$isa ?X #$osim:Instance) '(#$isa ?X #$Scientist))


(define rXY (pred) (mapcar #'ASSERTION-FORMULA  (ALL-TERM-ASSERTIONS pred)))
;;cdolist  #$effectOfAction-Props
;;

;;(#$effectOfAction-Props


;; (variable-p '?X)
;; (constant-p '?X)

;;(constant-p 'X)

;;CYC(91): (mapcar #'variable-p '(#$isa ?X #$Dog))
;;(NIL NIL NIL)
;;CYC(92):  (mapcar #'constant-p '(#$isa ?X #$Dog))
;;(T NIL T)
;;CYC(93):  (mapcar #'el-var? '(#$isa ?X #$Dog))
;;(NIL T NIL)
;;CYC(94): (mapcar #'el-var? '(#$isa ?var0  #$Dog))
;;(NIL T NIL)
;;CYC(95): (mapcar #'variable-p '(#$isa ?var0  #$Dog))
;;(NIL NIL NIL)


(cand (find-constant "SKF-2853676952") (fi-rename (find-constant "SKF-2853676952") "ConversationBetweenAgentsFn"))
(cand (find-constant "SKF-3567349991") (fi-rename (find-constant "SKF-3567349991") "CommunicationActSingleBetweenAgentsWhenWhereFn"))
(cand (find-constant "SKF-1926505029") (fi-rename (find-constant "SKF-1926505029") "CommunicationActSingleWhenBetweenAgentsFn"))
(cand (find-constant "SKF-9239624728") (fi-rename (find-constant "SKF-9239624728") "CommunicationActSingleWhereBetweenAgentsFn"))
(cand (find-constant "SKF-1086471246") (fi-rename (find-constant "SKF-1086471246") "CommunicationActSingleBetweenAgentsFn"))
(cand (find-constant "SKF-12903845") (fi-rename (find-constant "SKF-12903845") "ClaimingAgentPropDurationFn"))
(find-or-create-constant "communicatedMeaningOfTextString")
(cyc-assert '(#$isa #$communicatedMeaningOfTextString #$TernaryPredicate) '#$UniversalVocabularyMt)
(cyc-assert '(#$arg1Isa #$communicatedMeaningOfTextString #$CommunicationAct-Single) '#$UniversalVocabularyMt)
(cyc-assert '(#$arg2Isa #$communicatedMeaningOfTextString #$TextString) '#$UniversalVocabularyMt)
(cyc-assert '(#$arg3Isa #$communicatedMeaningOfTextString #$Microtheory) '#$UniversalVocabularyMt)
(cyc-assert '(#$comment #$communicatedMeaningOfTextString "In each #$CommunicationAct-Single that is deplicted via #$TextString this #$TernaryPredicate may yeild possibly one #$Microtheory which may be many literals.") '#$UniversalVocabularyMt)
(#$implies (#$and 
   (#$senderOfInfo ?COM ?PERSON1)
   (#$recipientOfInfo ?COM ?PERSON2)
   (#$startingPoint ?COM ?START)
   (#$endingPoint ?COM ?END)
   (#$communicatedMeaningOfTextString ?COM ?TEXT ?INFO))
   (knows

(communicatedMeaningOfTextString ?CommunicationAct-Single ?Text ?CycL)

(find-or-create-constant "CyPROXAssertableFn")
(find-or-create-constant "CyPROX")

(cyc-assert '(#$isa #$CyPROX #$PlugInProgram) '#$UniversalVocabularyMt)




(find-or-create-constant "CyPROXAssertableFn")
(cyc-assert '(#$isa #$CyPROXAssertableFn #$VariableArityFunction) '#$UniversalVocabularyMt)
(cyc-assert '(#$arg1Isa #$CyPROXAssertableFn #$TextString) '#$UniversalVocabularyMt)
(cyc-assert '(#$arg2Isa #$CyPROXAssertableFn #$Microtheory) '#$UniversalVocabularyMt)
(cyc-assert '(#$arg3Isa #$CyPROXAssertableFn #$CommunicationConvention) '#$UniversalVocabularyMt)
(cyc-assert '(#$resultIsa #$CyPROXAssertableFn #$CycLSentence-Assertible) '#$UniversalVocabularyMt)


(find-or-create-constant "textStringTrue")
(cyc-assert '(#$isa #$textStringTrue #$UnaryPredicate) '#$UniversalVocabularyMt)
(cyc-assert '(#$arg1Isa #$textStringTrue #$TextString) '#$UniversalVocabularyMt)


(cyc-assert 
'(#$implies 
  (#$and 
     (#$ist ?Mt (#$textStringTrue ?TextString))
     (#$knowsAbout ?Who ?Mt)
     (#$usesCommunicationConvention ?Mt ?CommunicationConvention)
     (#$evaluate ?Prop 
           (#$EvaluateSubLFn 
               (#$ExpandSubLFn 
                   (?TextString ?Mt ?CommunicationConvention) 
			(CALLPROLOGPRED '(#$CyPROXAssertableFn  ?TextString ?Mt ?CommunicationConvention))))))
  (#$and (#$genlMt ?Mt (#$TextStringPropositionalExtractionMtFn #$CyPROX ?CommunicationConvention ?TextString))
             (#$ist (#$TextStringPropositionalExtractionMtFn #$CyPROX ?CommunicationConvention ?TextString) ?Prop)))
	      '#$BaseKB '(:Direction :FORWARD))



(#$ist #$OSimCurrentSituationMt (#$textStringTrue "Lieutenant Worf sees alexander rozhenko."))
(#$ist #$OSimCurrentSituationMt (#$usesCommunicationConvention #$OSimCurrentSituationMt #$EnglishLanguage))
(cycMeaningOfTextString	?Conversation ?Text ?CycL)

(  CyclifyFn ?When ?Where ?Speaker ?Listener ?Context ?Text ?CycL)

(CyclifyFn ?Text) -> CycLExpression-Askable / CycLFormulaicSentence / CycLPropositionalSentence








CyclificationOfEnglishFn ?Event ?Speaker ?Listener ?Context ?Text


;;(ejoin '(1 2) '((1 2)(3 4)))
;;(define eprint (e1 l2) (cmaplist (e2 l2) (print (intersection e1 e2 ))))
;;(eprint '(1 2) '((1 2)(3 4)))
;;(defvar  *INDEXTODO* nil)

;;(mapcar #'intersection '(((?X . 2 )(?W . 3))((?X . 6)(?X . 7))) '(((?X . 2 )(?W . 3))((?X . 6)(?X . 7))))
;;(intersection '(((?X . 2 )(?W . 3))((?X . 6)(?X . 7)))  '( ((?X . 2 )(?W . 3)) ((?X . 6)(?X . 7)) ))

;; (passert 0 *STANDARD-OUTPUT*)

;;(clet ((x 0) (strm (OPEN-BINARY "cycdump.txt" :output)))
;;	(cdo
;;	  ((x 0 (+1 x))) ((= x (ASSERTION-COUNT)) (print "done")) (passert x strm)))
	 
;;(define passert (x strm) )      






;; StdOUTPUT
(clet ((x 0)) (cdo ((x 0 (+ 1 x))) 
  ((= x (CONSTANT-COUNT)) (print "done")(force-output strm)(close strm)) (progn  
  (clet ((asrt (FIND-CONSTANT-BY-INTERNAL-ID x))) (print (list 'find-or-create (CONSTANT-NAME asrt)))))))
(clet ((x 0) (strm *STANDARD-OUTPUT*)) (cdo ((x 0 (+ 1 x))) ((= x (ASSERTION-COUNT)) (print "done")(force-output strm)) 
   (progn  (clet ((asrt (FIND-ASSERTION-BY-ID x))) (print (list 'CYC-ASSERT (list 'QUOTE (ASSERTION-FORMULA asrt)) 
   (list 'QUOTE (ASSERTION-MT asrt)) (list 'QUOTE (list :STRENGTH (ASSERTION-STRENGTH asrt)  :DIRECTION (ASSERTION-DIRECTION asrt)))) strm)))))
 
;; StdOUTPUT 10
 (clet ((x 0) (strm NIL)) (cdo ((x 0 (+ 1 x))) ((= x 10) (print "done")(force-output strm)) 
   (progn  (clet ((asrt (FIND-ASSERTION-BY-ID x))) (print (list 'CYC-ASSERT (list 'QUOTE (ASSERTION-FORMULA asrt)) 
   (list 'QUOTE (ASSERTION-MT asrt)) (list 'QUOTE (list :STRENGTH (ASSERTION-STRENGTH asrt)  :DIRECTION (ASSERTION-DIRECTION asrt)))) )))))
   

;;(assertion-el-mt)

;; files
(clet ((x 0) (strm (OPEN-BINARY "constdump.lisp" :output))) (cdo ((x 0 (+ 1 x))) 
  ((= x (CONSTANT-COUNT)) (print "done")(force-output strm)(close strm)) (progn  
  (clet ((asrt (FIND-CONSTANT-BY-INTERNAL-ID x))) (cand asrt (print (list 'find-or-create-constant (CONSTANT-NAME asrt)) strm) (force-output strm) T)))))

                                 1499765         


;;(open-tcp-stream "localhost" 3602)
(clet ((x 0) (lastassert (+ 0  (ASSERTION-COUNT))) (strm (OPEN-BINARY "asserts.lisp" :output))) 
;(clet ((x 0) (lastassert (+ 0  (ASSERTION-COUNT))) (strm (open-tcp-stream "localhost" 3801))) 
  (cdo ((x 0 (+ 1 x))) 
  ((>= x lastassert) (print "done")(force-output strm)(close strm)) 
;  (cor (output-stream strm) (csetq strm (open-tcp-stream "localhost" 3801)) 
  (clet ((asrt (FIND-ASSERTION-BY-ID x))) (cand asrt (printassert2 asrt strm))))

(define printassert2 (asrt strm) 
   (ret (cand asrt
    (clet ((elform (ASSERTION-FORMULA asrt))(hlmt (ASSERTION-MT asrt)) (pred nil))
      (cand elform (consp elform) (csetq pred (car elform))
       (cnot (member pred '( #$skolem #$termOfUnit)))
       ;;(cnot (member pred '(#$isa #$genlMt #$siblingDisjointExceptions #$genls #$arity #$resultGenl #$resultIsa #$arityMin #$arityMax #$skolem #$termOfUnit)))
       (cnot (search "#$SKF" (princ-to-string elform))) 
         (progn 
	   (cand (nart-p hlmt) (csetq hlmt (nart-el-formula hlmt)))
           (print (list 'without-wff-semantics (list 'CYC-ASSERT (list 'QUOTE elform) (list 'quote hlmt) (list 'QUOTE (list :STRENGTH (ASSERTION-STRENGTH asrt)  :DIRECTION (ASSERTION-DIRECTION asrt))))) strm)))))))
	     
	     


(eval-remote-api (host port sent) (
  (clet (
	(load-tell "assertdump0.lisp")
		(load-tell "assertdump.lisp")


(define load-tell (filename)  
  (clet ((*got* nil)(*res* nil) (strm (OPEN-BINARY filename :input))) 
    (cdo ((*got* nil (read strm nil :eof)))
    ((equal *got* :eof))
    ;;  (princ ";; ")(print *got*)
      (cand (consp *got*) (cnot (member #$thereExists (flatten *got*)))
      (progn (csetq *res* (eval *got*)) 
      (cor *res* (print *got*)))))))
      

 
;;(resultIsa Percent Real0-1) in UniversalVocabularyMt
(cyc-assert (#$resultIsaArg #$TheSetOf 1) '#$UniversalVocabularyMt)
;;Mt : UniversalVocabularyMt (genls PersonTypeByMentalFeature BLOTypeByPhysiology)
;;Mt : CoreCycLMt (genls Set-Mathematical ExistingObjectType)


	   
                                        ;;  1499927
(define savepred-extent (const) 
  (clet ((x 0) (lastid (+ 1499924))(strm (OPEN-BINARY (constant-name const) :output)))
    (cdo ((x 0 (+ 1 x)))
  ((> x lastid) (print "done")(force-output strm)(close strm)) 
  (progn  (clet ((asrt (FIND-ASSERTION-BY-ID x))(form (ASSERTION-EL-FORMULA asrt)))
           (cand asrt form (consp form) (equal const (car form)) 
    		   (printassert asrt strm)(force-output strm)))))))

(reassert-const #$nameString)

(define reassert-const (const) 
    (cdo ((x (- (assertion-count) 1) (- 1 x)))
     ((= x 0) (print "done"))
     (progn  (clet ((asrt (FIND-ASSERTION-BY-ID x))(form (ASSERTION-EL-FORMULA asrt)))
           (cand asrt form (consp form) (equal const (car form)) (reassert2 asrt))))))
		   
		   
(define reassert2 (asrt) 
   (ret (cand asrt
    (clet ((elform (ASSERTION-EL-FORMULA asrt))(hlmt (ASSERTION-MT asrt)) (pred nil))
      (cand elform (consp elform) (csetq pred (car elform))
       (cnot (member pred '(#$skolem #$termOfUnit)))
       (cnot (member #$thereExists (flatten elform)))
       ;;(cnot (member pred '(#$isa #$genlMt #$siblingDisjointExceptions #$genls #$arity #$resultGenl #$resultIsa #$arityMin #$arityMax #$skolem #$termOfUnit)))
       (cnot (search "#$SKF" (write-to-string elform))) 
         (progn 
	   (cand (nart-p hlmt) (csetq hlmt (nart-el-formula hlmt)))
           (CYC-ASSERT elform hlmt)))))))
;;           (CYC-ASSERT elform hlmt (list :STRENGTH (ASSERTION-STRENGTH asrt) :DIRECTION (ASSERTION-DIRECTION asrt)))))))))





;;(cyc-assert (#$isa #$Agent-Generic #$AtemporalNecessarilyEssentialCollectionType) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
;;(cyc-assert (#$isa #$Person #$AtemporalNecessarilyEssentialCollectionType) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
;;(cyc-assert (#$isa #$Organization #$AtemporalNecessarilyEssentialCollectionType) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
;;(cyc-assert (#$isa #$Place #$AtemporalNecessarilyEssentialCollectionType) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))

(Cyc-assert '(#$implies (#$familyName ?WHO ?ANY) (#$ist #$UniversalVocabularyMt (#$isa ?WHO #$Person))) '#$BaseKB  '(:DIRECTION :FORWARD))
(Cyc-assert '(#$implies (#$isa ?WHO #$Person) (#$ist #$UniversalVocabularyMt (#$isa ?WHO #$Person))) '#$BaseKB  '(:DIRECTION :FORWARD))
(Cyc-assert '(#$implies (#$isa ?WHAT #$Organization) (#$ist #$UniversalVocabularyMt (#$isa ?WHAT #$Organization))) '#$BaseKB  '(:DIRECTION :FORWARD))
(Cyc-assert '(#$implies (#$isa ?WHERE #$Place) (#$ist #$UniversalVocabularyMt (#$isa ?WHERE #$Place))) '#$BaseKB  '(:DIRECTION :FORWARD))

(Cyc-assert '(#$implies (#$genls ?StringType #$CharacterString) (#$defnSufficient ?StringType (#$SubLQuoteFn STRINGP))) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
(Cyc-assert  '(#$defnSufficient #$StringObject (#$SubLQuoteFn STRINGP)) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
(Cyc-assert  '(#$defnSufficient #$TextString (#$SubLQuoteFn STRINGP)) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
(Cyc-assert  '(#$resultIsa #$Percent #$Real0-1) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
(Cyc-assert  '(#$genls #$PersonTypeByMentalFeature #$BLOTypeByPhysiology) '#$UniversalVocabularyMt  '(:DIRECTION :FORWARD))
;;( Percent Real0-1) in UniversalVocabularyMt

(savepred-extent #$isa)
(savepred-extent #$genlMt)
(savepred-extent #$siblingDisjointExceptions)
(savepred-extent #$genls)
(savepred-extent #$arity)
(savepred-extent #$resultGenl)
(savepred-extent #$resultIsa)
(savepred-extent #$arityMin)
(savepred-extent #$arityMax)

(define isrenamed (x fs) (clet ((*const* (find-constant-by-internal-id x)) (*oldnames* (mapcar #'car (ask-template '(?W) (list '#$oldConstantName *const* '?W) '#$BookkeepingMt))))
  (cand (consp *oldnames*)
    (print (list 'clet (list 
     (list 'foundconst 
       (list 'find-constant-by-name 
         (car *oldnames*)))) (list 'cand 'foundconst (list 'RENAME-CONSTANT 'foundconst (constant-name *const*)))) fs))))

(define constinfo (x fs) (clet ((*const* (find-constant-by-internal-id x)) (*oldnames* (mapcar #'car (ask-template '(?W) (list '#$oldConstantName *const* '?W) '#$BookkeepingMt)))  )    
     (princ *const* fs)
     ;;(princ " " fs)(princ (length (all-term-assertions *const*)) fs)(princ " " fs)
     ;;(princ (fif *oldnames* (cons :oldname *oldnames*) (list :same (constant-name *const*))) fs)
     (terpri fs)))

(constinfo 0)
(constinfo 50)
(isrenamed 53773 *standard-output*)
(cdo ((*x* 0 (+ *x* 1))) ((= *x* (constant-count))) (constinfo *x* *standard-output*))

(clet ((strm (OPEN-BINARY "consts" :output))) (cdo ((*x* 0 (+ *x* 1))) ((= *x* (constant-count))(force-output strm)(close strm)) (constinfo *x* strm)))

(clet ((strm (OPEN-BINARY "renames2.lisp" :output)) 

(clet ((strm (OPEN-BINARY "renames.lisp" :output)) 
  (*renames* (ask-template 
  (list '?C '?O) (list '#$oldConstantName '?C '?O) '#$EverythingPSC)))
  (cdo ((rename  (car *renames*) (car *renames*)))((null *renames*)(force-output strm)(close strm))
   (progn 
   (cand (consp rename) (cnot (consp (car rename)))
   (print rename)
    (print (list 'clet (list 
     (list 'foundconst 
       (list 'find-constant
         (second rename)))) (list 'cand 'foundconst 
         (list 'cnot (list 'find-constant (constant-name (car rename))))
	  (list 'RENAME-CONSTANT 'foundconst (constant-name (car rename))) (list 'cyc-assert (list 'QUOTE (cons '#$oldConstantName rename )) '#$BookkeepingMt) )  )strm))
	 (csetq *renames* (cdr *renames*)))))
	 
Microtheory BaseKB UniveralVocabularyMt InformationStore AspatialInformationStore SourceMicrotheory MicrotheoryType DataMicrotheory Microtheory genlMt

isa
genls siblingDisjointExceptions
arity QuantitySlot BinaryPredicate genlPreds arityMin arityMax TernaryPredicate argIsa argGenl resultIsa

SAICLegacyAssertionsMt
TKBSourceSpindleHeadMt WeaponsOfMassDestructionTacticsMt OsamaBinLaden

CycLSentence-Assertible CycLSentence-Askable implies and equals not quotedIsa quotedArgument quotedCollection multiWordString 
quotedDefnIff afterAdding afterRemoving defnIff

genlInverse generalizations interArgReln1-2 genlPreds fanOutArg argFormat argsIsa transitiveViaArg conservativeViaArg typedGenlPreds interArgReln interArgDifferent constrainsArgs typedGenlInverse genlFunctions

MotleyFoolUKMortgageGlossary MotleyFoolUKFoolishGlossary UIARuleHoldingMt TheMotleyFoolUKCorpusMt MotleyFoolCorpusSOEMt SOEGlfMt GLFMt WesternBusinessPracticesMt CoABSMt MotleyFoolUKGlossaryMt glossaryMt UIAInternalVocabularyMt RKFExperimentalTermsMt

termOfUnit ist knownSentence positiveAmountOf admittedSentence pegInterpretations 
ist   The   TheFn   

TheList

TheList


(#$siblingDisjointExceptions #$CycLConstant #$Individual)
(#$siblingDisjointExceptions ?W #$Individual)

(implies 
       (integerRange ?INTEGER-TYPE ?RANGE) 
       (defnIff ?INTEGER-TYPE 
           (SubLQuoteFn CYC-INTEGER-RANGE)))
(implies 
       (and 
           (different ?LOW ?HIGH) 
           (termOfUnit ?NAT 
               (IntegerFromToFn ?LOW ?HIGH))) 
       (integerRange ?NAT 
           (Unity ?LOW ?HIGH))) in BaseKB 




(clet ((strm (OPEN-BINARY "renames.lisp" :output))) (cdo ((*x* 0 (+ *x* 1))) 
  ((= *x* (constant-count))(force-output strm)(close strm)) (isrenamed *x* strm)))

'(#$isa #$genlMt #$siblingDisjointExceptions #$genls #$arity #$resultGenl #$resultIsa #$arityMin #$arityMax)
'(#$skolem #$termOfUnit)

(cyc-query '(#$and (#$skolem ?SK) (#$performSubL (#$ExpandSubLFn (?SK) (FI-KILL ?SK)))) '#$EverythingPSC)

(contains 

(mapcar #'write-to-string (flatten '(#$isa (#$isa (#$isa (#$isa )))))

;(define assertSelfPatch 
;(define plremoval (cpred) NIL)


(find-or-create-constant "gameEval" )
(cyc-assert '(#$isa #$gameEval #$BinaryPredicate ) '#$UniversalVocabularyMt )
(cyc-assert '(#$comment #$gameEval "Defined via the defineRemovalPred #$gameEval -> gameEval" ) '#$UniversalVocabularyMt )
(cyc-assert '(#$isa #$gameEval #$RemovalModuleSupportedPredicate-Specific ) '#$CycAPIMt )
(cyc-assert '(#$arity #$gameEval 2 ) '#$UniversalVocabularyMt )
(inference-removal-module :removal-gameEval-pos '(:sense :pos :predicate #$gameEval :cost-expression 0 :completeness :incomplete :input-verify-pattern :anything :output-generate-pattern (:call gameEval-pos-proc :input ) ) )
(inference-removal-module :removal-gameEval-neg '(:sense :neg :predicate #$gameEval :cost-expression 0 :completeness :incomplete :input-verify-pattern :anything :output-generate-pattern (:call gameEval-neg-proc :input ) ) )
(register-solely-specific-removal-module-predicate #$gameEval )
(define gameEval-pos-proc (value ) (ret (callprologpred (list "gameEval-pos-proc" value ) ) ) )
(define gameEval-neg-proc (value ) (ret (callprologpred (list "gameEval-neg-proc" value ) ) ) )

;(cyc-query '(gameEval ?X 1) #$BaseKB)



(clet ((x 0) (strm (OPEN-BINARY "nartdump.lisp" :output))) (cdo ((x 0 (+ 1 x))) 
  ((= x (NART-COUNT)) (print "done")(force-output strm)(close strm)) (progn  
  (clet ((asrt (FIND-NART-BY-ID x))) (cand asrt (print (list 'CYC-NART (NART-EL-FORMULA asrt)) strm))))))
 
;;ASSERTED-WHEN
;;FI-REASSERT 

(defmacro asmac (asrt) (clet ((*the-cyclist* (ASSERTED-BY asrt)))  (ke- assert-now (ASSERTION-FORMULA asrt) (ASSERTION-MT asrt) '(:DIRECTION  (ASSERTION-DIRECTION arst) :STRENGTH (ASSERTION-STRENGTH asrt)))))


(define reassert0 (asrt)  (list 'FI-EDIT
     (list 'QUOTE (ASSERTION-FORMULA asrt)) (list 'QUOTE (ASSERTION-IST-FORMULA asrt)) 
        (list 'QUOTE (ASSERTION-MT asrt))(list 'QUOTE (ASSERTION-MT asrt))  (ASSERTION-STRENGTH asrt) (ASSERTION-DIRECTION asrt)))

(define reindex (term) (cdolist (x (all-term-assertions term)) (reassert x)))


;; StdOUTPUT 10
(define reassert1 (id) (clet ((asrt (FIND-ASSERTION-BY-ID id))) 
  (CYC-ASSERT (ASSERTION-FORMULA asrt) (ASSERTION-MT asrt) (list 'QUOTE :STRENGTH :MONOTONIC :DIRECTION (ASSERTION-DIRECTION asrt)))))
(define reassertall () (clet ((x 0))
   (cdo ((x (- (ASSERTION-COUNT) 3) (- 1 x))) ((= x 0) (print "done")(force-output)) (reassert1 (FIND-ASSERTION-BY-ID x))))
   )
   


;;todo (FIND-ASSERTION-BY-ID 1307101)
;;todo (FIND-ASSERTION-BY-ID 916639)
;;todo (FIND-ASSERTION-BY-ID 916620)
;;assertion(592952,'synonymousExternalConcept',['Gardening','WordNet-Version2_0',"V01691800"],'WordNetMappingMt',['synonymousExternalConcept','Gardening','WordNet-Version2_0',"V01691800"],'TRUE','FORWARD','DEFAULT','NIL','NIL','ASSERTED-TRUE-DEF',20040816,'#$Cyc').
;;0-894078 need svar    1226884 1473611
;;649296	 	1490542
(clet ((x 0) (strm (OPEN-BINARY "plassertion4.prolog" :output))) (cdo ((x 1490542 (+ 1 x))) 
  ((= x  (assertion-COUNT)) (print "done")(force-output strm)(close strm))     
  (progn (force-output strm)(cor (= x 1490544) (= x 1491260) (= x 1491447) (= x 1474882) (plassertion x strm)))))
  
  
(clet ((x 0) (strm *standard-output*)) (cdo ((x 0 (+ 1 x))) 
  ((= x (+ 10 (assertion-COUNT))) (print "done")(force-output strm)(close strm))     
   (plassertion x strm)))


(clet ((x 0) (strm (OPEN-BINARY "plconstants.prolog" :output))) (cdo ((x 0 (+ 1 x))) 
  ((= x (+ 10 (CONSTANT-COUNT))) (print "done")(force-output strm)(close strm))     
   (plconstant x strm)))
   
(clet ((x 0) (strm (OPEN-BINARY "PLNART.prolog" :output))) (cdo ((x 0 (+ 1 x))) 
  ((= x (+ 10 (NART-COUNT))) (print "done")(force-output strm)(close strm))     
   (PLNART x strm)))
   
(clet ((x 0) (strm (OPEN-BINARY "pldeduction.prolog" :output))) (cdo ((x 0 (+ 1 x))) 
  ((= x (+ 10 (DEDUCTION-COUNT))) (print "done")(force-output strm)(close strm))     
   (pldeduction x strm)))
   
(plassertion 916640 *standard-output*)


(define plassertion1 (x strm) (progn (clet ((asrt (FIND-ASSERTION-BY-ID x))) 
    (cand asrt (princ "assertion(" strm) 
          (progn (princ x strm) (princ "," strm) 
           (plwrite (car (ASSERTION-FORMULA asrt)) strm) (princ "," strm) 
           (plwrite (ASSERTION-FORMULA asrt) strm) (princ "," strm) 
           (plwrite (ASSERTION-MT asrt) strm)  (princ "," strm) 
           (plwrite (ASSERTION-EL-FORMULA asrt) strm) (princ ",'" strm) 
           (princ (ASSERTION-TRUTH asrt) strm) (princ "','" strm) 
	   (princ (ASSERTION-DIRECTION asrt) strm) (princ "','" strm)
	   (princ (ASSERTION-STRENGTH asrt) strm) (princ "','" strm)
           (princ (DEDUCED-ASSERTION? asrt) strm) 
	   (princ "','" strm) 
           (princ (ASSERTION-HAS-DEPENDENTS-P asrt) strm) 
	   (princ "','" strm) 
           (princ (GET-ASSERTED-ARGUMENT  asrt) strm) (princ "'," strm) 
           (plnumber (ASSERTED-WHEN asrt) strm) (princ ",'" strm) 
           (princ (ASSERTED-BY asrt) strm) (princ "')." strm)
	    (princ #\newline strm)))
	   (ret t))))

(define plassertion (x strm) (progn (clet ((asrt (FIND-ASSERTION-BY-ID x))) 
    (cand asrt (princ "assertion(" strm) 
       (clet ((form (ASSERTION-FORMULA asrt)))
          (progn (princ x strm) (princ "," strm) 
           (plwrite (car form) strm) (princ "," strm) 
           (plwrite (cdr form) strm) (princ "," strm) 
           (plwrite (ASSERTION-MT asrt) strm)  (princ "," strm) 
           (plwrite (ASSERTION-EL-FORMULA asrt) strm) (princ ",'" strm) 
           (princ (ASSERTION-TRUTH asrt) strm) (princ "','" strm) 
	   (princ (ASSERTION-DIRECTION asrt) strm) (princ "','" strm)
	   (princ (ASSERTION-STRENGTH asrt) strm) (princ "','" strm)
           (princ (DEDUCED-ASSERTION? asrt) strm) (princ "','" strm) 
           (princ (ASSERTION-HAS-DEPENDENTS-P asrt) strm) (princ "','" strm) 
           (princ (GET-ASSERTED-ARGUMENT  asrt) strm) (princ "'," strm) 
           (plnumber (ASSERTED-WHEN asrt) strm) (princ ",'" strm) 
           (princ (ASSERTED-BY asrt) strm) (princ "')." strm)
	    (princ #\newline strm)))
	   (ret t)))))
	   
(define plnumber (term strm) 
   (progn 
      (cand (null term) (princ 0 strm) (ret t))
      (princ term strm) (ret t)))

(define plnart (x strm) (progn (clet ((asrt (FIND-NART-BY-ID x))) 
    (cand asrt (princ "nart(" strm) 
           (princ x strm) (princ "," strm) 
           (plwrite (NART-HL-FORMULA asrt) strm) (princ "," strm) 
           (plwrite asrt strm) (princ ")." strm) 
	   (princ #\newline strm)(force-output strm)))))

(define plconstant (x strm) (progn (clet ((asrt (FIND-CONSTANT-BY-INTERNAL-ID x))) 
    (cand asrt (princ "constant(" strm) 
           (plwrite asrt strm) (princ "," strm)  
           (plwrite (CONSTANT-INTERNAL-ID asrt) strm) (princ ",'" strm) 
           (princ (guid-to-string (CONSTANT-EXTERNAL-ID  asrt)) strm) (princ "'," strm) 
	   (plwrite (MIN-ISA asrt) strm) 
	   ;;(plwrite (ALL-ISA asrt) strm) (princ "," strm) 
	  ;; (plwrite (ALL-NOT-ISA asrt) strm) (princ "," strm) 
	  ;; (plwrite (ALL-INSTANCES asrt) strm)
	   (princ ")." strm) (princ #\newline strm)))))

(define pldeduction (x strm) (progn (clet ((asrt (FIND-DEDUCTION-BY-ID x))) 
    (cand asrt (princ "deduction(" strm) 
           (princ x strm) (princ "," strm) 
           (plwrite (DEDUCTION-ASSERTION asrt) strm) (princ ")." strm) (princ #\newline strm)(force-output strm)))))


(define plcons (term strm) 
  (progn 
    (princ "[" strm) (plwrite (car term) strm) (plsentrest (cdr term) strm) (ret t)))

(define  plsentrest (term strm) 
  (progn
	(cand (null term) (princ "]" strm) (ret t))
        (cand (consp term) (princ "," strm) (plwrite (car term) strm) (plsentrest (cdr term) strm) (ret t))
	(princ "|" strm) (plwrite term strm) (princ "]" strm) (ret t)))

(define cleanString (term) (ret (SUBSTITUTE #\space  #\newline (SUBSTITUTE #\'  #\" (SUBSTITUTE #\'  #\\ term)))))
(define cleanVarname (term) (ret (SUBSTITUTE #\_ #\- (SUBSTITUTE #\_ #\?  (SUBSTITUTE #\_ #\:  (SUBSTITUTE #\_ #\|  (SUBSTITUTE #\_ #\%  (SUBSTITUTE #\_ #\#  (SUBSTITUTE #\_ #\$  (SUBSTITUTE #\_ #\`  term))))))))))

(define plstring (term strm) 
   (progn
     (cand (null term) (progn (princ #\" strm) (princ #\" strm) (ret t)))
     (cand (stringp term) (progn (princ #\" strm) (princ (cleanString term) strm) (princ #\" strm) (ret t)))
     (ret (plstring (write-to-string term)))))
     

(define plwrite (term strm) 
  (progn
    (cand (numberp term) (progn (princ term strm) (ret t)))
    (cand (EL-VAR? term) (progn (princ "var(_" strm)(princ (cleanVarname (write-to-string term)) strm)(princ ",'"  strm) (princ term strm)  (princ "')"  strm) (ret t)))
    (cand (stringp term) (progn (plstring term strm) (ret t)))
    (cand (NART-P term) (progn (princ "nart(" strm) (plwrite (NART-EL-FORMULA term) strm) (princ  ")" strm) (ret t)))
    (cand (ASSERTION-P term) (progn (princ "assertionID(" strm) (princ (ASSERTION-ID term) strm) (princ  ")" strm) (ret t)))
    (cand (constant-P term) (progn (princ  "'" strm)(princ  (CONSTANT-NAME term) strm)(princ  "'" strm) (ret t)))
    (cand (null term)  (progn (princ "[]" strm) (ret t)))
    (cand (consp term)  (progn (ret (plcons term strm))))
 ;;   (cand (symbolp term)  (progn  (princ "'" strm) (princ term strm) (princ "'" strm) (ret t)) )
    (cand (symbolp term)  (progn  (princ "svar(_" strm) (princ (cleanVarname (symbol-name term)) strm) (princ ",'" strm) (princ (write-to-string term) strm) (princ "')" strm) (ret t)) )
  ;;  (cand (VARIABLE-P  term) (plwrite (DEFAULT-EL-VAR-FOR-HL-VAR term) strm) (ret t))
    (cand (VARIABLE-P  term)  (progn (princ "_HLVAR" strm) (princ (VARIABLE-ID term) strm) (ret t)))
    (cand (fort-P term)  (progn  (princ "fort('" strm) (princ term strm) (princ  "')" strm) (ret t)))
    (princ #\" strm)(princ "BAD: " strm) (princ (cleanString (write-to-string term)) strm) (princ #\" strm) (ret t)))

(clet ((x 0) (strm (OPEN-BINARY "plassertion9.prolog" :output))) (cdo ((x 1525801 (+ 1 x))) 
  ((= x  (assertion-COUNT)) (print "done")(force-output strm)(close strm))     
  (progn (force-output strm)(cor (= x 1493941) (= x 1491260) (= x 1494062) (= x 1491703)  (= x 1491703)  (= x 1491703) (plassertion x strm)))))
  


(plassertion 0 *STANDARD-OUTPUT*)
(plassertion 29849 *STANDARD-OUTPUT*)
(plassertion 720416 *STANDARD-OUTPUT*)
(plassertion 1129185 *STANDARD-OUTPUT*)
(plassertion 77742 *STANDARD-OUTPUT*)
(plconstant 0 *STANDARD-OUTPUT*)
(plconstant 10 *STANDARD-OUTPUT*)

	

;; Narts are 
(define c2p (term) 
  (progn
    (cand (numberp term) (ret (WRITE-TO-STRING term)))
    (cand (EL-VAR? term) (ret (CCONCATENATE "var(" (STRING-LEFT-TRIM "?" (remove #\- (WRITE-TO-STRING term))) ",'" (WRITE-TO-STRING term) "')" )))
    (cand (stringp term) (ret (CCONCATENATE (PRINC-TO-STRING #\") term (PRINC-TO-STRING #\"))))
    (cand (NART-P term) (ret(CCONCATENATE "nart(" (c2p (NART-EL-FORMULA term)) ")")))
    (cand (ASSERTION-P term) (ret (c2p (ASSERTION-IST-FORMULA term))))
    (cand (constant-P term) (ret (CCONCATENATE "'" (CONSTANT-NAME term) "'" )))
    (cand (null term) (ret "[]"))
    (cand (consp term) (ret (CCONCATENATE "[" (c2p (car term)) "|" (c2p (cdr term)) "]")))
    (ret (CCONCATENATE "'" (WRITE-TO-STRING term) "'" ))))


(clet ((x 0)) (cdo ((x (- (ASSERTION-COUNT) 3) (- 1 x))) ((= x 0) (print "done")(force-output)) (reassert1 x)))
  

(FIND-ASSERTION-BY-ID (- (ASSERTION-COUNT) 1))
(define reassert (asrt) (progn (reassert1 asrt) T))
(define reindex (term) (cdolist (x (all-term-assertions term)) (reassert x)))

(define fwassert (asrt)  (CYC-ASSERT (ASSERTION-IST-FORMULA asrt) (ASSERTION-MT asrt) (list 'QUOTE :STRENGTH :MONOTONIC :DIRECTION :FORWARD)))
(define makeforward (term) (cdolist (x (all-term-assertions term)) (fwassert x)))

(makefw #$arity)

(define reindex (asrt) T)
(define reassert (asrt) T)
;;(reindex #$osim:cyc_bot_1)
(reindex #$preconditionFor-Props )
(reindex #$preconditionFor-PropSit)
(reindex #$causes-SitProp)
(reindex #$preconditionFor-SitTypeSitType)
(reindex #$effectOfAction-Props)
(reindex #$BPV-CurrentStateStaticMt)

;;(reindex #$sufficientFor-Props)
;;(reindex #$preconditionForMethod)
;;(reindex #$arity)
(reindex #$CoABS-OAPreconditionsMt)
(reindex #$actionSequence)
(reindex #$PlanningVocabularyMt)
(reindex #$ProgrammingDomainVocabularyMt)
(reindex #$ProgrammingDomainMt)
(reindex #$planForTask)
;(reindex #$ComputerProgrammer)
;(reindex #$TransportationPlanningMt)
(reindex #$methodForAction)
;(reindex #$resourceTypeAvailable)
;(reindex #$engages-Military)
;(reindex #$TaskInteractionPrerequisitesMt)
(reindex #$CycLToPDDLTranslatorMt)
(reindex #$UniversalVocabularyMt)
(reindex #$CycLSentence)
;(reindex #$WorldGeographyDualistMt)
(reindex #$HPKBVocabMt)


(reindex #$ModernMilitaryTacticsMt)
(reindex #$CycLToPDDLTranslatorMt)
;(reindex #$EasternEuropeanCountry)
;(reindex #$completelyEnumerableCollection)
;(reindex #$genlPreds)
(reindex #$ist)

;(reindex #$purposeInEvent)
;(reindex #$Iran)
;(reindex #$Country)



;(reindex #$TheList)
;(reindex #$actionSequence)
;(reindex #$actionSequence)
;(reindex #$actionSequence)
;(reindex #$actionSequence)


(fi-rename #$ALI_BABA-ACCOUNT-PS  "EDB-ACCOUNT-PS" )
(fi-rename #$ALI_BABA-ACCOUNT-PS  "EDB-ACCOUNT-PS" )
(fi-rename #$ALI_BABA-ENTITY-LS  "EDB-ENTITY-LS" )
(fi-rename #$ALI_BABA-ENTITYATTRIBUTE-LS  "EDB-ENTITYATTRIBUTE-LS" )
(fi-rename #$ALI_BABA-ENTITYATTRIBUTE-PS  "EDB-ENTITYATTRIBUTE-PS" )
(fi-rename #$ALI_BABA-ENTITYATTRIBUTETYPE-LS  "EDB-ENTITYATTRIBUTETYPE-LS" )
(fi-rename #$ALI_BABA-ENTITYATTRIBUTETYPE-PS  "EDB-ENTITYATTRIBUTETYPE-PS" )
(fi-rename #$ALI_BABA-ENTITYGROUP-LS  "EDB-ENTITYGROUP-LS" )
(fi-rename #$ALI_BABA-ENTITYGROUP-PS  "EDB-ENTITYGROUP-PS" )
(fi-rename #$ALI_BABA-ENTITYTYPE-LS  "EDB-ENTITYTYPE-LS" ); new name is "ALI_BABA-ENTITYTYPE-LS"
(fi-rename #$ALI_BABA-ENTITYTYPE-PS  "EDB-ENTITYTYPE-PS" )
(fi-rename #$ALI_BABA-ENTITY_ENTITYTYPE-LS  "EDB-ENTITY_ENTITYTYPE-LS" )
(fi-rename #$ALI_BABA-ENTITY_ENTITYTYPE-PS  "EDB-ENTITY_ENTITYTYPE-PS" )
(fi-rename #$ALI_BABA-EVENT-LS  "EDB-EVENT-LS" )
(fi-rename #$ALI_BABA-EVENT-PS  "EDB-EVENT-PS" )
(fi-rename #$ALI_BABA-GPE-LS  "EDB-GPE-LS" )
(fi-rename #$ALI_BABA-GPE-PS  "EDB-GPE-PS" )
(fi-rename #$ALI_BABA-LINKATTRIBUTE-LS  "EDB-LINKATTRIBUTE-LS" )
(fi-rename #$ALI_BABA-LINKATTRIBUTE-PS  "EDB-LINKATTRIBUTE-PS" )
(fi-rename #$ALI_BABA-LINKATTRIBUTETYPE-LS  "EDB-LINKATTRIBUTETYPE-LS" )
(fi-rename #$ALI_BABA-LINKATTRIBUTETYPE-PS  "EDB-LINKATTRIBUTETYPE-PS" )
(fi-rename #$ALI_BABA-LINKGROUP-PS  "EDB-LINKGROUP-PS" )
(fi-rename #$ALI_BABA-LINKGROUPMEMBER-PS  "EDB-LINKGROUPMEMBER-PS" )
(fi-rename #$ALI_BABA-LINK_LINKTYPE-PS  "EDB-LINK_LINKTYPE-PS" )
(fi-rename #$ALI_BABA-LOCATION-PS  "EDB-LOCATION-PS" )
(fi-rename #$ALI_BABA-ORGANIZATION-LS  "EDB-ORGANIZATION-LS" )
(fi-rename #$ALI_BABA-ORGANIZATION-PS  "EDB-ORGANIZATION-PS" )
(fi-rename #$ALI_BABA-PERSON-LS  "EDB-PERSON-LS" )
(fi-rename #$ALI_BABA-REPORT-PS  "EDB-REPORT-PS" )
(fi-rename #$ALI_BABA-REPORT_SOURCE-PS  "EDB-REPORT_SOURCE-PS" )
(fi-rename #$ALI_BABA-SOURCE-PS  "EDB-SOURCE-PS" )
(fi-rename #$EDBAliBabaPhysicalSchemaMt  "EDBPhysicalSchemaMt" )


Warning: NAME change for "EDB-ACCOUNT-PS" ; new name is "ALI_BABA-ACCOUNT-PS"
Warning: NAME change for "EDB-ENTITY-LS" ; new name is "ALI_BABA-ENTITY-LS"
Warning: NAME change for "EDB-ENTITY-PS" ; new name is "ALI_BABA-ENTITY-PS"
Warning: NAME change for "EDB-ENTITYATTRIBUTE-LS" ; new name is "ALI_BABA-ENTITYATTRIBUTE-LS"
Warning: NAME change for "EDB-ENTITYATTRIBUTE-PS" ; new name is "ALI_BABA-ENTITYATTRIBUTE-PS"
Warning: NAME change for "EDB-ENTITYATTRIBUTETYPE-LS" ; new name is "ALI_BABA-ENTITYATTRIBUTETYPE-LS"
Warning: NAME change for "EDB-ENTITYATTRIBUTETYPE-PS" ; new name is "ALI_BABA-ENTITYATTRIBUTETYPE-PS"
Warning: NAME change for "EDB-ENTITYGROUP-LS" ; new name is "ALI_BABA-ENTITYGROUP-LS"
Warning: NAME change for "EDB-ENTITYGROUP-PS" ; new name is "ALI_BABA-ENTITYGROUP-PS"
Warning: NAME change for "EDB-ENTITYTYPE-PS" ; new name is "ALI_BABA-ENTITYTYPE-PS"
Warning: NAME change for "EDB-ENTITY_ENTITYTYPE-LS" ; new name is "ALI_BABA-ENTITY_ENTITYTYPE-LS"
Warning: NAME change for "EDB-ENTITY_ENTITYTYPE-PS" ; new name is "ALI_BABA-ENTITY_ENTITYTYPE-PS"
Warning: NAME change for "EDB-EVENT-LS" ; new name is "ALI_BABA-EVENT-LS"
Warning: NAME change for "EDB-EVENT-PS" ; new name is "ALI_BABA-EVENT-PS"
Warning: NAME change for "EDB-GPE-LS" ; new name is "ALI_BABA-GPE-LS"
Warning: NAME change for "EDB-GPE-PS" ; new name is "ALI_BABA-GPE-PS"
Warning: NAME change for "EDB-LINKATTRIBUTE-LS" ; new name is "ALI_BABA-LINKATTRIBUTE-LS"
Warning: NAME change for "EDB-LINKATTRIBUTE-PS" ; new name is "ALI_BABA-LINKATTRIBUTE-PS"
Warning: NAME change for "EDB-LINKATTRIBUTETYPE-LS" ; new name is "ALI_BABA-LINKATTRIBUTETYPE-LS"
Warning: NAME change for "EDB-LINKATTRIBUTETYPE-PS" ; new name is "ALI_BABA-LINKATTRIBUTETYPE-PS"
Warning: NAME change for "EDB-LINKGROUP-PS" ; new name is "ALI_BABA-LINKGROUP-PS"
Warning: NAME change for "EDB-LINKGROUPMEMBER-PS" ; new name is "ALI_BABA-LINKGROUPMEMBER-PS"
Warning: NAME change for "EDB-LINK_LINKTYPE-PS" ; new name is "ALI_BABA-LINK_LINKTYPE-PS"
Warning: NAME change for "EDB-LOCATION-LS" ; new name is "ALI_BABA-LOCATION-LS"
Warning: NAME change for "EDB-LOCATION-PS" ; new name is "ALI_BABA-LOCATION-PS"
Warning: NAME change for "EDB-ORGANIZATION-LS" ; new name is "ALI_BABA-ORGANIZATION-LS"
Warning: NAME change for "EDB-ORGANIZATION-PS" ; new name is "ALI_BABA-ORGANIZATION-PS"
Warning: NAME change for "EDB-PERSON-LS" ; new name is "ALI_BABA-PERSON-LS"
Warning: NAME change for "EDB-PERSON-PS" ; new name is "ALI_BABA-PERSON-PS"
Warning: NAME change for "EDB-REPORT-PS" ; new name is "ALI_BABA-REPORT-PS"
Warning: NAME change for "EDB-REPORT_SOURCE-PS" ; new name is "ALI_BABA-REPORT_SOURCE-PS"
Warning: NAME change for "EDB-SOURCE-PS" ; new name is "ALI_BABA-SOURCE-PS"
Warning: NAME change for "EDBPhysicalSchemaMt" ; new name is "EDBAliBabaPhysicalSchemaMt"
loading partition constant shells
