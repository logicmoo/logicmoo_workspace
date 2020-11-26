;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tools & utilities for PrologMUD
;;
;; !!!!!!!!!!!!!!!
;; IMPORTANT NOTES:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External interface:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Depends on interface: 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;}}}EDOC
(define FORCE-PRINT (string) (print string) (force-output))


(defmacro kif-defvar (n v) 
  (ret `(progn (defparameter ,n ,v) (csetq ,n ,v) (defvar ,n ,v))))

(kif-defvar *every1000* 1)
(kif-defvar *file-output* *standard-output*)


(define mud-rename (trm str)
 (clet ((found (gethash trm *renames* )))
   (pwhen (stringp found) (ret found))
 
 (pwhen (null str) (ret ()))
 (punless (stringp str) (ret ()) (throw `(must-stringp ,str)))

 (pwhen (gethash trm *renames*) (ret ()))
 (punless (stringp str) (csetq str (string str)))
 (sethash trm *renames* str)
 (sethash str *kif-to-cycl-hashtable* trm)
 (pwhen (cor () (equal (rem (cinc  *every1000*) 10000) 1))  (print `(cyc-rename ,trm ,str) *standard-output*)(force-output *standard-output*))
  ;; (plw-str  "forward_default_d('iBookkeepingMt','oldConstantName'," ) (plw-atom str ) (plw-str  ",") (plw-str (write-to-string (constant-name trm))) (plw-str  ")." ) (plw-nl)   
 (fi-assert `(#$oldConstantName ,trm ,(constant-name trm)) '#$BookkeepingMt)
 (cyc-rename trm str)
 ;; (set-dispatch-macro-character #\# #\$ #'FIND-RENAMED-CONSTANT-RMF)
 ))
   

(define result-isa-p (fn type)
 (punless (function? fn) (ret ()))
 (pwhen (askm `(#$resultGenl ,fn ,type)) (ret #$resultGenl))
 (pwhen (askm `(#$and (#$resultGenl ,fn ?RESULT) (#$isa ?RESULT ,fn ))) (ret t))
 (pwhen (askm `(#$resultIsa ,fn ,type)) (ret #$resultIsa))
 (pwhen (askm `(#$and (#$hlPrototypicalInstance ,fn ?RESULT) (genls ?RESULT ,type))) (ret #$resultIsa))
 )
;; (guess-rename #$ThePrototypicalFixedArityFunction)


(define offer-prefix (trm prefix)
 (punless (constant-p trm) (ret nil)) 
 (pwhen (gethash trm *renames*) (ret ()))
 (csetq *const* trm)
 (clet ((str (constant-name trm)) (woDashes (remove-dashes str)))
   (pwhen (equal (search prefix str) 0) (ret ()))
   (pwhen (LOWER-CASE-P (CHAR str 0)) (ret ()))
   (punless (atom-need-quote str) (ret ())) 
   (pwhen (function? trm) (punless (search "nF" (REVERSE woDashes))(csetq woDashes (cconcatenate woDashes "Fn"))))
 (mud-rename trm (cconcatenate prefix woDashes))))

(kif-defvar *renames* (MAKE-HASH-TABLE 336790 ))

(kif-defvar *typeprefixes* '())

(csetq  *typeprefixes-old* `(
(#$Action "act" "tAct" "iAct_")(#$Goal  "goal" "tGoal" "iGoal_" )(#$Capability  "cap"  "tCap" "cap")(#$IBTContentType "tIbo" "tIbo" "ibo")(#$Event  "tEvent" "tEvent" "iEvent_")  (#$Situation  "state" "tState" "iState_")(#$FormulaTemplate  "ui_" "uiTopic_" "iUI_")(#$Topic  "ui_" "uiTopic_" "iUI_")
 (#$PhysicalPartOfObject "tPartType" "tPartType" "iPartType_")
 ;;KB7166 (#$SpecifiedPartTypeCollection "ttPartType" "ttPartType" "tPartType")
 (#$OrganismClassificationType  "tTypeOf" "ttTypeOf" "tClassificationOf_")(#$ConventionalClassificationType  "tTypeOf" "ttTypeOf" "tClassificationOf_")(#$SpaceRegion "tPlace"  "tPlaceLike" "iLoc_")(#$TwoDimensionalThing     "tPlacePartOf"  "tPlacePartOf" "iLocPart_") ;; (#$AbstractVisualStructure "tPlacePartOf"  ttPlacePartOf" "iLocPart_")
 (#$OpenGISGeometry         "tPlacePartOf"  "tPlacePartOf" "iLocPart_") ;; time can be in these forms:  TimePoint,Interval/Segment,  
(#$TimeInterval  "timeOf" "timeOf"  "iTimeOf_")(#$TimeParameter  "timeOf" "timeOf" "iTimeOf_")(#$ScalarInterval  "v" "vt" nil)(#$Quantity  "quant" "vt" "v")(#$LinguisticObject "x" "xt" "x");; (#$TemplateParsingSyntacticConstraint "x" "xt" "x")
(#$LinguisticObjectType "xt" "xt" "xt")(#$PersonTypeByActivity  "mob" "mob" "mob")(#$Predicate  "iPred_" "rt" nil)(#$Function-Denotational  "iFunc_" "rt" nil)(#$Relation  "iRel_" "rt" nil)
(#$PredicateType  "rt" "ttRel" "rt")(#$FunctionType  "iFunc_" "ttRel" "rt")(#$RelationshipType  "iRel_" "ttRel" "rt")(#$SubLExpressionType  "ft" "ft" "ft")
(#$Group  "tGrouped" "tGrouped" "iGroup_")(#$Artifact  "tObject" "tObject" "iObj_")  (#$Place  "tPlace"  "ttPlaceLike" "iLoc_")(#$Microtheory  "mt" "mt" "mt")
(#$PropositionalConceptualWork "cw" "cw" "iCW_")(#$ConceptualWork "cw" "cw" "iCW_")(#$Agent-Generic  "mob" "mob" nil)(#$InformationBearingObject "ibo" "ibo" "iIBO_")
(#$AnalogyEntityWithStructuralEvaluation "cw" "cw" "iCW_")(#$TheList "u" "ut" "u") ;; (#$Tuple  "v" "vt" "v")
(#$Tuple  "u" "ut" "u")(#$ProgramObject "s_cw" "s_cw" "s_iCW_")(#$Field-LogicalOrPhysical "s_db" "s_db" "s_iDB_")(#$Schema-LogicalOrPhysical "s_db" "s_db" "s_iDB_")(#$CycSubjectClump "ui_" "uiTopic_" "iUI_")))


(csetq  *typeprefixes* `(
(#$Action "iAct" "tAct" "iAct")
(#$Goal  "iGoal" "tGoal" "iGoal" )
(#$Capability  "tCap"  "ttCap" "iCap")
(#$IBTContentType "tIbo" "ttIbo" "ibo")
(#$Event  "iEvent" "tEvent" "iEvent")  
(#$Situation  "iState" "tState" "iState")
(#$FormulaTemplate  "iUi" "tUiTopic" "iUI")
(#$Topic  "iUi" "tUiTopic" "iUI")
(#$PhysicalPartOfObject "tPartType" "ttPartType" "iPartType")
;;KB7166 (#$SpecifiedPartTypeCollection "ttPartType" "ttPartType" "tPartType")
(#$OrganismClassificationType  "tTypeOf" "ttTypeOf" "tClazz")
(#$ConventionalClassificationType  "tTypeOf" "ttTypeOf" "tClazz")

(#$SpaceRegion "iPlace"  "ttPlaceLike" "iLoc")

(#$TwoDimensionalThing     "iPlacePartOf"  "tPlacePartOf" "iLocPart")
;; (#$AbstractVisualStructure "tPlacePartOf"  ttPlacePartOf" "iLocPart")
(#$OpenGISGeometry         "iPlacePartOf"  "tPlacePartOf" "iLocPart")

;; time can be in these forms:  TimePoint,Interval/Segment,  
(#$TimeInterval  "iTime" "tTime"  "iTimeOf")
(#$TimeParameter  "iTimeOf" "tTimeOf" "iTimeOf")
(#$ScalarInterval  "v" "vt" nil)
(#$Quantity  "v" "vt" "v")
(#$LinguisticObject nil "xt" "x")
; (#$TemplateParsingSyntacticConstraint "x" "xt" "x");
(#$LinguisticObjectType "xt" "xt" "xt")
(#$PersonTypeByActivity  "tMob" "ttMob" "iMob")
(#$Predicate  "iPred" "rt" nil)
(#$Function-Denotational  "iFunc" "rt" nil)
(#$Relation  "iRel" "rt" nil)
(#$PredicateType  "rt" "ttRel" "rt")
(#$FunctionType  "iFunc" "ttRel" "rt")
(#$RelationshipType  "iRel" "ttRel" "rt")
(#$SubLExpressionType  "ft" "ft" "ft")
(#$Group  "iGrouped" "tGrouped" "iGroup")
(#$Artifact  "iObj" "tObject" "iObj")  
(#$Place  "iPlace"  "tPlaceLike" "iLoc")
(#$Microtheory  "iMt" "ttMt" "i")
(#$PropositionalConceptualWork "iCw" "tCw" "iCW")
(#$ConceptualWork "iCw" "tCw" "iCW")
(#$Agent-Generic  "iMob" "tMob" "i")
(#$InformationBearingObject "iIbo" "tIbo" "iIBO")
(#$AnalogyEntityWithStructuralEvaluation "iCw" "tCw" "iCW")

(#$TheList "u" "ut" "u")
;; (#$Tuple  "v" "vt" "v")
(#$Tuple  "u" "ut" "u")
(#$ProgramObject "s_cw" "s_cw" "s_iCW")
(#$Field-LogicalOrPhysical "s_db" "s_db" "s_iDB")
(#$Schema-LogicalOrPhysical "s_db" "s_db" "s_iDB")
(#$CycSubjectClump  "iUi" "tUiTopic" "iUI")
))


(define save-ocn ()  
  (cdolist (ab (ask-template '(?STR . ?CONST) `(#$oldConstantName ?CONST ?STR) #$EverythingPSC))
   (format t "~&(maybe-const-renamed ~s ~s)~%"  (car ab) (constant-name (cdr ab))))
 
  (cdolist (ab (ask-template '(?STR . ?CONST) `(#$mergedConstantName ?CONST ?STR) #$EverythingPSC))
   (format t "~&(maybe-const-merged ~s ~s)~%"  (car ab) (constant-name (cdr ab))))
  )

    
  

; ' (result-isa #$LocalTimeFn)

(define isa-p (i c)
 (pwhen (isa? i c) (ret 'isa?))
 (pwhen (askm `(#$quotedIsa ,i ,c)) (ret #$quotedIsa))
 (pwhen (askm `(#$isa ,i ,c)) (ret #$isa))
 ;(pwhen (sentence-clauses `(#$isa ,i ,c)) (ret #$isa))
 (cdolist (e (isa i)) (pwhen (askm `(#$genls ,e ,c)) (ret #$isa)))
)

(define genls-p (i c)
 (pwhen (genls? i c) (ret 'genls?))
 (pwhen (askm `(#$genls ,i ,c)) (ret #$genls))
; (pwhen (sentence-clauses `(#$genls ,i ,c)) (ret #$genls))
; (cdolist (e (genls i)) (pwhen (askm `(#$genls ,e ,c)) (ret #$genls)))
)
  

(define guess-fn-prefix (trm)
 (punless (function? trm) (ret nil))
 (punless (atom-need-quote trm) (ret nil))

 (pcond
  ((isa-p trm #$NLFunction) (ret "x"))
  ((isa-p trm #$UnitOfMeasureDenotingFunction) (ret "v"))
  )
 (cdolist (ts *typeprefixes*)
  (pwhen (second ts) 
   (clet ((why (result-isa-p trm (car ts))))
     (pwhen why  
	 (pwhen (equal #$resultIsa why)
              (pwhen (fourth ts) (fourth ts)))
     (ret  (second ts) )))))

 (pcond
  ((isa-p trm #$FunctionDenotingFunction) (ret "iFunc"))
  ((isa-p trm #$PredicateDenotingFunction) (ret "iPred"))
  ((result-isa-p trm #$SetOrCollection) (ret "tSetOf"))  
  ((isa-p trm #$SetDenotingFunction) (ret "tSetOf"))
  ((isa-p trm #$MicrotheoryDesignatingFunction-Denotational) (ret "iMt"))  
  ((result-isa-p trm #$CollectionType) (ret "ttColOf"))
  ((result-isa-p trm #$Collection) (ret "tColOf"))
  ((isa-p trm #$SetOrCollectionDenotingFunction) (ret "tColOf"))

  ;;KB7166 ((isa-p trm #$CodingFunctionTypeByOperationType) (ret "s_iCW"))
  ;;KB7166 ((isa-p trm #$CodingFunction) (ret "s_iCW"))
  ((isa-p trm #$ProgramFunction) (ret "s_iCW"))
  ;;KB7166 ((isa-p trm #$StandardLibraryFunction) (ret "s_iCW"))
  ((result-isa-p trm #$Field-LogicalOrPhysical) (ret "tSetOf"))
  )

(pcond
  ((result-isa-p trm #$SpatialThing-Localized) (ret "i"))
  ((isa-p trm #$RelationDenotingFunction) (ret "iRel"))
  ((isa-p trm #$UnreifiableFunction) (ret "u"))
  ((isa-p trm #$IndividualDenotingFunction) (ret "ir"))
  ;; ((isa-p trm #$ReifiableFunction) (ret "r"))
  ;; ((isa-p trm #$Function-Denotational) (ret "f"))
  ;; (t (ret ()))
 )
)

(define guess-rename-cached (trm &optional str)
 (punless (constant-p trm) (ret nil)) 
 (pwhen (gethash trm *renames*) (ret ()))
 (punless str (csetq str (constant-name trm)))
 (ret (WITH-ANY-MT (with-all-mts (guess-rename trm str)))))

(define atom-need-quote (str) 
 (pwhen (constant-p str) (csetq str (constant-name str)))
 (punless (equal (SUBSTITUTE  #\_ #\- str) str)
   (ret t)) (clet ((ch (char str 0)))
    (pwhen (member ch '(#\: #\' #\` #\#) #'char=) (ret nil))
     (ret (cor (cnot (lower-case-p ch)) (find #\: str)))))

(define guess-rename (trm &optional str prefixstr)
 (punless str (csetq str (constant-name trm)))
 (csetq str (remove-dashes str))
 (pwhen (predicate? trm) (ret str))
 (pwhen (function? trm)  
   (punless (search "nF"(REVERSE str))(csetq str (cconcatenate str "Fn")))
   (csetq prefixstr (guess-fn-prefix trm))
   (pwhen (stringp prefixstr)
     (ret (cconcatenate prefixstr str)))
    (ret ())
  )

 ;; (punless (atom-need-quote str) (pwhen (equal str (constant-name trm)) (ret ())))

 (pwhen (isa-p trm #$LearnedActivityType) (ret (cconcatenate "tAct" str )))


 (pwhen (collection? trm)
   (cdolist (ts *typeprefixes*)
     (pwhen (cand (third ts) (genls-p trm (car ts))) (ret (cconcatenate (third ts) str )))))

 (csetq prefixstr (guess-prefix2 trm))
 (pwhen prefixstr (ret (cconcatenate prefixstr str)))
 (force-print (cconcatenate "Unable to guess a better name for #$" (constant-name trm)))
 (ret ()))
 

(define guess-prefix2 (trm)
 (WITH-ANY-MT (with-all-mts (ret (guess-prefix3 trm)))))

(define guess-prefix3 (trm)
  (csetq *const* trm)

  (pwhen (function? trm) 
   (clet ((fnstr (guess-fn-prefix trm)))
        (pwhen fnstr (ret fnstr))
   (ret ())))

 (pcond
 ((isa-p trm #$RelationshipType) (ret "rt"))
 ((isa-p trm #$MicrotheoryType) (ret "ttMt"))
 ((genls-p trm #$FormulaTemplateTopicType) (ret "ttUiTopic"))
 ((isa-p trm #$CycLExpressionType) (ret "ft"))
    
 ((isa-p trm #$TotallyOrderedQuantityType) (ret "vt"))
 ((isa-p trm #$QuantityType) (ret "vt"))
    
 ((genls-p trm #$ObjectTypeBySensibleFeature) (ret "vt"))
 ((isa-p trm #$ObjectTypeBySensibleFeature) (ret "v"))

 ((genls-p trm #$VariedOrderCollection) (ret "vt"))
    
 ((cand (isa-p trm #$LinguisticObject)(isa-p trm #$Collection)) (ret "xt"))
 )

 (cdolist (ts *typeprefixes*)
  (pwhen (cand (fourth ts) (isa-p trm (car ts))) (ret (fourth ts) )))

 (pcond    
  ((genls? trm #$Collection) (ret "tt"))
  ((isa-p trm #$Collection) (ret "t"))

 ((isa-p trm #$SpatialThing) (ret "i"))
 ((isa-p trm #$TemporalThing) (ret "i"))
 ((isa-p trm #$VectorInterval) (ret "v"))
    
 ((isa-p trm #$UnitVectorInterval) (ret "v"))
 )
 (ret ())
   
;; (pwhen (isa-p trm #$Individual) (ret "i"))
;; (pwhen (all-genls trm) (ret "t"))
;; (ret "i")
)

(force-print #'guess-rename)

(define remove-dashes (str)
(clet ((dash (position #\- str)))
 (punless dash (ret str))
 (pwhen (search "--" str)
 (ret (SUBSTITUTE #\_ #\- str)))

 (clet ((dash1 (1+ dash)) (left (subseq str 0 dash)) (right (subseq str dash1)) )
 (pwhen (equal right "") (ret (SUBSTITUTE #\_ #\- str)))
 (clet ((ch (char-upcase (CHAR right 0))))
 (punless (ALPHA-CHAR-P ch)
   (ret (SUBSTITUTE #\_ #\- str)))
 (ret (remove-dashes (cconcatenate left (string (set-char right 0 ch)))))))))

(force-print #'remove-dashes)

(define maybe-rename (trm)
 (punless (constant-p trm) (ret nil)) 
 (pwhen (gethash trm *renames*) (ret ()))
 (csetq *const* trm)
 (clet ((str (constant-name trm)) (newname (with-prefix trm)))
 (pwhen (null newname)
   (ret nil))
 (mud-rename trm newname)
 (ret newname)))

(force-print #'maybe-rename)

#|
 (pwhen (equal (search "vt" newname) 0)
	 (cdolist ( v (all-instances trm))
	    (offer-prefix v "v")))
 (pwhen (equal (search "ct" newname) 0)
	 (cdolist ( v (all-instances trm))
	    (offer-prefix v "c")))
 (pwhen (equal (search "xt" newname) 0)
	 (cdolist ( v (all-instances trm))
	    (offer-prefix v "x")))
|#


(define with-prefix (trm)
 (WITH-ANY-MT (with-all-mts
  (punless (constant-p trm) (ret nil)) 
 (pwhen (gethash trm *renames*) (ret ()))
 (csetq *const* trm)
 (clet ((new (guess-rename-cached trm)))
    (punless (stringp new) (ret nil))
   (ret new)))))
     

(define askm (query) 
 (WITH-ANY-MT (with-all-mts 
   (clet  ((res (cyc-query query #$EverythingPSC))) 
     (pwhen res (ret res)))
)))



(define READ-UNTIL (quit-chars &optional (stream *STANDARD-INPUT*)(retstr ""))
   ;;  Read up to the char specified
     (pwhen (stringp quit-chars) (csetq quit-chars (STRING-TO-CHAR-LIST quit-chars)))
     (cdo ((lastchar (read-char stream) (read-char stream)))
          ((cand (member lastchar quit-chars) (> (length retstr) 0))
	          (unread-char lastchar stream )
	          (ret (values retstr lastchar)))
        (csetq retstr (cconcatenate retstr (string lastchar)))))


(define READ-UNTIL-ORIG (quit-chars &optional (stream *STANDARD-INPUT*)(retstr ""))
   ;;  Read up to the char specified
     (cdo ((lastchar (read-char stream)(read-char stream))) 
          ((member lastchar quit-chars)(unread-char lastchar stream )(ret (values retstr lastchar)))
        (csetq retstr (cconcatenate retstr (string lastchar)))))

(defmacro kif-warn (&rest code)(ret 
    `(with-error-handler 
         #'(lambda ()(format t 
	 "~& ;; kif-warn: ~s ~s ~&" *ERROR-MESSAGE* ',code)(fresh-line)(force-output)(fresh-line)(force-output)(kif-break *ERROR-MESSAGE*))
     (sl::progn ,@code))))

(define kif-break (&rest data)   
      (apply #'format (cons 't data)) (fresh-line)(force-output) (fresh-line)(force-output))

; (cdo ((*x* (nart-count) (+ *x* 1))) ((= *x* (constant-count))) (fi-assert `(,(foc "plainCycConstant") ,(find-constant-by-internal-id *x*)) '#$UniversalVocabularyMt))

(kif-defvar *kif-to-cycl-hashtable-init* '(
;; ("Nima-Gns-KS" . #$NGA-Gns-KS ) 
("forall" . #$forAll )
("exists" . #$thereExists )
("=>" . #$implies )
("<=>" . #$equiv )
("isa" . #$isa )
("instance-of" . #$isa )
("listof" . #$TheList )
("true" . #$True )
("false" . #$False )
))

(kif-defvar *T-READTABLE* (COPY-READTABLE *READTABLE*))
(kif-defvar *LEFT-PAREN-READER* (get-macro-character (char "()"  0)  *T-READTABLE*))  
(kif-defvar *kif-old-hash-dollar-reader* (GET-DISPATCH-MACRO-CHARACTER #\# #\$ *T-READTABLE*))
(kif-defvar *POP-PACKAGE* *PACKAGE*)
(kif-defvar *T-PACKAGE* (fif (find-package "EXPORTING")(find-package "EXPORTING")(make-package "EXPORTING" () ())))
(kif-defvar *UVMt* (find-constant "UniversalVocabularyMt"))
(kif-defvar *BASEKB* (find-constant "BaseKB"))
(kif-defvar *kif-KIFImport* (find-or-create-constant "KIFImport"))
(kif-defvar *kif-KIFImportRepairing* (find-or-create-constant "KIFImportRepairing"))
(ke-assert-now `(#$isa ,*kif-KIFImport*  #$Cyc-BasedProject) *UVMt*)
(ke-assert-now `(#$isa ,*kif-KIFImportRepairing*  #$Cyc-BasedProject) *UVMt*)
(kif-defvar *kif-author* (find-or-create-constant "KIFCyclist"))
(kif-defvar *kif-constant* (find-or-create-constant "KIFConstant"))
(kif-defvar *kif-mt* (find-or-create-constant "KIFLoadingMt"))

(kif-defvar *kif-not-wff* ())
(kif-defvar *KIF-REPAIRS* ())
(kif-defvar *KIF-REPAIRS-FAILED* ())
(kif-defvar *VocabularyMt* *UVMt*)
(kif-defvar *bookkeeping-info* (new-bookkeeping-info *the-cyclist* (the-date) *KE-PURPOSE* (the-second)))
(ke-assert-now `(#$isa ,*kif-mt*  #$Microtheory) *UVMt*)
(ke-assert-now `(#$isa ,*kif-author*  #$Cyclist) *UVMt*)
(ke-assert-now `(#$isa ,*kif-constant*  #$SubLExpressionType) *UVMt*)
(ke-assert-now `(#$genls ,*kif-constant*  #$CycLConstant) *UVMt*)

(define assoc-to-hash-table (alist &optional ht)
 (punless ht (csetq ht (make-hash-table 20)))
 (cdolist (pair alist)
    (sethash (car pair) ht (cdr pair)))
  (ret ht))

;; todo use #'string-equal
(kif-defvar *kif-to-cycl-hashtable* (assoc-to-hash-table *kif-to-cycl-hashtable-init* (make-hash-table 20 #'equalp) ))

(define string-to-char-list (str &optional (charlist ()))
    (cdo ((i (- (length str) 1) (1- i))) ((= i -1))
      (csetq charlist (cons (char str i) charlist)))
      (ret charlist))

(kif-defvar *symbol-break-chars* (cons #.(code-char 0) (append (string-to-char-list "()" SL::*WHITESPACE-1-CHARS*))))

(define read-until-symbol-end (&optional (stream *STANDARD-INPUT*)) (ret (read-until *symbol-break-chars* stream "" )))

(define frc (cycl &optional guid can-create)
 (punless (stringp cycl) (throw cycl cycl))
 (pwhen (equal 0 (search "#$" cycl)) (ret (frc (substring cycl 2) guid)))
 (pwhen (equal 0 (search "?" cycl)) (ret (intern cycl)))
 (pwhen (equal 0 (search "@" cycl)) (ret (intern (cconcatenate "?" (substring str 1)))))
 (clet ((constant nil))
    ;;(csetq constant (second (member-if #'(lambda (a) (ret (cand (stringp a) (string-equal a cycl)))) *kif-to-cycl-table-plist*)))
    (csetq constant (gethash cycl *kif-to-cycl-hashtable*))
    (pwhen constant (ret constant))
    (csetq constant (find-constant cycl))
    (pwhen constant (ret constant))
    (csetq constant (find-constant-by-name cycl))    
    (pwhen constant (ret constant))
    (csetq constant (car (old-constant-names cycl)))
    (pwhen constant (fi-assert `( #$oldConstantName ,constant ,cycl) #$BookkeepingMt) (ret constant))
    (csetq constant (ket-maybe-find-constant cycl))
    (pwhen constant (ret constant))
    (csetq constant (car (ask-template '?TERM `(#$oldConstantName ?TERM ,cycl) #$EverythingPSC)))
    (pwhen constant (fi-assert `(#$oldConstantName ,constant ,cycl) #$BookkeepingMt) (ret constant))
    (csetq constant (car (ask-template '?TERM `(#$mergedConstantName ?TERM ,cycl) #$EverythingPSC)))
    (pwhen constant (fi-assert `(#$mergedConstantName ,constant ,cycl) #$BookkeepingMt) (ret constant))
    (csetq constant (car (ask-template '?TERM `(#$mergedConstantGUID ?TERM ,cycl) #$EverythingPSC)))
    (pwhen constant (ret constant))
    (pwhen (equal 0 (string= (string-upcase cycl) cycl))(force-print `(assuming ,cycl))(ret (intern cycl)))
    ;;(pwhen guid (csetq constant (kif-by-id guid)) (pwhen constant (ret constant)))
    (csetq constant (car (constant-name-case-collisions cycl))) 
    (pwhen constant (fi-assert `(#$mergedConstantName ,constant ,cycl) #$BookkeepingMt)  (ret constant))
    (csetq constant (car (ask-template '?TERM `(#$termStrings ?TERM ,cycl) #$EverythingPSC)))

    (pwhen constant 
      ;;(fi-assert `(#$termStrings ,constant ,cycl) *UVMT*) 
      (ret constant))

    (pwhen (constant-name-available cycl)
        (pwhen can-create 
	 (csetq constant (kif-warn (create-constant cycl)))
         (pwhen constant (fi-assert `(#$quotedIsa ,constant ,*kif-constant*) *UVMT*) (ret constant))))
    ;;(csetq constant (kif-dwim cycl))
    (kif-break "~&~& kif-constant: ~s" `(,cycl ,guid ,constant))
    ;; (punless constant (csetq constant .... ))
    (ret constant)))

(define FIND-RENAMED-CONSTANT-RMF (stream c n )
   (clet ( symbol str (stream (fif (streamp stream) stream *STANDARD-INPUT*)))
     ;; (force-print stream c n)
      (cunwind-protect
       (progn
         (csetq str  (read-until-symbol-end stream))
         (csetq symbol (frc str)))
         ;;unwound to
         ())
         (ret (values symbol T))))


; (cdo ((*x* (nart-count) (+ *x* 1))) ((= *x* (constant-count))) (fi-assert `(,(foc "plainCycConstant") ,(find-constant-by-internal-id *x*)) '#$UniversalVocabularyMt))




(define namet()
 (mud-rename #$Collection "tCol")
 (mud-rename #$Event "tEvent")
 (mud-rename #$Individual "tIndividual")
 (mud-rename #$Simply "xSimplyOfFn")
 (mud-rename #$TheFn "iTheOfFn") 
 (mud-rename #$The "rIsTheFn") 
 ;;kb7166 (mud-rename #$The-Unreifiable "uUnreifiableTheFn") 
 (cdolist (trm (ALL-INSTANCES #$Function-Denotational)) (maybe-rename trm))
 (offer-prefix #$ThePrototypicalFixedArityFunction "uf")
  ;;kb7166(offer-prefix #$ThePrototypicalSextaryFunction "uf")
  ;;kb7166(offer-prefix #$ThePrototypicalQuintaryFunction "uf")
 (offer-prefix #$SkolemFuncNFn "r")
 (offer-prefix #$SubPathBetweenFn "iLoc")
 (offer-prefix #$AbducedTermFn "r")
 ;;kb7166(offer-prefix #$MathBugTypeFn "tColOf")
 (offer-prefix #$CNInstanceNamedFn "r")
 ;;kb7166(offer-prefix #$MaximalWithRespectToDurationFn "v")
 ;;kb7166(offer-prefix #$MinimalWithRespectToDurationFn "v")
 (offer-prefix #$IndexicalReferentFn-EvaluateAtEL-Placeholder "x")
 (offer-prefix #$TheTermToSolveForFn "u")
 (offer-prefix #$IntensionOfQuerySentenceFn "iCW")
 (offer-prefix #$ObjectDenotedByFn "x")
 (offer-prefix #$RequireWithinPartitionCorrespondencesFn "tColOf")
 (offer-prefix #$RequiredBaseCorrespondenceForMappingFn "tColOf")
 (offer-prefix #$ExcludedCross-PartitionCorrespondencesFn "tColOf")
 (offer-prefix #$RequiredTargetCorrespondenceForMappingFn "tColOf")
 (offer-prefix #$FocusedTermRankFn "tSetOf")
 ;;kb7166(offer-prefix #$AmountOfSubstanceFn "v")
 ;;kb7166(offer-prefix #$TableCellValueFn "s_iDB")
 (offer-prefix #$FieldValueFn "s_iDB")
 ;;kb7166(offer-prefix #$RDFTypedLiteralFn "r")
 ;;kb7166(offer-prefix #$GeneLocusFn "iPlace")
 (offer-prefix #$CiFn "iCW")
 (offer-prefix #$AnalogyMappingIDFn "iCW")
 (offer-prefix #$Reminding-AnalogyFn "iCW")
 (offer-prefix #$AnalogyMatcherFn "iCW")
 (offer-prefix #$ExecutableFunctionFn "s_iCW")
 
)

(define namet2()
 (cdolist (trm (ALL-INSTANCES #$LearnedActivityType)) (offer-prefix trm "tAct"))
 (cdolist (ts *typeprefixes*)
    (cdolist (trm (ALL-SPECS (car ts))) (pwhen (constant-p trm) (offer-prefix trm (third ts))))
    (cdolist (trm (ALL-INSTANCES (car ts))) (pwhen (constant-p trm) (offer-prefix trm (fourth ts))))
    (cdolist (trm (ALL-INSTANCES #$ObjectTypeBySensibleFeature) (offer-prefix trm "vt"))))

( do-constants (trm) (maybe-rename trm)))

;;  City
;; (LowerInchOfFn (WriteableSurfaceFn (BoundryFn (RegionFn (CityFn (WhereRulesFn "Bud Clark"))))))
;; (properNameString BudClarkTheFlasher "Bud Clark") ;; arg1 =Person
;; (mayorOf BudClarkTheFlasher CityOfPortlandInc) ;; PoliticalAgent
;; (controls CityOfPortlandInc CityOfPortland3D) ;; Region3D
;; (inRegion CityOfPortland3D CityOfPortlandOnMap) ;; Skin2D
;; (boundryOf CityOfPortlandOnMap MoatOfOregon) ;; Pathway
;; (writeableSurface MoatOfOregon PickFence666) ;; Skin3D
;; (lowerHalf PickFence666 PickFenceFootPlate) ;; PartOfPhysical
;; (LowerHalfFn (WriteableSurfaceFn (BoundryFn (RegionFn (CityFn (WhereRulesFn "Bud Clark"))))))
;; 

;3D Object made out of atoms
;; Body - Tangable volumous container of innerds   iObject_
;; Arms - Subdivided volume iObjectPartOf_
;; Skin Surface Area (covers all of Body incuding the Subdivisions)  (This a 2D surface)   iLoc_
;; subdivided Surfaces  iLocPartOf_

;3D region without mass
;; Place - Tangible volume    iLoc_
;; SubRegions - Subdivided volume  iLocPartOf_
;; 3D Border (covers all of region incuding the Subdivisions)  (This a 2D surface)    iPartOf_
;; Border subdivided   iPartOf_






;; Couch
;; SurfaceSkin   ...
;; Cushions
;; BetweenCushions
;; SurfacePlace

