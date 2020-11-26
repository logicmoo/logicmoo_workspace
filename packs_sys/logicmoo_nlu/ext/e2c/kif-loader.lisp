
(defvar *T-READTABLE* (COPY-READTABLE *READTABLE*))
(defvar *LEFT-PAREN-READER* (get-macro-character (char "()"  0)  *T-READTABLE*))  
(defvar *kif-old-hash-dollar-reader* (symbol-function (GET-DISPATCH-MACRO-CHARACTER #\# #\$ *T-READTABLE*)))
(defvar *POP-PACKAGE* *PACKAGE*)
(defvar *T-PACKAGE* (fif (find-package "EXPORTING")(find-package "EXPORTING")(make-package "EXPORTING")))
(defvar *UVMt* (find-constant "UniversalVocabularyMt"))
(defvar *BASEKB* (find-constant "BaseKB"))
(defvar *kif-KIFImport* (find-or-create-constant "KIFImport"))
(defvar *kif-KIFImportRepairing* (find-or-create-constant "KIFImportRepairing"))
(ke-assert-now `(#$isa ,*kif-KIFImport*  #$Cyc-BasedProject) *UVMt*)
(ke-assert-now `(#$isa ,*kif-KIFImportRepairing*  #$Cyc-BasedProject) *UVMt*)
(defvar *kif-author* (find-or-create-constant "KIFCyclist"))
(defvar *kif-constant* (find-or-create-constant "KIFConstant"))
(defvar *kif-mt* (find-or-create-constant "KIFLoadingMt"))

(defvar *kif-not-wff* ())
(defvar *KIF-REPAIRS* ())
(defvar *KIF-REPAIRS-FAILED* ())
(defvar *VocabularyMt* *UVMt*)
(defvar *bookkeeping-info* (new-bookkeeping-info *the-cyclist* (the-date) *KE-PURPOSE* (the-second)))
(ke-assert-now `(#$isa ,*kif-mt*  #$Microtheory) *UVMt*)
(ke-assert-now `(#$isa ,*kif-author*  #$Cyclist) *UVMt*)
(ke-assert-now `(#$isa ,*kif-constant*  #$SubLExpressionType) *UVMt*)
(ke-assert-now `(#$genls ,*kif-constant*  #$CycLConstant) *UVMt*)


(define kif ()
    (LOAD "kif-loader.lisp")
    (print  (loadl "assertions8045.lisp"))
    '(LOAD-KIF-KB "kb16.kif"))

(define kif-print (&rest body) 
    (fresh-line)
    (clet ((body (fif (equal 1 (length body)) (car body) body))
      (res (print body)))(force-output)(ret res)))

(kif-print '(LOADING "kif-loader.lisp"))
;; cunwind-protect hozed multiple-value-lists so thats the reason for the 'prognvals' weirdness


(defvar *kif-to-cycl-table-plist* '("Nima-Gns-KS" #$NGA-Gns-KS "forall" #$forAll "exists" #$thereExists "=>" #$implies "<=>" #$equiv "isa" #$isa "instance-of" #$isa "listof" #$TheList "true" #$True "false" #$False))
;;(defvar *T-PACKAGE* (make-package :T))

(define kif-break (&rest data)
   (with-package-case *CYC-PACKAGE* :UPCASE
      (apply #'format (cons 't data)) (fresh-line)(force-output) (fresh-line)(force-output)))

(defmacro kif-warn (&rest code)(ret 
    `(with-error-handler 
         #'(lambda ()(format t "~& ;; kif-warn: ~s ~s ~&" *ERROR-MESSAGE* ',code)(fresh-line)(force-output)(fresh-line)(force-output)(kif-break *ERROR-MESSAGE*))
     (sl::progn ,@code))))

(defmacro kif-pushnew (item list &optional (predicate '#'equal)) (ret
  `(clet ((item ,item)) (pwhen item (cpushnew item ,list ,predicate)) ,list)))

(defvar kif-assert-depth 0)
;;ASSERT-BOOKKEEPING-BINARY-GAF
(define kif-assert (cycl &optional mt &rest flags)
;;  (kif-print `(kif-assert (,cycl &optional ,mt &rest ,@flags)))
  (kif-warn (csetq *the-cyclist* (kif-author (kvalue-of '(:CREATOR :WHO) flags *the-cyclist*))))
  (kif-warn (csetq *the-date* (kvalue-of '(:CREATION-DATE :WHEN) flags (fif *the-date* *the-date* (the-date)))))
  (kif-warn (csetq *KE-PURPOSE* (kvalue-of '(:KE-PURPOSE :PURPOSE ) flags 
     (fif (= 0 kif-assert-depth ) *kif-KIFImport* *kif-KIFImportRepairing*))))
  (clet ((kif-assert-depth (+ 1 kif-assert-depth)))
  (kif-warn (csetq mt (kvalue-of '(:MICROTHEORY :MT) flags (fif mt mt *KIF-MT*))))
  (pwhen (consp mt)
    (clet ((mtn (MB-FIND-NART-OR-NAUT mt)))
     (pwhen mtn (csetq mt mtn))
     (kif-mt mtn)))
  (pwhen (equal (car cycl) #$ist)
    (csetq mt (second cycl))
    (csetq cycl (third cycl))
    (kif-mt mt))
  (pwhen (equal (car cycl) #$genlMt)
   (kif-mt (second cycl))
   (kif-mt (third cycl)))
  (csetq *bookkeeping-info* (new-bookkeeping-info *the-cyclist* *the-date* *KE-PURPOSE* (the-second)))
  (csetq flags (flatten flags))
  (punless (mt? mt)
   (kif-mt mt))
  (kif-warn (csetq *the-DIRECTION* (kvalue-of '(:DIRECTION) flags (fif (member :BACKWARD flags) :backward (fif (member :CODE flags) :CODE :FORWARD)))))
  (kif-warn (csetq *the-MONOTONICITY* (kvalue-of '(:STRENGTH :MONOTONICITY) flags (fif (member :default flags) :default :monotonic))))
  (clet ((prev nil)(cmd `(with-bookkeeping-info ,(quotify *bookkeeping-info*)
     (ke-assert-now ,(quotify CYCL) ,(quotify mt) ,(quotify *THE-MONOTONICITY*) ,(quotify *THE-DIRECTION* )))))

 (pwhen (< kif-assert-depth 2) (kif-warn 
   (pwhen (kif-ask `,cycl `(#$ist ,mt ,cycl) #$EverythingPSC) (ret  (kif-print `(#$ist ,(quotify mt) ,(quotify CYCL)))))))
 ;;(csetq prev (kif-print (KB-LOOKUP-ASSERTION cycl mt)))
 ;;(fif prev (ret prev))
 (kif-print (kif-warn (eval (kif-print cmd))))
;;maybe use SUGGEST-FORMULA-FIX-FOR-AT-VIOLATION
  (punless (> kif-assert-depth 6)
    (cdo ((why (hl-explanation-of-why-not-wff CYCL mt)(hl-explanation-of-why-not-wff CYCL mt)))
    ((cor (null why)(equal prev why)))
    (csetq prev why)
     (kif-print 'ke-assert-now cmd)
     (kif-print 'hl-explanation-of-why-not-wff why)
     (csetq why (kif-negate why mt))
     (kif-print 'kif-repair why)
     (pwhen (equal prev why) 
       (break "tried the same thing again expecting differnt results"))
     (fif (kif-assert why mt flags)
       (kif-pushnew  why *kif-repairs*) 
       (kif-pushnew  why *kif-repairs-failed*))))

    (punless (kif-warn (eval (kif-print cmd)))
        (cpush (list #$ist mt cycl) *kif-not-wff*)
        (kif-print 'hl-explanation-of-why-not-wff (hl-explanation-of-why-not-wff CYCL mt))
        (punless (> kif-assert-depth 1)
           (eval (kif-print `(WITHOUT-WFF-SEMANTICS ,cmd))))
        (kif-print "cant make wff")
        (ret nil)))
   (ret t)))
                       
(define kif-negate (cycl &optional mt (negated :false))
    (pwhen (assertion-p cycl) (ret #$True))
    (pwhen (keywordp cycl) (ret cycl))
    (pwhen (stringp cycl) (ret cycl))
    (pwhen (numberp cycl) (ret cycl))
    (pwhen (constant-p cycl) (ret cycl))
    (pwhen (symbolp cycl) (ret cycl))
    (clet ((c (car cycl)))
        (pwhen (member c '(#$defnSufficient #$evaluate #$equals)) (ret #$True))
        (pwhen (equal c #$not) (ret (kif-negate (second cycl) mt (fif (equal negated :false) :true negated))))
        (pwhen (member c '(QUOTE #$SubLQuoteFn #$admittedSentence)) (ret (kif-negate (second cycl) mt negated)))
        (pwhen (member c '(#$ist #$ist-Asserted)) (ret (kif-negate (third cycl) (second cycl) negated)))
        (pwhen (member c '(#$and #$or)) (ret (cons c (mapcar #'(lambda (cycl) (ret (kif-negate cycl mt negated))) (remove-if #'assertion-p  (cdr cycl))))))
        (pwhen (member c '(#$thereExists))(ret (cons #$forAll (cons (second cycl) (mapcar #'(lambda (cycl) (ret (kif-negate cycl mt negated))) (cddr cycl))))))
        (pwhen (member c '(#$forAll))(ret (cons #$thereExists (cons (second cycl) (mapcar #'(lambda (cycl) (ret (kif-negate cycl mt negated))) (cddr cycl)))))))
     (csetq cycl (POSSIBLY-NEGATE cycl negated))
     ;;(pwhen (equal negated :false) (csetq cycl (EL-NEGATE cycl)))
     (pwhen mt (ret `(#$ist ,mt ,cycl)))
    (ret cycl))

(define kif-read-from-string (str)
   (with-readcase   :PRESERVE 
     (ret (kif-warn (read-from-string str)))))


(define kif-constant (cycl &optional guid)
 (clet ((constant nil))
    (csetq constant (second (member-if #'(lambda (a) (ret (cand (stringp a) (string-equal a cycl)))) *kif-to-cycl-table-plist*)))
    (pwhen constant (ret (values constant guid)))
    (csetq constant (kif-foc cycl guid))
    (pwhen constant 
       (csetq constant (find-or-create-constant (constant-name constant)))
       (csetq *kif-to-cycl-table-plist* (cons cycl (cons constant *kif-to-cycl-table-plist*)))
       (ret (values constant cycl)))))

(define kif-foc (cycl &optional guid)
 (punless (stringp cycl) (throw cycl))
 (pwhen (equal 0 (search "#$" cycl)) (ret (kif-constant (substring cycl 2) guid)))
 (pwhen (equal 0 (search "?" cycl)) (ret (intern cycl)))
 (pwhen (equal 0 (search "@" cycl)) (ret (intern (cconcatenate "?" (substring str 1)))))
 (clet ((constant nil))
    (csetq constant (second (member-if #'(lambda (a) (ret (cand (stringp a) (string-equal a cycl)))) *kif-to-cycl-table-plist*)))
    (pwhen constant (ret constant))
    (csetq constant (find-constant cycl))
    (pwhen constant (ret constant))
    (csetq constant (FIND-constant-BY-NAME cycl))    
    (pwhen constant (ret constant))
    (csetq constant (car (OLD-constant-NAMES cycl)))
    (pwhen constant (kif-assert `(#$oldConstantName ,constant ,cycl)) (ret constant))
    (csetq constant (KET-MAYBE-FIND-constant cycl))
    (pwhen constant (ret constant))
    (csetq constant (car (kif-ask '?TERM `(#$oldConstantName ?TERM ,cycl) #$EverythingPSC)))
    (pwhen constant (kif-assert `(#$oldConstantName ,constant ,cycl)) (ret constant))
    (csetq constant (car (kif-ask '?TERM `(#$mergedConstantName ?TERM ,cycl) #$EverythingPSC)))
    (pwhen constant (kif-assert `(#$mergedConstantName ,constant ,cycl)) (ret constant))
    (csetq constant (car (kif-ask '?TERM `(#$mergedConstantGUID ?TERM ,cycl) #$EverythingPSC)))
    (pwhen constant (ret constant))
    (pwhen (equal 0 (string= (string-upcase cycl) cycl))(kif-print `(assuming ,cycl))(ret (intern cycl)))
    ;;(pwhen guid (csetq constant (kif-by-id guid)) (pwhen constant (ret constant)))
    (csetq constant (car (constant-NAME-CASE-COLLISIONS cycl))) 
    (pwhen constant (kif-assert `(#$mergedConstantName ,constant ,cycl))  (ret constant))
    (csetq constant (car (kif-ask '?TERM `(#$termStrings ?TERM ,cycl) #$EverythingPSC)))
    (pwhen constant (kif-assert `(#$termStrings ,constant ,cycl)) (ret constant))
    (pwhen (constant-NAME-AVAILABLE cycl)
        (csetq constant (kif-warn (create-constant cycl)))
        (pwhen constant (kif-assert `(#$quotedIsa ,constant ,*kif-constant*)) (ret constant)))
    ;;(csetq constant (kif-dwim cycl))
    (kif-break "~&~& kif-constant: ~s" `(,cycl ,guid ,constant))
    (ret constant)))
  
(define kif-cycls (cycl)
   (pwhen (consp cycl)
      (pwhen (equal 1 (length cycl))
        (clet ((str (write-to-string (first cycl)) ))
          (pwhen (equal 0 (search "@" str)) (ret (intern (cconcatenate "?" (substring str 1)))))))
      (ret (cons (kif-cycl (car cycl))(kif-cycls (cdr cycl)))))
   (ret (kif-cycl cycl)))

(define kif-cycl (cycl)
 (punless cycl (ret cycl))
 (pwhen (consp cycl)
  (clet ((constant (car cycl)))
   (pwhen (equal 'QUOTE constant) (ret (quotify (kif-cycl (second cycl)))))
   (pwhen (equal '|forall| constant)
      (pwhen (cnot (equal 3 (length cycl))) (kif-break "kif-cycl-forall ~s" cycl))
      (ret `(#$forAll ,(kif-cycl (car (second cycl))) ,(kif-cycl (third cycl)))))
   (pwhen (equal '|exists| constant) 
      (pwhen (cnot (equal 3 (length cycl))) (kif-break "kif-cycl-exists ~s" cycl))
      (ret `(#$thereExists ,(kif-cycl (car (second cycl))) ,(kif-cycl (third cycl)))))
   (csetq constant (kif-cycl constant))
   (pwhen (relation? constant) (ret (cons constant (kif-cycls (cdr cycl)))))
   (format t "~&~&~&~&~&~&~s~&" cycl)
   (ret (cons constant (kif-cycls (cdr cycl))))))
 (pwhen (equal cycl '@ARGS) (ret ?ARGS))
 (pwhen (keywordp cycl) (ret cycl))
 (pwhen (stringp cycl) (ret cycl))
 (pwhen (numberp cycl) (ret cycl))
 (pwhen (constant-p cycl) (ret cycl))
 (pwhen (kif-TERM-P cycl) (ret (kif-constant (STRING-FOR-kif-TERM cycl))))
 (pwhen (cor (fort-p cycl)(nart-p cycl) (el-variable-p cycl)(constant-p cycl)(hl-variable-p cycl)) (ret cycl))
 (pwhen (cor (EL-VARIABLE? cycl)(function? cycl)(relation? cycl)(keywordp cycl)(numberp cycl)(mt? cycl)) (ret cycl))
 (pwhen (symbolp cycl) (ret (kif-constant (symbol-name cycl)))))

(define kif-mt (cycl)
  (csetq cycl (kif-cycl cycl))
  (punless cycl (ret *kif-mt*))
  ;;(punless (mt? cycl) ;; )
  (ke-assert-now `(#$isa ,cycl #$Microtheory) #$UniversalVocabularyMt)
  (WITHOUT-WFF-SEMANTICS (ke-assert-now `(#$isa ,cycl #$Microtheory) #$UniversalVocabularyMt))
  (ret cycl))

(define kif-author (cycl)
  (csetq cycl (kif-cycl cycl))
  (punless cycl (ret *kif-author*))
  (punless (cyclist? cycl)
    (kif-assert `(#$isa ,cycl #$Cyclist) #$UniversalVocabularyMt))
  (ret cycl))

(define kvalue-of (keyn list default)
  (pwhen (consp keyn)
     (ret (kvalue-of (car keyn) list (kvalue-of (cdr keyn) list default))))
  (csetq list (member keyn list))
  (fif list (ret (second list)))
  (ret default))

(define load-kif-kb (filename)
 (clet ((flags nil)(CYCL nil)(*the-date* (the-date)))
  (with-open-file (stream filename :DIRECTION  :input)
   (with-readcase :PRESERVE
      (cunwind-protect
         (cdo ((flags (read stream nil :EOF nil) (read stream nil :EOF nil))) ((equal flags :EOF))
            (csetq CYCL (kvalue-of :KIF flags :ERROR))
            (punless (equal (car CYCL) '|termOfUnit|)
             (kif-print flags)(fresh-line)(force-output)
             (kif-warn (csetq mt (kif-mt (kvalue-of :MICROTHEORY flags *kif-mt*))))
             (kif-warn (csetq CYCL (progn `(,CYCL) (kif-cycl CYCL))))
              (kif-assert cycl mt flags))))))))

(define loadl (filename)
  (clet ((*READTABLE* *T-READTABLE*))
    (with-open-file (stream filename :DIRECTION  :input)
     (cdo ((form (read stream nil :eof)(read stream nil :eof)))
          ((equal form :eof))
       (eval form)))))
  
;;;; ======================================================================
;;;; Allows Reading of Undefined constants so they be mapped!
;;;; ======================================================================
;; since default &optioanl arguments are broken

(define case-sensitive-read (&optional (stream *INPUT-STREAM*) (eof-err-p t) eof-val rec-p) 
   (clet ((stream (coerce-stream stream ))(*INPUT-STREAM* stream))
      (with-readcase  :preserve (ret (values (read stream eof-err-p eof-val rec-p) t)))))

(define hash-dollar-reader (stream subchar arg)
   (declare (ignore subchar arg)) (ret (values (kif-cycl (case-sensitive-read stream t nil t)) t)))

(define kif-dollar (&rest val)
 (pwhen (equal 1 (length val))
   (pwhen val (ret (set-dispatch-macro-character #\# #\$ #'hash-dollar-reader *T-READTABLE*)))
   (set-dispatch-macro-character #\# #\$ *kif-old-hash-dollar-reader* *T-READTABLE*))
  (ret (cnot (equal (get-dispatch-macro-character #\# #\$) *kif-old-hash-dollar-reader* *T-READTABLE*))))

(kif-dollar t)
;;;; ======================================================================
;;;; ======================================================================
;;;; ======================================================================

#|
(kif-print #$DouglasMiles)

(fresh-line)(force-output)
|#

#|

(define mt-to-filename (mt)
  (csetq mt (write-to-string mt))
  (csetq mt (remove-if #'(lambda (a) (ret (member a `(#\Space #\# #\/ #\\ ,*quote-char* (char "(" 0) (char ")" 0) #\$)))) mt))
  (csetq mt (substitute-if #\- #'(lambda (a) (ret (member a '(#\. #\:  #\; )))) mt))
  (csetq mt (HTML-ENCODE-TEXT mt))
  (ret (cconcatenate mt ".kif")))

(define save-kif-kb (filestring)
 (CLET 
  ((*STANDARD-OUTPUT* (OPEN-TEXT-FILE filestring :output)))
  (WITH-ALL-MTS 
     (DO-BOOKKEEPING-ASSERTIONS (PRED ARG1 ARG2)
       (clet ((kif (CYCL-TO-kif `(,PRED ,ARG1 ARG2))))
         (kif-print kif))))        
  (cdolist (TERM *kif-pre-exported-terms*)
    (WITH-ALL-MTS 
      (CDOLIST (ASSERTION (ALL-TERM-ASSERTIONS TERM T))
          (CLET 
            ((FORMULA (ASSERTION-FORMULA ASSERTION))(mt (ASSERTION-mt ASSERTION))(kif (ASSERTION-kif-FORMULA ASSERTION)))
              (kif-print kif)))))))

(define kif-dwim (str) 
  (clet ((cycl str))
    (pcond
       ((kif-cyc-p str) (ret str))
       ((stringp str)
            (csetq str (string-trim *WHITESPACE-CHARS* str))
            (pwhen (char= (char "()" 0) (char str 0))
                 (ret (kif-cycl (kif-read-from-string str))))
            (pwhen (char= #\# (char str 0))
                 (pwhen (char= #\$ (char str 1))
                    (ret (find-or-create-constant (substring str 2))))
                 (ret (kif-cycl (kif-read-from-string str))))
            (csetq cycl (count #\space str))
            (pwhen (> cycl 0)
                 (ret (kif-cycl (kif-read-from-string (cconcatenate "(" str ")")))))
            (pwhen (equal 0 (search "?" str)) (ret (inter str)))
            (csetq cycl (find-constant str))
            (pwhen cycl (ret cycl))
            (ret `(#$InstanceNamedFn ,str  *kif-COLLECTION*)))
        ((cnot (consp str)) (ret str))
        ((member (car str) '(kif-dwim)) (ret (quotify (kif-dwim (second str)))))
        ((member (car str) '(quote)) (ret (quotify (kif-constant (second str)))))
        (t (ret (cons (kif-cycl (car str))(kif-constant (cdr str))))))))


(define kif-by-id (guid)
  (punless guid (ret guid))
  (clet ((constant nil))  
   (pwhen (numberp guid)
     (csetq constant (find-constant-by-external-id guid) )
     (pwhen constant (ret constant))
     (csetq constant (find-constant-by-internal-id guid) )
     (pwhen constant (ret constant))
     (csetq constant (find-constant-by-LEGACY-id guid) )
     (ret constant))
  (pwhen (cand (stringp guid) (POSSIBLY-GUID-STRING-P guid)) (csetq guid (string-to-guid guid)))
  (pwhen (guid-p guid) 
     (csetq constant (find-constant-by-guid guid))
     (pwhen constant (ret constant))
     (csetq constant (LOOKUP-constant-MERGED-GUID guid))
     (pwhen constant (ret constant))
     (csetq constant (KB-LOOKUP-constant-BY-GUID guid))
     (pwhen constant (ret constant))    
     (csetq guid (guid-to-string guid)))
    (csetq constant (car (kif-ask '?TERM `(#$mergedConstantGUID ?TERM ,guid) #$EverythingPSC)))
    (pwhen constant (ret constant))
    (pwhen (cand (stringp guid) (POSSIBLY-GUID-STRING-P guid)) (csetq guid (string-to-guid guid))
        (ret (kif-by-id (constant-GUID-TO-LEGACY-ID guid))))
     (ret constant)))
(csetq *kif-pre-exported-terms* '(#$oldConstantName #$isa #$genls #$arity #$arityMin #$arityMax))
(kif-print '(load-kif-kb "kb16.kif"))
(fresh-line)(force-output)
(define kif-break (&rest data)
   (with-package-case *CYC-PACKAGE* :UPCASE
      (fresh-line)(fresh-line)(force-output) (ret (apply #'break data))))

(fresh-line)(force-output)

(csetq *quote-char* #\")

(csetq *TASK-PROCESSOR-VERBOSITY* 9)
(csetq *SBHL-TRACE-LEVEL* 9)
(csetq *ALLOW-EXTERNAL-INFERENCE* nil)
(csetq *EVALUATABLE-BACKCHAIN-ENABLED* t)
(csetq *ALLOW-FORWARD-SKOLEMIZATION* t)
(csetq *UNBOUND-RULE-BACKCHAIN-ENABLED* t)
(csetq *SUSPEND-SBHL-TYPE-CHECKING?* t)
(csetq *REPORT-DEFAULT-METHOD-CALLS?* t)
(csetq *EVAL-IN-API-TRACED-FNS* '(T EVAL))
(csetq *EVAL-IN-API-LEVEL* -1)
(csetq *DEFAULT-TRANSFORMATION-MAX* 10000) ;; normal 1000
(csetq *DEFAULT-RECURSION-LIMIT* 10000) ;; normal 1000
(csetq *ALLOW-EXPANDED-RULES-AS-OWL-EXPORT-CANDIDATES?* t)
(csetq *ALLOW-BACKWARD-GAFS* t)
(csetq *HL-FAILURE-BACKCHAINING* t)
(csetq *PERFORM-EQUALS-UNIFICATION* t)
(csetq *READ-REQUIRE-CONSTANT-EXISTS* t)
(csetq *PERFORM-UNIFICATION-OCCURS-CHECK* t)
(csetq *READ-REQUIRE-CONSTANT-EXISTS* T   )
(csetq *SUSPEND-SBHL-TYPE-CHECKING?* T   )

;; changes
(csetq *EVALUATABLE-BACKCHAIN-ENABLED* nil   )
(csetq *ALLOW-FORWARD-SKOLEMIZATION* nil   )
(csetq *UNBOUND-RULE-BACKCHAIN-ENABLED* nil   )
(csetq *REPORT-DEFAULT-METHOD-CALLS?* nil   )
(csetq *ALLOW-EXPANDED-RULES-AS-OWL-EXPORT-CANDIDATES?* nil   )
(csetq *ALLOW-BACKWARD-GAFS* nil   )
(csetq *HL-FAILURE-BACKCHAINING* nil   )
(csetq *PERFORM-EQUALS-UNIFICATION* nil   )


(LOAD-KB-PRODUCT-SHARED-SYMBOLS  "units/5002/")
LOAD-ARG-TYPE-CACHE
LOAD-CONSTANT-SHELLS
LOAD-ASSERTION-DEFS
LOAD-ASSERTION-INDICES
LOAD-DEDUCTION-DEFS
LOAD-ASSERTION-SHELLS
LOAD-nart-SHELLS
LOAD-KB-INDEXING
LOAD-assertion-SHELLS
LOAD-CONSTANT-INDICES
APPLY LOAD-MISCELLANEOUS  ) 
   ( APPLY LOAD-NART-INDEX  ) 
   ( APPLY LOAD-NART-INDEX  ) 
   ( APPLY LOAD-NART-INDICES  ) 
   ( APPLY LOAD-NART-INDICES  ) 
   ( APPLY LOAD-NART-SHELL  ) 
   ( APPLY LOAD-NART-SHELL  ) 
   ( APPLY LOAD-NART-SHELLS 
 APPLY LOAD-RULE-SET-FROM-STREAM  ) 
   ( APPLY LOAD-RULE-UTILITY-EXPERIENCE  ) 
   ( APPLY LOAD-RULE-UTILITY-EXPERIENCE  ) 
   ( APPLY LOAD-SBHL-CACHE  ) 
   ( APPLY LOAD-SBHL-CACHE  ) 
   ( APPLY LOAD-SBHL-DATA  ) 
   ( APPLY LOAD-SBHL-DATA  ) 
   ( APPLY LOAD-SBHL-MISCELLANY  )

|#

#|
(defvar *EXTERNAL-LISP-PACKAGE* (sl::make-package "EXTERNAL" '("SL" "CYC") '("EXT")))
(defvar *EXPORTED-LISP-PACKAGE* (sl::make-package "EXPORTED" '() '("EXP")))


(cdolist (s *all-symbols-from-cyc*)
  (pwhen (cor (fboundp s)(boundp s))(export s (symbol-package s))))

(cdolist (s CYC::*all-symbols-from-cyc*)
  (pwhen (cor (fboundp s)(boundp s))(intern s :EXT)))

(cdolist (s CYC::*all-symbols-from-cyc*)
  (intern s (symbol-package s))
  (export s (symbol-package s))
  (unexport s (symbol-package s))
  (unintern s (symbol-package s)))

(cdolist (s *all-symbols-from-cyc*)
  (pwhen (cor (fboundp s)(boundp s))(import s :EXT)))

(cdolist (s *all-symbols-from-cyc*)
  (pwhen (cor (fboundp s)(boundp s))(export s (symbol-package s))))

(cdolist (s *all-symbols-from-cyc*)
  (pwhen (cor (fboundp s)(boundp s))(import s :EXT)))



(cdo-all-symbols (push 
(define other-package (pack) (ret (fif (eq pack *CYC-PACKAGE*)  *SUBLISP-PACKAGE*  *CYC-PACKAGE*)))

(define into-package ()
(in-package :CYC)
(cdo-symbols (sym CYC::*CYC-PACKAGE*)
 (pwhen (cor (boundp sym)(fboundp sym))(export sym :SL)(import sym :EXP)))
 ;; (in-package :EXP) (export sym :EXP)(in-package :CYC)))
(in-package :SUBLISP)
(cdo-symbols (sym CYC::*SUBLISP-PACKAGE*)
 (pwhen (cor (boundp sym)(fboundp sym))(export sym :SL)(import sym :EXP)))) (into-package)
 ;;(in-package :EXP) (export sym :EXP)(in-package :SUBLISP))))

(defvar *EXPORTED-LISP-PACKAGE* (sl::make-package "EXPORTED" '() '("EXP")))

(in-package :SL)
(cdo-symbols (sym CYC::*SUBLISP-PACKAGE*)
 (pwhen (eq CYC::*SUBLISP-PACKAGE* (symbol-package sym))
 (export sym)
 (pwhen (boundp sym)(fboundp sym)) (in-package :CYC) (import sym)(in-package :SL)))


(in-package :SL)

(cdo-symbols (sym *SUBLISP-PACKAGE*)
   (unexport sym *SUBLISP-PACKAGE*))

;;is this a MOVE SYMBOL  ?
   (clet ((f (find-symbol (symbol-name sym) *CYC-PACKAGE*)))
        (punless (eq f sym)
           (pcond
             ((null f)
              (unexport sym *SUBLISP-PACKAGE*)
              (export sym *CYC-PACKAGE*))
             ((cor (boundp sym) (fboundp sym) )
             ;; (unintern f *CYC-PACKAGE*)
              (export sym *SUBLISP-PACKAGE*)
              (export sym *CYC-PACKAGE*))
             ((cor (boundp f) (fboundp f) )
              (unintern sym)
               ;;*SUBLISP-PACKAGE*)
              (export f *SUBLISP-PACKAGE*)
              (export f *CYC-PACKAGE*))))

 (cdo-symbols (sym *SUBLISP-PACKAGE*)
  (pwhen (eq *SUBLISP-PACKAGE* (symbol-package sym))
     (export sym *SUBLISP-PACKAGE*)))
 (cdo-symbols (sym *SUBLISP-PACKAGE*)
   (clet ((f (find-symbol (symbol-name sym) *CYC-PACKAGE*)))
        (punless (eq f sym)
          (pwhen (fboundp f) (import f *SUBLISP-PACKAGE*))
          (pwhen (null f) (export sym *SUBLISP-PACKAGE*))
          (pwhen (boundp f) (import f *SUBLISP-PACKAGE*)))))
          
            (export sym 
           (pwhen (eq *CYC-PACKAGE* (symbol-package sym))
     (unexport sym *CYC-PACKAGE*)))
  (pwhen (find-symbol (symbol-name sym) *SUBLISP-PACKAGE*)
(defvar *EXTERNAL-LISP-PACKAGE* (sl::make-package "EXTERNAL" '("SL" "CYC") '("EXT")))

(cdolist (pack (list *SUBLISP-PACKAGE* *CYC-PACKAGE*))
   (cdo-all-symbols *SUBLISP-PACKAGE*(sym pack)
     (clet ((from (symbol-package sym)))
       (pwhen (fboundp sym)
         (export sym from)
         (import sym (other-package from)))))))

( make-dispatch-macro-character (char "{}" 0 ))
(symbol-function (get-macro-character (char "{}" 0 )))

(symbol-function (get-macro-character (char "()" 1)))
(csetq *SBHL-OBJECT-TYPE-CHECKING-P* nil)

|#

;;(REGISTER-DEFAULT-QL-PARSERS)
;;(csetq *ASSUME-FORWARD-DEDUCTION-IS-WF?* T)

(defmacro with-readcase (readcase &rest body) (ret
  `(with-package-case *PACKAGE* ,readcase ,@body)))

(defmacro with-package-case (package readcase &rest body) (ret
  `(clet (prognvals 
          (opack (string (package-name *PACKAGE*)))
          (*POP-PACKAGE* (fif (equal *PACKAGE* *T-PACKAGE*) *POP-PACKAGE* *PACKAGE*))
          (*PACKAGE* (fif (packagep ,package) ,package (find-package (string ,package))))
          (*READTABLE* *T-READTABLE*)
          (in-package (package-name *PACKAGE*))
          (ocase (READTABLE-CASE *READTABLE*)))
          (CSETF (READTABLE-CASE *READTABLE*) ,readcase)
      (cunwind-protect 
          (csetf prognvals (multiple-value-list (progn ,@body)))
          (CSETF (READTABLE-CASE *READTABLE*) ocase)
          (in-package opack)
          (values-list prognvals)))))

(define coerce-stream (stream)
  (ret (fif (streamp stream) stream *STANDARD-INPUT*)))

(define peek-ahead (stream &optional (char ()))
   (clet ((stream (coerce-stream stream ))(*INPUT-STREAM* stream))
      (csetq char (read-char stream))
      (unread-char char stream)
      (ret char)))

(define READ-UNTIL-IF (quitfn &optional (stream *STANDARD-INPUT*)(retstr ""))
   (clet ((stream (coerce-stream stream ))(*INPUT-STREAM* stream))
     (cdo ((lastchar (read-char stream)(read-char stream))) 
          ((funcall quitfn lastchar)(unread-char lastchar stream )(ret (values retstr lastchar)))
        (csetq retstr (cconcatenate retstr (string lastchar))))))

(define peek-useable-char (stream)
  (ret (second (multiple-value-list (READ-UNTIL-IF #'(lambda (char) (ret (cnot (member char SL::*WHITESPACE-1-CHARS*)))) stream)))))

(define consume-if (char &optional (stream *STANDARD-INPUT*))
  (pwhen (equal (peek-ahead stream) char) (ret (read-char stream))) 
   (ret nil))

(define get-ugly-reader (str)
  (csetq str (cconcatenate "UGLY-" (string-upcase str) "-READER"))
  (csetq str (find-symbol str ))
  (pwhen (fboundp str) (ret (symbol-function str))))

(define ugly-class-reader (stream c term)
   (consume-if c stream)
   (csetq c (read-until-if #'(lambda (ch) (ret (eq ch term))) stream))
   (consume-if term stream)
   (ret (CLASSES-RETRIEVE-CLASS (read-from-string c))))

(define ugly-as-reader (stream c term)
  (consume-if c stream)
  (clet ( mt (cycl (CASE-SENSITIVE-READ stream nil T nil)))
   (consume-if #\: stream)
   (csetq mt (CASE-SENSITIVE-READ stream nil T nil))
   (consume-if term stream)
   (ret (values (find-or-create-assertion cycl mt) T))))

(define UGLY-OBJECT-RMF (stream c n)
   (clet ((stream (coerce-stream stream ))(*INPUT-STREAM* stream) found symbol access nextchar)
          (csetq nextchar (peek-useable-char stream))
          (pwhen (SL::ALPHA-CHAR-P nextchar)
             (csetq found (multiple-value-list (READ-UNTIL-IF #'(LAMBDA (ch) (ret (member ch (cons #\: SL::*WHITESPACE-1-CHARS* )))) stream)))
             (csetq access (get-ugly-reader (car found)))
             (pwhen access 
               (print access)
               (csetq symbol (funcall access stream (second found) #\> ))
               (ret (values symbol t))))

          (pwhen (get-macro-character nextchar)
             (csetq found (CASE-SENSITIVE-READ stream nil T nil)))

          (pwhen (consume-if #\> stream ) 
            (pwhen (consp found)
               (ret (values (MB-FIND-NART-OR-NAUT found) t))))

          (pwhen (cand (equal nextchar #\: ) (consp found))
             (csetq symbol (CASE-SENSITIVE-READ stream nil T nil ))
              (ret (values (FIND-OR-CREATE-ASSERTION found symbol) t)))))




(define FREE-ALL-NARTS () (print `(not freeing narts)))
(define FREE-ALL-CONSTANTS () (print `(not freeing CONSTANTS)))
(define FREE-ALL-CLAUSE-STRUCS () (print `(not freeing CLAUSE-STRUCS)))
(define FREE-ALL-ASSERTIONS () (print `(not freeing ASSERTIONS)))
(define FREE-ALL-DEDUCTIONS () (print `(not freeing DEDUCTIONS)))

;;CYC(42):   (CREATE-STRUCTURE-INFO #$isa '(X  Y) '() '())
;;#<STRUCTURE-INFO 48D49570>

(define COERCIVE-LPARAMS-READER (stream subchar)
   (clet ((stream (coerce-stream stream )) found symbol access nextchar)
      (with-readcase (READTABLE-CASE *READTABLE*)
         (csetq found (read-delimited-list (char "()" 1) stream t)))
      (ret (values found t))))


(set-macro-character (char "()" 0) #'COERCIVE-LPARAMS-READER *T-READTABLE*)
(set-dispatch-macro-character #\# #\< #'UGLY-OBJECT-RMF *T-READTABLE*)



(define arg-phrase (const &optional (prefix "") (suffix "") (nil-phrase "SomethingExisting"))
 (punless const (ret (remove #\Space (string-proper (cconcatenate prefix nil-phrase suffix)))))
 (pwhen (consp const)
    (fif (= 1 (length const)) (ret (arg-phrase (car const) prefix suffix nil-phrase)))
    (punless (function? (car const)) (ret (arg-phrase (cons #$TheList const) prefix suffix nil-phrase))))
;;  (kif-print const)
  (pwhen (constant-p const)
    (ret (cconcatenate prefix  (remove #\- (constant-name const))  suffix)))
  (ret  (remove #\Space (string-proper (cconcatenate prefix " " (generate-phrase const)  " "  suffix)))))

(define lecar (el list)
 (pwhen list
    (punless el
      (pwhen (collection? list) (ret list))
      (pwhen (consp list)
       (clet ((c (car list))(le (arity c)))
           (pwhen le (ret (first-n (1+ le) list)))
         (pwhen (function? c)  (ret list))
         (ret c)))
      (ret list))
  (csetq list (lecar nil list))
 ;; (kif-print `(lecar ,el ,list))
  (ret list)))


(define kif-ask (temp cycl &rest mt)
 (csetq mt (fif mt (car mt) #$EverythingPSC))
  (clet ((*cache-inference-results* t)(*compute-inference-results* t)(*unique-inference-result-bindings* t)    
     (*generate-readable-fi-results* t))
    (ret (ask-template temp cycl mt 10 3 10 1000 ))))    

(define FN-ARGN-ISA  (skfconst arg ellist &optional defaultarg)
   (clet ((gfound (FN-ARGN-GREP skfconst arg ellist "Genl"))(ifound (FN-ARGN-GREP skfconst arg ellist "Isa"))
     (found (FN-ARGN-GREP skfconst arg ellist "")))
;;     (kif-print `(gfound ,arg ,(min-cols gfound) <= ,gfound))
;;     (kif-print `(ifound ,arg ,(min-cols ifound) <= ,ifound))
     (csetq gfound (lecar arg (min-cols gfound)))
     (pwhen gfound (ret gfound))
     (csetq ifound (lecar arg (min-cols ifound)))
     (pwhen ifound (ret ifound))
     (csetq found (lecar arg (min-cols found)))
     (pwhen found (ret found))
     (ret defaultarg)))



(define FN-ARGN-GREP  (skfconst arg ellist reqstr)
 (clet (r (argStr (write-to-string arg)))
   (cdolist (el ellist)
       (clet ((rel (car el))(uncasename (write-to-string rel)))
       (punless (cor (consp rel)
                (cnot (search reqstr uncasename))
                (member rel `(#$isa #$arity #$skolem #$implies))
                (cnot (equal (second el) skfconst))
                (< (length el) 2))
        (pwhen (= arg 0)
          (pwhen (search "esult" uncasename) 
            (kif-pushnew (lecar el (last el)) r)))
        (pwhen (> arg 0)
          (pwhen (search (cconcatenate argStr "Genl") uncasename)             
           (kif-pushnew  (lecar el (member-if #'COLLECTION? el)) r))
             (pwhen (search (cconcatenate argStr "Isa") uncasename)             
              (kif-pushnew  (lecar el (member-if #'COLLECTION? el)) r))
             (pwhen (search (cconcatenate argStr "-") uncasename)             
              (kif-pushnew  (lecar el (member-if #'COLLECTION? el)) r))
             (pwhen (search (cconcatenate "-" argStr ) uncasename)          
              (kif-pushnew  (lecar el (member-if #'COLLECTION? (reverse el))) r))
            (pwhen (search (cconcatenate "Isa") uncasename)             
              (kif-pushnew  (lecar el (second (member arg el))) r))
            (pwhen (search (cconcatenate "Genl") uncasename)            
              (kif-pushnew  (lecar el (second (member arg el))) r))
            (pwhen (search (cconcatenate "args") uncasename)        
              (kif-pushnew  (lecar el (second (member arg el))) r))
            (pwhen (search (cconcatenate "AndRest") uncasename)        
             (cdo ((targ arg (- 1 targ)))((< targ 1))
              (kif-pushnew  (lecar el (second (member arg el))) r)))
            (pwhen (search (cconcatenate "Arg" argStr) uncasename)             
              (kif-pushnew  (lecar el (second (member arg el))) r)))
              )))
      (ret (delete-if #'null r))))

(define nth-or (arg list default)
  (clet ((result (nth arg list)))
    (ret (fif result result default))))

 #|
 (pwhen skfconst
 (pwhen (numberp skfconst) (ret (suggestFunctionName (find-constant (cconcatenate "SKF-" (write-to-string skfconst))) argsIsa prefix disallowed)))
 (pwhen (stringp skfconst) (ret (suggestFunctionName (find-constant skfconst) argsIsa prefix disallowed)))
 (pwhen (symbolp skfconst) (ret (suggestFunctionName (find-constant (symbol-name skfconst)) argsIsa prefix disallowed)))
 |#

 (define suggestFunctionName (skfconst &optional defaultarg suffixes skfname argsIsa len
   (prefixes '("" "Of" "-With" "And" "-Of" "And" "-And" "And" "And" "And")))
 (pwhen (search "SKF-" (constant-name skfconst))
   (punless (search "6425563" (constant-name skfconst))
    (clet 
       ((prefix "")
        (len (arity skfconst))
        (ellist (mapcar #'ASSERTION-EL-FORMULA (ALL-TERM-ASSERTIONS skfconst))))
     (punless len (csetq (arity skfconst)))
     (punless len (nth 2 (car (member-if #'lambda (a) (ret (equal (car a) #$arity)) ellist ))))
     (punless skfname (csetq skfname (cconcatenate "-" (remove-if #'NOT-DIGIT-CHAR-P (constant-name skfconst)))))
     (kif-pushnew (cconcatenate skfname "Fn") suffixes)
     (cdo ((arg (length argsIsa) (+ 1 arg))) ((> arg len))
       (clet ((argIsa (FN-ARGN-ISA skfconst arg ellist defaultarg)))
           (punless argIsa (ret nil))
         (csetq argsIsa (append argsIsa (list argIsa)))))
     (pwhen (cand (> len 0) (cnot (equal (nth 0 argsIsa)(nth 1 argsIsa)))) (kif-pushnew "Fn" suffixes))

     (kif-print `(suggestFunctionName ,skfconst ,defaultarg ,suffixes ,skfname ,argsIsa ,len))

     (cdo ((arg 0 (+ 1 arg)))((> arg len))
          (clet ((argIsa (nth-or arg argsIsa defaultarg))(prep (nth-or arg prefixes "And")))
          (punless argIsa (cdolist (el ellist) (kif-print el))(ret nil))
          (pwhen (cand (> arg 0)(equal (nth (- arg 1) argsIsa) argIsa) (cpush "-" prefixes) (csetq prep "AndAnother")))
       (csetq prefix  (cconcatenate prefix (arg-phrase argIsa prep "")))))
   (cdolist (suffix suffixes)
       (clet  
         ((suggest (remove-if #'INVALID-CONSTANT-NAME-CHAR-P (cconcatenate prefix suffix)))
          (constant (find-constant suggest)))
          (pwhen (equal skfconst constant) (ret nil))
          (punless constant 
             (ret suggest))))
    (ret nil)))))



 #|(ret (cconcatenate prefix skfname "Fn")))

   (csetq prefix (arg-phrase (FN-ARGN-ISA skfconst 1 ellist) (cconcatenate prefix "Of") ""))

   (pwhen (> len 1)
       (csetq suffix (arg-phrase (FN-ARGN-ISA skfconst 2 ellist) "-" ""))
       (pwhen (> len 2) (csetq suffix (cconcatenate suffix "By" (arg-phrase (FN-ARGN-ISA skfconst 3 ellist) "" "")  )))
      (cdo ((arg 4 (1+ arg))) ((> arg len))
         (csetq suffix (arg-phrase (FN-ARGN-ISA skfconst arg ellist) (cconcatenate suffix more ) ""))))

   (csetq suffix (cconcatenate suffix "Fn"))

   (clet ((suggest (find-constant (cconcatenate prefix suffix))))
      (pwhen (equal skfconst suggest) (ret nil))
     (pwhen (cor t suggest )
       (ret (cconcatenate prefix skfname suffix)))
    (ret (cconcatenate prefix suffix)))))))
    (list (mapcar #'second (mapcar #'ASSERTION-EL-FORMULA (ALL-TERM-ASSERTIONS #$skolem))))
|#
(define renameSkolems (&rest list)
  (pwhen (consp list)
      (cdolist (skfconst (flatten list))
       (clet ((suggest (suggestFunctionName  skfconst)))
         (pwhen suggest
           (punless (string-equal (constant-name skfconst) suggest)
           (kif-print `(rename-constant ,skfconst ,suggest))
           (rename-constant skfconst suggest)))))))

;;(csetq *skfs* (mapcar #'second (mapcar #'ASSERTION-EL-FORMULA (ALL-TERM-ASSERTIONS #$skolem))))
 (csetq *skfs*  (ask-template '?SKF `(#$skolem ?SKF) #$EverythingPSC nil))
;;(renameSkolems (nthcdr 1000 (ask-template '?SKF `(#$skolem ?SKF) #$EverythingPSC nil 4000)))
(kif-print `(LOAD "kif-loader.lisp"))


