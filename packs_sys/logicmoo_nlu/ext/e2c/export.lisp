;;;{{{DOC
;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Copyright (c) 2006 Cycorp, Inc.  All rights reserved.
;;;
;;; @module EXPORT-PROLOG
;;; @features :Cyc-Prolog
;;;
;;; @owner dmiles
;;; @owner logicmoo
;;;
;;; @created 2006/01/31
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <description of purpose of file>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External interface:
;;;
;;; <list of the public functions>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File organization:
;;;
;;; <outline of file contents>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;}}}EDOC

(sl:csetq cyc::*OLD-PACKAGE* *PACKAGE*)
(in-package "CYC")

(defvar *renames* (MAKE-HASH-TABLE 336790  #'equal))


'(sl:define safely-rename-or-merge (Before After)
  (punless (equal Before After)
  (clet ((b (find-constant Before)) (a (find-constant After)))
     (pwhen b
          ;; purposely doesnt do anything (just prints)
       (fif a (print `(cyc-merge ,a ,b)) (cyc-rename-fast b After))))))

(sl:define safely-rename-now (b a) 
  (cyc-rename-fast b a))

;; fast dirty rename ;;(_SET-STRUCTURE-SLOT b 3 After)
(sl:define cyc-rename-fast (b After) 
  (sethash b *renames* After))

(sl:define safely-rename-or-merge (b a)
  (cyc-rename-fast b a))

(cvs-id "$Id: export-prolog.lisp 129084 2009-10-30 21:09:07Z rck $")

;;;;;;;;;;;;;;;;;;;;;
;; system-translation
;;;;;;;;;;;;;;;;;;;;;
(sl:csetq cyc::*CB-DEFAULT-INDEX-VIEW* :legacy)

(declaim
 (faccess public
          prolog-need-quote)
 (faccess private
          ))

(defmacro my-funcall (type assrt) (ret `( no-errors 'error (funcall ,type ,assrt))))
(defmacro my-debug (&rest code) (ret `( no-errors ',code  ,@code)))

(define my-second (lst) (clet ((d (cdr lst))) (fif (consp d) (ret (car d)) (ret d))))
;;(define my-third (lst) (ret (no-errors lst (third lst))))

;; (define my-second (lst) (ret (second lst)))
(define my-third (lst) (ret (third lst)))


;; translating Sublisp into Java RTL
(define te2c ()
(clet ((*current-system-translation* nil))
  (translate-one-system
   (list :system "SUBLISP"
      :backend :SL2Java
      :features 
 '(:CYC-DATE :CYC-TIME :CYC-QUANT :CYC-HTML :CYC-BROWSER :CYC-EXTERNAL :CYC-EVENT :COMMON-LISP)
      :input-directory
      "/opt/cyc/e2c/"
      :output-directory
      "/opt/cyc/e2c/translatedCode/"
      :system-manifest
      "/opt/cyc/e2c/system-manifest.lisp"
      ))))


(define FORCE-PRINT (string) (print string) (force-output))

(define Trans ()
  (load "/opt/cyc/e2c/export.lisp")
  (ret    
    (clet ((ts-file (TRANSLATE-FILE "JavaTest" "/opt/cyc/e2c/export.lisp"))
        (fout (OPEN-TEXT "/opt/cyc/e2c/export.trans" :output)))
	(SHOW-TRANS-SUBL-FILE ts-file fout)(close FOUT)
        ;;(C-BACKEND-OUTPUT-FILE-AND-HEADER-FILE ts-file "/opt/cyc/e2c/export.c")
         (JAVA-BACKEND-OUTPUT-FILE ts-file "/opt/cyc/e2c/export.java")
        ts-file)
	))

(defvar *a* ())
(csetq *a* ())
(defvar *ASSRTID* ())
(define assertion-id-plusn (assrt) (ret (- (assertion-id assrt))))
(define deduction-id-plusn (assrt) (ret (+ 988000000000 (deduction-id assrt))))

;; #-:LOADING 
;; (csetq *a* (find-assertion-by-id 7475354))
;;#-:LOADING
;; (punless *a* (csetq *a* (find-assertion-by-id (random (assertion-count)))))

(defvar *assrtform* ())
(defvar *ded* ())
(defvar *every1* 1)
(defvar *oftenFlush* 10)
(defvar *oftenProgress* 100)


(define my-coerce-string (trm)
 (pwhen (constant-p trm) (ret (constant-name trm)))
 (pwhen (stringp trm) (ret trm))
 (pwhen (characterp trm) (ret (string trm)))
 (ret (prin1-to-string trm)))


(defvar *renames* (MAKE-HASH-TABLE 336790  #'equal))
'(load "e2c/renames-fixed.lisp")

(defvar *kbnum* "988")

;;;; use (load "cynd/osim.lisp")
;;;; use (load "/opt/cyc/e2c/export.lisp")



(define plw-str (p) (princ p))
'(define plw-str (p) (princ p) (force-output))
  
(define plw-nl () (format t "~%"))


(define prolog-no-need-quote (str) ()
  ;; (ret (cnot (prolog-need-quote str)))
  )

(define prolog-need-quote (str) 
 (punless (equal (SUBSTITUTE  #\_ #\- str) str)
   (ret t)) (clet ((ch (char str 0)))
    (pwhen (member ch '(#\: #\' #\` #\#) #'char=) (ret nil))
     (ret (cor (cnot (lower-case-p ch)) (find #\: str)))))

(define plw-list (trm0 &optional (begstr "") (endstr "") (nullstr "")) 
 (pcond
 ( (null trm0 )  (progn (plw-str begstr) (plw-str endstr)))
 ( (atom trm0 )  (plw-term trm0))
 ( (null (cdr trm0)) (progn (plw-str begstr) (plw-term (car trm0)) (plw-str endstr)))
 ( (cnot (consp (cdr trm0))) 
   (my-debug (progn (plw-str begstr) (plw-term (car trm0)) (plw-str " |/**/ ") (plw-term (cdr trm0)) (plw-str endstr))))
 (t
  (progn 
   (plw-str begstr) 
   (plw-term (car trm0)) 
   (plw-cdr (cdr trm0) "," "|" nullstr)
   (plw-str endstr)))))

(define plw-cdr (trm &optional (commastr ",")(barstr "|") (nullstr "")) 
 (pcond
 ((null trm)  (plw-str nullstr))
 ((cnot (consp trm))  (progn (plw-str barstr)(plw-term trm)))
 ((null (cdr trm))  (progn (plw-str commastr) (plw-term (car trm))))
 ((cnot (consp (cdr trm)))
     (progn (plw-str commastr) (plw-term (car trm)) (plw-str barstr) (plw-term (cdr trm))))
  (t  (progn 
             (plw-str commastr) 
	     (plw-term (car trm))
             (plw-cdr (cdr trm) commastr barstr nullstr)))))

(define plw-atom (trm &optional (out *standard-output*))
 (pif 
   (constant-p trm) (plw-atom (constant-prolog-name trm))
 (progn 
 (csetq trm (constant-prolog-name (my-coerce-string trm)))
  (pif (cnot (prolog-no-need-quote trm)) 
    (progn (progn (plw-str "'") (plw-str trm) (plw-str "'")))
    (plw-str trm)))))


(define simple-name (trm)
 (pwhen (constant-p trm) (ret (constant-name trm)))
 (pwhen (stringp trm) (ret trm))
 (ret (my-coerce-string trm)))
 
(define plw-naut (str trm)
(pcond 
 ((cnot (consp trm)) 
	 (progn (princ str) (princ "0(") (plw-term trm) (princ ")")))
 (t 
  (clet ((pred (elt trm 0)) (cdrtrm (cdr trm)) (*VA* (ISA? pred #$VariableArityRelation)))	 
 (pcond 
  ((function? pred)
        (pcond 
	  ((ISA? pred #$UnreifiableFunction)
		  (progn (plw-str str) (plw-str "U(") (plw-term pred) (plw-argz *va* cdrtrm) (plw-str ")" )))
	  ((ISA? pred #$ReifiableFunction)
		  (progn (plw-str str) (plw-str "R(") (plw-term pred) (plw-argz *va* cdrtrm) (plw-str ")" )))
	   (t (plw-str str) (plw-str "D(") (plw-term pred) (plw-argz *va* cdrtrm) (plw-str ")" ))))
  ;((equal (length trm) 1) (progn (plw-str str) (plw-str "1(") (plw-term (first trm)) (plw-str ")")))
  ;((equal (length trm) 2) (progn (plw-str str) (plw-str "2(") (plw-term (first trm)) (plw-str ",") (plw-term (my-second trm)) (plw-str ")")))
  ;((equal (length trm) 3) (progn (plw-str str) (plw-str "3(") (plw-term (first trm)) (plw-str ",") (plw-term (my-second trm)) (plw-str ",") (plw-term (my-third trm)) (plw-str ")")))
  ((stringp (my-second trm)) (progn (plw-str str) (plw-str "S(") (plw-term (first trm)) (plw-argz t (cdr trm)) (plw-str ")")))
  (t (my-debug (progn (plw-str str) (plw-str "N(") (plw-term (first trm)) (plw-argz t (cdr trm)) (plw-str ")")))))))))
      
                     
(define assertion-varnames (assrt)
  (clet ((varnames ()))
   (cdolist (v (assertion-el-variables assrt))
     (csetq varnames (append varnames (list (pl-varname  v)))))
     (ret varnames)))

(define plw-dotl (cdrtrm)
  (plw-list cdrtrm "[" "]"))
         
(define plw-argz (va cdrtrm) 
 (pif va 
  (pif (null cdrtrm) 
   (plw-str ",'[]'")
   (progn (plw-str ",") (plw-dotl cdrtrm)))
  (pwhen (null (null cdrtrm)) (plw-str ",") (plw-list  cdrtrm))))

(define plw-string (trm) 
 (pcond 
   ((cnot (cand (search " " trm) (< 3 (length trm)))) (princ (write-to-string trm)))
   (t (clet ((b (split trm))(a (car b))(d (cdr b)))
       (pcond ((cnot d) (princ (write-to-string trm)))
       (t 
          (princ "s(") (princ (write-to-string a))
         (cdolist (c d) (princ ",") (princ (write-to-string c)))
	 (princ ")")))))))

(define rsw (str) (ret str))

(define split (str)
  (clet ((word nil) (prev #\Space) (tokens nil) (*split-ignore-punctuation?* ())   (slen (length str)))
     (cdotimes (index slen)
       (clet ((curr (char str index)))
         (pcond ((eq curr #\Space) (pwhen word (cpush (rsw word) tokens) (csetq word nil)))
;; self
	        ((member curr '(#\' #\" #\; #\? #\% #\, #\& #\( #\) ))
                 (pwhen word
                   (cpush (rsw word) tokens)
                   (csetq word nil))
                 (cpush (my-coerce-string curr) tokens))
;; beginer
                ((member curr '(#\< #\# ))
                   (pwhen word (cpush (rsw word) tokens))
                   (csetq word (my-coerce-string curr)))		   
;; finisher
                ((member curr '( #\> #\$))
                   (pif word
		       (progn (csetq word (format nil "~A~C" word curr))
                       (cpush (rsw word) tokens)
                       (csetq word nil))
		       (cpush (my-coerce-string curr) tokens)))
;; period
              ((cand (eq curr #\.) (cor (eq index (- slen 1)) (eq (char str (+ index 1)) #\Space)))
                   (pwhen word
                       (cpush (rsw word) tokens) 
                       (csetq word nil)
                       (cpush "." tokens)))
;; aalphanums
                ((cor (alphanumericp curr) (member curr '( #\_ #\. #\# #\- #\/ #\< )) )
                 (pif word (csetq word (format nil "~A~C" word curr)) (csetq word (format nil "~C" curr))))
;; rest
            (t (pwhen word (cpush (rsw word) tokens)) (csetq word nil)
		    (cpush (my-coerce-string curr) tokens)))
         (csetq prev curr)))
     (pwhen word (cpush (rsw word) tokens))
     (ret (nreverse tokens))))


(define pl-varname (trm)(ret (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm))))

(define unescape (str)
 (csetq str (SUBSTITUTE #\| #\Return (SUBSTITUTE #\| #\Linefeed str)))
 (ret (fif (< (length str) 3) str (string-trim '(#\Space #\|) str))))

(define plw-term (trm &optional (is-pred ()))
 (pcond 
  ((nart-p trm) (plw-naut "nart" (nart-hl-formula trm)))
  ((assertion-p trm) (plw-term (assertion-el-ist-formula trm)))
  ((equal trm "[]") (plw-str "'[]'"))
  ((null trm) (plw-str "'[]'"))
  ((HL-variable-p trm) (progn (plw-term (nth (variable-id trm) (ASSERTION-EL-VARIABLES *a*)))))
  ((EL-variable-p trm) (progn 
        ;;(plw-str "'$VAR'('")
	(plw-str (pl-varname trm)) 
	;;(plw-str "')")
	))
  ((constant-p trm) (plw-atom trm))
  ((keywordp trm)
     (progn 
            ;;(plw-str "'$KW'(" )          
             ;; (plw-atom (symbol-name trm))
	        ;;(plw-str ")")

            ;;(plw-str "kw_")
            ;;(plw-str (string-downcase (symbol-name trm)))
           
            (plw-atom (cconcatenate ":" (symbol-name trm)))

	    ))
  ((symbolp trm) (progn (plw-str "'") (plw-str (package-name (symbol-package trm))) (plw-str ":") 
                                   (plw-str (symbol-name trm)) 
                                   (plw-str "'")))
  ((stringp trm) (plw-string (unescape trm)))
  ((cnot (consp trm)) (plw-str (write-to-string trm)))
  ;;((numberp trm)  (plw-str trm))
  (t  (plw-term-cons trm is-pred ))))

(define plw-term-cons (trm &optional (is-pred ())) 
   (clet ((pred (car trm)) (wtrm trm) (cdrtrm (cdr trm)))
    (pcond
	;;( (equal pred 'QUOTE) (progn (plw-str "[")(plw-list cdrtrm)(plw-str "]")))
	;;( (equal pred #$TheList) (progn (plw-str "[")(plw-list cdrtrm)(plw-str "]")))
	;;( (equal pred ".")  (plw-dotl cdrtrm))
	( (function? pred) (plw-naut "u" wtrm))
	((cor (member pred *no-tify*) (cand (constant-p pred) (predicate? pred))) (progn (plw-atom pred) (plw-list cdrtrm "(" ")")))
        ((cnot (cor is-pred (predicate? pred)))  (progn (plw-naut "u" wtrm)))
	( (constant-p pred)
		(progn (plw-str  "t(") 
		    (plw-atom pred)
		    (pwhen cdrtrm (plw-list cdrtrm "," " "))
		  (plw-str ")")))
	(t (progn (plw-list wtrm "t(" ")"))))))

;;(plw-term-cons '(1 2 3))

;;(plw-term-cons '(1 2 3))
  
(defvar *no-tify* (all-instances #$WFFConstraintPredicate))
(define pre-export ()
	(cdolist (s (all-instances #$Quantifier))
	  (cpushnew s *no-tify*))
	(cdolist (s (all-instances #$HLPredicate))
	  (cpushnew s *no-tify*))
	(cdolist (s (all-instances #$MicrotheoryDesignatingPredicate))
	  (cpushnew s *no-tify*))
	(cdolist (s (all-instances #$LogicalConnective))
	  (cpushnew s *no-tify*))
	(cdolist (s (all-instances #$KBDependentRelation))
	  (cpushnew s *no-tify*))
	(cdolist (s (all-instances #$ModalPredicate))
	  (cpushnew s *no-tify*))
	(cpushnew #$trueRule *no-tify*) )

#-:LOADING
(force-print "loading export.lisp...")

(define constant-prolog-name (trm)
 (clet ((found (gethash trm *renames* )))
   (pwhen (stringp found) (ret found))
 (pwhen (constant-p trm)
   (ret (constant-prolog-name (constant-name trm))))
 (ret trm)))   

(define plw-writel (assrt)
   (clet ((carform (car assrt))(cdrform (cdr assrt)))
      (plw-term (car assrt))
  (pwhen cdrform (plw-list cdrform "," " "))))

(define assertion-id-plusn-with-vars-string (assrt)
  (csetq elvars (ASSERTION-EL-VARIABLES assrt))
  (punless elvars (ret  (assertion-id-plusn assrt)))
  (clet ((idstr (cconcatenate "'a" (my-coerce-string (assertion-id-plusn assrt)) "'")))
  (pwhen elvars
     (csetq idstr (cconcatenate idstr  "("))
     (csetq idstr (cconcatenate idstr (pl-varname (car elvars))))
     (cdolist (v (cdr elvars))
	     (csetq idstr (cconcatenate idstr ","))
	     (csetq idstr (cconcatenate idstr (pl-varname v))))
     (csetq idstr (cconcatenate idstr ")")))
  (ret idstr)))

(define assertion-id-plusn-string (assrt)
  (ret (cconcatenate "a" *kbnum* "_" (my-coerce-string (assertion-id-plusn assrt)))))


(define showa (assrt &optional out) 
 (clet (elvars found arg0 (*standard-output* (fif out out *standard-output*)))
  (pwhen (integerp assrt) (csetq found (FIND-ASSERTION-BY-ID assrt))(pwhen found (csetq assrt found)))
  (pwhen (consp assrt) (csetq found (FIND-ASSERTION-ANY-MT assrt))(pwhen found (csetq assrt found)))
  (csetq *assrtform* (assertion-formula assrt))
  (csetq arg0 (car *assrtform*))
  (pwhen *assrtform*
     (punless (cand nil (equal arg0 #$termOfUnit))
	  (csetq *a* assrt)
      (csetq elvars (ASSERTION-EL-VARIABLES *a*))
      (csetq *assrtid* (assertion-id-plusn-with-vars-string assrt))

      (fif (equal arg0 #$not) 
         (progn (plw-str "assertion_content_not(")(plw-list (my-second *assrtform*)) (plw-str ",") (plw-str *assrtid*) (plw-str ").")(plw-nl))
      (fif (cand (equal arg0 #$implies) (my-second *assrtform*) (my-third *assrtform*))

	     (progn (plw-str "assertion_content_implies(") 
                (my-debug (plw-list (my-third *assrtform*)))
                (plw-str ",")
                (my-debug (plw-term (my-second *assrtform*)))
                (plw-str ",") (plw-str *assrtid*) (plw-str ").")(plw-nl))

         
         (my-debug (progn (plw-str "assertion_content(")(plw-list *assrtform*) (plw-str ",") (plw-str *assrtid*) (plw-str ").")(plw-nl)))))

	  ( ruleinfo assrt 'assertion-mt)
	  (pwhen elvars (plw-str "assertion_varnames(")(plw-str *assrtid*) (plw-str ",[") (plw-list (assertion-varnames *a*)) (plw-str "]).")(plw-nl))

	  (my-debug ( plw-ruleinfo (fif (RULE-ASSERTION? assrt) "rule" "fact")))
      (my-debug ( plw-ruleinfo (string-downcase (symbol-name (assertion-strength assrt)))))
	  (my-debug ( plw-ruleinfo (string-downcase (symbol-name (assertion-direction assrt)))))
 	  (my-debug (ruleinfo-not assrt 'ASSERTION-P "assertion"))
      (mruleinfo assrt)
      ))))
          


(define plw-ruleinfo (str &optional value)
 (plw-str "assertion_")(plw-str str)
      (plw-str "(") 
      (plw-str *assrtid*)
      (punless (booleanp value) 
         (plw-str ",")
	  (fif (symbolp value) 
        (plw-str (string-downcase (symbol-name value)))
         ( progn 
	 (plw-term value)	 
	 )))	 
	 (plw-str ").")(plw-nl))


(define ASSERTION-VARIABLE-GUARD (assrt)
 (pwhen (integerp assrt) (csetq assrt (FIND-ASSERTION-BY-ID assrt)))
  (ret (FORMULA-VARIABLES-ARG-CONSTRAINTS (assertion-formula assrt) (assertion-mt assrt))))





(define ruleinfo (assrt type &optional (str nil))
 (pif (consp type) 
  (cdolist (tipe type)(ruleinfo assrt tipe))
 (pwhen (fboundp type)
  (clet ((value (my-funcall type assrt)))
   (pwhen value      
      (punless str (csetq str (simple-type-str type)))
      (my-debug (plw-ruleinfo str value)))))))


(defvar *error-message* *terminal-io*)

(defmacro no-errors (err &rest code)
 (ret 
 `(clet ((haderror ())(lastVal ()))   
  (with-error-handler
   #'(lambda (&rest whatevah) 
    (csetq haderror *error-message* )
    (force-output)(format t "~%~&")(force-output)
    (format *terminal-io* "~&~&~&~&;; made-handler ~S durring: ~S err= ~S ~S ~&~&~&" 
      *error-message* whatevah ,err ',code) 
      (force-output)(format t "~%~&")(force-output)
      ;; (break)
      (ret haderror))
   (csetq lastVal (progn (csetq lastVal ())(csetq haderror ()) ,@code)))
    (fif haderror (progn (print haderror) ',err) lastVal))))


(define ruleinfo-not (assrt type &optional (str nil))
  (pif (consp type) 
   (cdolist (tipe type)
     (ruleinfo-not assrt tipe))
  (pwhen (fboundp type)
    (punless (my-debug (funcall type assrt))
    (punless str (csetq str (simple-type-str type)))
     (plw-ruleinfo (cconcatenate "not_" str))))))


(define mruleinfo (assrt) 
 (fif (GAF-ASSERTION? assrt)
  (progn 
   (ruleinfo assrt '(NOT-ASSERTIBLE-ASSERTION-P  	CVA-ASSERTION-P     TVA-ASSERTION-P))
   ))
 (fif (RULE-ASSERTION? assrt) 
 (progn 
  (ruleinfo assrt '( SELF-LOOPING-RULE? PROOF-VIEW-PRUNING-RULE? SELF-EXPANDING-RULE? INERT-RULE?
  LOOKS-LIKE-KAPPA-REPHRASE-RULE?  EXPANSION-RULE? APPEARS-IN-FORWARD-REIFICATION-RULE? PSP-TRACED-RULE?
  UNIVERSAL-LIFTING-RULE? DECONTEXTUALIZED-RULE? SOME-SUGGESTED-PRAGMA-SENTENCES-FOR-RULE? CURRENT-NC-RULE? 
  SINGLE-LITERAL-ANTECEDENT-RULE? CONSIDERED-BUT-NOT-SUCCESSFUL-RULE? CONSTRAINT-RULE? 
  LIFTING-CONSEQUENT-RULE? ASSERTIVE-WFF-RULE? TWO-LITERAL-ANTECEDENT-RULE? REWRITE-RULE? 
  SINGLE-LITERAL-RULE? OBSOLETE-RULE? SKOLEM-RULE? FORWARD-TMS-RULE? CYCL-REFORMULATION-RULE? IS-KNOWN-ILP-EXPORT-RULE? 
  VERBOSIFIED-EL-RULE? PPH-REQUIRED-RULE? NON-ABDUCIBLE-RULE? CYCL-REFORMULATOR-RULE? 
  LIFTING-RULE?))
    (ruleinfo assrt '(ATOMIC-ASSERTION? GROUND-ASSERTION?)))

 (progn 
   (ruleinfo-not assrt '( ATOMIC-ASSERTION? GROUND-ASSERTION?))
   (ruleinfo assrt '(CONSIDERED-RULE?))   
  ))
 (ruleinfo-not assrt '(VALID-PPH-BLACKLIST-ASSERTION? WHY-KBS-ASSERTION? EXPORTABLE-ASSERTION? TRUE-ASSERTION? HL-ASSERTION-P 
   NON-TVA-GAF-ASSERTION-P  STR-META-ASSERTION-P FIRST-ORDER-ASSERTION-P ))
 (ruleinfo assrt '( 
 asserted-assertion?
 deduced-assertion?
 code-assertion?
 
ASSERTION-HAS-DEPENDENTS-P
ASSERTION-VARIABLE-GUARD 
ASSERTED-BY
ASSERTED-WHEN

;;  (load "/mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/ext/e2c/export.lisp")
  

SYNTACTICALLY-ILL-FORMED-ASSERTION?  SOME-CONSERVATIVE-VIA-ARG-ASSERTION?
RELEVANT-ASSERTION? REFORMULATOR-RELEVANT-ASSERTION? PPH-HL-SUPPORT-WITH-ISOMORPHIC-ASSERTION? 
OWL-EXPORTABLE-RULE-ASSERTION? NL-TRIE-SYNTACTIC-ASSERTION? IST-SENTENCE-DESIGNATING-ASSERTION?
INVALID-ASSERTION? INFERENCE-RELEVANT-ASSERTION? INFERENCE-IRRELEVANT-ASSERTION? INDIRECT-LEXICAL-ASSERTION?
IMPLEMENTATION-ASSERTION? HAS-GENSTRING-ASSERTION? FALSE-ASSERTION? CURRENTLY-FORWARD-PROPAGATING-ASSERTION? 
CONTEXTUALLY-DEPENDENT-LEXICAL-ASSERTION? COMPUTED-SKOLEM-ASSERTION? CHECK-FOR-ISOMORPHIC-ASSERTION? 
BACKWARD-RULE-REQUIRED-FOR-ASSERTION? 
GEN-TEMPLATE-QUERY-SENTENCE-ASSERTION-P RULE-META-ASSERTION-P FTEMPLATE-POLYCANONICALIZED-ASSERTION-P 
UNBOUND-RULE-ASSERTION-P LIFTING-ASSERTION-P ASSERTION-PRODUCED-BY-TEMPORAL-PROJECTION? 
ABNORMAL-ASSERTION-P ABBREVIATION-ASSERTION-P 
TERM-OF-UNIT-ASSERTION-P LIST-OF-RULE-ASSERTION-P 
DOCUMENTATION-ASSERTION-P BOOKKEEPING-ASSERTION-P META-ASSERTION-P HIGHER-ORDER-ASSERTION-P 
GTQS-ASSERTION-P 

assertion-has-meta-assertions? 

; ASSERTION-ARGUMENTS
; ASSERTION-CONSTANTS
; ASSERTION-FLAGS
ASSERTION-NEEDS-REVIEW-P
; ASSERTION-DEPENDENTS
ASSERTION-USES-NON-EXTENSIONAL-SET?
ASSERTION-SYNTACTICALLY-ILLFORMED?
 )))

;(defmacro no-errors (&rest code) (ret  `(with-error-handler *err-handler* (multiple-value-list ,code ))))

;(cdo-symbols (S) (pwhen (equalp 0 (search "ASSERTION-" (symbol-name S)))(pwhen (fboundp S)  (print (symbol-name S)))))


(define remove-string-ending (str targ) 
  (clet ((len (- (length str)(length targ))))
   (fif (< len 2) (ret str)
   (fif (eql 0 (search (reverse targ) (reverse str)))
     (ret (substring str 0 len))
   (ret str)))))

(define remove-string-starting (str targ) 
   (pwhen (eql 0 (search  targ str))
     (ret (substring str (length targ))))
   (ret str))


;; (simple-type-str "foo-p")
(define simple-type-str (type)
  (csetq type (SUBSTITUTE #\_ #\- (string-downcase (my-coerce-string type))))
  (csetq type (remove-string-starting type "assertion_"))
  (csetq type (remove-string-ending type "_p"))
  (csetq type (remove-string-ending type "_assertions?"))
  (csetq type (remove-string-ending type "_assertion?"))
  (csetq type (remove-string-ending type "_assertion"))
  (csetq type (remove-string-ending type "_rule?"))
  (ret (remove-string-ending type "_rule")))



#|
cd /mnt/sdb1/researchcyc-4.0q/server/cyc/run
timedatectl set-ntp 0 ; timedatectl set-time "2013-08-09 23:39:00" ; timedatectl
./bin/run-cyc.sh
(load "e2c/prologmud.lisp")
timedatectl set-ntp 1 ; timedatectl
|#


(define rana (&optional a)  
   (csetq a (find-assertion-by-id (random (assertion-count))))  
   (cinc  *every1*)
   (pwhen (equal (rem *every1* 2) 1) 
     (cdo  ((anum 1 (1+ anum))) ((= anum 500))
      (pwhen (rule-assertion? a) (ret a))
        (csetq a (find-assertion-by-id (random (assertion-count))))))
   (ret a))

(define w (&optional (start 0))
  (load "e2c/export.lisp") ;; (load "e2c/prologmud.lisp") 
 (w0 start))

(define w0 (&optional (start 0))
(cdo  ((anum (+ start 250) (1- anum))) ((< anum start))
 (csetq *a* (fif (eql start 0) (rana) (find-assertion-by-id anum)))
   (format t "~%~% ")(princ "/* (w " )(princ (assertion-id-plusn *a*) )(princ ") */" )(force-output)
   (showa *a*)
 (force-output))
  (print (list 'w start))
 (force-output)) 


(define plw-support (d)
 (plw-term (list (support-mt d) (support-sentence d) (symbol-name (support-truth d)) (symbol-name (support-strength d)) (symbol-name (support-module d)))))

(define plw-id (str int)
  (plw-str str)
  (plw-str *kbnum*)  
  (plw-str int))

;; (kb-hl-support-count)

(define plw-deduction (ded)
 (csetq	 *ded* ded)
 (csetq *a* (deduction-assertion ded))
 (cdolist 
    (d (KB-DEDUCTION-SUPPORTS ded))

  (plw-str "deduction(")
	 (plw-id "d" (deduction-id-plusn ded))
	 (plw-str ",")
	 (pcond 
	   ((assertion-p *a*) (plw-id "'a" (assertion-id-plusn *a*) "'"))
	   (t (plw-term *a*)))
	 (plw-str ",")
     
      (pcond 
        ((assertion-p d)(plw-id "a" (assertion-id-plusn d)))
        ((HL-SUPPORT-P d)(plw-support d)) 
	((SUPPORT-P d)(plw-support d)) 
     (t (plw-id (type-of d) d)))
 ;;(plw-str ",")(plw-str (string-downcase (symbol-name (KB-DEDUCTION-STRENGTH ded))))
 ;;(plw-str ",")(plw-str (string-downcase (symbol-name (KB-DEDUCTION-TRUTH ded))))
  (plw-str ").")(plw-nl)))
  
(define foc (trm) (ret (find-or-create-constant trm)))

(define ds () (ds1 (foc "denotationAndString")))

(define ds1 (term) 
 ;; (load "e2c/prologmud.lisp")
  (sL::clet ((*file-output* (SL::OPEN-TEXT (cconcatenate (simple-name term) ".pl") :output)) 
    (*standard-output* *file-output*))
   (WITH-ANY-MT (with-all-mts (map-term #'showa term *file-output*)))
 (SL::close *file-output*)))

(define dt () 
(load "e2c/prologmud.lisp")
 (sL::clet ((*file-output* (SL::OPEN-TEXT "/tmp/supports.pl" :output)))
  ;; 0 
  (cdo  ((anum 0 (1+ anum))) ((= anum (deduction-count)))
  (plw-deduction (find-deduction-by-id anum)))
  (SL::close *file-output*)))

(define loadr ()
 (pwhen (equal (constant-prolog-name "between") "between") 
     (force-print (constant-prolog-name "between"))
     '(load "/mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/ext/e2c/const-renamer.lisp")
     '(load "/mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/ext/e2c/renames-real.lisp")
     '(load "/mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/ext/e2c/renames-fixed.lisp")
     (load "/tmp/dump_consts.lisp")
     (force-print (constant-prolog-name "between"))
     (force-print (constant-prolog-name #$between))
     ))

#-:LOADING
;; (force-print "hi0" )

;; 8014          4287714 
(define ri (&optional (start 8014) (len 100)) 
 (load "/mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/ext/e2c/export.lisp")
(cdo 
 ((anum start (1+ anum))) ((= anum (+ start len)))
 (clet ((assrt (find-assertion-by-id anum))) (showa assrt))))


;; ((anum 1190000 (1+ anum))) ((= anum 1190005))
(define ri2 (&optional (start 5848550) (len 50000)) 
 (load "/mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/ext/e2c/export.lisp")
(cdo  
 ((anum start (1+ anum))) ((= anum (+ start len)))
   (clet ((assrt (find-assertion-by-id anum)))
   
    ( pwhen   assrt
    (force-output)(format t "~%~&")(force-output)

    
    (fif (equal (assertion-hl-formula assrt) (assertion-el-formula assrt))
     (progn (princ ";;" )(force-print (assertion-hl-formula assrt)))
     (progn (princ ";;" )(force-print (list (assertion-hl-formula assrt) 'vs (assertion-el-formula assrt))))
      (force-output)(format t "~%~&")(force-output))
     (my-debug (showa assrt)) (force-output)))))
 

;; 5848503-5851381

(define dumpt (&optional (start 5848550))
  (pre-export)
  (loadr)  
  (punless start 
    (sL::clet ((*progress-input* (SL::OPEN-TEXT  "/tmp/dump988-progress.txt" :input)))
   (csetq start (read *progress-input*))(close *progress-input*)))

	(sL::clet ((*file-output* (SL::OPEN-TEXT (cconcatenate "/tmp/dump988-" (write-to-string start) "-X.txt") :output)))
(princ "
:- style_check(-discontiguous). 
:- style_check(-singleton).
:- include('dir.header').
" *file-output* )
(noting-percent-progress "exporting KB..."
 (clet ((total (+ (assertion-count) 200000)))
  (cdo ((anum start (1+ anum))) ((= anum total))
	 (clet ((assrt (find-assertion-by-id anum)))
     ( pwhen   assrt
      (cinc *every1*)

	  (pwhen  (equal (rem *every1* *oftenFlush*) 0)
           (force-output *file-output*)
           (sL::clet ((*progress-output* (SL::OPEN-TEXT  "/tmp/dump988-progress.txt" :output)))
             (print anum *progress-output*)(force-output *progress-output*)(close *progress-output*))
             (princ "." *terminal-io*)(force-output *terminal-io*)
           )

	  (pwhen  (equal (rem  *every1* *oftenProgress*) 0)
           (showa assrt *terminal-io*) (force-output *terminal-io*))

   (force-output *file-output*) (showa assrt *file-output*)(force-output *file-output*))
   (note-percent-progress anum (- total start))))))
 (force-output *file-output*)(SL::close *file-output*)))


;; #-:LOADING (force-output) #-:LOADING (plw-str "hi1")


;; (LOAD-KE-TEXT-FILE #$CycAdministrator "e2c/e2c-new-0.ke" :agenda t)

(define et ()
  (fif ( PROBE-FILE "e2c/export.lisp") (load "e2c/export.lisp")(load "/opt/cyc/e2c/export.lisp"))
   (dumpt))

;;(asserted-by (find-assertion-by-id (random (assertion-count))))

(define save-consts ()
 (sL::clet ((*file-output* (SL::OPEN-TEXT "/tmp/dump_consts.lisp" :output)))
   (maphash #'(lambda (key value) (format *file-output* "(safely-rename-or-merge ~S ~S) ~%" key value))
             *renames*)          
  (SL::close *file-output*))
)

  



(define save-constsl ()
 (sL::clet ((*file-output* (SL::OPEN-TEXT "/tmp/dump_consts.lisp" :output)))
   (cdo ((*x* 0 (+ *x* 1))) ((= *x* (constant-count)))
     (clet ((const (FIND-CONSTANT-BY-INTERNAL-ID *x*))(found (gethash const *renames* )))
       (pwhen found
          (clet ((name (constant-name const)))         
          (format *file-output* "(safely-rename-or-merge ~S ~S) ~%" name found)))))
  (SL::close *file-output*)))

;; (load "dump_consts.lisp")
(define save-consts2 ()
 (sL::clet ((*file-output* (SL::OPEN-TEXT "/tmp/dump_consts.pl" :output)))
   (cdo ((*x* 0 (+ *x* 1))) ((= *x* (constant-count)))
     (clet ((const (FIND-CONSTANT-BY-INTERNAL-ID *x*))(found (gethash const *renames* )))
       (pwhen found
          (clet ((name (constant-name const)))         
          (format *file-output* "rnc('~A','~A'). ~%" name found)))))
  (SL::close *file-output*)))

#-:LOADING
(force-output)
(define check-consts ()
   (cdo ((*x* 0 (+ *x* 1))) ((= *x* (constant-count)))
     (clet ((const (FIND-CONSTANT-BY-INTERNAL-ID *x*))(found (gethash const *renames* )))
       (punless found
          (clet ((name (constant-name const)))
         (pwhen (prolog-need-quote name)
          (format t "(safely-rename-or-merge ~S ~S) ~%" name (cconcatenate "cyc" name ))))))))
(define check-consts2 ()
   (cdo ((*x* 0 (+ *x* 1))) ((= *x* (constant-count)))
     (clet ((const (FIND-CONSTANT-BY-INTERNAL-ID *x*))(found (gethash const *renames* )))
       (pwhen found
         (pwhen (prolog-need-quote found)
	   (clet ((name (constant-name const))(newname (simplRename  found)))
          (format t "(safely-rename-or-merge ~S ~S) ~%" name newname)
	  (sethash const *renames* newname)))))))
     

(define simplRename (found)
   (clet ((nfound (SUBSTITUTE #\_ #\- (SUBSTITUTE #\_ #\: found))))
    (clet ((ch (char nfound 0))(ch1 (char nfound 1)))
     (pwhen (cnot (lower-case-p ch))
      (ret (cconcatenate "i" nfound )))
     (pwhen (cand (eql ch #\t)(lower-case-p ch1))
        (ret (cconcatenate "t" (string-upcase (my-coerce-string ch1)) (substring nfound 2))))
   (ret nfound))))

#|
(check-consts2)
|#


#-:LOADING
(force-print "done loading export.lisp...")

;; (dumpt)

' (sl:csetq *PACKAGE* cyc::*OLD-PACKAGE*)
