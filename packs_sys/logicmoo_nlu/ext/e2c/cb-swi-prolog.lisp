
(defun foc (str) (find-or-create-constant str))

(defvar *prologMt* (foc "PrologDataMt"))
(defvar *theList* (foc "TheList"))
(defvar *entailedBy* (foc "prolog:entailedBy"))
(defvar *prologFn* (foc "prolog:fn"))
(defvar *prologPredicate* (foc "prolog:ProgrammingPredicate"))
(defvar *prologCons* (list *prologFn* "."))
;;(defvar *prologNil* (foc "TheEmptyList"))
(defvar *prologNil* ())
(defvar *lispNil* (list *prologFn* "[]"))

(defun prolog-assert-now (sent &optional (mt *prologmt*)) (prolog-assert-now-real sent mt))

(defun prolog-assert-now-real (sent &optional (mt *prologmt*))
  (let (
        (this (list 'ke-assert-now (list 'quote sent) (list 'quote mt) :monotonic :FORWARD ))
        (retval (car (FIND-ASSERTIONS-CYCL sent mt))))
    (print this)(force-output)
    (if (assertion-p retval) (ret retval))
    (let ((res (eval this))(retval (car (FIND-ASSERTIONS-CYCL sent mt))))
      (print retval)(if (equal res t) (ret retval))
      (print (list :ERROR (HL-EXPLANATION-OF-WHY-NOT-WFF sent mt)))
      (force-output))))


(prolog-assert-now (list (foc "isa") *prologMt* (foc "DataMicrotheory")) (foc "BaseKB"))
(prolog-assert-now (list (foc "isa") *prologMt* (foc "ApplicationContext")) (foc "BaseKB"))
(prolog-assert-now (list (foc "comment") *prologMt* "The #$DataMicrotheory that holds a the prolog program used by <a href=\"cg?cb-prolog&mt=PrologDataMt&prolog=Listing&code=true.\" target=\"cyc-main\">Listing of #$PrologDataMt</a>") *prologMt*)


(prolog-assert-now (list (foc "isa") *prologPredicate* (foc "Collection")) *prologMt*)
(prolog-assert-now (list (foc "genls") *prologPredicate* (foc "Relation")) *prologMt*)
;;(prolog-assert-now (list (foc "genls") *prologPredicate* (foc "Predicate")) *prologMt* )
(PROLOG-ASSERT-NOW
 (LIST (FOC "implies") (LIST (FOC "isa") '?PRED *PROLOGPREDICATE*)
   (LIST (FOC "and") 
     (LIST (FOC "completeExtentEnumerable") '?PRED)
     ;; (LIST (FOC "argsQuotedIsa") '?PRED (FOC "CycLIndexedTerm")) 
     (LIST (FOC "argsIsa") '?PRED (FOC "Thing"))
     ;; (LIST (FOC "argFormat") '?PRED 2 (FOC "openEntryFormatInArgs"))
     (LIST (FOC "argFormat") '?PRED 1 (FOC "openEntryFormatInArgs"))
     ;;(LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveSomeTermsAtEL"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "DontReOrderCommutativeTerms"))
     ;;(LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "AllowKeywordVariables"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveSomeTermsAtELAndAllowKeywordVariables"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "AllowGenericArgVariables"))
     (LIST (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveVariablesAtEL"))
     ))
 *PROLOGMT*)

(defvar *prologSource* (foc "prolog:sourceCode"))
(defvar *ist* (foc "ist"))

(prolog-assert-now (list (foc "isa") *prologSource* (foc "BinaryPredicate")) *prologMt*)
(prolog-assert-now (list (foc "isa") *prologSource* (foc "StrictlyFunctionalSlot")) *prologMt*)
(prolog-assert-now (list (foc "isa") *prologSource* *prologPredicate*) *prologMt*)
(prolog-assert-now (list (foc "arg1Isa") *prologSource* (foc "Individual")) *prologMt*)
(prolog-assert-now (list (foc "arg2Isa") *prologSource* (foc "List")) *prologMt*)
(prolog-assert-now (list (foc "argFormat") *prologSource* 2 (foc "openEntryFormatInArgs")) *prologMt*)


(prolog-assert-now (list (foc "isa") *entailedBy* (foc "TransitiveBinaryPredicate")) *prologMt*)
(prolog-assert-now (list (foc "isa") *entailedBy* (foc "AsymmetricBinaryPredicate")) *prologMt*)
(prolog-assert-now (list (foc "isa") *entailedBy* *prologPredicate*) *prologMt*)
;;(prolog-assert-now (list (foc "argsQuotedIsa") *entailedBy* (foc "CycLSentence")) *prologMt*)
(prolog-assert-now (list (foc "comment") *entailedBy* "The prolog neck (:-) predicate - ?arg1 :- ?arg2, ?arg3, ... .") *prologMt*)

;;(prolog-assert-now (list (foc "argsQuotedIsa") *prologClause* (foc "CycLExpression")) *prologMt*)
;;(prolog-assert-now (list (foc "isa") *prologFn* (foc "IndividualDenotingFunction")) *prologMt*)
;;(prolog-assert-now (list (foc "isa") *prologFn* (foc "TotalFunction")) *prologMt*)
(prolog-assert-now (list (foc "isa") *prologFn* (foc "VariableArityPredicate")) *prologMt*)
;;(prolog-assert-now (list (foc "isa") *prologFn* (foc "UnreifiableFunction")) *prologMt*)
(prolog-assert-now (list (foc "comment") *prologFn* "A prolog predicate clause - ?arg1(?arg2, ?arg3, ...).") *prologMt*)
;;(prolog-assert-now (list (foc "resultIsa") *prologFn* (foc "Thing")) *prologMt*)

(prolog-assert-now (list (foc "isa") *prologFn* *prologPredicate*) *prologMt*)



(defmacro defprolog (name farity args &rest body)
  (let ((arity (length `,args))
        (fname (concatenate 'string `,name "/" (write-to-string `,farity))))
    (let ((sym (intern `,fname)))
      (let ((fort (prolog-make-cycl `,fname `,arity)))
        (setf (get `,sym 'evaluable) t)
        (setf (get `,sym 'prolog_name) `,name)
        (setf (get `,sym 'prolog_arity) `,farity)
        (setf (get `,sym 'lisp_arity) `,arity)
        (prolog-assert-now (list (foc "arity") `,fort `,arity ) *prologMt* ) 
        (prolog-assert-now (list (foc "evaluationDefn") `,fort (list (foc "SubLQuoteFn") `,sym)) *prologMt* ) 
        `(defun-evaluation-defn ,sym ,args (ret (progn . ,body)))))))

(defun prolog-sentences-to-string (trms )
  (ret (if (null trms) " " (string-concat (prolog-to-string (car trms)) "." (make-string 2 #\Newline) (prolog-sentences-to-string (cdr trms))))))

(defun prolog-need-quote (str) (let ((ch (char str 0)))
                                 (if (member ch '(#\: #\' #\` #\#) #'char=) (ret nil))
                                 (ret (or (not (lower-case-p ch)) (find #\: str)))))


(defun prolog-list-to-string (fun trm &optional (begstr "") (endstr "") (nullstr "")) 
  (if (null trm ) (ret (string-concat begstr endstr)))
  (if (atom trm ) (ret (funcall fun trm)))
  (ret (string-concat begstr (funcall fun (car trm)) (prolog-cdr-to-string fun (cdr trm) "," "|" nullstr) endstr )))

(defun prolog-cdr-to-string (fun trm &optional (commastr ",")(barstr "|") (nullstr "")) 
  (cond
   ((null trm) (ret nullstr))
   ((consp trm) (ret (string-concat commastr (funcall fun (car trm)) 
                       (prolog-cdr-to-string fun (cdr trm) commastr barstr nullstr))))
   (t (ret (string-concat barstr (funcall fun trm))))))


(defun prolog-to-name (trm)
  (cond
   ((stringp trm) (ret (if (prolog-need-quote trm) (string-concat "'" trm "'") trm)))
   ((constant-p trm) (ret (string-concat "#$" (constant-name trm) "")))
   (t (prolog-to-name (prolog-to-string trm)))))


(defun prolog-to-string (trm )
  (cond 
   ((nart-p trm) (ret (string-concat "nart(" (prolog-to-string (nart-el-formula trm)) ")")))
   ((assertion-p trm) (ret (prolog-to-string (assertion-el-ist-formula trm))))
   ((equal trm *prologNil*) (ret "[]"))
   ((null trm) (ret "[]"))
   ((el-variable-p trm) (ret (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm))))
   ((hl-variable-p trm) (ret (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm))))
   ((constant-p trm) (ret (string-concat "#$" (constant-name trm) "")))
   ((stringp trm) (ret (write-to-string trm)))
   ((numberp trm) (ret (write-to-string trm)))
   ((symbolp trm) (ret (prolog-to-name (symbol-name trm))))
   ((not (consp trm)) (ret (prolog-to-name (write-to-string trm))))
   (t 
    (let ((pred (elt trm 0))(trm (cdr trm)))
      (if (equal pred *ist*) (ret (prolog-to-string (cdr trm))))
      (if (equal pred *prologFn*) 
          (progn 
            (setq pred (car trm))(setq trm (cdr trm))
        (if (equal pred ".") (ret (string-concat "[" (prolog-to-string (first trm)) "|" (prolog-to-string (second trm)) "]" )))
        (if (member pred *xfx* #'equal) 
            (ret (string-concat (prolog-to-string (car trm)) " " (prolog-to-name pred) " " 
                (prolog-list-to-string #'prolog-to-string (cdr trm) "" "" ))))
        (ret (string-concat (prolog-to-name pred) (prolog-list-to-string #'prolog-to-string trm "(" ")")))
        ))(ret (prolog-list-to-string #'prolog-to-string trm "[" "]"))))))



(defun prolog-make-cycl (trm &optional (mt *prologmt*))
  (cond 
   ((assertion-p trm) (ret trm))
   ((null trm) (ret *prologNil*))
   ((el-variable-p trm) (ret trm))
   ((hl-variable-p trm) (ret trm))
   ((constant-p trm) (ret trm))
   ((fort-p trm) (ret trm))
   ((stringp trm) (ret trm))
   ((numberp trm) (ret trm))
   ((prolog-predicatep trm) (ret trm))
   ((nart-p trm) (ret trm))
   ((atom trm) (ret (list *prologFn* trm)))
   (t (let ((pred (car trm))(trm (cdr trm)))
        (if (equal pred *ist*) (ret (prolog-make-cycl (elt trm 2) (elt trm 1))))
        (if (member pred '("." '|.| *prologCons*)) (ret (prolog-make-cons (prolog-make-cycl (car trm) mt)(prolog-make-cycl (cdr trm) mt))))
        (if (not (equal pred *prologFn*) ) (ret (prolog-make-cons (prolog-make-cycl pred)(prolog-make-cycl trm))))
        (setq pred (car trm) trm (cdr trm))
        (if (member pred '("." '|.| *prologCons*)) (ret (prolog-make-cons (prolog-make-cycl (car trm) mt)(prolog-make-cycl (cdr trm) mt))))
        (ret (cons *prologFn* (cons (prolog-make-pred pred) (prolog-make-cdr trm))))))))

(defun prolog-make-cdr (trm &optional (mt *prologmt*))
  (if 
      (atom trm) 
      (if trm (prolog-make-cycl trm mt) trm)
    (cons (prolog-make-cycl (car trm) mt) (prolog-make-cdr (cdr trm) mt))))

(defun prolog-make-cons (car cdr) (ret (cons car cdr)))

(defun prolog-car (trm)
  (ret (if (prolog-consp trm) (second trm) (third trm))))

(defun prolog-cdr (trm)
  (ret (if (prolog-consp trm) (third trm) (fourth trm))))

(defun prolog-consp (trm)
  (and (consp trm) (if (equal (car trm) *prologfn*) (equal (second trm) *prologCons*) t)))

(defun prolog-predicatep (fort)
  (ret (and (fort-p fort) 
         (or (predicate? fort)(cyc-query (list (foc "isa") fort *prologPredicate*) *prologMt*)))))



(defun prolog-make-op (trm)
  (cond
   ((stringp trm) (ret (list (foc "OperatorFn") trm)))
   ((el-variable-p trm) (ret trm))
   ((hl-variable-p trm) (ret trm))
   ((fort-p trm) (ret trm))
   ((nart-p trm) (ret trm))
   ((constant-p trm) (ret trm))
   ((symbolp trm) (ret (list (foc "OperatorFn") (symbol-name trm))))
   (t (ret trm))))

(defun prolog-make-pred (trm &optional arity)
  ;; (print (list 'prolog-make-pred (list 'quote trm)))
  (cond
   ((el-variable-p trm) (ret trm))
   ((prolog-predicatep trm) (ret trm))
   ((constant-p trm) (ret trm))
   ((symbolp trm) (ret (prolog-make-pred (symbol-name trm))))
   ((stringp trm) (ret trm))
   ((atom trm) (ret trm))
   ((dotted-list-p trm) (ret (prolog-make-pred (car trm) '??)))
   (t
    (let ((pred (elt trm 0)))
      (if (equal *prologFn* pred) (ret (prolog-make-pred (cdr trm))))
      (if (equal '|:-| pred) (ret (prolog-make-pred (elt trm 1))))
      (if (equal *ist* pred) (ret (prolog-make-pred (elt trm 2))))
      ;;(if (equal *entailedBy* pred) (ret (prolog-make-pred (elt trm 1))))
      (if (null arity) (setq arity (- (length trm) 1)))
      (ret (prolog-make-pred pred arity))))))



#|
(print (prolog-make-cycl '("member" ?item (?item . ?rest))))
(print (prolog-make-pred '("member" ?item (?item . ?rest))))
(print (prolog-make-cycl '(|:-| ("member" ?item (?x . ?rest)) ("member" ?item ?rest))))
(print (prolog-make-pred '(|:-| ("member" ?item (?x . ?rest)) ("member" ?item ?rest))))
(load "prolog_to_cycl.lisp")
(print (prolog-make-cycl 1))
|#

 
(defun first-answer (ansrs)
  (ret (if (consp ansrs) (car ansrs))))


(defun prolog-pred-source (pred &optional (mt *prologmt*))
  (first-answer (ask-template '?cycl (list *prologSource* (prolog-make-op pred) (cons *theList* '?cycl)) mt)))

(defun prolog-update-code (pred new &optional old (mt *prologMt*))
  (setq pred (prolog-make-op pred))
  (if (null old) (setq old (prolog-pred-source pred mt)))
  (if old (fi-unassert (list *prologSource* pred (cons *theList* old)) mt))
  (prolog-assert-now (list *prologSource* pred (cons *theList* new)) mt))

(defun prolog-assertz (sent &optional pred (mt *prologmt*))
  (if (null pred)(setq pred (prolog-make-pred sent)))
  (let ((code (prolog-pred-source pred))
        (nxt (prolog-make-cycl sent mt))
        (new (append code (list nxt))))
    ;;(print (list code '=> new))(force-output)
    ;;(print (list *prologSource* pred (cons *theList* new)))
    ;;(prolog-assert-now (list *prologSource* pred (cons *theList* new)) mt)))
    (prolog-update-code pred new code mt)))


;;(fi-assert (list *prologSource* pred (cons *theList* new)) mt)))
;; (if (null nxt) (throw :UNEVALUATABLE (list :ERROR "unassertable" nxt mt)))
;;


;;(prolog-make-cycl '("member" ?item (?item . ??rest)) *prologMt*)


;; TODO - dump mt to FOPL
(defun prolog-listing (mt) 
  (let ((res nil))
    (if (mt? mt) (setq mt (ask-template '?pred (list *prologSource* '?pred '?first) mt)))
    (if (atom mt) (setq mt (list mt)))
    (mapcar #'(lambda (x) (setq res (append res (prolog-pred-source x)))) mt)
    (ret res)))


;;(prolog-make-cycl '(|member| ?item (?item . ??rest)) *prologMt*)
(if (prolog-pred-source '|member|) ()
  (progn 
    ;;(prolog-make-cycl '(|:-| ("member" ?item (??skip . ?rest)) ("member" ?item ?rest)) *prologMt*)
    (prolog-assertz '(|member| ?item (?item . ??rest)) *prologMt*)
    (prolog-assertz '(|:-| (|member| ?item (??skip . ?rest)) (|member| ?item ?rest)) *prologMt*)
    ))

#|

CODE 
CODESTR 
HTTPVARSIN 
MT 
MTSTRING 
PROLOGCMDSTR 


|#




(defun prolog-readmacro-star (stream char)
  (let ((*standard-input* (if (streamp stream) stream *standard-input*))(red (read_term (rch))))
    (princ "; Read: ")(princ red)(terpri)(force-output)
    (ret (values red))))


(set-macro-character #\% (get-macro-character #\;))
(set-macro-character #\$ #'prolog-readmacro-star)

(myprint "loaded")


'(defun main (mainloop))



(defun fix-httpvars (httpvars)
  (if (null httpvars) ()
    (let ((item (car httpvars))(httpvars (cdr httpvars)))
      (if (and (consp item) (stringp (second item)) (stringp (car httpvars)))
          (fix-httpvars (cons (list (car item) (string-concat (second item) "|" (car httpvars))) (cdr httpvars)))
        (cons item (fix-httpvars httpvars))))))

(defun httpvar (name httpvars) 
  (let ((retval ()))
    (and httpvars name 
      (mapcar 
          #'(lambda (trm) 
              (cond 
               ((and (atom trm) (equal name trm)) (setq retval (cons t retval)))
               ((and (consp trm) (equal name (car trm))) (setq retval (cons (if (consp (cdr trm)) (cadr trm)(cdr trm)) retval)))
               (t nil)))
        httpvars ))
    retval))

;; todo my appoligies for using format - just quick way to maintain html insead of cyc's better html interface
;; test with 
#+CYC-BROWSER
(define-html-handler cb-prolog (httpvarsIn)
  (defun prolog-assert-now (sent &optional (mt *prologmt*)) )
  ;;(load "cb_prolog.lisp")
  (defun prolog-assert-now (sent &optional (mt *prologmt*)) (prolog-assert-now-real sent mt))
  (let ((*standard-output* *html-stream*)(read-error NIL)(httpvars (fix-httpvars httpvarsIn))
        (prologcmdstr (car (httpvar "prolog" httpvars)))
        (mtstring (car (httpvar "mt" httpvars)))
        (codestr (remove-comments (car (httpvar "code" httpvars))))
        (code :UNREAD)
        (mt NIL))
    (unwind-protect (progn (if (and (stringp mtstring) (CONSTANT-NAME-SPEC-P mtstring)) (foc mtstring))) t)
    (if (null mt) (setq mt *prologMt*))
    (setq mtstring (constant-name mt))
    (format t "<HTML>
 	<HEAD>
 		<TITLE>Prolog In Cyc</TITLE>
 		<META NAME='GENERATOR' Content='Microsoft Visual Studio .NET 8.0'>
 	</HEAD>
 	<BODY>
 		<FORM method='get' action='cg?cb-prolog' ID='prolog_form'>
 			<INPUT id='cb-prolog' type='hidden' name='cb-prolog'>
 			<BR>
 			Interpret in&nbsp;Mt &nbsp; <INPUT id='mt' type='text' name='mt' value='~a'> 
 			<INPUT id='listing' type='submit' value='Listing' name='prolog'> &nbsp; <a href='cg?cb-cf&c~a'>Examine ~a</a><BR>
 			<BR>
 			<TEXTAREA wrap=off id='code' name='code' rows='16' cols='100%'>" 
      mtstring (constant-internal-id mt) mtstring )
    (force-output)
    (unwind-protect
        (cond
         ((weak-string-equal prologcmdstr "Listing") 
          (setq code (prolog-listing mt))
          (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
          (princ (prolog-sentences-to-string code))
          )
         ;;(unwind-protect (ccatch :UNEVALUATABLE read-error (setq code (prolog-read codestr))) t)
         ((weak-string-equal prologcmdstr "call (?-)") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- ")(princ (prolog-to-string code))(princ ".")(terpri)(terpri)(force-output)
            (prolog-call code mt)))
         ((weak-string-equal prologcmdstr "assert(z)") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- assert(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-assertz code mt)))
         ((weak-string-equal prologcmdstr "asserta") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- asserta(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-asserta code mt)))
         ((weak-string-equal prologcmdstr "retract") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- retract(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-retract code mt)))
         ((weak-string-equal prologcmdstr "retractall") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- retractall(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-retractall code mt)))
         ((weak-string-equal prologcmdstr "listing") 
          (let 
              ((code (string-to-term codestr)))
            (princ "%% cycl = ")(prin1 code)(terpri)(terpri)(force-output)
            (princ "%% ?- listing(")(princ (prolog-to-string code))(princ ").")(terpri)(terpri)(force-output)
            (prolog-retractall code mt)))
         (t (princ codestr)))
      
      (format t "</TEXTAREA><BR>
 			<BR>
 			<INPUT id='call' type='submit' value='call (?-)' name='prolog'> &nbsp; 
  <INPUT id='assert' type='submit' value='assert(z)' name='prolog'> &nbsp;
  <INPUT id='asserta' type='submit' value='asserta' name='prolog'> &nbsp;
  <INPUT id='retract' type='submit' value='retract' name='prolog'>
  <INPUT id='retractall' type='submit' value='retractall' name='prolog'>
  <INPUT id='eval-subLisp' type='submit' value='eval_SubLisp' name='prolog'>
  <INPUT id='eval-subProlog' type='submit' value='eval_SubProlog' name='prolog'>
  <INPUT id='Listing' type='submit' value='Listing' name='prolog'>
 			<BR>
 		</FORM>
 		<BR>
 		<FORM method='post' action='cg?cb-prolog-upload' ID='upload_form'>
 			<INPUT id='cb-prolog-upload' type='hidden' name='cb-prolog'>
 			Upload Prolog Source Code<BR>
 			<INPUT id='filename' type='file' name='filename'> &nbsp;&nbsp; <INPUT id='upload' type='submit' value='upload' name='upload'>
 			<P>
 			</P>
 		</FORM>
  DebugInfo:
 <pre>HTTPVARS = ~s</pre>
 	</BODY>
 </HTML>"
        httpvars)) 
    (force-output)))


(print "Done loading file")
'(DEFINE MLG () (CUNWIND-PROTECT (progn (CSETF (READTABLE-CASE *READTABLE*) :PRESERVE) (MLG3)) (CSETF (READTABLE-CASE *READTABLE*) :UPCASE)))
