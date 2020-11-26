

;;(ARGNAMES-FROM-ARGLIST '((&optional (serr '*error-stack*))(&rest handler) &rest body))
'(print "                                    ...4")                  

(define mapfuncall-with-args (fn list &rest arg2-N)
    (ret (mapfuncall #'(lambda (item) (ret (apply fn item arg2-N))) list)))
           
(defmacro protect-error  ((&optional (err '*error-stack*)) (&rest handler) &rest body) (ret
    `(clet ((scode 'SCODE))
        (with-error-handler 
            #'(lambda()(csetq ,err (cons (cons *ERROR-MESSAGE* ',body) ,err)))
           (csetq scode (multiple-value-list (progn ,@body))))
        (fif (consp scode) (values-list scode) (progn ,@handler)))))

(defmacro inspect-arg (form &optional (serr '*error-stack*)) (ret
   `(clet (( result (list 'quote ',form) ))       
    (with-error-handler #'(lambda ()( csetq result (list 'quote ',form))) (csetq result ,form))
    result)))

(defmacro inspect-form (form &optional (serr '*error-stack*)) 
    (punless (consp form)  (ret `(inspect-arg ,form serr)))
   `(cons ,(fif (FUNCTION-SYMBOL-P (car form) `,(car form) (inspect-form (car form))))
          ,@(mapfuncall-with-args #'inspect-arg (cdr form) serr))
         (ret `(inspect-arg ,form serr)))



(defmacro tracer-error (stack code) 
  (ret 
   #'(lambda () (ret 
      (with-error-handler 
          #'(lambda () (ret (break (force-format t "tracer-error ~S durring: ~S~%" *error-message* code))))
         (force-format t "; ERROR~a ~s ~s~%" (make-tabs (length *error-stack*)) *error-message* (car stack))
         (force-format t "; SOURCE~a ~s ~s~%" (make-tabs (length stack)) (car stack) code)
         (pwhen (> 10 (length *error-stack*)) (break ">10"))
         (rplacd (car stack) (*error-message*))
         (csetq *error-stack* (cons (car stack) *error-stack*)))))))
            ;;(ret (values nil (break *error-message*)))))
      ;;(ret (values nil t)))))

          ;;(pwhen (equal *trace-tag* :ERROR-SIGNALED)(csetq *trace-tag* (force-print (break "What shall I return?"))))
          ;;(punless (macro-operator-p ',op) (setq code (cons op (mapfuncall #'make-trace (cdr code)))))
       ;;  *trace-tag*)))

(define trace-stack-print (stack &optional (depth 10))
   (clet ((size (length stack)))
   (pwhen (> depth size)(csetq depth size))      
    (cdotimes (n depth)
        (trace-format"STACK(~a): ~a ~s" 
                (- size (- (- depth 1) n)) 
                (make-string (* 2 (- (- depth 1) n)))
                 (nth (- (- depth 1) n) *trace-tag*)))))


(defmacro cl:break (tag &optional code) 
 (clet 
 ((fun #'(lambda ()
  (clet ((*parent-errors* *error-stack*))
   (clet ((*error-stack* *error-stack*))
        (csetq *error-stack* (cons (cons  *trace-tag*   *error-message*) *error-stack*))
        (trace-format"ERROR: ~s  ~a of ~a" *ERROR-MESSAGE* (length *error-stack*) (length *parent-errors*))
        (trace-format"BLOCK(~a): ~s"  *trace-tag*  (car *active-block-names*))
        (trace-stack-print *trace-tag*)
        (trace-format"CODE(0-n): ~s" (safe-car *trace-code*))
        (trace-format"FORM(~a): ~s"  *trace-tag*  (safe-car *trace-next*))
      ;;  (trace-format"* (R)un <mode>~s" *next-runmode*)
         ;;(trace-format"* (B)acktrace <depth> (~s . ~s)" (car (trace-parent))(cdr (trace-parent)))
         (trace-format"* (U)p <up> ~s" *trace-tag*)
         (trace-format"* (G)oto <frame> ~s"  *trace-tag* )
         (trace-format"* (E)rrors <amount> <current>  ~s" (length *error-stack*))
         (trace-format"* (S)tack <amount> <current> ~s" (length *trace-tag*))
         (trace-format"* (B)lock names <amount> <current> ~s" (length *active-block-names*))
         (trace-format"* (V)alues <list> ~s" *trace-tag*)
         (trace-format"* (C)hange <source> ~s"  *trace-tag* )
         (trace-format"* (I)nspect ~s ~s" *trace-code* *trace-code*)
         (trace-format"* (D)ebug ~s" *trace-tag*)
         (csetq todostr (LEFT-TRIM-WHITESPACE (cconcatenate (read-line) *NEWLINE*)))
         (CMULTIPLE-VALUE-BIND (cmd consumed) (read-from-string todostr nil :NEXT)
           (csetq todostr (LEFT-TRIM-WHITESPACE (substring todostr consumed)))
              (pcase cmd
   ;;              (M (csetf (trace-runmode) (read-from-string todostr nil *next-runmode*)))
                  (U (csetq *trace-goto* (read-from-string todostr nil *trace-tag*)))
                  (G (csetq *trace-goto* (read-from-string todostr nil *trace-goto*)))
                  (E (trace-stack-print *error-stack* (read-from-string todostr nil 5)))
                  (S (trace-stack-print *trace-tag* (read-from-string todostr nil 5)))
                  (B (trace-stack-print *active-block-names* (read-from-string todostr nil 5)))
                  (V (csetq *trace-tag* (read-from-string todostr nil *trace-tag*)))
                  (C (rplaca *trace-tag* (read-from-string todostr nil *trace-code*)))
                  (D (break (format nil "~s"  *trace-tag* )))
                  (I (clet ((*error-stack* ())(form ()))
                        (csetq (trace-runmode) ':INSPECT)
                        (csetf form (inspect-form (trace-source) *error-stack*))
                        (trace-stack-print (reverse *error-stack*))(force-print form)
                        (replacd  *trace-tag*  form)))                   
                  ((:NEXT NEXT N R) (csetf (trace-runmode) ':TRACE))
                  (otherwise 
                    (CATCH-ERROR-MESSAGE (*ERROR-MESSAGE*) (eval cmd))))))))))
                    (ret fun)))
                                          
'(print "              break                      ...4")        


;;ARGNAMES-FROM-ARGLIST 
;;(FUNCTION-SYMBOL-ARGLIST  '
(define transform-varblock (name patternIn bodyIn)
    ;;(force-print `(trace-varblock ',name ',patternIn ',bodyIn))
     (let ((pattern patternIn)(nargs ())(body bodyIn))
      (punless (consp body) (pwhen body (print body) (break "transform-varblock bodyIn was not list")))
      (csetq body (transform-block bodyIn 'form  name))
      (cdo ((op (car pattern)(car pattern))(pattern (cdr pattern)(cdr pattern)))((null op))
        (cond      
         ((consp op) 
             (csetq nargs (append nargs `(,(car op))))
             (csetq body (cons (cons 'trace-initvar op) body)))
         ((string-member op '(&key &aux)) 
             (csetq nargs (append nargs `(&rest lkeys)))
             (csetq body `((KeyLet ,pattern ,@body)))
             (csetq pattern ()))
         (t (csetq nargs (append nargs (list op))))))

      (punless (consp body) (pwhen body (print body) (break "transform-varblock bodyIn was not list")))
       (ret 
           (let ((trace `(list ',name ,@(mapfuncall #'var-traceable nargs))))
          `( (,@nargs)
                 (let ((*funcall-form* ,trace) (*trace-stack* (cons *funcall-form* *trace-stack*)))
                      (ret (trace-progn (trace-format :funcall "lexical:: ~s" *funcall-form*) ,@body))))))))

;;(punless (fboundp 'defmethod) (shadow-macro :CL defmethod (name pattern &rest body)(ret `(defun ',name ',pattern ,@body))))

(define transform-function-expression (form)
  (let ((vals (cmultiple-value-list (function-lambda-expression form))))
    (punless (car vals) (rplaca vals (third vals)))
    (ret (values vals))))


(define transform-block (form &optional (transkey 'form) name (environment nil) funcall)
 (ret 
   (cond 
     ((functionp form) (ret (list 'function (transform-block (transform-function-expression form) transkey name environment funcall))))
     ((SELF-EVALUATING-FORM-P form) form)
     ((cor (keywordp form)(numberp form)(stringp form)) form)
   ;; ((member form *incompatable*)(ret (trace-symbol "cl::" form)))
     ((cor (null form)(equal (type-of form) 'filecomment)(atom form)(stringp form)(numberp form)) (ret form))
     (t 
        (let ((op (car form))(cdrform (cdr form))(new-transkey (lookup-caller-pattern op)))
          (pwhen (symbolp op)
             (cond 
               ;;((string-member op '(progn)) (ret `(trace-progn  ,@(mapfuncall #'(lambda (item) (ret (transform-block item 'form name ))) cdrform))))
               ((string-member op '("defun" defmacro))                  
                  (ret (cons op (cons (transform-block (car cdrform)) 
                       (transform-varblock (transform-block (car cdrform)) (car (cdr cdrform)) (cdr (cdr cdrform)))))))
                       ;;(transform-varblock (transform-block (car cdrform)) (car (cdr cdrform)) `((ret (progn ,@(cdr (cdr cdrform))))))))))
               ((string-member op '(lambda))    ;;name     transkey         body 
                  (ret (cons op (transform-varblock (gensym) (car cdrform)(cdr cdrform))))))))
         (ret (mapfuncall #'(lambda (x)(ret (transform-block x 'form name))) form))))))

(define transform-block (form &optional (transkey 'form) name (environment nil)) 
  (pwhen (functionp form) 
      (ret (list 'function (transform-block (transform-function-expression form) transkey name environment))))
  (pwhen (consp form)
    (CLET((OP (CAR FORM))(CDRFORM (CDR FORM)))
      (PWHEN (named-member OP '("LAMBDA")) 
            (RET (RET (CONS OP (cl:transform-lambda CDRFORM (GENSYM))))))
      (PWHEN (named-member OP '("DEFUN" "cdefmacro" "cdefine" "cdefine" "cl:DEFUN" "cl:defmacro" "cdefmacro" ))
             (ret `(,op ,(car (cdr form)) name (cl:transform-lambda (cdddr form)))))
      (ret (mapfuncall-with-args #'transform-block form 'form name environment)))))


(define transform-block-return (body name)
   ;;(ret body)
   (pwhen body
   (pwhen (stringp (safe-car body)) (ret (transform-block-return (cdr body) name)))
   ;;(pwhen (equal (car body) '(progn))  (ret (transform-block-return (cdr body) name)))
   (pwhen (equal (caar body) 'progn)  (ret (transform-block-return (append (cadr body) (cdr body)) name)))
   (pwhen (cdr body) (ret (cons (car body) (transform-block-return (cdr body) name))))
   (ret body)))



#|

(define trace-stack-print (&optional (stack *trace-tag*) &rest lkeys)
      (KeyLet 
        ((length (length stack))
          (number length)
          (max length)
          (offset (- length number))

          (name "FRAME")(showfn #'print)         
          (print-grow 3)(print-space #\.)
          (print-offset (* offset print-grow))
          (print-width (* max print-grow))   
          (print-lines 10)(print-pause 3))))

(define trace-stack-print (&optional (stack *trace-tag*) &rest lkeys)
      (KeyLet 
        ((length (length stack))
          (number length)
          (max length)
          (offset (- length number))

          (name "FRAME")(showfn #'print)         
          (print-grow 3)(print-space #\.)
          (print-offset (* offset print-grow))
          (print-width (* max print-grow))   
          (print-lines 10)(print-pause 3)
         
          (framefn #'car)(nextfn #'cdr)
          (donefn #'(lambda (stack) (ret (cor (null stack) (< max 0)))))
          (to-key T)(to-value T)(test #'equal)
          (untilframefn #' (lambda (frame)
              (ret (cor       (funcall-if test to-key (car frame))(funcall-if test to-value (cdr frame)))))))

        (pwhen (< 0 print-width) (csetq print-width (- 0 print-width)))
        (pwhen (< 0 print-offset) (csetq print-offset (+ print-width print-offset)))

        (fif (null stack) (ret (trace-format"~a Is Empty" name)))
        (cdo ((max max (1- max))              
              (number number (1- number))
              (offset offset (1+ offset))
              (lines 0 lines)
              (print-offset print-offset (+ print-grow print-offset))
              (frame (funcall framefn stack)(funcall framefn stack))
              (stack stack (funcall nextfn stack)))
             ((cor (null stack)(funcall-if untilframefn frame)(funcall-if donefn frame))(ret stack))
                (pwhen (cand (> print-offset 0) (< print-offset print-width) )
                    (trace-format"~a(~a) ~s" name number (make-string screen-offset print-space))
                    (funcall-if showfn frame)(force-output)
                    (incf lines)
                    (pwhen (lines > pause-lines)(sleep pause)(csetq lines 0))))))

|#                              

#|


;;       (punless (equal '(lisp) expr) (trace-block (cadr expr) (print (eval (force-print expr))))))))

    ;;(load "common.lisp")(load "common-lisp.lisp")(in-package :SYS ))
;;(lisp) 

(defvar *caller-pattern-table* (make-hash-table 23 #'equal) "Stores patterns for function destructuring.")

(define lookup-caller-pattern (name)
 (ret (gethash name *caller-pattern-table*)))
;;(transform-block bodyIn 'form  name)))

(define best-symbol (current &optional packsearch)
  (fif current
      (fif (consp current)
           (cons (best-symbol (car current) packsearch)(best-symbol (cdr current) packsearch))
           (fif (symbolp current)
              (clet ((initial current)(name (symbol-name current))) 
                (cdolist (into (ensure-list (fif packsearch packsearch (list-all-packages))))                   
                    (csetq current (better-symbol current 
                        (find-symbol name into))))
                    (csetq current (better-symbol initial current))
                    (trace-format" Using ~a from ~a %" (trace-symbol current) (trace-symbol initial))
                    (ret current)))))
  (ret current))

;;       (SELF-EVALUATING-FORM-P form)(not (consp form))(keywordp form)(null form)(equal (type-of form) 'filecomment)
;;       (atom form)(stringp form)(numberp form)) form)| 

;;some tests


'(print (transform-block '
 (shadow-defun :CL intersection (list-1 list-2 &key (test #'eql)(key #'identity) test-not)
   (pwhen test-not (setq test (cl:lambda (x y)(not (funcall test-not x y)))))
   (intersection list-1 list-2 test key))))

'(print (transform-block '
 (shadow-defun :CL member (item list &key (test #'eql)(key #'identity) test-not)
   (pwhen test-not (setq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))  
   (member item list test key))))


(defmacro cdefmacro (symbol pattern &rest body)
    (clet ((inside (intern (format nil "m-~a" (symbol-name symbol)) (symbol-package symbol)))
      (definside  `(defmacro ,inside ,pattern (ret (trace-block ',symbol ,@body))))
      (defoutside  `(defmacro ,symbol (&rest args) 
         (clet ((call (cons ',inside args))) 
             (ret `(values-list (clet ((*trace-tag* (cons ',call *trace-tag*))) (multiple-value-list ,call)))))))
      (defall `(progn ,definside ,defoutside)))
      (mapfuncall #'identity (mapfuncall #'eval (mapfuncall #'identity (cdr defall)))) 
      (ret defall)))

(defmacro cdefine (symbol pattern &rest body)
    (clet ((inside (intern (format nil "f-~a" (symbol-name symbol)) (symbol-package symbol)))
      (definside  `(define ,inside ,pattern (ret (trace-block ',symbol ,@body))))
      (defoutside  `(define ,symbol (&rest args) (clet ((*trace-tag* (cons (cons ',inside args) *trace-tag*)))
       (ret (apply (symbol-function ',inside) args)))))
      (defall `(progn ,definside ,defoutside)))
      (mapfuncall #'identity (mapfuncall #'eval (mapfuncall #'identity (cdr defall))))
      (ret defall)))

|#
(print `(LOAD "trace.lisp"))


(define trace-load (filespec &rest lkeys)
 (printl filespec lkeys)
 (KeyLet (verbose print if-does-not-exist external-format)
  (SL::clet ((*standard-input* (SL::OPEN-TEXT filespec :input))) 
    (cdo () ()
    (clet ((expr (SL::read *standard-input* nil :EOF)))
     (pwhen (equal expr :EOF)
        (SL::close *standard-input*)
        (ret T))
      (trace-eval expr))))))

(define trace-eval (code) 
  (csetq *trace-stack* ())
   (csetq *trace-tag* ())
  ;;(printl code)
  (ret (eval (trace code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Intitally setup packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *EXTERNAL-LISP-PACKAGE* (sl::make-package "EXTERNAL" '() '("EXT")))
(defvar *SYSTEM-PACKAGE* (sl::make-package "SYSTEM" '("EXTERNAL") '("SYS" "INT" "INTERNAL")))
(defvar *COMMON-LISP-PACKAGE* (sl::make-package "COMMON-LISP" '("SYSTEM" "EXTERNAL" "SL" "CYC") '("LISP" "CL")))
(defvar *default-package-use* '("CL" "SYS" "EXT" "CYC" "SL"))
(defvar *COMMON-LISP-USER-PACKAGE* (sl::make-package "COMMON-LISP-USER" *default-package-use* '("USER")))


(defvar *SUBLISP-DEFMACRO* (find-symbol "DEFMACRO" :SUBLISP))
(defmacro CYC::SUBLISP-DEFMACRO (symbol pattern &rest body)
  (csetq symbol (eval `,symbol))
  (printl symbol pattern body)
  (ret (cons *SUBLISP-DEFMACRO* (cons symbol (cons pattern `((ret (progn ,@body))))))))
(in-package :SYSTEM)
(SL::defvar CYC::*SYSTEM-DEFMACRO* (SL::intern (SL::make-symbol "DEFMACRO") :SYSTEM))
(SL::EXPORT CYC::*SYSTEM-DEFMACRO* :CYC)
(SL::IMPORT CYC::*SYSTEM-DEFMACRO* :CYC)
(CYC::SUBLISP-DEFMACRO CYC::*SYSTEM-DEFMACRO* (symbol pattern SL::&rest body)
  (SL::ret (SL::cons CYC::*SUBLISP-DEFMACRO* `(,symbol ,pattern (SL::ret (CYC::trace-defun ,symbol ,pattern ,@body))))))
(SL::in-package :CYC)
(import *SYSTEM-DEFMACRO* :CYC)
(print (list (symbol-package (find-symbol "DEFMACRO" )) (fboundp (find-symbol "DEFMACRO" :SYSTEM ))))


  
(defvar *SUBLISP-DEFINE* (find-symbol "DEFINE" :SUBLISP))
(in-package :SYSTEM)
(SL::defvar CYC::*SYSTEM-DEFINE* (SL::intern (SL::make-symbol "DEFINE") :SYSTEM))
(SL::EXPORT CYC::*SYSTEM-DEFINE* :CYC)
(SL::IMPORT CYC::*SYSTEM-DEFINE* :CYC)
(CYC::SUBLISP-DEFMACRO CYC::*SYSTEM-DEFINE* (symbol pattern SL::&rest body)
  (SL::ret (SL::cons CYC::*SUBLISP-DEFINE* `(,symbol ,pattern (SL::ret (CYC::trace-defun ,symbol ,pattern ,@body))))))
(SL::in-package :CYC)
(import *SYSTEM-DEFINE* :CYC)
(print (list (symbol-package (find-symbol "DEFINE" )) (fboundp (find-symbol "DEFINE" :SYSTEM ))))

  
(DEFMACRO ALTER-DEFINE (OLDSAVE ORGIPACKAGE NEWSAVE DEF-STR VARS &REST BODY)(RET 
  `(SL::PROGN (DEFVAR ,OLDSAVE (SL::FIND-SYMBOL ,DEF-STR ,ORGIPACKAGE))
    (SL::IN-PACKAGE :SYSTEM)
    (SL::DEFVAR ,NEWSAVE (SL::INTERN (SL::MAKE-SYMBOL ,DEF-STR) :SYSTEM))
    (SL::EXPORT ,NEWSAVE :CYC)
    (SL::IMPORT ,NEWSAVE :CYC)
    (CYC::SUBLISP-DEFMACRO ,NEWSAVE ,VARS (SL::RET (SL::PROGN ,@BODY)))
    (SL::IN-PACKAGE :CYC)
    (SL::IMPORT ,NEWSAVE :CYC)
    (SL::PRINT (SL::LIST (SL::SYMBOL-PACKAGE (SL::FIND-SYMBOL ,DEF-STR )) (SL::FBOUNDP (SL::FIND-SYMBOL ,DEF-STR :SYSTEM )))))))
#|

(defvar *SUBLISP-LAMBDA* (find-symbol "LAMBDA" :SUBLISP))
(in-package :SYSTEM)
(SL::defvar CYC::*SYSTEM-LAMBDA* (SL::intern (SL::make-symbol "LAMBDA") :SYSTEM))
(SL::EXPORT CYC::*SYSTEM-LAMBDA* :CYC)
(SL::IMPORT CYC::*SYSTEM-LAMBDA* :CYC)
(CYC::SUBLISP-DEFMACRO CYC::*SYSTEM-LAMBDA* (pattern SL::&rest body)
  (SL::ret (SL::cons CYC::*SUBLISP-LAMBDA* `(,pattern (SL::ret (CYC::trace-defun NIL ,pattern ,@body))))))
(SL::in-package :CYC)
(import *SYSTEM-LAMBDA* :CYC)
(print (list (symbol-package (find-symbol "LAMBDA" )) (fboundp (find-symbol "LAMBDA" :SYSTEM ))))


(defvar *SUBLISP-UNINTERN* (find-symbol "UNINTERN" :SUBLISP))
(in-package :SYSTEM)
(SL::defvar CYC::*SYSTEM-UNINTERN* (SL::intern (SL::make-symbol "UNINTERN") :SYSTEM))
(SL::EXPORT CYC::*SYSTEM-UNINTERN* :CYC)
(SL::IMPORT CYC::*SYSTEM-UNINTERN* :CYC)
(define unintern (a b)
   (SL::progn (SL::WITH-ERROR-HANDLER #'(SL::LAMBDA ()()) (SL::ret (SL::unintern a b))) (SL::ret (SL::unintern a b))))
(SL::in-package :CYC)
(import *SYSTEM-UNINTERN* :CYC)


(print (list (symbol-package (find-symbol "UNINTERN" )) (fboundp (find-symbol "UNINTERN" :SYSTEM ))))
|#
(csetq SL::KEYWORD-PACKAGE (find-package :KEYWORD))
;;(csetq CYC::KEYWORD-PACKAGE (find-package :KEYWORD))


(defvar *resolve-symbols* ())
(defvar *all-shadowing-symbols* ())
(defvar *all-internal-symbols* ())
(defvar *all-external-symbols* ())
(in-package "SUBLISP")
(export 'lambda-list-keywords)
(in-package "CYC")
(defvar *sticky-symbols* (append '( SL::&BODY SL::NIL) lambda-list-keywords))

(define import-symbol (name from &optional (to *PACKAGE*))
   (clet ((old  (find-symbol name TO)))
    ;;(pwhen (eq (symbol-package old) from) (ret (find-symbol name TO)))
    (intern NAME TO)
    (with-error-handler #'(SL::LAMBDA ()())(unintern (find-symbol name TO) TO))
    (with-error-handler #'(SL::LAMBDA ()())(unintern (find-symbol name TO) TO))
        (unintern 'CYC::NIL *CYC-PACKAGE*)
    (import (find-symbol "NIL" :SL) :CYC)
    (ret (values-list (list (import (find-symbol NAME FROM)) old)))))

(import-symbol "YES-OR-NO-P" :SUBLISP)

(defvar *fboundp-symbols* ())
(defvar *boundp-symbols* ())

;;(defmacro let (vars &rest body) (ret `(clet ,vars (prog-t ,@body))))
;;(defmacro prog-t (&rest body) (ret (fif body (fif (cdr body) `(progn (trace-warn ,(car body)) (prog-t ,@(cdr body)))`,(car body)))))
;;(define other-package (pack) (ret (fif (eq pack *CYC-PACKAGE*)  *SUBLISP-PACKAGE*  *CYC-PACKAGE*)))

(define import-symbols (&optional (to *PACKAGE*))
    (csetq *boundp-symbols* ())
    (FORMAT t "~& ;; manually importing symbols into ~a ~&" to)(force-output)
    (cdo-all-symbols (a) (punless (keywordp a) (pwhen (boundp a) (cpush a *boundp-symbols*)))) 
    (cdolist (a *boundp-symbols*)
         (clet ((from (symbol-package a))(n (symbol-name a))(c (find-symbol n to)))
           (punless (eq a c) (import-symbol n from))))
    
    (csetq *fboundp-symbols* ())

    (cdo-all-symbols (a) (pwhen (fboundp a) (cpush a *fboundp-symbols*)))
    (cdolist (a *fboundp-symbols*)
         (clet ((from (symbol-package a))(n (symbol-name a))(c (find-symbol n to)))
           (punless (eq a c) (import-symbol n from to))))

    (import ()))

(progn 
 (in-package :CYC)
 (cyc::import-symbols)
 (in-package :SUBLISP)
 (cyc::import-symbols))

(in-package (package-name *loader-package*))

(cdo-external-symbols (s *PACKAGE*)
  (print s)
  )



