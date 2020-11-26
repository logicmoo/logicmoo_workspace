#|

SubL is a programming language intended to be very similar to a simplified version of Common Lisp where those features that are either 
complex, rarely-used, or difficult to implement in a prodecural language have been removed. Lets put some back.

Sometimes it is hard to port your Common Lisp applications to SubL.
Until you do, you will not be able to translate-block-it with Cyc's internal translate-blockr.

During the interim, here are some usefull functions and macros. 
<b>Please help out by [http:://www.cycfoundation.org/foundation/index.php?title=Common_Lisp_Compatibility&action=edit editing] this page.</b>

The goal will be here to implement as much of the Common Lisp language as possible based on the 
[http:://www.lisp.org/HyperSpec/FrontMatter/Chapter-Index.html HyperSpec]


*[[Programming]] is based largly on [http:://www.cyc.com/cycdoc/ref/subl-reference.html SubL Reference]

|#
;;<pre><nowiki>

;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-
;;
;; Douglas R. Miles
;;
;; Saved into a file called common.lisp
;; 05/08/2006  (load "e2c/common.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intitally setup packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(in-package "CYC")




#|
;;Initialize the task processor pool for requests.
(INITIALIZE-API-TASK-PROCESSORS) 
;;Initialize the task processor pool for requests.
(INITIALIZE-BG-TASK-PROCESSORS )
;;Initialize the task processor pool for requests.
(INITIALIZE-CONSOLE-TASK-PROCESSORS )
 (SHOW-API-TASK-PROCESSORS )
;;Provides a convenient alias for DISPLAY-API-TASK-PROCESSORS.
(SHOW-API-TP-MSGS  )
;;Show and reset the task processor background messages for thetask-process-pool.
(SHOW-BG-TP-MSGS )
;;Show and reset the task processor background messages for thetask-process-pool.
(SHOW-CONSOLE-TP-MSGS  )
;;(TRANSLATOR-RET-OPTIMIZE-BODY )
|#
;;(define dispatch-macro-in-package (s c n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The package CL uses SYSTEM as it's shadow
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *SYSTEM-PACKAGE* (sl::make-package "SYSTEM" '() '("SYS")))
(defvar *COMMON-LISP-PACKAGE* (sl::make-package "COMMON-LISP" '("SYS") '("CL" "LISP")))
(defvar *default-package-use* (list *COMMON-LISP-PACKAGE* *SYSTEM-PACKAGE* *CYC-PACKAGE* *SUBLISP-PACKAGE*))
;;(defvar *COMMON-LISP-USER-PACKAGE* (sl::make-package "COMMON-LISP-USER" '("CL" "CYC") '("USER")))

(import () :CL)
(import () :SYS)

(DEFVAR *SUBLISP-DEFMACRO* (find-symbol "DEFMACRO" :SUBLISP))
(DEFVAR *SUBLISP-DEFINE* (find-symbol "DEFINE" :SUBLISP))
(DEFVAR *SUBLISP-LAMBDA* (find-symbol "LAMBDA" :SUBLISP))
(DEFVAR *SUBLISP-FUNCTION* (find-symbol "FUNCTION" :SUBLISP))

(defvar *T-PACKAGE* *package*)
(defvar *T-READTABLE* (COPY-READTABLE *READTABLE*))

;;(defmacro cl-lambda (args &rest stuff) (ret `#'(lambda ,args (ret (progn ,@stuff)))))
(define define-anonymous-function (arguments body) 
   (clet ((name (gensym "LAMBDA-"))) (eval `(define ,name ,arguments (ret (progn ,@body)))) (ret (symbol-function name))))

(defmacro cl-lambda (arguments &body body)  (ret (define-anonymous-function arguments body)))



(define force-format (strm &rest body)(clet ((res (apply #'format (cons strm body))))(pif (streamp strm) (output-stream-p strm) (force-output))(ret res)))
(define force-princ (&rest body)(clet ((res (apply #'princ body)))(force-output)(ret res)))
(define force-print (&rest body) (clet ((body (fif (equal 1 (length body)) (car body) body))(res (print body)))(force-output)(ret res)))
(define lisp () (load "common.lisp")(load "common-lisp.lisp")(in-package :SYS ))

;;EXPAND-DEFINE-LIST-ELEMENT-PREDICATOR  (FUNCTION-NAME FUNCTION-SCOPE ELEMENT-VAR TYPE BODY) 
;; ARGNAMES-FROM-ARGLIST  
;;MAKE-PROCESS-WITH-ARGS  (NAME FUNCTION &OPTIONAL ARGS) 
 
(define consify (list) (ret (fif (consp list) list (fif list (list list) ()))))
(defmacro every (fn &rest seq) (ret (every-list fn seq)))
(defmacro every-list (fn seq) (ret (fif (car seq) (cand (apply fn (mapcar #'car seq)) (every-list fn (mapcar #'cdr seq))) t)))
(defmacro puthash (key value table) (ret (sethash key table value)))
(defmacro some (fn &rest seq) (ret (some-list fn seq)))
(defmacro some-list (fn seq) (ret (pwhen (car seq) (cor (apply fn (mapcar #'car seq)) (some-list fn (mapcar #'cdr seq))))))
(defmacro and (&rest body) (ret (fif body (fif (cdr body) `(pwhen ,(car body) (and ,@(cdr body)))`,(car body)))))
(defmacro assert (test &rest body))
(defmacro defsetf (access-fn update-fn) (ret `(SL::_DEF-CSETF ,access-fn ,update-fn)))
(defmacro destructuring-bind (pattern datum &rest body)(ret `(cdestructuring-bind ,pattern ,datum (trace-progn ,@body))))
(defmacro do (var+list exit &rest body) (ret `(cdo (,@(mapcar #'trace-varinit var+list)) ,(trace-each exit) (trace-progn ,@body))))
(defmacro dolist (var+list &rest body) (ret `(cdolist (,(car var+list) ,(second var+list)) (trace-progn  ,@body))))
(defmacro dotimes (var integer &rest body) (ret `(cdotimes ,var (trace-lisp ,integer) (trace-progn  ,@body))))
(defmacro handler-case (form &rest cases) (ret form))
(defmacro if (cond true &optional false) (ret `(fif (trace-lisp ,cond) (trace-progn ,true) (trace-progn ,false))))
(defmacro let (var+list &rest body) (ret `(clet (,@(mapcar #'trace-varinit var+list)) (trace-progn ,@body))))
(defmacro let* (var+list &rest body) (ret `(clet (,@(mapcar #'trace-varinit var+list)) (trace-progn ,@body))))
(defmacro memq (item list) (ret `(member ,item ,list #'eq)))
(defmacro multiple-value-bind (var+list form &rest body) (ret `(cmultiple-value-bind ,var+list ,form (trace-progn ,@body))))
(defmacro or (&rest body) (ret (fif body (fif (cdr body) `(pcond ((trace-lisp ,(car body))) ((or ,@(cdr body))))`(trace-lisp ,(car body))))))
(defmacro pop (place) (ret `(clet ((f1rst (car ,place))) (cpop ,place) f1rst)))
(defmacro prog1 (body1 &rest body) (ret `(clet ((prog1res (trace-progn ,body1))) (trace-progn  ,@body) prog1res)))
(defmacro prog2 (body1 body2 &rest body) (ret `(clet ((prog1res (trace-progn ,body1))(prog2res (trace-progn ,body2))) (trace-progn  ,@body) prog2res)))
(defmacro prog3 (body1 body2 body3 &rest body) (ret `(clet ((prog1res (trace-progn ,body1))(prog2res (trace-progn ,body2))(prog3res (trace-progn ,body3))) (trace-progn  ,@body) prog3res)))
(defmacro pushnew (item place &key key test test-not) (ret (fif test (list 'cpushnew item place test)(list 'cpushnew item place))))
(defmacro return-from (name value) (ret `(ret ,value)))
;;(defmacro setf (&rest pairs) (ret (pwhen pairs `(sl::progn (_setf ,(car pairs) (trace-progn ,(cadr pairs)))(setf ,@(cddr pairs))))))
(defmacro setf (&rest pairs) (ret `(csetf ,@pairs)))
;;todo (defmacro setq (&rest pairs) (ret (pwhen pairs `(sl::progn (csetq ,(car pairs) (trace-lisp ,(cadr pairs))) (setq ,@(cddr pairs))))))
(defmacro setq (&rest pairs) (ret `(csetq ,@pairs)))
(defmacro unless (cond &rest body) (ret `(punless (trace-lisp ,cond) (trace-progn  ,@body))))
(defmacro when (cond &rest body) (ret `(pwhen (trace-lisp ,cond) (trace-progn  ,@body))))
(defmacro typep (form type) (ret (cor (eq type t)(same-classes (type-of form) type))))
(defmacro cond (&rest body) (ret (cons 'pcond (mapcar #'(lambda (x) (ret `( ,@(mapcar #'(lambda (xz) (ret `(trace-lisp ,xz))) x)))) body))))
(defmacro case (test &rest body) (ret `(pcase ,test ,@(mapcar #'(lambda (x) (ret `(,(car x) (trace-progn ,@(cdr x))) )) body))))
(defmacro eval-when (a &rest b) (ret (cons 'trace-progn b)))
(defvar internal-time-units-per-second *internal-time-units-per-second*)

(defconstant most-positive-fixnum *most-positive-fixnum* "is that fixnum closest in value to positive infinity provided by the implementation, and greater than or equal to both 2^15 - 1 and array-dimension-limit.")
(defconstant most-negative-fixnum *most-negative-fixnum* "is that fixnum closest in value to negative infinity provided by the implementation, and less than or equal to -2^15")

;; cunwind-protect hozed multiple-value-lists so thats the reason for the 'prognvals' weirdness
(defmacro with-package-case (package readcase &rest body) (ret
  `(clet ((*READTABLE* *T-READTABLE*)(*PACKAGE* *T-PACKAGE*)
      (prognval nil)
          (ocase (READTABLE-CASE *READTABLE*))(opack (string (package-name *PACKAGE*))))
          (in-package (string (fif (packagep ,package)(package-name ,package) ,package)))
          (CSETF (READTABLE-CASE *READTABLE*) ,readcase)
      (cunwind-protect (csetf prognvals (multiple-value-list (progn ,@body)))
      (CSETF (READTABLE-CASE *READTABLE*) ocase)(in-package opack)(values-list prognvals)))))


;;  Read up to the char specified
(define READ-UNTIL (quit-chars &optional (stream *STANDARD-INPUT*)(retstr ""))
     (cdo ((lastchar (read-char stream)(read-char stream))) 
          ((member lastchar quit-chars)(unread-char lastchar stream )(ret (values retstr lastchar)))
        (csetq retstr (cconcatenate retstr (string lastchar)))))


;; #>CL ::DEFINE interns a non-exported non-inherited into package CL
;; #>CL ::DEFINE interns an exported non-inherited into package CL
;; maybe somehow/day use the SUBLISP::SHARPSIGN-COLON-RMF reader
(define IN-PACKAGE-RMF (stream c n &optional (into-package *KEYWORD-PACKAGE*)(pop-package *PACKAGE*)(exported :INTERNAL))
      (clet ( symbol found access symbolname (stream (fif (streamp stream) stream *STANDARD-INPUT*)))
      ;; (force-print stream c n)
       (cunwind-protect
        (progn
          (in-package (package-name into-package))
          (csetq found (read-from-string (read-until '(#\: #\Space) stream "")))
          (csetq found (eval found))
          (punless (packagep found) (csetq found (find-package (string found))))          
          (pwhen (packagep found) (csetq into-package found))
          ;;(punless into-package (cerror "(MAKE-PACKAGE ~s)" "Unknown into-package: ~a" found found `(MAKE-PACKAGE ,found)))
          (read-char stream)
          (csetq symbolname (read-char stream))
          (unread-char symbolname stream)
          (punless (equal symbolname #\: )(csetq exported :EXTERNAL))
          (IN-PACKAGE (package-name into-package))
          (csetq symbolname (read stream nil t nil))
          (pcond 
            ;; false alarm
           ((numberp symbolname)(csetq symbol symbolname))
           ;; oh well at least we READ from the PACKAGE requested
           ((consp symbolname)(csetq symbol symbolname))
           ;; one might use a STRINGP to ensure not to try to intern too early    "DEFINE"
           ((cor (symbolp symbolname)(stringp symbolname))
              (cmultiple-value-bind (symbol access) 
                 (find-symbol (string symbolname) into-package))
              (csetq found (symbol-package symbol))
              (pcase access
                  (NIL 
                    (csetq symbol (make-symbol (string symbolname)))
                    (csetq symbol (intern symbol into-package)))
                  (otherwise
                    (force-format t ";; ~s ~&" `(':symbolname ',symbolname ',exported ':TO ',into-package 
                    ':FOUND ',symbol ':IN ',found ':access ',access))
                    (punless (eq found into-package)
                        (csetq symbol (make-symbol (string symbolname)))
                        (import symbol into-package)
                        (import symbol into-package))))
            (pwhen 
               (equal exported :EXTERNAL)
               (export symbol into-package)
               (import symbol pop-package)
               (import symbol pop-package)))))
          ;;unwound to
          (IN-PACKAGE (package-name pop-package)))
          (ret (values symbol T))))

(set-dispatch-macro-character #\# #\> #'IN-PACKAGE-RMF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERROR HANDLING
;; Like the 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *trace-notify* () "Trace these types")
(define trace-format (type string &rest body)
  (pwhen (equal type :funcall) 
    (pcond 
      ((null *trace-notify*) (ret nil))
      ((equal T *trace-notify*)) ;; all funcalls
      ((member (car (car rest)) *trace-notify*))
      (t (ret nil))))
  (pwhen (equal type :trace) (punless *trace-notify* (ret nil)))
  (fresh-line)
  (apply #'format (cons T (cons string body)))
  (fresh-line)
  (force-format t " ;;~S;;~%" *trace-stack*)
  (force-output))


(defvar *trace-stack* () "contains information about the last toplevel funcall")
(csetq *trace-stack* ())
(defmacro trace-dump (&optional (stack *trace-stack*) (depth 7) (offset 0))
    (punless  (numberp depth) (csetq depth (fif depth 10000 0)))
    (pwhen stack
        (cdo ((depth depth (1- depth))(stack stack (cdr stack))(*funcall* (car stack) (car stack)))
             ((cor (null stack)(> 0 depth))(ret stack))
             (format t ";  STACK:~a  ~s~%" (make-string (* 5 (+ offset (length *trace-stack*)))) *funcall*)))
    (format t ";  STACK ~a  ~s~%" (make-string (* 5 offset)) :EMPTY))

(defvar *current-code* :NONE "The current/parent code info")
(defvar *current-eh* *error-handler* "The current/parent error handler")
(defvar *results-list* :UNCALLED "The current/parent result info")
(defvar *current-fn* (cons *results-list* NIL) "current frame")

(defvar *error-stack* () "The first error info")
(csetq *error-stack* ())

(define make-tabs (n) (ret (format nil "(~a):~a" n (make-string (* 5 n)))))



(defmacro no-errors (code) (ret  (with-error-handler *err-handler* (multiple-value-list `,code ))))
(csetq *err-handler* #'(lambda () (ret nil)))

(defmacro make-handler (code) 
  (ret #'(lambda (&rest whatevah) (ret (break "~&~&~&~&;; made-handler ~S durring: ~S ~S ~&" *error-message* whatevah code)))))
         ;;(force-format t ";; ERROR ~a ~s ~s~%" (make-tabs (length *error-stack*)) *error-message* (car stack))
         ;;(force-format t ";; SOURCE ~a ~s ~s~%" (make-tabs (length stack)) (car stack) ,code)
         ;;(pwhen (> 0 (length *error-stack*)) (break ">0 - 10"))
         ;;(rplacd (car stack) (*error-message*))
       ;;  (csetq *error-stack* (cons (cons *error-message* code )*error-stack*)))))

            ;;(ret (values nil (break *error-message*)))))
      ;;(ret (values nil t)))))

(defvar *err-handler* #'(lambda () (break "toplevel")))
(csetq *err-handler* (make-handler "TOPLEVEL CODE"))

(csetq *current-fn* (make-handler "NO CODE"))
(defvar *current-stack*  (list *current-fn*))
#|
(defmacro trace-lisp (code) 
 (clet 
 ((*current-fn* *current-fn*)
 (*err-handler* (make-handler code))
 (*results-list* *results-list*))
 (csetq (*current-fn* #'(lambda (SL::&REQ-0 SL::&ENVIRONMENT ENV) (ret  (with-error-handler *err-handler* (multiple-value-list `,code ))))))
  (ret `(clet ((*results-list* *results-list*)(csetq *results-list* (funcall *current-fn* (list *current-fn* )))
                   (values-list *results-list*)))))
 (macroexpand-1 '(trace-lisp 1))
 (trace-lisp 1)
|#
(defmacro trace-lisp (code) (ret code))
(defmacro trace-lisp (code) (ret (trace-eval code)))
(defmacro trace-eval (code) 
(clet ((*current-fn* *current-fn*)
(*err-handler* (make-handler code))
(*results-list* *results-list*))  (ret 
 `(clet ((*current-stack* (cons *current-fn* *current-stack*))
   (*current-fn* #'(lambda () (ret  
         (with-error-handler *err-handler* 
         (multiple-value-list (progn (force-print (quotify ,CODE)) ,code )))))))
       (values-list (apply *current-fn* (values)))))))

(defmacro trace-lisp (code) (ret code))

;;(macroexpand-1 '(trace-lisp 1))(trace-lisp 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro KeyLET (keys &rest body) (ret 
  `(clet ,(mapcar #'var-of keys) 
    ,@(mapcar #'(lambda (key) (ret `(init-keyval ,(var-of key) ,(init-of key)))) keys)
    ,@body)))

(define var-of (larg)
  (ret (pif (consp larg) (car larg) larg)))

(define init-of (larg)
  (ret (pif (consp larg) (cadr larg) nil)))

(defmacro key-present-p (key &optional (keylistname 'lkeys)) 
  (ret `(member-if #'(lambda (x) (ret (cand (symbolp x)(symbolp ,key) (equal (symbol-name x) (symbol-name ,key))))) ,keylistname)))

(defmacro init-keyval (key &optional default) (ret 
  `(csetq ,key (fif (key-present-p ',key) (cadr (key-present-p ',key)) ,default))))
                
(defmacro var-traceable (vx)
  (cond
   ((symbolp vx)
     (ret (pif (char= #\& (char (symbol-name vx) 0)) (list 'quote vx) `(list 'quote ,vx))))
   ((consp vx) (ret (var-traceable (car vx))))
   (t (ret (list 'quote vx)))))


(defmacro trace-initvar (var value) (ret `(funless ,var (csetq ,var (trace-progn ,value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trace-progn - Some of the features of the system must be accessable from everywhere
;; Like the 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter warning-errors 0)
(defmacro trace-warn (&rest code)
  (ret `(with-error-handler #'(lambda ()
   (force-format t "~%;; ERRROR: ~a~%;; code: ~a~%" *error-message* ',code)
   (pwhen (> (cinc warning-errors) 10) (break "trace-warn ~s >= ~a " warning-errors 10)))
   (sl::progn ,@code))))

(defmacro trace-progn (&rest code) (ret (cons 'sl::progn (mapcar #'(lambda (xz) (ret `(trace-lisp ,xz))) code) )))

(defmacro trace-code (code)
   (pcond 
       ((SELF-EVALUATING-FORM-P code)(ret code))
       (nil (consp code)(ret 
         `(cons ',(car code) 
               (mapcar #'(lambda (ele)
                    (with-error-handler #'(lambda ()) (ret (eval ele)))
                    (ret (quote ele)))
                    ',(cdr code)))))
       ((consp code)(ret 
         `(cons ',(car code) (mapcar #'trace-code-fn ',(cdr code)))))
       (t (ret (quotify code)))))


(define coerce-package (name &optional default) (ret
 (pcond 
   ((packagep name) (ret name))
   ((find-package (string name)))
   ((null name) default)
    ((symbolp name)(ret (coerce-package (symbol-name name)(symbol-package name))))
       (t (ret default)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-symbol
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *all-shadowing-symbols*)

(defvar *sticky-symbols* '((*SUBLISP-PACKAGE* . LAMBDA)(*SUBLISP-PACKAGE* . LAMBDA)(*SUBLISP-PACKAGE* . NIL)))

(define map-each (var fn list &rest body) (ret `(mapcar #'(lambda (e) (ret (apply ,fn (cons e ,body))))) list))

(define trace-varinit (var) (ret (fif (consp var) `(,(car var) ,@(trace-each (cdr var))) var)))

(define trace-each (list) (ret (mapcar #'(lambda (xz) (ret `(trace-lisp ,xz ))) list)))

;;(cdo ((i 0 (1+ i))) ((= i 10))(format t "~a,~a~%" i (constant-name (find-constant-by-internal-id i))))
;;(cdo ((i 0 (1+ i))) ((= i (constant-count)))(format t "~a,~a~%" i (constant-name (find-constant-by-internal-id i))))
(define nstring (sym)
   (pcond
      ((null sym) (ret "NIL"))
      ((stringp sym) (ret sym))
   ;;   ((consp sym) (ret (nstring (car sym))))
      ((symbolp sym) (ret (symbol-name sym)))
      ((packagep sym) (ret (package-name sym)))
      ((packagep sym) (ret (nstring (find-symbol (package-name sym) :KEYWORD))))
      (t (ret (write-to-string sym)))))



(define describe-symbol (sym &optional (packageIn *PACKAGE*))
 ;;(ret (write-to-string sym))
 (pwhen (consp sym) (ret (cons (describe-symbol (car sym) packageIn)(describe-symbol (cdr sym) packageIn))))
;; (pwhen (stringp sym) (csetq sym (car (find-all-symbols sym :use (append (list package *package* )(LIST-ALL-PACKAGES))))))
 (clet ((package (coerce-package packageIn  *PACKAGE*))(packname (nstring package)))
   (pcond 
     ((null sym) (ret "SL::NIL"))
     ((cnot (symbolp sym))(ret (write-to-string sym)))
     (t          
       (clet ((name (string sym))(sympack (symbol-package sym)))
        (pwhen (null sympack) (ret (format nil "~a!#:~a" packname name)))
	;;  (format t "looking up ~a::~a" (nstring sympack) name )
        ;;(ret (format nil "~a::~a" (nstring sympack) name ))
          (cmultiple-value-bind (suggest pstatus) (find-symbol name (coerce-package packname))
              (pcase pstatus  
                    (NIL (ret (format nil "~a~~!~a" packname (describe-symbol sym sympack))))
                    (:inherited (ret (format nil  "~a~~~a" packname (describe-symbol suggest (symbol-package suggest)))))
                    (:internal (csetq name (format nil  "::~a" name)))
                    (:external (csetq name (format nil  ":~a"  name))))
              (csetq name (fif (cnot (eq sympack packageIn)) 
                  (format nil  "~a@~a" (nstring sympack) name)
                  (format nil  "~a~a" (nstring sympack) name)))
              (pwhen (fboundp sym) 
                   (csetq name (format nil "#'~a ~a \"~a\")" name (FUNCTION-SYMBOL-ARGLIST sym) (symbol-function sym))))
              (pwhen (MACRO-OPERATOR-P sym)
                    (csetq name (format nil "(macrocall ~a ~a)"  name  )))
              (pwhen (FUNCTION-SYMBOL-P sym)
                    (csetq name (format nil "(funcall ~a ~a)"  name  )))
              (pwhen (boundp sym) 
                  (fif (keywordp sym) 
                        (csetq name (format nil "<~a>" name)) 
                        (csetq name (format nil "[~a]" name)))))
                    (ret name))))))

 (describe-symbol 'CONS)


(define better-symbol (suggest current) (ret (> (symbol-priority suggest)(symbol-priority current))))

(define symbol-priority (sym &optional (start 1))
  (pwhen (cor (null sym)(keywordp sym)) (ret 0))
  (pwhen (fboundp sym) (cinc start 5))
  (pwhen (boundp sym) (cinc start 3))
  (pwhen (member-if #'(lambda (a) (ret (search a (symbol-name sym)))) '("&" "#" "@" "%" "*" "_"))(cinc start 1))
  (ret start))

(define share-symbols (&optional (from (remove *KEYWORD-PACKAGE* (LIST-ALL-PACKAGES))) (to *PACKAGE*)(count 0))
  (punless from (csetq from (remove *KEYWORD-PACKAGE* (LIST-ALL-PACKAGES))))
  (punless (consp from) (csetq from (list from)))
  (punless to (csetq to *PACKAGE*))
  (punless (consp to) (csetq from (remove to from)) (csetq to (list to)))
  (cdo-all-symbols (s)
    (clet ((f (symbol-package s)))
       (pwhen (member f from))
         (clet ((w (symbol-priority s)))
         (pwhen (> w 1)
           (cdolist (p to)
              (pwhen (> w (symbol-priority (find-symbol (symbol-name s) p)))
                 ;;(FORCE-FORMAT t ";; importing ~a::~a <- ~a ~&"  p s f)
                 (cinc count)(import s p)(import s p)))))))
    (FORCE-FORMAT t ";; shared ~a symbols" count))

(define RESHARE-SYMBOLS ()
 (clet ((usefull-packages (remove *KEYWORD-PACKAGE* (LIST-ALL-PACKAGES)))) 
   (share-symbols usefull-packages (remove *COMMON-LISP-PACKAGE* usefull-packages)))
 (cdo-symbols (sym *SYSTEM-PACKAGE*) (export sym *SYSTEM-PACKAGE*)))

(define best-symbol (current &optional packsearch)
  (fif current
      (fif (consp current)
         (cons (best-symbol (car current) packsearch)(best-symbol (cdr current) packsearch))
         (fif (symbolp current)
            (clet ((initial current)(best current))
               (cdolist (pack (consify (fif packsearch packsearch (list-all-packages))))
                    (csetq best (better-symbol best (find-symbol (string (nstring current)) pack))))
               (ret (values best current)))))))
       
;; (use-symbol 'SYS::STREAM-OPEN-P :CYC #'better-symbol  :external)           
(define use-symbol (symbols &optional (target *package*) (keep #'better-symbol) (inheriting :external))
    (csetq target (coerce-package target))

    (fif (consp symbols)  (ret (sl::mapcar #'(lambda (x) (ret (use-symbol x target keep inheriting))) symbols)))

    (punless (cand symbols (symbolp symbols)) symbols)

    (clet ((from *package*)(name (symbol-name symbols))(package (symbol-package symbols)))       
        (cmultiple-value-bind (suggest pstatus) (find-symbol name package)
         (pwhen (cnot (eq symbols suggest)) (force-format t "; Rotten symbol ~a instead of ~a~%" (describe-symbol suggest package)
                            (describe-symbol symbols package)))
          (cmultiple-value-bind (visible tstatus) (find-symbol name target)
              (pwhen
                  ((null visible) (shadowing-import symbols target)))
                 #|                      
                 ((eq suggest visible) (ret (values visible tstatus))) ;;  (force-format t ";; ~a ~a~%" tstatus (describe-symbol suggest target))                    
                 ((cand (functionp keep)(eq visible (funcall keep suggest visible)))
                     (force-format t "; Keeping ~a instead of ~a mode = ~a~%" (describe-symbol visible target) (describe-symbol suggest target) pstatus)
                     (ret (values visible tstatus)))
                 ((null suggest) (ret (values NIL NIL))) ;;  (force-format t ";; ~a ~a~%" tstatus (describe-symbol suggest target))
                 (t
                   (force-format t "; Using ~a instead of ~a mode = ~a~%" (describe-symbol suggest target) (describe-symbol visible target) tstatus)
                   (shadowing-import package visible)
                   (pwhen (equal tstatus :inherited) (import visible target))
                   (csetq pstatus tstatus)
                   (trace-warn (unintern visible target)))))
               (pcase pstatus  
                    (:internal ;;(force-format t ";; Interning ~a~%" (describe-symbol suggest target))
                       (sl::import suggest target)(sl::intern suggest target))
                    (:external ;;(force-format t ";; Exporting ~a~%" (describe-symbol suggest target))
                       (sl::import suggest target)(sl::intern suggest target)(sl::export suggest target))
                    (:inherited ;;(force-format t ";; Inheriting ~a~%" (describe-symbol suggest target))
                       (sl::import suggest target)(sl::export suggest target)))
                       |#
             (ret (values suggest pstatus))))))
                  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USE-PACKAGE
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define USE-PACKAGE (package &optional (target *package*) (keep #'better-symbol)(inheriting :external)done) 
    (csetq target (coerce-package target))
    (cdolist (pack (consify package))
        (csetq pack (coerce-package pack))
        (cdo-external-symbols (sym pack)
	     (format t ";  POSSIBLY USING ~a ~%" sym)
            (csetq done (cons (use-symbol sym target keep inheriting) done))))
     (ret done))


(define UNUSE-PACKAGE (package &optional (target *package*) (keep #'better-symbol)(inheriting :external)done) 
    (csetq target (coerce-package target))
    (cdolist (pack (consify package))
        (csetq pack (coerce-package pack))
        (cdo-external-symbols (sym pack)
           (clet ((status (cmultiple-value-list (find-symbol (symbol-name sym) target))))
             (pwhen (eq (symbol-package (car status)) package)
                  (pwhen (eq :INHERITED (second status))
                   (unimport sym target)
                   (unintern (make-shadow (symbol-name sym) target))))))))

(define import-in-all (symbols &optional importpacks)
    (cdolist (into (consify (fif importpacks importpacks (list-all-packages))))
       (import symbols into)))


;;; shadowing-import  --  Public
;;;
;;;    If a conflicting symbol is present, unintern it, otherwise just
;;; stick the symbol in.
(define make-shadow (symbol &optional (package *package*) (internals *sticky-symbols*) imports)
  (csetq package (coerce-package package *PACKAGE*))
 (pcond 
   ((null symbol) (ret nil))
   ((consp symbol)(ret (cons (make-shadow (car symbol) package internals imports)(make-shadow (cdr symbol) package internals imports))))
   ((symbolp symbol)(ret (make-shadow (symbol-name symbol) package internals imports)))
   ((stringp symbol)
     (clet ((pstatus ())(found  (string-member symbol internals)))
       ;;(pwhen found (throw (car found)))
       (cmultiple-value-bind 
          (found pstatus) (find-symbol symbol package)
           (pwhen (null pstatus)
;;              (csetq symbol (make-symbol symbol))
              (csetq symbol (intern symbol package))
              (export symbol package)
             (ret symbol))
            (clet ((fpack (symbol-package found)))
              (punless (eq fpack package)
                  (pcase pstatus
                    (:inherited
                       (csetq symbol (make-symbol (string symbol)))
                       (import symbol package)
                       (import symbol package)
                       (export symbol package)
		       (ret symbol))
                    (:external
                       (csetq symbol (make-symbol (string symbol)))
                       (unexport found package)
                       (import symbol package)
                       (import symbol package)
                       (export symbol package)
		       (ret symbol))
                    (:internal
                       (csetq symbol (make-symbol (string symbol)))
                       (import symbol package)
                       (import symbol package)
		       (ret symbol)) 
		       ))
		 ;; (print `(found ,pstatus ,fpack ,found))
		 )

                (ret found))))
      (t (ret symbol))))

;;ARGNAMES-FROM-ARGLIST 
;;  (translate-varblock 'A '(B) '(progn B))
;; (FUNCTION-SYMBOL-ARGLIST  '
(define translate-varblock (name patternIn bodyIn)
 (clet ((pattern patternIn)(nargs ())(body bodyIn))
     (punless (consp body) 
        (pwhen body (print body) 
          (break "translate-varblock bodyIn was not list")))
      (csetq body (translate-block bodyIn 'form name))
      (cdo ((op (car pattern)(car pattern))(pattern (cdr pattern)(cdr pattern)))
        ((null op))
        (force-print op)
        (pcond
         ((consp op)
             (csetq nargs (append nargs `(,(car op))))
             (csetq body (cons (cons 'trace-initvar op) body)))
         ((string-member op '(&key &aux))
             (csetq nargs (append nargs `(&rest lkeys)))
             (csetq body `((KeyLET ,pattern ,@body)))
             (csetq pattern ()))
         (t (csetq nargs (append nargs (list op))))))
      (break)
               (force-print body)
      (punless (consp body) (pwhen body (print body) (break "translate-varblock bodyIn was not list")))
       (ret 
           (clet ((trace `(list ',name ,@(mapcar #'var-traceable nargs))))
               (force-print trace)
          `( (,@nargs)
                 (clet ((*funcall-form* ,trace) (*trace-stack* (cons *funcall-form* *trace-stack*)))
                      (ret (trace-progn (trace-format :funcall "lexical:: ~s" *funcall-form*) ,@body))))))))


(sl::defmacro shadow-defun (package name &rest args-body)
 ;;  (force-print (list 'shadow-defun (describe-symbol name) package))
    (clet ((cl (make-shadow name package)))
      (ret `(define ,cl ,@(translate-varblock cl (car args-body) (cdr args-body))))))

(sl::defmacro shadow-macro (package name &rest args-body)
  ;;(pwhen (consp name)(null name)(csetq package *PACKAGE* name package args-body  (cons name args-body)))
   (force-print (list 'shadow-macro (describe-symbol name) package))
    (clet ((cl (make-shadow name package)))
      (ret `(sl::defmacro ,cl ,@(translate-varblock cl (car args-body)(cdr args-body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSLATION for CL to SUBL - Some of the features of the system must be accessable from everywhere
;; Like the 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *caller-pattern-table* (SL::make-hash-table 23 #'equal) "Stores patterns for function destructuring.")

(define lookup-caller-pattern (name) (ret (gethash name *caller-pattern-table*)))
;;(translate-block bodyIn 'form  name)))

(define string-member (item list)
   (ret (member-if  #'(lambda (ele) (ret (STRING-EQUAL (nstring ele) (nstring item)))) list)))

(define FIND-ALL-SYMBOLS (name &rest lkeys)
    (KEYLET ((use (LIST-ALL-PACKAGES)) results (test #'true)(test-not #'null))
       (cdolist (pack use)
         (clet ((sym (find-symbol (string name) pack)))
          (pwhen sym (pwhen (funcall test sym) (csetq results (cons sym results))))))
       (ret results)))

(define translate-function-expression (form)
  (clet ((vals (cmultiple-value-list (function-lambda-expression form))))
    (punless (car vals) (rplaca vals (third vals)))
    (ret (values vals))))

(define translate-block (form &optional (transkey 'form) name (environment nil) funcall)
 (ret 
   (cond 
     ((SELF-EVALUATING-FORM-P form) form)
     ((functionp form) (ret (list 'function (translate-block (translate-function-expression form) transkey name environment funcall))))
     ((cor (keywordp form)(numberp form)(stringp form)) form)
   ;;  ((member form *incompatable*)(ret (describe-symbol "SYS::" form)))
     ((cor (null form)(equal (type-of form) 'filecomment)(atom form)(stringp form)(numberp form)) (ret form))
     (t 
       (trace-progn
        (clet ((op (car form))(cdrform (cdr form))(new-transkey (lookup-caller-pattern op)))
          (pwhen (symbolp op)
             (cond 
               ;;((string-member op '(sl::progn)) (ret `(trace-progn  ,@(mapcar #'(lambda (item) (ret (translate-block item 'form name ))) cdrform))))
               ((string-member op '("defun" define))                  
                  (ret (cons op (cons (translate-block (car cdrform)) 
                       (translate-varblock (translate-block (car cdrform)) (car (cdr cdrform)) (cdr (cdr cdrform)))))))
                       ;;(translate-varblock (translate-block (car cdrform)) (car (cdr cdrform)) `((ret (sl::progn ,@(cdr (cdr cdrform))))))))))
               ((string-member op '(lambda))    ;; name     transkey         body 
                  (ret (cons op (translate-varblock (gensym) (car cdrform)(cdr cdrform)))))))

            (ret (mapcar #'(lambda (x) (ret (translate-block x 'form name))) form))))))))



(define describe-package (&optional (package *PACKAGE*))
      (csetq package (coerce-package package))
      (clet ((reference (intern (package-name package)))
              (inherited (intern "inherited" package))
              (internal (intern "internal" package))
              (external (intern "external" package)))    
            (csetf (symbol-value external) ())
            (csetf (symbol-value internal) ())
            (csetf (symbol-value inherited) ())
            (CDO-ALL-SYMBOLS (sym package)
             (clet ((name (symbol-name sym)))
               (cmultiple-value-bind (suggest cstatus) (find-symbol name package)
                (pwhen suggest 
                  (csetq suggest (list name (package-name (symbol-package suggest))))
                        (pcase cstatus
                            (:internal (cpush suggest (symbol-value internal) #'equal))
                            (:inherited (cpush suggest (symbol-value inherited) #'equal))
                            (:external (cpush suggest (symbol-value external) #'equal))
                            (otherwise))))))
       (ret (csetf (symbol-value reference)
          (print `(:type ,(type-of package)
                  :name ,(package-name package) 
                  :nicknames ,(package-nicknames package)
                  :package-use-list ,(package-use-list package)
                  :package-used-by-list ,(package-used-by-list package)
                  :package-shadowing-symbols ,(package-shadowing-symbols package)
                 (:internal ,(length (symbol-value internal)))
                 (:external ,(length (symbol-value external)))
                 (:inherited ,(length (symbol-value inherited)))))))))

;;(describe-package :SYS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define shadow functions and macros
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; (lock-package :SYSTEM)
;;; Shadow  --  Public
;;;
;;;
(define shadow (suggest &optional (package *package*))
  "Make an internal symbol in Package with the same name as each of the
  specified symbols, adding the new symbols to the Package-Shadowing-Symbols.
  If a symbol with the given name is already present in Package, then
  the existing symbol is placed in the shadowing symbols list if it is
  not already present."
 (clet ((name (symbol-name suggest))(package (coerce-package package)))
   (cmultiple-value-bind (s w) (find-symbol name package)
    (pwhen (cor (cnot w) (eq w :inherited))
       (csetq s (make-symbol name))
       (intern s package))
     (shadowing-import package s)))
    (ret t))


;;; shadowing-import  --  Public
;;;
;;;    If a conflicting symbol is present, unintern it, otherwise just
;;; stick the symbol in.
;;;
(define shadowing-import (sym &optional (package *package*))
  "Import Symbols into package, disregarding any name conflict.  If
  a symbol of the same name is present, then it is uninterned.
  The symbols are added to the Package-Shadowing-Symbols."
  (clet ((package (coerce-package package)))
      (cmultiple-value-bind (s w) (find-symbol (symbol-name sym) package)
        (punless (cand w (cnot (eq w :inherited)) (eq s sym))
          (pwhen (cor (eq w :internal) (eq w :external))
            ;;
            ;; If it was shadowed, we don't want Unintern to flame out...
            ;;(csetq *all-shadowing-symbols* (remove (cons package s)))
            (unintern s package))
      (intern sys package))
    (shadowing-import package sym)))
  (ret t))


;;(punless (fboundp 'defmethod) (defmacro  #>SYS::defmethod (name pattern &rest body)(ret `(defun ',name ',pattern ,@body))))

  #|



*CANONICALIZE-CLAUSE-SENTENCE-TERMS-SENSE-LAMBDA* value: NIL
*CONTAINING-SUBEXPRESSIONS-LAMBDA-TERM* value: NIL
*INVALID-LAMBDA-LIST-MESSAGE* value: Lambda list ~S of method ~S of interface ~S is not a valid lambda list.
*MERGE-DNF-LAMBDA-VAR* value: NIL
*POSITION-IF-BINARY-LAMBDA-ARG2* value: NIL
*POSITION-IF-BINARY-LAMBDA-FUNC* value: NIL
*RBP-LAMBDA-LAYER* value: NIL
*RKF-IRRELEVANT-TERM-LAMBDA-DOMAIN-MT* value: NIL
*RULE-DNF-LAMBDA-VAR* value: NIL
*TACTIC-STRATEGIC-PRODUCTIVITY-AND-COMPLETENESS-WORSE-LAMBDA-STRATEGY* value: NIL
*UIA-IRRELEVANT-PRECISION-SUGGESTION-LAMBDA-AGENDA* value: NIL
*UIA-IRRELEVANT-PRECISION-SUGGESTION-LAMBDA-MT* value: NIL
API-APPLY-LAMBDA [function] (REQ-0 REQ-1)


(API-APPLY-LAMBDA '(&rest r) '(1 2 3))

AR-PHRASE-DIVIDE-EQ-BEST-LAMBDA-SUBSTITUTE [function] (REQ-0)
CLASSES-LAMBDA-LIST-GIVEN-METHOD-DECL [function] (REQ-0)
CLASSES-VALID-LAMBDA-LIST-P [function] (REQ-0)
CONTAINING-SUBEXPRESSIONS-LAMBDA-FN [function] (REQ-0)
CYC-LAMBDA [function] (&OPTIONAL OPT-0 OPT-1 OPT-2 OPT-3 OPT-4 OPT-5)
EVAL-IN-API-USER-LAMBDA-FN? [function] (REQ-0)
FILTERED-LAMBDA-LIST unbound
function-expression [function] (REQ-0)
GENERALITY-SORT-LAMBDA [function] (REQ-0)
KBQ-FILTER-QUERY-SET-RUN-TO-QUERIES-LAMBDA [function] (REQ-0)
KBQ-FILTER-QUERY-SET-RUN-TO-QUERIES-NOT-LAMBDA [function] (REQ-0)
KCT-FILTER-TEST-SET-RUN-TO-TESTS-LAMBDA [function] (REQ-0)
KCT-FILTER-TEST-SET-RUN-TO-TESTS-NOT-LAMBDA [function] (REQ-0)
LAMBDA-EXPRESSION? [function] (REQ-0)
LAMBDA-FUNCTION-ARITY [function] (REQ-0)
LAMBDA-FUNCTION-EXPRESSION [function] (REQ-0)
LAMBDA-FUNCTION-FORMAL-ARGS [function] (REQ-0)
LAMBDA-FUNCTION-P [function] (REQ-0)
LAMBDA-LIST unbound
LAMBDA-SUBEVENT? [function] (REQ-0)
LAMBDA-SYNTAX-P [function] (REQ-0)
METHOD-LAMBDA-LIST [function] (REQ-0)
METHOD-LISTENERS-FILTERED-LAMBDA-LIST [function] (REQ-0 REQ-1)
METHODS-FILTER-OPTION-WORDS-FROM-LAMBDA-LIST [function] (REQ-0)
METHODS-LAMBDA-LIST-TO-LISTED-ARG-VALUE-EXPRESSION [function] (REQ-0)
OBJECT-METHOD-LAMBDA-LIST-METHOD [function] (REQ-0 REQ-1)
POSITION-IF-BINARY-LAMBDA [function] (REQ-0)
RBP-RB-LAYER-EXEMPT-RULE-LAMBDA? [function] (REQ-0)
REMOVAL-LAMBDA [function] (REQ-0)
RKF-IRRELEVANT-TERM-LAMBDA? [function] (REQ-0)
UIA-IRRELEVANT-PRECISION-SUGGESTION-TERM-LAMBDA? [function] (REQ-0)
_CSETF-METHOD-LAMBDA-LIST [function] (REQ-0 REQ-1)
FUNCTOR-IN-BODY-P  (SYMBOL BODY) 
SUBL-NON-VARIABLE-NON-KEYWORD-SYMBOL-P
SUBL-NON-VARIABLE-SYMBOL-P
SUBL-PERFORMATIVE-P


EVERY-NTH  (N LIST) 
 

ARGS-FROM-ARG-LIST  
 

 DEFINE-API-OBSOLETE

 TRANSLATOR-RET-OPTIMIZE-BODY
 TRANSLATE-FORM-EXPANSION-FACTOR	 	
FUNCTOR-IN-EXPRESSION-P  (FUNCTOR EXPRESSION) 
    (csetq packname (package-name (csetq package (coerce-package *PACKAGE*))))
  |#


;;based on (sethash key table value)
(defmacro catch (tag &rest body)
 (ret `(apply #'values
        (clet ((*thrown* :unthrown)
           (*result* :unevaled))
           (ccatch ,tag *thrown* (csetq *result* (multiple-value-list (trace-progn  ,@body))))
           (fif (equal *result* :unevaled) (list *thrown*) *result*)))))

(define map-sequences (function sequences)
 (ret (fif
    (member () sequences) ()
    (cons (apply function (mapcar #'car sequences))
       (map-sequences function (mapcar #'cdr sequences))))))

(define map (result-type function &rest sequences)
 (ret (fif result-type (coerce (map-sequences function sequences) result-type)
    (sl::progn (map-sequences function sequences) nil))))


(define concatenate (cltype &rest pattern) (ret `(coerce (cconcatenate ,@pattern) ,cltype)))
(define string-concat (&rest list)  (ret (apply #'cconcatenate (cons "" (flat-string (flatten list))))))
(define concat (&rest list)(ret (apply #'cconcatenate (cons "" (mapcar (lambda (x)(ret (fif (stringp x) x (coerce x 'string) ))) list)))))
(define string-concat (&rest list)(ret (apply #'cconcatenate (cons "" (mapcar #'(lambda (x) (ret (fif (stringp x) x (coerce x 'string) ))) list)))))
(defmacro string-concat (&rest list) (ret `(cconcatenate ,@list)))
(define string-concat (&rest list)(ret (apply #'cconcatenate (cons "" (mapcar #'(lambda (x)(ret (fif (stringp x) x (coerce x 'string) ))) list)))))

(define same-classes (current target) (ret (equal current target)))
(defvar *coerce-methods* (sl::make-hash-table 32))
(define coerce (value result-type &optional (subclassfn #'same-classes))
 (clet ((vtype (type-of value))
     (len value)
     (cltype result-type)
     (howto (gethash result-type *coerce-methods*)))

     (pwhen (equal result-type vtype) (ret value))
     (pwhen howto
      (sl::progn 
       (csetq howto (pcond ((assoc vtype howto subclassfn))((assoc 't howto))))
       (pwhen howto 
        (ret (sl::eval 
         `(clet ((value ',value)(result-type ',result-type)(vtype ',vtype))
           ,(cdr howto)))))))

    (funless (cand (consp cltype)
           (csetq len (second cltype))
           (csetq cltype (car cltype)))
     (fif
      (consp value)
      (csetq len (length value))))
    (pcase cltype ('t (ret value))
        ('sequence
        (fif
         (sequencep value)
         (ret (copy-seq value))
         (csetq value (write-to-string value)))
        (csetq cltype (make-vector len))
        (cdo ((idx 0 (+ 1 idx)))
           ((= idx len)
           (ret cltype ))
           (set-aref cltype idx (elt value idx))))
        ('character
        (pcond
         ((characterp value)
         (ret value))
         ((numberp value)
         (ret (code-char value)))
         ((stringp value)
         (ret (char value 0)))
         (t (ret (char (coerce value 'string ) 0)))))
        ('number
        (pcond
         ((numberp value)
         (ret value))
         ((characterp value)
         (ret (char-code value)))
         ((stringp value)
         (ret (string-to-number value)))
         (t (ret (string-to-number (write-to-string value))))))
        ('integer (ret (round (coerce value 'number))))
        ('fixnum (ret (round (coerce value 'number))))
        ('float (ret (float (coerce value 'number))))
        ('real (ret (float (coerce value 'number))))
        ('flonum (ret (float (coerce value 'number))))
        ('string
        (pcond
         ((stringp value)
         (ret value))
         ((characterp value)
         (ret (make-string 1 value)))
         ((sequencep value)
         (csetq cltype (make-string len))
         (cdo ((idx 0 (+ 1 idx)))
            ((= idx len)
            (ret cltype ))
            (set-aref cltype idx (coerce (elt value idx) 'character))))
         (t (ret (write-to-string value)))))
        ('list
        (pcond
         ((listp value)
         (ret list))
         ((sequencep value)
         (csetq cltype nil)
         (cdo ((idx len (- idx 1)))
            ((= idx 0)
            (ret cltype ))
            (csetq cltype (cons (elt value idx) cltype))))
         (t (csetq cltype nil)
          (csetq value (write-to-string value))
          (cdo ((idx len (- idx 1)))
             ((= idx 0)
             (ret cltype ))
             (csetq cltype (cons (elt value idx) cltype))))))
        ('cons
        (pcond
         ((listp value)
         (ret list))
         ((sequencep value)
         (csetq cltype nil)
         (cdo ((idx len (- idx 1)))
            ((= idx 0)
            (ret cltype ))
            (csetq cltype (cons (elt value idx) cltype))))
         (t (csetq cltype nil)
          (csetq value (write-to-string value))
          (cdo ((idx len (- idx 1)))
             ((= idx 0)
             (ret cltype ))
             (csetq cltype (cons (elt value idx) cltype))))))
        ('keypair
        (pcond
         ((atom value)
         (ret list value))
         (t (ret (coerce value 'cons)))))
        ('alist (csetq cltype (csetq cltype nil))
        (fif
         (sequencep value) t (csetq value (coerce value 'sequence)))
        (cdo ((idx 0 (+ 1 idx)))
           ((= idx len)
           (ret cltype))
           (csetq result-type (coerce (elt value idx) 'cons))
           (csetq cltype (acons (car result-type)
                     (cdr result-type) cltype)))
        (ret cltype))
        ('hash-table
        (fif
         (hash-table-p value)
         (ret value))
        (csetq cltype (sl::make-hash-table len))
        (fif
         (sequencep value) t (csetq value (coerce value 'sequence)))
        (cdo ((idx 0 (+ 1 idx)))
           ((= idx len)
           (ret cltype))
           (print (list 'coerce value result-type cltype len (elt value idx)))
           (csetq result-type (coerce (elt value idx) 'keypair))
           (sethash (car result-type) cltype (cdr result-type))))
        (otherwise (ret value)))
    (throw :coerce (list value result-type)))
 (ret value))


(defmacro defcoerce (to from &rest body) 
 "the body assumes bindings will be present for value vtype result-type and howto. (car howto) will yeild the original from durring defcoerce
 example:: (defcoerce string t (string value))
 (car howto) = > t 
 so that the (coerce #\a 'string ) procedure can know that vtype when character was found as a subclass of t"
  (ret `(sethash ',to *coerce-methods* (acons ',from '(sl::progn ,@body) (gethash ',to *coerce-methods*)))))

(print '(load "common.lisp")) (terpri)
(punless (sl::member :CYC-COMMON-LISP sl::*features*)
   (force-print '(LOAD "common-lisp.lisp"))
   (fif (cand nil (sl::yes-or-no-p) )
    (progn
       (cyc::USE-PACKAGE '(:CYC :SL) :SYSTEM
       (cpushnew :CYC-COMMON-LISP sl::*features*))
       (defcoerce chew t (cconcatenate (string value) "-chew"))
       (print (coerce "stringy" 'chew))
    ;;   (in-package "SYSTEM"))
    )
    (progn
       (CDO-SYMBOLS (sym *SUBLISP-PACKAGE*) (export sym *SUBLISP-PACKAGE*))
       (CDO-SYMBOLS (sym *CYC-PACKAGE*) (export sym *CYC-PACKAGE*))
       (defcoerce chew t (cconcatenate (string value) "-chew"))
       (print (coerce "stringy" 'chew)))))
  ;;     (in-package "INT"))))


;;(in-package "LISP")
;;(import-in-all (best-symbol '(make-shadow defun shadow-defun shadow-macro defun shadow-operator)))

;;(make-shadow '(PROGN MAKE-HASH-TABLE LOAD STRING-DOWNCASE MAKE-STRING LOOP ) :SYS) 
;;(defun #>SYS::string-downcase (str) (sl::string-downcase (string str)))

;;(make-shadow 'define :SYS)
(sl::defmacro #>SYS::cl-define (suggest pattern &rest body)
     (ret `(trace-progn (sl::define ,suggest ,@(translate-varblock suggest pattern body)))))

(sl::defmacro defun (suggest pattern &rest body)
     (ret `(trace-progn (sl::define ,suggest ,@(translate-varblock suggest pattern body)))))



;;(make-shadow 'lambda :SYS) 
;;(sl::defmacro cl::lambda (pattern &rest body) (ret `(sl::lambda ,pattern (ret (trace-progn ,@body)))))

;;(make-shadow #>SYS::defstruct :SYS)
(sl::define #>SYS::defstruct (name &rest rest) 
   (clet ((slots  (mapcar #'(lambda (x) 
                (ret (fif (atom x) x (car x)))) rest)))
    `(sl::defstruct (,name) ,@slots)))

;;(SYS::defstruct filecomment start end src block-p)

;;;(in-package "CL")

(defvar *incompatable* '(create-instance isa all-instances comment arity load-kb 
 cdefmacro SYS::flatten assoc-equal ordered-set-difference ordered-intersection quotify permute trim-whitespace first-char 
 last-char ends-with starts-with string-to-number read make-string cdefmacro string-downcase make-hash-table 
 loop intersection defstruct equal member remove remove-duplicates delete-duplicates subsetp))

(define SYS::make-package (name &rest lkeys) 
   (KeyLET ((use *default-package-use*) nicknames)
    (force-print `(SYS::make-package ,name ,use ,nicknames))
     (ret (cyc::eval (force-print 
       `(sl::make-package ,name 
               ',(reverse (mapcar #'package-name (mapcar #'coerce-package use)))
               ',(mapcar #'SYS::make-keyword nicknames)))))))

#|

#> CL 'EWRT
;;(in-package (package-name savepack))

(find-symbol "READ" :SYS)
(csetq sym  (make-symbol "PCOND"))
(import sym :SYS)
(intern "PCOND" :SYS)
(find-symbol "PCOND" :SYS)

(intern (make-symbol "ACONS") :SYS)
(find-symbol "ACONS" :SYS)

(shadow-macro CL read (&rest body) (ret (cons 'SL::read  body)))
(print '(shadow-macro CL sl::eval (&rest body) (ret (cons 'SL::eval  body))))

(define SYS::read (&rest body)       
  (terpri) 
  (ret (cons 'sl::read  body)))

|#

(sl::defmacro shadow-operator (package name &optional (other name) (callpattern '(body) ) (varpattern `(&rest ,@callpattern)))
    (clet ((cl (make-shadow name package)))      
        (print cl) 
        (ret (print `(sl::defmacro ,cl ,varpattern (ret (cons ',other ,@callpattern)))))))

(shadow-operator :CL push cpush)
(shadow-operator :SYS svref aref)
(shadow-operator :SYS vset set-aref)
(shadow-operator :SYS incf cinc)
(shadow-operator :SYS decf cdec)
(shadow-operator :SYS not cnot)

(shadow-operator :CYC push cpush)
(shadow-operator :CYC svref aref)
(shadow-operator :CYC vset set-aref)
(shadow-operator :CYC incf cinc)
(shadow-operator :CYC decf cdec)
(shadow-operator :CYC not cnot)

;;(shadow-operator :SYS progn trace-progn)
;;(shadow-operator :SYS char-int char-code)


;;(shadow-macro SYS::apply (fn &rest body) (ret `(apply ,fn ,@body)))

;;(export '(SYS::load like-funcall 'eval ))
#|
#+CRISPY
'(defun #>SYS::eval (code) 
    (force-print `(sl::eval ,code))
    (ret (SL::eval `(trace-progn ,code))))

;;(defmacro  #>SYS::handler-case (form &rest cases) (print (list 'handler-case form cases)) (ret `,form))

(csetq  *load-verbose* t)
(csetq *load-print* t)

#+CRISPY
(defun #>SYS::load (filespec &rest lkeys)
 (KeyLet (verbose print if-does-not-exist external-format)
  (SL::clet ((*standard-input* (SL::OPEN-TEXT filespec :input))) 
    (cdo () ()
    (clet ((expr (SL::read *standard-input* nil :EOF)))
     (pwhen (equal expr :EOF)
        (SL::close *standard-input*)
        (ret T))
      (SYS::eval expr))))))

|#
;;(defmacro  #>SYS::defstub (feat symb &optional default) `(define ,symb (&rest body) (format t "~s~%" (cons ',feat (cons ',symb body))) ,default))

#|

'(defmacro  #>SYS::go (label)
 (clet ((name (label-to-functionname label)))
  `(throw ,name #',name)))

'(defmacro  #>SYS::tagbody (&body body)
    "The emulation of tagbody/go by catch/throw is considerably less obvious than the emulation of block/return-from. 
    This is because tagbody defines a number of different labels rather than a single block name, and because the parsing of the 
    tagbody body is considerably more complicated. The various segments of the tagbody are emulated by a labels nest of mutually 
    recursive functions, which are forced to all execute at the correct dynamic depth by means of a 
    'trampoline. If the implementation implements the 'tail recursion' optimization for functions 
    which have no arguments and return no values, and if the simpler cases of go's are optimized away, then this emulation can be quite efficient."
             (clet ((init-tag (gensym)) (go-tag (gensym)) (return-tag (gensym))
                    
                    (functions
                     (mapcon
                         #'(lambda (seq &aux (label (car seq) (s (cdr seq)))
                                        (when (atom label)
                                          (let ((p (position-if #'atom s)))
                                            `((,(label-to-functionname label) ()
                                                 ,@(subseq s 0 (or p (length s)))
                                                 ,(if p `(,(label-to-functionname (elt s p)))
                                                    `(throw ,return-tag 'nil)))))))
                             `(,init-tag ,@body))))
                    `(let* ((,go-tag (list nil)) (,return-tag (list nil))
                                                 ,@(mapcar #'(lambda (f) `(,(car f) ,go-tag)) functions))
                       (catch ,return-tag
                              (labels ,functions
                                (let ((nxt-label #',(caar functions)))
                                  (loop (csetq nxt-label (catch ,go-tag (funcall nxt-label)))))))))))


(defmacro  #>SYS::labels (fns &body forms)
             "CIRCULAR ENVIRONMENTS OF 'LABELS EMULATED BY 'FLET AND 'SETQ: It is generally believed that the circular environments of labels cannot be 
    obtained by means of flet. This is incorrect, as the following emulation (reminiscent of Scheme) shows. 
    With a more sophisticated macro-expansion, this emulation can be optimized into production-quality code."
             (let* ((fnames (mapcar #'car fns))
                    (nfnames (mapcar #'(lambda (ignore) (gensym)) fnames))
                    (nfbodies (mapcar #'(lambda (f) `#'(lambda ,@(cdr f))) fns)))
               `(let ,(mapcar #'(lambda (nf) `(,nf #'(lambda () ()))) nfnames)
                  (flet ,(mapcar #'(lambda (f nf) `(,f (&rest a) (apply ,nf a)))
                           fnames nfnames)
                    (flet ,fns
                      (sl::progn ,@(mapcar #'(lambda (f nf) `(csetq ,nf #',f))
                                 fnames nfnames))
                      ,@forms)))))

|#
;;(defmacro  #>SYS::loop (&rest body) (ret `(loop ,@body)))
#|
;;;; CLtL2 and ANSI CL Compatibility
  
(defmacro  #>SYS::loop (&rest exps)
 ;;"supports both ansi and simple loop. warning:: not every loop keyword is supported."
 (format t "~%~s~%" `(SL::loop ,@exps))(force-output)
 (punless (member-if #'symbolp exps) (ret `(loop ,@exps))) 
 (pcase (car exps) 
   ((until while) (ret exps))
   (for ;;(SYS::loop-for (cdr exps)))
    (break "SYS::loop-for"))
   (repeat (break "SYS::loop-repeat"))
   (otherwise (ret `(SL::loop ,@exps))))

'(defmacro  #>SYS::loop-for (var from-in start/list &rest exps)
  (pcase 
    from-in
    (from (SYS::loop-for-from var start/list))))


;; some tests
'(print (translate-block '
 (defun #>SYS::member (item list &key (test #'eql)(key #'identity) test-not)
   (pwhen test-not (setq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))  
   (member item list test key))))
|#

(sl::defmacro defun (name pattern &rest body)  
   ;;(force-print (list 'defun (describe-symbol name) pattern body))
   (ret `(trace-progn (sl::define ,name ,pattern (ret (trace-progn ,@body))))))

(sl::defmacro #>CL::defmacro (name pattern &rest body)  
   ;;(force-print (list 'defun (describe-symbol name) pattern body))
   (ret `(trace-progn (sl::defmacro ,name ,pattern (ret (trace-progn ,@body))))))



(macroexpand-1 '(defun #>SYS::make-string (size &rest lkeys)
 (clet (element-type initial-element initial-contents)
        (init-keyval initial-element #\space)
  (ret (SL::make-string size initial-element)))))

(defun #>SYS::make-string (size &rest lkeys)
 (clet (element-type initial-element initial-contents)
        (init-keyval initial-element #\space)
  (ret (SL::make-string size initial-element))))

(defun #>SYS::make-hash-table (&rest lkeys)
 (clet (test size rehash-size rehash-threshold)
  (init-keyval size 64)(init-keyval test #'eql)
  (ret (SL::make-hash-table size test))))

;; barely started coding
(defun #>SYS::make-array (dimensions &rest lkeys)
 (clet (element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset)
  (init-keyval initial-element) 
  (ret (SL::make-vector dimensions initial-element))))

;; barely started coding
(defun #>SYS::array-dimensions (array subdim)
  (ret (pcase
    subdim 
    (0 (length array)
    (t (length (nth subdim array)))))))


;;(SYS::defun force-print (stuff) (print stuff) (force-output) stuff)
(force-print "this is not really cl!")
;;(macroexpand '(defun force-print (stuff) (print stuff) (force-output)stuff))
;;(macroexpand '(trace-progn 'MYPRINT (STUFF) (PRINT STUFF) (FORCE-OUTPUT) STUFF))
;;(macroexpand '(trace-progn  (PRINT STUFF) (FORCE-OUTPUT) STUFF))
(define structure-slot (object slot)
   (ret (pcond 
       ((structurep object) (sl::_structure-slot object slot)))))

(define set-structure-slot (object slot value)
   (ret (pcond 
       ((structurep object) (sl::_set-structure-slot object slot value)))))

#|
(sl::progn 
 (defconstant *dtp-delay* 'delay)
 
 (defun #>SYS::delay-p (SL::object)
  (ret (cand (SL::_structures-bag-p SL::object)
        (eq (SL::_structure-slot SL::object 1) *dtp-delay*))))
 
 (defun #>SYS::delay-value (SL::object)
  (check-type SL::object delay-p)
  (ret (SL::_structure-slot SL::object 2)))
 
 (defun #>SYS::delay-function (SL::object)
  (check-type SL::object delay-p)
  (ret (SL::_structure-slot SL::object 3)))
 
 (defun #>SYS::_csetf-delay-value (SL::object SL::value)
  (check-type SL::object delay-p)
  (ret (SL::_set-structure-slot SL::object 2 SL::value)))
 
 (defun #>SYS::_csetf-delay-function (SL::object SL::value)
  (check-type SL::object delay-p)
  (ret (SL::_set-structure-slot SL::object 3 SL::value)))
 (SL::_def-csetf 'delay-value '_csetf-delay-value)
 (SL::_def-csetf 'delay-function '_csetf-delay-function)
 
 (defun #>SYS::make-delay (&optional SL::arglist)
  (clet ((SL::new (SL::_new-structure *dtp-structures-bag* 2)))
     (SL::_clear-sub-structure SL::new 2 *dtp-delay*)
     (clet ((#::next SL::arglist))
        (loop
         (fif
          #::next
          
          (clet ((#::current-arg (car #::next))
             (#::current-value (cadr #::next)))
             (pcase #::current-arg
                (:value (_csetf-delay-value SL::new #::current-value))
                (:function (_csetf-delay-function SL::new #::current-value))
                (otherwise (error (format nil "invalid slot ~s for construction function" #::current-arg))))
             (csetq #::next (cddr #::next)))
          (ret SL::new))))
     (ret SL::new)))
 (identity 'delay))

|#


#|
Function MAP-INTO (still writing also require the array stuff way below)

Syntax::

map-into result-sequence function &rest sequences => result-sequence

Arguments and Values::

result-sequence--a proper sequence.
function--a designator for a function of as many arguments as there are sequences.
sequence--a proper sequence.

Description::

Destructively modifies result-sequence to contain the results of applying function to each element in the argument sequences in turn. 

Examples::

 (setq a (list 1 2 3 4) b (list 10 10 10 10)) => (10 10 10 10)
 (map-into a #'+ a b) => (11 12 13 14)
 a => (11 12 13 14)
 b => (10 10 10 10)
 (setq k '(one two three)) => (ONE TWO THREE)
 (map-into a #'cons k a) => ((ONE . 11) (TWO . 12) (THREE . 13) 14)
 (map-into a #'gensym) => (#::G9090 #::G9091 #::G9092 #::G9093)
 a => (#::G9090 #::G9091 #::G9092 #::G9093)

(defun #>SYS::map-into (result-sequence function &rest sequences)
 "Destructively set elements of RESULT-SEQUENCE to the results
 of applying FUNCTION to respective elements of SEQUENCES."
 (clet ((arglist (make-list (length sequences)))
     (n (fif (listp result-sequence)
        most-positive-fixnum
        (array-dimension result-sequence 0))))
  ;; arglist is made into a list of pattern for each call
  ;; n is the length of the longest vector
  (pwhen sequences
   (csetf n (min n (SYS::loop for seq in sequencesminimize (length seq)))))
  ;; shadow-defun :SYS some shared functions::
  (clet
   ((*do-one-call* 
    #'(lambda (i)
     (ret (cdolist (seq sequences)
        (cdolist (arg arglist)
        (cdo (fif (listp (first seq))
           (csetf (first arg)
              (pop (first seq)))
           (csetf (first arg) 
              (aref (first seq) i))))))))
     (apply function arglist))
    (*do-result* 
     #'(lambda (i) 
     (ret (fif (cand (vectorp result-sequence)
         (array-has-fill-pointer-p result-sequence))
       (csetf (fill-pointer result-sequence) 
          (max i (fill-pointer result-sequence))))))))
   ;; (declare (inline *do-one-call*))
   ;; Decide if the result is a list or vector,
   ;; and SYS::loop through each element
   (fif (listp result-sequence)
     (SYS::loop for i from 0 to (- n 1)
        for r on result-sequence
        do (csetf (first r)
             (*do-one-call* i))
        finally (*do-result* i))
     (SYS::loop for i from 0 to (- n 1)
        do (csetf (aref result-sequence i)
             (*do-one-call* i))
        finally (*do-result* i))))
   result-sequence))
|#

(defvar *complement-fns* (sl::make-hash-table 31) "defcomplement Hashtable to lookup how things like (complement #'member) might return")
(defun #>SYS::complement (fn)
 "If FN returns y, then (paip-complement FN) returns (not y)."
 (ret (pcond
   ((gethash fn *complement-fns*))
   (t #'(lambda (&rest pattern) (ret (cnot (apply fn pattern))))))))

;; example:: (defcomplement < >=)
(defmacro  #>SYS::defcomplement (posfn negfn)
  (ret `(sl::progn (sethash #',posfn *complement-fns* #',negfn) (sethash #',negfn *complement-fns* #',posfn))))


;; emits 
;;;;;;;;;;;;;;;;;   
(defun #>SYS::MEMBER (ITEM LIST &REST LKEYS) 
 (RET 
  (trace-progn  
  (CLET (TEST KEY TEST-NOT) 
    (init-keyval KEY (FUNCTION IDENTITY)) (init-keyval TEST (FUNCTION EQL)) 
    (pWHEN TEST-NOT (SETQ TEST (FUNCTION (LAMBDA (X Y) (RET (CNOT (FUNCALL TEST-NOT X Y)))))))
    (MEMBER ITEM LIST TEST KEY)))))


'(print (translate-block '
 (defun #>SYS::intersection (list-1 list-2 &key (test #'eql)(key #'identity) test-not)
   (pwhen test-not (setq test #'(lambda (x y)(not (funcall test-not x y)))))
   (intersection list-1 list-2 test key))))

(defun #>SYS::intersection (list-1 list-2 &rest lkeys)
 (trace-progn 'SYS::intersection list-1 list-2 '&rest lkeys
 (clet (test key test-not)
    (init-keyval test)(init-keyval key)(init-keyval test-not)   
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (ret (intersection list-1 list-2 test key )))))


(defun #>SYS::remove (item list &rest lkeys )
 ;;(force-print `(SYS::remove ,item ,list &rest ,lkeys))
 (clet (test from-end test-not start end count key)
    (init-keyval test #'eql)(init-keyval key #'identity)(init-keyval test-not)(init-keyval from-end)(init-keyval start 0)(init-keyval end)(init-keyval count)
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (pwhen from-end (ret (reverse (remove item (reverse list) test key start end count))))
    (ret (remove item list test key start end count))))

(defun #>SYS::remove-duplicates (list &rest lkeys)
 (trace-progn 'SYS::remove-duplicates (list '&rest lkeys) ())
 (clet (test from-end test-not start end count key)
    (init-keyval test)(init-keyval key)(init-keyval test-not)(init-keyval from-end)(init-keyval start)(init-keyval end)(init-keyval count)
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (funless start (csetq start 0))
    (pwhen from-end (ret (reverse (remove-duplicates (reverse list) test key start end))))
    (ret (remove-duplicates list test key start end))))

(defun #>SYS::delete-duplicates (list &rest lkeys)  
 (trace-progn 'SYS::delete-duplicates (list '&rest lkeys)
 (clet (test from-end test-not start end count key)
    (init-keyval test)(init-keyval key)(init-keyval test-not)(init-keyval from-end)(init-keyval start)(init-keyval end)(init-keyval count)
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (funless start (csetq start 0))
    (pwhen from-end (ret (reverse (delete-duplicates (reverse list) test key start end))))
    (ret (delete-duplicates list test key start end)))))

(defun #>SYS::subsetp (list list2 &rest lkeys)
 (trace-progn 'SYS::subsetp (list list2 '&rest lkeys)
 (clet (test key test-not)
    (init-keyval test)(init-keyval key)(init-keyval test-not)   
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (ret (subsetp list list2 test key)))))

#||#

;;(USE-PACKAGE :SYS :CYC #'better-symbol)






;;(shadow-macro SYS::apply (fn &rest body) (ret `(apply ,fn ,@body)))
#|
;;(export '(SYS::load like-funcall 'eval ))
#+CRISPY
'(defun #>SYS::eval (code) 
    (force-print `(sl::eval ,code))
    (ret (SL::eval `(trace-progn ,code))))

;;(defmacro  #>SYS::handler-case (form &rest cases) (print (list 'handler-case form cases)) (ret `,form))

(csetq  *load-verbose* t)
(csetq *load-print* t)

#+CRISPY
(defun #>SYS::load (filespec &rest lkeys)
 (KeyLet (verbose print if-does-not-exist external-format)
  (SL::clet ((*standard-input* (SL::OPEN-TEXT filespec :input))) 
    (cdo () ()
    (clet ((expr (SL::read *standard-input* nil :EOF)))
     (pwhen (equal expr :EOF)
        (SL::close *standard-input*)
        (ret T))
      (SYS::eval expr))))))

|#




(defmacro  #>SYS::defstub (feat symb &optional default) `(define ,symb (&rest body) (format t "~s~%" (cons ',feat (cons ',symb body))) ,default))

#|

(defmacro  #>SYS::go (label)
 (let ((name (label-to-functionname label)))
  `(throw ,name #',name)))

(defmacro  #>SYS::tagbody (&body body)
    "The emulation of tagbody/go by catch/throw is considerably less obvious than the emulation of block/return-from. 
    This is because tagbody defines a number of different labels rather than a single block name, and because the parsing of the 
    tagbody body is considerably more complicated. The various segments of the tagbody are emulated by a labels nest of mutually 
    recursive functions, which are forced to all execute at the correct dynamic depth by means of a 
    'trampoline. If the implementation implements the 'tail recursion' optimization for functions 
    which have no arguments and return no values, and if the simpler cases of go's are optimized away, then this emulation can be quite efficient."
             (let* ((init-tag (gensym)) (go-tag (gensym)) (return-tag (gensym))
                    
                    (functions
                     (mapcon
                         #'(lambda (seq &aux (label (car seq) (s (cdr seq)))
                                        (when (atom label)
                                          (let ((p (position-if #'atom s)))
                                            `((,(label-to-functionname label) ()
                                                 ,@(subseq s 0 (or p (length s)))
                                                 ,(if p `(,(label-to-functionname (elt s p)))
                                                    `(throw ,return-tag 'nil)))))))
                             `(,init-tag ,@body))))
                    `(let* ((,go-tag (list nil)) (,return-tag (list nil))
                                                 ,@(mapcar #'(lambda (f) `(,(car f) ,go-tag)) functions))
                       (catch ,return-tag
                              (labels ,functions
                                (let ((nxt-label #',(caar functions)))
                                  (loop (csetq nxt-label (catch ,go-tag (funcall nxt-label)))))))))))


(defmacro  #>SYS::labels (fns &body forms)
             "CIRCULAR ENVIRONMENTS OF 'LABELS EMULATED BY 'FLET AND 'SETQ: It is generally believed that the circular environments of labels cannot be 
    obtained by means of flet. This is incorrect, as the following emulation (reminiscent of Scheme) shows. 
    With a more sophisticated macro-expansion, this emulation can be optimized into production-quality code."
             (let* ((fnames (mapcar #'car fns))
                    (nfnames (mapcar #'(lambda (ignore) (gensym)) fnames))
                    (nfbodies (mapcar #'(lambda (f) `#'(lambda ,@(cdr f))) fns)))
               `(let ,(mapcar #'(lambda (nf) `(,nf #'(lambda () ()))) nfnames)
                  (flet ,(mapcar #'(lambda (f nf) `(,f (&rest a) (apply ,nf a)))
                           fnames nfnames)
                    (flet ,fns
                      (sl::progn ,@(mapcar #'(lambda (f nf) `(csetq ,nf #',f))
                                 fnames nfnames))
                      ,@forms)))))

|#
;;(defmacro  #>SYS::loop (&rest body) (ret `(loop ,@body)))


#|
;; barely started coding
(defun #>SYS::array-dimensions (array subdim)
  (ret (pcase
    subdim 
    (0 (length array)
    (t (length (nth subdim array)))))))

;;;; CLtL2 and ANSI CL Compatibility
  
(defmacro  #>SYS::loop (&rest exps)
 ;;"supports both ansi and simple loop. warning:: not every loop keyword is supported."
 (format t "~%~s~%" `(SL::loop ,@exps))(force-output)
 (punless (member-if #'symbolp exps) (ret `(loop ,@exps))) 
 (pcase (car exps) 
   ((until while) (ret exps))
   (for ;;(SYS::loop-for (cdr exps)))
    (break "SYS::loop-for"))
   (repeat (break "SYS::loop-repeat"))
   (otherwise (ret `(SL::loop ,@exps))))

'(defmacro  #>SYS::loop-for (var from-in start/list &rest exps)
  (pcase 
    from-in
    (from (SYS::loop-for-from var start/list
 ))))


;; some tests
'(print (translate-block '
 (defun #>SYS::member (item list &key (test #'eql)(key #'identity) test-not)
   (pwhen test-not (setq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))  
   (member item list test key))))
|#

#| XXXXXXXXXXXXXXXXXXXXXxx
(defmacro  #>SYS::_setf (place value)
    (csetq value (sl::eval `(trace-progn value)))
    (with-error-handler
      #'(lambda () (ret value))
      (ret (csetf place value)))
   (pwhen (consp place)
     (clet ((object (cadr place))(type (type-of object))(slot (car place))(args (cddr place))(slotname (string slot)))
            (csetq slotname (get-type-slot-args type slot args))
            (csetq value (sl::eval `(,slotname ,object ,@(append args (list value)))))))
   (ret place))

  
(defmacro  #>SYS::_getf (place value)
  (csetq value (sl::eval `(trace-progn value)))
  (with-error-handler
      #'(lambda () (ret place))
      (ret (aref place value)))
    (pwhen (consp place)
     (clet ((object (cadr place))(type (type-of object))(slot (car place))(args (cddr place))(slotname (string slot)))
            (csetq slotname (get-type-slot-args type slot args  '("PUT" "GET-" "GET" "REF-" "REF")))
            (csetq value (sl::eval `(,slotname ,object ,@args)))))
  (ret place))
       
 (define get-type-slot-args (type slot args &optional trylist)
    (clet ((slotname (string slot))(typename (string slot)))
      (ret (member-if  #'(lambda (header) 
                (clet ((name (cconcatenate header slotname))(namef (find-symbol name))
                       tname (cconcatenate header slotname (typename type) ))(tnamef (find-symbol tname)))
                       (pwhen (fboundp tnamef) (ret tnamef))
                       (pwhen (fboundp namef) (ret namef))))
                       (append try '("_CSETF-" "SET-" "PUT-" "SET" )))))
   |# 
   ;;; XXXXXXXXXXXXXXXXXXXXXxx

#|
Function MAP-INTO (still writing also require the array stuff way below)

Syntax::

map-into result-sequence function &rest sequences => result-sequence

Arguments and Values::

result-sequence--a proper sequence.
function--a designator for a function of as many arguments as there are sequences.
sequence--a proper sequence.

Description::

Destructively modifies result-sequence to contain the results of applying function to each element in the argument sequences in turn. 

Examples::

 (setq a (list 1 2 3 4) b (list 10 10 10 10)) => (10 10 10 10)
 (map-into a #'+ a b) => (11 12 13 14)
 a => (11 12 13 14)
 b => (10 10 10 10)
 (setq k '(one two three)) => (ONE TWO THREE)
 (map-into a #'cons k a) => ((ONE . 11) (TWO . 12) (THREE . 13) 14)
 (map-into a #'gensym) => (#::G9090 #::G9091 #::G9092 #::G9093)
 a => (#::G9090 #::G9091 #::G9092 #::G9093)

(defun #>SYS::map-into (result-sequence function &rest sequences)
 "Destructively set elements of RESULT-SEQUENCE to the results
 of applying FUNCTION to respective elements of SEQUENCES."
 (clet ((arglist (make-list (length sequences)))
     (n (fif (listp result-sequence)
        most-positive-fixnum
        (array-dimension result-sequence 0))))
  ;; arglist is made into a list of pattern for each call
  ;; n is the length of the longest vector
  (pwhen sequences
   (csetf n (min n (SYS::loop for seq in sequencesminimize (length seq)))))
  ;; shadow-defun :SYS some shared functions::
  (clet
   ((*do-one-call* 
    #'(lambda (i)
     (ret (cdolist (seq sequences)
        (cdolist (arg arglist)
        (cdo (fif (listp (first seq))
           (csetf (first arg)
              (pop (first seq)))
           (csetf (first arg) 
              (aref (first seq) i))))))))
     (apply function arglist))
    (*do-result* 
     #'(lambda (i) 
     (ret (fif (cand (vectorp result-sequence)
         (array-has-fill-pointer-p result-sequence))
       (csetf (fill-pointer result-sequence) 
          (max i (fill-pointer result-sequence))))))))
   ;; (declare (inline *do-one-call*))
   ;; Decide if the result is a list or vector,
   ;; and SYS::loop through each element
   (fif (listp result-sequence)
     (SYS::loop for i from 0 to (- n 1)
        for r on result-sequence
        do (csetf (first r)
             (*do-one-call* i))
        finally (*do-result* i))
     (SYS::loop for i from 0 to (- n 1)
        do (csetf (aref result-sequence i)
             (*do-one-call* i))
        finally (*do-result* i))))
   result-sequence))
|##|

(defvar *complement-fns* (sl::make-hash-table 31) "defcomplement Hashtable to lookup how things like (complement #'member) might return")
(defun #>SYS::complement (fn)
 "If FN returns y, then (paip-complement FN) returns (not y)."
 (ret (pcond
   ((gethash fn *complement-fns*))
   (t #'(lambda (&rest pattern) (ret (cnot (apply fn pattern))))))))

;; example:: (defcomplement < >=)
(defmacro  #>SYS::defcomplement (posfn negfn)
  (ret `(sl::progn (sethash #',posfn *complement-fns* #',negfn) (sethash #',negfn *complement-fns* #',posfn))))


;; emits 
;;;;;;;;;;;;;;;;;   
(defun #>SYS::MEMBER (ITEM LIST &REST LKEYS) 
 (RET 
  (trace-progn  
  (CLET (TEST KEY TEST-NOT) 
    (init-keyval KEY (FUNCTION IDENTITY)) (init-keyval TEST (FUNCTION EQL)) 
    (pWHEN TEST-NOT (SETQ TEST (FUNCTION (LAMBDA (X Y) (RET (CNOT (FUNCALL TEST-NOT X Y)))))))
    (MEMBER ITEM LIST TEST KEY)))))


'(print (translate-block '
 (defun #>SYS::intersection (list-1 list-2 &key (test #'eql)(key #'identity) test-not)
   (pwhen test-not (setq test #'(lambda (x y)(not (funcall test-not x y)))))
   (intersection list-1 list-2 test key))))

(defun #>SYS::intersection (list-1 list-2 &rest lkeys)
 (trace-progn 'SYS::intersection (list-1 list-2 '&rest lkeys)
 (clet (test key test-not)
    (init-keyval test)(init-keyval key)(init-keyval test-not)   
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (ret (intersection list-1 list-2 test key )))))


(defun #>SYS::remove (item list &rest lkeys )
 ;;(force-print `(SYS::remove ,item ,list &rest ,lkeys))
 (clet (test from-end test-not start end count key)
    (init-keyval test #'eql)(init-keyval key #'identity)(init-keyval test-not)(init-keyval from-end)(init-keyval start 0)(init-keyval end)(init-keyval count)
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (pwhen from-end (ret (reverse (remove item (reverse list) test key start end count))))
    (ret (remove item list test key start end count))))

(defun #>SYS::remove-duplicates (list &rest lkeys)
 (trace-progn 'SYS::remove-duplicates (list '&rest lkeys) ())
 (clet (test from-end test-not start end count key)
    (init-keyval test)(init-keyval key)(init-keyval test-not)(init-keyval from-end)(init-keyval start)(init-keyval end)(init-keyval count)
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (funless start (csetq start 0))
    (pwhen from-end (ret (reverse (remove-duplicates (reverse list) test key start end))))
    (ret (remove-duplicates list test key start end))))

(defun #>SYS::delete-duplicates (list &rest lkeys)  
 (trace-progn 'SYS::delete-duplicates (list '&rest lkeys)
 (clet (test from-end test-not start end count key)
    (init-keyval test)(init-keyval key)(init-keyval test-not)(init-keyval from-end)(init-keyval start)(init-keyval end)(init-keyval count)
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (funless start (csetq start 0))
    (pwhen from-end (ret (reverse (delete-duplicates (reverse list) test key start end))))
    (ret (delete-duplicates list test key start end)))))

(defun #>SYS::subsetp (list list2 &rest lkeys)
 (trace-progn 'SYS::subsetp (list list2 '&rest lkeys)
 (clet (test key test-not)
    (init-keyval test)(init-keyval key)(init-keyval test-not)   
    (funless key (csetq key #'identity))
    (funless test (csetq test #'eql))
    (pwhen test-not (csetq test #'(lambda (x y)(ret (cnot (funcall test-not x y))))))
    (ret (subsetp list list2 test key)))))
|#

;;(USE-PACKAGE :SYS :CYC #'better-symbol)

(reshare-symbols)       
(cpushnew :CYC-COMMON-LISP sl::*features*)


(punless (null (find-symbol "DEFINE" :SYS)) (unintern (find-symbol "DEFINE") :SYS))
(punless (null (find-symbol "DEFINE" :CL)) (unintern (find-symbol "DEFINE") :CL))
(import 'SL::DEFINE :SYS)
(export 'SL::DEFINE :SYS)

