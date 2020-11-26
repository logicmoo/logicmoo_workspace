;;Saved into a file called common_lisp.lisp <?


;; ussually CYC
(defvar *cl-importing-package* *package*) 

;;(in-package "SUBLISP")
(defmacro prog1 (body1 &body body) (ret `(clet ((prog1res ,body1)) ,@body prog1res)))


(sl:defmacro defun (symbolp args sl:&body body) 
             (ret `(progn  
                     ;; (sl::export '(,symbolp))
                     (format t ";; ~A cl-defun \"~A\" ~S " ,(package-name  *package* )',symbolp ',args) (terpri)(force-output)

                     (sl::define ,symbolp ,args (ret (progn ,@body))))))

(sl:defmacro cl-defun (symbolp args sl:&body body) 
             (ret `(progn  
                     ;; (sl::export '(,symbolp))
                     (format t ";; ~A cl-defun \"~A\" ~S " ,(package-name  *package* )',symbolp ',args) (terpri)(force-output)
                     (sl::define ,symbolp ,args (ret (progn ,@body))))))

;;(sl::in-package "CL")
;;(sl::import '(defun defmacro) *cl-package*)
(defmacro cl-defmacro (symbolp args sl:&body body) 
  (ret `(progn  
          ;; (sl::export '(,symbolp))
          (format t ";; ~A defmacro-like-cl \"~A\" ~S " ,(package-name *package* )',symbolp ',args) (terpri)(force-output)
          ( sl::defmacro ,symbolp ,args (ret (progn ,@body))))))

;;(sl::export '(cl::defmacro-like-cl) *cl-package*)


(cl-defmacro memq (item my-list)
             `(member ,item ,my-list :test #'eq))

(defun cons-when (cond f) 
	(if (and cond f) (cons cond f ) nil))


(defun ele (num obj)
  (cond
    ((vectorp obj)(aref obj num))
    ((listp obj)(nth num obj))
    ((iterator-p obj)(ele num (ITERATOR-VALUE-LIST  (COPY-ITERATOR obj))))
    ((SET-P obj)(ele num (SET-ELEMENT-LIST obj)))
    ((SET-CONTENTS-P obj)(ele num (SET-CONTENTS-ELEMENT-LIST obj)))
    ))

#|
;; (cl-rewrite-function 'set-dispatch-macro-character)

(cl-defmacro psetq (&rest pairs)
             ;; not use reverse for build order consistency
             (do* ((pairs pairs (cddr pairs))
                   (tmp (gensym) (gensym))
                   (inits (list nil))
                   (inits-splice inits)
                   (setqs (list nil))
                   (setqs-splice setqs))
                  ((null pairs) (when (cdr inits)
                                  `(let ,(cdr inits)
                                     (setq ,@(cdr setqs))
                                     nil)))
               (setq inits-splice
                     (cdr (rplacd inits-splice (list (list tmp (cadr pairs)))))
                   setqs-splice
                     (cddr (rplacd setqs-splice (list (car pairs) tmp))))))


(cl-defmacro return (&optional result)
             `(return-from nil ,result))

(defun equal (x y)
  (cond
   ((eql x y) t)
   ((consp x) (and (consp y) (equal (car x) (car y)) (equal (cdr x) (cdr y))))
   ((stringp x) (and (stringp y) (string= x y)))
   ((bit-vector-p x) (and (bit-vector-p y) (= (length x) (length y))
                          (dotimes (i (length x) t)
                            (unless (eql (aref x i) (aref y i))
                              (return nil)))))
   ((pathnamep x) (and (pathnamep y)
                       (equal (pathname-host x) (pathname-host y))
                       (equal (pathname-device x) (pathname-device y))
                       (equal (pathname-directory x) (pathname-directory y))
                       (equal (pathname-name x) (pathname-name y))
                       (equal (pathname-type x) (pathname-type y))
                       (equal (pathname-version x) (pathname-version y))))
   (t nil)))
|#
#|
(defun identity (object)
  object)

(defun complement (function)
  #'(lambda (&rest arguments) (not (apply function arguments))))

(defun constantly (object)
  #'(lambda (&rest arguments)
      (declare (ignore arguments))
      object))

(cl-defmacro and (&rest forms)
             (cond
              ((null forms) t)
              ((null (cdr forms)) (car forms))
              (t `(when ,(car forms)
                    (and ,@(cdr forms))))))

(cl-defmacro or (&rest forms)
             (cond
              ((null forms) nil)
              ((null (cdr forms)) (car forms))
              (t (let ((tmp (gensym)))
                   `(let ((,tmp ,(car forms)))
                      (if ,tmp
                          ,tmp
                        (or ,@(cdr forms))))))))

(cl-defmacro cond (&rest clauses)
             (when clauses
               (let ((test1 (caar clauses))
                     (forms1 (cdar clauses)))
                 (if forms1
                     `(if ,test1
                          (progn ,@forms1)
                        (cond ,@(cdr clauses)))
                   (let ((tmp (gensym)))
                     `(let ((,tmp ,test1))
                        (if ,tmp
                            ,tmp
                          (cond ,@(cdr clauses)))))))))

(cl-defmacro when (test-form &rest forms)
             `(if ,test-form
                  (progn ,@forms)
                nil))

(cl-defmacro unless (test-form &rest forms)
             `(if ,test-form
                  nil
                (progn ,@forms)))

;;(defmacro block-to-tagname (bname) (ret `(gensym ',bname)))
(defmacro block-to-tagname (bname) (print (ret `',bname)))

(cl-defmacro case (keyform &rest clauses)(expand-case keyform clauses))

(cl-defmacro ccase (keyplace &rest clauses)
             (let* ((clauses (mapcar #'(lambda (clause)
                                         (let ((key (first clause))
                                               (forms (rest clause)))
                                           `(,(%list key) ,@forms)))
                               clauses))
                    (expected-type `(member ,@(apply #'append (mapcar #'car clauses))))
                    (block-name (gensym))
                    (tag (gensym)))
               `(block ,block-name
                  (tagbody
                    ,tag
                    (return-from ,block-name
                      (case ,keyplace
                        ,@clauses
                        (t (restart-case (error 'type-error :datum ,keyplace
                                           :expected-type ',expected-type)
                             (store-value (value)
                                          :report (lambda (stream)
                                                    (store-value-report stream ',keyplace))
                                          :interactive store-value-interactive
                                          (setf ,keyplace value)
                                          (go ,tag))))))))))


(cl-defmacro ecase (keyform &rest clauses)
             (let* ((clauses (mapcar #'(lambda (clause)
                                         (let ((key (first clause))
                                               (forms (rest clause)))
                                           `(,(%list key) ,@forms)))
                               clauses))
                    (expected-type `(member ,@(apply #'append (mapcar #'car clauses)))))
               `(case ,keyform
                  ,@clauses
                  (t (error 'type-error :datum ,keyform :expected-type ',expected-type)))))

(cl-defmacro typecase (keyform &rest clauses)
             (let* ((last (car (last clauses)))
                    (clauses (mapcar #'(lambda (clause)
                                         (let ((type (first clause))
                                               (forms (rest clause)))
                                           (if (and (eq clause last)
                                                    (member type '(otherwise t)))
                                               clause
                                             `((,type) ,@forms))))
                               clauses)))
               (expand-case keyform clauses :test #'typep)))

(cl-defmacro ctypecase (keyplace &rest clauses)
             (let ((expected-type `(or ,@(mapcar #'car clauses)))
                   (block-name (gensym))
                   (tag (gensym)))
               `(block ,block-name
                  (tagbody
                    ,tag
                    (return-from ,block-name
                      (typecase ,keyplace
                        ,@clauses
                        (t (restart-case (error 'type-error
                                           :datum ,keyplace
                                           :expected-type ',expected-type)
                             (store-value (value)
                                          :report (lambda (stream)
                                                    (store-value-report stream ',keyplace))
                                          :interactive store-value-interactive
                                          (setf ,keyplace value)
                                          (go ,tag))))))))))



(cl-defmacro etypecase (keyform &rest clauses)
             `(typecase ,keyform
                ,@clauses
                (t (error 'type-error
                     :datum ',keyform :expected-type '(or ,@(mapcar #'car clauses))))))
|#
#|
(cl-defmacro multiple-value-bind (vars values-form &body body)
             (cond
              ((null vars)
               `(progn ,@body))
              ((null (cdr vars))
               `(let ((,(car vars) ,values-form))
                  ,@body))
              (t
               (let ((rest (gensym)))
                 `(multiple-value-call #'(lambda (&optional ,@vars &rest ,rest)
                                           (declare (ignore ,rest))
                                           ,@body)
                    ,values-form)))))



(cl-defmacro multiple-value-list (form)
             `(multiple-value-call #'list ,form))


(cl-defmacro multiple-value-setq (vars form)
             `(values (setf (values ,@vars) ,form)))
;;  (let ((temps (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) vars)))
;;    `(multiple-value-bind ,temps ,form
;;       (setq ,@(mapcan #'(lambda (var temp) (list var temp)) vars temps))
;;       ,(car temps))))

(defun values-list (list)
  (check-type list proper-list)
  (apply #'values list))

(cl-defmacro nth-value (n form)
             `(nth ,n (multiple-value-list ,form)))

(define-setf-expander values (&rest places &environment env)
  (let (all-temps all-vars 1st-newvals rest-newvals all-setters all-getters)
    (dolist (place places)
      (multiple-value-bind (temps vars newvals setter getter)
          (get-setf-expansion place env)
        (setq all-temps    (cons temps all-temps)
            all-vars     (cons vars all-vars)
            1st-newvals  (cons (car newvals) 1st-newvals)
            rest-newvals (cons (cdr newvals) rest-newvals)
            all-setters  (cons setter all-setters)
            all-getters  (cons getter all-getters))))
    (values (apply #'append (reverse (append rest-newvals all-temps)))
            (append (apply #'append (reverse all-vars))
                    (make-list (reduce #'+ rest-newvals :key #'length)))
            (reverse 1st-newvals)
            `(values ,@(reverse all-setters))
            `(values ,@(reverse all-getters)))))
;;(define-setf-expander apply (function &rest args)
;;  (assert (and (listp function)
;;               (= (list-length function) 2)
;;               (eq (first function) 'function)
;;               (symbolp (second function))))
;;  (let ((function (cadr function))
;;        (newvals (list (gensym)))
;;        (temps (mapcar #'(lambda (arg) (gensym)) args)))
;;    (values temps
;;            args
;;            newvals
;;            `(apply #'(setf ,function) ,(car newvals) ,@vars)
;;            `(apply #',function ,@temps))))

(cl-defmacro prog (vars &body body)
             (flet ((declare-p (expr)
                               (and (consp expr) (eq (car expr) 'declare))))
               (do ((decls nil)
                    (forms body (cdr forms)))
                   ((not (declare-p (car forms))) `(block nil
                                                     (let ,vars
                                                       ,@(reverse decls)
                                                       (tagbody ,@forms))))
                 (push (car forms) decls))))

(cl-defmacro prog* (vars &body body)
             (multiple-value-bind (decls forms) (split-into-declarations-and-forms body)
               `(block nil
                  (let* ,vars
                    ,@(reverse decls)
                    (tagbody ,@forms)))))

(cl-defmacro prog1 (first-form &rest more-forms)
             (let ((result (gensym)))
               `(let ((,result ,first-form))
                  ,@more-forms
                  ,result)))

(cl-defmacro prog2 (first-form second-form &rest more-forms)
             `(prog1 (progn ,first-form ,second-form) ,@more-forms))


(cl-defmacro setf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (cond
                ((zerop nargs) nil)
                ((= nargs 2)
                 (let ((place (car pairs))
                       (value-form (cadr pairs)))
                   (cond
                    ((symbolp place)
                     `(setq ,place ,value-form))
                    ((consp place)
                     (if (eq (car place) 'the)
                         `(setf ,(caddr place) (the ,(cadr place) ,value-form))
                       (multiple-value-bind (temps vars newvals setter getter)
                           (get-setf-expansion place env)
                         (declare (ignore getter))
                         `(let (,@(mapcar #'list temps vars))
                            (multiple-value-bind ,newvals ,value-form
                              ,setter))))))))
                (t
                 (do* ((pairs pairs (cddr pairs))
                       (setfs (list 'progn))
                       (splice setfs))
                      ((endp pairs) setfs)
                   (setq splice (cdr (rplacd splice
                                             `((setf ,(car pairs) ,(cadr pairs)))))))))))

(cl-defmacro psetf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (if (< nargs 4)
                   `(progn (setf ,@pairs) nil)
                 (let ((setters nil))
                   (labels ((expand (pairs)
                                    (if pairs
                                        (multiple-value-bind (temps vars newvals setter getter)
                                            (get-setf-expansion (car pairs) env)
                                          (declare (ignore getter))
                                          (setq setters (cons setter setters))
                                          `(let (,@(mapcar #'list temps vars))
                                             (multiple-value-bind ,newvals ,(cadr pairs)
                                               ,(expand (cddr pairs)))))
                                      `(progn ,@setters nil))))
                     (expand pairs))))))

(cl-defmacro shiftf (&rest places-and-newvalue &environment env)
             (let ((nargs (length places-and-newvalue)))
               (assert (>= nargs 2))
               (let ((place (car places-and-newvalue)))
                 (multiple-value-bind (temps vars newvals setter getter)
                     (get-setf-expansion place env)
                   `(let (,@(mapcar #'list temps vars))
                      (multiple-value-prog1 ,getter
                        (multiple-value-bind ,newvals
                            ,(if (= nargs 2)
                                 (cadr places-and-newvalue)
                               `(shiftf ,@(cdr places-and-newvalue)))
                          ,setter)))))))

(cl-defmacro rotatef (&rest places &environment env)
             (if (< (length places) 2)
                 nil
               (multiple-value-bind (temps vars newvals setter getter)
                   (get-setf-expansion (car places) env)
                 `(let (,@(mapcar #'list temps vars))
                    (multiple-value-bind ,newvals (shiftf ,@(cdr places) ,getter)
                      ,setter)
                    nil))))
|#

(defvar *eval-mode* (list :load-toplevel :execute) )
(defmacro eval-when (when &body body) (ret `(if (intersection ',when *eval-mode*) (progn ,@body))))


;; transliterations
(defmacro let (&body body) (ret `( clet ,@body)))
(defmacro let* (&body body) (ret `( clet ,@body)))
(defmacro dotimes (&body body) (ret `(cdotimes ,@body)))
(defmacro case (&body body) (ret `( pcase ,@body)))
(defmacro if (&body body) (ret `(fif ,@body)))
(defmacro do (&body body) (ret `( cdo ,@body)))
(defmacro not (&body body) (ret `(cnot ,@body)))
(defmacro or (&body body) (ret `(cor ,@body)))
(defmacro cond (&body body) (ret `( pcond ,@body)))
(defmacro and (&body body) (ret `(cand ,@body)))
(defmacro unless (&body body) (ret `(funless ,@body)))
(defmacro when (&body body) (ret `(pwhen ,@body)))
(defmacro setq (&body body) (ret `( csetq ,@body)))
(defmacro setf (&body body) (ret `(csetf ,@body)))
(defmacro pushnew (item place) (ret `(progn (cpushnew ,item ,place) ,place)))
(defmacro push (&body body) (ret `(cpush ,@body)))
(defmacro pop (place) 
  (ret `(let ((f1rst (elt ,place 0))) (CPOP) f1rst)))
(defmacro concatenate (cltype &body args) (ret `(coerce (cconcatenate ,@args) ,cltype)))

;;(defmacro until (test &body body)"Repeatedly evaluate BODY until TEST is true."(ret `(do ()(,test) ,@body)))
(defmacro make-array (size &key initial-element ) (ret `(make-vector ,size  ,initial-element)))

(defmacro svref (array idx) (ret `(aref ,array ,idx)))
;;(defmacro incf (arg1 &body body) (ret `(fif (null body) (cincf arg1) (progn (cincf ,@body) ,@body)))
(defmacro incf (&body body) (ret `(cinc ,@body)))
(defmacro decf (&body body) (ret `(cdec ,@body)))

(defmacro unwind-protect (protected-form &body body) (ret `(cunwind-protect ,protected-form ,@body)))
(defmacro destructuring-bind (args datum &body body) (ret `(cdestructuring-bind ,args ,datum  ,@body)))
(defmacro multiple-value-bind (args datum &body body) (ret `(cmultiple-value-bind  ,args ,datum  ,@body)))
(defmacro cmultiple-value-list (value &rest ignore) (ret `(multiple-value-list ,value)))

(defmacro debug-print (&body stuff)
  (print stuff)(terpri)(force-output)
  (pcond
   ;; ((cdr stuff) (ret `(print (cons 'progn ,stuff))))
   ;;  ((consp stuff) (ret `(print (cons 'prog1 ,stuff)))) 
   (t (ret `(print (eval ',@stuff))))))

;;(defmacro concat (&rest body) (ret `(progn (mapcar #'(lambda (x) (if (not (stringp x)) (debug-print (cons 'concat ',body)))) ,body)(apply #'cconcatenate (cons "" ,body)))))
(define concat (&rest list) (ret (apply #'cconcatenate (cons "" (mapcar #'(lambda (x) (ret (if (stringp x) x (coerce x 'string) ))) list)))))


(defmacro catch (tag &body body)
  (ret 
   `(apply #'values  
           (let ((*thrown* :UNTHROWN) (*result* :UNEVALED))
             ;;(print (list 'eval (cons 'catch (cons ',tag  ',body))))(terpri)
             (ccatch ,tag *thrown* (setq *result* (multiple-value-list (progn ,@body))))
             (cond
              ((equal *result* :UNEVALED) (list *thrown*))
              (t *result*))))))

(define map-sequences (function sequences)
  (ret (fif (member () sequences) () (cons (apply function (mapcar #'car sequences)) (map-sequences function (mapcar #'cdr sequences))))))

(define map (result-type function &body sequences)
  (ret (fif result-type (coerce (map-sequences function sequences) result-type) (progn (map-sequences function sequences) nil))))

(define cl-make-string (&rest rest)
  (ret (make-string (find 'numberp rest #'funcall)(find #'characterp rest 'funcall))))

;;(define coerce (value result-type) (ret value))
;;are hashtables supposed ot be coercable back and forth from alists? 
(define coerce (value result-type)
  (clet ((len value)(vtype (type-of value))(cltype result-type))
        (pwhen (equal result-type vtype) (ret value))
        (unless (cand (consp cltype) (setq len (second cltype)) (setq cltype (car cltype)))
          (if (consp value) (setq len (length value))))
        ;;     (print (list 'coerce value result-type cltype len))
        (case cltype
          ('t (ret value))
          ('sequence
           (if (sequencep value) (ret (copy-seq value)) (setq value (write-to-string value)))
           (setq cltype (make-vector len))
           (do ((idx 0 (+ 1 idx))) ((= idx len) (ret  cltype )) (set-aref cltype idx (elt value idx))))
          ('character
           (cond
            ((characterp value) (ret value))
            ((numberp value) (ret (code-char value)))
            ((stringp value) (ret (char value 0)))
            (t (ret (char (coerce value 'string ) 0)))))
          ('number
           (cond
            ((numberp value) (ret value))
            ((characterp value) (ret (char-code value)))
            ((stringp value) (ret (string-to-number value)))
            ;; not like CL
            (t (ret (string-to-number (write-to-string value))))))
          ('integer
           (ret (round (coerce value 'number))))
          ('fixnum
           (ret (round (coerce value 'number))))
          ('float
           (ret (float (coerce value 'number))))
          ('real
           (ret (float (coerce value 'number))))
          ('flonum
           (ret (float (coerce value 'number))))
          ('string 
           (cond
            ((stringp value) (ret value))
            ((characterp value) (ret (make-string 1 value)))
            ((sequencep value) (setq cltype (make-string len))
             (do ((idx 0 (+ 1 idx))) ((= idx len) (ret cltype )) (set-aref cltype idx (coerce (elt value idx) 'character))))
            (t (ret (write-to-string value)))))
          ('list 
           (cond
            ((listp value) (ret list))
            ((sequencep value)
             (setq cltype nil)
             (do ((idx len (- idx 1))) ((= idx 0) (ret  cltype )) (setq cltype (cons (elt value idx) cltype))))
            (t 
             (setq cltype nil)
             (setq value (write-to-string value))
             (do ((idx len (- idx 1))) ((= idx 0) (ret  cltype )) (setq cltype (cons (elt value idx) cltype))))))
          ('cons 
           (cond
            ((listp value) (ret list))
            ((sequencep value)
             (setq cltype nil)
             (do ((idx len (- idx 1))) ((= idx 0) (ret  cltype )) (setq cltype (cons (elt value idx) cltype))))
            (t 
             (setq cltype nil)
             (setq value (write-to-string value))
             (do ((idx len (- idx 1))) ((= idx 0) (ret  cltype )) (setq cltype (cons (elt value idx) cltype))))))
          ;; not finished
          ('keypair
           (cond
            ((atom value) (ret list value))
            (t (ret (coerce value 'cons)))))
          ;; not finished
          ('alist
           ;;(if (hash-table-p value) (ret value))
           (setq cltype (setq cltype nil))
           (if (sequencep value) t (setq value (coerce value 'sequence)))
           (do ((idx 0 (+ 1 idx))) ((= idx len) (ret cltype)) 
             (setq result-type (coerce (elt value idx) 'cons))
             (setq cltype (acons (car result-type) (cdr result-type) cltype)))
           (ret cltype))
          ;; not finished
          ('hash-table
           (if (hash-table-p value) (ret value))
           (setq cltype (make-hash-table len))
           (if (sequencep value) t (setq value (coerce value 'sequence)))
           (do ((idx 0 (+ 1 idx))) ((= idx len) (ret cltype)) 
             (print (list 'coerce value result-type cltype len (elt value idx)))
             (setq result-type (coerce (elt value idx) 'keypair))
             (sethash (car result-type) cltype (cdr result-type))))
          ;; not like CL
          (otherwise (ret value)))
        (throw :coerce (list value result-type)))
  (ret value))





;;;;(load "sublisp-cl.lisp")
#|

(define FIND-ALL-SYMBOLS (stringp &optional (packagelist (list-all-packages)) (status '(:inherited :external :internal)))
  (ret (if packagelist
           (clet ((package (car packagelist))(res (multiple-values-list (find-symbol stringp package))))
                 (if  
                     (member (cdr res) status)
                     (cons (car res) (FIND-ALL-SYMBOLS stringp (cdr packagelist) status ))
                   (FIND-ALL-SYMBOLS stringp (cdr packagelist) status ))))))

(defun eval-remote (server &rest remote)  (print remote))

;; 
;;  (load "common_lisp.lisp")(macroexpand '(defstub :COMMON-LISP DEFPACKAGE))
(define defstub (pack symb &rest body)
  ;;  (clet ((symb `,symbn))
  (let ((sname (if (symbolp symb) (symbol-name symb) (if (stringp symb) symb "")))
        (fpack (if (packagep pack) pack (find-package pack)))
        (fsym  (if fpack (find-symbol sname fpack) (find-symbol sname))))
    (when (and(symbolp symb)(fboundp symb)) (ret `(symbol-function ',symb)))
    (when (and(symbolp fsym)(fboundp fsym)) (ret `(symbol-function ',fsym)))
    (when (and(symbolp fsym)(fboundp fsym)(member fpack *packages-local*)) (ret `(symbol-function ',fsym)))
    (unless (symbolp fsym)(setq fsym symb))
    (unless (symbolp fsym)(setq fsym (intern sname)))
    (unless fpack (setq fpack (symbol-package fsym)))
    (setq sname (concat (package-name fpack) "::" sname))
    (ret
     (print `(eval 
              ',(print (if body
                           ;;(list 'defmacro fsym (list 'quote (car body))(list 'ret (list 'BQ-LIST* (cons '(quote eval-remote) (cons (list 'quote sname) (cdr body))))))
                           `(defmacro ,fsym ,(car body) (ret `(eval-remote ,,sname ,,@(cdr body))))
                         
                         (list 'defmacro fsym '(&rest args)(list 'ret (list 'BQ-LIST* '(quote eval-remote) (list 'quote sname) 'args))))))))))


;;(define do-server4005 (in-stream out-stream)(print (read in-stream) out-stream))

(defstub :common-lisp 'defpackage)


;; We will show that only one of the three non-local exit mechanisms block/return-from, tagbody/go, catch/throw is required to be primitive, by showing how to emulate any two in terms of the third.[4] We first emulate block/return-from in terms of catch/throw. We map the block name into the name of a lexical variable which will hold the unique tag which distinguishes this dynamical block from any other. If trivial return-from's are optimized away, then this emulation can be quite efficient.
(cl-defmacro return-from-no (bname exp)
             "BLOCK/RETURN-FROM EMULATED BY CATCH/THROW"
             (let ((tagname (block-to-tagname bname)))
               `(throw ,tagname ,exp)))

(cl-defmacro block-no (bname &body forms)
             "BLOCK/RETURN-FROM EMULATED BY CATCH/THROW"
             (let ((tagname (block-to-tagname bname)))
               `(let ((,tagname (list nil))) ; Unique cons cell used as catch tag.
                  (catch ,tagname (progn ,@forms)))))

;; dont know if this is correct

(defmacro return (body) (ret `(ret ,body)))




(defconstant *unbound-value* (list nil))

(defun msymbol-value (var)
  (if (boundp var) (symbol-value var) *unbound-value*))

(defun mset (var val)
  (if (eq val *unbound-value*) (makunbound var) (set var val)))

(defmacro progv (syms vals &body forms)
  (let* ((vsyms (gensym)) (vvals (gensym)) (vovals (gensym)))
    `(let* ((,vsyms ,syms)
            (,vvals ,vals)
            (,vovals ,(mapcar #'msymbol-value ,vsyms)))
       (unwind-protect
           (progn (mapc #'mset ,vsyms ,vvals)
             (mapc #'makunbound (subseq ,vsyms (min (length ,vsyms) (length ,vvals))))
             ,@forms )
         (mapc #'mset ,vsyms ,vovals)))))

;;EMULATE "THE" USING "LET" AND "DECLARE"
;;The emulation of the the special form emphasizes the fact that there is a run-time type test which must be passed in order for the program to proceed. Of course, a clever compiler can eliminate the run-time test if it can prove that it will always succeed--e.g., the gcd function always returns an integer if it returns at all.

(defmacro the (typ exp)
  (if (and (consp typ) (eq (car typ) 'values))
      (let ((vals (gensym)))
        `(let ((,vals (multiple-value-list ,exp)))
           (assert (= (length ,vals) ,(length (cdr typ))))
           ,@(mapcar #'(lambda (typ i) `(assert (typep (elt ,vals ,i) ',typ)))
               (cdr typ) (iota-list (length (cdr typ))))
           (values-list ,vals)))
    (let ((val (gensym)))
      `(let ((,val ,exp))
         (assert (typep ,val ',typ))
         (let ((,val ,val)) (declare (type ,typ ,val))
           ,val)))))



(cl-defmacro go (label)
             "TAGBODY/GO EMULATED BY CATCH/THROW"
             (let ((name (label-to-functionname label)))
               `(throw ,name #',name)))

(cl-defmacro tagbody-no (&body body)
             "TAGBODY/GO EMULATED BY CATCH/THROW"
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
                                  (loop (setq nxt-label (catch ,go-tag (funcall nxt-label)))))))))))

(print "The emulation of tagbody/go by catch/throw is considerably less obvious than the emulation of block/return-from. 
This is because tagbody defines a number of different labels rather than a single block name, and because the parsing of the 
tagbody body is considerably more complicated. The various segments of the tagbody are emulated by a labels nest of mutually 
recursive functions, which are forced to all execute at the correct dynamic depth by means of a 
'trampoline. If the implementation implements the 'tail recursion' optimization for functions 
which have no arguments and return no values, and if the simpler cases of go's are optimized away, then this emulation can be quite efficient."
       )


(cl-defmacro labels (fns &body forms)
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
                      (progn ,@(mapcar #'(lambda (f nf) `(setq ,nf #',f))
                                 fnames nfnames))
                      ,@forms)))))

;;(* + - / /= < <= = > > >= ABS ACONS ACOS ADJOIN ALPHA-CHAR-P ALPHANUMERICP APPEND AREF ASH ASIN ASSOC ASSOC-IF ATAN ATOM 
;; BOOLE BOOLEAN BOTH-CASE-P BQ-CONS BQ-VECTOR BUTLAST BYTE CAAR CADR CAR CCONCATENATE CDAR CDDR CDR CEILING CERROR CHAR CHAR-CODE CHAR-DOWNCASE CHAR-EQUAL CHAR-GREATERP CHAR-LESSP CHAR-NOT-EQUAL CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-UPCASE CHAR/= CHAR< CHAR<= CHAR= CHAR> CHAR>= CHARACTERP CLRHASH 
;; CMERGE CODE-CHAR CONS CONSP CONSTANTP CONSTRUCT-FILENAME COPY-ALIST COPY-LIST COPY-SEQ COPY-TREE COS COUNT COUNT-IF CREDUCE CURRENT-PROCESS DATE-RELATIVE-GUID-P DECODE-FLOAT DECODE-UNIVERSAL-TIME DELETE DELETE-DUPLICATES DELETE-IF DIGIT-CHAR DIGIT-CHAR-P DISASSEMBLE-INTEGER-TO-FIXNUMS DPB EIGHTH ELT ENCODE-UNIVERSAL-TIME ENDP EQ EQL EQUAL EQUALP EVENP EXIT EXP EXPT FALSE FIFTH FILL FIND FIND-IF FIND-PACKAGE FIND-SYMBOL FIRST FIXNUMP FLOAT FLOAT-DIGITS FLOAT-RADIX FLOAT-SIGN FLOATP FLOOR FORCE-OUTPUT FORMAT FOURTH FRESH-LINE FUNCTION-SPEC-P FUNCTIONP GC GC-DYNAMIC GC-EPHEMERAL GC-FULL GENSYM GENTEMP GET GET-DECODED-TIME GET-INTERNAL-REAL-TIME GET-INTERNAL-REAL-TIME GET-INTERNAL-RUN-TIME GET-UNIVERSAL-TIME GET-UNIVERSAL-TIME GETF GETHASH GETHASH-WITHOUT-VALUES GUID-P GUID-STRING-P GUID-TO-STRING GUID/= GUID< GUID<= GUID= GUID> GUID>= HASH-TABLE-COUNT HASH-TABLE-P HASH-TABLE-SIZE HASH-TABLE-TEST IDENTITY IGNORE INFINITY-P INT/ INTEGER-DECODE-FLOAT INTEGER-LENGTH INTEGERP INTERN INTERRUPT-PROCESS INTERSECTION ISQRT KEYWORDP KILL-PROCESS LAST LDB LDIFF LENGTH LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION LIST LIST* LIST-ALL-PACKAGES LIST-LENGTH LISTP LISTP LOCK-IDLE-P LOCK-P LOG LOGAND LOGANDC1 LOGANDC2 LOGBITP LOGCOUNT LOGEQV LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2 LOGTEST LOGXOR LOWER-CASE-P MAKE-HASH-TABLE MAKE-LOCK MAKE-LOCK MAKE-STRING MAKUNBOUND MAX MEMBER MEMBER-IF MIN MINUSP MISMATCH MOD NBUTLAST NCONC NEW-GUID NINTERSECTION NINTH NOT-A-NUMBER-P NOTE-PERCENT-PROGRESS NOTIFY NRECONC NREVERSE NSET-DIFFERENCE NSET-EXCLUSIVE-OR NSTRING-CAPITALIZE NSTRING-DOWNCASE NSTRING-UPCASE NSUBLIS NSUBST NSUBST-IF NSUBSTITUTE NSUBSTITUTE-IF NTH NTHCDR NULL NUMBERP NUMBERP NUNION ODDP PAIRLIS PEEK-CHAR PLUSP POSITION POSITION-IF PRIN1 PRIN1-TO-STRING PRINC PRINC-TO-STRING PRINT PROCESS-ACTIVE-P PROCESS-BLOCK PROCESS-NAME PROCESS-STATE PROCESS-UNBLOCK PROCESS-WAIT PROCESS-WAIT-WITH-TIMEOUT PROCESS-WHOSTATE PROCESSP RANDOM RASSOC RASSOC-IF READ-FROM-STRING READ-FROM-STRING-IGNORING-ERRORS REM REMF REMHASH REMOVE REMOVE-DUPLICATES REMOVE-IF REPLACE REST REVAPPEND REVERSE REVERSE ROOM ROUND RPLACA RPLACD SCALE-FLOAT SEARCH SECOND SEED-RANDOM SEQUENCEP SET-AREF SET-CONSING-STATE SET-DIFFERENCE SET-NTH SEVENTH SHOW-PROCESSES SIN SIXTH QUIT SLEEP SORT SQRT STABLE-SORT STRING STRING-CAPITALIZE STRING-DOWNCASE STRING-EQUAL STRING-GREATERP STRING-LEFT-TRIM STRING-LESSP STRING-NOT-EQUAL STRING-NOT-GREATERP STRING-NOT-LESSP STRING-RIGHT-TRIM STRING-TO-GUID STRING-TRIM STRING-UPCASE STRING/= STRING< STRING<= STRING= STRING> STRING>= STRINGP SUBLIS SUBLISP::PROPERTY-LIST-MEMBER SUBSEQ SUBSETP SUBST SUBST-IF SUBSTITUTE SUBSTITUTE-IF SXHASH SYMBOL-FUNCTION SYMBOL-NAME SYMBOLP SYMBOLP TAILP TAN TENTH TERPRI THIRD TREE-EQUAL TRUE TRUNCATE TYPE-OF UNINTERN UNION UPPER-CASE-P VALID-PROCESS-P VALUES VECTOR VECTORP WARN WRITE-IMAGE Y-OR-N-P YES-OR-NO-P ZEROP)



(DEFMACRO HANDLER-CASE-CAD (FORM &REST CASES)
  (ret (LET ((NO-ERROR-CLAUSE (ASSOC ':NO-ERROR CASES)))
         (IF NO-ERROR-CLAUSE
             (LET ((NORMAL-RETURN (MAKE-SYMBOL "NORMAL-RETURN"))
                   (ERROR-RETURN  (MAKE-SYMBOL "ERROR-RETURN")))
               `(BLOCK ,ERROR-RETURN
                  (MULTIPLE-VALUE-CALL #'(LAMBDA ,@(CDR NO-ERROR-CLAUSE))
                    (BLOCK ,NORMAL-RETURN
                      (RETURN-FROM ,ERROR-RETURN
                        (HANDLER-CASE (RETURN-FROM ,NORMAL-RETURN ,FORM)
                          ,@(REMOVE NO-ERROR-CLAUSE CASES)))))))
           (LET ((TAG (GENSYM))
                 (VAR (GENSYM))
                 (ANNOTATED-CASES (MAPCAR #'(LAMBDA (CASE) (CONS (GENSYM) CASE))
                                    CASES)))
             `(BLOCK ,TAG
                (LET ((,VAR NIL))
                  ,VAR				;ignorable
                  (TAGBODY
                    (HANDLER-BIND ,(MAPCAR #'(LAMBDA (ANNOTATED-CASE)
                                               (LIST (CADR ANNOTATED-CASE)
                                                     `#'(LAMBDA (TEMP)
                                                          ,@(IF (CADDR ANNOTATED-CASE)
                                                                `((SETQ ,VAR TEMP)))
                                                          (GO ,(CAR ANNOTATED-CASE)))))
                                     ANNOTATED-CASES)
                      (RETURN-FROM ,TAG ,FORM))
                    ,@(MAPCAN #'(LAMBDA (ANNOTATED-CASE)
                                  (LIST (CAR ANNOTATED-CASE)
                                        (LET ((BODY (CDDDR ANNOTATED-CASE)))
                                          `(RETURN-FROM ,TAG
                                             ,(COND ((CADDR ANNOTATED-CASE)
                                                     `(LET ((,(CAADDR ANNOTATED-CASE)
                                                               ,VAR))
                                                        ,@BODY))
                                                    ((NOT (CDR BODY))
                                                     (CAR BODY))
                                                    (T
                                                     `(PROGN ,@BODY)))))))
                        ANNOTATED-CASES)))))))))
|#


(load "cycdcg.lisp")

