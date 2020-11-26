;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PTTP; Base: 10. -*-
;;; Copyright (c) 1986 Mark E. Stickel, SRI International, Menlo Park, CA 94025  USA
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(unless (find-package :pttp) (make-package "PTTP"))
;;(in-package (make-package "pttp"))

(defvar *PTTP-PACK* *PACKAGE*)
(pushnew :SYMBOLICS *FEATURES*)

#+SYMBOLICS (import 'user::stack-let)
#+SYMBOLICS (import 'user::stack-let*)

(eval-when (eval compile load)
  (setq *print-radix* nil))

#-SYMBOLICS
(eval-when (eval compile load)
  (defmacro stack-let (bindings &body body)
    ;; stack-let on the 3600 is like let except variable values are consed on the stack, not in the heap
    ;; thereby diminishing allocation/deallocation costs
    `(let ,bindings . ,body)))

(defvar float-internal-time-units-per-second (float internal-time-units-per-second))

(defvar and-connective '|,/2|)
(defvar or-connective  '\;/2)

;; dynamic variables used during compilation

(eval-when (eval compile load)
  (defvar name nil))				; name of procedure being compiled
(defvar arity)					; arity of procedure being compiled
(defvar clause-numbers)				; association list of clauses and their numbers
(defvar first-argument-type)			; the type of the first argument to the procedure
(defvar traceable)				; compile-procedure option to compile code for tracing
(defvar unbounded-search)			; compile-procedure option to not compile code for depth-bounded search
(defvar unsafe-unification)			; compile-procedure option to not use the occurs check during unification
(defvar incomplete-inference)			; compile-procedure option to compile ME reduction operations
(defvar allow-repeated-goals)			; compile-procedure option to compile ME pruning operations
(defvar trace-calls t)				; if nil, tracing will not be compiled regardless of :traceable option
(defvar count-calls t)				; compile code to count successful unifications?
(defvar recompile nil)				; recompile procedure even if clauses and parameters are the same
(defvar print-clauses t)			; print input clauses
(defvar print-compile-names t)			; print names of functions as they are compiled
(defvar print-compile-times t)			; print compilation time

;; when variable names are included in the variable,
;; variables are represented by (variable-level pointer-to-value . variable-name) for bound variables
;;                      and     (variable-level       nil        . variable-name) for unbound variables
;; 
;; when variable names are not included in the variable,
;; variables are represented by (variable-level . pointer-to-value) for bound variables
;;                      and     (variable-level .       nil       ) for unbound variables

;; this encoding assumes that integers will not be used as functors

;;(eval-when (eval compile load)
;;  (pushnew :include-name-in-variable *features*))

(defun new-variable (var-name var-level)
  #-include-name-in-variable (declare (ignore var-name))
  (list* var-level nil #+include-name-in-variable var-name))

(defmacro variable-p (x)
  ;; x nonatomic
  `(integerp (car ,x)))

(defmacro variable-level (x)
  `(car ,x))

(eval-when (eval compile load)
  (defmacro variable-value (x)
    #+include-name-in-variable `(cadr ,x)
    #-include-name-in-variable `(cdr ,x)))

(defmacro variable-name (x)
  #-include-name-in-variable (declare (ignore x))
  #+include-name-in-variable `(cddr ,x)
  #-include-name-in-variable `'_)

(defmacro dereference (x &key (if-constant t) (if-variable nil) (if-compound nil))
  ;; dereferences x leaving result in x, returns t if result is atomic, nil if not
  (user::assert (symbolp x))
  (let ((y (gensym)))
    `(do nil (nil)
       (cond ((atom ,x) (return ,if-constant))
	     ((variable-p ,x) (unless (let ((,y (variable-value ,x))) (when ,y (setq ,x ,y))) (return ,if-variable)))
	     (t (return ,if-compound))))))

(defvar *trail-array* (make-array 10000))

(defvar *trail* 0)

(defmacro trail-variable-p (var)
  `(< (variable-level ,var) !level!))

(defmacro bind-variable-to-term (var term trail?)
  ;; returns non-nil value (term is never nil)
  (user::assert (symbolp var))
  (user::assert (member trail? '(:trail :dont-trail :maybe-trail)))
  (cond ((eq trail? :dont-trail)
	 `(progn (setf (variable-value ,var) ,term)
		 t))
	((eq trail? :trail)
	 `(progn (setf (svref *trail-array* (incf *trail*)) ,var)
		 (setf (variable-value ,var) ,term)
		 t))
	(t `(progn (if (trail-variable-p ,var)
		       (setf (svref *trail-array* (incf *trail*)) ,var))
		   (setf (variable-value ,var) ,term)
		   t))))

(defmacro undo-bindings nil
  ;; MUST RETURN NIL
  `(let ((trail *trail*))
     (when (> trail !old-trail!)
       (do ((trail-array *trail-array*))
	   (nil)
	 (setf (variable-value (svref trail-array trail)) nil)
	 (when (= (decf trail) !old-trail!)
	   (setq *trail* !old-trail!)
	   (return nil))))))

(defmacro safe-bind-variable-to-compound (var term trail? undo?)
  ;; returns non-nil value if binding is successful,
  ;; undoes bindings on trail and returns nil if not successful
  (user::assert (symbolp term))
  `(if (variable-occurs-in-terms-p ,var (cdr ,term))
       ,(if undo? `(undo-bindings) nil)
       (bind-variable-to-term ,var ,term ,trail?)))

(defmacro pcall (term (vars unbound) level continuation)
  (stack-list-new-variables
    unbound
    (let* ((formals (head-locs (cdr term) 'a))
	   (actuals (mapcar #'(lambda (x) (term-constructor x vars)) (cdr term)))
	   (decls (mapcan #'(lambda (x y) (if (or (atom y) (eq (car y) 'quote)) nil (list (list x y)))) formals actuals))
	   (args (mapcar #'(lambda (x y) (if (or (atom y) (eq (car y) 'quote)) y x)) formals actuals)))
      (if decls
	  `(stack-let ,decls (,(car term) ,@args ,level ,continuation))
	  `(,(car term) ,@args ,level ,continuation)))))

(defvar *ncalls* 0)				; counter for number of inferences

(eval-when (eval compile load)
  (defun wrap-count-calls (form)
    (if (and count-calls (cnot (eq name 'query/0)))
	`(progn (incf *ncalls*) ,form)
	form)))

(eval-when (eval compile load)
  (defun head-locs (terms &optional (name 'arg) nth)
    (let* ((prop (if nth 'nth-arguments 'arguments))
	   (n (if (numberp terms) terms (length terms)))
	   (l (get name prop)))
      (or (cdr (assoc n l))
	  (let (w)
	    (dotimes (i n)
	      (push (if nth
			`(nth ,(1+ i) ,name)
			(intern (concatenate 'string "!" (symbol-name name) (princ-to-string (1+ i)) "!") *PTTP-PACK*))
		    w))
	    (setq w (nreverse w))
	    (setf (get name prop) (cons (cons n w) l))
	    w)))))

;; UNIFICATION

(defun ground-term-p (term)
  (dereference term
	       :if-constant  t
	       :if-variable  nil
	       :if-compound  (ground-terms-p (cdr term))))

(defun ground-terms-p (terms)
  (do ((l terms (cdr l)) (term))
      ((null l) t)
    (setq term (car l))
    (dereference term
		 :if-constant  nil
		 :if-variable  (return-from ground-terms-p nil)
		 :if-compound  (cond ((null (cdr l)) (setq l term))
				     ((cnot (ground-terms-p (cdr term)))
				      (return-from ground-terms-p nil))))))

(defun variable-occurs-in-term-p (var term)
  ;; works for both external (e.g., X) and internal (i.e., (level pointer-or-nil . name)) variable representations
  ;; i.e., we check (eq var term) in the constant as well as variable case
  ;; it would be slightly more efficient to handle only the internal representation when running Prolog programs
  (dereference term
	       :if-constant  (eq var term)
	       :if-variable  (eq var term)
	       :if-compound  (variable-occurs-in-terms-p var (cdr term))))

(defun variable-occurs-in-terms-p (var terms)
  ;; works for both external (e.g., X) and internal (i.e., (level pointer-or-nil . name)) variable representations
  ;; i.e., we check (eq var term) in the constant as well as variable case
  ;; it would be slightly more efficient to handle only the internal representation when running Prolog programs
  (do ((l terms (cdr l)) (term))
      ((null l) nil)
    (setq term (car l))
    (dereference term
		 :if-constant  (if (eq var term) (return-from variable-occurs-in-terms-p t))
		 :if-variable  (if (eq var term) (return-from variable-occurs-in-terms-p t))
		 :if-compound  (cond ((null (cdr l)) (setq l term))
				     ((variable-occurs-in-terms-p var (cdr term))
				      (return-from variable-occurs-in-terms-p t))))))

(defmacro unify-macro (unify-list-fun trail? safely?)
  ;; undoes bindings on trail and returns nil if not successful
  `(dereference t1
		:if-constant  (dereference t2
					   :if-constant  (or (eql t1 t2) (undo-bindings))
					   :if-variable  (bind-variable-to-term t2 t1 ,trail?)
					   :if-compound  (undo-bindings))
		:if-variable  (dereference t2
					   :if-constant  (bind-variable-to-term t1 t2 ,trail?)
					   :if-variable  (or (eq t1 t2)
							     (if (<= (variable-level t1) (variable-level t2))
								 (bind-variable-to-term t2 t1 ,trail?)
								 (bind-variable-to-term t1 t2 ,trail?)))
					   :if-compound  ,(if safely?
							      `(safe-bind-variable-to-compound t1 t2 ,trail? t)
							      `(bind-variable-to-term t1 t2 ,trail?)))
		:if-compound  (dereference t2
					   :if-constant  (undo-bindings)
					   :if-variable  ,(if safely?
							      `(safe-bind-variable-to-compound t2 t1 ,trail? t)
							      `(bind-variable-to-term t2 t1 ,trail?))
					   :if-compound  (or (eq t1 t2)
							     (if (eq (car t1) (car t2))
								 (let ((l1 (cdr t1)) (l2 (cdr t2)))
								   (unify-list-macro ,unify-list-fun ,trail? ,safely?))
								 (undo-bindings))))))

(defmacro unify-list-macro (unify-list-fun trail? safely?)
  ;; undoes bindings on trail and returns nil if not successful
  `(block unify-list
     (do ((t1) (t2))
	 ((null l1) (return t))
       (setq t1 (car l1)) (setq l1 (cdr l1))
       (setq t2 (car l2)) (setq l2 (cdr l2))
       (dereference t1
		    :if-constant  (dereference t2
					       :if-constant  (unless (eql t1 t2)
							       (return-from unify-list (undo-bindings)))
					       :if-variable  (bind-variable-to-term t2 t1 ,trail?)
					       :if-compound  (return-from unify-list (undo-bindings)))
		    :if-variable  (dereference t2
					       :if-constant  (bind-variable-to-term t1 t2 ,trail?)
					       :if-variable  (or (eq t1 t2)
								 (if (<= (variable-level t1) (variable-level t2))
								     (bind-variable-to-term t2 t1 ,trail?)
								     (bind-variable-to-term t1 t2 ,trail?)))
					       :if-compound  ,(if safely?
								  `(unless (safe-bind-variable-to-compound t1 t2 ,trail? t)
								     (return-from unify-list nil))
								  `(bind-variable-to-term t1 t2 ,trail?)))
		    :if-compound  (dereference t2
					       :if-constant  (return-from unify-list (undo-bindings))
					       :if-variable  ,(if safely?
								  `(unless (safe-bind-variable-to-compound t2 t1 ,trail? t)
								     (return-from unify-list nil))
								  `(bind-variable-to-term t2 t1 ,trail?))
					       :if-compound  (cond ((eq t1 t2))
								   ((cnot (eq (car t1) (car t2)))
								    (return-from unify-list (undo-bindings)))
								   ((null l1) (setq l1 (cdr t1)) (setq l2 (cdr t2)))
								   ((cnot ,(if (eq trail? :maybe-trail)
									      `(,unify-list-fun (cdr t1) (cdr t2) !old-trail! !level!)
									      `(,unify-list-fun (cdr t1) (cdr t2) !old-trail!)))
								    (return-from unify-list nil))))))))

(defun always-trails-unify (t1 t2 !old-trail!)
  (unify-macro always-trails-unify-list :trail t))

(defun always-trails-unify-list (l1 l2 !old-trail!)
  (unify-list-macro always-trails-unify-list :trail t))

(defun maybe-trails-unify (t1 t2 !old-trail! !level!)
  (unify-macro maybe-trails-unify-list :maybe-trail t))

(defun maybe-trails-unify-list (l1 l2 !old-trail! !level!)
  (unify-list-macro maybe-trails-unify-list :maybe-trail t))

(defun unsafe-always-trails-unify (t1 t2 !old-trail!)
  (unify-macro unsafe-always-trails-unify-list :trail nil))

(defun unsafe-always-trails-unify-list (l1 l2 !old-trail!)
  (unify-list-macro unsafe-always-trails-unify-list :trail nil))

(defun unsafe-maybe-trails-unify (t1 t2 !old-trail! !level!)
  (unify-macro unsafe-maybe-trails-unify-list :maybe-trail nil))

(defun unsafe-maybe-trails-unify-list (l1 l2 !old-trail! !level!)
  (unify-list-macro unsafe-maybe-trails-unify-list :maybe-trail nil))

(defmacro unify-argument-with-constant (actual formal &key trail-is-nil)
  (user::assert (or (atom formal) (eq (car formal) 'quote) (eq (car formal) 'nth)))
  `(let ((!temp! ,actual))
     (dereference !temp!
		  :if-constant  ,(if trail-is-nil `(eql !temp! ,formal) `(or (eql !temp! ,formal) (undo-bindings)))
		  :if-variable  (bind-variable-to-term !temp! ,formal :trail)
		  :if-compound  ,(if trail-is-nil nil `(undo-bindings)))))

(defmacro unify-argument-with-compound (actual formal &key trail-is-nil unsafe)
  (user::assert (symbolp formal))
  `(let ((!temp! ,actual))
     (dereference !temp!
		  :if-constant  ,(if trail-is-nil nil `(undo-bindings))
		  :if-variable  ,(if unsafe
				     `(bind-variable-to-term !temp! ,formal :trail)
				     `(safe-bind-variable-to-compound !temp! ,formal :trail ,(cnot trail-is-nil)))
		  :if-compound  (or (eq !temp! ,formal)
				    (if (eq (car !temp!) (car ,formal))
					,(if unsafe
					     `(unsafe-maybe-trails-unify-list (cdr !temp!) (cdr ,formal) !old-trail! !level!)
					     `(maybe-trails-unify-list (cdr !temp!) (cdr ,formal) !old-trail! !level!))
					,(if trail-is-nil nil `(undo-bindings)))))))

(defmacro identical-to-constant (term constant)
  (user::assert (symbolp constant))
  (if (symbolp term)
      `(dereference ,term
		    :if-constant  (eql ,term ,constant)
		    :if-variable  nil
		    :if-compound  nil)
      (let ((temp (gensym)))
	`(let ((,temp ,term))
	   (identical-to-constant ,temp ,constant)))))

(defmacro identical-to-variable (term variable)
  (user::assert (symbolp variable))
  (if (symbolp term)
      `(dereference ,term
		    :if-constant  nil
		    :if-variable  (eq ,term ,variable)
		    :if-compound  nil)
      (let ((temp (gensym)))
	`(let ((,temp ,term))
	   (identical-to-variable ,temp ,variable)))))

(defmacro identical-to-compound (term compound)
  (user::assert (symbolp compound))
  (if (symbolp term)
      `(dereference ,term
		    :if-constant  nil
		    :if-variable  nil
		    :if-compound  (or (eq ,term ,compound)
				      (and (eq (car ,term) (car ,compound))
					   (identical-list (cdr ,term) (cdr ,compound)))))
      (let ((temp (gensym)))
	`(let ((,temp ,term))
	   (identical-to-compound ,temp ,compound)))))

(defmacro identical (t1 t2)
  (if (symbolp t2)
      `(dereference ,t2
		    :if-constant  (identical-to-constant ,t1 ,t2)
		    :if-variable  (identical-to-variable ,t1 ,t2)
		    :if-compound  (identical-to-compound ,t1 ,t2))
      (let ((temp (gensym)))
	`(let ((,temp ,t2))
	   (identical ,t1 ,temp)))))

(defmacro identical-list (l1 l2)
  `(block identical-list
     (do ((l1 ,l1) (l2 ,l2) (t1) (t2))
	 ((null l1) (return t))
       (setq t1 (car l1)) (setq l1 (cdr l1))
       (setq t2 (car l2)) (setq l2 (cdr l2))
       (dereference t2
		    :if-constant  (unless (identical-to-constant t1 t2) (return-from identical-list nil))
		    :if-variable  (unless (identical-to-variable t1 t2) (return-from identical-list nil))
		    :if-compound  (dereference t1
					       :if-constant  (return-from identical-list nil)
					       :if-variable  (return-from identical-list nil)
					       :if-compound  (cond ((eq t1 t2))
								   ((cnot (eq (car t1) (car t2)))
								    (return-from identical-list nil))
								   ((null l1) (setq l1 (cdr t1)) (setq l2 (cdr t2)))
								   ((cnot (identical-list-fun (cdr t1) (cdr t2)))
								    (return-from identical-list nil))))))))

(defun identical-list-fun (l1 l2)
  (identical-list l1 l2))

;; SUPPORT FOR OUTPUT AND TRACING

(eval-when (eval compile load)
  (defmacro specifier (x)
    `(get ,x 'specifier))
  (defmacro precedence (x)
    `(get ,x 'precedence)))

(defvar *writing* t)				; use prefix, postfix, infix operators during printing

(defun function-case (x)
  (string-downcase x))

(defun constant-case (x)
  (string-downcase x))

(defun variable-case (x)
  (string-capitalize x))

(defun display-term (term)
  (let ((*writing* nil))
    (write-term term)))

(defun write-term (term)
  (dereference term)
  (cond ((atom term) 
           (princ (if (symbolp term) (constant-case term) term)))
	((variable-p term) 
	   (princ (variable-case (variable-name term))) (princ "_") (princ (variable-level term)))
	(t (write-functor-and-arguments (car term) (cdr term))))
  (force-output)
  term)

(defun parenthesize-argument (arg prec rel)
  (dereference arg
	       :if-constant  nil
	       :if-variable  nil
	       :if-compound  (let ((argprec (precedence (car arg))))
			       (and argprec (funcall rel prec argprec)))))

(defun write-functor-and-arguments (fn args &aux spec prec)
  (cond ((eq fn 'cons/2)
	 (princ "[")
	 (write-term (car args))
	 (do ((x (cadr args) (caddr x)))
	     (nil)
	   (dereference x)
	   (cond ((eq x '|[]|) (princ "]") (return))
		 ((and (cnot (atom x)) (eq (car x) 'cons/2)) (princ ",") (write-term (cadr x)))
		 (t (princ "|") (write-term x) (princ "]") (return)))))
	((and *writing* (setq spec (specifier fn)))
	 (setq prec (precedence fn))
	 (case spec
	   ((fx fy)
	    (princ (function-case (functor-name fn)))
	    (princ " ")
	    (cond ((parenthesize-argument (car args) prec (if (eq spec 'fx) #'<= #'<))
		   (princ "(") (write-term (car args)) (princ ")"))
		  (t (write-term (car args)))))
	   ((xf yf)
	    (cond ((parenthesize-argument (car args) prec (if (eq spec 'fx) #'<= #'<))
		   (princ "(") (write-term (car args)) (princ ")"))
		  (t (write-term (car args))))
	    (princ " ")
	    (princ (function-case (functor-name fn))))
	   ((xfx xfy yfx yfy)
	    (cond ((parenthesize-argument (car args) prec (if (member spec '(xfx xfy)) #'<= #'<))
		   (princ "(") (write-term (car args)) (princ ")"))
		  (t (write-term (car args))))
	    (princ " ")
	    (princ (function-case (functor-name fn)))
	    (princ " ")
	    (cond ((parenthesize-argument (cadr args) prec (if (member spec '(xfx yfx)) #'<= #'<))
		   (princ "(") (write-term (cadr args)) (princ ")"))
		  (t (write-term (cadr args)))))))
	(t (princ (function-case (functor-name fn)))
	   (when args
	     (princ "(")
	     (cond ((parenthesize-argument (car args) 0 #'<)
		    (princ "(") (write-term (car args)) (princ ")"))
		   (t (write-term (car args))))
	     (dolist (term (cdr args))
	       (princ ",")
	       (cond ((parenthesize-argument term 0 #'<)
		      (princ "(") (write-term term) (princ ")"))
		     (t (write-term term))))
	     (princ ")"))))
  nil)

(defun write-functor-and-arguments* (fn &rest args)
  (write-functor-and-arguments fn args))

(defun write-clause (clause &optional number variables)
  (let ((goalp (and (eq (car clause) '<-/2) (equal (cadr clause) '(query/0)))))
  (fresh-line)
  (when goalp (princ " ----------------") (terpri))
  (when number
    (if (consp number)
	(format t "~3D~A. " (car number) (cdr number))
	(format t "~3D.  " number)))
  (if goalp
      (let (quantified)
	(write-term (cadr clause))
	(princ " <- ")
	(dolist (v variables)
	  (when (variable-occurs-in-term-p v (caddr clause))
	    (princ "(") (write-term v) (princ ")") (setq quantified t)))
	(when quantified (princ " "))
	(write-term (caddr clause)))
      (let (quantified)
	(dolist (v variables)
	  (when (variable-occurs-in-term-p v clause)
	    (princ "(") (write-term v) (princ ")") (setq quantified t)))
	(when quantified (princ " "))
	(write-term clause)))
  (princ ".")))

(defvar *tracing* nil)
(defvar *spy-points* nil)

(defmacro trace-call-fail (nm form)
  (let ((args (head-locs (get nm 'arity))))
    `(progn
       (when (or *tracing* (member ',nm *spy-points*))
	 (format t "~&~VT~4D CALL " (+ !level! !level! -1) !level!) (write-functor-and-arguments* ',nm . ,args))
       ,form
       (when (or *tracing* (member ',nm *spy-points*))
	 (format t "~&~VT~4D FAIL " (+ !level! !level! -1) !level!) (write-functor-and-arguments* ',nm . ,args)))))

(defmacro trace-exit-redo (nm form)
  (let ((args (head-locs (get nm 'arity))))
    `(progn (when (or *tracing* (member ',nm *spy-points*))
	      (format t "~&~VT~4D EXIT " (+ !level! !level! -1) !level!) (write-functor-and-arguments* ',nm . ,args))
	    ,form
	    (when (or *tracing* (member ',nm *spy-points*))
	      (format t "~&~VT~4D REDO " (+ !level! !level! -1) !level!) (write-functor-and-arguments* ',nm . ,args)))))

(defun wrap-call-fail-trace (form)
  (if traceable `(trace-call-fail ,name ,form) form))

(eval-when (eval compile load)
  (defun wrap-exit-redo-trace (form)
    (if traceable `(trace-exit-redo ,name ,form) form)))

;; CONSECUTIVELY BOUNDED DEPTH-FIRST SEARCH

(defvar *remaining-depth* 1000000)		; effectively infinite values so that depth-bounded code
(defvar *prev-depth-increment* 1000001)		; will run outside of search calls
(defvar *minus-next-depth-increment* -1000)

(defvar *old-remaining-depths* nil)
(defvar *old-prev-depth-increments* nil)
(defvar *old-minus-next-depth-increments* nil)

;; search is a Prolog predicate which takes
;; goal argument so that the user does not have to insert
;; end-search calls between the goals and additional
;; code to do something with, e.g., print, each solution

(defvar *trace-search* t)			; print a message when starting search on each level, etc.
(defvar *trace-search-calls* t)			; include number of inferences in the message
(defvar *trace-search-time*)			; time spent printing search messages to be excluded from execution time

(defmacro trace-search (&rest args)
  `(when *trace-search*
     (let ((start-time (get-internal-run-time)))
       (fresh-line)
       (when *trace-search-calls*
	 (format t "~11:D inferences so far.   " *ncalls*))
       (format t ,@args)
       (incf *trace-search-time* (- (get-internal-run-time) start-time)))))

(defun begin-search (maximum-depth minimum-depth default-depth-increment !level! !continuation!)
  (stack-let ((*old-remaining-depths* (cons *remaining-depth* *old-remaining-depths*))
	      (*old-prev-depth-increments* (cons *prev-depth-increment* *old-prev-depth-increments*))
	      (*old-minus-next-depth-increments* (cons *minus-next-depth-increment* *old-minus-next-depth-increments*)))
    (let (*remaining-depth* *prev-depth-increment* *minus-next-depth-increment* cut)
      (dereference maximum-depth) (if (null maximum-depth) (setq maximum-depth 1000000))
      (dereference minimum-depth) (if (null minimum-depth) (setq minimum-depth 0))
      (dereference default-depth-increment) (if (null default-depth-increment) (setq default-depth-increment 1))
      (setq *remaining-depth* minimum-depth)
      (setq *prev-depth-increment* (1+ minimum-depth))
      (do nil (nil)
	(when (> *remaining-depth* maximum-depth) (trace-search "Search ended, maximum depth reached. ") (return nil))
	(trace-search "Start searching with ~:[no subgoals~;at most ~2D subgoal~:P~]. " (> *remaining-depth* 0) *remaining-depth*)
	(setq *minus-next-depth-increment* -1000)
	(setq cut t)
	(unwind-protect
	    (progn (funcall !continuation! !level!) (setq cut nil))
	  (when cut (trace-search "Search ended by cut. ")))
	(let ((next-depth-increment (- *minus-next-depth-increment*)))
	  (when (= next-depth-increment 1000) (trace-search "Search ended, no more inferences possible. ") (return nil))
	  (setq next-depth-increment (max next-depth-increment default-depth-increment))
	  (incf *remaining-depth* next-depth-increment)
	  (setq *prev-depth-increment* next-depth-increment))))))

(defmacro end-search (form)
  ;; executes form only for solutions that were not discovered in a previous search with lower depth bound
  `(if (< *remaining-depth* *prev-depth-increment*)
       (let* ((*remaining-depth* (car *old-remaining-depths*))
	      (*prev-depth-increment* (car *old-prev-depth-increments*))
	      (*minus-next-depth-increment* (car *old-minus-next-depth-increments*))
	      (*old-remaining-depths* (cdr *old-remaining-depths*))
	      (*old-prev-depth-increments* (cdr *old-prev-depth-increments*))
	      (*old-minus-next-depth-increments* (cdr *old-minus-next-depth-increments*)))
	 ,form)))

(defmacro with-n-subgoals (n form)
  `(let ((*remaining-depth* (- *remaining-depth* ,n)))
     (cond ((minusp *remaining-depth*)
	    (if (> *remaining-depth* *minus-next-depth-increment*) (setq *minus-next-depth-increment* *remaining-depth*))
	    nil)
	   (t ,form))))

(defun wrap-depth-test (form clause-body)
  (if unbounded-search
      form
      (let ((n (clause-body-length clause-body T)))
	(if (> n 0)
	    `(with-n-subgoals ,n ,form)
	    form))))

(defun not-solvable (!arg1! !arg2! !level! !continuation! &aux (!old-trail! *trail*))
  ;; !arg2! specifies depth of search
  (incf !level!)
  (trace-call-fail
    not/2
    (dereference !arg1!
		 :if-variable  (error "NOT was given non-compound argument ~A" !arg1!)
		 :if-constant  (error "NOT was given non-compound argument ~A" !arg1!)
		 :if-compound  (if (ground-term-p !arg1!)
				   (let ((*trace-search* (and *tracing* *trace-search*)))
				     (begin-search !arg2! !arg2! nil !level!
						   #'(lambda (lev)
						       ;;#+(OR SYMBOLICS TI) (declare (sys:downward-function))
						       (apply (car !arg1!)
							      (append (cdr !arg1!)	; inefficient
								      (list lev
									    #'(lambda (lev)
										;;#+(OR SYMBOLICS TI) (declare (sys:downward-function))
										(declare (ignore lev))
										(undo-bindings)
										(return-from not-solvable nil)))))))
				     (trace-exit-redo not/2 (funcall !continuation! !level!)))
				   (error "NOT was given non-ground argument ~A" !arg1!)))))

;; MODEL-ELIMINATION REDUCTION RULE

(defun ancestors-name (nm)
  (or (get nm 'ancestors)
      (let ((w (intern (concatenate 'string "*" (symbol-name nm) "-ANCESTORS*") *PTTP-PACK*)))
	(setf (get nm 'ancestors) w)
	w)))

(defun wrap-push-ancestor (form)
  (if (or (not allow-repeated-goals) (not incomplete-inference))
      (let ((nname (ancestors-name name)))
	(if (= arity 0)
	    `(let ((,nname t))
	       ,form)
	    `(stack-let ((,nname (cons ,(cond ((= arity 0) t)
					      ((= arity 1) '!arg1!)
					      (t '!args!))
				       ,nname)))
	       ,form)))
      form))

(defun wrap-pop-ancestor (form)
  (if (or (not allow-repeated-goals) (not incomplete-inference))
      (let ((nname (ancestors-name name)))
	(if (= arity 0)
	    `(let ((,nname nil))
	       ,form)
	    `(let ((,nname (cdr ,nname)))
	       ,form)))
      form))

(defmacro reduce-by-ancestor (arity type)
  (let ((count-calls t) (traceable nil))	; must recompile this and its calls to change counting and tracing
    `(dolist (!ancestor! !ancestors!)
       (when ,(cond ((eq type :constant-first-argument)
		     (if (= arity 1)
			 `(unify-argument-with-constant !ancestor! !arg1! :trail-is-nil t)
			 `(and (unify-argument-with-constant (car !ancestor!) !arg1! :trail-is-nil t)
			       ,(if (= arity 2)
				    `(always-trails-unify-list (cdr !ancestor!) (cdr !args!) !old-trail!)
				    `(always-trails-unify-list (cdr !ancestor!) (cdr !args!) !old-trail!)))))
		    ((eq type :variable-first-argument)
		     (if (= arity 1)
			 `(always-trails-unify !ancestor! !arg1! !old-trail!)
			 `(always-trails-unify-list !ancestor! !args! !old-trail!)))
		    ((eq type :compound-first-argument)
		     (if (= arity 1)
			 `(unify-argument-with-compound !ancestor! !arg1! :trail-is-nil t)
			 `(and (unify-argument-with-compound (car !ancestor!) !arg1! :trail-is-nil t)
			       ,(if (= arity 2)
				    `(always-trails-unify-list (cdr !ancestor!) (cdr !args!) !old-trail!)
				    `(always-trails-unify-list (cdr !ancestor!) (cdr !args!) !old-trail!)))))
		    (t (error "Unrecognized first argument type ~A" type)))
	 ,(wrap-count-calls (wrap-exit-redo-trace `(funcall !continuation! !level!)))
	 (if (= *trail* !old-trail!)
	     (return t)
	     (undo-bindings))))))

(defun reduce-by-ancestor-for-constant-first-argument/1 (!ancestors! !arg1! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 1 :constant-first-argument))

(defun reduce-by-ancestor-for-variable-first-argument/1 (!ancestors! !arg1! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 1 :variable-first-argument))

(defun reduce-by-ancestor-for-compound-first-argument/1 (!ancestors! !arg1! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 1 :compound-first-argument))

(defun reduce-by-ancestor-for-constant-first-argument/2 (!ancestors! !arg1! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 2 :constant-first-argument))

(defun reduce-by-ancestor-for-variable-first-argument/2 (!ancestors! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 2 :variable-first-argument))

(defun reduce-by-ancestor-for-compound-first-argument/2 (!ancestors! !arg1! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 2 :compound-first-argument))

(defun reduce-by-ancestor-for-constant-first-argument/3 (!ancestors! !arg1! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 3 :constant-first-argument))

(defun reduce-by-ancestor-for-variable-first-argument/3 (!ancestors! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 3 :variable-first-argument))

(defun reduce-by-ancestor-for-compound-first-argument/3 (!ancestors! !arg1! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 3 :compound-first-argument))

(defun reduce-by-ancestor-for-constant-first-argument/4+ (!ancestors! !arg1! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 4 :constant-first-argument))

(defun reduce-by-ancestor-for-variable-first-argument/4+ (!ancestors! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 4 :variable-first-argument))

(defun reduce-by-ancestor-for-compound-first-argument/4+ (!ancestors! !arg1! !args! !old-trail! !level! !continuation!)
  (reduce-by-ancestor 4 :compound-first-argument))

(defun reduce-by-ancestor-call-fun (arity type)
  (case arity
    (1 (case type
	 (:constant-first-argument  'reduce-by-ancestor-for-constant-first-argument/1)
	 (:variable-first-argument  'reduce-by-ancestor-for-variable-first-argument/1)
	 (:compound-first-argument 'reduce-by-ancestor-for-compound-first-argument/1)))
    (2 (case type
	 (:constant-first-argument  'reduce-by-ancestor-for-constant-first-argument/2)
	 (:variable-first-argument  'reduce-by-ancestor-for-variable-first-argument/2)
	 (:compound-first-argument 'reduce-by-ancestor-for-compound-first-argument/2)))
    (3 (case type
	 (:constant-first-argument  'reduce-by-ancestor-for-constant-first-argument/3)
	 (:variable-first-argument  'reduce-by-ancestor-for-variable-first-argument/3)
	 (:compound-first-argument 'reduce-by-ancestor-for-compound-first-argument/3)))
    (t (case type
	 (:constant-first-argument  'reduce-by-ancestor-for-constant-first-argument/4+)
	 (:variable-first-argument  'reduce-by-ancestor-for-variable-first-argument/4+)
	 (:compound-first-argument 'reduce-by-ancestor-for-compound-first-argument/4+)))))

(defun reduce-by-ancestor-call (name arity type)
  (let ((ancestors (ancestors-name (negated-functor name))))
    (if (= arity 0)
	`(when ,ancestors
	   ,(wrap-count-calls (wrap-exit-redo-trace `(progn (funcall !continuation! !level!) (return-from ,name nil)))))
	`(when ,ancestors
	   (,(reduce-by-ancestor-call-fun arity type)
	    ,ancestors
	    ,@(cond ((= arity 0) nil)
		    ((= arity 1) `(!arg1!))
		    (t (if (eq type :variable-first-argument) `(!args!) `(!arg1! !args!))))
	    !old-trail!
	    !level!
	    !continuation!)))))

;; MODEL ELIMINATION PRUNING

(defmacro identical-to-ancestor (arity type)
  (let ((temp (cond ((= arity 2) `(identical (cadr !ancestor!) !arg2!))
		    ((= arity 3) `(and (identical (cadr !ancestor!) !arg2!)
				       (identical (caddr !ancestor!) !arg3!)))
		    ((>= arity 4) `(identical-list (cdr !ancestor!) (cdr !args!))))))
    `(dolist (!ancestor! !ancestors!)
       (when ,(cond ((eq type :constant-first-argument)
		     (if (= arity 1)
			 `(identical-to-constant !ancestor! !arg1!)
			 `(and (identical-to-constant (car !ancestor!) !arg1!) ,temp)))
		    ((eq type :variable-first-argument)
		     (if (= arity 1)
			 `(identical-to-variable !ancestor! !arg1!)			     
			 `(and (identical-to-variable (car !ancestor!) !arg1!) ,temp)))
		    ((eq type :compound-first-argument)
		     (if (= arity 1)
			 `(identical-to-compound !ancestor! !arg1!)
			 `(and (identical-to-compound (car !ancestor!) !arg1!) ,temp)))
		    (t (error "Unrecognized first argument type ~A" type)))
	 (return t)))))

(defun identical-to-ancestor-for-constant-first-argument/1 (!ancestors! !arg1!)
  (identical-to-ancestor 1 :constant-first-argument))

(defun identical-to-ancestor-for-variable-first-argument/1 (!ancestors! !arg1!)
  (identical-to-ancestor 1 :variable-first-argument))

(defun identical-to-ancestor-for-compound-first-argument/1 (!ancestors! !arg1!)
  (identical-to-ancestor 1 :compound-first-argument))

(defun identical-to-ancestor-for-constant-first-argument/2 (!ancestors! !arg1! !arg2!)
  (identical-to-ancestor 2 :constant-first-argument))

(defun identical-to-ancestor-for-variable-first-argument/2 (!ancestors! !arg1! !arg2!)
  (identical-to-ancestor 2 :variable-first-argument))

(defun identical-to-ancestor-for-compound-first-argument/2 (!ancestors! !arg1! !arg2!)
  (identical-to-ancestor 2 :compound-first-argument))

(defun identical-to-ancestor-for-constant-first-argument/3 (!ancestors! !arg1! !arg2! !arg3!)
  (identical-to-ancestor 3 :constant-first-argument))

(defun identical-to-ancestor-for-variable-first-argument/3 (!ancestors! !arg1! !arg2! !arg3!)
  (identical-to-ancestor 3 :variable-first-argument))

(defun identical-to-ancestor-for-compound-first-argument/3 (!ancestors! !arg1! !arg2! !arg3!)
  (identical-to-ancestor 3 :compound-first-argument))

(defun identical-to-ancestor-for-constant-first-argument/4+ (!ancestors! !arg1! !args!)
  (identical-to-ancestor 4 :constant-first-argument))

(defun identical-to-ancestor-for-variable-first-argument/4+ (!ancestors! !arg1! !args!)
  (identical-to-ancestor 4 :variable-first-argument))

(defun identical-to-ancestor-for-compound-first-argument/4+ (!ancestors! !arg1! !args!)
  (identical-to-ancestor 4 :compound-first-argument))

(defun identical-to-ancestor-call-fun (arity type)
  (case arity
    (1 (case type
	 (:constant-first-argument  'identical-to-ancestor-for-constant-first-argument/1)
	 (:variable-first-argument  'identical-to-ancestor-for-variable-first-argument/1)
	 (:compound-first-argument 'identical-to-ancestor-for-compound-first-argument/1)))
    (2 (case type
	 (:constant-first-argument  'identical-to-ancestor-for-constant-first-argument/2)
	 (:variable-first-argument  'identical-to-ancestor-for-variable-first-argument/2)
	 (:compound-first-argument 'identical-to-ancestor-for-compound-first-argument/2)))
    (3 (case type
	 (:constant-first-argument  'identical-to-ancestor-for-constant-first-argument/3)
	 (:variable-first-argument  'identical-to-ancestor-for-variable-first-argument/3)
	 (:compound-first-argument 'identical-to-ancestor-for-compound-first-argument/3)))
    (t (case type
	 (:constant-first-argument  'identical-to-ancestor-for-constant-first-argument/4+)
	 (:variable-first-argument  'identical-to-ancestor-for-variable-first-argument/4+)
	 (:compound-first-argument 'identical-to-ancestor-for-compound-first-argument/4+)))))

(defun identical-to-ancestor-call (name arity type)
  (let ((ancestors (ancestors-name name)))
    (if (= arity 0)
	ancestors
	`(when ,ancestors
	   (,(identical-to-ancestor-call-fun arity type)
	    ,ancestors
	    ,@(cond ((= arity 0) nil)
		    ((= arity 1) `(!arg1!))
		    ((= arity 2) `(!arg1! !arg2!))
		    ((= arity 3) `(!arg1! !arg2! !arg3!))
		    (t `(!arg1! !args!))))))))

;; COMPILER

(defun clause-head (clause)
  (cond
    ((or (eq (car clause) '<-/2) (eq (car clause) '<-))
     (cadr clause))
    ((or (eq (car clause) '->/2) (eq (car clause) '->))
     (caddr clause))
    (t
     clause)))

(defun clause-pred (clause)
  (car (clause-head clause)))

(defun clause-args (clause)
  (cdr (clause-head clause)))

(defun clause-body (clause)
  (cond
    ((or (eq (car clause) '<-/2) (eq (car clause) '<-))
     (caddr clause))
    ((or (eq (car clause) '->/2) (eq (car clause) '->))
     (cadr clause))
    (t
     '(true/0))))

(defun replace-variable-in-term (var value term)
  (cond ((eq var term) value)
	((atom term) term)
	(t (let ((z (replace-variable-in-terms var value (cdr term))))
	     (if (eq z (cdr term)) term (cons (car term) z))))))

(defun replace-variable-in-terms (var value terms)
  (cond ((null terms) nil)
	(t (let ((x (replace-variable-in-term var value (car terms)))
		 (y (replace-variable-in-terms var value (cdr terms))))
	     (if (and (eq x (car terms)) (eq y (cdr terms))) terms (cons x y))))))			      

(defun term-constructor (term vars)
  (cond ((atom term)
	 (cond ((eq term '_) `(list* !level! nil #+include-name-in-variable '_))	; i.e., (new-variable '_ !level!)
	       ((member term vars) term)
	       (t (list 'quote term))))
	(t (let ((args (mapcar #'(lambda (x) (term-constructor x vars)) (cdr term))))
	     (cond ((every #'(lambda (x) (and (not (atom x)) (eq (car x) 'quote))) args) (list 'quote term))
		   (t (list* 'list (list 'quote (car term)) args)))))))

(defun term-constructors (terms vars &aux newterms)
  (setq newterms (mapcar #'(lambda (term) (term-constructor term vars)) terms))
  (cond ((every #'(lambda (x) (and (not (atom x)) (eq (car x) 'quote))) newterms) (list 'quote terms))
	(t (cons 'list newterms))))

(defun wrap-progn (forms)
  (cond ((null forms) nil)
	((null (cdr forms)) (car forms))
	(t (cons 'progn forms))))

(defun wrap-bind-args (form)
  (if (and (or (not allow-repeated-goals) (not incomplete-inference)) (> arity 1))
      `(stack-let ((!args! (list . ,(head-locs arity))))
	 ,form)
      form))

(defvar trail-is-nil)

(defun wrap-undo-bindings (form)
  (if trail-is-nil
      form
      `(progn ,form (undo-bindings))))

(defun stack-list-new-variables (variables form)
  (cond ((null variables) form)
	(t (list 'stack-let
		 (mapcar
		   #'(lambda (v)
		       (list v `(list* !level! nil #+include-name-in-variable ',v) ))	; i.e., `(new-variable ',v !level!)
		   variables)
		 form))))

(eval-when (eval compile load)
  (defmacro invisible-functor-p (x)
    `(get ,x 'invisible-functor)))

(defun clause-body-length (body &optional fl)
  (cond ((atom body) 0)
	((eq (car body) 'true/0) 0)
	((eq (car body) 'fail/0) 0)
	((eq (car body) 'false/0) 0)
	((eq (car body) 'nsubgoals/1) (if fl (cadr body) 0))
	((eq (car body) and-connective) (+ (clause-body-length (cadr body) fl) (clause-body-length (caddr body) fl)))
	((eq (car body) or-connective)
	 (let ((l1 (clause-body-length (cadr body) fl)) (l2 (clause-body-length (caddr body) fl)))
	   (if (= l1 l2)
	       l1
	       (error "OR branches ~A and ~A not of same length.~%Proof printing won't work." (cadr body) (caddr body)))))
	((member (car body) '(search/1 search/2 search/3 search/4)) (clause-body-length (cadr body) fl))
	((invisible-functor-p (car body)) 0)
	(t 1)))

(defun compile-clause-body1 (body vars unbound continuation)
  (cond ((eq body '!) `(progn ,continuation (undo-bindings) (return-from ,name nil)))
	((eq body '?!) (if (member first-argument-type '(:variable-bound-to-constant-first-argument
							 :variable-bound-to-compound-first-argument))
			   continuation
			   `(progn ,continuation (when (= *trail* !old-trail!) (return-from ,name nil)))))
	((eq body 'end-search) `(end-search ,continuation))
	((member (car body) '(search/1 search/2 search/3 search/4))
	 (compile-clause-body1 `(,and-connective (begin-search ,(caddr body) ,(car (cdddr body)) ,(cadr (cdddr body)))
						 (,and-connective ,(cadr body) end-search))
			       vars unbound continuation))
	((equal body '(true/0)) continuation)
	((equal body '(fail/0)) nil)
	((equal body '(false/0)) nil)
	((eq (car body) 'nsubgoals/1) continuation)
	((eq (car body) and-connective)
	 (cond ((and (not (atom (cadr body))) (eq (car (cadr body)) and-connective))
		(compile-clause-body1 `(,and-connective ,(cadr (cadr body)) (,and-connective ,(caddr (cadr body)) ,(caddr body)))
				      vars unbound continuation))
	       ((and (not (atom (cadr body))) (eq (car (cadr body)) or-connective))
		(compile-clause-body1 `(,or-connective (,and-connective ,(cadr (cadr body)) ,(caddr body))
						       (,and-connective ,(caddr (cadr body)) ,(caddr body)))
				      vars unbound continuation))
	       (t (compile-clause-body1 (cadr body)
					vars
					unbound
					(compile-clause-body1 (caddr body)
							      vars
							      (remove-if #'(lambda (v) (variable-occurs-in-term-p v (cadr body)))
									 unbound)
							      continuation)))))
	((eq (car body) or-connective)
	 (let ((x (compile-clause-body1 (cadr body) vars unbound continuation))
	       (y (compile-clause-body1 (caddr body) vars unbound continuation)))
	   (cond ((and (not (atom x)) (eq (car x) 'progn))
		  (cond ((and (not (atom y)) (eq (car y) 'progn)) `(progn ,@(cdr x) ,@(cdr y)))
			(t `(progn ,@(cdr x) ,y))))
		 ((and (not (atom y)) (eq (car y) 'progn)) `(progn ,x ,@(cdr y)))
		 (t `(progn ,x ,y)))))
	(t `(pcall ,body
		   (,(remove-if-not #'(lambda (v) (variable-occurs-in-term-p v body)) vars)
		    ,(remove-if-not #'(lambda (v) (variable-occurs-in-term-p v body)) unbound))
		   !level!
		   ,(if (and (not (atom continuation)) (eq (car continuation) 'funcall) (eq (caddr continuation) '!level!))
			(cadr continuation)
			`(function (lambda (!new-level!)
				     ;;#+(OR SYMBOLICS TI) (declare (sys:downward-function))
				     ,(SUBST '!NEW-LEVEL! '!LEVEL! CONTINUATION)	; WARNING: PUT !LEVEL! INSIDE MACRO IF NO SUBST DESIRED
				     )))))))

(defun compile-clause-body (body vars unbound)
  #+SYMBOLICS (declare (special *clausenum*))
  (if (equal body '(true/0))
      (setq body '?!)				;  automatically cut if unit clause subsumes goal
      )
  (let* ((length (clause-body-length body))
	 (nonunit (> length 0))
	 (incomplete-inference (or incomplete-inference (not nonunit)))
	 (allow-repeated-goals (or allow-repeated-goals (not nonunit)))
	 (x (compile-clause-body1
	      body vars unbound (wrap-pop-ancestor (wrap-exit-redo-trace `(funcall !continuation! !level!))))))
    `(PROGN #+SYMBOLICS (SETQ !NSUBGOALS! '(,LENGTH ,*CLAUSENUM*))
	    ,(wrap-count-calls (wrap-push-ancestor (wrap-undo-bindings x))))))

(defun compile-clause1 (headpats headlocs body vars unbound trail-is-nil)
  (cond ((null headpats) (compile-clause-body body vars unbound))
	((eq (car headpats) '_) (compile-clause1 (cdr headpats) (cdr headlocs) body vars unbound trail-is-nil))
	((member (car headpats) unbound)
	 (if (and (not (variable-occurs-in-terms-p (car headpats) (cdr headpats)))
		  (not (variable-occurs-in-term-p (car headpats) body)))
	     (compile-clause1 (cdr headpats) (cdr headlocs) body vars unbound trail-is-nil)
	     (if (atom (car headlocs))
		 (compile-clause1
		   (replace-variable-in-terms (car headpats) (car headlocs) (cdr headpats))
		   (cdr headlocs)
		   (replace-variable-in-term (car headpats) (car headlocs) body)
		   (cons (car headlocs) vars) (remove (car headpats) unbound) trail-is-nil)
		 `(let ((,(car headpats) ,(car headlocs)))
		    ,(compile-clause1 (cdr headpats) (cdr headlocs) body
				      vars (remove (car headpats) unbound) trail-is-nil)))))
	((member (car headpats) vars)
	 (cond ((and (eq (car headpats) '!arg1!) (member first-argument-type '(:constant-first-argument
									       :variable-bound-to-constant-first-argument)))
		`(when (unify-argument-with-constant ,(car headlocs) !arg1! :trail-is-nil ,trail-is-nil)
		   ,(compile-clause1 (cdr headpats) (cdr headlocs) body vars unbound nil)))
	       ((and (eq (car headpats) '!arg1!) (member first-argument-type '(:compound-first-argument
									       :variable-bound-to-compound-first-argument)))
		`(when (unify-argument-with-compound ,(car headlocs) !arg1! :unsafe ,unsafe-unification)
		   ,(compile-clause1 (cdr headpats) (cdr headlocs) body vars unbound nil)))
	       (t `(when (,(if unsafe-unification 'unsafe-maybe-trails-unify 'maybe-trails-unify)
			  ,(car headlocs) ,(car headpats) !old-trail! !level!)
		     ,(compile-clause1 (cdr headpats) (cdr headlocs) body vars unbound nil)))))
	((OR (atom (car headpats)) (EQ (CAAR HEADPATS) 'NTH))
	 `(when (unify-argument-with-constant ,(car headlocs)
					      ,(IF (AND (NOT (ATOM (CAR HEADPATS))) (EQ (CAAR HEADPATS) 'NTH))
						   (CAR HEADPATS)
						   `',(car headpats))
					      :trail-is-nil ,trail-is-nil)
	    ,(compile-clause1 (cdr headpats) (cdr headlocs) body vars unbound nil)))
	(t (let ((newunbound (remove-if #'(lambda (v) (variable-occurs-in-term-p v (car headpats))) unbound)))
	     (stack-list-new-variables
	       (remove-if-not #'(lambda (v) (variable-occurs-in-term-p v (car headpats))) unbound)
	       `(stack-let
		  ((!compound! ,(term-constructor (car headpats) vars)))
		  (when (unify-argument-with-compound
			  ,(car headlocs) !compound! :trail-is-nil ,trail-is-nil :unsafe ,unsafe-unification)
		    ,(compile-clause1 (cdr headpats) (cdr headlocs) body vars newunbound nil))))))))

(defun compile-clause (headpats headlocs body vars unbound &aux (trail-is-nil t))
  (compile-clause1 headpats headlocs body vars unbound trail-is-nil))

(defun all-distinct-variable-arguments (clause variables)
  (let (seen)
    (dolist (arg (clause-args clause) t)
      (if (or (eq arg '_) (and (member arg variables) (not (member arg seen))))
	  (push arg seen)
	  (return nil)))))

(defun all-constant-arguments (clause variables)
  (dolist (arg (clause-args clause) t)
    (if (or (not (atom arg)) (eq arg '_) (member arg variables)) (return nil))))

(defun compile-procedure-for-constant-first-argument (clauses variables)
  (do ((first-argument-type :constant-first-argument)
       (clauses clauses (cdr clauses))
       (compiled-clauses nil)
       (unbound variables)
       (clause)
       (*clausenum*))
      ((null clauses)
       (wrap-progn
	 (nconc
	   (if (not allow-repeated-goals) (list `(when ,(identical-to-ancestor-call name arity first-argument-type) (return-from ,name nil))))
	   (if (not incomplete-inference) (list `(when ,(reduce-by-ancestor-call name arity first-argument-type) (return-from ,name nil))))
	   (nreverse compiled-clauses))))
    (declare (special *clausenum*))
    (setq clause (car clauses))
    (setq *clausenum* (cdr (assoc clause clause-numbers)))
    (unless (not (atom (first (clause-args clause))))
      (push (cond ((eq (first (clause-args clause)) '_)
		   (wrap-depth-test
		     (compile-clause (rest (clause-args clause)) (rest (head-locs (clause-args clause)))
				     (clause-body clause) variables unbound)
		     (clause-body clause)))
		  ((member (first (clause-args clause)) variables)
		   (wrap-depth-test
		     (compile-clause (replace-variable-in-terms (first (clause-args clause)) '!arg1! (rest (clause-args clause)))
				     (rest (head-locs (clause-args clause)))
				     (replace-variable-in-term (first (clause-args clause)) '!arg1! (clause-body clause))
				     (cons '!arg1! variables)
				     (remove (first (clause-args clause)) unbound))
		     (clause-body clause)))
		  ((AND (NOT (NULL (CDR CLAUSES)))
			(ALL-CONSTANT-ARGUMENTS CLAUSE VARIABLES)
			(EQUAL (CLAUSE-BODY CLAUSE) (CLAUSE-BODY (CADR CLAUSES)))
			(ALL-CONSTANT-ARGUMENTS (CADR CLAUSES) VARIABLES))
		   (SETQ *CLAUSENUM* NIL)
		   (SETQ CLAUSES (CDR CLAUSES))
		   (DO ((VALUES (LIST (IF (= ARITY 1) (FIRST (CLAUSE-ARGS (CAR CLAUSES))) (CLAUSE-ARGS (CAR CLAUSES)))
				      (IF (= ARITY 1) (FIRST (CLAUSE-ARGS CLAUSE)) (CLAUSE-ARGS CLAUSE)))
				(CONS (IF (= ARITY 1) (FIRST (CLAUSE-ARGS (CAR CLAUSES))) (CLAUSE-ARGS (CAR CLAUSES))) VALUES)))
		       ((NOT (AND (NOT (NULL (CDR CLAUSES)))
				  (EQUAL (CLAUSE-BODY CLAUSE) (CLAUSE-BODY (CADR CLAUSES)))
				  (ALL-CONSTANT-ARGUMENTS (CADR CLAUSES) VARIABLES)))
			`(DOLIST (!VECTOR! ',(NREVERSE VALUES))
			   (WHEN (EQL !ARG1! ,(IF (= ARITY 1) `!VECTOR! `(CAR !VECTOR!)))
			     ,(WRAP-DEPTH-TEST
				(IF (= ARITY 1)
				    (COMPILE-CLAUSE NIL NIL (clause-body clause) VARIABLES UNBOUND)
				    (COMPILE-CLAUSE (HEAD-LOCS (REST (CLAUSE-ARGS CLAUSE)) '!VECTOR! T)
						    (CDR (HEAD-LOCS (CLAUSE-ARGS CLAUSE)))
						    (CLAUSE-BODY CLAUSE) VARIABLES UNBOUND))
				(CLAUSE-BODY CLAUSE)))))
		     (SETQ CLAUSES (CDR CLAUSES))))
		  (t `(when (eql !arg1! ',(first (clause-args clause)))
			,(wrap-depth-test
			   (compile-clause (rest (clause-args clause)) (rest (head-locs (clause-args clause)))
					   (clause-body clause) variables unbound)
			   (clause-body clause)))))
	    compiled-clauses))))

(defun compile-procedure-for-compound-first-argument (clauses variables)
  (do ((first-argument-type :compound-first-argument)
       (clauses clauses (cdr clauses))
       (compiled-clauses nil)
       (unbound variables)
       (clause)
       (*clausenum*))
      ((null clauses)
       (wrap-progn
	 (nconc
	   (if (not allow-repeated-goals) (list `(when ,(identical-to-ancestor-call name arity first-argument-type) (return-from ,name nil))))
	   (if (not incomplete-inference) (list `(when ,(reduce-by-ancestor-call name arity first-argument-type) (return-from ,name nil))))
	   (nreverse compiled-clauses))))
    (declare (special *clausenum*))
    (setq clause (car clauses))
    (setq *clausenum* (cdr (assoc clause clause-numbers)))
    (unless (and (atom (first (clause-args clause)))
		 (not (eq (first (clause-args clause)) '_))
		 (not (member (first (clause-args clause)) variables)))
      (push (cond ((eq (first (clause-args clause)) '_)
		   (wrap-depth-test
		     (compile-clause (rest (clause-args clause)) (rest (head-locs (clause-args clause)))
				     (clause-body clause) variables unbound)
		     (clause-body clause)))
		  ((member (first (clause-args clause)) variables)
		   (wrap-depth-test
		     (compile-clause (replace-variable-in-terms (first (clause-args clause)) '!arg1! (rest (clause-args clause)))
				     (rest (head-locs (clause-args clause)))
				     (replace-variable-in-term (first (clause-args clause)) '!arg1! (clause-body clause))
				     (cons '!arg1! variables)
				     (remove (first (clause-args clause)) unbound))
		     (clause-body clause)))
		  (t `(when (eq (car !arg1!) ',(car (first (clause-args clause))))
			,(wrap-depth-test
			   (compile-clause (append (rest (first (clause-args clause)))
						   (rest (clause-args clause)))
					   (append (head-locs (rest (first (clause-args clause))) '!arg1! t)
						   (rest (head-locs (clause-args clause))))
					   (clause-body clause) variables unbound)
			   (clause-body clause)))))
	    compiled-clauses))))

(defun compile-procedure-for-variable-first-argument (clauses  variables)
  (do ((first-argument-type :variable-first-argument)
       (clauses clauses (cdr clauses))
       (compiled-clauses nil)
       (unbound variables)
       (clause)
       (*clausenum*))
      ((null clauses)
       (wrap-progn
	 (nconc
	   (if (not allow-repeated-goals) (list `(when ,(identical-to-ancestor-call name arity first-argument-type) (return-from ,name nil))))
	   (if (not incomplete-inference) (list `(when ,(reduce-by-ancestor-call name arity first-argument-type) (return-from ,name nil))))
	   (nreverse compiled-clauses))))
    (declare (special *clausenum*))
    (setq clause (car clauses))
    (setq *clausenum* (cdr (assoc clause clause-numbers)))
    (push (cond ((eq (first (clause-args clause)) '_)
		 (wrap-depth-test
		   (compile-clause (rest (clause-args clause)) (rest (head-locs (clause-args clause)))
				   (clause-body clause) variables unbound)
		   (clause-body clause)))
		((member (first (clause-args clause)) variables)
		 (wrap-depth-test
		   (compile-clause (replace-variable-in-terms (first (clause-args clause)) '!arg1! (rest (clause-args clause)))
				   (rest (head-locs (clause-args clause)))
				   (replace-variable-in-term (first (clause-args clause)) '!arg1! (clause-body clause))
				   (cons '!arg1! variables)
				   (remove (first (clause-args clause)) unbound))
		   (clause-body clause)))
		((AND (NOT (NULL (CDR CLAUSES)))
		      (ALL-CONSTANT-ARGUMENTS CLAUSE VARIABLES)
		      (EQUAL (CLAUSE-BODY CLAUSE) (CLAUSE-BODY (CADR CLAUSES)))
		      (ALL-CONSTANT-ARGUMENTS (CADR CLAUSES) VARIABLES))
		 (SETQ *CLAUSENUM* NIL)
		 (SETQ CLAUSES (CDR CLAUSES))
		 (DO ((FIRST-ARGUMENT-TYPE :VARIABLE-BOUND-TO-CONSTANT-FIRST-ARGUMENT)
		      (VALUES (LIST (IF (= ARITY 1) (FIRST (CLAUSE-ARGS (CAR CLAUSES))) (CLAUSE-ARGS (CAR CLAUSES)))
				    (IF (= ARITY 1) (FIRST (CLAUSE-ARGS CLAUSE)) (CLAUSE-ARGS CLAUSE)))
			      (CONS (IF (= ARITY 1) (FIRST (CLAUSE-ARGS (CAR CLAUSES))) (CLAUSE-ARGS (CAR CLAUSES))) VALUES)))
		     ((NOT (AND (NOT (NULL (CDR CLAUSES)))
				(EQUAL (CLAUSE-BODY CLAUSE) (CLAUSE-BODY (CADR CLAUSES)))
				(ALL-CONSTANT-ARGUMENTS (CADR CLAUSES) VARIABLES)))
		      `(DOLIST (!VECTOR! ',(NREVERSE VALUES))
			 ,(WRAP-DEPTH-TEST
			    (IF (= ARITY 1)
				`(PROGN (BIND-VARIABLE-TO-TERM !ARG1! !VECTOR! :TRAIL)
					,(COMPILE-CLAUSE NIL NIL (CLAUSE-BODY CLAUSE) VARIABLES UNBOUND)
					(UNDO-BINDINGS))
				`(PROGN (BIND-VARIABLE-TO-TERM !ARG1! (CAR !VECTOR!) :TRAIL)
					,(COMPILE-CLAUSE (HEAD-LOCS (REST (CLAUSE-ARGS CLAUSE)) '!VECTOR! T)
							 (CDR (HEAD-LOCS (CLAUSE-ARGS CLAUSE)))
							 (CLAUSE-BODY CLAUSE) VARIABLES UNBOUND)
					(UNDO-BINDINGS)))
			    (CLAUSE-BODY CLAUSE))))
		   (SETQ CLAUSES (CDR CLAUSES))))
		((atom (first (clause-args clause)))
		 (let ((first-argument-type :variable-bound-to-constant-first-argument))
		   (wrap-depth-test
		     `(progn (bind-variable-to-term !arg1! ',(first (clause-args clause)) :trail)
			     ,(compile-clause (rest (clause-args clause)) (rest (head-locs (clause-args clause)))
					      (clause-body clause) variables unbound)
			     (undo-bindings))
		     (clause-body clause))))
		(t (wrap-depth-test
		     (let ((first-argument-type :variable-bound-to-compound-first-argument)
			   (unbound (remove-if #'(lambda (v) (variable-occurs-in-term-p v (first (clause-args clause)))) unbound)))
		       (stack-list-new-variables
			 (remove-if-not #'(lambda (v) (variable-occurs-in-term-p v (first (clause-args clause)))) variables)
			 `(stack-let
			    ((!compound! ,(term-constructor (first (clause-args clause)) variables)))
			    (progn (bind-variable-to-term !arg1! !compound! :trail)
				   ,(compile-clause (rest (clause-args clause)) (rest (head-locs (clause-args clause)))
						    (clause-body clause) variables unbound)
				   (undo-bindings)))))
		     (clause-body clause))))
	  compiled-clauses)))

(defun compile-procedure (name variables clauses &key (traceable nil) (unbounded-search nil) (unsafe-unification nil)
			  (incomplete-inference nil) (allow-repeated-goals nil) (split-procedure nil) (collapse-clauses nil)
			  &aux (arity (get name 'arity)) parameters (unbound variables) (lisp-compile-time 0))
  (declare (ignore collapse-clauses))
  (WHEN (EQ NAME 'QUERY/0)
    (SETQ TRACEABLE NIL)
    (SETQ UNBOUNDED-SEARCH T)
    (SETQ INCOMPLETE-INFERENCE T)
    (SETQ ALLOW-REPEATED-GOALS T))
  (IF (= ARITY 0) (SETQ SPLIT-PROCEDURE NIL))
  (if (not trace-calls) (setq traceable nil))
  (setq parameters (list 'count-calls count-calls
			 'CLAUSE-NUMBERS (MAPCAR #'(LAMBDA (CLAUSE) (CDR (ASSOC CLAUSE CLAUSE-NUMBERS :TEST #'EQUAL))) CLAUSES)
			 :variables variables
			 :traceable traceable
			 :unbounded-search unbounded-search
			 :unsafe-unification unsafe-unification
			 :incomplete-inference incomplete-inference
			 :allow-repeated-goals allow-repeated-goals
			 :split-procedure split-procedure))
  (when (or recompile
	    (not (equal clauses (get name 'compiled-clauses)))
	    (not (equal parameters (get name 'compiled-parameters))))
    (let (arglist auxlist namec names namev defn defnc defns defnv)
      (WHEN (NOT ALLOW-REPEATED-GOALS)
	(DOLIST (CLAUSE CLAUSES (SETQ ALLOW-REPEATED-GOALS T))
	  (WHEN (> (CLAUSE-BODY-LENGTH (CLAUSE-BODY CLAUSE)) 0)
	    (RETURN))))
      (setq arglist (append (head-locs arity) '(!level! !continuation!)))
      (setq auxlist (append arglist '(&aux (!old-trail! *trail*) #+SYMBOLICS !NSUBGOALS!)))
      (when (or (not allow-repeated-goals) (not incomplete-inference))
	(eval `(defvar ,(ancestors-name name) nil))
	(eval `(defvar ,(ancestors-name (negated-functor name)) nil)))
      (setq defn (list 'lambda
		       (if (and split-procedure (not (= arity 0))) arglist auxlist)
		       `(incf !level!)
		       (list 'block name
			     (wrap-call-fail-trace
			       (cond ((= arity 0)
				      (do ((first-argument-type nil)
					   (clauses clauses (cdr clauses))
					   (compiled-clauses nil)
					   (clause)
					   (*clausenum*))
					  ((null clauses)
					   (wrap-progn
					     (nconc
					       (if (not allow-repeated-goals) (list `(when ,(identical-to-ancestor-call name 0 nil)
								    (return-from ,name nil))))
					       (if (not incomplete-inference) (list `(when ,(reduce-by-ancestor-call name 0 nil)
								     (return-from ,name nil))))
					       (nreverse compiled-clauses))))
					(declare (special *clausenum*))
					(setq clause (car clauses))
					(setq *clausenum* (cdr (assoc clause clause-numbers)))
					(push (wrap-depth-test (compile-clause nil nil (clause-body clause) variables unbound)
							       (clause-body clause))
					      compiled-clauses)))
				     (split-procedure
				      (setq namec (intern (concatenate 'string (symbol-name name) "C") *PTTP-PACK*))
				      (setq names (intern (concatenate 'string (symbol-name name) "S") *PTTP-PACK*))
				      (setq namev (intern (concatenate 'string (symbol-name name) "V") *PTTP-PACK*))
				      (let ((nm (functor-name name)))
					(setf (get namec 'name) nm)
					(setf (get names 'name) nm)
					(setf (get namev 'name) nm)
					(setf (get namec 'arity) arity)
					(setf (get names 'arity) arity)
					(setf (get namev 'arity) arity))
				      (setq defnc (list 'lambda
							auxlist
							(list 'block name
							      (wrap-bind-args
								(compile-procedure-for-constant-first-argument clauses variables)))))
				      (setq defns (list 'lambda
							auxlist
							(list 'block name
							      (wrap-bind-args
								(compile-procedure-for-compound-first-argument clauses variables)))))
				      (setq defnv (list 'lambda
							auxlist
							(list 'block name
							      (wrap-bind-args
								(compile-procedure-for-variable-first-argument clauses variables)))))
				      `(dereference
					 !arg1!
					 :if-constant  (,namec . ,arglist)
					 :if-compound  (,names . ,arglist)
					 :if-variable  (,namev . ,arglist)))
				     (t (wrap-bind-args
					  `(dereference
					     !arg1!
					     :if-constant  ,(compile-procedure-for-constant-first-argument clauses variables)
					     :if-compound  ,(compile-procedure-for-compound-first-argument clauses variables)
					     :if-variable  ,(compile-procedure-for-variable-first-argument clauses variables)))))))))
      (when print-compile-names (format t "~&~A compiled from PTTP to LISP" name))
      (setq lisp-compile-time (get-internal-run-time))
      (when (and split-procedure (> arity 0))
	(compile namec defnc)
	(compile names defns)
	(compile namev defnv))
      (compile name defn)
      (when print-compile-names (format t "~&~A compiled from LISP to machine code" name))
      (setq lisp-compile-time (- (get-internal-run-time) lisp-compile-time)))
    (setf (get name 'compiled-clauses) clauses)
    (setf (get name 'compiled-parameters) parameters))
  lisp-compile-time)

(defun print-clauses (clauses variables)
  (when variables
    (fresh-line)
    (terpri)
    (cond ((null (cdr variables))
	   (princ " The symbol ")
	   (write-term (car variables))
	   (princ " denotes a variable."))
	  (t (princ " The symbols ")
	     (write-term (car variables))
	     (do ((v (cdr variables) (cdr v)))
		 ((null (cdr v)) (princ " and ") (write-term (car v)))
	       (princ ", ") (write-term (car v)))
	     (princ " denote variables."))))
    (dolist (clause clauses)
      (write-clause clause (cdr (assoc clause clause-numbers)))))

(defun predicate-clauses (predicate clauses)
  (remove-if-not #'(lambda (clause) (eq (clause-pred clause) predicate))
		 clauses))

(defun program (variables wffs &rest options)
  (let (clauses predicates wff-number start-time stop-time (lisp-compile-time 0))
    (setq start-time (get-internal-run-time))
    (setq wffs (canonicalize-functors-in-terms wffs))
    (setq stop-time (get-internal-run-time))

    (when print-clauses
      (setq wff-number 0)
      (setq clause-numbers nil)
      (dolist (wff wffs)
	(incf wff-number)
	(push (cons wff wff-number) clause-numbers))
      (print-clauses wffs variables))

    (setq start-time (- (get-internal-run-time) (- stop-time start-time)))
    (setq wff-number 0)
    (setq clause-numbers nil)
    (dolist (wff wffs)
      (incf wff-number)
      (let ((cls (clauses-from-wff wff)))
	(cond ((null cls))			;if wff is tautology 2005-07-21
              ((null (cdr cls))
	       (push (car cls) clauses)
	       (push (cons (car clauses) wff-number) clause-numbers)
	       (pushnew (clause-pred (car clauses)) predicates))
	      (t (let ((literal-number 0))
		   (dolist (cl cls)
		     (incf literal-number)
		     (push cl clauses)
		     (push (cons (car clauses) (cons wff-number (numbered-letter literal-number))) clause-numbers)
		     (pushnew (clause-pred (car clauses)) predicates)
		     (pushnew (negated-functor (clause-pred (car clauses))) predicates)))))))
    (setq clauses (nreverse clauses))
    (setq predicates (nreverse predicates))
    
    (dolist (pred predicates)
      (incf lisp-compile-time (apply #'compile-procedure (list* pred variables (predicate-clauses pred clauses) options))))
    (setq stop-time (get-internal-run-time))
    
    (when print-compile-times
      (format t "~2&Compilation time: ~,3F seconds (PTTP) + ~,3F seconds (LISP)~%"
	      (/ (- stop-time start-time lisp-compile-time) float-internal-time-units-per-second)
	      (/ lisp-compile-time float-internal-time-units-per-second)))
    
    nil))
 
(defvar *print-proof* t)

(defvar *print-proof-time* 0)

(defun query-success (!level!)
  (declare (ignore !level!))
  #+SYMBOLICS
  (let ((start-time (get-internal-run-time)))
    #+DMILES (when *print-proof* (scl:condition-bind ((print-proof 'print-proof)) (scl:signal 'print-proof)))
    (print-proof 'print-proof)
    (incf *print-proof-time* (- (get-internal-run-time) start-time)))
  nil)

(defun query (&optional variables goal &rest options)
  (when goal
    (apply #'program variables (list* `((<- (query) ,goal)) options)))
  (let (start-time stop-time value time)
    (setq *ncalls* 0)
    (setq *trail* 0)
    (setq *trace-search-time* 0)
    (setq *print-proof-time* 0)
    (setq start-time (get-internal-run-time))
    (setq value (query/0 0 #'query-success))
    (setq stop-time (get-internal-run-time))
    (when (> *ncalls* 0)
      (setq time (/ (max 1 (- stop-time start-time *trace-search-time* *print-proof-time*))
		    (float internal-time-units-per-second)))
      (format t "~2&Execution time: ~:D inferences in ~,3F seconds (~,2F K lips)~%" *ncalls* time (/ *ncalls* time 1000)))
    value))

;; INPUT EXPRESSION CANONICALIZATION

(defun make-functor (name arity)
  (let ((l (get name 'functors)))
    (or (cdr (assoc arity l))
	(let ((w (intern (concatenate 'string (symbol-name name) "/" (princ-to-string arity)) *PTTP-PACK*)))
	  (setf (get name 'functors) (cons (cons arity w) l))
	  (setf (get w 'arity) arity)
	  (setf (get w 'name) name)
	  w))))

(defun negated-functor (name)
  (or (get name 'negation)
      (let* ((s (symbol-name name))
	     (w (intern (if (and (>= (length s) 2) (char= (char s 0) '#\~))
			    (subseq s 1)
			    (concatenate 'string "~" s))
			*PTTP-PACK*)))
	(setf (get name 'negation) w)
	(setf (get w 'negation) name)
	(let ((arity (get name 'arity))) (when arity (setf (get w 'arity) arity)))
	w)))

(defun functor-name (x)
  (or (get x 'name)
      (let* ((s (symbol-name x)) (n (length s)))
	(if (digit-char-p (char s (1- n)))
	    (do ((n (- n 2) (1- n)) (ch))
		((= n 0) x)
	      (setq ch (char s n))
	      (cond ((digit-char-p ch))
		    ((char= ch '#\/) (return (setf (get x 'name) (subseq s 0 n))))
		    (t (return (setf (get x 'name) x)))))
	    x))))

(defun functor-arity (x)
  (or (get x 'arity)
      (let* ((s (symbol-name x)) (n (length s)) (arity (digit-char-p (char s (1- n)))))
	(if arity
	    (do ((n (- n 2) (1- n)) (tens 10 (* 10 tens)) (ch) (num))
		((= n 0) '?)
	      (setq ch (char s n))
	      (setq num (digit-char-p ch))
	      (cond (num (incf arity (* tens num)))
		    ((char= ch '#\/) (setf (get x 'arity) arity) (return arity))
		    (t (return '?))))
	    '?))))

(defun negate (x)
  (cond ((equal x '(true/0)) '(false/0))
	((equal x '(false/0)) '(true/0))
	((and (not (atom x)) (eq (car x) and-connective)) (list or-connective (negate (cadr x)) (negate (caddr x))))
	((and (not (atom x)) (eq (car x) or-connective)) (list and-connective (negate (cadr x)) (negate (caddr x))))
	(t (cons (negated-functor (car x)) (cdr x)))))

(defun conjoin (x y)
  (cond ((equal x '(true/0)) y)
	((equal y '(true/0)) x)
	((equal x '(false/0)) x)
	((equal y '(false/0)) y)
	((and (not (atom x)) (not (atom y)) (eq (car x) (negated-functor (car y))) (equal (cdr x) (cdr y))) '(false/0))
	((and (not (atom x)) (eq (car x) and-connective)) (list and-connective (cadr x) (list and-connective (caddr x) y)))
	(t (list and-connective x y))))

(defun disjoin (x y)
  (cond ((equal x '(false/0)) y)
	((equal y '(false/0)) x)
	((equal x '(true/0)) x)
	((equal y '(true/0)) y)
	((and (not (atom x)) (not (atom y)) (eq (car x) (negated-functor (car y))) (equal (cdr x) (cdr y))) '(true/0))
	((and (not (atom x)) (eq (car x) or-connective)) (list or-connective (cadr x) (list or-connective (caddr x) y)))
	(t (list or-connective x y))))

(defun convert-string-to-list (s)
  (do ((i (1- (length s)) (1- i)) (w '|[]|))
      ((< i 0) w)
    (setq w `(cons/2 ,(char-code (char s i)) ,w))))

(defun canonicalize-functors-in-term (term)
  (cond ((atom term) (cond ((null term) '|[]|)
			   ((stringp term) (convert-string-to-list term))
			   (t term)))
	((not (symbolp (car term))) (error "Nonsymbol functor ~A" (car term)))
	((member (car term) '(and \ \& \,))
	 (if (null (cddr term))
	     (canonicalize-functors-in-term (cadr term))
	     (conjoin (canonicalize-functors-in-term (cadr term))
		      (if (cdddr term)
			  (canonicalize-functors-in-term (cons (car term) (cddr term)))
			  (canonicalize-functors-in-term (caddr term))))))
	((member (car term) '(or \ \| \;))
	 (if (null (cddr term))
	     (canonicalize-functors-in-term (cadr term))
	     (disjoin (canonicalize-functors-in-term (cadr term))
		      (if (cdddr term)
			  (canonicalize-functors-in-term (cons (car term) (cddr term)))
			  (canonicalize-functors-in-term (caddr term))))))
	((member (car term) '(\ imply imp implies))
	 (disjoin (negate (canonicalize-functors-in-term (cadr term))) (canonicalize-functors-in-term (caddr term))))
	((member (car term) '(\ equiv equ iff))
	 (let ((x (canonicalize-functors-in-term (cadr term))) (y (canonicalize-functors-in-term (caddr term))))
	   (conjoin (disjoin (negate x) y) (disjoin x (negate y)))))
	(t (cons (make-functor (car term) (length (cdr term))) (canonicalize-functors-in-terms (cdr term))))))

(defun canonicalize-functors-in-terms (terms)
  (cond ((null terms) nil)
	(t (let ((x (canonicalize-functors-in-term (car terms))) (y (canonicalize-functors-in-terms (cdr terms))))
	     (if (and (eq x (car terms)) (eq y (cdr terms))) terms (cons x y))))))

;; CONVERSION OF NONCLAUSAL INPUTS TO CLAUSE FORM

(defun clause-body-for-literal (lit wff &aux ~f)
  (cond ((atom wff) wff)
	((eq (car wff) and-connective) (disjoin (clause-body-for-literal lit (cadr wff)) (clause-body-for-literal lit (caddr wff))))
	((eq (car wff) or-connective) (conjoin (clause-body-for-literal lit (cadr wff)) (clause-body-for-literal lit (caddr wff))))
	((equal lit wff) '(true/0))
	((and (eq (car lit) (setq ~f (negated-functor (car wff)))) (equal (cdr lit) (cdr wff))) '(false/0))
	(t (cons ~f (cdr wff)))))

(defun literals-in-wff (wff &optional literals)
  (cond ((and (not (atom wff)) (or (eq (car wff) and-connective) (eq (car wff) or-connective)))
	 (literals-in-wff (caddr wff) (literals-in-wff (cadr wff) literals)))
	(t (pushnew wff literals :test #'equal))))

(defun clauses-from-wff (wff)
  (cond ((eq (car wff) and-connective)
	 (nconc (clauses-from-wff (cadr wff)) (clauses-from-wff (caddr wff))))
	((eq (car wff) or-connective)
	 (let (result)
	   (dolist (lit (literals-in-wff wff))
	     (when (not (atom lit))		;don't promote ! to head
	       (push (list '<-/2 lit (clause-body-for-literal lit wff)) result)))
	   result))
        ((eq (car wff) 'true/0) nil)		;if wff is tautology 2005-07-21
	(t (list wff))))

;; PROLOG BUILT-IN PREDICATES

;; AUXILIARY VARIABLE !NSUBGOALS! WITH VALUE '(0 NIL) NECESSARY FOR PROOF PRINTING
;; DUE TO BUG IN DBG:FRAME-LOCAL-VALUE THAT CAUSES AN ERROR IF NAMED VARIABLE ISN'T FOUND

(defun call/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(1 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (dereference x
	       :if-variable  (error "CALL was given non-compound argument ~A" x)
	       :if-constant  (error "CALL was given non-compound argument ~A" x)
	       :if-compound  (apply (car x) (append (cdr x) (list !level! !continuation!)))))	; inefficient

(defun not/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (not-solvable x 1000000 !level! !continuation!))

(defun not/2 (x y !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (not-solvable x y !level! !continuation!))

(defun y-or-n-p/0 (!level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (if (y-or-n-p)
      (funcall !continuation! !level!)
      nil))

(defun y-or-n-p/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (fresh-line) (write-term x) (princ "? ")
  (if (y-or-n-p)
      (funcall !continuation! !level!)
      nil))

(defun atomic/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (dereference x) (funcall !continuation! !level!)))

(defun atom/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (dereference x) (symbolp x) (funcall !continuation! !level!)))

(defun integer/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (dereference x) (integerp x) (funcall !continuation! !level!)))

(defun var/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (dereference x
	       :if-constant  nil
	       :if-variable  (funcall !continuation! !level!)
	       :if-compound  nil))

(defun nonvar/1 (x !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (dereference x
	       :if-constant  (funcall !continuation! !level!)
	       :if-variable  nil
	       :if-compound  (funcall !continuation! !level!)))

(defun functor/3 (term functor arity !level! !continuation! &aux (!old-trail! *trail*) #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (dereference
    term
    :if-variable
    (when (and (dereference functor) (dereference arity))
      (case arity
	(0 (progn (bind-variable-to-term term functor :trail)
		  (funcall !continuation! !level!)
		  (undo-bindings)))
	(1 (stack-let ((struct (list functor
				     (list* !level! nil #+include-name-in-variable 'z1))))	; (new-variable '_)
	     (progn (bind-variable-to-term term struct :trail)
		    (funcall !continuation! !level!)
		    (undo-bindings))))
	(2 (stack-let ((struct (list functor
				     (list* !level! nil #+include-name-in-variable 'z1)
				     (list* !level! nil #+include-name-in-variable 'z2))))
	     (progn (bind-variable-to-term term struct :trail)
		    (funcall !continuation! !level!)
		    (undo-bindings))))
	(3 (stack-let ((struct (list functor
				     (list* !level! nil #+include-name-in-variable 'z1)
				     (list* !level! nil #+include-name-in-variable 'z2)
				     (list* !level! nil #+include-name-in-variable 'z3))))
	     (progn (bind-variable-to-term term struct :trail)
		    (funcall !continuation! !level!)
		    (undo-bindings))))
	(4 (stack-let ((struct (list functor
				     (list* !level! nil #+include-name-in-variable 'z1)
				     (list* !level! nil #+include-name-in-variable 'z2)
				     (list* !level! nil #+include-name-in-variable 'z3)
				     (list* !level! nil #+include-name-in-variable 'z4))))
	     (progn (bind-variable-to-term term struct :trail)
		    (funcall !continuation! !level!)
		    (undo-bindings))))
	(5 (stack-let ((struct (list functor
				     (list* !level! nil #+include-name-in-variable 'z1)
				     (list* !level! nil #+include-name-in-variable 'z2)
				     (list* !level! nil #+include-name-in-variable 'z3)
				     (list* !level! nil #+include-name-in-variable 'z4)
				     (list* !level! nil #+include-name-in-variable 'z5))))
	     (progn (bind-variable-to-term term struct :trail)
		    (funcall !continuation! !level!)
		    (undo-bindings))))
	(otherwise (error "Functor argument of FUNCTOR has arity ~A.  Unimplemented" arity))))
    :if-constant
    (when (and (always-trails-unify functor term !old-trail!) (always-trails-unify arity 0 !old-trail!))
      (funcall !continuation! !level!) (undo-bindings))
    :if-compound 
    (when (and (always-trails-unify functor (car term) !old-trail!) (always-trails-unify arity (length (cdr term)) !old-trail!))
      (funcall !continuation! !level!) (undo-bindings))))

(defun arg/3 (index term arg !level! !continuation! &aux (!old-trail! *trail*) #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (dereference term
	       :if-variable  (error "ARG was given non-compound second argument ~A" term)
	       :if-constant  (error "ARG was given non-compound second argument ~A" term)
	       :if-compound  (progn (dereference index)
				    (cond ((not (integerp index))
					   (error "ARG was given non-integer first argument ~A" index))
					  ((or (< index 1) (> index (length (cdr term))))
					   (error "ARG was given out-of-range first argument ~A for term ~A" index term))
					  (t (when (always-trails-unify arg (nth (1- index) (cdr term)) !old-trail!)
					       (funcall !continuation! !level!) (undo-bindings)))))))

(defun is/2 (x y !level! !continuation! &aux (!old-trail! *trail*) #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (labels ((evaluate (term) (if (dereference term)
				term
				(case (length (cdr term))
				  (0 (funcall (car term)))
				  (1 (funcall (car term) (evaluate (cadr term))))
				  (2 (funcall (car term) (evaluate (cadr term)) (evaluate (caddr term))))
				  (3 (funcall (car term) (evaluate (cadr term)) (evaluate (cadddr term))))
				  (otherwise (error "Function argument of IS has ~A arguments.  Unimplemented" (length (cdr term))))))))
    (let ((y (evaluate y)))
      (when (unify-argument-with-constant x y :trail-is-nil t)
	(funcall !continuation! !level!) (undo-bindings)))))

(defun =/2 (x y !level! !continuation! &aux (!old-trail! *trail*) #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (when (always-trails-unify x y !old-trail!)
    (funcall !continuation! !level!) (undo-bindings)))

(defun \\=/2 (x y !level! !continuation! &aux (!old-trail! *trail*) #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (if (always-trails-unify x y !old-trail!)
      (undo-bindings)
      (funcall !continuation! !level!)))

(defun unsafe-=/2 (x y !level! !continuation! &aux (!old-trail! *trail*) #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (when (unsafe-always-trails-unify x y !old-trail!)
    (funcall !continuation! !level!) (undo-bindings)))

(defun unsafe-\\=/2 (x y !level! !continuation! &aux (!old-trail! *trail*) #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (if (unsafe-always-trails-unify x y !old-trail!)
      (undo-bindings)
      (funcall !continuation! !level!)))

(defun ==/2 (x y !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (identical x y) (funcall !continuation! !level!)))

(defun \\==/2 (x y !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (not (identical x y)) (funcall !continuation! !level!)))

(defun >/2 (x y !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (dereference x) (dereference y) (> x y) (funcall !continuation! !level!)))

(defun </2 (x y !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (dereference x) (dereference y) (< x y) (funcall !continuation! !level!)))

(defun >=/2 (x y !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (dereference x) (dereference y) (>= x y) (funcall !continuation! !level!)))

(defun =</2 (x y !level! !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (and (dereference x) (dereference y) (<= x y) (funcall !continuation! !level!)))

(setf (invisible-functor-p 'nl/0) t)
(defun nl/0 (!level! !continuation!)
  (incf !level!)
  (terpri) (funcall !continuation! !level!))

(setf (invisible-functor-p 'dislay/1) t)
(defun display/1 (x !level! !continuation!)
  (incf !level!)
  (display-term x) (funcall !continuation! !level!))

(setf (invisible-functor-p 'write/1) t)
(defun write/1 (x !level! !continuation!)
  (incf !level!)
  (write-term x) (funcall !continuation! !level!))

(setf (invisible-functor-p 'trace/0) t)
(defun trace/0 (&optional (!level! 0) !continuation!)
  (incf !level!)
  (setq *tracing* t) (if !continuation! (funcall !continuation! !level!)))

;; and !continuation! arguments of trace/0, notrace/0, spy/1, nospy/1, nodebug/0, debugging/0, op/3
;; are optional to make them easier to use as user callable functions

(setf (invisible-functor-p 'notrace/0) t)
(defun notrace/0 (&optional (!level! 0) !continuation!)
  (incf !level!)
  (setq *tracing* nil) (if !continuation! (funcall !continuation! !level!)))

(setf (invisible-functor-p 'spy/1) t)
(defun spy/1 (x &optional (!level! 0)!continuation!)
  ;; takes single predicate argument, e.g., (spy reverse/2)
  ;; (spy reverse), (spy (reverse 2)), (spy [reverse,concatenate]), etc. not allowed
  (incf !level!)
  (pushnew x *spy-points*) (if !continuation! (funcall !continuation! !level!)))

(setf (invisible-functor-p 'nospy/1) t)
(defun nospy/1 (x &optional (!level! 0) !continuation!)
  ;; takes single predicate argument, e.g., (nospy reverse/2)
  ;; (nospy reverse), (nospy (reverse 2)), (nospy [reverse,concatenate]), etc. not allowed
  (incf !level!)
  (setq *spy-points* (delete x *spy-points*)) (if !continuation! (funcall !continuation! !level!)))

(setf (invisible-functor-p 'nodebug/0) t)
(defun nodebug/0 (&optional (!level! 0) !continuation!)
  (incf !level!)
  (setq *spy-points* nil) (if !continuation! (funcall !continuation! !level!)))

(setf (invisible-functor-p 'debugging/0) t)
(defun debugging/0 (&optional (!level! 0) !continuation!)
  (incf !level!)
  (cond ((null *spy-points*) (princ "[]"))
	(t (princ "[") (princ (car *spy-points*)) (dolist (x (cdr *spy-points*)) (princ ",") (princ x)) (princ "]")))
  (if !continuation! (funcall !continuation! !level!)))

(defun op/3 (prec spec name &optional (!level! 0) !continuation! #+SYMBOLICS &AUX #+SYMBOLICS (!NSUBGOALS! '(0 NIL)))
  #+SYMBOLICS (DECLARE (IGNORE !NSUBGOALS!))
  (incf !level!)
  (when (not (numberp prec)) (error "Non-numeric precedence ~A" prec))
  (case spec
    ((fx fy xf yf)
     (let ((arity (functor-arity name)))
       (case arity
	 (? (setq name (make-functor name 1)) (setf (precedence name) prec) (setf (specifier name) spec))
	 (1 (setf (precedence name) prec) (setf (specifier name) spec))
	 (otherwise (error "Functor ~A is ~D-ary, but specifier ~A is 1-ary" name arity spec)))))
    ((xfx xfy yfx yfy)
     (let ((arity (functor-arity name)))
       (case arity
	 (? (setq name (make-functor name 2)) (setf (precedence name) prec) (setf (specifier name) spec))
	 (2 (setf (precedence name) prec) (setf (specifier name) spec))
	 (otherwise (error "Functor ~A is ~D-ary, but specifier ~A is 2-ary" name arity spec)))))
    (otherwise (error "Unknown position/associativity specifier ~A" spec)))
  (if !continuation! (funcall !continuation! !level!)))

(op/3 1200 'xfx '<-)				; unimplemented
(op/3 1200 'xfx '-->)				; unimplemented
(op/3 1200 'fx  '|:-|)				; unimplemented
(op/3 1200 'fx  '?-)				; unimplemented
(op/3 1100 'xfy or-connective)
(op/3 1100 'xf  '\;)				; unimplemented
(op/3 1050 'xfy '->)				; unimplemented
(op/3 1000 'xfy and-connective)
(op/3  800 'fx  'not)				; unimplemented
(op/3  700 'xfx '=)
(op/3  700 'xfx '\\=)
(op/3  700 'xfx '==)
(op/3  700 'xfx '\\==)
(op/3  700 'xfx 'is)
(op/3  700 'xfx '|=..|)				; unimplemented
(op/3  700 'xfx '<)
(op/3  700 'xfx '>)
(op/3  700 'xfx '=<)
(op/3  700 'xfx '>=)
(op/3  500 'yfx '+)
(op/3  500 'yfx '-)
(op/3  500 'fx  '+)
(op/3  500 'fx  '-)
(op/3  400 'yfx '*)
(op/3  400 'yfx '/)
(op/3  300 'xfx 'mod)

;; PROLOG BUILT-IN FUNCTIONS (FOR IS/2 PREDICATE INTERPRETATION)

(defun |+/1| (m)
  m)

(defun |-/1| (m)
  (- m))

(defun |+/2| (m n)
  (+ m n))

(defun |-/2| (m n)
  (- m n))

(defun */2 (m n)
  (* m n))

(defun //2 (m n)
  (truncate m n))

(defun mod/2 (m n)
  (rem m n))

(defun cputime/0 nil
  (get-internal-run-time))

;; PROOF PRINTING FACILITY
;; this works only on Symbolics machines because it examines the stack to find goals and subgoals
;; if including variable names in variables is turned off for speed, variable names printed in proof may be ambiguous

#+(and SYMBOLICS DMILES)
(scl:defflavor print-proof () (scl:condition))

#+(and SYMBOLICS DMILES)
(scl:defmethod (dbg:report print-proof) (stream) (declare (ignore stream)) nil)

#+SYMBOLICS
(defun print-proof (condition-object)
  #+DMILES 
  (dbg:with-erring-frame (frame-ptr condition-object)
    (let (goals)
      (do ((frame-ptr frame-ptr (dbg:frame-previous-interesting-active-frame frame-ptr)))
	  ((null frame-ptr))
	(let* ((l (dbg:get-frame-function-and-args frame-ptr))
	       (n (and (symbolp (car l))
		       (not (invisible-functor-p (car l)))
		       (not (getf (get (car l) 'compiled-parameters) :split-procedure))
		       (get (car l) 'arity))))
	  (when n
	    (let ((k (DBG:FRAME-LOCAL-VALUE FRAME-PTR '!NSUBGOALS! T)))
	      ;; Symbolics bug: above results in an error if there is no !nsubgoals! variable in the stack frame
	      (push (if k
			(list n l (car k) (cadr k))
			(list n l 0 nil))
		    goals)))))
      (format t "~2&Proof:~%Goal#  Wff#  Wff Instance~%-----  ----  ------------")
      (print-proof1 goals 0 1 0)
      nil)))

#+SYMBOLICS
(defun print-proof1 (goals goalnum ngoals level)
  (dotimes (i ngoals)
    (declare (ignore i))
    (let ((arity (caar goals)) (goal (cadar goals)) (nsubgoals (caddar goals)) (number (cadddr (car goals))))
      (format t "~&(~3D)" goalnum)
      (cond ((consp number) (format t "~5D~A  " (car number) (cdr number)))
	    (number (format t "~5D   " number))
	    (t (princ "        ")))
      (dotimes (i level) (declare (ignore i)) (princ "   "))
      (write-functor-and-arguments (car goal) (subseq goal 1 (1+ arity)))
      (cond ((= nsubgoals 0) (princ ".") (setq goalnum (1+ goalnum)) (setq goals (cdr goals)))
	    (t (princ " <-")
	       (let ((first t))
		 (dolist (subgoal (collect-goals (cdr goals) nsubgoals))
		   (cond (first (princ " ") (setq first nil))
			 (t (princ " , ")))
		   (write-functor-and-arguments (car subgoal) (subseq subgoal 1 (1+ (get (car subgoal) 'arity))))))
	       (princ ".")
	       (multiple-value-setq (goals goalnum) (print-proof1 (cdr goals) (1+ goalnum) nsubgoals (1+ level)))))))
  (values goals goalnum))

#+SYMBOLICS
(defun collect-goals (goals ngoals)
  (let (w)
    (dotimes (i ngoals)
      (declare (ignore i))
      (push (cadar goals) w)
      (setq goals (skip-goals (cdr goals) (caddar goals))))
    (nreverse w)))

#+SYMBOLICS
(defun skip-goals (goals ngoals)
  (dotimes (i ngoals)
    (declare (ignore i))
    (setq goals (skip-goals (cdr goals) (caddar goals))))
  goals)

(defun numbered-letter (n)
  (case n
    ( 1 "a") ( 2 "b") ( 3 "c") ( 4 "d") ( 5 "e") ( 6 "f") ( 7 "g") ( 8 "h") ( 9 "i") (10 "j")
    (11 "k") (12 "l") (13 "m") (14 "n") (15 "o") (16 "p") (17 "q") (18 "r") (19 "s") (20 "t")
    (21 "u") (22 "v") (23 "w") (24 "x") (25 "y") (26 "z")))

(defun chang&lee-examples nil
  (print 'chang&lee-examples)
  (chang&lee-example-0)
  (chang&lee-example-1)
  (chang&lee-example-2)
  (chang&lee-example-3)
  (chang&lee-example-4)
  (chang&lee-example-5)
  (chang&lee-example-6)
  (chang&lee-example-7)
  (chang&lee-example-8)
  (chang&lee-example-9))

#|

 (load "cynd/cb-pttp.lisp")
 (CHANG&LEE-EXAMPLE-0)

 (chang&lee-examples)
 (defvar *tracing* '(query father/2))
|#

(defun chang&lee-example-0 nil
  (print 'chang&lee-example-0)
  (format t "~&Prove that in an associative system with left and right solutions,~%there is a right identity element.")
  (program '(x m f w yn)
	   '((isa (motherof x) female)
	     (isa (fatherof x) male)
	     (~father baby mom)
	     (mother baby mom)
	     (parent baby dad)
	     (-> (and (mother x m)(\\= m f)(parent x f)) (father x f))
	     (<- (mother baby m) (and (y-or-n-p)))
	     (-> (mother x m) (parent x m))
	     (-> (father x m) (parent x m))
	     (<- (query) (and (search (and (mother baby w) (princ "ANSER=")(write w) ! ) ))))
	   :incomplete-inference t)
  (query)
  )

(defun chang&lee-example-1 nil
  (print 'chang&lee-example-1)
  (format t "~&Prove that in an associative system with left and right solutions,~%there is a right identity element.")
  (program '(u v w x y z)
	   '((p (g x y) x y)
	     (p x (h x y) y)
	     (-> (and (p x y u) (p y z v) (p x v w)) (p u z w))
	     (-> (and (p x y u) (p y z v) (p u z w)) (p x v w))
	     (<- (query) (and (search (p (k x) x (k x))) !)))
	   :incomplete-inference t)
  (query))

(defun chang&lee-example-2 nil
  (print 'chang&lee-example-2)
  (format t "~&In an associative system with an identity element, if the square~%of every element is the identity, the system~%is commutative.")
  (program '(u v w x y z)
	   '((p e x x)
	     (p x e x)
	     (p x x e)
	     (p a b c)
	     (-> (and (p x y u) (p y z v) (p x v w)) (p u z w))
	     (-> (and (p x y u) (p y z v) (p u z w)) (p x v w))
	     (<- (query) (and (search (p b a c)) !)))
	   :incomplete-inference t)
  (query))

(defun chang&lee-example-3 nil
  (print 'chang&lee-example-3)
  (format t "~&In a group the left identity element is also a right identity.")
  (program '(u v w x y z)
	   '((p e x x)
	     (p (i x) x e)
	     (-> (and (p x y u) (p y z v) (p x v w)) (p u z w))
	     (-> (and (p x y u) (p y z v) (p u z w)) (p x v w))
	     (<- (query) (and (search (p a e a)) !)))
	   :incomplete-inference t)
  (query))

(defun chang&lee-example-4 nil
  (print 'chang&lee-example-4)
  (format t "~&In a group with left inverse and left identity every element~%has a right inverse.")
  (program '(u v w x y z)
	   '((p e x x)
	     (p (i x) x e)
	     (-> (and (p x y u) (p y z v) (p x v w)) (p u z w))
	     (-> (and (p x y u) (p y z v) (p u z w)) (p x v w))
	     (<- (query) (and (search (p a x e)) !)))
	   :incomplete-inference t)
  (query))

(defun chang&lee-example-5 nil
  (print 'chang&lee-example-5)
  (format t "~&If S is a nonempty subset of a group such that if x,y belong to S,~%then x*y^-1 belongs to S, then the identity e belongs to S.")
  (program '(u v w x y z)
	   '((p e x x)
	     (p x e x)
	     (p x (i x) e)
	     (p (i x) x e)
	     (s a)
	     (-> (and (s x) (s y) (p x (i y) z)) (s z))
	     (-> (and (p x y u) (p y z v) (p x v w)) (p u z w))
	     (-> (and (p x y u) (p y z v) (p u z w)) (p x v w))
	     (<- (query) (and (search (s e)) !)))
	   :incomplete-inference t)
  (query))

(defun chang&lee-example-6 nil
  (print 'chang&lee-example-6)
  (format t "~&If S is a nonempty subset of a group such that if x,y belong to S,~%then x*y^-1 belongs to S, then S contains x^-1 whenever it contains x.")
  (program '(u v w x y z)
	   '((p e x x)
	     (p x e x)
	     (p x (i x) e)
	     (p (i x) x e)
	     (s a)
	     (-> (and (s x) (s y) (p x (i y) z)) (s z))
	     (-> (and (p x y u) (p y z v) (p x v w)) (p u z w))
	     (-> (and (p x y u) (p y z v) (p u z w)) (p x v w))
	     (<- (query) (and (search (s (i a))) !)))
	   :incomplete-inference t)
  (query))

(defun chang&lee-example-7 nil
  (print 'chang&lee-example-7)
  (format t "~&If a is a prime and a = b^2/c^2 then a divides b.")
  (program '(u x y z)
	   '((p a)
	     (m a (s c) (s b))
	     (m x x (s x))
	     (or (~m x y z) (m y x z))
	     (or (~m x y z) (d x z))
	     (or (~p x) (~m y z u) (~d x u) (d x y) (d x z))
	     (<- (query) (and (search (d a b)) !))))
  (query))

(defun chang&lee-example-8 nil
  (print 'chang&lee-example-8)
  (format t "~&Any number greater than 1 has a prime divisor.")
  (program '(x y z)
	   '((l 1 a)
	     (d x x)
	     (or (p x) (d (g x) x))
	     (or (p x) (l 1 (g x)))
	     (or (p x) (l (g x) x))
	     (or (~p x) (~d x a))		; negation of theorem
	     (or (~d x y) (~d y z) (d x z))
	     (or (~l 1 x) (~l x a) (p (f x)))
	     (or (~l 1 x) (~l x a) (d (f x) x))
	     (<- (query) (and (search (and (p x) (d x a))) !))))
  (query))

(defun chang&lee-example-9 nil
  (print 'chang&lee-example-9)
  (format t "~&There exist infinitely many primes.")
  (program '(x y)
	   '((l x (f x))
	     (~l x x)
	     (or (~l x y) (~l y x))
	     (or (~d x (f y)) (l y x))
	     (or (p x) (d (h x) x))
	     (or (p x) (p (h x)))
	     (or (p x) (l (h x) x))
	     (or (~p x) (~l a x) (l (f a) x))	; negation of theorem
	     (<- (query) (and (search (and (p x) (l a x) (~l (f a) x))) !))))
  (query))
