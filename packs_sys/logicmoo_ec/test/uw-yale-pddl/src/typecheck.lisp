(in-package "PDDL")

; Type checkers for literals and other pc-type things.

(defun skeleton-parse (skel dom)
   (multiple-value-bind (args argtypes flg)
                        (typed-list-split skel #'qvar-p t dom)
      (do ((al args (cdr al))
           (duplicate-vars '()))
          ((null al)
           (values argtypes
                   (note-duplicate-args duplicate-vars
                       flg)))
	(cond ((member (car al) (cdr al) :test #'equal)
	       (pushnew (car al) duplicate-vars))))))

; Sometimes we want to rename all the variables in a structure
; as we go, so that no two variables have the same name.  We do this
; conservatively, so a variable is renamed only if it conflicts with 
; a previously encountered variable.  Variable-translation tables 
; (vartranstabs) are passed up and down to accomplish this.  These
; are lists of "vartranses" with old names and new.
; The feature is turned off by passing NOTRANS instead of an a-list.

; Table is a list of these
(defstruct (vartrans (:constructor make-vartrans (sym active trans)))
   sym
   active   ; nil if variable no longer in scope; however, it still prevents
            ; this variable from being used elsewhere
   trans)

;; Procedures typically return 3 values: flagged form, internal form,
;; and revised vartranstab.

(defun goal-parse (gd qvarbdgs vartranstab dom)
   (cond ((consp gd)
	  (multiple-value-bind (gd flg)
			       (list-smooth gd #'any)
	     (case (car gd)
		(and
		 (multiple-value-bind (ga-flg ga-int vt)
				      (goal-args-parse (cdr gd) qvarbdgs
						       vartranstab dom)
		    (values `(and ,@ga-flg ,@flg)
			    `(and ,@ga-int)
			    vt)))
		 (exists
		  (multiple-value-bind (ex-flg ex-int vt)
				       (quantified-goal-parse
					   gd qvarbdgs vartranstab dom)
		     (values (verify-requirement ex-flg
						 ':existential-preconditions
						 dom)
			     ex-int
			     vt)))
		 (forall
		  (multiple-value-bind (fa-flg fa-int vt)
				       (quantified-goal-parse
					  gd qvarbdgs vartranstab dom)
		     (values (verify-requirement 
			        fa-flg ':universal-preconditions dom)
			     fa-int
			     vt)))
                 (or
		  (multiple-value-bind (al-flg al-int vt)
				       (goal-args-parse (cdr gd) qvarbdgs
							vartranstab dom)
                     (values (verify-requirement
			        `(or ,@al-flg ,@flg)
				':disjunctive-preconditions
				dom)
			     `(or ,@al-int)
			     vt)))
		 (not
		  (cond ((null (cdr gd))
			 (values (flagexp "No arguments" gd)
				 gd
				 vartranstab))
			(t
			 (multiple-value-bind (ng-flg ng-int vt)
					      (not-goal-arg-parse
					            (cadr gd)
						    qvarbdgs vartranstab dom)
			    (values `(not ,ng-flg
					  ,@(cond ((not (null (cddr gd)))
						   `(,(flagexp
						         "Excess arguments"
							 (cddr gd))))
						  (t '()))
					  ,@flg)
				    `(not ,ng-int)
				    vt)))))
                 (imply
		  (multiple-value-bind
		           (ant-flg ant-int vt)
			   (cond ((null (cdr gd))
				  (values (flagexp
					     "Missing first argument" "__")
					  nil
					  vartranstab))
				 (t
				  (goal-parse (cadr gd) qvarbdgs
					      vartranstab dom)))
		     (multiple-value-bind
		              (cnq-flg cnq-int vt)
			      (cond ((null (cddr gd))
				     (values (flagexp
					       "Missing second argument" "__")
					     nil
					     vt))
				    (t
				     (goal-parse (caddr gd) qvarbdgs vt dom)))
			 (values (verify-requirement
				    `(imply ,ant-flg ,cnq-flg
				      ,@(cond ((null (cdddr gd))
					       '())
					      (t
					       (flagexp
						 "Excess arguments"
						 (cdddr gd))))
				      ,@flg)
				    ':disjunctive-preconditions
				    dom)
				 `(imply ,ant-int ,cnq-int)
				 vt))))
                 (t
                  (atomic-formula-parse gd qvarbdgs vartranstab dom)))))
         (t
          (values (flagexp "Illegal goal" gd) gd vartranstab))))
                   
(defun not-goal-arg-parse (notarg qvarbdgs vartranstab dom)
   (cond ((domain-declares-requirement dom ':disjunctive-preconditions)
          (goal-parse notarg qvarbdgs vartranstab dom))
         (t
          (literal-parse notarg qvarbdgs nil vartranstab dom))))

(defun quantified-goal-parse (qgoal qvarbdgs vartranstab dom)
   (multiple-value-bind (new-qvarbdgs var-flg vartranstab)
                        (qvar-list-parse (cadr qgoal) qvarbdgs vartranstab dom)
      (multiple-value-bind (b-flg b-int vt)
			   (cond ((null (cddr qgoal))
				  (values (flagexp
					     "Missing quantified goal body"
					     "__")
					  nil
					  vartranstab))
				 (t
				  (goal-parse (caddr qgoal)
					      (append new-qvarbdgs qvarbdgs)
					      vartranstab dom)))
       (values `(,(car qgoal)
		 ,var-flg
		 ,b-flg
		 ,@(cond ((> (length qgoal) 3)
			  `(,(flagexp "Excess arguments" (cdddr qgoal))))
			 (t '())))
	       `(,(car qgoal) ,new-qvarbdgs ,b-int)
	       (vartranstab-deactivate vt new-qvarbdgs)))))

(defun goal-args-parse (gd-args qvarbdgs vartranstab dom)
   (let ((al-int '()) (al-flg '()) a-int a-flg)
      (dolist (x gd-args)
	 (multiple-value-setq (a-flg a-int vartranstab)
			      (goal-parse x qvarbdgs vartranstab dom))
	    (push a-int al-int)
	    (push a-flg al-flg))
      (values (nreverse al-flg)
	      (nreverse al-int)
	      vartranstab)))

(defun effect-parse (eff qvarbdgs vartranstab dom)
   (cond ((consp eff)
	  (multiple-value-bind (eff flg)
			       (list-smooth eff #'any)
             (case (car eff)
                (not
		 (cond ((null (cdr eff))
			(values (flagexp "No arguments" eff)
				eff
				vartranstab))
		       (t
			 (multiple-value-bind (a-flg a-int vt)
					      (atomic-formula-parse
					          (cadr eff) qvarbdgs
						  vartranstab dom)
			    (values `(not ,a-flg
					  ,@(cond ((not (null (cddr eff)))
						   `(,(flagexp
						         "Excess arguments"
							 (cddr eff))))
						  (t '()))
					  ,@flg)
				    `(not ,a-int)
				    vt)))))
                (and
		 (let ((cl-int '()) (cl-flg '())
		       c-int c-flg)
		    (dolist (e (cdr eff))
		       (multiple-value-setq (c-flg c-int vartranstab)
					    (effect-parse e qvarbdgs
							  vartranstab dom))
		       (push c-int cl-int)
		       (push c-flg cl-flg))
		    (values
		       `(and ,@(nreverse cl-flg) ,@flg)
		       `(and ,@(nreverse cl-int))
		       vartranstab)))
                (forall
		 (multiple-value-bind (g-flg g-int vt)
				      (quantified-effect-parse
				          eff qvarbdgs vartranstab dom)
                    (values (verify-requirement
			       g-flg ':conditional-effects dom)
			    g-int
			    vt)))
                (when
		 (multiple-value-bind (w-flg w-int vt)
				      (when-parse
				         eff qvarbdgs vartranstab dom)
                    (values (verify-requirement
			       w-flg ':conditional-effects dom)
			    w-int
			    vt)))
                (change
		 (multiple-value-bind (c-flg c-int vt)
				      (change-parse
				         eff qvarbdgs vartranstab dom)
		    (values (verify-requirement
			       c-flg ':fluents dom)
			    c-int
			    vt)))
                (t
                 (atomic-formula-parse eff qvarbdgs vartranstab dom)))))
         (t
          (values (flagexp "Impossible effect " eff)
		  eff
		  vartranstab))))
                
(defun quantified-effect-parse (eff qvarbdgs vartranstab dom)
  (cond ((null (cdr eff))
	 (values
	    (flagexp "Quantifier with no variables"
		     eff)
	    eff
	    vartranstab))
        (t
         (multiple-value-bind (newvarbdgs flg-vars vt)
                              (qvar-list-parse (cadr eff) qvarbdgs
					       vartranstab dom)
	    ;(format t "after qvar-list-parse, vt = ~s~%" vt)
            (cond ((cddr eff)
		   (multiple-value-bind (e-flg e-int vt)
					(effect-parse (caddr eff)
						      (append newvarbdgs
							      qvarbdgs)
						      vt dom)
		      ;(format t "vt = ~s~%" vt)
                      (values `(forall ,flg-vars
				       ,e-flg
				       ,@(cond ((null (cdddr eff))
						'())
					       (t
						`(,(flagexp "Excess arguments"
							    (cdddr eff))))))
			      `(forall ,newvarbdgs ,e-int)
			      (vartranstab-deactivate vt newvarbdgs))))
                  (t
                   (values `(flagexp "Quantifier with no body"
				    `(forall ,flg-vars))
			   `(forall ,newvarbdgs nil)
			   (vartranstab-deactivate vt newvarbdgs))))))))

(defun when-parse (eff qvarbdgs vartranstab dom)
   (cond ((null (cdr eff))
	  (values
	     (flagexp "Missing arguments" eff)
             '(when)
	     vartranstab))
         (t
	  (multiple-value-bind (g-flg g-int vt)
			       (goal-parse (cadr eff) qvarbdgs vartranstab dom)
	     (multiple-value-bind (e-flg e-int vt)
				  (cond ((null (cddr eff))
				         (values (flagexp
						    "Missing effect in when"
						    "__")
						 nil
						 vt))
					(t (effect-parse
                                              (caddr eff) qvarbdgs vt dom)))
                (values `(when ,g-flg ,e-flg)
			`(when ,g-int ,e-int)
			vt))))))

(defun change-parse (eff qvarbdgs vartranstab dom)
   (cond ((null (cdr eff))
          (values (flagexp "Missing arguments" eff)
		  '(change)
		  vartranstab))
         (t
	  (multiple-value-bind (f-flg f-int vt)
			       (term-parse
				   (cadr eff) fluent-type* qvarbdgs vartranstab
				   nil dom t)
	     (multiple-value-bind (v-flg v-int vt)
				  (cond ((cddr eff)
					 (term-parse
					    (caddr eff) expression-type*
					    qvarbdgs vt nil dom t))
					(t
					 (values (flagexp "Missing argument"
							  "__")
						 nil
						 vt)))
                (values `(change ,f-flg ,v-flg)
			`(change ,f-int ,v-int)
			vt))))))

; new version returns only new qvars.  Could and should eventually
; flush qvarbdgs arg. completely.

(defun qvar-list-parse (vardecls qvarbdgs vartranstab dom)
                       (declare (ignore qvarbdgs))
   (multiple-value-bind (vars types flg)
                        (typed-list-split vardecls #'qvar-p nil dom)
      (let ((newvars '())
	    newv)
	 (dolist (v vars)
	    (multiple-value-setq (newv vartranstab)
				 (new-var-trans (qvar-sym v)
						vartranstab))
	    (push newv newvars))
         (values
	    ;(nconc  ... qvarbdgs)
	    (mapcar #'(lambda (v ty)
				(make-pddl-bdg
				   :sym v
				   :val ty
				   :domain nil))
			   (nreverse newvars)
			   types)
            flg
	    vartranstab))))

(defun new-var-trans (sym vartranstab)
   (cond ((and vartranstab (not (consp vartranstab)))
	  (values sym vartranstab))
	 (t
	  (let ((i 0) (newsym sym) (ss (symbol-name sym)))
	     (loop
		(cond ((do ((vtl vartranstab (cdr vtl)))
			   ((or (null vtl)
				(or (eq (vartrans-sym (car vtl))
					newsym)
				    (eq (vartrans-trans (car vtl))
					newsym)))
			    (null vtl)))
			      ;(not (assoc newsym vt :test #'eq))
		       (return
			  (values newsym
				  (cons (make-vartrans sym t newsym)  
					vartranstab)))))
		(setq newsym (intern (concatenate 'string
						  ss "," (format nil "~s" i))))
		(setq i (+ i 1)))))))

; Find the first vartrans for each element of bdgs, and deactivate it
(defun vartranstab-deactivate (vt bdgs)
   (cond ((consp vt)
	  (dolist (b bdgs)
	     (do ((vtl vt (cdr vtl)))
		 ((or (null vtl)
		      (and (vartrans-active (car vtl))
			   (eq (vartrans-trans (car vtl))
			       (pddl-bdg-sym b))))
		  (cond ((null vtl)
			 (cerror "I'll overlook it"
				 "No active vartrans entry for ~s" b))
			(t
			 (setf (vartrans-active (car vtl))
			       nil))))))))
   vt)

(defun trans-lookup (varsym vartranstab)
   (cond ((and vartranstab (not (consp vartranstab)))
	  varsym)
	 (t
	  (do ((vtl vartranstab (cdr vtl)))
	      ((or (null vtl)
		   (and (vartrans-active (car vtl))
			(eq (vartrans-sym (car vtl))
			    varsym)))
	       (cond ((null vtl)
		      varsym)
		     (t (vartrans-trans (car vtl)))))))))
; If we don't find it, the variable is unbound, and we'll catch an
; error soon.

(defun literal-parse (lit qvarbdgs vartranstab decl-consts dom)
  (multiple-value-bind (lit flg)
                       (list-smooth lit #'any)
     (let ((fctr (car lit)))
	(cond ((eq fctr 'suppress-syntax-check)
               (values (cadr lit) (cadr lit) vartranstab))
              ((eq fctr 'not)
	       (multiple-value-bind (l-flg l-int vt)
				    (literal-parse (cadr lit) qvarbdgs
				       vartranstab decl-consts dom)
	          (values `(not ,l-flg ,@flg)
			  `(not ,l-int)
			  vt)))
              (t
	       (multiple-value-bind (a-flg a-int vt)
				    (clean-atomic-formula-parse
				        lit qvarbdgs
					vartranstab decl-consts dom)
                  (values `(,@a-flg ,@flg)
			  a-int
			  vt)))))))

(defun atomic-formula-parse (fmla qvarbdgs vartranstab dom)
   (multiple-value-bind (fmla flg)
                        (list-smooth fmla #'any)
      (multiple-value-bind (f-flg f-int vt)
			   (clean-atomic-formula-parse
			       fmla qvarbdgs vartranstab nil dom)
          (values `(,@f-flg ,@flg)
		  f-int
		  vt))))

; If decl-consts is t, declare unknown constants as you find them.
(defun clean-atomic-formula-parse (fmla qvarbdgs vartranstab decl-consts dom)
   (let ((fctr (car fmla)))
      (cond ((symbolp fctr)
	     (let ((fbdg (find-domain-bdg fctr dom)))
		(cond ((or (not fbdg)
                           (pddl-bdg-unbound fbdg))
                       (values `(,(flagexp "Undeclared predicate" fctr)
                                 ,@(cdr fmla))
                               fmla
                               vartranstab))
		      ((functor-p (pddl-bdg-val fbdg))
		       (let ((rangetype (functor-rangetype
					    (pddl-bdg-val fbdg))))
			  (cond ((is-subtype rangetype proposition-type*)
				 (fctr-args-parse
				     fctr (cdr fmla)
				     (functor-argtypes (pddl-bdg-val fbdg))
				     qvarbdgs vartranstab 
				     decl-consts dom nil))
				(t
				 (values `(,(flagexp "Functor not a predicate"
						     fctr)
					   ,@(cdr fmla))
					 fmla
					 vartranstab)))))
                      ((pddl-type-p (pddl-bdg-val fbdg))
		       (fctr-args-parse fctr (cdr fmla)
					(list object-type*)
					qvarbdgs vartranstab
					decl-consts dom nil))
		      (t
		       (values `(,(flagexp "Nonfunctor in predicate position"
					   fctr)
				 ,@(cdr fmla))
			       fmla
			       vartranstab)))))
	    (t
	     (values `(,(flagexp "Nonsymbol in predicate position"
				 fctr)
		       ,@(cdr fmla))
		     fmla
		     vartranstab)))))
                         
(defun fctr-args-parse (fctr args argtypes qvarbdgs vartranstab 
			decl-consts dom eval-context)
   (multiple-value-bind (al-flg al-int vt)
			(args-parse
			    args
			    argtypes qvarbdgs vartranstab 
			    decl-consts dom eval-context)
      (values `(,fctr ,@al-flg)
	      `(,fctr ,@al-int)
	      vt)))

; eval-context is t in evaluation contexts, where complex terms are
; allowed. (This is superseded if type is an expression-type, which is
; probably a better way to handle all cases.)
(defun term-parse (term type qvarbdgs vartranstab decl-consts dom eval-context)
   (cond ((symbolp term)
          (let ((symtype (symbolic-term-type term dom)))
             (cond ((null symtype)
		    (cond (decl-consts
			   (let ((cbdg (place-domain-bdg term dom)))
			      (setf (pddl-bdg-val cbdg)
				    (make-constant
					    :name term
					    :type type))
			      (values term term vartranstab)))
			  (t
			   (values (flagexp "Undefined term" term)
				   term
				   vartranstab))))
                   ((pddl-bdg-p symtype)
		    (values (format-flg term
					"Illegal term (denotes ~s)"
					(pddl-bdg-val symtype))
			    term
			    vartranstab))
                   ((is-subtype symtype type)
                    (values term term vartranstab))
                   (t
                    (values (wrong-type-arg term symtype type)
			    term
			    vartranstab)))))
         ((qvar-p term)
	  (let ((realsym (trans-lookup (qvar-sym term) vartranstab)))
             (let ((b (qvar-lookup realsym qvarbdgs)))
		(cond (b
		       (values (cond ((is-subtype (pddl-bdg-val b) type)
				      term)
				     (t
				      (wrong-type-arg
				         term (pddl-bdg-val b) type)))
			       (make-qvar realsym)
			       vartranstab))
		      (t
		       ;(cerror "Duh" "undecl ~s" term)
		       (values (flagexp "Undeclared quantified variable"
					term)
			       term
			       vartranstab))))))
         ((consp term)
          (cond ((or eval-context
		     (is-subtype type expression-type*))
                 (functional-term-parse term type qvarbdgs vartranstab dom))
                (t
		 (values (flagexp "Functional term in illegal context"
				  term)
			 term
			 vartranstab))))
         ((const-of-type term type)
	  (values term term vartranstab))
         (t
	  (values (format-flg term "Constant of wrong type -- wanted ~s" type)
		  term
		  vartranstab))))

(defun functional-term-parse (term type qvarbdgs vartranstab dom)
   (multiple-value-bind (term flg)
			(list-smooth term #'any)
      (let ((fctr (car term)))
         (cond ((eq fctr 'sum)
		(sum-parse term type qvarbdgs vartranstab dom))
	       ((eq fctr 'constant)
		(constant-parse term type qvarbdgs vartranstab dom))
	       ((symbolp fctr)
		(let ((fbdg (find-domain-bdg fctr dom)))
		   (cond ((or (not fbdg)
                              (pddl-bdg-unbound fbdg))
			  (values (flagexp "Undefined functor"
					   `(,@term ,@flg))
				  term
				  vartranstab))
			 ((functor-p (pddl-bdg-val fbdg))
			  (let ((fdef (pddl-bdg-val fbdg)))
			     (multiple-value-bind (t-flg t-int vt)
						  (fctr-args-parse
						     fctr (cdr term)
                                                     (functor-argtypes fdef)
                                                     qvarbdgs vartranstab
						     nil dom t)
				(values (cond ((is-subtype
						  (functor-rangetype fdef)
						  type)
					       t-flg)
					      (t
					       (wrong-type-arg
						  t-flg
						  (functor-rangetype fdef)
						  type)))
					t-int
					vt))))
                         (t
			  (values (flagexp "Nonfunctor in functional position"
					   `(,(car term)
					     ,@(cdr term)
					     ,@flg))
				  term
				  vartranstab)))))
               (t
		(values (flagexp "Nonsymbol in functional position"
				 `(,@term ,@flg))
			term
			vartranstab))))))
                       
(defun sum-parse (fmla type qvarbdgs vartranstab dom)
   (cond ((= (length fmla) 4)
	  (multiple-value-bind (new-qvarbdgs var-flg vt)
			       (qvar-list-parse (cadr fmla) qvarbdgs
						vartranstab dom)
	     (let ((all-qvarbdgs (append new-qvarbdgs qvarbdgs)))
		(multiple-value-bind (g-flg g-int vt)
				     (goal-parse (caddr fmla)
						 all-qvarbdgs vt dom)
		   (multiple-value-bind (s-flg s-int vt)
					(term-parse (cadddr fmla)
						    type
						    all-qvarbdgs vt
						    nil dom t)
		      (values (verify-requirement
			         `(sum ,var-flg ,g-flg ,s-flg)
				 ':fluents dom)
			      `(sum ,new-qvarbdgs ,g-int ,s-int)
			      (vartranstab-deactivate vt new-qvarbdgs)))))))
	 (t
	  (values (verify-requirement
		     (flagexp "Wrong number of arguments"
			      fmla)
		     ':fluents dom)
		  fmla
		  vartranstab))))

(defun constant-parse (fmla type qvarbdgs vartranstab dom)
   (cond ((= (length fmla) 2)
	  (cond ((is-subtype type fluent-type*)
		 (let ((bty (fluent-type-base type)))
		    (multiple-value-bind (c-flg c-int vartranstab)
					 (functional-term-parse
					     (cadr fmla)
					     bty qvarbdgs vartranstab dom)
		       (values `(constant ,c-flg)
			       `(constant ,c-int)
			       vartranstab))))
		(t
		 (values (flagexp "Fluent illegal in this context"
				  fmla)
			 fmla
			 vartranstab))))
	 (t
	  (values (flagexp "Wrong number of arguments" fmla)
		  fmla vartranstab))))

(defun args-parse (argl argtypes qvarbdgs vartranstab
		   decl-consts dom eval-context)
   (cond ((and (not (null argtypes))
               (eq (car argtypes) '&rest))
	  (let ((al-int '()) (al-flg '()) a-int a-flg)
	     (dolist (a argl)
		(multiple-value-setq (a-flg a-int vartranstab)
				     (term-parse
				        a (cadr argtypes)
					qvarbdgs vartranstab
					decl-consts dom eval-context))
		(push a-int al-int)
		(push a-flg al-flg))
	     (values (nreverse al-flg)
		     (nreverse al-int)
		     vartranstab)))
         ((null argl)
          (cond ((null argtypes)
		 (values '() '() vartranstab))
                (t
		 (values `(,(format-flg (mapcar #'pddl-type-defn argtypes)
					"Missing ~s args of types"
					(length argtypes)))
			 '()
			 vartranstab))))
         ((null argtypes)
	  (values `(,(flagexp "Excess args" argl))
		  '()
		  vartranstab))
         (t
	  (multiple-value-bind (a-flg a-int vt)
			       (term-parse (car argl) (car argtypes)
					   qvarbdgs vartranstab
					   decl-consts dom eval-context)
	     (multiple-value-bind (al-flg al-int vt)
				  (args-parse (cdr argl) (cdr argtypes)
					      qvarbdgs vt
					      decl-consts dom eval-context)
		(values `(,a-flg ,@al-flg)
			`(,a-int ,@al-int)
			vt))))))

; Returns nil if symbol is unbound; bdg if not a term
(defun symbolic-term-type (term dom)
   (let ((tbdg (find-domain-bdg term dom)))
      (cond ((or (not tbdg) (pddl-bdg-unbound tbdg))
             nil)
            (t
             (let ((val (pddl-bdg-val tbdg)))
                (cond ((constant-p val)
                       (constant-type val))
                      ((domain-var-p val)
                       (domain-var-type val))
                      (t tbdg)))))))

(defun wrong-type-arg (arg argtype wanted)
  (format-flg arg
     "Wrong type arg -- wanted ~s, got ~s//"
     wanted argtype))

(defun note-duplicate-args (v flg)
   (cond ((null v) flg)
         (t (format-flg flg
               "Duplicate args (~s)" v))))

(defun note-decl-problems (duplicate-names badtypes flg)
   (cond ((not (null duplicate-names))
;	  (cerror "..." "Dups: ~s" duplicate-names)
          (setq flg (flagexp
                        (with-output-to-string (srm)
                           (format srm "Attempt to redeclare existing names ")
			   (dolist (dup duplicate-names)
                              (format srm "~s (domain: ~s)// "
                                      (pddl-bdg-sym dup)
                                      (domain-name (pddl-bdg-domain dup)))))
                        flg))))
   (cond ((null (null badtypes))
          (setq flg (flagexp
                       (format nil "Bad types: ~s " badtypes)
                       flg))))
   flg)

; badinits is a list of domain-vars
(defun note-bad-inits (badinits flg)
  (cond ((null badinits)
         flg)
        (t
         (flagexp
             (with-output-to-string (srm)
                (format srm "Domain var(s) bound to object of wrong type ")
                (dolist (bi badinits)
                   (format srm "~s ~s (want ~)/ "
                           (domain-var-name bi)
                           (domain-var-val bi)
                           (domain-var-type bi))))
             flg))))

; Split a typed list into a list of exps and a list of types (two
; lists have same length).  Return as values explist, typelist, 
; and flagged input.  pred tests if exp is legal.
(defun typed-list-split (tl pred allow-rest dom)
   (multiple-value-bind (l flg)
			(typed-list-map
			    #'(lambda (el ty)
				 (cond ((eq el '&rest)
                                        (values (list (list el el))
                                                (list el)))
                                       ((funcall pred el)
					(values (list (list el ty))
						(list el)))
				       (t
					(values '()
						(list (flagexp "Illegal in this context"
							       el))))))
			    tl allow-rest dom)
      (values (mapcar #'car l)
	      (mapcar #'cadr l)
	      flg)))

; Apply fcn to all elements of tl and link results together.
; Fcn returns two values: new elements of output list, and new elements
; of flg output.
(defun typed-list-map (fcn tl allow-rest dom)
   (multiple-value-bind (tl flg)
			(list-smooth tl #'any)
      (labels ((domap (r allow-rest)
		  (cond ((null r)
			 (values '() object-type* '()))
			((eq (car r) '-)
			 (cond ((cdr r)
				(multiple-value-bind (outl ty flg)
						     (domap (cddr r)
                                                            allow-rest)
						     (declare (ignore ty))
				   (cond ((is-type-exp (cadr r))
                                          (multiple-value-bind
                                                 (typ flg-typ)
                                                 (type-eval (cadr r) dom)
					     (values outl typ
                                                  `(- ,flg-typ
                                                      ,@flg))))
					 (t
					  (values outl object-type*
						  `(- ,(flagexp "Illegal type"
								(cadr r))
						    ,@flg))))))
			       (t
				(values '() object-type*
					`(,(flagexp
					      "Hyphen followed by nothing" '-)
					  ,@flg)))))
			(t
			 (multiple-value-bind (outl ty flg)
					      (domap (cdr r)
                                                     (and allow-rest
                                                          (not (eq (car r)
                                                                   '&rest))))
			    (map-next (car r) ty outl flg)))))
	       (map-next (elt ty outl flg)
		  (cond ((and (eq elt '&rest)
			      (not allow-rest))
			 ; don't call fcn
			 (values outl ty
				 `(,(flagexp "out of place"
					     '&rest)
				   ,@flg)))
			(t
			 (multiple-value-bind (new-outl new-flg)
					      (funcall fcn elt ty)
			    (values (append new-outl outl)
				    ty
				    (append new-flg flg)))))))
	  (multiple-value-bind (outl ty flg)
			       (domap tl allow-rest)
			       (declare (ignore ty))
             (cond ((and (member '- tl)
                         (not (domain-declares-requirement dom ':typing)))
                    (setq flg `(,(flagexp
                                    "Illegal for domain not declaring requirement :typing "
				    flg)))))
	     (values outl flg)))))

(defun is-type-exp (x)
   (or (symbolp x)
       (and (consp x)
	    (member (car x) '(either fluent expression)))))

; Returns type + flagged version
(defun type-eval (typexp dom)
   (cond ((symbolp typexp)
          (let ((tbdg (find-domain-bdg typexp dom)))
             (cond ((or (not tbdg)
                        (pddl-bdg-unbound tbdg))
                    (values object-type*
                            (flagexp "Undeclared type name"
				     typexp)))
                   ((pddl-type-p (pddl-bdg-val tbdg))
                    (values (pddl-bdg-val tbdg) typexp))
                   (t
                    (values object-type*
                            (format-flg typexp
					"Not a type (value ~s)"
					`(local ,(pddl-bdg-val tbdg))))))))
         ((atom typexp)
          (values object-type*
                  (flagexp "Meaningless type expression"
                           typexp)))
         ((eq (car typexp) 'either)
          (multiple-value-bind (argl flg-junk)
                               (list-smooth (cdr typexp) #'any)
             (let ((components '())
                   (flg-args '()))
                (dolist (a argl)
                   (multiple-value-bind (at a-flg)
                                        (type-eval a dom)
                      (push at components)
                      (push a-flg flg-args)))
                (setq components (reverse components))
                (setq flg-args (reverse flg-args))
		(values (make-either-type
                           :defn `(either ,@flg-args)
			   :components components
			   :parents components)
			`(either ,@flg-args ,@flg-junk)))))
         ((member (car typexp) '(fluent expression))
          (multiple-value-bind (argl flg-junk)
                               (list-smooth (cdr typexp) #'any)
             (cond ((null argl)
                    (values (cond ((eq (car typexp) 'fluent)
                                   fluent-type*)
                                  (t
                                   object-type*))
			    `(,(flagexp "Missing arguments" typexp)
                              ,@flg-junk)))
                   (t
                    (multiple-value-bind (at a-flg)
                                         (type-eval (car argl) dom)
                       (values (cond ((eq (car typexp) 'fluent)
                                      (make-fluent-type
                                          :defn `(fluent ,a-flg)
                                          :base at
					  :const-tester
					    (pddl-type-const-tester at)
                                          :parents (list fluent-type*)))
                                     (t
                                      (make-expression-type
                                          :defn `(expression ,a-flg)
                                          :actual at
					  :const-tester
					    (pddl-type-const-tester at)
                                          :parents (list expression-type*))))
                               `(,(car typexp)
                                 ,a-flg
                                 ,@(cond ((null (cdr argl)) '())
                                         (t
                                          `(,(flagexp "Excess arguments"
                                                      (cddr argl)))))
                                 ,@flg-junk)))))))
         (t
          (values object-type*
                  (flagexp "Meaningless type expression"
                           typexp)))))

(defun list-verify-requirement (flg reqname dom)
   (cond ((domain-declares-requirement dom reqname)
          flg)
         (t
          (list (format-flg flg
                   "Illegal for domain not declaring requirement ~s"
                   reqname)))))

(defun verify-requirement (flg reqname dom)
   (cond ((domain-declares-requirement dom reqname)
          flg)
         (t
          (format-flg flg
                 "Illegal for domain not declaring requirement ~s "
                 reqname))))

; Used to extract args from parsed expressions.  l may contain junk,
; but already flagged and therefore harmless.
(defun get-arg-or-empty (n l)
   (cond ((null l) '())
	 ((eq (car l) n) (cadr l))
	 ((null (cdr l)) '())
	 (t (get-arg-or-empty n (cddr l)))))

(defun get-field-or-empty (kw keyword-tab)
   (let ((p (assoc kw keyword-tab :test #'eq)))
      (cond (p (cadr p)) (t '()))))

(defun get-field (kw keyword-tab)
   (assoc kw keyword-tab :test #'eq))

(defun set-field (kw keyword-tab newval)
  (cons (list kw newval)
        (let ((p (get-field kw keyword-tab))) 
          (cond (p
		 (remove p keyword-tab :test #'eq :count 1))
                (t keyword-tab)))))

(defun keyword-list-smooth (l keywords required)
   (multiple-value-bind (l flg)
                        (list-smooth l #'any)
      (let ((items '()))
	 (do ((xl l (cddr xl)))
	     ((or (null xl)
		  (null (cdr xl)))
	      (values items
		      `(,@(reverse flg)
			,@(mapcan #'(lambda (r)
				       (cond ((get-field r items)
					      '())
					     (t
					      `(,(flagexp "Missing keyword"
							  r)))))
				  required)
			,@(cond ((null xl) '())
				((consp xl)
				 `(,(flagexp "Odd element of keyword list"
					     (car xl))))
				(t
				 `(,(flagexp "Junk at end of list . "
					     xl)))))))
	   (let ((k1 (car xl))
		 (v1 (cadr xl)))
	      (cond ((symbolp k1)
		     (dolist (kk keywords
			      (setq flg `(,(flagexp "Illegal keyword" k1)
					  ,@flg)))
			(cond ((eq k1 kk)
			       (push (list k1 v1) items)
			       (return))
			      ((and (consp kk)
				    (member k1 kk))
			       (push (list (car kk) v1) items)
			       (return)))))
		    (t
		     (setq flg `(,(flagexp "Nonsymbolic keyword" k1)
				 ,@flg)))))))))
                     
(defun list-smooth (l element-okay)
   (labels ((collect (l)
               (cond ((consp l)
                      (let ((a (car l)))
                        (let ((mh (pddl-form-handler a)))
                          (cond ((and mh (eq (car mh) 'macro))
                                 (multiple-value-bind (here there)
				                      (funcall (cadr mh)
                                                               a)
				                      (declare (ignore there))
                                   (multiple-value-bind
                                          (r flg)
					  (collect (append here (cdr l)))
                                      (values r flg))))
                                (t
				 (multiple-value-bind (r flg)
				                      (collect (cdr l))
				   (cond ((funcall element-okay a)
					  (values (cons a r)
						  flg))
					 (t
					  (values
                                             r
                                             (cons (flagexp
                                                      "Illegal in this context"
						      a)
                                                   flg))))))))))
                     ((null l)
                      (values '() '()))
                     (t
                      (values '() (list (flagexp "Junk at end of list"
                                                 l)))))))
      (collect l)))

(defun conj-append (c1 c2)
   (let ((cl1 (cond ((eq (car c1) 'and)
		     (cdr c1))
		    (t (list c1))))
	 (cl2 (cond ((eq (car c2) 'and)
		     (cdr c2))
		    (t (list c2)))))
      (let ((cl (append cl1 cl2)))
	 (cond ((= (length cl) 1) (car cl))
	       (t `(and ,@cl))))))
