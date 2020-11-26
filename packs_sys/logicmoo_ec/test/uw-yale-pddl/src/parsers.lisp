(in-package "PDDL")

; The way this works: domain-parse returns a (define (domain ...) ...) 
; expression 
; with syntactic errors annotated.  I use the morpheme "flg" ("flagged 
; subexps") to mean such an annotated expression.
; domain-parse has as side effect adding all
; the definitions it can decipher to the relevant tables (just as a
; production version would).
; The components are parsed in an order such that the side effects
; necessary to understand component X are parsed before component X.
; Hence the parse functions need only to be passed the domain in order
; to work; all the required information is already stored there.

(defun requirement-parse (req builtins implications flg-junk)
   (multiple-value-bind (flg-builtins built-doms)
                        (builtins-parse builtins)
      (multiple-value-bind (flg-implics implied-reqs)
	                   (implications-parse implications)
         (setf (requirement-builtins req) built-doms)
         (setf (requirement-implies req)
               implied-reqs)
         `(define (requirement ,(requirement-name req))
		  ,flg-builtins
		  ,flg-implics
		  ,@flg-junk))))

(defun domain-parse (dom parents require types constants domain-vars
		     predicates functors timeless safety axioms actions
                     methods flg-junk)
   (domain-reset dom) ; destroy *all* rule groups, among other things
   (multiple-value-bind (flg-parents pdl)
		        (parents-parse parents)
      (multiple-value-bind (flg-req reql)
			   (require-parse require)
	 (setf (domain-parents dom) pdl)
	 (setf (domain-requirements dom) reql)
	 (reset-ancestors dom)
         ; The order of var evals is significant, because earlier ones have
         ; side effects the later ones depend on:
         (let ((flg-types (mapcar #'(lambda (ty) (types-parse ty dom))
                                  types))
               (flg-constants (constant-lists-parse ':constants constants dom))
               (flg-domainvars (mapcar #'(lambda (dl) (domain-vars-parse dl dom))
                                       domain-vars))
	       (flg-predicates (predicates-parse predicates dom))
               (flg-functors (functors-parse functors dom))
	       (flg-timeless (timeless-parse timeless dom))
	       (flg-safety (safety-parse safety dom (own-rule-group dom)))
	       (flg-axioms (axioms-parse axioms dom (own-rule-group dom)))
	       (flg-actions (actions-parse actions dom))
	       (flg-methods (methods-parse methods dom (own-rule-group dom))))
	    (setf (domain-timeless dom) flg-timeless)
	    `(define (domain ,(domain-name dom))
		(:extends ,@flg-parents)
		(:requirements ,@flg-req)
		,@flg-types
		,@flg-constants
		,@flg-domainvars
		(:predicates ,@flg-predicates)
                ,@flg-functors
                (:timeless ,@flg-timeless)
		,@flg-safety
                ,@flg-axioms
                ,@flg-actions
                ,@flg-methods
                ,@flg-junk)))))

(defun addendum-parse (addname dom methods axioms safeties flg-junk)
   (domain-make-current dom)
   (let ((rg (domain-place-rule-group dom addname)))
      (let ((real-rg (cond ((rule-group-p rg) rg)
			   (t nil))))
         (cond (real-rg
                (setf (domain-generation dom)
                      (next-generation))
                (setf (rule-group-rules rg) '())))
	 (let ((flg-safety (safety-parse safeties dom real-rg))
	       (flg-axioms (axioms-parse axioms dom real-rg))
	       (flg-methods (methods-parse methods dom real-rg)))
            (setf (rule-group-generation rg)
                  (domain-generation dom))
	    `(define (addendum
		        ,(cond (real-rg addname)
			       (t
				(flag-wrong-type rg addname 'addendum))))
		(:domain ,(domain-name dom))
		,@flg-safety
		,@flg-axioms
		,@flg-methods
                ,@flg-junk)))))

(defun situation-parse (name dom obs inits flg-junk)
      (domain-make-current dom)
      (multiple-value-bind (sit flg-consts flg-inits)
			   (build-initial-situation name (list dom) obs inits)
	 `(define (situation ,(cond ((situation-p sit) name)
				    (t (flag-wrong-type
						sit name 'situation)))
	     (:domain ,(domain-name dom))
	     ,@flg-consts
	     (:inits ,@flg-inits)
	     ,@flg-junk))))

(defun problem-parse (name dom requirements sits obs inits goals expansions
                      lnth flg-junk)
   (domain-make-current dom)
   (multiple-value-bind (prob okay)
                        (get-global-pddl-symbol name)
      (cond (okay
	     (format *error-output*
		     "Warning -- redefining problem ~s~%"
		     name)))
      (setq prob (make-problem :name name))
      (set-global-pddl-symbol name prob)
      (multiple-value-bind (flg-req reql)
			   (require-parse requirements)
      	 (let ((base-sit (cond ((null sits) nil)
			       ((= (length sits) 1)
				(let ((b (find-domain-bdg-val (car sits) dom)))
				   (cond ((initial-situation-p b)
					  b)
					 (t
                                          (let ((b (get-global-pddl-symbol
                                                      (car sits))))
                                             (cond ((problem-p b)
                                                    (problem-sit b))
                                                   (t
					            (flagexp "Not a situation"
						             (car sits)))))))))
			       (t (flagexp "Illegal situation spec" sits)))))
	    (multiple-value-bind (sit flg-obs flg-inits)
				 (cond ((or (not base-sit)
					    (initial-situation-p base-sit))
					(cond ((and base-sit
						    (null obs)
						    (null inits)
						    (null requirements))
					       (values base-sit '() '()))
					      (t
					       (build-initial-situation
						  (intern
						        (concatenate 'string
							   (symbol-name name)
							   "-initial-situation"))
						  (cond (base-sit
							 (list dom
							       (initial-situation-domain
								base-sit)))
							(t (list dom)))
						  obs inits))))
				       (t
					(values nil nil nil)))
	        (cond ((and (initial-situation-p sit)
			    (initial-situation-p base-sit)
			    (not (eq sit base-sit)))
		       (setf (initial-situation-parent sit)
			     base-sit)))
		(multiple-value-bind (flg-goal flg-act flg-length)
				     (cond ((initial-situation-p sit)
					    (fill-problem
					        prob sit reql goals expansions
						lnth))
					   (t
					    (values nil nil nil)))
		     `(define (problem ,name)
			(:domain ,(domain-name dom))
			,@(cond ((null requirements) '())
				(t `((:requirements ,@flg-req))))
			,@flg-obs
			,@(cond (base-sit
				 `((:situation
				      ,(initial-situation-name
						 base-sit))))
				(t '()))
			(:inits ,@flg-inits)
			,@(cond (flg-goal `((:goal ,flg-goal)))
				(t '()))
			,@(cond (flg-act
				 `((:expansion ,flg-act)))
				(t '()))
			,@(cond ((not (or flg-goal flg-act))
				 `((flagexp "Must have goal or expansion"
					    "__")))
				(t '()))
			,@(cond (flg-length `((:length ,@flg-length)))
				(t
				 '()))
			,@flg-junk)))))))
		     
(defun fill-problem (prob sit reql goals expansions lnth)
  (let ((subdom (initial-situation-domain sit)))
    (setf (domain-requirements subdom) reql)
    (let ((flg-goal
	     (cond ((= (length goals) 1)
		    (goal-parse (car goals) '() 'notrans subdom))
		   ((null goals) nil)
		   (t (flagexp "Multiple goals"
			       goals)))))
      (multiple-value-bind (flg-act graph)
			   (cond ((= (length expansions) 1)
				  (action-spec-parse
				     (car expansions)
				     '() 'notrans subdom))
				 ((null expansions)
				  nil)
				 (t
				  (flagexp "Multiple expansions" expansions)))
	  (multiple-value-bind (l l-flg-junk)
			       (problem-length-parse lnth)
	     (setf (problem-sit prob) sit)
	     (cond ((not (flagged-subexpression-p flg-goal))
		    (setf (problem-goal prob) flg-goal)))
	     (cond ((not (flagged-subexpression-p flg-act))
		    (setf (problem-expansion prob)
			  graph)))
	     (setf (problem-length prob) l)
	     (values flg-goal flg-act `(,@l ,@l-flg-junk)))))))


(defun build-initial-situation (name doms obs inits)
   (let ((sit (domain-place-situation (car doms) name)))
      (let ((subdom (initial-situation-domain sit)))
	(setf (domain-parents subdom) doms)
	(domain-reset subdom)
	(reset-ancestors subdom)
	(let ((flg-consts (constant-lists-parse ':objects obs subdom)))
	    (multiple-value-bind (props prop-flg-junk)
				 (list-smooth inits #'consp)
	       (let ((lits (mapcar #'(lambda (a)
				         (literal-parse a '() t 'notrans subdom))
				   props)))
		  (setf (initial-situation-delta sit) lits)
		  (values sit flg-consts (append lits prop-flg-junk))))))))

(defun constant-lists-parse (argname cll dom)
  (mapcar #'(lambda (cl)
	       `(,argname ,@(constants-parse cl dom)))
	  cll))

; The "parsing" functions after this always return as their first value a 
; version of their input annotated with syntax-check comments.

(defun builtins-parse (builtins)
   (let ((bl '())
         (flg '()))
      (multiple-value-bind (bnames flg-junk)
                           (list-smooth builtins #'symbolp)
         (dolist (bn bnames)
            (let ((dom (try-domain-with-name bn nil)))
               (cond ((domain-p dom)
                      (push dom bl)
                      (setq flg `(,bn ,@flg)))
                     ((not dom)
                      (setq flg `(,(flagexp "Undefined domain"
                                            bn)
                                  ,@flg)))
                     (t
                      (setq flg `(,(flag-wrong-type dom bn 'domain)
                                  ,@flg))))))
         (values `(:builtins ,@(reverse flg) ,@flg-junk)
                 bl))))

(defun implications-parse (implics)
   (let ((il '())
         (flg '()))
      (multiple-value-bind (inames flg-junk)
                           (list-smooth implics #'symbolp)
         (dolist (impn inames)
            (let ((req (try-requirement-with-name impn nil)))
               (cond ((requirement-p req)
                      (push req il)
                      (setq flg `(,impn ,@flg)))
                     ((not req)
                      (setq flg `(,(flagexp "Undefined requirement"
                                            impn)
                                  ,@flg)))
                     (t
                      (setq flg `(,(flag-wrong-type
                                      req impn 'requirement)
                                  ,@flg))))))
         (values `(:implies ,@(reverse flg) ,@flg-junk)
                 il))))

(defun parents-parse (parents)
  (let ((pdl '())
        (flg '()))
     (multiple-value-bind (parents flg-junk)
			  (list-smooth parents #'symbolp)
	(dolist (par parents)
	   (let ((dom (try-domain-with-name par nil)))
	      (cond ((domain-p dom)
		     (setq pdl (adjoin dom pdl))
		     (setq flg `(,par ,@flg)))
                    ((not dom)
                     (setq flg `(,(flagexp "Parent undefined"
                                           par)
                                 ,@flg)))
		    (t
		     (setq flg `(,(flagexp "Parent not a domain" par)
				 ,@flg))))))
	(values `(,@(reverse flg) ,@flg-junk)
		pdl))))
		     
(defun require-parse (require)
   (let ((rql '())
	 ;(builtins '())
	 )
      (multiple-value-bind (require flg)
			   (list-smooth require #'symbolp)
	 (dolist (req require)
	    (multiple-value-bind (x okay)
				 (get-global-pddl-symbol req)
	       (cond (okay
		      (cond ((requirement-p x)
                             (setq rql (union (requirement-implications x)
                                              rql))
			     (setq flg `(,req ,@flg)))
			    (t (setq flg `(,(flagexp "Nonrequirement"
						     req)
					   ,@flg)))))
		     (t
		      (setq flg `(,(flagexp "Unknown requirement" req)
				  ,@flg))))))
	 (values (reverse flg)
		 rql))))

; You can't use typed-list-split or typed-list-map here because
; types defined earlier in the list are used later.
(defun types-parse (types dom)
   (labels ((types-track (tl new)
               (cond ((null tl)
                      (declare-types new object-type*)
                      '())
                     ((eq (car tl) '-)
                      (cond ((null (cdr tl))
                             `(,@(declare-types new object-type*)
                               -  ,(flagexp "Missing type" "__")))
			    (t
                             (multiple-value-bind (superty flg)
                                                  (type-eval (cadr tl)
                                                             dom)
                                `(,@(declare-types new superty)
                                  - ,flg
                                  ,@(types-track (cddr tl) '()))))))
                     ((or (symbolp (car tl))
			  (and (consp (car tl))
			       (symbolp (caar tl))))
                      `(,@(types-track (cdr tl) (cons (car tl) new))))
                     (t
                      `(,(flagexp "Illegal type name" (car tl))
                        ,@(types-track (cdr tl) new)))))
            (declare-types (new superty)
               (let ((flg-new '()))
                  (dolist (n new)
                     (push (declare-type n superty dom) flg-new))
                  flg-new)))
      `(:types ,@(types-track types '()))))

(defun declare-type (newt superty dom)
   (multiple-value-bind (newty const-tester)
			(cond ((consp newt)
			       (values (car newt) (cadr newt)))
			      (t
			       (values newt nil)))
      (let ((typbdg (place-domain-bdg newty dom)))
	 (cond ((pddl-bdg-unbound typbdg)
		(setf (pddl-bdg-val typbdg)
		      (make-pddl-type
			 :defn newty
			 :const-tester (eval const-tester)
			 :parents (list superty)))
		newty)
	       (t
		(format-flg newty
			    "Attempt to define named already defined ~
				  (value ~s)"
			    typbdg))))))

(defun constants-parse (constants dom)
		(multiple-value-bind (newconsts types flg)
				     (typed-list-split
                                         constants #'symbolp nil dom)
		   (do ((cl newconsts (cdr cl))
			(tl types (cdr tl))
			(duplicate-constants '()))
		       ((null cl)
			(note-decl-problems duplicate-constants '()
                           flg))
		      (let ((cbdg (place-domain-bdg (car cl) dom)))
			 (cond ((pddl-bdg-unbound cbdg)
				(let ((ctype (car tl)))
				   (setf (pddl-bdg-val cbdg)
					 (make-constant
					    :name (car cl)
					    :type ctype))))
			       (t
				(pushnew cbdg duplicate-constants)))))))

(defun domain-vars-parse (domain-vars dom)
		(multiple-value-bind (dvars types flg)
				     (typed-list-split
					domain-vars
					#'(lambda (x)
					     (or (symbolp x)
						 (and (consp x)
						      (symbolp (car x))
						      (not (null (cdr x)))
						      (null (cddr x)))))
                                        nil dom)
		   (do ((dvl dvars (cdr dvl))
			(tl types (cdr tl))
			(duplicate-names '())
			(badinits '()))
		       ((null dvl)
			(note-bad-inits badinits
			   (note-decl-problems duplicate-names '()
                              `(:domain-variables ,@flg))))
		      (multiple-value-bind
                                  (dvar init)
				  (cond ((symbolp (car dvl))
					 (values (car dvl)
                                                 unbound-sym-marker*))
					(t
					 (values (car (car dvl))
						 (cadr (car dvl)))))
			 (let ((dbdg (place-domain-bdg dvar dom)))
			    (cond ((or (pddl-bdg-unbound dbdg)
				       ; Okay if previously unbound,
                                       ; or bound to
				       ; a domain-var in an ancestor
				       (and (domain-var-p (pddl-bdg-val dbdg))
					    (not (eq (pddl-bdg-domain dbdg)
						     dom))))
				   (let ((dvtype (car tl)))
				      (setq dbdg
					    (place-local-domain-bdg
					       dvar dom))
				      (setf (pddl-bdg-val dbdg)
					    (make-domain-var
					       :name dvar
					       :val init
					       :type dvtype))
				      (cond ((not (const-of-type
						     init dvtype))
					     (push (pddl-bdg-val dbdg)
						   badinits)))))
				  (t
				   (pushnew dbdg duplicate-names))))))))

(defun predicates-parse (predicates dom)
   (multiple-value-bind (pred-defs flg)
                        (list-smooth predicates #'consp)
      (let ((preds-flg '())
            (duplicate-names '()))
         (dolist (skel pred-defs)
            (multiple-value-bind (argtypes skel-flg)
                                 (skeleton-parse (cdr skel) dom)
               (setq preds-flg `((,(car skel) ,@skel-flg)
                                 ,@preds-flg))
               (let ((pbdg (place-domain-bdg (car skel) dom)))
                  (cond ((pddl-bdg-unbound pbdg)
                         (setf (pddl-bdg-val pbdg)
                               (make-functor
                                     :name (car skel)
                                     :rangetype proposition-type*
                                     :argtypes argtypes)))
                        (t
                         (pushnew pbdg duplicate-names))))))
         (note-decl-problems duplicate-names '()
            `(,@(reverse preds-flg) ,@flg)))))

(defun functors-parse (funcls dom)
   (mapcar #'(lambda (funcl)
                (multiple-value-bind (empty flg)
				     (typed-list-map
				         #'(lambda (form ty)
					      (functor-parse form ty dom))
					 funcl nil dom)
				     (declare (ignore empty))
		   `(:functors ,@flg)))
	   funcls))

(defun functor-parse (frm typ dom)
   (cond ((symbolp (car frm))
	  (multiple-value-bind (args argtypes form-flg)
			       (typed-list-split (cdr frm) #'any t dom)
                               (declare (ignore args))
	     (let ((fbdg (place-domain-bdg (car frm) dom)))
                (cond ((pddl-bdg-unbound fbdg)
                       (setf (pddl-bdg-val fbdg)
                             (make-functor
                                :name (car frm)
                                :rangetype typ
                                :argtypes argtypes))
                       (values '() (list `(,(car frm) ,@form-flg))))
                      (t
		       (values '() (list `(,(format-flg (car frm)
						  "Already defined (value ~s)"
						  fbdg)))))))))
         (t
          (values '()
		  (list (flagexp "Nonsymbol in functor position"
                         frm))))))

(defun timeless-parse (timeless dom)
   (multiple-value-bind (literals flg)
                        (list-smooth timeless #'consp)
     `(,@(mapcar #'(lambda (lit)
                         (literal-parse lit '() nil 'notrans dom))
		 literals)
       ,@flg)))

(defun safety-parse (safety dom rg)
   (cond ((null safety) '())
         (t
	  (multiple-value-bind (safety flg)
			       (list-smooth safety #'consp)
             (list-verify-requirement
		`(,@(mapcar #'(lambda (s)
                                 (let ((sr
					(make-safety-condition
					   :goal (goal-parse
						    s '() 'notrans dom))))
                                    (cond (rg (push sr
                                                    (rule-group-rules rg))))))
				
			    safety)
		  ,@flg)
                ':safety-constraints dom)))))

(defun axioms-parse (axioms dom rg)
   (cond ((null axioms) '())
         (t
	  (multiple-value-bind (axioms flg)
			       (list-smooth axioms #'consp)
	     (let ((flg-all '()))
		(dolist (a axioms)
		   (multiple-value-bind (flg-a defn)
					(axiom-parse a dom)
		      (push flg-a flg-all)
                      (cond (rg
		             (add-to-rule-group defn rg)))))
                (let ((final-flg `(,@(reverse flg-all)
		                   ,@flg)))
                   (cond ((eq (domain-name dom) 'no-op)
                          final-flg)
                         (t
                          (list-verify-requirement
                             final-flg ':domain-axioms dom)))))))))

; axiom-body  is of form (:context ... :implies ...)
(defun axiom-parse (axiom-body dom)
   (multiple-value-bind (items bad-keyword-flg)
                        (keyword-list-smooth axiom-body
                                             '((:vars :parameters)
                                               (:antecedent :context)
                                               (:consequent :implies)
					       :procedure)
                                             '(:consequent))
      (multiple-value-bind (qvarbdgs vars-flg)
			   (qvar-list-parse
			      (get-field-or-empty ':vars items)
			      '() 'notrans dom)
	 (let ((af (get-field ':antecedent items))
	       (cf (get-field ':consequent items))
	       (pf (get-field ':procedure items)))
	    (let ((ant (cond (af
			      (conj-append
			         (goal-parse
				    (cadr af) qvarbdgs 'notrans dom)
				 `(and ,@(qvarbdgs-constraints qvarbdgs))))
			     (t
			      `(and ,@(qvarbdgs-constraints qvarbdgs))))))
	       (let ((axdef 
		      `(,@(cond ((null vars-flg) '())
				(t `(:vars ,vars-flg)))
			,@(cond ((equal ant '(and))
				 '())
				(t 
				 `(:context ,ant)))
			,@(cond (pf
				 `(:procedure ,(cadr pf)))
				(t '()))
			,@(cond (cf
				 `(:implies ,(literal-parse
						(cadr cf)
						qvarbdgs nil 'notrans dom)))
				(t '()))
			,@bad-keyword-flg)))
		 (values
		    `(:axiom ,@axdef)
		    (cond (pf
			   (make-procedural-axiom
			      :vars (get-arg-or-empty ':vars axdef)
			      :fcn (proc-compile
				      (get-arg-or-empty ':procedure axdef))
			      :antecedent (get-arg-or-empty ':context axdef)
			      :consequent (get-arg-or-empty ':implies
							    axdef)))
			  (t
			   (make-axiom
			      :vars (get-arg-or-empty ':vars axdef)
			      :antecedent (get-arg-or-empty ':context axdef)
			      :consequent (get-arg-or-empty ':implies axdef)))))))))))

(defun proc-compile (procdef)
   (cond ((atom procdef)
	  (eval procdef))
	 ((eq (car procdef) 'function)
	  (cond ((symbolp (cadr procdef))
		 (compile (cadr procdef))
		 (symbol-function (cadr procdef)))
		(t
		 (compile nil (cadr procdef)))))
	 (t (eval procdef))))


(defun problem-length-parse (l)
   (list-smooth l #'(lambda (x) (and (consp x)
                                     (member (car x) '(:serial :parallel))
                                     (numberp (cadr x))))))