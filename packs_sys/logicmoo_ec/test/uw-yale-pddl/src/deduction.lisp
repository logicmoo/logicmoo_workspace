(in-package "PDDL")

; Depends on expdt, unify, index

(defun new-situation (sit al dl act)
   (let ((new-index (exp-index-copy (get-situation-index sit))))
      (dolist (d dl)
	 (exp-ob-index d new-index nil))
      (dolist (a al)
	 (cond ((not (member a (exp-fetch a new-index t) :test #'eq))
		(exp-ob-index a new-index t))))
;      (cerror  "Okay" "Produced new index from dl = ~s, al = ~s~%"
;	       dl al)
      (uniquify-situation new-index
			  (cons act (situation-path SIT))
			  (find-init-situation sit))))

(defun seq-project (sit acts dom)
   (do ((al acts) (next-sit))
       ((null al)
	sit)
     (setf next-sit (project sit (car al) dom))
     (cond ((not next-sit)
	    (return nil)))
     (setf sit next-sit)
     (setf al (cdr al))))

(declaim (special discrim-bdgs*))

(defun project (sit act dom)
   (setf act (uniquify-action act dom))
   (let ((p (assoc act (situation-succs SIT) :test #'eq)))
      (cond (p
             (values (sitsuccspec-sit p)
                     (sitsuccspec-adds p)
                     (sitsuccspec-deletes p)))
            (t
             (fresh-act-project sit act dom t)))))

(defun fresh-act-project (sit act dom alter)
	     (let ((b (find-domain-bdg (car act) dom)))
	        (cond ((and b (action-functor-p (pddl-bdg-val b)))
		       (let ((adefn (action-functor-defn
				        (pddl-bdg-val b)))
			     (aid (new-varid)))
			  (multiple-value-bind (ok e)
					       (unify (action-defn-term
						         adefn)
						      act
						      aid ground-id* nil)
			     (cond (ok
				    (cond ((action-defn-precond adefn)
					   (let ((pre (deduce (action-defn-precond
								 adefn)
							      aid e sit dom)))
					      (cond ((null pre) nil)
						    (t
						     (effect-project
						        act adefn aid (car pre)
							sit dom alter)))))
					  (t
					   (effect-project
					      act adefn aid nil sit dom alter))))
				   (t
				    (format *error-output*
					    "Action definition fails to ~
                                             unify: ~s~%"
					    act)
				    nil)))))
		      (t
		       (format *error-output*
			       "Bad action functor ~s~%"
			       (car act))
		       nil))))
				    
(defun effect-project (act adefn aid bdgs sit dom alter)
  (multiple-value-bind (succ adds dels)
                       (do-effect act
			  (action-defn-effect adefn)
			  aid bdgs sit dom)
     (cond (alter
            (push (make-sitsuccspec
                   :act act
                   :sit succ
                   :adds adds
                   :deletes dels)
                  (situation-succs
                   sit))))
     (values succ adds dels)))

(defun do-effect (act eff id bdgs sit dom)
   (multiple-value-bind (adds deletes)
		        (deduce-effects eff id bdgs sit dom)
      (values (new-situation sit adds deletes act)
              adds
              deletes)))

(defun deduce-effects (effect did subst sit dom)
  (case (car effect)
    (and
      (let ((adds '()) (deletes '()) al dl)
	(dolist (e (cdr effect))
	   (multiple-value-setq (al dl)
				(deduce-effects e did subst sit dom))
	   (setf adds (nconc al adds))
	   (setf deletes (nconc dl deletes)))
	(values adds deletes)))
    (not
     (values '()
	     (mapcar #'(lambda (d)
			  (uniquify-occasion (varsubst d did subst)
					     dom))
		     (cdr effect))))
    (change
     (let ((old-val (deduce `(current-value ,(cadr effect) ?old)
			    did subst sit dom)))
        (cond ((= (length old-val) 1)
	       (let ((new-val (deduce `(fluent-eval ,(caddr effect)
						    ?new)
				      did (car old-val)
				      sit dom)))
		  (cond ((= (length new-val) 1)
			 (labels ((fluent-val-occ (var)
				     (uniquify-occasion
				        (varsubst `(current-value
						      ,(cadr effect)
						      ,var)
						  did (car new-val))
					dom)))
			    (values (list (fluent-val-occ '?new))
				    (list (fluent-val-occ '?old)))))
			(t
			 (format *error-output*
				 "Fluent ~s yields new-val ~s~%"
				 (unsafe-varsubst (cadr effect) did subst)
				 new-val)
			 (values '() '())))))
	      (t
	       (format *error-output*
		       "Fluent ~s yields old values ~s~%"
		       (unsafe-varsubst (cadr effect) did subst)
		       old-val)
	       (values '() '())))))
    (when
      (let ((adds '()) (deletes '()) al dl)
	 (dolist (a (deduce (cadr effect) did subst sit dom))
	    (multiple-value-setq (al dl)
				 (deduce-effects (caddr effect) did a sit dom))
	    (setf adds (nconc al adds))
	    (setf deletes (nconc dl deletes)))
	 (values adds deletes)))
    (t
     (values (list (uniquify-occasion (varsubst effect did subst)
				      dom))
	     '()))))


; These are not really bdgenv lists; they are used as flags.
(defvar many-bdgs* (list (make-varbdg 'many 0 (make-expclo 'many nil))))

(defun ground-deduce (query sit dom)
   (not (null (deduce query ground-id* '() sit dom))))

(defvar DUMMY-ID* 0)

(defun loc-deduce (pat sit)
   (assertion-fetch pat pat dummy-id* '() (get-situation-index sit)))

(defun get-var-val (v al)
	   (cond ((= (length al) 1)
                  (get-var-val-1 v (car al)))
		 (t
		  (cerror "I'll assume a value of nil"
			  "No unique value for ~s  in ~s" 
			  v al)
		  nil)))

(defun get-var-val-1 (v e)
   (expclo-skel (varbdg-val (uvar-lookup v dummy-id* e))))

(defvar trace-deduce* nil)

(defun deduce (query qid bdgs sit dom)
   (delete many-bdgs*
	   (deduce-maybe-many query qid bdgs sit dom)
	   :test #'eq))

; first value is t if possibly many solutions, but procedural query handlers
; couldn't tell.  
(defun deduce-check-many (query qid bdgs sit dom)
   (let ((sl (deduce-maybe-many query qid bdgs sit dom)))
      (cond ((member many-bdgs* sl :test #'eq)
	     (values t (delete many-bdgs* sl :test #'eq)))
	    (T
	     (values nil sl)))))

(defun deduce-maybe-many (query qid bdgs sit dom)
   (cond (trace-deduce*
	  (format t "query = ~s qid = ~s ~%   env = ~s ~% -> ~s~%"
		  query qid bdgs (unsafe-varsubst query qid bdgs))))
   ;(cerror "No problem" "query = ~s" query)
   (multiple-value-bind (qa qd bdgs)
			(expression-car-cdr query qid bdgs)
      (case qa
	 (and (conj-deduce qd qid bdgs sit dom))
	 (or
	  (mapcan #'(lambda (d)
		      (deduce-maybe-many d qid bdgs sit dom))
		  qd))
	 (not
	  (let ((p (unsafe-varsubst (car qd) qid bdgs)))
	     (note-if-failed
	       p nil qid
	       (cond ((and (has-qvars p)
                           (not (member (car p) '(forall exists))))
                      ; gross hack
		      (many))
		     (t
		      (multiple-value-bind (many envs)
					   (deduce-check-many
					      p qid bdgs sit dom)
			 (cond ((or many (not (null envs)))
				'())
			       (t (list bdgs)))))))))
	 (t
	  (let ((iq (unsafe-varsubst query qid bdgs)))
	     (note-if-failed
		query t qid
		(nconc (sit-assertion-fetch iq query qid bdgs sit)
		       (let ((rules (fetch-axioms iq dom))
			     (answers '())
			     a)
			  (dolist (r rules answers)
			     (setf a (rule-deduce query qid r bdgs sit dom))
			     (setf answers (nconc answers a)))))))))))

(defvar note-failed-deductive-goals* nil)

(defvar failed-deductive-goals* '())

(defun note-if-failed (query positive qid answers)
   (cond ((and note-failed-deductive-goals*
	       (every #'(lambda (a) (eq a many-bdgs*))
		      answers))
	  (push (cond (positive query) (t `(not ,query)))
		failed-deductive-goals*)))
   (cond (trace-deduce*
	  (format t "Answers = ~s~%"
		  (mapcar #'(lambda (a) (unsafe-varsubst query qid a))
			  answers))))
;   (cond ((eq (car query) 'locomotive-mass)
;	  (format t "LM answers = ~s~%"
;		  (mapcar #'(lambda (a) (unsafe-varsubst query qid a))
;			  answers))))

   answers)

(defun sit-assertion-fetch (qpat upat id bdgs sit)
       (nconc (assertion-fetch qpat upat id bdgs (get-situation-index sit))
	      (assertion-fetch qpat upat id bdgs
			       (initial-situation-timeless-index
				   (find-init-situation sit)))))

(defun assertion-fetch (qpat upat id bdgs ind)
   (mapcan #'(lambda (occ)
	        (multiple-value-bind (ok e)
				     (unify upat (OCCASION-PROP OCC)
					    id 0 BDGS)
                    (cond (ok (list e))
			  (t
			   (cond ((eq e discrim-bdgs*)
				  (try-rehash force-rehash qpat occ
					      (occasion-prop occ)
					      ind nil)))
			   '()))))
	   (exp-fetch qpat ind nil)))

(defun rule-deduce (query qid rule bdgs sit dom)
   ;(format t "rule-deduce ~s~%" (unsafe-varsubst query qid bdgs))
   (let ((rule-id (new-varid))
	 (ant (axiom-antecedent rule)))
      (multiple-value-bind (ok e)
			   (unify (axiom-consequent rule)
				  query rule-id qid bdgs)
	 (cond (ok
		(mapcar #'(lambda (a) 
			    (bdgenv-contract a rule-id))
			(cond ((procedural-axiom-p rule)
			       (let ((pal
				        (funcall
					   (procedural-axiom-fcn rule)
					   query  ;(axiom-consequent rule)
					   rule-id e sit dom)))
				  (cond (ant
					 (mapcan #'(lambda (a)
						     (deduce-maybe-many
						        ant rule-id a sit dom))
						 pal))
					(t pal))))
			      (ant
			       (deduce-maybe-many
				  ant rule-id e sit dom))
			      (t (list e)))))
	       (t
                (cond ((eq e discrim-bdgs*)
		       (try-rehash force-rehash query rule
				   (axiom-consequent rule)
				   (indexed-domain-axioms dom)
				   nil)))
                '())))))

(defun conj-deduce (conjl qid a sit dom)
   ;(format t "Conjunction ~s id = ~s~%" (unsafe-varsubst conjl qid a) qid)
   (cond ((null conjl)
	  (list a))
	 (t
	  (let ((answers '()) b)
	     (dolist (a (deduce-maybe-many (car conjl) qid a sit dom)
			answers)
	        (cond ((eq a many-bdgs*)
		       (setf b (list many-bdgs*)))
		      (t
;		       (format t "Answer ~s to ~s ~%/   Remainder ~s~%"
;			       a
;			       (car conjl)
;			       (unsafe-varsubst (cdr conjl) qid a))
		       (setf b (conj-deduce (cdr conjl) qid a sit dom))))
;		(cond ((circular answers)
;		       (setq bad-answers* answers)
;		       (error "Circular answers")))
;		(cond ((circular b)
;		       (setq b* b)
;		       (error "Circular b")))
		(setf answers (nconc answers b)))))))

(defun many () (list many-bdgs*))

(defun objects-mentioned (sit)
   (reduce #'(lambda (l occ)
	        (reduce #'(lambda (l x)
			    (adjoin x l :test #'eq))
			(cdr (occasion-prop occ))
			:initial-value l))
	   (get-situation-contents sit)
	   :initial-value '()))

(defun pddl-eval (e dom)
   (cond ((symbolp e)
	  (let ((b (find-domain-bdg e dom)))
	     (cond (b
		    (let ((v (pddl-bdg-val b)))
		       (cond ((domain-var-p v)
			      (domain-var-val v))
			     ((constant-p v)
			      (constant-name v))  ; I guess
			     (t
			      (error "Symbol not usable in eval-context: ~s"
				     e)))))
		   (t
		    (error "Unbound domain variable ~s" e)))))
	 ((atom e) e)
	 (t
	  (apply (symbol-function (car e))
		 (mapcar #'(lambda (x) (pddl-eval x dom))
			 (cdr e))))))

(defun fluent-eval (e vid bdgs sit dom)
   ;(format t "Evaluating ~s ~%" (unsafe-varsubst e vid bdgs))
   (let ((val 
	   (cond ((qvar-p e) 
		  (let ((b (uvar-lookup (qvar-sym e) vid bdgs)))
		     (cond (b
			    (fluent-eval (expclo-skel (varbdg-val b))
					 (expclo-id (varbdg-val b))
					 bdgs sit dom))
			   (t
			    '*unevaluable))))
		 ((symbolp e)
		  (let ((b (find-domain-bdg e dom)))
		     (cond (b
			    (let ((v (pddl-bdg-val b)))
			       (cond ((domain-var-p v)
				      (domain-var-val v))
				     ((constant-p v)
				      (cond ((is-subtype (constant-type v)
							 fluent-type*)
					     (fluent-current-value e sit dom))
					    (t
					     (constant-name v))))
				     (t
				      (error "Symbol not usable in eval-context: ~s"
					     e)))))
			   (t
			    (error "Unbound domain variable ~s" e)))))
		 ((atom e) e)
		 ((eq (car e) 'sum)
		  (sum-eval (caddr e) (cadddr e)
			    vid bdgs sit dom))
		 ((eq (car e) 'constant)
		  (let ((v (unsafe-varsubst (cadr e) vid bdgs)))
		     (cond ((has-qvars v) '*unevaluable)
			   (t (pddl-eval v dom)))))
		 (t
		  (let ((args (mapcar #'(lambda (x) (fluent-eval x vid bdgs sit dom))
				      (cdr e))))
		     (cond ((member '*unevaluable args :test #'eq)
			    '*unevaluable)
			   (t
			    (apply (symbol-function (car e))
				   args))))))))
      ;(format t "Value of ~s = ~s~%" (unsafe-varsubst e vid bdgs) val)
      val))

(defun fluent-current-value (flu sit dom)
  (let ((ansl (deduce `(current-value
			  ,flu ?fluentval)
		      dummy-id* '() sit dom)))
     (cond ((= (length ansl) 1)
	    (let ((b (uvar-lookup 'fluentval dummy-id* (car ansl))))
	       (cond (b
		      (expclo-skel (varbdg-val b)))
		     (t
		      (error "Fluentval not bound getting value of ~s"
			     flu)))))
	   ((null ansl)
	    '*unevaluable)
	   (t
	    (format *error-output*
		    "Fluent ~s has multiple values ~s"
		    flu ansl)
	    '*unevaluable))))

(defun sum-eval (p e vid bdgs sit dom)
   (let ((total 0)
	 (instances (deduce p vid bdgs sit dom)))
      (dolist (a instances total)
	 (let ((v (fluent-eval e vid a sit dom)))
	    (cond ((eq v '*unevaluable)
		   (return v)))
	    (setf total (+ total v))))))

(defun unifiers (t1 t2 e1 e2 e)
   (multiple-value-bind (ok e)
			(unify t1 t2 e1 e2 e)
       (cond (ok (list e))
	     (t '()))))

; WARNING: The first argument to a procedural query handler is
; almost always useless, because its variables (a) are not substed
; out and (b) have the wrong id, and so (c) cannot be substed out
; from within the procedure.

(define (addendum procedural-query-handlers)
   (:domain no-op)   ;numbers
   (:axiom
      :implies (many)
      :procedure #'(lambda (pat vid bdgs sit dom)
		      (declare (ignore pat vid bdgs sit dom))
		      (many)))
   (:axiom
      :vars (?x)
      :implies (has-vars ?x)
      :procedure #'(lambda (pat vid bdgs sit dom)
		      (declare (ignore pat sit dom))
		      (cond ((has-uvars '?x vid bdgs)
			     (list bdgs))
			    (t '()))))

   (:axiom
       :vars (?x)
       :implies (object ?x)
       :procedure #'(lambda (pat vid bdgs sit dom)
		            (declare (ignore pat dom))
		       (cond ((has-uvars '?x vid bdgs)
			      (mapcan #'(lambda (ob)
					   (multiple-value-bind
					            (ok e)
						    (unify '?x ob
							   vid ground-id* bdgs)
					      (cond (ok (list e))
						    (t '()))))
				      (objects-mentioned sit)))
			     (t
			      (list bdgs)))))

   (:axiom
       :vars (?a)
       :implies (assertion ?a)
       :procedure #'(lambda (pat vid bdgs sit dom)
		            (declare (ignore pat dom))
		       (sit-assertion-fetch (unsafe-varsubst '?a vid bdgs) '?a
					    vid bdgs sit)))

)

(define (addendum equality-defn)
   (:domain dom-equality)
   (:axiom
      :vars (?x - object)
      :implies (= ?x ?x)))

(define (addendum procedural-query-handlers)
   (:domain expressions)

   (:axiom
      :vars (?e - (expression object) ?x - object)
      :implies (eval ?e ?x)
      :procedure #'(lambda (pat vid bdgs sit dom)
		           (declare (ignore sit))
                      ;(format t "In eval~%")
		      (setq pat (unsafe-varsubst '?e vid bdgs))
		      ; Okay because vars are deadly anyway
		      (cond ((not (has-qvars pat))
			     (multiple-value-bind
				           (ok e)
					   (unify (pddl-eval pat dom)
						  '?x
						  vid vid bdgs)
				   (cond (ok (list e))
					 (t '()))))
			       (t (many)))))

   (:axiom
      :vars (?p - (expression proposition))
      :implies (test ?p)
      :context (eval (is-true ?p) t))

   (:axiom
      :vars (?i ?l ?h - integer)
      :implies (bounded-int ?i ?l ?h)
      :procedure #'(lambda (p vid bdgs sit dom)
		      (declare (ignore sit))
		      (setq p (unsafe-varsubst '(?i ?l ?h) vid bdgs))
		      (cond ((and (not (has-qvars (cadr p)))
				  (not (has-qvars (caddr p))))
			     (let ((lo (pddl-eval (cadr p) dom))
				   (hi (pddl-eval (caddr p) dom)))
			       (cond ((has-qvars (car p))
				      (do ((i lo (+ i 1))
					   (a '()
					      (nconc (unifiers '?i i
							       vid 0 bdgs)
						     a)))
					  ((= i hi)
					   a)))
				     (t
				      (let ((i (pddl-eval (car p) dom)))
					(cond ((and (integerp i)
						    (<= lo i)
						    (<= i hi))
					       (list bdgs))
					      (t '())))))))
			    (t (many)))))
)

(define (addendum procedural-query-handlers)
   (:domain dom-fluents)
   (:axiom
       :vars (?x ?ty)
       :implies (fluent ?ty ?x)
       :procedure #'(lambda (pat vid bdgs sit dom)
		       (setq pat (unsafe-varsubst '(?ty ?x) vid bdgs))
		       (cond ((or (has-qvars (car pat))
				  (has-qvars (cadr pat)))
			      (many))
			     (t
			      (let ((fty (type-eval (car pat) dom)))
			         (mapcan
				    #'(lambda (a)
					(let ((sty (unsafe-varsubst
						      '?sty vid a)))
					  (cond ((and (not (has-qvars sty))
						      (is-subtype
						         (type-eval
							    sty dom)
							 fty))
						 (list a))
						(t '()))))
				    (deduce `(fluent ?sty ?x) vid bdgs
					    sit dom)))))))
   (:axiom
       :vars (?e - (expression (fluent object)) ?x - object)
       :implies (fluent-eval ?e ?x)
       :procedure #'(lambda (pat vid bdgs sit dom)
		            (declare (ignore pat))
		       (let ((v (fluent-eval '?e vid bdgs sit dom)))
			  (cond ((eq v '*unevaluable)
				 (many))
				(t
				 (multiple-value-bind
				           (ok e)
					   (unify v '?x 
						  vid vid bdgs)
				   (cond (ok (list e))
					 (t '()))))))))

   (:axiom
       :vars (?e - (expression (fluent object)))
       :implies (fluent-test ?e)
       :context (fluent-eval (is-true ?e) t))
)

(define (addendum forall-handler)
   (:domain universal-precs)
   (:axiom
       :vars (?b ?e)
       :implies (suppress-syntax-check (forall ?b ?e))
       :procedure
       #'(lambda (pat vid bdgs sit dom)
                 (declare (ignore pat))
            (multiple-value-bind (e bdgs)
                                 (safe-varsubst '?e vid bdgs)
              (cond ((eq (car e) 'imply)
                     (multiple-value-bind (many antansl)
                                          (deduce-check-many
                                             (cadr e) vid bdgs sit dom)
                       (cond (many (many))
                             ((every #'(lambda (a)
                                          (deduce (caddr e) vid a sit dom))
                                     antansl)
                              (list bdgs))
                             (t '()))))
                    (t
                     (cerror "The goal will fail"
                             "Can't handle general forall goal ~s"
                             e)))))))

(define (addendum exists-handler)
   (:domain existential-precs)
   (:axiom
       :vars (?b ?e)
       :implies (suppress-syntax-check (exists ?b ?e))
       :procedure
       #'(lambda (pat vid bdgs sit dom)
                 (declare (ignore pat))
            (multiple-value-bind (e bdgs)
                                 (safe-varsubst '?e vid bdgs)
               (multiple-value-bind (many ansl)
                                    (deduce-check-many e vid bdgs sit dom)
                  (cond (many (many))
                        ((null ansl) '())
                        (t
                         ;(format t "Found: ~s~%" (list bdgs))
                         (list bdgs))))))))

(defun sits-compare (s1 s2)
   (let ((c1 (get-situation-contents s1))
	 (c2 (get-situation-contents s2)))
      (format *terminal-io*
	      "Situation 1: ~s~%Situation 2: ~s~%Both: ~s"
	      (set-difference C1 C2 :test #'eq)
	      (set-difference C2 C1 :test #'eq)
	      (intersection c1 c2 :test #'eq))))

;Really dumb version, allows just one occurrence of any given variable 
(def-deductive-macro equation expressions (e)
   (let ((l (cadr e))
	 (r (caddr e)))
      (let ((lvars (isolate-vars l))
	    (rvars (isolate-vars r))
            (w (make-qvar (gensym))))
         `(or ,@(mapcar #'(lambda (vspec)
                            `(and (has-vars ,(car vspec))
                                  (eval ,(funcall (cadr vspec) r)
                                        ,(car vspec))))
                        lvars)
              ,@(mapcar #'(lambda (vspec)
                            `(and (has-vars ,(car vspec))
                                  (eval ,(funcall (cadr vspec) l)
                                        ,(car vspec))))
                        rvars)
              (and (not (has-vars ,l))
                   (not (has-vars ,r))
                   (eval ,l ,w)
                   (eval ,r ,w))))))

; The following two procedures are for use in automatically generated
; type-deduction rules.  (See index.lisp.)
; BECAUSE THEY ARE PROCEDURAL QUERY HANDLERS, the WARNING above applies
; It's ugly, but crucial that the variable used by type-index (see index.lisp)
; is called ?x.

(defun constant-type-predication (ty rid env dom)
   (let ((ob (unsafe-varsubst '?x rid env)))
      ;(format t "env = ~s ~%  ob = ~s~%" env ob)
      ;(cerror "All right" "ob = ~s" ob)
      (cond ((atom ob)
	     (cond ((symbolp ob)
		    (let ((termtype (symbolic-term-type ob dom)))
		       (cond ((and (pddl-type-p termtype)
				   (not (eq (pddl-type-defn termtype)
					    (pddl-type-defn ty)))
					; If eq, already in db
				   (is-subtype termtype ty))
			      (list env))
			     (t '()))))
		   ((const-of-type ob ty)
		    (list env))
		   (t '())))
	    (t '()))))

(defun collect-subtype-elements (subty rid env sit dom)
   (cond ((has-uvars '?x rid env)
	  (deduce `(,(pddl-type-defn subty) ?x) rid env sit dom))
	 (t '())))

(defun is-true (x) (not (not x)))

(defun nconc-check (l1 l2)
   (cond ((null l2) l1)
	 ((shorter l1 500)
	  (do ((l l1 (cdr l)))
	      ((or (null l) (eq l l2))
	       (cond ((null l) (nconc l1 l2))
		     (t
		      (error "Circular nconc"))))))
	 (t
	  (error "Suspicious nconc"))))
       
