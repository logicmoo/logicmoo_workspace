(in-package "PDDL")

; Parser of actions and expansions.

(defun actions-parse (actions dom)
   (let ((mr (domain-place-rule-group dom dom)))
      (multiple-value-bind (actions flg)
			   (list-smooth actions #'consp)
	  (let ((flg-al '()))
	     (dolist (a actions)
		(multiple-value-bind (flg-a defn)
				     (action-parse a dom)
		   (push flg-a flg-al)
		   (cond (defn (add-to-rule-group defn mr)))))
	     `(,@(reverse flg-al)
	       ,@flg)))))

(defun action-parse (action-body dom)
   (multiple-value-bind (fctr items bad-keyword-flg)
			(extract-params-etc action-body
					    '(:vars :parameters :precondition
					      :maintain :effect :expansion
					      :only-in-expansions)
					    '())
      (cond (fctr
	     (multiple-value-bind (qvarbdgs param-flg vartranstab)
				  (qvar-list-parse
				     (get-field-or-empty ':parameters items)
				     '() '() dom)
		(let ((fbdg (place-domain-bdg fctr dom)))
		   (cond ((not (pddl-bdg-unbound fbdg))
			  (setq fctr
				(note-decl-problems (list fbdg) nil fctr))
			  (setf fbdg nil)))
		   (multiple-value-bind (def-flg meth-varbdgs graph
					 precond maint eff vt)
					(action-def-body-parse
					   items qvarbdgs t vartranstab dom)
					(declare (ignore vt))
		      (cond ((and (not (atom graph))
				  (symbolp fctr))
			     (let ((mrg (domain-place-rule-group dom 'domain)))
				(add-to-rule-group
				   (def-body-to-method
				      fctr fctr
				      (action-functor-term fctr qvarbdgs)
				      (qvarbdgs-constraints qvarbdgs)
				      (qvarbdgs-constraints meth-varbdgs)
				      nil nil graph)
				   mrg))))
		      (values
			 `(:action ,fctr
			   :parameters ,param-flg
			   ,@def-flg
			   ,@(cond ((and graph
					 (not (domain-declares-requirement
						 dom ':action-expansions)))
				    `(,(format-flg ':action-expansions
					      "Action expansion illegal for ~
					       domain not declaring")))
				   (t '()))
			   ,@(cond ((cond (graph (not eff))
					  (t eff))
				    '())
				   (t 
				    ;(format t "graph = ~s eff = ~s~%"
					;    graph eff)
				    `(,(flagexp "Must have :expansion or :effect, but not both"
						"__"))))
			   ,@bad-keyword-flg)
			 (cond ((symbolp fctr)
				(define-action fctr fbdg qvarbdgs
				               meth-varbdgs items
					       precond maint eff graph))
			       (t nil)))))))
            (t
	     (values 
		`(,(flagexp "Illegal action functor"
			    (car action-body))
		  ,@(cdr action-body))
		nil)))))

(defun define-action (fctr fbdg qvarbdgs meth-varbdgs items
		      precond maint eff graph)
   (let ((defn (make-action-defn
		    :functor fctr
		    :term (action-functor-term fctr qvarbdgs)
		    :args (qvarbdgs-constraints qvarbdgs)
		    :vars (qvarbdgs-constraints meth-varbdgs)
		    :precond precond
		    :effect eff
		    :maintain maint
		    :has-methods (not (not graph))
		    :only-in-expansions
		      (let ((e (get-field-or-empty
				  ':only-in-expansions items)))
			   (cond ((null e) 'nil)
				 (t e))))))
      (cond (fbdg
	     (setf (pddl-bdg-val fbdg)
		   (make-action-functor
		      :name fctr
		      :argtypes (mapcar #'pddl-bdg-val qvarbdgs)
		      :defn defn))))
      defn))

(defun methods-parse (methods dom rg)
   (cond ((null methods) '())
         (t
	  (multiple-value-bind (methods flg)
			       (list-smooth methods #'consp)
	      (let ((flg-al '()))
		 (dolist (a methods)
		    (multiple-value-bind (flg-a defn)
					 (method-parse a dom)
		       (cond (defn 
			      (let ((fval (find-domain-bdg-val
					       (action-method-functor defn)
					       dom)))
				 (cond ((and (functor-p fval)
					     (is-subtype
						(functor-rangetype fval)
						action-type*))
					(cond (rg
                                               (add-to-rule-group defn rg))))
				       (t
					(setq flg-a
					      (format-flg flg-a
						  "Method definition for ~
						  non-action-functor ~
						  (value ~s)"
						  fval)))))))
		       (push flg-a flg-al)))
		 (list-verify-requirement
		    `(,@(reverse flg-al)
		      ,@flg)
		    ':action-expansions
		    dom))))))

; Returns flagged method-body + defined method
(defun method-parse (method-body dom)
   (multiple-value-bind (fctr items bad-keyword-flg)
			(extract-params-etc
			   method-body 
			   '(:vars :parameters :precondition
			     :maintain :effect :expansion :name)
			   '(:expansion))
      (cond (fctr
	     (multiple-value-bind (qvarbdgs param-flg vartranstab)
				  (qvar-list-parse
				     (get-field-or-empty ':parameters items)
				     '() '() dom)
		(let ((fbdg (find-domain-bdg fctr dom)))
		   (cond ((or (not fbdg)
			      (pddl-bdg-unbound fbdg))
			  (setq fctr (flagexp "Method for undefined action"
					      fctr)))
			 (t
			  (let ((fval (pddl-bdg-val fbdg))
				(newargtypes (mapcar #'pddl-bdg-val
						     qvarbdgs)))
			     (cond ((functor-p fval)
				    (cond ((not (arg-subtypes
						   newargtypes
						   (functor-argtypes fval)))
					   (setq param-flg
						 (flagexp
						    "Mismatch with original argtypes"
						    param-flg)))))
				   (t
				    (setq fctr
					  (note-decl-problems
					     (list fbdg) '() fctr))))))))
		(multiple-value-bind (def-flg meth-varbdgs graph
				      precond maint eff vt)
				     (action-def-body-parse
				        items qvarbdgs nil
					vartranstab dom)
				     (declare (ignore eff vt))
		   (let ((name (get-field-or-empty ':name items)))
		      (values
			 `(:method ,fctr
			   ,@(cond (name
				    (cond ((symbolp name) `(,name))
					  (t `(,(flagexp
						   "Illegal method name"
						   name)))))
				   (t '()))
			   :parameters ,param-flg
			   ,@def-flg
			   ,@bad-keyword-flg)
			 (cond ((and graph (symbolp fctr))
				(def-body-to-method
				  name fctr
				  (action-functor-term fctr qvarbdgs)
				  (qvarbdgs-constraints qvarbdgs)
				  (qvarbdgs-constraints meth-varbdgs)
				  precond maint graph))
			       (t nil)))))))
            (t
             (values
	        `(:method ,(flagexp "Illegal action functor"
			       (car method-body))
			  ,@(cdr method-body))
                nil)))))

; Returns functor, fields as a-list, junk
(defun extract-params-etc (body keywords required)
   (multiple-value-bind (fctr mcd-style-params)
                        (cond ((and (not (symbolp (car body)))
                                    (symbolp (caar body)))
                               (values (caar body)
                                       (cdar body)))
                              (t
                               (values (car body) '())))
      (cond ((symbolp fctr)
             (multiple-value-bind (items bad-keyword-flg)
                                  (keyword-list-smooth
                                     (cdr body)
				     keywords required)
		(let ((params (append mcd-style-params
                                      (get-field-or-empty
                                         ':parameters items))))
		   (values fctr
			   (set-field ':parameters items params)
			   bad-keyword-flg))))
	    (t (values nil nil)))))

(defun def-body-to-method (name fctr term args vars precond maint graph)
      (make-action-method
         :name name
         :functor fctr
	 :term term
         :args args
	 :vars vars
	 :graph (def-body-annotations 
		    graph precond maint)))

(defun action-functor-term (fctr varbdgs)
  `(,fctr ,@(mapcar #'(lambda (b) (make-qvar (pddl-bdg-sym b)))
		    varbdgs)))

; Returns parsed body in keyword-arg form, 
; plus bindings from :vars
; plus expansion graph (nil if none, t if delayed)
; plus precond, maint, eff (nil for absent ones)
; plus revised vartranstab (important only in-context)
(defun action-def-body-parse (items qvarbdgs allow-effect vartranstab dom)
  (let ((vars-field (get-field-or-empty ':vars items)))
   (multiple-value-bind (meth-varbdgs vars-flg vartranstab)
                        (qvar-list-parse
                           vars-field '() vartranstab dom)
      (cond ((and vars-field
                  (not (or (domain-declares-requirement
                              dom ':quantified-preconditions)
                           (domain-declares-requirement
                              dom ':conditional-effects))))
             (setq vars-flg (flagexp "Illegal for domain not declaring requirements :quantified-preconditions and :conditional-effects"
                                     vars-flg))))
      (setq qvarbdgs (append meth-varbdgs qvarbdgs))
      (multiple-value-bind (exp-flg graph vt)
			   (let ((xf (get-field ':expansion items)))
                              (cond (xf
                                     (setq xf (cadr xf))))
			      (cond ((atom xf) ; smoothed?
				     (case xf
					((t :methods)
					 (values nil t vartranstab))
					((nil)
					 (values nil nil vartranstab))
					(t
					 (values (flagexp "Illegal as :expansion"
							  xf)
						 nil
						 vartranstab))))
				    (t
				     (action-spec-parse
				         xf qvarbdgs vartranstab dom))))
	 ;(format t "vt = ~s~%" vt)
         ; [[ check graph for circularity & duplicate tags ]]
	 (let ((pf (get-field ':precondition items))
	       (mf (get-field ':maintain items))
	       (ef (get-field ':effect items)))
	    (labels ((build-field (field parser argname vt)
			(cond (field
			       (multiple-value-bind
				               (f-flg f-int vt)
					       (funcall parser
							(cadr field)
							qvarbdgs vt dom)
				  (values `(,argname ,f-flg)
					  f-int
					  vt)))
			      (t
			       (values '() nil vt)))))
	       (multiple-value-bind (p-flg p-int vt)
				    (build-field pf #'goal-parse
						 ':precondition vt)
		  (multiple-value-bind (m-flg m-int vt)
				       (build-field mf #'goal-parse
						    ':maintain vt)
		     (multiple-value-bind (e-flg e-int vt)
					  (build-field ef #'effect-parse
						       ':effect vt)
			(cond ((and e-flg
				    (not allow-effect))
			       (setf e-flg
				  `(:effect
				    ,(flagexp
				        ":effect not allowed in this context"
					(cadr e-flg)))))) 
;			(format t "p-flg = ~s, p-int = ~s, m-flg = ~s , m-int = ~s, e-flg = ~s, e-int = ~s~%"
;				p-flg p-int m-flg m-int e-flg e-int)
			(values 
			   `(,@(cond ((null vars-flg) '())
				     (t `(:vars ,vars-flg)))
			       ,@p-flg
			       ,@m-flg
			       ,@(cond (exp-flg `(:expansion ,exp-flg))
				       (t '()))
			       ,@e-flg)
			   meth-varbdgs
			   graph
			   p-int m-int e-int
			   (vartranstab-deactivate vt meth-varbdgs)))))))))))
	    
; Returns two values: flagged action spec, plus expansion-graph, plus
; revised vartranstab 
(defun action-spec-parse (action-spec qvarbdgs vartranstab dom)
   (cond ((consp action-spec)
          (multiple-value-bind (action-spec flg-junk)
                               (list-smooth action-spec
                                            #'any)
             (multiple-value-bind (flg-action-spec timetab vartranstab)
                                  (clean-action-spec-parse
                                      action-spec qvarbdgs vartranstab dom)
                (values `(,@flg-action-spec
                          ,@flg-junk)
                        timetab
			vartranstab))))
	 (t
          (values (flagexp "Impossible action spec" action-spec)
                  nil
		  vartranstab))))

(defun clean-action-spec-parse (action-spec qvarbdgs vartranstab dom)
   (let ((fctr (car action-spec))
	 (args (cdr action-spec)))
      (case fctr
	 (in-context
	  (in-context-parse args qvarbdgs vartranstab dom))
	 (choice
	  (choice-parse args qvarbdgs vartranstab dom))
	 ((forsome choose)
	  (forsome-parse args qvarbdgs vartranstab dom))
	 (series
	  (series-parse args qvarbdgs vartranstab dom))
	 (parallel
	  (parallel-parse 'parallel args qvarbdgs vartranstab dom))
	 (tag
	  (tag-parse args qvarbdgs vartranstab dom))
	 (constrained
          (multiple-value-bind (flg graph vartranstab)
                               (constrained-parse args qvarbdgs
						  vartranstab dom)
             (values (list-verify-requirement flg ':dag-expansions dom)
                     graph
		     vartranstab)))
	 (foreach
	  (foreach-parse args qvarbdgs vartranstab dom))
	 (t
	  (action-term-parse action-spec qvarbdgs vartranstab dom)))))

(defun action-term-parse (action-spec qvarbdgs vartranstab dom)
   (let ((fctr (car action-spec))
	 (args (cdr action-spec)))
       (cond ((symbolp fctr)
	      (let ((abdg (find-domain-bdg fctr dom)))
		 (cond ((or (not abdg)
			    (pddl-bdg-unbound abdg))
			(values `(,(flagexp "Undefined action functor"
					 action-spec))
				empty-expansion-graph*
				vartranstab))
		       ((and (functor-p (pddl-bdg-val abdg))
			     (is-subtype (functor-rangetype
					       (pddl-bdg-val abdg))
					 action-type*))
			(let ((atypes (functor-argtypes
					 (pddl-bdg-val abdg))))
			   (multiple-value-bind (al-flg al-int vt)
						(args-parse
						   args atypes
						   qvarbdgs vartranstab
						   nil dom nil)
			       (values `(,(car action-spec)
					 ,@al-flg)
				       (singleton-expansion-graph
					  `(,(car action-spec)
					    ,@al-int))
				       vt))))
		       (t
			(values
			    `(,(format-flg action-spec
				  "Action-spec begins with ~
				   non-action-functor ~
				   (declared as ~s in domain ~s)"
				  (pddl-bdg-val abdg)
				  (pddl-bdg-domain abdg)))
			       empty-expansion-graph*
			       vartranstab)))))
	     (t
	      (values (flagexp "Meaningless action functor"
			       action-spec)
		      empty-expansion-graph*
		      vartranstab)))))

(defun in-context-parse (in-context-spec qvarbdgs vartranstab dom)
   (multiple-value-bind (in-context-spec in-c-junk-flg)
                        (list-smooth in-context-spec
				     #'any)
      (cond ((null in-context-spec)
	     (values `(in-context ,(flagexp "Missing arguments" "__")
                                  ,@in-c-junk-flg)
		     empty-expansion-graph*
		     vartranstab))
	    (t
	     (multiple-value-bind (flg-a1 timetab vt)
		                  (action-spec-parse (car in-context-spec)
				      qvarbdgs vartranstab dom)
                (multiple-value-bind (def-body-flg augtab vt)
                                     (constraints-def-body-parse
                                         (cdr in-context-spec)
                                         qvarbdgs vt timetab dom)
                   (let ((flg-spec `(in-context ,flg-a1
						,@def-body-flg
						,@in-c-junk-flg)))
                      (values flg-spec
                              (expansion-graph-new-layer
                                 augtab
				 `(in-context
				     ,(expansion-graph-act timetab)
				     ,@(annotations-unparse timetab)))
			      vt))))))))

(defun choice-parse (choice-spec qvarbdgs vartranstab dom)
   (let ((flg-acts '())
         (subgraphs '()))
      (dolist (a choice-spec)
         (multiple-value-bind (flg-a tt vt)
                              (action-spec-parse a qvarbdgs vartranstab dom)
            (push flg-a flg-acts)
            (push tt subgraphs)
	    (setf vartranstab vt)))
      (setf subgraphs (reverse subgraphs))
      (let ((act-flg `(choice ,@(reverse flg-acts))))
         (values
            act-flg
	    (expansion-graph-encapsulate
	       (expansion-graphs-parallelize
		  subgraphs
		  'choice))
	    vartranstab))))

(defun forsome-parse (forsome-spec qvarbdgs vartranstab dom)
   (multiple-value-bind (forsome junk-flg)
                        (list-smooth forsome-spec #'listp)
      (cond ((null forsome)
             (values
                `(forsome (flagexp "Defective quantifier" "__")
                          ,@junk-flg)
                empty-expansion-graph*
		vartranstab))
            (t
             (multiple-value-bind (new-qvarbdgs qvar-flg vartranstab)
		                  (qvar-list-parse (car forsome)
                                                   qvarbdgs vartranstab dom)
                (cond ((null (cdr forsome))
                       (values
                          `(forsome ,qvar-flg
                                    ,(flagexp "Quantifier with no body"
                                              "__")
                                    ,@junk-flg)
                          empty-expansion-graph*
			  (vartranstab-deactivate vartranstab new-qvarbdgs)))
                      (t
                       (multiple-value-bind (flg timetab vt)
                                            (action-spec-parse
                                               (cadr forsome)
                                               (append new-qvarbdgs qvarbdgs)
					       vartranstab dom)
                          (let ((flg-spec
				   `(forsome ,qvar-flg
                                       ,flg
                                       ,@junk-flg)))
                             (values flg-spec
                                     (expansion-graph-encapsulate
				        (expansion-graph-new-layer
                                           timetab
					   `(forsome ,new-qvarbdgs
						     ,(expansion-graph-act
						         timetab))))
				     (vartranstab-deactivate vt new-qvarbdgs)))))))))))

(defun series-parse (action-spec qvarbdgs vartranstab dom)
   (let ((flg-acts '())
         (subgraphs '()))
      (dolist (a action-spec)
         (multiple-value-bind (flg-a tt vt)
                              (action-spec-parse a qvarbdgs vartranstab dom)
            (push flg-a flg-acts)
            (push tt subgraphs)
	    (setf vartranstab vt)))
      (let ((act-flg
               `(series ,@(reverse flg-acts))))
         (values
            act-flg
            (expansion-graphs-serialize (reverse subgraphs))
	    vartranstab))))

(defun parallel-parse (fctr action-spec qvarbdgs vartranstab dom)
   (let ((flg-acts '())
         (subgraphs '()))
      (dolist (a action-spec)
         (multiple-value-bind (flg-a tt vt)
                              (action-spec-parse a qvarbdgs vartranstab dom)
            (push flg-a flg-acts)
            (push tt subgraphs)
	    (setq vartranstab vt)))
      (let ((act-flg `(,fctr ,@(reverse flg-acts))))
         (values
            act-flg
            (expansion-graphs-parallelize (reverse subgraphs)
					  'parallel)
	    vartranstab))))

(defun foreach-parse (args qvarbdgs vartranstab dom)
   (multiple-value-bind (foreach junk-flg)
			(list-smooth args #'listp)
      (cond ((null foreach)
	     (values
	        `(foreach (flagexp "Defective quantifier" "__")
			  ,@junk-flg)
		empty-expansion-graph*
		vartranstab))
            (t
             (multiple-value-bind (new-qvarbdgs qvar-flg vt)
		                  (qvar-list-parse (car foreach)
                                                   qvarbdgs vartranstab dom)
		(setf qvarbdgs (append new-qvarbdgs qvarbdgs))
                (cond ((or (null (cdr foreach))
			   (null (cddr foreach)))
                       (values
                          `(foreach ,qvar-flg
                                    ,(flagexp "Quantifier with small body"
                                              "__")
                                    ,@junk-flg)
                          empty-expansion-graph*
			  (vartranstab-deactivate vt new-qvarbdgs)))
                      (t
		       (multiple-value-bind (goal-flg goal-int vt)
					    (goal-parse
					        (cadr foreach)
						qvarbdgs vt dom)
			  (multiple-value-bind (flg timetab vt)
					       (action-spec-parse
						  (caddr foreach)
						  qvarbdgs vt dom)
			     (let ((flg-spec
				      `(foreach ,qvar-flg
					  ,goal-flg
					  ,flg
					  ,@junk-flg)))
			        (values flg-spec
					(expansion-graph-encapsulate
					   (expansion-graph-new-layer
                                              timetab
					      `(foreach
						  ,new-qvarbdgs
						  ,goal-int
						  ,(expansion-graph-act
						      timetab))))
					(vartranstab-deactivate
					    vt new-qvarbdgs))))))))))))

(defun tag-parse (action-spec qvarbdgs vartranstab dom)
   (multiple-value-bind (tags realact flg-junk)
                        (tags-parse action-spec)
      (cond ((flagged-subexpression-p realact)
	     (values `(tag ,(flagexp "Tags tagging nothing"
			            tags)
                           ,realact)
		     empty-expansion-graph*
		     vartranstab))
	    (t
             (multiple-value-bind (act-flg timetab vt)
                                  (action-spec-parse
				      realact
				      qvarbdgs vartranstab dom)
                (let ((bad-flg '())
                      (goodtags '()))
		   (dolist (tg tags
			    (values `(tag ,@goodtags
					  ,act-flg
					  ,@(reverse bad-flg)
					  ,@flg-junk)
				    (expansion-graph-add-tags
					goodtags timetab)
				    vt))
		      (multiple-value-bind (e w)
					   (action-label-parse tg)
					   (declare (ignore w))
			  (let ((oldval (tag-already-bound e timetab dom)))
			     (cond (oldval
				    (setq bad-flg
					  `(,(format-flg tg
						"Action label already ~
						 bound (value ~s)"
						oldval)
					    ,@bad-flg)))
				   (t
				    (push tg goodtags))))))))))))

(defun constrained-parse (action-spec qvarbdgs vartranstab dom)
   (cond ((null action-spec)
	  (values (flagexp "Empty constrained action-spec"
			   action-spec)
		  empty-expansion-graph*
		  vartranstab))
	 (t
	  (multiple-value-bind (flg-a timetab vartranstab)
			       (action-spec-parse (car action-spec)
						  qvarbdgs vartranstab dom)
             ;(setq tt* timetab)
             ;(cerror "okay"
             ;        "got new timetab")
	     (let ((fakes-flg '()))
	        (dolist (c (cdr action-spec))
		   (multiple-value-bind (fake-flg time-entity ntab vt)
		       (action-constraint-parse
					   c timetab qvarbdgs vartranstab dom)
                                        (declare (ignore time-entity))
		      (push fake-flg fakes-flg)
		      (setq timetab ntab)
		      (setf vartranstab vt)))
		(setq fakes-flg (reverse fakes-flg))
		(values (verify-requirement
			     `(constrained ,flg-a ,@fakes-flg)
			     ':dag-expansions dom)
			timetab
			vartranstab))))))

; action-spec bottoms out in tags, not terms
; returns 4 values: flagged output; time-entity (point or interval) denoted by
; action-spec (nil if nonsense); augmented timetab; augmented vartranstab
(defun action-constraint-parse (action-spec timetab qvarbdgs vartranstab dom)
   (cond ((and (consp action-spec)
	       (not (member (car action-spec) '(< >))))
          (multiple-value-bind (action-spec flg-junk)
                               (list-smooth action-spec
                                            #'any)
             (multiple-value-bind (flg-action-spec time-entity timetab vt)
                                  (clean-action-constraint-parse
                                      action-spec timetab qvarbdgs
				      vartranstab dom)
                (values `(,@flg-action-spec
                          ,@flg-junk)
                        time-entity
                        timetab
			vt))))
	 (t
	  (multiple-value-bind (tag which)
			       (action-label-parse action-spec)
	     (let ((a (assoc tag (expansion-graph-tags timetab))))
		(cond (a
		       (values action-spec
			       (time-entity-begin-or-end (cadr a) which)
			       timetab
			       vartranstab))
		      ((symbolp tag)
		       (let ((v (symbol-any-value action-spec dom)))
			  (cond (v
				 (values (format-flg action-spec
					    "Symbol not allowed as action label ~
					     (value = ~s)"
					    v)
					 nil
					 timetab vartranstab))
				(t
				 (values (flagexp "Undefined task label"
						  action-spec)
					 nil
					 timetab
					 vartranstab)))))
		      (t
		       (values (flagexp "Impossible task label" action-spec)
			       nil
			       timetab
			       vartranstab))))))))

(defun legal-action-label (x)
   (or (symbolp x)
       (and (consp x)
	    (member (car x) '(< >)))))

(defun action-label-parse (x)
   (cond ((consp x)
	  (values (cadr x) (car x)))
	 (t
	  (values x nil))))

(defun clean-action-constraint-parse (action-spec timetab qvarbdgs
				      vartranstab dom)
   (let ((fctr (car action-spec))
	 (args (cdr action-spec)))
      (case fctr
	 (in-context
	  (in-context-constraint-parse args timetab qvarbdgs vartranstab dom))
	 (series 
	  (series-constraint-parse args timetab qvarbdgs vartranstab dom))
	 (parallel
	  (parallel-constraint-parse args timetab qvarbdgs vartranstab dom))
	 ((choice forsome choose foreach tag constrained)
	  (values (flagexp "Illegal inside action constraint"
			   action-spec)
                  nil timetab vartranstab))
	 (t
	  (values (flagexp "Meaningless action constraint"
			   action-spec)
		  nil timetab vartranstab)))))

(defun in-context-constraint-parse (action-spec timetab qvarbdgs
				    vartranstab dom)
   (multiple-value-bind (flg-a time-entity timetab vt)
                        (action-constraint-parse
			    (car action-spec)
			    timetab qvarbdgs vartranstab dom)
      (multiple-value-bind (def-body-flg augtab vt)
                           (constraints-def-body-parse
			       (cdr action-spec)
			       qvarbdgs vt timetab dom)
         (let ((flg-spec `(in-context ,flg-a ,@def-body-flg)))
            (values flg-spec
                    time-entity
                    (expansion-graph-new-layer
                       augtab flg-spec)
		    vt)))))

(defun series-constraint-parse (action-spec timetab qvarbdgs vartranstab dom)
   (multiple-value-bind (action-spec flg-junk)
                        (list-smooth action-spec #'any)
      (do ((sl action-spec (cdr sl))
           (flg-steps '())
           (time-ents '()))
          ((null sl)
	   (multiple-value-bind (aug-graph whole)
	                        (time-entities-serialize
                                   (reverse time-ents)
                                   timetab)
                 (values
                    `(series ,@(reverse flg-steps) ,@flg-junk)
                    whole
                    aug-graph
		    vartranstab)))
        (multiple-value-bind (flg-s time-ent ntab vt)
                             (action-constraint-parse
			         (car sl)
				 timetab qvarbdgs vartranstab dom)
           (push flg-s flg-steps)
           (cond (time-ent  ; elide nonsense
                  (push time-ent time-ents)))
           (setq timetab ntab)
	   (setq vartranstab vt)))))

; there's no need for these guys to smooth again; this could be flushed.

(defun parallel-constraint-parse (action-spec timetab qvarbdgs vartranstab dom)
   (multiple-value-bind (action-spec flg-junk)
                        (list-smooth action-spec #'any)
      (do ((sl action-spec (cdr sl))
           (flg-steps '())
           (time-ents '()))
          ((null sl)
           (multiple-value-bind (aug-graph whole)
                                (time-entities-parallelize
                                   (reverse time-ents)
                                   timetab)
              (values
                 `(parallel ,flg-steps ,@flg-junk)
                 whole
                 aug-graph
		 vartranstab)))
        (multiple-value-bind (flg-s time-ent ntab vt)
                             (action-constraint-parse
			        (car sl) timetab qvarbdgs vartranstab dom)
           (push flg-s flg-steps)
           (cond (time-ent  ; elide nonsense
                  (push time-ent time-ents)))
           (setq timetab ntab)
	   (setq vartranstab vt)))))

(defun tag-constraint-parse (action-spec timetab qvarbdgs vartranstab dom)
   (multiple-value-bind (tags realact flg-junk)
                        (tags-parse action-spec)
      (multiple-value-bind (flg-act timething timetab vt)
                           (action-constraint-parse
                              realact timetab qvarbdgs vartranstab dom)
         (values `(tag ,@tags ,@flg-act ,@flg-junk)
                 timething
                 (expansion-graph-add-tags tags timetab)
		 vt))))

(defun tags-parse (action-spec)
   (let ((goodtags '())
         (realact nil)
         (flg-junk '()))
      (dolist (x action-spec)
         (cond ((legal-action-label x)
		(push x goodtags))
               (realact
                (setq flg-junk `(,(flagexp "Extra action in tag"
                                           x)
                                 ,@flg-junk)))
               (t
                (setq realact x))))
      (values goodtags
              (or realact (flagexp "Action missing in tag expression"
                                   "__"))
              flg-junk)))

(defun constraints-def-body-parse (def-body qvarbdgs vartranstab timetab dom)
  (multiple-value-bind (items bad-keyword-flg)
                       (keyword-list-smooth
			  def-body
			  '(:precondition :maintain :effect)
			  '())
     (multiple-value-bind (def-body-flg
			   hopefully-empty-vars
			   hopefully-empty-graph
			   precond maint eff vt)
	                  (action-def-body-parse
			     items qvarbdgs nil vartranstab dom)
			  (declare (ignore eff))
	(cond ((not (null hopefully-empty-vars))
	       (cerror "I will ignore them"
		       "Unexpected nonempty :vars for ~s"
		       items)))
        (cond ((and hopefully-empty-graph
                    (not (expansion-graph-is-empty hopefully-empty-graph)))
	       (cerror "I will ignore it"
		       "Unexpected nonempty expansion-graph for ~s"
		       items)))
        (values `(,@def-body-flg ,@bad-keyword-flg)
	        (def-body-annotations
	           timetab precond maint)
		vt))))

(defun def-body-annotations (graph precond maint)
   (cond ((expansion-graph-is-empty graph) '())
	 (t
	  (expansion-graph-add-conds
	      (nconc (cond (precond
			     (list (make-annotation
				       :type 'precondition
				       :time (expansion-graph-begin graph)
				       :prop precond)))
			    (t '()))
		      (cond (maint
			     (list (make-annotation
				      :type 'maintain
				      :time (expansion-graph-main graph)
				      :prop maint)))
			    (t '())))
	      graph))))

(defun qvarbdgs-constraints (qvarbdgs)
   (mapcan #'(lambda (v)
	        (let ((typename (pddl-type-defn (pddl-bdg-val v))))
		   (let ((p (type-predify
				   typename
			           (make-qvar (pddl-bdg-sym v)))))
		     (cond ((not (eq (car p) 'object))
			    (list p))
			   (t '())))))
	   qvarbdgs))

; Make sure free variables reserve their own names.  Any clashing
; bound variable will be renamed.
;(defun bdgs-to-vartranstab (qvarbdgs)
;   (mapcar #'(lambda (v)
;	        (list (pddl-bdg-sym v) (pddl-bdg-sym v)))
;	   qvarbdgs))

; Change type into predicate
(defun type-predify (n x)
      (cond ((symbolp n)
	     `(,n ,x))
	    ((eq (car n) 'either)
	     `(or ,@(mapcar #'(lambda (s) (type-predify s x))
			    (cdr n))))
	    ((eq (car n) 'fluent)
	     `(fluent ,(cadr n) ,x))
	    (t
	     `(object ,x))))
