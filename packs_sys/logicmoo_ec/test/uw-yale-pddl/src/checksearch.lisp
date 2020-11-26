(in-package "PDDL")

(declaim (special flag-count*))

; The goal is to assign a unique time and bdgs.  But if that's impossible,
; we retain intervals and annotations in hopes that higher-level constraints
; will solve everything.

(defstruct actiongoal
   action
   l h
   in-progress
   (recursive nil)
   answers)    ; list of action-occs 

; Records occurrence of an expansion graph in the timeline.
(defstruct (graph-instance)
   bdgs        ; variable bindings
   assignments ; interval assignments
   new-intervals
   new-annotations)

; steps is a list of primitive actions.
; actions is a list of lists of the form (l h act), claiming
; that act occurs (somewhere?) over the interval [l,h].

; Returns <success, action-cache>

(defvar handle-expansions* nil)

(defun solution-file-check (solnfile probfile)
   (let ((probexp 
            (with-open-file (probsrm probfile :direction ':input)
               (read probsrm)))
         (solnexp
            (with-open-file (solnsrm solnfile :direction ':input)
               (read solnsrm))))
      (cond ((and (consp probexp)
                  (eq (car probexp) 'define)
                  (consp (cadr probexp))
                  (eq (caadr probexp) 'problem))
             (eval probexp)
             (let ((prob (get-global-pddl-symbol (cadr (cadr probexp)))))
               (solution-check solnexp '() prob)))
            (t
             (error "Not a problem definition: ~s" probexp)))))

(defun solution-check (steps actions prob)
   (cond ((symbolp prob)
	  (setq prob (get-global-pddl-symbol prob))))
   (cond ((not (problem-p prob))
	  (format t "Not a problem: ~s~%" prob)
	  (return-from solution-check nil)))
   (problem-initialize prob)
   (let ((idom (domain-indexify (problem-domain prob))))
      (let ((sits (make-array (list (+ (length steps) 1))))
	    (okaysofar t))
	 (setf (aref sits 0)
	       (make-sitact (problem-sit prob) nil))
	 (setq okaysofar (sitact-sit (aref sits 0)))
	 (do ((sl steps (cdr sl))
	      (i 1 (+ i 1)))
	     ((or (not okaysofar)
		  (null sl)))
	    (setq okaysofar
		  (project (sitact-sit (aref sits (- i 1)))
			   (car sl) idom))
	    (cond ((not okaysofar)
		   (format t
			   "Projection failed at step ~s ~s~%"
			   i (car sl))
		   (return-from solution-check nil)))
	    (setf (aref sits i)
		  (make-sitact okaysofar (car sl))))
	 (cond ((and (problem-goal prob)
		     (not (deduce (problem-goal prob) dummy-id* '()
				  (sitact-sit (aref sits (length steps)))
				  idom)))
		(format t "Goal not true in final situation~%")
		(return-from solution-check nil))
	       ((null actions)
		(return-from solution-check t))
	       ((not handle-expansions*)
		(format t "Sorry -- :expansion checking not yet implemented~%")
		(return-from solution-check t)))
	 (let ((flag-count* 0)
	       (cache (exp-index-init #'actiongoal-action)))
	    (multiple-value-bind (flg-act graph vt)
				 (parallel-parse 'parallel
						 (cond ((problem-expansion prob)
							(cons (problem-expansion
							         prob)
							      actions))
						       (t actions))
						 '() 'notrans
						 idom)
				 (declare (ignore vt))
	       (cond ((> flag-count* 0)
		      (format t
			      "Syntactic errors in ~s~%"
			      flg-act)
		      nil)
		     (t
		      (let ((answers
			       (expansion-graph-verify
				   graph 0 (length steps)
				   (new-varid) '() '() cache sits idom)))
; don't we have to check the leftover annotations?
			 answers))))))))

(defvar trace-verify* nil)

(defun action-term-verify (act l h qid bdgs constraints cache sits idom)
   (multiple-value-bind (act bdgs)
			(safe-varsubst act qid bdgs)
      (cond (trace-verify*
	     (format t "Verifying ~s over [~s,~s]~%"
		     act l h)))
      (let ((prev (exp-fetch act cache nil)))
	 (dolist (ag prev (action-term-cache-and-verify
			       act l h qid bdgs constraints
			       cache sits idom))
	    (cond ((and (act-subsumes (actiongoal-action ag)
				      act)
			(<= (actiongoal-l ag)
			    l)
			(<= h
			    (actiongoal-h ag)))
		   (cond ((actiongoal-in-progress ag)
			  (setf (actiongoal-recursive ag) t)))
		   (return (match-to-cached-actiongoal
			      ag act qid bdgs))))))))

(defun act-subsumes (act1 act2)
   (let ((id1 (new-varid))
	 (id2 (new-varid)))
      (multiple-value-bind (ok e)
			   (unify act1 act2 id1 id2 '())
	 (and ok
	      (every #'(lambda (b)
			  (not (= (varbdg-id b) id2)))
		     e)))))

(defun match-to-cached-actiongoal (ag act qid bdgs)
   (cond (trace-verify* (format t " ... found in cache~%")))
   (let ((answers
	   (mapcan
	      #'(lambda (a)
		   (multiple-value-bind
			    (ok e)
			    (unify act
				   (action-occ-act-substed a)
				   qid ground-id*
				   bdgs)
		      (cond (ok
			     (list
				(make-action-occ
				   :act act  
				   :id qid
				   :bdgs e
				   :time (action-occ-time a)
				   :timetab (action-occ-timetab a)
				   :intervals (action-occ-intervals a)
				   :annotations
				      (action-occ-annotations a))))
			    (t '()))))
	      (actiongoal-answers ag))))
     (cond (trace-verify*
	    (actiongoals-show answers act)))
     answers))

; act is already fully (and safely) varsubsted.
(defun action-term-cache-and-verify (act l h qid bdgs constraints
				     cache sits idom)
   (let ((ag (make-actiongoal :action act
			      :l l :h l
			      :in-progress t :answers '()))
	 (fctr (car act)))
      (exp-ob-index ag cache t)
      (prog1
	 (let ((afctr (pddl-bdg-val (find-domain-bdg fctr idom)))
	       (aid (new-varid)))
	    (cond ((not (action-functor-p afctr))
		   (error "Action term has nonaction functor: ~s"
			  act)))
	    (let ((adefn (action-functor-defn afctr)))
	       (multiple-value-bind
			(ok abdgs)
			(unify act (action-defn-term adefn)
			       qid aid bdgs)
		  (cond (ok
			 (let ((adefn-precond
				  (varid-convert
				      (append (conj-flatten
					         (action-defn-precond adefn))
					      (action-defn-vars adefn)
					      (action-defn-args adefn))
				      aid qid abdgs)))
			    (let ((allconstraints
				     (append constraints
					     adefn-precond)))
			       (format t "Checking for methods~%")
			       (cond ((action-defn-has-methods adefn)
				      (methods-verify-and-cache
					  (action-methods fctr idom)
					  act l h qid abdgs
					  allconstraints ag cache sits idom))
				     (t
				      (action-term-unify-and-cache
					 act l h qid abdgs
					 allconstraints ag sits idom))))))
			(t
			 (cond (trace-verify*
				(format t "No occurrences of ~s ~
                                           (unification failed)~%"
					act)))
			 '())))))
	  (setf (actiongoal-in-progress ag) nil))))
				        
(defun action-term-unify-and-cache (act l h qid abdgs
				    constraints ag sits idom)
  (do ((i (+ l 1) (+ i 1))
       (solns '()))
      ((> i h)
       (setf (actiongoal-answers ag) 
	     solns)
       (cond (trace-verify*
	      (actiongoals-show solns act)))
       solns)
    (multiple-value-bind (ok e)
			 (unify act
				(sitact-act (aref sits i))
				qid ground-id* abdgs)
       (cond (ok
	      (dolist (ab (deduce `(and ,@constraints) qid e
				  (sitact-sit (aref sits i))
				  idom))
		 (push (make-action-occ
			  :act act
			  :id qid
			  :bdgs ab
			  :time (list (- i 1) i))
		       solns)))))))

(defun methods-verify-and-cache (methods act l h qid abdgs 
				 constraints ag cache sits idom)
  (let ((i 0)
	old-answers new-answers)
    (loop 
       (setf old-answers (actiongoal-answers ag))
       (setf new-answers '())
       (dolist (m methods)
	  (setq new-answers
		(nconc
		    (method-verify
		        m act l h qid abdgs
			constraints cache sits idom)
		    new-answers)))
       (setf (actiongoal-answers ag) new-answers)
       (cond ((or (not (actiongoal-recursive ag))
		  (= (length new-answers)
		     (length old-answers)))
	      (cond (trace-verify*
		     (actiongoals-show new-answers act)))
	      (return new-answers))
	     ((< (length new-answers)
		 (length old-answers))
	      (error "Fumbled recursive answers")))
       (setf i (+ i 1))
       (cond ((> i 50)
	      (error "Apparently infinite recursion"))))))

(defun actiongoals-show (answers act)
   (format t "Occurrences of ~s:~%   ~s~%"
	   act answers))

(defun method-verify (meth act l h qid bdgs
		      constraints cache sits dom)
   (let ((mid (new-varid)))
      (multiple-value-bind (ok mbdgs)
			   (unify act (action-method-term meth)
				  qid mid bdgs)
	 (cond (ok
		(let ((conl
		         (append (action-method-args meth)
				 (action-method-vars meth)
				 (varid-convert constraints qid mid mbdgs)))
		      (graph      ;  (expansion-graph-rename ...)
			    (action-method-graph meth)))
		   (format t "Augmented constraints: ~s~%" conl)
		   (mapcar #'(lambda (ac)
			        (action-occ-lift act ac qid))
			   (expansion-graph-verify
				    graph l h mid mbdgs
				    conl cache sits dom))))
	       (t
		(cerror "Okay" "Unification failure")
		'())))))

; Returns list of action-occs.
(defun expansion-graph-verify (graph l h qid bdgs constraints
			       cache sits idom)
  (let ((ints (intervals-topo-sort
	         (expansion-graph-intervals graph))))
    (cond ((eq ints '*circular)
	   (format t
		   "Circular action expansion~%")
	   '())
	  ((null ints)
	   ; This is either impossible or a no-op
	    (list (make-action-occ
			    :act nil
			    :id qid
			    :bdgs bdgs
			    :time l)))
	  (t
	   (let ((anns (nconc (mapcar #'(lambda (con)
					   (make-annotation
					      :type 'precondition
					      :time (expansion-graph-begin
						       graph)
					      :prop con))
				      constraints)
			      (expansion-graph-annotations graph))))
	      (let ((def-solns
			(definite-intervals-verify
			  ints '() anns
			  l h qid bdgs '() '()
			  cache sits idom))
		    (overall-solns '()))
	         (dolist (ds def-solns)
		    (let ((assts (graph-instance-assignments ds))
			  (bdgs (graph-instance-bdgs ds))
			  (allints (intervals-topo-sort
				      (append ints
					      (graph-instance-new-intervals ds))))
			  (allanns (append anns (graph-instance-new-annotations ds))))
		       (let ((indef-solns
			        (indefinite-intervals-verify
				       allints assts allanns
				       l h qid bdgs sits idom)))
			  (cond ((not (null indef-solns))
				 (cond ((= (length indef-solns) 1)
					(push
					   (make-action-occ
					      :id qid
					      :bdgs (cadar indef-solns)
					      :time (asst-tab-time
						       (caar indef-solns)))
					   overall-solns))
				       (t
					(multiple-value-bind
					       (l h)
					       (indefinite-solutions-time
						  graph
					          indef-solns)
					   (push
					      (make-action-occ
					         :id qid
						 :bdgs bdgs
						 :time (list l h)
						 :intervals allints
						 :timetab assts
						 :annotations allanns)
					      overall-solns)))))))))
		 overall-solns))))))

				       ; [Could also check for case
				       ; where indef-solns agree on
				       ; times and disagree only on
				       ; values for subvariables.]


; prior is a table giving integer values
; to timepoints already encountered.
; Returns list of 
;   <assignment-tab, bdgs, new-intervals>
(defun definite-intervals-verify (ints prior annotations l h qid bdgs
				  newints newanns
				  cache sits idom)
   (cond ((null ints)
	  (list (make-graph-instance
		   :bdgs bdgs
		   :assignments prior
		   :new-intervals newints
		   :new-annotations newanns)))
	 (t
	  (let ((nextint (car ints)))
	     (cond ((member (time-interval-kind nextint)
				'(pull-early pull-late connector compound)
				:test #'eq)
		    ; Skip for now, storing provisional assignment to
		    ; endpoint.  Will be properly constrained
		    ; by indefinite-intervals-verify later.
		    (definite-intervals-verify
		        (cdr ints)
			(cond ((assoc (time-interval-end nextint)
				      prior
				      :test #'eq)
			       prior)
			      (t
			       (cons (make-time-asst
				        (time-interval-end nextint)
					(let ((p (assoc (time-interval-begin
							   nextint)
							prior)))
					   (cond (p (cadr p))
						 (t l)))
					nil)
				     prior)))
			annotations l h qid bdgs
			newints newanns cache sits idom))
		   (t
		    (mapcan #'(lambda (s)
				 (cond ((time-exact (action-occ-time s))
					(let ((new-assts
					         (list (make-time-asst
							  (time-interval-begin
							     (car ints))
							  (time-first
							    (action-occ-time s))
							  t)
						       (make-time-asst
							  (time-interval-end
							     (car ints))
							  (time-last
							   (action-occ-time s))
							  t))))
					  (cond ((check-maintains
						    annotations new-assts prior
						    qid (action-occ-bdgs s)
						    sits idom)
						 (definite-intervals-verify
							 (cdr ints)
							 (cons new-assts prior)
							 annotations
							 l h qid bdgs
							 newints newanns
							 cache sits idom))
						(t '()))))
				       (t
					(definite-intervals-verify
						(cdr ints)
						(action-occ-timetab s)
						annotations
						l h qid bdgs
						(append (action-occ-intervals
							   s)
							newints)
						(append (action-occ-annotations s)
							newanns)
						cache sits idom))))
			    (definite-interval-verify
			         (car ints) prior
				 l h qid bdgs
				 (timepoint-preconds
				    (time-interval-begin (car ints))
				    annotations)
				 cache sits idom))))))))
			     
(defun definite-interval-verify (nextint prior l h qid bdgs
				 constraints cache sits dom)
  (let ((a (time-interval-action nextint))
	(beg (time-interval-begin nextint)))
    (let ((p (assoc beg prior :test #'eq)))
      (cond (p
	     (setq l (max l (cadr p))))))
    (case (time-interval-kind nextint)
       (action
	(action-term-verify a l h qid bdgs constraints
			    cache sits dom))
       (encapsulation
	(let ((ingraph (time-interval-action nextint)))
	   (let ((act (time-interval-action (expansion-graph-main ingraph))))
	      (case (car act)
	         (foreach
		  (foreach-graph-verify act ingraph l h qid bdgs
					constraints cache sits dom))
		 (forsome
		  (expansion-graph-verify ingraph l h qid bdgs
					  (append constraints
						  (qvarbdgs-constraints
						     (cadr act)))
					  cache sits dom))
		 (choice
		  (mapcan #'(lambda (sub)
			       (expansion-graph-verify
				  sub l h qid bdgs
				  constraints cache sits dom))
			  (expansion-graph-subgraphs ingraph)))
		 (t
		  (error "Bad encapsulation ~s"
			 ingraph))))))
       (t
	(error "Bad interval ~s" nextint)))))

(defun foreach-graph-verify (a graph l h qid bdgs constraints cache sits idom)
      ; Syntax checker didn't save internal form of vars, so we have to
      ; rederive it.
  (let ((qvarbdgs (cadr a)))
     (let ((filter (cons (caddr a)
			 (append (qvarbdgs-constraints qvarbdgs)
				 constraints)))
	   (solns '()))
        (do ((i l (+ i 1)))
	    ((> i h))
	  (let ((allinstances (deduce `(and ,@filter)
				      qid bdgs (sitact-sit (aref sits i))
				      idom)))
	     ; Really should check that all free vars in a are bound at this
	     ; point
	     (let ((pgraph
		      (expansion-graphs-parallelize
		          (mapcar #'(lambda (inst)
				       (expansion-graph-varsubst
					   graph qid inst))
				  allinstances)
			  'parallel)))
	        (setq solns
		      (nconc solns
			     (expansion-graph-verify pgraph i h qid bdgs '()
						     cache sits idom))))))
	solns)))

; Returns list of pairs (new-assignment new-bdgs).
(defun indefinite-intervals-verify (intervals prior annotations
				    l h id qvarbdgs sits dom)
   (labels ((try (ints bdgs)
	       (cond ((null ints)
		      (list (list prior bdgs)))
		     (t
		      (mapcan #'(lambda (bs)
				   (mapcan #'(lambda (es)
					        (try (cdr ints) (cadr es)))
					   (point-solve (time-interval-end
							   (car ints))
							(car bs)
							(cadr bs))))
			      (point-solve (time-interval-begin (car ints))
					   prior
					   bdgs)))))
	    (point-solve (tp prior bdgs)
	       (let ((p (assoc tp prior :test #'eq)))
		  (cond ((and p (time-asst-permanent p))
			 (list (list prior bdgs)))
			(t
			 (multiple-value-bind (earliest latest)
					      (indef-timepoint-range
					         tp
						 (cond (p (time-asst-coord p))
						       (t l))
						 h intervals prior)
			    (do ((i earliest (+ i 1))
				 (solns '()))
				((> i latest)
				 solns)
			      (dolist (b (check-annotations-at
					    annotations
					    (list (make-time-asst tp i t))
					    prior
					    id bdgs sits dom))
				 (push b solns)))))))))
      (try intervals qvarbdgs)))

;Sketch of idea: A timepoint is assigned a definite numeric coordinate
;if its interval has a definite noncomposite action (primitive or expanded). 
;Intervals are either: connectors, composites, actions, or encapsulators.
;Only the "action" types have definite endpoints.  A timepoint can be the
;beginpoint or endpoint of more than one interval.  It is definite if it
;occurs as the beginpoint or endpoint of at least one action interval.

;An indefinite timepoint is tested after the entire network is done.  
;It can then be assigned any time coordinate consistent with the intervals
;it occurs in, except that if it occurs as the endpoint of a "pull early" 
;connector, then it must be no later than the latest begin of any 
;"pull early" connector; and if it occurs as the beginpoint of a "pull 
;late" connector, then it must be no later than the earliest endpoint of 
;any "pull late" connector.

;prior tab must record, not just coordinate, but also whether it's 
;definite or provisional

