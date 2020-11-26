(in-package "PDDL")

(defstruct (action-occ
	      (:print-function
	          (lambda (ac srm k)
		     (declare (ignore k))
		     (format srm "#<Occurrence of ~s over ~s>"
			     (action-occ-act-substed ac)
			     (action-occ-time ac)))))
			     
   (act nil)  ; nil when graph
   id
   bdgs
   time   ; interval over which it occurs (symbolic if not fully constrained)
   (timetab nil)   ; constraints in symbolic case
   (intervals '()) ; subintervals added by attempt to find this action
   (annotations '()) ; annotations added
)

; Assume that acts are ground: 
(defun action-occ-act-substed (ac) (action-occ-act ac))

; Records assignment of actual time (integer situation number) to a timepoint
(defstruct (time-asst
	      (:type list)
	      (:constructor make-time-asst (point coord permanent)))
   (point nil) ; :type timepoint
   (coord 0 :type integer)
   permanent)  ; nil if anonymous point, assigned earliest time provisionally

; Times are either integers, pairs of integers, or lists of pairs of integers.

(defun time-exact (tm)
   (cond ((numberp tm) t)
	 ((consp tm)
	  (and (time-exact (car tm))
	       (time-exact (cadr tm))))
	 (t nil)))

(defun time-first (tm)
   (cond ((atom tm) tm)
	 (t
	  (time-first (car tm)))))

(defun time-last (tm)
   (cond ((atom tm) tm)
	 (t
	  (cond ((null (cdr tm))
		 (time-last (car tm)))
		(t
		 (time-last (cdr tm)))))))

; Abbrev for list (sit act), where act leads to sit
(defstruct (sitact (:type list) (:constructor make-sitact (sit act)))
   sit
   act)
; need to add field to keep track of whether it's occurred in an expansion
; -- unless we do a post-check pass of some kind.

; Cache entry for attempt to find action somewhere in interval [l,h]


(defun action-occ-lift (act occ qid)
   (let ((lifted (action-occ-rename occ))
	 (upenv (action-occ-bdgs occ))
	 (downid (action-occ-id occ)))
      (setq upenv (annotations-convert lifted downid qid upenv))
      (setf (action-occ-act lifted)
	    (varsubst act qid upenv))
      (setf (action-occ-bdgs lifted)
	    (bdgenv-contract upenv downid))
      lifted))

(defun action-occ-adapt (act qid cached bdgs)
   (multiple-value-bind (ok e)
			(unify act
			       (action-occ-act-substed cached)
			       qid ground-id*
			       bdgs)
      (cond (ok
	     (let ((new (action-occ-rename cached)))
	        (setf (action-occ-id new) qid)
		(setq e (annotations-convert new (action-occ-id cached) qid e))
		(setf (action-occ-bdgs new) e)
		(list new)))
	    (t '()))))

(defun annotations-convert (occ oldid newid env)
  (let (p)
     (dolist (ann (action-occ-annotations occ))
	(multiple-value-setq (p env)
			     (varid-convert (annotation-prop ann)
					    oldid newid env))
	(setf (annotation-prop ann)
	      p))
     env))

; Given an action-occ, produce a new version with fresh timepoints and time-intervals.
; Don't worry about variables; they may or may not be renamed by another program
; later.
(defun action-occ-rename (occ)
   (let ((at (action-occ-time occ)))
      (cond ((time-exact at)
	     (make-action-occ
		:act (action-occ-act occ)
		:id (action-occ-id occ)
		:bdgs (action-occ-bdgs occ)
		:time at))
	    (t
	     (let ((c (copy-tab)))
		(make-action-occ
		   :act (action-occ-act occ)
		   :id (action-occ-id occ)
		   :bdgs (action-occ-bdgs occ)
		   :time (time-copy at c)
		   :timetab (mapcar #'(lambda (ta)
					 (make-time-asst
					    (thing-copy
						     (time-asst-point ta)
						     c #'tp-init #'tp-fill)
					    (time-asst-coord ta)
					    (time-asst-permanent ta)))
				    (action-occ-timetab occ))
		   :intervals (mapcar #'(lambda (ti)
					   (thing-copy ti c #'ti-init #'ti-fill))
				      (action-occ-intervals occ))
		   :annotations (mapcar #'(lambda (ann)
					     (thing-copy ann c #'an-init #'an-fill))
					(action-occ-annotations occ))))))))

(defun asst-tab-time (tab)
   (cond ((null tab)
	  (error "Empty time-assignment table"))
	 (t
	  (let ((low (time-asst-coord (car tab)))
		(high (time-asst-coord (car tab))))
	     (dolist (ta (cdr tab))
		(let ((c (time-asst-coord ta)))
		   (cond ((< c low)
			  (setq low c))
			 ((> c high)
			  (setq high c)))))
	     (list low high)))))

; If they all agree on a time, take; else return a timepoint
(defun indefinite-solutions-time (graph solns)
   (let ((b (time-interval-begin (expansion-graph-main graph)))
	 (e (time-interval-end (expansion-graph-main graph))))
      (labels ((lookup-time (tp)
		  (let ((tm nil))
		     (dolist (s solns (or tm tp))
			(let ((tpp (assoc tp (car s) :test #'eq)))
			   (cond (tpp
				  (cond (tm
					 (cond ((not (= (cadr tpp) tm))
						; Don't agree
						(return tp))))
					(t
					 (setq tm (cadr tpp)))))))))))
         (list (lookup-time b)
	       (lookup-time e)))))

       
; Return values <earliest, latest> giving range of time coords for
; anonymous point tp.
(defun indef-timepoint-range (tp earliest latest ints prior)
   (let ((pulled-early nil)
	 (pulled-late nil))
      (dolist (c ints)
         (cond ((eq (time-interval-end c)
		    tp)
		(cond ((eq (time-interval-kind c)
			   'pull-early)
		       (setq pulled-early t)))
		(let ((predtime
		         (assoc (time-interval-begin c)
				prior
				:test #'eq)))
		   (cond (predtime
			  (setq earliest
				(max earliest
				     (time-asst-coord predtime))))
			 (t
			  (error "Fumbled predecessor ~s"
				 c)))))
; late-pullers are treated asymmetrically from early-pullers because
; we're working left-to-right, so successors haven't been assigned yet.
	       ((eq (time-interval-begin c)
		    tp)
		(cond ((eq (time-interval-kind c)
			   'pull-late)
		       (setq pulled-late t)))
		(let ((succtime
		         (assoc (time-interval-end c)
				prior
				:test #'eq)))
		   (cond ((and succtime
			       (time-asst-permanent
				succtime))
			  (setq latest
				(min latest
				     (time-asst-coord succtime))))
			 (t
			  (multiple-value-bind (succ-earliest succ-latest)
					       (indef-timepoint-range
					         (time-interval-end c)
						 (cond (succtime
							(time-asst-coord
							   succtime))
						       (t earliest))
						 latest
						 ints prior)
					       (declare (ignore succ-latest))
			     (setq latest
				   (min latest succ-earliest)))))))))
      (cond ((and pulled-early
		  pulled-late)
	     (cond ((= earliest latest)
		    (values earliest latest))
		   (t
		    ; Inconsistent
		    (values 1 0))))
	    ((<= earliest latest)
	     (values earliest latest))
	    (t
	     (values 1 0)))))

(defun check-annotations-at (annotations new-assignments old-assignments
			     id bdgs sits dom)
   (labels ((next-annotation (al bdgs)
	       (cond ((null al)
		      (cond ((check-maintains
				annotations
				new-assignments old-assignments
				id bdgs sits dom)
			     (list (list (append new-assignments
						 old-assignments)
					 bdgs)))
			    (t '())))
		     (t
		      (let ((ann (car al)))
			 (cond ((eq (annotation-type ann) 'precondition)
				(let ((p (assoc (annotation-time ann)
						new-assignments
						:test #'eq)))
				   (cond (p
					  (mapcan
					      #'(lambda (b)
						   (next-annotation
						      (cdr al)
						      b))
					      (deduce (annotation-prop
							 ann)
						      id bdgs
						      (sitact-sit
							 (aref
							    sits
							    (time-asst-coord
							       p)))
						      dom)))
					 (t
					  (next-annotation (cdr al)
							   bdgs)))))
			       (t
				(next-annotation (cdr al) bdgs))))))))
       (next-annotation annotations bdgs)))

(defun check-maintains (annotations new-assignments old-assignments
			id bdgs sits dom)
   (labels ((next-annotation (al)
	       (cond ((null al) t)
		     (t
		      (let ((ann (car al)))
			 (cond ((eq (annotation-type ann) 'maintain)
				(let ((mb (time-interval-begin
					     (annotation-time ann)))
				      (me (time-interval-end
					     (annotation-time ann))))
				   (let ((ab (assoc mb new-assignments
						    :test #'eq))
					 (ae (assoc me new-assignments
						    :test #'eq)))
				      (cond ((or ab ae)
					     (setq ab
						   (or ab
						       (assoc
							   mb old-assignments
							   :test #'eq)))
					     (setq ae
						   (or ae
						       (assoc
							   me old-assignments
							   :test #'eq)))
					     (cond ((and ab ae
							 (time-asst-permanent
							     ab)
							 (time-asst-permanent
							     ae))
						    (do ((i (time-asst-coord
							       ab)
							    (+ i 1))
							 (m (annotation-prop
							        ann)))
							((> i
							    (time-asst-coord
							        ae))
							 (next-annotation
							    (cdr al)))
						      (cond ((null
							       (deduce
								  m id bdgs
								  (sitact-sit
								    (aref
								      sits i))
								  dom))
							     (return nil)))))
						   (t
						    (next-annotation
						        (cdr al)))))
					    (t
					     (next-annotation (cdr al)))))))
			       (t
				(next-annotation (cdr al)))))))))
      (next-annotation annotations)))

(defun time-copy (tm c)
   (cond ((numberp tm) tm)
	 ((timepoint-p tm)
	  (thing-copy tm c #'tp-init #'tp-fill))
	 ((consp tm)
	  (cons (time-copy (car tm) c)
		(time-copy (cadr tm) c)))
	 (t tm)))

(defun tp-init (tp)
   (make-timepoint :whichend (timepoint-whichend tp)))

(defun tp-fill (new old c)
   (setf (timepoint-interval new)
	 (thing-copy (timepoint-interval old)
		     c #'ti-init #'ti-fill)))

(defun ti-init (ti)
   (make-time-interval
      :kind (time-interval-kind ti)
      :action (time-interval-action ti)))

(defun ti-fill (new old c)
   (setf (time-interval-begin new)
	 (thing-copy (time-interval-begin old)
		     c #'tp-init #'tp-fill))
   (setf (time-interval-end new)
	 (thing-copy (time-interval-end old)
		     c #'tp-init #'tp-fill)))

(defun an-init (an)
   (make-annotation
      :type (annotation-type an)
      :prop (annotation-prop an)))

(defun an-fill (new old c)
   (setf (annotation-time new)
	 (let ((tm (annotation-time old)))
	    (cond ((timepoint-p tm)
		   (thing-copy tm c #'tp-init #'tp-fill))
		  ((time-interval-p tm)
		   (thing-copy tm c #'ti-init #'ti-fill))
		  (t  ; bogus, but let it pass
		   tm)))))

; If there are too many solutions for indefinite intervals, just kick
; the whole thing up to the higher level.
;(defun massage-partial-solution (indef-soln intervals qid)
;   (let ((assignments (car indef-soln))
;	 (bdgs (cadr indef-soln)))
;      (list assignments
;	    intervals
;	    annotations
;	    bdgs)))
   



(defun intervals-topo-sort (intervals)
   (let ((output '())
	 (ints (mapcar #'(lambda (int)
			    (cond ((and (eq (time-interval-kind int)
					    'action)
					(equal (time-interval-action int)
					       '(--)))
				   (make-time-interval
				      :kind 'pull-early
				      :action nil
				      :begin (time-interval-begin int)
				      :end (time-interval-end int)))
				  (t int)))
		       intervals))
	 found)
      (loop
         (setq found nil)
         (dolist (int ints)
	    (cond ((not (some #'(lambda (i)
				   (eq (time-interval-begin i)
				       (time-interval-end int)))
			      ints))
		   (setq found int)
		   (return))))
	 (cond (found
		(push found output))
	       ((not found)
		(format *error-output* "Circular graph ~s" intervals)
		(return '*circular)))
	 (setq ints (delete found ints :test #'eq :count 1))
	 (cond ((null ints)
		(return output))))))

(defun expansion-graph-varsubst (gr id bdgs)
   (let ((tc (copy-tab)))
      (labels ((eg-init (eg)
		  (make-expansion-graph
		     :encapsulated (expansion-graph-encapsulated eg)))
	       (eg-fill (new old c)
		  (setf (expansion-graph-main new)
			(let ((m (expansion-graph-main old)))
			   (cond (m
				  (time-interval-varsubst m))
				 (t nil))))
		  (setf (expansion-graph-others new)
			(mapcar #'time-interval-varsubst
				(expansion-graph-others old)))
		  (setf (expansion-graph-tags new)
			(mapcar #'(lambda (tp)
				      (list (car tp)
					    (time-entity-varsubst
					       (cadr tp))))
				(expansion-graph-tags old)))
		  (setf (expansion-graph-subgraphs new)
			(mapcar #'(lambda (sub)
				      (thing-copy sub c #'eg-init #'eg-fill))
				(expansion-graph-subgraphs old)))
		  (setf (expansion-graph-annotations new)
			(mapcar #'(lambda (a)
				     (thing-copy
				        a c #'an-init
					    #'(lambda (new old c)
						      (declare (ignore c))
						 (setf (annotation-time new)
						       (time-entity-varsubst
							  (annotation-time new)))
						 (setf (annotation-prop new)
						       (varsubst
							      (annotation-prop old)
							      id bdgs)))))
				(expansion-graph-annotations old))))
	       (time-entity-varsubst (te)
		  (cond ((timepoint-p te)
			 (timepoint-varsubst te))
			((time-interval-p te)
			 (time-interval-varsubst te))
			(t ; bogus, but what the hell
			 te)))
	       (time-interval-varsubst (ti)
		  (thing-copy
		     ti tc #'ti-init
			   #'(lambda (new old c)
				(setf (time-interval-begin new)
				      (timepoint-varsubst
					 (time-interval-begin old)))
				(setf (time-interval-end new)
				      (timepoint-varsubst
					 (time-interval-end old)))
				(setf (time-interval-action new)
				      (let ((a (time-interval-action old)))
					(cond ((expansion-graph-p a)
					       (thing-copy
						  a c #'eg-init #'eg-fill))
					      (t
					       (varsubst a id bdgs))))))))
	       (timepoint-varsubst (tp)
		  (thing-copy
		     tp tc #'tp-init
		           #'(lambda (new old c)
				     (declare (ignore c))
				(setf (timepoint-interval new)
				      (time-entity-varsubst
					 (timepoint-interval old)))))))
	  (thing-copy gr tc #'eg-init #'eg-fill))))

(defun timepoint-preconds (tp annotations)
   (mapcan #'(lambda (a)
	        (cond ((and (eq (annotation-type a)
				'precondition)
			    (eq (annotation-time a)
				tp))
		       (list (annotation-prop a)))
		      (t '())))
	   annotations))