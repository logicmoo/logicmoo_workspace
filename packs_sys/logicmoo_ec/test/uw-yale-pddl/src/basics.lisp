(in-package "PDDL")

(defun add-to-rule-group (x rg)
   (push x (rule-group-rules rg)))

(defun expansion-graph-new-layer (eg act)
   (cond ((expansion-graph-is-empty eg)
	  (singleton-expansion-graph act))
         (t
	  (make-expansion-graph
	     :main (make-time-interval
		      :kind 'compound
		      :action act
		      :begin (expansion-graph-begin eg)
		      :end (expansion-graph-end eg))
	     :others (cons (expansion-graph-main eg)
			   (expansion-graph-others eg))
	     :tags (expansion-graph-tags eg)
	     :annotations (expansion-graph-annotations eg)
	     :subgraphs (list eg)))))

; choice, foreach, and forsome conceal their subgraphs
(defun expansion-graph-encapsulate (eg)
	  (make-expansion-graph
	     :encapsulated t
	     :main (make-time-interval 
		      :kind 'encapsulation
		      :action eg
		      :begin (expansion-graph-begin eg)
		      :end (expansion-graph-end eg))
	     :others '()
	     :tags '()
	     :annotations '()
	     :subgraphs (list eg)))

(defun expansion-graph-add-conds (annotations timetab)
   (cond ((null annotations) timetab)
         (t
          (make-expansion-graph
             :main (expansion-graph-main timetab)
             :others (expansion-graph-others timetab)
             :tags (expansion-graph-tags timetab)
             :annotations (append annotations (expansion-graph-annotations timetab))
             :subgraphs (expansion-graph-subgraphs timetab)))))

(defun annotations-unparse (timetab)
   (mapcan #'(lambda (a)
	        (list (case (annotation-type a)
			 (precondition ':precondition)
			 (maintain ':maintain)
			 (effect ':effect))
		      (annotation-prop a)))
	   (expansion-graph-annotations timetab)))

(defun expansion-graph-add-tags (tags timetab)
   (cond ((or (null tags)
              (expansion-graph-is-empty timetab))
          timetab)
         (t
	  (let ((newtab (make-expansion-graph
			   :main (expansion-graph-main timetab)
			   :others (expansion-graph-others timetab)
			   :tags (expansion-graph-tags timetab)
			   :annotations (expansion-graph-annotations timetab)
			   :subgraphs (expansion-graph-subgraphs timetab))))
	     (let ((overall-int (expansion-graph-main newtab)))
		(dolist (tag tags)
		   (cond ((symbolp tag)
			  (push (list tag overall-int)
				(expansion-graph-tags newtab)))
			 (t
			  (push (list (cadr tag)
				      (cond ((eq (car tag) '<)
					     (time-interval-begin
                                                overall-int))
					    (t (time-interval-end
                                                   overall-int))))
				(expansion-graph-tags newtab)))))
		newtab)))))

(defun expansion-graphs-parallelize (gl afctr)   ; act
   (setq gl (remove-if #'expansion-graph-is-empty
                       gl))
   (cond ((null gl)
          (singleton-expansion-graph `(,afctr)))
         (t
          (let ((alltags (mapcan #'(lambda (eg)
                                      (copy-list (expansion-graph-tags eg)))
                                 gl))
                (allannotations (mapcan #'(lambda (eg)
                                           (copy-list (expansion-graph-annotations eg)))
                                       gl))
                (newmain (make-time-interval
			    :kind 'compound
                            :action `(,afctr
				        ,@(mapcar #'expansion-graph-act
						  gl))
                            :begin nil :end nil))
                (connectors '()))
             (cond ((null (cdr gl))
		    (setf (time-interval-begin newmain)
			  (expansion-graph-begin (car gl)))
		    (setf (time-interval-end newmain)
			  (expansion-graph-end (car gl))))
                   (t
                    (let ((newbeg (make-timepoint
				     :whichend '<
				     :interval newmain))
			  (newend (make-timepoint
				     :whichend '>
				     :interval newmain)))
		       (setf (time-interval-begin newmain)
			     newbeg)
		       (setf (time-interval-end newmain)
			     newend)
		       (dolist (eg gl)
			  (push (make-time-interval
				   :kind 'pull-early
				   :action nil
				   :begin (expansion-graph-end eg)
				   :end newend)
				connectors)
                          (push (make-time-interval
				   :kind 'pull-late
                                   :action nil
                                   :begin newbeg
                                   :end (expansion-graph-begin eg))
                                connectors)))))
	     (make-expansion-graph
                   :main newmain
                   :others (append connectors (expansion-graphs-union gl))
                   :subgraphs gl
                   :annotations allannotations
                   :tags alltags)))))

(defun expansion-graphs-serialize (gl)   ;act
   (setq gl (remove-if #'expansion-graph-is-empty
                       gl))
   (cond ((null gl) (singleton-expansion-graph '(series)))
         (t
          (let ((newmain (make-time-interval
			     :kind 'compound
                             :action `(series ,@(mapcar
						   #'expansion-graph-act
						   gl))
                             :begin (expansion-graph-begin (car gl))
                             :end (expansion-graph-end (car (last gl)))))
                (alltags (mapcan #'(lambda (eg)
                                      (copy-list (expansion-graph-tags eg)))
                                 gl))
                (allannotations (mapcan #'(lambda (eg)
                                           (copy-list (expansion-graph-annotations eg)))
                                       gl))
		(connectors
                   (do ((graphs gl (cdr graphs))
                        (cl '() (cons (make-time-interval
				         :kind 'connector
				         :action nil
					 :begin (expansion-graph-end
						   (car graphs))
					 :end (expansion-graph-begin
					         (cadr graphs)))
				      cl)))
	               ((or (null graphs) (null (cdr graphs)))
                        (reverse cl)))))
             (make-expansion-graph
                :main newmain
                :others (append connectors (expansion-graphs-union gl))
                :subgraphs gl
                :tags alltags
                :annotations allannotations)))))

; Collect all time intervals
(defun expansion-graphs-union (gl)
   (mapcan #'(lambda (eg)
	        (cons (expansion-graph-main eg)
		      (copy-list (expansion-graph-others eg))))
	   gl))

(defun singleton-expansion-graph (act)
   (make-expansion-graph
       :main (new-time-interval (cond (act 'action) (t 'connector))
				act)
       :others '()
       :tags '()
       :annotations '()
       :subgraphs '()))

; Returns augmented graph + interval denoting whole series
(defun time-entities-serialize (points-n-intervals graph)
   (cond ((null points-n-intervals)
	  (values graph nil))
	 ((null (cdr points-n-intervals))
	  (values graph (car points-n-intervals)))
         (t
          (do ((entl points-n-intervals (cdr entl)))
              ((or (null (cdr entl))
                   (not (find-time-interval
                           (time-entity-end (car entl))
                           (time-entity-begin (cadr entl))
                           graph)))
               (cond ((null (cdr entl))
                      (let ((old (find-time-interval
                                    (time-entity-begin
                                       (car points-n-intervals))
                                    (time-entity-end
                                        (car (last points-n-intervals)))
                                    graph)))
                         (cond (old
                                ; already serialized
                                (values graph old))
                               (t
                                (adjoin-serialization
                                   points-n-intervals graph)))))
                     (t
                      (adjoin-serialization
                         points-n-intervals graph))))))))

(defun adjoin-serialization (points-n-intervals graph)
   (let ((newgraph (expansion-graph-copy graph)))          
     (do ((entl points-n-intervals (cdr entl)))
	 ((null (cdr entl))
          (let ((whole
                   (graph-adjoin-interval
		      'compound
                      (time-entity-begin (car points-n-intervals))
	              (time-entity-end (car (last points-n-intervals)))
                      nil
                      newgraph)))
             (values newgraph whole)))
       (graph-adjoin-interval
	  'connector
          (time-entity-end (car entl))
          (time-entity-begin (cadr entl))
          nil
          newgraph))))

; Returns augmented graph + interval for entire parallelization
(defun time-entities-parallelize (points-n-intervals graph)
   (cond ((null points-n-intervals)
          (values graph nil))
         ((null (cdr points-n-intervals))
          (values graph (car points-n-intervals)))
         (t
          (let ((forepoint
                   (do ((fp (expansion-graph-begin-points graph)
                            (cdr fp)))
                       ((or (null fp)
                            (every #'(lambda (e)
                                        (find-time-interval
                                           (car fp) (time-entity-begin e)
                                           graph))
                                   points-n-intervals))
                        (cond ((null fp) nil)
                              (t (car fp))))))
                (aftpoint
		   (do ((ap (expansion-graph-end-points graph)
                            (cdr ap)))
                       ((or (null ap)
                            (every #'(lambda (e)
                                        (find-time-interval
                                           (time-entity-end e)
                                           (car ap) graph))
                                   points-n-intervals))
                        (cond ((null ap) nil)
                              (t (car ap)))))))
             (cond ((and forepoint aftpoint)
                    (let ((whole (find-time-interval
                                    forepoint aftpoint graph)))
                       (cond (whole
                              (values graph whole))
                             (t
                              (adjoin-parallelization
                                 points-n-intervals forepoint aftpoint
                                 graph)))))
                   (t
                    (adjoin-parallelization
                       points-n-intervals forepoint aftpoint graph)))))))

(defun adjoin-parallelization (points-n-intervals forepoint aftpoint graph)
   (let ((newgraph (expansion-graph-copy graph))
         (par-int (make-time-interval :kind 'compound
				      :action nil :begin nil :end nil)))
      (push par-int (expansion-graph-others newgraph))
      (cond ((not forepoint)
             (setq forepoint
                   (make-timepoint :whichend '< :interval par-int))))
      (cond ((not aftpoint)
             (setq aftpoint
                   (make-timepoint :whichend '> :interval par-int))))
      (setf (time-interval-begin par-int) forepoint)
      (setf (time-interval-end par-int) aftpoint)
      (do ((el points-n-intervals (cdr el)))
          ((null el))
         (graph-adjoin-interval 'pull-late
				forepoint
                                (time-entity-begin (car el))
                                nil
                                newgraph)
         (graph-adjoin-interval 'pull-early
				(time-entity-end (car el))
                                aftpoint
                                nil
                                newgraph))
      (values newgraph par-int)))

; This alters the graph; use only when graph is under construction
; Returns old or new interval
(defun graph-adjoin-interval (kind beg end act graph)
   (let ((old
            (cond (act nil)
                  (t
                   (find-time-interval
		      beg end graph)))))
      (or old
          (let ((new (make-time-interval
		        :kind kind
                        :begin beg :end end :action act)))
             (push new (expansion-graph-others graph))
             new))))

(defun find-time-interval (beg end graph)
   (cond ((and (eq (time-interval-begin
		      (expansion-graph-main graph))
		   beg)
	       (eq (time-interval-end
		      (expansion-graph-main graph))
		   end))
	  (expansion-graph-main graph))
	 (t
          (do ((il (expansion-graph-others graph)
                   (cdr il)))
              ((or (null il)
                   (and (eq (time-interval-begin (car il))
                            beg)
                        (eq (time-interval-end (car il))
                            end)))
               (cond ((null il) nil)
                     (t (car il))))))))

(defun time-entity-begin-or-end (e which)
   (case which
      (< (time-entity-begin e))
      (> (time-entity-end e))
      (t e)))

(defun domain-reset (dom)
   (setf (domain-generation dom)  ;(+ (domain-generation dom) 1) ??
	 (next-generation))
   (setf (domain-local-bdgs dom) '())
   (cond ((domain-inherited-bdgs dom)
          (clrhash (domain-inherited-bdgs dom)))))
; Should it be marked "under construction" somehow?

; By convention, the actions, etc. of a domain are stored in a
; rule group whose "name" is the domain itself (not the name of the domain)
(defun own-rule-group (dom)
   (let ((rg (domain-place-rule-group dom dom)))
      (cond ((not (rule-group-p rg))
             (error "Domain's own rule group fumbled ~s (~s)"
                    dom rg)))
      rg))

(defun domain-place-rule-group (dom name)
   (let ((rgb (assoc name (domain-local-bdgs dom) :test #'eq))
	 rg)
      (cond (rgb
	     (setf rg (pddl-bdg-val (cadr rgb))))
	    (t
	     (setf rg (make-rule-group
			:name name
			:domain dom
			:generation (domain-generation dom)
			:rules '()))
             (setq rgb (make-pddl-bdg
			  :sym name
			  :val rg
			  :domain dom))
	     (push (list name rgb)
		   (domain-local-bdgs dom))))
      (cond ((rule-group-p rg)
             rg)
            (t (format-flg name
                  "Not a rule group (value ~s)"
                  rg)))))

(defun domain-place-situation (dom name)
   (let ((s (place-domain-bdg name dom)))
      (cond ((pddl-bdg-unbound s)
	     (let ((new
		      (make-initial-situation
		       :name name
		       :domain nil
		       :generation 0  ; initially not up to date
		       :delta '()
		       :sit-index nil)))
	        (let ((subdom (new-domain `(domain ,new))))
		   (setf (domain-parents subdom) (list dom))
		   (setf (initial-situation-domain new) subdom)
		   (reset-ancestors subdom))
		(setf (pddl-bdg-val s) new)))
	    ((not (situation-p (pddl-bdg-val s)))
	     (error "Not a situation: (value ~s)"
		    s)))
      (pddl-bdg-val s)))

(defun try-requirement-with-name (name create)
   (multiple-value-bind (v okay)
                        (get-global-pddl-symbol name)
      (cond (okay v)
            (create
             (let ((new (make-requirement
                           :name name :builtins '() :implies '())))
                (set-global-pddl-symbol name new)
                new))
            (t nil))))

(defun try-domain-with-name (name create)
   (multiple-value-bind (v okay)
			(get-global-pddl-symbol name)
      (cond (okay v)
            (create
	     (let ((new (new-domain name)))
	        (set-global-pddl-symbol name new)
		new))
            (t nil))))

(defun domain-declares-requirement (dom reqname)
  (domain-make-current dom)
  (some #'(lambda (anc)
	     (some #'(lambda (req)
			(eq (requirement-name req) reqname))
		   (domain-requirements anc)))
	(domain-ancestors dom)))

(defun requirement-implications (req)
   (labels ((pursue (req)
               (adjoin req
                       (mapcan #'pursue
                               (requirement-implies req))
                       :test #'eq)))
      (pursue req)))

(defun tag-already-bound (sym graph dom)
   (let ((p (assoc sym (expansion-graph-tags graph))))
      (cond (p `(tag ,sym))
	    (t
	     (symbol-any-value sym dom)))))

(defun symbol-any-value (sym dom)
   (let ((bdg (place-domain-bdg sym dom)))
      (cond ((pddl-bdg-unbound bdg)
             (multiple-value-bind (v okay)
                                  (get-global-pddl-symbol sym)
                (cond (okay `(global ,v))
                      (t nil))))
            (t
             `(local ,(pddl-bdg-val bdg))))))

(defun find-domain-bdg-val (sym dom)
   (let ((bdg (find-domain-bdg sym dom)))
      (cond (bdg (pddl-bdg-val bdg))
            (t nil))))

(defun find-domain-bdg (sym dom)
   (find-or-place-domain-bdg sym dom nil))

(defun place-domain-bdg (sym dom)
   (find-or-place-domain-bdg sym dom t))

(defun find-or-place-domain-bdg (sym dom create)
   ;(domain-make-current dom)
   (let ((inh (place-domain-inherited-bdgs dom)))
      (let ((pb (gethash sym inh)))
	 (or pb
	     (labels ((cache-bdg (b)
			 (setf (gethash (pddl-bdg-sym b) inh)
			       b)
			 b))
		(dolist (a (domain-ancestors dom)
                           (cond (create
				  (let ((pb (make-pddl-bdg
						  :sym sym
						  :val unbound-sym-marker*
						  :domain dom)))
				    (push (list sym pb)
                                          (domain-local-bdgs dom))
				    (cache-bdg pb)))
                                 (t nil)))
		    (let ((pb (assoc sym (domain-local-bdgs a) :test #'eq)))
		       (cond (pb
			      (return (cache-bdg (cadr pb))))))))))))

; When place-domain-bdg finds a binding, it copies it to the local table.
; Side effects to local bdg are seen in original scope.
; The following makes a new binding, which does not share with the old.
(defun pddl-rebind (sym val dom)
   (let ((inh (place-domain-inherited-bdgs dom)))
      (setf (gethash sym inh)
            (make-pddl-bdg
                :sym sym
                :val val
                :domain dom))))

(defun place-domain-inherited-bdgs (dom)
   (let ((inh (domain-inherited-bdgs dom)))
      (cond ((not inh)
             (setq inh (make-hash-table :test #'eq))
             (setf (domain-inherited-bdgs dom)
                   inh)))
      inh))

(defun place-local-domain-bdg (sym dom)
   (let ((pb (assoc sym (domain-local-bdgs dom))))
      (cond ((not pb)
	     (setq pb (list sym (make-pddl-bdg
				   :sym sym
				   :val unbound-sym-marker*
				   :domain dom)))
	     (push pb (domain-local-bdgs dom))))
      (cadr pb)))

; Check that ancestors haven't changed.
(defun domain-make-current (dom)
   (cond ((not (domain-is-current dom))
	  (reset-ancestors dom))))

(defun domain-is-current (dom)
   (and (not (null (domain-ancestors dom)))
        (let ((g (domain-generation dom)))
           (dolist (a (domain-ancestors dom) t)
              (cond ((> (domain-generation a) g)
                     (return nil)))))))

; This really should check if dom inherits symbols
; multiply.
; Also: need to formalize order to look for domain vars in

(defvar universal-ancestors* '()
   "Domains inherited by absolutely every domain except themselves")

(defun reset-ancestors (dom)
   (let ((ancl universal-ancestors*))
      (setf (domain-generation dom) 0)
      (cond ((domain-inherited-bdgs dom)
             (clrhash (domain-inherited-bdgs dom))))
      (labels ((note (d)
		 (domain-make-current d)
		 ;(format t "Noting ~s~%" d)
		 ;(setq ancl (adjoin d ancl :test #'eq))
		 (setq ancl
		       (nconc
			  (mapcan #'(lambda (a)
				       (cond ((some #'(lambda (r)
							(same-domain
						           a r))
						    ancl)
					      '())
					     (t (list a))))
				  (domain-ancestors d))
			  ancl))))
	 (dolist (par (domain-parents dom))
            (note par))
         (labels ((collect-req (req)
		     (dolist (b (requirement-builtins req))
			(note b))
                     (dolist (im (requirement-implies req))
                        (collect-req im))))
	    (dolist (req (domain-requirements dom))
               (collect-req req)))
         ; Make sure domain is in its own ancestor list
	 (setf (domain-ancestors dom)
               (cons dom (remove dom ancl :test #'same-domain)))
	 (setf (domain-generation dom)
	       (next-generation))
	 ancl)))

(defun new-domain (name)
   (let ((new (make-domain
                 :name name
                 :generation (next-generation)
                 :ancestors '()
                 :parents '()
                 :requirements '()
                 :local-bdgs '())))
      new))

(defvar generation* 0
   "Counter used to mark 'freshness' of domain, which should be fresher than all its ancestors")

(defun next-generation ()
   (setq generation* (+ generation* 1)))

(defun set-global-pddl-symbol (sym val)
   (setf (gethash sym pddl-symbol-table*) val)
   val)

(defun get-global-pddl-symbol (sym)
  (gethash sym pddl-symbol-table*))

(defun const-of-type (c pt)
   (or (let ((tester (pddl-type-const-tester pt)))
	  (and tester
	       (funcall tester c)))
       (and (either-type-p pt)
            (some #'(lambda (ct) (const-of-type c ct))
                  (either-type-components pt)))
       (some #'(lambda (par) (const-of-type c par))
	     (pddl-type-parents pt))
       (cond ((fluent-type-p pt)
	      (const-of-type c (fluent-type-base pt)))
	     ((expression-type-p pt)
	      (const-of-type c (expression-type-actual pt)))
	     (t nil))))

(defun arg-subtypes (at1 at2)
   (cond ((null at1)
          (cond ((null at2) t)
                (t nil)))
         ((null at2)
          nil)
         ((eq (car at1) '&rest)
          (cond ((eq (car at2) '&rest)
                 (arg-subtypes (cdr at1) (cdr at2)))
                (t nil)))
         ((eq (car at2) '&rest)
          nil)
         ((is-subtype (car at1) (car at2))
          (arg-subtypes (cdr at1) (cdr at2)))
         (t nil)))

(defun is-subtype (t1 t2)
   (or (eq t1 t2)
       (and (either-type-p t2)
            (some #'(lambda (ct) (is-subtype t1 ct))
                  (either-type-components t2)))
       (some #'(lambda (pt) (is-subtype pt t2))
	     (pddl-type-parents t1))
       (cond ((fluent-type-p t2)
	      (cond ((fluent-type-p t1)
		     (is-subtype (fluent-type-base t1)
				 (fluent-type-base t2)))
		    (t
		     (is-subtype t1 (fluent-type-base t2)))))
	     ((expression-type-p t2)
	      (cond ((expression-type-p t1)
		     (is-subtype (expression-type-actual t1)
				 (expression-type-actual t2)))
		    (t
		     (is-subtype t1 (expression-type-actual t2)))))
	     (t nil))))

(defun pathname-new-extension (pname ext)
   (make-pathname
       :host (pathname-host pname)
       :device (pathname-device pname)
       :directory (pathname-directory pname)
       :name (pathname-name pname)
       :type ext))

(defun flag-wrong-type (ob name type)
   (format-flg name
      "Wrong type (value ~s, not of type ~t)"
      ob type))

(defun qvar-lookup (sym bdgs)
   (do ((bl bdgs (cdr bl)))
       ((or (null bl)
            (eq (pddl-bdg-sym (car bl)) sym))
        (cond ((null bl) nil)
              (t (car bl))))))

;(defvar universal-ancestors*)

; Inherited by everyone:

(defvar basic-domain*
        (let ((bd (make-domain
                     :name 'pddl-basics
                     :generation 0
                     :ancestors '()
                     :parents '()
                     :requirements '()
                     :local-bdgs '()
                     :inherited-bdgs nil)))
           (setf (domain-ancestors bd) (list bd))
           (set-global-pddl-symbol 'basic bd)
           (setf (domain-local-bdgs bd)
                 (list (list 'object
			     (make-pddl-bdg
			        :sym 'object
				:val object-type*
				:domain bd))
                       (list 'proposition
                             (make-pddl-bdg
                                :sym 'proposition
                                :val object-type*
                                :domain bd))
                       (list 'action
                             (make-pddl-bdg
                                :sym 'action
                                :val action-type*
                                :domain bd))))
           bd)
  "Domain inherited by all other domains")
          
(setq universal-ancestors* (list basic-domain*))
(set-global-pddl-symbol 'pddl-basics basic-domain*)

(defun type-name-p (sym dom)
   (let ((b (find-domain-bdg sym dom)))
      (and b (pddl-type-p (pddl-bdg-val b)))))
