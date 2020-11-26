(in-package "PDDL")

; This file builds on top of the types in types.lisp to define indexing
; structures for domains.  The idea is that the domain can be defined
; independently of indexing, but whenever it is updated its indexes are
; rebuilt.

;(DEPENDS-ON AT-RUN-TIME UNPOP/ UNIFY SETINDEX HACKS/ EXPDT)

(defvar uvar-topcoord* (topcoord #'qvar-p))
(declaim (special topcoord*))

(setq topcoord* uvar-topcoord*)

(defvar try-rehash* t)

(defvar break-on-rehash-failure* nil)

(defvar rehash-attempts* 0)
(defvar rehash-failures* 0) 

(defmacro try-rehash (rehasher pat avoid avpat &rest args)
   `(cond (try-rehash* 
	   (setf rehash-attempts* (+ rehash-attempts* 1))
	   (let ((dif (discrim ,pat ,avpat uvar-topcoord*)))
	      (cond ((and dif
			  (,rehasher ,pat ,avoid dif ,@args)))
                    (t
                     (cond (break-on-rehash-failure*
                            (cerror "I will proceed"
                               "Rehash failed after no match of ~s and ~s"
                               ,pat ,avoid)))
		     (setf rehash-failures* (+ rehash-failures* 1))))))))


(defstruct (indexed-domain
	      (:include domain)
	      (:print-function
	         (lambda (id srm k)
		    (declare (ignore k))
		    (format srm "#<Indexed domain ~s>"
			    (domain-name id)))))
   (fill-time 0 :type integer)
   (axioms nil)  ; :type exp_index    get packages figured out
;   (action-defns '()) ; alist of functor, defn pairs
   ;(methods nil) ; :type exp_index  ; actions & methods
   (actions nil) ; :type exp_index  ; uniquified action terms
   (occasions nil) 
)

; Action defn with associated term for easy access
;(defstruct (termed-action-defn
;	      (:type list)
;   functor
;   term
;   defn)

(defun indexed-domain-with-name (name create)
   (let ((dom (try-domain-with-name name create)))
      (cond (dom
	     (domain-indexify dom))
	    (t nil))))

(defvar idom*)

(defun domain-indexify (dom)
   (cond ((indexed-domain-p dom)
	  dom)
	 (t
	  (let ((domname (domain-name dom)))
             (multiple-value-bind (dset dget)
                                  (domain-name-procs dom)
	        (let ((value-of-name (funcall dget)))
	           (cond ((indexed-domain-p value-of-name)
		          value-of-name)
		         (t
		          (let ((idom (make-indexed-domain
                                         :fill-time 0
                                         :name (domain-name dom)
                                         :generation (domain-generation dom)
                                         :ancestors (domain-ancestors dom)
                                         :parents (domain-parents dom)
                                         :requirements (domain-requirements dom)
                                         :local-bdgs (domain-local-bdgs dom)
                                         :inherited-bdgs (domain-inherited-bdgs
                                                            dom))))
                             (funcall dset idom)
			     idom)))))))))

(defun fetch-axioms (pat idom)
   ;(indexed-domain-make-current idom)
   (exp-fetch pat (indexed-domain-axioms idom) nil))

;(defun fetch-methods (pat idom)
;   (indexed-domain-make-current idom)
;   (exp-fetch pat (indexed-domain-methods idom) nil))

(defun indexed-domain-make-current (idom)
   (domain-make-current idom)
   (cond ((or (null (indexed-domain-axioms idom))
	      ;(null (indexed-domain-methods idom))
	      (let ((g (indexed-domain-fill-time idom)))
		 (dolist (a (domain-ancestors idom) nil)
		    (cond ((> (domain-generation a) g)
			   (return t))))))
	  ; Some ancestor redefined; rebuild indexes
	  (setf (indexed-domain-axioms idom)
		(exp-index-init #'axiom-consequent))
;	  (setf (indexed-domain-methods idom)
;		(exp-index-init #'termed-action-term))
	  (setf (indexed-domain-generation idom)
		(next-generation))
	  (indexed-domain-fill idom))))

(defun indexed-domain-fill (idom)
   (dolist (dom (indexed-domain-ancestors idom))
       (dolist (bdg (domain-local-bdgs dom))
	  (let ((v (pddl-bdg-val (cadr bdg))))
	     (cond ((rule-group-p v)
		    (rule-group-index v idom))
		   ((pddl-type-p v)
		    ;(format t "Indexing ~s in ~s~%" v dom)
		    (type-index v idom))))))
   (setf (indexed-domain-fill-time idom)
	 (next-generation)))

(defun rule-group-index (rg idom)
  ;(format t "Indexing ~s ~%" rg)
  (dolist (def (rule-group-rules rg))
     (cond ((axiom-p def)
	    ;(format t "    Indexing ~s~%" def)
            (let ((expant (expand-deductive-macros
                                  (axiom-antecedent def)
                                  idom)))
	       (exp-ob-index
                  (cond ((procedural-axiom-p def)
                         (make-procedural-axiom
                            :vars (axiom-vars def)
                            :antecedent expant
                            :consequent (axiom-consequent def)
                            :fcn (procedural-axiom-fcn def)))
                        (t
                         (make-axiom
                            :vars (axiom-vars def)
                            :antecedent expant
                            :consequent (axiom-consequent def))))
		  (indexed-domain-axioms idom)
		  t)))
	   ((action-defn-p def)
            (let ((fctr (action-defn-functor def)))
               (let ((fctrdef (find-domain-bdg-val fctr idom)))
                  (cond ((action-functor-p fctrdef)
                         (pddl-rebind
                            fctr
                            (make-action-functor
                                :name fctr
		                :argtypes (functor-argtypes fctrdef)
		                :defn 
                                   (action-defn-expand-deductive-macros
                                      def
                                      idom))
                            idom))
                        (t
                         (error "Incoherent action defn ~s" def)))))))))

(defun action-defn-expand-deductive-macros (def dom)
   (make-action-defn
      :functor (action-defn-functor def)
      :term (action-defn-term def)
      :args (action-defn-args def)
      :vars (action-defn-vars def)
      :precond (expand-deductive-macros 
                   (action-defn-precond def)
                   dom)
      :effect (expand-deductive-macros
                  (action-defn-effect def)
                  dom)
      :maintain (action-defn-maintain def)
      :has-methods (action-defn-has-methods def)
      :only-in-expansions (action-defn-only-in-expansions def)))

;						(exp-ob-index
;						   (action-termify def)
;						   (indexed-domain-methods
;						      idom)
;						   t)


; Find domain-specific deductive macros and expand
(defun expand-deductive-macros (exp idom)
   (cond ((or (atom exp) (qvar-p exp)) exp)
         (t
          (let ((fcn (car exp)))
             (cond ((member fcn '(and or not when change imply) :test #'eq)
                    `(,fcn ,@(mapcar #'(lambda (e)
                                          (expand-deductive-macros
                                             e idom))
                                     (cdr exp))))
                   ((member fcn '(forall exists) :test #'eq)
                    `(,fcn ,(cadr exp)
                           ,(expand-deductive-macros (caddr exp)
                                                     idom)))
                   (t
                    (let ((b (find-domain-bdg fcn idom)))
                       (cond (b
                              (let ((v (pddl-bdg-val b)))
                                 (cond ((functor-p v)
                                        (let ((fctr (pddl-bdg-val b)))
                                           (cond ((functor-macro fctr)
                                                  (expand-deductive-macros
                                                      (funcall
                                                         (functor-macro fctr)
                                                         exp)
                                                      idom))
                                                 (t
                                                  (mapcar
                                                      #'(lambda (e)
                                                          (expand-deductive-macros
                                                             e idom))
                                                      exp)))))
                                       ((pddl-type-p v)
                                        exp)
                                       (t
                                        (error "Crazy predicate ~s"
                                               exp)))))
                             (t
                              (error "Unknown predicate ~s" exp))))))))))

;type-index creates axioms for sub/super type links
(defun type-index (ty dom)
   (let ((handled '())
	 (typename (pddl-type-defn ty)))
      (labels ((super-if-sub (ty)
		  (cond ((not (member ty handled :test #'eq))
			 (push ty handled)
			 (dolist (par (pddl-type-parents ty))
			    (cond ((not (eq (pddl-type-defn par)
					    'object))
				   (let ((pp (type-predify
						(pddl-type-defn par)
						(make-qvar 'x))))
				      (exp-ob-index
					 (make-procedural-axiom
					    :vars (list '?x)
					    :fcn
					       #'(lambda (qr rid env sit dom)
						         (declare (ignore qr))
						    (collect-subtype-elements
						        ty rid env sit dom))
					    :consequent pp)
					 (indexed-domain-axioms dom)
					 t)
				      (super-if-sub par)))))))))
	  (cond ((and (symbolp typename)
		      (not (eq typename 'object)))
		 (let ((pp (type-predify typename (make-qvar 'x))))
		    (exp-ob-index
		       (make-procedural-axiom
			  :vars (list '?x)
			  :antecedent '(and)
			  :fcn 
			     #'(lambda (qr rid env sit dom)
				       (declare (ignore qr sit))
				  (constant-type-predication
				     ty rid env dom))
			  :consequent pp)
		       (indexed-domain-axioms dom)
		       t))))
	  (super-if-sub ty))))

;(defun action-termify (act-or-meth)
;   (make-termed-action
;      :term (cond ((action-defn-p act-or-meth)
;		   (cons (action-defn-functor act-or-meth)
;			 (var-constraints-args
;			    (action-defn-args act-or-meth))))
;		  (t
;		   (cons (action-method-functor act-or-meth)
;			 (var-constraints-args
;			    (action-method-args act-or-meth)))))
;      :act-or-meth act-or-meth))

(defun var-constraints-args (cl)
   (labels ((find-first-uvar (x)
	       (cond ((qvar-p x) x)
		     ((atom x) nil)
		     (t
		      (or (find-first-uvar (car x))
			  (find-first-uvar (cdr x)))))))
      (mapcar #'find-first-uvar
	      cl)))

; The adds and deletes fields are currently not used.  They are reserved
; for storing inevitable effects of an action (i.e., those that occur no
; matter what the situation).
(defstruct progress-spec
   action
   (adds '*unknown)
   (deletes '*unknown))

(defun uniquify-action (a context)
   (let ((index (indexed-domain-place-action-index context)))
      (dolist (cand (exp-fetch a index t)
		    (let ((new (make-progress-spec :action a)))
		       (exp-ob-index new index t)
		       (progress-spec-action new)))
	 (cond ((equal (progress-spec-action cand)
		       a)
		(return (progress-spec-action cand)))))))

(defun indexed-domain-place-action-index (idom)
   (let ((ai (indexed-domain-actions idom)))
      (cond ((not ai)
	     (setq ai (exp-index-init #'progress-spec-action))
	     (setf (indexed-domain-actions idom)
		   ai)))
      ai))

(defun occasion-prop (occ) OCC)

(defun uniquify-occasion (occ context)
  (let ((index (indexed-domain-place-occasion-index context)))
     (dolist (cand (exp-fetch occ index t)
		   (progn
		       (exp-ob-index occ index t)
		       occ))
	(cond ((equal cand OCC)
	       (return cand))))))

(defun indexed-domain-place-occasion-index (idom)
   (let ((oi (indexed-domain-occasions idom)))
      (cond ((not oi)
	     (setq oi (exp-index-init #'occasion-prop))
	     (setf (indexed-domain-occasions idom)
		   oi)))
      oi))

(defun uniquify-situation (sit path init)
   (let ((ind (initial-situation-sit-index init))
	 (conts (exp_index-contents sit nil)))
      (cond ((not ind)
	     (setq ind (make-eq_setindex #'get-situation-contents))
	     (eq-index-add init ind)
	     (setf (initial-situation-sit-index init)
		   ind)))
      (dolist (cs (elts-eq-index-fetch conts ind)
		  (let ((new (make-subsequent-situation
				:index sit
				:contents conts
				:path path
				:init init)))
		    (eq-index-add new ind)
		    new))
	(cond ((and (exp-index-subconts sit (situation-index cs))
		    (exp-index-subconts (situation-index cs) sit))
	       (return cs))))))

(defstruct (sitsuccspec
	      (:type list))
   act
   sit
   (opt nil :type boolean)
   (adds '() :type list)
   (deletes '() :type list))

(defun initial-situation-domain-indexify (isit)
   (let ((idom (initial-situation-domain isit)))
     (cond ((not (indexed-domain-p idom))
	    (setf idom (domain-indexify idom))
	    (setf (initial-situation-domain isit)
		  idom)))
     (indexed-domain-make-current idom)
     idom))


; Initialize timeless index of problem with all the timeless facts
; from domain and parent sit.
(defun problem-initialize (prob)
   (situation-initialize (problem-sit prob)))

(defun situation-initialize (sit)
      (let ((idom (initial-situation-domain-indexify sit)))
	 (indexed-domain-make-current idom)
	 (cond ((or (not (initial-situation-timeless-index sit))
		    (not (situation-index sit))
		    (some #'(lambda (a)
			   (> (domain-generation a)
			      (initial-situation-generation sit)))
		      (domain-ancestors idom)))
		(let ((timeless-ind (exp-index-init #'occasion-prop))
		      (timeful-ind (exp-index-init #'occasion-prop))
		      (checked-domains '()))
		   (setf (initial-situation-timeless-index sit)
			 timeless-ind)
		   (setf (initial-situation-index sit)
			 timeful-ind)
		   (setf (initial-situation-sit-index sit)
			 nil)
                   (setf (situation-succs sit) '())
		   (labels ((check-next (dom)
			       (cond ((not (member dom checked-domains
						   :test #'eq))
				      (push dom checked-domains)
				      ;(format t "Checking ~s ~%" dom)
				      (dolist (sup (domain-ancestors dom))
					 (check-next sup))
				      (grab-from-domain dom))))
			    (grab-from-domain (pdom)
			       ;(format t "~% Grabbing from ~s :" pdom)
			       (dolist (p (domain-timeless pdom))
				  (add-or-subtract p t))
			       (dolist (v (domain-local-bdgs pdom))
				  (let ((b (pddl-bdg-val (cadr v))))
				     (cond ((constant-p b)
					    ;(format t " Got ~s " b)
					    (let ((ty (constant-type b)))
					       (let ((p (type-predify
							   (pddl-type-defn ty)
							   (car v))))
					          (cond ((not (member (car p)
								      '(object or)
								      :test #'eq))
							 ;(format t " Adding ~s ~%" p)
							 (exp-ob-index
							    (uniquify-occasion
							       p idom)
							    timeless-ind
							    t))))))))))
			    (add-or-subtract (p whether-add)
			       (cond ((eq (car p) 'not)
				      (add-or-subtract
				         (cadr p)
					 (not whether-add)))
				     (t
				      (exp-ob-index
				          (uniquify-occasion p idom)
					  (cond ((type-name-p (car p) idom)
						 timeless-ind)
						(t timeful-ind))
					  whether-add)))))
		      (check-next idom)
		      (do ((next-sit sit (initial-situation-parent next-sit)))
			  ((not next-sit))
			 (dolist (c (initial-situation-delta next-sit))
			    (add-or-subtract c t)))
		      (setf (initial-situation-generation sit)
			    (next-generation))))))
         sit))

(defun get-situation-contents (sit)
   (let ((c (situation-contents sit)))
      (cond ((eq c '*uncomputed)
	     (setq c (exp_index-contents
		         (get-situation-index sit)
			 nil))
	     (setf (situation-contents sit) c)))
      c))
      
; Build it if necessary 
(defun get-situation-index (sit)
      (or (situation-index sit)
	  (cond ((initial-situation-p sit)
		 (situation-index (situation-initialize sit)))
		(t
		 (error "Non-initial situation with no index ~s" sit)))))

;	  (let ((ind (exp-index-init #'occasion-prop)))
;	     (dolist (occ (situation-contents sit))
;		(exp-ob-index occ ind t))
;	     (setf (situation-index sit) ind)
;	     ind)))



(setf universal-ancestors*
      (mapcar #'domain-indexify universal-ancestors*))

(defun all-rule-groups (dom)
   (let ((groups '())
	 (checked-domains '()))
      (labels ((check-next (dom)
		  (cond ((not (member dom checked-domains :test #'eq))
			 (push dom checked-domains)
			 (dolist (bdg (domain-local-bdgs dom))
			    (let ((x (pddl-bdg-val (cadr bdg))))
			      (cond ((rule-group-p x)
				     (push x groups)))))
			 (dolist (sup (domain-ancestors dom))
			    (check-next sup))))))
	  (check-next dom)
	  groups)))

(defun action-methods (fctr idom)
   (let ((gl (all-rule-groups idom))
	 (meths '()))
      (dolist (g gl)
	 (dolist (x (rule-group-rules g))
	    (cond ((and (action-method-p x)
			(eq (action-method-functor x)
			    fctr))
		   (push x meths)))))
      meths))

(defmacro def-deductive-macro (name domain args &rest body)
   `(define-ded-macro
       ',name
       (must-domain-with-name ',domain)
       #'(lambda ,args ,@body)))
  
(defun must-domain-with-name (name)
   (let ((dom (try-domain-with-name name nil)))
      (cond ((not dom)
             (error "Undefined domain ~s" name)))
      dom))
                    
(defun define-ded-macro (name dom fcn)
   (let ((fctrb (find-domain-bdg 'equation dom)))
     (cond ((and fctrb (functor-p (pddl-bdg-val fctrb)))
            (setf (domain-generation dom)
                  (next-generation))
            (setf (functor-macro (pddl-bdg-val fctrb))
                  fcn))
           (t
            (error "Can't define ~s as deductive macro (must be declared as predicate)"
                   name)))))
