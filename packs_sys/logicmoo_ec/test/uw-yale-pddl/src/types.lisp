(IN-PACKAGE "PDDL")

(defvar pddl-version* "1.6")

(defvar pddl-read-table*)

(set-macro-character #\? #'(lambda (s c) (declare (ignore c))
			      (let ((x (read s)))
				 (list '\? x)))
		     pddl-read-table*)

(defun qvar-p (x) (and (consp x) (eq (car x) `\?)))
(defun qvar-sym (x) (cadr x))
(defun make-qvar (sym) `(\? ,sym))

; These are used to flag errors on output
(defstruct (flagged-subexpression
	       (:print-function
		  (lambda (fs srm k)
		     (declare (ignore k))
                     (cond (*print-pretty*
                            (pprint-logical-block (srm nil
                                                    :prefix "<<"
                                                    :suffix ">>")
                               (write (flagged-subexpression-message fs)
                                      :stream srm
                                      :escape nil)
                               (pprint-indent :block 0 srm)
                               (pprint-newline :linear srm)
                               (pprint (flagged-subexpression-exp fs)
                                      srm)))
                           (t
		            (format srm "<<~a: ~s>>"
				 (flagged-subexpression-message fs)
				 (flagged-subexpression-exp fs)))))))
   (message "" :type string)
   exp)

(defvar flag-count* 0
   "Count of number of expressions flagged")

(defun flagexp (mess exp)
   (setq flag-count* (+ flag-count* 1))
   (make-flagged-subexpression :message mess :exp exp))

(defun format-flg (exp &rest format-args)
   (flagexp (with-output-to-string (srm)
               (apply #'format srm format-args))
            exp))

(defstruct (requirement
	      (:print-function
	          (lambda (req srm k)
		     (declare (ignore k))
		     (format srm "#<Requirement ~s>"
			     (requirement-name req)))))
   (name nil :type symbol)
   (builtins '() :type list)
   (implies '() :type list))
; builtins is a list of builtin domains to be inherited by
; domains declaring this requirement
; implies is a list of requirements implied by this one.

(defstruct (pddl-type
	      (:print-function
	         (lambda (pt srm k)
		    (declare (ignore k))
		    (format srm "#<PDDL-type ~s>"
			    (pddl-type-defn pt)))))
   defn  ; name or compound thingie
   (const-tester nil)        ; predicate to test Lisp constants
   (parents '() :type list)) ; list of supertypes

(defvar type-type*
    (make-pddl-type :defn 'type)
  "The type of pddl types")

(defun any (x)
   (declare (ignore x))
   t)

(defvar object-type*
    (make-pddl-type :defn 'object
                    :const-tester #'any))

(defvar proposition-type* (make-pddl-type :defn 'proposition
					  :parents (list object-type*)))

(defvar action-type* (make-pddl-type :defn 'action
				     :parents (list object-type*)))

(defvar fluent-type* (make-pddl-type :defn 'fluent
                                     :parents (list object-type*)))

; This is a bit of a crock, for handling evaluation contexts
(defvar expression-type* (make-pddl-type :defn 'expression
                                         :parents (list object-type*)))

(defstruct (fluent-type
              (:include pddl-type))
   (base nil)) ; :type pddl-type

(defstruct (either-type
              (:include pddl-type))
   (components nil :type list))

(defstruct (expression-type
              (:include pddl-type))
   (actual nil))  ; :type pddl-type

(defvar pddl-symbol-table* (make-hash-table :test #'eq)
  "Name space for requirements, domains, and problems")
; and macros?

; Binding of names with domain scope, as well as quantified variables.
(defstruct (pddl-bdg
	      (:print-function
	          (lambda (pb srm k)
		     (declare (ignore k))
		     (format srm "#<Bdg ~s>" (pddl-bdg-sym pb)))))
   (sym nil)  ; :type symbol -- usually
   val domain)  ; where the binding lives normally.
; Represents quantified-var bindings, in which case val is type.

(defvar unbound-sym-marker* (list 'unbound-symbol)
  "Val of sym until it has a real val")

(defun pddl-bdg-unbound (bdg) (eq (pddl-bdg-val bdg) unbound-sym-marker*))

(defstruct (constant
              (:print-function
                  (lambda (pc srm k)
                     (declare (ignore k))
                     (format srm "#<Constant ~s>"
                                 (constant-name pc)))))
   (name nil :type symbol)
   (type nil))  ; :type pddl-type

(defstruct (domain-var
              (:print-function
                 (lambda (dv srm k)
                    (declare (ignore k))
                    (format srm "#<Domain variable ~s=~s>"
                            (domain-var-name dv)
                            (domain-var-val dv)))))
   (name nil :type symbol)
   val
   (type nil))  ; :type pddl-type

; For predicates, actions, and eval-cntxt thingies.
(defstruct (functor
              (:print-function
                  (lambda (fctr srm k)
                     (declare (ignore k))
                     (case (pddl-type-defn (functor-rangetype fctr))
                        (proposition
                         (format srm "#<Predicate ~s>"
                                     (functor-name fctr)))
                        (action
                         (format srm "#<Rogue action functor ~s>"
                                     (functor-name fctr)))
                        (t
                         (format srm "#<Functor ~s>"
                                     (functor-name fctr)))))))
   (name nil :type symbol)
   (rangetype nil)  ; :type pddl-type  ; proposition or action
   (argtypes '() :type list)
   (macro nil))

; Rules are axioms, action schemas, :safety-conditions, and :methods.
; Contents of rule-group are rules.
(defstruct (rule-group
	      (:print-function
	          (lambda (rg srm k)
		     (declare (ignore k))
		     (cond ((eq (rule-group-name rg)
				(rule-group-domain rg))
			    (format srm "#<Main rule group for ~s>"
				        (rule-group-domain rg)))
			   (t
			    (format srm "#<Rule group ~s in ~s>"
				    (rule-group-name rg)
				    (rule-group-domain rg)))))))
   (name nil)  ; :type (or symbol domain)
   (domain nil)  ; :type domain
   (generation 0 :type integer)
   (rules '() :type list))

(defstruct (axiom
	     (:print-function
	        (lambda (ax srm k)
		  (declare (ignore k))
		  (format srm "#<Axiom ~s>"
			  (axiom-consequent ax)))))
   vars
   antecedent
   consequent)

; "Axiom" defining procedural attachment
(defstruct (procedural-axiom
	     (:include axiom)
	     (:print-function
	        (lambda (ax srm k)
		  (declare (ignore k))
		  (format srm "#<Procedural axiom ~s>"
			  (axiom-consequent ax)))))
   fcn)

(defstruct (action-defn
	     (:print-function
	        (lambda (ad srm k)
		  (declare (ignore k))
		  (format srm "#<Action defn ~s>"
			  (action-defn-functor ad)))))
   (functor nil :type symbol)
   term
   args    ; list of preconds
   vars    ; ditto
   precond
   effect
   (maintain nil)
   has-methods
   only-in-expansions)

(defstruct (action-functor
	      (:include functor (rangetype action-type*))
              (:print-function
                  (lambda (fctr srm k)
                     (declare (ignore k))
		     (format srm "#<Action functor ~s>"
			     (functor-name fctr)))))
   defn)  ; duplicate pointer to action-defn

(defstruct (action-method
	    (:print-function
	       (lambda (md srm k)
		  (declare (ignore k))
		  (cond ((action-method-name md)
			 (format srm "#<Method defn ~s for ~s>"
				     (action-method-name md)
				     (action-method-functor md)))
			(t
			 (format srm "#<Method defn for ~s>"
				     (action-method-functor md)))))))
   name
   functor
   term
   args    ; list of preconds
   vars    ; ditto
   graph)

(defstruct (safety-condition
              (:print-function
                  (lambda (sc srm k)
                     (declare (ignore k))
                     (format srm "#<Safety condition ~s>"
                             (safety-condition-goal sc)))))
   goal)

(defstruct (timepoint
              (:print-function
                  (lambda (tp srm k)
                     (declare (ignore k))
                     (format srm "#<Timepoint (~s ~s)>"
                             (timepoint-whichend tp)
                             (timepoint-interval tp)))))
  (whichend nil :type symbol)
  (interval nil)) ; timepoint gets shared by other intervals after creation

(defstruct (time-interval
              (:print-function
                  (lambda (tk srm k)
                     (declare (ignore k))
		     (let ((a (time-interval-action tk)))
		       (format srm "#<Time-interval ~s>"
			       (let ((kind (time-interval-kind tk)))
				  (cond ((eq kind 'action)
					 a)
					((eq kind 'compound)
					 (car a))
					((eq kind 'encapsulation)
					 (car (time-interval-action
						 (expansion-graph-main a))))
					(t
					 kind))))))))
   kind     ; action, compound, encapsulation, connector, pull-early, pull-late
   action   ; nil if artificial connection;
            ; This is normally an action term, but it is never 
            ; a foreach or forsome; for those cases, the action
            ; is the entire graph for the corresponding subnet.
   (begin nil)  ; these are nil only during consruction
   (end nil))

(defun new-time-interval (kind a)
   (let ((b (make-timepoint :whichend '<))
         (e (make-timepoint :whichend '>)))
      (let ((tk (make-time-interval :kind kind :action a :begin b :end e)))
         (setf (timepoint-interval b) tk)
         (setf (timepoint-interval e) tk)
	 tk)))

(defun time-entity-begin (ent)
   (cond ((time-interval-p ent)
          (time-interval-begin ent))
         (t ent)))

(defun time-entity-end (ent)
   (cond ((time-interval-p ent)
          (time-interval-end ent))
         (t ent)))

(defstruct (annotation
	      (:print-function
	          (lambda (c srm k)
	             (declare (ignore k))
	             (format srm "#<Annotation ~s [~s] ~s>"
	                 (annotation-type c)
	                 (annotation-time c)
	                 (annotation-prop c)))))
   (type nil :type symbol) ; precondition, maintain, effect, filter
   time  ; either timepoint or time-interval
   prop)  ; proposition
   
; Keeps track of interval orderings in action expansions.
(defstruct (expansion-graph
              (:print-function
                  (lambda (tt srm k)
                     (declare (ignore k))
		     (let ((main (expansion-graph-main tt)))
		        (format srm "#<Expansion graph~a ~s ~s>"
			   (cond ((expansion-graph-encapsulated tt)
				  "^")
				 (t "_"))
			   (cond (main
				  (expansion-graph-act tt))
				 (t 'anon))
			   (+ (cond (main 1)
				    (t 0))
			      (length (expansion-graph-others tt))))))))
   (encapsulated nil)
   main   ; main interval, subsuming whole graph
   others  ; all the other intervals
   tags
   subgraphs
   annotations)

(defun expansion-graph-act (eg)
   (let ((m (expansion-graph-main eg)))
      (cond (m
	     (cond ((expansion-graph-p m)
		    (expansion-graph-act m))
		   (t (time-interval-action m))))
	    (t
	     (error "Expansion graph has no action")))))

(defun expansion-graph-intervals (g)
   (cons (expansion-graph-main g)
	 (expansion-graph-others g)))


; These things are side-effected only when fresh.  Bigger ones are
; composed out of smaller ones functionally.

(defvar empty-expansion-graph* (make-expansion-graph
				  :main nil
				  :others '()
				  :tags '()
				  :subgraphs '()
				  :annotations '()))

(defun expansion-graph-is-empty  (eg)
   (not (expansion-graph-main eg)))

(defun expansion-graph-begin (eg)
   (time-interval-begin (expansion-graph-main eg)))

(defun expansion-graph-end (eg)
   (time-interval-end (expansion-graph-main eg)))

(defun expansion-graph-copy (g)
   (make-expansion-graph
       :main (expansion-graph-main g)
       :others (expansion-graph-others g)
       :tags (expansion-graph-tags g)
       :subgraphs (expansion-graph-subgraphs g)
       :annotations (expansion-graph-annotations g)))

(defun expansion-graph-begin-points (g)
   (cond ((expansion-graph-is-empty g) '())
         (t
          (do ((il (expansion-graph-others g)
                   (cdr il))
               (bl (list (time-interval-begin
                             (expansion-graph-main g)))
                   (adjoin (time-interval-begin
                              (car il))
                           bl
                           :test #'eq)))
              ((null il) bl)))))

(defun expansion-graph-end-points (g)
   (cond ((expansion-graph-is-empty g) '())
         (t
          (do ((il (expansion-graph-others g)
                   (cdr il))
               (el (list (time-interval-end
                             (expansion-graph-main g)))
                   (adjoin (time-interval-end
                              (car il))
                           el
                           :test #'eq)))
              ((null il) el)))))

(defstruct (domain
	      (:print-function
	         (lambda (d s k)
		    (declare (ignore k))
		    (format s "#<Domain ~s>" (domain-name d)))))
   (name 'unnamed-domain) ; see below
   (generation 0 :type integer)
   (ancestors '() :type list) 
      ; duplicate-free list of ancestors, including those from reqs.
      ; ** and including this domain itself **
   (parents '() :type list) 
   (requirements '() :type list)
   (timeless '() :type list)  ; facts true in every situation
   (local-bdgs '())  ; alist of symbols & bdgs.
   (inherited-bdgs nil))

(defun same-domain (d1 d2)
   (let ((n1 (domain-name d1))
	 (n2 (domain-name d2)))
      (or (eq n1 n2)
	  (and (consp n1) (consp n2)
	       (eq (car n1) (car n2))
	       (eq (cadr n1) (cadr n2))))))

(defstruct (situation)
   (path '())     ; One series of actions to reach it
   (index nil)    ; Contents indexed for fast access
   (contents '*uncomputed)       ; index contents as list
   (succs '())   ; successor situations
   (mark 0 :type integer))

(defstruct (initial-situation
	      (:include situation)
	      (:print-function
                (lambda (sit srm k)
                   (declare (ignore k))
		   (format srm "#<Situation ~s>"
			   (initial-situation-name sit)))))
   name
   domain       ; This is not a named domain, but a contrived subdomain
   generation  
   (parent nil)  ; initial-situation this is based on, if any
   delta        ; List of literals giving difference between parent and this
   (timeless-index nil)  ; timeless facts indexed (includes from superdomains).
   (sit-index nil))   ; setindex of all situations reachable from here.

(defstruct (subsequent-situation
	      (:include situation)
	      (:print-function
	          (lambda (sit srm k)
		          (declare (ignore k))
		     (format srm "#<Situation /~s>"
			     ;(length (situation-contents sit))
			     (length (situation-path sit))))))
   (init nil))  ; :type initial-situation

(defun find-init-situation (sit)
   (cond ((initial-situation-p sit) sit)
	 (t (subsequent-situation-init sit))))

(defstruct (problem
            (:print-function
                (lambda (prob srm k)
                   (declare (ignore k))
                   (format srm "#<Problem ~s>"
                               (problem-name prob)))))
   name
   (sit nil)  ; :type initial-situation
   goal
   expansion
   (length 0))  ; :type integer --- or something else

(defun problem-domain (p) (initial-situation-domain (problem-sit p)))

; Domain names are always either symbols with global pddl values, or
; of the form (domain <s>), where s is an initial situation.  The
; following procedure hides this complexity by returning two procedures,
; one that sets the value of its name, and one that retrieves it (which normally
; returns the same domain again).

(defun domain-name-procs (dom)
   (let ((domname (domain-name dom)))
      (cond ((symbolp domname)
             (values #'(lambda (new)
                          (set-global-pddl-symbol domname new))
                     #'(lambda ()
                          (get-global-pddl-symbol domname))))
            ((and (consp domname)
                  (eq (car domname) 'domain)
                  (initial-situation-p (cadr domname)))
			     ; situation subdomain
             (let ((sit (cadr domname)))
                (values #'(lambda (new)
                            (setf (initial-situation-domain sit)
                                             new))
                        #'(lambda ()
                             (initial-situation-domain sit)))))
            (t
             (error "Crazy domain name ~s"
                    domname)))))
