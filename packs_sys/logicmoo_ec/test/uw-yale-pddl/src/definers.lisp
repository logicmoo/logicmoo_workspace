(in-package "PDDL")

(defvar pddl-syntax-table* (make-hash-table :test #'eq)
  "Table to store syntax handlers and macros in")
; entries in table are of form (type -info-), where type is
; one of definer, pddl-macro, or top-level

(defun pddl-form-handler (form)
   (cond ((and (consp form) (symbolp (car form)))
	  (gethash (car form) pddl-syntax-table*))
	 (t nil)))
   
(defmacro def-pddl-form-handler (sym type &rest body)
   (let ((fun-name (intern (concatenate 'string
                                        (symbol-name sym)
                                        "-HANDLER")
			   "PDDL")))
      `(progn
	  (defun ,fun-name
	    ,@body)
          (try-enter-in-syntax-table
              ',sym (list ',type #',fun-name)))))

(defun try-enter-in-syntax-table (sym entry)
   (let ((x (gethash sym pddl-syntax-table*)))
      (cond ((or (not x)
                 (eq (car entry) (car x))
                 (y-or-n-p "Attempting to change ~s from ~s to ~s.  Allow?"
                           sym (car x) (car entry)))
             (setf (gethash sym pddl-syntax-table*)
                   entry)))))

(declaim (special strict*))

; The expectation is that checking the syntax of a domain will add
; an entry for that domain to the pddl-domain-table*.  Later forms can 
; be checked as if they had been included in the original spec.

(def-pddl-form-handler define top-level (def-form)
   (cond ((and (consp (cadr def-form))
	       (symbolp (caadr def-form)))
          (let ((h (pddl-form-handler (cadr def-form))))
	     (cond ((and h (eq (car h) 'definer))
                    (funcall (cadr h) def-form))
		   (t
		    `(define ,(flagexp "Undefinable" (cadr def-form))
			   ,@(cddr def-form))))))
	 (t
	  `(define ,(flagexp "Unintelligible" (cdr def-form))))))

(def-pddl-form-handler requirement definer (def-form)
   (let ((name (extract-defined-name def-form)))
      (cond ((symbolp name)
	     (let ((req (try-requirement-with-name name t)))
	        (cond ((requirement-p req)
                       (multiple-value-bind (fields flg-junk)
          			            (list-smooth
                                                (cddr def-form) #'consp)
			  (let ((builtins (append-field ':builtins fields))
				(implications (append-field ':implies fields))
				(flg-fields
				   (collect-bad-fields '(:builtins :implies)
						       fields)))
			     (requirement-parse
                                req builtins implications
				`(,@flg-fields ,@flg-junk)))))
		      (t
		       `(define (requirement
                                   ,(flag-wrong-type req name 'requirement))
                                ,@(cddr def-form))))))
	    (t
	     `(define (requirement
                            ,(flagexp "Unintelligible requirement name"
				      name))
                      ,@(cddr def-form))))))

(def-pddl-form-handler domain definer (def-form)
   (let ((name (extract-defined-name def-form)))
      (cond ((symbolp name)
             (let ((dom (try-domain-with-name name t)))
                (cond ((domain-p dom)
                       (multiple-value-bind (fields flg-junk)
                                             (list-smooth
                                                (cddr def-form) #'consp)
			  (let ((parents (append-field ':extends fields))
				(require (append-field ':requirements fields))
				(types (collect-field ':types fields))
				(constants (collect-field ':constants fields))
				(domain-vars (collect-field ':domain-variables
							    fields))
				(predicates (append-field
                                               ':predicates fields))
				(functors (collect-field ':functors fields))
				(timeless (append-field ':timeless fields))
				(safety (append-field ':safety fields))
				(axioms (collect-field ':axiom fields))
				(actions (collect-field ':action fields))
				(methods (collect-field ':method fields))
				(flg-fields (collect-bad-fields
					       '(:extends :requirements :types
						 :constants :domain-vars
                                                 :predicates
						 :functors :timeless :safety
						 :axiom :action :method)
					       fields)))
			     (domain-parse dom
				parents require types constants domain-vars
				predicates functors timeless safety axioms
                                actions methods `(,@flg-fields ,@flg-junk)))))
	              (t
                       `(define (domain ,(flag-wrong-type dom name 'domain))
                                ,@(cddr def-form))))))
            (t
	     `(define (domain ,@name) ,@(cddr def-form))))))

(def-pddl-form-handler addendum definer (def-form)
   (cond (strict*
	  (flagexp "Addenda not allowed in strict subset"
		   def-form))
	 (t
	  (multiple-value-bind (status name dom fields flg-junk)
			       (massage-domain-entity-def-form def-form)
	     (case status
		(:okay
		 (let ((methods (collect-field ':method fields))
		       (axioms (collect-field ':axiom fields))
		       (safeties (collect-field ':safety fields))
		       (bad-fields (collect-bad-fields
				       '(:method :axiom :safety :domain)
				    fields)))
		    (addendum-parse name dom methods axioms safeties
				    `(,@bad-fields ,@flg-junk))))
		(t
		 (note-domain-entity-bogosity status name dom fields flg-junk
					      def-form)))))))         

(def-pddl-form-handler situation definer (def-form)
   (multiple-value-bind (status name dom fields flg-junk)
                        (massage-domain-entity-def-form def-form)
      (case status
         (:okay
          (let ((obs (collect-field ':objects fields))
                (inits (append-field ':init fields))
                (bad-fields (collect-bad-fields '(:domain :objects :init)
                                                fields)))
             (situation-parse name dom obs inits
                              `(,@bad-fields ,@flg-junk))))
         (t
          (note-domain-entity-bogosity status name dom fields flg-junk
                                       def-form)))))

(def-pddl-form-handler problem definer (def-form)
   (multiple-value-bind (status name dom fields flg-junk)
                        (massage-domain-entity-def-form def-form)
      (case status
         (:okay
          (let ((requirements (append-field ':requirements fields))
                (situation (append-field ':situation fields))
                (obs (collect-field ':objects fields))
                (lnth (append-field ':length fields))
                (inits (append-field ':init fields))
                (goal (append-field ':goal fields))
		(expansion (append-field ':expansion fields))
                (bad-fields (collect-bad-fields '(:domain :requirements
						  :situation :objects :init
                                                  :length :goal :expansion)
                                                fields)))
             (problem-parse name dom requirements
                            situation obs inits goal expansion lnth
                            `(,@bad-fields ,@flg-junk))))
         (t
          (note-domain-entity-bogosity status name dom fields flg-junk
                                       def-form)))))

(defun massage-domain-entity-def-form (def-form)
   (let ((name (extract-defined-name def-form)))
      (cond ((symbolp name)
	     (multiple-value-bind (fields flg-junk)
				   (list-smooth
				      (cddr def-form) #'consp)
                (let ((domnames (append-field ':domain fields)))
                  (cond ((and (= (length domnames) 1)
                              (symbolp (car domnames)))
                         (let ((dom (try-domain-with-name
                                       (car domnames) nil)))
                            (cond ((domain-p dom)
                                   (values ':okay name dom fields flg-junk))
                                  (dom
                                   (values ':undef name 
		                           (flag-wrong-type dom (car domnames)
				                            'domain)
                                           fields flg-junk))
		                  (t
                                   (values ':undef name
		                           (flagexp "Undefined domain"
			                            (car domnames))
                                           fields flg-junk)))))
                        (t
                         (values ':illegal name domnames fields flg-junk))))))
            (t
             (values ':bogus name nil nil nil)))))

(defun note-domain-entity-bogosity (status name dom fields flg-junk
                                    def-form)
   (case status                                     
      (:undef
       `(define (,(caadr def-form) ,name)
	   (:domain ,dom)
	   ,@fields ,@flg-junk))
      (:illegal
       (flagexp "Incoherent domain name"
		`(define (,(caadr def-form) ,name)
	            ,@(cddr def-form))))
      (:bogus
       `(define (,(caadr def-form) ,@name) ,@(cddr def-form)))))

(defun extract-defined-name (def-form)
   (let ((n (cdadr def-form)))
      (cond ((and (= (length n) 1)
		  (symbolp (car n)))
	     (car n))
	    (t (flagexp "Unintelligible name"
			n)))))

; Like collect-field, but append results together
(defun append-field (name fields)
   (mapcan #'copy-list (collect-field name fields)))

; Collect all e such that (name . e) occurs in fields.
(defun collect-field (name fields)
   (cond ((null fields) '())
         ((eq (caar fields) name)
          (cons (cdar fields) (collect-field name (cdr fields))))
         (t
          (collect-field name (cdr fields)))))

(defun collect-bad-fields (good fields)
   (labels ((coll (fields)
               (cond ((null fields) '())
                     ((member (caar fields) good :test #'eq)
                      (coll (cdr fields)))
                     (t
                      (cons (car fields)
                            (coll (cdr fields))))))
	    (out-of-order (fl gl)
	       (cond ((null fl) nil)
		     (t
		      (let ((tgl (member (caar fields) gl
					 :test #'eq)))
			 (cond (tgl
				(out-of-order (cdr fl) tgl))
			       (t (car fields))))))))
      (let ((b (coll fields)))
         (cond ((null b)
		(cond (strict*
		       (let ((latefield
			        (out-of-order fields good)))
			  (cond (latefield
				 `(,(flagexp "Field out of order"
					     (car latefield))))
				(t '()))))
		      (t '())))
               (t `(,(flagexp "Meaningless in this context"
                              b)))))))
