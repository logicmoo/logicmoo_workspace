(in-package "PDDL")

; Built-in stuff

(define (requirement :strips))

(define (requirement :domain-axioms))

(define (domain no-op)
   (:predicates
       (has-vars ?x)
       (many)
       (assertion ?a))
   (:action --   ; no-op
      :parameters ()
      :effect (and)))

(setq universal-ancestors*
      (list basic-domain* (try-domain-with-name 'no-op nil)))

(define (domain basic-types)   
   ;(:requirements :typing :domain-axioms)
   (:types (number #'numberp) - object
           (float #'floatp)
	   (integer #'integerp) - number))
;   (:axiom
;      :vars (?x)
;      :context (and)
;      :implies (object ?x))

(define (requirement :typing)
   (:builtins basic-types))

(define (requirement :disjunctive-preconditions))

(define (domain dom-equality)
   (:requirements :typing :domain-axioms)
   ;(:extends numbers)
   (:predicates (= ?x ?y - (expression (fluent object)))))

(define (requirement :equality)
   (:builtins dom-equality))

; Defined by addenda in deduction.lisp
(define (domain existential-precs)
   (:requirements :domain-axioms))
(define (domain universal-precs)
   (:requirements :domain-axioms))
   
(define (requirement :existential-preconditions)
   (:builtins existential-precs))

(define (requirement :universal-preconditions)
   (:builtins universal-precs))

(define (requirement :quantified-preconditions)
   (:implies :existential-preconditions :universal-preconditions))

(define (requirement :conditional-effects))

(define (requirement :action-expansions))

(define (requirement :foreach-expansions)
   (:implies :action-expansions))

(define (requirement :dag-expansions)
   (:implies :action-expansions))

(define (requirement :subgoal-through-axioms)
   (:implies :domain-axioms))

(define (requirement :safety-constraints))

(define (domain expressions)
   (:requirements :typing :domain-axioms)
   ;(:extends numbers)
   (:constants t)

   (:predicates (> ?x ?y - (expression (fluent number)))
		(< ?x ?y - (expression (fluent number)))
		(>= ?x ?y - (expression (fluent number)))
		(<= ?x ?y - (expression (fluent number)))
                (eval ?e - (expression object) ?x - object)
                (equation ?e1 ?e2 - (expression (fluent number)))
                (test ?e - (expression proposition))
		(bounded-int ?i ?l ?h - integer))
   (:functors (+ &rest ?n - (expression (fluent number)))
              (- &rest ?n - (expression (fluent number)))
              (* &rest ?n - (expression (fluent number)))
              (/ ?x ?y - (expression (fluent number)))
	      - (expression (fluent number))
              (is-true ?x - (expression object))))

(define (requirement :expression-evaluation)
   (:builtins expressions)
   (:implies :domain-axioms))

(define (domain dom-fluents)
   (:requirements :expression-evaluation)
   (:predicates (fluent ?ty ?x)
		(current-value ?f - (fluent object) ?v - object)
		(fluent-eval ?e - (expression (fluent object)) ?x - object)
		(fluent-test ?e - (expression (fluent proposition)))))

(define (requirement :fluents)
   (:builtins dom-fluents)
   (:implies :expression-evaluation))

(define (requirement :open-world))

(define (requirement :true-negation)
   (:implies :open-world))

(define (requirement :adl)
   (:implies :strips :typing :disjunctive-preconditions :equality
             :quantified-preconditions :conditional-effects))

(define (requirement :ucpop)
   (:implies :adl :domain-axioms :safety-constraints))

; Return list of pairs (v f), where f applied to the other
; side of the equation yields an expression equal to v
(defun isolate-vars (e)
   (cond ((qvar-p e)
          (list (list e #'identity)))
         ((atom e)
          '())
         (t
          (case (car e)
             ((+ -)
              (let ((op (case (car e) (+ '-) (t '+))))
                 (do  ((bow '())
		       (stern (cdr e))
		       (r '()))
		      ((null stern)
		       r)
                   (cond ((qvar-p (car stern))
			  (let ((args `(,@(cdr stern) ,@bow)))
			     (push (list
				      (car stern)
				      #'(lambda (other)
					  `(,op ,other ,@args)))
				   r))))
		    (setf bow (cons (car stern) bow))
                    (setf stern (cdr stern)))))
             (t '())))))
                    