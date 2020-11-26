;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SNLP blocks world with 3 operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain snlp-bw2)
    (:requirements :adl)
  
  (:predicates (clear ?x)
	       (on-table ?x)
	       (on ?x ?y))

    (:action stack
	       :parameters (?x ?y)
	       :precondition
	       (AND (clear ?x)
		    (clear ?y)
		    (on-table ?x))
	       :effect
	       (AND (on ?x ?y)
		    (NOT (on-table ?x))
		    (NOT (clear ?y))))
  (:action unstack
	     :parameters (?x ?y)
	     :precondition
	     (AND (clear ?x)
		  (on ?x ?y))
	     :effect
	     (AND (on-table ?x)
		  (NOT (on ?x ?y))
		  (clear ?y)))
  (:action move
	     :parameters (?x ?y ?z)
	     :precondition
	     (AND (on ?x ?y)
		  (clear ?x)
		  (clear ?z))
	     :effect
	     (AND (on ?x ?z)
		  (clear ?y)
		  (NOT (clear ?z))
		  (NOT (on ?x ?y)))))