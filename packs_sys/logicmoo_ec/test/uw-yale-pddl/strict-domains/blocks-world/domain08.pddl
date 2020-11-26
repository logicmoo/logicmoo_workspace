;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SNLP blocks world with 2 operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain snlp-bw1)
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
		  (clear ?y))))