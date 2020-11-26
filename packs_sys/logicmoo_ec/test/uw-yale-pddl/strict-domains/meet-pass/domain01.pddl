;;;Railroad meet/pass problem.  trains cannot occupy tracks at the same time.

(define (domain meet-pass)
  (:requirements :strips)
  (:predicates (clear ?x)
	       (connected ?x ?y)
	       (at ?x ?y))
  (:action move-train
	     :parameters (?x ?y ?z)
	     :precondition (and (clear ?z)
				 (connected ?y ?z)
				 (at ?x ?y))
	     :effect (and (not (clear ?z))
			   (clear ?y)
			   (at ?x ?z)
			   (not (at ?x ?y)))))



