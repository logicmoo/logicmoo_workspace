;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simpler domain - vehicles, but no boarding and disembarking

(define (domain road-operators)
  (:requirements :strips)
  (:predicates (at ?v ?l)
	       (road ?l1 ?l2)
	       (bridge ?l1 ?l2)
	       (place ?l)
	       (vehicle ?v))

  (:action drive
	     :parameters (?vehicle ?location1 ?location2)
	     :precondition (and (at ?vehicle ?location1) 
				 (road ?location1 ?location2))
	     :effect
	     (and (at ?vehicle ?location2)
		   (not (at ?vehicle ?location1))))
  (:action cross
	     :parameters (?vehicle ?location1 ?location2)
	     :precondition (and (at ?vehicle ?location1) 
				 (bridge ?location1 ?location2))
	     :effect
	     (and (at ?vehicle ?location2)
		   (not (at ?vehicle ?location1)))))

