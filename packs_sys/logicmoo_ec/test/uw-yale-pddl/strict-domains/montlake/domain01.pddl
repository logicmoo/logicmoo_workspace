;; The Montlake bridge is broken.  They keep taking the pin out manually.
;; I have written a domain in its honor.

(define (domain montlake)
  (:requirements :strips :conditional-effects :domain-axioms :disjunctive-preconditions)

  (:predicates (semaphore ?color)
	       (first-gates-up)
	       (second-gates-up)
	       (all-gates-up)
	       (all-gates-down)
	       (bridge-up)
	       (boats-waiting)
	       (pin-in)
	       (joe-at ?place))

  (:constants red yellow green 
	      tower pin landing)

  (:action switch-semaphore
	     :parameters ()
	     :effect (and (when (and (semaphore red)
					(all-gates-up))
				  (and (not (semaphore red))
					(semaphore green)))
			   (when (semaphore green)
				  (and (not (semaphore green))
					(semaphore yellow)))
			   (when (semaphore yellow)
				  (and (not (semaphore yellow))
					(semaphore red))))
	     )
  (:action shut-first-gates
	     :parameters ()
	     :precondition (semaphore red)
	     :effect (not (first-gates-up))
	     )
  (:action shut-second-gates
	     :parameters ()
	     :precondition (not (first-gates-up))
	     :effect (not (second-gates-up))
	     )
  (:action open-first-gates
	     :parameters ()
	     :precondition (second-gates-up)
	     :effect (first-gates-up)
	     )
  (:action open-second-gates
	     :parameters ()
	     :precondition (and (pin-in)
				 (joe-at tower))
	     :effect (second-gates-up)
	     )
  (:action pull-pin
	     :parameters ()
	     :precondition (and (all-gates-down)
				 (joe-at pin))
	     :effect (not (pin-in))
	     )
  (:action push-pin
	     :parameters ()
	     :precondition (and (all-gates-down)
				   (joe-at pin))
	     :effect (pin-in)
	     )
  (:action joe-moves
	     :parameters (?here ?there)
	     :precondition (joe-at ?here)
	     :effect (and (not (joe-at ?here))
			   (joe-at ?there))
	     )
  (:action open-bridge
	     :parameters ()
	     :precondition (and (all-gates-down)
				 (not (pin-in))
				 (or (joe-at tower)
				      (joe-at landing)))
	     :effect (and (bridge-up)
			   (not (boats-waiting)))
	     )
  (:action close-bridge
	     :parameters ()
	     :effect (not (bridge-up))
	     )
  (:axiom 
	  :context (and (first-gates-up)
			 (second-gates-up))
	  :implies (all-gates-up))
  (:axiom 
	  :context (and (not (first-gates-up))
			 (not (second-gates-up)))
	  :implies (all-gates-down))
    
    )


