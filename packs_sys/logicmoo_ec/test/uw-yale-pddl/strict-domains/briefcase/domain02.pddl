;;;;;  Another briefcase world

(define (domain uni-bw)
  (:requirements :adl)
  (:constants B)
  (:predicates (place ?l)
	       (obj ?o)
	       (at ?thing ?l)
	       (in ?thing))
  (:action mov-b
	     ;; The place typing requires additions to the initial conditions
	     ;; (else nothing is a place).
	     ;; :parameters (?m (place ?l))       ; added place typing
	     :parameters (?m ?l)
	     :precondition (and (at B ?m) (not (= ?m ?l)))
	     :effect
	     (and (at b ?l) (not (at B ?m))
		   (forall (?z)
			    (when (and (in ?z) (not (= ?z B)))
				   (and (at ?z ?l) (not (at ?z ?m)))))))

  (:action take-out
	     :parameters (?x)
	     :precondition (in ?x)           ; changed from (not (= ?x B))
	     :effect (not (in ?x)))

  (:action put-in                     ; changed to non-conditional op
	     :parameters (?x ?l)
	     :precondition (and (at ?x ?l) (at B ?l) (not (= ?x B)))
	     :effect (in ?x)))