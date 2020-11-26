;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strips version of the simple briefcase world

(define (domain briefcase-strips)
  (:requirements :strips :equality)
  (:constants B)
  (:predicates (at ?x ?y)
	       (in ?x ?y))
  (:action move-briefcase
	     :parameters (?m ?l)
	     :precondition (and (at B ?m) (not (= ?m ?l)))
	     :effect (and (not (at B ?m))
			   (at B ?l)))
  (:action take-out
	     :parameters (?x ?y)
	     :precondition (and (at B ?y) (in ?x B) (not (= ?x B)))
	     :effect (and (not (in ?x B))
			   (at ?x ?y)))
  (:action put-in
	     :parameters (?x ?y)
	     :precondition (and (at ?x ?y) (at B ?y) (not (= ?x B)))
	     :effect (and (not (at ?x ?y)) (in ?x B))))

