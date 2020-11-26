
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  McDermott blocks world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain mcd-blocksworld)
  (:requirements :adl :universal-preconditions :disjunctive-preconditions)
  (:constants Table)
  (:predicates (on ?x ?y)
	       (clear ?x)
	       (block ?b)
	       (above ?x ?y))
  (:action puton
	     :parameters (?x ?y ?d)
	     :precondition (and (not (= ?x ?y)) (not (= ?x table)) (not (= ?d ?y))
				 (on ?x ?d)
				 (or (= ?x Table)
				      (forall (?b) (imply (block ?b) (not (on ?b ?x)))))
				 (or (= ?y Table)
				      (forall (?b) (imply (block ?b) (not (on ?b ?y))))))
	     :effect
	     (and (on ?x ?y) (not (on ?x ?d))
		   (forall (?c)
			    (when (or (= ?y ?c) (above ?y ?c))
				   (above ?x ?c)))
		   (forall (?e)
			    (when (and (above ?x ?e) (not (= ?y ?e))
					 (not (above ?y ?e)))
				   (not (above ?x ?e)))))))
