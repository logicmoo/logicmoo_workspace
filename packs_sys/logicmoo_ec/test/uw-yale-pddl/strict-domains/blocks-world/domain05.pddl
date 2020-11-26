
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple strips blocks world from Smith and Peot

(define (domain simple-blocks)
  (:requirements :strips :equality)
  (:predicates (on ?x ?y)
	       (clear ?x))
  (:constants Table)

  (:action PutTable
	     :parameters (?x ?z)
	     :precondition (and (on ?x ?z) (clear ?x)
                                 (not (= ?x Table)) (not (= ?z Table)))
	     :effect (and (on ?x Table) (clear ?z) (not (on ?x ?z))))
  (:action Put
	     :parameters (?x ?y ?z)
	     :precondition (and (on ?x ?z) (clear ?x) (clear ?y)
                                  (not (= ?x Table)) (not (= ?y Table)) (not (= ?x ?y)))
	     :effect (and (on ?x ?y) (clear ?z)
                           (not (on ?x ?z)) (not (clear ?y)))))

