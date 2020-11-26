;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  AT&T blocks world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ( domain att-bw )
    ( :requirements :strips :equality :disjunctive-preconditions)
  (:predicates (is-table ?t)
	       (block ?b)
	       (clear ?x)
	       (on ?x ?y))
  (:constants table)


    ( :action move
        :parameters (?obj ?source ?dest)   ; move obj from source to dest
        :precondition (AND (OR (is-table ?dest) (clear ?dest))
                           (block ?obj)
                           (clear ?obj)
                           (on ?obj ?source)
                           (not (= ?obj ?source)) (not (= ?obj ?dest))
			   (not (= ?source ?dest)) (not (= ?obj table)))
        :effect (AND (NOT (on ?obj ?source))
                     (clear ?source)
                     (NOT (clear ?dest))
                     (on ?obj ?dest))))

