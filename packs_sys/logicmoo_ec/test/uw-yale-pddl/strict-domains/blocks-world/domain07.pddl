
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  AT&T blocks world without disjunctive preconditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ( domain att-bw2 )
    ( :requirements :strips :equality)
  (:predicates (is-table ?t)
	       (block ?b)
	       (clear ?b)
	       (on ?x ?y))
  (:constants table)
	       
    ( :action move-to-table
        :parameters (?obj ?source ?dest)   ; move obj from source to dest
        :precondition (AND (is-table ?dest)
                           (block ?obj)
                           (clear ?obj)
                           (on ?obj ?source)
                           (not (= ?obj ?source)) (not (= ?obj ?dest))
			   (not (= ?source ?dest)) (not (= ?obj table)))
        :effect (AND (NOT (on ?obj ?source))
                     (clear ?source)
                     (on ?obj ?dest)))

    ( :action move-to-block
        :parameters (?obj ?source ?dest)   ; move obj from source to dest
        :precondition (AND (clear ?dest)
                           (block ?obj)
                           (block ?dest)
                           (clear ?obj)
                           (on ?obj ?source)
                           (not (= ?obj ?source)) (not (= ?obj ?dest))
			   (not (= ?source ?dest)) (not (= ?obj table)))
        :effect (AND (NOT (on ?obj ?source))
                     (NOT (clear ?dest))
                     (clear ?source)
                     (on ?obj ?dest))))