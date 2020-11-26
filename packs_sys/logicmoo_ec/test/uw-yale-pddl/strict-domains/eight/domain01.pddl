;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The eight-puzzle is a game in which you attempt to move the pieces into a
;;; certain arrangement from some random arrangement.  The board is two
;;; dimensional, and you can only move pieces left, right, up, and down into
;;; an adjacent empty square.
;;;
;;; These problems are all completely impossible for UCPOP.
;;; 
;;;                                 --==>> Marc Friedman, October 1995
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The board:
;; S1 S2 S3
;; S4 S5 S6
;; S7 S8 S9

;; The goal:
;; P1 P2 P3
;; P4 P5 P6
;; P7 P8 P9


(define (domain eight-puzzle)
  (:requirements :strips :equality :domain-axioms :disjunctive-preconditions)
  
  (:constants S1 S2 S3 S4 S5 S6 S7 S8 S9
	      P1 P2 P3 P4 P5 P6 P7 P8 P9)
  (:predicates (adjacent ?x ?y)
	       (at ?p ?s)
	       (solved)
	       (empty ?s))
  
  (:axiom 
          :vars (?x ?y)
	  :context (or
		    (and (= ?x S1)
			 (= ?y S2))
		    (and (= ?x S1)
			 (= ?y S4))
		    (and (= ?x S2)
			 (= ?y S1))
		    (and (= ?x S2)
			 (= ?y S3))
		    (and (= ?x S2)
			 (= ?y S5))
		    (and (= ?x S3)
			 (= ?y S2))
		    (and (= ?x S3)
			 (= ?y S6))
		    (and (= ?x S4)
			 (= ?y S1))
		    (and (= ?x S4)
			 (= ?y S5))
		    (and (= ?x S4)
			 (= ?y S7))
		    (and (= ?x S5)
			 (= ?y S4))
		    (and (= ?x S5)
			 (= ?y S2))
		    (and (= ?x S5)
			 (= ?y S6))
		    (and (= ?x S5)
			 (= ?y S8))
		    (and (= ?x S6)
			 (= ?y S5))
		    (and (= ?x S6)
			 (= ?y S3))
		    (and (= ?x S6)
			 (= ?y S9))
		    (and (= ?x S7)
			 (= ?y S4))
		    (and (= ?x S7)
			 (= ?y S8))
		    (and (= ?x S8)
			 (= ?y S7))
		    (and (= ?x S8)
			 (= ?y S5))
		    (and (= ?x S8)
			 (= ?y S9))
		    (and (= ?x S9)
			 (= ?y S8))
		    (and (= ?x S9)
			 (= ?y S6)))
	  :implies (adjacent ?x ?y))
  (:axiom 
	  :context (and
		    (at P1 S1)
		    (at P2 S2)
		    (at P3 S3)
		    (at P4 S4)
		    (at P5 S5)
		    (at P6 S6)
		    (at P7 S7)
		    (at P8 S8)
		    )
	  :implies (solved))
  (:action slide
	     :parameters (?piece ?from ?to)
	     :precondition (and
			    (adjacent ?from ?to)
			    (at ?piece ?from)
			    (empty ?to)
			    )
	     :effect (and (empty ?from)
			   (not (empty ?to))
			   (at ?piece ?to)
			   (not (at ?piece ?from)))))



