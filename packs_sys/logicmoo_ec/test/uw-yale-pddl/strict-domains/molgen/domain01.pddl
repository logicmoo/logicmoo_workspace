; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.
;
;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; molgen domain

(define (domain molgen-adl)
  (:requirements :adl :conditional-effects :equality :existential-preconditions)

  (:constants LINKER)
  (:predicates (mRNA ?x)
	       (molecule ?x)
	       (connected-cDNA-mRNA ?x)
	       (single-strand ?x)
	       (hair-pin ?x)
	       (double-strand ?x)
	       (cleavable ?x)
	       (cleaved ?x)
	       (bacterium ?x)
	       (accepts ?x ?y)
	       (contains ?x ?y)
	       (antibiotic ?x)
	       (pure ?x)
	       (resists ?x ?y))
	       
	       

  ;; steps for building DNA molecules from mRNA
  (:action reverse-transcribe
	     :parameters (?x)
	     :precondition (mRNA ?x)
	     :effect (connected-cDNA-mRNA ?x))
  (:action separate
	     :parameters (?x)
	     :precondition (connected-cDNA-mRNA ?x)
	     :effect (and (single-strand ?x)
			   (not (connected-cDNA-mRNA ?x))))
  (:action polymerize
	     :parameters (?x)
	     :precondition (single-strand ?x)
	     :effect (and (hair-pin ?x)
			   (not (single-strand ?x))))
  (:action digest
	     :parameters (?x)
	     :precondition (hair-pin ?x)
	     :effect (and (double-strand ?x)
			   (not (hair-pin ?x))))

  ;; steps for splicing DNA molecules
  (:action ligate
	     :parameters (?x ?y)
	     :precondition (not (= ?x ?y))
	     :effect
	     (and (when (and (double-strand ?y) (= ?x LINKER))
		     (cleavable ?y))
		   (when (and (cleaved ?x) (cleaved ?y) (not (= ?x LINKER)))
		     (and (contains ?x ?y) (cleavable ?y)
			   (not (cleaved ?x)) (not (cleaved ?y))))))

  (:action cleave
	     :parameters (?x)
	     :precondition (cleavable ?x)
	     :effect (and (cleaved ?x)
			   (not (cleavable ?x))))

  ;; Step for inserting a molecule into an organism
  (:action transform
	     :parameters (?x ?y)
	     :precondition (and (bacterium ?y) 
				(not (= ?x ?y))
				 (cleavable ?x) ; molecule must be whole
				 (accepts ?x ?y)) ; Is molecule accepted?
	     :effect (and (contains ?x ?y)
			   (not (cleavable ?x))))

  ;; purify a culture with an antibiotic
  (:action screen
	     :parameters (?x ?y ?z)
	     :precondition (and (bacterium ?x) (antibiotic ?z) 
				(not (= ?x ?y)) (not (= ?y ?z)) (not (= ?x ?z))
				 (resists ?z ?y)(contains ?y ?x))
	     :effect (pure ?x)))



