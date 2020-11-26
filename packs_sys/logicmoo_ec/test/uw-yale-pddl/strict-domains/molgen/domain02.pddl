;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strips version of the Molgen domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain molgen-strips)
  (:requirements :strips :equality :existential-preconditions)
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
			   (hair-pin ?x)))

  ;; steps for splicing DNA molecules

  (:action ligate1
	     :parameters (?y)
	     :precondition (and (double-strand ?y)
				 (not (= ?y linker)))
	     :effect (cleavable ?y))

  (:action ligate2
	     :parameters (?y ?x)
	     :precondition (and (cleaved ?x) (cleaved ?y)
				 (not (= ?x linker)))
	     :effect (and (contains ?x ?y) (cleavable ?y)
			   (not (cleaved ?x)) (not (cleaved ?y))))

  (:action cleave
	     :parameters (?x)
	     :precondition (cleavable ?x)
	     :effect (and (cleaved ?x)
			   (not (cleavable ?x))))

  ;; Step for inserting a molecule into an organism
  (:action transform
	     :parameters (?x ?y)
	     :precondition (and (bacterium ?y)
				 (cleavable ?x) ; molecule must be whole
				 (accepts ?x ?y) ; Is molecule accepted?
				 (not (= ?x ?y)))
	     :effect (and (contains ?x ?y)
			   (not (cleavable ?x))))

  ;; purify a culture with an antibiotic
  (:action screen
	     :parameters (?x ?y ?z)
	     :precondition (and (bacterium ?x)
				 (antibiotic ?z)
				 (resists ?z ?y)
				 (contains ?y ?x)
				 (not (= ?x ?y)) (not (= ?y ?z)) (not (= ?x ?z)))
	     :effect (pure ?x))
  )