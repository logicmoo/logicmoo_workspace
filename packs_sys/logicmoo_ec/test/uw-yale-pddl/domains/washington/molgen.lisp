" (c) 1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package :domains)

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

(define (problem rat-insulin-adl)
    (:domain molgen-adl)
  (:objects insulin-gene e-coli-exosome junk-exosome
	    e-coli junk antibiotic-1)
  (:init (molecule insulin-gene)
	 (molecule e-coli-exosome)
	 (molecule junk-exosome) (molecule linker)
	 (bacterium e-coli) (bacterium junk)
	 (antibiotic antibiotic-1)
	 (mRNA insulin-gene)
	 (cleavable e-coli-exosome)
	 (cleavable junk-exosome)
	 (accepts junk-exosome junk)
	 (accepts e-coli-exosome e-coli)
	 (resists antibiotic-1 e-coli-exosome))
  (:goal (AND (exists (?y)
		      (and (bacterium ?y) 
			   (exists (?x)
				   (and (molecule ?x)
					(contains insulin-gene ?x)
					(contains ?x ?y)
					(pure ?y))))))))

;;;UCPOP(30): (bf-control 'rat-insulin)
;;;
;;;Initial  : ((MOLECULE INSULIN-GENE) (MOLECULE E-COLI-EXOSOME)
;;;            (MOLECULE JUNK-EXOSOME) (MOLECULE LINKER) (BACTERIUM E-COLI)
;;;            (BACTERIUM JUNK) (ANTIBIOTIC ANTIBIOTIC-1) (MRNA INSULIN-GENE)
;;;            (CLEAVABLE E-COLI-EXOSOME) (CLEAVABLE JUNK-EXOSOME)
;;;            (ACCEPTS JUNK-EXOSOME JUNK) (ACCEPTS E-COLI-EXOSOME E-COLI)
;;;            (RESISTS ANTIBIOTIC-1 E-COLI-EXOSOME))
;;;
;;;Step 1  : (REVERSE-TRANSCRIBE INSULIN-GENE)   Created 10
;;;           0  -> (MRNA INSULIN-GENE)
;;;Step 2  : (SEPARATE INSULIN-GENE)   Created 9
;;;           10 -> (CONNECTED-CDNA-MRNA INSULIN-GENE)
;;;Step 3  : (POLYMERIZE INSULIN-GENE)   Created 8
;;;           9  -> (SINGLE-STRAND INSULIN-GENE)
;;;Step 4  : (DIGEST INSULIN-GENE)   Created 7
;;;           8  -> (HAIR-PIN INSULIN-GENE)
;;;Step 5  : (LIGATE LINKER INSULIN-GENE)   Created 6
;;;           7  -> (DOUBLE-STRAND INSULIN-GENE)
;;;Step 6  : (CLEAVE INSULIN-GENE)   Created 5
;;;           6  -> (CLEAVABLE INSULIN-GENE)
;;;Step 7  : (CLEAVE E-COLI-EXOSOME)   Created 4
;;;           0  -> (CLEAVABLE E-COLI-EXOSOME)
;;;Step 8  : (LIGATE INSULIN-GENE E-COLI-EXOSOME)   Created 3
;;;           5  -> (CLEAVED INSULIN-GENE)
;;;           4  -> (CLEAVED E-COLI-EXOSOME)
;;;Step 9  : (TRANSFORM E-COLI-EXOSOME E-COLI)   Created 2
;;;           3  -> (CLEAVABLE E-COLI-EXOSOME)
;;;           0  -> (ACCEPTS E-COLI-EXOSOME E-COLI)
;;;           0  -> (BACTERIUM E-COLI)
;;;Step 10 : (SCREEN E-COLI E-COLI-EXOSOME ANTIBIOTIC-1)   Created 1
;;;           0  -> (RESISTS ANTIBIOTIC-1 E-COLI-EXOSOME)
;;;           2  -> (CONTAINS E-COLI-EXOSOME E-COLI)
;;;           0  -> (BACTERIUM E-COLI)
;;;           0  -> (ANTIBIOTIC ANTIBIOTIC-1)
;;;
;;;Goal    : (EXISTS ((BACTERIUM ?YGOAL) (MOLECULE ?XGOAL))
;;;           (AND (CONTAINS INSULIN-GENE ?XGOAL) (CONTAINS ?XGOAL ?YGOAL)
;;;            (PURE ?YGOAL)))
;;;           3  -> (CONTAINS INSULIN-GENE E-COLI-EXOSOME)
;;;           2  -> (CONTAINS E-COLI-EXOSOME E-COLI)
;;;           1  -> (PURE E-COLI)
;;;           0  -> (BACTERIUM E-COLI)
;;;           0  -> (MOLECULE E-COLI-EXOSOME)
;;;Complete!
;;;
;;;UCPOP (Init = 13 ; Goals = 3 ) => Win  (10 steps)     CPU 6850
;;;     Nodes (V = 896 ; Q = 203 ; C = 1255)             Branch 1.2265625
;;;     Working Unifies: 3176                            Bindings added: 1600
;;;NIL

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

(define (problem rat-insulin-strips)
    (:domain molgen-strips)
  (:objects insulin-gene e-coli-exosome junk-exosome
	    e-coli junk antibiotic-1)
  (:init (molecule insulin-gene)
	 (molecule e-coli-exosome)
	 (molecule junk-exosome) (molecule linker)
	 (bacterium e-coli) (bacterium junk)
	 (antibiotic antibiotic-1)
	 (mRNA insulin-gene)
	 (cleavable e-coli-exosome)
	 (cleavable junk-exosome)
	 (accepts junk-exosome junk)
	 (accepts e-coli-exosome e-coli)
	 (resists antibiotic-1 e-coli-exosome))
  (:goal (and (exists (?x ?y)
		      (and (bacterium ?y)
			   (molecule ?x)
			   (contains insulin-gene ?x)
			   (contains ?x ?y)
			   (pure ?y))))))

(DEFINE (PROBLEM MOLGEN1-STRIPS) (:DOMAIN MOLGEN-STRIPS) 
  (:objects insulin-gene e-coli-exosome junk-exosome e-coli junk
	    antibiotic-1)
  
  (:init
 (MOLECULE INSULIN-GENE) (MOLECULE E-COLI-EXOSOME) (MOLECULE JUNK-EXOSOME)
  (MOLECULE LINKER) (BACTERIUM E-COLI) (BACTERIUM JUNK)
  (ANTIBIOTIC ANTIBIOTIC-1) (MRNA INSULIN-GENE) (CLEAVABLE E-COLI-EXOSOME)
  (CLEAVABLE JUNK-EXOSOME) (ACCEPTS JUNK-EXOSOME JUNK)
  (ACCEPTS E-COLI-EXOSOME E-COLI) (RESISTS ANTIBIOTIC-1 E-COLI-EXOSOME))
  (:GOAL
 (AND (EXISTS (?X ?Y)
  (AND (BACTERIUM ?Y) (MOLECULE ?X) (CONTAINS INSULIN-GENE ?X)
   (CONTAINS ?X ?Y) (PURE ?Y))))))

