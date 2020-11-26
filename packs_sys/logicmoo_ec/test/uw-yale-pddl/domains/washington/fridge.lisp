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
;; Dan's fridge domain
(define (domain fridge-domain)
  (:requirements :strips :equality :typing)
  (:types screw backplane compressor)
  (:predicates (screwed ?s)
	       (holds ?s ?b)
	       (in-place ?b)
	       (part-of ?b ?f)
	       (fridge ?f)
	       (fridge-on ?f)
	       (covers ?b - object
		       ?x - compressor)
	       (attached ?x - compressor)
	       (ok ?c))
	       

  (:action unfasten
	     :parameters (?x - screw ?y - backplane)
	     :precondition (and (screwed ?X) (holds ?x ?y) )
	     :effect (not (screwed ?X)))
  (:action fasten
	     :parameters (?x - screw ?y - backplane)
	     :precondition (and (not (screwed ?X)) (holds ?x ?y))
	     :effect (screwed ?X))
  (:action remove-backplane
	     :parameters (?x - backplane ?f ?a ?b ?c ?d)
	     :precondition (and (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (in-place ?x) (part-of ?x ?f) 
				 (not (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x)  
				 (holds ?c ?x)  (holds ?d ?x)
				 (not (screwed ?a)) (not (screwed ?b)) 
				 (not (screwed ?c)) (not (screwed ?d)))
	     :effect (not (in-place ?X)))
  (:action attach-backplane
	     :parameters (?x - backplane ?f ?a ?b ?c ?d)
	     :precondition (and (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (not (in-place ?x))
				 (part-of ?x ?f) (not (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x) 
				 (holds ?c ?x)  (holds ?d ?x)
				 (not (screwed ?a)) (not (screwed ?b))
				 (not (screwed ?c)) (not (screwed ?d)))
	     :effect (in-place ?X))
  (:action start-fridge
	     :parameters (?f ?a ?b ?c ?d ?x)
	     :precondition (and (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (backplane ?x) (in-place ?x) (part-of ?x ?f)
				 (holds ?a ?x)(holds ?b ?x)
				 (holds ?c ?x)(holds ?d ?x)
				 (screwed ?a) (screwed ?b) 
				 (screwed ?c) (screwed ?d)
				 (not (fridge-on ?f)))
	     :effect (fridge-on ?f))
  (:action stop-fridge
	     :parameters (?f)
	     :precondition (fridge-on ?f)
	     :effect
	     (not (fridge-on ?f)))
  (:action change-compressor
	     :parameters (?x ?y - compressor ?a)
	     :precondition (and (not (= ?x ?y)) (backplane ?a) 
				 (not (in-place ?a))
				 (covers ?a ?x)
				 (attached ?x) (not (attached ?y)))
	     :effect (and (not (attached ?X)) (attached ?y)
			   (not (covers ?a ?x)) (covers ?a ?y))))

(define (problem fixa)
    (:domain fridge-domain)
  (:objects s1 s2 s3 s4 - screw
	    b1 - backplane
	    c1 c2 - compressor
	    f1)
  (:init (fridge f1)
	 (covers b1 c1) (part-of b1 f1)
	 (holds s1 b1)  (holds s2 b1)  (holds s3 b1)
	 (holds s4 b1)
	 (ok c1) (ok c2) (fridge-on f1)
	 (screwed s1) (screwed s2) (screwed s3) (screwed s4)
	 (in-place b1) (attached c1))
  (:goal (and (attached c2) (ok c2)))
  (:length (:serial 7 ) (:parallel 3)))

(define (problem fixb)
    (:domain fridge-domain)
  (:objects s1 s2 s3 s4 - screw
	    b1 - backplane
	    c1 c2 - compressor
	    f1)
  (:init (fridge f1)
	 (covers b1 c1) (part-of b1 f1)
	 (holds s1 b1)  (holds s2 b1)  (holds s3 b1)
	 (holds s4 b1)
	 (ok c1) (ok c2) (fridge-on f1)
	 (screwed s1) (screwed s2) (screwed s3) (screwed s4)
	 (in-place b1) (attached c1))
  (:length (:serial 13 ) (:parallel 6))
  (:goal (and (attached c2) (ok c2) (fridge-on f1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The same thing, without typing.

(define (domain fridge-domain-strips)
    (:requirements :adl)
  
  (:predicates (screwed ?s)
	       (holds ?s ?b)
	       (in-place ?b)
	       (part-of ?b ?f)
	       (fridge-on ?f)
	       (fridge ?f)
	       (covers ?b ?x)
	       (attached ?x)
	       (ok ?c)
	       (screw ?s)
	       (backplane ?b)
	       (compressor ?c))

  (:action unfasten
	     :parameters (?x ?y)
			  :precondition (AND 
					 (screw ?x) (backplane ?y)
					 (screwed ?X) (holds ?x ?y) )
	     :effect (NOT (screwed ?X)))
  (:action fasten
	     :parameters (?x ?y)
	     :precondition (AND 
			    (screw ?x) (backplane ?y)
			    (NOT (screwed ?X)) (holds ?x ?y))
	     :effect (screwed ?X))
  (:action remove-backplane
	     :parameters (?x ?f ?a ?b ?c ?d)
	     :precondition (AND 
			    (backplane ?x)
			    (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (in-place ?x) (part-of ?x ?f) (NOT (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x)  
				 (holds ?c ?x)  (holds ?d ?x)
				 (NOT (screwed ?a)) (NOT (screwed ?b)) 
				 (NOT (screwed ?c)) (NOT (screwed ?d)))
	     :effect (NOT (in-place ?X)))
  (:action attach-backplane
	     :parameters (?x ?f ?a ?b ?c ?d)
	     :precondition (AND 
			    (backplane ?x)
			    (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (NOT (in-place ?x))
				 (part-of ?x ?f) (NOT (fridge-on ?f))
				 (holds ?a ?x)  (holds ?b ?x) 
				 (holds ?c ?x)  (holds ?d ?x)
				 (NOT (screwed ?a)) (NOT (screwed ?b))
				 (NOT (screwed ?c)) (NOT (screwed ?d)))
	     :effect (in-place ?X))
  (:action start-fridge
	     :parameters (?f ?a ?b ?c ?d ?x)
	     :precondition (AND (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (backplane ?x) (in-place ?x) (part-of ?x ?f)
				 (holds ?a ?x)(holds ?b ?x)(holds ?c ?x)(holds ?d ?x)
				 (screwed ?a) (screwed ?b) (screwed ?c) (screwed ?d)
				 (NOT (fridge-on ?f)))
	     :effect (fridge-on ?f))
  (:action stop-fridge
	     :parameters (?f)
	     :precondition (fridge-on ?f)
	     :effect
	     (NOT (fridge-on ?f)))
  (:action change-compressor
	     :parameters (?x ?y ?a)
	     :precondition (AND (not (= ?x ?y)) (backplane ?a) (NOT (in-place ?a))
				 (covers ?a ?x)
				 (compressor ?x) (compressor ?y) 
				 (attached ?x) (NOT (attached ?y)))
	     :effect (AND (NOT (attached ?X)) (attached ?y)
			   (NOT (covers ?a ?x)) (covers ?a ?y))))

(define (problem fixa-strips)
    (:domain fridge-domain-strips)
  (:objects s1 s2 s3 s4 b1 c1 c2 f1)
  (:init (screw s1)
	 (screw s2) (screw s3) (screw s4) 
	 (backplane b1)
	 (compressor c1) (compressor c2) (fridge f1)
	 (covers b1 c1) (part-of b1 f1)
	 (holds s1 b1)  (holds s2 b1)  (holds s3 b1)
	 (holds s4 b1)
	 (ok c1) (ok c2) (fridge-on f1)
	 (screwed s1) (screwed s2) (screwed s3) (screwed s4)
	 (in-place b1) (attached c1))
  (:goal (AND (attached c2) (ok c2)))
  (:length (:serial 7 ) (:parallel 3)))

(define (problem fixb-strips)
    (:domain fridge-domain-strips)
  (:objects s1 s2 s3 s4 b1 c1 c2 f1)
  (:init (screw s1)
	 (screw s2) (screw s3) (screw s4) 
	 (backplane b1)
	 (compressor c1) (compressor c2) (fridge f1)
	 (covers b1 c1) (part-of b1 f1)
	 (holds s1 b1)  (holds s2 b1)  (holds s3 b1)
	 (holds s4 b1)
	 (ok c1) (ok c2) (fridge-on f1)
	 (screwed s1) (screwed s2) (screwed s3) (screwed s4)
	 (in-place b1) (attached c1))
  (:goal (AND (attached c2) (ok c2) (fridge-on f1)))
  (:length (:serial 13 ) (:parallel 6))
    )

