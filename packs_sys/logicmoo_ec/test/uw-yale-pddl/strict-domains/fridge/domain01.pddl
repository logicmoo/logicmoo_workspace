; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dan's fridge domain
(define (domain fridge-domain-typed)
  (:requirements :strips :equality :typing)
  (:types screw backplane compressor fridge)
  (:predicates (screwed ?s)
	       (holds ?s ?b)
	       (in-place ?b)
	       (part-of ?b ?f)
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
	     :parameters (?x - backplane ?f - fridge
			  ?a ?b ?c ?d - screw)
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
	     :parameters (?x - backplane ?f - fridge
			  ?a ?b ?c ?d - screw)
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
	     :parameters (?f - fridge ?a ?b ?c ?d - screw ?x - backplane)
	     :precondition (and (not (= ?a ?b)) (not (= ?a ?c)) (not (= ?a ?d))
				 (not (= ?b ?c)) (not (= ?b ?d)) (not (= ?c ?d))
				 (in-place ?x) (part-of ?x ?f)
				 (holds ?a ?x)(holds ?b ?x)
				 (holds ?c ?x)(holds ?d ?x)
				 (screwed ?a) (screwed ?b) 
				 (screwed ?c) (screwed ?d)
				 (not (fridge-on ?f)))
	     :effect (fridge-on ?f))
  (:action stop-fridge
	     :parameters (?f - fridge)
	     :precondition (fridge-on ?f)
	     :effect
	     (not (fridge-on ?f)))
  (:action change-compressor
	     :parameters (?x ?y - compressor ?a - backplane)
	     :precondition (and (not (= ?x ?y))  
				 (not (in-place ?a))
				 (covers ?a ?x)
				 (attached ?x) (not (attached ?y)))
	     :effect (and (not (attached ?X)) (attached ?y)
			   (not (covers ?a ?x)) (covers ?a ?y))))



