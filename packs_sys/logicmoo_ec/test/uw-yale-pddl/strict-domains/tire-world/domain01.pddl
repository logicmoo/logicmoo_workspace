; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flat-tire domain (from Stuart Russell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First the Strips version

(define (domain flat-tire-strips)
  (:requirements :strips :equality)

  (:constants wrench jack pump)
  (:predicates (annoyed)
	       (container ?c)
	       (locked ?c)
	       (open ?c)
	       (in ?x ?c)
	       (have ?x)
	       (nut ?n)
	       (hub ?h)
	       (loose ?x ?h)
	       (tight ?x ?h)
	       (on-ground ?h)
	       (unfastened ?h)
	       (on ?x ?h)
	       (wheel ?w)
	       (free ?h)
	       (inflated ?w)
	       (intact ?w))
  
  (:action cuss
      :effect (not (annoyed)))

  (:action open-container
      :parameters (?c)
      :precondition (and (container ?c) (not (locked ?c)) (not (open ?c)))
      :effect (open ?c))

  (:action close-container
      :parameters (?c)
      :precondition (and (container ?c) (open ?c))
      :effect (not (open ?c)))

  (:action fetch
      :parameters (?x ?c)
      :precondition (and (container ?c) (in ?x ?c) (open ?c) (not (= ?x ?c)))
      :effect (and (have ?x)
		    (not (in ?x ?c))))

  (:action put-away
      :parameters (?x ?c)
      :precondition (and (container ?c) (have ?x) (open ?c) (not (= ?x ?c)))
      :effect (and (in ?x ?c)
		    (not (have ?x))))

  (:action loosen
      :parameters (?x ?h)
      :precondition (and (nut ?x) (hub ?h) (have wrench)
			  (tight ?x ?h) (on-ground ?h) (not (= ?x ?h)))
      :effect (and (loose ?x ?h)
		    (not (tight ?x ?h))))

  (:action tighten
      :parameters (?x ?h)
      :precondition (and (nut ?x) (hub ?h) (have wrench) (loose ?x ?h)
			  (on-ground ?h) (not (= ?x ?h)))
      :effect (and (tight ?x ?h)
		    (not (loose ?x ?h))))

  (:action jack-up
      :parameters (?h)
      :precondition (and (hub ?h) (on-ground ?h) (have jack))
      :effect (and (not (on-ground ?h))
		    (not (have jack))))

  ;; jacking down wheel x on hub y (dependency would be better)
  (:action jack-down
      :parameters (?h)
      :precondition (and (hub ?h) (not (on-ground ?h)))
      :effect (and (on-ground ?h)
		    (have jack)))

  (:action remove-nuts
      :parameters (?x ?h)
      :precondition (and (nut ?x) (hub ?h) (not (= ?x ?h))
			  (not (on-ground ?h)) (not (unfastened ?h))
			  (have wrench) (loose ?x ?h))
      :effect (and (have ?x) (unfastened ?h)
		    (not (on ?x ?h)) (not (loose ?x ?h))))

  (:action put-on-nuts
      :parameters (?x ?h)
      :precondition (and (nut ?x) (hub ?h) (not (= ?x ?h))
			  (have wrench) (unfastened ?h)
			  (not (on-ground ?h)) (have ?x))
      :effect
      (and (loose ?x ?h) (not (unfastened ?h)) (not (have ?x))))

  (:action remove-wheel
      :parameters (?w ?h)
      :precondition (and  (wheel ?w) (hub ?h) (not (= ?w ?h))
			   (not (on-ground ?h)) (on ?w ?h) (unfastened ?h))
      :effect (and (have ?w) (free ?h) (not (on ?w ?h))))

  (:action put-on-wheel
      :parameters (?w ?h)
      :precondition (and (hub ?h) (wheel ?w) (not (= ?w ?h)) (have ?w)
			  (free ?h) (unfastened ?h) (not (on-ground ?h)))
      :effect
      (and (on ?w ?h) (not (have ?w)) (not (free ?h))))

  (:action inflate
      :parameters (?w)
      :precondition (and (wheel ?w) (have pump) (not (inflated ?w))
			  (intact ?w))
      :effect (inflated ?w)))


