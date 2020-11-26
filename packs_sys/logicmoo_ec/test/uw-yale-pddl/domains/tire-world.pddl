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
  (:requirements :strips)

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
      :precondition (and (container ?c) (in ?x ?c) (open ?c))
      :effect (and (have ?x)
		    (not (in ?x ?c))))

  (:action put-away
      :parameters (?x ?c)
      :precondition (and (container ?c) (have ?x) (open ?c) )
      :effect (and (in ?x ?c)
		    (not (have ?x))))

  (:action loosen
      :parameters (?x ?h)
      :precondition (and (nut ?x) (hub ?h) (have wrench)
			  (tight ?x ?h) (on-ground ?h) )
      :effect (and (loose ?x ?h)
		    (not (tight ?x ?h))))

  (:action tighten
      :parameters (?x ?h)
      :precondition (and (nut ?x) (hub ?h) (have wrench) (loose ?x ?h)
			  (on-ground ?h) )
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
      :precondition (and (nut ?x) (hub ?h) 
			  (not (on-ground ?h)) (not (unfastened ?h))
			  (have wrench) (loose ?x ?h))
      :effect (and (have ?x) (unfastened ?h)
		    (not (on ?x ?h)) (not (loose ?x ?h))))

  (:action put-on-nuts
      :parameters (?x ?h)
      :precondition (and (nut ?x) (hub ?h) 
			  (have wrench) (unfastened ?h)
			  (not (on-ground ?h)) (have ?x))
      :effect
      (and (loose ?x ?h) (not (unfastened ?h)) (not (have ?x))))

  (:action remove-wheel
      :parameters (?w ?h)
      :precondition (and  (wheel ?w) (hub ?h) 
			   (not (on-ground ?h)) (on ?w ?h) (unfastened ?h))
      :effect (and (have ?w) (free ?h) (not (on ?w ?h))))

  (:action put-on-wheel
      :parameters (?w ?h)
      :precondition (and (hub ?h) (wheel ?w)  (have ?w)
			  (free ?h) (unfastened ?h) (not (on-ground ?h)))
      :effect
      (and (on ?w ?h) (not (have ?w)) (not (free ?h))))

  (:action inflate
      :parameters (?w)
      :precondition (and (wheel ?w) (have pump) (not (inflated ?w))
			  (intact ?w))
      :effect (inflated ?w)))


(DEFINE (PROBLEM FIX-STRIPS1) 
    (:DOMAIN FLAT-TIRE-STRIPS)
  (:objects wheel1 wheel2 the-hub nuts trunk)
  (:init (WHEEL WHEEL1) (WHEEL WHEEL2) (HUB the-HUB) (NUT NUTS) (CONTAINER TRUNK)
	  (INTACT WHEEL2) (IN JACK TRUNK) (IN PUMP TRUNK) (IN WHEEL2 TRUNK)
	  (IN WRENCH TRUNK) (ON WHEEL1 the-HUB) (ON-GROUND the-HUB) (TIGHT NUTS the-HUB)
	  (NOT (LOCKED TRUNK)) (NOT (OPEN TRUNK)) (NOT (UNFASTENED the-HUB))
	  (NOT (INFLATED WHEEL2)) (NOT (INFLATED WHEEL1)) (NOT (INTACT WHEEL1)))
  (:GOAL (AND (ON WHEEL2 the-HUB)))
  (:length (:serial 9) (:parallel 7)))



(DEFINE (PROBLEM FIX-STRIPS2) 
    (:DOMAIN FLAT-TIRE-STRIPS)
  (:objects wheel1 wheel2 the-hub nuts trunk)
  (:init (WHEEL WHEEL1) (WHEEL WHEEL2) (HUB the-HUB) (NUT NUTS) (CONTAINER TRUNK) 
	 (INTACT WHEEL2) (IN JACK TRUNK) (IN PUMP TRUNK) (IN WHEEL2 TRUNK) 
	 (IN WRENCH TRUNK) (ON WHEEL1 THE-HUB) (ON-GROUND THE-HUB) (TIGHT NUTS THE-HUB) 
	 (NOT (LOCKED TRUNK)) (NOT (OPEN TRUNK)) (NOT (UNFASTENED THE-HUB)) 
	 (NOT (INFLATED WHEEL2)) (NOT (INFLATED WHEEL1)) (NOT (INTACT WHEEL1)))
  (:GOAL (AND (NOT (OPEN TRUNK)) (IN JACK TRUNK) (IN PUMP TRUNK) (IN WHEEL1 TRUNK) 
	      (IN WRENCH TRUNK) (ON WHEEL2 THE-HUB)))
  (:length (:serial 14) (:parallel 10)))

(DEFINE (PROBLEM FIX-STRIPS3) 
    (:DOMAIN FLAT-TIRE-STRIPS)
  (:objects wheel1 wheel2 the-hub nuts trunk)
  (:init (WHEEL WHEEL1) (WHEEL WHEEL2) (HUB THE-HUB) (NUT NUTS) (CONTAINER TRUNK) 
	 (INTACT WHEEL2) (IN JACK TRUNK) (IN PUMP TRUNK) (IN WHEEL2 TRUNK) 
	 (IN WRENCH TRUNK) (ON WHEEL1 THE-HUB) (ON-GROUND THE-HUB) 
	 (TIGHT NUTS THE-HUB) (NOT (LOCKED TRUNK)) (NOT (OPEN TRUNK)) 
	 (NOT (UNFASTENED THE-HUB)) (NOT (INFLATED WHEEL2)) (NOT (INFLATED WHEEL1)) 
	 (NOT (INTACT WHEEL1)))
  (:GOAL (AND (ON WHEEL2 THE-HUB) (TIGHT NUTS THE-HUB) (INFLATED WHEEL2)))
  (:length (:serial 14) (:parallel 10)))

(DEFINE (PROBLEM FIX-STRIPS4) 
    (:DOMAIN FLAT-TIRE-STRIPS) 
  (:objects wheel1 wheel2 the-hub nuts trunk)
  (:init (WHEEL WHEEL1) (WHEEL WHEEL2) (HUB THE-HUB) (NUT NUTS) (CONTAINER TRUNK) 
	 (INTACT WHEEL2) (IN JACK TRUNK) (IN PUMP TRUNK) (IN WHEEL2 TRUNK) 
	 (IN WRENCH TRUNK) (ON WHEEL1 THE-HUB) (ON-GROUND THE-HUB) 
	 (TIGHT NUTS THE-HUB) (NOT (LOCKED TRUNK)) (NOT (OPEN TRUNK)) 
	 (NOT (UNFASTENED THE-HUB)) (NOT (INFLATED WHEEL2)) (NOT (INFLATED WHEEL1)) 
	 (NOT (INTACT WHEEL1)))
  (:GOAL (AND (NOT (OPEN TRUNK)) (IN JACK TRUNK) (IN PUMP TRUNK) (IN WHEEL1 TRUNK) 
	      (IN WRENCH TRUNK) (INFLATED WHEEL2) (ON WHEEL2 THE-HUB) 
	      (TIGHT NUTS THE-HUB)))
  (:length (:serial 19) (:parallel 12)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Version with variable typing

(define (domain flat-tire-typing)
  (:requirements :strips :typing)
;;		 :cwa			;unfastened, open, locked

  (:types container nut hub tool wheel)
  (:constants wrench pump jack - tool)
  (:predicates (annoyed)
	       (locked ?c - container)
	       (open ?c - container)
	       (in ?x - (either tool wheel) ?c - container)
	       (have ?x - (either tool nut wheel))
	       (tight ?n - nut ?h - hub)
	       (loose ?n - nut ?h - hub)
	       (on-ground ?h - hub)
	       (unfastened ?h - hub)
	       (on ?n - (either nut wheel) ?h - hub)
	       (free ?h - hub)
	       (inflated ?w - wheel)
	       (intact ?w - wheel))


  (:action cuss
      :effect (not (annoyed)))

  (:action open-container
      :parameters (?c - container)
      :precondition (and (not (locked ?c)) (not (open ?c)))
      :effect (open ?c))

  (:action close-container
      :parameters (?c - container)
      :precondition (open ?c)
      :effect (not (open ?c)))

  (:action fetch
      :parameters (?x - (either tool wheel) ?c - container)
      :precondition (and  (in ?x ?c) (open ?c))
      :effect (and (have ?x)
		    (not (in ?x ?c))))

  (:action put-away
      :parameters (?x - (either tool wheel) ?c - container)
      :precondition (and  (have ?x) (open ?c))
      :effect (and (in ?x ?c)
		    (not (have ?x))))

  (:action loosen
      :parameters (?n - nut ?h - hub)
      :precondition (and  (have wrench) (tight ?n ?h)
			  (on-ground ?h))
      :effect (and (loose ?n ?h)
		    (not (tight ?n ?h))))

  (:action tighten
      :parameters (?n - nut ?h - hub)
      :precondition (and  (have wrench) (loose ?n ?h)
			  (on-ground ?h))
      :effect (and (tight ?n ?h)
		    (not (loose ?n ?h))))

  (:action jack-up
      :parameters (?h - hub)
      :precondition (and (on-ground ?h) (have jack))
      :effect (and (not (on-ground ?h))
		    (not (have jack))))

  ;; jacking down wheel x on hub y (dependency would be better)
  (:action jack-down
      :parameters (?h - hub)
      :precondition (not (on-ground ?h))
      :effect (and (on-ground ?h)
		    (have jack)))

  (:action remove-nuts
      :parameters (?n - nut ?h - hub)
      :precondition (and 
			  (not (on-ground ?h)) (not (unfastened ?h))
			  (have wrench) (loose ?n ?h))
      :effect (and (have ?n) (unfastened ?h)
		    (not (on ?n ?h)) (not (loose ?n ?h))))

  (:action put-on-nuts
      :parameters (?n - nut ?h - hub)
      :precondition (and 
			  (have wrench) (unfastened ?h)
			  (not (on-ground ?h)) (have ?n))
      :effect
      (and (loose ?n ?h) (not (unfastened ?h)) (not (have ?n))))

  (:action remove-wheel
      :parameters (?w - wheel ?h - hub)
      :precondition (and  (not (on-ground ?h))
			  (on ?w ?h) (unfastened ?h))
      :effect (and (have ?w) (free ?h) (not (on ?w ?h))))

  (:action put-on-wheel
      :parameters (?w - wheel ?h - hub)
      :precondition (and  (have ?w) (free ?h) (unfastened ?h)
			  (not (on-ground ?h)))
      :effect
      (and (on ?w ?h) (not (have ?w)) (not (free ?h))))

  (:action inflate
      :parameters (?w - wheel)
      :precondition (and (have pump) (not (inflated ?w)) (intact ?w))
      :effect (inflated ?w)))

(define (problem fixit)			;everything put away
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (intact wheel2)
	 (in jack boot) (in pump boot)
	 (in wheel2 boot) (in wrench boot)
	 (on wheel1 the-hub) (on-ground the-hub) (tight nuts the-hub))
  (:goal (and (not (open boot)) (in jack boot) (in pump boot)
	      (in wheel1 boot) (in wrench boot)
	      (tight nuts the-hub) (inflated wheel2)(on wheel2 the-hub))))

(define (problem fix1)			;just fetch stuff from trunk
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (intact wheel2)
	 (in jack boot) (in pump boot)
	 (in wheel2 boot) (in wrench boot)
	 (on wheel1 the-hub) (on-ground the-hub) (tight nuts the-hub))
  (:goal (and (have jack) (have pump) (have wheel2)
	      (have wrench))))

(define (problem fix2)			; get the car jacked up
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (intact wheel2)
	 (open boot)
	 (have jack) (have pump) (have wheel2) (have wrench)
	 (on wheel1 the-hub) (on-ground the-hub) (tight nuts the-hub))
  (:goal (and (inflated wheel2) (not (on-ground the-hub))
	      (loose nuts the-hub))))

(define (problem fix3)			;tools not put away
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (intact wheel2)
	 (have pump) (have wheel2)
	 (have wrench) (on wheel1 the-hub) (inflated wheel2)
	 (loose nuts the-hub))
  (:goal (and (tight nuts the-hub) (on-ground the-hub)
	      (on wheel2 the-hub)
	      )))

(define (problem fix4)			; trunk not closed.
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (intact wheel2)
	 (have jack) (have pump) (have wheel1)
	 (have wrench) (open boot)
	 (inflated wheel2)
	 (on wheel2 the-hub)
	 (tight nuts the-hub) (on-ground the-hub)
	 )
  (:goal (and
	  (in jack boot) (in pump boot) (in wheel1 boot)
	  (in wrench boot) (inflated wheel2) (on wheel2 the-hub)
	  (tight nuts the-hub))))

(define (problem fix5)			; fixit, but start with trunk open
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (open boot) (in jack boot) (in pump boot)
	 (in wheel1 boot)
	 (in wrench boot) (inflated wheel2) (on wheel2 the-hub)
	 (tight nuts the-hub))
  (:goal (and
	  (not (open boot)) (in jack boot) (in pump boot)
	  (in wheel1 boot)
	  (in wrench boot) (inflated wheel2) (on wheel2 the-hub)
	  (tight nuts the-hub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain flat-tire-adl)
  (:requirements :adl :universal-preconditions)

  (:types container hub physobj - object 
	  nut tool wheel - physobj)
  (:constants wrench pump jack - tool
	      ground - object)
  (:predicates (annoyed)
	       (locked ?c - container)
	       (open ?c - container)
	       (in ?x - physobj ?c - container)
	       (have ?x - physobj)
	       (tight ?n - nut ?h - hub)
	       (loose ?n - nut ?h - hub)
	       (on-ground ?h - hub)
	       (unfastened ?h - hub)
	       (on ?n - (either nut wheel) ?x - object)
	       (free ?h - hub)
	       (inflated ?w - wheel)
	       (intact ?w - wheel)
	       (jacked ?h - hub))
  
  (:action cuss
      :effect (not (annoyed)))

  (:action open-container
      :parameters (?c - container)
      :precondition (and (not (locked ?c)) (not (open ?c)))
      :effect (open ?c))

  (:action close-container
      :parameters (?c - container)
      :precondition (open ?c)
      :effect (not (open ?c)))

  (:action fetch
	     :parameters (?x - physobj ?c - container)
      :precondition (and  (in ?x ?c) (open ?c))
      :effect (and (have ?x)
		    (not (in ?x ?c))))

  (:action put-away
      :parameters (?x - physobj ?c - container)
      :precondition (and  (have ?x) (open ?c))
      :effect (and (in ?x ?c)
		    (not (have ?x))))

  (:action loosen
	     :parameters (?n - nut ?h - hub)
	     :precondition (and  (have wrench) (tight ?n ?h)
				 (not (jacked ?h)))
	     :effect (and (loose ?n ?h)
			   (not (tight ?n ?h))))

  (:action tighten
	     :parameters (?n - nut ?h - hub)
	     :precondition (and  (have wrench) (loose ?n ?h)
				 (not (jacked ?h)))
	     :effect (and (tight ?n ?h)
			   (not (loose ?n ?h))))

  (:action jack-up
	     :parameters (?h - hub)
	     :precondition (and (not (jacked ?h)) (have jack))
	     :effect (and (jacked ?h)
			   (forall (?x - (either nut wheel)) (when (on ?x ?h)
					  (not (on ?x ground))))
			   (not (have jack))))

  ;; jacking down wheel x on hub y (dependency would be better)
  (:action jack-down
	     :parameters (?h - hub)
	     :precondition (jacked ?h)
	     :effect (and (not (jacked ?h))
			  (forall (?y - (either nut wheel)) (when (on ?y ?h) 
							      (on ?y ground)))
			   (have jack)))

  (:action undo
	     :parameters (?n - nut ?h - hub)
	     :precondition (and 
				 (jacked ?h) (not (unfastened ?h))
				 (have wrench) (loose ?n ?h))
	     :effect (and (have ?n) (unfastened ?h)
			   (not (on ?n ?h)) (not (loose ?n ?h))))

  (:action do-up
	     :parameters (?n - nut ?h - hub)
	     :precondition (and 
				 (have wrench) (unfastened ?h)
				 (jacked ?h) (have ?n))
	     :effect
	     (and (loose ?n ?h) (not (unfastened ?h)) (not (have ?n))))

  (:action remove-wheel
	     :parameters (?w - wheel ?h - hub)
	     :precondition (and  (jacked ?h)
				 (on ?w ?h) (unfastened ?h))
	     :effect (and (have ?w) (free ?h) (not (on ?w ?h))))

  (:action put-on-wheel
	     :parameters (?w - wheel ?h - hub)
	     :precondition (and  (have ?w) (free ?h)
				 (unfastened ?h) (jacked ?h))
	     :effect
	     (and (on ?w ?h) (not (have ?w)) (not (free ?h))))

  (:action inflate
	     :parameters (?w - wheel)
	     :precondition (and (have pump) (not (inflated ?w)) (intact ?w))
	     :effect (inflated ?w)))


(define (problem fixit2)
    (:domain flat-tire-adl)
  (:objects wheel1 wheel2 - wheel
	    the-hub - hub  nuts - nut  boot - container)
  (:init (intact wheel2)
	 (in jack boot) (in pump boot)
	 (in wheel2 boot) (in wrench boot)
	 (on wheel1 the-hub) (on wheel1 ground) (tight nuts the-hub))
  (:goal (and
	  (not (open boot))
	  (forall (?x - tool) (in ?x boot))
	  (in wheel1 boot)
	  (tight nuts the-hub) (inflated wheel2)(on wheel2 the-hub))))
