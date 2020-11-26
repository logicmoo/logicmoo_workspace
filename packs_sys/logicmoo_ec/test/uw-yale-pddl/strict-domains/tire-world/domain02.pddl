;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Version with variable typing

(define (domain flat-tire-typing)
  (:requirements :strips :equality :typing)
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
      :precondition (and (not (= ?x ?c)) (in ?x ?c) (open ?c))
      :effect (and (have ?x)
		    (not (in ?x ?c))))

  (:action put-away
      :parameters (?x - (either tool wheel) ?c - container)
      :precondition (and (not (= ?x ?c)) (have ?x) (open ?c))
      :effect (and (in ?x ?c)
		    (not (have ?x))))

  (:action loosen
      :parameters (?n - nut ?h - hub)
      :precondition (and (not (= ?n ?h)) (have wrench) (tight ?n ?h)
			  (on-ground ?h))
      :effect (and (loose ?n ?h)
		    (not (tight ?n ?h))))

  (:action tighten
      :parameters (?n - nut ?h - hub)
      :precondition (and (not (= ?n ?h)) (have wrench) (loose ?n ?h)
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
      :precondition (and (not (= ?n ?h))
			  (not (on-ground ?h)) (not (unfastened ?h))
			  (have wrench) (loose ?n ?h))
      :effect (and (have ?n) (unfastened ?h)
		    (not (on ?n ?h)) (not (loose ?n ?h))))

  (:action put-on-nuts
      :parameters (?n - nut ?h - hub)
      :precondition (and (not (= ?n ?h))
			  (have wrench) (unfastened ?h)
			  (not (on-ground ?h)) (have ?n))
      :effect
      (and (loose ?n ?h) (not (unfastened ?h)) (not (have ?n))))

  (:action remove-wheel
      :parameters (?w - wheel ?h - hub)
      :precondition (and (not (= ?w ?h)) (not (on-ground ?h))
			  (on ?w ?h) (unfastened ?h))
      :effect (and (have ?w) (free ?h) (not (on ?w ?h))))

  (:action put-on-wheel
      :parameters (?w - wheel ?h - hub)
      :precondition (and (not (= ?w ?h)) (have ?w) (free ?h) (unfastened ?h)
			  (not (on-ground ?h)))
      :effect
      (and (on ?w ?h) (not (have ?w)) (not (free ?h))))

  (:action inflate
      :parameters (?w - wheel)
      :precondition (and (have pump) (not (inflated ?w)) (intact ?w))
      :effect (inflated ?w)))
