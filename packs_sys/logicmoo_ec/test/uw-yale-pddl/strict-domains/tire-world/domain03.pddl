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
      :precondition (and (not (= ?x ?c)) (in ?x ?c) (open ?c))
      :effect (and (have ?x)
		    (not (in ?x ?c))))

  (:action put-away
      :parameters (?x - physobj ?c - container)
      :precondition (and (not (= ?x ?c)) (have ?x) (open ?c))
      :effect (and (in ?x ?c)
		    (not (have ?x))))

  (:action loosen
	     :parameters (?n - nut ?h - hub)
	     :precondition (and (not (= ?n ?h)) (have wrench) (tight ?n ?h)
				 (not (jacked ?h)))
	     :effect (and (loose ?n ?h)
			   (not (tight ?n ?h))))

  (:action tighten
	     :parameters (?n - nut ?h - hub)
	     :precondition (and (not (= ?n ?h)) (have wrench) (loose ?n ?h)
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
	     :precondition (and (not (= ?n ?h))
				 (jacked ?h) (not (unfastened ?h))
				 (have wrench) (loose ?n ?h))
	     :effect (and (have ?n) (unfastened ?h)
			   (not (on ?n ?h)) (not (loose ?n ?h))))

  (:action do-up
	     :parameters (?n - nut ?h - hub)
	     :precondition (and (not (= ?n ?h))
				 (have wrench) (unfastened ?h)
				 (jacked ?h) (have ?n))
	     :effect
	     (and (loose ?n ?h) (not (unfastened ?h)) (not (have ?n))))

  (:action remove-wheel
	     :parameters (?w - wheel ?h - hub)
	     :precondition (and (not (= ?w ?h)) (jacked ?h)
				 (on ?w ?h) (unfastened ?h))
	     :effect (and (have ?w) (free ?h) (not (on ?w ?h))))

  (:action put-on-wheel
	     :parameters (?w - wheel ?h - hub)
	     :precondition (and (not (= ?w ?h)) (have ?w) (free ?h)
				 (unfastened ?h) (jacked ?h))
	     :effect
	     (and (on ?w ?h) (not (have ?w)) (not (free ?h))))

  (:action inflate
	     :parameters (?w - wheel)
	     :precondition (and (have pump) (not (inflated ?w)) (intact ?w))
	     :effect (inflated ?w)))