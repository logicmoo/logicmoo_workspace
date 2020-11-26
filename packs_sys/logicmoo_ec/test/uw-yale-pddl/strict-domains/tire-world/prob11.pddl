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