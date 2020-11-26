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