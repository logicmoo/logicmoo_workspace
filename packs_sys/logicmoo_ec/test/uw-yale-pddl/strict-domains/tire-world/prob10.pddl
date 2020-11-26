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