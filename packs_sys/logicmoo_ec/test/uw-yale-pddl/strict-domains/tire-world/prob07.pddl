(define (problem fix2)			; get the car jacked up
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (intact wheel2)
	 (open boot)
	 (have jack) (have pump) (have wheel2) (have wrench)
	 (on wheel1 the-hub) (on-ground the-hub) (tight nuts the-hub))
  (:goal (and (inflated wheel2) (not (on-ground the-hub))
	      (loose nuts the-hub))))