(define (problem fix1)			;just fetch stuff from trunk
    (:domain flat-tire-typing)
  (:objects wheel1 wheel2 - wheel the-hub - hub nuts - nut boot - container)
  (:init (intact wheel2)
	 (in jack boot) (in pump boot)
	 (in wheel2 boot) (in wrench boot)
	 (on wheel1 the-hub) (on-ground the-hub) (tight nuts the-hub))
  (:goal (and (have jack) (have pump) (have wheel2)
	      (have wrench))))