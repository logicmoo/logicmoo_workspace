(define (problem get-paid2)             ; graph-plan 4 steps, 6 actions
    (:domain briefcase-world)
  (:objects home office - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:goal (and (at P home) (at D office) (at B home)))
  (:length (:serial 4) (:parallel 4)))