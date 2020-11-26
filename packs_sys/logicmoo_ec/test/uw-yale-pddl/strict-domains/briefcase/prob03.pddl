(define (problem get-paid3)             ; graph-plan 6 steps, 8 actions
    (:domain briefcase-world)
  (:objects home office bank - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:goal (and (at P bank) (at D office) (at B home)))
  (:length (:serial 6) (:parallel 6))
    )