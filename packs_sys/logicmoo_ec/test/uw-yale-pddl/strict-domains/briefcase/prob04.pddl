(define (problem get-paid4)             ; graph-plan 6 steps, 8 actions
    (:domain briefcase-world)
  (:objects home office bank - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:goal (and (at B home) (at D office) (at P bank)))
  (:length (:serial 6) (:parallel 6))
    )