(define (domain zeon)
  (:predicates
    (aircraft ?x0)
    (person ?x0)
    (city ?x0)
    (flevel ?x0)
    (at ?x0 ?x1)
    (in ?x0 ?x1)
    (fuel_level ?x0 ?x1)
    (next ?x0 ?x1)
    (autstate_1_2)
    (autstate_1_3)
    (autstate_1_1)
    (prev_autstate_1_2)
    (prev_autstate_1_3)
    (prev_autstate_1_1)
    (autstate_2_2)
    (autstate_2_3)
    (autstate_2_1)
    (prev_autstate_2_2)
    (prev_autstate_2_3)
    (prev_autstate_2_1)
    (aut_in_final_1)
    (aut_in_final_2)
  )
  (:action board
    :parameters (?x0 ?x1 ?x2)
    :precondition 
      (and
        (person ?x0)
        (and
          (aircraft ?x1)
          (and
            (city ?x2)
            (and
              (at ?x0 ?x2)
              (at ?x1 ?x2)))))
    :effect
      (and
        (in ?x0 ?x1)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_3)
          (prev_autstate_2_3))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (at ?x0 ?x2))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_3))
          (not 
            (prev_autstate_2_3)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
      )
    )
  (:action debark
    :parameters (?x0 ?x1 ?x2)
    :precondition 
      (and
        (person ?x0)
        (and
          (aircraft ?x1)
          (and
            (city ?x2)
            (and
              (in ?x0 ?x1)
              (at ?x1 ?x2)))))
    :effect
      (and
        (at ?x0 ?x2)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_3)
          (prev_autstate_2_3))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (in ?x0 ?x1))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_3))
          (not 
            (prev_autstate_2_3)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
      )
    )
  (:action fly
    :parameters (?x0 ?x1 ?x2 ?x3 ?x4)
    :precondition 
      (and
        (aircraft ?x0)
        (and
          (city ?x2)
          (and
            (at ?x0 ?x1)
            (and
              (fuel_level ?x0 ?x3)
              (next ?x4 ?x3)))))
    :effect
      (and
        (at ?x0 ?x2)
        (fuel_level ?x0 ?x4)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_3)
          (prev_autstate_2_3))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (fuel_level ?x0 ?x3))
        (not 
          (at ?x0 ?x1))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_3))
          (not 
            (prev_autstate_2_3)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
      )
    )
  (:action refuel
    :parameters (?x0 ?x1 ?x2 ?x3)
    :precondition 
      (and
        (aircraft ?x0)
        (and
          (city ?x1)
          (and
            (fuel_level ?x0 ?x2)
            (next ?x2 ?x3))))
    :effect
      (and
        (fuel_level ?x0 ?x3)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_3)
          (prev_autstate_2_3))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (fuel_level ?x0 ?x2))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_3))
          (not 
            (prev_autstate_2_3)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
      )
    )
(:derived 
    (autstate_1_2)
    (prev_autstate_1_2)
)

(:derived 
    (autstate_1_3)
    (or
      (and
        (prev_autstate_1_2)
        (at person1 city2))
      (prev_autstate_1_3))
)

(:derived 
    (autstate_1_1)
    (or
      (and
        (prev_autstate_1_3)
        (at person1 city3))
      (prev_autstate_1_1))
)

(:derived 
    (autstate_2_2)
    (prev_autstate_2_2)
)

(:derived 
    (autstate_2_3)
    (or
      (and
        (prev_autstate_2_2)
        (at person2 city3))
      (prev_autstate_2_3))
)

(:derived 
    (autstate_2_1)
    (or
      (and
        (prev_autstate_2_3)
        (at person2 city2))
      (prev_autstate_2_1))
)

(:derived 
    (aut_in_final_1)
    (autstate_1_1)
)

(:derived 
    (aut_in_final_2)
    (autstate_2_1)
)

)