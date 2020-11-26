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
    (autstate_2_3 ?x0)
    (autstate_2_1 ?x0)
    (autstate_2_4 ?x0)
    (autstate_2_2 ?x0)
    (prev_autstate_2_3 ?x0)
    (prev_autstate_2_1 ?x0)
    (prev_autstate_2_4 ?x0)
    (prev_autstate_2_2 ?x0)
    (autstate_3_3 ?x0)
    (autstate_3_1 ?x0)
    (autstate_3_4 ?x0)
    (autstate_3_2 ?x0)
    (prev_autstate_3_3 ?x0)
    (prev_autstate_3_1 ?x0)
    (prev_autstate_3_4 ?x0)
    (prev_autstate_3_2 ?x0)
    (aut_in_final_1)
    (aut_in_final_2 ?x0)
    (aut_in_final_3 ?x0)
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
        (forall (?x3)
          (when
            (autstate_2_3 ?x3)
            (prev_autstate_2_3 ?x3)))

        (forall (?x3)
          (when
            (autstate_2_1 ?x3)
            (prev_autstate_2_1 ?x3)))

        (forall (?x3)
          (when
            (autstate_2_4 ?x3)
            (prev_autstate_2_4 ?x3)))

        (forall (?x3)
          (when
            (autstate_2_2 ?x3)
            (prev_autstate_2_2 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_3 ?x3)
            (prev_autstate_3_3 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_1 ?x3)
            (prev_autstate_3_1 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_4 ?x3)
            (prev_autstate_3_4 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_2 ?x3)
            (prev_autstate_3_2 ?x3)))

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
        (forall (?x3)
          (when
            (not 
              (autstate_2_3 ?x3))
            (not 
              (prev_autstate_2_3 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_2_1 ?x3))
            (not 
              (prev_autstate_2_1 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_2_4 ?x3))
            (not 
              (prev_autstate_2_4 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_2_2 ?x3))
            (not 
              (prev_autstate_2_2 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_3 ?x3))
            (not 
              (prev_autstate_3_3 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_1 ?x3))
            (not 
              (prev_autstate_3_1 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_4 ?x3))
            (not 
              (prev_autstate_3_4 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_2 ?x3))
            (not 
              (prev_autstate_3_2 ?x3))))

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
        (forall (?x3)
          (when
            (autstate_2_3 ?x3)
            (prev_autstate_2_3 ?x3)))

        (forall (?x3)
          (when
            (autstate_2_1 ?x3)
            (prev_autstate_2_1 ?x3)))

        (forall (?x3)
          (when
            (autstate_2_4 ?x3)
            (prev_autstate_2_4 ?x3)))

        (forall (?x3)
          (when
            (autstate_2_2 ?x3)
            (prev_autstate_2_2 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_3 ?x3)
            (prev_autstate_3_3 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_1 ?x3)
            (prev_autstate_3_1 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_4 ?x3)
            (prev_autstate_3_4 ?x3)))

        (forall (?x3)
          (when
            (autstate_3_2 ?x3)
            (prev_autstate_3_2 ?x3)))

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
        (forall (?x3)
          (when
            (not 
              (autstate_2_3 ?x3))
            (not 
              (prev_autstate_2_3 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_2_1 ?x3))
            (not 
              (prev_autstate_2_1 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_2_4 ?x3))
            (not 
              (prev_autstate_2_4 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_2_2 ?x3))
            (not 
              (prev_autstate_2_2 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_3 ?x3))
            (not 
              (prev_autstate_3_3 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_1 ?x3))
            (not 
              (prev_autstate_3_1 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_4 ?x3))
            (not 
              (prev_autstate_3_4 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_3_2 ?x3))
            (not 
              (prev_autstate_3_2 ?x3))))

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
        (forall (?x5)
          (when
            (autstate_2_3 ?x5)
            (prev_autstate_2_3 ?x5)))

        (forall (?x5)
          (when
            (autstate_2_1 ?x5)
            (prev_autstate_2_1 ?x5)))

        (forall (?x5)
          (when
            (autstate_2_4 ?x5)
            (prev_autstate_2_4 ?x5)))

        (forall (?x5)
          (when
            (autstate_2_2 ?x5)
            (prev_autstate_2_2 ?x5)))

        (forall (?x5)
          (when
            (autstate_3_3 ?x5)
            (prev_autstate_3_3 ?x5)))

        (forall (?x5)
          (when
            (autstate_3_1 ?x5)
            (prev_autstate_3_1 ?x5)))

        (forall (?x5)
          (when
            (autstate_3_4 ?x5)
            (prev_autstate_3_4 ?x5)))

        (forall (?x5)
          (when
            (autstate_3_2 ?x5)
            (prev_autstate_3_2 ?x5)))

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
        (forall (?x5)
          (when
            (not 
              (autstate_2_3 ?x5))
            (not 
              (prev_autstate_2_3 ?x5))))

        (forall (?x5)
          (when
            (not 
              (autstate_2_1 ?x5))
            (not 
              (prev_autstate_2_1 ?x5))))

        (forall (?x5)
          (when
            (not 
              (autstate_2_4 ?x5))
            (not 
              (prev_autstate_2_4 ?x5))))

        (forall (?x5)
          (when
            (not 
              (autstate_2_2 ?x5))
            (not 
              (prev_autstate_2_2 ?x5))))

        (forall (?x5)
          (when
            (not 
              (autstate_3_3 ?x5))
            (not 
              (prev_autstate_3_3 ?x5))))

        (forall (?x5)
          (when
            (not 
              (autstate_3_1 ?x5))
            (not 
              (prev_autstate_3_1 ?x5))))

        (forall (?x5)
          (when
            (not 
              (autstate_3_4 ?x5))
            (not 
              (prev_autstate_3_4 ?x5))))

        (forall (?x5)
          (when
            (not 
              (autstate_3_2 ?x5))
            (not 
              (prev_autstate_3_2 ?x5))))

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
        (forall (?x4)
          (when
            (autstate_2_3 ?x4)
            (prev_autstate_2_3 ?x4)))

        (forall (?x4)
          (when
            (autstate_2_1 ?x4)
            (prev_autstate_2_1 ?x4)))

        (forall (?x4)
          (when
            (autstate_2_4 ?x4)
            (prev_autstate_2_4 ?x4)))

        (forall (?x4)
          (when
            (autstate_2_2 ?x4)
            (prev_autstate_2_2 ?x4)))

        (forall (?x4)
          (when
            (autstate_3_3 ?x4)
            (prev_autstate_3_3 ?x4)))

        (forall (?x4)
          (when
            (autstate_3_1 ?x4)
            (prev_autstate_3_1 ?x4)))

        (forall (?x4)
          (when
            (autstate_3_4 ?x4)
            (prev_autstate_3_4 ?x4)))

        (forall (?x4)
          (when
            (autstate_3_2 ?x4)
            (prev_autstate_3_2 ?x4)))

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
        (forall (?x4)
          (when
            (not 
              (autstate_2_3 ?x4))
            (not 
              (prev_autstate_2_3 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_2_1 ?x4))
            (not 
              (prev_autstate_2_1 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_2_4 ?x4))
            (not 
              (prev_autstate_2_4 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_2_2 ?x4))
            (not 
              (prev_autstate_2_2 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_3_3 ?x4))
            (not 
              (prev_autstate_3_3 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_3_1 ?x4))
            (not 
              (prev_autstate_3_1 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_3_4 ?x4))
            (not 
              (prev_autstate_3_4 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_3_2 ?x4))
            (not 
              (prev_autstate_3_2 ?x4))))

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
        (and
          (at person1 city1)
          (at person2 city1)))
      (prev_autstate_1_3))
)

(:derived 
    (autstate_1_1)
    (or
      (and
        (prev_autstate_1_3)
        (and
          (at person1 city2)
          (at person2 city2)))
      (prev_autstate_1_1))
)

(:derived 
    (autstate_2_1 ?x0)
    (or
      (and
        (prev_autstate_2_3 ?x0)
        (not 
          (at person1 ?x0)))
      (prev_autstate_2_1 ?x0))
)

(:derived 
    (autstate_2_4 ?x0)
    (or
      (prev_autstate_2_3 ?x0)
      (prev_autstate_2_4 ?x0))
)

(:derived 
    (autstate_2_2 ?x0)
    (or
      (and
        (prev_autstate_2_3 ?x0)
        (at person1 ?x0))
      (and
        (prev_autstate_2_4 ?x0)
        (at person1 ?x0)))
)

(:derived 
    (autstate_3_1 ?x0)
    (or
      (and
        (prev_autstate_3_3 ?x0)
        (not 
          (at person1 ?x0)))
      (prev_autstate_3_1 ?x0))
)

(:derived 
    (autstate_3_4 ?x0)
    (or
      (prev_autstate_3_3 ?x0)
      (prev_autstate_3_4 ?x0))
)

(:derived 
    (autstate_3_2 ?x0)
    (or
      (and
        (prev_autstate_3_3 ?x0)
        (at person1 ?x0))
      (and
        (prev_autstate_3_4 ?x0)
        (at person1 ?x0)))
)

(:derived 
    (aut_in_final_1)
    (autstate_1_1)
)

(:derived 
    (aut_in_final_2 ?x0)
    (or
      (autstate_2_1 ?x0)
      (autstate_2_2 ?x0))
)

(:derived 
    (aut_in_final_3 ?x0)
    (or
      (autstate_3_1 ?x0)
      (autstate_3_2 ?x0))
)

)