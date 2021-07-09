(define (domain power)
 (:requirements :typing :durative-actions :fluents :time 
                :negative-preconditions :timed-initial-literals)
 (:types equipment)
 (:constants unit - equipment)
 (:predicates (day) (commsOpen) (readyForObs1) (readyForObs2) 
                    (gotObs1) (gotObs2)
                    (available ?e - equipment))
 (:functions (demand) (supply) (soc) (charge-rate) (daytime) 
             (heater-rate) (dusk) (dawn)
             (fullTime) (partTime1) (partTime2) 
             (obs1Time) (obs2Time) (obs1-rate) (obs2-rate) 
             (A-rate) (B-rate) (C-rate) (D-rate) (safeLevel) 
             (solar-const))


(:process charging
 :parameters ()
 :precondition (and (< (demand) (supply)) (day)) 
 :effect (and (increase (soc) (* #t (* (* (- (supply) (demand)) 
                                          (charge-rate))
                                       (- 100 (soc)))
                                  )))
)

(:process discharging
 :parameters ()
 :precondition (> (demand) (supply))
 :effect (decrease soc (* #t (- (demand) (supply))))
)

(:process generating
 :parameters ()
 :precondition (day)
 :effect (and (increase (supply) 
                    (* #t (* (* (solar-const) (daytime))
                             (+ (* (daytime) 
                                   (- (* 4 (daytime)) 90)) 450))))
              (increase (daytime) (* #t 1)))
)

(:process night-operations
 :parameters ()
 :precondition (not (day))
 :effect (and (increase (daytime) (* #t 1))
              (decrease (soc) (* #t (heater-rate))))
)

(:event nightfall
 :parameters ()
 :precondition (and (day) (>= (daytime) (dusk)))
 :effect (and (assign (daytime) (- (dawn)))
              (not (day)))
)

(:event daybreak
 :parameters ()
 :precondition (and (not (day)) (>= (daytime) 0))
 :effect (day)
)

(:durative-action fullPrepare
 :parameters ()
 :duration (= ?duration (fullTime))
 :condition (and (at start (available unit))
                 (over all (> (soc) (safelevel))))
 :effect (and (at start (not (available unit)))
              (at start (increase (demand) (A-rate)))
              (at end (available unit))
              (at end (decrease (demand) (A-rate)))
              (at end (readyForObs1))
              (at end (readyForObs2)))
)

(:durative-action prepareObs1
 :parameters ()
 :duration (= ?duration (partTime1))
 :condition (and (at start (available unit))
                 (over all (> (soc) (safelevel))))
 :effect (and (at start (not (available unit)))
              (at start (increase (demand) (B-rate)))
              (at end (available unit))
              (at end (decrease (demand) (B-rate)))
              (at end (readyForObs1)))
)

(:durative-action prepareObs2
 :parameters ()
 :duration (= ?duration (partTime2))
 :condition (and (at start (available unit))
                 (over all (> (soc) (safelevel))))
 :effect (and (at start (not (available unit)))
              (at start (increase (demand) (C-rate)))
              (at end (available unit))
              (at end (decrease (demand) (C-rate)))
              (at end (readyForObs2)))
)

(:durative-action observe1
 :parameters ()
 :duration (= ?duration (obs1Time))
 :condition (and (at start (available unit))
                 (at start (readyForObs1))
                 (over all (> (soc) (safelevel)))
                 (over all (not (commsOpen))))
 :effect (and (at start (not (available unit)))
              (at start (increase (demand) (obs1-rate)))
              (at end (available unit))
              (at end (decrease (demand) (obs1-rate)))
              (at end (not (readyForObs1)))
              (at end (gotObs1)))
)

(:durative-action observe2
 :parameters ()
 :duration (= ?duration (obs2Time))
 :condition (and (at start (available unit))
                 (at start (readyForObs2))
                 (over all (> (soc) (safelevel)))
                 (over all (not (commsOpen))))
 :effect (and (at start (not (available unit)))
              (at start (increase (demand) (obs2-rate)))
              (at end (available unit))
              (at end (decrease (demand) (obs2-rate)))
              (at end (not (readyForObs2)))
              (at end (gotObs2)))
)
)



(define
    (domain ballphysics)
    (:requirements :time :typing)
    (:types
        ball - object
    )
    (:predicates
        (held ?b - ball)
    )
    (:functions
        (velocity ?b - ball)
        (distance-to-floor ?b - ball)
    )
    (:process FALLING
        :parameters (?b - ball)
        :precondition (and
            (not (held ?b))
            (< (velocity ?b) 100)
        )
        :effect (and
            (increase (velocity ?b) (* #t 9.8))
            (decrease (distance-to-floor ?b) (* #t (velocity ?b)))
        )
    )
    (:event HIT-GROUND
        :parameters (?b - ball)
        :precondition (and
            (not (held ?b))
            (<= (distance-to-floor ?b) 0)
            (> (velocity ?b) 0)
        )
        :effect (
            (assign (velocity ?b) (* -0.8 (velocity ?b)))
        )
    )

    ; Actions omitted
)
