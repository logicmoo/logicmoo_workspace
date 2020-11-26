(define (problem get-paid)              ; graph-plan 2 steps, 3 actions
    (:domain briefcase-world)
  (:objects home office - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:length (:serial 4) (:parallel 4))
  (:goal (and (at B office) (at D office) (at P home))))



;;;UCPOP(33): (bf-control 'get-paid)
;;;
;;;Initial  : ((PLACE HOME) (PLACE OFFICE) (OBJECT P) (OBJECT D) (OBJECT B)
;;;            (AT B HOME) (AT P HOME) (AT D HOME) (IN P))
;;;
;;;Step 1  : (PUT-IN D HOME)        Created 3
;;;           0  -> (AT D HOME)
;;;           0  -> (AT B HOME)
;;;Step 2  : (TAKE-OUT P)           Created 2
;;;Step 3  : (MOV-B HOME OFFICE)    Created 1
;;;           3  -> (IN D)
;;;           0  -> (AT B HOME)
;;;           2  -> (NOT (IN P))
;;;
;;;Goal    : (AND (AT B OFFICE) (AT D OFFICE) (AT P HOME))
;;;           1  -> (AT B OFFICE)
;;;           1  -> (AT D OFFICE)
;;;           0  -> (AT P HOME)
;;;Complete!
;;;
;;;UCPOP (Init = 9  ; Goals = 4 ) => Win  (3 steps)     CPU 134
;;;     Nodes (V = 20  ; Q = 10  ; C = 31  )             Branch 1.5
;;;     Working Unifies: 278                             Bindings added: 37
;;;NIL

