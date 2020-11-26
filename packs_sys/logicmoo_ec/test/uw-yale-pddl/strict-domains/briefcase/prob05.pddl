(define (problem uget-paid)
    (:domain uni-bw)
  (:objects P D home office)
  (:init (at B home) (at P home) (at D home) (in P) )
  (:goal (and (at B office) (at D office) (at P home)))
  (:length (:serial 3) (:parallel 3)))


;;;UCPOP(42): (bf-control 'uget-paid)
;;;
;;;Initial  : ((AT B HOME) (AT P HOME) (AT D HOME) (IN P))
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
;;;UCPOP (Init = 5  ; Goals = 4 ) => Win  (3 steps)     CPU 150
;;;     Nodes (V = 23  ; Q = 11  ; C = 44  )             Branch 1.4782609
;;;     Working Unifies: 254                             Bindings added: 65
;;;NIL

