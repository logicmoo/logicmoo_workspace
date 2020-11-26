(define (problem sussman-anomaly)       ; graphplan 3 steps
    (:domain blocks-world-domain)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on C A) (on A Table) (on B Table)
	 (clear C) (clear B) (clear Table))
  (:goal (and (on B C) (on A B)))
  (:length (:serial 3) (:parallel 3)))



;;;UCPOP(22): (bf-control 'sussman-anomaly)
;;;
;;;Initial  : ((BLOCK A) (BLOCK B) (BLOCK C) (BLOCK TABLE) (ON C A) (ON A TABLE)
;;;            (ON B TABLE) (CLEAR C) (CLEAR B) (CLEAR TABLE))
;;;
;;;Step 1  : (PUTON C TABLE A)      Created 2
;;;           0  -> (ON C A)
;;;           0  -> (CLEAR C)
;;;           0  -> (CLEAR TABLE)
;;;Step 2  : (PUTON B C TABLE)      Created 3
;;;           0  -> (ON B TABLE)
;;;           0  -> (CLEAR B)
;;;           0  -> (CLEAR C)
;;;Step 3  : (PUTON A B TABLE)      Created 1
;;;           0  -> (ON A TABLE)
;;;           2  -> (CLEAR A)
;;;           0  -> (CLEAR B)
;;;
;;;Goal    : (AND (ON B C) (ON A B))
;;;           3  -> (ON B C)
;;;           1  -> (ON A B)
;;;Complete!
;;;
;;;UCPOP (Init = 10 ; Goals = 3 ) => Win  (3 steps)     CPU 283
;;;     Nodes (V = 51  ; Q = 25  ; C = 82  )             Branch 1.4901961
;;;     Working Unifies: 481                             Bindings added: 202
;;;NIL

