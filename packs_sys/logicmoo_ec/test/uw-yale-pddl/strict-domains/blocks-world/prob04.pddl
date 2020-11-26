(define (problem mcd-sussman-anomaly)
    (:domain mcd-blocksworld)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on c a) (on b table) (on a table))
  (:goal (and (on b c) (on a b))))



;;;UCPOP(41): (bf-control 'mcd-sussman-anomaly)
;;;
;;;Initial  : ((BLOCK A) (BLOCK B) (BLOCK C) (BLOCK TABLE) (ON C A) (ON B TABLE)
;;;            (ON A TABLE))
;;;
;;;Step 1  : (PUTON C TABLE A)      Created 2
;;;           0  -> (ON C A)
;;;           0  -> (NOT (ON TABLE C))
;;;           0  -> (NOT (ON C C))
;;;           0  -> (NOT (ON B C))
;;;           0  -> (NOT (ON A C))
;;;Step 2  : (PUTON B C TABLE)      Created 3
;;;           0  -> (ON B TABLE)
;;;           0  -> (NOT (ON TABLE B))
;;;           0  -> (NOT (ON C B))
;;;           0  -> (NOT (ON B B))
;;;           0  -> (NOT (ON A B))
;;;           0  -> (NOT (ON TABLE C))
;;;           0  -> (NOT (ON C C))
;;;           0  -> (NOT (ON B C))
;;;           0  -> (NOT (ON A C))
;;;Step 3  : (PUTON A B TABLE)      Created 1
;;;           0  -> (ON A TABLE)
;;;           0  -> (NOT (ON TABLE A))
;;;           2  -> (NOT (ON C A))
;;;           0  -> (NOT (ON B A))
;;;           0  -> (NOT (ON A A))
;;;           0  -> (NOT (ON TABLE B))
;;;           0  -> (NOT (ON C B))
;;;           0  -> (NOT (ON B B))
;;;           0  -> (NOT (ON A B))
;;;
;;;Goal    : (AND (ON B C) (ON A B))
;;;           3  -> (ON B C)
;;;           1  -> (ON A B)
;;;Complete!
;;;
;;;UCPOP (Init = 7  ; Goals = 3 ) => Win  (3 steps)     CPU 400
;;;     Nodes (V = 54  ; Q = 25  ; C = 101 )             Branch 1.462963
;;;     Working Unifies: 976                             Bindings added: 163
;;;NIL

