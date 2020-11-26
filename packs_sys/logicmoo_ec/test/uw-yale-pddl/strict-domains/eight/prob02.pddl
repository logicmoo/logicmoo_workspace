;; - 8 7         1 2 3
;; 6 5 4  --==>> 4 5 6
;; 3 2 1         7 8 -

(define (problem 8-2)
    (:domain eight-puzzle)
  (:init   (at P1 S9)
	   (at P2 S8)
	   (at P3 S7)
	   (at P4 S6)
	   (at P5 S5)
	   (at P6 S4)
	   (at P7 S3)
	   (at P8 S2)
	   (empty S1))
  (:goal (and (solved))))