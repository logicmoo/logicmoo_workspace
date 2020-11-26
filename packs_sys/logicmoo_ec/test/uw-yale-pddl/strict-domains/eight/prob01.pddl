;; 1 2 3         1 2 3
;; 4 5 6  --==>> 4 5 6
;; 7 - 9         7 8 -

(define (problem 8-1)
    (:domain eight-puzzle)
  (:init   (at P1 S1)
	   (at P2 S2)
	   (at P3 S3)
	   (at P4 S4)
	   (at P5 S5)
	   (at P6 S6)
	   (at P7 S7)
	   (at P8 S9)
	   (empty S8))
  (:goal (and (solved))))