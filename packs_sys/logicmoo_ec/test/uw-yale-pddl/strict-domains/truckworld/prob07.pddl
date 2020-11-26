;;; 
;;; part of the fourth test case of Steve's world.
;;; this doesn't use abstract operators
;;; this doesn't work with limit = 2000; perhaps 10000 or something will do
;;; but don't bother :-)
;;; 
(define (problem p5)
    (:domain simple-domain)
  (:objects N-0-0 N-0-1 N-0-2 N-1-0 N-1-1 N-1-2 N-2-0 N-2-1 N-2-2
	    R1 R2 R3 R4 R5 R6 R7 R8 R9
	    G1 G2 G3
	    D1 the-box)
  (:init (truck-at N-0-0)
	 (at G1 N-1-2) (at G2 N-2-2) (dump-at D1 N-1-1) (at the-BOX N-0-0)
	 (at G3 N-0-2)

	 (garbage G1) (garbage G2) (garbage G3)
	 (box the-BOX) (dumpster D1)
	 (location N-0-0) (location N-0-1) (location N-1-1) (location N-1-0)
	 (location N-0-2) (location N-2-0) (location N-1-2) (location N-2-1)
	 (location N-2-2)
	 (fuel-full)
	 (road R1 N-0-0 N-0-1) (road R1 N-0-1 N-0-0)
	 (road R2 N-0-1 N-0-2) (road R2 N-0-2 N-0-1)
	 (road R3 N-0-1 N-1-1) (road R3 N-1-1 N-0-1)
	 (road R4 N-1-0 N-1-1) (road R4 N-1-1 N-1-0)
	 (road R5 N-1-1 N-2-1) (road R5 N-2-1 N-1-1)
	 (road R6 N-1-2 N-2-2) (road R6 N-2-2 N-1-2)
	 (road R7 N-2-0 N-2-1) (road R7 N-2-1 N-2-0)
	 (road R8 N-2-1 N-2-2) (road R8 N-2-2 N-2-1)
	 (road R9 N-0-2 N-1-2) (road R9 N-1-2 N-0-2)

	 (liftable G1) (liftable G2)
	 (liftable G3))

;  (:rank-fun truck-rank-func-3)
  (:goal (and (garbage-disposed G1)
		(garbage-disposed G2)
		(garbage-disposed G3)
		))
    )