;;; 
;;; We have 2 glasses at N-0-0.  We want to get them to N-1-1 unbroken.
;;; 

(define (problem p3)
    (:domain simple-domain)
  (:objects N-0-0 N-0-1 G1 B1 G2 D1 the-BOX R1)
  (:init (truck-at N-0-0)
	 (at G1 N-0-0) (at B1 N-0-1) (at G2 N-0-0)
	 (dump-at D1 N-0-0) (at the-BOX N-0-0)

	 (glass G1) (glass G2) (box the-BOX) (bomb B1) (g-dumpster D1)
	 (location N-0-0) (location N-0-1)
	 (road R1 N-0-1 N-0-0)
	 (road R1 N-0-0 N-0-1)
	 (bumpy R1)
	 (liftable the-box) (liftable G1) (liftable G2)
	 (not (broken G1)) (not (broken G2)))
;  (:rank-fun truck-rank-func)
  (:goal (and (at G1 N-0-1) (at G2 N-0-1) (not (broken G1)) (not (broken G2)))))


;UCPOP(444): (bf-control 'p3)
;
;Initial  : ((TRUCK-AT N-0-0) (AT G1 N-0-0) (AT B1 N-0-1) (AT G2 N-0-0) (DUMP-AT D1 N-0-0)
;            (AT BOX N-0-0) (GLASS G1) (GLASS G2) (BOX BOX) (BOMB B1)
;            (G-DUMPSTER D1) (LOCATION N-0-0) (LOCATION N-0-1) (ROAD R1 N-0-1 N-0-0)
;            (ROAD R1 N-0-0 N-0-1) (BUMPY R1) (LIFTABLE BOX) (LIFTABLE G1)
;            (LIFTABLE G2) (NOT (BROKEN G1)) (NOT (BROKEN G2)))
;
;Step 1  : (PICKUP G2 N-0-0)         Created 10
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G2)
;           0  -> (AT G2 N-0-0)
;Step 2  : (PUTIN G2 BOX)         Created 9
;           0  -> (BOX BOX)
;           10 -> (HOLDING G2)
;Step 3  : (PICKUP BOX N-0-0)        Created 3
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE BOX)
;           0  -> (AT BOX N-0-0)
;Step 4  : (PICKUP G1 N-0-0)         Created 5
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G1)
;           0  -> (AT G1 N-0-0)
;Step 5  : (PUTIN G1 BOX)         Created 4
;           0  -> (BOX BOX)
;           5  -> (HOLDING G1)
;Step 6  : (TRAVEL N-0-0 N-0-1 R1)      Created 6
;           9  -> (NOT (HOLDING G2))
;           4  -> (NOT (HOLDING G1))
;           0  -> (NOT (FUEL-EMPTY))
;           0  -> (NOT (TOLLED R1))
;           0  -> (ROAD R1 N-0-0 N-0-1)
;           0  -> (LOCATION N-0-1)
;           0  -> (TRUCK-AT N-0-0)
;Step 7  : (TAKEOUT BOX G2 N-0-1)    Created 8
;           6  -> (TRUCK-AT N-0-1)
;           9  -> (IN G2 BOX)
;           0  -> (BOX BOX)
;           3  -> (HOLDING BOX)
;Step 8  : (TAKEOUT BOX G1 N-0-1)    Created 2
;           6  -> (TRUCK-AT N-0-1)
;           4  -> (IN G1 BOX)
;           0  -> (BOX BOX)
;           3  -> (HOLDING BOX)
;Step 9  : (PUTDOWN G2 G2 N-0-1)     Created 7
;           6  -> (TRUCK-AT N-0-1)
;           8  -> (HOLDING G2)
;           0  -> (LIFTABLE G2)
;Step 10 : (PUTDOWN G1 G1 N-0-1)     Created 1
;           6  -> (TRUCK-AT N-0-1)
;           2  -> (HOLDING G1)
;           0  -> (LIFTABLE G1)
;
;Goal    : (AND (AT G1 N-0-1) (AT G2 N-0-1) (NOT (BROKEN G1)) (NOT (BROKEN G2)))
;           0  -> (NOT (BROKEN G2))
;           0  -> (NOT (BROKEN G1))
;           7  -> (AT G2 N-0-1)
;           1  -> (AT G1 N-0-1)
;Facts:
;Complete!
;
;UCPOP Stats: Initial terms = 21;   Goals = 5 ;  Success (10 steps)
;      Created 487 plans, but explored only 338
;      CPU time:    6.4660 sec
;      Branching factor: 37.266
;      Working Unifies: 1160
;      Bindings Added: 276
;#plan<S=11; O=0; U=0; F=0>
;#<unprintable object @ #x1c61436>
