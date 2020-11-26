;;; in addition to p3,  we now want to break a glass G3
;;; We can either set a bomb to blow G3 up,  or get it and walk thru the
;;; bumpy road

(define (problem p3a)
    (:domain simple-domain)
  (:objects N-0-0 N-0-1 G1 G2 G3 B1 D1 the-box R1)
  (:init (truck-at N-0-0)
	 (at G1 N-0-0) (at B1 N-0-1) (at G2 N-0-0)
	 (at G3 N-0-1)
	 (dump-at D1 N-0-0) (at the-BOX N-0-0)

	 (glass G3)(glass G1) (glass G2) (box the-BOX) (bomb B1) (g-dumpster D1)
	 (location N-0-0) (location N-0-1)
	 (road R1 N-0-1 N-0-0)
	 (road R1 N-0-0 N-0-1)

	 (bumpy R1)
	 (liftable the-box) (liftable G1) (liftable G2) (liftable G3)
	 (not (broken G1)) (not (broken G2)) (not (broken G3)))
;  (:rank-fun truck-rank-func)
  (:goal (and (at G1 N-0-1) (at G2 N-0-1) (not (broken G1)) (not (broken
								   G2))
	       (broken G3)
	       )))

;UCPOP(17): (bf-control 'p3a)
;
;Initial  : ((TRUCK-AT N-0-0) (AT G1 N-0-0) (AT B1 N-0-1) (AT G2 N-0-0)
;            (AT G3 N-0-1) (DUMP-AT D1 N-0-0) (AT BOX N-0-0) (GLASS G3)
;            (GLASS G1) (GLASS G2) (BOX BOX) (BOMB B1) (G-DUMPSTER D1)
;            (LOCATION N-0-0) (LOCATION N-0-1) (ROAD R1 N-0-1 N-0-0)
;            (ROAD R1 N-0-0 N-0-1) (BUMPY R1) (LIFTABLE BOX) (LIFTABLE G1)
;            (LIFTABLE G2) (LIFTABLE G3) (NOT (BROKEN G1)) (NOT (BROKEN G2))
;            (NOT (BROKEN G3)))
;
;Step 1  : (PICKUP G2 N-0-0)      Created 10
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G2)
;           0  -> (AT G2 N-0-0)
;Step 2  : (PUTIN G2 BOX)         Created 9
;           0  -> (BOX BOX)
;           10 -> (HOLDING G2)
;Step 3  : (PICKUP BOX N-0-0)     Created 3
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE BOX)
;           0  -> (AT BOX N-0-0)
;Step 4  : (PICKUP G1 N-0-0)      Created 5
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G1)
;           0  -> (AT G1 N-0-0)
;Step 5  : (PUTIN G1 BOX)         Created 4
;           0  -> (BOX BOX)
;           5  -> (HOLDING G1)
;Step 6  : (TRAVEL N-0-0 N-0-1 R1)   Created 6
;           0  -> (NOT (HOLDING G3))
;           9  -> (NOT (HOLDING G2))
;           4  -> (NOT (HOLDING G1))
;           0  -> (NOT (FUEL-EMPTY))
;           0  -> (NOT (TOLLED R1))
;           0  -> (ROAD R1 N-0-0 N-0-1)
;           0  -> (LOCATION N-0-1)
;           0  -> (TRUCK-AT N-0-0)
;Step 7  : (SET-BOMB B1 N-0-1)    Created 11
;           6  -> (TRUCK-AT N-0-1)
;           0  -> (BOMB B1)
;           0  -> (AT B1 N-0-1)
;           0  -> (NOT (BROKEN G3))
;           0  -> (AT G3 N-0-1)
;           0  -> (GLASS G3)
;           9  -> (NOT (HOLDING G2))
;           0  -> (NOT (AT G2 N-0-1))
;           4  -> (NOT (HOLDING G1))
;           0  -> (NOT (AT G1 N-0-1))
;Step 8  : (TAKEOUT BOX G2 N-0-1)   Created 8
;           6  -> (TRUCK-AT N-0-1)
;           9  -> (IN G2 BOX)
;           0  -> (BOX BOX)
;           3  -> (HOLDING BOX)
;Step 9  : (TAKEOUT BOX G1 N-0-1)   Created 2
;           6  -> (TRUCK-AT N-0-1)
;           4  -> (IN G1 BOX)
;           0  -> (BOX BOX)
;           3  -> (HOLDING BOX)
;Step 10 : (PUTDOWN G2 G2 N-0-1)   Created 7
;           6  -> (TRUCK-AT N-0-1)
;           8  -> (HOLDING G2)
;           0  -> (LIFTABLE G2)
;Step 11 : (PUTDOWN G1 G1 N-0-1)   Created 1
;           6  -> (TRUCK-AT N-0-1)
;           2  -> (HOLDING G1)
;           0  -> (LIFTABLE G1)
;
;Goal    : (AND (AT G1 N-0-1) (AT G2 N-0-1) (NOT (BROKEN G1)) (NOT (BROKEN G2))
;           (BROKEN G3))
;           11 -> (BROKEN G3)
;           0  -> (NOT (BROKEN G2))
;           0  -> (NOT (BROKEN G1))
;           7  -> (AT G2 N-0-1)
;           1  -> (AT G1 N-0-1)
;Facts:
;Complete!
;
;UCPOP Stats: Initial terms = 25;   Goals = 6 ;  Success (11 steps)
;      Created 1533 plans, but explored only 1003
;      CPU time:    8.1670 sec
;      Branching factor: 79.622
;      Working Unifies: 5585
;      Bindings Added: 891
;#plan<S=12; O=0; U=0; F=0>
;#Stats:<cpu time = 8.1670>
