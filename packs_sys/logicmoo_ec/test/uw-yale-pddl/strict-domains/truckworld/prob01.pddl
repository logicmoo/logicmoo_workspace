;;; collection world test case 1
;;;  Here's a world with only one node, in which there are
;;;  two glasses, a bomb, and a collector for broken glass.  The
;;;  task is to dispose of both the glasses, which is simple:
;;;  you just blow them up.

(define (problem p1)
    (:domain simple-domain)
  (:objects N-0-0 G1 B1 G2 D1)
  (:init (truck-at N-0-0)
	 (at G1 N-0-0) (at B1 N-0-0) (at G2 N-0-0) (dump-at D1 N-0-0)
	 (bomb B1) (glass G1) (glass G2)
	 (g-dumpster D1)
	 (liftable G1)
	 (liftable G2)
	 (location N-0-0)
	 (not (broken G1))
	 (not (broken G2)))
;  (:rank-fun truck-rank-func)
  (:goal (and (disposed G1) (disposed G2))))

;UCPOP(442): (bf-control 'p1)
;
;Initial  : ((TRUCK-AT N-0-0) (AT G1 N-0-0) (AT B1 N-0-0) (AT G2 N-0-0) (DUMP-AT D1 N-0-0)
;            (BOMB B1) (GLASS G1) (GLASS G2) (G-DUMPSTER D1) (LIFTABLE G1)
;            (LIFTABLE G2) (LOCATION N-0-0) (NOT (BROKEN G1)) (NOT (BROKEN G2)))
;
;Step 1  : (SET-BOMB B1 N-0-0)       Created 2
;           0  -> (NOT (BROKEN G2))
;           0  -> (AT G2 N-0-0)
;           0  -> (GLASS G2)
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (BOMB B1)
;           0  -> (AT B1 N-0-0)
;           0  -> (NOT (BROKEN G1))
;           0  -> (AT G1 N-0-0)
;           0  -> (GLASS G1)
;Step 2  : (PICKUP G2 N-0-0)         Created 4
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G2)
;           0  -> (AT G2 N-0-0)
;Step 3  : (PICKUP G1 N-0-0)         Created 3
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G1)
;           0  -> (AT G1 N-0-0)
;Step 4  : (DISPOSE-ALL-BROKEN-GLASS D1 N-0-0)   Created 1
;           4  -> (HOLDING G2)
;           2  -> (BROKEN G2)
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (G-DUMPSTER D1)
;           0  -> (DUMP-AT D1 N-0-0)
;           3  -> (HOLDING G1)
;           2  -> (BROKEN G1)
;           0  -> (TRUCK-AT N-0-0)
;
;Goal    : (AND (DISPOSED G1) (DISPOSED G2))
;           1  -> (DISPOSED G2)
;           1  -> (DISPOSED G1)
;Facts:
;Complete!
;
;UCPOP Stats: Initial terms = 14;   Goals = 3 ;  Success (4 steps)
;      Created 57 plans, but explored only 35
;      CPU time:    0.6000 sec
;      Branching factor:  7.229
;      Working Unifies: 116
;      Bindings Added: 32
;#plan<S=5; O=0; U=0; F=0>
;#<unprintable object @ #x1c926a6>
