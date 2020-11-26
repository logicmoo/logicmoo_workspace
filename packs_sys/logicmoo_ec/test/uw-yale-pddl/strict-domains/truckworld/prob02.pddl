;;; collection world test case 2
;;;  Now things are a little more complicated:  there are
;;;  still two glasses at N-0-0 along with a broken glass
;;;  consumer, but the only way to dispose of an unbroken glass
;;;  is at N-0-1, and the road between them is bumpy.  Also there is no bomb
;;;  at N-0-0, but there is one at N-0-1.   So there is an obvious best
;;;  plan:  take both of the glasses over the bumpy road with a box and
;;;  dispose of them at N-0-1, but there are many other options that your
;;;  planner might get stuck on.   Notice that there's an additional
;;;  flag to this world indicating whether you want to think about
;;;  paying toll.  So your actual call to start simulator will be
;;;  '(make-test-world-two :toll t) or :toll nil

;;; I use the tolled option (R1 is tolled)


(define (problem p2)
    (:domain simple-domain)
  (:objects N-0-0 N-0-1 G1 B1 G2 D1 the-BOX R1)
  (:init (truck-at N-0-0)
	 (at G1 N-0-0) (at B1 N-0-1) (at G2 N-0-0) (dump-at D1 N-0-1)
	 (at the-BOX N-0-0)

	 (glass G1) (glass G2) (box the-BOX) (bomb B1) (g-dumpster D1)
	 (location N-0-0) (location N-0-1)

	 (road R1 N-0-0 N-0-1) (road R1 N-0-1 N-0-0)
	 (atm N-0-0) (fuel-full)
	 (tolled R1)
	 (bumpy R1)
	 (liftable the-box) (liftable G1) (liftable G2)
	 (not (broken G1)) (not (broken G2)))
;  (:rank-fun truck-rank-func)
  (:goal (and (disposed G1) (disposed G2))))

;UCPOP(443): (bf-control 'p2)
;
;Initial  : ((TRUCK-AT N-0-0) (AT G1 N-0-0) (AT B1 N-0-1) (AT G2 N-0-0) (DUMP-AT D1 N-0-1)
;            (AT BOX N-0-0) (GLASS G1) (GLASS G2) (BOX BOX) (BOMB B1)
;            (G-DUMPSTER D1) (LOCATION N-0-0) (LOCATION N-0-1) (ROAD R1 N-0-0 N-0-1)
;            (ROAD R1 N-0-1 N-0-0) (ATM N-0-0) (FUEL-FULL) (TOLLED R1) (BUMPY R1)
;            (LIFTABLE BOX) (LIFTABLE G1) (LIFTABLE G2) (NOT (BROKEN G1))
;            (NOT (BROKEN G2)))
;
;Step 1  : (PICKUP G2 N-0-0)         Created 6
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G2)
;           0  -> (AT G2 N-0-0)
;Step 2  : (PICKUP G1 N-0-0)         Created 5
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G1)
;           0  -> (AT G1 N-0-0)
;Step 3  : (GET-ALL-MONEY-I-EVER-NEED N-0-0 N-0-0)   Created 4
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (ATM N-0-0)
;Step 4  : (PAY-TOLL YES-TOLL R1)   Created 3
;           4  -> (HAVE-MONEY)
;Step 5  : (TRAVEL N-0-0 N-0-1 R1)      Created 2
;           0  -> (GLASS G2)
;           6  -> (HOLDING G2)
;           0  -> (BUMPY R1)
;           0  -> (GLASS G1)
;           5  -> (HOLDING G1)
;           0  -> (NOT (FUEL-EMPTY))
;           3  -> (NOT (TOLLED R1))
;           0  -> (ROAD R1 N-0-0 N-0-1)
;           0  -> (LOCATION N-0-1)
;           0  -> (TRUCK-AT N-0-0)
;Step 6  : (DISPOSE-ALL-BROKEN-GLASS D1 N-0-1)   Created 1
;           6  -> (HOLDING G2)
;           2  -> (BROKEN G2)
;           2  -> (TRUCK-AT N-0-1)
;           0  -> (G-DUMPSTER D1)
;           0  -> (DUMP-AT D1 N-0-1)
;           5  -> (HOLDING G1)
;           2  -> (BROKEN G1)
;           2  -> (TRUCK-AT N-0-1)
;
;Goal    : (AND (DISPOSED G1) (DISPOSED G2))
;           1  -> (DISPOSED G2)
;           1  -> (DISPOSED G1)
;Facts:
;Complete!
;
;UCPOP Stats: Initial terms = 24;   Goals = 3 ;  Success (6 steps)
;      Created 100 plans, but explored only 62
;      CPU time:    1.5660 sec
;      Branching factor: 10.355
;      Working Unifies: 206
;      Bindings Added: 67
;#plan<S=7; O=0; U=0; F=0>
;#<unprintable object @ #x1a0f3de>
