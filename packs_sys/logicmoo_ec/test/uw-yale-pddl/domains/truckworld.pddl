; (c) 1993,1994,1995 Copyright (c) University of Washington
;  Written by Chung Kwok.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Truckworld domain
;;;   Hanks' truckworld simulation world (simplfied)
;;;   Also demonstrates some search control written in lisp/ranking function,
;;;   not scr.
;;; 
;;;   I've put some effort into reducing number of open conditions
;;;   by using some implicit axioms (not ucpop's axioms)
;;;   For example,  (holding ?x) means (liftable ?x) [not the other way though]

;;; I've tried to minimize the # of simplifying assumptions.
;;; The only ones are:
;;; medium truck speed (see comments in travel operator)
;;; get money at most once
;;;   I don't think this is simplifying at all though.  Think of the
;;;   truck which will look at the plan (which is the purpose of planning
;;;   really),  and it can decide how much $$ it needs.  It will not,
;;;   because of this relaxation,  do stupid things like spending money all
;;;   over the place.  This is because if you spend money you'll get more
;;;   steps and thus the plan is not as good as others.  Moreover,  my
;;;   truck does not get any money if it doesn't need any.
;;; get fuel from dispenser at most once
;;;   Similar reasoning as money.  I think it suffice to ask the truck to
;;;   refuel itself if needed.
;;; unlimited hand capacity
;;;   UCPOP doesn't handle quantity in any reasonably easy way, just look
;;;   at how messy it is for fuel.  I think it's a responsibility of the
;;;   execution system to do tasks like dispose of the fuel drum.  Also I
;;;   think these details should be part of a hierchical system and
;;;   abstract out.
;;; 

(define (domain simple-domain)
  (:requirements :adl :typing)

  (:predicates (bomb ?b)
	       (truck-at ?n)
	       (at ?b ?n)
	       (bomb-at ?b ?n)
	       (glass ?g)
	       (holding ?o)
	       (broken ?g)
	       (liftable ?o)
	       (tolled ?r)
	       (fuel-empty)
	       (location ?n)
	       (road ?r ?n1 ?n2)
	       (bumpy ?r)
	       (fuel-full)
	       (fuel-half)
	       (yes-toll ?r)
	       (dump-at ?d ?n)
	       (g-dumpster ?d)
	       (disposed ?g)
	       (garbage ?g)
	       (garbage-disposed ?g)
	       (box ?b)
	       (in ?o ?b)
	       (atm ?n)
	       (fuel-dispenser ?n)
	       (have-fuel)
	       (have-money)
	       (dumpster ?d)
	       )

  ;; (truck-at ?n) => (location ?n)
  (:action set-bomb
	     :parameters (?b ?n)
	     :precondition (and (bomb ?b) (truck-at ?n) (at ?b ?n))
	     :effect (and
		      (not (bomb-at ?b ?n))
		      (forall (?g) (when (and (glass ?g)
					      (or (at ?g ?n)
						  (holding ?g))
					      (not (broken ?g)))
				     (broken ?g)))
		      ))

;;; both p1 and p2 explores more plans with this op
;   (:action pickup-all
;      :parameters ((truck-at ?n))
;      :precondition ()
;      :effect (forall (?o)
;		      (when (and (liftable ?o) (at ?o ?n))
;			(and (not (at ?o ?n)) (holding ?o)))))

  (:action pickup
	     :parameters (?o ?n)
	     :precondition (and (liftable ?o) (truck-at ?n) (at ?o ?n))
	     :effect (and (not (at ?o ?n)) (holding ?o)))

  (:action putdown
            ;; :parameters ((liftable ?o) (holding ?o) (truck-at ?n))
            :parameters (?o ?n)
	    :precondition (and (holding ?o) (truck-at ?n))
	    :effect (and (at ?o ?n) (not (holding ?o))))

  ;;
  ;; road N-0-0 N-0-1 => neq location N-0-0 and location N-0-1
  ;; travel considers both toll and fuel
  ;; to not get bogged to tedious planning but also to show fuel
  ;; consumption works,  I choose to walk with medium speed everytime.
  ;; I think it make no sense to ask ucpop to choose truck's speed;
  ;; it'll just get confused and does nothing.  Just look at the number
  ;; of conditions we have here.
  ;;
  (:action travel
	     :parameters (?N-0-0 ?N-0-1 ?r)
	     :precondition (and (not (tolled ?r))
				(not (fuel-empty)))
	     :effect (and (not (truck-at ?N-0-0))
			  ;; this solves ucpop's strange precondition ordering
			  (when (and (truck-at ?N-0-0) (location ?N-0-1) (road ?r ?N-0-0 ?N-0-1))
			    (truck-at ?N-0-1))
			  (when (bumpy ?r)
			    (forall (?g)
				    (when (and (holding ?g) (glass ?g))
				      (broken ?g))))
			  (when (fuel-full)
			    (and (fuel-half) (not (fuel-full)) (not (fuel-empty))))
			  (when (fuel-half)
			    (and (not (fuel-half)) (fuel-empty) (not (fuel-full))))
			  ))

  ;; this just pay the toll
  (:action pay-toll
	     :parameters (?r)
	     :precondition (and (have-money) (yes-toll ?r))
	     :effect (not (tolled ?r)))

  ;;broken => glass
  (:action dispose-all-broken-glass
	    :parameters (?d ?n)
	    :precondition (and (dump-at ?d ?n) (g-dumpster ?d) (truck-at ?n))
	    :effect (forall (?g)
			    (when (and (truck-at ?n) (broken ?g) (holding ?g))
			      (and (not (holding ?g))
				   (disposed ?g)))))

  (:action dispose-all-garbage
	     :parameters (?d ?n)
	     :precondition (and (dump-at ?d ?n) (dumpster ?d) (truck-at ?n))
	     :effect (forall (?g)
			     (when (and (garbage ?g) (holding ?g))
			       (and (not (holding ?g))
				    (garbage-disposed ?g)))))

  ;;; holding => liftable
  (:action putin
	     :parameters (?o ?b)
	     :precondition (and (holding ?o) (box ?b))
	     :effect (and (not (holding ?o))
			  (in ?o ?b)))

  (:action takeout
	     :parameters (?b ?o ?n)
	     :precondition (and (in ?o ?b) (or (holding ?b) (at ?b ?n))
				(box ?b) (truck-at ?n))
	     :effect (and (not (in ?o ?b))
			  (holding ?o)))

    ;;;we're only going to get-money at most once
    ;;;because at execution time,  we know how many money we need to get
    ;;;given the plan
    (:action get-all-money-I-ever-need
	       :parameters (?n)
	       :precondition (and (truck-at ?n) (atm ?n))
	       :effect (have-money))

    ;;;we're only going to get-fuel at most once
    (:action get-all-fuel-I-ever-need
	       :parameters (?n)
	       :precondition  (and (have-money) (truck-at ?n) (fuel-dispenser ?n))
	       :effect (have-fuel))

    ;;;but we refuel if we run out.
    (:action refuel
	      :parameters ()
	      :precondition (have-fuel)
	      :effect (and (fuel-full) (not (fuel-empty))))
    )


;;;;;;;;;;;;; SEARCH CONTROL
;;; 
;;; weird links that we should not consider
;;; e.g. should not use pickup to solve putdown IMMEDIATELY and vice versa
;;; 
;(defvar *opp-actions*
;    '((putin . pickup) (putdown . takeout) (putdown . pickup) (pickup . putdown)))

;(defun step-with-id (id plan)
;  (find id (plan-steps plan) :key #'p-step-id))

;(defun action-pred (id plan)
;  (car (p-step-action (find id (plan-steps plan)
;			    :key #'p-step-id))))

;;; This rule rejects all links that links are unrelated (but predicates
;;; match)

;(defun strange-seq (plan seq)
;  ;; we only need to check the first (which is last being put) link
;  (let* ((l (car (plan-links plan)))
;	 (id1 (link-id1 l))
;	 (id2 (link-id2 l)))
;    (if (and (numberp id1)
;	     (numberp id2)
;	     (= (abs (- id1 id2)) 1)
;	     (member (cons (action-pred id1 plan)
;			   (action-pred id2 plan))
;		     seq
;		     :test #'equal))
;	t nil)))

;;; simple ranking function for problem 1 and 2
;(defun truck-rank-func (plan)
;  (+ (length (plan-steps plan))
;     (length (plan-flaws plan))
;     (if (or (strange-seq plan *opp-actions*) (recursive-seq plan)) 999999 0)
;     ))

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


;;; remove all putdown->pickup->putdown... plans
;;; These recursive steps has to unify absolutely to be wrong (And rejected)
;;; This ensures less incompleteness.

;(defun recursive-seq (plan)
;  (let ((o1 (car (plan-ordering plan)))
;	(l (cdr (plan-ordering plan))))
;    (dolist (o2 l nil)
;      (when (= (car o2) (cadr o1))
;	(let* ((a1 (p-step-action (find (cadr o2) (plan-steps plan)
;					:key #'p-step-id)))
;	       (a2 (p-step-action (find (car o1) (plan-steps plan)
;					:key #'p-step-id))))
;	  (when (equal (unify a1 a2 (plan-bindings plan)) '(nil))
;	    (return-from recursive-seq t)))))))

;;; not used: do linking first for truck-at
;(defun truck-at-link-first (plan)
;  (let ((reason (plan-reason plan)))
;    (when (and (eq (car reason) :step)
;	     (eq (car (caddr reason)) 'truck-at))
;	t)))

;;; not used: do linking first
;(defun link-first (plan)
;  (let ((reason (plan-reason plan)))
;    (when (and (eq (car reason) :step)
;	     (eq (car (caddr reason)) 'truck-at))
;	t)))

;;; do what it says
;(defun dont-put-garbage-in-box (plan)
;  (let ((reason (plan-reason plan)))
;    (when (and (eq (car reason) :step)
;	       (eq (car (caddr reason)) 'holding)
;	       (eq (action-pred (cadr reason) plan) 'takeout))
;	t)))


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

;(defun delay-travel (plan)
;  (let ((ret 0))
;    (dolist (s (plan-steps plan) ret)
;      (when (eq (car (p-step-action s)) 'travel)
;	  (incf ret)))))

;;; assume only travel can produce truck-at
;;; don't walk around in loops
;(defun dont-look-back (plan)
;  (let* ((travelled
;	  (mapcar #'p-step-action
;		  (remove-if-not #'(lambda (s)
;				     (eq (car (p-step-action s))
;					 'travel))
;				 (cdr (plan-steps plan)))))
;	 (visited (bind-variable (mapcar #'cadr travelled)
;				 (plan-bindings plan))))
;    (when (/= (length (remove-duplicates visited))
;	      (length visited))
;      t)))

;;; do not solve a fuel goal by making a new step
;(defun dont-use-step-for-fuel (plan)
;  (let ((reason (plan-reason plan)))
;    (when (and (eq (car reason) :step)
;	       (or (member (car (caddr reason)) '(fuel-full fuel-half
;						  fuel-empty))
;		   (and (eq (car (caddr reason)) 'not)
;			(member (caadr (caddr reason)) '(fuel-full fuel-half
;						       fuel-empty)))))
;      t)))

;;; returns the initial conditions of the plan
;(defun initial-conditions (plan)
;  (effect-add (car (p-step-add (find 0 (plan-steps plan) :key #'p-step-id)))))

;;; for some reasons the truck wants to pay,  but find out much much later
;;; it's a bad idea.
;(defun dont-pay-if-not-needed (plan)
;  (let ((reason (plan-reason plan)))
;    (when (and (eq (car reason) :step)
;	       (let ((action (p-step-action (car (plan-steps plan)))))
;		 (and (eq (car action) 'pay-toll)
;		      (not (member
;			    (bind-variable (cdr action)
;					   (plan-bindings plan))
;			    (initial-conditions plan) :test #'equal)))))
;      t)))

;(defun plan-reason (plan) (cdr (assoc :reason (plan-other plan))))

;;; try not to make close world assumption to solve fuel goals
;;; link to steps instead
;(defun use-prev-step-for-fuel (plan)
;  (let ((reason (plan-reason plan)))
;    (when (and (eq (car reason) :cw-assumption)
;	       (equal (link-condition (car (plan-links plan)))
;		      '(not (fuel-empty))))
;      t)))

;;; not used:  link to a real step instead of linking to step 0 for
;;; solving truck-at goals
;(defun use-prev-step-for-travel (plan)
;  (let ((reason (plan-reason plan)))
;    (when (and (eq (car reason) :link)
;	       (eq (car (caddr reason)) 'truck-at)
;	       (= (cadr reason) 0)
;	       (not (member (bind-variable (caddr reason) (plan-bindings plan))
;			    (initial-conditions plan)
;			    :test #'equal)))
;      (print "HERE")
;      t)))
;;; 
;;; more sophiscated ranking function for p4
;;; this forces the truck to pickup stuff before it moves,  thus
;;; reduces the plans generated by at least half.
;;; 
;(defun truck-rank-func-2 (plan)
;  (+
;   (length (plan-steps plan))
;   (length (plan-flaws plan))
;   (if (use-prev-step-for-fuel plan) 1 0)
;   (if (or (dont-pay-if-not-needed plan) (dont-use-step-for-fuel plan)
;	   (dont-look-back plan)
;	   (strange-seq plan (cons '(travel . pickup) *opp-actions*))
;	   (recursive-seq plan)
;	   (dont-put-garbage-in-box plan)) 999999 0)
;   ))


;;; collection world test case 3

;;;  Here is a straightforward transport task:  take six pieces of garbage
;;;  across four nodes, taking into account tolls and fuel consumption.
;;;  There is a fuel dispenser at the first and third node, and ATMs at the first
;;;  and second, so you have to carry money with you to traverse the
;;;  last road.

(define (problem p4)
    (:domain simple-domain)
  (:objects N-0-0 N-0-1 N-0-2 N-0-3 
	    G1 G2 G3 G4 G5 G6
	    R1 R2 R3
	    the-box D1)
  (:init (truck-at N-0-0)
	 (at G1 N-0-0) (at G2 N-0-0) (dump-at D1 N-0-3) (at the-BOX N-0-0)
	 (at G3 N-0-0) (at G4 N-0-0) (at G5 N-0-0) (at G6 N-0-0)

	 (garbage G1) (garbage G2) (garbage G3)
	 (garbage G4) (garbage G5) (garbage G6)
	 (box the-BOX) (dumpster D1)
	 (location N-0-0) (location N-0-1) (location N-0-2) (location N-0-3)

	 (road R1 N-0-0 N-0-1) (road R1 N-0-1 N-0-0)
	 (road R2 N-0-1 N-0-2) (road R2 N-0-2 N-0-1)
	 (road R3 N-0-2 N-0-3) (road R3 N-0-3 N-0-2)

	 (atm N-0-0) (atm N-0-1) (fuel-dispenser N-0-0) (fuel-full)
	 (tolled R1) (tolled R2) (yes-toll R1) (yes-toll R2)

	 (liftable G1) (liftable G2)
	 (liftable G3) (liftable G4) (liftable G5) (liftable G6))

;  (:rank-fun truck-rank-func-2)
  (:goal (and (garbage-disposed G1) (garbage-disposed G2)
	      (garbage-disposed G3)
	      (garbage-disposed G4)
	      (garbage-disposed G5)(garbage-disposed G6)
	      )))

;;; This doesn't force the truck to pickup before it moves.
;(defun truck-rank-func-2a (plan)
;  (+
;   (length (plan-steps plan))
;   (length (plan-flaws plan))
;   (if (use-prev-step-for-fuel plan) 1 0)
;   (if (or (dont-pay-if-not-needed plan) (dont-use-step-for-fuel plan)
;	   (dont-look-back plan)
;	   (strange-seq plan *opp-actions*)
;	   (recursive-seq plan)
;	   (dont-put-garbage-in-box plan)) 999999 0)
;   ))

;;; similar to p4,  but the garbage are scattered all over the place,
;;; and the fuel dispenser is at N-0-1 instead of N-0-0,  slightly more
;;; difficult
(define (problem p4a)
    (:domain simple-domain)
  (:objects N-0-0 N-0-1 N-0-2 N-0-3
	    G1 G2 G3 G4 G5 G6
	    R1 R2 R3
	    the-box D1)
  (:init (truck-at N-0-0)
	 (at G1 N-0-0) (at G2 N-0-2) (dump-at D1 N-0-3) (at the-BOX N-0-0)
	 (at G3 N-0-0) (at G4 N-0-1) (at G5 N-0-2) (at G6 N-0-1)

	 (garbage G1) (garbage G2) (garbage G3)
	 (garbage G4) (garbage G5) (garbage G6)
	 (box the-BOX) (dumpster D1)
	 (location N-0-0) (location N-0-1) (location N-0-2) (location N-0-3)

	 (road R1 N-0-0 N-0-1) (road R1 N-0-1 N-0-0)
	 (road R2 N-0-1 N-0-2) (road R2 N-0-2 N-0-1)
	 (road R3 N-0-2 N-0-3) (road R3 N-0-3 N-0-2)

	 (atm N-0-0) (atm N-0-1) (fuel-dispenser N-0-1) (fuel-full)
	 (tolled R1) (tolled R2) (yes-toll R1)
	 (yes-toll R2)
	 (liftable G1) (liftable G2)
	 (liftable G3) (liftable G4) (liftable G5) (liftable G6))

;  (:rank-fun truck-rank-func-2a)
  (:goal (and (garbage-disposed G1) (garbage-disposed G2)
 	       (garbage-disposed G3)
 	       (garbage-disposed G4)
 	       (garbage-disposed G5)(garbage-disposed G6)
	       ))
)


;;; 
;;; I toyed with the idea of abstract operators,  aka hierchical planning,
;;; but there are too many problems to be resolved,  but I'm sure it can
;;; solve the 4th world in a snap,  because it doesn't think about low
;;; level details like going thru which road (I use routes: abstract roads
;;; instead).
;;; Anyway just something to think about.
;;; 
;(defun truck-rank-func-3 (plan)
;    (+
;     (length (plan-steps plan))
;     (length (plan-flaws plan))
;     (if (or (dont-look-back plan)
;	     (strange-seq plan *opp-actions*)
;	     (recursive-seq plan)
;	     (dont-put-garbage-in-box plan)) 999999 0)
;     ))

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


;;; p1-p4 runs:
;
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
;UCPOP(445): (bf-control 'p4)
;
;Initial  : ((TRUCK-AT N-0-0) (AT G1 N-0-0) (AT G2 N-0-0) (DUMP-AT D1 N-0-3) (AT BOX N-0-0)
;            (AT G3 N-0-0) (AT G4 N-0-0) (AT G5 N-0-0) (AT G6 N-0-0) (GARBAGE G1)
;            (GARBAGE G2) (GARBAGE G3) (GARBAGE G4) (GARBAGE G5) (GARBAGE G6)
;            (BOX BOX) (DUMPSTER D1) (LOCATION N-0-0) (LOCATION N-0-1) (LOCATION N-0-2)
;            (LOCATION N-0-3) (ROAD R1 N-0-0 N-0-1) (ROAD R1 N-0-1 N-0-0) (ROAD R2 N-0-1 N-0-2)
;            (ROAD R2 N-0-2 N-0-1) (ROAD R3 N-0-2 N-0-3) (ROAD R3 N-0-3 N-0-2) (ATM N-0-0) (ATM N-0-1)
;            (FUEL-DISPENSER N-0-0) (FUEL-FULL) (TOLLED R1) (TOLLED R2)
;            (YES-TOLL R1) (YES-TOLL R2) (LIFTABLE G1) (LIFTABLE G2)
;            (LIFTABLE G3) (LIFTABLE G4) (LIFTABLE G5) (LIFTABLE G6))
;
;Step 1  : (PICKUP G6 N-0-0)         Created 15
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G6)
;           0  -> (AT G6 N-0-0)
;Step 2  : (PICKUP G5 N-0-0)         Created 14
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G5)
;           0  -> (AT G5 N-0-0)
;Step 3  : (PICKUP G4 N-0-0)         Created 13
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G4)
;           0  -> (AT G4 N-0-0)
;Step 4  : (PICKUP G3 N-0-0)         Created 12
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G3)
;           0  -> (AT G3 N-0-0)
;Step 5  : (PICKUP G2 N-0-0)         Created 11
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G2)
;           0  -> (AT G2 N-0-0)
;Step 6  : (GET-ALL-MONEY-I-EVER-NEED N-0-0 N-0-0)   Created 7
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (ATM N-0-0)
;Step 7  : (GET-ALL-FUEL-I-EVER-NEED N-0-0 N-0-0)   Created 10
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (FUEL-DISPENSER N-0-0)
;           7  -> (HAVE-MONEY)
;Step 8  : (PAY-TOLL YES-TOLL R2)   Created 8
;           7  -> (HAVE-MONEY)
;Step 9  : (PAY-TOLL YES-TOLL R1)   Created 6
;           7  -> (HAVE-MONEY)
;Step 10 : (PICKUP G1 N-0-0)         Created 2
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G1)
;           0  -> (AT G1 N-0-0)
;Step 11 : (TRAVEL N-0-0 N-0-1 R1)      Created 5
;           0  -> (FUEL-FULL)
;           0  -> (NOT (FUEL-EMPTY))
;           6  -> (NOT (TOLLED R1))
;           0  -> (ROAD R1 N-0-0 N-0-1)
;           0  -> (LOCATION N-0-1)
;           0  -> (TRUCK-AT N-0-0)
;Step 12 : (TRAVEL N-0-1 N-0-2 R2)      Created 4
;           5  -> (NOT (FUEL-EMPTY))
;           8  -> (NOT (TOLLED R2))
;           0  -> (ROAD R2 N-0-1 N-0-2)
;           0  -> (LOCATION N-0-2)
;           5  -> (TRUCK-AT N-0-1)
;Step 13 : (REFUEL)               Created 9
;           10 -> (HAVE-FUEL)
;Step 14 : (TRAVEL N-0-2 N-0-3 R3)      Created 3
;           9  -> (NOT (FUEL-EMPTY))
;           0  -> (NOT (TOLLED R3))
;           0  -> (ROAD R3 N-0-2 N-0-3)
;           0  -> (LOCATION N-0-3)
;           4  -> (TRUCK-AT N-0-2)
;Step 15 : (DISPOSE-ALL-GARBAGE D1 N-0-3)   Created 1
;           15 -> (HOLDING G6)
;           0  -> (GARBAGE G6)
;           14 -> (HOLDING G5)
;           0  -> (GARBAGE G5)
;           13 -> (HOLDING G4)
;           0  -> (GARBAGE G4)
;           12 -> (HOLDING G3)
;           0  -> (GARBAGE G3)
;           11 -> (HOLDING G2)
;           0  -> (GARBAGE G2)
;           3  -> (TRUCK-AT N-0-3)
;           0  -> (DUMPSTER D1)
;           0  -> (DUMP-AT D1 N-0-3)
;           2  -> (HOLDING G1)
;           0  -> (GARBAGE G1)
;
;Goal    : (AND (GARBAGE-DISPOSED G1) (GARBAGE-DISPOSED G2)
;           (GARBAGE-DISPOSED G3) (GARBAGE-DISPOSED G4) (GARBAGE-DISPOSED G5)
;           (GARBAGE-DISPOSED G6))
;           1  -> (GARBAGE-DISPOSED G6)
;           1  -> (GARBAGE-DISPOSED G5)
;           1  -> (GARBAGE-DISPOSED G4)
;           1  -> (GARBAGE-DISPOSED G3)
;           1  -> (GARBAGE-DISPOSED G2)
;           1  -> (GARBAGE-DISPOSED G1)
;Facts:
;Complete!
;
;UCPOP Stats: Initial terms = 41;   Goals = 7 ;  Success (15 steps)
;      Created 693 plans, but explored only 483
;      CPU time:   20.2500 sec
;      Branching factor: 50.627
;      Working Unifies: 2708
;      Bindings Added: 303
;#plan<S=16; O=0; U=0; F=0>
;#<unprintable object @ #x1c9265e>
;UCPOP(446):
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
;UCPOP(54): (bf-control 'p4a)
;Initial  : ((TRUCK-AT N-0-0) (AT G1 N-0-0) (AT G2 N-0-2) (DUMP-AT D1 N-0-3)
;            (AT BOX N-0-0) (AT G3 N-0-0) (AT G4 N-0-1) (AT G5 N-0-2)
;            (AT G6 N-0-1) (GARBAGE G1) (GARBAGE G2) (GARBAGE G3) (GARBAGE G4)
;            (GARBAGE G5) (GARBAGE G6) (BOX BOX) (DUMPSTER D1) (LOCATION N-0-0)
;            (LOCATION N-0-1) (LOCATION N-0-2) (LOCATION N-0-3)
;            (ROAD R1 N-0-0 N-0-1) (ROAD R1 N-0-1 N-0-0) (ROAD R2 N-0-1 N-0-2)
;            (ROAD R2 N-0-2 N-0-1) (ROAD R3 N-0-2 N-0-3) (ROAD R3 N-0-3 N-0-2)
;            (ATM N-0-0) (ATM N-0-1) (FUEL-DISPENSER N-0-1) (FUEL-FULL)
;            (TOLLED R1) (TOLLED R2) (YES-TOLL R1) (YES-TOLL R2) (LIFTABLE G1)
;            (LIFTABLE G2) (LIFTABLE G3) (LIFTABLE G4) (LIFTABLE G5)
;            (LIFTABLE G6))

;Step 1  : (PICKUP G3 N-0-0)      Created 12
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G3)
;           0  -> (AT G3 N-0-0)
;Step 2  : (GET-ALL-MONEY-I-EVER-NEED N-0-0 N-0-0)   Created 7
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (ATM N-0-0)
;Step 3  : (PAY-TOLL YES-TOLL R2)   Created 8
;           7  -> (HAVE-MONEY)
;Step 4  : (PAY-TOLL YES-TOLL R1)   Created 6
;           7  -> (HAVE-MONEY)
;Step 5  : (PICKUP G1 N-0-0)      Created 2
;           0  -> (TRUCK-AT N-0-0)
;           0  -> (LIFTABLE G1)
;           0  -> (AT G1 N-0-0)
;Step 6  : (TRAVEL N-0-0 N-0-1 R1)   Created 5
;           0  -> (FUEL-FULL)
;           0  -> (NOT (FUEL-EMPTY))
;           6  -> (NOT (TOLLED R1))
;           0  -> (ROAD R1 N-0-0 N-0-1)
;           0  -> (LOCATION N-0-1)
;           0  -> (TRUCK-AT N-0-0)
;Step 7  : (PICKUP G6 N-0-1)      Created 15
;           5  -> (TRUCK-AT N-0-1)
;           0  -> (LIFTABLE G6)
;           0  -> (AT G6 N-0-1)
;Step 8  : (PICKUP G4 N-0-1)      Created 13
;           5  -> (TRUCK-AT N-0-1)
;           0  -> (LIFTABLE G4)
;           0  -> (AT G4 N-0-1)
;Step 9  : (GET-ALL-FUEL-I-EVER-NEED N-0-1 N-0-1)   Created 10
;           5  -> (TRUCK-AT N-0-1)
;           0  -> (FUEL-DISPENSER N-0-1)
;           7  -> (HAVE-MONEY)
;Step 10 : (TRAVEL N-0-1 N-0-2 R2)   Created 4
;           5  -> (NOT (FUEL-EMPTY))
;           8  -> (NOT (TOLLED R2))
;           0  -> (ROAD R2 N-0-1 N-0-2)
;           0  -> (LOCATION N-0-2)
;           5  -> (TRUCK-AT N-0-1)
;Step 11 : (PICKUP G5 N-0-2)      Created 14
;           4  -> (TRUCK-AT N-0-2)
;           0  -> (LIFTABLE G5)
;           0  -> (AT G5 N-0-2)
;Step 12 : (PICKUP G2 N-0-2)      Created 11
;           4  -> (TRUCK-AT N-0-2)
;           0  -> (LIFTABLE G2)
;           0  -> (AT G2 N-0-2)
;Step 13 : (REFUEL)               Created 9
;           10 -> (HAVE-FUEL)
;Step 14 : (TRAVEL N-0-2 N-0-3 R3)   Created 3
;           9  -> (NOT (FUEL-EMPTY))
;           0  -> (NOT (TOLLED R3))
;           0  -> (ROAD R3 N-0-2 N-0-3)
;           0  -> (LOCATION N-0-3)
;           4  -> (TRUCK-AT N-0-2)
;Step 15 : (DISPOSE-ALL-GARBAGE D1 N-0-3)   Created 1
;           15 -> (HOLDING G6)
;           0  -> (GARBAGE G6)
;           14 -> (HOLDING G5)
;           0  -> (GARBAGE G5)
;           13 -> (HOLDING G4)
;           0  -> (GARBAGE G4)
;           12 -> (HOLDING G3)
;           0  -> (GARBAGE G3)
;           11 -> (HOLDING G2)
;           0  -> (GARBAGE G2)
;           3  -> (TRUCK-AT N-0-3)
;           0  -> (DUMPSTER D1)
;           0  -> (DUMP-AT D1 N-0-3)
;           2  -> (HOLDING G1)
;           0  -> (GARBAGE G1)

;Goal    : (AND (GARBAGE-DISPOSED G1) (GARBAGE-DISPOSED G2)
;           (GARBAGE-DISPOSED G3) (GARBAGE-DISPOSED G4) (GARBAGE-DISPOSED G5)
;           (GARBAGE-DISPOSED G6))
;           1  -> (GARBAGE-DISPOSED G6)
;           1  -> (GARBAGE-DISPOSED G5)
;           1  -> (GARBAGE-DISPOSED G4)
;           1  -> (GARBAGE-DISPOSED G3)
;           1  -> (GARBAGE-DISPOSED G2)
;           1  -> (GARBAGE-DISPOSED G1)
;Facts:
;Complete!
; 
;UCPOP Stats: Initial terms = 41;   Goals = 7 ;  Success (15 steps)
;      Created 1112 plans, but explored only 740
;      CPU time:    8.0670 sec
;      Branching factor: 95.927
;      Working Unifies: 3475
;      Bindings Added: 533
;#plan<S=16; O=0; U=0; F=0>
;#Stats:<cpu time = 8.0670>


