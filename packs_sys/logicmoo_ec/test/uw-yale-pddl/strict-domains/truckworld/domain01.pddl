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


; The following is left over from UCPOP:

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

