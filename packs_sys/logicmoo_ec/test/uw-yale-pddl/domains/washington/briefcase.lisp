" (c) 1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package :domains)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jscott's briefcase world
;;; Modified 6/1/97, by D. E. Smith to make sure variables were bound by
;;; operator preconditions

(define (domain briefcase-world)
  (:requirements :strips :equality :typing :conditional-effects)

  (:types place)
  (:constants B P D)
  (:predicates (at ?thing - object
		   ?l - place)
	       (in ?thing - object))

  (:action mov-b
      :parameters (?m ?l - place)       ; added place
      :precondition (and (at B ?m) (not (= ?m ?l)))
      :effect (and (at b ?l) (not (at B ?m))
		   (when (in P)
		     (and (at P ?l) (not (at P ?m))))
		   (when (in D)
		     (and (at D ?l) (not (at D ?m))))))

  (:action take-out
      :parameters (?x)
      :precondition (in ?x)             ; changed from (not (= ?x B))
      :effect (not (in ?x)))

  (:action put-in
      :parameters (?x - object ?l - place)
      :precondition (and (at ?x ?l) (at B ?l) (not (= ?x B)))
      :effect (in ?x)))

(define (problem get-paid)              ; graph-plan 2 steps, 3 actions
    (:domain briefcase-world)
  (:objects home office - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:length (:serial 4) (:parallel 4))
  (:goal (and (at B office) (at D office) (at P home))))

;;;UCPOP(33): (bf-control 'get-paid)
;;;
;;;Initial  : ((PLACE HOME) (PLACE OFFICE) (OBJECT P) (OBJECT D) (OBJECT B)
;;;            (AT B HOME) (AT P HOME) (AT D HOME) (IN P))
;;;
;;;Step 1  : (PUT-IN D HOME)        Created 3
;;;           0  -> (AT D HOME)
;;;           0  -> (AT B HOME)
;;;Step 2  : (TAKE-OUT P)           Created 2
;;;Step 3  : (MOV-B HOME OFFICE)    Created 1
;;;           3  -> (IN D)
;;;           0  -> (AT B HOME)
;;;           2  -> (NOT (IN P))
;;;
;;;Goal    : (AND (AT B OFFICE) (AT D OFFICE) (AT P HOME))
;;;           1  -> (AT B OFFICE)
;;;           1  -> (AT D OFFICE)
;;;           0  -> (AT P HOME)
;;;Complete!
;;;
;;;UCPOP (Init = 9  ; Goals = 4 ) => Win  (3 steps)     CPU 134
;;;     Nodes (V = 20  ; Q = 10  ; C = 31  )             Branch 1.5
;;;     Working Unifies: 278                             Bindings added: 37
;;;NIL

(define (problem get-paid2)             ; graph-plan 4 steps, 6 actions
    (:domain briefcase-world)
  (:objects home office - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:goal (and (at P home) (at D office) (at B home)))
  (:length (:serial 4) (:parallel 4)))

(define (problem get-paid3)             ; graph-plan 6 steps, 8 actions
    (:domain briefcase-world)
  (:objects home office bank - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:goal (and (at P bank) (at D office) (at B home)))
  (:length (:serial 6) (:parallel 6))
    )

(define (problem get-paid4)             ; graph-plan 6 steps, 8 actions
    (:domain briefcase-world)
  (:objects home office bank - place)
  (:init (at B home) (at P home) (at D home) (in P))
  (:goal (and (at B home) (at D office) (at P bank)))
  (:length (:serial 6) (:parallel 6))
    )

;;;;;  Another briefcase world

(define (domain uni-bw)
  (:requirements :adl)
  (:constants B)
  (:predicates (place ?l)
	       (obj ?o)
	       (at ?thing ?l)
	       (in ?thing))
  (:action mov-b
	     ;; The place typing requires additions to the initial conditions
	     ;; (else nothing is a place).
	     ;; :parameters (?m (place ?l))       ; added place typing
	     :parameters (?m ?l)
	     :precondition (and (at B ?m) (not (= ?m ?l)))
	     :effect
	     (and (at b ?l) (not (at B ?m))
		   (forall (?z)
			    (when (and (in ?z) (not (= ?z B)))
				   (and (at ?z ?l) (not (at ?z ?m)))))))

  (:action take-out
	     :parameters (?x)
	     :precondition (in ?x)           ; changed from (not (= ?x B))
	     :effect (not (in ?x)))

  (:action put-in                     ; changed to non-conditional op
	     :parameters (?x ?l)
	     :precondition (and (at ?x ?l) (at B ?l) (not (= ?x B)))
	     :effect (in ?x)))

;;;UCPOP(42): (bf-control 'uget-paid)
;;;
;;;Initial  : ((AT B HOME) (AT P HOME) (AT D HOME) (IN P))
;;;
;;;Step 1  : (PUT-IN D HOME)        Created 3
;;;           0  -> (AT D HOME)
;;;           0  -> (AT B HOME)
;;;Step 2  : (TAKE-OUT P)           Created 2
;;;Step 3  : (MOV-B HOME OFFICE)    Created 1
;;;           3  -> (IN D)
;;;           0  -> (AT B HOME)
;;;           2  -> (NOT (IN P))
;;;
;;;Goal    : (AND (AT B OFFICE) (AT D OFFICE) (AT P HOME))
;;;           1  -> (AT B OFFICE)
;;;           1  -> (AT D OFFICE)
;;;           0  -> (AT P HOME)
;;;Complete!
;;;
;;;UCPOP (Init = 5  ; Goals = 4 ) => Win  (3 steps)     CPU 150
;;;     Nodes (V = 23  ; Q = 11  ; C = 44  )             Branch 1.4782609
;;;     Working Unifies: 254                             Bindings added: 65
;;;NIL

(define (problem uget-paid)
    (:domain uni-bw)
  (:objects P D home office)
  (:init (at B home) (at P home) (at D home) (in P) )
  (:goal (and (at B office) (at D office) (at P home)))
  (:length (:serial 3) (:parallel 3)))

(define (problem uget-paid2)
    (:domain uni-bw)
  (:objects P D home office)
  (:init (place home)(place office) (obj p)(obj d)(obj b)
	 (at B home) (at P home) (at D home) (in P))
  (:goal (and (at P home) (at D office) (at B home)))
  (:length (:serial 4) (:parallel 4))
    )

(define (problem uget-paid3)
    (:domain uni-bw)
  (:objects P D home office bank)
  (:init (place home)(place office)(place bank)
	 (obj p)(obj d)(obj b)
	 (at B home) (at P home) (at D home) (in P))
  (:goal (and (at P bank) (at D office) (at B home)))
  (:length (:serial 6) (:parallel 6))
    )

(define (problem uget-paid4)
    (:domain uni-bw)
  (:objects P D home office bank)
  (:init (place home)(place office)(place bank)
	 (obj p)(obj d)(obj b)
	 (at B home) (at P home) (at D home) (in P))
  (:goal (and (at B home) (at D office) (at P bank)))
  (:length (:serial 6) (:parallel 6))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced briefcase world
;;;   This domain is rather interesting - should be extended more -DSW 12/92

;;; Note:  This is not a PDDL-legal domain, because of the 
;;; :procedural-attachment requirement
;;(define (domain office-world)
;;  (:requirements :adl :open-world :procedural-attachment)
;;  (:action move
;;	     :parameters (?b ?l (place ?m))
;;	     :precondition (and (briefcase ?b) (at ?b ?l) (not (= ?m ?l)))
;;	     :effect (and (at ?b ?m)
;;			   (not (at ?B ?l))
;;			   (forall (?x)
;;				    (when (and (object ?x) (in ?x ?b))
;;				      (and (at ?x ?m) (not (at ?x ?l)))))))
;;
;;  (:action take-out
;;	     :parameters (?x ?b)
;;	     :precondition (in ?x ?b)
;;	     :effect (not (in ?x ?b)))
;;
;;  (:action put-in
;;	     :parameters (?x ?b ?l)
;;	     :precondition (and (at ?x ?l) (not (= ?x ?B)) (at ?B ?l)
;;				 (briefcase ?b))
;;	     :effect (in ?x ?b))
;;
;;  (:action print-check-for
;;	    :parameters (?p ?c)
;;	    :precondition (and (person ?p) (new-object ?c))
;;	    :effect (and (object ?c) (check ?c) (at ?c office)
;;			  (written-for ?p ?c)))
;;
;;  (:fact (new-object ?x)
;;	 (when (variable? ?x)
;;	   (list (setb ?x (gensym "obj-")))))
;;  )
;;
;;(define (problem all-home)
;;    :domain office-world
;;    :init (and (object d) (object b) (briefcase b)
;;		(at B home) (at d office))
;;    :goal (and (forall (object ?o)
;;		   (at ?o home))))
;;
;;(define (problem all3-home)
;;    :domain office-world
;;    :init (and (object d) (object b) (object p) (briefcase b)
;;		(at B home) (at d office) (at p home))
;;    :goal (and (forall (object ?o)
;;		   (at ?o home))))
;;
;;(define (problem office1)
;;    :domain office-world
;;    :init (and (place home) (place office) (person sam) (person sue)
;;		(object dict) (object b) (briefcase b)
;;		(at B home) (at Dict home))
;;    :goal (and (forall (person ?p)
;;		   (:exists (object ?c)
;;			    (and (written-for ?p ?c))))))
;;
;;(define (problem office2)
;;    :domain office-world
;;    :init (and (place home) (place office) (person sam) (person sue)
;;		(object dict) (object b) (briefcase b)
;;		(at B home) (at Dict home))
;;    :goal (and (forall (object ?x) (at ?x office))))
;;
;;(define (problem office3)
;;    :domain office-world
;;    :init (and (place home) (place office) (person sam) (person sue)
;;		(object dict) (object b) (briefcase b)
;;		(at B home) (at Dict home))
;;    :goal (and (forall (person ?p)
;;		   (:exists (object ?c)
;;			    (and (check ?c) (written-for ?p ?c))))))
;;
;;(define (problem office4)
;;    :domain office-world
;;    :init (and (place home) (place office) (person sam) (person sue)
;;		(object dict) (object b) (briefcase b)
;;		(at B home) (at Dict home))
;;    :goal (and (forall (object ?x) (at ?x office))
;;		(forall (person ?p)
;;			 (:exists (object ?c)
;;				  (and (check ?c) (written-for ?p ?c))))))
;;
;;(define (problem office5)
;;    :domain office-world
;;    :init (and (place home) (place office) (person sam) (person sue)
;;		(object dict) (object b) (briefcase b)
;;		(at B home) (at Dict home))
;;    :goal (and (forall (object ?x) (at ?x home))
;;		(forall (person ?p)
;;			 (:exists (object ?c)
;;				  (and (check ?c) (written-for ?p ?c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strips version of the simple briefcase world

(define (domain briefcase-strips)
  (:requirements :strips :equality)
  (:constants B)
  (:predicates (at ?x ?y)
	       (in ?x ?y))
  (:action move-briefcase
	     :parameters (?m ?l)
	     :precondition (and (at B ?m) (not (= ?m ?l)))
	     :effect (and (not (at B ?m))
			   (at B ?l)))
  (:action take-out
	     :parameters (?x ?y)
	     :precondition (and (at B ?y) (in ?x B) (not (= ?x B)))
	     :effect (and (not (in ?x B))
			   (at ?x ?y)))
  (:action put-in
	     :parameters (?x ?y)
	     :precondition (and (at ?x ?y) (at B ?y) (not (= ?x B)))
	     :effect (and (not (at ?x ?y)) (in ?x B))))

(define (problem get-paid-strips)
  (:domain briefcase-strips)
  (:objects D P home office)
  (:init (at B home)
	 (at D home)
	 (at P home)
	 (in P B))
  (:goal (and (at B office)
	      (at D office)
	      (at P home)))
  (:length (:serial 3) (:parallel 3)))

(define (problem get-paid2-strips)
  (:domain briefcase-strips)
  (:objects D P home office)
  (:init (at B home)
	 (at P home)
	 (at D home)
	 (in P B))
  (:goal (and (at B home)
	      (at D office)
	      (at P home)))
  (:length (:serial 4) (:parallel 4)))

(define (problem get-paid3-strips)
  (:domain briefcase-strips)
  (:objects D P home office bank)
  (:init (at B home)
	 (at P home)
	 (at D home)
	 (in P B))
  (:goal (and (at B home)
	      (at D office)
	      (at P bank)))
  (:length (:serial 6) (:parallel 6)))

