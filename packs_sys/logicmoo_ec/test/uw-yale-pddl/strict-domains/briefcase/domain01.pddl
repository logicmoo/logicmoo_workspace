; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.
;
;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.


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






