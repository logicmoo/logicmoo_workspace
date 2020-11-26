; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ferry domain

(define (domain ferry)
  (:requirements :strips :equality)

  (:constants ferry)
  (:predicates (at ?x ?y)
	       (place ?x)
	       (auto ?x)
	       (at-ferry ?x)
	       (empty-ferry)
	       (on ?x ?y))

  (:action board
	     :parameters (?x ?y)
	     :precondition (and (at ?x ?y)
				 (place ?y)
				 (auto ?x)
				 (at-ferry ?y)
				 (empty-ferry))
	     :effect (and (on ?x ferry)
			   (not (at ?x ?y))
			   (not (empty-ferry))))
  (:action sail
	     :parameters (?x ?y)
	     :precondition (and (at-ferry ?x)
				 (place ?x)
				 (place ?y)
				 (not (= ?x ?y)))
	     :effect (and (at-ferry ?y)
			   (not (at-ferry ?x))))
  (:action debark
	     :parameters (?x ?y)
	     :precondition (and (on ?x ferry)
				 (auto ?x)
				 (place ?y)
				 (at-ferry ?y))
	     :effect (and (not (on ?x ferry))
			   (at ?x ?y)
			   (empty-ferry))))










