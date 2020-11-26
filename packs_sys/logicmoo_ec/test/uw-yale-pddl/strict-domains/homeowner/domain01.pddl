; (c) 1993,1994 Copyright (c) University of Washington
;  Written by Tony Barrett.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here lies an encoding of Pednault's IJCAI-91 example
;;
;;  You bought a house and discover that after turning on water,
;;  it pours out of holes in the wall.  Using the three actions from
;;  below, the homeowner's problem is to find a way to have the water
;;  on without having all those holes in the wall.
;;
;;  1) A "Fixing the wall" action is only effective when the
;;     plumbing is good.
;;  2) A "Fixing the plumbing" action is only good when the water
;;     is off.
;;  2) A "Turning the faucet" action will bash the wall only
;;     when the plumbing is bad and when you turn it to "ON".
;;
;;  The first two actions are encoded as (FIX ?it).
;;  The second is (TURN-FAUCET ?how).
;;
;; J. Scott Penberthy 3/92
;;

(define (domain homeowner)
  (:requirements :adl :equality :conditional-effects)

  (:predicates (good-plumbing)
	       (holey-walls)
	       (water ?on))
  (:constants wall plumbing on off)
  
  ;; a FIX operator -- a handyman can do anything, within limits
  (:action fix
	     :parameters (?it)
	     :precondition (object ?it)
	     :effect
	     (and (when (and (= ?it wall) (good-plumbing))
			  (not (holey-walls)))
		   (when (and (= ?it wall) (not (good-plumbing)) 
				(water off))
			  (not (holey-walls)))
		   (when (and (= ?it plumbing) (water off))
			  (good-plumbing))))

  ;; another operator for turning the water on/off
  (:action turn-faucet
	     :parameters (?how)
	     :effect
	     (and (water ?how)
		   (forall (?s)
			    (when (and (not (= ?s ?how)) (water ?s))
				   (not (water ?s))))
		   (when (and (= ?how ON) (not (good-plumbing)))
			  (holey-walls)))))



