; (c) 1993,1994 Copyright (c) University of Washington
;  Written by students in UW CS.
;
;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strips version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain woodshop-adl)
  (:requirements :adl :universal-preconditions :conditional-effects)
  (:predicates (boxed ?o)
	       (in-spray-hood ?o)
	       (dusty ?o)
	       (surface ?o ?s)
	       (color ?c)
	       (painted ?o ?c)
	       (shape ?o ?s)
	       (has-hole ?o ?d)
	       (physobj ?o)
	       )
  (:constants round			; from the drill
	      rough)			; from the saw


  (:action sand
             :parameters (?o ?s)
             :precondition (and (not (boxed ?o)) (not (in-spray-hood ?o)))
             :effect (and (dusty ?o)
                           (surface ?o ?s)
                           (forall (?c) (when (color ?c) (not (painted ?o ?c))))))
  (:action vacuum
             :parameters (?o)
             :precondition (and (not (boxed ?o))
                                 (not (in-spray-hood ?o)))
             :effect (not (dusty ?o)))
  ;; (:action paint
  ;;            :parameters (?newcol)
  ;;            :precondition (color ?newcol)
  ;;            :effect (and (forall ((object ?o))
  ;;                                   (when (and (in-spray-hood ?o) (not (boxed ?o)))
  ;;                                     (painted ?o ?newcol)))
  ;;                          (forall ((object ?o))
  ;;                                  (when (and (in-spray-hood ?o) (not (boxed ?o))
  ;;                                                (:exists (color ?c)
  ;;                                                        (and (painted ?o ?c)
  ;;                                                              (not (= ?newcol ?c)))))
  ;;                                    (and (not (painted ?o ?oldcol)))))))
  ;; 

  (:action paint
	     :parameters (?newcol)
	     :precondition (color ?newcol)
	     :effect (and (forall (?o)
				    (when (and (physobj ?o) (in-spray-hood ?o) (not (boxed ?o)))
				      (painted ?o ?newcol)))
			   (forall (?o)
				    (when (and (physobj ?o) (in-spray-hood ?o)(not (boxed ?o)))
				      (forall (?c) 
					       (when (and (color ?c) (not (= ?c ?newcol))) 
						 (not (painted ?o ?c))))
				      ))))
  
  
  (:action forklift
	     :parameters (?o)
	     :precondition (not (boxed ?o))
	     :effect  (and (when (not (in-spray-hood ?o))
			      (in-spray-hood ?o))
			    (when (in-spray-hood ?o)
                              (not (in-spray-hood ?o)))))
  (:action drill
             :parameters (?o ?diam)
             :precondition (and (not (boxed ?o)) (not (shape ?o round))
                                 (not (in-spray-hood ?o)))
             :effect (and (has-hole ?o ?diam)
                           (dusty ?o)))
  (:action saw
             :parameters (?o ?newshape ?oldshape)
             :precondition (and (not (boxed ?o)) (not (in-spray-hood ?o))
                                 (shape ?o ?oldshape) (not (= ?newshape ?oldshape)))
             :effect (and (shape ?o ?newshape)
                           (surface ?o rough)
                           (not (shape ?o ?oldshape))
                           (dusty ?o)))
  (:action put-in-box
             :parameters (?o)
             :precondition (and (not (boxed ?o)) (not (in-spray-hood ?o)))
             :effect (boxed ?o)))







