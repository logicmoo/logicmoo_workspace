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



(define (problem woodshop-adl-1)
    (:domain woodshop-adl)
  (:objects o blob green plain three-eighths smooth square)
  (:init  (physobj o) (not (in-spray-hood o)) (shape O BLOB)
	  (color green) (color plain)
	  (painted O plain)
	  (surface O ROUGH))
  (:goal   (and (has-hole O three-eighths)
		(painted O GREEN)
		(surface O SMOOTH)
		(shape O SQUARE)
		(not (dusty O)) (boxed O))))

(define (problem woodshop-adl-2)
    (:domain woodshop-adl)
  (:objects o green plain three-eighths smooth)
  (:init  (physobj o) (not (in-spray-hood o)) (shape O round)
	  (color green) (color plain) 
	  (painted O plain) (surface O ROUGH))
  (:goal   (and (has-hole O three-eighths)
		(painted O GREEN)
		(surface O SMOOTH)
		(shape O round)
		(not (dusty O)) (boxed O))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strips version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain woodshop-strips)
  (:requirements :strips :typing :equality)
  (:constants round - objshape)
  (:types size objshape physobj color)
  (:predicates (hole ?obj ?sz)
	       (shape ?obj ?sh)
	       (inhood ?obj)
	       (inbox ?obj)
	       (dusty ?obj)
	       (rough ?obj)
	       (painted ?obj)
	       (colored ?obj ?color))

  (:action drill
	     :parameters (?obj - physobj  ?sz - size  ?sh - objshape)
             :precondition (and (not (hole ?obj ?sz)) (shape ?obj ?sh) (not (= ?sh round))
                                 (not (inhood ?obj)) (not (inbox ?obj)))
             :effect
                (and (dusty ?obj)
                      (rough ?obj)
                      (hole ?obj ?sz)))


   (:action saw
	      :parameters (?obj - physobj  ?origsh ?newsh - objshape)
              :precondition (and (not (inhood ?obj)) (shape ?obj ?origsh)
                                  (not (inbox ?obj)))
              :effect
                 (and (shape ?obj ?newsh)
                       (not (shape ?obj ?origsh))
                       (dusty ?obj)
                       (rough ?obj)))

   (:action sander
              :parameters (?obj - physobj)
              :precondition (and (not (inhood ?obj)) (not (inbox ?obj)))
              :effect
                 (and (not (rough ?obj))
                       (dusty ?obj)
                       (not (painted ?obj))))

   (:action sprayer
	      :parameters (?obj - physobj  ?origcol ?newcol - color)
              :precondition (and (inhood ?obj) (not (inbox ?obj))
                                  (colored ?obj ?origcol))
              :effect
                 (and (colored ?obj ?newcol)
                       (not (colored ?obj ?origcol))
                       (painted ?obj)))

   (:action forklift-out
              :parameters (?obj - physobj)
              :precondition (and (inhood ?obj) (not (inbox ?obj)))
              :effect
                 (not (inhood ?obj)))


    (:action forklift-in
              :parameters (?obj - physobj)
              :precondition (and (not (inhood ?obj)) (not (inbox ?obj)))
              :effect
                 (inhood ?obj))


   (:action vacuum
              :parameters (?obj - physobj)
              :precondition (and (not (painted ?obj)) (not (inbox ?obj))
                                  (not (inhood ?obj)) (dusty ?obj))
              :effect
                 (not (dusty ?obj)))

   (:action packer
              :parameters (?obj - physobj)
              :precondition (and (not (inbox ?obj)) (not (inhood ?obj)))
              :effect
                  (inbox ?obj)))

(define (problem woodshop-strips-1)
    (:domain woodshop-strips)
  (:objects a - physobj
	    natural green - color
	    square - objshape
	    three-eighths - size)
  (:init  (not (inbox a)) (not (inhood a)) (shape a round) (colored a natural)
           (rough a) (not (hole a three-eighths)))
  (:length (:serial 9) (:parallel 9))  
  (:goal (and (colored a green) (shape a round) (hole a three-eighths) (not (dusty a))
               (inbox a) (not (rough a)))))




