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






