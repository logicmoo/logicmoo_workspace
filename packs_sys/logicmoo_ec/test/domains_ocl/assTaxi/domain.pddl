; ***************************************************************************
; *  All rights reserved. Use of this software is permitted for non-commercial
; *  research purposes, and it may be copied only for that use.  All copies must
; *  include this copyright message.  This software is made available AS IS, and
; *  neither the GIPO team nor the University of Huddersfield make any warranty
; *  about the software or its performance.
; *
; *  Automatically generated PDDL Domain from  GIPO Version 3.0
; *
; *  Author: Stefan Helmchen (C0416041)
; *  Institution: University of Huddersfield
; *  Date created: Mon Nov 27 10:35:43 GMT 2006
; *  Date last modified: 2015/07/07 at 01:33:39 PM PDT
; *  Description:
; *    
; * OCL File name : assTaxi.ocl
; * Object Life History : assTaxi.gfx
; *************************************************************************

(define (domain mytaxi)
  (:requirements :strips :equality :typing :conditional-effects)

  (:types  taxi location person fuel)


  (:predicates
    (taxiState ?taxi1 - taxi)
    (tLocation ?taxi1 - taxi ?location1 - location)
    (insideTaxi ?person1 - person ?taxi1 - taxi)
    (pLocation ?person1 - person ?location1 - location)
    (outsideTaxi ?person1 - person)
    (connects ?location1 - location ?location2 - location)
    (usefuel ?fuel1 - fuel ?fuel2 - fuel)
    (fillUpFuel ?fuel1 - fuel ?fuel2 - fuel)
    (tFuel ?taxi1 - taxi ?fuel1 - fuel)
  )

  (:action getOut
       :parameters ( ?Taxi - taxi ?Location - location ?Person - person)
       :precondition (and 
            (taxiState ?Taxi)
            (tLocation ?Taxi ?Location)
            (insideTaxi ?Person ?Taxi)
            (pLocation ?Person ?Location)
       )
       :effect (and 
            (not (insideTaxi ?Person ?Taxi))
            (outsideTaxi ?Person)
        )
    )
  (:action getIn
       :parameters ( ?Taxi - taxi ?Location - location ?Person - person)
       :precondition (and 
            (taxiState ?Taxi)
            (tLocation ?Taxi ?Location)
            (outsideTaxi ?Person)
            (pLocation ?Person ?Location)
       )
       :effect (and 
            (not (outsideTaxi ?Person))
            (insideTaxi ?Person ?Taxi)
        )
    )
  (:action move
       :parameters ( ?Taxi - taxi ?LocationA - location ?FuelA - fuel ?LocationB - location ?FuelB - fuel)
       :precondition (and 
            (taxiState ?Taxi)
            (tLocation ?Taxi ?LocationA)
            (tFuel ?Taxi ?FuelA)
            (connects ?LocationA ?LocationB)
            (usefuel ?FuelA ?FuelB)
       )
       :effect 
        (and 
            (not (tLocation ?Taxi ?LocationA))
            (not (tFuel ?Taxi ?FuelA))
            (tLocation ?Taxi ?LocationB)
            (tFuel ?Taxi ?FuelB)
            (forall (?Person - person)
              (when (and
                (insideTaxi ?Person ?Taxi)
                (pLocation ?Person ?LocationA)
                (connects ?LocationA ?LocationB)
               )
                 (and
                   (not (pLocation ?Person ?LocationA))
                   (pLocation ?Person ?LocationB)
               )))
        )
    )
  (:action fillFuel
       :parameters ( ?Taxi - taxi ?LocationA - location ?FuelA - fuel ?FuelB - fuel)
       :precondition (and 
            (taxiState ?Taxi)
            (tLocation ?Taxi ?LocationA)
            (tFuel ?Taxi ?FuelA)
            (fillUpFuel ?FuelA ?FuelB)
       )
       :effect (and 
            (not (tFuel ?Taxi ?FuelA))
            (tFuel ?Taxi ?FuelB)
        )
    )
 )
  
