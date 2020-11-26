; ***************************************************************************
; *  All rights reserved. Use of this software is permitted for non-commercial
; *  research purposes, and it may be copied only for that use.  All copies must
; *  include this copyright message.  This software is made available AS IS, and
; *  neither the GIPO team nor the University of Huddersfield make any warranty
; *  about the software or its performance.
; *
; *  Automatically generated PDDL Domain from  GIPO Version 3.0
; *
; *  Author: Gaz
; *  Institution: University of Huddersfield
; *  Date created: Wed Dec 13 23:50:18 GMT 2006
; *  Date last modified: 2015/07/26 at 07:15:31 AM PDT
; *  Description:
; *    
; * OCL File name : c0251943_WashingMachine.ocl
; * Object Life History : WashingMachine.gfx
; *************************************************************************

(define (domain washing)
  (:requirements :strips :equality :typing)

  (:types  door power machine clothes)


  (:predicates
    (doorOpen ?door1 - door)
    (doorClosed ?door1 - door)
    (locked ?door1 - door)
    (unplugged ?power1 - power)
    (pluggedIn ?power1 - power)
    (turnedOn ?power1 - power)
    (empty ?machine1 - machine ?door1 - door ?power1 - power)
    (washing ?machine1 - machine ?door1 - door ?clothes1 - clothes ?power1 - power)
    (fullDirty ?machine1 - machine ?door1 - door ?clothes1 - clothes ?power1 - power)
    (fullClean ?machine1 - machine ?door1 - door ?clothes1 - clothes ?power1 - power)
    (dirty ?clothes1 - clothes)
    (clean ?clothes1 - clothes)
    (beingWashed ?clothes1 - clothes)
  )
  (:action close
       :parameters ( ?Door - door)
       :precondition 
            (doorOpen ?Door)
       :effect (and 
            (not (doorOpen ?Door))
            (doorClosed ?Door)
        )
    )
  (:action finishedWash
       :parameters ( ?Door - door ?Clothes - clothes ?Machine - machine ?Power - power)
       :precondition (and 
            (locked ?Door)
            (beingWashed ?Clothes)
            (washing ?Machine ?Door ?Clothes ?Power)
       )
       :effect (and 
            (not (locked ?Door))
            (not (beingWashed ?Clothes))
            (not (washing ?Machine ?Door ?Clothes ?Power))
            (doorClosed ?Door)
            (clean ?Clothes)
            (fullClean ?Machine ?Door ?Clothes ?Power)
        )
    )
  (:action open
       :parameters ( ?Door - door)
       :precondition 
            (doorClosed ?Door)
       :effect (and 
            (not (doorClosed ?Door))
            (doorOpen ?Door)
        )
    )
  (:action unplug
       :parameters ( ?Power - power)
       :precondition 
            (pluggedIn ?Power)
       :effect (and 
            (not (pluggedIn ?Power))
            (unplugged ?Power)
        )
    )
  (:action plugIn
       :parameters ( ?Power - power)
       :precondition 
            (unplugged ?Power)
       :effect (and 
            (not (unplugged ?Power))
            (pluggedIn ?Power)
        )
    )
  (:action turnOff
       :parameters ( ?Machine - machine ?Door - door ?Power - power)
       :precondition (and 
            (empty ?Machine ?Door ?Power)
            (turnedOn ?Power)
       )
       :effect (and 
            (not (turnedOn ?Power))
            (pluggedIn ?Power)
        )
    )
  (:action turnOn
       :parameters ( ?Power - power)
       :precondition 
            (pluggedIn ?Power)
       :effect (and 
            (not (pluggedIn ?Power))
            (turnedOn ?Power)
        )
    )
  (:action add
       :parameters ( ?Clothes - clothes ?Door - door ?Machine - machine ?Power - power)
       :precondition (and 
            (dirty ?Clothes)
            (doorOpen ?Door)
            (empty ?Machine ?Door ?Power)
       )
       :effect (and 
            (not (empty ?Machine ?Door ?Power))
            (fullDirty ?Machine ?Door ?Clothes ?Power)
        )
    )
  (:action remove
       :parameters ( ?Clothes - clothes ?Door - door ?Machine - machine ?Power - power)
       :precondition (and 
            (clean ?Clothes)
            (doorOpen ?Door)
            (fullClean ?Machine ?Door ?Clothes ?Power)
       )
       :effect (and 
            (not (fullClean ?Machine ?Door ?Clothes ?Power))
            (empty ?Machine ?Door ?Power)
        )
    )
  (:action startWash
       :parameters ( ?Power - power ?Clothes - clothes ?Machine - machine ?Door - door)
       :precondition (and 
            (turnedOn ?Power)
            (dirty ?Clothes)
            (fullDirty ?Machine ?Door ?Clothes ?Power)
            (doorClosed ?Door)
       )
       :effect (and 
            (not (dirty ?Clothes))
            (not (fullDirty ?Machine ?Door ?Clothes ?Power))
            (not (doorClosed ?Door))
            (beingWashed ?Clothes)
            (washing ?Machine ?Door ?Clothes ?Power)
            (locked ?Door)
        )
    )
  (:action time
       :parameters ( ?Clothes - clothes)
       :precondition 
            (clean ?Clothes)
       :effect (and 
            (not (clean ?Clothes))
            (dirty ?Clothes)
        )
    )
  )


