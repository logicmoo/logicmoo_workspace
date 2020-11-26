; ***************************************************************************
; *  All rights reserved. Use of this software is permitted for non-commercial
; *  research purposes, and it may be copied only for that use.  All copies must
; *  include this copyright message.  This software is made available AS IS, and
; *  neither the GIPO team nor the University of Huddersfield make any warranty
; *  about the software or its performance.
; *
; *  Automatically generated PDDL Domain from  GIPO Version 3.0
; *
; *  Author: Bhonda
; *  Institution: University of Huddersfield
; *  Date created: Wed Dec 13 11:42:46 GMT 2006
; *  Date last modified: 2006/12/15 at 04:26:22 AM GMT
; *  Description:
; *    
; * OCL File name : toasterWorldv2.ocl
; * Object Life History : toasterWorld.gfx
; *************************************************************************

(define (domain nonetw)
  (:requirements :strips :equality :typing)

  (:types  plates toaster item loc spreads kitchenware)


  (:predicates
    (cleanPlate ?plates1 - plates)
    (dirtyPlate ?plates1 - plates)
    (pluggedIn ?toaster1 - toaster)
    (notPluggedIn ?toaster1 - toaster)
    (isCold ?item1 - item)
    (location ?item1 - item ?loc1 - loc)
    (isToasted ?item1 - item)
    (isMade ?item1 - item)
    (inJar ?spreads1 - spreads)
    (item ?spreads1 - spreads ?item1 - item)
    (onToast ?spreads1 - spreads)
    (clean ?kitchenware1 - kitchenware)
    (dirty ?kitchenware1 - kitchenware)
    (next ?loc1 - loc ?loc2 - loc)
  )
  (:action wash
       :parameters ( ?Kitchenware - kitchenware)
       :precondition 
            (dirty ?Kitchenware)
       :effect (and 
            (not (dirty ?Kitchenware))
            (clean ?Kitchenware)
        )
    )
  (:action use
       :parameters ( ?Item - item ?Kitchenware - kitchenware ?Spreads - spreads)
       :precondition (and 
            (isToasted ?Item)
            (location ?Item side)
            (clean ?Kitchenware)
            (inJar ?Spreads)
            (item ?Spreads nullItem)
       )
       :effect (and 
            (not (clean ?Kitchenware))
            (not (inJar ?Spreads))
            (not (item ?Spreads nullItem))
            (dirty ?Kitchenware)
            (onToast ?Spreads)
            (item ?Spreads ?Item)
        )
    )
  (:action toast
       :parameters ( ?Toaster - toaster ?Item - item)
       :precondition (and 
            (pluggedIn ?Toaster)
            (isCold ?Item)
            (location ?Item toaster)
       )
       :effect (and 
            (not (isCold ?Item))
            (isToasted ?Item)
        )
    )
  (:action cleanThePlate
       :parameters ( ?Plates - plates)
       :precondition 
            (dirtyPlate ?Plates)
       :effect (and 
            (not (dirtyPlate ?Plates))
            (cleanPlate ?Plates)
        )
    )
  (:action plugIn
       :parameters ( ?Toaster - toaster)
       :precondition 
            (notPluggedIn ?Toaster)
       :effect (and 
            (not (notPluggedIn ?Toaster))
            (pluggedIn ?Toaster)
        )
    )
  (:action unplug
       :parameters ( ?Toaster - toaster)
       :precondition 
            (pluggedIn ?Toaster)
       :effect (and 
            (not (pluggedIn ?Toaster))
            (notPluggedIn ?Toaster)
        )
    )
  (:action serve
       :parameters ( ?Plates - plates ?LocA - loc ?LocB - loc ?Item - item)
       :precondition (and 
            (cleanPlate ?Plates)
            (next ?LocA ?LocB)
            (isToasted ?Item)
            (location ?Item ?LocA)
            (next ?LocA ?LocB)
       )
       :effect (and 
            (not (cleanPlate ?Plates))
            (not (isToasted ?Item))
            (not (location ?Item ?LocA))
            (dirtyPlate ?Plates)
            (isMade ?Item)
            (location ?Item ?LocB)
        )
    )
  (:action move
       :parameters ( ?Item - item ?LocA - loc ?LocB - loc)
       :precondition (and 
            (isCold ?Item)
            (location ?Item ?LocA)
            (next ?LocA ?LocB)
       )
       :effect (and 
            (not (location ?Item ?LocA))
            (location ?Item ?LocB)
        )
    )
  (:action moveHot
       :parameters ( ?Item - item ?LocA - loc ?LocB - loc)
       :precondition (and 
            (isToasted ?Item)
            (location ?Item ?LocA)
            (next ?LocA ?LocB)
       )
       :effect (and 
            (not (location ?Item ?LocA))
            (location ?Item ?LocB)
        )
    )
  )

