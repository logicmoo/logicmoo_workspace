;PDDL Domain Plain Text

; ***************************************************************************
; *  All rights reserved. Use of this software is permitted for non-commercial
; *  research purposes, and it may be copied only for that use.  All copies must
; *  include this copyright message.  This software is made available AS IS, and
; *  neither the GIPO team nor the University of Huddersfield make any warranty
; *  about the software or its performance.
; *
; *  Automatically generated PDDL Domain from  GIPO Version 3.0
; *
; *  Author: Darren
; *  Institution: University of Huddersfield
; *  Date created: Tue Nov 14 16:25:00 GMT 2006
; *  Date last modified: 2006/12/03 at 05:41:15 PM GMT
; *  Description:
; *    
; * OCL File name : chameleonWorld.ocl
; * Object Life History : chameleonWorld.gfx
; *************************************************************************

(define (domain chameleonWorld)
  (:requirements :strips :equality :typing)

  (:types  door flexarium chameleon box substrate)


  (:predicates
    (doorOpen ?door1 - door)
    (doorClosed ?door1 - door)
    (dirty ?flexarium1 - flexarium)
    (clean ?flexarium1 - flexarium)
    (inBox ?chameleon1 - chameleon ?box1 - box)
    (inHands ?chameleon1 - chameleon)
    (inFlexarium ?chameleon1 - chameleon)
    (boxOpen ?box1 - box)
    (boxClosed ?box1 - box)
    (insideFlexarium ?substrate1 - substrate)
    (outsideFlexarium ?substrate1 - substrate)
  )
  (:action takeOutFlex
       :parameters ( ?Door - door ?Chameleon - chameleon)
       :precondition (and 
            (doorOpen ?Door)
            (inFlexarium ?Chameleon)
       )
       :effect (and 
            (not (inFlexarium ?Chameleon))
            (inHands ?Chameleon)
        )
    )
  (:action putInBox
       :parameters ( ?Box - box ?Chameleon - chameleon)
       :precondition (and 
            (boxOpen ?Box)
            (inHands ?Chameleon)
       )
       :effect (and 
            (not (inHands ?Chameleon))
            (inBox ?Chameleon ?Box)
        )
    )
  (:action takeOutBox
       :parameters ( ?Box - box ?Chameleon - chameleon)
       :precondition (and 
            (boxOpen ?Box)
            (inBox ?Chameleon ?Box)
       )
       :effect (and 
            (not (inBox ?Chameleon ?Box))
            (inHands ?Chameleon)
        )
    )
  (:action putInFlex
       :parameters ( ?Door - door ?Substrate - substrate ?Flexarium - flexarium ?Chameleon - chameleon)
       :precondition (and 
            (doorOpen ?Door)
            (insideFlexarium ?Substrate)
            (clean ?Flexarium)
            (inHands ?Chameleon)
       )
       :effect (and 
            (not (inHands ?Chameleon))
            (inFlexarium ?Chameleon)
        )
    )
  (:action openDoor
       :parameters ( ?Door - door)
       :precondition 
            (doorClosed ?Door)
       :effect (and 
            (not (doorClosed ?Door))
            (doorOpen ?Door)
        )
    )
  (:action closeDoor
       :parameters ( ?Door - door)
       :precondition 
            (doorOpen ?Door)
       :effect (and 
            (not (doorOpen ?Door))
            (doorClosed ?Door)
        )
    )
  (:action time
       :parameters ( ?Flexarium - flexarium)
       :precondition 
            (clean ?Flexarium)
       :effect (and 
            (not (clean ?Flexarium))
            (dirty ?Flexarium)
        )
    )
  (:action wash
       :parameters ( ?Chameleon - chameleon ?Box - box ?Door - door ?Substrate - substrate ?Flexarium - flexarium)
       :precondition (and 
            (inBox ?Chameleon ?Box)
            (doorOpen ?Door)
            (outsideFlexarium ?Substrate)
            (dirty ?Flexarium)
       )
       :effect (and 
            (not (dirty ?Flexarium))
            (clean ?Flexarium)
        )
    )
  (:action addCleanNewspaper
       :parameters ( ?Flexarium - flexarium ?Door - door ?Chameleon - chameleon ?Box - box ?Substrate - substrate)
       :precondition (and 
            (clean ?Flexarium)
            (doorOpen ?Door)
            (inBox ?Chameleon ?Box)
            (outsideFlexarium ?Substrate)
       )
       :effect (and 
            (not (outsideFlexarium ?Substrate))
            (insideFlexarium ?Substrate)
        )
    )
  (:action removeDirtyNewspaper
       :parameters ( ?Flexarium - flexarium ?Door - door ?Chameleon - chameleon ?Box - box ?Substrate - substrate)
       :precondition (and 
            (dirty ?Flexarium)
            (doorOpen ?Door)
            (inBox ?Chameleon ?Box)
            (insideFlexarium ?Substrate)
       )
       :effect (and 
            (not (insideFlexarium ?Substrate))
            (outsideFlexarium ?Substrate)
        )
    )
  (:action openBox
       :parameters ( ?Box - box)
       :precondition 
            (boxClosed ?Box)
       :effect (and 
            (not (boxClosed ?Box))
            (boxOpen ?Box)
        )
    )
  (:action closeBox
       :parameters ( ?Box - box)
       :precondition 
            (boxOpen ?Box)
       :effect (and 
            (not (boxOpen ?Box))
            (boxClosed ?Box)
        )
    )
  )


