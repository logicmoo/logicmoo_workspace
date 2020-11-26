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

(define (problem task4)
   (:domain washing)
   (:objects
         door1 door2 - door
         plug1 plug2 - power
         myMachine yourMachine - machine
         lights darks cotton silk - clothes
        )
    (:init
        (empty yourMachine door2 plug2)
        (doorClosed door1)
        (dirty darks)
        (dirty lights)
        (unplugged plug1)
        (pluggedIn plug2)
        (doorClosed door2)
        (fullDirty myMachine door1 darks plug1)
        )
    (:goal
      (and
        (empty myMachine door1 plug1)
        (empty yourMachine door2 plug2)
        (doorOpen door1)
        (clean lights)
        (unplugged plug1)
        (clean darks)
        (unplugged plug2)
        (doorOpen door2)
       ))
)

