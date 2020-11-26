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

(define (problem task1)
   (:domain washing)
   (:objects
         door1 door2 - door
         plug1 plug2 - power
         myMachine yourMachine - machine
         lights darks cotton silk - clothes
        )
    (:init
        (pluggedIn plug1)
        )
    (:goal
      (and
        (unplugged plug1)
       ))
)
