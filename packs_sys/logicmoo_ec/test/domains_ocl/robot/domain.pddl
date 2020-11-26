; ***************************************************************************
; *  All rights reserved. Use of this software is permitted for non-commercial
; *  research purposes, and it may be copied only for that use.  All copies must
; *  include this copyright message.  This software is made available AS IS, and
; *  neither the GIPO team nor the University of Huddersfield make any warranty
; *  about the software or its performance.
; *
; *  Automatically generated PDDL Domain from  GIPO Version 3.0
; *
; *  Author: Doris Liu
; *  Institution: University of Huddersfield
; *  Date created: prior to 2000
; *  Date last modified: 2001/10/24 at 12:40:59 PM BST
; *  Description:
; *    In this domain a robot must navigate from room to room with the
; *    objective of pushing boxes from their initial position/room to some
; *    desired room. The rooms are connected by doors. The robot is
; *    constrained to only push one box at a time and can ony push a
; *    box by first by being in the same room as the box and being
; *    near the box. The robot can only pass through a door if it is
; *    at that door. The robot can only pus a box through a door if
; *    the robot is at the door and near the box. Going through
; *    a door takes the robot and any pushed boxes to the
; *    connecting room.
; * OCL File name : robot.ocl
; *************************************************************************

(define (domain robot)
  (:requirements :strips :equality :typing)

  (:types  room door box robot)


  (:predicates
    (in_room ?box1 - box ?room1 - room)
    (at_door ?box1 - box ?door1 - door ?room1 - room)
    (robot_in ?robot1 - robot ?room1 - room)
    (robot_at ?robot1 - robot ?door1 - door ?room1 - room)
    (robot_near ?robot1 - robot ?box1 - box)
    (connect ?room1 - room ?room2 - room ?door1 - door)
  )

  (:action goto_box
       :parameters ( ?Box - box ?Room - room ?Robot - robot)
       :precondition (and 
            (in_room ?Box ?Room)
            (robot_in ?Robot ?Room)

       ) :effect (and 
            (robot_near ?Robot ?Box)
        
    ))
  (:action leave_box
       :parameters ( ?Box - box ?Room - room ?Robot - robot)
       :precondition (and   
            (in_room ?Box ?Room)
            (robot_in ?Robot ?Room)
            (robot_near ?Robot ?Box)       
       ) :effect (and 
            (not (robot_near ?Robot ?Box))
        
    ))
  (:action gotodoor
       :parameters ( ?Robot - robot ?Room - room ?Door - door ?R - room)
       :precondition (and  
            (robot_in ?Robot ?Room)
       ) :effect (and 
            (robot_at ?Robot ?Door ?Room)
            (connect ?Room ?R ?Door)
        
    ))
  (:action gothrudoor
       :parameters ( ?Robot - robot ?Room - room ?Door - door ?R - room)
       :precondition (and   
            (robot_in ?Robot ?Room)
            (robot_at ?Robot ?Door ?Room)
            (connect ?Room ?R ?Door)       
       ) :effect (and 
            (not (robot_in ?Robot ?Room))
            (not (robot_at ?Robot ?Door ?Room))
            (robot_in ?Robot ?R)
        
    ))
  (:action pushtodoor
       :parameters ( ?Box - box ?Room - room ?Door - door ?Room1 - room ?Robot - robot ?R - room)
       :precondition (and   
            (in_room ?Box ?Room)
            (robot_in ?Robot ?Room)
            (robot_near ?Robot ?Box)
       ) :effect (and 
            (at_door ?Box ?Door ?Room)
            (connect ?Room ?Room1 ?Door)
            (robot_at ?Robot ?Door ?Room)
            (connect ?Room ?R ?Door)
        
    ))
  (:action pushthrudoor
       :parameters ( ?Box - box ?Room - room ?Door - door ?Room1 - room ?Robot - robot)
       :precondition (and   
            (in_room ?Box ?Room)
            (at_door ?Box ?Door ?Room)
            (connect ?Room ?Room1 ?Door)
            (robot_in ?Robot ?Room)
            (robot_near ?Robot ?Box)
            (robot_at ?Robot ?Door ?Room)
            (connect ?Room ?Room1 ?Door)       
       ) :effect (and 
            (not (in_room ?Box ?Room))
            (not (at_door ?Box ?Door ?Room))
            (not (robot_in ?Robot ?Room))
            (not (robot_at ?Robot ?Door ?Room))
            (in_room ?Box ?Room1)
            (robot_in ?Robot ?Room1)
    ))
  )

