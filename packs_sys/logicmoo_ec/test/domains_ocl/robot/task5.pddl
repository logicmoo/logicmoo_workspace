
(define (problem task5)
   (:domain robot)
   (:objects
         room1 room2 room3 room4 room5 room6 room7 - room
         door12 door23 door24 door25 door35 door45 door47 door56 door57 door67 - door
         box1 box2 big_box - box
         tom - robot
        )
    (:init
        (robot_in tom room2)
        (robot_near tom box1)
        (in_room box1 room2)
        (at_door box1 door25 room2)
        (in_room box2 room6)
        (in_room big_box room4)
        (at_door big_box door24 room4)
        (connect room2 room3 door23)
        (connect room3 room2 door23)
        (connect room2 room4 door24)
        (connect room4 room2 door24)
        (connect room2 room5 door25)
        (connect room5 room2 door25)
        (connect room6 room5 door56)
        (connect room5 room6 door56)
        (connect room5 room3 door35)
        (connect room3 room5 door35)
        (connect room5 room4 door45)
        (connect room4 room5 door45)
        (connect room4 room7 door47)
        (connect room7 room4 door47)
        (connect room5 room7 door57)
        (connect room7 room5 door57)
        (connect room6 room7 door67)
        (connect room7 room6 door67)
        (connect room2 room1 door12)
        (connect room1 room2 door12)
        )
    (:goal
      (and
        (robot_in tom room5)
        (in_room box2 room6)
        (at_door box2 door56 room6)
        (in_room big_box room1)
       ))
)

