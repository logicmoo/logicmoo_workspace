(define: (domain default)
  (:requirements :strips :equality :typing :conditional-effects)
  (:predicates 
              (in_room ?x1 - box ?x2 - room)
              (at_door ?x1 - box ?x2 - door ?x3 - room)
              (robot_in ?x1 - robot ?x2 - room)
              (robot_at ?x1 - robot ?x2 - door ?x3 - room)
              (robot_near ?x1 - robot ?x2 - box)
              (connect ?x1 - room ?x2 - room ?x3 - door)
)

  (:action goto_box
         :parameters ( ?x4 - robot ?x5 - box ?x6 - room)
         :precondition (and 
(in_room ?x5 ?x6)(robot_in ?x4 ?x6))
         :effect 
(robot_near ?x4 ?x5)

  (:action leave_box
         :parameters ( ?x1 - robot ?x2 - box ?x3 - room)
         :precondition (and 
(in_room ?x2 ?x3)(robot_in ?x1 ?x3)(robot_near ?x1 ?x2))
         :effect 
(not (robot_near ?x1 ?x2))


  (:action gotodoor
         :parameters ( ?x1 - robot ?x1 - room ?x2 - door ?x3 - room)
         :precondition (and 
(connect ?x3 ?x1 ?x2)(robot_in ?x1 ?x3))
         :effect 
(robot_at ?x1 ?x2 ?x3)

  (:action gothrudoor
         :parameters ( ?x2 - robot ?x3 - door ?x4 - room ?x5 - room)
         :precondition (and 
(robot_in ?x2 ?x4)(robot_at ?x2 ?x3 ?x4)(connect ?x4 ?x5 ?x3))
         :effect (and 
(robot_in ?x2 ?x5)(not (robot_in ?x2 ?x4))
(not (robot_at ?x2 ?x3 ?x4))
)

  (:action pushtodoor
         :parameters ( ?x1 - robot ?x1 - room ?x2 - box ?x3 - door ?x4 - room)
         :precondition (and 
(in_room ?x2 ?x4)(connect ?x4 ?x1 ?x3)(robot_in ?x1 ?x4)(robot_near ?x1 ?x2))
         :effect (and 
(at_door ?x2 ?x3 ?x4)(robot_at ?x1 ?x3 ?x4))

  (:action pushthrudoor
         :parameters ( ?x2 - robot ?x3 - box ?x4 - door ?x5 - room ?x6 - room)
         :precondition (and 
(in_room ?x3 ?x5)(at_door ?x3 ?x4 ?x5)(robot_in ?x2 ?x5)(robot_near ?x2 ?x3)(robot_at ?x2 ?x4 ?x5)(connect ?x5 ?x6 ?x4))
         :effect (and 
(in_room ?x3 ?x6)(not (in_room ?x3 ?x5))
(not (at_door ?x3 ?x4 ?x5))
(robot_in ?x2 ?x6)(not (robot_in ?x2 ?x5))
(not (robot_at ?x2 ?x4 ?x5))
)

)

)
