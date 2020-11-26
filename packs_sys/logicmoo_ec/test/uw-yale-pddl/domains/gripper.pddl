(define (domain gripper-strips)
   (:predicates (room ?r)
		(ball ?b)
		(gripper ?g)
		(at-robby ?r)
		(at ?b ?r)
		(free ?g)
		(carry ?o ?g))

   (:action move
       :parameters  (?from ?to)
       :precondition (and  (room ?from) (room ?to) (at-robby ?from))
       :effect (and  (at-robby ?to)
		     (not (at-robby ?from))))



   (:action pick
       :parameters (?obj ?room ?gripper)
       :precondition  (and  (ball ?obj) (room ?room) (gripper ?gripper)
			    (at ?obj ?room) (at-robby ?room) (free ?gripper))
       :effect (and (carry ?obj ?gripper)
		    (not (at ?obj ?room)) 
		    (not (free ?gripper))))


   (:action drop
       :parameters  (?obj  ?room ?gripper)
       :precondition  (and  (ball ?obj) (room ?room) (gripper ?gripper)
			    (carry ?obj ?gripper) (at-robby ?room))
       :effect (and (at ?obj ?room)
		    (free ?gripper)
		    (not (carry ?obj ?gripper)))))

(define (domain gripper-typed)
   (:requirements :typing)
   (:types room ball gripper)
   (:constants left right - gripper)
   (:predicates (at-robby ?r - room)
		(at ?b - ball ?r - room)
		(free ?g - gripper)
		(carry ?o - ball ?g - gripper))

   (:action move
       :parameters  (?from ?to - room)
       :precondition (at-robby ?from)
       :effect (and  (at-robby ?to)
		     (not (at-robby ?from))))



   (:action pick
       :parameters (?obj - ball ?room - room ?gripper - gripper)
       :precondition  (and  (at ?obj ?room) (at-robby ?room) (free ?gripper))
       :effect (and (carry ?obj ?gripper)
		    (not (at ?obj ?room)) 
		    (not (free ?gripper))))


   (:action drop
       :parameters  (?obj - ball ?room - room ?gripper - gripper)
       :precondition  (and  (carry ?obj ?gripper) (at-robby ?room))
       :effect (and (at ?obj ?room)
		    (free ?gripper)
		    (not (carry ?obj ?gripper)))))


(define (problem gripper2)
    (:domain gripper-typed)
  (:objects roomA roomB - room Ball1 Ball2 - ball)


  (:init 
     (at-robby roomA) 
     (free left) 
     (free right)  
     (at Ball1 roomA)
     (at Ball2 roomA))

  (:goal (and (at Ball1 roomB) (at Ball2 roomB))))

(define (problem strips-gripper2)
    (:domain gripper-strips)
  (:objects roomA roomB Ball1 Ball2 left right)

   (:init 

   (room roomA)
   (room roomB)
   (ball Ball1)
   (ball Ball2)
   (gripper left)
   (gripper right)
   (at-robby roomA) 
   (free left) 
   (free right)  
   (at Ball1 roomA)
   (at Ball2 roomA))

   (:goal (and (at Ball1 roomB) (at Ball2 roomB))))

(define (problem gripper4)
    (:domain gripper-typed)
  (:objects roomA roomB - room Ball1 Ball2 Ball3 Ball4 - ball)

    (:init 

    (at-robby roomA) 
    (free left) 
    (free right)  
    (at Ball1 roomA)
    (at Ball2 roomA)
    (at Ball3 roomA)
    (at Ball4 roomA))

    (:goal (and (at Ball1 roomB) (at Ball2 roomB)
                (at Ball3 roomB) (at Ball4 roomB))))

(define (problem strips-gripper4)
    (:domain gripper-strips)
  (:objects roomA roomB Ball1 Ball2 Ball3 Ball4 left right)


    (:init 

    (room roomA)
    (room roomB)
    (ball Ball1)
    (ball Ball2)
    (ball Ball3)
    (ball Ball4)
    (gripper left)
    (gripper right)
    (at-robby roomA) 
    (free left) 
    (free right)  
    (at Ball1 roomA)
    (at Ball2 roomA)
    (at Ball3 roomA)
    (at Ball4 roomA))

    (:goal (and (at Ball1 roomB) (at Ball2 roomB)
                (at Ball3 roomB) (at Ball4 roomB))))

(define (problem gripper6)
    (:domain gripper-typed)
  (:objects roomA roomB - room Ball1 Ball2 Ball3 Ball4  Ball5 Ball6 - ball)

   (:init 

   (at-robby roomA) 
   (free left) 
   (free right)  
   (at Ball1 roomA)
   (at Ball2 roomA)
   (at Ball3 roomA)
   (at Ball4 roomA)
   (at Ball5 roomA)
   (at Ball6 roomA))

(:goal (and (at Ball1 roomB) (at Ball2 roomB)
            (at Ball3 roomB) (at Ball4 roomB)
            (at Ball5 roomB) (at Ball6 roomB)
)))

(define (problem strips-gripper6)
    (:domain gripper-strips)
  (:objects roomA roomB Ball1 Ball2 Ball3 Ball4  Ball5 Ball6 left right)


   (:init 

   (room roomA)
   (room roomB)
   (ball Ball1)
   (ball Ball2)
   (ball Ball3)
   (ball Ball4)
   (ball Ball5)
   (ball Ball6)
   (gripper left)
   (gripper right)
   (at-robby roomA) 
   (free left) 
   (free right)  
   (at Ball1 roomA)
   (at Ball2 roomA)
   (at Ball3 roomA)
   (at Ball4 roomA)
   (at Ball5 roomA)
   (at Ball6 roomA))

   (:goal (and (at Ball1 roomB) (at Ball2 roomB)
               (at Ball3 roomB) (at Ball4 roomB)
               (at Ball5 roomB) (at Ball6 roomB)
   )))

(define (problem gripper8)
    (:domain gripper-typed)
  (:objects roomA roomB - room
            Ball1 Ball2 Ball3 Ball4  Ball5 Ball6 Ball7 Ball8  - ball)


   (:init 

   (at-robby roomA) 
   (free left) 
   (free right)  
   (at Ball1 roomA)
   (at Ball2 roomA)
   (at Ball3 roomA)
   (at Ball4 roomA)
   (at Ball5 roomA)
   (at Ball6 roomA)
   (at Ball7 roomA)
   (at Ball8 roomA)
   )

   (:goal (and (at Ball1 roomB) (at Ball2 roomB)
               (at Ball3 roomB) (at Ball4 roomB)
               (at Ball5 roomB) (at Ball6 roomB)
               (at Ball7 roomB) (at Ball8 roomB))))

(define (problem strips-gripper8)
    (:domain gripper-strips)
  (:objects roomA roomB Ball1 Ball2 Ball3 Ball4  Ball5 Ball6 
            Ball7 Ball8 left right)


   (:init 

   (room roomA)
   (room roomB)
   (ball Ball1)
   (ball Ball2)
   (ball Ball3)
   (ball Ball4)
   (ball Ball5)
   (ball Ball6)
   (ball Ball7)
   (ball Ball8)

   (gripper left)
   (gripper right)
   (at-robby roomA) 
   (free left) 
   (free right)  
   (at Ball1 roomA)
   (at Ball2 roomA)
   (at Ball3 roomA)
   (at Ball4 roomA)
   (at Ball5 roomA)
   (at Ball6 roomA)
   (at Ball7 roomA)
   (at Ball8 roomA)
   )

   (:goal (and (at Ball1 roomB) (at Ball2 roomB)
               (at Ball3 roomB) (at Ball4 roomB)
               (at Ball5 roomB) (at Ball6 roomB)
               (at Ball7 roomB) (at Ball8 roomB)   )))
