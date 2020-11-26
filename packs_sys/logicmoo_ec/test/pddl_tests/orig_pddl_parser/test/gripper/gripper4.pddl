(define (problem gripper4)
    (:domain gripper)
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
