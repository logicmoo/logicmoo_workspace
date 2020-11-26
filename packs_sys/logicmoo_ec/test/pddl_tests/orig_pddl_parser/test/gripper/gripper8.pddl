(define (problem gripper8)
    (:domain gripper)
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
