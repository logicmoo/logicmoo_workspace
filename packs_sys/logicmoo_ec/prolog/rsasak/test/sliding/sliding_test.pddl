(define (problem sliding_test)
(:domain sliding_domain)
(:objects p1 p2 p3 p4 b1)
		  
(:init 
(block b1)

(left p1 p2)
(left p3 p4)

(over p1 p3)
(over p2 p4)

(at b1 p3)
(free p1)
(free p2)
(free p4)
)

(:goal (at b1 p1))
		
)