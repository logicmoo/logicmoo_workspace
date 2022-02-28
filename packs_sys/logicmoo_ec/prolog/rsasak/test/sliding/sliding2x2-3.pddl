(define (problem sliding2x2-3)
(:domain sliding_domain)
(:objects p1 p2 p3 p4 b1 b2 b3)

(:init
(block b1)
(block b2)
(block b3)

(left p1 p2)
(left p3 p4)

(over p1 p3)
(over p2 p4)

(at b1 p3)
(at b2 p1)
(at b3 p2)
(free p4)
)

(:goal (and (at b1 p1)
			(at b2 p2)
			(at b3 p3)))

)
