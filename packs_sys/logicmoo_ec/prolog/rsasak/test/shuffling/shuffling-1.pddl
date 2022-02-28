(define (problem shuffling-1)
(:domain shuffling_domain)
(:objects block1 block2 pos1 pos2)

(:init
(at block1 pos2)
(at block2 pos1)

(left pos1 pos2)
)
(:goal
    (and (at block1 pos1)
         (at block2 pos2)))
)
