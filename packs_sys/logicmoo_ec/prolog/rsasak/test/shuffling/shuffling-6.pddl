(define (problem shuffling-6)
(:domain shuffling_domain)
(:objects block1 block2 block3 block4 block5 block6
          pos1 pos2 pos3 pos4 pos5 pos6)

(:init
(at block1 pos2)
(at block2 pos5)
(at block3 pos1)
(at block4 pos4)
(at block5 pos6)
(at block6 pos3)

(left pos1 pos2)
(left pos2 pos3)
(left pos3 pos4)
(left pos4 pos5)
(left pos5 pos6)
)
(:goal
    (and (at block1 pos1)
         (at block2 pos2)
         (at block3 pos3)
         (at block4 pos4)
         (at block5 pos5)
         (at block6 pos6)))
)
