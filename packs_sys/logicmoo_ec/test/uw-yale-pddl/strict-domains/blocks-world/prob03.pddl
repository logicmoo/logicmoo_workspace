(define (problem tower-invert4)         ; graphplan 6 steps, 8 actions
    (:domain blocks-world-domain)
  (:objects A B C D)
  (:init (block A) (block B) (block C) (block D) (block Table)
	 (on a b) (on b c) (on c d) (on d table)
	 (clear a) (clear table))
  (:goal (and (on b c) (on c d) (on d a)))
  (:length (:serial 6) (:parallel 6)))