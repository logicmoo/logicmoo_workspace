(define (problem tower-invert3)         ; graphplan 4 steps
    (:domain blocks-world-domain)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on a b) (on b c) (on c table)
	 (clear a) (clear table))
  (:goal (and (on b c) (on c a)))
  (:length (:serial 4) (:parallel 4)))