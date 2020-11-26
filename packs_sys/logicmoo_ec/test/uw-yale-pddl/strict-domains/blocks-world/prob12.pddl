(define (problem simple-block1)         ; sussman anomaly - graphplan 3 steps
    (:domain simple-blocks)
  (:objects A B C)
  (:goal (and (on a b) (on b c)))
  (:init (on a table)
	 (on c a) (clear c)
	 (on b table) (clear b))
  (:length (:serial 3) (:parallel 3)))