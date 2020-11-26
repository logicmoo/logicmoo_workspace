(define (problem simple-block3)         ; graphplan 4 steps, 6 actions
    (:domain simple-blocks)
  (:objects A B C D E)
  (:goal (and (on d e) (on c d) (on b c) (on a b)))
  (:init (on a table) (on b a)
	 (on c table) (on d c)
	 (on e table) (clear e)
	 (clear d) (clear b))
  (:length (:serial 4) (:parallel 4)))