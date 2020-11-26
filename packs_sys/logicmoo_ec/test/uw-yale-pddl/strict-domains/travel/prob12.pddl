(define (problem Run-Jack-Run)		;no solution, endless recursion
    (:domain roads)
  (:objects Jack KI Rockwell)
  (:length (:serial -1) (:parallel -1))
  (:goal (and (at Jack KI) (at Jack Rockwell)))
  (:init (at jack Rockwell)
	 (road Rockwell KI) (road KI Rockwell)))