(define (problem woodshop-strips-1)
    (:domain woodshop-strips)
  (:objects a - physobj
	    natural green - color
	    square - objshape
	    three-eighths - size)
  (:init  (not (inbox a)) (not (inhood a)) (shape a round) (colored a natural)
           (rough a) (not (hole a three-eighths)))
  (:length (:serial 9) (:parallel 9))  
  (:goal (and (colored a green) (shape a round) (hole a three-eighths) (not (dusty a))
               (inbox a) (not (rough a)))))