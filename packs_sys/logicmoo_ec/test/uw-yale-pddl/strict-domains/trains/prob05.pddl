(define (problem T-Trains2)
    (:domain T-Trains)
  (:objects avon bath corning dansville elmira - city
	    tr1 tr2 tr3 tr4 tr5 - track
	    e1 e2 e3 - engine
	    bc1 bc2 bc3 bc4 tc1 - car
	    ors1 - orange-food
	    bas1 - bananas
	    oj-fac1 - oj-fac
	    )
  (:init
   (connects tr1 avon bath) (connects tr1 bath avon)
   (connects tr2 bath corning) (connects tr4 corning dansville)
   (connects tr3 avon dansville) (connects tr3 dansville avon)
   ;;  (connects tr2 corning bath) (connects tr4 dansville corning)     
   (connects tr5 corning elmira) (connects tr5 elmira corning)
   (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
   (oranges ors1) (comm ors1) (comm bas1)
   (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
   (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
   (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
   (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
   (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning))
  (:goal (AND (exists (?x - orange-food) (and (oj ?x) (at ?x dansville))))))