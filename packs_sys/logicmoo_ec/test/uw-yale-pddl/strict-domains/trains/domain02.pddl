;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; T-Trains ("Typed" version of the Trains domain)          
;;
(define (domain T-Trains)
  (:requirements :strips :typing :conditional-effects :existential-preconditions)
  (:types city engine car bananas orange-food oj-fac track)
  (:predicates (at ?thing - object
		   ?c - city)
	       (connects ?tr - track  ?c1 ?c2 - city)
	       (coupled ?e - engine  ?car - car)
	       (boxcar ?car - car)
	       (empty ?car - car)
	       (tanker-car ?car - car)
	       (loose ?car - car)
	       (in ?thing - object
		   ?car - car)
	       (oranges ?o)
	       (oj ?o)
	       (comm ?comm))
	       
	       
  (:action mv-engine
      :parameters (?eng - engine  ?cityone ?citytwo - city  ?track - track ?car - car)
		  ; ?car is a "hidden" parameter
      :precondition (and (at ?eng ?cityone) (connects ?track ?cityone ?citytwo))
      :effect (and (at ?eng ?citytwo) (not (at ?eng ?cityone))
		    (when (coupled ?eng ?car)
			(and (at ?car ?citytwo)
			      (not (at ?car ?cityone)) ))))
  
  (:action ld-oranges
      :parameters (?ors - orange-food  ?car - car ?city - city)
      :precondition (and (oranges ?ors) (boxcar ?car) (empty ?car)
			  (at ?ors ?city) (at ?car ?city) )
      :effect (and (not (empty ?car)) (in ?ors ?car) 
		    (not (at ?ors ?city))) )
  
  (:action ld-bananas
      :parameters (?bas - bananas ?car - car ?city - city)
      :precondition (and (boxcar ?car) (empty ?car)
			  (at ?bas ?city) (at ?car ?city) )
      :effect (and (not (empty ?car)) (in ?bas ?car) 
		    (not (at ?bas ?city))) )
  
  (:action ld-oj
      :parameters (?oj - orange-food ?car - car ?city - city)
      :precondition (and (oj ?oj) (tanker-car ?car) (empty ?car)
			  (at ?oj ?city) (at ?car ?city) )
      :effect (and (not (empty ?car)) (in ?oj ?car) 
		    (not (at ?oj ?city))) )
  
  (:action make-oj
      :parameters (?o - orange-food ?fac - oj-fac ?city - city)
      :precondition (and (oranges ?o) (at ?o ?city) (at ?fac ?city) )
      :effect (and (oj ?o) (not (oranges ?o))) )
  
  (:action unload
      :parameters (?comm - (either bananas orange-food) ?car - car ?city - city)
      :precondition  (and (comm ?comm) (in ?comm ?car) (at ?car ?city)) 
      :effect (and (not (in ?comm ?car)) (empty ?car) 
		    (at ?comm ?city)) )
  
  (:action couple
      :parameters (?eng - engine ?car - car ?city - city)
      :precondition (and (loose ?car) (at ?eng ?city) (at ?car ?city) )
      :effect (and (coupled ?eng ?car) (not (loose ?car))) )
  
  (:action uncouple
      :parameters (?eng - engine ?car - car)
      :precondition (and (coupled ?eng ?car))
      :effect (and (loose ?car) (not (coupled ?eng ?car))) ) 
  )