;;; From: Alfonso Gerevini <gerevini@minerva.ing.unibs.it>
;;; To: weld@cs.washington.edu
;;; Subject: Trains and T-Trains
;;; Cc: schubert@cs.rochester.edu, gerevini@bsing.ing.unibs.it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A TRAINS DOMAIN (loosely based on TRAINS-93 world)     
;;
;; From A. Gerevini & L.K. Schubert's AIPS-96 paper.

(define (domain Trains)
  (:requirements :strips :conditional-effects :existential-preconditions)
  (:predicates (engine ?e)
	       (at ?thing ?city)
	       (coupled ?e ?car)
	       (oranges ?o)
	       (boxcar ?car)
	       (empty ?car)
	       (bananas ?b)
	       (tanker-car ?car)
	       (oj-fac ?fac)
	       (oj ?o)
	       (loose ?car)
	       (connects ?tr ?c1 ?c2)
	       (in ?thing ?car)
	       (city ?c)
	       (track ?tr)
	       (car ?c)
	       (comm ?c)
	       )


  (:action mv-engine
      :parameters (?eng ?cityone ?citytwo ?track ?car)
		  ; ?car is a "hidden" parameter
      :precondition (and (engine ?eng) (at ?eng ?cityone) 
			  (connects ?track ?cityone ?citytwo))
      :effect (and (at ?eng ?citytwo) (not (at ?eng ?cityone))
                    (when (coupled ?eng ?car) 
		      (and (at ?car ?citytwo)
				(not (at ?car ?cityone)) ))))
  
  (:action ld-oranges
      :parameters (?ors ?car ?city)
      :precondition (and (oranges ?ors) (boxcar ?car) 
			  (empty ?car)
                          (at ?ors ?city) (at ?car ?city) )
      :effect (and (not (empty ?car)) (in ?ors ?car) 
		    (not (at ?ors ?city))) )
  
  (:action ld-bananas
      :parameters (?bas ?car ?city)
      :precondition (and (bananas ?bas) (boxcar ?car) 
			  (empty ?car)
                          (at ?bas ?city) (at ?car ?city) )
      :effect (and (not (empty ?car)) (in ?bas ?car) 
		    (not (at ?bas ?city))) )
  
  (:action ld-oj
      :parameters (?oj ?car ?city)
      :precondition (and (oj ?oj) 
			  (tanker-car ?car) 
			  (empty ?car)
                          (at ?oj ?city) (at ?car ?city) )
      :effect (and (not (empty ?car)) (in ?oj ?car) 
		    (not (at ?oj ?city))) )
  
  (:action make-oj
      :parameters (?o ?fac ?city)
      :precondition (and (oranges ?o) (oj-fac ?fac) 
                          (at ?o ?city) (at ?fac ?city) )
      :effect (and (oj ?o) (not (oranges ?o))) )
  
  (:action unload
      :parameters (?comm ?car ?city)
      :precondition (and (in ?comm ?car) (at ?car ?city))
      :effect (and (not (in ?comm ?car)) (empty ?car) 
		    (at ?comm ?city)) )
  
  (:action couple
      :parameters (?eng ?car ?city)
      :precondition (and (engine ?eng) (car ?car) (loose ?car) 
                          (at ?eng ?city) (at ?car ?city) )
      :effect (and (coupled ?eng ?car) (not (loose ?car))) )
  
  (:action uncouple
      :parameters (?eng ?car)
      :precondition (coupled ?eng ?car)
      :effect (and (loose ?car) (not (coupled ?eng ?car))) ) 
  ) 


;; Initial states of Trains1/2/3 and of T-Trains1/2/3:
;; the initial state of Trains3 (T-Trains3) is the same as that of
;; Trains1 (T-Trains1) except that oj-fac1 and e3 are at Corning instead
;; of Elmira. The initial state of Trains2 is the same as that of Trains3
;; except that the connections from Corning to Bath and from Dansville to
;; Corning are disabled (say, for maintenance).
;; 
(define (problem Trains1)
    (:domain Trains)
  (:objects avon bath corning dansville elmira
		  tr1 tr2 tr3 tr4 tr5
		  e1 e2 e3
		  bc1 bc2 bc3 bc4 tc1
		  ors1 bas1 oj-fac1)
  (:init
   (city avon) (city bath) (city corning) (city dansville) (city elmira)
   (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
   (connects tr1 avon bath) (connects tr1 bath avon) 
   (connects tr2 bath corning) (connects tr2 corning bath)
   (connects tr3 avon dansville) (connects tr3 dansville avon)
   (connects tr4 dansville corning) (connects tr4 corning dansville)
   (connects tr5 corning elmira) (connects tr5 elmira corning)
   (engine e1) (engine e2) (engine e3)    
   (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
   (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
   (oranges ors1) (bananas bas1) (oj-fac oj-fac1)
   (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
   (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
   (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
   (at bc3 dansville) (at tc1 corning) (at ors1 corning)
   (at e2 elmira) (at e3 elmira) (at bc4 elmira) (at oj-fac1 elmira))
  (:goal (AND (exists (?x) (and (oranges ?x) (at ?x bath))))))

(define (problem Trains2)
    (:domain Trains)
  (:objects avon bath corning dansville elmira
		  tr1 tr2 tr3 tr4 tr5
		  e1 e2 e3
		  bc1 bc2 bc3 bc4 tc1
		  ors1 bas1 oj-fac1)
  (:init
   (city avon) (city bath) (city corning) (city dansville) (city elmira)
   (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
   (connects tr1 avon bath) (connects tr1 bath avon)
   (connects tr2 bath corning) (connects tr4 corning dansville)
   (connects tr3 avon dansville) (connects tr3 dansville avon)
   ;;  (connects tr2 corning bath) (connects tr4 dansville corning)     
   (connects tr5 corning elmira) (connects tr5 elmira corning)
   (engine e1) (engine e2) (engine e3)
   (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
   (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
   (oranges ors1) (bananas bas1) (oj-fac oj-fac1)
   (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
   (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
   (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
   (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
   (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning)) 
  (:goal (AND (exists (?x) (and (oj ?x) (at ?x dansville))))))

(define (problem Trains3)
    (:domain Trains)
  (:objects avon bath corning dansville elmira
		  tr1 tr2 tr3 tr4 tr5
		  e1 e2 e3
		  bc1 bc2 bc3 bc4 tc1
		  ors1 bas1 oj-fac1)
  (:init
   (city avon) (city bath) (city corning) (city dansville) (city elmira)
   (track tr1) (track tr2) (track tr3) (track tr4) (track tr5)
   (connects tr1 avon bath) (connects tr1 bath avon)
   (connects tr2 bath corning) (connects tr2 corning bath)
   (connects tr3 avon dansville) (connects tr3 dansville avon)
   (connects tr4 dansville corning) (connects tr4 corning dansville)
   (connects tr5 corning elmira) (connects tr5 elmira corning)
   (engine e1) (engine e2) (engine e3)
   (car bc1) (car bc2) (car bc3) (car bc4) (car tc1)
   (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
   (oranges ors1) (bananas bas1) (oj-fac oj-fac1)
   (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
   (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
   (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
   (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
   (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning)) 
  (:goal (AND (exists (?x) (and (oj ?x) (at ?x dansville))))))


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


(define (problem T-Trains1)
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
   (connects tr2 bath corning) (connects tr2 corning bath)
   (connects tr3 avon dansville) (connects tr3 dansville avon)
   (connects tr4 dansville corning) (connects tr4 corning dansville)
   (connects tr5 corning elmira) (connects tr5 elmira corning)
   (engine e1) (engine e2) (engine e3)
   (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
   (oranges ors1) (comm ors1) (comm bas1)
   (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
   (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
   (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
   (at bc3 dansville) (at tc1 corning) (at ors1 corning)
   (at e2 elmira) (at e3 elmira) (at bc4 elmira) (at oj-fac1 elmira))
  (:goal (AND (exists (?x - orange-food) (and (oranges ?x) (at ?x bath))))))


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


(define (problem T-Trains3)
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
   (connects tr2 bath corning) (connects tr2 corning bath)
   (connects tr3 avon dansville) (connects tr3 dansville avon)
   (connects tr4 dansville corning) (connects tr4 corning dansville)
   (connects tr5 corning elmira) (connects tr5 elmira corning)
   (boxcar bc1) (boxcar bc2) (boxcar bc3) (boxcar bc4) (tanker-car tc1)
   (oranges ors1) (comm ors1) (comm bas1)
   (empty bc1) (empty bc2) (empty bc3) (empty bc4) (empty tc1)
   (loose bc1) (loose bc2) (loose bc3) (loose bc4) (loose tc1)
   (at e1 avon) (at bas1 avon) (at bc1 bath) (at bc2 bath)
   (at bc3 dansville) (at tc1 corning) (at ors1 elmira)
   (at e2 elmira) (at e3 corning) (at bc4 elmira) (at oj-fac1 corning))  
  (:goal (AND (exists (?x - orange-food) (and (oj ?x) (at ?x dansville)))) ))




