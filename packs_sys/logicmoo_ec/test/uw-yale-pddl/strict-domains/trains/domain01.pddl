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







